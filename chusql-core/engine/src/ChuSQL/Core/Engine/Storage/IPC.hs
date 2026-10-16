{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Core.Engine.Storage.IPC (
    IPCStorage (..),
    Env (..),
    envForDatabase,
    defaultEnv,
    runIPCStorage,
    Request (..),
    Response (..),
    Account (..),
    SchemaColumn (..),
    TableInfo (..),
    StorageLink (..),
    setStorageLink,
    localStorageLink,
    sendRequest,
    sendRawRequest,
    closeConnection,
    setDatabaseName,
    getDatabaseName,
    doListTables,
    doLookupByColumn,
    doInsert,
    doInsertMany,
    doDeleteKeys,
    doDescribeTable,
    doDropTable,
    doListCatalog,
    databaseSnapshot,
    snapshotDatabase,
) where

import ChuSQL.Core.Engine.Parallel (parallelShardLimit, poolRun, workerPool)
import ChuSQL.Core.Engine.Storage
import ChuSQL.Core.Engine.Storage.FFI (closeStorage, openStorage, storageRequest)
import ChuSQL.Core.Model
import ChuSQL.Core.Protocol (Account (..), Request (..), Response (..), SchemaColumn (..), TableInfo (..))
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, putMVar, readMVar, takeMVar, withMVar)
import Control.Exception (bracket, mask_)
import Control.Monad (when)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap qualified as KM
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Foldable (for_)
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)

-- IPC 存储实现：把 MonadStorage 的操作变成发给存储库的 JSON 请求。

-- 复用相同提交点的不可变快照版本。
databaseSnapshot :: Maybe (String, Database) -> Env -> IO (Either String (String, Database))
databaseSnapshot previous env = do
    response <- ask env "database_snapshot" (ReqDatabaseSnapshot (fst <$> previous)) Just
    pure $ case response of
        Left err -> Left err
        Right (RespSnapshotUnchanged checkpoint) -> case previous of
            Just cached | fst cached == checkpoint -> Right cached
            _ -> Left "snapshot version does not match cached database"
        Right (RespDomainSnapshot infos rows checkpoint) -> do
            db <- snapshotDatabase infos rows
            Right (checkpoint, db)
        Right (RespError err) -> Left err
        _ -> Left "unexpected database snapshot response"

-- 从同一版本的结构和行集构建引擎数据库。
snapshotDatabase :: [TableInfo] -> [(String, [Row])] -> Either String Database
snapshotDatabase infos rows = mapM load infos
  where
    -- 布局、对象身份与数据必须来自同一版本。
    load info = do
        values <- maybe (Left "incomplete database snapshot") Right (lookup (tiTable info) rows)
        let name = tiTable info
        Right (name, Table name (schemaToColumns (tiColumns info)) values (Just (metaOf info)) (tiIdentity info) (tiSchemaVersion info))

-- | 一条会话的存储环境：当前库名随会话走，不共享
data Env = Env
    { envDatabase :: Maybe String
    , envSchemaGuard :: Maybe Database
    }

newtype IPCStorage a = IPCStorage
    { runIPCStorageIn :: Env -> IO a
    }

-- | 按库名造一个会话环境
envForDatabase :: Maybe String -> Env
envForDatabase name = Env name Nothing

-- | 进程默认环境：读全局库名，给测试与 CLI 用
defaultEnv :: IO Env
defaultEnv = envForDatabase <$> readMVar databaseRef

-- | 用进程默认环境跑（兼容既有调用点）
runIPCStorage :: IPCStorage a -> IO a
runIPCStorage action = defaultEnv >>= runIPCStorageIn action

instance Functor IPCStorage where
    fmap f (IPCStorage m) = IPCStorage (fmap f . m)

instance Applicative IPCStorage where
    pure = IPCStorage . const . pure
    IPCStorage mf <*> IPCStorage ma = IPCStorage $ \env -> mf env <*> ma env

instance Monad IPCStorage where
    IPCStorage m >>= k = IPCStorage $ \env -> do
        a <- m env
        runIPCStorageIn (k a) env

-- | 一条到存储库的链路：管请求与关闭
data StorageLink = StorageLink
    { slRequest :: BS.ByteString -> IO (Either String BS.ByteString)
    , slClose :: IO ()
    }

{-# NOINLINE linkRef #-}

-- | 进程全局的存储链路
linkRef :: MVar (Maybe StorageLink)
linkRef = unsafePerformIO (newMVar Nothing)

{-# NOINLINE linkUsers #-}

-- | 在途请求计数与空闲信号
linkUsers :: (MVar Int, MVar ())
linkUsers = unsafePerformIO $ (,) <$> newMVar 0 <*> newMVar ()

-- | 登记请求并在结束时释放链路占用
withStorageLink :: (Maybe StorageLink -> IO a) -> IO a
withStorageLink = bracket enter leave
  where
    (users, idle) = linkUsers
    -- \| 登记当前链路的在途请求
    enter = withMVar linkRef $ \current -> do
        case current of
            Nothing -> pure ()
            Just _ -> modifyMVar_ users $ \count -> do
                when (count == 0) $ takeMVar idle
                pure (count + 1)
        pure current
    -- \| 最后一个请求结束后发出空闲信号
    leave Nothing = pure ()
    leave (Just _) = mask_ $ modifyMVar_ users $ \count -> do
        when (count == 1) $ putMVar idle ()
        pure (count - 1)

-- | 装入一条链路；一个进程只装一次
setStorageLink :: StorageLink -> IO ()
setStorageLink link = modifyMVar_ linkRef $ \_ -> withMVar (snd linkUsers) (const (pure (Just link)))

-- | 打开本地的存储动态库，得到一条链路
localStorageLink :: Maybe FilePath -> IO (Either String StorageLink)
localStorageLink path = fmap (fmap toLink) (openStorage path)
  where
    -- \| 把句柄包成一条链路
    toLink handle =
        StorageLink
            { slRequest = storageRequest handle
            , slClose = closeStorage handle
            }

-- | 关掉当前链路（进程退出前调用）；没有链路就什么都不做
closeConnection :: IO ()
closeConnection = modifyMVar_ linkRef $ \current -> withMVar (snd linkUsers) $ \_ -> do
    for_ current slClose
    pure Nothing

-- | 发一条请求并解码应答；链路没装就报错，不崩
sendRequest :: Request -> IO Response
sendRequest req = withStorageLink $ \case
    Nothing -> pure (RespError "storage not configured")
    Just link ->
        do
            raw <- slRequest link (BL.toStrict (encode req))
            case raw of
                Left e -> pure (RespError ("storage error: " ++ e))
                Right bytes ->
                    case eitherDecodeStrict (BS.dropWhileEnd (== 13) bytes) of
                        Left e -> pure (RespError ("decode error: " ++ e))
                        Right resp -> pure resp

-- | 原样转发一条载荷给存储库
sendRawRequest :: BS.ByteString -> IO (Either String BS.ByteString)
sendRawRequest payload = withStorageLink $ \current -> do
    case current of
        Nothing -> pure (Left "storage not configured")
        Just link -> slRequest link payload

{-# NOINLINE databaseRef #-}

-- | 进程全局的当前库名
databaseRef :: MVar (Maybe String)
databaseRef = unsafePerformIO (newMVar Nothing)

-- | 设定当前库名（会话层决定，本层只记住并发给存储）
setDatabaseName :: Maybe String -> IO ()
setDatabaseName name = modifyMVar_ databaseRef (const (pure name))

-- | 当前库名
getDatabaseName :: IO (Maybe String)
getDatabaseName = readMVar databaseRef

-- | 给语句类请求盖上当前库名
stampDatabase :: Maybe String -> Request -> Request
stampDatabase Nothing req = req
stampDatabase (Just db) req
    | globalRequest req = req
    | otherwise = ReqInDatabase db req

-- | 是不是不该带库名的全局请求
globalRequest :: Request -> Bool
globalRequest ReqDatabase{} = True
globalRequest ReqAllCatalog = True
globalRequest ReqAccountsList = True
globalRequest ReqAccountCreate{} = True
globalRequest ReqAccountReset{} = True
globalRequest ReqAccountLogin{} = True
globalRequest ReqAccountDrop{} = True
globalRequest ReqPing = True
globalRequest _ = False

-- | 发请求并把响应翻成 Either
ask :: Env -> String -> Request -> (Response -> Maybe a) -> IO (Either String a)
ask env what req recognize = do
    resp <- case envSchemaGuard env of
        Nothing -> sendRequest (stampDatabase (envDatabase env) req)
        Just dependencies -> guardedRequest env dependencies req
    pure $ case resp of
        RespError e -> Left e
        other -> case recognize other of
            Just a -> Right a
            Nothing -> Left ("unexpected response to " ++ what)

-- | 发送依赖守卫并要求存储确认执行
guardedRequest :: Env -> Database -> Request -> IO Response
guardedRequest env dependencies req = do
    let payload = case A.toJSON (stampDatabase (envDatabase env) req) of
            A.Object fields -> A.Object (KM.insert "schema_guard" (A.toJSON (map guardOf dependencies)) fields)
            other -> other
    result <- sendRawRequest (BL.toStrict (encode payload))
    pure $ case result >>= eitherDecodeStrict of
        Left err -> RespError err
        Right (A.Object fields)
            | KM.lookup "status" fields == Just (A.String "guarded")
            , Just response <- KM.lookup "response" fields -> case A.fromJSON response of
                A.Success decoded -> decoded
                A.Error err -> RespError ("decode error: " ++ err)
        Right value -> case A.fromJSON value of
            A.Success (RespError err) -> RespError err
            _ -> RespError "storage did not confirm bound schema guard"
  where
    -- \| 编码依赖身份、版本与完整列定义
    guardOf (name, table) =
        A.object
            [ "table" A..= name
            , "database_id" A..= fmap (\(TableIdentity database _) -> show database) (tableIdentity table)
            , "object_id" A..= fmap (\(TableIdentity _ object) -> show object) (tableIdentity table)
            , "schema_version" A..= fmap show (tableSchemaVersion table)
            , "columns" A..= map toWire (tableCols table)
            ]

-- | 只认"成了"（多数写操作都这样）
okOnly :: Response -> Maybe ()
okOnly RespOk = Just ()
okOnly _ = Nothing

-- | 发 Scan
doScan :: Env -> String -> IO (Either String [Row])
doScan env t = ask env "scan" (ReqScan t) $ \case
    RespRows rows -> Just rows
    _ -> Nothing

-- | 发带投影的 Scan
doScanColumns :: Env -> String -> [String] -> IO (Either String [Row])
doScanColumns env t cols = ask env "scan" (ReqScanColumns t cols) $ \case
    RespRows rows ->
        Just
            [[(c, v) | c <- cols, Just v <- [lookup c row]] | row <- rows]
    _ -> Nothing

-- | 发分片 Scan：只取第 shard 片
doScanShard :: Env -> String -> Maybe [String] -> Int -> Int -> IO (Either String [Row])
doScanShard env t cols shard shards = ask env "scan_shard" (ReqScanShard t cols shard shards) $ \case
    RespRows rows -> Just rows
    _ -> Nothing

-- | 发 Insert（索引由存储层维护）
doInsert :: Env -> String -> Row -> IO (Either String ())
doInsert env t r = ask env "insert" (ReqInsert t r) okOnly

-- | 发 InsertBatch：一批行一次请求
doInsertMany :: Env -> String -> [Row] -> IO (Either String ())
doInsertMany env t rs = ask env "insert_batch" (ReqInsertBatch t rs) okOnly

-- | 发 DeleteKeys
doDeleteKeys :: Env -> String -> [Int] -> IO (Either String ())
doDeleteKeys env t ks = ask env "delete_keys" (ReqDeleteKeys t ks) okOnly

-- | 发 ListTables
doListTables :: Env -> IO (Either String [String])
doListTables env = ask env "list_tables" ReqListTables $ \case
    RespTables ts -> Just ts
    _ -> Nothing

-- | 一次拿全库 schema
doListCatalog :: Env -> IO (Either String [TableInfo])
doListCatalog env = ask env "list_catalog" ReqListCatalog $ \case
    RespCatalog xs -> Just xs
    _ -> Nothing

-- | 按某一列的索引取一行（键可以是整数或字符串）
doLookupByColumn :: Env -> String -> String -> Value -> IO (Either String IndexResult)
doLookupByColumn env t c k = ask env "lookup_by_index" (ReqLookupByColumn t c k) $ \case
    RespRows rows -> Just (IndexRows rows)
    RespNoIndex -> Just NoIndex
    _ -> Nothing

-- | 范围扫描：没有可用索引就回 Nothing，让上层退回扫描
doScanRange ::
    Env ->
    String ->
    String ->
    Maybe (Value, Bool) ->
    Maybe (Value, Bool) ->
    IO (Either String (Maybe [Row]))
doScanRange env t c lo hi = ask env "range_by_index" (ReqRangeByIndex t c lo hi) $ \case
    RespRows rows -> Just (Just rows)
    RespNoIndex -> Just Nothing
    _ -> Nothing

-- | 发 DescribeTable
doDescribeTable :: Env -> String -> IO (Either String TableInfo)
doDescribeTable env t = ask env "describe_table" (ReqDescribeTable t) $ \case
    RespSchema info -> Just info
    _ -> Nothing

-- | 发 DropTable
doDropTable :: Env -> String -> IO (Either String ())
doDropTable env t = ask env "drop_table" (ReqDropTable t) okOnly

-- | 线上 schema 转本地列
schemaToColumns :: [SchemaColumn] -> [(String, Column)]
schemaToColumns = map toColumn

-- | 线上 schema 转本地 column
toColumn :: SchemaColumn -> (String, Column)
toColumn sc =
    ( scName sc
    , Column
        { columnType = fromMaybe CStr (parseColumnType (scType sc))
        , columnNullable = scNullable sc
        , columnDefault = scDefault sc
        , columnAutoIncrement = scAutoIncrement sc
        , columnUnique = scUnique sc
        , columnPrimaryKey = scPrimaryKey sc
        , columnCheck = scCheck sc
        }
    )

-- | 本地列转线上 schema
toWire :: (String, Column) -> SchemaColumn
toWire (n, c) =
    SchemaColumn
        { scName = n
        , scType = typeName (columnType c)
        , scNullable = columnNullable c
        , scDefault = columnDefault c
        , scAutoIncrement = columnAutoIncrement c
        , scPrimaryKey = columnPrimaryKey c
        , scUnique = columnUnique c
        , scCheck = columnCheck c
        }

instance MonadStorage IPCStorage where
    -- \| 将依赖守卫限定于本次执行环境
    withSchemaGuard dependencies action = IPCStorage $ \env ->
        runIPCStorageIn action env{envSchemaGuard = Just dependencies}

    -- \| 写前握手，旧存储不能先写入后才报错
    withWriteSchemaGuard dependencies action = IPCStorage $ \env -> case dependencies of
        [] -> pure (Left "bound write requires a target schema guard")
        (target, _) : _ -> do
            ready <- guardedRequest env dependencies (ReqValidateWrite target)
            case ready of
                RespOk -> runIPCStorageIn (withSchemaGuard dependencies action) env
                RespError err -> pure (Left err)
                _ -> pure (Left "unexpected response to validate_write")

    -- \| 建一个库
    createDatabase name = IPCStorage $ \env -> ask env "create_database" (ReqDatabase "create_database" name) okOnly

    -- \| 删一个库
    dropDatabase name = IPCStorage $ \env -> ask env "drop_database" (ReqDatabase "drop_database" name) okOnly

    -- \| 切当前库
    useDatabase name = IPCStorage $ \env -> ask env "use_database" (ReqDatabase "use_database" name) okOnly

    -- \| 列出现有库
    listDatabases = IPCStorage $ \env -> ask env "list_databases" (ReqDatabase "list_databases" "") $ \case
        RespTables names -> Just names
        _ -> Nothing

    -- \| 发 Scan
    scan t = IPCStorage (`doScan` t)

    -- \| 只取指定列
    scanColumns t cols = IPCStorage (\env -> doScanColumns env t cols)

    -- \| 并行分片数：按 RTS 能力数，封顶在 parallelShardLimit
    parallelShards = IPCStorage $ \_ -> do max 1 . min parallelShardLimit <$> getNumCapabilities

    -- \| 分片并行扫，再按分片序拼接
    scanShards width t cols
        | width <= 1 = IPCStorage (\env -> maybe (doScan env t) (doScanColumns env t) cols)
        | otherwise = IPCStorage $ \env -> do
            pool <- workerPool width
            parts <- poolRun pool [doScanShard env t cols shard width | shard <- [0 .. width - 1]]
            pure (fmap concat (sequence parts))

    -- \| 发 Insert（索引由存储层自己维护）
    insert t r = IPCStorage (\env -> doInsert env t r)

    -- \| 一批行一次请求：N 行只落一次盘
    insertMany t rs = IPCStorage (\env -> doInsertMany env t rs)

    -- \| 按 id 批量删行
    deleteKeys t ks = IPCStorage (\env -> doDeleteKeys env t ks)

    -- \| 发 ReplaceAll
    replaceAll t rs = IPCStorage $ \env -> ask env "replace_all" (ReqReplaceAll t rs) okOnly

    -- \| 按某一列的索引取一行（没有索引就回 NoIndex）
    lookupByColumn t c k = IPCStorage (\env -> doLookupByColumn env t c k)

    -- \| 范围扫描（没有索引就回 Nothing）
    scanRange t c lo hi = IPCStorage (\env -> doScanRange env t c lo hi)

    -- \| 发 CreateTable
    createTable name cols = IPCStorage $ \env -> ask env "create_table" (ReqCreateTable name (map toWire cols)) okOnly

    -- \| ALTER：整表换列定义与全部行
    replaceSchema name cols rows = IPCStorage $ \env -> ask env "replace_schema" (ReqReplaceSchema name (map toWire cols) rows) okOnly

    -- \| 发 DropTable
    dropTable name = IPCStorage (`doDropTable` name)

    -- \| 建索引
    createIndex t c = IPCStorage $ \env -> ask env "create_index" (ReqCreateIndex t c) okOnly

    -- \| 删索引
    dropIndex t c = IPCStorage $ \env -> ask env "drop_index" (ReqDropIndex t c) okOnly

    -- \| 删一列
    dropColumn t c = IPCStorage $ \env -> ask env "drop_column" (ReqDropColumn t c) okOnly

    -- \| 校验一致快照并复查提交点后发布领域
    alterDomain name validate = IPCStorage $ \env -> do
        snapshotResult <- ask env "domain_snapshot" ReqDomainSnapshot $ \case
            RespDomainSnapshot infos rows checkpoint -> Just (infos, rows, checkpoint)
            _ -> Nothing
        case snapshotResult of
            Left err -> pure (Left err)
            Right (infos, rows, checkpoint) -> case mapM (loadDomainEntry rows) infos >>= validate of
                Left err -> pure (Left err)
                Right cols -> ask env "alter_domain" (ReqAlterDomain name (map toWire cols) checkpoint) okOnly
      where
        -- \| 保留快照身份并拒绝缺失的表数据
        loadDomainEntry rows info = do
            values <- maybe (Left "incomplete domain snapshot") Right (lookup (tiTable info) rows)
            let key = tiTable info
            Right (key, Table key (schemaToColumns (tiColumns info)) values (Just (metaOf info)) (tiIdentity info) (tiSchemaVersion info))

    -- 在存储读门内获取同一版本的完整快照。
    snapshot = IPCStorage $ \env -> fmap (fmap snd) (databaseSnapshot Nothing env)

    -- \| 只问数据字典要结构，设了当前库只留它的表
    schema = IPCStorage $ \env -> do
        result <- ask env "all_catalogs" ReqAllCatalog $ \case
            RespCatalog infos -> Just infos
            _ -> Nothing
        pure (fmap (concatMap (visibleTables (envDatabase env))) result)

-- | 线上表信息转引擎侧统计
metaOf :: TableInfo -> TableMeta
metaOf info = TableMeta (tiRows info) (tiStats info) (tiHistograms info) (tiIndexes info)

-- | 没设库名就照单全收；设了库名只收这个库的表，键名去掉库名前缀
visibleTables :: Maybe String -> TableInfo -> [(String, Table)]
visibleTables current info
    | Just db <- current =
        [bare named | unqualify (Just db) (tiTable info) /= tiTable info]
    | otherwise = [bare info]
  where
    named = info{tiTable = unqualify current (tiTable info)}
    -- \| 组装成不带行的表
    bare i = (tiTable i, Table (tiTable i) (schemaToColumns (tiColumns i)) [] (Just (metaOf i)) (tiIdentity i) (tiSchemaVersion i))
