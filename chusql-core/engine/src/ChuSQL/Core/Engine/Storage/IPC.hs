{-# LANGUAGE ImportQualifiedPost #-}
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
) where

import ChuSQL.Core.Model
import ChuSQL.Core.Protocol (Account (..), Request (..), Response (..), SchemaColumn (..), TableInfo (..))
import ChuSQL.Core.Engine.Parallel (parallelShardLimit, poolRun, workerPool)
import ChuSQL.Core.Engine.Storage
import ChuSQL.Core.Engine.Storage.FFI (closeStorage, openStorage, storageRequest)
import Control.Concurrent (getNumCapabilities)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)

-- IPC 存储实现：把 MonadStorage 的操作变成发给存储库的 JSON 请求。

-- | 一条会话的存储环境：当前库名随会话走，不共享
data Env = Env
    { envDatabase :: Maybe String
    }

newtype IPCStorage a = IPCStorage
    { runIPCStorageIn :: Env -> IO a
    }

-- | 按库名造一个会话环境
envForDatabase :: Maybe String -> Env
envForDatabase = Env

-- | 进程默认环境：读全局库名，给测试与 CLI 用
defaultEnv :: IO Env
defaultEnv = Env <$> readMVar databaseRef

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

-- | 装入一条链路；一个进程只装一次
setStorageLink :: StorageLink -> IO ()
setStorageLink link = modifyMVar_ linkRef (const (pure (Just link)))

-- | 打开本地的存储动态库，得到一条链路
localStorageLink :: Maybe FilePath -> IO (Either String StorageLink)
localStorageLink path = fmap (fmap toLink) (openStorage path)
  where
    -- | 把句柄包成一条链路
    toLink handle =
        StorageLink
            { slRequest = storageRequest handle
            , slClose = closeStorage handle
            }

-- | 关掉当前链路（进程退出前调用）；没有链路就什么都不做
closeConnection :: IO ()
closeConnection = modifyMVar_ linkRef $ \current -> do
    case current of
        Nothing -> pure ()
        Just link -> slClose link
    pure Nothing

-- | 发一条请求并解码应答；链路没装就报错，不崩
sendRequest :: Request -> IO Response
sendRequest req = do
    current <- readMVar linkRef
    case current of
        Nothing -> pure (RespError "storage not configured")
        Just link -> do
            raw <- slRequest link (BL.toStrict (encode req))
            case raw of
                Left e -> pure (RespError ("storage error: " ++ e))
                Right bytes -> case eitherDecodeStrict (BS.dropWhileEnd (== 13) bytes) of
                    Left e -> pure (RespError ("decode error: " ++ e))
                    Right resp -> pure resp

-- | 原样转发一条载荷给存储库
sendRawRequest :: BS.ByteString -> IO (Either String BS.ByteString)
sendRawRequest payload = do
    current <- readMVar linkRef
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
    resp <- sendRequest (stampDatabase (envDatabase env) req)
    pure $ case resp of
        RespError e -> Left e
        other -> case recognize other of
            Just a -> Right a
            Nothing -> Left ("unexpected response to " ++ what)

-- | 只认"成了"（多数写操作都这样）
okOnly :: Response -> Maybe ()
okOnly RespOk = Just ()
okOnly _ = Nothing

-- | 发 Scan
doScan :: Env -> String -> IO (Either String [Row])
doScan env t = ask env "scan" (ReqScan t) $ \resp -> case resp of
    RespRows rows -> Just rows
    _ -> Nothing

-- | 发带投影的 Scan
doScanColumns :: Env -> String -> [String] -> IO (Either String [Row])
doScanColumns env t cols = ask env "scan" (ReqScanColumns t cols) $ \resp -> case resp of
    RespRows rows -> Just [[(c, v) | c <- cols, Just v <- [lookup c row]] | row <- rows]
    _ -> Nothing

-- | 发分片 Scan：只取第 shard 片
doScanShard :: Env -> String -> Maybe [String] -> Int -> Int -> IO (Either String [Row])
doScanShard env t cols shard shards = ask env "scan_shard" (ReqScanShard t cols shard shards) $ \resp -> case resp of
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

-- | 发 ReplaceAll
doReplaceAll :: Env -> String -> [Row] -> IO (Either String ())
doReplaceAll env t rs = ask env "replace_all" (ReqReplaceAll t rs) okOnly

-- | 发 ListTables
doListTables :: Env -> IO (Either String [String])
doListTables env = ask env "list_tables" ReqListTables $ \resp -> case resp of
    RespTables ts -> Just ts
    _ -> Nothing

-- | 一次拿全库 schema
doListCatalog :: Env -> IO (Either String [TableInfo])
doListCatalog env = ask env "list_catalog" ReqListCatalog $ \resp -> case resp of
    RespCatalog xs -> Just xs
    _ -> Nothing

-- | 按某一列的索引取一行（键可以是整数或字符串）
doLookupByColumn :: Env -> String -> String -> Value -> IO (Either String IndexResult)
doLookupByColumn env t c k = ask env "lookup_by_index" (ReqLookupByColumn t c k) $ \resp -> case resp of
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
doScanRange env t c lo hi = ask env "range_by_index" (ReqRangeByIndex t c lo hi) $ \resp -> case resp of
    RespRows rows -> Just (Just rows)
    RespNoIndex -> Just Nothing
    _ -> Nothing

-- | 发 DescribeTable
doDescribeTable :: Env -> String -> IO (Either String TableInfo)
doDescribeTable env t = ask env "describe_table" (ReqDescribeTable t) $ \resp -> case resp of
    RespSchema info -> Just info
    _ -> Nothing

-- | 给某一列建索引
doCreateIndex :: Env -> String -> String -> IO (Either String ())
doCreateIndex env t c = ask env "create_index" (ReqCreateIndex t c) okOnly

-- | 去掉某一列的索引
doDropIndex :: Env -> String -> String -> IO (Either String ())
doDropIndex env t c = ask env "drop_index" (ReqDropIndex t c) okOnly

-- | 删一列（列定义、这一列上的索引、每行里的那一格一起没）
doDropColumn :: Env -> String -> String -> IO (Either String ())
doDropColumn env t c = ask env "drop_column" (ReqDropColumn t c) okOnly

-- | 发 ReplaceSchema（ALTER 用）
doReplaceSchema :: Env -> String -> [SchemaColumn] -> [Row] -> IO (Either String ())
doReplaceSchema env t cols rs = ask env "replace_schema" (ReqReplaceSchema t cols rs) okOnly

-- | 发 CreateTable
doCreateTable :: Env -> String -> [SchemaColumn] -> IO (Either String ())
doCreateTable env t cols = ask env "create_table" (ReqCreateTable t cols) okOnly

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
    -- \| 建一个库
    createDatabase name = IPCStorage $ \env -> ask env "create_database" (ReqDatabase "create_database" name) okOnly
    -- \| 删一个库
    dropDatabase name = IPCStorage $ \env -> ask env "drop_database" (ReqDatabase "drop_database" name) okOnly
    -- \| 切当前库
    useDatabase name = IPCStorage $ \env -> ask env "use_database" (ReqDatabase "use_database" name) okOnly
    -- \| 列出现有库
    listDatabases = IPCStorage $ \env -> ask env "list_databases" (ReqDatabase "list_databases" "") $ \resp -> case resp of
        RespTables names -> Just names
        _ -> Nothing
    -- \| 发 Scan
    scan t = IPCStorage (\env -> doScan env t)
    -- \| 只取指定列
    scanColumns t cols = IPCStorage (\env -> doScanColumns env t cols)

    -- \| 并行分片数：按 RTS 能力数，封顶在 parallelShardLimit
    parallelShards = IPCStorage $ \_ -> do
        caps <- getNumCapabilities
        pure (max 1 (min parallelShardLimit caps))

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
    replaceAll t rs = IPCStorage (\env -> doReplaceAll env t rs)

    -- \| 按某一列的索引取一行（没有索引就回 NoIndex）
    lookupByColumn t c k = IPCStorage (\env -> doLookupByColumn env t c k)

    -- \| 范围扫描（没有索引就回 Nothing）
    scanRange t c lo hi = IPCStorage (\env -> doScanRange env t c lo hi)

    -- \| 发 CreateTable
    createTable name cols = IPCStorage (\env -> doCreateTable env name (map toWire cols))

    -- \| ALTER：整表换列定义与全部行
    replaceSchema name cols rows = IPCStorage (\env -> doReplaceSchema env name (map toWire cols) rows)

    -- \| 发 DropTable
    dropTable name = IPCStorage (\env -> doDropTable env name)

    -- \| 建索引
    createIndex t c = IPCStorage (\env -> doCreateIndex env t c)

    -- \| 删索引
    dropIndex t c = IPCStorage (\env -> doDropIndex env t c)

    -- \| 删一列
    dropColumn t c = IPCStorage (\env -> doDropColumn env t c)

    -- \| 列表 + 逐表扫描拼库
    snapshot = IPCStorage $ \env -> do
        result <- doListCatalog env
        case result of
            Left _ -> pure []
            Right xs -> mapM (loadEntry env) xs
      where
        loadEntry env info = do
            rows <- doScan env (tiTable info)
            pure
                ( tiTable info
                , Table (tiTable info) (schemaToColumns (tiColumns info)) (either (const []) id rows) (Just (metaOf info))
                )

    -- \| 只问数据字典要结构，设了当前库只留它的表
    schema = IPCStorage $ \env -> do
        result <- ask env "all_catalogs" ReqAllCatalog $ \resp -> case resp of
            RespCatalog infos -> Just infos
            _ -> Nothing
        pure $ case result of
            Left _ -> []
            Right xs -> concatMap (visibleTables (envDatabase env)) xs

-- | 线上表信息转引擎侧统计
metaOf :: TableInfo -> TableMeta
metaOf info = TableMeta (tiRows info) (tiStats info) (tiHistograms info) (tiIndexes info)

-- | 没设库名就照单全收；设了库名只收这个库的表，键名去掉库名前缀
visibleTables :: Maybe String -> TableInfo -> [(String, Table)]
visibleTables current info
    | Just db <- current =
        if unqualify (Just db) (tiTable info) /= tiTable info
            then [bare named]
            else []
    | otherwise = [bare info]
  where
    named = info {tiTable = unqualify current (tiTable info)}
    -- | 组装成不带行的表
    bare i = (tiTable i, Table (tiTable i) (schemaToColumns (tiColumns i)) [] (Just (metaOf i)))
