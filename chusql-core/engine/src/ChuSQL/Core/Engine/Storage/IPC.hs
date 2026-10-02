{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Core.Engine.Storage.IPC (
    IPCStorage (..),
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
import ChuSQL.Core.Engine.Storage
import ChuSQL.Core.Engine.Storage.FFI (closeStorage, openStorage, storageRequest)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.Aeson (eitherDecodeStrict, encode)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import System.IO.Unsafe (unsafePerformIO)

-- IPC 存储实现：把 MonadStorage 的操作变成发给存储库的 JSON 请求。

newtype IPCStorage a = IPCStorage
    { runIPCStorage :: IO a
    }

instance Functor IPCStorage where
    fmap f (IPCStorage m) = IPCStorage (fmap f m)

instance Applicative IPCStorage where
    pure = IPCStorage . pure
    IPCStorage mf <*> IPCStorage ma = IPCStorage (mf <*> ma)

instance Monad IPCStorage where
    IPCStorage m >>= k = IPCStorage $ do
        a <- m
        runIPCStorage (k a)

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
ask :: String -> Request -> (Response -> Maybe a) -> IO (Either String a)
ask what req recognize = do
    current <- getDatabaseName
    resp <- sendRequest (stampDatabase current req)
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
doScan :: String -> IO (Either String [Row])
doScan t = ask "scan" (ReqScan t) $ \resp -> case resp of
    RespRows rows -> Just rows
    _ -> Nothing

-- | 发 Insert（索引由存储层维护）
doInsert :: String -> Row -> IO (Either String ())
doInsert t r = ask "insert" (ReqInsert t r) okOnly

-- | 发 InsertBatch：一批行一次请求
doInsertMany :: String -> [Row] -> IO (Either String ())
doInsertMany t rs = ask "insert_batch" (ReqInsertBatch t rs) okOnly

-- | 发 DeleteKeys
doDeleteKeys :: String -> [Int] -> IO (Either String ())
doDeleteKeys t ks = ask "delete_keys" (ReqDeleteKeys t ks) okOnly

-- | 发 ReplaceAll
doReplaceAll :: String -> [Row] -> IO (Either String ())
doReplaceAll t rs = ask "replace_all" (ReqReplaceAll t rs) okOnly

-- | 发 ListTables
doListTables :: IO (Either String [String])
doListTables = ask "list_tables" ReqListTables $ \resp -> case resp of
    RespTables ts -> Just ts
    _ -> Nothing

-- | 一次拿全库 schema
doListCatalog :: IO (Either String [TableInfo])
doListCatalog = ask "list_catalog" ReqListCatalog $ \resp -> case resp of
    RespCatalog xs -> Just xs
    _ -> Nothing

-- | 按某一列的索引取一行（键可以是整数或字符串）
doLookupByColumn :: String -> String -> Value -> IO (Either String IndexResult)
doLookupByColumn t c k = ask "lookup_by_index" (ReqLookupByColumn t c k) $ \resp -> case resp of
    RespRows rows -> Just (IndexRows rows)
    RespNoIndex -> Just NoIndex
    _ -> Nothing

-- | 范围扫描：没有可用索引就回 Nothing，让上层退回扫描
doScanRange ::
    String ->
    String ->
    Maybe (Value, Bool) ->
    Maybe (Value, Bool) ->
    IO (Either String (Maybe [Row]))
doScanRange t c lo hi = ask "range_by_index" (ReqRangeByIndex t c lo hi) $ \resp -> case resp of
    RespRows rows -> Just (Just rows)
    RespNoIndex -> Just Nothing
    _ -> Nothing

-- | 发 DescribeTable
doDescribeTable :: String -> IO (Either String TableInfo)
doDescribeTable t = ask "describe_table" (ReqDescribeTable t) $ \resp -> case resp of
    RespSchema info -> Just info
    _ -> Nothing

-- | 给某一列建索引
doCreateIndex :: String -> String -> IO (Either String ())
doCreateIndex t c = ask "create_index" (ReqCreateIndex t c) okOnly

-- | 去掉某一列的索引
doDropIndex :: String -> String -> IO (Either String ())
doDropIndex t c = ask "drop_index" (ReqDropIndex t c) okOnly

-- | 删一列（列定义、这一列上的索引、每行里的那一格一起没）
doDropColumn :: String -> String -> IO (Either String ())
doDropColumn t c = ask "drop_column" (ReqDropColumn t c) okOnly

-- | 发 ReplaceSchema（ALTER 用）
doReplaceSchema :: String -> [SchemaColumn] -> [Row] -> IO (Either String ())
doReplaceSchema t cols rs = ask "replace_schema" (ReqReplaceSchema t cols rs) okOnly

-- | 发 CreateTable
doCreateTable :: String -> [SchemaColumn] -> IO (Either String ())
doCreateTable t cols = ask "create_table" (ReqCreateTable t cols) okOnly

-- | 发 DropTable
doDropTable :: String -> IO (Either String ())
doDropTable t = ask "drop_table" (ReqDropTable t) okOnly

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
    createDatabase name = IPCStorage (ask "create_database" (ReqDatabase "create_database" name) okOnly)
    -- \| 删一个库
    dropDatabase name = IPCStorage (ask "drop_database" (ReqDatabase "drop_database" name) okOnly)
    -- \| 切当前库
    useDatabase name = IPCStorage (ask "use_database" (ReqDatabase "use_database" name) okOnly)
    -- \| 列出现有库
    listDatabases = IPCStorage $ ask "list_databases" (ReqDatabase "list_databases" "") $ \resp -> case resp of
        RespTables names -> Just names
        _ -> Nothing
    -- \| 发 Scan
    scan t = IPCStorage (doScan t)
    -- \| 只取指定列
    scanColumns t cols = IPCStorage $ ask "scan" (ReqScanColumns t cols) $ \resp -> case resp of
        RespRows rows -> Just [[(c, v) | c <- cols, Just v <- [lookup c row]] | row <- rows]
        _ -> Nothing

    -- \| 发 Insert（索引由存储层自己维护）
    insert t r = IPCStorage (doInsert t r)

    -- \| 一批行一次请求：N 行只落一次盘
    insertMany t rs = IPCStorage (doInsertMany t rs)

    -- \| 按 id 批量删行
    deleteKeys t ks = IPCStorage (doDeleteKeys t ks)

    -- \| 发 ReplaceAll
    replaceAll t rs = IPCStorage (doReplaceAll t rs)

    -- \| 按某一列的索引取一行（没有索引就回 NoIndex）
    lookupByColumn t c k = IPCStorage (doLookupByColumn t c k)

    -- \| 范围扫描（没有索引就回 Nothing）
    scanRange t c lo hi = IPCStorage (doScanRange t c lo hi)

    -- \| 发 CreateTable
    createTable name cols = IPCStorage (doCreateTable name (map toWire cols))

    -- \| ALTER：整表换列定义与全部行
    replaceSchema name cols rows = IPCStorage (doReplaceSchema name (map toWire cols) rows)

    -- \| 发 DropTable
    dropTable name = IPCStorage (doDropTable name)

    -- \| 建索引
    createIndex t c = IPCStorage (doCreateIndex t c)

    -- \| 删索引
    dropIndex t c = IPCStorage (doDropIndex t c)

    -- \| 删一列
    dropColumn t c = IPCStorage (doDropColumn t c)

    -- \| 列表 + 逐表扫描拼库
    snapshot = IPCStorage $ do
        result <- doListCatalog
        case result of
            Left _ -> pure []
            Right xs -> mapM loadEntry xs
      where
        loadEntry info = do
            rows <- doScan (tiTable info)
            pure
                ( tiTable info
                , Table (tiTable info) (schemaToColumns (tiColumns info)) (either (const []) id rows)
                )

    -- \| 只问数据字典要结构，设了当前库只留它的表
    schema = IPCStorage $ do
        current <- getDatabaseName
        result <- ask "all_catalogs" ReqAllCatalog $ \resp -> case resp of
            RespCatalog infos -> Just infos
            _ -> Nothing
        pure $ case result of
            Left _ -> []
            Right xs -> concatMap (visibleTables current) xs

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
    bare i = (tiTable i, Table (tiTable i) (schemaToColumns (tiColumns i)) [])
