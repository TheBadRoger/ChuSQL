{-# LANGUAGE CPP #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Storage.IPC (
    IPCStorage (..),
    Request (..),
    Response (..),
    Account (..),
    SchemaColumn (..),
    TableInfo (..),
    sendRequest,
    closeConnection,
    defaultPipeName,
    setPipeName,
    getPipeName,
    doListTables,
    doLookupByColumn,
    doInsert,
    doInsertMany,
    doDeleteKeys,
    doDescribeTable,
    doDropTable,
    doListCatalog,
) where

import ChuSQL.Model
import ChuSQL.Storage
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (IOException, try)
#if !defined(mingw32_HOST_OS)
import Control.Exception (onException)
#endif
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict, encode, object, withObject, (.:), (.:?), (.!=), (.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (fromMaybe)
import Data.Scientific (floatingOrInteger, fromFloatDigits)
import Data.Text qualified as T
import System.IO
import System.IO.Unsafe (unsafePerformIO)
#if !defined(mingw32_HOST_OS)
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix), SocketType (Stream), close, connect, defaultProtocol, socket, socketToHandle)
import System.Environment (lookupEnv)
#endif

-- IPC 存储实现：每个存储方法翻成管道上的一条 JSON 请求。
-- 端点：Windows 是具名管道 \\.\pipe\<name>，Unix 是文件系统套接字 <dir>/<name>.sock。

#if !defined(mingw32_HOST_OS)
-- | Unix 套接字路径的最大长度（Linux 108 含结尾 NUL，BSD 104，这里留余量）
maxSocketPathLength :: Int
maxSocketPathLength = 100
#endif

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

{-# NOINLINE connectionRef #-}
connectionRef :: MVar (Maybe (FilePath, Handle))
connectionRef = unsafePerformIO (newMVar Nothing)

-- | 打开端点连接：Windows 把管道当文件打开，Unix 连一个 AF_UNIX 套接字
openPipe :: FilePath -> IO Handle
#if defined(mingw32_HOST_OS)
openPipe path = do
    h <- openFile path ReadWriteMode
    hSetBuffering h LineBuffering
    hSetNewlineMode h noNewlineTranslation
    pure h
#else
openPipe path = do
    if length path > maxSocketPathLength
        then ioError (userError ("socket path too long (" ++ show (length path) ++ " bytes): " ++ path))
        else pure ()
    sock <- socket AF_UNIX Stream defaultProtocol
    connect sock (SockAddrUnix path) `onException` close sock
    h <- socketToHandle sock ReadWriteMode
    hSetBuffering h LineBuffering
    hSetNewlineMode h noNewlineTranslation
    pure h

-- | Unix 套接字目录：$XDG_RUNTIME_DIR → $TMPDIR → /tmp（与 Rust 侧同一规则）
socketDir :: IO String
socketDir = do
    runtime <- fmap nonEmpty (lookupEnv "XDG_RUNTIME_DIR")
    tmp <- fmap nonEmpty (lookupEnv "TMPDIR")
    pure (fromMaybe "/tmp" (runtime <> tmp))
  where
    nonEmpty v = case v of
        Just s | not (null s) -> Just s
        _ -> Nothing
#endif

-- | 关掉缓存里的句柄（没有就什么都不做）
closeCached :: Maybe (FilePath, Handle) -> IO ()
closeCached Nothing = pure ()
closeCached (Just (_, h)) = hClose h

-- | 拿一个可用句柄；路径没变就复用，变了就换。
getConnection :: FilePath -> IO Handle
getConnection path = modifyMVar connectionRef $ \cached -> case cached of
    Just (p, h)
        | p == path -> pure (Just (p, h), h)
    _ -> do
        _ <- try (closeCached cached) :: IO (Either IOException ())
        h <- openPipe path
        pure (Just (path, h), h)

-- | 丢掉缓存的连接（出错后调用，下次会重连）
dropConnection :: IO ()
dropConnection = modifyMVar_ connectionRef $ \cached -> do
    _ <- try (closeCached cached) :: IO (Either IOException ())
    pure Nothing

-- | 关闭并清空连接。可选，进程退出前调用。
closeConnection :: IO ()
closeConnection = dropConnection

-- | 没配置管名时用的端点，与 chusql.toml 的 [server] pipe_name 默认值一致
defaultPipeName :: String
defaultPipeName = "chusql-joint"

{-# NOINLINE pipeNameRef #-}
pipeNameRef :: MVar String
pipeNameRef = unsafePerformIO (newMVar defaultPipeName)

-- | 设定本进程要连的端点；配置从哪来由调用方决定（本层不读环境变量）
setPipeName :: String -> IO ()
setPipeName name = modifyMVar_ pipeNameRef (\_ -> pure name)

-- | 当前端点
getPipeName :: IO String
getPipeName = readMVar pipeNameRef

-- | 端点的完整路径
pipePath :: IO String
#if defined(mingw32_HOST_OS)
pipePath = do
    name <- getPipeName
    pure ("\\\\.\\pipe\\" ++ name)
#else
pipePath = do
    name <- getPipeName
    dir <- socketDir
    pure (dir ++ "/" ++ name ++ ".sock")
#endif

-- | 发一条请求，管道不通就重连
sendRequest :: Request -> IO Response
sendRequest req = do
    path <- pipePath
    result <- try (roundTrip req path) :: IO (Either IOException Response)
    case result of
        Left e -> do
            dropConnection
            pure (RespError ("pipe error: " ++ show e))
        Right r -> pure r

-- | 一条请求-响应往返（复用缓存里的句柄）
roundTrip :: Request -> FilePath -> IO Response
roundTrip req path = do
    h <- getConnection path
    BL.hPutStr h (encode req)
    BSC.hPutStr h "\n"
    hFlush h
    line <- BSC.hGetLine h
    let cleaned = BSC.dropWhileEnd (== '\r') line
    case eitherDecodeStrict cleaned of
        Left err -> pure (RespError ("decode: " ++ err))
        Right r -> pure r

data SchemaColumn = SchemaColumn
    { scName :: String
    , scType :: String
    , scNullable :: Bool
    , scDefault :: Maybe Value
    , scAutoIncrement :: Bool
    , scPrimaryKey :: Bool
    , scUnique :: Bool
    , scCheck :: Maybe String
    }
    deriving (Show, Eq)

instance ToJSON SchemaColumn where
    toJSON sc =
        object
            [ "name" .= scName sc
            , "ty" .= scType sc
            , "nullable" .= scNullable sc
            , "default" .= fmap valueToJSON (scDefault sc)
            , "auto_increment" .= scAutoIncrement sc
            , "primary_key" .= scPrimaryKey sc
            , "unique" .= scUnique sc
            , "check" .= scCheck sc
            ]

instance FromJSON SchemaColumn where
    parseJSON = withObject "SchemaColumn" $ \o -> do
        n <- o .: "name"
        t <- o .: "ty"
        nullable <- o .:? "nullable" .!= True
        defJson <- o .:? "default"
        def <- mapM valueFromJSON defJson
        auto <- o .:? "auto_increment" .!= False
        pk <- o .:? "primary_key" .!= False
        uniq <- o .:? "unique" .!= False
        chk <- o .:? "check"
        pure (SchemaColumn n t nullable def auto pk uniq chk)

data Request
    = ReqPing
    | ReqDatabase String String
    | ReqAllCatalog
    | ReqInDatabase String Request
    | ReqAccountsList
    | ReqAccountCreate T.Text T.Text
    | ReqAccountReset T.Text T.Text
    | ReqAccountLogin T.Text (Maybe T.Text)
    | ReqAccountDrop T.Text
    | ReqScan String
    | ReqScanColumns String [String]
    | ReqInsert String Row
    | ReqInsertBatch String [Row]
    | ReqDeleteKeys String [Int]
    | ReqReplaceAll String [Row]
    | ReqListTables
    | ReqLookupByColumn String String Value
    | ReqRangeByIndex String String (Maybe (Value, Bool)) (Maybe (Value, Bool))
    | ReqDescribeTable String
    | ReqCreateTable String [SchemaColumn]
    | ReqDropTable String
    | ReqCreateIndex String String
    | ReqDropIndex String String
    | ReqDropColumn String String
    | ReqReplaceSchema String [SchemaColumn] [Row]
    | ReqListCatalog

instance ToJSON Request where
    toJSON (ReqDatabase method name) = object ["method" .= method, "database" .= name]
    toJSON ReqAllCatalog = object ["method" .= ("all_catalogs" :: T.Text)]
    toJSON (ReqInDatabase name req) = case toJSON req of
        A.Object fields -> A.Object (KM.insert "database" (A.String (T.pack name)) fields)
        other -> other
    toJSON ReqAccountsList = object ["method" .= ("accounts_list" :: T.Text)]
    toJSON (ReqAccountCreate u h) = object ["method" .= ("account_create" :: T.Text), "user" .= u, "password_hash" .= h]
    toJSON (ReqAccountReset u h) = object ["method" .= ("account_reset" :: T.Text), "user" .= u, "password_hash" .= h]
    toJSON (ReqAccountLogin u at) = object ["method" .= ("account_login" :: T.Text), "user" .= u, "at" .= at]
    toJSON (ReqAccountDrop u) = object ["method" .= ("account_drop" :: T.Text), "user" .= u]
    toJSON ReqPing = object ["method" .= ("ping" :: T.Text)]
    toJSON (ReqScan t) = object ["method" .= ("scan" :: T.Text), "table" .= t]
    toJSON (ReqScanColumns t cols) = object ["method" .= ("scan" :: T.Text), "table" .= t, "columns" .= cols]
    toJSON (ReqInsert t r) =
        object
            [ "method" .= ("insert" :: T.Text)
            , "table" .= t
            , "row" .= rowToJSON r
            ]
    toJSON (ReqInsertBatch t rs) =
        object
            [ "method" .= ("insert_batch" :: T.Text)
            , "table" .= t
            , "rows" .= map rowToJSON rs
            ]
    toJSON (ReqDeleteKeys t ks) =
        object
            [ "method" .= ("delete_keys" :: T.Text)
            , "table" .= t
            , "keys" .= ks
            ]
    toJSON (ReqReplaceAll t rs) =
        object
            [ "method" .= ("replace_all" :: T.Text)
            , "table" .= t
            , "rows" .= map rowToJSON rs
            ]
    toJSON ReqListTables = object ["method" .= ("list_tables" :: T.Text)]
    toJSON (ReqLookupByColumn t c k) =
        object
            [ "method" .= ("lookup_by_index" :: T.Text)
            , "table" .= t
            , "column" .= c
            , "key" .= valueToJSON k
            ]
    toJSON (ReqRangeByIndex t c lo hi) =
        object
            [ "method" .= ("range_by_index" :: T.Text)
            , "table" .= t
            , "column" .= c
            , "lo" .= fmap (valueToJSON . fst) lo
            , "lo_inclusive" .= maybe True snd lo
            , "hi" .= fmap (valueToJSON . fst) hi
            , "hi_inclusive" .= maybe True snd hi
            ]
    toJSON (ReqDescribeTable t) =
        object
            [ "method" .= ("describe_table" :: T.Text)
            , "table" .= t
            ]
    toJSON (ReqCreateTable t cols) =
        object
            [ "method" .= ("create_table" :: T.Text)
            , "table" .= t
            , "columns" .= cols
            ]
    toJSON (ReqDropTable t) =
        object ["method" .= ("drop_table" :: T.Text), "table" .= t]
    toJSON (ReqCreateIndex t c) =
        object
            [ "method" .= ("create_index" :: T.Text)
            , "table" .= t
            , "column" .= c
            ]
    toJSON (ReqDropIndex t c) =
        object
            [ "method" .= ("drop_index" :: T.Text)
            , "table" .= t
            , "column" .= c
            ]
    toJSON (ReqDropColumn t c) =
        object
            [ "method" .= ("drop_column" :: T.Text)
            , "table" .= t
            , "column" .= c
            ]
    toJSON (ReqReplaceSchema t cols rs) =
        object
            [ "method" .= ("replace_schema" :: T.Text)
            , "table" .= t
            , "columns" .= cols
            , "rows" .= map rowToJSON rs
            ]
    toJSON ReqListCatalog = object ["method" .= ("list_catalog" :: T.Text)]

data TableInfo = TableInfo
    { tiTable :: String
    , tiColumns :: [SchemaColumn]
    , tiRows :: Int
    , tiIndexes :: [String]
    , tiStats :: [(String, Int, Bool)]
    }
    deriving (Show, Eq)

data Account = Account
    { accountId :: Integer
    , accountUser :: T.Text
    , accountHash :: T.Text
    , accountRevision :: Integer
    , accountRegisteredAt :: T.Text
    , accountLastLoginAt :: Maybe T.Text
    } deriving (Eq)

instance Show Account where
    show a = "Account " ++ show (accountUser a) ++ " revision=" ++ show (accountRevision a)

instance FromJSON Account where
    parseJSON = withObject "Account" $ \o -> Account <$> o .: "id" <*> o .: "user"
        <*> o .: "password_hash" <*> o .: "revision"
        <*> o .:? "registered_at" .!= "" <*> o .:? "last_login_at"

instance ToJSON Account where
    toJSON a = object ["id" .= accountId a, "user" .= accountUser a,
        "password_hash" .= accountHash a, "revision" .= accountRevision a,
        "registered_at" .= accountRegisteredAt a, "last_login_at" .= accountLastLoginAt a]

data Response
    = RespPong
    | RespAccounts [Account]
    | RespRows [Row]
    | RespTables [String]
    | RespSchema TableInfo
    | RespNoIndex
    | RespOk
    | RespError String
    | RespCatalog [TableInfo]

instance FromJSON Response where
    parseJSON = withObject "Response" $ \o -> do
        status <- o .: "status"
        case status :: T.Text of
            "accounts" -> RespAccounts <$> o .: "accounts"
            "pong" -> pure RespPong
            "ok" -> pure RespOk
            "no_index" -> pure RespNoIndex
            "rows" -> RespRows <$> (o .: "rows" >>= mapM rowFromJSON)
            "tables" -> RespTables <$> o .: "tables"
            "error" -> RespError <$> o .: "message"
            "schema" -> RespSchema <$> parseTable (A.Object o)
            "catalog" -> do
                xs <- o .: "schemas" :: Parser [A.Value]
                RespCatalog <$> mapM parseTable xs
            other -> fail ("unknown status: " ++ T.unpack other)
      where
        parseTable :: A.Value -> Parser TableInfo
        parseTable = withObject "TableInfo" $ \o -> do
            t <- o .: "table"
            cols <- o .: "columns"
            cnt <- o .: "row_count"
            idx <- o .:? "indexes" .!= []
            sts <- o .:? "stats" .!= []
            idxCols <- mapM (\v -> withObject "IndexWire" (.: "column") v) (idx :: [A.Value])
            stats <- mapM parseStat (sts :: [A.Value])
            pure (TableInfo t cols cnt idxCols stats)

        parseStat :: A.Value -> Parser (String, Int, Bool)
        parseStat = withObject "ColumnStat" $ \o -> do
            name <- o .: "name"
            distinct <- o .: "distinct"
            capped <- o .:? "capped" .!= False
            pure (name, distinct, capped)

-- | 值编码成 JSON
valueToJSON :: Value -> A.Value
valueToJSON VNull = A.Null
valueToJSON (VInt n) = A.Number (fromIntegral n)
valueToJSON (VFloat d) = A.Number (fromFloatDigits d)
valueToJSON (VStr s) = A.String (T.pack s)
valueToJSON (VBool b) = A.Bool b

-- | JSON 解回值
valueFromJSON :: A.Value -> Parser Value
valueFromJSON A.Null = pure VNull
valueFromJSON (A.Number n) =
    case floatingOrInteger n :: Either Double Integer of
        Right i -> pure (VInt (fromIntegral i))
        Left d -> pure (VFloat d)
valueFromJSON (A.String s) = pure (VStr (T.unpack s))
valueFromJSON (A.Bool b) = pure (VBool b)
valueFromJSON _ = fail "unsupported value type"

-- | 一行编码成 JSON
rowToJSON :: Row -> A.Value
rowToJSON r =
    A.Object (KM.fromList [(K.fromString k, valueToJSON v) | (k, v) <- r])

-- | JSON 解回一行
rowFromJSON :: A.Value -> Parser Row
rowFromJSON (A.Object o) = mapM toPair (KM.toList o)
  where
    toPair (k, v) = do
        val <- valueFromJSON v
        pure (K.toString k, val)
rowFromJSON _ = fail "row must be a JSON object"

-- | 发请求并把响应翻成 Either
ask :: String -> Request -> (Response -> Maybe a) -> IO (Either String a)
ask what req recognize = do
    resp <- sendRequest req
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
    createDatabase name = IPCStorage (ask "create_database" (ReqDatabase "create_database" name) okOnly)
    dropDatabase name = IPCStorage (ask "drop_database" (ReqDatabase "drop_database" name) okOnly)
    useDatabase name = IPCStorage (ask "use_database" (ReqDatabase "use_database" name) okOnly)
    listDatabases = IPCStorage $ ask "list_databases" (ReqDatabase "list_databases" "") $ \resp -> case resp of
        RespTables names -> Just names
        _ -> Nothing
    -- \| 发 Scan
    scan t = IPCStorage (doScan t)
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

    -- \| 只问数据字典要结构，一行数据都不拉过来
    schema = IPCStorage $ do
        result <- ask "all_catalogs" ReqAllCatalog $ \resp -> case resp of
            RespCatalog infos -> Just infos
            _ -> Nothing
        pure $ case result of
            Left _ -> []
            Right xs ->
                [ (tiTable i, Table (tiTable i) (schemaToColumns (tiColumns i)) [])
                | i <- xs
                ]
