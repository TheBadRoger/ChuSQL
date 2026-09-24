{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Storage.IPC (
    IPCStorage (..),
    Request (..),
    Response (..),
    SchemaColumn (..),
    sendRequest,
    closeConnection,
    doListTables,
    doLookupByKey,
    doInsertWith,
    doDescribeTable,
    doDropTable,
    doListCatalog,
) where

import ChuSQL.Model
import ChuSQL.Storage
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict, encode, object, withObject, (.:), (.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.Scientific (floatingOrInteger)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.IO
import System.IO.Unsafe (unsafePerformIO)

-- IPC 存储实现：每个存储方法翻成管道上的一条 JSON 请求。

-- * 存储实现

-- | 命名管道上的存储
newtype IPCStorage a = IPCStorage
    { runIPCStorage :: IO a
    }

-- | Functor：转发给 IO
instance Functor IPCStorage where
    fmap f (IPCStorage m) = IPCStorage (fmap f m)

-- | Applicative：转发给 IO
instance Applicative IPCStorage where
    pure = IPCStorage . pure
    IPCStorage mf <*> IPCStorage ma = IPCStorage (mf <*> ma)

-- | Monad：顺序执行
instance Monad IPCStorage where
    IPCStorage m >>= k = IPCStorage $ do
        a <- m
        runIPCStorage (k a)

-- * 全局连接缓存

-- | 全局句柄缓存：按管道路径缓存，路径变了就换连接。
{-# NOINLINE connectionRef #-}
connectionRef :: MVar (Maybe (FilePath, Handle))
connectionRef = unsafePerformIO (newMVar Nothing)

-- | 打开管道
openPipe :: FilePath -> IO Handle
openPipe path = do
    h <- openFile path ReadWriteMode
    hSetBuffering h LineBuffering
    hSetNewlineMode h noNewlineTranslation
    pure h

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

-- * 管道

-- | 管道路径
pipePath :: IO String
pipePath = do
    name <- maybe "chusql-storage" id <$> lookupEnv "CHUSQL_PIPE"
    pure ("\\\\.\\pipe\\" ++ name)

-- | 发一条请求读一条响应；管道不通就回 RespError，并丢掉缓存等下次重连。
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

-- * 编解码

-- | 一列的 schema
data SchemaColumn = SchemaColumn
    { scName :: String
    , scType :: String
    }
    deriving (Show, Eq)

-- | SchemaColumn 编码
instance ToJSON SchemaColumn where
    toJSON (SchemaColumn n t) = object ["name" .= n, "ty" .= t]

-- | SchemaColumn 解码
instance FromJSON SchemaColumn where
    parseJSON = withObject "SchemaColumn" $ \o -> do
        n <- o .: "name"
        t <- o .: "ty"
        pure (SchemaColumn n t)

-- * 请求

-- | 请求类型
data Request
    = ReqPing
    | ReqScan String
    | ReqInsert String Row (Maybe Int)
    | ReqReplaceAll String [Row]
    | ReqListTables
    | ReqLookupByKey String Int
    | ReqDescribeTable String
    | ReqCreateTable String [SchemaColumn]
    | ReqDropTable String
    | ReqListCatalog

-- | 请求编码
instance ToJSON Request where
    toJSON ReqPing = object ["method" .= ("ping" :: T.Text)]
    toJSON (ReqScan t) = object ["method" .= ("scan" :: T.Text), "table" .= t]
    toJSON (ReqInsert t r mk) =
        object $
            [ "method" .= ("insert" :: T.Text)
            , "table" .= t
            , "row" .= rowToJSON r
            ]
                ++ case mk of
                    Nothing -> []
                    Just k -> ["key" .= k]
    toJSON (ReqReplaceAll t rs) =
        object
            [ "method" .= ("replace_all" :: T.Text)
            , "table" .= t
            , "rows" .= map rowToJSON rs
            ]
    toJSON ReqListTables = object ["method" .= ("list_tables" :: T.Text)]
    toJSON (ReqLookupByKey t k) =
        object
            [ "method" .= ("lookup_by_index" :: T.Text)
            , "table" .= t
            , "key" .= k
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
    toJSON ReqListCatalog = object ["method" .= ("list_catalog" :: T.Text)]

-- | 响应类型
data Response
    = RespPong
    | RespRows [Row]
    | RespTables [String]
    | RespSchema [SchemaColumn] Int
    | RespOk
    | RespError String
    | RespCatalog [(String, [SchemaColumn], Int)]

-- | 响应解码
instance FromJSON Response where
    parseJSON = withObject "Response" $ \o -> do
        status <- o .: "status"
        case status :: T.Text of
            "pong" -> pure RespPong
            "ok" -> pure RespOk
            "rows" -> RespRows <$> (o .: "rows" >>= mapM rowFromJSON)
            "tables" -> RespTables <$> o .: "tables"
            "error" -> RespError <$> o .: "message"
            "schema" -> do
                cols <- o .: "columns"
                cnt <- o .: "row_count"
                pure (RespSchema cols cnt)
            "catalog" -> do
                xs <- o .: "schemas"
                parsed <- mapM parseTableSchema xs
                pure (RespCatalog parsed)
            other -> fail ("unknown status: " ++ T.unpack other)
      where
        -- \| 解析一条 list_catalog 记录。
        parseTableSchema = withObject "TableSchema" $ \o -> do
            t <- o .: "table"
            cols <- o .: "columns"
            cnt <- o .: "row_count"
            pure (t, cols, cnt)

-- | 值编码成 JSON
valueToJSON :: Value -> A.Value
valueToJSON (VInt n) = A.Number (fromIntegral n)
valueToJSON (VStr s) = A.String (T.pack s)
valueToJSON (VBool b) = A.Bool b

-- | JSON 解回值
valueFromJSON :: A.Value -> Parser Value
valueFromJSON (A.Number n) =
    case floatingOrInteger n :: Either Double Integer of
        Right i -> pure (VInt (fromIntegral i))
        Left _ -> fail "non-integer number"
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
    -- \| 解一个键值对
    toPair (k, v) = do
        val <- valueFromJSON v
        pure (K.toString k, val)
rowFromJSON _ = fail "row must be a JSON object"

-- * 内部封装

-- | 发 Scan
doScan :: String -> IO (Either String [Row])
doScan t = do
    resp <- sendRequest (ReqScan t)
    pure $ case resp of
        RespRows rows -> Right rows
        RespError e -> Left e
        _ -> Left "unexpected response to scan"

-- | 发 Insert，可带索引键。
doInsertWith :: String -> Row -> Maybe Int -> IO (Either String ())
doInsertWith t r mk = do
    resp <- sendRequest (ReqInsert t r mk)
    pure $ case resp of
        RespOk -> Right ()
        RespError e -> Left e
        _ -> Left "unexpected response to insert"

-- | 发 ReplaceAll
doReplaceAll :: String -> [Row] -> IO (Either String ())
doReplaceAll t rs = do
    resp <- sendRequest (ReqReplaceAll t rs)
    pure $ case resp of
        RespOk -> Right ()
        RespError e -> Left e
        _ -> Left "unexpected response to replace_all"

-- | 发 ListTables
doListTables :: IO (Either String [String])
doListTables = do
    resp <- sendRequest ReqListTables
    pure $ case resp of
        RespTables ts -> Right ts
        RespError e -> Left e
        _ -> Left "unexpected response to list_tables"

-- | 一次拿全库 schema（表名 + 列 + 行数）。
doListCatalog :: IO (Either String [(String, [SchemaColumn], Int)])
doListCatalog = do
    resp <- sendRequest ReqListCatalog
    pure $ case resp of
        RespCatalog xs -> Right xs
        RespError e -> Left e
        _ -> Left "unexpected response to list_catalog"

-- | 发 LookupByIndex
doLookupByKey :: String -> Int -> IO (Either String (Maybe Row))
doLookupByKey t k = do
    resp <- sendRequest (ReqLookupByKey t k)
    pure $ case resp of
        RespRows [] -> Right Nothing
        RespRows (r : _) -> Right (Just r)
        RespError e -> Left e
        _ -> Left "unexpected response to lookup_by_index"

-- | 发 DescribeTable
doDescribeTable :: String -> IO (Either String ([SchemaColumn], Int))
doDescribeTable t = do
    resp <- sendRequest (ReqDescribeTable t)
    pure $ case resp of
        RespSchema cols n -> Right (cols, n)
        RespError e -> Left e
        _ -> Left "unexpected response to describe_table"

-- | 发 CreateTable
doCreateTable :: String -> [SchemaColumn] -> IO (Either String ())
doCreateTable t cols = do
    resp <- sendRequest (ReqCreateTable t cols)
    pure $ case resp of
        RespOk -> Right ()
        RespError e -> Left e
        _ -> Left "unexpected response to create_table"

-- | 发 DropTable
doDropTable :: String -> IO (Either String ())
doDropTable t = do
    resp <- sendRequest (ReqDropTable t)
    pure $ case resp of
        RespOk -> Right ()
        RespError e -> Left e
        _ -> Left "unexpected response to drop_table"

-- * 表结构

-- | 线上 schema 转本地列
schemaToColumns :: [SchemaColumn] -> [(String, Column)]
schemaToColumns = map go
  where
    go (SchemaColumn n "int") = (n, TInt)
    go (SchemaColumn n "str") = (n, TStr)
    go (SchemaColumn n "bool") = (n, TBool)
    go (SchemaColumn n _) = (n, TStr)

-- * MonadStorage 实例

-- | 存储方法全走管道
instance MonadStorage IPCStorage where
    -- \| 发 Scan
    scan t = IPCStorage (doScan t)

    -- \| 发 Insert
    insert t r mk = IPCStorage (doInsertWith t r mk)

    -- \| 发 ReplaceAll
    replaceAll t rs = IPCStorage (doReplaceAll t rs)

    -- \| 发 LookupByIndex
    lookupByKey t k = IPCStorage (doLookupByKey t k)

    -- \| 发 CreateTable
    createTable name cols = IPCStorage (doCreateTable name (map toWire cols))
      where
        -- \| 本地列类型转线上字符串
        toWire (n, TInt) = SchemaColumn n "int"
        toWire (n, TStr) = SchemaColumn n "str"
        toWire (n, TBool) = SchemaColumn n "bool"

    -- \| 发 DropTable
    dropTable name = IPCStorage (doDropTable name)

    -- \| 列表 + 逐表扫描拼库
    snapshot = IPCStorage $ do
        result <- doListCatalog
        case result of
            Left _ -> pure []
            Right xs -> mapM loadEntry xs
      where
        -- \| 加载一张表（schema 来自 catalog，行来自 scan）
        loadEntry (t, cols, _n) = do
            rows <- doScan t
            pure (t, Table t (schemaToColumns cols) (either (const []) id rows))
