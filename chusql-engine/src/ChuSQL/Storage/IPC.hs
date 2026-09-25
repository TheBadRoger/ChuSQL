{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Storage.IPC (
    IPCStorage (..),
    Request (..),
    Response (..),
    SchemaColumn (..),
    TableInfo (..),
    sendRequest,
    closeConnection,
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
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict, encode, object, withObject, (.:), (.:?), (.!=), (.=))
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
    | ReqInsert String Row
    | ReqInsertBatch String [Row]
    | ReqDeleteKeys String [Int]
    | ReqReplaceAll String [Row]
    | ReqListTables
    | ReqLookupByColumn String String Int
    | ReqDescribeTable String
    | ReqCreateTable String [SchemaColumn]
    | ReqDropTable String
    | ReqCreateIndex String String
    | ReqDropIndex String String
    | ReqListCatalog

-- | 请求编码
instance ToJSON Request where
    toJSON ReqPing = object ["method" .= ("ping" :: T.Text)]
    toJSON (ReqScan t) = object ["method" .= ("scan" :: T.Text), "table" .= t]
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
    toJSON ReqListCatalog = object ["method" .= ("list_catalog" :: T.Text)]

-- | 一张表的线上信息：列 + 行数 + 索引 + 列统计
data TableInfo = TableInfo
    { tiTable :: String
    , tiColumns :: [SchemaColumn]
    , tiRows :: Int
    , tiIndexes :: [String]
    , tiStats :: [(String, Int, Bool)]
    }
    deriving (Show, Eq)

-- | 响应类型
data Response
    = RespPong
    | RespRows [Row]
    | RespTables [String]
    | RespSchema TableInfo
    | RespNoIndex
    | RespOk
    | RespError String
    | RespCatalog [TableInfo]

-- | 响应解码
instance FromJSON Response where
    parseJSON = withObject "Response" $ \o -> do
        status <- o .: "status"
        case status :: T.Text of
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
        -- \| 解一张表的 schema（新字段都给了默认值，老响应也能解）
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

        -- \| 解一条列统计
        parseStat :: A.Value -> Parser (String, Int, Bool)
        parseStat = withObject "ColumnStat" $ \o -> do
            name <- o .: "name"
            distinct <- o .: "distinct"
            capped <- o .:? "capped" .!= False
            pure (name, distinct, capped)

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

-- | 发一条请求，把响应翻成 Either。
--
-- 每个操作的区别只有两处：发什么请求、认哪种响应。所以固定套路收在这里：
-- 对面报错 → `Left`；认得这种响应 → `Right`；其余一律算协议错（比如对面版本对不上）。
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

-- | 发 Insert（索引由存储层自己维护，这里不传 key）
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

-- | 一次拿全库 schema（表名 + 列 + 行数 + 索引 + 统计）。
doListCatalog :: IO (Either String [TableInfo])
doListCatalog = ask "list_catalog" ReqListCatalog $ \resp -> case resp of
    RespCatalog xs -> Just xs
    _ -> Nothing

-- | 按某一列的索引取一行。
-- 返回 `NoIndex` 表示"这个列上没有索引"，上层据此退回全表扫描。
doLookupByColumn :: String -> String -> Int -> IO (Either String IndexResult)
doLookupByColumn t c k = ask "lookup_by_index" (ReqLookupByColumn t c k) $ \resp -> case resp of
    RespRows [] -> Just (IndexRow Nothing)
    RespRows (r : _) -> Just (IndexRow (Just r))
    RespNoIndex -> Just NoIndex
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

-- | 发 CreateTable
doCreateTable :: String -> [SchemaColumn] -> IO (Either String ())
doCreateTable t cols = ask "create_table" (ReqCreateTable t cols) okOnly

-- | 发 DropTable
doDropTable :: String -> IO (Either String ())
doDropTable t = ask "drop_table" (ReqDropTable t) okOnly

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

    -- \| 发 CreateTable
    createTable name cols = IPCStorage (doCreateTable name (map toWire cols))
      where
        -- \| 本地列类型转线上字符串
        toWire (n, TInt) = SchemaColumn n "int"
        toWire (n, TStr) = SchemaColumn n "str"
        toWire (n, TBool) = SchemaColumn n "bool"

    -- \| 发 DropTable
    dropTable name = IPCStorage (doDropTable name)

    -- \| 建索引
    createIndex t c = IPCStorage (doCreateIndex t c)

    -- \| 删索引
    dropIndex t c = IPCStorage (doDropIndex t c)

    -- \| 列表 + 逐表扫描拼库
    snapshot = IPCStorage $ do
        result <- doListCatalog
        case result of
            Left _ -> pure []
            Right xs -> mapM loadEntry xs
      where
        -- \| 加载一张表（schema 来自 catalog，行来自 scan）
        loadEntry info = do
            rows <- doScan (tiTable info)
            pure
                ( tiTable info
                , Table (tiTable info) (schemaToColumns (tiColumns info)) (either (const []) id rows)
                )

    -- \| 只问数据字典要结构，一行数据都不拉过来
    schema = IPCStorage $ do
        result <- doListCatalog
        pure $ case result of
            Left _ -> []
            Right xs ->
                [ (tiTable i, Table (tiTable i) (schemaToColumns (tiColumns i)) [])
                | i <- xs
                ]
