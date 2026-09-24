{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Storage.IPC (
    IPCStorage (..),
    Request (..),
    Response (..),
    SchemaColumn (..),
    sendRequest,
    doListTables,
    doLookupByKey,
    doInsertKeyed,
    doDescribeTable,
) where

import ChuSQL.Model
import ChuSQL.Storage
import Control.Exception (bracket)
import Data.Aeson (FromJSON (..), ToJSON (..), eitherDecodeStrict, encode, object, withObject, (.:), (.=))
import Data.Aeson qualified as A
import Data.Aeson.Key qualified as K
import Data.Aeson.KeyMap qualified as KM
import Data.Aeson.Types (Parser)
import Data.ByteString.Char8 qualified as BSC
import Data.ByteString.Lazy qualified as BL
import Data.List (nub)
import Data.Scientific (floatingOrInteger)
import Data.Text qualified as T
import System.Environment (lookupEnv)
import System.IO

-- IPC 存储实现：每个 MonadStorage 方法翻译成管道上的一条 JSON 请求。

-- | IPC 实现：通过命名管道连 Rust 存储进程。
newtype IPCStorage a = IPCStorage
    { runIPCStorage :: IO a
    }

-- | Functor：把 fmap 转发给底下的 IO。
instance Functor IPCStorage where
    fmap f (IPCStorage m) = IPCStorage (fmap f m)

-- | Applicative：把 pure / <*> 转发给底下的 IO。
instance Applicative IPCStorage where
    pure = IPCStorage . pure
    IPCStorage mf <*> IPCStorage ma = IPCStorage (mf <*> ma)

-- | Monad：顺序执行，转发给底下的 IO。
instance Monad IPCStorage where
    IPCStorage m >>= k = IPCStorage $ do
        a <- m
        runIPCStorage (k a)

-- | 管道路径；默认 \\.\pipe\chusql-storage，可用 CHUSQL_PIPE 覆盖。
pipePath :: IO String
pipePath = do
    name <- maybe "chusql-storage" id <$> lookupEnv "CHUSQL_PIPE"
    pure ("\\\\.\\pipe\\" ++ name)

-- | 打开管道、跑闭包、关闭。
withPipe :: (Handle -> IO a) -> IO a
withPipe = bracket openPipe hClose
  where
    -- \| 二进制读写、不做换行翻译，跟 Rust 侧对齐。
    openPipe = do
        path <- pipePath
        h <- openFile path ReadWriteMode
        hSetBuffering h LineBuffering
        hSetNewlineMode h noNewlineTranslation
        pure h

-- | 发一条请求，读一条响应。
sendRequest :: Request -> IO Response
sendRequest req = withPipe $ \h -> do
    BL.hPutStr h (encode req)
    BSC.hPutStr h "\n"
    hFlush h
    line <- BSC.hGetLine h
    let cleaned = BSC.dropWhileEnd (== '\r') line
    case eitherDecodeStrict cleaned of
        Left err -> pure (RespError ("decode: " ++ err))
        Right r -> pure r

-- * JSON 编解码

-- | 一列的 schema；对应 Rust 侧的 SchemaColumn。
data SchemaColumn = SchemaColumn
    { scName :: String
    , scType :: String
    }
    deriving (Show, Eq)

-- | SchemaColumn 编码；Haskell 侧只发 DescribeTable，编码用不上，暂存实现。
instance ToJSON SchemaColumn where
    toJSON (SchemaColumn n t) = object ["name" .= n, "ty" .= t]

-- | SchemaColumn 解码：从 {"name": "...", "ty": "int"|"str"|"bool"} 读回。
instance FromJSON SchemaColumn where
    parseJSON = withObject "SchemaColumn" $ \o -> do
        n <- o .: "name"
        t <- o .: "ty"
        pure (SchemaColumn n t)

-- | 请求；对应 Rust 侧带 method tag 的 enum。
data Request
    = ReqPing
    | ReqScan String
    | ReqInsert String Row (Maybe Int)
    | ReqReplaceAll String [Row]
    | ReqListTables
    | ReqLookupByKey String Int
    | ReqDescribeTable String

-- | 请求编码：method 字段 + 各自参数。
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

-- | 响应；对应 Rust 侧带 status tag 的 enum。
data Response
    = RespPong
    | RespRows [Row]
    | RespTables [String]
    | RespSchema [SchemaColumn]
    | RespOk
    | RespError String

-- | 响应解码：按 status 字段分派。
instance FromJSON Response where
    parseJSON = withObject "Response" $ \o -> do
        status <- o .: "status"
        case status :: T.Text of
            "pong" -> pure RespPong
            "ok" -> pure RespOk
            "rows" -> RespRows <$> (o .: "rows" >>= mapM rowFromJSON)
            "tables" -> RespTables <$> o .: "tables"
            "schema" -> RespSchema <$> o .: "columns"
            "error" -> RespError <$> o .: "message"
            other -> fail ("unknown status: " ++ T.unpack other)

-- | Value 编码成 JSON 的 number / string / bool。
valueToJSON :: Value -> A.Value
valueToJSON (VInt n) = A.Number (fromIntegral n)
valueToJSON (VStr s) = A.String (T.pack s)
valueToJSON (VBool b) = A.Bool b

-- | JSON 解回 Value；number 只接受整数。
valueFromJSON :: A.Value -> Parser Value
valueFromJSON (A.Number n) =
    case floatingOrInteger n :: Either Double Integer of
        Right i -> pure (VInt (fromIntegral i))
        Left _ -> fail "non-integer number"
valueFromJSON (A.String s) = pure (VStr (T.unpack s))
valueFromJSON (A.Bool b) = pure (VBool b)
valueFromJSON _ = fail "unsupported value type"

-- | 一行编成 JSON object。
rowToJSON :: Row -> A.Value
rowToJSON r =
    A.Object (KM.fromList [(K.fromString k, valueToJSON v) | (k, v) <- r])

-- | JSON object 解成一行。
rowFromJSON :: A.Value -> Parser Row
rowFromJSON (A.Object o) = mapM toPair (KM.toList o)
  where
    -- \| 解一个键值对。
    toPair (k, v) = do
        val <- valueFromJSON v
        pure (K.toString k, val)
rowFromJSON _ = fail "row must be a JSON object"

-- * 内部请求封装

-- | 发 Scan 并把结果取成 Either。
doScan :: String -> IO (Either String [Row])
doScan t = do
    resp <- sendRequest (ReqScan t)
    pure $ case resp of
        RespRows rows -> Right rows
        RespError e -> Left e
        _ -> Left "unexpected response to scan"

-- | 发 Insert（不带索引键）。
doInsert :: String -> Row -> IO (Either String ())
doInsert t r = do
    resp <- sendRequest (ReqInsert t r Nothing)
    pure $ case resp of
        RespOk -> Right ()
        RespError e -> Left e
        _ -> Left "unexpected response to insert"

-- | 发 Insert（带索引键）。
doInsertKeyed :: String -> Row -> Int -> IO (Either String ())
doInsertKeyed t r k = do
    resp <- sendRequest (ReqInsert t r (Just k))
    pure $ case resp of
        RespOk -> Right ()
        RespError e -> Left e
        _ -> Left "unexpected response to insert"

-- | 发 ReplaceAll。
doReplaceAll :: String -> [Row] -> IO (Either String ())
doReplaceAll t rs = do
    resp <- sendRequest (ReqReplaceAll t rs)
    pure $ case resp of
        RespOk -> Right ()
        RespError e -> Left e
        _ -> Left "unexpected response to replace_all"

-- | 发 ListTables。
doListTables :: IO (Either String [String])
doListTables = do
    resp <- sendRequest ReqListTables
    pure $ case resp of
        RespTables ts -> Right ts
        RespError e -> Left e
        _ -> Left "unexpected response to list_tables"

-- | 发 LookupByIndex。
doLookupByKey :: String -> Int -> IO (Either String (Maybe Row))
doLookupByKey t k = do
    resp <- sendRequest (ReqLookupByKey t k)
    pure $ case resp of
        RespRows [] -> Right Nothing
        RespRows (r : _) -> Right (Just r)
        RespError e -> Left e
        _ -> Left "unexpected response to lookup_by_index"

-- | 发 DescribeTable。
doDescribeTable :: String -> IO (Either String [SchemaColumn])
doDescribeTable t = do
    resp <- sendRequest (ReqDescribeTable t)
    pure $ case resp of
        RespSchema cols -> Right cols
        RespError e -> Left e
        _ -> Left "unexpected response to describe_table"

-- | 把 Rust 的 schema 列转成 Model 的 Column；未知类型降级为 TStr。
schemaToColumns :: [SchemaColumn] -> [(String, Column)]
schemaToColumns = map go
  where
    -- \| 按 ty 字符串映射成 Column 构造子。
    go (SchemaColumn n "int") = (n, TInt)
    go (SchemaColumn n "str") = (n, TStr)
    go (SchemaColumn n "bool") = (n, TBool)
    go (SchemaColumn n _) = (n, TStr)

-- | 从若干行推断列名与类型；catalog 拿不到时的兜底。
inferColumns :: [Row] -> [(String, Column)]
inferColumns rows = [(n, inferType n) | n <- names]
  where
    -- \| 全部出现过的列名。
    names = nub (concatMap (map fst) rows)

    -- \| 用第一个出现的值推断列类型；找不到就给 TStr。
    inferType n = case [v | r <- rows, Just v <- [lookup n r]] of
        (VInt _ : _) -> TInt
        (VStr _ : _) -> TStr
        (VBool _ : _) -> TBool
        [] -> TStr

-- | 先 describe 拿列，再 scan 拿行；describe 拿不到时退回扫行推断。
loadTable :: String -> IO Table
loadTable t = do
    schemaR <- doDescribeTable t
    case schemaR of
        Right cols -> do
            rowsR <- doScan t
            case rowsR of
                Right rows -> pure (Table t (schemaToColumns cols) rows)
                Left _ -> pure (Table t (schemaToColumns cols) [])
        Left _ -> do
            rowsR <- doScan t
            case rowsR of
                Right rows -> pure (Table t (inferColumns rows) rows)
                Left _ -> pure (Table t [] [])

-- * MonadStorage 实例

-- | IPC 实现：所有方法都走管道。
instance MonadStorage IPCStorage where
    -- \| 发 Scan。
    scan t = IPCStorage (doScan t)

    -- \| 发 Insert，不带索引键。
    insert t r = IPCStorage (doInsert t r)

    -- \| 发 ReplaceAll。
    replaceAll t rs = IPCStorage (doReplaceAll t rs)

    -- \| 发 LookupByIndex。
    lookupByKey t k = IPCStorage (doLookupByKey t k)

    -- \| ListTables + 逐表 (describe, scan)，拼成 Database。
    snapshot = IPCStorage $ do
        result <- doListTables
        case result of
            Left _ -> pure []
            Right ts -> mapM loadDbEntry ts
      where
        -- \| 加载一个 (表名, 表) 对。
        loadDbEntry t = do
            tbl <- loadTable t
            pure (t, tbl)
