{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Core.Protocol (
    SchemaColumn (..),
    Request (..),
    TxnOp (..),
    Response (..),
    TableInfo (..),
    Account (..),
    QueryResult (..),
    valueToJSON,
    valueFromJSON,
    rowToJSON,
    rowFromJSON,
    queryResultJson,
) where

import ChuSQL.Core.Model (Histogram (..), Row, Value (..))
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser)
import Data.Scientific (floatingOrInteger, fromFloatDigits)
import qualified Data.Text as T
import Data.Text (Text)

-- 线上协议类型：存储层请求/响应与查询结果集。

-- | 数据字典里一列的线上形状
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

-- | 列编码成 JSON
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

-- | JSON 解回列
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

-- | 存储层请求
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
    | ReqScanShard String (Maybe [String]) Int Int
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
    | ReqApplyTransaction [TxnOp]

-- | 事务提交里的一条写操作
data TxnOp
    = TxnUpsert String [Row]
    | TxnDelete String [Int]
    | TxnReplace String [Row]
    deriving (Show, Eq)

-- | 事务操作编码成 JSON
instance ToJSON TxnOp where
    toJSON (TxnUpsert t rs) =
        object
            [ "op" .= ("upsert" :: T.Text)
            , "table" .= t
            , "rows" .= map rowToJSON rs
            ]
    toJSON (TxnDelete t ks) =
        object
            [ "op" .= ("delete" :: T.Text)
            , "table" .= t
            , "ids" .= ks
            ]
    toJSON (TxnReplace t rs) =
        object
            [ "op" .= ("replace" :: T.Text)
            , "table" .= t
            , "rows" .= map rowToJSON rs
            ]

-- | 请求编码成 JSON
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
    toJSON (ReqScanShard t cols shard shards) =
        object
            [ "method" .= ("scan_shard" :: T.Text)
            , "table" .= t
            , "columns" .= cols
            , "shard" .= shard
            , "shards" .= shards
            ]
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
    toJSON (ReqApplyTransaction ops) =
        object
            [ "method" .= ("apply_transaction" :: T.Text)
            , "ops" .= ops
            ]

-- | 一张表的梗概：列、行数、索引与列统计
data TableInfo = TableInfo
    { tiTable :: String
    , tiColumns :: [SchemaColumn]
    , tiRows :: Int
    , tiIndexes :: [String]
    , tiStats :: [(String, Int, Bool)]
    , tiHistograms :: [(String, Histogram)]
    }
    deriving (Show, Eq)

-- | 表信息编码成 JSON
instance ToJSON TableInfo where
    toJSON info =
        object
            [ "table" .= tiTable info
            , "columns" .= tiColumns info
            , "row_count" .= tiRows info
            , "indexes" .= map (\name -> object ["column" .= name]) (tiIndexes info)
            , "stats" .= map (statJson (tiHistograms info)) (tiStats info)
            ]
      where
        statJson hists (name, distinct, capped) =
            object (statFields ++ histFields (lookup name hists))
          where
            statFields = ["name" .= name, "distinct" .= distinct, "capped" .= capped]
            histFields Nothing = []
            histFields (Just h) = ["lo" .= histLow h, "hi" .= histHigh h, "hist" .= histBuckets h]

-- | 线上直方图还原；区间或桶缺了就当作没有
histogramFromWire :: Maybe Double -> Maybe Double -> [Int] -> Maybe Histogram
histogramFromWire (Just low) (Just high) counts
    | not (null counts) = Just (Histogram low high counts)
histogramFromWire _ _ _ = Nothing

-- | JSON 解回表信息
instance FromJSON TableInfo where
    parseJSON = withObject "TableInfo" $ \o -> do
        table <- o .: "table"
        columns <- o .: "columns"
        count <- o .: "row_count"
        indexes <- o .:? "indexes" .!= []
        stats <- o .:? "stats" .!= []
        indexColumns <- mapM (\v -> withObject "IndexWire" (.: "column") v) (indexes :: [A.Value])
        parsed <- mapM parseStat (stats :: [A.Value])
        let parsedStats = [(name, distinct, capped) | (name, distinct, capped, _) <- parsed]
            parsedHistograms = [(name, h) | (name, _, _, Just h) <- parsed]
        pure (TableInfo table columns count indexColumns parsedStats parsedHistograms)
      where
        parseStat = withObject "ColumnStat" $ \o -> do
            name <- o .: "name"
            distinct <- o .: "distinct"
            capped <- o .:? "capped" .!= False
            low <- o .:? "lo"
            high <- o .:? "hi"
            counts <- o .:? "hist" .!= []
            pure (name, distinct, capped, histogramFromWire low high counts)

-- | 一条账号记录
data Account = Account
    { accountId :: Integer
    , accountUser :: T.Text
    , accountHash :: T.Text
    , accountRevision :: Integer
    , accountRegisteredAt :: T.Text
    , accountLastLoginAt :: Maybe T.Text
    } deriving (Eq)

-- | 账号的展示：只露账号名与版本号
instance Show Account where
    show a = "Account " ++ show (accountUser a) ++ " revision=" ++ show (accountRevision a)

-- | JSON 解回账号
instance FromJSON Account where
    parseJSON = withObject "Account" $ \o -> Account <$> o .: "id" <*> o .: "user"
        <*> o .: "password_hash" <*> o .: "revision"
        <*> o .:? "registered_at" .!= "" <*> o .:? "last_login_at"

-- | 账号编码成 JSON
instance ToJSON Account where
    toJSON a = object ["id" .= accountId a, "user" .= accountUser a,
        "password_hash" .= accountHash a, "revision" .= accountRevision a,
        "registered_at" .= accountRegisteredAt a, "last_login_at" .= accountLastLoginAt a]

-- | 存储层响应
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

-- | JSON 解回响应
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
            "schema" -> RespSchema <$> parseJSON (A.Object o)
            "catalog" -> do
                xs <- o .: "schemas" :: Parser [A.Value]
                RespCatalog <$> mapM parseJSON xs
            other -> fail ("unknown status: " ++ T.unpack other)

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
    -- | 键值对解成一个字段
    toPair (k, v) = do
        val <- valueFromJSON v
        pure (K.toString k, val)
rowFromJSON _ = fail "row must be a JSON object"

-- | 一条语句的结果集：列名与排好的行
data QueryResult = QueryResult
    { qrColumns :: [Text]
    , qrRows :: [[Value]]
    , qrRowCount :: Int
    , qrTruncated :: Bool
    , qrDatabase :: Maybe Text
    }
    deriving (Eq, Show)

-- | JSON 解回结果集
instance FromJSON QueryResult where
    parseJSON = withObject "QueryResult" $ \o -> do
        cols <- o .: "columns"
        rows <- o .: "rows"
        QueryResult cols <$> mapM (mapM valueFromJSON) rows
            <*> o .:? "rowCount" .!= 0
            <*> o .:? "truncated" .!= False
            <*> o .:? "database"

-- | 结果还原成 JSON（JSON 输出与 TCP 结果集共用）
queryResultJson :: QueryResult -> A.Value
queryResultJson result =
    object
        [ "columns" .= qrColumns result
        , "rows" .= map (map valueToJSON) (qrRows result)
        , "rowCount" .= qrRowCount result
        , "truncated" .= qrTruncated result
        , "database" .= qrDatabase result
        ]
