module ChuSQL.Model (Column (..), Value (..), Row, Table (..), Database, colNames, colType, lookupTable, allColumns, qualify, unqualify) where

import Data.Hashable (Hashable (..))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

-- 数据模型：列类型、值、行、表、数据库，外加几个查表小工具。

-- * 类型
-- | 列的类型
data Column
    = TInt
    | TStr
    | TBool
    deriving (Show, Eq)

-- | 列的值
data Value
    = VInt Int
    | VStr String
    | VBool Bool
    deriving (Show, Eq)

-- | 值要能当哈希键用（等值连接建哈希表）
instance Hashable Value where
    hashWithSalt s (VInt n) = hashWithSalt (hashWithSalt s (0 :: Int)) n
    hashWithSalt s (VStr t) = hashWithSalt (hashWithSalt s (1 :: Int)) t
    hashWithSalt s (VBool b) = hashWithSalt (hashWithSalt s (2 :: Int)) b

-- | 一行：列名 -> 值
type Row = [(String, Value)]

-- | 一张表
data Table = Table
    { tableName :: String
    , tableCols :: [(String, Column)]
    , tableRows :: [Row]
    }
    deriving (Show)

-- | 数据库：多张表
type Database = [(String, Table)]

-- * 工具
-- | 取表的列名
colNames :: Table -> [String]
colNames = map fst . tableCols

-- | 查某列的类型
colType :: Table -> String -> Maybe Column
colType t c = lookup c (tableCols t)

-- | 按名查表，没有就报错
lookupTable :: Database -> String -> Either String Table
lookupTable db t = maybe (Left ("unknown table: " ++ t)) Right (lookup t db)

-- | 投影哨兵 "*"
allColumns :: String
allColumns = "*"

-- | 给列名加别名前缀：`Just "u"` + `name` -> `u.name`（没有别名就原样）
qualify :: Maybe String -> String -> String
qualify Nothing c = c
qualify (Just a) c = a ++ "." ++ c

-- | 去掉别名前缀：`Just "u"` + `u.name` -> `name`（没有这个前缀就原样返回）
unqualify :: Maybe String -> String -> String
unqualify Nothing c = c
unqualify (Just a) c = fromMaybe c (stripPrefix (a ++ ".") c)
