module ChuSQL.Model (Column (..), Value (..), Row, Table (..), Database, colNames, colType, lookupTable, allColumns, qualify, unqualify) where

import Data.Hashable (Hashable (..))
import Data.List (stripPrefix)
import Data.Maybe (fromMaybe)

-- 数据模型：列类型、值、行、表、数据库，外加几个查表小工具。

data Column
    = TInt
    | TStr
    | TBool
    deriving (Show, Eq)

data Value
    = VInt Int
    | VStr String
    | VBool Bool
    deriving (Show, Eq)

instance Hashable Value where
    hashWithSalt s (VInt n) = hashWithSalt (hashWithSalt s (0 :: Int)) n
    hashWithSalt s (VStr t) = hashWithSalt (hashWithSalt s (1 :: Int)) t
    hashWithSalt s (VBool b) = hashWithSalt (hashWithSalt s (2 :: Int)) b

type Row = [(String, Value)]

data Table = Table
    { tableName :: String
    , tableCols :: [(String, Column)]
    , tableRows :: [Row]
    }
    deriving (Show)

type Database = [(String, Table)]

-- | 取表的列名
colNames :: Table -> [String]
colNames = map fst . tableCols

-- | 查某列的类型
colType :: Table -> String -> Maybe Column
colType t c = lookup c (tableCols t)

-- | 按名查表，没有就报错
lookupTable :: Database -> String -> Either String Table
lookupTable db t = maybe (Left ("unknown table: " ++ t)) Right (lookup t db)

allColumns :: String
allColumns = "*"

-- | 给列名加别名前缀
qualify :: Maybe String -> String -> String
qualify Nothing c = c
qualify (Just a) c = a ++ "." ++ c

-- | 去掉别名前缀
unqualify :: Maybe String -> String -> String
unqualify Nothing c = c
unqualify (Just a) c = fromMaybe c (stripPrefix (a ++ ".") c)
