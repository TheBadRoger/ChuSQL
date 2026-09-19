module ChuSQL.Model where

-- 列的值：整数、字符串、布尔
data Value
    = VInt Int
    | VStr String
    | VBool Bool
    deriving (Show, Eq)

-- 一行：列名 → 值
type Row = [(String, Value)]

-- 一张表
data Table = Table
    { tableName :: String
    , tableCols :: [String]
    , tableRows :: [Row]
    }
    deriving (Show)

-- 数据库：多张表
type Database = [(String, Table)]
