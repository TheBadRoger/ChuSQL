module ChuSQL.Model (Column (..), Value (..), Row, Table (..), Database, colNames, colType, lookupTable, allColumns) where

-- 数据模型：列类型、值、行、表、数据库，外加"按名查表 / 查列类型"这类通用小工具。

-- 列的类型
data Column
    = TInt
    | TStr
    | TBool
    deriving (Show, Eq)

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
    , tableCols :: [(String, Column)]
    , tableRows :: [Row]
    }
    deriving (Show)

-- 数据库：多张表
type Database = [(String, Table)]

-- 获取列名
colNames :: Table -> [String]
colNames = map fst . tableCols

-- 查某一列的类型
colType :: Table -> String -> Maybe Column
colType t c = lookup c (tableCols t)

-- 按名字查表；表不存在就报错
lookupTable :: Database -> String -> Either String Table
lookupTable db t = maybe (Left ("unknown table: " ++ t)) Right (lookup t db)

-- 投影列清单里的哨兵值：它表示"全部列，不用挑"，不是列名
allColumns :: String
allColumns = "*"
