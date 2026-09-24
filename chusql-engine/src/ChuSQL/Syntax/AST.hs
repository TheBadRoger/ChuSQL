module ChuSQL.Syntax.AST (Query (..), FromClause (..), Expr (..), SortDir (..), makeSelect) where

-- SQL 语法树：四类语句（SELECT / INSERT / DELETE / UPDATE）+ 表达式、FROM 从句、排序方向。

-- 查询语句
data Query
    = Select
        { selectCols :: [String]
        , selectFrom :: FromClause
        , selectWhere :: Maybe Expr
        , selectOrderBy :: [(String, SortDir)]
        , selectLimit :: Maybe Int
        }
    | Insert String [String] [Expr]
    | Delete String (Maybe Expr)
    | Update String [(String, Expr)] (Maybe Expr)
    deriving (Show, Eq)

-- FROM 从句：单表或 JOIN。
data FromClause
    = FromTable (Maybe String) String
    | FromJoin FromClause (Maybe String) String Expr
    deriving (Show, Eq)

-- 条件表达式
data Expr
    = Col String
    | LitInt Int
    | LitStr String
    | LitBool Bool
    | Gt Expr Expr
    | Lt Expr Expr
    | Eq Expr Expr
    | And Expr Expr
    | Or Expr Expr
    deriving (Show, Eq)

-- 排序方向
data SortDir
    = Asc
    | Desc
    deriving (Show, Eq)

-- 便捷构造：造一条最简单的 SELECT（给测试和演示用）。
makeSelect :: [String] -> String -> Maybe Expr -> Query
makeSelect cols tbl w =
    Select
        { selectCols = cols
        , selectFrom = FromTable Nothing tbl
        , selectWhere = w
        , selectOrderBy = []
        , selectLimit = Nothing
        }
