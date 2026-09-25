module ChuSQL.Syntax.AST (Statement (..), FromClause (..), Expr (..), SortDir (..), makeSelect) where

import ChuSQL.Model (Column (..))

-- 语法树：语句、数据来源、条件表达式、排序方向。

-- * 语句

-- | 一条语句：查 / 加 / 删 / 改 / 建表 / 删表 / 建索引 / 删索引
data Statement
    = Select
        { selectCols :: [String]
        , selectFrom :: FromClause
        , selectWhere :: Maybe Expr
        , selectOrderBy :: [(String, SortDir)]
        , selectLimit :: Maybe Int
        }
    | -- \| 表、列、若干行（`VALUES (...), (...)` 可以一次给多行）
      Insert String [String] [[Expr]]
    | Delete String (Maybe Expr)
    | Update String [(String, Expr)] (Maybe Expr)
    | CreateTable String [(String, Column)]
    | DropTable String
    | -- \| 给某一列建索引（列名即索引名）
      CreateIndex String String
    | DropIndex String String
    deriving (Show, Eq)

-- | 数据来源：单表，或两表拼接
data FromClause
    = FromTable (Maybe String) String
    | FromJoin FromClause (Maybe String) String Expr
    deriving (Show, Eq)

-- * 表达式

-- | 条件表达式
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

-- * 排序

-- | 排序方向
data SortDir
    = Asc
    | Desc
    deriving (Show, Eq)

-- * 构造

-- | 造一个最简单的 SELECT
makeSelect :: [String] -> String -> Maybe Expr -> Statement
makeSelect cols tbl w =
    Select
        { selectCols = cols
        , selectFrom = FromTable Nothing tbl
        , selectWhere = w
        , selectOrderBy = []
        , selectLimit = Nothing
        }
