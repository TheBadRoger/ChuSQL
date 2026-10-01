module ChuSQL.Syntax.AST (Statement (..), FromClause (..), JoinKind (..), Expr (..), Subquery (..), SortDir (..), makeSelect) where

import ChuSQL.Model (Column (..), ColumnType, Value (..))

-- 语法树：语句、数据来源、条件表达式、排序方向。

data Statement
    = Select
        { selectCols :: [String]
        , selectFrom :: FromClause
        , selectWhere :: Maybe Expr
        , selectGroupBy :: [String]
        , selectOrderBy :: [(String, SortDir)]
        , selectLimit :: Maybe Int
        }
    | SelectExpr
        { selectItems :: [(String, Expr)]
        , selectFrom :: FromClause
        , selectWhere :: Maybe Expr
        , selectGroupBy :: [String]
        , selectOrderBy :: [(String, SortDir)]
        , selectLimit :: Maybe Int
        }
    | Insert String [String] [[Expr]]
    | Delete String (Maybe Expr)
    | Update String [(String, Expr)] (Maybe Expr)
    | CreateTable String [(String, Column)]
    | DropTable String
    | CreateIndex String String
    | DropIndex String String
    | DropColumn String String
    | AddColumn String (String, Column)
    | RenameColumn String String String
    | AlterColumnType String String ColumnType
    | AlterColumnDefault String String (Maybe Value)
    | AlterColumnNull String String Bool
    | CreateUser String String
    | AlterUser String String
    | DropUser String
    -- | 新角色：权限的载体，用户靠成员资格继承它的权限
    | CreateRole String
    -- | 删角色：连同它的授权与成员一起删
    | DropRole String
    -- | GRANT <权限>[, ...] ON <表|*> TO <角色>
    | GrantPrivileges [String] String String
    -- | REVOKE <权限>[, ...] ON <表|*> FROM <角色>
    | RevokePrivileges [String] String String
    -- | GRANT <角色> TO <用户>[, ...]：给用户加角色
    | GrantRole String [String]
    -- | REVOKE <角色> FROM <用户>[, ...]
    | RevokeRole String [String]
    | CreateDatabase String
    | DropDatabase String
    | UseDatabase String
    | ShowDatabases
    deriving (Show, Eq)

-- | 连接类型：内连接与左外连接
data JoinKind
    = InnerJoin
    | LeftJoin
    deriving (Show, Eq)

data FromClause
    = FromTable (Maybe String) String
    | FromJoin JoinKind FromClause (Maybe String) String Expr
    | FromUnit
    deriving (Show, Eq)

-- | 子查询：语句，加上它对外层列的引用（自由变量，语义检查时填）
data Subquery = Subquery
    { subqueryStatement :: Statement
    , subqueryRefs :: [String]
    }
    deriving (Show, Eq)

data Expr
    = Col String
    | LitNull
    | LitInt Int
    | LitFloat Double
    | LitStr String
    | LitBool Bool
    | LitDate String
    | LitTimestamp String
    | LitBlob String
    | Add Expr Expr
    | Sub Expr Expr
    | Mul Expr Expr
    | Div Expr Expr
    | Neg Expr
    | Gt Expr Expr
    | Lt Expr Expr
    | Eq Expr Expr
    | And Expr Expr
    | Or Expr Expr
    | IsNull Expr
    | IsNotNull Expr
    | CountAll
    | CountOf Expr
    | SumOf Expr
    | AvgOf Expr
    | MinOf Expr
    | MaxOf Expr
    | ScalarSub Subquery
    | InSub Expr Subquery Bool
    | InList Expr [Expr] Bool
    | ExistsSub Subquery Bool
    deriving (Show, Eq)

data SortDir
    = Asc
    | Desc
    deriving (Show, Eq)

-- | 造一个最简单的 SELECT
makeSelect :: [String] -> String -> Maybe Expr -> Statement
makeSelect cols tbl w =
    Select
        { selectCols = cols
        , selectFrom = FromTable Nothing tbl
        , selectWhere = w
        , selectGroupBy = []
        , selectOrderBy = []
        , selectLimit = Nothing
        }
