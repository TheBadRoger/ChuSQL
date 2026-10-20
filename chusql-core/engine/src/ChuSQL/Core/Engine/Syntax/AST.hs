module ChuSQL.Core.Engine.Syntax.AST (Statement (..), DomainDefinition (..), DomainAlter (..), FromClause (..), JoinKind (..), Expr (..), Subquery (..), Quantifier (..), CompareOp (..), SortDir (..), OrderKey (..), makeSelect, distinctOrdering, orderPosition, orderExpression, orderKeyExpression) where

import ChuSQL.Core.Engine.Runtime.Functions (FunctionId)
import ChuSQL.Core.Engine.Runtime.Types (RuntimeValue, TypeExpr, TypeId)
import ChuSQL.Core.Model (Column (..), ColumnType, Value (..))

-- 语法树：语句、数据来源、条件表达式、排序方向。

-- | 领域的基础列属性及具名检查条件
data DomainDefinition = DomainDefinition Column [(String, String)] deriving (Show, Eq)

-- | 领域约束及默认值的结构变更
data DomainAlter
    = DomainDefault (Maybe Value)
    | DomainNullable Bool
    | DomainAddCheck (Maybe String) String
    | DomainDropCheck String
    deriving (Show, Eq)

data Statement
    = Select
        { selectItems :: [(String, Expr)]
        -- ^ 输出名称与投影表达式，普通列用 Col
        , selectFrom :: FromClause
        , selectWhere :: Maybe Expr
        , selectGroupBy :: [String]
        , selectHaving :: Maybe Expr
        , selectOrderBy :: [(OrderKey, SortDir)]
        , selectLimit :: Maybe Int
        }
    | SelectDistinct Statement
    | Insert String [String] [[Expr]]
    | Delete String (Maybe Expr)
    | Update String [(String, Expr)] (Maybe Expr)
    | CreateTable String [(String, Column)]
    | CreateDomain String DomainDefinition
    | AlterDomain String DomainAlter
    | DropDomain String
    | ShowDomains
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
    | AlterIdentity String [(String, Bool)]
    | ShowGrants
    | ShowRoles
    | DropUser String
    | -- | 新角色：权限的载体，用户靠成员资格继承它的权限
      CreateRole String
    | -- | 删角色：连同它的授权与成员一起删
      DropRole String
    | -- | GRANT 权限 ON 表 TO 角色，末位表示带 WITH GRANT OPTION
      GrantPrivileges [String] String String Bool
    | -- | REVOKE 权限 ON 表 FROM 角色
      RevokePrivileges [String] String String
    | RevokeGrantOption [String] String String
    | -- | GRANT 角色 TO 用户：给用户加角色
      GrantRole String [String]
    | -- | REVOKE <角色> FROM <用户>[, ...]
      RevokeRole String [String]
    | CreateDatabase String
    | DropDatabase String
    | UseDatabase String
    | ShowDatabases
    | -- | 开事务：BEGIN / START TRANSACTION
      BeginTransaction
    | -- | 串行化快照事务
      BeginSerializableTransaction
    | -- | 提交事务
      CommitTransaction
    | -- | 回滚事务
      RollbackTransaction
    | -- | 建保存点：SAVEPOINT 名字
      Savepoint String
    | -- | 回滚到保存点：ROLLBACK TO [SAVEPOINT] 名字
      RollbackToSavepoint String
    | -- | 释放保存点：RELEASE [SAVEPOINT] 名字
      ReleaseSavepoint String
    deriving (Show, Eq)

-- | 连接类型：内连接与左外连接
data JoinKind
    = InnerJoin
    | LeftJoin
    deriving (Show, Eq)

-- | 数据来源：单表、派生表（子查询，输出列已改成裸列名）、带连接的来源，或空
data FromClause
    = FromTable (Maybe String) String
    | FromSubquery (Maybe String) Statement
    | FromJoin JoinKind FromClause FromClause Expr
    | FromUnit
    deriving (Show, Eq)

-- | 子查询：语句，加上它对外层列的引用（自由变量，语义检查时填）
data Subquery
    = Subquery
        { subqueryStatement :: Statement
        , subqueryRefs :: [String]
        }
    | BoundSubquery
        { subqueryStatement :: Statement
        , subqueryRefs :: [String]
        , subquerySlots :: [(String, Expr)]
        }
    deriving (Show, Eq)

-- | 量词：ANY 至少一行成立，ALL 每一行都成立
data Quantifier
    = AnyQ
    | AllQ
    deriving (Show, Eq)

-- | 量词比较里的比较运算符
data CompareOp
    = CmpEq
    | CmpNe
    | CmpGt
    | CmpGtE
    | CmpLt
    | CmpLtE
    deriving (Show, Eq)

-- | 条件表达式：列、字面量、算术、比较、逻辑、聚合与子查询
data Expr
    = Col String
    | ColumnSlot Int String
    | OuterColumnSlot Int Int String
    | FunctionCall String [Expr]
    | BoundFunction FunctionId TypeId [Expr]
    | Construct TypeExpr [Expr]
    | BoundConstruct TypeId [Expr]
    | EnumLiteral ColumnType String
    | TypedLiteral ColumnType String
    | RuntimeLiteral TypeId RuntimeValue
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
    | GtE Expr Expr
    | LtE Expr Expr
    | NotEq Expr Expr
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
    | QuantCmp CompareOp Expr Subquery Quantifier
    deriving (Show, Eq)

-- | 排序方向：升序或降序
data SortDir
    = Asc
    | Desc
    deriving (Show, Eq)

-- | 区分排序列、输出序号与来源表达式
data OrderKey = OrderColumn String | OrderPosition Int | OrderExpression Expr
    deriving (Show, Eq)

-- | 造一个最简单的 SELECT
makeSelect :: [String] -> String -> Maybe Expr -> Statement
makeSelect cols tbl w =
    Select
        { selectItems = [(c, Col c) | c <- cols]
        , selectFrom = FromTable Nothing tbl
        , selectWhere = w
        , selectGroupBy = []
        , selectHaving = Nothing
        , selectOrderBy = []
        , selectLimit = Nothing
        }

-- | 将去重排序映射到输出列
distinctOrdering :: Statement -> Either String [(OrderKey, SortDir)]
distinctOrdering q@Select{} = resolveDistinctOrdering q
distinctOrdering _ = Left "DISTINCT requires SELECT"

-- | 校验排序序号并返回从零开始的位置
orderPosition :: Int -> OrderKey -> Either String (Maybe Int)
orderPosition width (OrderPosition position)
    | position > 0, position <= width = Right (Just (position - 1))
    | otherwise = Left "ORDER BY: position is outside the select list"
orderPosition _ _ = Right Nothing

-- | 按序号、唯一别名或来源列解析排序表达式
orderExpression :: [(String, Expr)] -> OrderKey -> Either String Expr
orderExpression _ (OrderExpression expression) = Right expression
orderExpression items key@(OrderPosition _) = do
    position <- orderPosition (length items) key
    case position of
        Just slot -> Right (snd (items !! slot))
        Nothing -> Left "ORDER BY: missing output position"
orderExpression items (OrderColumn column) = case [expression | (label, expression) <- items, label == column] of
    [] -> Right (Col column)
    [expression] -> Right expression
    expressions | all (== Col column) expressions -> Right (Col column)
    _ -> Left ("ORDER BY: ambiguous projection alias: " ++ column)

-- | 收集原始排序键中可能含子查询的表达式
orderKeyExpression :: OrderKey -> Maybe Expr
orderKeyExpression (OrderColumn column) = Just (Col column)
orderKeyExpression (OrderExpression expression) = Just expression
orderKeyExpression (OrderPosition _) = Nothing

-- | 解析去重输出的排序列
resolveDistinctOrdering :: Statement -> Either String [(OrderKey, SortDir)]
resolveDistinctOrdering q = mapM resolve (selectOrderBy q)
  where
    items = case q of
        Select{selectItems = projected} -> projected
        _ -> []
    -- \| 排序键必须出现在去重结果中
    resolve (key, direction) = do
        position <- orderPosition (length items) key
        case position of
            Just _ -> Right (key, direction)
            Nothing -> case [slot | (slot, (label, expression)) <- zip [1 ..] items, matches key label expression] of
                [slot] -> Right (OrderPosition slot, direction)
                [] -> Left "DISTINCT: ORDER BY column must appear in the select list"
                _ -> Left "DISTINCT: ambiguous ORDER BY column"
    -- \| 名称或完整表达式必须对应去重输出项
    matches (OrderColumn column) label expression = label == column || expression == Col column
    matches (OrderExpression key) _ expression = key == expression
    matches (OrderPosition _) _ _ = False
