module ChuSQL.Core.Engine.Runtime.Query (
    BoundQuery,
    QueryType (..),
    queryTypes,
    queryReads,
    bindQuery,
    executeQueryM,
) where

import ChuSQL.Core.Engine.Algebra.Eval (evalRelOpM)
import ChuSQL.Core.Engine.Algebra.Expr (hasSubquery)
import ChuSQL.Core.Engine.Algebra.Op (RelOp (..))
import ChuSQL.Core.Engine.Algebra.Optimize (optimize)
import ChuSQL.Core.Engine.Algebra.Planner (translate)
import ChuSQL.Core.Engine.Runtime.Dependencies (domainDependencies, queryTables, validateDependencies)
import ChuSQL.Core.Engine.Runtime.Relation (bindRelOpColumns)
import ChuSQL.Core.Engine.Runtime.SQL (sqlType)
import ChuSQL.Core.Engine.Runtime.Types (TypeId)
import ChuSQL.Core.Engine.Semantic (prepare, querySchema)
import ChuSQL.Core.Engine.Storage (MonadStorage (schema, withSchemaGuard))
import ChuSQL.Core.Engine.Syntax.AST (Statement)
import ChuSQL.Core.Model
import Data.List (nub)

-- SQL 查询绑定边界：保存解析后的类型、函数身份及输出 schema。

-- | 保存运行时或兼容列类型
data QueryType
    = EnumType String [String]
    | RuntimeType TypeId
    | CompatibilityType ColumnType
    | DomainType String QueryType
    | NullType
    deriving (Show, Eq)

-- | 保存仅由 schema 绑定的执行计划
data BoundQuery = BoundQuery Database [String] [(String, Column)] [(String, QueryType)] RelOp

-- | 查询绑定计划的输出类型身份
queryTypes :: BoundQuery -> [(String, QueryType)]
queryTypes (BoundQuery _ _ _ types _) = types

-- | 从扫描布局提取读取列，复杂子查询保守读取完整行
queryReads :: BoundQuery -> [(String, Maybe [String])]
queryReads (BoundQuery _ dependencies _ _ plan) =
    [(name, columns name) | name <- dependencies]
  where
    -- \| 合并同表的全部扫描需求
    columns name = case scanReads plan of
        Nothing -> Nothing
        Just requirements -> case [cols | (table, cols) <- requirements, table == name] of
            [] -> Nothing
            values -> nub . concat <$> sequence values
    -- \| 子查询表达式保持完整布局，其余沿算子树收集
    scanReads op = case op of
        Scan _ table cols -> Just [(table, cols)]
        Lookup _ table _ _ -> Just [(table, Nothing)]
        Range _ table _ _ _ -> Just [(table, Nothing)]
        Compute items child | not (any (hasSubquery . snd) items) -> scanReads child
        Filter expression child | not (hasSubquery expression) -> scanReads child
        Project _ child -> scanReads child
        Distinct child -> scanReads child
        Limit _ child -> scanReads child
        Derived _ child -> scanReads child
        Sort _ child -> scanReads child
        BoundSort items child | not (any (hasSubquery . fst) items) -> scanReads child
        Aggregate _ items child | not (any (hasSubquery . snd) items) -> scanReads child
        BoundAggregate keys items child | not (any (hasSubquery . snd) (keys ++ items)) -> scanReads child
        Join _ left right condition | not (hasSubquery condition) -> (++) <$> scanReads left <*> scanReads right
        Unit -> Just []
        _ -> Nothing

-- | 绑定 SQL 查询并优化确定的计划
bindQuery :: Database -> Statement -> Either String BoundQuery
bindQuery catalog statement = do
    let schemaOnly = [(name, table{tableRows = []}) | (name, table) <- catalog]
    resolved <- prepare schemaOnly statement
    columns <- querySchema schemaOnly resolved
    plan <- translate resolved
    types <- mapM typeOf columns
    bound <- bindRelOpColumns schemaOnly [] (optimize schemaOnly plan)
    let sources = queryTables resolved
        dependencies = sources ++ domainDependencies schemaOnly sources
    Right (BoundQuery schemaOnly dependencies columns types bound)
  where
    -- \| 解析输出列的运行时身份
    typeOf (name, column) = do
        ty <- bindingType (columnType column)
        Right (name, ty)
    -- \| 保留阶段外类型的兼容表示
    bindingType CNull = Right NullType
    bindingType (CDomain name base) = DomainType name <$> bindingType base
    bindingType (CEnum name labels) = Right (EnumType name labels)
    bindingType CDate = Right (CompatibilityType CDate)
    bindingType CTimestamp = Right (CompatibilityType CTimestamp)
    bindingType CBlob = Right (CompatibilityType CBlob)
    bindingType ty = RuntimeType <$> sqlType ty

-- | 执行绑定查询并校验输出类型
executeQueryM :: (MonadStorage m) => BoundQuery -> m (Either String [Row])
executeQueryM (BoundQuery catalog dependencies columns _ plan) = do
    current <- schema
    case current >>= validateDependencies guardedTables of
        Left err -> pure (Left err)
        Right () -> do
            result <-
                withSchemaGuard
                    guardedTables
                    (evalRelOpM catalog plan)
            pure (result >>= mapM validateRow)
  where
    guardedTables = [(name, table) | (name, table) <- catalog, name `elem` dependencies]
    -- \| 校验完整输出行而不重新解析函数
    validateRow row
        | map fst row /= map fst columns = Left "bound query output schema mismatch"
        | otherwise = mapM validateColumn (zip columns row)
    -- \| 校验输出列的物理值
    validateColumn ((name, column), (_, value))
        | columnType column == CNull, value == VNull = Right (name, value)
        | valueFits (columnType column) value = Right (name, value)
        | otherwise = Left ("bound query output type mismatch: " ++ name)
