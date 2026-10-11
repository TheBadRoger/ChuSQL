module ChuSQL.Core.Engine.Runtime.Query
    ( BoundQuery, QueryType (..), queryTypes, bindQuery, executeQueryM
    ) where

import ChuSQL.Core.Engine.Algebra.Eval (evalRelOpM)
import ChuSQL.Core.Engine.Algebra.Op (RelOp)
import ChuSQL.Core.Engine.Algebra.Optimize (optimize)
import ChuSQL.Core.Engine.Algebra.Planner (translate)
import ChuSQL.Core.Engine.Semantic (prepare, querySchema)
import ChuSQL.Core.Engine.Syntax.AST (Statement)
import ChuSQL.Core.Engine.Storage (MonadStorage (schema, withSchemaGuard))
import ChuSQL.Core.Engine.Runtime.SQL (sqlType)
import ChuSQL.Core.Engine.Runtime.Types (TypeId)
import ChuSQL.Core.Engine.Runtime.Relation (bindRelOpColumns)
import ChuSQL.Core.Engine.Runtime.Dependencies (queryTables, domainDependencies)
import ChuSQL.Core.Model

-- SQL 查询绑定边界：保存解析后的类型、函数身份及输出 schema。

-- | 保存运行时或兼容列类型
data QueryType = RuntimeType TypeId | CompatibilityType ColumnType
    | DomainType String QueryType | NullType deriving (Show, Eq)

-- | 保存仅由 schema 绑定的执行计划
data BoundQuery = BoundQuery Database [String] [(String, Column)] [(String, QueryType)] RelOp

-- | 查询绑定计划的输出类型身份
queryTypes :: BoundQuery -> [(String, QueryType)]
queryTypes (BoundQuery _ _ _ types _) = types

-- | 绑定 SQL 查询并优化确定的计划
bindQuery :: Database -> Statement -> Either String BoundQuery
bindQuery catalog statement = do
    let schemaOnly = [(name, table {tableRows = []}) | (name, table) <- catalog]
    resolved <- prepare schemaOnly statement
    columns <- querySchema schemaOnly resolved
    plan <- translate resolved
    types <- mapM typeOf columns
    bound <- bindRelOpColumns schemaOnly [] (optimize schemaOnly plan)
    let sources = queryTables resolved
        dependencies = sources ++ domainDependencies schemaOnly sources
    Right (BoundQuery schemaOnly dependencies columns types bound)
  where
    -- | 解析输出列的运行时身份
    typeOf (name, column) = do
        ty <- bindingType (columnType column)
        Right (name, ty)
    -- | 保留阶段外类型的兼容表示
    bindingType CNull = Right NullType
    bindingType (CDomain name base) = DomainType name <$> bindingType base
    bindingType CDate = Right (CompatibilityType CDate)
    bindingType CTimestamp = Right (CompatibilityType CTimestamp)
    bindingType CBlob = Right (CompatibilityType CBlob)
    bindingType ty = RuntimeType <$> sqlType ty

-- | 执行绑定查询并校验输出类型
executeQueryM :: MonadStorage m => BoundQuery -> m (Either String [Row])
executeQueryM (BoundQuery catalog dependencies columns _ plan) = do
    current <- schema
    case current >>= validateSchema of
        Left err -> pure (Left err)
        Right () -> do
            result <- withSchemaGuard [(name, table) | (name, table) <- catalog, name `elem` dependencies]
                (evalRelOpM catalog plan)
            pure (result >>= mapM validateRow)
  where
    -- | 拒绝绑定目录中的结构变化
    validateSchema current = mapM_ (validateTable current)
        [(name, table) | (name, table) <- catalog, name `elem` dependencies]
    -- | 比较列定义而不比较数据或统计
    validateTable current (name, expected) = case lookup name current of
        Just actual | tableIdentity actual /= tableIdentity expected -> Left ("bound query object changed: " ++ name)
        Just actual | tableSchemaVersion actual /= tableSchemaVersion expected -> Left ("bound query schema changed: " ++ name)
        Just actual | tableCols actual == tableCols expected -> Right ()
        _ -> Left ("bound query schema changed: " ++ name)
    -- | 校验完整输出行而不重新解析函数
    validateRow row
        | map fst row /= map fst columns = Left "bound query output schema mismatch"
        | otherwise = mapM validateColumn (zip columns row)
    -- | 校验输出列的物理值
    validateColumn ((name, column), (_, value))
        | columnType column == CNull, value == VNull = Right (name, value)
        | valueFits (columnType column) value = Right (name, value)
        | otherwise = Left ("bound query output type mismatch: " ++ name)
