module ChuSQL.Core.Engine.Runtime.Query
    ( BoundQuery, QueryType (..), queryTypes, bindQuery, executeQueryM
    ) where

import ChuSQL.Core.Engine.Algebra.Eval (evalRelOpM)
import ChuSQL.Core.Engine.Algebra.Op (RelOp)
import ChuSQL.Core.Engine.Algebra.Optimize (optimize)
import ChuSQL.Core.Engine.Algebra.Planner (translate)
import ChuSQL.Core.Engine.Semantic (prepare, querySchema)
import ChuSQL.Core.Engine.Syntax.AST (Statement)
import ChuSQL.Core.Engine.Storage (MonadStorage)
import ChuSQL.Core.Engine.Runtime.SQL (sqlType)
import ChuSQL.Core.Engine.Runtime.Types (TypeId)
import ChuSQL.Core.Model

-- SQL 查询绑定边界：保存解析后的类型、函数身份及输出 schema。

-- | 保存运行时或兼容列类型
data QueryType = RuntimeType TypeId | CompatibilityType ColumnType | NullType deriving (Show, Eq)

-- | 保存仅由 schema 绑定的执行计划
data BoundQuery = BoundQuery Database [(String, Column)] [(String, QueryType)] RelOp

-- | 查询绑定计划的输出类型身份
queryTypes :: BoundQuery -> [(String, QueryType)]
queryTypes (BoundQuery _ _ types _) = types

-- | 绑定 SQL 查询并优化确定的计划
bindQuery :: Database -> Statement -> Either String BoundQuery
bindQuery catalog statement = do
    let schemaOnly = [(name, table {tableRows = []}) | (name, table) <- catalog]
    resolved <- prepare schemaOnly statement
    columns <- querySchema schemaOnly resolved
    plan <- translate resolved
    types <- mapM typeOf columns
    Right (BoundQuery schemaOnly columns types (optimize schemaOnly plan))
  where
    -- | 解析输出列的运行时身份
    typeOf (name, column) = do
        ty <- bindingType (columnType column)
        Right (name, ty)
    -- | 保留阶段外类型的兼容表示
    bindingType CNull = Right NullType
    bindingType CDate = Right (CompatibilityType CDate)
    bindingType CTimestamp = Right (CompatibilityType CTimestamp)
    bindingType CBlob = Right (CompatibilityType CBlob)
    bindingType ty@(CDomain _ base)
        | typeClassOf base == TemporalClass || base == CBlob = Right (CompatibilityType ty)
    bindingType ty = RuntimeType <$> sqlType ty

-- | 执行绑定查询并校验输出类型
executeQueryM :: MonadStorage m => BoundQuery -> m (Either String [Row])
executeQueryM (BoundQuery catalog columns _ plan) = do
    result <- evalRelOpM catalog plan
    pure (result >>= mapM validateRow)
  where
    -- | 校验完整输出行而不重新解析函数
    validateRow row
        | map fst row /= map fst columns = Left "bound query output schema mismatch"
        | otherwise = mapM validateColumn (zip columns row)
    -- | 校验输出列的物理值
    validateColumn ((name, column), (_, value))
        | columnType column == CNull, value == VNull = Right (name, value)
        | valueFits (columnType column) value = Right (name, value)
        | otherwise = Left ("bound query output type mismatch: " ++ name)
