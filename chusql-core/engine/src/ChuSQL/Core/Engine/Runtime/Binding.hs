module ChuSQL.Core.Engine.Runtime.Binding
    ( Expression (..), TypedExpression, expressionType, bindExpression, evaluateExpression
    , Plan (..), TypedPlan, planSchema, bindPlan, executePlan
    ) where

import ChuSQL.Core.Engine.Runtime.Types
import ChuSQL.Core.Engine.Runtime.Functions
import Control.Monad (filterM)
import Data.List (nub, sort)

-- 仅依据 schema 绑定表达式与查询，执行已解析的类型和函数身份。

-- | 表达未绑定的类型化语言
data Expression = Literal TypeExpr RuntimeValue | Reference String | Apply String [Expression]
    | ListLiteral TypeExpr [Expression] | NothingLiteral TypeExpr | JustLiteral Expression
    | TupleLiteral [Expression] deriving (Show, Eq)

-- | 保存绑定后的表达式节点
data ExpressionNode = Constant RuntimeValue | BoundReference String | BoundApply FunctionId [TypedExpression]
    | BoundList [TypedExpression] | BoundNothing | BoundJust TypedExpression | BoundTuple [TypedExpression]
    deriving (Show, Eq)

-- | 保存表达式的确定类型
data TypedExpression = TypedExpression TypeId ExpressionNode deriving (Show, Eq)

-- | 提取表达式类型身份
expressionType :: TypedExpression -> TypeId
expressionType (TypedExpression tid _) = tid

-- | 绑定类型、列与函数身份
bindExpression :: TypeRegistry -> FunctionRegistry -> [(String, TypeId)] -> Expression -> Either String TypedExpression
bindExpression types functions env expression = case expression of
    Literal ty value -> do
        tid <- descriptorId <$> resolveType types ty
        validateValue types tid value
        Right (TypedExpression tid (Constant value))
    Reference name -> case [tid | (key, tid) <- env, key == name] of
        [tid] -> describeType types tid >> Right (TypedExpression tid (BoundReference name))
        [] -> Left ("unknown column: " ++ name)
        _ -> Left ("ambiguous column: " ++ name)
    Apply name arguments -> do
        bound <- mapM bind arguments
        sig <- resolveFunction functions name (map expressionType bound)
        Right (TypedExpression (signatureResult sig) (BoundApply (signatureId sig) bound))
    ListLiteral child arguments -> do
        childId <- descriptorId <$> resolveType types child
        bound <- mapM bind arguments
        if all ((== childId) . expressionType) bound
            then compound (listType child) (BoundList bound)
            else Left "list element type mismatch"
    NothingLiteral child -> compound (maybeType child) BoundNothing
    JustLiteral argument -> do
        bound <- bind argument
        compound (maybeType (typeExpression (expressionType bound))) (BoundJust bound)
    TupleLiteral arguments -> do
        bound <- mapM bind arguments
        compound (tupleType (map (typeExpression . expressionType) bound)) (BoundTuple bound)
  where
    -- | 绑定当前作用域的子表达式
    bind = bindExpression types functions env
    -- | 校验并构造复合表达式
    compound ty node = do
        tid <- descriptorId <$> resolveType types ty
        Right (TypedExpression tid node)

-- | 执行已绑定表达式并校验输出
evaluateExpression :: TypeRegistry -> FunctionRegistry -> [(String, RuntimeValue)] -> TypedExpression -> Either String RuntimeValue
evaluateExpression types functions row (TypedExpression tid node) = do
    value <- case node of
        Constant v -> Right v
        BoundReference name -> case [v | (key, v) <- row, key == name] of
            [v] -> Right v
            [] -> Left ("missing bound column: " ++ name)
            _ -> Left ("duplicate bound column: " ++ name)
        BoundApply fid arguments -> mapM evaluate arguments >>= invokeFunction types functions fid
        BoundList arguments -> RList <$> mapM evaluate arguments
        BoundNothing -> Right (RMaybe Nothing)
        BoundJust argument -> RMaybe . Just <$> evaluate argument
        BoundTuple arguments -> RTuple <$> mapM evaluate arguments
    validateValue types tid value
    Right value
  where
    -- | 执行当前行的子表达式
    evaluate = evaluateExpression types functions row

-- | 表达未绑定的关系计划
data Plan = TableScan String | SingleRow | Projection [(String, Expression)] Plan
    | Selection Expression Plan deriving (Show, Eq)

-- | 保存绑定后的关系节点
data PlanNode = BoundScan String | BoundSingleRow | BoundProjection [(String, TypedExpression)] TypedPlan
    | BoundSelection TypedExpression TypedPlan deriving (Show, Eq)

-- | 保存输出 schema 和关系节点
data TypedPlan = TypedPlan [(String, TypeId)] PlanNode deriving (Show, Eq)

-- | 提取计划的输出 schema
planSchema :: TypedPlan -> [(String, TypeId)]
planSchema (TypedPlan output _) = output

-- | 使用目录 schema 绑定查询计划
bindPlan :: TypeRegistry -> FunctionRegistry -> [(String, [(String, TypeExpr)])] -> Plan -> Either String TypedPlan
bindPlan types functions catalog plan = case plan of
    TableScan name -> case [columns | (key, columns) <- catalog, key == name] of
        [columns] -> do
            uniqueNames (map fst columns)
            bound <- mapM bindColumn columns
            Right (TypedPlan bound (BoundScan name))
        [] -> Left ("unknown table: " ++ name)
        _ -> Left ("ambiguous table: " ++ name)
    SingleRow -> Right (TypedPlan [] BoundSingleRow)
    Projection items source -> do
        child <- bind source
        uniqueNames (map fst items)
        bound <- mapM (bindItem (planSchema child)) items
        Right (TypedPlan [(name, expressionType value) | (name, value) <- bound] (BoundProjection bound child))
    Selection condition source -> do
        child <- bind source
        predicate <- bindExpression types functions (planSchema child) condition
        bool <- descriptorId <$> resolveType types boolType
        if expressionType predicate == bool
            then Right (TypedPlan (planSchema child) (BoundSelection predicate child))
            else Left "selection predicate must be Bool"
  where
    -- | 绑定子计划
    bind = bindPlan types functions catalog
    -- | 解析 schema 列的类型身份
    bindColumn (name, ty) = do
        tid <- descriptorId <$> resolveType types ty
        Right (name, tid)
    -- | 绑定投影项
    bindItem env (name, value) = do
        bound <- bindExpression types functions env value
        Right (name, bound)

-- | 拒绝重复输出列
uniqueNames :: [String] -> Either String ()
uniqueNames names
    | nub names == names = Right ()
    | otherwise = Left "duplicate schema column"

-- | 执行绑定计划并校验存储返回值
executePlan :: TypeRegistry -> FunctionRegistry -> (String -> Either String [[(String, RuntimeValue)]])
    -> TypedPlan -> Either String [[(String, RuntimeValue)]]
executePlan types functions scan (TypedPlan output node) = do
    rows <- case node of
        BoundScan name -> scan name
        BoundSingleRow -> Right [[]]
        BoundProjection items child -> do
            input <- executePlan types functions scan child
            mapM (project items) input
        BoundSelection condition child -> do
            input <- executePlan types functions scan child
            filterM (keep condition) input
    mapM_ (validateRow output) rows
    Right [[(name, value) | (name, _) <- output, Just value <- [lookup name row]] | row <- rows]
  where
    -- | 计算一行的投影项
    project items row = mapM (projectItem row) items
    -- | 计算命名输出项
    projectItem row (name, expression) = do
        value <- evaluateExpression types functions row expression
        Right (name, value)
    -- | 计算选择条件
    keep condition row = do
        value <- evaluateExpression types functions row condition
        case value of
            RBool result -> Right result
            _ -> Left "bound predicate returned a non-Bool value"
    -- | 校验存储行的完整 schema
    validateRow columns row
        | sort (map fst row) /= sort (map fst columns) = Left "storage row schema mismatch"
        | otherwise = mapM_ (validateColumn row) columns
    -- | 按列名校验返回值的类型
    validateColumn row (name, tid) = case lookup name row of
        Just value -> validateValue types tid value
        Nothing -> Left ("missing bound column: " ++ name)
