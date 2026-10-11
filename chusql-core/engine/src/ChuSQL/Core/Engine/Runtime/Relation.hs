module ChuSQL.Core.Engine.Runtime.Relation (bindRelOpColumns) where

import ChuSQL.Core.Engine.Algebra.Op
import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Model (Database)

-- 优化完成后按算子输入布局绑定列槽位，外层行作为独立尾部布局。

-- | 绑定关系算子的输入列位置
bindRelOpColumns :: Database -> [String] -> RelOp -> Either String RelOp
bindRelOpColumns db outer = bind
  where
    -- \| 递归绑定每个算子的输入布局
    bind op = case op of
        Filter expression child -> Filter <$> expressionIn child expression <*> bind child
        Compute items child -> Compute <$> mapM (itemIn child) items <*> bind child
        Aggregate keys items child ->
            BoundAggregate
                <$> mapM (itemIn child) [(name, Col name) | name <- keys]
                <*> mapM (itemIn child) items
                <*> bind child
        BoundAggregate keys items child ->
            BoundAggregate
                <$> mapM (itemIn child) keys
                <*> mapM (itemIn child) items
                <*> bind child
        Join kind left right condition ->
            Join kind
                <$> bind left
                <*> bind right
                <*> bindSlots (relOpCols db left ++ relOpCols db right) outer condition
        Project columns child -> do
            let names = if columns == ["*"] then relOpCols db child else columns
            items <- mapM (itemIn child) [(name, Col name) | name <- names]
            Compute items <$> bind child
        Distinct child -> Distinct <$> bind child
        Sort ordering child ->
            BoundSort
                <$> mapM (sortIn child) [(Col name, direction) | (name, direction) <- ordering]
                <*> bind child
        BoundSort ordering child -> BoundSort <$> mapM (sortIn child) ordering <*> bind child
        Limit count child -> Limit count <$> bind child
        Derived alias child -> Derived alias <$> bind child
        _ -> Right op
    -- \| 使用子计划输出布局绑定表达式
    expressionIn child = bindSlots (relOpCols db child) outer
    -- \| 保留输出标签并绑定计算项
    itemIn child (name, expression) = (,) name <$> expressionIn child expression
    -- \| 绑定排序键并保留方向
    sortIn child (expression, direction) = (,) <$> expressionIn child expression <*> pure direction

-- | 绑定当前表达式的列引用
bindSlots :: [String] -> [String] -> Expr -> Either String Expr
bindSlots columns outer expression = case expression of
    Col name -> case slots columns name of
        slot : _ -> Right (ColumnSlot slot name)
        [] -> outerReference name
    ColumnSlot _ name -> case slots columns name of
        slot : _ -> Right (ColumnSlot slot name)
        [] -> Left ("unknown local column: " ++ name)
    OuterColumnSlot _ _ name -> outerReference name
    FunctionCall name arguments -> FunctionCall name <$> mapM bind arguments
    BoundFunction fid tid arguments -> BoundFunction fid tid <$> mapM bind arguments
    Construct ty arguments -> Construct ty <$> mapM bind arguments
    BoundConstruct tid arguments -> BoundConstruct tid <$> mapM bind arguments
    Add a b -> binary Add a b
    Sub a b -> binary Sub a b
    Mul a b -> binary Mul a b
    Div a b -> binary Div a b
    Neg a -> Neg <$> bind a
    Gt a b -> binary Gt a b
    Lt a b -> binary Lt a b
    Eq a b -> binary Eq a b
    GtE a b -> binary GtE a b
    LtE a b -> binary LtE a b
    NotEq a b -> binary NotEq a b
    And a b -> binary And a b
    Or a b -> binary Or a b
    IsNull a -> IsNull <$> bind a
    IsNotNull a -> IsNotNull <$> bind a
    CountOf a -> CountOf <$> bind a
    SumOf a -> SumOf <$> bind a
    AvgOf a -> AvgOf <$> bind a
    MinOf a -> MinOf <$> bind a
    MaxOf a -> MaxOf <$> bind a
    InSub a query negated -> InSub <$> bind a <*> pure query <*> pure negated
    InList a values negated -> InList <$> bind a <*> mapM bind values <*> pure negated
    QuantCmp op a query quantifier -> QuantCmp op <$> bind a <*> pure query <*> pure quantifier
    _ -> Right expression
  where
    -- \| 绑定当前布局的子表达式
    bind = bindSlots columns outer
    -- \| 查找布局中首次出现的列
    slots names name = [slot | (slot, key) <- zip [0 ..] names, key == name]
    -- \| 在外层布局绑定并保留作用域
    outerReference name = case slots outer name of
        slot : _ -> Right (OuterColumnSlot (length columns) slot name)
        [] -> Left ("unknown column: " ++ name)
    -- \| 绑定两个操作数
    binary constructor a b = constructor <$> bind a <*> bind b
