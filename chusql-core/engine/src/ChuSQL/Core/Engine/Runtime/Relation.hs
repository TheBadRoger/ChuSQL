module ChuSQL.Core.Engine.Runtime.Relation (bindRelOpColumns, bindWriteColumns, bindSlots, orderRow) where

import ChuSQL.Core.Engine.Algebra.Op
import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Model (Database, Row, lookupTable, tableCols)

-- 按关系或写语句的输入布局绑定列槽位，并排列存储返回行。

-- | 按目标表布局绑定写入表达式
bindWriteColumns :: Database -> Statement -> Either String Statement
bindWriteColumns db statement = case statement of
    Insert table columns rows -> Insert table columns <$> mapM (mapM (bindSlots [] [])) rows
    Update table assignments condition -> do
        columns <- map fst . tableCols <$> lookupTable db table
        bound <- mapM (bindItem columns) assignments
        predicate <- mapM (bindSlots columns []) condition
        Right (Update table bound predicate)
    Delete table condition -> do
        columns <- map fst . tableCols <$> lookupTable db table
        Delete table <$> mapM (bindSlots columns []) condition
    _ -> Right statement
  where
    -- | 保留写入目标并绑定右侧表达式
    bindItem columns (name, expression) = (,) name <$> bindSlots columns [] expression

-- | 按绑定布局排列一行并拒绝缺列
orderRow :: [String] -> Row -> Either String Row
orderRow columns row = mapM column columns
  where
    -- | 保留列名并取得物理值
    column name = case lookup name row of
        Just value -> Right (name, value)
        Nothing -> Left ("missing bound column: " ++ name)

-- | 绑定关系算子的输入列位置
bindRelOpColumns :: Database -> [String] -> RelOp -> Either String RelOp
bindRelOpColumns db outer = bind
  where
    -- \| 递归绑定每个算子的输入布局
    bind op = case op of
        Filter expression child -> Filter <$> expressionIn child expression <*> bind child
        Compute items child -> Compute <$> mapM (itemIn child) items <*> bind child
        Aggregate keys items child -> bind (BoundAggregate [(name, Col name) | name <- keys] items child)
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
        SemiJoin kind leftKey rightKey left right query ->
            SemiJoin kind
                <$> expressionIn left leftKey
                <*> expressionIn right rightKey
                <*> bind left
                <*> bind right
                <*> pure query
        Project columns child -> do
            let names = if columns == ["*"] then relOpCols db child else columns
            items <- mapM (itemIn child) [(name, Col name) | name <- names]
            Compute items <$> bind child
        Distinct child -> Distinct <$> bind child
        Sort ordering child -> bind (BoundSort [(Col name, direction) | (name, direction) <- ordering] child)
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
    ColumnSlot slot name -> ColumnSlot <$> rebindSlot columns slot name "unknown local column: " <*> pure name
    OuterColumnSlot width slot name
        | width < 0 -> Left ("bound column position mismatch: " ++ name)
        | otherwise -> OuterColumnSlot (length columns)
            <$> rebindSlot outer slot name "unknown column: " <*> pure name
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
    ScalarSub query -> ScalarSub <$> bindSubquery query
    InSub a query negated -> InSub <$> bind a <*> bindSubquery query <*> pure negated
    InList a values negated -> InList <$> bind a <*> mapM bind values <*> pure negated
    ExistsSub query negated -> ExistsSub <$> bindSubquery query <*> pure negated
    QuantCmp op a query quantifier -> QuantCmp op <$> bind a <*> bindSubquery query <*> pure quantifier
    _ -> Right expression
  where
    -- \| 绑定当前布局的子表达式
    bind = bindSlots columns outer
    -- \| 查找布局中首次出现的列
    slots names name = [slot | (slot, key) <- zip [0 ..] names, key == name]
    -- | 保留有效位置，仅允许唯一列重定位
    rebindSlot names slot name missing
        | slot < 0 = Left ("bound column position mismatch: " ++ name)
        | take 1 (drop slot names) == [name] = Right slot
        | otherwise = case slots names name of
            [position] -> Right position
            [] -> Left (missing ++ name)
            _ -> Left ("ambiguous bound column: " ++ name)
    -- \| 在外层布局绑定并保留作用域
    outerReference name = case slots outer name of
        slot : _ -> Right (OuterColumnSlot (length columns) slot name)
        [] -> Left ("unknown column: " ++ name)
    -- | 将子查询自由变量绑定到父输入布局
    bindSubquery query = do
        references <- case query of
            Subquery _ names -> mapM reference names
            BoundSubquery _ names references
                | map fst references == names -> mapM rebind references
                | otherwise -> Left "bound subquery references mismatch"
        Right (BoundSubquery (subqueryStatement query) (subqueryRefs query) references)
    -- | 补回限定名之前先绑定真实物理列
    reference name = do
        let physical = if name `elem` (columns ++ outer) then name else reverse (takeWhile (/= '.') (reverse name))
        boundReference <- bind (Col physical)
        Right (name, boundReference)
    -- | 重绑定时保留局部或外层身份
    rebind (name, boundReference) = (,) name <$> bind boundReference
    -- \| 绑定两个操作数
    binary constructor a b = constructor <$> bind a <*> bind b
