module ChuSQL.Core.Engine.Runtime.Relation (bindOptimizedRelOp, bindRelOpIdentities, bindRelOpColumns, bindWriteIdentities, bindWriteColumns, bindSlots, columnLayout, resolveColumnSlot, orderRow) where

import ChuSQL.Core.Engine.Algebra.Op
import ChuSQL.Core.Engine.Algebra.Optimize (optimize)
import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Model (Database, Row, lookupTable, tableCols)

-- 按关系或写语句的输入布局绑定列槽位，并排列存储返回行。

-- | 按目标表布局绑定写入表达式
bindWriteColumns :: Database -> Statement -> Either String Statement
bindWriteColumns db statement = bindWriteIdentities db statement >>= bindWriteWith True db

-- | 按目标表原始列序号绑定写入表达式身份
bindWriteIdentities :: Database -> Statement -> Either String Statement
bindWriteIdentities = bindWriteWith False

-- | 复用写入遍历绑定身份或执行槽位
bindWriteWith :: Bool -> Database -> Statement -> Either String Statement
bindWriteWith lower db statement = case statement of
    Insert table columns rows -> Insert table columns <$> mapM (mapM (bindColumns lower [] [])) rows
    Update table assignments condition -> do
        columns <- map fst . tableCols <$> lookupTable db table
        bound <- mapM (bindItem columns) assignments
        predicate <- mapM (bindColumns lower columns []) condition
        Right (Update table bound predicate)
    Delete table condition -> do
        columns <- map fst . tableCols <$> lookupTable db table
        Delete table <$> mapM (bindColumns lower columns []) condition
    _ -> Right statement
  where
    -- \| 保留写入目标并绑定右侧表达式
    bindItem columns (name, expression) = (,) name <$> bindColumns lower columns [] expression

-- | 按绑定布局排列一行并拒绝缺列
orderRow :: [String] -> Row -> Either String Row
orderRow columns row = mapM column columns
  where
    -- \| 保留列名并取得物理值
    column name = case lookup name row of
        Just value -> Right (name, value)
        Nothing -> Left ("missing bound column: " ++ name)

-- | 完成结构优化后绑定身份，再生成执行槽位
bindOptimizedRelOp :: Database -> [String] -> RelOp -> Either String RelOp
bindOptimizedRelOp db outer plan = bindRelOpIdentities db outer (optimize db plan) >>= bindRelOpColumns db outer

-- | 绑定关系算子的输入列位置
bindRelOpColumns :: Database -> [String] -> RelOp -> Either String RelOp
bindRelOpColumns = bindRelOpWith (bindLayoutSlots True)

-- | 绑定查询内列身份，保留至执行槽位生成前
bindRelOpIdentities :: Database -> [String] -> RelOp -> Either String RelOp
bindRelOpIdentities = bindRelOpWith (bindLayoutSlots False)

-- | 按同一算子路径绑定身份或执行槽位
bindRelOpWith :: ([(ColumnId, String)] -> [String] -> Expr -> Either String Expr) -> Database -> [String] -> RelOp -> Either String RelOp
bindRelOpWith bindExpression db outer = bind []
  where
    -- \| 递归绑定每个算子的输入布局
    bind path op = case op of
        Filter expression child -> Filter <$> expressionIn path child expression <*> bind (inputPath path op) child
        Compute items child -> Compute <$> mapM (itemIn (path ++ [0]) child) items <*> bind (inputPath path op) child
        Aggregate keys items child -> bind path (BoundAggregate [(name, Col name) | name <- keys] items child)
        BoundAggregate keys items child ->
            BoundAggregate
                <$> mapM (itemIn (path ++ [0]) child) keys
                <*> mapM (itemIn (path ++ [0]) child) items
                <*> bind (inputPath path op) child
        Join kind left right condition ->
            Join kind
                <$> bind (path ++ [0]) left
                <*> bind (path ++ [1]) right
                <*> bindExpression (relOpLayout db (path ++ [0]) left ++ relOpLayout db (path ++ [1]) right) outer condition
        SemiJoin kind leftKey rightKey left right query ->
            SemiJoin kind
                <$> expressionIn (path ++ [0]) left leftKey
                <*> expressionIn (path ++ [1]) right rightKey
                <*> bind (path ++ [0]) left
                <*> bind (path ++ [1]) right
                <*> pure query
        Project columns child -> do
            let names = if columns == ["*"] then relOpCols db child else columns
            items <- mapM (itemIn (path ++ [0]) child) [(name, Col name) | name <- names]
            Compute items <$> bind (inputPath path op) child
        Distinct child -> Distinct <$> bind (inputPath path op) child
        Sort ordering child -> bind path (BoundSort [(Col name, direction) | (name, direction) <- ordering] child)
        BoundSort ordering child -> BoundSort <$> mapM (sortIn path child) ordering <*> bind (inputPath path op) child
        Limit count child -> Limit count <$> bind (inputPath path op) child
        Derived alias child -> Derived alias <$> bind (inputPath path op) child
        _ -> Right op
    -- \| 输出算子为输入保留独立路径
    inputPath path op = case op of
        Compute{} -> path ++ [0]
        Project{} -> path ++ [0]
        BoundAggregate{} -> path ++ [0]
        _ -> path
    -- \| 使用子计划输出布局绑定表达式
    expressionIn path child = bindExpression (relOpLayout db path child) outer
    -- \| 保留输出标签并绑定计算项
    itemIn path child (name, expression) = (,) name <$> expressionIn path child expression
    -- \| 绑定排序键并保留方向
    sortIn path child (expression, direction) = (,) <$> expressionIn path child expression <*> pure direction

-- | 绑定当前表达式的列引用
bindSlots :: [String] -> [String] -> Expr -> Either String Expr
bindSlots columns outer expression = bindColumns False columns outer expression >>= bindColumns True columns outer

-- | 为单行作用域构造原始列身份布局
bindColumns :: Bool -> [String] -> [String] -> Expr -> Either String Expr
bindColumns lower columns = bindLayoutSlots lower (columnLayout columns)

-- | 按单行原始列序号分配身份
columnLayout :: [String] -> [(ColumnId, String)]
columnLayout columns = [(ColumnId (SourceId []) slot, name) | (slot, name) <- zip [0 ..] columns]

-- | 只按指定布局的身份解析位置和显示名
resolveColumnSlot :: [(ColumnId, String)] -> ColumnId -> String -> Either String (Int, String)
resolveColumnSlot layout cid name =
    let matches = [(slot, key) | (slot, (identity, key)) <- zip [0 ..] layout, identity == cid]
     in case filter ((== name) . snd) matches ++ matches of
            match : _ -> Right match
            [] -> Left ("unknown column identity: " ++ name)

-- | 按布局解析列身份，按需降为执行槽位
bindLayoutSlots :: Bool -> [(ColumnId, String)] -> [String] -> Expr -> Either String Expr
bindLayoutSlots lower layout outer expression = case expression of
    ColumnRef cid name -> do
        (slot, key) <- resolveColumnSlot layout cid name
        Right (if lower then ColumnSlot slot key else ColumnRef cid key)
    OuterColumnRef cid name -> do
        (slot, key) <- resolveColumnSlot outerLayout cid name
        Right (if lower then OuterColumnSlot (length columns) slot key else OuterColumnRef cid key)
    Col name -> case slots columns name of
        slot : _ -> bind (ColumnRef (fst (layout !! slot)) name)
        [] -> outerReference name
    ColumnSlot slot name -> do
        position <- rebindSlot columns slot name "unknown local column: "
        if lower
            then Right (ColumnSlot position name)
            else Right (ColumnRef (fst (layout !! position)) name)
    OuterColumnSlot width slot name
        | width < 0 -> Left ("bound column position mismatch: " ++ name)
        | otherwise -> do
            position <- rebindSlot outer slot name "unknown column: "
            Right (outerColumn position name)
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
    -- \| 提取输入布局的显示列名
    columns = map snd layout
    -- \| 外层行身份独立于局部计划的来源路径
    outerLayout = columnLayout outer
    -- \| 保留外层作用域，按需生成执行槽位
    outerColumn slot name
        | lower = OuterColumnSlot (length columns) slot name
        | otherwise = OuterColumnRef (ColumnId (SourceId []) slot) name
    -- \| 在同一身份布局绑定子表达式
    bind = bindLayoutSlots lower layout outer
    -- \| 查找布局中首次出现的列
    slots names name = [slot | (slot, key) <- zip [0 ..] names, key == name]
    -- \| 保留有效位置，仅允许唯一列重定位
    rebindSlot names slot name missing
        | slot < 0 = Left ("bound column position mismatch: " ++ name)
        | take 1 (drop slot names) == [name] = Right slot
        | otherwise = case slots names name of
            [position] -> Right position
            [] -> Left (missing ++ name)
            _ -> Left ("ambiguous bound column: " ++ name)
    -- \| 在外层布局绑定并保留作用域
    outerReference name = case slots outer name of
        slot : _ -> Right (outerColumn slot name)
        [] -> Left ("unknown column: " ++ name)
    -- \| 将子查询自由变量绑定到父输入布局
    bindSubquery query = do
        references <- case query of
            Subquery _ names -> mapM reference names
            BoundSubquery _ names references
                | map fst references == names -> mapM rebind references
                | otherwise -> Left "bound subquery references mismatch"
        Right (BoundSubquery (subqueryStatement query) (subqueryRefs query) references)
    -- \| 补回限定名之前先绑定真实物理列
    reference name = do
        let physical = if name `elem` (columns ++ outer) then name else reverse (takeWhile (/= '.') (reverse name))
        boundReference <- bind (Col physical)
        Right (name, boundReference)
    -- \| 重绑定时保留局部或外层身份
    rebind (name, boundReference) = (,) name <$> bind boundReference
    -- \| 绑定两个操作数
    binary constructor a b = constructor <$> bind a <*> bind b
