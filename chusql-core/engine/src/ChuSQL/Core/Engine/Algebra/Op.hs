module ChuSQL.Core.Engine.Algebra.Op (RelOp (..), SemiKind (..), relOpCols, relOpLayout, hasBoundPlanColumns, renderPlan) where

import ChuSQL.Core.Engine.Algebra.Expr (hasBoundColumns)
import ChuSQL.Core.Engine.Syntax.AST (ColumnId (..), Expr (..), JoinKind (..), SortDir, SourceId (..), Statement)
import ChuSQL.Core.Model
import Data.Maybe (fromMaybe)

-- 关系代数算子定义、输出列推导与算子树文本渲染。

-- | 区分存在匹配、无匹配与 NULL 感知排除
data SemiKind = Matching | NonMatching | NotIn deriving (Show, Eq)

data RelOp
    = Scan (Maybe String) String (Maybe [String])
    | Lookup (Maybe String) String String Value
    | Range (Maybe String) String String (Maybe (Value, Bool)) (Maybe (Value, Bool))
    | Filter Expr RelOp
    | Project [String] RelOp
    | Compute [(String, Expr)] RelOp
    | Aggregate [String] [(String, Expr)] RelOp
    | BoundAggregate [(String, Expr)] [(String, Expr)] RelOp
    | Distinct RelOp
    | Unit
    | Sort [(String, SortDir)] RelOp
    | BoundSort [(Expr, SortDir)] RelOp
    | Limit Int RelOp
    | Join JoinKind RelOp RelOp Expr
    | -- | 末项仅保存独立 IN 的物化身份
      SemiJoin SemiKind Expr Expr RelOp RelOp (Maybe Statement)
    | Derived (Maybe String) RelOp
    deriving (Show, Eq)

-- | 算子会产出哪些列
relOpCols :: Database -> RelOp -> [String]
relOpCols _ Unit = []
relOpCols _ (Compute items _) = map fst items
relOpCols _ (Aggregate keys aggs _) = keys ++ map fst aggs
relOpCols _ (BoundAggregate keys aggs _) = map fst keys ++ map fst aggs
relOpCols _ (Scan mAlias _ (Just cols)) = map (qualify mAlias) cols
relOpCols db (Scan mAlias tbl Nothing) =
    case lookup tbl db of
        Nothing -> []
        Just t -> map (prefix ++) (colNames t)
  where
    prefix = maybe "" (++ ".") mAlias
relOpCols db (Lookup mAlias tbl _ _) = relOpCols db (Scan mAlias tbl Nothing)
relOpCols db (Range mAlias tbl _ _ _) = relOpCols db (Scan mAlias tbl Nothing)
relOpCols db (Filter _ x) = relOpCols db x
relOpCols _ (Project ["*"] _) = ["*"]
relOpCols _ (Project cols _) = cols
relOpCols db (Distinct x) = relOpCols db x
relOpCols db (Sort _ x) = relOpCols db x
relOpCols db (BoundSort _ x) = relOpCols db x
relOpCols db (Limit _ x) = relOpCols db x
relOpCols db (Join _ l r _) = relOpCols db l ++ relOpCols db r
relOpCols db (SemiJoin _ _ _ l _ _) = relOpCols db l
relOpCols db (Derived mAlias x) = map (qualify mAlias) (relOpCols db x)

-- | 为来源实例和输出列分配查询内身份
relOpLayout :: Database -> [Int] -> RelOp -> [(ColumnId, String)]
relOpLayout db path op = case op of
    Scan alias table selected ->
        [(identity slot, qualify alias name) | name <- fromMaybe names selected, (slot, key) <- zip [0 ..] names, key == name]
      where
        names = maybe [] colNames (lookup table db)
    Lookup alias table _ _ -> layout (Scan alias table Nothing)
    Range alias table _ _ _ -> layout (Scan alias table Nothing)
    Compute items child -> outputs items child
    Project ["*"] child -> childLayout child
    Project names child -> outputs [(name, Col name) | name <- names] child
    Aggregate keys items child -> outputs ([(name, Col name) | name <- keys] ++ items) child
    BoundAggregate keys items child -> outputs (keys ++ items) child
    Join _ left right _ -> relOpLayout db (path ++ [0]) left ++ relOpLayout db (path ++ [1]) right
    SemiJoin _ _ _ left _ _ -> relOpLayout db (path ++ [0]) left
    Derived alias child -> [(cid, qualify alias name) | (cid, name) <- layout child]
    Filter _ child -> layout child
    Distinct child -> layout child
    Sort _ child -> layout child
    BoundSort _ child -> layout child
    Limit _ child -> layout child
    Unit -> []
  where
    -- \| 构造当前来源的列身份
    identity = ColumnId (SourceId path)
    -- \| 保留透传算子的来源路径
    layout = relOpLayout db path
    -- \| 为产生输出的算子隔离输入来源
    childLayout = relOpLayout db (path ++ [0])
    -- \| 直接引用保留来源，计算列使用新身份
    outputs items child =
        [(reference slot expression, name) | (slot, (name, expression)) <- zip [0 ..] items]
      where
        input = childLayout child
        -- \| 从输入布局提取直接引用的身份
        reference slot expression = case expression of
            Col name -> case [cid | (cid, key) <- input, key == name] of
                cid : _ -> cid
                [] -> identity slot
            ColumnRef cid _ -> cid
            ColumnSlot position name -> case drop position input of
                (cid, key) : _ | position >= 0, key == name -> cid
                _ -> identity slot
            _ -> identity slot

-- | 检查算子树是否依赖已绑定的列布局
hasBoundPlanColumns :: RelOp -> Bool
hasBoundPlanColumns op = case op of
    Filter expression child -> hasBoundColumns expression || hasBoundPlanColumns child
    Compute items child -> any (hasBoundColumns . snd) items || hasBoundPlanColumns child
    Aggregate _ items child -> any (hasBoundColumns . snd) items || hasBoundPlanColumns child
    BoundAggregate keys items child -> any (hasBoundColumns . snd) (keys ++ items) || hasBoundPlanColumns child
    BoundSort items child -> any (hasBoundColumns . fst) items || hasBoundPlanColumns child
    Join _ left right condition -> hasBoundColumns condition || children left right
    SemiJoin _ leftKey rightKey left right _ -> hasBoundColumns leftKey || hasBoundColumns rightKey || children left right
    Project _ child -> hasBoundPlanColumns child
    Distinct child -> hasBoundPlanColumns child
    Sort _ child -> hasBoundPlanColumns child
    Limit _ child -> hasBoundPlanColumns child
    Derived _ child -> hasBoundPlanColumns child
    _ -> False
  where
    -- \| 检查两个输入的绑定状态
    children left right = hasBoundPlanColumns left || hasBoundPlanColumns right

-- | 连接的显示名
kindLabel :: JoinKind -> String
kindLabel InnerJoin = "Join"
kindLabel LeftJoin = "LeftJoin"

-- | 渲染成缩进文本
renderPlan :: RelOp -> String
renderPlan = go 0
  where
    -- \| 按缩进渲染一个算子
    go ind op = case op of
        Scan a t cols -> pad ++ "Scan " ++ show a ++ " " ++ show t ++ maybe "" ((" columns=" ++) . show) cols
        Lookup _ t c k -> pad ++ "Lookup " ++ show t ++ "." ++ show c ++ " = " ++ show k
        Range _ t c lo hi -> pad ++ "Range " ++ show t ++ "." ++ show c ++ " " ++ show lo ++ " .. " ++ show hi
        Filter e x -> pad ++ "Filter " ++ show e ++ "\n" ++ go (ind + 1) x
        Project c x -> pad ++ "Project " ++ show c ++ "\n" ++ go (ind + 1) x
        Compute c x -> pad ++ "Compute " ++ show c ++ "\n" ++ go (ind + 1) x
        Aggregate keys aggs x ->
            pad
                ++ "Aggregate keys="
                ++ show keys
                ++ " aggs="
                ++ show [label ++ " <- " ++ show e | (label, e) <- aggs]
                ++ "\n"
                ++ go (ind + 1) x
        Distinct x -> pad ++ "Distinct\n" ++ go (ind + 1) x
        BoundAggregate keys aggs x ->
            pad
                ++ "BoundAggregate keys="
                ++ show keys
                ++ " aggs="
                ++ show aggs
                ++ "\n"
                ++ go (ind + 1) x
        Unit -> pad ++ "Unit"
        Sort s x -> pad ++ "Sort " ++ show s ++ "\n" ++ go (ind + 1) x
        BoundSort s x -> pad ++ "BoundSort " ++ show s ++ "\n" ++ go (ind + 1) x
        Limit n x -> pad ++ "Limit " ++ show n ++ "\n" ++ go (ind + 1) x
        Join k l r c -> pad ++ kindLabel k ++ " on " ++ show c ++ "\n" ++ go (ind + 1) l ++ "\n" ++ go (ind + 1) r
        SemiJoin k left right l r _ ->
            pad
                ++ "SemiJoin "
                ++ show k
                ++ " keys="
                ++ show (left, right)
                ++ "\n"
                ++ go (ind + 1) l
                ++ "\n"
                ++ go (ind + 1) r
        Derived a x -> pad ++ "Derived " ++ show a ++ "\n" ++ go (ind + 1) x
      where
        pad = replicate (ind * 2) ' '
