module ChuSQL.Core.Engine.Algebra.Op (RelOp (..), relOpCols, renderPlan) where

import ChuSQL.Core.Model
import ChuSQL.Core.Engine.Syntax.AST (Expr, JoinKind (..), SortDir)

-- 关系代数算子定义、输出列推导与算子树文本渲染。

data RelOp
    = Scan (Maybe String) String (Maybe [String])
    | Lookup (Maybe String) String String Value
    | Range (Maybe String) String String (Maybe (Value, Bool)) (Maybe (Value, Bool))
    | Filter Expr RelOp
    | Project [String] RelOp
    | Compute [(String, Expr)] RelOp
    | Aggregate [String] [(String, Expr)] RelOp
    | Unit
    | Sort [(String, SortDir)] RelOp
    | Limit Int RelOp
    | Join JoinKind RelOp RelOp Expr
    deriving (Show, Eq)

-- | 算子会产出哪些列
relOpCols :: Database -> RelOp -> [String]
relOpCols _ Unit = []
relOpCols _ (Compute items _) = map fst items
relOpCols _ (Aggregate keys aggs _) = keys ++ map fst aggs
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
relOpCols db (Sort _ x) = relOpCols db x
relOpCols db (Limit _ x) = relOpCols db x
relOpCols db (Join _ l r _) = relOpCols db l ++ relOpCols db r

-- | 连接的显示名
kindLabel :: JoinKind -> String
kindLabel InnerJoin = "Join"
kindLabel LeftJoin = "LeftJoin"

-- | 渲染成缩进文本
renderPlan :: RelOp -> String
renderPlan = go 0
  where
    -- | 按缩进渲染一个算子
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
        Unit -> pad ++ "Unit"
        Sort s x -> pad ++ "Sort " ++ show s ++ "\n" ++ go (ind + 1) x
        Limit n x -> pad ++ "Limit " ++ show n ++ "\n" ++ go (ind + 1) x
        Join k l r c -> pad ++ kindLabel k ++ " on " ++ show c ++ "\n" ++ go (ind + 1) l ++ "\n" ++ go (ind + 1) r
      where
        pad = replicate (ind * 2) ' '
