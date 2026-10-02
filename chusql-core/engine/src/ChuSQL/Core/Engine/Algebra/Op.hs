module ChuSQL.Core.Engine.Algebra.Op (RelOp (..), renderPlan) where

import ChuSQL.Core.Model (Value)
import ChuSQL.Core.Engine.Syntax.AST (Expr, JoinKind (..), SortDir)

-- 关系代数算子定义与算子树文本渲染。

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
