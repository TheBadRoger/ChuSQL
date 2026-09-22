module ChuSQL.Algebra.Op (RelOp (..), renderPlan) where

import ChuSQL.Syntax.AST (Expr, SortDir)

-- 关系代数算子
data RelOp
    = Scan (Maybe String) String
    | Filter Expr RelOp
    | Project [String] RelOp
    | Sort [(String, SortDir)] RelOp
    | Limit Int RelOp
    | Join RelOp RelOp Expr
    deriving (Show, Eq)

-- | 把算子树画成缩进文本（演示和调试用）
renderPlan :: RelOp -> String
renderPlan = go 0
  where
    go ind op = case op of
        Scan a t -> pad ++ "Scan " ++ show a ++ " " ++ show t
        Filter e x -> pad ++ "Filter " ++ show e ++ "\n" ++ go (ind + 1) x
        Project c x -> pad ++ "Project " ++ show c ++ "\n" ++ go (ind + 1) x
        Sort s x -> pad ++ "Sort " ++ show s ++ "\n" ++ go (ind + 1) x
        Limit n x -> pad ++ "Limit " ++ show n ++ "\n" ++ go (ind + 1) x
        Join l r c -> pad ++ "Join on " ++ show c ++ "\n" ++ go (ind + 1) l ++ "\n" ++ go (ind + 1) r
      where
        pad = replicate (ind * 2) ' '
