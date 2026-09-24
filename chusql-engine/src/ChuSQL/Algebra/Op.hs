module ChuSQL.Algebra.Op (RelOp (..), renderPlan) where

import ChuSQL.Syntax.AST (Expr, SortDir)

-- 关系代数算子，以及把算子树渲染成文本。

-- | 算子树的一个节点
data RelOp
    = Scan (Maybe String) String
    | Lookup String Int
    | Filter Expr RelOp
    | Project [String] RelOp
    | Sort [(String, SortDir)] RelOp
    | Limit Int RelOp
    | Join RelOp RelOp Expr
    deriving (Show, Eq)

-- | 渲染成缩进文本
renderPlan :: RelOp -> String
renderPlan = go 0
  where
    go ind op = case op of
        Scan a t -> pad ++ "Scan " ++ show a ++ " " ++ show t
        Lookup t k -> pad ++ "Lookup " ++ show t ++ " " ++ show k
        Filter e x -> pad ++ "Filter " ++ show e ++ "\n" ++ go (ind + 1) x
        Project c x -> pad ++ "Project " ++ show c ++ "\n" ++ go (ind + 1) x
        Sort s x -> pad ++ "Sort " ++ show s ++ "\n" ++ go (ind + 1) x
        Limit n x -> pad ++ "Limit " ++ show n ++ "\n" ++ go (ind + 1) x
        Join l r c -> pad ++ "Join on " ++ show c ++ "\n" ++ go (ind + 1) l ++ "\n" ++ go (ind + 1) r
      where
        pad = replicate (ind * 2) ' '
