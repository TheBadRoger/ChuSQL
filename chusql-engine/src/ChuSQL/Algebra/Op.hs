module ChuSQL.Algebra.Op (RelOp (..), renderPlan) where

import ChuSQL.Syntax.AST (Expr, SortDir)

-- 关系代数算子，以及把算子树渲染成文本。

-- | 算子树的一个节点
data RelOp
    = Scan (Maybe String) String
    | -- \| 按某一列的值点查一行：别名、表、列、值。
      -- 只是"这里可以走点查"的意思，到底有没有索引由存储层回答
      -- （那个列上没有索引就退回全表扫描，见 Eval）。
      -- 别名要留着，回来的一行才能和 `Scan 别名 表` 长得一样。
      Lookup (Maybe String) String String Int
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
        Lookup _ t c k -> pad ++ "Lookup " ++ show t ++ "." ++ show c ++ " = " ++ show k
        Filter e x -> pad ++ "Filter " ++ show e ++ "\n" ++ go (ind + 1) x
        Project c x -> pad ++ "Project " ++ show c ++ "\n" ++ go (ind + 1) x
        Sort s x -> pad ++ "Sort " ++ show s ++ "\n" ++ go (ind + 1) x
        Limit n x -> pad ++ "Limit " ++ show n ++ "\n" ++ go (ind + 1) x
        Join l r c -> pad ++ "Join on " ++ show c ++ "\n" ++ go (ind + 1) l ++ "\n" ++ go (ind + 1) r
      where
        pad = replicate (ind * 2) ' '
