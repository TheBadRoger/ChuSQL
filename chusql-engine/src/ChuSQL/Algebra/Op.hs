module ChuSQL.Algebra.Op where

import ChuSQL.SQLSyntax.AST (Expr, SortDir)

-- 关系代数算子
data RelOp
    = Scan (Maybe String) String
    | Filter Expr RelOp
    | Project [String] RelOp
    | Sort [(String, SortDir)] RelOp
    | Limit Int RelOp
    | Join RelOp RelOp Expr
    deriving (Show, Eq)
