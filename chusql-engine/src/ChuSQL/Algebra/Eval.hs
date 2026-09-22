module ChuSQL.Algebra.Eval (evalRelOp) where

import ChuSQL.Algebra.Expr (evalCondForRow)
import ChuSQL.Algebra.Op
import ChuSQL.Algebra.Sort (sortRows)
import ChuSQL.Model
import Control.Monad (filterM)

-- 投影
project :: [String] -> Row -> Row
project ["*"] row = row
project cs row = [(c, v) | (c, v) <- row, c `elem` cs]

-- 执行关系代数运算
addPrefix :: String -> Row -> Row
addPrefix prefix = map (\(k, v) -> (prefix ++ k, v))

evalRelOp :: Database -> RelOp -> Either String [Row]
evalRelOp db (Scan mAlias tbl) = do
    table <- lookupTable db tbl
    let prefix = maybe "" (++ ".") mAlias
    Right (map (addPrefix prefix) (tableRows table))
evalRelOp db (Filter e op) = do
    rows <- evalRelOp db op
    filterM (evalCondForRow e) rows
evalRelOp db (Project cols op) = do
    rows <- evalRelOp db op
    Right (map (project cols) rows)
evalRelOp db (Sort spec op) = do
    rows <- evalRelOp db op
    Right (sortRows spec rows)
evalRelOp db (Limit n op) = do
    rows <- evalRelOp db op
    Right (take n rows)
evalRelOp db (Join left right cond) = do
    lrows <- evalRelOp db left
    rrows <- evalRelOp db right
    let cross = [l ++ r | l <- lrows, r <- rrows] -- 笛卡尔积
    filterM (evalCondForRow cond) cross
