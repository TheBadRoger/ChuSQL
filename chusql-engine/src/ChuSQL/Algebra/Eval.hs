module ChuSQL.Algebra.Eval (evalRelOp, evalRelOpM) where

import ChuSQL.Algebra.Expr (evalCondForRow)
import ChuSQL.Algebra.Op
import ChuSQL.Algebra.Sort (sortRows)
import ChuSQL.Model
import ChuSQL.Storage (MonadStorage (..))
import ChuSQL.Syntax.AST (Expr (..))
import Control.Monad (filterM)

-- 执行：按关系算子树真正算出结果行（Scan 阶段会给列名加上别名前缀）。

-- 投影：只保留清单里的列；哨兵 "*" 表示全部列。
project :: [String] -> Row -> Row
project ["*"] row = row
project cs row = [(c, v) | (c, v) <- row, c `elem` cs]

-- 给一行里的每个列名加上别名前缀（如 "u."）。
addPrefix :: String -> Row -> Row
addPrefix prefix = map (\(k, v) -> (prefix ++ k, v))

-- 索引查找的约定：主键列名是 "id"（优化器只在这种情况下才把过滤改写成 Lookup）。
keyCondition :: Int -> Expr
keyCondition k = Eq (Col "id") (LitInt k)

-- 扫描一张表；有别名就给每个列名加上前缀，JOIN 两边同名列靠它区分。
evalScan :: Database -> Maybe String -> String -> Either String [Row]
evalScan db mAlias tbl = do
    table <- lookupTable db tbl
    let prefix = maybe "" (++ ".") mAlias
    Right (map (addPrefix prefix) (tableRows table))

-- 执行关系代数运算：Scan / Lookup / Filter / Project / Sort / Limit / Join 各一支。
evalRelOp :: Database -> RelOp -> Either String [Row]
evalRelOp db (Scan mAlias tbl) = evalScan db mAlias tbl
evalRelOp db (Lookup tbl k) = do
    rows <- evalScan db Nothing tbl
    filterM (evalCondForRow (keyCondition k)) rows
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

-- | Monadic 版本：Lookup 直接问存储要一行（走索引），其余算子照原样递归。
evalRelOpM :: (MonadStorage m) => Database -> RelOp -> m (Either String [Row])
evalRelOpM _ (Lookup t k) = do
    result <- lookupByKey t k
    pure $ case result of
        Left e -> Left e
        Right Nothing -> Right []
        Right (Just r) -> Right [r]
evalRelOpM db (Scan a t) = pure (evalScan db a t)
evalRelOpM db (Filter p x) = do
    result <- evalRelOpM db x
    pure $ do
        rows <- result
        filterM (evalCondForRow p) rows
evalRelOpM db (Project cols x) = do
    result <- evalRelOpM db x
    pure (map (project cols) <$> result)
evalRelOpM db (Sort specs x) = do
    result <- evalRelOpM db x
    pure (sortRows specs <$> result)
evalRelOpM db (Limit n x) = do
    result <- evalRelOpM db x
    pure (take n <$> result)
evalRelOpM db (Join l r c) = do
    ls <- evalRelOpM db l
    rs <- evalRelOpM db r
    pure $ do
        leftRows <- ls
        rightRows <- rs
        let cross = [x ++ y | x <- leftRows, y <- rightRows]
        filterM (evalCondForRow c) cross
