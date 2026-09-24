module ChuSQL.Algebra.Eval (evalRelOp, evalRelOpM) where

import ChuSQL.Algebra.Expr (evalCondForRow)
import ChuSQL.Algebra.Op
import ChuSQL.Algebra.Sort (sortRows)
import ChuSQL.Model
import ChuSQL.Storage (MonadStorage (..))
import ChuSQL.Syntax.AST (Expr (..))
import Control.Monad (filterM)

-- 执行：按算子树算出结果行（Scan 阶段会加别名前缀）。

-- * 工具
-- | 只保留清单里的列
project :: [String] -> Row -> Row
project ["*"] row = row
project cs row = [(c, v) | (c, v) <- row, c `elem` cs]

-- | 给列名加别名前缀
addPrefix :: String -> Row -> Row
addPrefix prefix = map (\(k, v) -> (prefix ++ k, v))

-- | 主键匹配条件（id = k）
keyCondition :: Int -> Expr
keyCondition k = Eq (Col "id") (LitInt k)

-- | 扫描一张表
evalScan :: Database -> Maybe String -> String -> Either String [Row]
evalScan db mAlias tbl = do
    table <- lookupTable db tbl
    let prefix = maybe "" (++ ".") mAlias
    Right (map (addPrefix prefix) (tableRows table))

-- * 求值
-- | 纯求值（不需要存储）
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
    let cross = [l ++ r | l <- lrows, r <- rrows]
    filterM (evalCondForRow cond) cross

-- | 单子求值：Lookup 问存储
evalRelOpM :: (MonadStorage m) => Database -> RelOp -> m (Either String [Row])
evalRelOpM db (Scan a t) = pure (evalScan db a t)
evalRelOpM _ (Lookup t k) = do
    result <- lookupByKey t k
    pure $ case result of
        Left e -> Left e
        Right Nothing -> Right []
        Right (Just r) -> Right [r]
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
