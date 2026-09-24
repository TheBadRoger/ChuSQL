module ChuSQL.Algebra.Eval (evalRelOp, evalRelOpM) where

import ChuSQL.Algebra.Expr (evalCondForRow)
import ChuSQL.Algebra.Op
import ChuSQL.Algebra.Sort (sortRows)
import ChuSQL.Model
import ChuSQL.Storage (MonadStorage (..))
import ChuSQL.Syntax.AST (Expr (..))
import Control.Monad (filterM, foldM)

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

-- | 把两边的行按条件配对（边配边筛，不先建整张积表）
filterPairs :: (Row -> Either String Bool) -> [Row] -> [Row] -> Either String [Row]
filterPairs cond lrows rrows = go lrows []
  where
    go [] acc = Right (reverse acc)
    go (l : ls) acc = do
        acc' <- foldM (keep l) acc rrows
        go ls acc'
    keep l acc r = do
        let row = l ++ r
        ok <- cond row
        pure (if ok then row : acc else acc)

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
    filterPairs (evalCondForRow cond) lrows rrows

-- | 单子求值：Lookup 问存储
evalRelOpM :: (MonadStorage m) => RelOp -> m (Either String [Row])
evalRelOpM (Scan mAlias t) = do
    result <- scan t
    pure $ do
        rows <- result
        case mAlias of
            Nothing -> Right rows
            Just a -> Right (map (addPrefix (a ++ ".")) rows)
evalRelOpM (Lookup t k) = do
    result <- lookupByKey t k
    pure $ case result of
        Left e -> Left e
        Right Nothing -> Right []
        Right (Just row) -> Right [row]
evalRelOpM (Filter p x) = do
    result <- evalRelOpM x
    pure $ do
        rows <- result
        filterM (evalCondForRow p) rows
evalRelOpM (Project cols x) = do
    result <- evalRelOpM x
    pure (fmap (map (project cols)) result)
evalRelOpM (Sort spec x) = do
    result <- evalRelOpM x
    pure (fmap (sortRows spec) result)
evalRelOpM (Limit n x) = do
    result <- evalRelOpM x
    pure (fmap (take n) result)
evalRelOpM (Join l r cond) = do
    ls <- evalRelOpM l
    rs <- evalRelOpM r
    pure $ do
        left <- ls
        right <- rs
        filterPairs (evalCondForRow cond) left right
