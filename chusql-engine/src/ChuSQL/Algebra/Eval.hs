module ChuSQL.Algebra.Eval (evalRelOp, evalRelOpM) where

import ChuSQL.Algebra.Expr (evalCondForRow)
import ChuSQL.Algebra.Op
import ChuSQL.Algebra.Sort (sortRows)
import ChuSQL.Model
import ChuSQL.Storage (IndexResult (..), MonadStorage (..))
import ChuSQL.Syntax.AST (Expr (..))
import Control.Monad (filterM, foldM)
import qualified Data.HashMap.Strict as HM

-- 执行：按算子树算出结果行（Scan 阶段会加别名前缀）。

-- * 工具

-- | 只保留清单里的列
project :: [String] -> Row -> Row
project ["*"] row = row
project cs row = [(c, v) | (c, v) <- row, c `elem` cs]

-- | 给列名加别名前缀
addPrefix :: Maybe String -> Row -> Row
addPrefix mAlias = map (\(k, v) -> (qualify mAlias k, v))

-- | 扫描一张表
evalScan :: Database -> Maybe String -> String -> Either String [Row]
evalScan db mAlias tbl = do
    table <- lookupTable db tbl
    Right (map (addPrefix mAlias) (tableRows table))

-- | 点查退化成"扫描 + 按条件筛"时用的条件：`列 = 值`（列名带上别名前缀）
lookupCondition :: Maybe String -> String -> Int -> Expr
lookupCondition mAlias col k = Eq (Col (qualify mAlias col)) (LitInt k)

-- * 等值连接

-- | 等值连接的两侧键：左行取哪列、右行取哪列
data EquiKeys = EquiKeys
    { ekLeft :: String
    , ekRight :: String
    }

-- | 一批行的列名（空批给空清单，此时一律退回嵌套循环）
rowsCols :: [Row] -> [String]
rowsCols (r : _) = map fst r
rowsCols [] = []

-- | 认出 a = b 这种两列等值条件，并且能分清谁在左谁在右
equiKeys :: [Row] -> [Row] -> Expr -> Maybe EquiKeys
equiKeys lrows rrows (Eq (Col a) (Col b))
    | a /= b, a `elem` lcols, b `elem` rcols = Just (EquiKeys a b)
    | a /= b, b `elem` lcols, a `elem` rcols = Just (EquiKeys b a)
  where
    lcols = rowsCols lrows
    rcols = rowsCols rrows
equiKeys _ _ _ = Nothing

-- | 哈希连接：右表建哈希表，左表逐行探测。
-- 输出顺序与嵌套循环一致（左表顺序为主，右表顺序为辅）。
hashJoin :: EquiKeys -> [Row] -> [Row] -> [Row]
hashJoin keys lrows rrows = concatMap probe lrows
  where
    -- 同一键的右行按原顺序排在桶里
    buckets =
        HM.fromListWith
            (flip (++))
            [(v, [r]) | r <- rrows, Just v <- [lookup (ekRight keys) r]]
    probe l = case lookup (ekLeft keys) l of
        Nothing -> []
        Just v -> [l ++ r | r <- HM.lookupDefault [] v buckets]

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

-- | 一次连接：能认出等值键就走哈希，否则退回嵌套循环
joinRows :: Expr -> [Row] -> [Row] -> Either String [Row]
joinRows cond lrows rrows = case equiKeys lrows rrows cond of
    Just keys -> Right (hashJoin keys lrows rrows)
    Nothing -> filterPairs (evalCondForRow cond) lrows rrows

-- * 求值

-- | 纯求值（不需要存储）
evalRelOp :: Database -> RelOp -> Either String [Row]
evalRelOp db (Scan mAlias tbl) = evalScan db mAlias tbl
-- 纯求值没有索引可问：退回"扫描 + 按条件筛"，和未优化时的 Filter 完全等价
evalRelOp db (Lookup mAlias tbl col k) = do
    rows <- evalScan db mAlias tbl
    filterM (evalCondForRow (lookupCondition mAlias col k)) rows
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
    joinRows cond lrows rrows

-- | 单子求值：Lookup 先问存储有没有索引，没有就退回扫描
evalRelOpM :: (MonadStorage m) => RelOp -> m (Either String [Row])
evalRelOpM (Scan mAlias t) = do
    result <- scan t
    pure $ do
        rows <- result
        case mAlias of
            Nothing -> Right rows
            Just _ -> Right (map (addPrefix mAlias) rows)
evalRelOpM (Lookup mAlias t col k) = do
    result <- lookupByColumn t col k
    case result of
        Left e -> pure (Left e)
        -- 这个列上没有索引：老老实实扫一遍（结果和未优化时一样）
        Right NoIndex -> do
            rows <- evalRelOpM (Scan mAlias t)
            pure $ do
                rs <- rows
                filterM (evalCondForRow (lookupCondition mAlias col k)) rs
        Right (IndexRow Nothing) -> pure (Right [])
        Right (IndexRow (Just row)) -> pure (Right [addPrefix mAlias row])
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
        joinRows cond left right
