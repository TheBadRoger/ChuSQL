module ChuSQL.Core.Engine.Algebra.JoinOrder (reorderJoins) where

import ChuSQL.Core.Engine.Algebra.Cost (pointRows, rangeRows, tableMetaOf)
import ChuSQL.Core.Engine.Algebra.Expr (colsInExpr, hasDivision)
import ChuSQL.Core.Engine.Algebra.Op (RelOp (..), relOpCols)
import ChuSQL.Core.Engine.Syntax.AST (Expr (..), JoinKind (..))
import ChuSQL.Core.Model
import Data.List (partition)

-- 内连接链顺序优化：按统计估算中间结果，只在行序不可观察处重排。

-- | 重排入口：顶层行序可观察
reorderJoins :: Database -> RelOp -> RelOp
reorderJoins db = pass db False

-- | 往下带“行序是否不可观察”，可观察时不动内连接
pass :: Database -> Bool -> RelOp -> RelOp
pass db _ (Sort spec x) = Sort spec (pass db True x)
pass db _ (Limit n x) = Limit n (pass db False x)
pass db _ (Aggregate keys aggs x) = Aggregate keys aggs (pass db False x)
pass db free (Project cols x) = Project cols (pass db free x)
pass db free (Compute items x) = Compute items (pass db free x)
pass db free (Filter p x) = Filter p (pass db free x)
pass db free (Derived a x) = Derived a (pass db free x)
pass db free (Join kind l r c)
    | free
    , Just (leaves, conds) <- innerChain (Join kind l r c)
    , length leaves > 1
    , Just order <- cheaperOrder db leaves conds =
        rebuildChain db (map (pass db free) order) conds
    | otherwise = Join kind (pass db free l) (pass db free r) c
pass _ _ op = op

-- | 把内连接链拆成叶子与条件，遇到外连接就放弃
innerChain :: RelOp -> Maybe ([RelOp], [Expr])
innerChain (Join InnerJoin l r c) = do
    (ls, lcs) <- innerChain l
    (rs, rcs) <- innerChain r
    pure (ls ++ rs, lcs ++ rcs ++ [c])
innerChain (Join _ _ _ _) = Nothing
innerChain op = Just ([op], [])

-- | 叶子算子的估算行数，拿不到统计就是 Nothing
leafRows :: Database -> RelOp -> Maybe Int
leafRows db op = case op of
    Scan _ t _ -> rowCountOf db t
    Lookup _ t c _ -> pointRows <$> tableMetaOf db t <*> pure c
    Range _ t _ _ _ -> rangeRows <$> tableMetaOf db t
    Filter p x -> scaleRows db p x <$> leafRows db x
    Project _ x -> leafRows db x
    _ -> Nothing

-- | 表统计里的行数
rowCountOf :: Database -> String -> Maybe Int
rowCountOf db t = metaRowCount <$> tableMetaOf db t

-- | 过滤之后的大致行数：等值看不同值数，其他按四分之一
scaleRows :: Database -> Expr -> RelOp -> Int -> Int
scaleRows db p x rows = case p of
    Eq (Col k) _ -> case distinctOf db x k of
        Just d | d > 1 -> max 1 (rows `div` d)
        _ -> quarter
    Gt _ _ -> quarter
    Lt _ _ -> quarter
    GtE _ _ -> quarter
    LtE _ _ -> quarter
    NotEq _ _ -> quarter
    QuantCmp _ _ _ _ -> quarter
    And a b -> max 1 (min (scaleRows db a x rows) (scaleRows db b x rows))
    _ -> quarter
  where
    quarter = max 1 (rows `div` 4)

-- | 叶子对应的别名与表名
leafSource :: RelOp -> Maybe (Maybe String, String)
leafSource (Scan a t _) = Just (a, t)
leafSource (Lookup a t _ _) = Just (a, t)
leafSource (Range a t _ _ _) = Just (a, t)
leafSource (Filter _ x) = leafSource x
leafSource (Project _ x) = leafSource x
leafSource _ = Nothing

-- | 某列在叶子统计里的不同值数
distinctOf :: Database -> RelOp -> String -> Maybe Int
distinctOf db leaf col = do
    (alias, t) <- leafSource leaf
    m <- tableMetaOf db t
    num <- lookup (unqualify alias col) [(c, n) | (c, n, _) <- metaDistinct m]
    pure num

-- | 若干叶子里某列最大的不同值数
distinctOn :: Database -> [RelOp] -> String -> Maybe Int
distinctOn db leaves col = case [d | leaf <- leaves, Just d <- [distinctOf db leaf col]] of
    [] -> Nothing
    ds -> Just (maximum ds)

-- | 两侧等值键里较大的不同值数
keyDistinct :: Database -> [RelOp] -> RelOp -> [Expr] -> Maybe Int
keyDistinct db have x conds = case [v | Eq (Col a) (Col b) <- conds, Just v <- [pair a b]] of
    (v : _) -> Just v
    [] -> Nothing
  where
    haveCols = concatMap (relOpCols db) have
    xCols = relOpCols db x
    -- | 这一对列是不是两侧之间的等值键
    pair a b
        | a `elem` haveCols, b `elem` xCols = bigger (distinctOn db have a) (distinctOf db x b)
        | b `elem` haveCols, a `elem` xCols = bigger (distinctOn db have b) (distinctOf db x a)
        | otherwise = Nothing
    -- | 两个可选统计取大的那个
    bigger (Just a) (Just b) = Just (max a b)
    bigger (Just a) Nothing = Just a
    bigger Nothing (Just b) = Just b
    bigger Nothing Nothing = Nothing

-- | 一次连接之后的大致行数
stepRows :: Database -> [RelOp] -> Int -> RelOp -> [Expr] -> Int
stepRows db have acc x conds = case keyDistinct db have x conds of
    Just v | v > 1 -> max 1 (acc * rows `div` v)
    _ -> acc * rows
  where
    rows = maybe 1 id (leafRows db x)

-- | 一批叶子按给定顺序连接，累加每一步的中间结果
chainCost :: Database -> [RelOp] -> [Expr] -> Int
chainCost db (l : ls) conds = go [l] (maybe 1 id (leafRows db l)) ls
  where
    -- | 逐步接上后面的叶子
    go _ _ [] = 0
    go have acc (x : xs) =
        let card = stepRows db have acc x conds
         in card + go (have ++ [x]) card xs
chainCost _ [] _ = 0

-- | 贪心扩展一个顺序：每次挑中间结果最小的下一张
growOrder :: Database -> [RelOp] -> [Expr] -> [Int] -> [Int] -> [Int]
growOrder db leaves conds have rest = case rest of
    [] -> have
    _ ->
        let next = pick rest
         in growOrder db leaves conds (have ++ [next]) (filter (/= next) rest)
  where
    -- | 候选里成本最低的那个，同价取序号小的
    pick (i : is) = foldl cheaper i is
    pick [] = 0
    cheaper i j = if costOf j < costOf i then j else i
    -- | 这个前缀的估算成本
    costOf i = chainCost db [leaves !! k | k <- have ++ [i]] conds

-- | 每个起点贪心一遍，取最便宜的顺序
bestOrder :: Database -> [RelOp] -> [Expr] -> [RelOp]
bestOrder db leaves conds = pick (map start [0 .. n - 1])
  where
    n = length leaves
    -- | 从第 i 张表开始的贪心顺序
    start i = [leaves !! k | k <- growOrder db leaves conds [i] (filter (/= i) [0 .. n - 1])]
    -- | 成本最低的那个
    pick (o : os) = foldl cheaper o os
    pick [] = []
    cheaper o o' = if cost o' < cost o then o' else o
    -- | 一个顺序的估算成本
    cost o = chainCost db o conds

-- | 统计齐全、没有除法且确实更省时给出新顺序
cheaperOrder :: Database -> [RelOp] -> [Expr] -> Maybe [RelOp]
cheaperOrder db leaves conds
    | any hasDivision conds = Nothing
    | any unknown leaves = Nothing
    | bestCost < textCost = Just best
    | otherwise = Nothing
  where
    -- | 这个叶子是不是拿不到统计
    unknown x = case leafRows db x of
        Just _ -> False
        Nothing -> True
    best = bestOrder db leaves conds
    bestCost = chainCost db best conds
    textCost = chainCost db leaves conds

-- | 按新顺序重建左深链，条件挂到两侧都齐的那一步
rebuildChain :: Database -> [RelOp] -> [Expr] -> RelOp
rebuildChain db (l : ls) conds = go (relOpCols db l) l ls conds
  where
    -- | 逐个接上后面的叶子
    go _ acc [] rest = applyConds rest acc
    go have acc (x : xs) rest =
        let have' = have ++ relOpCols db x
            (now, later) = partition (\c -> colsInExpr c `subsetOf` have') rest
         in go have' (Join InnerJoin acc x (conjOf now)) xs later
    -- | 若干条件用 AND 串起来
    conjOf [] = LitBool True
    conjOf (c : cs) = foldl And c cs
rebuildChain _ [] _ = Unit

-- | 挂不上去的条件套成 Filter
applyConds :: [Expr] -> RelOp -> RelOp
applyConds [] op = op
applyConds (c : cs) op = Filter c (applyConds cs op)

-- | 条件的列是不是都在可用列里
subsetOf :: [String] -> [String] -> Bool
subsetOf need have = all (`elem` have) need
