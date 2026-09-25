module ChuSQL.Algebra.Optimize (optimize, pushProject) where

import ChuSQL.Algebra.Expr (colsInExpr, evalExpr)
import ChuSQL.Algebra.Op (RelOp (..))
import ChuSQL.Model
import ChuSQL.Syntax.AST (Expr (..))

-- 查询优化：谓词下推 + 投影下推 + 常量折叠，跑到不动点。

-- * 谓词下推
-- | 算子会产出哪些列
relOpCols :: Database -> RelOp -> [String]
relOpCols db (Scan mAlias tbl) =
    case lookup tbl db of
        Nothing -> []
        Just t -> map (prefix ++) (colNames t)
  where
    prefix = maybe "" (++ ".") mAlias
relOpCols db (Lookup mAlias tbl _ _) = relOpCols db (Scan mAlias tbl)
relOpCols db (Filter _ x) = relOpCols db x
relOpCols _ (Project ["*"] _) = ["*"]
relOpCols _ (Project cols _) = cols
relOpCols db (Sort _ x) = relOpCols db x
relOpCols db (Limit _ x) = relOpCols db x
relOpCols db (Join l r _) = relOpCols db l ++ relOpCols db r

-- | need 是否都在 have 里
isSubsetOf :: [String] -> [String] -> Bool
isSubsetOf _ ["*"] = True
isSubsetOf need have = all (`elem` have) need

-- | 把 AND 链拆成清单
splitConj :: Expr -> [Expr]
splitConj (And a b) = splitConj a ++ splitConj b
splitConj e = [e]

-- | 把条件清单用 AND 串起来
combineConj :: [Expr] -> Maybe Expr
combineConj [] = Nothing
combineConj (e : es) = Just (foldl And e es)

-- | 条件按列归属分两边
partitionPred :: [String] -> [Expr] -> ([Expr], [Expr])
partitionPred have = foldr pick ([], [])
  where
    pick e (yes, no)
        | colsInExpr e `isSubsetOf` have = (e : yes, no)
        | otherwise = (yes, e : no)

-- | 依次套上若干 Filter
applyFilters :: [Expr] -> RelOp -> RelOp
applyFilters [] op = op
applyFilters (e : es) op = applyFilters es (Filter e op)

-- | Filter 压在 Join 上时下推
pushJoin :: Database -> Expr -> RelOp -> RelOp -> Expr -> RelOp
pushJoin db p l r c =
    let preds = splitConj p
        lCols = relOpCols db l
        rCols = relOpCols db r
        (lPs, rest1) = partitionPred lCols preds
        (rPs, bothPs) = partitionPred rCols rest1
        l' = applyFilters lPs l
        r' = applyFilters rPs r
        joined = Join l' r' c
     in case combineConj bothPs of
            Nothing -> joined
            Just p' -> Filter p' joined

-- | 合并两层 Filter 或挪到 Sort 下
pushOne :: Database -> RelOp -> RelOp
pushOne _ (Filter p (Filter q x)) = Filter (And p q) x
pushOne _ (Filter p (Sort spec x)) = Sort spec (Filter p x)
pushOne _ op = op


-- * 投影下推
-- | 是否要全部列
needsAll :: [String] -> Bool
needsAll = elem allColumns

-- | 叶子算子的投影下推
pushProjectLeaf :: Database -> [String] -> RelOp -> RelOp
pushProjectLeaf db need leaf
    | needsAll need = leaf
    | all (`elem` need) (relOpCols db leaf) = leaf
    | otherwise = Project need leaf

-- | 把需要哪些列往下传
pushProject :: Database -> [String] -> RelOp -> RelOp
pushProject db _ (Project cols x) = case pushProject db cols x of
    Project cols' y | cols' == cols -> Project cols y
    other -> Project cols other
pushProject db need (Filter p x) = Filter p (pushProject db (need ++ colsInExpr p) x)
pushProject db need (Sort spec x) = Sort spec (pushProject db (need ++ map fst spec) x)
pushProject db need (Limit n x) = Limit n (pushProject db need x)
pushProject db need (Join l r c)
    | needsAll need = Join (pushProject db need l) (pushProject db need r) c
    | otherwise =
        let tot = need ++ colsInExpr c
            lCols = relOpCols db l
            rCols = relOpCols db r
            lNeed = if needsAll lCols then ["*"] else [x | x <- tot, x `elem` lCols]
            rNeed = if needsAll rCols then ["*"] else [x | x <- tot, x `elem` rCols]
         in Join (pushProject db lNeed l) (pushProject db rNeed r) c
pushProject db need leaf@(Scan _ _) = pushProjectLeaf db need leaf
pushProject db need leaf@(Lookup _ _ _ _) = pushProjectLeaf db need leaf


-- * 常量折叠
-- | 折一个表达式节点
foldNode :: Expr -> Expr
foldNode e
    | null (colsInExpr e) = case evalExpr e [] of
        Right (VInt n) -> LitInt n
        Right (VStr s) -> LitStr s
        Right (VBool b) -> LitBool b
        Left _ -> e
    | otherwise = e

-- | 自底向上折整棵表达式
foldConstants :: Expr -> Expr
foldConstants (Gt a b) = foldNode (Gt (foldConstants a) (foldConstants b))
foldConstants (Lt a b) = foldNode (Lt (foldConstants a) (foldConstants b))
foldConstants (Eq a b) = foldNode (Eq (foldConstants a) (foldConstants b))
foldConstants (And a b) = foldNode (And (foldConstants a) (foldConstants b))
foldConstants (Or a b) = foldNode (Or (foldConstants a) (foldConstants b))
foldConstants e = e


-- * 执行优化
-- | 单节点重写
rewriteNode :: Database -> RelOp -> RelOp
-- 单列等值条件改成点查：这里是"可以走索引"的意思，
-- 到底走不走得成由存储层回答（这个列上没有索引就退回全表扫描，见 Eval），
-- 所以优化器不必先知道表上到底有哪些索引。
-- 别名要留在节点里：回来的一行必须和 `Scan 别名 表` 长得一样，不然后面取列就对不上了。
rewriteNode _ (Filter (Eq (Col k) (LitInt v)) (Scan mAlias t)) =
    Lookup mAlias t (unqualify mAlias k) v
rewriteNode db (Filter p (Join l r c)) = pushJoin db p l r c
rewriteNode db (Filter p x) = case foldConstants p of
    LitBool True -> x
    p' -> pushOne db (Filter p' x)
rewriteNode _ (Project ["*"] x) = x
rewriteNode db op = pushOne db op

-- | 自底向上遍历一次
optimizeRelOp :: Database -> RelOp -> RelOp
optimizeRelOp db = rewriteNode db . descend
  where
    descend (Filter p x) = Filter p (optimizeRelOp db x)
    descend (Project cols x) = Project cols (optimizeRelOp db x)
    descend (Sort spec x) = Sort spec (optimizeRelOp db x)
    descend (Limit n x) = Limit n (optimizeRelOp db x)
    descend (Join l r c) = Join (optimizeRelOp db l) (optimizeRelOp db r) c
    descend x = x

-- | 反复跑到不动点
optimizePredicates :: Database -> RelOp -> RelOp
optimizePredicates db op =
    let op' = optimizeRelOp db op
     in if op' == op then op else optimizePredicates db op'

-- | 优化总入口
optimize :: Database -> RelOp -> RelOp
optimize db op = pushProject db ["*"] (optimizePredicates db op)
