module ChuSQL.Core.Engine.Algebra.Optimize (optimize, pushProject) where

import ChuSQL.Core.Engine.Algebra.Cost (indexCheaper, pointRows, rangeRows, tableMetaOf)
import ChuSQL.Core.Engine.Algebra.Expr (colsInExpr, evalExpr, hasDivision)
import ChuSQL.Core.Engine.Algebra.JoinOrder (reorderJoins)
import ChuSQL.Core.Engine.Algebra.Op (RelOp (..), SemiKind (..), relOpCols)
import ChuSQL.Core.Engine.Algebra.Planner (translate)
import ChuSQL.Core.Engine.Syntax.AST (Expr (..), FromClause (..), JoinKind (..), Statement (..), Subquery (..))
import ChuSQL.Core.Model
import Data.List (nub)

-- 查询优化：谓词下推、投影下推、常量折叠、扫描方式与连接顺序选择。

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
    -- \| 按列归属收到左边或右边
    pick e (yes, no)
        | colsInExpr e `isSubsetOf` have = (e : yes, no)
        | otherwise = (yes, e : no)

-- | 依次套上若干 Filter
applyFilters :: [Expr] -> RelOp -> RelOp
applyFilters es op = foldl (flip Filter) op es

-- | Filter 压在内连接上时下推
pushJoin :: Database -> Expr -> RelOp -> RelOp -> Expr -> RelOp
pushJoin db p l r c =
    let preds = splitConj p
        lCols = relOpCols db l
        rCols = relOpCols db r
        (lPs, rest1) = partitionPred lCols preds
        (rPs, bothPs) = partitionPred rCols rest1
        l' = applyFilters lPs l
        r' = applyFilters rPs r
        joined = Join InnerJoin l' r' c
     in case combineConj bothPs of
            Nothing -> joined
            Just p' -> Filter p' joined

-- | Filter 压在左连接上时只下推纯左表的谓词
pushLeftJoin :: Database -> Expr -> RelOp -> RelOp -> Expr -> RelOp
pushLeftJoin db p l r c =
    let (lPs, rest) = partitionPred (relOpCols db l) (splitConj p)
        joined = Join LeftJoin (applyFilters lPs l) r c
     in case combineConj rest of
            Nothing -> joined
            Just p' -> Filter p' joined

-- | 合并两层 Filter 或挪到 Sort 下
pushOne :: Database -> RelOp -> RelOp
pushOne _ (Filter p (Filter q x))
    | not (hasDivision p || hasDivision q) = Filter (And p q) x
pushOne _ (Filter p (Sort spec x))
    | not (hasDivision p) = Sort spec (Filter p x)
pushOne _ op = op

-- | 检查子计划中可能报错的除法
planHasDivision :: RelOp -> Bool
planHasDivision (Filter p x) = hasDivision p || planHasDivision x
planHasDivision (Join _ l r c) = hasDivision c || planHasDivision l || planHasDivision r
planHasDivision (SemiJoin _ left right l r _) = hasDivision left || hasDivision right || planHasDivision l || planHasDivision r
planHasDivision (Compute items x) = any (hasDivision . snd) items || planHasDivision x
planHasDivision (Project _ x) = planHasDivision x
planHasDivision (Distinct x) = planHasDivision x
planHasDivision (Sort _ x) = planHasDivision x
planHasDivision (Limit _ x) = planHasDivision x
planHasDivision (Derived _ x) = planHasDivision x
planHasDivision _ = False

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
pushProject _ _ Unit = Unit
pushProject _ _ op@BoundSort{} = op
pushProject _ _ op@BoundAggregate{} = op
pushProject db _ (Compute items x) = Compute items (pushProject db (concatMap (colsInExpr . snd) items) x)
pushProject db _ (Aggregate keys aggs x) =
    Aggregate keys aggs (pushProject db (keys ++ concatMap (colsInExpr . snd) aggs) x)
pushProject db _ (Project cols x) = case pushProject db cols x of
    Project cols' y | cols' == cols -> Project cols y
    other -> Project cols other
pushProject db need (Filter p x) = Filter p (pushProject db (need ++ colsInExpr p) x)
pushProject db _ (Distinct x) = Distinct (pushProject db (relOpCols db x) x)
pushProject db need (Sort spec x) = Sort spec (pushProject db (need ++ map fst spec) x)
pushProject db need (Limit n x) = Limit n (pushProject db need x)
pushProject db need (SemiJoin kind left right l r query) =
    SemiJoin
        kind
        left
        right
        (pushProject db (need ++ colsInExpr left) l)
        (pushProject db (colsInExpr right) r)
        query
pushProject db need (Join kind l r c)
    | needsAll need = Join kind (pushProject db need l) (pushProject db need r) c
    | otherwise =
        let tot = need ++ colsInExpr c
            lCols = relOpCols db l
            rCols = relOpCols db r
            lNeed = if needsAll lCols then ["*"] else [x | x <- tot, x `elem` lCols]
            rNeed = if needsAll rCols then ["*"] else [x | x <- tot, x `elem` rCols]
         in Join kind (pushProject db lNeed l) (pushProject db rNeed r) c
pushProject db need leaf@(Scan a t _)
    | needsAll need = leaf
    | all (`elem` need) (relOpCols db leaf) = leaf
    | otherwise = Scan a t (Just (nub [column | requested <- need, let column = physicalColumn requested, column `elem` available]))
  where
    available = map (unqualify a) (relOpCols db (Scan a t Nothing))
    -- \| 把扫描限定列换成存储物理列名
    physicalColumn column = unqualify (Just (deriveQualifier a t)) (unqualify a column)
pushProject db need (Derived a x)
    | needsAll need || all (`elem` wanted) (relOpCols db x) = Derived a (pushProject db ["*"] x)
    | otherwise = Derived a (Project wanted (pushProject db wanted x))
  where
    wanted = nub (map (unqualify a) need)
pushProject db need leaf@(Lookup{}) = pushProjectLeaf db need leaf
pushProject db need leaf@(Range{}) = pushProjectLeaf db need leaf

-- | 折一个表达式节点
foldNode :: Expr -> Expr
foldNode e
    | null (colsInExpr e) = case evalExpr e [] of
        Right VNull -> LitNull
        Right (VInt n) -> LitInt n
        Right (VFloat d) -> LitFloat d
        Right (VStr s) -> LitStr s
        Right (VBool b) -> LitBool b
        Right (VRuntime tid value) -> RuntimeLiteral tid value
        Left _ -> e
    | otherwise = e

-- | 自底向上折整棵表达式
foldConstants :: Expr -> Expr
foldConstants (Add a b) = foldNode (Add (foldConstants a) (foldConstants b))
foldConstants (Sub a b) = foldNode (Sub (foldConstants a) (foldConstants b))
foldConstants (Mul a b) = foldNode (Mul (foldConstants a) (foldConstants b))
foldConstants (Div a b) = foldNode (Div (foldConstants a) (foldConstants b))
foldConstants (Neg a) = foldNode (Neg (foldConstants a))
foldConstants (BoundConstruct tid arguments) = BoundConstruct tid (map foldConstants arguments)
foldConstants (BoundFunction fid tid arguments) = foldNode (BoundFunction fid tid (map foldConstants arguments))
foldConstants (Gt a b) = foldNode (Gt (foldConstants a) (foldConstants b))
foldConstants (Lt a b) = foldNode (Lt (foldConstants a) (foldConstants b))
foldConstants (Eq a b) = foldNode (Eq (foldConstants a) (foldConstants b))
foldConstants (GtE a b) = foldNode (GtE (foldConstants a) (foldConstants b))
foldConstants (LtE a b) = foldNode (LtE (foldConstants a) (foldConstants b))
foldConstants (NotEq a b) = foldNode (NotEq (foldConstants a) (foldConstants b))
foldConstants (QuantCmp op a sq q) = QuantCmp op (foldConstants a) sq q
foldConstants (And a b) = foldNode (And (foldConstants a) (foldConstants b))
foldConstants (Or a b) = foldNode (Or (foldConstants a) (foldConstants b))
foldConstants e = e

-- | 点查值不值得改写；没有统计就按老规矩改
worthLookup :: Database -> String -> String -> Bool
worthLookup db t col = case tableMetaOf db t of
    Nothing -> True
    Just m -> indexCheaper m col (pointRows m col)

-- | 范围扫描值不值得改写；没有统计就按老规矩改
worthRange :: Database -> String -> String -> Maybe (Value, Bool) -> Maybe (Value, Bool) -> Bool
worthRange db t col lo hi = case tableMetaOf db t of
    Nothing -> True
    Just m -> indexCheaper m col (rangeRows m col lo hi)

-- | 将独立 IN 或简单等值 EXISTS 变为半连接
semiJoin :: Database -> Expr -> RelOp -> Maybe RelOp
semiJoin db (InSub left query negated) source
    | null (subqueryRefs query)
    , safeKey left
    , Right right <- translate (subqueryStatement query)
    , [column] <- relOpCols db right
    , column /= "*" =
        Just (SemiJoin (if negated then NotIn else Matching) left (Col column) source right (Just (subqueryStatement query)))
semiJoin db (ExistsSub query negated) source
    | [reference] <- subqueryRefs query
    , Select items (FromTable alias table) (Just (Eq (Col a) (Col b))) [] Nothing [] Nothing <- subqueryStatement query
    , let scan = Scan alias table Nothing
          columns = relOpCols db scan
    , all (localProjection columns . snd) items
    , Just column <- localKey columns reference a b
    , let physical = if reference `elem` relOpCols db source then reference else reverse (takeWhile (/= '.') (reverse reference))
    , physical `elem` relOpCols db source =
        Just
            ( SemiJoin
                (if negated then NonMatching else Matching)
                (Col physical)
                (Col "$semikey")
                source
                (Compute (items ++ [("$semikey", Col column)]) scan)
                Nothing
            )
semiJoin _ _ _ = Nothing

-- | 只允许无求值错误的左键
safeKey :: Expr -> Bool
safeKey Col{} = True
safeKey LitNull = True
safeKey LitInt{} = True
safeKey LitFloat{} = True
safeKey LitStr{} = True
safeKey LitBool{} = True
safeKey _ = False

-- | 投影仅保留当前来源的普通列
localProjection :: [String] -> Expr -> Bool
localProjection columns (Col column) = column `elem` columns
localProjection _ expression = safeKey expression && null (colsInExpr expression)

-- | 提取与唯一外层引用比较的本地列
localKey :: [String] -> String -> String -> String -> Maybe String
localKey columns reference a b
    | a == reference, b `elem` columns, b /= reference = Just b
    | b == reference, a `elem` columns, a /= reference = Just a
    | otherwise = Nothing

-- | 单节点重写
rewriteNode :: Database -> RelOp -> RelOp
rewriteNode db (Filter expression source)
    | Just joined <- semiJoin db expression source = joined
rewriteNode db (Filter (Eq (Col k) (LitInt v)) (Scan mAlias t Nothing))
    | worthLookup db t (unqualify mAlias k) = Lookup mAlias t (unqualify mAlias k) (VInt v)
rewriteNode db (Filter (Eq (Col k) (LitStr v)) (Scan mAlias t Nothing))
    | worthLookup db t (unqualify mAlias k) = Lookup mAlias t (unqualify mAlias k) (VStr v)
rewriteNode db (Filter (And (Gt (Col k1) (LitInt lo)) (Lt (Col k2) (LitInt hi))) (Scan mAlias t Nothing))
    | k1 == k2 && worthRange db t (unqualify mAlias k1) (Just (VInt lo, False)) (Just (VInt hi, False)) =
        Range mAlias t (unqualify mAlias k1) (Just (VInt lo, False)) (Just (VInt hi, False))
rewriteNode db (Filter (And (Gt (Col k1) (LitStr lo)) (Lt (Col k2) (LitStr hi))) (Scan mAlias t Nothing))
    | k1 == k2 && worthRange db t (unqualify mAlias k1) (Just (VStr lo, False)) (Just (VStr hi, False)) =
        Range mAlias t (unqualify mAlias k1) (Just (VStr lo, False)) (Just (VStr hi, False))
rewriteNode db (Filter (Gt (Col k) (LitInt v)) (Scan mAlias t Nothing))
    | worthRange db t (unqualify mAlias k) (Just (VInt v, False)) Nothing =
        Range mAlias t (unqualify mAlias k) (Just (VInt v, False)) Nothing
rewriteNode db (Filter (Gt (Col k) (LitStr v)) (Scan mAlias t Nothing))
    | worthRange db t (unqualify mAlias k) (Just (VStr v, False)) Nothing =
        Range mAlias t (unqualify mAlias k) (Just (VStr v, False)) Nothing
rewriteNode db (Filter (Lt (Col k) (LitInt v)) (Scan mAlias t Nothing))
    | worthRange db t (unqualify mAlias k) Nothing (Just (VInt v, False)) =
        Range mAlias t (unqualify mAlias k) Nothing (Just (VInt v, False))
rewriteNode db (Filter (Lt (Col k) (LitStr v)) (Scan mAlias t Nothing))
    | worthRange db t (unqualify mAlias k) Nothing (Just (VStr v, False)) =
        Range mAlias t (unqualify mAlias k) Nothing (Just (VStr v, False))
rewriteNode db (Filter (And (GtE (Col k1) (LitInt lo)) (LtE (Col k2) (LitInt hi))) (Scan mAlias t Nothing))
    | k1 == k2 && worthRange db t (unqualify mAlias k1) (Just (VInt lo, True)) (Just (VInt hi, True)) =
        Range mAlias t (unqualify mAlias k1) (Just (VInt lo, True)) (Just (VInt hi, True))
rewriteNode db (Filter (And (GtE (Col k1) (LitInt lo)) (Lt (Col k2) (LitInt hi))) (Scan mAlias t Nothing))
    | k1 == k2 && worthRange db t (unqualify mAlias k1) (Just (VInt lo, True)) (Just (VInt hi, False)) =
        Range mAlias t (unqualify mAlias k1) (Just (VInt lo, True)) (Just (VInt hi, False))
rewriteNode db (Filter (And (Gt (Col k1) (LitInt lo)) (LtE (Col k2) (LitInt hi))) (Scan mAlias t Nothing))
    | k1 == k2 && worthRange db t (unqualify mAlias k1) (Just (VInt lo, False)) (Just (VInt hi, True)) =
        Range mAlias t (unqualify mAlias k1) (Just (VInt lo, False)) (Just (VInt hi, True))
rewriteNode db (Filter (And (GtE (Col k1) (LitStr lo)) (LtE (Col k2) (LitStr hi))) (Scan mAlias t Nothing))
    | k1 == k2 && worthRange db t (unqualify mAlias k1) (Just (VStr lo, True)) (Just (VStr hi, True)) =
        Range mAlias t (unqualify mAlias k1) (Just (VStr lo, True)) (Just (VStr hi, True))
rewriteNode db (Filter (And (GtE (Col k1) (LitStr lo)) (Lt (Col k2) (LitStr hi))) (Scan mAlias t Nothing))
    | k1 == k2 && worthRange db t (unqualify mAlias k1) (Just (VStr lo, True)) (Just (VStr hi, False)) =
        Range mAlias t (unqualify mAlias k1) (Just (VStr lo, True)) (Just (VStr hi, False))
rewriteNode db (Filter (And (Gt (Col k1) (LitStr lo)) (LtE (Col k2) (LitStr hi))) (Scan mAlias t Nothing))
    | k1 == k2 && worthRange db t (unqualify mAlias k1) (Just (VStr lo, False)) (Just (VStr hi, True)) =
        Range mAlias t (unqualify mAlias k1) (Just (VStr lo, False)) (Just (VStr hi, True))
rewriteNode db (Filter (GtE (Col k) (LitInt v)) (Scan mAlias t Nothing))
    | worthRange db t (unqualify mAlias k) (Just (VInt v, True)) Nothing =
        Range mAlias t (unqualify mAlias k) (Just (VInt v, True)) Nothing
rewriteNode db (Filter (GtE (Col k) (LitStr v)) (Scan mAlias t Nothing))
    | worthRange db t (unqualify mAlias k) (Just (VStr v, True)) Nothing =
        Range mAlias t (unqualify mAlias k) (Just (VStr v, True)) Nothing
rewriteNode db (Filter (LtE (Col k) (LitInt v)) (Scan mAlias t Nothing))
    | worthRange db t (unqualify mAlias k) Nothing (Just (VInt v, True)) =
        Range mAlias t (unqualify mAlias k) Nothing (Just (VInt v, True))
rewriteNode db (Filter (LtE (Col k) (LitStr v)) (Scan mAlias t Nothing))
    | worthRange db t (unqualify mAlias k) Nothing (Just (VStr v, True)) =
        Range mAlias t (unqualify mAlias k) Nothing (Just (VStr v, True))
rewriteNode db (Filter p (Join kind l r c))
    | not (hasDivision p || planHasDivision (Join kind l r c)) =
        case kind of
            InnerJoin -> pushJoin db p l r c
            LeftJoin -> pushLeftJoin db p l r c
rewriteNode db (Filter p x) = case foldConstants p of
    LitBool True -> x
    p' -> pushOne db (Filter p' x)
rewriteNode _ (Project ["*"] x) = x
rewriteNode _ (Compute items x) = Compute [(label, foldConstants e) | (label, e) <- items] x
rewriteNode db op = pushOne db op

-- | 自底向上遍历一次
optimizeRelOp :: Database -> RelOp -> RelOp
optimizeRelOp db = rewriteNode db . descend
  where
    -- \| 先递归处理子算子
    descend (Filter p x) = Filter p (optimizeRelOp db x)
    descend (Project cols x) = Project cols (optimizeRelOp db x)
    descend (Compute items x) = Compute items (optimizeRelOp db x)
    descend (Aggregate keys aggs x) = Aggregate keys aggs (optimizeRelOp db x)
    descend (Distinct x) = Distinct (optimizeRelOp db x)
    descend (Sort spec x) = Sort spec (optimizeRelOp db x)
    descend (Limit n x) = Limit n (optimizeRelOp db x)
    descend (Join kind l r c) = Join kind (optimizeRelOp db l) (optimizeRelOp db r) c
    descend (SemiJoin kind left right l r query) = SemiJoin kind left right (optimizeRelOp db l) (optimizeRelOp db r) query
    descend (Derived a x) = Derived a (optimizeRelOp db x)
    descend x = x

-- | 反复跑到不动点
optimizePredicates :: Database -> RelOp -> RelOp
optimizePredicates db op =
    let op' = optimizeRelOp db op
     in if op' == op then op else optimizePredicates db op'

-- | 优化总入口
optimize :: Database -> RelOp -> RelOp
optimize db op = pushProject db ["*"] (reorderJoins db (optimizePredicates db op))
