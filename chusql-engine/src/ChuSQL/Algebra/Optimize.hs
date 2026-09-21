module ChuSQL.Algebra.Optimize where

import ChuSQL.Algebra.Op (RelOp (..))
import ChuSQL.Model
import ChuSQL.SQLSyntax.AST (Expr (..))
import ChuSQL.SQLSyntax.Expr (evalExpr)

-- * 谓词下推

-- | 收集表达式里用到的列名；谓词下推判断条件归属、常量折叠判断能否求值，都要用它
colsInExpr :: Expr -> [String]
colsInExpr (Col c) = [c]
colsInExpr (Gt a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Lt a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Eq a b) = colsInExpr a ++ colsInExpr b
colsInExpr (And a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Or a b) = colsInExpr a ++ colsInExpr b
colsInExpr _ = []

-- | 一个算子会产出哪些列；返回 ["*"] 表示"全部列，不用挑"
relOpCols :: Database -> RelOp -> [String]
relOpCols db (Filter _ x) = relOpCols db x
relOpCols db (Sort _ x) = relOpCols db x
relOpCols db (Limit _ x) = relOpCols db x
relOpCols _ (Project ["*"] _) = ["*"]
relOpCols _ (Project cols _) = cols
relOpCols db (Join l r _) = relOpCols db l ++ relOpCols db r
relOpCols db (Scan mAlias tbl) =
    case lookup tbl db of
        Nothing -> []
        Just t -> map (prefix ++) (tableCols t)
  where
    prefix = maybe "" (++ ".") mAlias

-- | need 里的列是不是都能在 have 里找到；have 为 ["*"] 时一律为真
isSubsetOf :: [String] -> [String] -> Bool
isSubsetOf _ ["*"] = True
isSubsetOf need have = all (`elem` have) need

-- | 把一个用 AND 串起来的条件拆成清单：a AND b AND c 变成 [a, b, c]
splitConj :: Expr -> [Expr]
splitConj (And a b) = splitConj a ++ splitConj b
splitConj e = [e]

-- | 反过来：把条件清单用 AND 重新串成一个；空清单表示"没有条件"，返回 Nothing
combineConj :: [Expr] -> Maybe Expr
combineConj [] = Nothing
combineConj (e : es) = Just (foldl And e es)

-- | 把条件按"只用到哪一边的列"分成两堆：能下推过去的 / 必须留在原地的
partitionPred :: [String] -> [Expr] -> ([Expr], [Expr])
partitionPred have = foldr pick ([], [])
  where
    pick e (yes, no)
        | colsInExpr e `isSubsetOf` have = (e : yes, no)
        | otherwise = (yes, e : no)

-- | 把一组条件依次套到算子上，一个条件一层 Filter
applyFilters :: [Expr] -> RelOp -> RelOp
applyFilters [] op = op
applyFilters (e : es) op = applyFilters es (Filter e op)

-- | Filter 压在 Join 上时的下推规则：条件按列归属拆到左右两侧，跨两边的才留在 Join 上方
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

-- | Filter 压在 Filter / Sort 上时的下推规则：合并上下两层 Filter，或把 Filter 挪到 Sort 下面
pushOne :: Database -> RelOp -> RelOp
pushOne _ (Filter p (Filter q x)) = Filter (And p q) x
pushOne _ (Filter p (Sort spec x)) = Sort spec (Filter p x)
pushOne _ op = op

-- * 投影下推

-- | 工具函数
needsAll :: [String] -> Bool
needsAll = elem "*"

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
pushProject db need scan@(Scan _ _)
    | needsAll need = scan
    | all (`elem` need) (relOpCols db scan) = scan
    | otherwise = Project need scan

-- * 常量折叠

-- | 折一个表达式：不含列就直接求值替换成字面量，含列或求值失败则原样返回
foldNode :: Expr -> Expr
foldNode e
    | null (colsInExpr e) = case evalExpr e [] of
        Right (VInt n) -> LitInt n
        Right (VStr s) -> LitStr s
        Right (VBool b) -> LitBool b
        Left _ -> e
    | otherwise = e

-- | 自底向上折叠整个表达式：先把子表达式折干净，再折自己
foldConstants :: Expr -> Expr
foldConstants (Gt a b) = foldNode (Gt (foldConstants a) (foldConstants b))
foldConstants (Lt a b) = foldNode (Lt (foldConstants a) (foldConstants b))
foldConstants (Eq a b) = foldNode (Eq (foldConstants a) (foldConstants b))
foldConstants (And a b) = foldNode (And (foldConstants a) (foldConstants b))
foldConstants (Or a b) = foldNode (Or (foldConstants a) (foldConstants b))
foldConstants e = e

-- * 执行优化

-- | 单节点重写：把 Filter 分派给上面几条规则，另外负责恒真条件删除与恒等投影消除
rewriteNode :: Database -> RelOp -> RelOp
rewriteNode db (Filter p (Join l r c)) = pushJoin db p l r c
rewriteNode db (Filter p x) = case foldConstants p of
    LitBool True -> x
    p' -> pushOne db (Filter p' x)
rewriteNode _ (Project ["*"] x) = x
rewriteNode db op = pushOne db op

-- | 自底向上遍历整棵算子树，每个节点套一次 rewriteNode
optimizeRelOp :: Database -> RelOp -> RelOp
optimizeRelOp db = rewriteNode db . descend
  where
    descend (Filter p x) = Filter p (optimizeRelOp db x)
    descend (Project cols x) = Project cols (optimizeRelOp db x)
    descend (Sort spec x) = Sort spec (optimizeRelOp db x)
    descend (Limit n x) = Limit n (optimizeRelOp db x)
    descend (Join l r c) = Join (optimizeRelOp db l) (optimizeRelOp db r) c
    descend x = x

-- | 谓词下推 + 常量折叠：反复应用规则，直到算子树的形状不再变化（到不动点为止）
optimizePredicates :: Database -> RelOp -> RelOp
optimizePredicates db op =
    let op' = optimizeRelOp db op
     in if op' == op then op else optimizePredicates db op'

-- | 优化器总入口：先把谓词下推与常量折叠跑到不动点，再做一次投影下推
optimize :: Database -> RelOp -> RelOp
optimize db op = pushProject db ["*"] (optimizePredicates db op)
