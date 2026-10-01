module ChuSQL.Algebra.Planner (translate) where

import ChuSQL.Algebra.Expr (aggregatesIn)
import ChuSQL.Algebra.Op
import ChuSQL.Syntax.AST

-- 计划生成：把语句翻译成算子树。

-- | 把 FROM 子句翻成 Scan / Join
fromToRelOp :: FromClause -> RelOp
fromToRelOp FromUnit = Unit
fromToRelOp (FromTable mAlias tbl) = Scan mAlias tbl Nothing
fromToRelOp (FromJoin kind left mAlias tbl cond) =
    Join kind (fromToRelOp left) (Scan mAlias tbl Nothing) cond

-- | 带上 WHERE 的来源
source :: FromClause -> Maybe Expr -> RelOp
source fromC mWhere = case mWhere of
    Nothing -> fromToRelOp fromC
    Just w -> Filter w (fromToRelOp fromC)

-- | 给用到的聚合起内部列名，同一个聚合只算一次
nameAggs :: [(String, Expr)] -> [(String, Expr)]
nameAggs items = go (1 :: Int) [] (concatMap (aggregatesIn . snd) items)
  where
    go :: Int -> [(String, Expr)] -> [Expr] -> [(String, Expr)]
    go _ acc [] = reverse acc
    go n acc (e : es)
        | any ((== e) . snd) acc = go n acc es
        | otherwise = go (n + 1) (("agg" ++ show n, e) : acc) es

-- | 把聚合调用换成对聚合结果的引用
rewriteAggs :: [(String, Expr)] -> Expr -> Expr
rewriteAggs aggs e = case [label | (label, agg) <- aggs, agg == e] of
    (label : _) -> Col label
    [] -> case e of
        Add a b -> Add (go a) (go b)
        Sub a b -> Sub (go a) (go b)
        Mul a b -> Mul (go a) (go b)
        Div a b -> Div (go a) (go b)
        Neg a -> Neg (go a)
        Gt a b -> Gt (go a) (go b)
        Lt a b -> Lt (go a) (go b)
        Eq a b -> Eq (go a) (go b)
        And a b -> And (go a) (go b)
        Or a b -> Or (go a) (go b)
        IsNull a -> IsNull (go a)
        IsNotNull a -> IsNotNull (go a)
        other -> other
  where
    go = rewriteAggs aggs

-- | 过滤 + 分组：没有分组也没有聚合时原样返回
grouped :: [String] -> [(String, Expr)] -> RelOp -> (RelOp, [(String, Expr)])
grouped keys items base
    | null keys && null aggs = (base, items)
    | otherwise = (Aggregate keys aggs base, [(label, rewriteAggs aggs e) | (label, e) <- items])
  where
    aggs = nameAggs items

-- | 排序 + 截断
limited :: [(String, SortDir)] -> Maybe Int -> RelOp -> RelOp
limited orderBy mLimit op = case mLimit of
    Nothing -> sorted
    Just n -> Limit n sorted
  where
    sorted = case orderBy of
        [] -> op
        spec -> Sort spec op

-- | 翻成：过滤 -> 分组 -> 排序 -> 截断 -> 取列
translate :: Statement -> Either String RelOp
translate q = case q of
    Select
        { selectCols = cols
        , selectFrom = fromC
        , selectWhere = mWhere
        , selectGroupBy = groupBy
        , selectOrderBy = orderBy
        , selectLimit = mLimit
        } -> do
            let (plan, _) = grouped groupBy [] (source fromC mWhere)
            Right (Project cols (limited orderBy mLimit plan))
    SelectExpr items fromC mWhere groupBy orderBy mLimit -> do
        let (plan, items') = grouped groupBy items (source fromC mWhere)
        Right (Compute items' (limited orderBy mLimit plan))
    _ -> Left "only SELECT is supported by translate"
