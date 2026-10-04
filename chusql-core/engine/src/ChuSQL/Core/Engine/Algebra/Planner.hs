module ChuSQL.Core.Engine.Algebra.Planner (translate) where

import ChuSQL.Core.Engine.Algebra.Expr (aggregatesIn, colsInExpr)
import ChuSQL.Core.Engine.Algebra.Op
import ChuSQL.Core.Engine.Syntax.AST
import Data.List (nub)

-- 计划生成：把语句翻译成算子树。

-- | FROM 子句翻成关系算子
fromToRelOp :: FromClause -> Either String RelOp
fromToRelOp FromUnit = Right Unit
fromToRelOp (FromTable mAlias tbl) = Right (Scan mAlias tbl Nothing)
fromToRelOp (FromSubquery mAlias stmt) = Derived mAlias <$> translate stmt
fromToRelOp (FromJoin kind left right cond) =
    Join kind <$> fromToRelOp left <*> fromToRelOp right <*> pure cond

-- | 带上 WHERE 的来源
source :: FromClause -> Maybe Expr -> Either String RelOp
source fromC mWhere = do
    op <- fromToRelOp fromC
    Right (maybe op (`Filter` op) mWhere)

-- | 给用到的聚合起内部列名，同一个聚合只算一次
nameAggs :: [(String, Expr)] -> [(String, Expr)]
nameAggs items = go (1 :: Int) [] (concatMap (aggregatesIn . snd) items)
  where
    -- | 逐个聚合起名，重复的不再起名
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
        GtE a b -> GtE (go a) (go b)
        LtE a b -> LtE (go a) (go b)
        NotEq a b -> NotEq (go a) (go b)
        QuantCmp op a sq q -> QuantCmp op (go a) sq q
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
            plan0 <- source fromC mWhere
            let (plan, _) = grouped groupBy [] plan0
            Right (Project cols (limited orderBy mLimit plan))
    SelectExpr items fromC mWhere groupBy orderBy mLimit -> do
        plan0 <- source fromC mWhere
        let (plan, items') = grouped groupBy items plan0
        Right $ if any (\(c, _) -> c `elem` map fst items') orderBy
            then orderedProjection items' orderBy mLimit plan
            else Compute items' (limited orderBy mLimit plan)
    _ -> Left "only SELECT is supported by translate"

-- | 在排序前计算别名并保留输出顺序
orderedProjection :: [(String, Expr)] -> [(String, SortDir)] -> Maybe Int -> RelOp -> RelOp
orderedProjection items orderBy mLimit plan =
    Compute items (limited ordering mLimit (Compute (carried ++ keys) plan))
  where
    carried = [(column, Col column) | column <- nub (concatMap (colsInExpr . snd) items)]
    keys = [("$order" ++ show n, maybe (Col c) id (lookup c items)) | (n, (c, _)) <- zip [1 :: Int ..] orderBy]
    ordering = [(key, direction) | ((key, _), (_, direction)) <- zip keys orderBy]
