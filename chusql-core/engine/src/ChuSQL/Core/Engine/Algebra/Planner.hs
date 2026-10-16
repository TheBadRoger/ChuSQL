{-# LANGUAGE TupleSections #-}

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
    -- \| 逐个聚合起名，重复的不再起名
    go :: Int -> [(String, Expr)] -> [Expr] -> [(String, Expr)]
    go _ acc [] = reverse acc
    go n acc (e : es)
        | any ((== e) . snd) acc = go n acc es
        | otherwise = go (n + 1) (("$agg" ++ show n, e) : acc) es

-- | 把聚合调用换成对聚合结果的引用
rewriteAggs :: [(String, Expr)] -> Expr -> Expr
rewriteAggs aggs e = case [label | (label, agg) <- aggs, agg == e] of
    (label : _) -> Col label
    [] -> case e of
        Construct tid arguments -> Construct tid (map go arguments)
        BoundConstruct tid arguments -> BoundConstruct tid (map go arguments)
        FunctionCall name arguments -> FunctionCall name (map go arguments)
        BoundFunction fid tid arguments -> BoundFunction fid tid (map go arguments)
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
        InSub a sq negated -> InSub (go a) sq negated
        InList a values negated -> InList (go a) (map go values) negated
        And a b -> And (go a) (go b)
        Or a b -> Or (go a) (go b)
        IsNull a -> IsNull (go a)
        IsNotNull a -> IsNotNull (go a)
        other -> other
  where
    -- \| 递归替换当前查询层的聚合引用
    go = rewriteAggs aggs

-- | 排序 + 截断
limited :: [(String, SortDir)] -> Maybe Int -> RelOp -> RelOp
limited orderBy mLimit op = case mLimit of
    Nothing -> sorted
    Just n -> Limit n sorted
  where
    sorted = case orderBy of
        [] -> op
        spec -> Sort spec op

-- | 生成过滤、聚合、HAVING、排序和投影计划
translate :: Statement -> Either String RelOp
translate q = case q of
    SelectDistinct inner -> do
        ordering <- distinctOrdering inner
        plan <- case inner of
            s@Select{} -> translate s{selectOrderBy = [], selectLimit = Nothing}
            _ -> Left "DISTINCT requires SELECT"
        let labels = case inner of
                Select{selectItems = items} -> map fst items
                _ -> []
        keys <- mapM (distinctKey labels) ordering
        namedOrdering <- mapM (columnKey labels) ordering
        let sorted
                | null keys = Distinct plan
                | any (\(column, _) -> length (filter (== column) labels) > 1) namedOrdering = BoundSort keys (Distinct plan)
                | otherwise = Sort namedOrdering (Distinct plan)
        Right (maybe sorted (`Limit` sorted) (selectLimit inner))
    Select items fromC mWhere groupBy having orderBy mLimit -> do
        plan0 <- source fromC mWhere
        expressions <- mapM (orderExpression items . fst) orderBy
        let aggs =
                nameAggs
                    ( items
                        ++ [("having", expression) | expression <- maybe [] pure having]
                        ++ map ("order",) expressions
                    )
            aggregate = not (null groupBy && null aggs && null having)
            grouped = if aggregate then Aggregate groupBy aggs plan0 else plan0
            plan = maybe grouped (\condition -> Filter (rewriteAggs aggs condition) grouped) having
            items' = [(label, rewriteAggs aggs expression) | (label, expression) <- items]
            ordering = zip (map (rewriteAggs aggs) expressions) (map snd orderBy)
            -- \| 简单列排序保留可优化的名称算子
            named (Col column, direction) = Just (column, direction)
            named _ = Nothing
            -- \| 保持普通投影在 LIMIT 之后求值
            projected child
                | all (\(label, expression) -> expression == Col label) items' = Project (map fst items') child
                | otherwise = Compute items' child
        Right
            ( case mapM named ordering of
                Just spec -> projected (limited spec mLimit plan)
                Nothing -> orderedProjection items' ordering mLimit plan
            )
    _ -> Left "only SELECT is supported by translate"
  where
    -- \| 简单投影的序号映射到来源列
    columnKey columns (column, direction) = do
        position <- orderPosition (length columns) column
        case position of
            Just slot -> Right (columns !! slot, direction)
            Nothing -> Left "DISTINCT: sorting requires an output position"
    -- \| 去重后按指定输出位置或唯一名称排序
    distinctKey columns (column, direction) = do
        position <- orderPosition (length columns) column
        case position of
            Just slot -> Right (ColumnSlot slot (columns !! slot), direction)
            Nothing -> Left "DISTINCT: sorting requires an output position"

-- | 在排序前计算别名并保留输出顺序
orderedProjection :: [(String, Expr)] -> [(Expr, SortDir)] -> Maybe Int -> RelOp -> RelOp
orderedProjection items orderBy mLimit plan =
    Compute items (limited ordering mLimit (Compute (carried ++ keys) plan))
  where
    keys = [("$order" ++ show n, expression) | (n, (expression, _)) <- zip [1 :: Int ..] orderBy]
    ordering = [(key, direction) | ((key, _), (_, direction)) <- zip keys orderBy]
    carried = [(column, Col column) | column <- nub (concatMap (colsInExpr . snd) items)]
