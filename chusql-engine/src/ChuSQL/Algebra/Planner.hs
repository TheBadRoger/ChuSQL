module ChuSQL.Algebra.Planner (translate) where

import ChuSQL.Algebra.Op
import ChuSQL.Syntax.AST

-- 计划生成：把语句翻译成算子树。

-- | 把 FROM 子句翻成 Scan / Join
fromToRelOp :: FromClause -> RelOp
fromToRelOp (FromTable mAlias tbl) = Scan mAlias tbl
fromToRelOp (FromJoin left mAlias tbl cond) =
    Join (fromToRelOp left) (Scan mAlias tbl) cond

-- | 翻成：过滤 -> 排序 -> 截断 -> 取列
translate :: Statement -> Either String RelOp
translate q = case q of
    Select
        { selectCols = cols
        , selectFrom = fromC
        , selectWhere = mWhere
        , selectOrderBy = orderBy
        , selectLimit = mLimit
        } -> do
            let base = fromToRelOp fromC
                filtered = case mWhere of
                    Nothing -> base
                    Just w -> Filter w base
                sorted = case orderBy of
                    [] -> filtered
                    spec -> Sort spec filtered
                limited = case mLimit of
                    Nothing -> sorted
                    Just n -> Limit n sorted
            Right (Project cols limited)
    _ -> Left "only SELECT is supported by translate"
