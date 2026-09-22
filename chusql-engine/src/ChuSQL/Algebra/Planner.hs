module ChuSQL.Algebra.Planner (translate) where

import ChuSQL.Algebra.Op
import ChuSQL.Syntax.AST

-- 内部查询语句转执行计划
fromToRelOp :: FromClause -> RelOp
fromToRelOp (FromTable mAlias tbl) = Scan mAlias tbl
fromToRelOp (FromJoin left mAlias tbl cond) =
    Join (fromToRelOp left) (Scan mAlias tbl) cond

translate :: Query -> Either String RelOp
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
