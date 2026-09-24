module ChuSQL.Algebra.Planner (translate) where

import ChuSQL.Algebra.Op
import ChuSQL.Syntax.AST

-- 计划翻译：把 AST 里的 SELECT 翻译成关系代数算子树（只翻译，不做合法性检查）。

-- 把 FROM 从句转成 Scan / Join 算子。
fromToRelOp :: FromClause -> RelOp
fromToRelOp (FromTable mAlias tbl) = Scan mAlias tbl
fromToRelOp (FromJoin left mAlias tbl cond) =
    Join (fromToRelOp left) (Scan mAlias tbl) cond

-- 翻译入口：把 SELECT 翻译成算子树；其它语句类型不支持（返回 Left）。
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
