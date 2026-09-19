module ChuSQL.Algebra.Planner where

import ChuSQL.Algebra.Op
import ChuSQL.Model
import ChuSQL.SQLSyntax.AST

-- * 存在性检查
checkColumns :: [String] -> [String] -> Either String ()
checkColumns available requested =
    case [c | c <- requested, c /= "*", c `notElem` available] of
        [] -> Right ()
        (c : _) -> Left ("unknown column: " ++ c)

checkFrom :: Database -> FromClause -> Either String ()
checkFrom db (FromTable _ tbl) =
    case lookup tbl db of
        Just _ -> Right ()
        Nothing -> Left ("unknown table: " ++ tbl)
checkFrom db (FromJoin left _ tbl _) = do
    _ <- checkFrom db left
    case lookup tbl db of
        Just _ -> Right ()
        Nothing -> Left ("unknown table: " ++ tbl)

-- | 收集 FromClause 里所有可用的列名
availableColumns :: Database -> FromClause -> Either String [String]
availableColumns db (FromTable mAlias tbl) = do
    table <- maybe (Left ("unknown table: " ++ tbl)) Right (lookup tbl db)
    let prefix = maybe "" (++ ".") mAlias
    Right (map (prefix ++) (tableCols table))
availableColumns db (FromJoin left mAlias tbl _) = do
    lcols <- availableColumns db left
    table <- maybe (Left ("unknown table: " ++ tbl)) Right (lookup tbl db)
    let prefix = maybe "" (++ ".") mAlias
    Right (lcols ++ map (prefix ++) (tableCols table))

-- | 检查数据库列明
checkColumnsFrom :: Database -> FromClause -> [String] -> Either String ()
checkColumnsFrom db fromC requested = do
    available <- availableColumns db fromC
    checkColumns available requested

-- 内部查询语句转执行计划
fromToRelOp :: FromClause -> RelOp
fromToRelOp (FromTable mAlias tbl) = Scan mAlias tbl
fromToRelOp (FromJoin left mAlias tbl cond) =
    Join (fromToRelOp left) (Scan mAlias tbl) cond

translate :: Database -> Query -> Either String RelOp
translate db q = case q of
    Select
        { selectCols = cols
        , selectFrom = fromC
        , selectWhere = mWhere
        , selectOrderBy = orderBy
        , selectLimit = mLimit
        } -> do
            -- 检查所有表和列
            _ <- checkFrom db fromC
            _ <- checkColumnsFrom db fromC cols
            _ <- checkColumnsFrom db fromC (map fst orderBy)
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
