module ChuSQL.Engine (runQuery, rowsOf) where

import ChuSQL.Algebra.Eval (evalRelOp)
import ChuSQL.Algebra.Expr (evalCondForRow, evalExpr)
import ChuSQL.Algebra.Optimize (optimize)
import ChuSQL.Algebra.Planner (translate)
import ChuSQL.Model
import ChuSQL.Semantic (check)
import ChuSQL.Syntax.AST
import Control.Monad (filterM)

-- | 带检查的查询入口：先做语义检查，再交给下面的执行
runQuery :: Database -> Query -> Either String (Database, [Row])
runQuery db q = do
    _ <- check db q
    runQueryUnchecked db q

-- | 只要结果行（丢掉更新后的数据库），给演示和测试用
rowsOf :: Either String (Database, [Row]) -> Either String [Row]
rowsOf = fmap snd

-- 把表整体换掉（调用方已经查过，表一定存在）
replaceTable :: String -> Table -> Database -> Database
replaceTable name table = map (\(n, t) -> if n == name then (n, table) else (n, t))

-- 修改表
applyUpdates :: [(String, Expr)] -> Row -> Either String Row
applyUpdates [] row = Right row
applyUpdates ((col, e) : rest) row = do
    v <- evalExpr e row
    let row' = setColumn col v row
    applyUpdates rest row'
  where
    setColumn c v r = map (\(k, val) -> if k == c then (k, v) else (k, val)) r

-- 实际执行（调用方请确保已过 check）
runQueryUnchecked :: Database -> Query -> Either String (Database, [Row])
runQueryUnchecked db q@Select{} = do
    relOp <- translate q
    rows <- evalRelOp db (optimize db relOp)
    Right (db, rows)

-- 处理插入
runQueryUnchecked db (Insert tbl cols vals) = do
    values <- mapM (\e -> evalExpr e []) vals
    if length cols /= length values
        then Left "column count does not match value count"
        else do
            table <- lookupTable db tbl
            let newRow = zip cols values
            Right (replaceTable tbl table{tableRows = tableRows table ++ [newRow]} db, [])

-- 处理删除
runQueryUnchecked db (Delete tbl mWhere) = do
    table <- lookupTable db tbl
    rows <- case mWhere of
        Nothing -> Right []
        Just e -> filterM (shouldKeep e) (tableRows table)
    Right (replaceTable tbl table{tableRows = rows} db, [])
  where
    -- 保留 = 条件不成立
    shouldKeep :: Expr -> Row -> Either String Bool
    shouldKeep e row = not <$> evalCondForRow e row

-- 处理表更新
runQueryUnchecked db (Update tbl assigns mWhere) = do
    table <- lookupTable db tbl
    rows <- mapM (updateRow mWhere assigns) (tableRows table)
    Right (replaceTable tbl table{tableRows = rows} db, [])
  where
    updateRow :: Maybe Expr -> [(String, Expr)] -> Row -> Either String Row
    updateRow cond asgns row = do
        keep <- case cond of
            Nothing -> Right True
            Just e -> evalCondForRow e row
        if keep
            then applyUpdates asgns row
            else Right row
