module ChuSQL.SQLSyntax.Executor where

import ChuSQL.Algebra.Eval (evalRelOp)
import ChuSQL.Algebra.Optimize (optimize)
import ChuSQL.Algebra.Planner (translate)
import ChuSQL.Model
import ChuSQL.SQLSyntax.AST
import ChuSQL.SQLSyntax.Expr (evalCondForRow, evalExpr)
import Control.Monad (filterM)

-- 表里加新行
updateTable :: String -> (Table -> Table) -> Database -> Either String Database
updateTable name f db = do
    table <- maybe (Left ("unknown table: " ++ name)) Right (lookup name db)
    let table' = f table
    Right (map (\(n, t) -> if n == name then (n, table') else (n, t)) db)

-- 修改表
applyUpdates :: [(String, Expr)] -> Row -> Either String Row
applyUpdates [] row = Right row
applyUpdates ((col, e) : rest) row = do
    v <- evalExpr e row
    let row' = setColumn col v row
    applyUpdates rest row'
  where
    setColumn c v r = map (\(k, val) -> if k == c then (k, v) else (k, val)) r

-- 处理查询
runQuery :: Database -> Query -> Either String (Database, [Row])
runQuery db q@Select{} = do
    relOp <- translate db q
    rows <- evalRelOp db (optimize db relOp)
    Right (db, rows)

-- 处理插入
runQuery db (Insert tbl cols vals) = do
    values <- mapM (\e -> evalExpr e []) vals
    if length cols /= length values
        then Left "column count does not match value count"
        else do
            let newRow = zip cols values
            db' <-
                updateTable
                    tbl
                    (\t -> t{tableRows = tableRows t ++ [newRow]})
                    db
            Right (db', [])

-- 处理删除
runQuery db (Delete tbl mWhere) = do
    table <- maybe (Left ("unknown table: " ++ tbl)) Right (lookup tbl db)
    rows <- case mWhere of
        Nothing -> Right []
        Just e -> filterM (shouldKeep e) (tableRows table)
    db' <- updateTable tbl (\t -> t{tableRows = rows}) db
    Right (db', [])
  where
    -- 保留 = 条件不成立
    shouldKeep :: Expr -> Row -> Either String Bool
    shouldKeep e row = not <$> evalCondForRow e row

-- 处理表更新
runQuery db (Update tbl assigns mWhere) = do
    table <- maybe (Left ("unknown table: " ++ tbl)) Right (lookup tbl db)
    rows <- mapM (updateRow mWhere assigns) (tableRows table)
    db' <- updateTable tbl (\t -> t{tableRows = rows}) db
    Right (db', [])
  where
    updateRow :: Maybe Expr -> [(String, Expr)] -> Row -> Either String Row
    updateRow cond asgns row = do
        keep <- case cond of
            Nothing -> Right True
            Just e -> evalCondForRow e row
        if keep
            then applyUpdates asgns row
            else Right row
