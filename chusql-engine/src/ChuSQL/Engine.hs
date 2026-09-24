module ChuSQL.Engine (runStatement, runStatementM, rowsOf) where

import ChuSQL.Algebra.Eval (evalRelOpM)
import ChuSQL.Algebra.Expr (evalCondForRow, evalExpr)
import ChuSQL.Algebra.Optimize (optimize)
import ChuSQL.Algebra.Planner (translate)
import ChuSQL.Model
import ChuSQL.Semantic (check)
import ChuSQL.Storage (MonadStorage (..))
import ChuSQL.Storage.Memory (MemoryStorage (runMemoryStorage))
import ChuSQL.Syntax.AST
import Control.Monad (filterM)

-- 引擎入口：语义检查 + 分发执行，以及内存实现与泛型版本。

-- * 对外入口
-- | 跑一条语句（内存实现）
runStatement :: Database -> Statement -> Either String (Database, [Row])
runStatement db q = do
    (result, db') <- runMemoryStorage (runStatementM q) db
    rows <- result
    Right (db', rows)

-- | 只要结果行
rowsOf :: Either String (Database, [Row]) -> Either String [Row]
rowsOf = fmap snd


-- | 泛型入口：先检查再执行
runStatementM :: (MonadStorage m) => Statement -> m (Either String [Row])
runStatementM q = do
    db <- snapshot
    case check db q of
        Left err -> pure (Left err)
        Right _ -> runStatementUncheckedM q

-- * 分发执行
-- | 按语句类型分发（已检查过）
runStatementUncheckedM :: (MonadStorage m) => Statement -> m (Either String [Row])
runStatementUncheckedM q@Select{} = do
    db <- snapshot
    case translate q of
        Left e -> pure (Left e)
        Right relOp -> evalRelOpM db (optimize db relOp)

runStatementUncheckedM (Insert tbl cols vals) =
    case mapM (\e -> evalExpr e []) vals of
        Left err -> pure (Left err)
        Right values
            | length cols /= length values ->
                pure (Left "column count does not match value count")
            | otherwise -> do
                result <- insert tbl (zip cols values)
                pure (result >> Right [])

runStatementUncheckedM (Delete tbl mWhere) = do
    rowsResult <- scan tbl
    case rowsResult of
        Left err -> pure (Left err)
        Right rows -> do
            let kept = case mWhere of
                    Nothing -> Right []
                    Just e -> filterM (shouldKeep e) rows
            case kept of
                Left err -> pure (Left err)
                Right keptRows -> do
                    result <- replaceAll tbl keptRows
                    pure (result >> Right [])
  where
    -- \| 保留 = 条件不成立
    shouldKeep :: Expr -> Row -> Either String Bool
    shouldKeep e row = not <$> evalCondForRow e row

runStatementUncheckedM (Update tbl assigns mWhere) = do
    rowsResult <- scan tbl
    case rowsResult of
        Left err -> pure (Left err)
        Right rows -> do
            let newRows = mapM (updateRow mWhere assigns) rows
            case newRows of
                Left err -> pure (Left err)
                Right rs -> do
                    result <- replaceAll tbl rs
                    pure (result >> Right [])
  where
    -- \| 命中条件就套用赋值
    updateRow :: Maybe Expr -> [(String, Expr)] -> Row -> Either String Row
    updateRow cond asgns row = do
        keep <- case cond of
            Nothing -> Right True
            Just e -> evalCondForRow e row
        if keep then applyUpdates asgns row else Right row

runStatementUncheckedM (CreateTable name cols) = do
    result <- createTable name cols
    pure (result >> Right [])
runStatementUncheckedM (DropTable name) = do
    result <- dropTable name
    pure (result >> Right [])

-- * 更新辅助
-- | 依次求值并覆盖列
applyUpdates :: [(String, Expr)] -> Row -> Either String Row
applyUpdates [] row = Right row
applyUpdates ((col, e) : rest) row = do
    v <- evalExpr e row
    let row' = map (\(k, val) -> if k == col then (k, v) else (k, val)) row
    applyUpdates rest row'
