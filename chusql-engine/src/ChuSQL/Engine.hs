module ChuSQL.Engine (runQuery, runQueryM, rowsOf) where

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

-- | 对外入口：签名不变，内部走内存实现。
runQuery :: Database -> Query -> Either String (Database, [Row])
runQuery db q = do
    (result, db') <- runMemoryStorage (runQueryM q) db
    rows <- result
    Right (db', rows)

-- | 只要结果行（丢掉更新后的数据库），给演示和测试用。
rowsOf :: Either String (Database, [Row]) -> Either String [Row]
rowsOf = fmap snd

-- * 泛型入口：走 MonadStorage，未来 IPCStorage 复用同一份代码

-- | 语义检查 + 分发到具体语句。
runQueryM :: (MonadStorage m) => Query -> m (Either String [Row])
runQueryM q = do
    db <- snapshot
    case check db q of
        Left err -> pure (Left err)
        Right _ -> runQueryUncheckedM q

-- | SELECT：拿快照，跑 translate → optimize → evalRelOp。
runQueryUncheckedM :: (MonadStorage m) => Query -> m (Either String [Row])
runQueryUncheckedM q@Select{} = do
    db <- snapshot
    case translate q of
        Left e -> pure (Left e)
        Right relOp -> evalRelOpM db (optimize db relOp)

-- \| INSERT：求值后追加一行。
runQueryUncheckedM (Insert tbl cols vals) =
    case mapM (\e -> evalExpr e []) vals of
        Left err -> pure (Left err)
        Right values
            | length cols /= length values ->
                pure (Left "column count does not match value count")
            | otherwise -> do
                result <- insert tbl (zip cols values)
                pure (result >> Right [])

-- \| DELETE：扫表、筛掉命中行、整体写回。
runQueryUncheckedM (Delete tbl mWhere) = do
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
    -- \| 保留 = 条件不成立。
    shouldKeep :: Expr -> Row -> Either String Bool
    shouldKeep e row = not <$> evalCondForRow e row

-- \| UPDATE：扫表、逐行改、整体写回。
runQueryUncheckedM (Update tbl assigns mWhere) = do
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
    -- \| 命中条件就套用赋值，否则原样返回。
    updateRow :: Maybe Expr -> [(String, Expr)] -> Row -> Either String Row
    updateRow cond asgns row = do
        keep <- case cond of
            Nothing -> Right True
            Just e -> evalCondForRow e row
        if keep then applyUpdates asgns row else Right row

-- | 按 assignments 依次求值并覆盖对应列。
applyUpdates :: [(String, Expr)] -> Row -> Either String Row
applyUpdates [] row = Right row
applyUpdates ((col, e) : rest) row = do
    v <- evalExpr e row
    let row' = map (\(k, val) -> if k == col then (k, v) else (k, val)) row
    applyUpdates rest row'
