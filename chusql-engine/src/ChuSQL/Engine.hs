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
    -- 只要结构：语义检查和查询优化都只看列，一行数据都不碰，
    -- 所以这里不用取整库快照（那会把所有行拉一遍）
    db <- schema
    case check db q of
        Left err -> pure (Left err)
        Right _ -> runStatementUncheckedM db q

-- * 分发执行

-- | 按语句类型分发（已检查过）
runStatementUncheckedM :: (MonadStorage m) => Database -> Statement -> m (Either String [Row])
runStatementUncheckedM db q@Select{} =
    case translate q of
        Left e -> pure (Left e)
        Right relOp -> evalRelOpM (optimize db relOp)
runStatementUncheckedM _ (Insert tbl cols rows) =
    -- 多行一次交给存储：N 行只落一次盘（键怎么算、索引怎么写是存储层的事）
    case mapM toRow rows of
        Left err -> pure (Left err)
        Right rs -> do
            result <- insertMany tbl rs
            pure (result >> Right [])
  where
    -- \| 一行字面量求值成一行数据
    toRow :: [Expr] -> Either String Row
    toRow vals = do
        values <- mapM (\e -> evalExpr e []) vals
        if length cols /= length values
            then Left "column count does not match value count"
            else Right (zip cols values)
runStatementUncheckedM _ (Delete tbl mWhere) = do
    rowsResult <- scan tbl
    case rowsResult of
        Left err -> pure (Left err)
        Right rows -> case splitByCondition mWhere rows of
            Left err -> pure (Left err)
            Right (doomed, kept) ->
                -- 要删的行都有 id 就走行级删（存储层原地删，代价只和删几行有关）；
                -- 没有 id 的表定位不到行，只能整表写回。
                case mapM rowId doomed of
                    Right ids -> do
                        result <- deleteKeys tbl ids
                        pure (result >> Right [])
                    Left _ -> do
                        result <- replaceAll tbl kept
                        pure (result >> Right [])
  where
    -- \| 这一行的 id（要拿来当删除键）
    rowId :: Row -> Either String Int
    rowId r = case lookup "id" r of
        Just (VInt k) -> Right k
        _ -> Left "row has no integer id"
runStatementUncheckedM _ (Update tbl assigns mWhere) = do
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
runStatementUncheckedM _ (CreateTable name cols) = do
    result <- createTable name cols
    pure (result >> Right [])
runStatementUncheckedM _ (DropTable name) = do
    result <- dropTable name
    pure (result >> Right [])
runStatementUncheckedM _ (CreateIndex tbl col) = do
    result <- createIndex tbl col
    pure (result >> Right [])
runStatementUncheckedM _ (DropIndex tbl col) = do
    result <- dropIndex tbl col
    pure (result >> Right [])

-- * 更新辅助

-- | 按条件把行分成"要删的"和"留着的"两拨（没有条件就全都要删）
splitByCondition :: Maybe Expr -> [Row] -> Either String ([Row], [Row])
splitByCondition cond = go [] []
  where
    -- \| 一行一行过，保持原顺序；条件求值出错就立刻停（和 filterM 一样）
    go doomed kept [] = Right (reverse doomed, reverse kept)
    go doomed kept (r : rs) = do
        hit <- case cond of
            Nothing -> Right True
            Just e -> evalCondForRow e r
        go (if hit then r : doomed else doomed) (if hit then kept else r : kept) rs

-- | 依次求值并覆盖列
applyUpdates :: [(String, Expr)] -> Row -> Either String Row
applyUpdates [] row = Right row
applyUpdates ((col, e) : rest) row = do
    v <- evalExpr e row
    let row' = map (\(k, val) -> if k == col then (k, v) else (k, val)) row
    applyUpdates rest row'
