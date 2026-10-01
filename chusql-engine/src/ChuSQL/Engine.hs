module ChuSQL.Engine (runStatement, runStatementM, rowsOf) where

import ChuSQL.Algebra.Eval (evalCondForRowM, evalExprM, evalRelOpM)
import ChuSQL.Algebra.Expr (evalCondForRow, evalExpr)
import ChuSQL.Algebra.Optimize (optimize)
import ChuSQL.Algebra.Planner (translate)
import ChuSQL.Model
import ChuSQL.Semantic (prepare)
import ChuSQL.Storage (MonadStorage (..))
import ChuSQL.Storage.Memory (MemoryStorage (runMemoryStorage))
import ChuSQL.Syntax.AST
import ChuSQL.Syntax.Parser (parseExpression)
import Data.Maybe (mapMaybe)

-- 引擎入口：语义检查 + 分发执行，以及内存实现与泛型版本。

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
runStatementM (CreateDatabase name) = fmap (fmap (const [])) (createDatabase name)
runStatementM (DropDatabase name) = fmap (fmap (const [])) (dropDatabase name)
runStatementM (UseDatabase name) = fmap (fmap (const [])) (useDatabase name)
runStatementM ShowDatabases = fmap (fmap (map (\name -> [("database", VStr name)]))) listDatabases
runStatementM q = do
    db <- schema
    case prepare db q of
        Left err -> pure (Left err)
        Right resolved -> runStatementUncheckedM db resolved

-- | 按语句类型分发（已检查过）
runStatementUncheckedM :: (MonadStorage m) => Database -> Statement -> m (Either String [Row])
runStatementUncheckedM db q@Select{} =
    case translate q of
        Left e -> pure (Left e)
        Right relOp -> evalRelOpM db (optimize db relOp)
runStatementUncheckedM db q@SelectExpr{} =
    case translate q of
        Left e -> pure (Left e)
        Right relOp -> evalRelOpM db (optimize db relOp)
runStatementUncheckedM db (Insert tbl cols rows) =
    case lookupTable db tbl of
        Left e -> pure (Left e)
        Right table -> do
            current <- existingRows tbl table
            case current >>= \current' -> buildRows table current' cols rows of
                Left err -> pure (Left err)
                Right rs -> do
                    result <- insertMany tbl rs
                    pure (result >> Right [])
runStatementUncheckedM db (Delete tbl mWhere) = do
    rowsResult <- scan tbl
    case rowsResult of
        Left err -> pure (Left err)
        Right rows -> do
            split <- splitByConditionM db mWhere rows
            case split of
                Left err -> pure (Left err)
                Right (doomed, kept) ->
                    case mapM rowId doomed of
                        Right ids -> do
                            result <- deleteKeys tbl ids
                            pure (result >> Right [])
                        Left _ -> do
                            result <- replaceAll tbl kept
                            pure (result >> Right [])
  where
    rowId :: Row -> Either String Int
    rowId r = case lookup "id" r of
        Just (VInt k) -> Right k
        _ -> Left "row has no integer id"
runStatementUncheckedM db (Update tbl assigns mWhere) =
    case lookupTable db tbl of
        Left e -> pure (Left e)
        Right table -> do
            rowsResult <- scan tbl
            case rowsResult of
                Left err -> pure (Left err)
                Right rows -> do
                    newRows <- mapM (updateRowM db table mWhere assigns) rows
                    case sequence newRows of
                        Left err -> pure (Left err)
                        Right rs -> do
                            result <- replaceAll tbl rs
                            pure (result >> Right [])
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
runStatementUncheckedM _ (DropColumn tbl col) = do
    result <- dropColumn tbl col
    pure (result >> Right [])
runStatementUncheckedM db (AddColumn tbl def) =
    alterTable db tbl (\table -> addColumnTo def (tableCols table) (tableRows table))
runStatementUncheckedM db (RenameColumn tbl old new) =
    alterTable db tbl (\table -> renameColumnIn old new (tableCols table) (tableRows table))
runStatementUncheckedM db (AlterColumnType tbl col ty) =
    alterTable db tbl (\table -> retypeColumn col ty (tableCols table) (tableRows table))
runStatementUncheckedM db (AlterColumnDefault tbl col def) =
    alterTable db tbl (\table -> setColumnDefault col def (tableCols table) (tableRows table))
runStatementUncheckedM db (AlterColumnNull tbl col nullable) =
    alterTable db tbl (\table -> setColumnNullable col nullable (tableCols table) (tableRows table))
runStatementUncheckedM _ CreateUser{} = pure (Left "CREATE USER is executed by the account service")
runStatementUncheckedM _ AlterUser{} = pure (Left "ALTER USER is executed by the account service")
runStatementUncheckedM _ DropUser{} = pure (Left "DROP USER is executed by the account service")
runStatementUncheckedM _ CreateRole{} = pure (Left "CREATE ROLE is executed by the privilege service")
runStatementUncheckedM _ DropRole{} = pure (Left "DROP ROLE is executed by the privilege service")
runStatementUncheckedM _ GrantPrivileges{} = pure (Left "GRANT is executed by the privilege service")
runStatementUncheckedM _ RevokePrivileges{} = pure (Left "REVOKE is executed by the privilege service")
runStatementUncheckedM _ GrantRole{} = pure (Left "GRANT is executed by the privilege service")
runStatementUncheckedM _ RevokeRole{} = pure (Left "REVOKE is executed by the privilege service")
runStatementUncheckedM _ q@CreateDatabase{} = runStatementM q
runStatementUncheckedM _ q@DropDatabase{} = runStatementM q
runStatementUncheckedM _ q@UseDatabase{} = runStatementM q
runStatementUncheckedM _ ShowDatabases = runStatementM ShowDatabases

-- | 只有需要读全表才能判断约束时才扫表
existingRows :: (MonadStorage m) => String -> Table -> m (Either String [Row])
existingRows tbl table
    | needsRows table = scan tbl
    | otherwise = pure (Right [])
  where
    needsRows t = any (constraintColumn . snd) (tableCols t)
    constraintColumn c = columnAutoIncrement c || columnUnique c || columnPrimaryKey c

-- | 插一行：补齐缺列、给自增值、收类型、查约束
buildRows :: Table -> [Row] -> [String] -> [[Expr]] -> Either String [Row]
buildRows table current cols rows = do
    values <- mapM (mapM (\e -> evalExpr e [])) rows
    if any (\vals -> length vals /= length cols) values
        then Left "column count does not match value count"
        else do
            let auto = autoColumn table
            go auto (autoStart auto current) values []
  where
    go _ _ [] acc = do
        let built = reverse acc
        mapM_ (runChecks table) built
        enforceNotNull (tableCols table) built
        enforceUnique (tableCols table) (current ++ built)
        Right built
    go auto counter (vals : rest) acc = do
        (row, counter') <- buildRow table auto cols counter vals
        go auto counter' rest (row : acc)

-- | 按列定义补出一行
buildRow :: Table -> Maybe (String, Column) -> [String] -> Int -> [Value] -> Either String (Row, Int)
buildRow table auto cols counter vals = go (tableCols table) counter []
  where
    provided = zip cols vals
    go [] c acc = Right (reverse acc, c)
    go ((name, col) : rest) c acc = do
        (v, c') <- pick name col c
        v' <- coerceValue (columnType col) v
        go rest c' ((name, v') : acc)
    pick name col c
        | Just (autoName, _) <- auto
        , name == autoName
        , maybe True (== VNull) (lookup name provided) = Right (VInt c, c + 1)
        | Just v <- lookup name provided = Right (v, bump name col c v)
        | Just d <- columnDefault col = Right (d, c)
        | columnNullable col = Right (VNull, c)
        | otherwise = Left ("INSERT: column " ++ name ++ " is NOT NULL and has no default")
    bump name _ c v
        | Just (autoName, _) <- auto
        , name == autoName = max (c + 1) (valueInt v + 1)
        | otherwise = c

valueInt :: Value -> Int
valueInt (VInt n) = n
valueInt _ = 0

-- | 自增列（至多一个）
autoColumn :: Table -> Maybe (String, Column)
autoColumn table = case [(n, c) | (n, c) <- tableCols table, columnAutoIncrement c] of
    (entry : _) -> Just entry
    [] -> Nothing

-- | 下一个自增值
autoStart :: Maybe (String, Column) -> [Row] -> Int
autoStart Nothing _ = 1
autoStart (Just (name, _)) rows = case [n | r <- rows, Just (VInt n) <- [lookup name r]] of
    [] -> 1
    ns -> maximum ns + 1

-- | 执行 CHECK 约束
runChecks :: Table -> Row -> Either String ()
runChecks table row = mapM_ check (tableCols table)
  where
    check (name, col) = case columnCheck col of
        Nothing -> Right ()
        Just text -> case parseExpression text of
            Left err -> Left ("CHECK on " ++ name ++ ": " ++ firstLine err)
            Right e -> case evalCondForRow e row of
                Left err -> Left ("CHECK on " ++ name ++ ": " ++ err)
                Right True -> Right ()
                Right False -> Left ("CHECK on " ++ name ++ " failed")

firstLine :: String -> String
firstLine = takeWhile (/= '\n')

-- | NOT NULL 检查
enforceNotNull :: [(String, Column)] -> [Row] -> Either String ()
enforceNotNull cols rows = mapM_ check cols
  where
    check (name, col)
        | columnNullable col = Right ()
        | any (\r -> lookup name r == Just VNull) rows = Left ("NOT NULL: column " ++ name ++ " cannot be null")
        | otherwise = Right ()

-- | 唯一性检查（NULL 之间不算冲突）
enforceUnique :: [(String, Column)] -> [Row] -> Either String ()
enforceUnique cols rows = mapM_ check targets
  where
    targets = [(n, c) | (n, c) <- cols, columnUnique c || columnPrimaryKey c]
    check (name, _) = go name [] (mapMaybe (lookup name) rows)
    go _ _ [] = Right ()
    go name seen (VNull : rest) = go name seen rest
    go name seen (v : rest)
        | v `elem` seen = Left ("UNIQUE: duplicate value in column " ++ name)
        | otherwise = go name (v : seen) rest

-- | UPDATE 的一行（条件与赋值都可能带子查询）
updateRowM :: (MonadStorage m) => Database -> Table -> Maybe Expr -> [(String, Expr)] -> Row -> m (Either String Row)
updateRowM db table cond asgns row = do
    keep <- case cond of
        Nothing -> pure (Right True)
        Just e -> evalCondForRowM db e row
    case keep of
        Left err -> pure (Left err)
        Right False -> pure (Right row)
        Right True -> do
            row' <- applyUpdatesM db table asgns row
            pure (row' >>= \r -> runChecks table r >> Right r)

-- | 依次求值、收类型并覆盖列（表达式可以带子查询）
applyUpdatesM :: (MonadStorage m) => Database -> Table -> [(String, Expr)] -> Row -> m (Either String Row)
applyUpdatesM _ _ [] row = pure (Right row)
applyUpdatesM db table ((col, e) : rest) row = do
    v <- evalExprM db e row
    case v of
        Left err -> pure (Left err)
        Right val -> case colType table col of
            Just def -> case coerceValue (columnType def) val of
                Left err -> pure (Left err)
                Right v' -> applyUpdatesM db table rest (setColumn col v' row)
            Nothing -> applyUpdatesM db table rest (setColumn col val row)

-- | 覆盖一列
setColumn :: String -> Value -> Row -> Row
setColumn col v = map (\(k, val) -> if k == col then (k, v) else (k, val))

-- | 按条件把行分成待删和保留两拨（条件可以带子查询）
splitByConditionM :: (MonadStorage m) => Database -> Maybe Expr -> [Row] -> m (Either String ([Row], [Row]))
splitByConditionM db cond = go [] []
  where
    go doomed kept [] = pure (Right (reverse doomed, reverse kept))
    go doomed kept (r : rs) = do
        hit <- case cond of
            Nothing -> pure (Right True)
            Just e -> evalCondForRowM db e r
        case hit of
            Left err -> pure (Left err)
            Right h -> go (if h then r : doomed else doomed) (if h then kept else r : kept) rs

-- | 表结构变更的统一入口：扫全表，改完整体回写
alterTable ::
    (MonadStorage m) =>
    Database ->
    String ->
    (Table -> Either String ([(String, Column)], [Row])) ->
    m (Either String [Row])
alterTable db tbl change = case lookupTable db tbl of
    Left e -> pure (Left e)
    Right table -> do
        rowsResult <- scan tbl
        case rowsResult of
            Left e -> pure (Left e)
            Right rows -> case change table{tableRows = rows} >>= checkAltered of
                Left e -> pure (Left e)
                Right (cols, newRows) -> do
                    result <- replaceSchema tbl cols newRows
                    pure (result >> Right [])
  where
    checkAltered (cols, rows) = do
        let table = Table tbl cols rows
        mapM_ (runChecks table) rows
        enforceNotNull cols rows
        enforceUnique cols rows
        Right (cols, rows)

-- | ADD COLUMN：老行补默认值或 NULL，自增列从 1 开始
addColumnTo :: (String, Column) -> [(String, Column)] -> [Row] -> Either String ([(String, Column)], [Row])
addColumnTo (name, col) cols rows
    | name `elem` map fst cols = Left ("ADD COLUMN: column already exists: " ++ name)
    | otherwise = do
        values <- fill
        Right (cols ++ [(name, col)], [r ++ [(name, v)] | (r, v) <- zip rows values])
  where
    fill
        | columnAutoIncrement col = mapM (coerceValue (columnType col) . VInt) [1 .. length rows]
        | Just d <- columnDefault col = mapM (\_ -> coerceValue (columnType col) d) rows
        | columnNullable col = mapM (\_ -> coerceValue (columnType col) VNull) rows
        | null rows = Right []
        | otherwise = Left ("ADD COLUMN: column " ++ name ++ " is NOT NULL and has no default")

-- | RENAME COLUMN：列定义与每一行的键一起改
renameColumnIn :: String -> String -> [(String, Column)] -> [Row] -> Either String ([(String, Column)], [Row])
renameColumnIn old new cols rows
    | old `notElem` map fst cols = Left ("RENAME COLUMN: unknown column: " ++ old)
    | new `elem` map fst cols = Left ("RENAME COLUMN: column already exists: " ++ new)
    | otherwise = Right (renameKeys cols, map renameKeys rows)
  where
    renameKeys = map (\(k, v) -> if k == old then (new, v) else (k, v))

-- | ALTER COLUMN TYPE：老值收进新类型
retypeColumn :: String -> ColumnType -> [(String, Column)] -> [Row] -> Either String ([(String, Column)], [Row])
retypeColumn col ty cols rows
    | col `notElem` map fst cols = Left ("ALTER COLUMN: unknown column: " ++ col)
    | otherwise = do
        rows' <- mapM convert rows
        Right (map (\entry@(n, c) -> if n == col then (n, c{columnType = ty}) else entry) cols, rows')
  where
    convert row = case lookup col row of
        Nothing -> Right row
        Just v -> do
            v' <- coerceValue ty v
            Right (map (\(k, x) -> if k == col then (k, v') else (k, x)) row)

-- | ALTER COLUMN SET/DROP DEFAULT
setColumnDefault :: String -> Maybe Value -> [(String, Column)] -> [Row] -> Either String ([(String, Column)], [Row])
setColumnDefault col def cols rows
    | col `notElem` map fst cols = Left ("ALTER COLUMN: unknown column: " ++ col)
    | otherwise = Right (map (\entry@(n, c) -> if n == col then (n, c{columnDefault = def}) else entry) cols, rows)

-- | ALTER COLUMN SET/DROP NOT NULL
setColumnNullable :: String -> Bool -> [(String, Column)] -> [Row] -> Either String ([(String, Column)], [Row])
setColumnNullable col nullable cols rows
    | col `notElem` map fst cols = Left ("ALTER COLUMN: unknown column: " ++ col)
    | otherwise = Right (map (\entry@(n, c) -> if n == col then (n, c{columnNullable = nullable}) else entry) cols, rows)
