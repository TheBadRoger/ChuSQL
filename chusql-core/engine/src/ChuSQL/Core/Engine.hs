module ChuSQL.Core.Engine (runStatement, runStatementM, rowsOf, BoundWrite, bindWrite, executeWriteM, validateTableRows) where

import ChuSQL.Core.Engine.Algebra.Eval (evalCondForRowM, evalExprM)
import ChuSQL.Core.Engine.Algebra.Expr (evalCondForRow, evalExpr)
import ChuSQL.Core.Engine.Domain
import ChuSQL.Core.Engine.Runtime.Dependencies (domainDependencies, queryTables, typeDependencies, validateDependencies)
import ChuSQL.Core.Engine.Runtime.Query (bindQuery, executeQueryM)
import ChuSQL.Core.Engine.Runtime.Relation (bindWriteColumns, orderRow)
import ChuSQL.Core.Engine.Semantic (prepare, prepareCheckExpression)
import ChuSQL.Core.Engine.Storage (MonadStorage (..))
import ChuSQL.Core.Engine.Storage.Memory (MemoryStorage (runMemoryStorage))
import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Engine.Syntax.Parser (parseExpression)
import ChuSQL.Core.Model
import Data.HashMap.Strict qualified as HM
import Data.HashSet qualified as HS
import Data.Maybe (mapMaybe)

-- 引擎入口：检查语义、按语句分发执行，另有内存实现与泛型版本。

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
    result <- schema
    case result of
        Left err -> pure (Left err)
        Right db
            | SelectDistinct{} <- q -> runStatementUncheckedM db q
            | Select{} <- q -> runStatementUncheckedM db q
            | null (queryTables q) -> case prepare db q >>= bindWriteColumns db of
                Left err -> pure (Left err)
                Right resolved -> runStatementUncheckedM db resolved
            | otherwise -> case bindWrite db q of
                Left err -> pure (Left err)
                Right bound -> executeWriteM bound

-- | 保存已有表写入的 schema、依赖及执行动作
data BoundWrite = BoundWrite Database Database WriteAction

-- | 区分已绑定的数据修改与结构变更
data WriteAction
    = InsertAction String InsertPlan [[Expr]]
    | UpdateAction String UpdatePlan
    | DeleteAction String [String] (Maybe Expr)
    | StructureAction Statement

-- | 保存 INSERT 目标、输入宽度及约束
data InsertPlan = InsertPlan [BoundInsertColumn] Int (Maybe (String, Column)) CheckPlan ConstraintPlan

-- | 保存 UPDATE 布局、赋值、条件及约束
data UpdatePlan = UpdatePlan [String] [BoundAssignment] (Maybe Expr) CheckPlan ConstraintPlan

-- | 仅用 schema 绑定已有表写入及结构变更
bindWrite :: Database -> Statement -> Either String BoundWrite
bindWrite db statement = case statement of
    Select{} -> unsupported
    SelectDistinct{} -> unsupported
    _
        | null (queryTables statement) -> unsupported
        | otherwise -> do
            resolved <- prepare catalog statement >>= bindWriteColumns catalog
            let sources = queryTables resolved
            mapM_ (lookupTable catalog) sources
            let names = sources ++ domainDependencies catalog sources ++ typeDependencies catalog (declaredTypes resolved)
                dependencies = [(name, table) | (name, table) <- catalog, name `elem` names]
            action <- bindWriteAction catalog resolved
            Right (BoundWrite catalog dependencies action)
  where
    catalog = [(name, table{tableRows = []}) | (name, table) <- db]
    unsupported = Left "bound writes require an existing table target"
    -- \| 新声明类型也须保留领域依赖
    declaredTypes resolved = case resolved of
        AddColumn _ (_, column) -> [columnType column]
        AlterColumnType _ _ ty -> [ty]
        _ -> []

-- | 复查结构后执行可复用的绑定写计划
executeWriteM :: (MonadStorage m) => BoundWrite -> m (Either String [Row])
executeWriteM (BoundWrite catalog dependencies action) = do
    current <- schema
    case current >>= validateDependencies dependencies of
        Left err -> pure (Left err)
        Right () -> withWriteSchemaGuard dependencies (executeWriteActionM catalog action)

-- | 在绑定阶段固定目标槽位与行约束
bindWriteAction :: Database -> Statement -> Either String WriteAction
bindWriteAction catalog statement = case statement of
    Insert target columns rows -> do
        declared <- lookupTable catalog target
        columnsEffective <- effectiveColumns catalog (tableCols declared)
        let table = declared{tableCols = columnsEffective}
        checks <- checkPlan catalog table
        let plan =
                InsertPlan
                    (bindInsertTargets (tableCols table) columns)
                    (length columns)
                    (autoColumn table)
                    checks
                    (constraintPlan (tableCols table))
        Right (InsertAction target plan rows)
    Update target assignments condition -> do
        declared <- lookupTable catalog target
        columnsEffective <- effectiveColumns catalog (tableCols declared)
        let table = declared{tableCols = columnsEffective}
        bound <- bindUpdateTargets (tableCols table) assignments
        checks <- checkPlan catalog table
        let plan = UpdatePlan (map fst (tableCols table)) bound condition checks (constraintPlan (tableCols table))
        Right (UpdateAction target plan)
    Delete target condition -> do
        table <- lookupTable catalog target
        Right (DeleteAction target (map fst (tableCols table)) condition)
    _ -> Right (StructureAction statement)

-- | 直接执行已绑定的数据修改或结构动作
executeWriteActionM :: (MonadStorage m) => Database -> WriteAction -> m (Either String [Row])
executeWriteActionM _ (InsertAction target plan rows) = do
    current <- existingRows target plan
    case current >>= \current' -> buildRows plan current' rows of
        Left err -> pure (Left err)
        Right built -> do
            result <- insertMany target built
            pure (result >> Right [])
executeWriteActionM catalog (UpdateAction target (UpdatePlan columns assignments condition checks constraints)) = do
    current <- scan target
    case current >>= mapM (orderRow columns) of
        Left err -> pure (Left err)
        Right rows -> do
            updated <- mapM (updateRowM catalog checks condition assignments) rows
            case sequence updated >>= \newRows -> enforceConstraints constraints [] newRows >> Right newRows of
                Left err -> pure (Left err)
                Right newRows -> do
                    result <- replaceAll target newRows
                    pure (result >> Right [])
executeWriteActionM catalog (DeleteAction target columns condition) = do
    current <- scan target
    case current >>= mapM (orderRow columns) of
        Left err -> pure (Left err)
        Right rows -> do
            split <- splitByConditionM catalog condition rows
            case split of
                Left err -> pure (Left err)
                Right (doomed, kept) -> case mapM rowId doomed of
                    Right ids -> do
                        result <- deleteKeys target ids
                        pure (result >> Right [])
                    Left _ -> do
                        result <- replaceAll target kept
                        pure (result >> Right [])
  where
    -- \| 取整数主键，否则使用原有整表删除路径
    rowId row = case lookup "id" row of
        Just (VInt key) -> Right key
        _ -> Left "row has no integer id"
executeWriteActionM catalog (StructureAction statement) = runStatementUncheckedM catalog statement

-- | 绑定并执行查询语句
runQueryM :: (MonadStorage m) => Database -> Statement -> m (Either String [Row])
runQueryM db q = case bindQuery db q of
    Left e -> pure (Left e)
    Right bound -> executeQueryM bound

-- | 分发语句，查询由 BoundQuery 绑定
runStatementUncheckedM :: (MonadStorage m) => Database -> Statement -> m (Either String [Row])
runStatementUncheckedM db q@SelectDistinct{} = runQueryM db q
runStatementUncheckedM db q@Select{} = runQueryM db q
runStatementUncheckedM _ Insert{} = pure (Left "INSERT requires a bound write plan")
runStatementUncheckedM _ Update{} = pure (Left "UPDATE requires a bound write plan")
runStatementUncheckedM _ Delete{} = pure (Left "DELETE requires a bound write plan")
runStatementUncheckedM db (CreateTable name cols) = case validateDomainDefaults db cols of
    Left err -> pure (Left err)
    Right () -> do
        result <- guardDomainCreation db (map (columnType . snd) cols) (createTable name cols)
        pure (result >> Right [])
runStatementUncheckedM db (CreateDomain name definition) = case canonicalDomain db name definition of
    Left err -> pure (Left err)
    Right normalized -> do
        let DomainDefinition column _ = normalized
        result <- guardDomainCreation db [columnType column] (createTable (domainTableName name) (domainColumns normalized))
        pure (result >> Right [])
runStatementUncheckedM _ (AlterDomain name action) = do
    result <- alterDomain (domainTableName name) (prepareDomainAlter name action)
    pure (result >> Right [])
runStatementUncheckedM _ (DropDomain name) = do
    result <- dropTable (domainTableName name)
    pure (result >> Right [])
runStatementUncheckedM db ShowDomains =
    pure
        ( Right
            [ [("domain", VStr (drop (length (domainTableName "")) name)), ("base_type", VStr (typeName (columnType base)))]
            | (name, table) <- db
            , isDomainTable name
            , Just base <- [lookup "base" (tableCols table)]
            ]
        )
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
    alterTable db tbl (\table -> addColumnTo db def (tableCols table) (tableRows table))
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
runStatementUncheckedM _ AlterIdentity{} = pure (Left "ALTER ROLE is executed by the account service")
runStatementUncheckedM _ ShowGrants = pure (Left "SHOW GRANTS is executed by the account service")
runStatementUncheckedM _ ShowRoles = pure (Left "SHOW ROLES is executed by the account service")
runStatementUncheckedM _ DropUser{} = pure (Left "DROP USER is executed by the account service")
runStatementUncheckedM _ CreateRole{} = pure (Left "CREATE ROLE is executed by the privilege service")
runStatementUncheckedM _ DropRole{} = pure (Left "DROP ROLE is executed by the privilege service")
runStatementUncheckedM _ GrantPrivileges{} = pure (Left "GRANT is executed by the privilege service")
runStatementUncheckedM _ RevokeGrantOption{} = pure (Left "REVOKE is executed by the privilege service")
runStatementUncheckedM _ RevokePrivileges{} = pure (Left "REVOKE is executed by the privilege service")
runStatementUncheckedM _ GrantRole{} = pure (Left "GRANT is executed by the privilege service")
runStatementUncheckedM _ RevokeRole{} = pure (Left "REVOKE is executed by the privilege service")
runStatementUncheckedM _ q@CreateDatabase{} = runStatementM q
runStatementUncheckedM _ q@DropDatabase{} = runStatementM q
runStatementUncheckedM _ q@UseDatabase{} = runStatementM q
runStatementUncheckedM _ ShowDatabases = runStatementM ShowDatabases
runStatementUncheckedM _ BeginTransaction = pure (Left "BEGIN is executed by the session")
runStatementUncheckedM _ BeginSerializableTransaction = pure (Left "BEGIN is executed by the session")
runStatementUncheckedM _ CommitTransaction = pure (Left "COMMIT is executed by the session")
runStatementUncheckedM _ RollbackTransaction = pure (Left "ROLLBACK is executed by the session")
runStatementUncheckedM _ Savepoint{} = pure (Left "SAVEPOINT is executed by the session")
runStatementUncheckedM _ RollbackToSavepoint{} = pure (Left "ROLLBACK TO is executed by the session")
runStatementUncheckedM _ ReleaseSavepoint{} = pure (Left "RELEASE is executed by the session")

-- | 建表时在实际发布点复查领域定义
guardDomainCreation :: (MonadStorage m) => Database -> [ColumnType] -> m (Either String a) -> m (Either String a)
guardDomainCreation db types action
    | null dependencies = action
    | otherwise = withWriteSchemaGuard dependencies action
  where
    names = typeDependencies db types
    dependencies = [(name, table) | (name, table) <- db, name `elem` names]

-- | 只有需要读全表才能判断约束时才扫表
existingRows :: (MonadStorage m) => String -> InsertPlan -> m (Either String [Row])
existingRows tbl (InsertPlan _ _ auto _ (ConstraintPlan _ _ unique))
    | Just _ <- auto = scan tbl
    | not (null unique) = scan tbl
    | otherwise = pure (Right [])

-- | 插一行：补齐缺列、给自增值、收类型、查约束
buildRows :: InsertPlan -> [Row] -> [[Expr]] -> Either String [Row]
buildRows (InsertPlan layout width auto checks constraints) current rows = do
    values <- mapM (mapM (`evalExpr` [])) rows
    if any (\vals -> length vals /= width) values
        then Left "column count does not match value count"
        else go (autoStart auto current) values []
  where
    -- \| 逐行构造，最后统一查一遍约束
    go _ [] acc = do
        let built = reverse acc
        mapM_ (runChecks checks) built
        enforceConstraints constraints current built
        Right built
    go counter (vals : rest) acc = do
        (row, counter') <- buildRow layout auto counter vals
        go counter' rest (row : acc)

-- | 保存输出列定义及可选 VALUES 槽位
data BoundInsertColumn = BoundInsertColumn String Column (Maybe Int)

-- | 把已校验的目标清单绑定为输入槽位
bindInsertTargets :: [(String, Column)] -> [String] -> [BoundInsertColumn]
bindInsertTargets columns targets =
    [BoundInsertColumn name column (lookup name positions) | (name, column) <- columns]
  where
    positions = zip targets [0 ..]

-- | 按绑定槽位和列定义补出一行
buildRow :: [BoundInsertColumn] -> Maybe (String, Column) -> Int -> [Value] -> Either String (Row, Int)
buildRow layout auto counter vals = go layout counter []
  where
    provided = HM.fromList (zip [0 :: Int ..] vals)
    -- \| 按列定义逐列补值
    go [] c acc = Right (reverse acc, c)
    go (BoundInsertColumn name col position : rest) c acc = do
        supplied <- mapM (\slot -> maybe (Left ("missing bound INSERT value: " ++ name)) Right (HM.lookup slot provided)) position
        (v, c') <- pick name col supplied c
        v' <- coerceValue (columnType col) v
        go rest c' ((name, v') : acc)
    -- \| 决定这一列取什么值
    pick name col supplied c
        | Just (autoName, _) <- auto
        , name == autoName
        , maybe True (== VNull) supplied =
            Right (VInt c, c + 1)
        | Just v <- supplied = Right (v, bump name c v)
        | Just d <- columnDefault col = Right (d, c)
        | columnNullable col = Right (VNull, c)
        | otherwise = Left ("INSERT: column " ++ name ++ " is NOT NULL and has no default")
    -- \| 自增值跟着已有的值往前推
    bump name c v
        | Just (autoName, _) <- auto
        , name == autoName =
            max (c + 1) (valueInt v + 1)
        | otherwise = c

-- | 取自增值，不是整数就当 0
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

-- | 一次解析出来的 CHECK 条件
data CheckPlan = CheckPlan [String] [(String, Expr)] [(String, Int, Expr)]

-- | 按 schema 解析并绑定表上全部 CHECK
checkPlan :: Database -> Table -> Either String CheckPlan
checkPlan db table = do
    local <- mapM parse (mapMaybe checked (tableCols table))
    domains <- domainCheckPlan db (tableCols table)
    Right (CheckPlan (map fst (tableCols table)) local domains)
  where
    -- \| 只挑出带 CHECK 文本的列
    checked (name, col) = (,) name <$> columnCheck col
    -- \| 解析一列的 CHECK 文本
    parse (name, text) = case parseExpression text of
        Left err -> Left ("CHECK on " ++ name ++ ": " ++ firstLine err)
        Right e -> do
            bound <- prepareCheckExpression (tableCols table) e
            Right (name, bound)

-- | 执行解析好的 CHECK 条件
runChecks :: CheckPlan -> Row -> Either String ()
runChecks (CheckPlan columns plan domains) row = do
    ordered <- orderRow columns row
    mapM_ (check ordered) plan
    runDomainChecks domains ordered
  where
    -- \| 跑一条 CHECK
    check ordered (name, e) = case evalCondForRow e ordered of
        Left err -> Left ("CHECK on " ++ name ++ ": " ++ err)
        Right True -> Right ()
        Right False -> Left ("CHECK on " ++ name ++ " failed")

-- 校验最终行集的领域、检查与唯一约束。
validateTableRows :: Database -> Table -> Either String ()
validateTableRows db table = do
    effective <- effectiveColumns db (tableCols table)
    plan <- checkPlan db table
    let rows = tableRows table
    mapM_ (runChecks plan) rows
    ordered <- mapM (orderRow (map fst effective)) rows
    enforceConstraints (constraintPlan effective) [] ordered

-- | 只取第一行（报错提示用）
firstLine :: String -> String
firstLine = takeWhile (/= '\n')

-- | 保存非空及唯一键的固定列槽位
data ConstraintPlan = ConstraintPlan [String] [(String, Expr)] [(String, Expr)]

-- | 按 schema 一次绑定行集约束目标
constraintPlan :: [(String, Column)] -> ConstraintPlan
constraintPlan columns = ConstraintPlan (map fst columns) required unique
  where
    slots = [(name, column, ColumnSlot slot name) | (slot, (name, column)) <- zip [0 ..] columns]
    required = [(name, reference) | (name, column, reference) <- slots, not (columnNullable column)]
    unique = [(name, reference) | (name, column, reference) <- slots, columnUnique column || columnPrimaryKey column]

-- | 检查新行非空及合并旧行后的唯一性
enforceConstraints :: ConstraintPlan -> [Row] -> [Row] -> Either String ()
enforceConstraints (ConstraintPlan columns required unique) current rows = do
    enforceNotNull required rows
    orderedCurrent <- mapM (orderRow columns) current
    enforceUnique unique (orderedCurrent ++ rows)

-- | NOT NULL 检查
enforceNotNull :: [(String, Expr)] -> [Row] -> Either String ()
enforceNotNull targets rows = mapM_ check targets
  where
    -- \| 按 schema 槽位检查 NOT NULL
    check (name, reference) = do
        values <- mapM (evalExpr reference) rows
        if VNull `elem` values
            then Left ("NOT NULL: column " ++ name ++ " cannot be null")
            else Right ()

-- | 唯一性检查（NULL 之间不算冲突）
enforceUnique :: [(String, Expr)] -> [Row] -> Either String ()
enforceUnique targets rows = mapM_ check targets
  where
    -- \| 查这一列有没有重复值
    check (name, reference) = mapM (evalExpr reference) rows >>= go name HS.empty
    -- \| 逐个值比过去
    go _ _ [] = Right ()
    -- \| NULL 不算冲突，跳过
    go name seen (VNull : rest) = go name seen rest
    go name seen (v : rest)
        | HS.member (canonicalValue v) seen = Left ("UNIQUE: duplicate value in column " ++ name)
        | otherwise = go name (HS.insert (canonicalValue v) seen) rest

-- | UPDATE 的一行（条件与赋值都可能带子查询）
updateRowM :: (MonadStorage m) => Database -> CheckPlan -> Maybe Expr -> [BoundAssignment] -> Row -> m (Either String Row)
updateRowM db plan cond asgns row = do
    keep <- case cond of
        Nothing -> pure (Right True)
        Just e -> evalCondForRowM db e row
    case keep of
        Left err -> pure (Left err)
        Right False -> pure (Right row)
        Right True -> do
            row' <- applyUpdatesM db asgns row
            pure (row' >>= \r -> runChecks plan r >> Right r)

-- | 保存赋值目标槽位、列定义和右侧表达式
data BoundAssignment = BoundAssignment Int String Column Expr

-- | 按 schema 一次绑定全部赋值目标
bindUpdateTargets :: [(String, Column)] -> [(String, Expr)] -> Either String [BoundAssignment]
bindUpdateTargets columns = mapM bindTarget
  where
    -- \| 只接受唯一目标并保留列定义
    bindTarget (name, expression) = case [(slot, column) | (slot, (key, column)) <- zip [0 ..] columns, key == name] of
        [(slot, column)] -> Right (BoundAssignment slot name column expression)
        [] -> Left ("UPDATE: unknown target column: " ++ name)
        _ -> Left ("UPDATE: ambiguous target column: " ++ name)

-- | 依次求值并按目标槽位收类型和覆盖
applyUpdatesM :: (MonadStorage m) => Database -> [BoundAssignment] -> Row -> m (Either String Row)
applyUpdatesM _ [] row = pure (Right row)
applyUpdatesM db (assignment@(BoundAssignment _ _ _ e) : rest) row = do
    v <- evalExprM db e row
    case v >>= writeAssignment assignment row of
        Left err -> pure (Left err)
        Right updated -> applyUpdatesM db rest updated

-- | 校验目标位置并只覆盖该槽位的值
writeAssignment :: BoundAssignment -> Row -> Value -> Either String Row
writeAssignment (BoundAssignment slot name column _) row value = case splitAt slot row of
    (before, (key, _) : after)
        | slot >= 0
        , key == name -> do
            coerced <- coerceValue (columnType column) value
            Right (before ++ ((name, coerced) : after))
    (_, []) -> Left ("missing bound column: " ++ name)
    _ -> Left ("bound column position mismatch: " ++ name)

-- | 按条件把行分成待删和保留两拨（条件可以带子查询）
splitByConditionM :: (MonadStorage m) => Database -> Maybe Expr -> [Row] -> m (Either String ([Row], [Row]))
splitByConditionM db cond = go [] []
  where
    -- \| 逐行分流
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
        case rowsResult >>= mapM (orderRow (map fst (tableCols table))) of
            Left e -> pure (Left e)
            Right rows -> case change table{tableRows = rows} >>= checkAltered of
                Left e -> pure (Left e)
                Right (cols, newRows) -> do
                    result <- replaceSchema tbl cols newRows
                    pure (result >> Right [])
  where
    -- \| 改完再统一查一遍约束
    checkAltered (cols, rows) = do
        let table = Table tbl cols rows Nothing Nothing Nothing
        validateDomainDefaults db cols
        validateTableRows db table
        Right (cols, rows)

-- | ADD COLUMN：老行补默认值或 NULL
addColumnTo :: Database -> (String, Column) -> [(String, Column)] -> [Row] -> Either String ([(String, Column)], [Row])
addColumnTo db (name, declared) cols rows
    | name `elem` map fst cols = Left ("ADD COLUMN: column already exists: " ++ name)
    | otherwise = do
        effective <- effectiveColumns db [(name, declared)]
        values <- case effective of
            [(_, column)] -> fill column
            _ -> Left "invalid added column layout"
        Right (cols ++ [(name, declared)], [r ++ [(name, v)] | (r, v) <- zip rows values])
  where
    -- \| 使用领域继承属性填充已有行
    fill col
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
    -- \| 把这一列的老值收进新类型
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
