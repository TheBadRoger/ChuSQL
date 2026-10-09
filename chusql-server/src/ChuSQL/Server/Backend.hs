{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.Backend (
    StatementResult (..),
    Backend (..),
    columnsFromStatement,
    currentStamp,
    exprTables,
    internalTable,
    grantsTable,
    ipcBackend,
    memoryBackend,
    membersTable,
    optionsTable,
    rolesTable,
    scopeDdl,
    statementNeedsDatabase,
    statementTables,
    systemDatabaseName,
    tableInfoOf,
    tableRefsOf,
) where

import ChuSQL.Core.Engine (runStatement, runStatementM)
import ChuSQL.Core.Model
import ChuSQL.Core.Engine.Semantic (prepare)
import ChuSQL.Core.Engine.Storage (MonadStorage (schema, snapshot))
import ChuSQL.Core.Engine.Storage.IPC (
    Account (..),
    Env,
    IPCStorage (..),
    Request (..),
    Response (..),
    SchemaColumn (..),
    TableInfo (..),
    envForDatabase,
    sendRawRequest,
    sendRequest,
 )
import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Engine.Syntax.Parser (parseStatement)
import ChuSQL.Core.Protocol (TxnOp (..))
import Control.Concurrent.MVar (MVar, modifyMVar, readMVar)
import Data.List (nub)
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Text as T

-- 后端抽象：跑语句、取数据字典与账号，分 IPC（真存储）和内存（测试）两套实现。

data StatementResult = StatementResult
    { srColumns :: [String]
    , srRows :: [Row]
    }

data Backend = Backend
    { beStatement :: String -> IO (Either String StatementResult)
    , beCatalog :: IO (Either String [TableInfo])
    , bePing :: IO Bool
    , beAccounts :: Request -> IO (Either String [Account])
    , beWithDatabase :: String -> Backend
    , beDatabases :: IO (Either String [String])
    -- | 原样转发一条存储请求
    , beStorage :: A.Value -> IO (Either String A.Value)
    -- | 取一份整库快照，当事务的起点
    , beSnapshot :: IO (Either String Database)
    -- | 事务提交：把一批写操作交给存储层应用
    , beApplyTransaction :: [TxnOp] -> IO (Either String ())
    }

-- | 按给定列名取列，固定列顺序
columnsFromStatement :: Database -> Statement -> [String]
columnsFromStatement db stmt = case prepare db stmt of
    Right (Select cols _ _ _ _ _) -> cols
    Right (SelectExpr items _ _ _ _ _) -> map fst items
    Right ShowDomains -> ["domain", "base_type"]
    _ -> []

-- | 走本地存储链路的后端
ipcBackend :: IO Backend
ipcBackend = do
    let backend current = Backend
            { beStatement = \sql -> runIpc current sql
            , beCatalog = do
                response <- sendRequest (ReqInDatabase current ReqListCatalog)
                pure $ case response of
                    RespCatalog infos -> Right (filter (not . isDomainTable . tiTable) infos)
                    RespError err -> Left err
                    _ -> Left "unexpected catalog response"
            , bePing = do
                resp <- sendRequest ReqPing
                pure (case resp of RespPong -> True; _ -> False)
            , beAccounts = \req -> do
                response <- sendRequest req
                pure $ case response of
                    RespAccounts accounts -> Right accounts
                    RespError err -> Left err
                    _ -> Left "unexpected account response"
            , beWithDatabase = backend
            , beDatabases = do
                response <- sendRequest (ReqDatabase "list_databases" "")
                pure $ case response of
                    RespTables names -> Right names
                    RespError err -> Left err
                    _ -> Left "unexpected database response"
            , beStorage = \payload -> runRaw payload
            , beSnapshot = runIPCStorageIn snapshot (sessionEnv current)
            , beApplyTransaction = \ops -> do
                response <- sendRequest (ReqInDatabase current (ReqApplyTransaction ops))
                pure $ case response of
                    RespOk -> Right ()
                    RespError err -> Left err
                    _ -> Left "unexpected transaction response"
            }
    -- 没有默认库：没选库时 current 是空串，任何请求都得先选库
    pure (backend "")
  where
    -- | 会话的存储环境：库名跟这条连接走
    sessionEnv :: String -> Env
    sessionEnv current = envForDatabase (if null current then Nothing else Just current)

    -- | 转发一条存储请求并解析响应
    runRaw :: A.Value -> IO (Either String A.Value)
    runRaw payload = do
        response <- sendRawRequest (BL.toStrict (A.encode payload))
        pure $ case response of
            Left err -> Left err
            Right raw -> case A.eitherDecodeStrict raw of
                Left err -> Left ("storage response is not valid JSON: " ++ err)
                Right value -> Right value

    -- | 走存储链路跑一条语句：表名不再补当前库前缀
    -- 引擎侧的数据字典按当前库过滤之后键是裸表名，补前缀会让查表落空；
    -- 库名由 database 字段随请求带走，存储层自己按它分发。
    runIpc :: String -> String -> IO (Either String StatementResult)
    runIpc current sql = case parseStatement sql >>= scopeDdl current of
        Left e -> pure (Left e)
        Right stmt | null current && statementNeedsDatabase stmt -> pure (Left "no database selected")
        Right stmt -> do
            let env = sessionEnv current
            result <- runIPCStorageIn (runStatementM stmt) env
            case result of
                Left e -> pure (Left e)
                Right rows -> do
                    cols <- case rows of
                        [] -> do
                            db <- runIPCStorageIn schema env
                            pure (fmap (\structures -> columnsFromStatement structures stmt) db)
                        (r : _) -> pure (Right (map fst r))
                    pure (fmap (\names -> StatementResult names rows) cols)

-- | 测试用内存后端，状态是一个 Database
memoryBackend :: String -> MVar Database -> Backend
memoryBackend = memoryBackendWith False

-- | 构造内存后端，Bool 决定普通库还是系统库视角
memoryBackendWith :: Bool -> String -> MVar Database -> Backend
memoryBackendWith trusted name ref =
    Backend
        { beStatement = \sql -> case parseStatement sql >>= scopeDdl (if trusted then systemDatabaseName else name) of
            Left e -> pure (Left e)
            -- 内存夹具也有"库"的概念：USE 只能切到它有的两个名字（自己 + system），
            -- 与 beDatabases 口径一致；引擎的内存存储不实现库管理，所以拦在这里。
            Right (UseDatabase target)
                | target == name || target == systemDatabaseName -> pure (Right (StatementResult [] []))
                | otherwise -> pure (Left ("unknown database: " ++ target))
            Right stmt -> modifyMVar ref $ \db -> case runStatement (scoped db) stmt of
                Left e -> pure (db, Left e)
                Right (db', rows)
                    | any (\(key, _) -> hidden key && not (domainKey key)) db' -> pure (db, Left "reserved system table")
                    | otherwise -> pure (syncMemoryObjects name (mergeScoped db db'), Right (StatementResult (colsOf (scoped db) stmt rows) rows))
        , beCatalog = do
            db <- readMVar ref
            pure (Right (map tableInfoOf (filter (not . internalTable . fst) (scoped db))))
        , bePing = pure True
        , beAccounts = \req -> do
            stamp <- currentStamp
            modifyMVar ref $ \db -> case memoryAccounts db stamp req of
                Left err -> pure (db, Left err)
                Right accounts -> case memoryAccounts db stamp ReqAccountsList of
                    Left err -> pure (db, Left err)
                    Right previous -> pure (case req of ReqAccountsList -> db; ReqCatalogManage ReqAccountsList -> db; _ -> storeMemoryIdentities db previous accounts, Right accounts)
        , beWithDatabase = withDatabase
        , beDatabases = pure (Right (nub [name, systemDatabaseName]))
        , beStorage = memorySecurityRequest name ref
        , beSnapshot = Right . scoped <$> readMVar ref
        , beApplyTransaction = \ops -> modifyMVar ref $ \db -> case applyTransactionMemory ops (scoped db) of
            Left err -> pure (db, Left err)
            Right db' -> pure (mergeScoped db db', Right ())
        }
  where
    -- | 按视角过滤库里的表
    scoped db = [(lastTablePart key, table {tableName = lastTablePart key}) | (key, table) <- db, inScope key, domainKey key || not (hidden key)]
    -- | 限定键只属于指定工作库
    inScope key = case break (== '.') key of
        (_, []) -> True
        (database, _) -> T.toLower (T.pack database) == T.toLower (T.pack (if trusted then systemDatabaseName else name))
    -- | 该视角下要隐藏的表
    hidden = (if trusted then blocked else internalTable) . lastTablePart
    -- | 判断裸名或限定名是否为类型目录
    domainKey = isDomainTable . lastTablePart
    -- | 回写当前库并保留其他库和内部目录
    mergeScoped original changed =
        filter (\(key, _) -> not (inScope key) || (hidden key && not (domainKey key))) original
            ++ [(storageKey original key, table) | (key, table) <- changed]
    -- | 保留夹具中的原始数据库限定键
    storageKey original key = case [saved | (saved, _) <- original, inScope saved, lastTablePart saved == key] of
        saved : _ -> saved
        [] | any (\(saved, _) -> inScope saved && '.' `elem` saved) original -> (if trusted then systemDatabaseName else name) ++ "." ++ key
        [] -> key
    -- | 取结果列名，空结果回落到解析
    colsOf db stmt rows = case rows of
        [] -> columnsFromStatement db stmt
        (r : _) -> map fst r

    -- | 按目标库名挑视角
    withDatabase target
        | target == systemDatabaseName = memoryBackendWith True name ref
        | target == name = memoryBackendWith False name ref
        | null target = noDatabaseBackend (memoryBackendWith False name ref)
        | otherwise = unavailableDatabase ("unknown database: " ++ target) (memoryBackendWith False name ref)

-- 更新内存夹具的稳定表编号。
syncMemoryObjects :: String -> Database -> Database
syncMemoryObjects database db = (registry, Table registry [("name", TStr), ("id", TInt)] records Nothing) : filter ((/= registry) . fst) db
  where
    registry = "__system_object_ids"
    previous = [(key, identifier) | row <- maybe [] tableRows (lookup registry db), Just (VStr key) <- [lookup "name" row], Just (VInt identifier) <- [lookup "id" row]]
    keys = [if '.' `elem` key then key else database ++ "." ++ key | (key, _) <- db, not (internalTable (lastTablePart key))]
    retained = [(key, identifier) | (key, identifier) <- previous, key `elem` keys]
    high = maximum (0 : map snd previous)
    added = zip [key | key <- keys, key `notElem` map fst retained] [high + 1 ..]
    records = [[("name", VStr key), ("id", VInt identifier)] | (key, identifier) <- ("__high_water", high + length added) : retained ++ added]

-- 执行内存夹具的权限快照协议。
memorySecurityRequest :: String -> MVar Database -> A.Value -> IO (Either String A.Value)
memorySecurityRequest database ref payload = modifyMVar ref $ \original -> do
    let db = syncMemoryObjects database original
    pure $ case operation db of
        Left err -> (original, Left err)
        Right (next, value) -> (next, Right value)
  where
    -- 读取协议文本字段。
    field key = case payload of
        A.Object fields -> case KM.lookup key fields of
            Just (A.String value) -> Just value
            _ -> Nothing
        _ -> Nothing
    -- 解析并执行一条权限请求。
    operation db = case field "method" of
        Just "resolve_object" -> do
            selected <- maybe (Left "missing database") Right (field "database")
            if selected `notElem` [T.pack database, "system"] then Left "unknown database" else do
                identifier <- case field "table" of
                    Nothing -> Right A.Null
                    Just table -> case [number | row <- maybe [] tableRows (lookup "__system_object_ids" db), lookup "name" row == Just (VStr (T.unpack (selected <> "." <> table))), Just (VInt number) <- [lookup "id" row]] of
                        [number] -> Right (A.toJSON number)
                        _ -> Left "unknown table"
                Right (db, A.object ["status" A..= ("object" :: Text), "database_id" A..= (if selected == "system" then (2 :: Int) else 1), "object_id" A..= identifier])
        Just "object_acl_read" -> Right (db, A.object ["status" A..= ("rows" :: Text), "rows" A..= [A.object ["id" A..= (1 :: Int), "payload" A..= value] | row <- maybe [] tableRows (lookup acl db), Just (VStr value) <- [lookup "payload" row]]])
        Just "object_acl_replace" -> do
            let previous = case lookup acl db of Nothing -> Nothing; Just table -> case tableRows table of [[("payload", VStr value)]] -> Just (T.pack value); _ -> Just "invalid ACL"
            if previous /= field "expected" then Left "object ACL changed concurrently" else do
                value <- maybe (Left "missing ACL payload") Right (field "payload")
                Right ((acl, Table acl [("payload", TStr)] [[("payload", VStr (T.unpack value))]] Nothing) : filter ((/= acl) . fst) db, A.object ["status" A..= ("ok" :: Text)])
        _ -> Left "storage is not available in this backend"
    acl = "__system_object_acl"

-- | 把后端的语句与字典都改成同一条错误
unavailableDatabase :: String -> Backend -> Backend
unavailableDatabase message backend = backend
    { beStatement = \_ -> pure (Left message)
    , beCatalog = pure (Left message)
    , beSnapshot = pure (Left message)
    , beApplyTransaction = \_ -> pure (Left message)
    }

-- | 未选库时的视角：只放行不碰表的语句
noDatabaseBackend :: Backend -> Backend
noDatabaseBackend backend = backend
    { beStatement = \sql -> case parseStatement sql of
        Left e -> pure (Left e)
        Right stmt
            | statementNeedsDatabase stmt -> pure (Left "no database selected")
            | otherwise -> beStatement backend sql
    , beCatalog = pure (Left "no database selected")
    , beSnapshot = pure (Left "no database selected")
    , beApplyTransaction = \_ -> pure (Left "no database selected")
    }

-- | 把一批事务写操作应用到内存库：先按 id 删，再按追加写
applyTransactionMemory :: [TxnOp] -> Database -> Either String Database
applyTransactionMemory ops db = foldl step (Right db) ops
  where
    step acc op = do
        current <- acc
        case op of
            TxnDelete name keys -> do
                table <- lookupTable current name
                pure (put (name, table {tableRows = [r | r <- tableRows table, not (doomed keys r)]}) current)
            TxnUpsert name rows -> do
                table <- lookupTable current name
                pure (put (name, table {tableRows = tableRows table ++ rows}) current)
            TxnReplace name rows -> do
                table <- lookupTable current name
                pure (put (name, table {tableRows = rows}) current)

    -- | 换掉一张表，其余保持原顺序
    put entry = map (\one@(name, _) -> if name == fst entry then entry else one)

    -- | 这一行要不要删：按行里的 id 列比
    doomed keys r = case lookup "id" r of
        Just (VInt k) -> k `elem` keys
        _ -> False

-- | 语句里出现的表
statementTables :: Statement -> [Text]
statementTables stmt = nub (case stmt of
    Select{} ->
        tableRefsOf (selectFrom stmt)
            ++ concatMap exprTables (maybe [] (: []) (selectWhere stmt) ++ map Col (selectCols stmt ++ selectGroupBy stmt ++ map fst (selectOrderBy stmt)))
    SelectExpr{} ->
        tableRefsOf (selectFrom stmt)
            ++ concatMap exprTables (maybe [] (: []) (selectWhere stmt) ++ map snd (selectItems stmt) ++ map Col (selectGroupBy stmt ++ map fst (selectOrderBy stmt)))
    Insert table _ rows -> T.pack table : concatMap exprTables (concat rows)
    Update table assigns cond -> T.pack table : concatMap exprTables (map snd assigns ++ maybe [] (: []) cond)
    Delete table cond -> T.pack table : concatMap exprTables (maybe [] (: []) cond)
    CreateTable table _ -> [T.pack table]
    DropTable table -> [T.pack table]
    CreateIndex table _ -> [T.pack table]
    DropIndex table _ -> [T.pack table]
    DropColumn table _ -> [T.pack table]
    AddColumn table _ -> [T.pack table]
    RenameColumn table _ _ -> [T.pack table]
    AlterColumnType table _ _ -> [T.pack table]
    AlterColumnDefault table _ _ -> [T.pack table]
    AlterColumnNull table _ _ -> [T.pack table]
    _ -> [])

-- | 语句需不需要一个当前库：出现的表里有没写库名的就必须要
statementNeedsDatabase :: Statement -> Bool
statementNeedsDatabase stmt = case stmt of
    CreateTable{} -> True
    DropTable{} -> True
    CreateIndex{} -> True
    DropIndex{} -> True
    DropColumn{} -> True
    AddColumn{} -> True
    RenameColumn{} -> True
    AlterColumnType{} -> True
    AlterColumnDefault{} -> True
    AlterColumnNull{} -> True
    CreateDomain{} -> True
    DropDomain{} -> True
    ShowDomains -> True
    _ -> not (null (statementTables stmt))

-- | 将当前库限定的结构目标转为裸表名
scopeDdl :: String -> Statement -> Either String Statement
scopeDdl current statement = case statement of
    CreateTable table columns -> (\key -> CreateTable key columns) <$> target table
    DropTable table -> DropTable <$> target table
    CreateIndex table column -> (\key -> CreateIndex key column) <$> target table
    DropIndex table column -> (\key -> DropIndex key column) <$> target table
    DropColumn table column -> (\key -> DropColumn key column) <$> target table
    AddColumn table column -> (\key -> AddColumn key column) <$> target table
    RenameColumn table old new -> (\key -> RenameColumn key old new) <$> target table
    AlterColumnType table column ty -> (\key -> AlterColumnType key column ty) <$> target table
    AlterColumnDefault table column value -> (\key -> AlterColumnDefault key column value) <$> target table
    AlterColumnNull table column nullable -> (\key -> AlterColumnNull key column nullable) <$> target table
    _ -> statement <$ mapM_ checkReadTarget (statementTables statement)
  where
    -- | 数据引用限定在当前工作库
    checkReadTarget table
        | null current = Left "no database selected"
        | otherwise = case T.breakOn "." table of
            (_, suffix) | T.null suffix -> Right ()
            (database, _) | T.toLower database == T.toLower (T.pack current) -> Right ()
            _ -> Left ("table is outside the current database: " ++ T.unpack table)
    -- | 校验库限定名并取当前库中的表名
    target table
        | null current = Left "no database selected"
        | otherwise = case break (== '.') table of
            (_, []) -> Right table
            (database, '.' : key)
                | T.toLower (T.pack database) == T.toLower (T.pack current) -> Right key
            _ -> Left ("DDL target is outside the current database: " ++ table)

-- | FROM 子句里的所有表
tableRefsOf :: FromClause -> [Text]
tableRefsOf FromUnit = []
tableRefsOf (FromTable _ name) = [T.pack name]
tableRefsOf (FromSubquery _ stmt) = statementTables stmt
tableRefsOf (FromJoin _ left right cond) = tableRefsOf left ++ tableRefsOf right ++ exprTables cond

-- | 表达式里出现的表（子查询是唯一的来路）
exprTables :: Expr -> [Text]
exprTables expr = case expr of
    Col column | [database, table, _] <- T.splitOn "." (T.pack column) -> [database <> "." <> table]
    ScalarSub sub -> statementTables (subqueryStatement sub)
    ExistsSub sub _ -> statementTables (subqueryStatement sub)
    InSub value sub _ -> exprTables value ++ statementTables (subqueryStatement sub)
    QuantCmp _ value sub _ -> exprTables value ++ statementTables (subqueryStatement sub)
    InList value items _ -> concatMap exprTables (value : items)
    Add a b -> both a b
    Sub a b -> both a b
    Mul a b -> both a b
    Div a b -> both a b
    Gt a b -> both a b
    Lt a b -> both a b
    Eq a b -> both a b
    GtE a b -> both a b
    LtE a b -> both a b
    NotEq a b -> both a b
    And a b -> both a b
    Or a b -> both a b
    Neg a -> exprTables a
    IsNull a -> exprTables a
    IsNotNull a -> exprTables a
    CountOf a -> exprTables a
    SumOf a -> exprTables a
    AvgOf a -> exprTables a
    MinOf a -> exprTables a
    MaxOf a -> exprTables a
    _ -> []
  where
    -- | 两侧都取表名
    both a b = exprTables a ++ exprTables b

-- | 一张表的线上信息（内存实现：没有索引，统计与直方图现算）
tableInfoOf :: (String, Table) -> TableInfo
tableInfoOf (name, tbl) =
    TableInfo
        { tiTable = name
        , tiColumns = [wireColumn c ty | (c, ty) <- tableCols tbl]
        , tiRows = length (tableRows tbl)
        , tiIndexes = []
        , tiStats = [(c, distinctOf c, False) | (c, _) <- tableCols tbl]
        , tiHistograms = [(c, h) | (c, _) <- tableCols tbl, Just h <- [histogramOf (columnValues c)]]
        }
  where
    -- | 一列的所有值
    columnValues c = [v | r <- tableRows tbl, Just v <- [lookup c r]]

    -- | 一列的不同值个数
    distinctOf c = length (nub (columnValues c))

-- | 本地列类型转线上字符串（和 IPC 那边同一套写法）
wireType :: Column -> String
wireType = typeName . columnType

-- | 本地列转线上 schema
wireColumn :: String -> Column -> SchemaColumn
wireColumn name col =
    SchemaColumn
        { scName = name
        , scType = wireType col
        , scNullable = columnNullable col
        , scDefault = columnDefault col
        , scAutoIncrement = columnAutoIncrement col
        , scPrimaryKey = columnPrimaryKey col
        , scUnique = columnUnique col
        , scCheck = columnCheck col
        }

-- | 系统库名，账号、角色与授权表住在这里
systemDatabaseName :: String
systemDatabaseName = "system"

usersTable :: String
usersTable = "__system_users"

-- | 角色与授权表的名字，在系统库里但服务自己要读写
rolesTable :: String
rolesTable = "__system_roles"

grantsTable :: String
grantsTable = "__system_grants"

-- | 带 grant option 的授权单独一张表
optionsTable :: String
optionsTable = "__system_grant_options"

membersTable :: String
membersTable = "__system_members"

internalNames :: [String]
internalNames = [rolesTable, grantsTable, optionsTable, membersTable]

-- | 名字是否带内部表前缀
reserved :: String -> Bool
reserved = T.isPrefixOf "__system_" . T.toLower . T.pack

-- | 是不是服务自己要经请求通道读写的表
privilegeTable :: String -> Bool
privilegeTable name = T.unpack (T.toLower (T.pack name)) `elem` internalNames

-- | 是不是服务自己的内部表
internalTable :: String -> Bool
internalTable name = reserved name || privilegeTable name

-- | 外部语句能不能碰这张表
blocked :: String -> Bool
blocked name = reserved name && not (privilegeTable name)

-- | 当前 UTC 时间，写法与引擎的 timestamp 一致
currentStamp :: IO T.Text
currentStamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime

-- | 在内存账号表上执行一条账号请求
memoryAccounts :: Database -> T.Text -> Request -> Either String [Account]
memoryAccounts db stamp req = do
    case req of
        ReqAccountCreate name _ | T.toLower name == "public" -> Left "public is reserved for default privileges"
        ReqRoleCreate name | T.toLower name == "public" -> Left "public is reserved for default privileges"
        _ -> Right ()
    stored <- case lookup "__system_identities" db of
        Just table -> mapM decodeCredential (tableRows table)
        Nothing -> case lookup usersTable db of
            Nothing -> Right []
            Just table -> mapM decodeAccount (tableRows table)
    let accounts = stored
        highWater = maximum (0 : map accountId accounts ++ [toInteger n | row <- maybe [] tableRows (lookup "__system_identity_sequence" db), Just (VInt n) <- [lookup "id" row]])
        legacyNames = [T.toLower (T.pack name) | row <- maybe [] tableRows (lookup rolesTable db), Just (VStr name) <- [lookup "name" row]]
    case req of
        ReqCatalogManage command -> do
            let target = case command of
                    ReqAccountCreate name _ -> Just name
                    ReqAccountReset name _ -> Just name
                    ReqAccountDrop name -> Just name
                    ReqIdentityAlter name _ Nothing _ Nothing Nothing -> Just name
                    _ -> Nothing
                allowed = case command of ReqAccountsList -> True; ReqAccountCreate{} -> True; ReqAccountReset{} -> True; ReqAccountDrop{} -> True; ReqIdentityAlter _ _ Nothing _ Nothing Nothing -> True; _ -> False
            if not allowed || any (\a -> target == Just (accountUser a) && (accountIsSuperuser a || accountSystemCatalogManager a || accountAllowSudoAuth a)) accounts
                then Left "superuser required to manage privileged identities" else memoryAccounts db stamp command
        ReqAccountsList -> Right accounts
        ReqIdentityInitialize admin
            | any (`elem` map accountUser accounts) legacyNames || length (nub legacyNames) /= length legacyNames -> Left "identity name collision"
            | otherwise -> Right ([if legacyIdentity a && accountUser a == T.toLower admin then a{accountIsSuperuser = True} else a | a <- accounts] ++
                [Account (highWater + offset) role "" 1 stamp Nothing False False True False False | (offset, role) <- zip [1..] legacyNames])
        ReqRoleCreate u
            | any ((== T.toLower u) . accountUser) accounts -> Left "identity already exists"
            | otherwise -> Right (accounts ++ [Account (highWater + 1) (T.toLower u) "" 1 stamp Nothing False False True False False])
        ReqIdentityAlter u login super enabled manager sudo -> case filter ((== T.toLower u) . accountUser) accounts of
            [_] -> preserveSuper [if accountUser a == T.toLower u then a{accountCanLogin = maybe (accountCanLogin a) id login,
                accountIsSuperuser = maybe (accountIsSuperuser a) id super, accountEnabled = maybe (accountEnabled a) id enabled,
                accountSystemCatalogManager = maybe (accountSystemCatalogManager a) id manager,
                accountAllowSudoAuth = maybe (accountAllowSudoAuth a) id sudo,
                accountRevision = accountRevision a + 1} else a | a <- accounts] accounts
            _ -> Left "unknown account"
        ReqAccountCreate u h
            | any ((== T.toLower u) . accountUser) accounts -> Left "account already exists"
            | otherwise -> Right (accounts ++ [Account (highWater + 1) (T.toLower u) h 1 stamp Nothing True False True False False])
        ReqAccountReset u h -> case filter ((== T.toLower u) . accountUser) accounts of
            [_] -> Right [if accountUser x == T.toLower u then x{accountHash = h, accountRevision = accountRevision x + 1} else x | x <- accounts]
            _ -> Left "unknown account"
        ReqAccountLogin u at -> case filter ((== T.toLower u) . accountUser) accounts of
            [a] | accountEnabled a && accountCanLogin a -> Right [if accountUser x == T.toLower u then x{accountLastLoginAt = at} else x | x <- accounts]
            _ -> Left "identity cannot login"
        ReqAccountDrop u -> case filter ((== T.toLower u) . accountUser) accounts of
            [] -> Left "unknown account"
            _ -> preserveSuper [x | x <- accounts, accountUser x /= T.toLower u] accounts
        _ -> Left "invalid account request"
  where
    -- | 判断旧格式身份是否缺少属性
    legacyIdentity account = any (legacyRow (accountUser account)) (maybe [] tableRows (lookup usersTable db))
    -- | 检查旧身份的元数据版本
    legacyRow user row = case lookup "account" row of
        Just (VStr encoded) -> case A.decode (BL.pack encoded) of
            Just (A.Object fields) -> KM.lookup "user" fields == Just (A.String user) && not (KM.member "is_superuser" fields)
            _ -> False
        _ -> False
    -- | 读取旧格式账号
    decodeAccount row = case lookup "account" row of
        Just (VStr encoded) -> A.eitherDecode (BL.pack encoded)
        _ -> Left "invalid account record"
    -- | 合并身份与口令
    decodeCredential row = do
        identity <- decodeAccount row
        case [hash | credential <- maybe [] tableRows (lookup usersTable db), lookup "id" credential == Just (VInt (fromInteger (accountId identity))), Just (VStr hash) <- [lookup "password_hash" credential]] of
            [hash] -> Right identity{accountHash = T.pack hash}
            _ -> Left "invalid identity credential"
    -- | 保留启用的登录管理员
    preserveSuper next old
        | any activeSuper old && not (any activeSuper next) = Left "the last enabled login superuser cannot be removed"
        | otherwise = Right next
    -- | 判断可登录管理员
    activeSuper a = accountEnabled a && accountCanLogin a && accountIsSuperuser a

-- | 分开保存内存身份与口令并清理授权
storeMemoryIdentities :: Database -> [Account] -> [Account] -> Database
storeMemoryIdentities db previous accounts =
    (usersTable, Table usersTable [("id", TInt), ("password_hash", TStr)] credentials Nothing) :
    ("__system_identities", Table "__system_identities" [("account", TStr)] identities Nothing) :
    ("__system_identity_sequence", Table "__system_identity_sequence" [("id", TInt)] [[("id", VInt highWater)]] Nothing) :
    [(name, clean name table) | (name, table) <- db, name `notElem` [usersTable, "__system_identities", "__system_identity_sequence"]]
  where
    -- | 保存口令与身份编号
    credentials = [[("id", VInt (fromInteger (accountId a))), ("password_hash", VStr (T.unpack (accountHash a)))] | a <- accounts]
    -- | 保存不含口令的身份记录
    identities = [[("account", VStr (BL.unpack (A.encode a{accountHash = ""})))] | a <- accounts]
    -- | 保留已用编号上界
    highWater = maximum (0 : map (fromInteger . accountId) accounts ++ [n | row <- maybe [] tableRows (lookup "__system_identity_sequence" db), Just (VInt n) <- [lookup "id" row]])
    -- | 清理旧目录与失效的授权边
    clean name table
        | name == rolesTable = table{tableRows = []}
        | name `elem` [grantsTable, optionsTable, membersTable] = table{tableRows = filter active (tableRows table)}
        | otherwise = table
    -- | 识别已经删除的身份
    removed = [accountUser a | a <- previous, accountUser a `notElem` map accountUser accounts]
    -- | 排除已删除身份的授权和成员边
    active row = not (any (\field -> case lookup field row of Just (VStr value) -> T.pack value `elem` removed; _ -> False) ["role", "member"])
