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
    rolesTable,
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
    _ -> []

-- | 走本地存储链路的后端
ipcBackend :: IO Backend
ipcBackend = do
    let backend current = Backend
            { beStatement = \sql -> runIpc current sql
            , beCatalog = do
                response <- sendRequest (ReqInDatabase current ReqListCatalog)
                pure $ case response of
                    RespCatalog infos -> Right infos
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
            , beSnapshot = Right <$> runIPCStorageIn (snapshot :: IPCStorage Database) (sessionEnv current)
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
    runIpc current sql = case parseStatement sql of
        Left e -> pure (Left e)
        Right stmt -> do
            let env = sessionEnv current
            result <- runIPCStorageIn (runStatementM stmt) env
            case result of
                Left e -> pure (Left e)
                Right rows -> do
                    cols <- case rows of
                        [] -> do
                            db <- runIPCStorageIn (schema :: IPCStorage Database) env
                            pure (columnsFromStatement db stmt)
                        (r : _) -> pure (map fst r)
                    pure (Right (StatementResult cols rows))

-- | 测试用内存后端，状态是一个 Database
memoryBackend :: String -> MVar Database -> Backend
memoryBackend = memoryBackendWith False

-- | 构造内存后端，Bool 决定普通库还是系统库视角
memoryBackendWith :: Bool -> String -> MVar Database -> Backend
memoryBackendWith trusted name ref =
    Backend
        { beStatement = \sql -> case parseStatement sql of
            Left e -> pure (Left e)
            -- 内存夹具也有"库"的概念：USE 只能切到它有的两个名字（自己 + system），
            -- 与 beDatabases 口径一致；引擎的内存存储不实现库管理，所以拦在这里。
            Right (UseDatabase target)
                | target == name || target == systemDatabaseName -> pure (Right (StatementResult [] []))
                | otherwise -> pure (Left ("unknown database: " ++ target))
            Right stmt -> modifyMVar ref $ \db -> case runStatement (scoped db) stmt of
                Left e -> pure (db, Left e)
                Right (db', rows)
                    | any (hidden . fst) db' -> pure (db, Left "reserved system table")
                    | otherwise -> pure (filter (hidden . fst) db ++ db', Right (StatementResult (colsOf (scoped db) stmt rows) rows))
        , beCatalog = do
            db <- readMVar ref
            pure (Right (map tableInfoOf (filter (not . internalTable . fst) db)))
        , bePing = pure True
        , beAccounts = \req -> do
            stamp <- currentStamp
            modifyMVar ref $ \db -> case memoryAccounts db stamp req of
                Left err -> pure (db, Left err)
                Right accounts -> pure ((usersTable, Table usersTable [("account", TStr)]
                    [[("account", VStr (BL.unpack (A.encode a))) ] | a <- accounts] Nothing) : filter ((/= usersTable) . fst) db, Right accounts)
        , beWithDatabase = withDatabase
        , beDatabases = pure (Right (nub [name, systemDatabaseName]))
        , beStorage = \_ -> pure (Left "storage is not available in this backend")
        , beSnapshot = Right . scoped <$> readMVar ref
        , beApplyTransaction = \ops -> modifyMVar ref $ \db -> case applyTransactionMemory ops db of
            Left err -> pure (db, Left err)
            Right db' -> pure (db', Right ())
        }
  where
    -- | 按视角过滤库里的表
    scoped = if trusted then filter (not . blocked . fst) else visible
    -- | 该视角下要隐藏的表
    hidden = if trusted then blocked else internalTable
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
            ++ concatMap exprTables (maybe [] (: []) (selectWhere stmt))
    SelectExpr{} ->
        tableRefsOf (selectFrom stmt)
            ++ concatMap exprTables (maybe [] (: []) (selectWhere stmt) ++ map snd (selectItems stmt))
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
statementNeedsDatabase stmt = any (not . T.any (== '.')) (statementTables stmt)

-- | FROM 子句里的所有表
tableRefsOf :: FromClause -> [Text]
tableRefsOf FromUnit = []
tableRefsOf (FromTable _ name) = [T.pack name]
tableRefsOf (FromJoin _ left _ name cond) = tableRefsOf left ++ [T.pack name] ++ exprTables cond

-- | 表达式里出现的表（子查询是唯一的来路）
exprTables :: Expr -> [Text]
exprTables expr = case expr of
    ScalarSub sub -> statementTables (subqueryStatement sub)
    ExistsSub sub _ -> statementTables (subqueryStatement sub)
    InSub value sub _ -> exprTables value ++ statementTables (subqueryStatement sub)
    InList value items _ -> concatMap exprTables (value : items)
    Add a b -> both a b
    Sub a b -> both a b
    Mul a b -> both a b
    Div a b -> both a b
    Gt a b -> both a b
    Lt a b -> both a b
    Eq a b -> both a b
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

-- | 一张表的线上信息（内存实现：没有索引，统计现算）
tableInfoOf :: (String, Table) -> TableInfo
tableInfoOf (name, tbl) =
    TableInfo
        { tiTable = name
        , tiColumns = [wireColumn c ty | (c, ty) <- tableCols tbl]
        , tiRows = length (tableRows tbl)
        , tiIndexes = []
        , tiStats = [(c, distinctOf c, False) | (c, _) <- tableCols tbl]
        }
  where
    -- | 一列的不同值个数
    distinctOf c = length (nub [v | r <- tableRows tbl, Just v <- [lookup c r]])

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

membersTable :: String
membersTable = "__system_members"

internalNames :: [String]
internalNames = [rolesTable, grantsTable, membersTable]

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

-- | 去掉内部表
visible :: Database -> Database
visible = filter (not . internalTable . fst)

-- | 当前 UTC 时间，写法与引擎的 timestamp 一致
currentStamp :: IO T.Text
currentStamp = T.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime

-- | 在内存账号表上执行一条账号请求
memoryAccounts :: Database -> T.Text -> Request -> Either String [Account]
memoryAccounts db stamp req = do
    accounts <- case lookup usersTable db of
        Nothing -> Right []
        Just table -> mapM decodeAccount (tableRows table)
    case req of
        ReqAccountsList -> Right accounts
        ReqAccountCreate u h
            | any ((== T.toLower u) . accountUser) accounts -> Left "account already exists"
            | otherwise -> Right (accounts ++ [Account (1 + maximum (0 : map accountId accounts)) (T.toLower u) h 1 stamp Nothing])
        ReqAccountReset u h -> case filter ((== T.toLower u) . accountUser) accounts of
            [_] -> Right [if accountUser x == T.toLower u then x{accountHash = h, accountRevision = accountRevision x + 1} else x | x <- accounts]
            _ -> Left "unknown account"
        ReqAccountLogin u at -> case filter ((== T.toLower u) . accountUser) accounts of
            [_] -> Right [if accountUser x == T.toLower u then x{accountLastLoginAt = at} else x | x <- accounts]
            _ -> Left "unknown account"
        ReqAccountDrop u -> case filter ((== T.toLower u) . accountUser) accounts of
            [] -> Left "unknown account"
            _ -> Right [x | x <- accounts, accountUser x /= T.toLower u]
        _ -> Left "invalid account request"
  where
    -- | 把一行解回账号
    decodeAccount row = case lookup "account" row of
        Just (VStr encoded) -> A.eitherDecode (BL.pack encoded)
        _ -> Left "invalid account record"
