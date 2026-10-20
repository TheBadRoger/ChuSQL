{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module ChuSQL.Server.Session (
    QueryResult (..),
    Session,
    SessionError (..),
    engineErrorCode,
    newSession,
    closeSession,
    newSessionWith,
    sessionUser,
    sessionIsAdmin,
    sessionDatabase,
    isPlainIdentifier,
    authenticateSession,
    authenticateSessionCoded,
    authenticateLocalSessionCoded,
    switchDatabase,
    runStatement,
    runStatementCoded,
    runStatementAdmitted,
    transactionTimeout,
    runStorageCoded,
    catalog,
    accounts,
    roleViews,
    databases,
    policyOf,
    reloadPolicy,
    queryResultJson,
) where

import qualified ChuSQL.Core.Engine as Engine
import qualified ChuSQL.Core.Engine.Error as E
import ChuSQL.Core.Engine.Runtime.Dependencies (domainDependencies, queryTables, validateDependencies)
import qualified ChuSQL.Core.Engine.Runtime.Query as Bound
import ChuSQL.Core.Engine.Storage.IPC (TableInfo (..))
import ChuSQL.Core.Engine.Storage.Memory (MemoryStorage (runMemoryStorage))
import ChuSQL.Core.Engine.Syntax.AST (Statement (..))
import ChuSQL.Core.Engine.Syntax.Parser (parseStatement)
import ChuSQL.Core.Model (Column (..), Database, Row, Table (..), TableIdentity (..), Value (..), lastTablePart)
import ChuSQL.Core.Protocol (
    Account (..),
    QueryResult (..),
    Request (..),
    TxnOp (..),
    queryResultJson,
 )
import ChuSQL.Server.AccountTable (systemTableInfo)
import ChuSQL.Server.Auth (defaultSessionPolicy, newSessionStore)
import ChuSQL.Server.Accounts
import ChuSQL.Server.Backend (Backend (..), StatementResult (..), applyTransactionMemory, columnsFromStatement, scopeDdl)
import ChuSQL.Server.Policy (configurePasswordPolicy)
import ChuSQL.Server.Privileges (Grant (..), PrivilegeCommand (..), PrivilegeError (..), Privileges, RoleView, affectedAccounts, affectedObject, authorize, authorizeConnect, authorizeTables, claimObject, filterTables, listRoleViews, listSelfGrants, newPrivileges, prepareObject, privilegeCommand, reconcileObjects, runPrivilegeCommand)
import ChuSQL.Server.Security (commitSecurity, databaseSecurity, readSecurity, writeSecurity)
import Control.Exception (IOException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Bifunctor (first)
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlpha, isAlphaNum)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Word (Word64)
import GHC.Clock (getMonotonicTimeNSec)

-- SQL 会话：TCP 服务器的账号、权限与语句执行入口。

-- | 向调用方返回稳定错误码和消息
data SessionError = SessionError
    { sessCode :: Text
    , sessMessage :: Text
    }
    deriving (Eq, Show)

-- | 引擎报错翻成错误码
engineErrorCode :: String -> SessionError
engineErrorCode err = SessionError (T.pack (E.errorCode err)) (T.pack err)

data Session = Session
    { ssBackend :: Backend
    , ssAccounts :: Accounts
    , ssPrivileges :: Privileges
    , ssCurrent :: IORef Text
    , ssPrincipal :: IORef (Maybe Principal)
    , ssIdentityStamp :: IORef (Maybe (Integer, Integer))
    , ssSettingsFile :: FilePath
    , ssQueryPlan :: IORef (Maybe (Statement, QuerySchema, Bound.BoundQuery))
    , ssTransaction :: IORef (Maybe Transaction)
    , ssChanged :: [Text] -> IO ()
    }

-- 查询缓存只比较结构身份，不保留数据或统计签名。
type QuerySchema = [(String, Maybe TableIdentity, Maybe Word64, [(String, Column)])]

-- | 一条会话里的显式事务：开事务时的快照、事务内改到的状态与保存点栈
data Transaction = Transaction
    { txBase :: Database
    , txStaged :: Database
    , txSavepoints :: [(String, Database, [Statement])]
    , txStatements :: [Statement]
    , txObjects :: [(String, TableIdentity)]
    , txToken :: Maybe String
    , txSerializable :: Bool
    , txLoaded :: [(String, Maybe [String])]
    , txStarted :: Word64
    , txActivity :: Word64
    }

-- | 使用账号、角色与配置策略创建会话
newSession :: Backend -> Text -> FilePath -> IO Session
newSession backend name settingsFile = newSessionWith backend name settingsFile (const (pure ()))

-- | 开会话并接上账号变更通知回调
newSessionWith :: Backend -> Text -> FilePath -> ([Text] -> IO ()) -> IO Session
newSessionWith backend name settingsFile changed = do
    sessions <- newSessionStore getCurrentTime defaultSessionPolicy
    accountsService <- newAccounts backend sessions name
    privileges <- newPrivileges backend
    configurePasswordPolicy accountsService settingsFile
    -- 服务启动后只有系统库：没 USE 之前不预设任何工作库
    current <- newIORef ""
    principal <- newIORef Nothing
    identityStamp <- newIORef Nothing
    transaction <- newIORef Nothing
    queryPlan <- newIORef Nothing
    pure (Session backend accountsService privileges current principal identityStamp settingsFile queryPlan transaction changed)

-- 关闭连接时归还存储事务、版本引用与行锁。
closeSession :: Session -> IO ()
closeSession session = do
    existing <- readIORef (ssTransaction session)
    writeIORef (ssTransaction session) Nothing
    case existing >>= txToken of
        Nothing -> pure ()
        Just token -> do
            backend <- currentBackend session
            result <- beRollbackTransaction backend token
            either (ioError . userError) pure result

-- | 当前生效的口令策略（只有管理员能看）
policyOf :: Session -> IO (Either Text PasswordPolicy)
policyOf session = do
    admin <- sessionIsAdmin session
    if not admin
        then pure (Left "administrator required")
        else Right <$> accountsPolicy (ssAccounts session)

-- | 重读设置文件里的口令策略
reloadPolicy :: Session -> IO (Either Text PasswordPolicy)
reloadPolicy session = do
    admin <- sessionIsAdmin session
    if not admin
        then pure (Left "administrator required")
        else do
            outcome <- try (configurePasswordPolicy (ssAccounts session) (ssSettingsFile session)) :: IO (Either IOException ())
            case outcome of
                Left err -> pure (Left (T.pack (show err)))
                Right () -> policyOf session

-- | 当前登录的账号名
sessionUser :: Session -> IO (Maybe Text)
sessionUser session = fmap principalName <$> readIORef (ssPrincipal session)

-- | 是不是管理员
sessionIsAdmin :: Session -> IO Bool
sessionIsAdmin session = maybe False principalIsRoot <$> readIORef (ssPrincipal session)

-- | 当前库
sessionDatabase :: Session -> IO Text
sessionDatabase = readIORef . ssCurrent

-- | 与解析器一致的裸标识符判断
isPlainIdentifier :: Text -> Bool
isPlainIdentifier name =
    not (T.null name)
        && T.length name <= 64
        && (isAlpha (T.head name) || T.head name == '_')
        && T.all (\ch -> isAlphaNum ch || ch == '_') name

-- | 登录：root 比配置凭据，普通账号查系统表
authenticateSession :: Session -> Text -> Text -> IO (Either Text ())
authenticateSession session user password =
    fmap (first sessMessage) (authenticateSessionCoded session user password)

-- | 登录（带错误码）
authenticateSessionCoded :: Session -> Text -> Text -> IO (Either SessionError ())
authenticateSessionCoded session user password = readSecurity $ do
    result <- authenticate (ssAccounts session) user password
    case result of
        Left err -> pure (Left (accountErrorOf err))
        Right token -> do
            who <- currentPrincipal (ssAccounts session) token
            case who of
                Left err -> pure (Left (accountErrorOf err))
                Right principal -> storePrincipal session principal

-- | 本机 root 免口令登录会话
authenticateLocalSessionCoded :: Session -> Text -> IO (Either SessionError ())
authenticateLocalSessionCoded session user = readSecurity $ do
    result <- authenticateLocalRoot (ssAccounts session) user
    case result of
        Left err -> pure (Left (accountErrorOf err))
        Right token -> do
            who <- currentPrincipal (ssAccounts session) token
            case who of
                Left err -> pure (Left (accountErrorOf err))
                Right principal -> storePrincipal session principal

-- 绑定身份版本并清理旧登录的事务。
storePrincipal :: Session -> Principal -> IO (Either SessionError ())
storePrincipal session principal = do
    result <- findAccount (ssAccounts session) (principalName principal)
    case result of
        Left err -> pure (Left (accountErrorOf err))
        Right account -> do
            writeIORef (ssPrincipal session) (Just principal)
            writeIORef (ssIdentityStamp session) (Just (accountId account, accountRevision account))
            closeSession session
            writeIORef (ssCurrent session) ""
            pure (Right ())

-- | 换库；系统库只有 root 能进
switchDatabase :: Session -> Text -> IO (Either Text ())
switchDatabase session name =
    readSecurity $ do
        valid <- validatePrincipal session
        case valid of Left err -> pure (Left (sessMessage err)); Right () -> fmap (first sessMessage) (switchDatabaseCoded session name)

-- | 换库（带错误码）
switchDatabaseCoded :: Session -> Text -> IO (Either SessionError ())
switchDatabaseCoded session rawName
    | not (isPlainIdentifier name) = pure (Left (SessionError "bad_request" ("not a plain database name: " <> name)))
    | name == "system" = do
        admin <- maybe False principalIsCatalogManager <$> readIORef (ssPrincipal session)
        if admin then go else pure (Left (SessionError "forbidden" "the system database is only available to the administrator"))
    | otherwise = go
  where
    name = T.toLower (T.strip rawName)
    -- \| 真的切库并记下当前库
    go = do
        who <- readIORef (ssPrincipal session)
        allowed <- case who of
            Just principal | name /= "system" -> authorizeConnect (ssPrivileges session) principal name
            _ -> pure (Right ())
        case allowed of
            Left err -> pure (Left (privilegeErrorOf err))
            Right () -> do
                backend <- currentBackend session
                result <- beStatement backend ("USE " ++ T.unpack name)
                case result of
                    Left err -> pure (Left (engineErrorCode err))
                    Right _ -> writeIORef (ssCurrent session) name >> pure (Right ())

-- | 跑一条语句：USE、账号与角色语句分流，其余交引擎
runStatement :: Session -> Text -> IO (Either Text QueryResult)
runStatement session sql = fmap (first sessMessage) (runStatementCoded session sql)

-- | 跑一条语句（带错误码）
runStatementCoded :: Session -> Text -> IO (Either SessionError QueryResult)
runStatementCoded = runStatementAdmitted id

-- | 先取得执行名额，事务控制命令保留独立通路
runStatementAdmitted :: (IO (Either SessionError QueryResult) -> IO (Either SessionError QueryResult)) -> Session -> Text -> IO (Either SessionError QueryResult)
runStatementAdmitted admit session sql = do
    current <- readIORef (ssCurrent session)
    let parsed = parseStatement (T.unpack sql)
        -- \| 只有获得名额后才进入权限和数据库门
        execute = gate current parsed $ do
            valid <- validatePrincipal session
            case valid of
                Left err -> pure (Left err)
                Right () -> do
                    touchTransaction session
                    result <- runStatementUnlocked session sql parsed
                    touchTransaction session
                    pure result
    case parsed of
        Right statement | transactionControl statement -> execute
        _ -> admit execute
  where
    -- 对象及权限变更独占，数据请求按来源库排序。
    gate current parsed = case parsed of
        Right CommitTransaction -> databaseSecurity [(T.unpack current, not (beConcurrentTransactions (ssBackend session)))]
        Right statement | transactionDdl statement || isJust (accountCommand statement) || isJust (privilegeCommand statement) -> writeSecurity
        Right statement -> databaseSecurity (accesses current statement)
        Left _ -> readSecurity
    -- 当前库写入独占，跨库查询依赖共享。
    accesses current statement =
        (T.unpack current, writing statement && not (beConcurrentTransactions (ssBackend session))) : [(source name, False) | name <- queryTables statement]
      where
        -- 无限定表使用当前工作库。
        source name = case break (== '.') name of
            (database, '.' : _) -> T.unpack (T.toLower (T.pack database))
            _ -> T.unpack current
    -- 只有数据写语句需要独占目标库。
    writing Insert{} = True
    writing Update{} = True
    writing Delete{} = True
    writing _ = False

-- | 事务结束及保存点命令不等待查询名额
transactionControl :: Statement -> Bool
transactionControl CommitTransaction = True
transactionControl RollbackTransaction = True
transactionControl Savepoint{} = True
transactionControl RollbackToSavepoint{} = True
transactionControl ReleaseSavepoint{} = True
transactionControl _ = False

-- | 记录已认证连接最近一次事务活动
touchTransaction :: Session -> IO ()
touchTransaction session = do
    transaction <- readIORef (ssTransaction session)
    case transaction of
        Nothing -> pure ()
        Just tx -> do
            now <- getMonotonicTimeNSec
            writeIORef (ssTransaction session) (Just tx{txActivity = now})

-- | 返回事务下一次到期的微秒数，零表示已经到期
transactionTimeout :: Session -> Int -> Int -> IO (Maybe Int)
transactionTimeout session idle lifetime = do
    transaction <- readIORef (ssTransaction session)
    now <- getMonotonicTimeNSec
    pure $ case transaction of
        Nothing -> Nothing
        Just tx -> case [remaining | (seconds, since) <- [(idle, txActivity tx), (lifetime, txStarted tx)], seconds > 0, let remaining = toInteger seconds * 1000000000 - (toInteger now - toInteger since)] of
            [] -> Nothing
            values -> Just (fromInteger (max 0 (min (toInteger (maxBound :: Int)) ((minimum values + 999) `div` 1000))))

-- 在权限门内检查并执行管理员存储请求。
runStorageCoded :: Session -> A.Value -> IO (Either SessionError A.Value)
runStorageCoded session payload = gate $ do
    valid <- validatePrincipal session
    who <- readIORef (ssPrincipal session)
    case valid of
        Left err -> pure (Left err)
        Right () -> case who of
            Nothing -> pure (Left (SessionError "unauthorized" "sign in first"))
            Just principal | not (principalIsRoot principal) -> pure (Left (SessionError "forbidden" "administrator required"))
            Just _ -> fmap (first (SessionError "storage_error" . T.pack)) (beStorage (ssBackend session) payload)
  where
    -- 在线整理期间允许普通数据语句进入。
    gate = case payload of
        A.Object object | KM.lookup "method" object == Just (A.String "compact") -> readSecurity
        _ -> writeSecurity

-- 检查登录身份的当前版本和启用状态。
validatePrincipal :: Session -> IO (Either SessionError ())
validatePrincipal session = do
    who <- readIORef (ssPrincipal session)
    stamp <- readIORef (ssIdentityStamp session)
    case who of
        Just principal -> do
            result <- findAccount (ssAccounts session) (principalName principal)
            pure $ case result of
                Left err -> Left (accountErrorOf err)
                Right current | stamp == Just (accountId current, accountRevision current) && accountEnabled current && accountCanLogin current && (not (principalIsRoot principal) || accountIsSuperuser current) -> Right ()
                _ -> Left (SessionError "unauthorized" "session identity has changed")
        _ -> pure (Right ())

-- 在共享权限门内分发会话语句。
runStatementUnlocked :: Session -> Text -> Either String Statement -> IO (Either SessionError QueryResult)
runStatementUnlocked session sql parsed = case parsed of
    Right (UseDatabase name) -> do
        existing <- readIORef (ssTransaction session)
        case existing of
            Just _ -> pure (Left (SessionError "query_error" "cannot switch database inside a transaction"))
            Nothing -> do
                switched <- switchDatabaseCoded session (T.pack name)
                case switched of
                    Left err -> pure (Left err)
                    Right () -> do
                        current <- readIORef (ssCurrent session)
                        pure (Right (emptyResult (Just current)))
    Right BeginTransaction -> beginTransaction session False
    Right BeginSerializableTransaction -> beginTransaction session True
    Right CommitTransaction -> commitTransaction session
    Right RollbackTransaction -> rollbackTransaction session
    Right (Savepoint name) -> savepointTransaction session (T.pack name)
    Right (RollbackToSavepoint name) -> rollbackToSavepoint session (T.pack name)
    Right (ReleaseSavepoint name) -> releaseSavepoint session (T.pack name)
    Right ShowGrants -> showSelfGrants session
    Right ShowRoles -> showIdentities session
    Right statement
        | Just command <- accountCommand statement -> runAccount session command
        | Just command <- privilegeCommand statement -> runPrivilege session command
        | otherwise -> do
            transaction <- readIORef (ssTransaction session)
            if isJust transaction && transactionDdl statement
                then pure (Left (SessionError "query_error" "DDL is not allowed in a transaction"))
                else do
                    allowed <- authorized session statement
                    case allowed of
                        Left err -> pure (Left err)
                        Right ()
                            | isJust transaction && transactionData statement -> runInTransaction session statement
                            | beConcurrentTransactions (ssBackend session) && transactionData statement -> runAutocommit session statement
                            | otherwise -> runObjectStatement session statement sql
    Left _ -> runPlain session sql

-- | 事务里不许改结构，会话层直接拦下
transactionDdl :: Statement -> Bool
transactionDdl statement = case statement of
    CreateTable{} -> True
    CreateDomain{} -> True
    AlterDomain{} -> True
    DropDomain{} -> True
    DropTable{} -> True
    CreateIndex{} -> True
    DropIndex{} -> True
    DropColumn{} -> True
    AddColumn{} -> True
    RenameColumn{} -> True
    AlterColumnType{} -> True
    AlterColumnDefault{} -> True
    AlterColumnNull{} -> True
    CreateDatabase{} -> True
    DropDatabase{} -> True
    _ -> False

-- | 事务里只有数据语句走快照
transactionData :: Statement -> Bool
transactionData statement = case statement of
    SelectDistinct q -> transactionData q
    Select{} -> True
    Insert{} -> True
    Update{} -> True
    Delete{} -> True
    _ -> False

-- | 开事务：把当前库整库做一份快照当起点
beginTransaction :: Session -> Bool -> IO (Either SessionError QueryResult)
beginTransaction session serializable = do
    allowed <- sessionConnectAllowed session
    case allowed of Left err -> pure (Left err); Right () -> beginAuthorized session serializable

-- 在连接权限复核后建立事务快照。
beginAuthorized :: Session -> Bool -> IO (Either SessionError QueryResult)
beginAuthorized session serializable = do
    current <- readIORef (ssCurrent session)
    existing <- readIORef (ssTransaction session)
    case existing of
        Just _ -> pure (Left (SessionError "query_error" "already in a transaction"))
        Nothing
            | T.null current -> pure (Left (SessionError "no_database" "no database selected"))
            | otherwise -> do
                backend <- currentBackend session
                snapshotResult <-
                    if beConcurrentTransactions backend
                        then beBeginTransaction backend serializable
                        else fmap (fmap (Nothing,)) (beSnapshot backend)
                case snapshotResult of
                    Left err -> pure (Left (engineErrorCode err))
                    Right (token, db) -> do
                        objects <- snapshotObjectIds session (map fst db)
                        case objects of
                            Left err -> do
                                released <- maybe (pure (Right ())) (beRollbackTransaction backend) token
                                pure (either (Left . engineErrorCode) (const (Left err)) released)
                            Right ids -> do
                                now <- getMonotonicTimeNSec
                                writeIORef (ssTransaction session) (Just (Transaction db db [] [] ids token serializable (if isJust token then [] else [(name, Nothing) | (name, _) <- db]) now now))
                                pure (Right (emptyResult (Just current)))

-- | 提交：把快照差分交给存储层应用，成功才结束事务
commitTransaction :: Session -> IO (Either SessionError QueryResult)
commitTransaction session = do
    current <- readIORef (ssCurrent session)
    existing <- readIORef (ssTransaction session)
    case existing of
        Nothing -> pure (Left (SessionError "query_error" "no transaction in progress"))
        Just transaction -> do
            connected <- sessionConnectAllowed session
            permissions <- mapM (authorized session) (txStatements transaction)
            identities <- snapshotObjectIds session (map fst (txBase transaction))
            case connected >> sequence permissions >> identities of
                Left err -> pure (Left err)
                Right ids
                    | ids /= txObjects transaction -> pure (Left (SessionError "serialization_failure" "transaction object identity changed"))
                    | otherwise -> commitAuthorized session current transaction

-- 对象和权限复核后提交事务差分。
commitAuthorized :: Session -> Text -> Transaction -> IO (Either SessionError QueryResult)
commitAuthorized session current transaction
    | Just token <- txToken transaction = commitSecurity (T.unpack current) (commitMvcc session current transaction token)
    | otherwise = commitLegacy session current transaction

-- 在短提交门内校验最新合并行集并发布版本。
commitMvcc :: Session -> Text -> Transaction -> String -> IO (Either SessionError QueryResult)
commitMvcc session current transaction token = do
    backend <- currentBackend session
    let modified = changedTables (txBase transaction) (txStaged transaction)
        names = modified ++ domainDependencies (txBase transaction) modified
    preview <- bePreviewTransaction backend token names
    case preview of
        Left err -> pure (Left (engineErrorCode err))
        Right (checkpoint, candidate) -> do
            let dependencies = [(name, table) | (name, table) <- txBase transaction, name `elem` names]
                checked = validateDependencies dependencies candidate >> mapM_ (validateFinal candidate) modified
            case checked of
                Left err -> pure (Left (SessionError "serialization_failure" (T.pack err)))
                Right () -> do
                    result <- beCommitTransaction backend token checkpoint
                    case result of
                        Left err -> pure (Left (engineErrorCode err))
                        Right () -> do
                            writeIORef (ssTransaction session) Nothing
                            pure (Right (emptyResult (Just current)))
  where
    -- 复用 SQL 最终行集约束，避免存储重复规则。
    validateFinal candidate name = maybe (Left "transaction table missing") (Engine.validateTableRows candidate) (lookup name candidate)

-- 内存后端保留原差分提交与冲突行为。
commitLegacy :: Session -> Text -> Transaction -> IO (Either SessionError QueryResult)
commitLegacy session current transaction = do
    backend <- currentBackend session
    liveResult <- beSnapshot backend
    case liveResult of
        Left err -> pure (Left (engineErrorCode err))
        Right live -> do
            let base = txBase transaction
                diffs = transactionDiffs base (txStaged transaction) live
                modified =
                    [ name
                    | (name, diff) <- diffs
                    , not (null (tdDeletes diff) && null (tdWrites diff)) || isJust (tdReplace diff)
                    ]
                readTables = if txSerializable transaction && not (null modified) then nub (map lastTablePart (concatMap queryTables (txStatements transaction))) else []
                names = modified ++ readTables ++ domainDependencies base (modified ++ readTables)
                dependencies = [(name, table) | (name, table) <- base, name `elem` names]
                ops = transactionOps diffs
                checked = do
                    validateDependencies dependencies live
                    maybe (Right ()) Left (transactionConflict diffs)
                    mapM_ (validateRead base live) readTables
                    candidate <- applyTransactionMemory ops live
                    mapM_ (validateFinal candidate) modified
            case checked of
                Left msg -> pure (Left (SessionError "serialization_failure" (T.pack msg)))
                Right () -> do
                    applied <- beApplyTransaction backend ops
                    case applied of
                        Left err -> pure (Left (engineErrorCode err))
                        Right () -> do
                            writeIORef (ssTransaction session) Nothing
                            pure (Right (emptyResult (Just current)))
  where
    -- 拒绝读过的表出现数据变化或幻行。
    validateRead base live name = case (lookup name base, lookup name live) of
        (Just before, Just after) | tableRows before == tableRows after -> Right ()
        _ -> Left ("serializable transaction read changed: " ++ name)
    -- 复用引擎规则检查合并后的最终行集。
    validateFinal candidate name = case lookup name candidate of
        Nothing -> Left ("transaction table missing: " ++ name)
        Just table -> Engine.validateTableRows candidate table

-- 检查当前数据库连接权限。
sessionConnectAllowed :: Session -> IO (Either SessionError ())
sessionConnectAllowed session = do
    database <- readIORef (ssCurrent session)
    who <- readIORef (ssPrincipal session)
    case who of
        Just principal | not (principalIsRoot principal), not (T.null database) -> fmap (first privilegeErrorOf) (authorizeConnect (ssPrivileges session) principal database)
        _ -> pure (Right ())

-- 保存事务使用的 catalog 对象编号。
snapshotObjectIds :: Session -> [String] -> IO (Either SessionError [(String, TableIdentity)])
snapshotObjectIds session names = do
    database <- readIORef (ssCurrent session)
    if database == "system"
        then pure (Right [])
        else do
            backend <- currentBackend session
            result <- beCatalog backend
            pure $ do
                infos <- first (SessionError "serialization_failure" . T.pack) result
                let identities = Map.fromList [(tiTable info, identity) | info <- infos, Just identity <- [tiIdentity info]]
                mapM (one identities) (filter (not . T.isPrefixOf "__system_" . T.pack) names)
  where
    -- \| 用同一次目录读取复查全部表的稳定编号
    one identities name = case Map.lookup name identities of
        Just identity -> Right (name, identity)
        Nothing -> Left (SessionError "serialization_failure" "transaction object no longer exists")

-- | 回滚：快照丢掉，存储层没被改过
rollbackTransaction :: Session -> IO (Either SessionError QueryResult)
rollbackTransaction session = do
    current <- readIORef (ssCurrent session)
    existing <- readIORef (ssTransaction session)
    case existing of
        Nothing -> pure (Left (SessionError "query_error" "no transaction in progress"))
        Just transaction -> do
            backend <- currentBackend session
            result <- maybe (pure (Right ())) (beRollbackTransaction backend) (txToken transaction)
            case result of
                Left err -> pure (Left (engineErrorCode err))
                Right () -> do
                    writeIORef (ssTransaction session) Nothing
                    pure (Right (emptyResult (Just current)))

-- | 建保存点：把事务当前状态压进保存点栈
savepointTransaction :: Session -> Text -> IO (Either SessionError QueryResult)
savepointTransaction session name = do
    current <- readIORef (ssCurrent session)
    existing <- readIORef (ssTransaction session)
    case existing of
        Nothing -> pure (Left (SessionError "query_error" "no transaction in progress"))
        Just transaction -> do
            let stack = txSavepoints transaction ++ [(T.unpack name, txStaged transaction, txStatements transaction)]
            writeIORef (ssTransaction session) (Just transaction{txSavepoints = stack})
            pure (Right (emptyResult (Just current)))

-- | 回滚到保存点：事务状态退回保存点，该保存点保留
rollbackToSavepoint :: Session -> Text -> IO (Either SessionError QueryResult)
rollbackToSavepoint session name = do
    current <- readIORef (ssCurrent session)
    existing <- readIORef (ssTransaction session)
    case existing of
        Nothing -> pure (Left (SessionError "query_error" "no transaction in progress"))
        Just transaction -> case savepointIndex name (txSavepoints transaction) of
            Nothing -> pure (Left (SessionError "query_error" ("no such savepoint: " <> name)))
            Just index -> do
                let (_, staged, statements) = txSavepoints transaction !! index
                let next = transaction{txStaged = staged, txStatements = statements, txSavepoints = take (index + 1) (txSavepoints transaction)}
                stagedResult <- stageMvcc session next
                case stagedResult of
                    Left err -> pure (Left err)
                    Right () -> do
                        writeIORef (ssTransaction session) (Just next)
                        pure (Right (emptyResult (Just current)))

-- | 释放保存点：该保存点与它之后的保存点一起丢掉
releaseSavepoint :: Session -> Text -> IO (Either SessionError QueryResult)
releaseSavepoint session name = do
    current <- readIORef (ssCurrent session)
    existing <- readIORef (ssTransaction session)
    case existing of
        Nothing -> pure (Left (SessionError "query_error" "no transaction in progress"))
        Just transaction -> case savepointIndex name (txSavepoints transaction) of
            Nothing -> pure (Left (SessionError "query_error" ("no such savepoint: " <> name)))
            Just index -> do
                writeIORef (ssTransaction session) (Just transaction{txSavepoints = take index (txSavepoints transaction)})
                pure (Right (emptyResult (Just current)))

-- | 最近一次同名保存点的下标
savepointIndex :: Text -> [(String, Database, [Statement])] -> Maybe Int
savepointIndex name entries =
    case [index | (index, (label, _, _)) <- zip [0 ..] entries, T.pack label == name] of
        [] -> Nothing
        hits -> Just (last hits)

-- | 执行事务数据语句并取得修改行锁
runInTransaction :: Session -> Statement -> IO (Either SessionError QueryResult)
runInTransaction session statement = do
    existing <- readIORef (ssTransaction session)
    current <- readIORef (ssCurrent session)
    case existing of
        Nothing -> pure (Left (SessionError "query_error" "no transaction in progress"))
        Just transaction -> do
            loaded <- loadTransaction session statement transaction
            case loaded of
                Left err -> pure (Left err)
                Right ready -> do
                    executed <- runStaged session (T.unpack current) (txStaged ready) statement
                    case executed of
                        Left err -> pure (Left (engineErrorCode err))
                        Right (staged, result) -> do
                            let next = ready{txStaged = staged, txStatements = statement : txStatements ready}
                            locked <- stageMvcc session next
                            case locked of
                                Left err -> pure (Left err)
                                Right () -> do
                                    writeIORef (ssTransaction session) (Just next)
                                    pure (Right (resultOf result))

-- 复用最近查询的绑定计划，结构变化时重新绑定。
runStaged :: Session -> String -> Database -> Statement -> IO (Either String (Database, StatementResult))
runStaged session current db statement = case scopeDdl current statement of
    Left err -> pure (Left err)
    Right scoped | queryStatement scoped -> do
        bound <- cachedQuery session db scoped
        case bound of
            Left err -> pure (Left err)
            Right plan -> do
                pure $ do
                    (result, staged) <- runMemoryStorage (Bound.executeQueryM plan) db
                    rows <- result
                    Right (staged, StatementResult (map fst (Bound.queryTypes plan)) rows)
    Right scoped -> pure $ do
        (staged, rows) <- Engine.runStatement db scoped
        Right (staged, StatementResult (case rows of [] -> columnsFromStatement db statement; row : _ -> map fst row) rows)

-- 只读查询可复用绑定，权限仍逐语句复核。
queryStatement :: Statement -> Bool
queryStatement Select{} = True
queryStatement SelectDistinct{} = True
queryStatement _ = False

-- 缓存一个无行数据的计划，以结构签名控制失效。
cachedQuery :: Session -> Database -> Statement -> IO (Either String Bound.BoundQuery)
cachedQuery session db statement = do
    previous <- readIORef (ssQueryPlan session)
    let signature = [(name, tableIdentity table, tableSchemaVersion table, tableCols table) | (name, table) <- db]
    case previous of
        Just (saved, schemaOnly, plan) | saved == statement && schemaOnly == signature -> pure (Right plan)
        _ -> case Bound.bindQuery db statement of
            Left err -> pure (Left err)
            Right plan -> do
                writeIORef (ssQueryPlan session) (Just (statement, signature, plan))
                pure (Right plan)

-- 按来源加载原始快照，并补齐保存点中的未加载表。
loadTransaction :: Session -> Statement -> Transaction -> IO (Either SessionError Transaction)
loadTransaction session statement transaction = case txToken transaction of
    Nothing -> pure (Right transaction)
    Just token -> do
        current <- readIORef (ssCurrent session)
        let base = txBase transaction
            sources = map lastTablePart (queryTables statement)
            wanted = sources ++ domainDependencies base sources
        requirements <- case scopeDdl (T.unpack current) statement of
            Right scoped | queryStatement scoped -> fmap Bound.queryReads <$> cachedQuery session base scoped
            Left err -> pure (Left err)
            _ -> pure (Right [(name, Nothing) | name <- wanted])
        case requirements of
            Left err -> pure (Left (engineErrorCode err))
            Right requirementsByTable -> do
                let selected = [(name, fromMaybe Nothing (lookup name requirementsByTable)) | (name, _) <- base, name `elem` wanted]
                    -- \| 列不足时升级为完整原始行，写入总是要求完整行
                    needed (name, columns) = case lookup name (txLoaded transaction) of
                        Nothing -> Just (name, columns)
                        Just Nothing -> Nothing
                        Just (Just loaded) | maybe False (all (`elem` loaded)) columns -> Nothing
                        _ -> Just (name, Nothing)
                    names = mapMaybe needed selected
                if null names
                    then pure (Right transaction)
                    else do
                        backend <- currentBackend session
                        result <- beReadTransaction backend token names
                        pure $ case result of
                            Left err -> Left (engineErrorCode err)
                            Right tables -> do
                                first engineErrorCode (validateDependencies [(name, table) | (name, table) <- base, name `elem` map fst names] tables)
                                -- \| 将原始行补入未加载或只加载部分列的位置
                                let merge db = [(name, fromMaybe table (lookup name tables)) | (name, table) <- db]
                                Right
                                    transaction
                                        { txBase = merge base
                                        , txStaged = merge (txStaged transaction)
                                        , txSavepoints = [(name, merge saved, statements) | (name, saved, statements) <- txSavepoints transaction]
                                        , txLoaded = names ++ filter ((`notElem` map fst names) . fst) (txLoaded transaction)
                                        }

-- 自动提交语句同样拥有快照和原子提交边界。
runAutocommit :: Session -> Statement -> IO (Either SessionError QueryResult)
runAutocommit session statement = do
    begun <- beginTransaction session False
    case begun of
        Left err -> pure (Left err)
        Right _ -> do
            executed <- runInTransaction session statement
            case executed of
                Left err -> do
                    rolled <- rollbackTransaction session
                    pure (const (Left err) =<< rolled)
                Right rows -> do
                    committed <- commitTransaction session
                    case committed of
                        Right _ -> pure (Right rows)
                        Left err -> do
                            rolled <- rollbackTransaction session
                            pure (const (Left err) =<< rolled)

-- 比较事务最终行集，保留原表顺序。
changedTables :: Database -> Database -> [String]
changedTables base staged = [name | (name, table) <- staged, fmap tableRows (lookup name base) /= Just (tableRows table)]

-- 获取本事务实际修改行的锁并更新读依赖。
stageMvcc :: Session -> Transaction -> IO (Either SessionError ())
stageMvcc session transaction = case txToken transaction of
    Nothing -> pure (Right ())
    Just token -> do
        backend <- currentBackend session
        let modified = changedTables (txBase transaction) (txStaged transaction)
            ops = [TxnReplace name (tableRows table) | (name, table) <- txStaged transaction, name `elem` modified]
            dependencies = nub (map lastTablePart (concatMap queryTables (txStatements transaction)))
        fmap (first engineErrorCode) (beStageTransaction backend token ops dependencies)

-- | 一张表的提交差异与冲突
data TableDiff = TableDiff
    { tdDeletes :: [Int]
    , tdWrites :: [Row]
    , tdReplace :: Maybe [Row]
    , tdConflict :: Maybe String
    }

-- | 提交差分：相对快照列出要删、要写与要替换的行
transactionOps :: [(String, TableDiff)] -> [TxnOp]
transactionOps = concatMap opsOf
  where
    -- \| 一张表的请求：先删后写
    opsOf (name, diff) =
        [TxnDelete name (tdDeletes diff) | not (null (tdDeletes diff))]
            ++ [TxnUpsert name (tdWrites diff) | not (null (tdWrites diff))]
            ++ [TxnReplace name rows | Just rows <- [tdReplace diff]]

-- | 提交前校验：本事务动过的行是否已被别人改掉
transactionConflict :: [(String, TableDiff)] -> Maybe String
transactionConflict diffs =
    case [msg | (_, diff) <- diffs, Just msg <- [tdConflict diff]] of
        (msg : _) -> Just msg
        [] -> Nothing

-- | 逐表算提交差异，顺带找出冲突
transactionDiffs :: Database -> Database -> Database -> [(String, TableDiff)]
transactionDiffs base staged live = [(name, diffFor name) | name <- names]
  where
    -- \| 事务里碰得着的表：数据语句不增删表
    names = nubKeys (map fst base ++ map fst staged)

    -- \| 一张表的差异
    diffFor name
        | stagedRows == baseRows = emptyDiff
        | all (isJust . rowKey) (baseRows ++ stagedRows) = keyedDiff
        -- 动过，库里还是基线：整表替换
        | liveRows == baseRows = emptyDiff{tdReplace = Just stagedRows}
        | otherwise = emptyDiff{tdConflict = Just ("write conflict on table " ++ name)}
      where
        baseRows = rowsOf name base
        stagedRows = rowsOf name staged
        liveRows = rowsOf name live

        baseByKey = keyedRows baseRows
        liveByKey = keyedRows liveRows
        stagedByKey = keyedRows stagedRows
        stagedEntries = [(k, r) | r <- stagedRows, Just k <- [rowKey r]]

        -- 本事务动过的键：内容改过、新增，或者删掉
        written = [k | (k, r) <- stagedEntries, IM.lookup k baseByKey /= Just r]
        writtenSet = IS.fromList written
        removed = [k | r <- baseRows, Just k <- [rowKey r], IM.notMember k stagedByKey]
        removedSet = IS.fromList removed
        touched = written ++ removed

        -- 两边都删了算一致；其余只要跟基线不一样就是冲突
        victims =
            [ k
            | k <- touched
            , IM.lookup k liveByKey /= IM.lookup k baseByKey
            , not (IS.member k removedSet && IM.notMember k liveByKey)
            ]

        keyedDiff =
            emptyDiff
                { tdDeletes = [k | k <- touched, IM.member k liveByKey]
                , tdWrites = [r | (k, r) <- stagedEntries, IS.member k writtenSet]
                , tdConflict = case victims of
                    (k : _) -> Just ("write conflict on table " ++ name ++ ", id " ++ show k)
                    [] -> Nothing
                }

    -- \| 没有差异的空结果
    emptyDiff = TableDiff [] [] Nothing Nothing

    -- \| 表名去重但保持顺序
    nubKeys = foldr (\name acc -> if name `elem` acc then acc else name : acc) []

    -- \| 库里某张表的行
    rowsOf name db = maybe [] tableRows (lookup name db)

-- | 按整数 id 建立保留首行的查找表
keyedRows :: [Row] -> IM.IntMap Row
keyedRows rows = IM.fromListWith (\_ earlier -> earlier) [(key, row) | row <- rows, Just key <- [rowKey row]]

-- | 行 id：只有整数 id 才认，跟存储层删行的口径一致
rowKey :: Row -> Maybe Int
rowKey row = case lookup "id" row of
    Just (VInt k) -> Just k
    _ -> Nothing

-- | 普通身份跑语句前的权限判定；管理员与未登录放行
authorized :: Session -> Statement -> IO (Either SessionError ())
authorized session statement = do
    who <- readIORef (ssPrincipal session)
    case who of
        Just principal | not (principalIsRoot principal) -> do
            database <- readIORef (ssCurrent session)
            result <- authorize (ssPrivileges session) principal database statement
            pure (either (Left . privilegeErrorOf) (const (Right ())) result)
        _ -> pure (Right ())

-- | 落到引擎/存储层的一条语句
runPlain :: Session -> Text -> IO (Either SessionError QueryResult)
runPlain session sql = do
    backend <- currentBackend session
    result <- beStatement backend (T.unpack sql)
    pure (either (Left . engineErrorCode) (Right . resultOf) result)

-- 执行对象结构变更并维护所有者和授权。
runObjectStatement :: Session -> Statement -> Text -> IO (Either SessionError QueryResult)
runObjectStatement session statement sql = do
    who <- readIORef (ssPrincipal session)
    database <- readIORef (ssCurrent session)
    prepared <- case (who, statement) of
        (Just principal, CreateTable name _) | database /= "system" -> prepareObject (ssPrivileges session) principal database (T.pack name)
        _ -> pure (Right ())
    affected <- case statement of
        DropTable name | database /= "system" -> affectedObject (ssPrivileges session) database (T.pack name)
        DropDatabase name | name /= "system" -> affectedObject (ssPrivileges session) "" ("database:" <> T.pack name)
        _ -> pure (Right [])
    case prepared >> affected of
        Left err -> pure (Left (privilegeErrorOf err))
        Right names -> do
            result <- runPreparedObject session statement sql
            notifyChanged session names
            pure result

-- 执行已登记创建声明的对象变更。
runPreparedObject :: Session -> Statement -> Text -> IO (Either SessionError QueryResult)
runPreparedObject session statement sql = do
    result <- runPlain session sql
    case result of
        Left err -> case statement of
            CreateTable{} -> do
                cleaned <- reconcileObjects (ssPrivileges session)
                pure $ case cleaned of
                    Left failure -> Left (SessionError "storage_error" (sessMessage err <> "; creator intent cleanup failed: " <> privilegeMessage failure))
                    Right () -> Left err
            _ -> pure (Left err)
        Right value -> do
            who <- readIORef (ssPrincipal session)
            database <- readIORef (ssCurrent session)
            maintained <- case (who, statement) of
                (Just principal, CreateTable name _) | database /= "system" -> claimObject (ssPrivileges session) principal database (T.pack name)
                (Just principal, CreateDatabase name) -> claimObject (ssPrivileges session) principal "" ("database:" <> T.pack name)
                (_, DropTable{}) -> reconcileObjects (ssPrivileges session)
                (_, DropDatabase{}) -> reconcileObjects (ssPrivileges session)
                _ -> pure (Right ())
            case maintained of
                Right () -> pure (Right value)
                Left err -> do
                    cleanup <- case statement of
                        CreateTable name _ -> runPlain session ("DROP TABLE " <> T.pack name)
                        CreateDatabase name -> runPlain session ("DROP DATABASE " <> T.pack name)
                        _ -> pure (Right value)
                    pure $ case cleanup of
                        Left failure -> Left (SessionError "storage_error" (privilegeMessage err <> "; object cleanup failed: " <> sessMessage failure))
                        Right _ -> Left (privilegeErrorOf err)

-- | 数据字典；普通身份只看到有 SELECT 权的表
catalog :: Session -> IO (Either Text [TableInfo])
catalog session = withReadSession session (catalogUnlocked session)

-- 在权限共享门内读取可见表目录。
catalogUnlocked :: Session -> IO (Either Text [TableInfo])
catalogUnlocked session = do
    current <- readIORef (ssCurrent session)
    if T.null current
        then pure (Left "no database selected")
        else do
            backend <- currentBackend session
            result <- beCatalog backend
            case result of
                Left e -> pure (Left (T.pack e))
                Right infos -> do
                    who <- readIORef (ssPrincipal session)
                    visible <- case who of
                        Just principal | not (principalIsRoot principal) -> do
                            allowed <- filterTables (ssPrivileges session) principal current (map (T.pack . tiTable) infos)
                            pure (either (Left . privilegeMessage) (\names -> Right (filter (\info -> T.pack (tiTable info) `elem` names) infos)) allowed)
                        _ -> pure (Right infos)
                    case visible of
                        Left message -> pure (Left message)
                        Right tables
                            | current == "system" -> Right <$> withAccountInfo session tables
                            | otherwise -> pure (Right tables)

-- | 系统库里补上账号表
withAccountInfo :: Session -> [TableInfo] -> IO [TableInfo]
withAccountInfo session tables = do
    admin <- sessionIsAdmin session
    if not admin
        then pure tables
        else do
            result <- listAccounts (ssAccounts session)
            pure $ case result of
                Left _ -> tables
                Right rows -> systemTableInfo{tiRows = length rows} : tables

-- | 库清单（\l 用）
databases :: Session -> IO (Either Text [Text])
databases session = withReadSession session $ do
    result <- beDatabases (ssBackend session)
    who <- readIORef (ssPrincipal session)
    case (result, who) of
        (Left err, _) -> pure (Left (T.pack err))
        (Right names, Just principal) | not (principalIsRoot principal) -> do
            checks <- mapM (check principal . T.pack) names
            pure $ case [err | Left err@(PrivilegeError code _) <- checks, code /= "forbidden"] of
                err : _ -> Left (privilegeMessage err)
                [] -> Right [T.pack name | (name, Right ()) <- zip names checks]
        (Right names, _) -> pure (Right (map T.pack names))
  where
    -- 检查业务库连接或系统目录管理权限。
    check principal "system" = pure (if principalIsCatalogManager principal then Right () else Left (PrivilegeError "forbidden" "system catalog manager required"))
    check principal name = authorizeConnect (ssPrivileges session) principal name

-- 在共享权限门内复核会话并读取接口。
withReadSession :: Session -> IO (Either Text a) -> IO (Either Text a)
withReadSession session action = readSecurity $ do
    valid <- validatePrincipal session
    case valid of Left err -> pure (Left (sessMessage err)); Right () -> action

-- | 角色总览，只给管理员
roleViews :: Session -> IO (Either Text [RoleView])
roleViews session = withReadSession session $ do
    admin <- sessionIsAdmin session
    if not admin
        then pure (Left "administrator required")
        else do
            result <- listRoleViews (ssPrivileges session)
            pure (either (Left . privilegeMessage) Right result)

-- | 账号清单（\du 与账号管理页用）；只给管理员
accounts :: Session -> IO (Either Text [Account])
accounts session = withReadSession session $ do
    admin <- sessionIsAdmin session
    if not admin
        then pure (Left "administrator required")
        else do
            result <- listAccounts (ssAccounts session)
            pure (either (Left . T.pack . show) Right result)

-- | 当前库对应的后端
currentBackend :: Session -> IO Backend
currentBackend session = do
    name <- readIORef (ssCurrent session)
    pure (beWithDatabase (ssBackend session) (T.unpack name))

-- | 展示身份属性并隐藏口令
showIdentities :: Session -> IO (Either SessionError QueryResult)
showIdentities session = do
    admin <- maybe False principalIsCatalogManager <$> readIORef (ssPrincipal session)
    if not admin
        then pure (Left (SessionError "forbidden" "catalog manager required"))
        else do
            result <- beAccounts (ssBackend session) ReqAccountsList
            pure $ case result of
                Left err -> Left (SessionError "storage_error" (T.pack err))
                Right identities ->
                    Right
                        ( QueryResult
                            ["id", "name", "can_login", "is_superuser", "enabled", "system_catalog_manager", "created_at"]
                            [ [ VInt (fromInteger (accountId a))
                              , VStr (T.unpack (accountUser a))
                              , VBool (accountCanLogin a)
                              , VBool (accountIsSuperuser a)
                              , VBool (accountEnabled a)
                              , VBool (accountSystemCatalogManager a)
                              , VStr (T.unpack (accountRegisteredAt a))
                              ]
                            | a <- identities
                            ]
                            (length identities)
                            False
                            Nothing
                        )

-- | 执行身份管理并通知受影响会话
runAccount :: Session -> AccountCommand -> IO (Either SessionError QueryResult)
runAccount session command = do
    who <- readIORef (ssPrincipal session)
    case who of
        Nothing -> pure (Left (SessionError "unauthorized" "sign in first"))
        Just principal -> do
            affected <- case command of
                AlterAccountAttributes name _ -> affectedAccounts (ssPrivileges session) (DropRoleCommand name)
                DropAccount name -> affectedAccounts (ssPrivileges session) (DropRoleCommand name)
                _ -> pure (Right (accountCommandUsers command))
            case affected of
                Left err -> pure (Left (privilegeErrorOf err))
                Right names -> do
                    result <- runAccountCommand (ssAccounts session) principal command
                    case result of
                        Left err -> pure (Left (accountErrorOf err))
                        Right () -> do
                            cleaned <- reconcileObjects (ssPrivileges session)
                            notifyChanged session names
                            pure (either (Left . privilegeErrorOf) (const (Right (emptyResult Nothing))) cleaned)

-- | 一条账号命令影响的账号
accountCommandUsers :: AccountCommand -> [Text]
accountCommandUsers command = case command of
    CreateAccount _ _ -> []
    ResetAccountPassword name _ -> [name]
    DropAccount name -> [name]
    AlterAccountAttributes name _ -> [name]

-- | 角色语句走权限服务，普通账号拒绝；改完通知受影响账号
runPrivilege :: Session -> PrivilegeCommand -> IO (Either SessionError QueryResult)
runPrivilege session command = do
    who <- readIORef (ssPrincipal session)
    case who of
        Nothing -> pure (Left (SessionError "unauthorized" "sign in first"))
        Just principal -> do
            database <- readIORef (ssCurrent session)
            -- 受影响账号要趁命令落地之前算：删角色之后成员就查不到了
            affected <- affectedAccounts (ssPrivileges session) command
            case affected of
                Left err -> pure (Left (privilegeErrorOf err))
                Right names -> do
                    result <- runPrivilegeCommand (ssPrivileges session) principal database command
                    case result of
                        Left err -> pure (Left (privilegeErrorOf err))
                        Right () -> do
                            notifyChanged session names
                            pure (Right (emptyResult Nothing))

-- | 通知变更：把受影响的账号交给会话的变更钩子
notifyChanged :: Session -> [Text] -> IO ()
notifyChanged session names = ssChanged session (filter (not . T.null) names)

-- | 账号错误转成会话错误
accountErrorOf :: AccountError -> SessionError
accountErrorOf (AccountError code message) = SessionError code message

-- | 权限错误转成会话错误
privilegeErrorOf :: PrivilegeError -> SessionError
privilegeErrorOf (PrivilegeError code message) = SessionError code message

-- | 取权限错误的文案
privilegeMessage :: PrivilegeError -> Text
privilegeMessage (PrivilegeError _ message) = message

-- | 只带库名的空结果
emptyResult :: Maybe Text -> QueryResult
emptyResult = QueryResult [] [] 0 False

-- | 存储结果转成查询结果
resultOf :: StatementResult -> QueryResult
resultOf result =
    QueryResult
        { qrColumns = map T.pack (srColumns result)
        , qrRows = [map (cellOf row) (srColumns result) | row <- srRows result]
        , qrRowCount = length (srRows result)
        , qrTruncated = False
        , qrDatabase = Nothing
        }
  where
    -- \| 按列名取单元格
    cellOf row name = fromMaybe VNull (lookup name row)

-- 展示当前登录身份自己的有效权限。
showSelfGrants :: Session -> IO (Either SessionError QueryResult)
showSelfGrants session = do
    who <- readIORef (ssPrincipal session)
    case who of
        Nothing -> pure (Left (SessionError "unauthorized" "sign in first"))
        Just principal -> do
            grants <- listSelfGrants (ssPrivileges session) principal
            pure $ case grants of
                Left err -> Left (privilegeErrorOf err)
                Right rows ->
                    Right
                        ( QueryResult
                            ["identity", "privilege", "object", "grant_option"]
                            [[VStr (T.unpack (grantRole grant)), VStr (T.unpack (grantPrivilege grant)), VStr (T.unpack (grantObject grant)), VBool (grantable grant)] | grant <- rows]
                            (length rows)
                            False
                            Nothing
                        )
