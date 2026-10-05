{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.Session
    ( QueryResult (..)
    , Session
    , SessionError (..)
    , engineErrorCode
    , newSession
    , newSessionWith
    , sessionUser
    , sessionIsAdmin
    , sessionDatabase
    , isPlainIdentifier
    , authenticateSession
    , authenticateSessionCoded
    , authenticateSudoSessionCoded
    , switchDatabase
    , runStatement
    , runStatementCoded
    , runStorageCoded
    , catalog
    , accounts
    , roleViews
    , databases
    , policyOf
    , reloadPolicy
    , queryResultJson
    ) where

import ChuSQL.Core.Model (Database, Row, Table (..), Value (..))
import ChuSQL.Core.Protocol (Account (..), Request (..), QueryResult (..), TxnOp (..), queryResultJson)
import ChuSQL.Core.Engine.Storage.IPC (TableInfo (..))
import qualified ChuSQL.Core.Engine as Engine
import qualified ChuSQL.Core.Engine.Error as E
import ChuSQL.Core.Engine.Syntax.AST (Statement (..))
import ChuSQL.Core.Engine.Syntax.Parser (parseStatement)
import ChuSQL.Interface.AccountTable (systemTableInfo)
import ChuSQL.Interface.Auth (defaultSessionPolicy, newSessionStore)
import ChuSQL.Server.Accounts
import ChuSQL.Server.Backend (Backend (..), StatementResult (..), columnsFromStatement, scopeDdl)
import ChuSQL.Server.Policy (configurePasswordPolicy)
import ChuSQL.Server.Privileges (PrivilegeCommand (..), PrivilegeError (..), Privileges, RoleView, affectedAccounts, affectedObject, authorize, authorizeConnect, claimObject, prepareObject, reconcileObjects, filterTables, listRoleViews, newPrivileges, privilegeCommand, runPrivilegeCommand)
import ChuSQL.Server.Security (readSecurity, writeSecurity)
import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Maybe (isJust)
import qualified Data.IntMap.Strict as IM
import qualified Data.IntSet as IS
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Time.Clock (getCurrentTime)

-- SQL 会话：Web、命令行与 TCP 服务器共用的账号、权限与语句执行入口。

-- | 带错误码的失败：码与 Web REST 的错误码同一个词汇表
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
    , ssTransaction :: IORef (Maybe Transaction)
    , ssChanged :: [Text] -> IO ()
    }

-- | 一条会话里的显式事务：开事务时的快照、事务内改到的状态与保存点栈
data Transaction = Transaction
    { txBase :: Database
    , txStaged :: Database
    , txSavepoints :: [(String, Database, [Statement])]
    , txStatements :: [Statement]
    , txObjects :: [(String, A.Value)]
    }

-- | 开一个会话：账号与角色服务、配置策略都和 Web 端一致
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
    pure (Session backend accountsService privileges current principal identityStamp settingsFile transaction changed)

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

-- 登录已通过本机凭据校验的映射账号。
authenticateSudoSessionCoded :: Session -> Text -> IO (Either SessionError ())
authenticateSudoSessionCoded session user = readSecurity $ do
    result <- authenticateSudo (ssAccounts session) user
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
            writeIORef (ssTransaction session) Nothing
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
    -- | 真的切库并记下当前库
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
runStatementCoded session sql = gate $ do
    valid <- validatePrincipal session
    case valid of Left err -> pure (Left err); Right () -> runStatementUnlocked session sql
  where
    gate = case parseStatement (T.unpack sql) of
        Right statement | transactionDdl statement || isJust (accountCommand statement) || isJust (privilegeCommand statement) -> writeSecurity
        _ -> readSecurity

-- 在权限门内检查并执行管理员存储请求。
runStorageCoded :: Session -> A.Value -> IO (Either SessionError A.Value)
runStorageCoded session payload = writeSecurity $ do
    valid <- validatePrincipal session
    who <- readIORef (ssPrincipal session)
    case valid of
        Left err -> pure (Left err)
        Right () -> case who of
            Nothing -> pure (Left (SessionError "unauthorized" "sign in first"))
            Just principal | not (principalIsRoot principal) -> pure (Left (SessionError "forbidden" "administrator required"))
            Just _ -> fmap (first (SessionError "storage_error" . T.pack)) (beStorage (ssBackend session) payload)

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
runStatementUnlocked :: Session -> Text -> IO (Either SessionError QueryResult)
runStatementUnlocked session sql = case parseStatement (T.unpack sql) of
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
    Right BeginTransaction -> beginTransaction session
    Right CommitTransaction -> commitTransaction session
    Right RollbackTransaction -> rollbackTransaction session
    Right (Savepoint name) -> savepointTransaction session (T.pack name)
    Right (RollbackToSavepoint name) -> rollbackToSavepoint session (T.pack name)
    Right (ReleaseSavepoint name) -> releaseSavepoint session (T.pack name)
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
                            | otherwise -> runObjectStatement session statement sql
    Left _ -> runPlain session sql

-- | 事务里不许改结构，会话层直接拦下
transactionDdl :: Statement -> Bool
transactionDdl statement = case statement of
    CreateTable{} -> True
    CreateDomain{} -> True
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
    Select{} -> True
    SelectExpr{} -> True
    Insert{} -> True
    Update{} -> True
    Delete{} -> True
    _ -> False

-- | 开事务：把当前库整库做一份快照当起点
beginTransaction :: Session -> IO (Either SessionError QueryResult)
beginTransaction session = do
    allowed <- sessionConnectAllowed session
    case allowed of Left err -> pure (Left err); Right () -> beginAuthorized session

-- 在连接权限复核后建立事务快照。
beginAuthorized :: Session -> IO (Either SessionError QueryResult)
beginAuthorized session = do
    current <- readIORef (ssCurrent session)
    existing <- readIORef (ssTransaction session)
    case existing of
        Just _ -> pure (Left (SessionError "query_error" "already in a transaction"))
        Nothing
            | T.null current -> pure (Left (SessionError "no_database" "no database selected"))
            | otherwise -> do
                backend <- currentBackend session
                snapshotResult <- beSnapshot backend
                case snapshotResult of
                    Left err -> pure (Left (engineErrorCode err))
                    Right db -> do
                        objects <- snapshotObjectIds session (map fst db)
                        case objects of
                            Left err -> pure (Left err)
                            Right ids -> do
                                writeIORef (ssTransaction session) (Just (Transaction db db [] [] ids))
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
                Right ids | ids /= txObjects transaction -> pure (Left (SessionError "serialization_failure" "transaction object identity changed"))
                          | otherwise -> commitAuthorized session current transaction

-- 对象和权限复核后提交事务差分。
commitAuthorized :: Session -> Text -> Transaction -> IO (Either SessionError QueryResult)
commitAuthorized session current transaction = do
            backend <- currentBackend session
            liveResult <- beSnapshot backend
            case liveResult of
                Left err -> pure (Left (engineErrorCode err))
                Right live -> let diffs = transactionDiffs (txBase transaction) (txStaged transaction) live in case transactionConflict diffs of
                    Just msg -> pure (Left (SessionError "serialization_failure" (T.pack msg)))
                    Nothing -> do
                        applied <- beApplyTransaction backend (transactionOps diffs)
                        case applied of
                            Left err -> pure (Left (engineErrorCode err))
                            Right () -> do
                                writeIORef (ssTransaction session) Nothing
                                pure (Right (emptyResult (Just current)))

-- 检查当前数据库连接权限。
sessionConnectAllowed :: Session -> IO (Either SessionError ())
sessionConnectAllowed session = do
    database <- readIORef (ssCurrent session)
    who <- readIORef (ssPrincipal session)
    case who of
        Just principal | not (principalIsRoot principal), not (T.null database) -> fmap (first privilegeErrorOf) (authorizeConnect (ssPrivileges session) principal database)
        _ -> pure (Right ())

-- 保存事务使用的 catalog 对象编号。
snapshotObjectIds :: Session -> [String] -> IO (Either SessionError [(String, A.Value)])
snapshotObjectIds session names = do
    database <- readIORef (ssCurrent session)
    if database == "system" then pure (Right []) else do
        results <- mapM (one database) (filter (not . T.isPrefixOf "__system_" . T.pack) names)
        pure (sequence results)
  where
    -- 解析一个快照表的稳定对象编号。
    one database name = do
        result <- beStorage (ssBackend session) (A.object ["method" A..= ("resolve_object" :: Text), "database" A..= database, "table" A..= name])
        pure $ case result of
            Left err -> Left (SessionError "serialization_failure" (T.pack err))
            Right value@(A.Object fields) | KM.lookup "status" fields == Just (A.String "object") -> Right (name, value)
            Right _ -> Left (SessionError "serialization_failure" "transaction object no longer exists")

-- | 回滚：快照丢掉，存储层没被改过
rollbackTransaction :: Session -> IO (Either SessionError QueryResult)
rollbackTransaction session = do
    current <- readIORef (ssCurrent session)
    existing <- readIORef (ssTransaction session)
    case existing of
        Nothing -> pure (Left (SessionError "query_error" "no transaction in progress"))
        Just _ -> do
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
            writeIORef (ssTransaction session) (Just transaction {txSavepoints = stack})
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
                writeIORef
                    (ssTransaction session)
                    (Just transaction {txStaged = staged, txStatements = statements, txSavepoints = take (index + 1) (txSavepoints transaction)})
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
                writeIORef (ssTransaction session) (Just transaction {txSavepoints = take index (txSavepoints transaction)})
                pure (Right (emptyResult (Just current)))

-- | 最近一次同名保存点的下标
savepointIndex :: Text -> [(String, Database, [Statement])] -> Maybe Int
savepointIndex name entries =
    case [index | (index, (label, _, _)) <- zip [0 ..] entries, T.pack label == name] of
        [] -> Nothing
        hits -> Just (last hits)

-- | 事务里跑一条数据语句：只动内存快照，不碰存储层
runInTransaction :: Session -> Statement -> IO (Either SessionError QueryResult)
runInTransaction session statement = do
    existing <- readIORef (ssTransaction session)
    current <- readIORef (ssCurrent session)
    case existing of
        Nothing -> pure (Left (SessionError "query_error" "no transaction in progress"))
        Just transaction -> case scopeDdl (T.unpack current) statement >>= Engine.runStatement (txStaged transaction) of
            Left err -> pure (Left (engineErrorCode err))
            Right (staged, rows) -> do
                writeIORef (ssTransaction session) (Just transaction {txStaged = staged, txStatements = statement : txStatements transaction})
                pure (Right (resultOfStaged (txStaged transaction) statement rows))

-- | 事务内的结果集：空结果回落到解析出来的列名
resultOfStaged :: Database -> Statement -> [Row] -> QueryResult
resultOfStaged db statement rows = resultOf (StatementResult cols rows)
  where
    cols = case rows of
        [] -> columnsFromStatement db statement
        (r : _) -> map fst r

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
    -- | 一张表的请求：先删后写
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
    -- | 事务里碰得着的表：数据语句不增删表
    names = nubKeys (map fst base ++ map fst staged)

    -- | 一张表的差异
    diffFor name
        | stagedRows == baseRows = emptyDiff
        | all (isJust . rowKey) (baseRows ++ stagedRows) = keyedDiff
        -- 动过，库里还是基线：整表替换
        | liveRows == baseRows = emptyDiff {tdReplace = Just stagedRows}
        | otherwise = emptyDiff {tdConflict = Just ("write conflict on table " ++ name)}
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

    -- | 没有差异的空结果
    emptyDiff = TableDiff [] [] Nothing Nothing

    -- | 表名去重但保持顺序
    nubKeys = foldr (\name acc -> if name `elem` acc then acc else name : acc) []

    -- | 库里某张表的行
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
    if not admin then pure (Left (SessionError "forbidden" "catalog manager required")) else do
        result <- beAccounts (ssBackend session) ReqAccountsList
        pure $ case result of
            Left err -> Left (SessionError "storage_error" (T.pack err))
            Right identities -> Right (QueryResult ["id", "name", "can_login", "is_superuser", "enabled", "system_catalog_manager", "allow_sudo_auth", "created_at"]
                [[VInt (fromInteger (accountId a)), VStr (T.unpack (accountUser a)), VBool (accountCanLogin a),
                    VBool (accountIsSuperuser a), VBool (accountEnabled a), VBool (accountSystemCatalogManager a), VBool (accountAllowSudoAuth a), VStr (T.unpack (accountRegisteredAt a))] | a <- identities]
                (length identities) False Nothing)

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
emptyResult database = QueryResult [] [] 0 False database

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
    -- | 按列名取单元格
    cellOf row name = maybe VNull id (lookup name row)

