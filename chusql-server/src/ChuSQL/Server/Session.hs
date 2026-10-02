{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.Session
    ( QueryResult (..)
    , Session
    , SessionError (..)
    , engineErrorCode
    , newSession
    , sessionUser
    , sessionIsAdmin
    , sessionDatabase
    , isPlainIdentifier
    , authenticateSession
    , authenticateSessionCoded
    , switchDatabase
    , runStatement
    , runStatementCoded
    , catalog
    , accounts
    , roleViews
    , databases
    , policyOf
    , reloadPolicy
    , queryResultJson
    ) where

import ChuSQL.Core.Model (Value (..))
import ChuSQL.Core.Protocol (Account, QueryResult (..), queryResultJson)
import ChuSQL.Core.Engine.Storage.IPC (TableInfo (..))
import qualified ChuSQL.Core.Engine.Error as E
import ChuSQL.Core.Engine.Syntax.AST (Statement (..))
import ChuSQL.Core.Engine.Syntax.Parser (parseStatement)
import ChuSQL.Interface.AccountTable (systemTableInfo)
import ChuSQL.Interface.Auth (defaultSessionPolicy, newSessionStore)
import ChuSQL.Server.Accounts
import ChuSQL.Server.Backend (Backend (..), StatementResult (..))
import ChuSQL.Server.Policy (configurePasswordPolicy)
import ChuSQL.Server.Privileges (PrivilegeCommand, PrivilegeError (..), Privileges, RoleView, authorize, filterTables, listRoleViews, newPrivileges, privilegeCommand, runPrivilegeCommand)
import Control.Exception (IOException, try)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
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
    , ssSettingsFile :: FilePath
    }

-- | 开一个会话：账号与角色服务、配置策略都和 Web 端一致
newSession :: Backend -> Text -> FilePath -> IO Session
newSession backend name settingsFile = do
    sessions <- newSessionStore getCurrentTime defaultSessionPolicy
    accountsService <- newAccounts backend sessions name
    privileges <- newPrivileges backend
    configurePasswordPolicy accountsService settingsFile
    -- 服务启动后只有系统库：没 USE 之前不预设任何工作库
    current <- newIORef ""
    principal <- newIORef Nothing
    pure (Session backend accountsService privileges current principal settingsFile)

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
authenticateSessionCoded session user password = do
    result <- authenticate (ssAccounts session) user password
    case result of
        Left err -> pure (Left (accountErrorOf err))
        Right token -> do
            who <- currentPrincipal (ssAccounts session) token
            case who of
                Left err -> pure (Left (accountErrorOf err))
                Right principal -> do
                    writeIORef (ssPrincipal session) (Just principal)
                    pure (Right ())

-- | 换库；系统库只有 root 能进
switchDatabase :: Session -> Text -> IO (Either Text ())
switchDatabase session name =
    fmap (first sessMessage) (switchDatabaseCoded session name)

-- | 换库（带错误码）
switchDatabaseCoded :: Session -> Text -> IO (Either SessionError ())
switchDatabaseCoded session rawName
    | not (isPlainIdentifier name) = pure (Left (SessionError "bad_request" ("not a plain database name: " <> name)))
    | name == "system" = do
        admin <- sessionIsAdmin session
        if admin then go else pure (Left (SessionError "forbidden" "the system database is only available to the administrator"))
    | otherwise = go
  where
    name = T.toLower (T.strip rawName)
    -- | 真的切库并记下当前库
    go = do
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
runStatementCoded session sql = case parseStatement (T.unpack sql) of
    Right (UseDatabase name) -> do
        switched <- switchDatabaseCoded session (T.pack name)
        case switched of
            Left err -> pure (Left err)
            Right () -> do
                current <- readIORef (ssCurrent session)
                pure (Right (emptyResult (Just current)))
    Right statement
        | Just command <- accountCommand statement -> runAccount session command
        | Just command <- privilegeCommand statement -> runPrivilege session command
        | otherwise -> do
            allowed <- authorized session statement
            case allowed of
                Left err -> pure (Left err)
                Right () -> runPlain session sql
    Left _ -> runPlain session sql

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

-- | 数据字典；普通身份只看到有 SELECT 权的表
catalog :: Session -> IO (Either Text [TableInfo])
catalog session = do
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
databases session = do
    result <- beDatabases (ssBackend session)
    pure (either (Left . T.pack) (Right . map T.pack) result)

-- | 角色总览，只给管理员
roleViews :: Session -> IO (Either Text [RoleView])
roleViews session = do
    admin <- sessionIsAdmin session
    if not admin
        then pure (Left "administrator required")
        else do
            result <- listRoleViews (ssPrivileges session)
            pure (either (Left . privilegeMessage) Right result)

-- | 账号清单（\du 与账号管理页用）；只给管理员
accounts :: Session -> IO (Either Text [Account])
accounts session = do
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

-- | 账号语句走账号服务，普通账号一律拒绝
runAccount :: Session -> AccountCommand -> IO (Either SessionError QueryResult)
runAccount session command = do
    who <- readIORef (ssPrincipal session)
    case who of
        Nothing -> pure (Left (SessionError "unauthorized" "sign in first"))
        Just principal -> do
            result <- runAccountCommand (ssAccounts session) principal command
            pure (either (Left . accountErrorOf) (const (Right (emptyResult Nothing))) result)

-- | 角色语句走权限服务，普通账号拒绝
runPrivilege :: Session -> PrivilegeCommand -> IO (Either SessionError QueryResult)
runPrivilege session command = do
    who <- readIORef (ssPrincipal session)
    case who of
        Nothing -> pure (Left (SessionError "unauthorized" "sign in first"))
        Just principal -> do
            database <- readIORef (ssCurrent session)
            result <- runPrivilegeCommand (ssPrivileges session) principal database command
            pure (either (Left . privilegeErrorOf) (const (Right (emptyResult Nothing))) result)

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

