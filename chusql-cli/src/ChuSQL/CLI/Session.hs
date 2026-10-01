{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.CLI.Session
    ( QueryResult (..)
    , Session
    , newSession
    , sessionUser
    , sessionIsAdmin
    , sessionDatabase
    , isPlainIdentifier
    , authenticateSession
    , switchDatabase
    , runStatement
    , catalog
    , accountTable
    , roleViews
    , databases
    , queryResultJson
    ) where

import ChuSQL.Model (Value (..))
import ChuSQL.Storage.IPC (TableInfo (..))
import ChuSQL.Syntax.AST (Statement (..))
import ChuSQL.Syntax.Parser (parseStatement)
import ChuSQL.Web.Accounts
import ChuSQL.Web.API (configurePasswordPolicy, systemTableInfo)
import ChuSQL.Web.Auth (Credential, defaultSessionPolicy, newSessionStore)
import ChuSQL.Web.Backend (Backend (..), StatementResult (..))
import ChuSQL.Web.Privileges (PrivilegeCommand, PrivilegeError (..), Privileges, RoleView, authorize, filterTables, listRoleViews, newPrivileges, privilegeCommand, runPrivilegeCommand)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import Data.Char (isAlpha, isAlphaNum)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)

-- CLI 会话：直接连存储与引擎，但账号服务与 Web 管理端是同一套
-- （同一个 root 凭据，同一张 __chusql_users）。

data QueryResult = QueryResult
    { qrColumns :: [Text]
    , qrRows :: [[Value]]
    , qrRowCount :: Int
    , qrTruncated :: Bool
    , qrDatabase :: Maybe Text
    }
    deriving (Eq, Show)

data Session = Session
    { ssBackend :: Backend
    , ssAccounts :: Accounts
    , ssPrivileges :: Privileges
    , ssCurrent :: IORef Text
    , ssPrincipal :: IORef (Maybe Principal)
    }

-- | 开一个会话：账号与角色服务、配置策略都和 Web 端一致
newSession :: Backend -> Credential -> FilePath -> IO Session
newSession backend credential settingsFile = do
    sessions <- newSessionStore getCurrentTime defaultSessionPolicy
    accounts <- newAccounts backend sessions credential
    privileges <- newPrivileges backend
    configurePasswordPolicy accounts settingsFile
    -- 服务启动后只有系统库：没 USE 之前不预设任何工作库
    current <- newIORef ""
    principal <- newIORef Nothing
    pure (Session backend accounts privileges current principal)

-- | 当前登录的账号名
sessionUser :: Session -> IO (Maybe Text)
sessionUser session = fmap principalName <$> readIORef (ssPrincipal session)

-- | 是不是配置里的 root
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

-- | 登录：root 比配置里的凭据，普通账号查 __chusql_users
authenticateSession :: Session -> Text -> Text -> IO (Either Text ())
authenticateSession session user password = do
    result <- authenticate (ssAccounts session) user password
    case result of
        Left err -> pure (Left (accountMessage err))
        Right token -> do
            who <- currentPrincipal (ssAccounts session) token
            case who of
                Left err -> pure (Left (accountMessage err))
                Right principal -> do
                    writeIORef (ssPrincipal session) (Just principal)
                    pure (Right ())

-- | 换库；系统库只有 root 能进
switchDatabase :: Session -> Text -> IO (Either Text ())
switchDatabase session rawName
    | not (isPlainIdentifier name) = pure (Left ("not a plain database name: " <> name))
    | name == "system" = do
        admin <- sessionIsAdmin session
        if admin then go else pure (Left "the system database is only available to the administrator")
    | otherwise = go
  where
    name = T.toLower (T.strip rawName)
    go = do
        backend <- currentBackend session
        result <- beStatement backend ("USE " ++ T.unpack name)
        case result of
            Left err -> pure (Left (T.pack err))
            Right _ -> writeIORef (ssCurrent session) name >> pure (Right ())

-- | 跑一条语句：USE、账号语句与角色语句在这里分流，其余交给引擎
-- （普通身份先过一遍权限判定，管理员与未登录直接放行）
runStatement :: Session -> Text -> IO (Either Text QueryResult)
runStatement session sql = case parseStatement (T.unpack sql) of
    Right (UseDatabase name) -> do
        switched <- switchDatabase session (T.pack name)
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
authorized :: Session -> Statement -> IO (Either Text ())
authorized session statement = do
    who <- readIORef (ssPrincipal session)
    case who of
        Just principal | not (principalIsRoot principal) -> do
            database <- readIORef (ssCurrent session)
            result <- authorize (ssPrivileges session) principal database statement
            pure (either (Left . privilegeMessage) (const (Right ())) result)
        _ -> pure (Right ())

-- | 落到引擎/存储层的一条语句
runPlain :: Session -> Text -> IO (Either Text QueryResult)
runPlain session sql = do
    backend <- currentBackend session
    result <- beStatement backend (T.unpack sql)
    pure (either (Left . T.pack) (Right . resultOf) result)

-- | 数据字典（\dt 与 \d 用）；普通身份只看得到自己有 SELECT 的表
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
                    case who of
                        Just principal | not (principalIsRoot principal) -> do
                            allowed <- filterTables (ssPrivileges session) principal current (map (T.pack . tiTable) infos)
                            pure (either (Left . privilegeMessage) (\names -> Right (filter (\info -> T.pack (tiTable info) `elem` names) infos)) allowed)
                        _ -> pure (Right infos)

-- | 库清单（\l 用）
databases :: Session -> IO (Either Text [Text])
databases session = do
    result <- beDatabases (ssBackend session)
    pure (either (Left . T.pack) (Right . map T.pack) result)

-- | 系统库里的账号表：列定义与 Web 端共用一份，行数从同一套账号服务来
accountTable :: Session -> IO (Maybe TableInfo)
accountTable session = do
    admin <- sessionIsAdmin session
    if not admin
        then pure Nothing
        else do
            result <- listAccounts (ssAccounts session)
            pure (either (const Nothing) (\accounts -> Just systemTableInfo{tiRows = length accounts}) result)

-- | 角色总览（\dr 用）；与 Web 的 /api/roles 一样只给管理员
roleViews :: Session -> IO (Either Text [RoleView])
roleViews session = do
    admin <- sessionIsAdmin session
    if not admin
        then pure (Left "administrator required")
        else do
            result <- listRoleViews (ssPrivileges session)
            pure (either (Left . privilegeMessage) Right result)

-- | 当前库对应的后端
currentBackend :: Session -> IO Backend
currentBackend session = do
    name <- readIORef (ssCurrent session)
    pure (beWithDatabase (ssBackend session) (T.unpack name))

-- | 账号语句走账号服务，普通账号一律拒绝
runAccount :: Session -> AccountCommand -> IO (Either Text QueryResult)
runAccount session command = do
    who <- readIORef (ssPrincipal session)
    case who of
        Nothing -> pure (Left "sign in first")
        Just principal -> do
            result <- runAccountCommand (ssAccounts session) principal command
            pure (either (Left . accountMessage) (const (Right (emptyResult Nothing))) result)

-- | 角色语句走权限服务，普通账号一律拒绝；当前库决定授权对象的落库口径
runPrivilege :: Session -> PrivilegeCommand -> IO (Either Text QueryResult)
runPrivilege session command = do
    who <- readIORef (ssPrincipal session)
    case who of
        Nothing -> pure (Left "sign in first")
        Just principal -> do
            database <- readIORef (ssCurrent session)
            result <- runPrivilegeCommand (ssPrivileges session) principal database command
            pure (either (Left . privilegeMessage) (const (Right (emptyResult Nothing))) result)

accountMessage :: AccountError -> Text
accountMessage (AccountError _ message) = message

privilegeMessage :: PrivilegeError -> Text
privilegeMessage (PrivilegeError _ message) = message

emptyResult :: Maybe Text -> QueryResult
emptyResult database = QueryResult [] [] 0 False database

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
    cellOf row name = maybe VNull id (lookup name row)

-- | 结果还原成 JSON（JSON 输出用）
queryResultJson :: QueryResult -> A.Value
queryResultJson result =
    object
        [ "columns" .= qrColumns result
        , "rows" .= map (map valueToJson) (qrRows result)
        , "rowCount" .= qrRowCount result
        , "truncated" .= qrTruncated result
        , "database" .= qrDatabase result
        ]

valueToJson :: Value -> A.Value
valueToJson VNull = A.Null
valueToJson (VInt n) = A.Number (fromIntegral n)
valueToJson (VFloat d) = A.Number (fromFloatDigits d)
valueToJson (VStr s) = A.String (T.pack s)
valueToJson (VBool b) = A.Bool b
