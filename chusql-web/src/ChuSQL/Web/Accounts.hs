{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.Accounts (
    Accounts, AccountError (..), Principal (..), PasswordPolicy (..), defaultPasswordPolicy,
    newAccounts, accountsPolicy, setAccountsPolicy, passwordAllowed,
    rootUserName, principalName, principalIsRoot,
    authenticate, currentPrincipal,
    listAccounts, findAccount, AccountCommand (..), accountCommand, runAccountCommand,
) where

import ChuSQL.Storage.IPC (Account (..), Request (..))
import ChuSQL.Syntax.AST (Statement (..))
import ChuSQL.Web.Auth
import ChuSQL.Web.Backend (Backend (..), currentStamp)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.Char (isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T

-- 账号服务：root 来自配置，普通账号由 root 用 CREATE/ALTER/DROP USER 管，口令在这里做策略检查与 PBKDF2 派生。

data PasswordPolicy = PasswordPolicy
    { ppMinLength :: Int
    , ppClasses :: Int
    }
    deriving (Show, Eq)

-- | 默认：至少 12 个字符、至少两类字符
defaultPasswordPolicy :: PasswordPolicy
defaultPasswordPolicy = PasswordPolicy 12 2

data AccountError = AccountError Text Text deriving (Show, Eq)

-- | 当前身份：配置里的管理员，或系统表里的一个普通账号
data Principal
    = Root Text
    | Ordinary Account
    deriving (Eq)

instance Show Principal where
    show (Root name) = "root " ++ show name
    show (Ordinary a) = "account " ++ show (accountUser a)

-- | 这个身份叫什么
principalName :: Principal -> Text
principalName (Root name) = name
principalName (Ordinary a) = accountUser a

-- | 是不是配置里的管理员
principalIsRoot :: Principal -> Bool
principalIsRoot (Root _) = True
principalIsRoot (Ordinary _) = False

data Accounts = Accounts
    { acBackend :: Backend
    , acSessions :: SessionStore
    , acLock :: MVar ()
    , acPolicy :: IORef PasswordPolicy
    , acRoot :: Credential
    }

-- | 只记下配置根账号；root 不写系统表，所以启动时不需要任何持久化动作
newAccounts :: Backend -> SessionStore -> Credential -> IO Accounts
newAccounts backend sessions credential = do
    lock <- newMVar ()
    policy <- newIORef defaultPasswordPolicy
    pure (Accounts backend sessions lock policy credential)

-- | 账号名统一小写去空白
normalize :: Text -> Text
normalize = T.toLower . T.strip

-- | 配置里的管理员叫什么
rootName :: Accounts -> Text
rootName = normalize . credUser . acRoot

-- | 管理员名字（界面显示用）
rootUserName :: Accounts -> Text
rootUserName = rootName

accountsPolicy :: Accounts -> IO PasswordPolicy
accountsPolicy = readIORef . acPolicy

setAccountsPolicy :: Accounts -> PasswordPolicy -> IO ()
setAccountsPolicy service = writeIORef (acPolicy service)

-- | 口令策略：长度 + 字符类别（历史与有效期随自助改密一起取消）
passwordAllowed :: PasswordPolicy -> Text -> Either AccountError ()
passwordAllowed policy password
    | T.length password < ppMinLength policy || T.length password > 256 = bad "password length does not meet policy"
    | classes < ppClasses policy = bad "password needs more character classes"
    | otherwise = Right ()
  where
    bad = Left . AccountError "bad_request"
    classes =
        length
            ( filter id
                [ T.any isAsciiLower password
                , T.any isAsciiUpper password
                , T.any isDigit password
                , T.any (not . isAlphaNum) password
                ]
            )

-- | 账号名规则：ASCII 字母数字与点、短横线、下划线
validateUserName :: Text -> Either AccountError ()
validateUserName name
    | T.null name || T.length name > 64 || not (T.all allowed name) =
        Left (AccountError "bad_request" "invalid account name")
    | otherwise = Right ()
  where
    allowed c = isAscii c && (isAlphaNum c || c `elem` ("_.-" :: String))

-- | 新建/改口令时的额外限制：管理员的名字保留给配置里的 root
validateNewName :: Accounts -> Text -> Either AccountError ()
validateNewName service name = do
    validateUserName name
    if normalize name == rootName service
        then Left (AccountError "bad_request" "the administrator name is reserved")
        else Right ()

storageFailure :: AccountError
storageFailure = AccountError "storage_error" "account storage unavailable"

unauthorized :: AccountError
unauthorized = AccountError "unauthorized" "invalid account or session"

-- | 存储侧报错翻成账号错误
storageResult :: Either String [Account] -> Either AccountError ()
storageResult (Right _) = Right ()
storageResult (Left "account already exists") = Left (AccountError "conflict" "account already exists")
storageResult (Left "unknown account") = Left (AccountError "not_found" "unknown account")
storageResult (Left _) = Left storageFailure

-- | 读系统表里的普通账号
readAccounts :: Accounts -> IO (Either AccountError [Account])
readAccounts service = fmap (either (const (Left storageFailure)) Right) (beAccounts (acBackend service) ReqAccountsList)

-- | 当前令牌对应的身份；会话版本与账号 revision 对不上就失效
currentUnlocked :: Accounts -> Text -> IO (Either AccountError Principal)
currentUnlocked service token = do
    session <- lookupVersionedSession (acSessions service) token
    case session of
        Nothing -> pure (Left unauthorized)
        Just (user, revision)
            | user == rootName service && revision == Nothing -> pure (Right (Root (rootName service)))
            | otherwise -> do
                stored <- readAccounts service
                pure $ do
                    accounts <- stored
                    account <- case filter ((== user) . accountUser) accounts of
                        [a] -> Right a
                        _ -> Left unauthorized
                    if revision == Just (accountRevision account)
                        then Right (Ordinary account)
                        else Left unauthorized

currentPrincipal :: Accounts -> Text -> IO (Either AccountError Principal)
currentPrincipal service token = withMVar (acLock service) $ \_ -> currentUnlocked service token

-- | 登录：先比配置里的 root，再查系统表里的普通账号（没有切换身份的入口）。
--   配置里的口令留空即"免密"：只有管理员能用空口令进来，普通账号一律拒绝。
authenticate :: Accounts -> Text -> Text -> IO (Either AccountError Text)
authenticate service user password = withMVar (acLock service) $ \_ ->
    if normalize user == rootName service
        then signInRoot
        else
            if passwordless
                then pure (Left (AccountError "forbidden" "this server only accepts the administrator"))
                else signInOrdinary
  where
    passwordless = T.null (credEncoded (acRoot service))
    signInRoot
        | passwordless =
            if T.null password
                then Right <$> createVersionedSession (acSessions service) (rootName service) Nothing
                else pure (Left unauthorized)
        | verifyPassword (credEncoded (acRoot service)) password =
            Right <$> createVersionedSession (acSessions service) (rootName service) Nothing
        | otherwise = pure (Left unauthorized)
    signInOrdinary = do
        stored <- readAccounts service
        case stored of
            Left err -> pure (Left err)
            Right accounts -> case filter ((== normalize user) . accountUser) accounts of
                [a] | verifyPassword (accountHash a) password -> do
                    stamp <- currentStamp
                    written <- beAccounts (acBackend service) (ReqAccountLogin (accountUser a) (Just stamp))
                    case written of
                        Left _ -> pure (Left storageFailure)
                        Right _ -> Right <$> createVersionedSession (acSessions service) (accountUser a) (Just (accountRevision a))
                _ -> pure (Left unauthorized)

-- | 系统表里的普通账号清单（root 专用）
listAccounts :: Accounts -> IO (Either AccountError [Account])
listAccounts service = withMVar (acLock service) $ \_ -> readAccounts service

-- | 按用户名找一个普通账号
findAccount :: Accounts -> Text -> IO (Either AccountError Account)
findAccount service name = withMVar (acLock service) $ \_ -> do
    stored <- readAccounts service
    pure $ do
        accounts <- stored
        case filter ((== normalize name) . accountUser) accounts of
            [a] -> Right a
            [] -> Left (AccountError "not_found" "unknown account")
            _ -> Left storageFailure

-- | 建一个普通账号
createUser :: Accounts -> Text -> Text -> IO (Either AccountError ())
createUser service name password = withMVar (acLock service) $ \_ -> do
    policy <- accountsPolicy service
    case validateNewName service name >> passwordAllowed policy password of
        Left err -> pure (Left err)
        Right () -> do
            encoded <- hashPassword password
            storageResult <$> beAccounts (acBackend service) (ReqAccountCreate (normalize name) encoded)

-- | 改一个普通账号的口令（管理员重置，不受历史/有效期限制）
resetPassword :: Accounts -> Text -> Text -> IO (Either AccountError ())
resetPassword service name password = withMVar (acLock service) $ \_ -> do
    policy <- accountsPolicy service
    case validateNewName service name >> passwordAllowed policy password of
        Left err -> pure (Left err)
        Right () -> do
            encoded <- hashPassword password
            storageResult <$> beAccounts (acBackend service) (ReqAccountReset (normalize name) encoded)

-- | 删一个普通账号
dropUser :: Accounts -> Text -> IO (Either AccountError ())
dropUser service name = withMVar (acLock service) $ \_ ->
    case validateUserName name of
        Left err -> pure (Left err)
        Right () -> storageResult <$> beAccounts (acBackend service) (ReqAccountDrop (normalize name))

-- | 账号管理命令：SQL 语句与一键接口在这上面汇合
data AccountCommand
    = CreateAccount Text Text
    | ResetAccountPassword Text Text
    | DropAccount Text
    deriving (Show, Eq)

-- | 一条语句是不是账号管理语句
accountCommand :: Statement -> Maybe AccountCommand
accountCommand (CreateUser name password) = Just (CreateAccount (T.pack name) (T.pack password))
accountCommand (AlterUser name password) = Just (ResetAccountPassword (T.pack name) (T.pack password))
accountCommand (DropUser name) = Just (DropAccount (T.pack name))
accountCommand _ = Nothing

-- | 执行一条账号管理命令；普通账号一律拒绝
runAccountCommand :: Accounts -> Principal -> AccountCommand -> IO (Either AccountError ())
runAccountCommand _ (Ordinary _) _ = pure (Left (AccountError "forbidden" "administrator required"))
runAccountCommand service (Root _) command = case command of
    CreateAccount name password -> createUser service name password
    ResetAccountPassword name password -> resetPassword service name password
    DropAccount name -> dropUser service name
