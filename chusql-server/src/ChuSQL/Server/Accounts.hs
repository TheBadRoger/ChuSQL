{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.Accounts (
    Accounts, AccountError (..), Principal (..), PasswordPolicy (..), defaultPasswordPolicy,
    newAccounts, accountsPolicy, setAccountsPolicy, passwordAllowed,
    ensureRootAccount, administratorPasswordless, rootUserName, principalName, principalIsRoot,
    authenticate, currentPrincipal,
    listAccounts, findAccount, AccountCommand (..), accountCommand, runAccountCommand,
) where

import ChuSQL.Core.Engine.Error (accountErrorCode)
import ChuSQL.Core.Engine.Storage.IPC (Account (..), Request (..))
import ChuSQL.Core.Engine.Syntax.AST (Statement (..))
import ChuSQL.Interface.Auth
import ChuSQL.Interface.Policy (PasswordPolicy (..), defaultPasswordPolicy)
import ChuSQL.Server.Backend (Backend (..), currentStamp)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.Char (isAlphaNum, isAscii, isAsciiLower, isAsciiUpper, isDigit)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T

-- 账号服务：管理员是系统表里名字固定的那一行，口令哈希存在表里，普通账号由管理员用 CREATE/ALTER/DROP USER 管理。

data AccountError = AccountError Text Text deriving (Show, Eq)

-- | 当前身份：名字固定的管理员，或系统表里的一个普通账号
data Principal
    = Root Text
    | Ordinary Account
    deriving (Eq)

instance Show Principal where
    -- | 身份的可读形式
    show (Root name) = "root " ++ show name
    show (Ordinary a) = "account " ++ show (accountUser a)

-- | 这个身份叫什么
principalName :: Principal -> Text
principalName (Root name) = name
principalName (Ordinary a) = accountUser a

-- | 是不是名字固定的管理员
principalIsRoot :: Principal -> Bool
principalIsRoot (Root _) = True
principalIsRoot (Ordinary _) = False

data Accounts = Accounts
    { acBackend :: Backend
    , acSessions :: SessionStore
    , acLock :: MVar ()
    , acPolicy :: IORef PasswordPolicy
    , acRootName :: Text
    }

-- | 建账号服务，记下管理员名字
newAccounts :: Backend -> SessionStore -> Text -> IO Accounts
newAccounts backend sessions name = do
    lock <- newMVar ()
    policy <- newIORef defaultPasswordPolicy
    pure (Accounts backend sessions lock policy (normalize name))

-- | 账号名统一小写去空白
normalize :: Text -> Text
normalize = T.toLower . T.strip

-- | 管理员叫什么
rootName :: Accounts -> Text
rootName = acRootName

-- | 管理员名字（界面显示用）
rootUserName :: Accounts -> Text
rootUserName = rootName

-- | 读当前生效的口令策略
accountsPolicy :: Accounts -> IO PasswordPolicy
accountsPolicy = readIORef . acPolicy

-- | 换上一份新口令策略
setAccountsPolicy :: Accounts -> PasswordPolicy -> IO ()
setAccountsPolicy service = writeIORef (acPolicy service)

-- | 按策略检查口令长度与字符类别
passwordAllowed :: PasswordPolicy -> Text -> Either AccountError ()
passwordAllowed policy password
    | T.length password < ppMinLength policy || T.length password > 256 = bad "password length does not meet policy"
    | classes < ppClasses policy = bad "password needs more character classes"
    | otherwise = Right ()
  where
    -- | 构造一条请求错误
    bad = Left . AccountError "bad_request"
    -- | 命中的字符类别数
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
    -- | 该字符是否允许使用
    allowed c = isAscii c && (isAlphaNum c || c `elem` ("_.-" :: String))

-- | 校验新账号名，并挡掉管理员保留名
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

-- | 存储侧报错翻成账号错误，码统一由引擎错误表给出
storageResult :: Either String [Account] -> Either AccountError ()
storageResult (Right _) = Right ()
storageResult (Left message) = Left (AccountError code text)
  where
    -- | 认得的码原样用，其余存储细节不外泄
    code = T.pack (accountErrorCode message)
    text = if code == failureCode then failureText else T.pack message
    AccountError failureCode failureText = storageFailure

-- | 读系统表里的账号
readAccounts :: Backend -> IO (Either AccountError [Account])
readAccounts backend = fmap (either (const (Left storageFailure)) Right) (beAccounts backend ReqAccountsList)

-- | 表里没有管理员那一行就补一行空哈希，表示还没设口令
ensureRootAccount :: Backend -> Text -> IO (Either AccountError ())
ensureRootAccount backend name = do
    stored <- readAccounts backend
    case stored of
        Left err -> pure (Left err)
        Right accounts
            | any ((== normalize name) . accountUser) accounts -> pure (Right ())
            | otherwise -> do
                created <- beAccounts backend (ReqAccountCreate (normalize name) "")
                pure $ case created of
                    Right _ -> Right ()
                    Left "account already exists" -> Right ()
                    Left _ -> Left storageFailure

-- | 管理员是不是还没设口令
administratorPasswordless :: Backend -> Text -> IO Bool
administratorPasswordless backend name = do
    stored <- readAccounts backend
    pure $ case stored of
        Right accounts -> any (\a -> accountUser a == normalize name && T.null (accountHash a)) accounts
        Left _ -> False

-- | 按令牌解析当前身份，版本对不上即失效
currentUnlocked :: Accounts -> Text -> IO (Either AccountError Principal)
currentUnlocked service token = do
    session <- lookupVersionedSession (acSessions service) token
    case session of
        Nothing -> pure (Left unauthorized)
        Just (user, revision) -> do
            stored <- readAccounts (acBackend service)
            pure $ do
                accounts <- stored
                account <- case filter ((== user) . accountUser) accounts of
                    [a] -> Right a
                    _ -> Left unauthorized
                if revision == Just (accountRevision account)
                    then Right (if user == rootName service then Root user else Ordinary account)
                    else Left unauthorized

-- | 加锁解析当前身份
currentPrincipal :: Accounts -> Text -> IO (Either AccountError Principal)
currentPrincipal service token = withMVar (acLock service) $ \_ -> currentUnlocked service token

-- | 登录：比哈希，管理员未设口令时只收空口令
authenticate :: Accounts -> Text -> Text -> IO (Either AccountError Text)
authenticate service user password = withMVar (acLock service) $ \_ -> do
    stored <- readAccounts (acBackend service)
    case stored of
        Left err -> pure (Left err)
        Right accounts
            | name == rootName service -> signInRoot accounts
            | unset accounts -> pure (Left ordinaryRefused)
            | otherwise -> signInOrdinary accounts
  where
    name = normalize user
    ordinaryRefused = AccountError "forbidden" "this server only accepts the administrator"
    -- | 管理员那一行还没设口令（或压根不在表里）
    unset accounts = case filter ((== rootName service) . accountUser) accounts of
        [a] -> T.null (accountHash a)
        _ -> True
    -- | 管理员登录
    signInRoot accounts = case filter ((== rootName service) . accountUser) accounts of
        [a] | T.null (accountHash a) && T.null password -> issue a
        [a] | not (T.null (accountHash a)) && verifyPassword (accountHash a) password -> issue a
        _ -> pure (Left unauthorized)
    -- | 普通账号登录
    signInOrdinary accounts = case filter ((== name) . accountUser) accounts of
        [a] | not (T.null (accountHash a)) && verifyPassword (accountHash a) password -> issue a
        _ -> pure (Left unauthorized)
    -- | 记一次登录再发会话
    issue a = do
        stamp <- currentStamp
        written <- beAccounts (acBackend service) (ReqAccountLogin (accountUser a) (Just stamp))
        case written of
            Left _ -> pure (Left storageFailure)
            Right _ -> Right <$> createVersionedSession (acSessions service) (accountUser a) (Just (accountRevision a))

-- | 系统表里的账号清单（含管理员）
listAccounts :: Accounts -> IO (Either AccountError [Account])
listAccounts service = withMVar (acLock service) $ \_ -> readAccounts (acBackend service)

-- | 按用户名找一个账号
findAccount :: Accounts -> Text -> IO (Either AccountError Account)
findAccount service name = withMVar (acLock service) $ \_ -> do
    stored <- readAccounts (acBackend service)
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

-- | 设账号口令；管理员不受口令策略约束，空口令即免密
resetPassword :: Accounts -> Text -> Text -> IO (Either AccountError ())
resetPassword service name password = withMVar (acLock service) $ \_ -> do
    policy <- accountsPolicy service
    let admin = normalize name == rootName service
        checked = if admin then validateUserName name else validateNewName service name >> passwordAllowed policy password
    case checked of
        Left err -> pure (Left err)
        Right () -> do
            encoded <- if admin && T.null password then pure "" else hashPassword password
            storageResult <$> beAccounts (acBackend service) (ReqAccountReset (normalize name) encoded)

-- | 删一个普通账号
dropUser :: Accounts -> Text -> IO (Either AccountError ())
dropUser service name = withMVar (acLock service) $ \_ ->
    if normalize name == rootName service
        then pure (Left (AccountError "bad_request" "the administrator cannot be dropped"))
        else case validateUserName name of
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
