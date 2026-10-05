{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.Accounts (
    Accounts, AccountError (..), Principal (..), PasswordPolicy (..), defaultPasswordPolicy,
    newAccounts, accountsPolicy, setAccountsPolicy, passwordAllowed,
    ensureRootAccount, administratorPasswordless, rootUserName, principalName, principalIsRoot, principalIsCatalogManager,
    authenticate, authenticateSudo, currentPrincipal,
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
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T

-- 身份服务：按登录、最高权限和启用属性认证，管理身份与口令。

data AccountError = AccountError Text Text deriving (Show, Eq)

-- | 当前认证身份与最高权限标志
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

-- | 判断身份是否具备最高权限
principalIsRoot :: Principal -> Bool
principalIsRoot (Root _) = True
principalIsRoot (Ordinary a) = accountIsSuperuser a && accountEnabled a && accountCanLogin a

-- | 判断身份是否可管理系统目录
principalIsCatalogManager :: Principal -> Bool
principalIsCatalogManager principal = principalIsRoot principal || case principal of
    Ordinary a -> accountSystemCatalogManager a && accountEnabled a && accountCanLogin a
    Root _ -> True

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

-- | 校验新身份的名称
validateNewName :: Accounts -> Text -> Either AccountError ()
validateNewName _ name = validateUserName name

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
    initialized <- beAccounts backend (ReqIdentityInitialize (normalize name))
    let stored = either (const (Left storageFailure)) Right initialized
    case stored of
        Left err -> pure (Left err)
        Right accounts
            | any ((== normalize name) . accountUser) accounts -> pure (Right ())
            | otherwise -> do
                created <- beAccounts backend (ReqAccountCreate (normalize name) "")
                case created of
                    Right _ -> storageResult <$> beAccounts backend (ReqIdentityAlter (normalize name) Nothing (Just True) Nothing Nothing Nothing)
                    Left "account already exists" -> pure (Right ())
                    Left _ -> pure (Left storageFailure)

-- | 管理员是不是还没设口令
administratorPasswordless :: Backend -> Text -> IO Bool
administratorPasswordless backend name = do
    stored <- readAccounts backend
    pure $ case stored of
        Right accounts -> any (\a -> accountUser a == normalize name && accountIsSuperuser a && accountEnabled a && accountCanLogin a && T.null (accountHash a)) accounts
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
                if revision == Just (accountRevision account) && accountEnabled account && accountCanLogin account
                    then Right (if accountIsSuperuser account then Root user else Ordinary account)
                    else Left unauthorized

-- | 加锁解析当前身份
currentPrincipal :: Accounts -> Text -> IO (Either AccountError Principal)
currentPrincipal service token = withMVar (acLock service) $ \_ -> currentUnlocked service token

-- | 登录：比哈希，管理员未设口令时只收空口令
authenticate :: Accounts -> Text -> Text -> IO (Either AccountError Text)
authenticate service user password = withMVar (acLock service) $ \_ -> do
    initialized <- beAccounts (acBackend service) (ReqIdentityInitialize (rootName service))
    let stored = either (const (Left storageFailure)) Right initialized
    case stored of
        Left err -> pure (Left err)
        Right accounts
            | any (\a -> accountUser a == name && accountIsSuperuser a && accountEnabled a && accountCanLogin a) accounts -> signInRoot accounts
            | unset accounts -> pure (Left ordinaryRefused)
            | otherwise -> signInOrdinary accounts
  where
    name = normalize user
    ordinaryRefused = AccountError "forbidden" "this server only accepts the administrator"
    -- | 判断是否有已设置口令的登录管理员
    unset accounts = not (any (\a -> accountIsSuperuser a && accountEnabled a && accountCanLogin a && not (T.null (accountHash a))) accounts)
    -- | 管理员登录
    signInRoot accounts = case filter ((== name) . accountUser) accounts of
        [a] | T.null (accountHash a) && T.null password -> issue a
        [a] | accountCanLogin a && accountEnabled a && not (T.null (accountHash a)) && verifyPassword (accountHash a) password -> issue a
        _ -> pure (Left unauthorized)
    -- | 普通账号登录
    signInOrdinary accounts = case filter ((== name) . accountUser) accounts of
        [a] | accountCanLogin a && accountEnabled a && not (T.null (accountHash a)) && verifyPassword (accountHash a) password -> issue a
        _ -> pure (Left unauthorized)
    -- | 记一次登录再发会话
    issue a = do
        stamp <- currentStamp
        written <- beAccounts (acBackend service) (ReqAccountLogin (accountUser a) (Just stamp))
        case written of
            Left _ -> pure (Left storageFailure)
            Right _ -> Right <$> createVersionedSession (acSessions service) (accountUser a) (Just (accountRevision a))

-- 为已验证的本机身份创建独立登录会话。
authenticateSudo :: Accounts -> Text -> IO (Either AccountError Text)
authenticateSudo service user = withMVar (acLock service) $ \_ -> do
    stored <- readAccounts (acBackend service)
    case stored of
        Left err -> pure (Left err)
        Right accounts -> case filter ((== normalize user) . accountUser) accounts of
            [a] | accountEnabled a && accountCanLogin a && accountAllowSudoAuth a -> do
                stamp <- currentStamp
                written <- beAccounts (acBackend service) (ReqAccountLogin (accountUser a) (Just stamp))
                case written of
                    Left _ -> pure (Left storageFailure)
                    Right _ -> Right <$> createVersionedSession (acSessions service) (accountUser a) (Just (accountRevision a))
            _ -> pure (Left unauthorized)

-- | 系统表里的账号清单（含管理员）
listAccounts :: Accounts -> IO (Either AccountError [Account])
listAccounts service = withMVar (acLock service) $ \_ -> fmap (fmap (filter (\a -> accountCanLogin a || not (T.null (accountHash a))))) (readAccounts (acBackend service))

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
createUser service name password = do
    policy <- accountsPolicy service
    case validateNewName service name >> passwordAllowed policy password of
        Left err -> pure (Left err)
        Right () -> do
            encoded <- hashPassword password
            storageResult <$> beAccounts (acBackend service) (ReqAccountCreate (normalize name) encoded)

-- | 设账号口令；管理员不受口令策略约束，空口令即免密
resetPassword :: Accounts -> Text -> Text -> IO (Either AccountError ())
resetPassword service name password = do
    policy <- accountsPolicy service
    stored <- readAccounts (acBackend service)
    case stored of
        Left err -> pure (Left err)
        Right identities -> do
            let admin = any (\a -> accountUser a == normalize name && accountIsSuperuser a) identities
                checked = if admin then validateUserName name else validateUserName name >> passwordAllowed policy password
            case checked of
                Left err -> pure (Left err)
                Right () -> do
                    encoded <- if admin && T.null password then pure "" else hashPassword password
                    storageResult <$> beAccounts (acBackend service) (ReqAccountReset (normalize name) encoded)

-- | 删一个普通账号
dropUser :: Accounts -> Text -> IO (Either AccountError ())
dropUser service name =
    case validateUserName name of
            Left err -> pure (Left err)
            Right () -> storageResult <$> beAccounts (acBackend service) (ReqAccountDrop (normalize name))

-- | 账号管理命令：SQL 语句与一键接口在这上面汇合
data AccountCommand
    = CreateAccount Text Text
    | ResetAccountPassword Text Text
    | DropAccount Text
    | AlterAccountAttributes Text [(String, Bool)]
    deriving (Show, Eq)

-- | 一条语句是不是账号管理语句
accountCommand :: Statement -> Maybe AccountCommand
accountCommand (CreateUser name password) = Just (CreateAccount (T.pack name) (T.pack password))
accountCommand (AlterUser name password) = Just (ResetAccountPassword (T.pack name) (T.pack password))
accountCommand (DropUser name) = Just (DropAccount (T.pack name))
accountCommand (AlterIdentity name attributes) = Just (AlterAccountAttributes (T.pack name) attributes)
accountCommand _ = Nothing

-- | 按目录管理权限执行账号命令
runAccountCommand :: Accounts -> Principal -> AccountCommand -> IO (Either AccountError ())
runAccountCommand service principal command = withMVar (acLock service) $ \_ -> run
  where
    -- | 校验并执行身份管理权限
    run
        | not (principalIsCatalogManager principal) = pure (Left (AccountError "forbidden" "catalog manager required"))
        | not (principalIsRoot principal) = do
            identities <- readAccounts (acBackend service)
            case identities of
                Left err -> pure (Left err)
                Right accounts
                    | any (\a -> accountUser a == normalize (target command) && (accountIsSuperuser a || accountSystemCatalogManager a || accountAllowSudoAuth a)) accounts
                        || privilegedChange command -> pure (Left (AccountError "forbidden" "superuser required to manage privileged identities"))
                    | otherwise -> execute
        | otherwise = execute
    -- | 获取身份管理的目标
    target (CreateAccount name _) = name
    target (ResetAccountPassword name _) = name
    target (DropAccount name) = name
    target (AlterAccountAttributes name _) = name
    -- | 判断是否修改特权属性
    privilegedChange (AlterAccountAttributes _ attributes) = any (\(key, _) -> key `elem` ["superuser", "system_catalog_manager", "allow_sudo_auth"]) attributes
    privilegedChange _ = False
    -- | 执行已授权的身份命令
    execute = executeWith (if principalIsRoot principal then service else service{acBackend = (acBackend service){beAccounts = \request -> beAccounts (acBackend service) (ReqCatalogManage request)}})
    -- | 通过受限存储请求执行管理操作
    executeWith managed = case command of
        CreateAccount name _ | normalize name == "public" -> pure (Left (AccountError "bad_request" "public is reserved for default privileges"))
        CreateAccount name password -> createUser managed name password
        ResetAccountPassword name password -> resetPassword managed name password
        DropAccount name -> dropUser managed name
        AlterAccountAttributes name attributes
            | length (nub (map fst attributes)) /= length attributes -> pure (Left (AccountError "bad_request" "duplicate identity attribute"))
            | otherwise ->
                storageResult <$> beAccounts (acBackend managed) (ReqIdentityAlter (normalize name)
                    (lookup "login" attributes) (lookup "superuser" attributes) (lookup "enabled" attributes) (lookup "system_catalog_manager" attributes) (lookup "allow_sudo_auth" attributes))
