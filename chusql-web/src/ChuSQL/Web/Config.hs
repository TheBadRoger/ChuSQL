{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.Config (
    WebConfig (..),
    defaultWebConfig,
    defaultUser,
    defaultPassword,
    usingDefaultCredentials,
    loadWebConfig,
    resolveCredential,
    resolveStaticDir,
    staticDirCandidates,
) where

import ChuSQL.Web.Auth (Credential (..), hashPassword)
import Data.Char (toLower)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import System.Environment (lookupEnv)
import Text.Read (readMaybe)

-- 运行参数：命令行 > 环境变量 > 内置默认；默认只听本机 127.0.0.1:7777。

-- | 默认账号名
defaultUser :: Text
defaultUser = "root"

-- | 默认口令（只为方便本地测试；生产/对外必须覆盖）
defaultPassword :: Text
defaultPassword = "chusql"

data WebConfig = WebConfig
    { wcHost :: String
    , wcPort :: Int
    , wcStaticDir :: FilePath
    , wcUser :: Text
    , wcPasswordHash :: Maybe Text
    , wcPassword :: Maybe Text
    , wcStorageServer :: Maybe FilePath
    , wcPipeName :: Maybe String
    , wcDataDir :: Maybe FilePath
    , wcCookieSecure :: Bool
    , wcBodyLimit :: Int
    , wcSessionIdle :: Int
    , wcSessionMax :: Int
    , wcLoginMaxAttempts :: Int
    , wcLoginWindow :: Int
    , wcPageSize :: Int
    , wcMaxPageSize :: Int
    , wcMaxRows :: Int
    , wcMaxSqlLength :: Int
    , wcSeedDemo :: Bool
    }
    deriving (Show, Eq)

-- | 内置默认
defaultWebConfig :: WebConfig
defaultWebConfig =
    WebConfig
        { wcHost = "127.0.0.1"
        , wcPort = 7777
        , wcStaticDir = "static"
        , wcUser = defaultUser
        , wcPasswordHash = Nothing
        , wcPassword = Nothing
        , wcStorageServer = Nothing
        , wcPipeName = Nothing
        , wcDataDir = Nothing
        , wcCookieSecure = False
        , wcBodyLimit = 65536
        , wcSessionIdle = 8 * 3600
        , wcSessionMax = 24 * 3600
        , wcLoginMaxAttempts = 5
        , wcLoginWindow = 5 * 60
        , wcPageSize = 25
        , wcMaxPageSize = 500
        , wcMaxRows = 1000
        , wcMaxSqlLength = 20000
        , wcSeedDemo = False
        }

-- | 这份配置用的还是内置默认口令吗
usingDefaultCredentials :: WebConfig -> Bool
usingDefaultCredentials cfg = wcPasswordHash cfg == Nothing && wcPassword cfg == Nothing

-- | 备一份凭据：哈希 > 明文 > 内置默认
resolveCredential :: WebConfig -> IO Credential
resolveCredential cfg = case (wcPasswordHash cfg, wcPassword cfg) of
    (Just encoded, _) -> pure (Credential (wcUser cfg) encoded)
    (Nothing, Just plain) -> do
        encoded <- hashPassword plain
        pure (Credential (wcUser cfg) encoded)
    (Nothing, Nothing) -> do
        encoded <- hashPassword defaultPassword
        pure (Credential (wcUser cfg) encoded)

-- | 从环境变量读配置，缺省用内置默认
loadWebConfig :: IO WebConfig
loadWebConfig = do
    let d = defaultWebConfig
    host <- envOr "CHUSQL_WEB_HOST" (wcHost d)
    port <- envInt "CHUSQL_WEB_PORT" (wcPort d)
    staticDir <- envOr "CHUSQL_WEB_STATIC" (wcStaticDir d)
    user <- T.pack <$> envOr "CHUSQL_WEB_USER" (T.unpack (wcUser d))
    passwordHash <- envText "CHUSQL_WEB_PASSWORD_HASH"
    password <- envText "CHUSQL_WEB_PASSWORD"
    server <- envMaybe "CHUSQL_STORAGE_SERVER"
    pipe <- envMaybe "CHUSQL_PIPE"
    dataDir <- envMaybe "CHUSQL_DATA_DIR"
    secure <- envBool "CHUSQL_WEB_COOKIE_SECURE" (wcCookieSecure d)
    limit <- envInt "CHUSQL_WEB_BODY_LIMIT" (wcBodyLimit d)
    idle <- envInt "CHUSQL_WEB_SESSION_IDLE" (wcSessionIdle d)
    sessionMax <- envInt "CHUSQL_WEB_SESSION_MAX" (wcSessionMax d)
    loginMax <- envInt "CHUSQL_WEB_LOGIN_MAX_ATTEMPTS" (wcLoginMaxAttempts d)
    loginWindow <- envInt "CHUSQL_WEB_LOGIN_WINDOW" (wcLoginWindow d)
    pageSize <- envInt "CHUSQL_WEB_PAGE_SIZE" (wcPageSize d)
    maxPageSize <- envInt "CHUSQL_WEB_MAX_PAGE_SIZE" (wcMaxPageSize d)
    maxRows <- envInt "CHUSQL_WEB_MAX_ROWS" (wcMaxRows d)
    maxSqlLength <- envInt "CHUSQL_WEB_MAX_SQL_LENGTH" (wcMaxSqlLength d)
    seedDemo <- envBool "CHUSQL_WEB_SEED" (wcSeedDemo d)
    pure
        WebConfig
            { wcHost = host
            , wcPort = port
            , wcStaticDir = staticDir
            , wcUser = user
            , wcPasswordHash = passwordHash
            , wcPassword = password
            , wcStorageServer = server
            , wcPipeName = pipe
            , wcDataDir = dataDir
            , wcCookieSecure = secure
            , wcBodyLimit = limit
            , wcSessionIdle = idle
            , wcSessionMax = sessionMax
            , wcLoginMaxAttempts = loginMax
            , wcLoginWindow = loginWindow
            , wcPageSize = pageSize
            , wcMaxPageSize = maxPageSize
            , wcMaxRows = maxRows
            , wcMaxSqlLength = maxSqlLength
            , wcSeedDemo = seedDemo
            }

-- | 静态目录候选位置，按顺序取第一个存在的
staticDirCandidates :: FilePath -> [FilePath]
staticDirCandidates given =
    [given, "static", "chusql-web/static", "../chusql-web/static", "../static"]

-- | 找出真正能用的静态目录
resolveStaticDir :: FilePath -> IO (Maybe FilePath)
resolveStaticDir given = go (staticDirCandidates given)
  where
    go [] = pure Nothing
    go (d : ds) = do
        ok <- doesDirectoryExist d
        if ok then pure (Just d) else go ds

-- | 读一个字符串，缺省给默认值
envOr :: String -> String -> IO String
envOr name def = fromMaybe def <$> lookupEnv name

-- | 读一个环境变量（空串当没设）
envMaybe :: String -> IO (Maybe String)
envMaybe name = do
    v <- lookupEnv name
    pure (case v of Just s | not (null s) -> Just s; _ -> Nothing)

-- | 读成 Text
envText :: String -> IO (Maybe Text)
envText name = fmap (fmap T.pack) (envMaybe name)

-- | 读成整数，解析不出来就用默认值
envInt :: String -> Int -> IO Int
envInt name def = do
    v <- envMaybe name
    pure (fromMaybe def (v >>= \s -> readMaybe s))

-- | 读成布尔（1/true/yes/on 都算真）
envBool :: String -> Bool -> IO Bool
envBool name def = do
    v <- envMaybe name
    pure $ case v of
        Nothing -> def
        Just s -> map toLower s `elem` ["1", "true", "yes", "on"]
