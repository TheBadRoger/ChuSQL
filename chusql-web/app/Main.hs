{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.Web.Api (AppEnv (..), Live (..), newAppEnv, setLive, webApp)
import ChuSQL.Web.Auth (SessionPolicy (..), newSessionStore)
import ChuSQL.Web.Backend (Backend, ipcBackend)
import ChuSQL.Web.Demo (SeedReport (..), seedDemo)
import ChuSQL.Web.Config (
    WebConfig (..),
    defaultPassword,
    defaultUser,
    loadWebConfig,
    resolveCredential,
    resolveStaticDir,
    staticDirCandidates,
    usingDefaultCredentials,
 )
import ChuSQL.Web.RateLimit (newRateLimiter)
import ChuSQL.Web.Settings (effectiveSettings)
import ChuSQL.Web.StorageProcess (
    StorageProcess (..),
    findStorageServer,
    startStorageProcess,
    stopStorageProcess,
    waitForStorage,
 )
import Control.Exception (IOException, bracket, try)
import Data.Char (toLower)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time.Clock (getCurrentTime)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Text.Read (readMaybe)

-- Web 管理端入口：读配置、备凭据、拉起存储进程，最后起 Warp。

data Options = Options
    { optHost :: Maybe String
    , optPort :: Maybe Int
    , optStatic :: Maybe FilePath
    , optStorage :: Maybe FilePath
    , optUser :: Maybe Text
    , optCookieSecure :: Maybe Bool
    , optBodyLimit :: Maybe Int
    , optSessionIdle :: Maybe Int
    , optSessionMax :: Maybe Int
    , optLoginMaxAttempts :: Maybe Int
    , optLoginWindow :: Maybe Int
    , optPageSize :: Maybe Int
    , optMaxPageSize :: Maybe Int
    , optMaxRows :: Maybe Int
    , optMaxSqlLength :: Maybe Int
    , optSeedDemo :: Maybe Bool
    , optHelp :: Bool
    }

-- | 一个开关都没给
emptyOptions :: Options
emptyOptions =
    Options
        { optHost = Nothing
        , optPort = Nothing
        , optStatic = Nothing
        , optStorage = Nothing
        , optUser = Nothing
        , optCookieSecure = Nothing
        , optBodyLimit = Nothing
        , optSessionIdle = Nothing
        , optSessionMax = Nothing
        , optLoginMaxAttempts = Nothing
        , optLoginWindow = Nothing
        , optPageSize = Nothing
        , optMaxPageSize = Nothing
        , optMaxRows = Nothing
        , optMaxSqlLength = Nothing
        , optSeedDemo = Nothing
        , optHelp = False
        }

-- | 解析参数
parseArgs :: [String] -> Either String Options
parseArgs = go emptyOptions
  where
    go opts [] = Right opts
    go opts ("--help" : rest) = go opts{optHelp = True} rest
    go opts ("-h" : rest) = go opts{optHelp = True} rest
    go opts ("--host" : v : rest) = go opts{optHost = Just v} rest
    go opts ("--static" : v : rest) = go opts{optStatic = Just v} rest
    go opts ("--storage" : v : rest) = go opts{optStorage = Just v} rest
    go opts ("--user" : v : rest) = go opts{optUser = Just (T.pack v)} rest
    go opts ("--port" : v : rest) = withInt "--port" v (\n -> go opts{optPort = Just n} rest)
    go opts ("--body-limit" : v : rest) = withInt "--body-limit" v (\n -> go opts{optBodyLimit = Just n} rest)
    go opts ("--session-idle" : v : rest) = withInt "--session-idle" v (\n -> go opts{optSessionIdle = Just n} rest)
    go opts ("--session-max" : v : rest) = withInt "--session-max" v (\n -> go opts{optSessionMax = Just n} rest)
    go opts ("--login-max-attempts" : v : rest) = withInt "--login-max-attempts" v (\n -> go opts{optLoginMaxAttempts = Just n} rest)
    go opts ("--login-window" : v : rest) = withInt "--login-window" v (\n -> go opts{optLoginWindow = Just n} rest)
    go opts ("--page-size" : v : rest) = withInt "--page-size" v (\n -> go opts{optPageSize = Just n} rest)
    go opts ("--max-page-size" : v : rest) = withInt "--max-page-size" v (\n -> go opts{optMaxPageSize = Just n} rest)
    go opts ("--max-rows" : v : rest) = withInt "--max-rows" v (\n -> go opts{optMaxRows = Just n} rest)
    go opts ("--max-sql-length" : v : rest) = withInt "--max-sql-length" v (\n -> go opts{optMaxSqlLength = Just n} rest)
    go opts ("--seed" : v : rest) = go opts{optSeedDemo = Just (isTrue v)} rest
    go opts ("--cookie-secure" : v : rest) = go opts{optCookieSecure = Just (isTrue v)} rest
    go _ (a : _) = Left ("unrecognized argument or missing value: " ++ a)

    -- | 读一个整数开关
    withInt :: String -> String -> (Int -> Either String Options) -> Either String Options
    withInt name v k = maybe (Left (name ++ " needs an integer, got: " ++ v)) k (readMaybe v)

    -- | 布尔开关认 1/true/yes/on
    isTrue :: String -> Bool
    isTrue s = map toLower s `elem` ["1", "true", "yes", "on"]

-- | 帮助
usage :: IO ()
usage = do
    putStrLn "usage: chusql-web [options]"
    putStrLn ""
    putStrLn "  --host H               listen address              (CHUSQL_WEB_HOST, default 127.0.0.1)"
    putStrLn "  --port N               listen port                 (CHUSQL_WEB_PORT, default 7777)"
    putStrLn "  --static DIR           static asset directory      (CHUSQL_WEB_STATIC, default static)"
    putStrLn "  --user NAME            account name                (CHUSQL_WEB_USER, default admin)"
    putStrLn "  --storage SERVER       Rust storage exe to spawn   (CHUSQL_STORAGE_SERVER)"
    putStrLn "  --cookie-secure BOOL   add Secure to the cookie    (CHUSQL_WEB_COOKIE_SECURE, default false)"
    putStrLn "  --body-limit N         max request body bytes      (CHUSQL_WEB_BODY_LIMIT, default 65536)"
    putStrLn "  --session-idle N       session idle timeout, sec   (CHUSQL_WEB_SESSION_IDLE, default 28800)"
    putStrLn "  --session-max N        session max age, sec        (CHUSQL_WEB_SESSION_MAX, default 86400)"
    putStrLn "  --login-max-attempts N failed logins before 429    (CHUSQL_WEB_LOGIN_MAX_ATTEMPTS, default 5)"
    putStrLn "  --login-window N       lockout window, sec         (CHUSQL_WEB_LOGIN_WINDOW, default 300)"
    putStrLn "  --page-size N          rows per page by default    (CHUSQL_WEB_PAGE_SIZE, default 25)"
    putStrLn "  --max-page-size N      rows per page ceiling       (CHUSQL_WEB_MAX_PAGE_SIZE, default 500)"
    putStrLn "  --max-rows N           rows returned per query     (CHUSQL_WEB_MAX_ROWS, default 1000)"
    putStrLn "  --max-sql-length N     max SQL characters          (CHUSQL_WEB_MAX_SQL_LENGTH, default 20000)"
    putStrLn "  --seed BOOL            load demo data when the db is empty (CHUSQL_WEB_SEED, default false)"
    putStrLn ""
    putStrLn "password: CHUSQL_WEB_PASSWORD_HASH > CHUSQL_WEB_PASSWORD > built-in demo password"
    putStrLn ("without any of them the demo account is " ++ T.unpack defaultUser ++ " / " ++ T.unpack defaultPassword)
    putStrLn "storage knobs for the spawned process: CHUSQL_PAGE_SIZE / CHUSQL_BTREE_ORDER /"
    putStrLn "  CHUSQL_BUFFER_POOL_SIZE / CHUSQL_LOG / CHUSQL_PIPE / CHUSQL_DATA_DIR"

-- | 命令行覆盖配置
applyOptions :: WebConfig -> Options -> WebConfig
applyOptions cfg opts =
    cfg
        { wcHost = orElse (optHost opts) (wcHost cfg)
        , wcPort = orElse (optPort opts) (wcPort cfg)
        , wcStaticDir = orElse (optStatic opts) (wcStaticDir cfg)
        , wcStorageServer = case optStorage opts of
            Just p -> Just p
            Nothing -> wcStorageServer cfg
        , wcUser = orElse (optUser opts) (wcUser cfg)
        , wcCookieSecure = orElse (optCookieSecure opts) (wcCookieSecure cfg)
        , wcBodyLimit = orElse (optBodyLimit opts) (wcBodyLimit cfg)
        , wcSessionIdle = orElse (optSessionIdle opts) (wcSessionIdle cfg)
        , wcSessionMax = orElse (optSessionMax opts) (wcSessionMax cfg)
        , wcLoginMaxAttempts = orElse (optLoginMaxAttempts opts) (wcLoginMaxAttempts cfg)
        , wcLoginWindow = orElse (optLoginWindow opts) (wcLoginWindow cfg)
        , wcPageSize = orElse (optPageSize opts) (wcPageSize cfg)
        , wcMaxPageSize = orElse (optMaxPageSize opts) (wcMaxPageSize cfg)
        , wcMaxRows = orElse (optMaxRows opts) (wcMaxRows cfg)
        , wcMaxSqlLength = orElse (optMaxSqlLength opts) (wcMaxSqlLength cfg)
        , wcSeedDemo = orElse (optSeedDemo opts) (wcSeedDemo cfg)
        }
  where
    -- | 开关给了用开关的，否则保留原值
    orElse :: Maybe a -> a -> a
    orElse (Just x) _ = x
    orElse Nothing y = y

-- | 配了存储进程路径就拉起来，没配就假设外面已经跑着
startStorageIfWanted :: WebConfig -> IO (Either String (Maybe StorageProcess))
startStorageIfWanted cfg = case wcStorageServer cfg of
    Nothing -> pure (Right Nothing)
    Just given -> do
        found <- findStorageServer (Just given)
        case found of
            Left e -> pure (Left e)
            Right bin -> fmap (fmap Just) (startStorageProcess bin)

-- | 入口
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case parseArgs args of
        Left err -> do
            putStrLn ("bad arguments: " ++ err)
            usage
            exitFailure
        Right opts
            | optHelp opts -> usage
            | otherwise -> run opts

-- | 起服务
run :: Options -> IO ()
run opts = do
    base <- loadWebConfig
    let cfg = applyOptions base opts
    foundStatic <- resolveStaticDir (wcStaticDir cfg)
    case foundStatic of
        Nothing -> do
            putStrLn ("static directory not found; looked in: " ++ unwords (staticDirCandidates (wcStaticDir cfg)))
            exitFailure
        Just staticDir -> do
            cred <- resolveCredential cfg
            storage <- startStorageIfWanted cfg
            case storage of
                Left err -> do
                    putStrLn err
                    exitFailure
                Right maybeStorage -> do
                    backend <- ipcBackend
                    let policy =
                            SessionPolicy
                                (fromIntegral (wcSessionIdle cfg))
                                (fromIntegral (wcSessionMax cfg))
                    sessions <- newSessionStore getCurrentTime policy
                    limiter <-
                        newRateLimiter
                            getCurrentTime
                            (wcLoginMaxAttempts cfg)
                            (fromIntegral (wcLoginWindow cfg))
                    env0 <- newAppEnv backend sessions cred limiter staticDir
                    let env =
                            env0
                                { aeCookieSecure = wcCookieSecure cfg
                                , aeEffective = effectiveSettings cfg
                                , aeLog = \msg -> TIO.putStrLn ("[web] " <> msg)
                                }
                    setLive env (liveFromConfig cfg)
                    app <- webApp env
                    printBanner cfg staticDir maybeStorage
                    reportCredential cfg
                    waitForStorageReport
                    seedIfWanted cfg backend
                    let settings =
                            Warp.setPort (wcPort cfg) (Warp.setHost (fromString (wcHost cfg)) Warp.defaultSettings)
                    case maybeStorage of
                        Nothing -> serve settings app
                        Just sp -> bracket (pure sp) stopStorageProcess (const (serve settings app))

-- | 本次启动的配置摊成热改参数
liveFromConfig :: WebConfig -> Live
liveFromConfig cfg =
    Live
        { lvBodyLimit = wcBodyLimit cfg
        , lvSessionIdle = wcSessionIdle cfg
        , lvSessionMax = wcSessionMax cfg
        , lvLoginMaxAttempts = wcLoginMaxAttempts cfg
        , lvLoginWindow = wcLoginWindow cfg
        , lvPageSize = wcPageSize cfg
        , lvMaxPageSize = wcMaxPageSize cfg
        , lvMaxRows = wcMaxRows cfg
        , lvMaxSqlLength = wcMaxSqlLength cfg
        }

-- | 起 HTTP 服务，绑不上端口给人话
serve :: Warp.Settings -> Application -> IO ()
serve settings app = do
    result <- try (Warp.runSettings settings app) :: IO (Either IOException ())
    case result of
        Right () -> pure ()
        Left e -> do
            putStrLn ("failed to serve HTTP: " ++ show e)
            putStrLn "  the port is probably taken or reserved by the system; try another one: --port 7778"
            putStrLn "  on Windows: netsh interface ipv4 show excludedportrange protocol=tcp"
            exitFailure

-- | 口令是从哪来的（用了内置默认口令就明确警告）
reportCredential :: WebConfig -> IO ()
reportCredential cfg
    | usingDefaultCredentials cfg = do
        putStrLn ""
        putStrLn "  !! DEMO CREDENTIALS IN USE:"
        putStrLn ("  !!   user " ++ T.unpack (wcUser cfg) ++ " / password " ++ T.unpack defaultPassword)
        putStrLn "  !!   set CHUSQL_WEB_PASSWORD (or CHUSQL_WEB_PASSWORD_HASH) before exposing this server."
        putStrLn ""
    | otherwise = putStrLn ("password:     from CHUSQL_WEB_PASSWORD(_HASH)   (user " ++ T.unpack (wcUser cfg) ++ ")")

-- | 配了 seed 就补一份演示数据
seedIfWanted :: WebConfig -> Backend -> IO ()
seedIfWanted cfg backend
    | not (wcSeedDemo cfg) = pure ()
    | otherwise = do
        result <- seedDemo backend
        case result of
            Left e -> TIO.putStrLn ("[web] demo data skipped: " <> T.pack e)
            Right report ->
                TIO.putStrLn
                    ( "[web] demo data: "
                        <> (if null (srCreated report) then "already present" else "created " <> T.pack (unwords (srCreated report)))
                        <> (if null (srSkipped report) then "" else ", kept " <> T.pack (unwords (srSkipped report)))
                        <> (if null (srIndexes report) then "" else ", index on " <> T.pack (unwords (srIndexes report)))
                    )

-- | 报一下存储进程通不通
waitForStorageReport :: IO ()
waitForStorageReport = do
    up <- waitForStorage 20
    putStrLn
        ( if up
            then "storage:      up (named pipe answered ping)"
            else "storage:      DOWN (no ping reply; start the Rust storage process or pass --storage)"
        )

-- | 启动横幅
printBanner :: WebConfig -> FilePath -> Maybe StorageProcess -> IO ()
printBanner cfg staticDir maybeStorage = do
    putStrLn "==================== ChuSQL Web ===================="
    putStrLn ("url:          http://" ++ wcHost cfg ++ ":" ++ show (wcPort cfg) ++ "/")
    putStrLn ("static dir:   " ++ staticDir)
    putStrLn
        ( "limits:       body "
            ++ show (wcBodyLimit cfg)
            ++ "B, page "
            ++ show (wcPageSize cfg)
            ++ "/"
            ++ show (wcMaxPageSize cfg)
            ++ " rows, query rows "
            ++ show (wcMaxRows cfg)
            ++ ", sql "
            ++ show (wcMaxSqlLength cfg)
            ++ " chars"
        )
    putStrLn
        ( "auth:         session idle "
            ++ show (wcSessionIdle cfg)
            ++ "s / max "
            ++ show (wcSessionMax cfg)
            ++ "s, lockout after "
            ++ show (wcLoginMaxAttempts cfg)
            ++ " failures in "
            ++ show (wcLoginWindow cfg)
            ++ "s"
        )
    case maybeStorage of
        Just sp -> putStrLn ("storage proc: " ++ spPipe sp ++ "  data dir " ++ spDataDir sp)
        Nothing -> putStrLn "storage proc: assumed to run elsewhere (pipe name from CHUSQL_PIPE)"
