{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.Web.API (AppEnv (..), Live (..), newAppEnvAt, setLive, webApp)
import ChuSQL.Web.Auth (Credential (..), SessionPolicy (..), newSessionStore)
import ChuSQL.Web.Backend (Backend, ipcBackend)
import ChuSQL.Web.Demo (SeedReport (..), seedDemo)
import ChuSQL.Web.Config (
    WebConfig (..),
    defaultPassword,
    defaultUser,
    loadWebConfigAt,
    resolveCredential,
    resolvePipeName,
    resolveStaticDir,
    staticDirCandidates,
    usingDefaultCredentials,
 )
import ChuSQL.Web.RateLimit (newRateLimiter)
import ChuSQL.Storage.IPC (setPipeName)
import ChuSQL.Web.Settings (effectiveSettings, readSettingsFile)
import ChuSQL.Web.TOML (resolveConfigPath)
import ChuSQL.Web.StorageProcess (
    StorageProcess (..),
    findStorageServer,
    startStorageProcess,
    stopStorageProcess,
    waitForStorage,
 )
import Control.Exception (IOException, bracket, onException, try)
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
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
    { optConfig :: Maybe FilePath
    , optHost :: Maybe String
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
        { optConfig = Nothing
        , optHost = Nothing
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
    go opts ("--config" : v : rest) = go opts{optConfig = Just v} rest
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

    withInt :: String -> String -> (Int -> Either String Options) -> Either String Options
    withInt name v k = maybe (Left (name ++ " needs an integer, got: " ++ v)) k (readMaybe v)

    isTrue :: String -> Bool
    isTrue s = map toLower s `elem` ["1", "true", "yes", "on"]

-- | 帮助
usage :: IO ()
usage = do
    putStrLn "usage: chusql-web [options]"
    putStrLn ""
    putStrLn "config: chusql.toml sits at a fixed place and its sections hold every knob below;"
    putStrLn "        Windows: %APPDATA%\\ChuSQL\\chusql.toml"
    putStrLn "        other:   $XDG_CONFIG_HOME/ChuSQL/chusql.toml (or ~/.config/ChuSQL/chusql.toml)"
    putStrLn "        [web] host/port/static_dir/user/password and the limits; [server] pipe_name;"
    putStrLn "        [storage] data_dir; [page] size; [btree] order; [buffer] pool_size; [log] level"
    putStrLn ""
    putStrLn "  --config FILE          read that config file instead ([web] host ...)"
    putStrLn "  --host H               listen address              ([web] host, default 127.0.0.1)"
    putStrLn "  --port N               listen port                 ([web] port, default 7777)"
    putStrLn "  --static DIR           static asset directory      ([web] static_dir, default static)"
    putStrLn "  --user NAME            administrator name          ([web] user, default root)"
    putStrLn "  --storage SERVER       Rust storage exe to spawn   ([web] storage_server)"
    putStrLn "  --cookie-secure BOOL   add Secure to the cookie    ([web] cookie_secure, default false)"
    putStrLn "  --body-limit N         max request body bytes      ([web] body_limit, default 65536)"
    putStrLn "  --session-idle N       session idle timeout, sec   ([web] session_idle, default 28800)"
    putStrLn "  --session-max N        session max age, sec        ([web] session_max, default 86400)"
    putStrLn "  --login-max-attempts N failed logins before 429    ([web] login_max_attempts, default 5)"
    putStrLn "  --login-window N       lockout window, sec         ([web] login_window, default 300)"
    putStrLn "  --page-size N          rows per page by default    ([web] rows_per_page, default 25)"
    putStrLn "  --max-page-size N      rows per page ceiling       ([web] max_page_size, default 500)"
    putStrLn "  --max-rows N           rows returned per query     ([web] max_rows, default 1000)"
    putStrLn "  --max-sql-length N     max SQL characters          ([web] max_sql_length, default 20000)"
    putStrLn "  --seed BOOL            load demo data when the db is empty ([web] seed, default false)"
    putStrLn ""
    putStrLn "administrator password: user / password in the [web] section of chusql.toml, else the built-in demo"
    putStrLn "  an empty password means the administrator signs in with no password; ordinary accounts are then refused"
    putStrLn ("without either the demo administrator is " ++ T.unpack defaultUser ++ " / " ++ T.unpack defaultPassword)
    putStrLn "ordinary accounts: CREATE USER / ALTER USER / DROP USER from the SQL console (administrator only)"
    putStrLn "storage process: [server] pipe_name says where to talk and where a spawned one listens;"
    putStrLn "  [storage] data_dir, [page] size, [btree] order, [buffer] pool_size and [log] level are its own"

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
    orElse :: Maybe a -> a -> a
    orElse (Just x) _ = x
    orElse Nothing y = y

-- | 配了存储进程路径就拉起来，没配就假设外面已经跑着
startStorageIfWanted :: WebConfig -> FilePath -> IO (Either String (Maybe StorageProcess))
startStorageIfWanted cfg configPath = case wcStorageServer cfg of
    Nothing -> pure (Right Nothing)
    Just given -> do
        found <- findStorageServer (Just given)
        case found of
            Left e -> pure (Left e)
            Right bin -> fmap (fmap Just) (startStorageProcess bin configPath)

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
    configPath <- resolveConfigPath (optConfig opts)
    base <- loadWebConfigAt configPath
    let cfg = applyOptions base opts
    setPipeName (resolvePipeName cfg)
    foundStatic <- resolveStaticDir (wcStaticDir cfg)
    case foundStatic of
        Nothing -> do
            putStrLn ("static directory not found; looked in: " ++ unwords (staticDirCandidates (wcStaticDir cfg)))
            exitFailure
        Just staticDir -> do
            let settingsFile = configPath
            saved <- readSettingsFile settingsFile
            cred <- resolveCredential cfg saved
            storage <- startStorageIfWanted cfg configPath
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
                    env0 <- newAppEnvAt settingsFile backend sessions cred limiter staticDir
                        `onException` maybe (pure ()) stopStorageProcess maybeStorage
                    let env =
                            env0
                                { aeCookieSecure = wcCookieSecure cfg
                                , aeEffective = effectiveSettings cfg
                                , aeLog = TIO.putStrLn
                                }
                    setLive env (liveFromConfig cfg)
                    app <- webApp env
                    printBanner cfg configPath staticDir maybeStorage
                    reportCredential cred saved
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
reportCredential :: Credential -> Map.Map Text Text -> IO ()
reportCredential cred saved
    | usingDefaultCredentials saved = do
        putStrLn ""
        putStrLn "  !! Administrator credentials are not configured; the built-in demo password is in use:"
        putStrLn ("  !!   user " ++ T.unpack (credUser cred) ++ " / password " ++ T.unpack defaultPassword)
        putStrLn "  !!   set user / password in the [web] section of chusql.toml and restart"
        putStrLn ""
    | T.null (credEncoded cred) =
        putStrLn ("administrator: " ++ T.unpack (credUser cred) ++ " (passwordless: only the administrator may sign in)")
    | otherwise = putStrLn ("administrator: " ++ T.unpack (credUser cred) ++ " (credentials come from the settings file only)")

-- | 配了 seed 就补一份演示数据
seedIfWanted :: WebConfig -> Backend -> IO ()
seedIfWanted cfg backend
    | not (wcSeedDemo cfg) = pure ()
    | otherwise = do
        result <- seedDemo backend
        case result of
            Left e -> TIO.putStrLn ("demo data skipped: " <> T.pack e)
            Right report ->
                TIO.putStrLn
                    ( "demo data: "
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
            then "storage:      up (endpoint answered ping)"
            else "storage:      DOWN (no ping reply; start the Rust storage process or pass --storage)"
        )

-- | 启动横幅
printBanner :: WebConfig -> FilePath -> FilePath -> Maybe StorageProcess -> IO ()
printBanner cfg configPath staticDir maybeStorage = do
    putStrLn "==================== ChuSQL Web ===================="
    putStrLn ("url:          http://" ++ wcHost cfg ++ ":" ++ show (wcPort cfg) ++ "/")
    putStrLn ("config file:  " ++ configPath)
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
        Just sp -> putStrLn ("storage proc: spawned, talking on pipe " ++ spPipe sp)
        Nothing -> putStrLn "storage proc: assumed to run elsewhere (pipe name from [server] pipe_name)"
