{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.Web.API (AppEnv (..), Live (..), newAppEnvAt, setLive, webApp)
import ChuSQL.Interface.Auth (SessionPolicy (..), setPayloadPolicy)
import ChuSQL.Interface.Config (
    ServerConfig (..),
    WebConfig (..),
    defaultUser,
    loadServerConfigAt,
    loadWebConfigAt,
    legacyPasswordKey,
    resolveStaticDir,
    rootUserName,
    staticDirCandidates,
 )
import ChuSQL.Interface.Link (clientPing, closeClient, connectClient)
import ChuSQL.Interface.RateLimit (newRateLimiter)
import ChuSQL.Interface.Session (authenticateSession, newSession)
import ChuSQL.Interface.Settings (effectiveSettings)
import ChuSQL.Interface.TOML (resolveConfigPath)
import ChuSQL.Web.Demo (SeedReport (..), seedDemo)
import Control.Exception (IOException, try)
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

-- Web 管理端入口：读配置，把请求转给拥有存储的 server，最后起 Warp。

data Options = Options
    { optConfig :: Maybe FilePath
    , optHost :: Maybe String
    , optPort :: Maybe Int
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
    -- | 逐个参数累加进 Options
    go opts [] = Right opts
    go opts ("--help" : rest) = go opts{optHelp = True} rest
    go opts ("-h" : rest) = go opts{optHelp = True} rest
    go opts ("--config" : v : rest) = go opts{optConfig = Just v} rest
    go opts ("--listen-host" : v : rest) = go opts{optHost = Just v} rest
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

    -- | 解析一个整数参数
    withInt :: String -> String -> (Int -> Either String Options) -> Either String Options
    withInt name v k = maybe (Left (name ++ " needs an integer, got: " ++ v)) k (readMaybe v)

    -- | 把 1/true/yes/on 认成真
    isTrue :: String -> Bool
    isTrue s = map toLower s `elem` ["1", "true", "yes", "on"]

-- | 打印命令行用法
usage :: IO ()
usage = do
    putStrLn "usage: chusql-web [options]"
    putStrLn ""
    putStrLn "config: settings.toml sits at a fixed place and its sections hold every knob below;"
    putStrLn "        Windows: %APPDATA%\\ChuSQL\\settings.toml"
    putStrLn "        other:   $XDG_CONFIG_HOME/ChuSQL/settings.toml (or ~/.config/ChuSQL/settings.toml)"
    putStrLn "        [web] listen_host/port and the limits;"
    putStrLn "        [storage] data_dir/log_files; [page] size; [btree] order; [buffer] pool_size; [log] level"
    putStrLn ""
    putStrLn "  --config FILE          read that config file instead ([web] listen_host ...)"
    putStrLn "  --listen-host H        listen address              ([web] listen_host, default 127.0.0.1)"
    putStrLn "  --port N               listen port                 ([web] port, default 7778)"
    putStrLn ("  --user NAME            administrator name          (default " ++ T.unpack defaultUser ++ ")")
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
    putStrLn "administrator password: lives in the __system_users system table, never in settings.toml"
    putStrLn "  the installer always sets one; an account whose hash is still empty signs in with an empty password"
    putStrLn "  the administrator name comes from --user; settings.toml does not carry it"
    putStrLn "ordinary accounts: CREATE USER / ALTER USER / DROP USER from the SQL console (administrator only)"
    putStrLn "storage: the server owns the data directory and loads the chusql_core_storage library;"
    putStrLn "  this process is a plain TCP client: every statement, lookup and account request goes to it"
    putStrLn "  [server] listen_host / port says where it listens; [storage] data_dir, [page] size, [btree] order,"
    putStrLn "  [buffer] pool_size and [log] level are the server's own"

-- | 命令行覆盖配置
applyOptions :: WebConfig -> Options -> WebConfig
applyOptions cfg opts =
    cfg
        { wcHost = orElse (optHost opts) (wcHost cfg)
        , wcPort = orElse (optPort opts) (wcPort cfg)
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
    -- | 有就用新的，没有留旧值
    orElse :: Maybe a -> a -> a
    orElse (Just x) _ = x
    orElse Nothing y = y

-- | 程序入口
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

-- | 读配置装配环境，检查 server，起 Warp
run :: Options -> IO ()
run opts = do
    configPath <- resolveConfigPath (optConfig opts)
    base <- loadWebConfigAt configPath
    let cfg = applyOptions base opts
    foundStatic <- resolveStaticDir (wcStaticDir cfg)
    case foundStatic of
        Nothing -> do
            putStrLn ("static directory not found; looked in: " ++ unwords (staticDirCandidates (wcStaticDir cfg)))
            exitFailure
        Just staticDir -> do
            let settingsFile = configPath
                policy = SessionPolicy (fromIntegral (wcSessionIdle cfg)) (fromIntegral (wcSessionMax cfg))
            legacy <- legacyPasswordKey settingsFile
            config <- loadServerConfigAt settingsFile
            limiter <-
                newRateLimiter
                    getCurrentTime
                    (wcLoginMaxAttempts cfg)
                    (fromIntegral (wcLoginWindow cfg))
            env0 <- newAppEnvAt settingsFile (T.pack (scHost config)) (scPort config) limiter staticDir
            let env =
                    env0
                        { aeCookieSecure = wcCookieSecure cfg
                        , aeEffective = effectiveSettings cfg
                        , aeLog = TIO.putStrLn
                        }
            setPayloadPolicy (aeSessions env) policy
            setLive env (liveFromConfig cfg)
            app <- webApp env
            printBanner cfg configPath staticDir
            reportCredential cfg legacy
            checkServer (aeServerHost env) (aeServerPort env)
            seedIfWanted cfg (aeServerHost env) (aeServerPort env)
            let settings =
                    Warp.setPort (wcPort cfg) (Warp.setHost (fromString (wcHost cfg)) Warp.defaultSettings)
            serve settings app

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

-- | 报告管理员账号状态：配置里还留着明文口令键就提醒
reportCredential :: WebConfig -> Bool -> IO ()
reportCredential cfg legacy = do
    putStrLn ("administrator: " ++ T.unpack (rootUserName cfg))
    if legacy
        then do
            putStrLn "  !! the [web] section still carries a plaintext password key; it is ignored now"
            putStrLn "  !! remove it: the administrator password lives in the __system_users system table"
        else pure ()

-- | 配了 seed 就补演示数据（管理员已设口令时跳过）
seedIfWanted :: WebConfig -> Text -> Int -> IO ()
seedIfWanted cfg host port
    | not (wcSeedDemo cfg) = pure ()
    | otherwise = do
        opened <- connectClient host port
        case opened of
            Left message -> TIO.putStrLn ("demo data skipped: " <> message)
            Right client -> do
                session <- newSession client
                signed <- authenticateSession session (rootUserName cfg) ""
                case signed of
                    Left message -> TIO.putStrLn ("demo data skipped: " <> message <> " (set the administrator password after seeding)")
                    Right () -> do
                        result <- seedDemo session
                        case result of
                            Left message -> TIO.putStrLn ("demo data skipped: " <> message)
                            Right report ->
                                TIO.putStrLn
                                    ( "demo data: "
                                        <> (if null (srCreated report) then "already present" else "created " <> T.pack (unwords (srCreated report)))
                                        <> (if null (srSkipped report) then "" else ", kept " <> T.pack (unwords (srSkipped report)))
                                        <> (if null (srIndexes report) then "" else ", index on " <> T.pack (unwords (srIndexes report)))
                                    )
                closeClient client

-- | ping 一次 server，报告它通不通
checkServer :: Text -> Int -> IO ()
checkServer host port = do
    opened <- connectClient host port
    case opened of
        Left message -> putStrLn ("storage:      DOWN (" ++ T.unpack message ++ ")")
        Right client -> do
            probe <- clientPing client
            closeClient client
            case probe of
                Right () -> putStrLn "storage:      up (the server answered ping)"
                Left message -> putStrLn ("storage:      DOWN (" ++ T.unpack message ++ ")")

-- | 启动横幅
printBanner :: WebConfig -> FilePath -> FilePath -> IO ()
printBanner cfg configPath staticDir = do
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
    putStrLn "storage:      through the server that owns the data directory"
