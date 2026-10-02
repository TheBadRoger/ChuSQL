{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.Core.Engine.Storage.FFI (storageVersion)
import ChuSQL.Core.Engine.Storage.IPC (closeConnection, localStorageLink, setStorageLink)
import ChuSQL.Interface.Auth (Credential (..))
import ChuSQL.Interface.Config (
    WebConfig (..),
    defaultPassword,
    defaultUser,
    loadWebConfigAt,
    resolveCredential,
    usingDefaultCredentials,
 )
import ChuSQL.Interface.RateLimit (newRateLimiter)
import ChuSQL.Interface.Settings (readSettingsFile)
import ChuSQL.Interface.TOML (resolveConfigPath)
import ChuSQL.Server.Backend (Backend (..), ipcBackend)
import ChuSQL.Server.TCP (
    ServerConfig (..),
    ServerEnv,
    loadServerConfigAt,
    newServerEnv,
    runServer,
 )
import Control.Exception (IOException, bracket, try)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)
import Text.Read (readMaybe)

-- TCP 服务器入口：解析命令行与配置，装入存储库并以一行一 JSON 的协议对外服务。

data Options = Options
    { optConfig :: Maybe FilePath
    , optHost :: Maybe String
    , optPort :: Maybe Int
    , optUser :: Maybe Text
    , optMaxMessage :: Maybe Int
    , optMaxRows :: Maybe Int
    , optHelp :: Bool
    }

-- | 全部选项都取默认值
emptyOptions :: Options
emptyOptions =
    Options
        { optConfig = Nothing
        , optHost = Nothing
        , optPort = Nothing
        , optUser = Nothing
        , optMaxMessage = Nothing
        , optMaxRows = Nothing
        , optHelp = False
        }

-- | 解析参数
parseArgs :: [String] -> Either String Options
parseArgs = go emptyOptions
  where
    -- | 逐条消费参数列表
    go opts [] = Right opts
    go opts ("--help" : rest) = go opts{optHelp = True} rest
    go opts ("-h" : rest) = go opts{optHelp = True} rest
    go opts ("--config" : v : rest) = go opts{optConfig = Just v} rest
    go opts ("--host" : v : rest) = go opts{optHost = Just v} rest
    go opts ("--user" : v : rest) = go opts{optUser = Just (T.pack v)} rest
    go opts ("--port" : v : rest) = withInt "--port" v (\n -> go opts{optPort = Just n} rest)
    go opts ("--max-message" : v : rest) = withInt "--max-message" v (\n -> go opts{optMaxMessage = Just n} rest)
    go opts ("--max-rows" : v : rest) = withInt "--max-rows" v (\n -> go opts{optMaxRows = Just n} rest)
    go _ (a : _) = Left ("unrecognized argument or missing value: " ++ a)

    -- | 把参数值读成整数，读不出就报错
    withInt :: String -> String -> (Int -> Either String Options) -> Either String Options
    withInt name v k = maybe (Left (name ++ " needs an integer, got: " ++ v)) k (readMaybe v)

-- | 打印用法说明
usage :: IO ()
usage = do
    putStrLn "usage: chusql-server [options]"
    putStrLn ""
    putStrLn "config: chusql.toml sits at a fixed place and holds every knob;"
    putStrLn "        Windows: %APPDATA%\\ChuSQL\\chusql.toml"
    putStrLn "        other:   $XDG_CONFIG_HOME/ChuSQL/chusql.toml (or ~/.config/ChuSQL/chusql.toml)"
    putStrLn "        [storage] data_dir; [server] host/port/max_message/max_rows;"
    putStrLn "        [web] user/password (administrator credentials)"
    putStrLn ""
    putStrLn "  --config FILE          read that config file instead"
    putStrLn "  --host H               listen address              ([server] host, default 127.0.0.1)"
    putStrLn "  --port N               listen port                 ([server] port, default 7777)"
    putStrLn "  --user NAME            administrator name          ([web] user, default root)"
    putStrLn "  --max-message N        max bytes per request line  ([server] max_message, default 1048576)"
    putStrLn "  --max-rows N           rows returned per query     ([server] max_rows, default 1000)"
    putStrLn ""
    putStrLn "protocol: one JSON object per line; hello / login / query / storage / ping / quit"
    putStrLn "  {\"method\":\"login\",\"user\":\"root\",\"password\":\"...\"} then {\"method\":\"query\",\"sql\":\"SELECT 1\"}"
    putStrLn "  {\"method\":\"storage\",\"request\":{...}} forwards one storage request (Web and the CLI use this)"
    putStrLn ""
    putStrLn "storage: this process loads the chusql_core_storage library in-process and owns the data directory"
    putStrLn ""
    putStrLn "administrator password: user / password in the [web] section of chusql.toml, else the built-in demo"
    putStrLn "  an empty password means the administrator signs in with no password; ordinary accounts are then refused"
    putStrLn ("without either the demo administrator is " ++ T.unpack defaultUser ++ " / " ++ T.unpack defaultPassword)

-- | 命令行覆盖管理员
applyWebOptions :: WebConfig -> Options -> WebConfig
applyWebOptions cfg opts =
    cfg
        { wcUser = orElse (optUser opts) (wcUser cfg)
        }

-- | 命令行覆盖监听配置
applyServerOptions :: ServerConfig -> Options -> ServerConfig
applyServerOptions config opts =
    config
        { scHost = orElse (optHost opts) (scHost config)
        , scPort = orElse (optPort opts) (scPort config)
        , scMaxMessage = orElse (optMaxMessage opts) (scMaxMessage config)
        , scMaxRows = orElse (optMaxRows opts) (scMaxRows config)
        }

-- | 有 Just 就用它，否则用兜底值
orElse :: Maybe a -> a -> a
orElse (Just x) _ = x
orElse Nothing y = y

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

-- | 起服务：装入存储库并装配各服务组件
run :: Options -> IO ()
run opts = do
    configPath <- resolveConfigPath (optConfig opts)
    base <- loadWebConfigAt configPath
    let cfg = applyWebOptions base opts
    saved <- readSettingsFile configPath
    cred <- resolveCredential cfg saved
    config <- applyServerOptions <$> loadServerConfigAt configPath <*> pure opts
    opened <- localStorageLink (Just configPath)
    case opened of
        Left err -> do
            putStrLn ("cannot open the storage library: " ++ err)
            exitFailure
        Right link -> do
            setStorageLink link
            backend <- ipcBackend
            printBanner cfg config configPath
            checkStorage backend
            reportCredential cred saved
            limiter <-
                newRateLimiter
                    getCurrentTime
                    (wcLoginMaxAttempts cfg)
                    (fromIntegral (wcLoginWindow cfg))
            env <- newServerEnv backend cred configPath config limiter
            let announce port = putStrLn ("listening:    tcp://" ++ scHost config ++ ":" ++ show port)
            bracket (pure link) (const closeConnection) (const (serve env announce))

-- | 起 TCP 服务并处理绑定失败
serve :: ServerEnv -> (Int -> IO ()) -> IO ()
serve env announce = do
    result <- try (runServer env announce) :: IO (Either IOException ())
    case result of
        Right () -> pure ()
        Left e -> do
            putStrLn ("failed to serve TCP: " ++ show e)
            putStrLn "  the port is probably taken or reserved by the system; try another one: --port 7777"
            exitFailure

-- | 报告口令来源，用内置默认口令时告警
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

-- | 探测存储是否可用，答不上 ping 就退出
checkStorage :: Backend -> IO ()
checkStorage backend = do
    up <- bePing backend
    if up
        then putStrLn "storage:      up (the library answered ping)"
        else do
            putStrLn "storage:      DOWN (the storage library did not answer ping)"
            exitFailure

-- | 启动横幅
printBanner :: WebConfig -> ServerConfig -> FilePath -> IO ()
printBanner cfg config configPath = do
    version <- storageVersion
    putStrLn "================== ChuSQL Server ==================="
    putStrLn ("config file:  " ++ configPath)
    putStrLn ("protocol:     chusql 1 (one JSON object per line)")
    putStrLn
        ( "limits:       message "
            ++ show (scMaxMessage config)
            ++ "B, query rows "
            ++ show (scMaxRows config)
        )
    putStrLn ("storage:      chusql_core_storage library " ++ version)
    putStrLn ("data dir:     " ++ orElse (wcDataDir cfg) "(from the config file, else the default)")
