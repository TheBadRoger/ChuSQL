{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.Core.Engine.Storage.FFI (storageVersion)
import ChuSQL.Core.Engine.Storage.IPC (closeConnection, localStorageLink, setStorageLink)
import ChuSQL.Interface.Config (
    WebConfig (..),
    defaultUser,
    legacyPasswordKey,
    loadWebConfigAt,
    rootUserName,
 )
import ChuSQL.Interface.RateLimit (newRateLimiter)
import ChuSQL.Interface.TOML (resolveConfigPath)
import ChuSQL.Server.Accounts (administratorPasswordless)
import ChuSQL.Server.Backend (Backend (..), ipcBackend)
import ChuSQL.Server.TCP (
    ServerConfig (..),
    ServerEnv,
    loadServerConfigAt,
    newServerEnv,
    runServer,
 )
import Control.Exception (IOException, bracket, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import GHC.Conc (getNumProcessors, setNumCapabilities)
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
    go opts ("--listen-host" : v : rest) = go opts{optHost = Just v} rest
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
    putStrLn "config: settings.toml sits at a fixed place and holds every knob;"
    putStrLn "        Windows: %APPDATA%\\ChuSQL\\settings.toml"
    putStrLn "        other:   $XDG_CONFIG_HOME/ChuSQL/settings.toml (or ~/.config/ChuSQL/settings.toml)"
    putStrLn "        [storage] data_dir/log_files; [server] listen_host/port/max_message/max_rows;"
    putStrLn "        [web] listen_host/port and the limits; the administrator name only comes from --user"
    putStrLn ""
    putStrLn "  --config FILE          read that config file instead"
    putStrLn "  --listen-host H        listen address              ([server] listen_host, default 127.0.0.1)"
    putStrLn "  --port N               listen port                 ([server] port, default 7777)"
    putStrLn ("  --user NAME            administrator name          (default " ++ T.unpack defaultUser ++ ")")
    putStrLn "  --max-message N        max bytes per request line  ([server] max_message, default 1048576)"
    putStrLn "  --max-rows N           rows returned per query     ([server] max_rows, default 1000)"
    putStrLn ""
    putStrLn "protocol: one JSON object per line; hello / login / query / storage / ping / quit"
    putStrLn "  {\"method\":\"login\",\"user\":\"root\",\"password\":\"...\"} then {\"method\":\"query\",\"sql\":\"SELECT 1\"}"
    putStrLn "  {\"method\":\"storage\",\"request\":{...}} forwards one storage request (Web and the CLI use this)"
    putStrLn ""
    putStrLn "storage: this process loads the chusql_core_storage library in-process and owns the data directory"
    putStrLn ""
    putStrLn "administrator password: lives in the __system_users system table, never in settings.toml"
    putStrLn "  the administrator name comes from --user; settings.toml does not carry it"
    putStrLn ""
    putStrLn "first run: the installer calls csql-bootstrap, which creates the system catalog"
    putStrLn "  {\"method\":\"system_status\"} tells whether that happened; without it the server stops"

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

-- | 入口：开够能力数，分片扫描才真并行
main :: IO ()
main = do
    caps <- getNumProcessors
    setNumCapabilities (max 1 (min 8 caps))
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
    legacy <- legacyPasswordKey configPath
    let rootName = rootUserName cfg
    config <- applyServerOptions <$> loadServerConfigAt configPath <*> pure opts
    opened <- localStorageLink (Just configPath)
    case opened of
        Left err -> do
            putStrLn ("cannot open the storage library: " ++ err)
            putStrLn "Stop all database processes, then try: csql-bootstrap repair --config <settings.toml>"
            putStrLn "If data recovery is required, use csql-bootstrap recover; reset discards system identities and grants."
            exitFailure
        Right link -> do
            setStorageLink link
            backend <- ipcBackend
            printBanner cfg config configPath
            checkStorage backend
            checkInitialized backend
            reportCredential backend rootName legacy
            limiter <-
                newRateLimiter
                    getCurrentTime
                    (wcLoginMaxAttempts cfg)
                    (fromIntegral (wcLoginWindow cfg))
            env <- newServerEnv backend rootName configPath config limiter
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

-- | 系统目录没引导过就不许起服务
checkInitialized :: Backend -> IO ()
checkInitialized backend = do
    status <- beStorage backend (A.object ["method" A..= ("system_status" :: Text)])
    case status of
        Right value
            | statusInitialized value -> putStrLn "system:       ready (the catalog is initialized)"
        _ -> do
            putStrLn "!! the system catalog is not initialized; run the bootstrap program first:"
            putStrLn "!!   csql-bootstrap --config <settings.toml> --password-stdin"
            putStrLn "!! For a damaged existing system catalog, stop all processes and try csql-bootstrap repair or recover."
            exitFailure
  where
    -- | 应答里的 initialized 是不是 true
    statusInitialized (A.Object fields) = KM.lookup "initialized" fields == Just (A.Bool True)
    statusInitialized _ = False

-- | 报告管理员账号状态：配置里还留着明文口令、或还没设口令都提醒
reportCredential :: Backend -> Text -> Bool -> IO ()
reportCredential backend rootName legacy = do
    putStrLn ("administrator: " ++ T.unpack rootName)
    if legacy
        then do
            putStrLn "  !! the [web] section still carries a plaintext password key; it is ignored now"
            putStrLn "  !! remove it: the administrator password lives in the __system_users system table"
        else pure ()
    passwordless <- administratorPasswordless backend rootName
    if passwordless
        then do
            putStrLn "  !! the administrator has no password yet: sign in with an empty password, then set one"
            putStrLn "  !!   ALTER USER root IDENTIFIED BY '...'   (or use the account page in the Web UI)"
        else pure ()

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
    putStrLn ("log files:    " ++ wcLogFiles cfg)
