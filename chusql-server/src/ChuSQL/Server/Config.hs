{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.Config (
    RuntimeConfig (..),
    loadRuntimeConfigAt,
    defaultUser,
    rootUserName,
    ServerConfig (..),
    defaultServerConfig,
    loadServerConfigAt,
    validateServerConfig,
) where

import ChuSQL.Server.TOML (readSection)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, isNothing)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- 读取数据库运行参数与认证策略。

data RuntimeConfig = RuntimeConfig
    { rcUser :: Text
    , rcDataDir :: Maybe FilePath
    , rcLogFiles :: FilePath
    , rcLoginMaxAttempts :: Int
    , rcLoginWindow :: Int
    }
    deriving (Show, Eq)

-- 返回默认管理员名。
defaultUser :: Text
defaultUser = "root"

-- 使用显式指定的管理员名或默认值。
rootUserName :: RuntimeConfig -> Text
rootUserName cfg
    | not (T.null (T.strip (rcUser cfg))) = T.strip (rcUser cfg)
    | otherwise = defaultUser

-- 读取目录与认证限制，优先使用服务端配置。
loadRuntimeConfigAt :: FilePath -> IO RuntimeConfig
loadRuntimeConfigAt path = do
    storage <- readSection path "storage"
    server <- readSection path "server"
    let saved = server
        -- 读取非空配置值。
        text key values = case T.strip <$> Map.lookup key values of
            Just value | not (T.null value) -> Just value
            _ -> Nothing
        -- 读取整数或使用默认值。
        number key fallback = fromMaybe fallback (text key saved >>= readMaybe . T.unpack)
    pure
        ( RuntimeConfig
            ""
            (T.unpack <$> text "data_dir" storage)
            (maybe "./logs" T.unpack (text "log_files" storage))
            (number "login_max_attempts" 5)
            (number "login_window" 300)
        )

-- | 服务器监听参数
data ServerConfig = ServerConfig
    { scHost :: String
    , scPort :: Int
    , scMaxMessage :: Int
    , scMaxRows :: Int
    , scWorkerThreads :: Int
    , scMaxConcurrentQueries :: Int
    , scMaxConnections :: Int
    , scConnectionIdle :: Int
    , scTransactionIdle :: Int
    , scTransactionMax :: Int
    , scSocketPath :: Maybe FilePath
    , scLocalRootLogin :: Bool
    }
    deriving (Eq, Show)

-- | 默认本机监听、1 MiB 消息、1000 行、无本机免密
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig "127.0.0.1" 7777 (1024 * 1024) 1000 8 0 0 0 0 0 Nothing False

-- | 校验资源配置，零只在允许关闭限制时有效
validateServerConfig :: ServerConfig -> Either String ()
validateServerConfig config = do
    mapM_
        check
        [ ("worker_threads", scWorkerThreads config, 1, 256)
        , ("max_concurrent_queries", scMaxConcurrentQueries config, 0, 1000000)
        , ("max_connections", scMaxConnections config, 0, 1000000)
        , ("connection_idle", scConnectionIdle config, 0, 31536000)
        , ("transaction_idle", scTransactionIdle config, 0, 31536000)
        , ("transaction_max", scTransactionMax config, 0, 31536000)
        ]
    if scLocalRootLogin config && isNothing (scSocketPath config)
        then Left "server.local_root_login requires server.socket"
        else Right ()
  where
    -- \| 拒绝越界配置，避免误关资源上限
    check (key, value, low, high)
        | value >= low && value <= high = Right ()
        | otherwise = Left ("server." ++ key ++ " must be between " ++ show low ++ " and " ++ show high)

-- | 读 [server] 段键，缺的用默认
loadServerConfigAt :: FilePath -> IO ServerConfig
loadServerConfigAt path = do
    values <- readSection path "server"
    let
        -- \| 取一个键的非空字符串值
        textOf key fallback = case Map.lookup key values of
            Just value | not (T.null (T.strip value)) -> T.unpack (T.strip value)
            _ -> fallback
        -- \| 取一个键的整数值
        intOf key fallback = fromMaybe fallback (Map.lookup key values >>= readMaybe . T.unpack . T.strip)
        -- \| 取一个键的布尔值；只认 true 与 false
        boolOf key = case fmap (T.toLower . T.strip) (Map.lookup key values) of
            Nothing -> Right False
            Just "true" -> Right True
            Just "false" -> Right False
            Just _ -> Left ("server." ++ T.unpack key ++ " needs true or false")
        -- \| 取一个键的可选路径
        pathOf key = case Map.lookup key values of
            Just value | not (T.null (T.strip value)) -> Just (T.unpack (T.strip value))
            _ -> Nothing
        -- \| 已填写的资源数值必须是有效整数
        resourceInt key fallback = case Map.lookup key values of
            Nothing -> Right fallback
            Just value -> maybe (Left ("server." ++ T.unpack key ++ " needs an integer")) Right (readMaybe (T.unpack (T.strip value)))
        config = do
            localRoot <- boolOf "local_root_login"
            workers <- resourceInt "worker_threads" (scWorkerThreads defaultServerConfig)
            concurrent <- resourceInt "max_concurrent_queries" 0
            connections <- resourceInt "max_connections" 0
            idle <- resourceInt "connection_idle" 0
            transactionIdle <- resourceInt "transaction_idle" 0
            transactionMax <- resourceInt "transaction_max" 0
            let result =
                    defaultServerConfig
                        { scHost = textOf "listen_host" (scHost defaultServerConfig)
                        , scPort = intOf "port" (scPort defaultServerConfig)
                        , scMaxMessage = intOf "max_message" (scMaxMessage defaultServerConfig)
                        , scMaxRows = intOf "max_rows" (scMaxRows defaultServerConfig)
                        , scWorkerThreads = workers
                        , scMaxConcurrentQueries = concurrent
                        , scMaxConnections = connections
                        , scConnectionIdle = idle
                        , scTransactionIdle = transactionIdle
                        , scTransactionMax = transactionMax
                        , scSocketPath = pathOf "socket"
                        , scLocalRootLogin = localRoot
                        }
            validateServerConfig result
            Right result
    either (ioError . userError) pure config
