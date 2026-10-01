{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.Config (
    WebConfig (..),
    defaultWebConfig,
    defaultUser,
    defaultPassword,
    usingDefaultCredentials,
    loadWebConfigAt,
    resolveCredential,
    resolveStaticDir,
    staticDirCandidates,
    rootUserName,
    canonicalSettingKeys,
    resolvePipeName,
) where

import ChuSQL.Storage.IPC (defaultPipeName)
import ChuSQL.Web.Auth (Credential (..), hashPassword)
import ChuSQL.Web.TOML (readSection)
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import Text.Read (readMaybe)

-- 运行参数：命令行 > 全局 chusql.toml 的分区 > 内置默认，不看环境变量。
-- 配置文件位置是固定的（安装时定下，见 ChuSQL.Web.TOML.defaultConfigFile），
-- 想读别处的文件只能在命令行用 --config 指。
-- 自己那段是 [web]；管名与数据目录取自 [server] pipe_name / [storage] data_dir，
-- 因为它们是存储进程的事实，前端只当端点读。root 凭据只认 [web] 里的明文。

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
        , wcUser = ""
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

-- | [web] 分区里的键统一成连字符写法（scripts/chusql.toml 用的是下划线）
--   两种写法同时出现时，连字符（设置页写的规范形式）优先。
canonicalSettingKeys :: Map.Map Text Text -> Map.Map Text Text
canonicalSettingKeys saved =
    foldr (\(key, value) acc -> Map.insert (canonicalKey key) value acc) Map.empty (Map.toList saved)
  where
    canonicalKey key = fromMaybe key (lookup key keyAliases)

-- | 下划线写法到连字符写法的别名
keyAliases :: [(Text, Text)]
keyAliases =
    [ ("static_dir", "static-dir")
    , ("session_idle", "session-idle")
    , ("session_max", "session-max")
    , ("login_max_attempts", "login-max-attempts")
    , ("login_window", "login-window")
    , ("body_limit", "body-limit")
    , ("rows_per_page", "rows-per-page")
    , ("max_page_size", "max-page-size")
    , ("max_rows", "max-rows")
    , ("max_sql_length", "max-sql-length")
    , ("password_min_length", "password-min-length")
    , ("password_classes", "password-classes")
    , ("storage_server", "storage-server")
    ]

-- | 这份配置用的还是内置默认口令吗。
--   显式留空（password = ""）是"只允许管理员免密登录"，算已经配置过。
usingDefaultCredentials :: Map.Map Text Text -> Bool
usingDefaultCredentials saved = Map.notMember "password" saved

-- | 设置文件里某个键的非空值
nonEmptyValue :: Text -> Map.Map Text Text -> Maybe Text
nonEmptyValue key saved = case T.strip <$> Map.lookup key saved of
    Just value | not (T.null value) -> Just value
    _ -> Nothing

-- | root 凭据只认设置文件里的明文：给了明文就哈希它；显式留空就是免密（空编码串）；
--   没配这一项才回落到内置演示口令。
resolveCredential :: WebConfig -> Map.Map Text Text -> IO Credential
resolveCredential cfg saved = do
    encoded <- case Map.lookup "password" saved of
        Just plain | not (T.null (T.strip plain)) -> hashPassword (T.strip plain)
        Just _ -> pure ""
        Nothing -> hashPassword defaultPassword
    pure (Credential (rootUserName cfg saved) encoded)

-- | 管理员名字：命令行 --user > 设置文件 user > 内置默认
rootUserName :: WebConfig -> Map.Map Text Text -> Text
rootUserName cfg saved
    | not (T.null (T.strip (wcUser cfg))) = T.strip (wcUser cfg)
    | otherwise = fromMaybe defaultUser (nonEmptyValue "user" saved)

-- | 读配置：内置默认 < [web] 分区（端点取自 [server] / [storage]）。
--   user / password 不在这里读：它们由 rootUserName / resolveCredential 直接看设置文件。
loadWebConfigAt :: FilePath -> IO WebConfig
loadWebConfigAt path = do
    raw <- readSection path "web"
    server <- readSection path "server"
    storage <- readSection path "storage"
    pure (withEndpoint server storage (webConfigFromSection (canonicalSettingKeys raw)))

-- | 端点：存储进程 bind 的管名与它用的数据目录，前端只读不改
withEndpoint :: Map.Map Text Text -> Map.Map Text Text -> WebConfig -> WebConfig
withEndpoint server storage cfg =
    cfg
        { wcPipeName = T.unpack <$> nonEmptyValue "pipe_name" server
        , wcDataDir = T.unpack <$> nonEmptyValue "data_dir" storage
        }

-- | 实际要连的管名：配置里没有就用与存储层一致的默认值
resolvePipeName :: WebConfig -> String
resolvePipeName cfg = fromMaybe defaultPipeName (wcPipeName cfg)

-- | [web] 分区里的启动参数，缺失或坏值一律回到内置默认
webConfigFromSection :: Map.Map Text Text -> WebConfig
webConfigFromSection saved =
    defaultWebConfig
        { wcHost = T.unpack (sectionText "host" saved (T.pack (wcHost defaultWebConfig)))
        , wcPort = sectionInt "port" saved (wcPort defaultWebConfig)
        , wcStaticDir = T.unpack (sectionText "static-dir" saved (T.pack (wcStaticDir defaultWebConfig)))
        , wcCookieSecure = sectionBool "cookie-secure" saved (wcCookieSecure defaultWebConfig)
        , wcBodyLimit = sectionInt "body-limit" saved (wcBodyLimit defaultWebConfig)
        , wcSessionIdle = sectionInt "session-idle" saved (wcSessionIdle defaultWebConfig)
        , wcSessionMax = sectionInt "session-max" saved (wcSessionMax defaultWebConfig)
        , wcLoginMaxAttempts = sectionInt "login-max-attempts" saved (wcLoginMaxAttempts defaultWebConfig)
        , wcLoginWindow = sectionInt "login-window" saved (wcLoginWindow defaultWebConfig)
        , wcPageSize = sectionInt "rows-per-page" saved (wcPageSize defaultWebConfig)
        , wcMaxPageSize = sectionInt "max-page-size" saved (wcMaxPageSize defaultWebConfig)
        , wcMaxRows = sectionInt "max-rows" saved (wcMaxRows defaultWebConfig)
        , wcMaxSqlLength = sectionInt "max-sql-length" saved (wcMaxSqlLength defaultWebConfig)
        , wcSeedDemo = sectionBool "seed" saved (wcSeedDemo defaultWebConfig)
        , wcStorageServer = T.unpack <$> nonEmptyValue "storage-server" saved
        }

-- | 分区里一个键的非空字符串值
sectionText :: Text -> Map.Map Text Text -> Text -> Text
sectionText key saved def = fromMaybe def (nonEmptyValue key saved)

-- | 分区里的整数，解析不出来就用内置默认
sectionInt :: Text -> Map.Map Text Text -> Int -> Int
sectionInt key saved def = fromMaybe def (nonEmptyValue key saved >>= readMaybe . T.unpack)

-- | 分区里的布尔（1/true/yes/on 都算真）
sectionBool :: Text -> Map.Map Text Text -> Bool -> Bool
sectionBool key saved def = case nonEmptyValue key saved of
    Nothing -> def
    Just value -> map toLower (T.unpack value) `elem` ["1", "true", "yes", "on"]

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
