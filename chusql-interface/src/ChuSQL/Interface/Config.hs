{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Interface.Config (
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
    resolvePlainPassword,
    ServerConfig (..),
    defaultServerConfig,
    loadServerConfigAt,
) where

import ChuSQL.Interface.Auth (Credential (..), hashPassword)
import ChuSQL.Interface.TOML (readSection)
import Data.Char (toLower)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist)
import Text.Read (readMaybe)

-- Web 与 server 的运行参数：命令行 > chusql.toml 分区 > 内置默认。

-- | 默认账号名
defaultUser :: Text
defaultUser = "root"

-- | 默认口令（只为方便本地测试；生产/对外必须覆盖）
defaultPassword :: Text
defaultPassword = "chusql"

-- | Web 管理端配置
data WebConfig = WebConfig
    { wcHost :: String
    , wcPort :: Int
    , wcStaticDir :: FilePath
    , wcUser :: Text
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

-- | Web 管理端参数的内置默认值
defaultWebConfig :: WebConfig
defaultWebConfig =
    WebConfig
        { wcHost = "127.0.0.1"
        , wcPort = 7778
        , wcStaticDir = "static"
        , wcUser = ""
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

-- | 把设置键统一成连字符写法，重名时连字符优先
canonicalSettingKeys :: Map.Map Text Text -> Map.Map Text Text
canonicalSettingKeys saved =
    foldr (\(key, value) acc -> Map.insert (canonicalKey key) value acc) Map.empty (Map.toList saved)
  where
    -- | 取一个键的规范名
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
    ]

-- | 这份配置用的还是内置默认口令吗
usingDefaultCredentials :: Map.Map Text Text -> Bool
usingDefaultCredentials saved = Map.notMember "password" saved

-- | 设置文件里某个键的非空值
nonEmptyValue :: Text -> Map.Map Text Text -> Maybe Text
-- | 查一个键
nonEmptyValue key saved = case T.strip <$> Map.lookup key saved of
    Just value | not (T.null value) -> Just value
    _ -> Nothing

-- | root 凭据：明文就哈希，留空免密，缺省用演示口令
resolveCredential :: WebConfig -> Map.Map Text Text -> IO Credential
resolveCredential cfg saved = do
    encoded <- case Map.lookup "password" saved of
        Just plain | not (T.null (T.strip plain)) -> hashPassword (T.strip plain)
        Just _ -> pure ""
        Nothing -> hashPassword defaultPassword
    pure (Credential (rootUserName cfg saved) encoded)

-- | 管理员名字：命令行 > 设置文件 > 内置默认
rootUserName :: WebConfig -> Map.Map Text Text -> Text
rootUserName cfg saved
    | not (T.null (T.strip (wcUser cfg))) = T.strip (wcUser cfg)
    | otherwise = fromMaybe defaultUser (nonEmptyValue "user" saved)

-- | 读配置：默认叠上 [web] 与 [storage]
loadWebConfigAt :: FilePath -> IO WebConfig
loadWebConfigAt path = do
    raw <- readSection path "web"
    storage <- readSection path "storage"
    pure (withStorage storage (webConfigFromSection (canonicalSettingKeys raw)))

-- | 数据目录取自 [storage] 段
withStorage :: Map.Map Text Text -> WebConfig -> WebConfig
withStorage storage cfg =
    cfg{wcDataDir = T.unpack <$> nonEmptyValue "data_dir" storage}

-- | 管理员明文口令，没配回落到演示口令，显式留空回空串
resolvePlainPassword :: Map.Map Text Text -> Text
resolvePlainPassword saved = case Map.lookup "password" saved of
    Just plain -> T.strip plain
    Nothing -> defaultPassword

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
    -- | 逐个试
    go [] = pure Nothing
    go (d : ds) = do
        ok <- doesDirectoryExist d
        if ok then pure (Just d) else go ds

-- | 服务器监听参数
data ServerConfig = ServerConfig
    { scHost :: String
    , scPort :: Int
    , scMaxMessage :: Int
    , scMaxRows :: Int
    }
    deriving (Eq, Show)

-- | 默认本机 7777，行 1 MiB，最多 1000 行
defaultServerConfig :: ServerConfig
defaultServerConfig = ServerConfig "127.0.0.1" 7777 (1024 * 1024) 1000

-- | 读 [server] 段键，缺的用默认
loadServerConfigAt :: FilePath -> IO ServerConfig
loadServerConfigAt path = do
    values <- readSection path "server"
    let -- | 取一个键的非空字符串值
        textOf key fallback = case Map.lookup key values of
            Just value | not (T.null (T.strip value)) -> T.unpack (T.strip value)
            _ -> fallback
        -- | 取一个键的整数值
        intOf key fallback = case Map.lookup key values >>= readMaybe . T.unpack . T.strip of
            Just number -> number
            Nothing -> fallback
    pure
        defaultServerConfig
            { scHost = textOf "host" (scHost defaultServerConfig)
            , scPort = intOf "port" (scPort defaultServerConfig)
            , scMaxMessage = intOf "max_message" (scMaxMessage defaultServerConfig)
            , scMaxRows = intOf "max_rows" (scMaxRows defaultServerConfig)
            }
