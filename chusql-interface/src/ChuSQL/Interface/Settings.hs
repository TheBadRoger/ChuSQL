{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Interface.Settings (
    SettingItem (..),
    settingCatalogue,
    findItem,
    isRootOnly,
    isLockedSetting,
    isRestartRequired,
    readSettingsFile,
    writeSettingsFile,
    applySettings,
    liveKeys,
    defaultOf,
    effectiveSettings,
) where

import ChuSQL.Interface.Config (WebConfig (..), canonicalSettingKeys)
import ChuSQL.Interface.TOML (readSection, writeSection)
import Data.List (nub)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T

-- 设置目录，以及全局 settings.toml 的读、写与校验。

-- | 一个可配置项
data SettingItem = SettingItem
    { siKey :: Text
    , siLabel :: Text
    , siGroup :: Text
    , siKind :: Text
    , siDefault :: Text
    , siRootOnly :: Bool
    , siRestart :: Bool
    , siSection :: Text
    , siTomlKey :: Text
    }
    deriving (Show, Eq)

-- | 全部可配置项与界面顺序
settingCatalogue :: [SettingItem]
settingCatalogue =
    [ item "port" "HTTP port" "server" "int" "7778" True True
    , withTomlKey "listen_host" (item "listen-host" "Listen address" "server" "text" "127.0.0.1" True True)
    , item "cookie-secure" "Secure cookie (behind TLS)" "server" "bool" "0" True False
    , item "body-limit" "Max request body (bytes)" "limits" "int" "65536" False False
    , item "session-idle" "Session idle timeout (s)" "limits" "int" "28800" False False
    , item "session-max" "Session max age (s)" "limits" "int" "86400" False False
    , item "login-max-attempts" "Failed logins before lockout" "limits" "int" "5" False False
    , item "login-window" "Lockout window (s)" "limits" "int" "300" False False
    , item "rows-per-page" "Rows per page" "limits" "int" "25" False False
    , item "max-page-size" "Rows per page ceiling" "limits" "int" "500" False False
    , item "max-rows" "Rows per query" "limits" "int" "1000" False False
    , item "max-sql-length" "Max SQL characters" "limits" "int" "20000" False False
    , item "password-min-length" "口令最短长度" "auth" "int" "12" True False
    , item "password-classes" "口令字符类别数" "auth" "int" "2" True False
    , owned "data-dir" "Data directory" "storage" "text" "" "storage" "data_dir"
    , owned "log-files" "Log directory" "storage" "text" "./logs" "storage" "log_files"
    , owned "storage-page-size" "Storage page size" "storage" "int" "4096" "page" "size"
    , owned "storage-btree-order" "Storage B+tree order" "storage" "int" "4" "btree" "order"
    , owned "storage-buffer-pool" "Storage buffer pool pages" "storage" "int" "1024" "buffer" "pool_size"
    , owned "storage-log" "Storage log level" "storage" "text" "info" "log" "level"
    , item "seed" "Seed demo data on start" "storage" "bool" "1" True True
    , owned "tcp-listen-host" "TCP listen address" "network" "text" "127.0.0.1" "server" "listen_host"
    , owned "tcp-port" "TCP port" "network" "int" "7777" "server" "port"
    , owned "tcp-max-message" "TCP max request line (bytes)" "network" "int" "1048576" "server" "max_message"
    , owned "tcp-max-rows" "TCP rows per result" "network" "int" "1000" "server" "max_rows"
    ]
  where
    -- | web 自己的键：分区写死 [web]
    item k l g kind def rootOnly restart =
        SettingItem
            { siKey = k
            , siLabel = l
            , siGroup = g
            , siKind = kind
            , siDefault = def
            , siRootOnly = rootOnly
            , siRestart = restart
            , siSection = webSection
            , siTomlKey = k
            }
    -- 存储层的键：只读自己那段
    owned k l g kind def section tomlKey =
        SettingItem
            { siKey = k
            , siLabel = l
            , siGroup = g
            , siKind = kind
            , siDefault = def
            , siRootOnly = True
            , siRestart = True
            , siSection = section
            , siTomlKey = tomlKey
            }

    -- | 换掉落盘用的键名：界面键与 TOML 键不同名时用
    withTomlKey key spec = spec{siTomlKey = key}

-- | web 自己的分区名
webSection :: Text
webSection = "web"

-- | 目录里涉及的分区，去重
catalogueSections :: [Text]
catalogueSections = nub [siSection i | i <- settingCatalogue]

-- | 按 key 找一项
findItem :: Text -> Maybe SettingItem
findItem key = case [i | i <- settingCatalogue, siKey i == key] of
    (i : _) -> Just i
    [] -> Nothing

-- | 这项是不是重要参数（只有配置账号能改）
isRootOnly :: Text -> Bool
isRootOnly key = maybe False siRootOnly (findItem key)

-- | 网页端不能改的键（当前没有）
lockedKeys :: [Text]
lockedKeys = []

-- | 这项在界面上是不是只读
isLockedSetting :: Text -> Bool
isLockedSetting = (`elem` lockedKeys)

-- | 这项是不是要重启才生效
isRestartRequired :: Text -> Bool
isRestartRequired key = maybe False siRestart (findItem key)

-- | 改完立刻生效的配置键
liveKeys :: [Text]
liveKeys =
    [ "password-min-length", "password-classes"
    , "body-limit"
    , "session-idle"
    , "session-max"
    , "login-max-attempts"
    , "login-window"
    , "rows-per-page"
    , "max-page-size"
    , "max-rows"
    , "max-sql-length"
    ]

-- | 读目录里用到的每个分区，摊成界面键
readSettingsFile :: FilePath -> IO (Map.Map Text Text)
readSettingsFile path = fmap Map.unions (mapM readSectionOf catalogueSections)
  where
    -- | 读一个分区，摊成界面键
    readSectionOf section = do
        raw <- readSection path section
        let values = if section == webSection then canonicalSettingKeys raw else raw
        pure
            ( Map.fromList
                [ (siKey i, value)
                | i <- settingCatalogue
                , siSection i == section
                , Just value <- [Map.lookup (siTomlKey i) values]
                ]
            )

-- | 按分区写回，别的分区与注释原样保留
writeSettingsFile :: FilePath -> Map.Map Text Text -> IO (Either String ())
writeSettingsFile path values = writeSections catalogueSections
  where
    -- | 各分区依次写回
    writeSections [] = pure (Right ())
    writeSections (section : rest) = do
        let entries =
                Map.fromList
                    [ (siTomlKey i, value)
                    | i <- settingCatalogue
                    , siSection i == section
                    , Just value <- [Map.lookup (siKey i) values]
                    ]
        if Map.null entries
            then writeSections rest
            else do
                written <- writeSection path section entries
                case written of
                    Left err -> pure (Left err)
                    Right () -> writeSections rest

-- | 把界面传来的值合并进现有内容，只认目录里的键
applySettings :: Map.Map Text Text -> Map.Map Text Text -> Either String (Map.Map Text Text)
applySettings current = go (Right current) . Map.toList
  where
    -- | 逐项校验并合并
    go (Left e) _ = Left e
    go (Right acc) [] = Right acc
    go (Right acc) ((key, value) : rest) = case findItem key of
        Nothing -> Left ("unknown setting: " ++ T.unpack key)
        Just spec -> case checkKind spec value of
            Left e -> Left e
            Right () -> go (Right (Map.insert key value acc)) rest

-- | 按类型校验一个值，空串一律放行
checkKind :: SettingItem -> Text -> Either String ()
checkKind spec value
    | T.null (T.strip value) = Right ()
    | Just (lo, hi) <- lookup (siKey spec) [("password-min-length", (8, 256)), ("password-classes", (1, 4))] =
        case reads (T.unpack (T.strip value)) :: [(Int, String)] of
            [(n, "")] | n >= lo && n <= hi -> Right ()
            _ -> Left (T.unpack (siKey spec) ++ " is outside the permitted range")
    | siKind spec == "int" = case reads (T.unpack (T.strip value)) :: [(Int, String)] of
        [(_, "")] -> Right ()
        _ -> Left (T.unpack (siKey spec) ++ " needs an integer, got: " ++ T.unpack value)
    | siKind spec == "bool" = case T.toLower (T.strip value) of
        v | v `elem` ["0", "1", "true", "false", "yes", "no", "on", "off"] -> Right ()
        _ -> Left (T.unpack (siKey spec) ++ " needs a boolean, got: " ++ T.unpack value)
    | otherwise = Right ()

-- | 一项的内置默认值
defaultOf :: Text -> Text
defaultOf key = maybe "" siDefault (findItem key)

-- | 本进程启动时生效的配置，摊成键值对
effectiveSettings :: WebConfig -> Map.Map Text Text
effectiveSettings cfg =
    Map.fromList
        [ ("port", tshow (wcPort cfg))
        , ("listen-host", T.pack (wcHost cfg))
        , ("cookie-secure", boolText (wcCookieSecure cfg))
        , ("body-limit", tshow (wcBodyLimit cfg))
        , ("session-idle", tshow (wcSessionIdle cfg))
        , ("session-max", tshow (wcSessionMax cfg))
        , ("login-max-attempts", tshow (wcLoginMaxAttempts cfg))
        , ("login-window", tshow (wcLoginWindow cfg))
        , ("rows-per-page", tshow (wcPageSize cfg))
        , ("max-page-size", tshow (wcMaxPageSize cfg))
        , ("max-rows", tshow (wcMaxRows cfg))
        , ("max-sql-length", tshow (wcMaxSqlLength cfg))
        , ("seed", boolText (wcSeedDemo cfg))
        , ("data-dir", maybe "" T.pack (wcDataDir cfg))
        , ("log-files", T.pack (wcLogFiles cfg))
        ]
  where
    -- | 数值转文本
    tshow = T.pack . show
    -- | 布尔转 "1"/"0"
    boolText True = "1"
    boolText False = "0"
