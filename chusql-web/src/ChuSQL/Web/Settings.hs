{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.Settings (
    SettingItem (..),
    settingCatalogue,
    findItem,
    isRootOnly,
    isRestartRequired,
    settingsFileCandidates,
    resolveSettingsFile,
    readSettingsFile,
    writeSettingsFile,
    applySettings,
    liveKeys,
    defaultOf,
    effectiveSettings,
) where

import ChuSQL.Web.Config (WebConfig (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Control.Exception (IOException, try)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory, (</>))

-- Web 配置项目录，与启动脚本共用一份设置文件，支持热改与重启生效。

data SettingItem = SettingItem
    { siKey :: Text
    , siLabel :: Text
    , siGroup :: Text
    , siKind :: Text
    , siEnv :: Text
    , siDefault :: Text
    , siRootOnly :: Bool
    , siRestart :: Bool
    }
    deriving (Show, Eq)

-- | 全部可配置项（顺序即界面里的顺序）
settingCatalogue :: [SettingItem]
settingCatalogue =
    [ item "port" "HTTP port" "server" "int" "CHUSQL_WEB_PORT" "7777" True True
    , item "host" "Listen address" "server" "text" "CHUSQL_WEB_HOST" "127.0.0.1" True True
    , item "static-dir" "Static directory" "server" "text" "CHUSQL_WEB_STATIC" "static" True True
    , item "cookie-secure" "Secure cookie (behind TLS)" "server" "bool" "CHUSQL_WEB_COOKIE_SECURE" "0" True False
    , item "body-limit" "Max request body (bytes)" "limits" "int" "CHUSQL_WEB_BODY_LIMIT" "65536" False False
    , item "session-idle" "Session idle timeout (s)" "limits" "int" "CHUSQL_WEB_SESSION_IDLE" "28800" False False
    , item "session-max" "Session max age (s)" "limits" "int" "CHUSQL_WEB_SESSION_MAX" "86400" False False
    , item "login-max-attempts" "Failed logins before lockout" "limits" "int" "CHUSQL_WEB_LOGIN_MAX_ATTEMPTS" "5" False False
    , item "login-window" "Lockout window (s)" "limits" "int" "CHUSQL_WEB_LOGIN_WINDOW" "300" False False
    , item "rows-per-page" "Rows per page" "limits" "int" "CHUSQL_WEB_PAGE_SIZE" "25" False False
    , item "max-page-size" "Rows per page ceiling" "limits" "int" "CHUSQL_WEB_MAX_PAGE_SIZE" "500" False False
    , item "max-rows" "Rows per query" "limits" "int" "CHUSQL_WEB_MAX_ROWS" "1000" False False
    , item "max-sql-length" "Max SQL characters" "limits" "int" "CHUSQL_WEB_MAX_SQL_LENGTH" "20000" False False
    , item "user" "Account name" "auth" "text" "CHUSQL_WEB_USER" "root" True True
    , item "password" "Password (plain, hashed at start)" "auth" "secret" "CHUSQL_WEB_PASSWORD" "" True True
    , item "password-hash" "Password hash (pbkdf2)" "auth" "secret" "CHUSQL_WEB_PASSWORD_HASH" "" True True
    , item "data-dir" "Data directory" "storage" "text" "CHUSQL_DATA_DIR" "" True True
    , item "pipe-name" "Named pipe" "storage" "text" "CHUSQL_PIPE" "" True True
    , item "storage-page-size" "Storage page size" "storage" "int" "CHUSQL_PAGE_SIZE" "4096" True True
    , item "storage-btree-order" "Storage B+tree order" "storage" "int" "CHUSQL_BTREE_ORDER" "4" True True
    , item "storage-buffer-pool" "Storage buffer pool pages" "storage" "int" "CHUSQL_BUFFER_POOL_SIZE" "1024" True True
    , item "storage-log" "Storage log level" "storage" "text" "CHUSQL_LOG" "info" True True
    , item "seed" "Seed demo data on start" "storage" "bool" "CHUSQL_WEB_SEED" "1" True True
    ]
  where
    item k l g kind env def rootOnly restart =
        SettingItem
            { siKey = k
            , siLabel = l
            , siGroup = g
            , siKind = kind
            , siEnv = env
            , siDefault = def
            , siRootOnly = rootOnly
            , siRestart = restart
            }

-- | 按 key 找一项
findItem :: Text -> Maybe SettingItem
findItem key = case [i | i <- settingCatalogue, siKey i == key] of
    (i : _) -> Just i
    [] -> Nothing

-- | 这项是不是重要参数（只有配置账号能改）
isRootOnly :: Text -> Bool
isRootOnly key = maybe False siRootOnly (findItem key)

-- | 这项是不是要重启才生效
isRestartRequired :: Text -> Bool
isRestartRequired key = maybe False siRestart (findItem key)

-- | 改完立刻生效的配置键
liveKeys :: [Text]
liveKeys =
    [ "body-limit"
    , "session-idle"
    , "session-max"
    , "login-max-attempts"
    , "login-window"
    , "rows-per-page"
    , "max-page-size"
    , "max-rows"
    , "max-sql-length"
    ]

-- | 设置文件的候选位置
settingsFileCandidates :: [FilePath]
settingsFileCandidates =
    [ "script" </> "chusql.settings.json"
    , ".." </> "script" </> "chusql.settings.json"
    , "chusql.settings.json"
    ]

-- | 找现成的设置文件，否则挑目录已存在的候选
resolveSettingsFile :: IO FilePath
resolveSettingsFile = do
    found <- firstExistingFile settingsFileCandidates
    case found of
        Just path -> pure path
        Nothing -> do
            ready <- firstExistingDir settingsFileCandidates
            pure (maybe (headOr settingsFileCandidates "chusql.settings.json") id ready)
  where
    firstExistingFile [] = pure Nothing
    firstExistingFile (p : ps) = do
        ok <- doesFileExist p
        if ok then pure (Just p) else firstExistingFile ps

    -- | 候选所在目录是否已存在
    firstExistingDir [] = pure Nothing
    firstExistingDir (p : ps) = do
        let dir = takeDirectory p
        ok <- if null dir then pure True else doesDirectoryExist dir
        if ok then pure (Just p) else firstExistingDir ps

-- | 候选列表里的第一个
headOr :: [a] -> a -> a
headOr (x : _) _ = x
headOr [] d = d

-- | 读设置文件：键 -> 字符串值（文件不存在或坏掉都当空）
readSettingsFile :: FilePath -> IO (Map.Map Text Text)
readSettingsFile path = do
    exists <- doesFileExist path
    if not exists
        then pure Map.empty
        else do
            raw <- readFileUtf8 path
            pure $ case A.eitherDecodeStrict' raw of
                Left _ -> Map.empty
                Right (A.Object o) -> Map.fromList [(K.toText k, jsonToText v) | (k, v) <- KM.toList o]
                Right _ -> Map.empty
  where
    -- | JSON 值当字符串看
    jsonToText v = case v of
        A.String s -> s
        A.Number n -> T.pack (show n)
        A.Bool b -> if b then "1" else "0"
        _ -> ""

-- | 读文件（UTF-8 字节读，避免依赖系统编码）
readFileUtf8 :: FilePath -> IO BS.ByteString
readFileUtf8 = BS.readFile

-- | 写设置文件，只写非空值
writeSettingsFile :: FilePath -> Map.Map Text Text -> IO (Either String ())
writeSettingsFile path values = do
    let dir = takeDirectory path
    prepared <- try (ensureDir dir) :: IO (Either IOException ())
    case prepared of
        Left e -> pure (Left ("cannot create the settings directory " ++ dir ++ ": " ++ show e))
        Right () -> do
            let encoded =
                    A.encode
                        ( A.Object
                            ( KM.fromList
                                [ (K.fromText k, A.String v)
                                | (k, v) <- Map.toList values
                                , not (T.null v)
                                ]
                            )
                        )
            written <- try (BS.writeFile path (BL.toStrict encoded)) :: IO (Either IOException ())
            pure (either (Left . \e -> "cannot write " ++ path ++ ": " ++ show e) Right written)
  where
    ensureDir dir = if null dir then pure () else createDirectoryIfMissing True dir

-- | 把界面上传来的值合并进现有文件内容（只接受目录里有的键）
applySettings :: Map.Map Text Text -> Map.Map Text Text -> Either String (Map.Map Text Text)
applySettings current = go (Right current) . Map.toList
  where
    go (Left e) _ = Left e
    go (Right acc) [] = Right acc
    go (Right acc) ((key, value) : rest) = case findItem key of
        Nothing -> Left ("unknown setting: " ++ T.unpack key)
        Just spec -> case checkKind spec value of
            Left e -> Left e
            Right () -> go (Right (Map.insert key value acc)) rest

-- | 按类型校验一个值（空串表示"清掉"，一律放行）
checkKind :: SettingItem -> Text -> Either String ()
checkKind spec value
    | T.null (T.strip value) = Right ()
    | siKind spec == "int" = case reads (T.unpack (T.strip value)) :: [(Int, String)] of
        [(_, "")] -> Right ()
        _ -> Left (T.unpack (siKey spec) ++ " needs an integer, got: " ++ T.unpack value)
    | siKind spec == "bool" = case T.toLower (T.strip value) of
        v | v `elem` ["0", "1", "true", "false", "yes", "no", "on", "off"] -> Right ()
        _ -> Left (T.unpack (siKey spec) ++ " needs a boolean, got: " ++ T.unpack value)
    | otherwise = Right ()

-- | 一项的内置默认值（设置被清掉、也没有别处给值时用它）
defaultOf :: Text -> Text
defaultOf key = maybe "" siDefault (findItem key)

-- | 本进程启动时生效的配置，摊成键值对
effectiveSettings :: WebConfig -> Map.Map Text Text
effectiveSettings cfg =
    Map.fromList
        [ ("port", tshow (wcPort cfg))
        , ("host", T.pack (wcHost cfg))
        , ("static-dir", T.pack (wcStaticDir cfg))
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
        , ("user", wcUser cfg)
        , ("seed", boolText (wcSeedDemo cfg))
        , ("data-dir", maybe "" T.pack (wcDataDir cfg))
        , ("pipe-name", maybe "" T.pack (wcPipeName cfg))
        ]
  where
    tshow = T.pack . show
    boolText True = "1"
    boolText False = "0"
