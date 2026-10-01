{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.API (
    AppEnv (..),
    Live (..),
    defaultLive,
    setLive,
    readLive,
    applyLiveKeys,
    configurePasswordPolicy,
    webApp,
    sessionCookieName,
    parseCookieHeader,
    isIdentifier,
    renderRow,
    tableInfoJson,
    systemTableInfo,
    apiErrorJson,
    newAppEnv,
    newAppEnvAt,
) where

import ChuSQL.Model (Row, Value (..))
import ChuSQL.Semantic (check)
import ChuSQL.Storage.IPC (Account (..), SchemaColumn (..), TableInfo (..))
import ChuSQL.Syntax.AST (Statement (..))
import ChuSQL.Syntax.Parser (parseStatement)
import ChuSQL.Web.Accounts
import ChuSQL.Web.Actions (
    ColumnSpec (..),
    CreateTableSpec (..),
    coerceValue,
    columnTypeOf,
    createDatabaseSql,
    createIndexSql,
    createTableSql,
    deleteRowSql,
    dropColumnSql,
    dropDatabaseSql,
    dropIndexSql,
    dropTableSql,
    insertRowSql,
    isIdentifier,
    selectRowsSql,
    updateRowSql,
 )
import ChuSQL.Web.Auth (
    Credential (..),
    SessionPolicy (..),
    SessionStore,
    deleteSession,
    setSessionPolicy,
    sessionToken,
 )
import ChuSQL.Web.Backend (Backend, StatementResult (..), beCatalog, bePing, beStatement, beWithDatabase, beDatabases, statementNeedsDatabase)
import ChuSQL.Web.Demo (SeedReport (..), seedDemo)
import ChuSQL.Web.Privileges (
    Grant (..),
    PrivilegeCommand (..),
    PrivilegeError (..),
    Privileges,
    RoleView (..),
    authorize,
    authorizeTables,
    filterTables,
    listRoleViews,
    newPrivileges,
    privilegeCommand,
    runPrivilegeCommand,
 )
import ChuSQL.Web.RateLimit (RateLimiter, rateLimitBlock, rateLimitClear, rateLimitRecord, setRateLimit)
import ChuSQL.Web.Secure (bodyLimitDynamic, sameOriginOnly, securityHeaders, contentSecurityPolicy)
import ChuSQL.Web.Settings (
    SettingItem (..),
    applySettings,
    defaultOf,
    isLockedSetting,
    liveKeys,
    readSettingsFile,
    settingCatalogue,
    writeSettingsFile,
 )
import ChuSQL.Web.Static (contentTypeOf, readStatic, safeRelative)
import ChuSQL.Web.TOML (defaultConfigFile)
import ChuSQL.Web.UISettings (
    readUISettings,
    resolveUISettingsFile,
    uiSettingsFileCandidates,
    validateUISettings,
    writeUISettings,
 )
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON (..), eitherDecode, object, withObject, (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Network.HTTP.Types (status204, status400, status401, status403, status404, status409, status413, status429, status500)
import Network.Wai (Application)
import Text.Read (readMaybe)
import Web.Scotty

-- REST API 与页面交付：鉴权、JSON 与引擎结果的互转、错误到状态码的映射。

data Live = Live
    { lvBodyLimit :: Int
    , lvSessionIdle :: Int
    , lvSessionMax :: Int
    , lvLoginMaxAttempts :: Int
    , lvLoginWindow :: Int
    , lvPageSize :: Int
    , lvMaxPageSize :: Int
    , lvMaxRows :: Int
    , lvMaxSqlLength :: Int
    }
    deriving (Show, Eq)

-- | 热改项的默认值，与配置默认一致
defaultLive :: Live
defaultLive =
    Live
        { lvBodyLimit = 65536
        , lvSessionIdle = 8 * 3600
        , lvSessionMax = 24 * 3600
        , lvLoginMaxAttempts = 5
        , lvLoginWindow = 5 * 60
        , lvPageSize = 25
        , lvMaxPageSize = 500
        , lvMaxRows = 1000
        , lvMaxSqlLength = 20000
        }

data AppEnv = AppEnv
    { aeBackend :: Backend
    , aeSessions :: SessionStore
    , aeAccounts :: Accounts
    , aePrivileges :: Privileges
    , aeLimiter :: RateLimiter
    , aeStaticDir :: FilePath
    , aeCookieSecure :: Bool
    , aeLive :: IORef Live
    , aeSettingsFile :: FilePath
    , aeUISettingsFile :: FilePath
    , aeEffective :: Map.Map Text Text
    , aeDatabase :: Text
    , aeLog :: Text -> IO ()
    }

-- | 会话 Cookie 名
sessionCookieName :: Text
sessionCookieName = "chusql_session"

-- | 参数齐全地造一份环境（各项上限用内置默认，调用方按配置改）；配置文件用固定路径
newAppEnv :: Backend -> SessionStore -> Credential -> RateLimiter -> FilePath -> IO AppEnv
newAppEnv backend sessions cred limiter staticDir = do
    settingsFile <- resolveSettingsFileSafe
    newAppEnvAt settingsFile backend sessions cred limiter staticDir

-- | 同上，但明说读写哪份配置文件（--config 走这里，保证读写的和启动读的是同一份）
newAppEnvAt :: FilePath -> Backend -> SessionStore -> Credential -> RateLimiter -> FilePath -> IO AppEnv
newAppEnvAt settingsFile backend sessions cred limiter staticDir = do
    accounts <- newAccounts backend sessions cred
    privileges <- newPrivileges backend
    liveRef <- newIORef defaultLive
    configurePasswordPolicy accounts settingsFile
    uiSettingsFile <- resolveUISettingsFileSafe
    pure
        AppEnv
            { aeBackend = backend
            , aeSessions = sessions
            , aeAccounts = accounts
            , aePrivileges = privileges
            , aeLimiter = limiter
            , aeStaticDir = staticDir
            , aeCookieSecure = False
            , aeLive = liveRef
            , aeSettingsFile = settingsFile
            , aeUISettingsFile = uiSettingsFile
            , aeEffective = Map.empty
            , aeDatabase = ""
            , aeLog = const (pure ())
            }

-- | 系统库：账号表住在这里，其中的表都是系统表，只有管理员能进。
systemDatabase :: Text
systemDatabase = "system"

-- | 保留库：只有系统库删不掉、也建不了。
reservedDatabase :: Text -> Bool
reservedDatabase name = name == systemDatabase

configurePasswordPolicy :: Accounts -> FilePath -> IO ()
configurePasswordPolicy accounts path = do
    saved <- readSettingsFile path
    let values = Map.fromList [(key, Map.findWithDefault (defaultOf key) key saved) | key <- passwordKeys]
    case applySettings Map.empty values of
        Left err -> ioError (userError err)
        Right valid -> setAccountsPolicy accounts (policyFromValues defaultPasswordPolicy valid)

passwordKeys :: [Text]
passwordKeys = ["password-min-length", "password-classes"]

policyFromValues :: PasswordPolicy -> Map.Map Text Text -> PasswordPolicy
policyFromValues before values = PasswordPolicy
    (number "password-min-length" (ppMinLength before)) (number "password-classes" (ppClasses before))
  where
    number key fallback = case Map.lookup key values of
        Nothing -> fallback
        Just value -> fromMaybe fallback (readMaybe (T.unpack (if T.null value then defaultOf key else value)))

-- | 配置文件位置是固定的，这里只兜住「拿不到用户目录」的极端情况
resolveSettingsFileSafe :: IO FilePath
resolveSettingsFileSafe = do
    found <- try defaultConfigFile :: IO (Either IOException FilePath)
    pure (either (const "chusql.toml") id found)

-- | 定位 IDE 设置文件，找不到就定下首个候选路径
resolveUISettingsFileSafe :: IO FilePath
resolveUISettingsFileSafe = do
    found <- try resolveUISettingsFile :: IO (Either IOException FilePath)
    pure (either (const fallbackUISettingsFile) id found)
  where
    fallbackUISettingsFile = case uiSettingsFileCandidates of
        (p : _) -> p
        [] -> "chusql.ui.settings.json"

-- | 换一组热改参数（起服务前按配置文件初始化用）
setLive :: AppEnv -> Live -> IO ()
setLive env = writeIORef (aeLive env)

-- | 现在生效的热改参数
readLive :: AppEnv -> IO Live
readLive = readIORef . aeLive

-- | WAI 中间件用的实时请求体上限
liveBodyLimit :: AppEnv -> IO Int
liveBodyLimit env = lvBodyLimit <$> readLive env

-- | 组装 WAI 应用：Scotty 路由外面套三层中间件
webApp :: AppEnv -> IO Application
webApp env = do
    inner <- scottyApp (routes env)
    pure (securityHeaders (sameOriginOnly (bodyLimitDynamic (liveBodyLimit env) inner)))

-- | 路由表：界面一键操作全走结构化接口
routes :: AppEnv -> ScottyM ()
routes env = do
    get "/" (indexH env)
    get "/static/:file" (staticH env)
    get "/api/health" healthH
    get "/api/status" (statusH env)
    post "/api/login" (loginH env)
    post "/api/logout" (logoutH env)
    get "/api/session" (sessionH env)
    get "/api/settings" (settingsH env)
    put "/api/settings" (putSettingsH env)
    get "/api/ui-settings" (uiSettingsH env)
    put "/api/ui-settings" (putUISettingsH env)
    get "/api/databases" (databasesH env)
    post "/api/databases" (withDatabase env createDatabaseH)
    delete "/api/databases/:name" (withDatabase env dropDatabaseH)
    get "/api/tables" (withDatabase env tablesH)
    post "/api/tables" (withDatabase env createTableH)
    delete "/api/tables/:t" (withDatabase env dropTableH)
    get "/api/tables/:t" (withDatabase env tableH)
    get "/api/tables/:t/rows" (withDatabase env rowsH)
    post "/api/tables/:t/rows" (withDatabase env insertRowH)
    patch "/api/tables/:t/rows/:id" (withDatabase env updateRowH)
    delete "/api/tables/:t/rows/:id" (withDatabase env deleteRowH)
    post "/api/tables/:t/indexes" (withDatabase env createIndexH)
    delete "/api/tables/:t/indexes/:col" (withDatabase env dropIndexH)
    delete "/api/tables/:t/columns/:col" (withDatabase env dropColumnH)
    post "/api/demo-data" (withDatabase env demoDataH)
    get "/api/roles" (withDatabase env rolesH)
    post "/api/roles" (withDatabase env createRoleH)
    delete "/api/roles/:name" (withDatabase env dropRoleH)
    post "/api/roles/:name/grants" (withDatabase env grantRoleH)
    delete "/api/roles/:name/grants" (withDatabase env revokeRoleH)
    post "/api/roles/:name/members" (withDatabase env addRoleMemberH)
    delete "/api/roles/:name/members/:user" (withDatabase env removeRoleMemberH)
    post "/api/query" (withDatabase env queryH)
    notFound (notFoundH env)

-- | A request carries its own immutable database context; shared IPC never stores USE state.
--   没带 `X-ChuSQL-Database` 就是"没选库"：绑定空库，需要库的接口自己报 no database selected。
withDatabase :: AppEnv -> (AppEnv -> ActionM ()) -> ActionM ()
withDatabase env action = do
    principal <- requirePrincipal env
    chosen <- maybe "" (T.toLower . TL.toStrict) <$> header "X-ChuSQL-Database"
    if T.null chosen
        then action env{aeBackend = beWithDatabase (aeBackend env) "", aeDatabase = ""}
        else
            if not (isIdentifier chosen) || T.length chosen > 64
                then reply400 "bad_request" "invalid database name"
                else
                    if chosen == systemDatabase && not (principalIsRoot principal)
                        then reply403 "the system database is only available to the administrator"
                        else action env {aeBackend = beWithDatabase (aeBackend env) (T.unpack chosen), aeDatabase = chosen}

-- | 需要当前库的接口先过这一关（裸表名/建表都要先选库）。
--   走 accountError 而不是 reply400：它会 finish，否则 handler 还会往下跑并覆盖这个错误。
requireDatabase :: AppEnv -> ActionM ()
requireDatabase env =
    if T.null (aeDatabase env)
        then accountError (AccountError "no_database" "no database selected")
        else pure ()

-- | 数据库清单；系统库 system 只给管理员看。
databasesH :: AppEnv -> ActionM ()
databasesH env = do
    principal <- requirePrincipal env
    result <- liftIO (beDatabases (aeBackend env))
    case result of
        Left e -> engineError e
        Right names -> json (if principalIsRoot principal then names else filter (/= T.unpack systemDatabase) names)

-- | 新建数据库：名字过标识符白名单，真正建库交给存储层。
createDatabaseH :: AppEnv -> ActionM ()
createDatabaseH env = do
    requireAdmin env
    withJsonObject env $ \o -> case textField "name" o of
        Left err -> reply400 "bad_request" err
        Right name -> case createDatabaseSql name of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite env sql

-- | 删除数据库；系统库 system 一律拒绝。
dropDatabaseH :: AppEnv -> ActionM ()
dropDatabaseH env = do
    requireAdmin env
    name <- pathParam "name"
    if reservedDatabase name
        then reply400 "bad_request" "the system database cannot be dropped"
        else case dropDatabaseSql name of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite env sql

-- | 首页：登录与管理界面同一个 HTML
indexH :: AppEnv -> ActionM ()
indexH env = do
    content <- liftIO (readStatic (aeStaticDir env) "index.html")
    case content of
        Nothing -> do
            status status500
            json (apiErrorJson "missing_static" "index.html not found (check the --static directory)")
        Just page -> do
            nonce <- liftIO sessionToken
            let (beforeHead, afterHead) = BS.breakSubstring "</head>" page
                nonceMeta = TE.encodeUtf8 ("<meta name=\"chusql-style-nonce\" content=\"" <> nonce <> "\">")
                pageHtml = beforeHead <> nonceMeta <> afterHead
            setHeader "Content-Security-Policy" (TL.fromStrict (contentSecurityPolicy (Just nonce)))
            setHeader "Content-Type" "text/html; charset=utf-8"
            raw (BL.fromStrict pageHtml)
-- | 静态文件：白名单之外一律 404
staticH :: AppEnv -> ActionM ()
staticH env = do
    rel <- pathParam "file"
    if rel == "index.html" then indexH env else case safeRelative rel of
        Nothing -> reply404
        Just _ -> do
            content <- liftIO (readStatic (aeStaticDir env) (T.unpack rel))
            case content of
                Nothing -> reply404
                Just page -> do
                    setHeader "Content-Type" (TL.fromStrict (contentTypeOf (T.unpack rel)))
                    raw (BL.fromStrict page)

-- | 统一的 JSON 404
reply404 :: ActionM ()
reply404 = do
    status status404
    json (apiErrorJson "not_found" "no such resource")

-- | 兜底：任何没匹配上的路径
notFoundH :: AppEnv -> ActionM ()
notFoundH _ = reply404

-- | 只回答"进程在"，不碰存储
healthH :: ActionM ()
healthH = json (object ["status" .= ("ok" :: Text)])

-- | 进程 + 存储：启动脚本靠这个判断存储接上了没
statusH :: AppEnv -> ActionM ()
statusH env = do
    up <- liftIO (bePing (aeBackend env))
    json (object ["status" .= ("ok" :: Text), "storage" .= (if up then "up" else "down" :: Text)])

-- | 登录：对了发 Cookie，错了 401/429
loginH :: AppEnv -> ActionM ()
loginH env = do
    rawBody <- body
    limit <- liftIO (liveBodyLimit env)
    if BL.length rawBody > fromIntegral limit
        then reply413
        else case eitherDecode rawBody of
            Left _ -> reply400 "bad_request" "request body is not valid JSON"
            Right reqBody -> do
                let user = T.strip (lrUser reqBody)
                    password = lrPassword reqBody
                -- 口令允许为空：配置里口令留空时管理员就是免密登录
                if T.null user || T.length user > 64 || T.length password > 256
                    then reply400 "bad_request" "user name is empty or too long"
                    else do
                        blocked <- liftIO (rateLimitBlock (aeLimiter env) user)
                        case blocked of
                            Just left -> do
                                setHeader "Retry-After" (TL.fromStrict (T.pack (show (ceiling left :: Int))))
                                status status429
                                json (apiErrorJson "too_many_attempts" "too many failed sign-in attempts, try again later")
                            Nothing -> do
                                live <- liftIO (readLive env)
                                result <- liftIO (authenticate (aeAccounts env) user password)
                                case result of
                                    Right token -> do
                                        liftIO (rateLimitClear (aeLimiter env) user)
                                        setHeader "Set-Cookie" (TL.fromStrict (sessionCookie env token (Just (lvSessionMax live))))
                                        liftIO (aeLog env ("sign-in ok user=" <> user))
                                        status status204
                                    Left err@(AccountError code _) | code /= "unauthorized" -> accountError err
                                    Left _ -> do
                                        liftIO (rateLimitRecord (aeLimiter env) user)
                                        liftIO (aeLog env ("sign-in failed user=" <> user))
                                        status status401
                                        json (apiErrorJson "unauthorized" "invalid user name or password")

-- | 退出：删服务端会话 + 让浏览器丢 Cookie
logoutH :: AppEnv -> ActionM ()
logoutH env = do
    token <- requestSessionToken
    case token of
        Nothing -> pure ()
        Just t -> liftIO (deleteSession (aeSessions env) t)
    setHeader "Set-Cookie" (TL.fromStrict (sessionCookie env "" (Just 0)))
    status status204

-- | 当前登录的是谁
sessionH :: AppEnv -> ActionM ()
sessionH env = do
    principal <- requirePrincipal env
    policy <- liftIO (accountsPolicy (aeAccounts env))
    json (object ["user" .= principalName principal, "administrator" .= principalIsRoot principal,
        "policy" .= object ["minLength" .= ppMinLength policy, "classes" .= ppClasses policy]])

-- | 读设置：文件值、启动生效值与内置默认一起给出
settingsH :: AppEnv -> ActionM ()
settingsH env = do
    principal <- requirePrincipal env
    fileValues <- liftIO (readSettingsFile (aeSettingsFile env))
    let owner = principalIsRoot principal
    json
        ( object
            [ "file" .= aeSettingsFile env
            , "account" .= principalName principal
            , "owner" .= owner
            , "items" .= map (itemJson (aeEffective env) fileValues owner) settingCatalogue
            ]
        )

-- | 一个配置项在界面上的样子
itemJson :: Map.Map Text Text -> Map.Map Text Text -> Bool -> SettingItem -> A.Value
itemJson effectiveValues fileValues owner spec =
    object
        [ "key" .= siKey spec
        , "label" .= siLabel spec
        , "group" .= siGroup spec
        , "kind" .= siKind spec
        , "default" .= siDefault spec
        , "value" .= shownValue
        , "source" .= source
        , "rootOnly" .= siRootOnly spec
        , "editable" .= (not (isLockedSetting (siKey spec)) && (owner || not (siRootOnly spec)))
        , "locked" .= isLockedSetting (siKey spec)
        , "restart" .= siRestart spec
        , "live" .= (siKey spec `elem` liveKeys)
        , "set" .= hasValue
        ]
  where
    secret = siKind spec == "secret"
    fromFile = Map.lookup (siKey spec) fileValues
    fromProcess = Map.lookup (siKey spec) effectiveValues
    effectiveValue = fromMaybe (defaultOf (siKey spec)) fromProcess
    shownValue = if secret then "" else fromMaybe effectiveValue fromFile
    hasValue = fromFile /= Nothing
    source :: Text
    source
        | secret = if hasValue then "configured" else "not set"
        | fromFile /= Nothing = "settings file"
        | otherwise = "default"

-- | 改设置：写配置文件并让可热改项立即生效
putSettingsH :: AppEnv -> ActionM ()
putSettingsH env = do
    user <- requireUser env
    withJsonObject env $ \o -> case settingsUpdates o of
        Left err -> reply400 "bad_request" err
        Right updates -> do
            principal <- requirePrincipal env
            let locked = [k | k <- Map.keys updates, isLockedSetting k]
                forbidden =
                    [ k
                    | k <- Map.keys updates
                    , maybe False siRootOnly (findSetting k)
                    , not (principalIsRoot principal)
                    ]
            if not (null locked)
                then reply403 ("administrator credentials live in the settings file and cannot be changed here: " <> T.intercalate ", " locked)
                else
                    if not (null forbidden)
                        then reply403 ("administrator required to change: " <> T.intercalate ", " forbidden)
                        else do
                            current <- liftIO (readSettingsFile (aeSettingsFile env))
                            case applySettings current updates of
                                Left err -> reply400 "bad_request" (T.pack err)
                                Right merged -> do
                                    written <- liftIO (writeSettingsFile (aeSettingsFile env) merged)
                                    case written of
                                        Left err -> reply500 (T.pack err)
                                        Right () -> do
                                            applied <- liftIO (applyLiveKeys env updates)
                                            let needsRestart = [k | k <- Map.keys updates, not (k `elem` applied), not (T.null (Map.findWithDefault "" k updates))]
                                            liftIO (aeLog env ("settings updated by " <> user <> ": " <> T.intercalate ", " (Map.keys updates)))
                                            json
                                                ( object
                                                    [ "ok" .= True
                                                    , "file" .= aeSettingsFile env
                                                    , "applied" .= applied
                                                    , "restartRequired" .= needsRestart
                                                    ]
                                                )

-- | 读前端 IDE 设置（放在自己的文件里，与服务配置无关）
uiSettingsH :: AppEnv -> ActionM ()
uiSettingsH env = do
    _ <- requireUser env
    value <- liftIO (readUISettings (aeUISettingsFile env))
    json value

-- | 整份覆盖前端 IDE 设置，校验不过一律 400
putUISettingsH :: AppEnv -> ActionM ()
putUISettingsH env = do
    _ <- requireUser env
    rawBody <- body
    limit <- liftIO (liveBodyLimit env)
    if BL.length rawBody > fromIntegral limit
        then reply413
        else case eitherDecode rawBody of
            Left _ -> reply400 "bad_request" "request body is not valid JSON"
            Right payload -> case validateUISettings payload of
                Left err -> reply400 "bad_request" (T.pack err)
                Right valid -> do
                    written <- liftIO (writeUISettings (aeUISettingsFile env) valid)
                    case written of
                        Left err -> reply500 (T.pack err)
                        Right () -> status status204

-- | 账号表的名字：网页里像普通表一样浏览与编辑，但写入走账号命令
accountTableName :: String
accountTableName = "__chusql_users"

-- | 这张表是不是账号表
isAccountTable :: Text -> Bool
isAccountTable name = T.toLower name == T.pack accountTableName

-- | 账号表只住在系统库 system 里：在别的库上点名它一律 400。
accountRoute :: AppEnv -> Text -> ActionM Bool
accountRoute env name
    | not (isAccountTable name) = pure False
    | aeDatabase env /= systemDatabase = do
        reply400 "bad_request" "the account table lives in the system database"
        finish
    | otherwise = pure True

-- | 口令列：密文只在服务端产生，界面不支持排序与筛选
passwordColumn :: String
passwordColumn = "password"

-- | 账号表的列：id 主键、用户名唯一索引、口令密文、注册时间与最近登录时间
accountColumns :: [SchemaColumn]
accountColumns =
    [ accountColumnSpec "id" "int" False True True False
    , accountColumnSpec "user" "varchar(64)" False False False True
    , accountColumnSpec passwordColumn "varchar(256)" False False False False
    , accountColumnSpec "registered_at" "timestamp" False False False False
    , accountColumnSpec "last_login_at" "timestamp" True False False False
    ]

-- | 账号表的列（约束由账号服务管）
accountColumnSpec :: String -> String -> Bool -> Bool -> Bool -> Bool -> SchemaColumn
accountColumnSpec name ty nullable autoIncrement primary unique =
    SchemaColumn name ty nullable Nothing autoIncrement primary unique Nothing

-- | 账号行：平铺的各列
accountRowsOf :: [Account] -> [Row]
accountRowsOf = map accountRow
  where
    accountRow a =
        [ ("id", VInt (fromIntegral (accountId a)))
        , ("user", VStr (T.unpack (accountUser a)))
        , (passwordColumn, VStr (T.unpack (accountHash a)))
        , ("registered_at", VStr (T.unpack (accountRegisteredAt a)))
        , ("last_login_at", maybe VNull (VStr . T.unpack) (accountLastLoginAt a))
        ]

-- | 账号表的线上信息（列与普通表同构）
systemTableJson :: Int -> A.Value
systemTableJson count =
    object
        [ "table" .= (accountTableName :: String)
        , "kind" .= ("system" :: Text)
        , "rowCount" .= count
        , "indexes" .= [object ["column" .= ("user" :: Text), "builtIn" .= False]]
        , "columns" .= map accountColumnJson accountColumns
        ]
  where
    accountColumnJson sc =
        object
            [ "name" .= scName sc
            , "type" .= scType sc
            , "primaryKey" .= scPrimaryKey sc
            , "indexed" .= (scPrimaryKey sc || scUnique sc)
            , "prime" .= (scPrimaryKey sc || scUnique sc)
            , "unique" .= scUnique sc
            , "nullable" .= scNullable sc
            , "distinct" .= (-1 :: Int)
            , "statsCapped" .= False
            , "secret" .= (scName sc == passwordColumn)
            ]

-- | 读一遍账号表，读不到就 500
requireAccounts :: AppEnv -> ActionM [Account]
requireAccounts env = do
    result <- liftIO (listAccounts (aeAccounts env))
    either accountError pure result

-- | 口令列不接受排序与筛选
rejectSecretColumn :: Maybe (Text, Bool) -> [(Text, Value)] -> Either Text ()
rejectSecretColumn order filters
    | Just (col, _) <- order, col == T.pack passwordColumn = Left "the password column cannot be sorted"
    | any ((== T.pack passwordColumn) . fst) filters = Left "the password column cannot be filtered"
    | otherwise = Right ()

-- | 只有管理员能用账号表
rootOnly :: Principal -> ActionM ()
rootOnly principal =
    if principalIsRoot principal
        then pure ()
        else accountError (AccountError "forbidden" "administrator required")

-- | 账号表的一页数据（排序与筛选都在内存里做）
baseRowsH :: AppEnv -> ActionM ()
baseRowsH env = do
    principal <- requirePrincipal env
    rootOnly principal
    live <- liftIO (readLive env)
    limit <- intParam "limit" (lvPageSize live) 1 (lvMaxPageSize live)
    offset <- intParam "offset" 0 0 1000000
    rawFilter <- (queryParamMaybe "filter" :: ActionM (Maybe Text))
    order <- sortParam systemTableInfo
    case (order, parseFilters systemTableInfo rawFilter) of
        (Left err, _) -> reply400 "bad_request" err
        (_, Left err) -> reply400 "bad_request" err
        (Right sortOrder, Right filters) -> case rejectSecretColumn sortOrder filters of
            Left err -> reply400 "bad_request" err
            Right () -> do
                accounts <- requireAccounts env
                let rows = filterAccountRows filters (sortAccountRows sortOrder (accountRowsOf accounts))
                    page = take limit (drop offset rows)
                    cols = map scName accountColumns
                    (sortColumn, ascending) = splitSort sortOrder
                json
                    ( object
                        [ "table" .= accountTableName
                        , "columns" .= cols
                        , "rows" .= map (renderRow cols) page
                        , "total" .= length rows
                        , "limit" .= limit
                        , "offset" .= offset
                        , "sort" .= maybe A.Null A.String sortColumn
                        , "dir" .= (if ascending then "asc" else "desc" :: Text)
                        , "filters" .= map fst filters
                        ]
                    )

-- | 账号表的结构（排序校验用）
systemTableInfo :: TableInfo
systemTableInfo = TableInfo accountTableName accountColumns 0 [] []

-- | 按用户名排序（其余列不支持排序）
sortAccountRows :: Maybe (Text, Bool) -> [Row] -> [Row]
sortAccountRows Nothing rows = rows
sortAccountRows (Just (column, ascending)) rows =
    sortBy compareUser rows
  where
    compareUser a b = case (accountCell a column, accountCell b column) of
        (Just x, Just y) -> if ascending then compare x y else compare y x
        _ -> EQ
    accountCell row name = case lookup (T.unpack name) row of
        Just VNull -> Just ""
        Just (VStr s) -> Just s
        Just (VInt n) -> Just (show n)
        Just (VFloat d) -> Just (show d)
        _ -> Nothing

-- | 按等值条件筛账号行
filterAccountRows :: [(Text, Value)] -> [Row] -> [Row]
filterAccountRows [] rows = rows
filterAccountRows filters rows = [r | r <- rows, all (matches r) filters]
  where
    matches row (column, wanted) = case lookup (T.unpack column) row of
        Just value -> value == wanted
        Nothing -> False

-- | 账号表写入：插一行 = CREATE USER
baseInsertH :: AppEnv -> ActionM ()
baseInsertH env = do
    principal <- requirePrincipal env
    rootOnly principal
    withJsonObject env $ \o -> case valuesField o >>= readRowValues False accountColumns of
        Left err -> reply400 "bad_request" err
        Right values -> case (lookupValue "user" values, lookupValue (T.pack passwordColumn) values) of
            (Just (VStr user), Just (VStr password)) -> runAccountWrite env principal (CreateAccount (T.pack user) (T.pack password))
            _ -> reply400 "bad_request" "user and password are required"

-- | 账号表写入：改一行 = ALTER USER（只改口令，不许改名）
baseUpdateH :: AppEnv -> Text -> ActionM ()
baseUpdateH env target = do
    principal <- requirePrincipal env
    rootOnly principal
    withJsonObject env $ \o -> case valuesField o >>= readRowValues False accountColumns of
        Left err -> reply400 "bad_request" err
        Right values -> case (values, lookupValue (T.pack passwordColumn) values) of
            ([(_, VStr password)], Just _) -> runAccountWrite env principal (ResetAccountPassword target (T.pack password))
            _ -> reply400 "bad_request" "only the password of an existing account can be changed"

-- | 账号表写入：删一行 = DROP USER
baseDeleteH :: AppEnv -> Text -> ActionM ()
baseDeleteH env target = do
    principal <- requirePrincipal env
    rootOnly principal
    runAccountWrite env principal (DropAccount target)

-- | 账号写入：SQL 控制台与基表行接口共用的执行路径
runAccountWrite :: AppEnv -> Principal -> AccountCommand -> ActionM ()
runAccountWrite env principal command = do
    result <- liftIO (runAccountCommand (aeAccounts env) principal command)
    case result of
        Left err -> accountError err
        Right () -> json (object ["ok" .= True])

-- | 取一个列值
lookupValue :: Text -> [(String, Value)] -> Maybe Value
lookupValue name values = lookup (T.unpack name) values

accountError :: AccountError -> ActionM a
accountError (AccountError code message) = do
    status $ case code of
        "unauthorized" -> status401
        "forbidden" -> status403
        "not_found" -> status404
        "conflict" -> status409
        "storage_error" -> status500
        _ -> status400
    json (apiErrorJson code message)
    finish

-- | 按 key 找目录项
findSetting :: Text -> Maybe SettingItem
findSetting key = case [i | i <- settingCatalogue, siKey i == key] of
    (i : _) -> Just i
    [] -> Nothing

-- | 把能热改的设置应用到当前进程
applyLiveKeys :: AppEnv -> Map.Map Text Text -> IO [Text]
applyLiveKeys env updates = do
    beforePolicy <- accountsPolicy (aeAccounts env)
    setAccountsPolicy (aeAccounts env) (policyFromValues beforePolicy updates)
    before <- readLive env
    let wanted = [k | k <- liveKeys, Map.member k updates]
        intOf key = case T.strip (Map.findWithDefault "" key updates) of
            "" -> intFromText (effectiveValueOf (aeEffective env) key)
            given -> intFromText given
        after =
            before
                { lvBodyLimit = intOf "body-limit"
                , lvSessionIdle = intOf "session-idle"
                , lvSessionMax = intOf "session-max"
                , lvLoginMaxAttempts = intOf "login-max-attempts"
                , lvLoginWindow = intOf "login-window"
                , lvPageSize = intOf "rows-per-page"
                , lvMaxPageSize = intOf "max-page-size"
                , lvMaxRows = intOf "max-rows"
                , lvMaxSqlLength = intOf "max-sql-length"
                }
    writeIORef (aeLive env) after
    setSessionPolicy
        (aeSessions env)
        (SessionPolicy (fromIntegral (lvSessionIdle after)) (fromIntegral (lvSessionMax after)))
    setRateLimit (aeLimiter env) (lvLoginMaxAttempts after) (fromIntegral (lvLoginWindow after))
    pure wanted

-- | 某项无配置时的实际生效值
effectiveValueOf :: Map.Map Text Text -> Text -> Text
effectiveValueOf effectiveValues key = fromMaybe (defaultOf key) (Map.lookup key effectiveValues)

-- | 字符串当整数看（坏值回落到 0，不让它把进程带崩）
intFromText :: Text -> Int
intFromText given = fromMaybe 0 (readMaybe (T.unpack (T.strip given)))

-- | 表清单（列 / 行数 / 索引 / 列统计）；普通身份只看得到自己有 SELECT 的表，管理员额外看到账号表
tablesH :: AppEnv -> ActionM ()
tablesH env = do
    requireDatabase env
    principal <- requirePrincipal env
    result <- liftIO (beCatalog (aeBackend env))
    case result of
        Left e -> engineError e
        Right infos -> do
            readable <- readableTables env principal infos
            extra <- if aeDatabase env == systemDatabase && principalIsRoot principal
                then (: []) . systemTableJson . length <$> requireAccounts env
                else pure []
            json (map tableInfoJson readable ++ extra)

-- | 普通身份的表清单按 SELECT 权限过滤
readableTables :: AppEnv -> Principal -> [TableInfo] -> ActionM [TableInfo]
readableTables env principal infos
    | principalIsRoot principal = pure infos
    | otherwise = do
        result <- liftIO (filterTables (aePrivileges env) principal (aeDatabase env) (map (T.pack . tiTable) infos))
        case result of
            Left err -> privilegeError err
            Right allowed -> pure (filter (\info -> T.pack (tiTable info) `elem` allowed) infos)

-- | 单表结构
tableH :: AppEnv -> ActionM ()
tableH env = do
    requireDatabase env
    principal <- requirePrincipal env
    name <- pathParam "t"
    account <- accountRoute env name
    if account
        then do
            rootOnly principal
            accounts <- requireAccounts env
            json (systemTableJson (length accounts))
        else
            if not (isIdentifier name)
                then reply400 "bad_request" "invalid table name"
                else do
                    requirePrivilege env [(name, "select")]
                    result <- liftIO (beCatalog (aeBackend env))
                    case result of
                        Left e -> engineError e
                        Right infos -> case findTable name infos of
                            Nothing -> reply404
                            Just info -> json (tableInfoJson info)

-- | 分页浏览一张表，支持排序与等值过滤；账号表走内存里的账号行
rowsH :: AppEnv -> ActionM ()
rowsH env = do
    requireDatabase env
    name <- pathParam "t"
    account <- accountRoute env name
    if account
        then baseRowsH env
        else do
            requirePrivilege env [(name, "select")]
            if not (isIdentifier name)
                then reply400 "bad_request" "invalid table name"
                else do
                    catalogResult <- liftIO (beCatalog (aeBackend env))
                    case catalogResult of
                        Left e -> engineError e
                        Right infos -> case findTable name infos of
                            Nothing -> reply404
                            Just info -> do
                                live <- liftIO (readLive env)
                                limit <- intParam "limit" (lvPageSize live) 1 (lvMaxPageSize live)
                                offset <- intParam "offset" 0 0 1000000
                                rawFilter <- (queryParamMaybe "filter" :: ActionM (Maybe Text))
                                order <- sortParam info
                                case (order, parseFilters info rawFilter) of
                                    (Left err, _) -> reply400 "bad_request" err
                                    (_, Left err) -> reply400 "bad_request" err
                                    (Right sortOrder, Right filters) -> case selectRowsSql name sortOrder filters of
                                        Left err -> reply400 "bad_request" (T.pack err)
                                        Right sql -> do
                                            result <- liftIO (beStatement (aeBackend env) sql)
                                            case result of
                                                Left e -> engineError e
                                                Right res -> do
                                                    let cols = map scName (tiColumns info)
                                                        page = take limit (drop offset (srRows res))
                                                        (sortColumn, ascending) = splitSort sortOrder
                                                    json
                                                        ( object
                                                            [ "table" .= tiTable info
                                                            , "columns" .= cols
                                                            , "rows" .= map (renderRow cols) page
                                                            , "total" .= length (srRows res)
                                                            , "limit" .= limit
                                                            , "offset" .= offset
                                                            , "sort" .= maybe A.Null A.String sortColumn
                                                            , "dir" .= (if ascending then "asc" else "desc" :: Text)
                                                            , "filters" .= map fst filters
                                                            ]
                                                        )

data RowFilter = RowFilter Text Text

instance FromJSON RowFilter where
    parseJSON = withObject "RowFilter" $ \o -> RowFilter <$> o .: "column" <*> o .: "value"

-- | 解析 filter 参数，非法一律 400
parseFilters :: TableInfo -> Maybe Text -> Either Text [(Text, Value)]
parseFilters _ Nothing = Right []
parseFilters info (Just rawFilter)
    | T.null (T.strip rawFilter) = Right []
    | otherwise = case eitherDecode (BL.fromStrict (TE.encodeUtf8 rawFilter)) of
        Left _ -> Left "filter must be a JSON array of {column, value}"
        Right specs -> catMaybes <$> mapM toPair specs
  where
    toPair (RowFilter column rawValue)
        | T.null rawValue = Right Nothing
        | otherwise = do
            schema <- maybe (Left ("unknown column: " <> column)) Right (lookupColumn column)
            ty <- maybe (Left ("unsupported column type for: " <> column)) Right (columnTypeOf (T.pack (scType schema)))
            value <- either (Left . T.pack) Right (coerceValue ty (A.String rawValue))
            pure (Just (column, value))
    lookupColumn column = case [c | c <- tiColumns info, T.pack (scName c) == column] of
        (c : _) -> Just c
        [] -> Nothing

-- | 排序键拆开（没给排序时给 Nothing / asc）
splitSort :: Maybe (Text, Bool) -> (Maybe Text, Bool)
splitSort Nothing = (Nothing, True)
splitSort (Just (col, asc)) = (Just col, asc)

-- | 读 sort/dir 参数并校验列存在
sortParam :: TableInfo -> ActionM (Either Text (Maybe (Text, Bool)))
sortParam info = do
    rawSort <- (queryParamMaybe "sort" :: ActionM (Maybe Text))
    rawDir <- (queryParamMaybe "dir" :: ActionM (Maybe Text))
    pure $ case rawSort of
        Nothing -> Right Nothing
        Just column
            | not (isIdentifier column) -> Left "sort must be a plain column name"
            | column `notElem` map (T.pack . scName) (tiColumns info) -> Left ("unknown column: " <> column)
            | otherwise -> Right (Just (column, dirOf rawDir))
  where
    dirOf given = case fmap (T.toLower . T.strip) given of
        Just "desc" -> False
        _ -> True

-- | 建表：按列结构拼 CREATE TABLE
createTableH :: AppEnv -> ActionM ()
createTableH env = do
    requireDatabase env
    requireAdmin env
    withJsonObject env $ \o -> case decodeCreateTable o of
        Left err -> reply400 "bad_request" err
        Right spec -> case createTableSql spec of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite env sql

-- | 删表；账号表不许删
dropTableH :: AppEnv -> ActionM ()
dropTableH env = do
    requireDatabase env
    requireAdmin env
    name <- pathParam "t"
    account <- accountRoute env name
    if account
        then reply400 "bad_request" "the account table cannot be dropped"
        else case dropTableSql name of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite env sql

-- | 插一行，所有列必须给全；账号表上等于 CREATE USER
insertRowH :: AppEnv -> ActionM ()
insertRowH env = do
    requireDatabase env
    name <- pathParam "t"
    account <- accountRoute env name
    if account
        then baseInsertH env
        else do
            requirePrivilege env [(name, "insert")]
            info <- requireTable env name
            withJsonObject env $ \o -> case valuesField o >>= readRowValues False (tiColumns info) of
                Left err -> reply400 "bad_request" err
                Right row -> case insertRowSql name row of
                    Left err -> reply400 "bad_request" (T.pack err)
                    Right sql -> runWrite env sql

-- | 改一行，只改给出来的列；账号表上等于 ALTER USER（只改口令）
updateRowH :: AppEnv -> ActionM ()
updateRowH env = do
    requireDatabase env
    name <- pathParam "t"
    account <- accountRoute env name
    if account
        then do
            target <- pathParam "id"
            baseUpdateH env target
        else do
            requirePrivilege env [(name, "update")]
            key <- rowIdParam
            info <- requireTable env name
            withJsonObject env $ \o -> case valuesField o >>= readRowValues False (tiColumns info) of
                Left err -> reply400 "bad_request" err
                Right assigns -> case updateRowSql name key assigns of
                    Left err -> reply400 "bad_request" (T.pack err)
                    Right sql -> runWrite env sql

-- | 删一行；账号表上等于 DROP USER
deleteRowH :: AppEnv -> ActionM ()
deleteRowH env = do
    requireDatabase env
    name <- pathParam "t"
    account <- accountRoute env name
    if account
        then do
            target <- pathParam "id"
            baseDeleteH env target
        else do
            requirePrivilege env [(name, "delete")]
            key <- rowIdParam
            _ <- requireTable env name
            case deleteRowSql name key of
                Left err -> reply400 "bad_request" (T.pack err)
                Right sql -> runWrite env sql

-- | 建索引，索引列必须整数且唯一；账号表没有索引可建
createIndexH :: AppEnv -> ActionM ()
createIndexH env = do
    requireDatabase env
    requireAdmin env
    name <- pathParam "t"
    account <- accountRoute env name
    if account
        then reply400 "bad_request" "the account table has no indexes"
        else do
            info <- requireTable env name
            withJsonObject env $ \o -> case textField "column" o of
                Left err -> reply400 "bad_request" err
                Right column -> case columnTypeOf (columnTypeText (tiColumns info) column) of
                    Nothing -> reply400 "bad_request" ("unknown column: " <> column)
                    Just _ -> case createIndexSql name column of
                        Left err -> reply400 "bad_request" (T.pack err)
                        Right sql -> runWrite env sql

-- | 删索引（内建索引不放行：存储层也会拒，这里先给一句清楚的话）
dropIndexH :: AppEnv -> ActionM ()
dropIndexH env = do
    requireDatabase env
    requireAdmin env
    name <- pathParam "t"
    column <- pathParam "col"
    account <- accountRoute env name
    if account
        then reply400 "bad_request" "the account table has no indexes"
        else do
            _ <- requireTable env name
            if column == T.pack builtInIndexColumn
                then reply400 "bad_request" "the built-in id index cannot be dropped"
                else case dropIndexSql name column of
                    Left err -> reply400 "bad_request" (T.pack err)
                    Right sql -> runWrite env sql

-- | 删列，内建 id 列不放行；账号表的列是固定的
dropColumnH :: AppEnv -> ActionM ()
dropColumnH env = do
    requireDatabase env
    requireAdmin env
    name <- pathParam "t"
    column <- pathParam "col"
    account <- accountRoute env name
    if account
        then reply400 "bad_request" "the account table has no columns to drop"
        else do
            _ <- requireTable env name
            if column == T.pack builtInIndexColumn
                then reply400 "bad_request" "the built-in id column cannot be dropped"
                else case dropColumnSql name column of
                    Left err -> reply400 "bad_request" (T.pack err)
                    Right sql -> runWrite env sql

-- | 一键灌演示数据（缺什么补什么，可以反复点）
demoDataH :: AppEnv -> ActionM ()
demoDataH env = do
    requireDatabase env
    requireAdmin env
    result <- liftIO (seedDemo (aeBackend env))
    case result of
        Left e -> reply500 (T.pack e)
        Right report ->
            json
                ( object
                    [ "ok" .= True
                    , "created" .= srCreated report
                    , "skipped" .= srSkipped report
                    , "indexes" .= srIndexes report
                    ]
                )

-- | 角色总览：只有管理员能看
rolesH :: AppEnv -> ActionM ()
rolesH env = do
    principal <- requirePrincipal env
    rootOnly principal
    result <- liftIO (listRoleViews (aePrivileges env))
    case result of
        Left err -> privilegeError err
        Right views -> json (map roleViewJson views)

roleViewJson :: RoleView -> A.Value
roleViewJson view =
    object
        [ "name" .= roleName view
        , "grants" .= map grantJson (roleGrants view)
        , "members" .= roleMembers view
        ]

grantJson :: Grant -> A.Value
grantJson grant =
    object
        [ "privilege" .= grantPrivilege grant
        , "object" .= grantObject grant
        ]

-- | 建角色
createRoleH :: AppEnv -> ActionM ()
createRoleH env = withJsonObject env $ \o -> case textField "name" o of
    Left err -> reply400 "bad_request" err
    Right name -> runPrivilegeWrite env (CreateRoleCommand name)

-- | 删角色（连带清掉它的授权与成员）
dropRoleH :: AppEnv -> ActionM ()
dropRoleH env = pathParam "name" >>= \name -> runPrivilegeWrite env (DropRoleCommand name)

-- | 给角色加权限
grantRoleH :: AppEnv -> ActionM ()
grantRoleH env = do
    role <- pathParam "name"
    withJsonObject env $ \o -> case grantFields o of
        Left err -> reply400 "bad_request" err
        Right (privileges, objectName) -> runPrivilegeWrite env (GrantPrivilegesCommand privileges objectName role)

-- | 收角色的权限
revokeRoleH :: AppEnv -> ActionM ()
revokeRoleH env = do
    role <- pathParam "name"
    withJsonObject env $ \o -> case grantFields o of
        Left err -> reply400 "bad_request" err
        Right (privileges, objectName) -> runPrivilegeWrite env (RevokePrivilegesCommand privileges objectName role)

-- | 把用户加进角色
addRoleMemberH :: AppEnv -> ActionM ()
addRoleMemberH env = do
    role <- pathParam "name"
    withJsonObject env $ \o -> case memberFields o of
        Left err -> reply400 "bad_request" err
        Right users -> runPrivilegeWrite env (GrantRoleCommand role users)

-- | 把用户移出角色
removeRoleMemberH :: AppEnv -> ActionM ()
removeRoleMemberH env = do
    role <- pathParam "name"
    user <- pathParam "user"
    runPrivilegeWrite env (RevokeRoleCommand role [user])

-- | 权限（一条或一串）与对象
grantFields :: A.Object -> Either Text ([Text], Text)
grantFields o = do
    objectName <- textField "object" o
    privileges <- case KM.lookup "privileges" o of
        Just (A.Array xs) -> mapM stringValue (V.toList xs)
        _ -> Left "missing or non-array field: privileges"
    if null privileges then Left "at least one privilege is required" else pure (privileges, objectName)

-- | 成员（一条或一串）
memberFields :: A.Object -> Either Text [Text]
memberFields o = case (KM.lookup "user" o, KM.lookup "users" o) of
    (Just (A.String one), _) -> Right [one]
    (_, Just (A.Array xs)) -> mapM stringValue (V.toList xs)
    _ -> Left "missing or non-string field: user"

stringValue :: A.Value -> Either Text Text
stringValue (A.String value) = Right value
stringValue _ = Left "values must be strings"

-- | 角色命令：只有管理员能跑
runPrivilegeWrite :: AppEnv -> PrivilegeCommand -> ActionM ()
runPrivilegeWrite env command = do
    principal <- requirePrincipal env
    result <- liftIO (runPrivilegeCommand (aePrivileges env) principal (aeDatabase env) command)
    case result of
        Left err -> privilegeError err
        Right () -> json (object ["ok" .= True])

-- | 权限错误与账号错误共用一套状态码映射
privilegeError :: PrivilegeError -> ActionM a
privilegeError (PrivilegeError code message) = accountError (AccountError code message)

-- | 一键接口的权限门：普通身份必须持有对应的 (表, 权限)
requirePrivilege :: AppEnv -> [(Text, Text)] -> ActionM ()
requirePrivilege env needed = do
    principal <- requirePrincipal env
    allowed <- liftIO (authorizeTables (aePrivileges env) principal (aeDatabase env) needed)
    either privilegeError pure allowed

-- | 管理员专属操作（建库 / 建表 / 索引 / 列 / 演示数据）
requireAdmin :: AppEnv -> ActionM ()
requireAdmin env = requirePrincipal env >>= rootOnly

-- | 写操作统一回执
runWrite :: AppEnv -> String -> ActionM ()
runWrite env sql = do
    result <- liftIO (beStatement (aeBackend env) sql)
    case result of
        Left e -> engineError e
        Right _ -> json (object ["ok" .= True])

-- | 只收 JSON 对象，超限 413，非法 400
withJsonObject :: AppEnv -> (A.Object -> ActionM ()) -> ActionM ()
withJsonObject env k = do
    rawBody <- body
    limit <- liftIO (liveBodyLimit env)
    if BL.length rawBody > fromIntegral limit
        then reply413
        else case eitherDecode rawBody of
            Left _ -> reply400 "bad_request" "request body is not valid JSON"
            Right (A.Object o) -> k o
            Right _ -> reply400 "bad_request" "request body must be a JSON object"

-- | 表必须存在，否则 400/404 短路
requireTable :: AppEnv -> Text -> ActionM TableInfo
requireTable env name
    | not (isIdentifier name) = do
        reply400 "bad_request" "invalid table name"
        finish
    | otherwise = do
        result <- liftIO (beCatalog (aeBackend env))
        case result of
            Left e -> engineError e
            Right infos -> case findTable name infos of
                Nothing -> do
                    reply404
                    finish
                Just info -> pure info

-- | 路径里的行 id
rowIdParam :: ActionM Int
rowIdParam = do
    rawId <- pathParam "id"
    case readMaybe (T.unpack rawId) :: Maybe Int of
        Just n -> pure n
        Nothing -> do
            reply400 "bad_request" "row id must be an integer"
            finish

-- | 取一个字符串字段
textField :: Text -> A.Object -> Either Text Text
textField key o = case KM.lookup (K.fromText key) o of
    Just (A.String s) -> Right s
    _ -> Left ("missing or non-string field: " <> key)

-- | 取 `values` 对象
valuesField :: A.Object -> Either Text A.Object
valuesField o = case KM.lookup "values" o of
    Just (A.Object v) -> Right v
    _ -> Left "missing or non-object field: values"

-- | 取设置请求里的 values，值当字符串
settingsUpdates :: A.Object -> Either Text (Map.Map Text Text)
settingsUpdates o = do
    values <- valuesField o
    if KM.null values
        then Left "no settings were given"
        else Map.fromList <$> mapM convert (KM.toList values)
  where
    convert (k, v) = case v of
        A.String s -> Right (K.toText k, s)
        A.Number n -> Right (K.toText k, T.pack (show n))
        A.Bool b -> Right (K.toText k, if b then "1" else "0")
        _ -> Left ("setting " <> K.toText k <> " must be a string, number or boolean")

-- | 建表请求体
decodeCreateTable :: A.Object -> Either Text CreateTableSpec
decodeCreateTable o = do
    name <- textField "name" o
    colsValue <- maybe (Left "missing field: columns") Right (KM.lookup "columns" o)
    cols <- case colsValue of
        A.Array xs -> mapM decodeColumn (V.toList xs)
        _ -> Left "columns must be an array"
    pure (CreateTableSpec name cols)
  where
    decodeColumn (A.Object c) = ColumnSpec <$> textField "name" c <*> textField "type" c
    decodeColumn _ = Left "each column must be {name, type}"

-- | 把请求里的列值按表结构转成引擎值
readRowValues :: Bool -> [SchemaColumn] -> A.Object -> Either Text [(String, Value)]
readRowValues requireAll cols o = do
    provided <- mapM convert (KM.toList o)
    if requireAll
        then do
            let missing = [scName c | c <- cols, scName c `notElem` map fst provided]
            if null missing
                then pure provided
                else Left ("missing columns: " <> T.intercalate ", " (map T.pack missing))
        else pure provided
  where
    convert (k, v) = do
        let name = K.toString k
        schema <- maybe (Left ("unknown column: " <> T.pack name)) Right (lookupSchema name)
        ty <- maybe (Left ("unsupported column type for: " <> T.pack name)) Right (columnTypeOf (T.pack (scType schema)))
        value <- either (Left . T.pack) Right (coerceValue ty v)
        pure (name, value)
    lookupSchema name = case [c | c <- cols, scName c == name] of
        (c : _) -> Just c
        [] -> Nothing

-- | 某列在数据字典里的类型字符串（找不到就给空串）
columnTypeText :: [SchemaColumn] -> Text -> Text
columnTypeText cols name = case [c | c <- cols, scName c == T.unpack name] of
    (c : _) -> T.pack (scType c)
    [] -> ""

-- | 执行任意一条语句：管理语句走各自的服务，其余先过权限门
queryH :: AppEnv -> ActionM ()
queryH env = do
    principal <- requirePrincipal env
    let user = principalName principal
    rawBody <- body
    live <- liftIO (readLive env)
    if BL.length rawBody > fromIntegral (lvBodyLimit live)
        then reply413
        else case eitherDecode rawBody of
            Left _ -> reply400 "bad_request" "request body is not valid JSON"
            Right reqBody -> do
                let sql = T.strip (qrSql reqBody)
                if T.null sql
                    then reply400 "bad_request" "sql must not be empty"
                    else
                        if T.length sql > lvMaxSqlLength live
                            then reply413
                            else do
                                liftIO (aeLog env ("query user=" <> user <> " sql=" <> sql))
                                case parseStatement (T.unpack sql) of
                                    Left _ -> executeSql env live sql
                                    Right stmt -> case accountCommand stmt of
                                        Just command -> runAccountSql env stmt command
                                        Nothing -> case privilegeCommand stmt of
                                            Just command -> runPrivilegeSql env stmt command
                                            Nothing -> do
                                                if T.null (aeDatabase env) && statementNeedsDatabase stmt
                                                    then reply400 "no_database" "no database selected"
                                                    else do
                                                        allowed <- liftIO (authorize (aePrivileges env) principal (aeDatabase env) stmt)
                                                        case allowed of
                                                            Left err -> privilegeError err
                                                            Right () -> executeSql env live sql

-- | 把 SQL 交给引擎，结果按控制台口径回执
executeSql :: AppEnv -> Live -> Text -> ActionM ()
executeSql env live sql = do
    result <- liftIO (beStatement (aeBackend env) (T.unpack sql))
    case result of
        Left e -> engineError e
        Right res -> do
            let shown = take (lvMaxRows live) (srRows res)
            json
                ( object
                    [ "columns" .= srColumns res
                    , "rows" .= map (renderRow (srColumns res)) shown
                    , "rowCount" .= length (srRows res)
                    , "truncated" .= (length (srRows res) > lvMaxRows live)
                    , "database" .= (case parseStatement (T.unpack sql) of
                        Right (UseDatabase name) -> Just (T.toLower (T.pack name))
                        _ -> Nothing :: Maybe Text)
                    ]
                )

-- | 执行账号管理语句：只有管理员能跑
runAccountSql :: AppEnv -> Statement -> AccountCommand -> ActionM ()
runAccountSql env stmt command = case check [] stmt of
    Left e -> engineError e
    Right () -> do
        principal <- requirePrincipal env
        result <- liftIO (runAccountCommand (aeAccounts env) principal command)
        case result of
            Left err -> accountError err
            Right () ->
                json
                    ( object
                        [ "columns" .= ([] :: [Text])
                        , "rows" .= ([] :: [A.Value])
                        , "rowCount" .= (0 :: Int)
                        , "truncated" .= False
                        ]
                    )

-- | 执行角色与授权语句：只有管理员能跑
runPrivilegeSql :: AppEnv -> Statement -> PrivilegeCommand -> ActionM ()
runPrivilegeSql env stmt command = case check [] stmt of
    Left e -> engineError e
    Right () -> do
        principal <- requirePrincipal env
        result <- liftIO (runPrivilegeCommand (aePrivileges env) principal (aeDatabase env) command)
        case result of
            Left err -> privilegeError err
            Right () ->
                json
                    ( object
                        [ "columns" .= ([] :: [Text])
                        , "rows" .= ([] :: [A.Value])
                        , "rowCount" .= (0 :: Int)
                        , "truncated" .= False
                        ]
                    )

-- | 拿当前 Session 用户；没有就 401
requireUser :: AppEnv -> ActionM Text
requireUser env = principalName <$> requirePrincipal env

-- | 当前身份；没有会话就 401
requirePrincipal :: AppEnv -> ActionM Principal
requirePrincipal env = do
    token <- requireToken
    found <- liftIO (currentPrincipal (aeAccounts env) token)
    either accountError pure found

requireToken :: ActionM Text
requireToken = requestSessionToken >>= maybe (accountError (AccountError "unauthorized" "sign in first")) pure

-- | 请求里的会话令牌
requestSessionToken :: ActionM (Maybe Text)
requestSessionToken = do
    rawHeader <- header "Cookie"
    pure (fmap TL.toStrict rawHeader >>= \h -> parseCookieHeader sessionCookieName h)

-- | 从 Cookie 头里取一个键（没有就给 Nothing）
parseCookieHeader :: Text -> Text -> Maybe Text
parseCookieHeader name headerValue = case [v | part <- T.splitOn ";" headerValue, Just v <- [valueOf name part]] of
    (v : _) -> Just v
    [] -> Nothing
  where
    valueOf key part =
        let (k, rest) = T.breakOn "=" part
            value = T.strip (T.drop 1 rest)
         in if T.strip k == key && not (T.null value) then Just value else Nothing

-- | 取一个整数查询参数；非法给 400，越界就夹到区间里
intParam :: Text -> Int -> Int -> Int -> ActionM Int
intParam name def lo hi = do
    rawParam <- (queryParamMaybe (TL.fromStrict name) :: ActionM (Maybe Text))
    case rawParam of
        Nothing -> pure def
        Just rawText -> case readMaybe (T.unpack (T.strip rawText)) :: Maybe Int of
            Nothing -> do
                status status400
                json (apiErrorJson "bad_request" (name <> " must be an integer"))
                finish
            Just n -> pure (max lo (min hi n))

-- | 会话 Cookie 的值（maxAge 给 0 就是删掉）
sessionCookie :: AppEnv -> Text -> Maybe Int -> Text
sessionCookie env token maxAge =
    T.concat
        [ sessionCookieName
        , "="
        , token
        , "; Path=/; HttpOnly; SameSite=Strict"
        , maybe "" (\n -> "; Max-Age=" <> T.pack (show n)) maxAge
        , if aeCookieSecure env then "; Secure" else ""
        ]

-- | 一行按给定列名渲染成 JSON 数组（缺列给 null）
renderRow :: [String] -> Row -> [A.Value]
renderRow cols row = [maybe A.Null valueJson (lookup c row) | c <- cols]

-- | 一个值转 JSON
valueJson :: Value -> A.Value
valueJson VNull = A.Null
valueJson (VInt n) = A.Number (fromIntegral n)
valueJson (VFloat d) = A.Number (fromFloatDigits d)
valueJson (VStr s) = A.String (T.pack s)
valueJson (VBool b) = A.Bool b

-- | 一张表的线上信息：列、行数、索引与统计（普通表 kind = table）
tableInfoJson :: TableInfo -> A.Value
tableInfoJson info =
    object
        [ "table" .= tiTable info
        , "kind" .= ("table" :: Text)
        , "rowCount" .= tiRows info
        , "indexes" .= [object ["column" .= c, "builtIn" .= (c == builtInIndexColumn)] | c <- tiIndexes info]
        , "columns" .= map columnJson (tiColumns info)
        ]
  where
    columnJson sc =
        object
            [ "name" .= scName sc
            , "type" .= scType sc
            , "primaryKey" .= isPrimaryKey
            , "indexed" .= isIndexed
            , "prime" .= (isPrimaryKey || isIndexed)
            , "distinct" .= distinctCount
            , "statsCapped" .= statsCapped
            , "secret" .= False
            , "nullable" .= scNullable sc
            , "default" .= fmap valueJson (scDefault sc)
            , "autoIncrement" .= scAutoIncrement sc
            , "unique" .= scUnique sc
            , "check" .= scCheck sc
            ]
      where
        name = scName sc
        isPrimaryKey = scPrimaryKey sc || (name == builtInIndexColumn && scType sc == "int")
        isIndexed = name `elem` tiIndexes info
        stat = [s | s@(n, _, _) <- tiStats info, n == name]
        distinctCount = case stat of
            ((_, d, _) : _) -> d
            [] -> -1
        statsCapped = case stat of
            ((_, _, capped) : _) -> capped
            [] -> False

-- | 存储层的内建索引列名 id，界面与服务端都按这条规则认
builtInIndexColumn :: String
builtInIndexColumn = "id"

-- | 统一错误体
apiErrorJson :: Text -> Text -> A.Value
apiErrorJson code message = object ["error" .= code, "message" .= message]

-- | 400
reply400 :: Text -> Text -> ActionM ()
reply400 code message = do
    status status400
    json (apiErrorJson code message)

-- | 403（登录了但这件事不归你管）
reply403 :: Text -> ActionM ()
reply403 message = do
    status status403
    json (apiErrorJson "forbidden" message)

-- | 413
reply413 :: ActionM ()
reply413 = do
    status status413
    json (apiErrorJson "too_large" "request body too large")

-- | 500（部署/后端问题专用，用户输入错误一律 400）
reply500 :: Text -> ActionM ()
reply500 message = do
    status status500
    json (apiErrorJson "internal" message)

-- | 引擎报的错映射成状态码：没有这张表是 404，其余是 400
engineError :: String -> ActionM a
engineError err = do
    let message = T.pack err
        missing = "unknown table" `T.isInfixOf` message
    status (if missing then status404 else status400)
    json (apiErrorJson (if missing then "not_found" else "query_error") message)
    finish

-- | 在字典里按名字找表
findTable :: Text -> [TableInfo] -> Maybe TableInfo
findTable name = go
  where
    go [] = Nothing
    go (i : is) = if tiTable i == T.unpack name then Just i else go is

data LoginRequest = LoginRequest
    { lrUser :: Text
    , lrPassword :: Text
    }

instance FromJSON LoginRequest where
    parseJSON = withObject "LoginRequest" $ \o ->
        LoginRequest <$> o .: "user" <*> o .: "password"

data QueryRequest = QueryRequest
    { qrSql :: Text
    }

instance FromJSON QueryRequest where
    parseJSON = withObject "QueryRequest" $ \o -> QueryRequest <$> o .: "sql"
