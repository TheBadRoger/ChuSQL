{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.API (
    AppEnv (..),
    WebSession (..),
    Live (..),
    defaultLive,
    setLive,
    readLive,
    applyLiveKeys,
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

import ChuSQL.Core.Model (Row, Value (..))
import ChuSQL.Core.Protocol (Account (..), QueryResult (..), SchemaColumn (..), TableInfo (..))
import ChuSQL.Interface.AccountTable (accountColumns, accountTableName, isAccountTable, passwordColumn, systemTableInfo)
import ChuSQL.Interface.Actions (
    ColumnSpec (..),
    CreateTableSpec (..),
    alterUserSql,
    coerceValue,
    columnTypeOf,
    createDatabaseSql,
    createIndexSql,
    createRoleSql,
    createTableSql,
    createUserSql,
    deleteRowSql,
    dropColumnSql,
    dropDatabaseSql,
    dropIndexSql,
    dropRoleSql,
    dropTableSql,
    dropUserSql,
    grantPrivilegesSql,
    grantRoleSql,
    insertRowSql,
    isIdentifier,
    revokePrivilegesSql,
    revokeRoleSql,
    selectRowsSql,
    updateRowSql,
 )
import ChuSQL.Interface.Auth (
    PayloadStore,
    SessionPolicy (..),
    createPayloadSession,
    defaultSessionPolicy,
    dropPayload,
    lookupPayload,
    newPayloadStore,
    sessionToken,
    setPayloadPolicy,
    sweepPayloads,
 )
import ChuSQL.Interface.Link (
    Client,
    clientPing,
    clientPolicy,
    clientReloadPolicy,
    closeClient,
    connectClient,
 )
import ChuSQL.Interface.Protocol (Grant (..), RoleView (..))
import ChuSQL.Interface.RateLimit (RateLimiter, rateLimitBlock, rateLimitClear, rateLimitRecord, setRateLimit)
import ChuSQL.Interface.Session (
    Session,
    authenticateSession,
    catalog,
    databases,
    isPlainIdentifier,
    newSession,
    roleViews,
    runStatement,
    sessionDatabase,
    sessionIsAdmin,
    switchDatabase,
 )
import qualified ChuSQL.Interface.Session as FrontSession
import ChuSQL.Interface.Settings (
    SettingItem (..),
    applySettings,
    defaultOf,
    isLockedSetting,
    liveKeys,
    readSettingsFile,
    settingCatalogue,
    writeSettingsFile,
 )
import ChuSQL.Interface.TOML (defaultConfigFile)
import ChuSQL.Web.Demo (SeedReport (..), seedDemo)
import ChuSQL.Web.Secure (bodyLimitDynamic, sameOriginOnly, securityHeaders, contentSecurityPolicy)
import ChuSQL.Web.Static (contentTypeOf, readStatic, safeRelative)
import ChuSQL.Web.UISettings (
    readUISettings,
    resolveUISettingsFile,
    uiSettingsFileCandidates,
    validateUISettings,
    writeUISettings,
 )
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, withMVar)
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON (..), eitherDecode, object, withObject, (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (nub, sortBy)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Scientific (fromFloatDigits)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Encoding.Error as TEE
import qualified Data.Text.Lazy as TL
import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector as V
import Network.HTTP.Types (Status, status204, status400, status401, status403, status404, status409, status413, status429, status500)
import Network.Wai (Application, Request (..))
import Text.Read (readMaybe)
import Web.Scotty

-- REST API 与页面交付：鉴权、JSON 与引擎结果的互转、错误到状态码的映射。

-- | 一组可热改的运行参数
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

-- | Web 进程共享的运行环境
data AppEnv = AppEnv
    { aeServerHost :: Text
    , aeServerPort :: Int
    , aeSessions :: PayloadStore WebSession
    , aeLimiter :: RateLimiter
    , aeStaticDir :: FilePath
    , aeCookieSecure :: Bool
    , aeLive :: IORef Live
    , aeSettingsFile :: FilePath
    , aeUISettingsFile :: FilePath
    , aeEffective :: Map.Map Text Text
    , aeLog :: Text -> IO ()
    }

-- | 一条网页会话：TCP 连接、登录状态与账号名
data WebSession = WebSession
    { wsClient :: Client
    , wsSession :: Session
    , wsUser :: Text
    , wsAdmin :: Bool
    }

-- | 会话 Cookie 名
sessionCookieName :: Text
sessionCookieName = "chusql_session"

-- | 造一份运行环境，各项上限用内置默认
newAppEnv :: Text -> Int -> RateLimiter -> FilePath -> IO AppEnv
newAppEnv host port limiter staticDir = do
    settingsFile <- resolveSettingsFileSafe
    newAppEnvAt settingsFile host port limiter staticDir

-- | 同上，但显式指定配置文件路径
newAppEnvAt :: FilePath -> Text -> Int -> RateLimiter -> FilePath -> IO AppEnv
newAppEnvAt settingsFile host port limiter staticDir = do
    sessions <- newPayloadStore getCurrentTime defaultSessionPolicy
    liveRef <- newIORef defaultLive
    uiSettingsFile <- resolveUISettingsFileSafe
    pure
        AppEnv
            { aeServerHost = host
            , aeServerPort = port
            , aeSessions = sessions
            , aeLimiter = limiter
            , aeStaticDir = staticDir
            , aeCookieSecure = False
            , aeLive = liveRef
            , aeSettingsFile = settingsFile
            , aeUISettingsFile = uiSettingsFile
            , aeEffective = Map.empty
            , aeLog = const (pure ())
            }

-- | 系统库：账号表住在这里，只有管理员能进
systemDatabase :: Text
systemDatabase = "system"

-- | 保留库：只有系统库删不掉、也建不了。
reservedDatabase :: Text -> Bool
reservedDatabase name = name == systemDatabase

-- | 定位配置文件路径，失败时回落到固定名
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

-- | 组装 WAI 应用：路由外套四层中间件
webApp :: AppEnv -> IO Application
webApp env = do
    inner <- scottyApp (routes env)
    locks <- newMVar Map.empty
    pure (securityHeaders (sameOriginOnly (bodyLimitDynamic (liveBodyLimit env) (serializeSessions locks inner))))

-- | 同一条会话的请求串行执行
serializeSessions :: MVar (Map.Map Text (MVar ())) -> Application -> Application
serializeSessions locks app req respond = case sessionKey req of
    Nothing -> app req respond
    Just key -> do
        lock <- lockFor locks key
        withMVar lock (const (app req respond))
  where
    -- | 取请求所属的会话令牌
    sessionKey _ = do
        rawCookie <- lookup "Cookie" (requestHeaders req)
        parseCookieHeader sessionCookieName (TE.decodeUtf8With TEE.lenientDecode rawCookie)

-- | 一条会话一把锁（按 Cookie 令牌去重）
lockFor :: MVar (Map.Map Text (MVar ())) -> Text -> IO (MVar ())
lockFor locks key = modifyMVar locks $ \known -> case Map.lookup key known of
    Just lock -> pure (known, lock)
    Nothing -> do
        lock <- newMVar ()
        pure (Map.insert key lock known, lock)

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

-- | 先认会话，再按请求头切本会话的库
withDatabase :: AppEnv -> (AppEnv -> ActionM ()) -> ActionM ()
withDatabase env action = do
    ws <- requireSession env
    chosen <- maybe "" (T.toLower . TL.toStrict) <$> header "X-ChuSQL-Database"
    if not (T.null chosen) && (not (isPlainIdentifier chosen) || T.length chosen > 64)
        then reply400 "bad_request" "invalid database name"
        else
            if chosen == systemDatabase && not (wsAdmin ws)
                then reply403 "the system database is only available to the administrator"
                else do
                    ready <- liftIO (if T.null chosen then pure (Right ()) else switchDatabase (wsSession ws) chosen)
                    case ready of
                        Left message -> serverError message
                        Right () -> action env

-- | 取当前库名，未选库时报错
requireDatabaseName :: WebSession -> ActionM Text
requireDatabaseName ws = do
    current <- liftIO (sessionDatabase (wsSession ws))
    if T.null current
        then apiError "no_database" "no database selected"
        else pure current

-- | 取本请求的会话，没有令牌就 401
requireSession :: AppEnv -> ActionM WebSession
requireSession env = do
    token <- requestSessionToken >>= maybe (apiError "unauthorized" "sign in first") pure
    found <- liftIO (lookupPayload (aeSessions env) token)
    case found of
        Nothing -> apiError "unauthorized" "sign in first"
        Just (_, ws) -> pure ws

-- | 管理员专属操作
requireAdminSession :: AppEnv -> ActionM WebSession
requireAdminSession env = do
    ws <- requireSession env
    adminOnly ws
    pure ws

-- | 只有管理员能碰的东西
adminOnly :: WebSession -> ActionM ()
adminOnly ws
    | wsAdmin ws = pure ()
    | otherwise = apiError "forbidden" "administrator required"

-- | 数据库清单；系统库 system 只给管理员看。
databasesH :: AppEnv -> ActionM ()
databasesH env = do
    ws <- requireSession env
    result <- liftIO (databases (wsSession ws))
    case result of
        Left message -> serverError message
        Right names -> json (if wsAdmin ws then names else filter (/= systemDatabase) names)

-- | 新建数据库：校验名字后交给 server
createDatabaseH :: AppEnv -> ActionM ()
createDatabaseH env = do
    ws <- requireAdminSession env
    withJsonObject env $ \o -> case textField "name" o of
        Left err -> reply400 "bad_request" err
        Right name -> case createDatabaseSql name of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite ws sql

-- | 删除数据库；系统库 system 一律拒绝。
dropDatabaseH :: AppEnv -> ActionM ()
dropDatabaseH env = do
    ws <- requireAdminSession env
    name <- pathParam "name"
    if reservedDatabase name
        then reply400 "bad_request" "the system database cannot be dropped"
        else case dropDatabaseSql name of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite ws sql

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

-- | 探活：回进程与存储状态，顺带清理过期会话
statusH :: AppEnv -> ActionM ()
statusH env = do
    expired <- liftIO (sweepPayloads (aeSessions env))
    liftIO (mapM_ (closeClient . wsClient) expired)
    opened <- liftIO (connectClient (aeServerHost env) (aeServerPort env))
    case opened of
        Left message -> json (object ["status" .= ("ok" :: Text), "storage" .= ("down" :: Text), "detail" .= message])
        Right client -> do
            probe <- liftIO (clientPing client)
            liftIO (closeClient client)
            case probe of
                Right () -> json (object ["status" .= ("ok" :: Text), "storage" .= ("up" :: Text)])
                Left message -> json (object ["status" .= ("ok" :: Text), "storage" .= ("down" :: Text), "detail" .= message])

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
                            Nothing -> openSession env user password

-- | 用用户口令连 server，成功后建会话
openSession :: AppEnv -> Text -> Text -> ActionM ()
openSession env user password = do
    live <- liftIO (readLive env)
    opened <- liftIO (connectClient (aeServerHost env) (aeServerPort env))
    case opened of
        Left message -> do
            liftIO (aeLog env ("sign-in failed user=" <> user <> " " <> message))
            status status500
            json (apiErrorJson "internal" message)
        Right client -> do
            session <- liftIO (newSession client)
            signed <- liftIO (authenticateSession session user password)
            case signed of
                Left message -> do
                    liftIO (closeClient client)
                    liftIO (rateLimitRecord (aeLimiter env) user)
                    liftIO (aeLog env ("sign-in failed user=" <> user <> " " <> message))
                    if errorCodeOf message == "unauthorized"
                        then do
                            status status401
                            json (apiErrorJson "unauthorized" "invalid user name or password")
                        else serverError message
                Right () -> do
                    admin <- liftIO (sessionIsAdmin session)
                    token <- liftIO (createPayloadSession (aeSessions env) user (WebSession client session user admin))
                    liftIO (rateLimitClear (aeLimiter env) user)
                    setHeader "Set-Cookie" (TL.fromStrict (sessionCookie env token (Just (lvSessionMax live))))
                    liftIO (aeLog env ("sign-in ok user=" <> user))
                    status status204

-- | 退出：收掉会话与连接，清除 Cookie
logoutH :: AppEnv -> ActionM ()
logoutH env = do
    token <- requestSessionToken
    case token of
        Nothing -> pure ()
        Just t -> do
            gone <- liftIO (dropPayload (aeSessions env) t)
            case gone of
                Nothing -> pure ()
                Just ws -> liftIO (closeClient (wsClient ws))
    setHeader "Set-Cookie" (TL.fromStrict (sessionCookie env "" (Just 0)))
    status status204

-- | 当前登录者与生效的口令策略
sessionH :: AppEnv -> ActionM ()
sessionH env = do
    ws <- requireSession env
    policy <- liftIO (clientPolicy (wsClient ws))
    case policy of
        Left message -> serverError message
        Right (minLength, classes) ->
            json
                ( object
                    [ "user" .= wsUser ws
                    , "administrator" .= wsAdmin ws
                    , "policy" .= object ["minLength" .= minLength, "classes" .= classes]
                    ]
                )

-- | 读设置：文件值、启动生效值与内置默认一起给出
settingsH :: AppEnv -> ActionM ()
settingsH env = do
    ws <- requireSession env
    fileValues <- liftIO (readSettingsFile (aeSettingsFile env))
    let owner = wsAdmin ws
    json
        ( object
            [ "file" .= aeSettingsFile env
            , "account" .= wsUser ws
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
    ws <- requireSession env
    let user = wsUser ws
    withJsonObject env $ \o -> case settingsUpdates o of
        Left err -> reply400 "bad_request" err
        Right updates -> do
            let locked = [k | k <- Map.keys updates, isLockedSetting k]
                forbidden =
                    [ k
                    | k <- Map.keys updates
                    , maybe False siRootOnly (findSetting k)
                    , not (wsAdmin ws)
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
                                            -- 口令策略住在 server：让它重读自己的设置文件
                                            reloaded <- liftIO (clientReloadPolicy (wsClient ws))
                                            let policyKeys = ["password-min-length", "password-classes"]
                                                appliedNow = case reloaded of
                                                    Right _ -> nub (applied ++ [k | k <- policyKeys, Map.member k updates])
                                                    Left _ -> applied
                                                needsRestart = [k | k <- Map.keys updates, not (k `elem` appliedNow), not (T.null (Map.findWithDefault "" k updates))]
                                            liftIO (aeLog env ("settings updated by " <> user <> ": " <> T.intercalate ", " (Map.keys updates)))
                                            json
                                                ( object
                                                    [ "ok" .= True
                                                    , "file" .= aeSettingsFile env
                                                    , "applied" .= appliedNow
                                                    , "restartRequired" .= needsRestart
                                                    ]
                                                )

-- | 读前端 IDE 的设置文件
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

-- | 点账号表时只认系统库，否则 400
accountRoute :: WebSession -> Text -> ActionM Bool
accountRoute ws name
    | not (isAccountTable name) = pure False
    | otherwise = do
        current <- liftIO (sessionDatabase (wsSession ws))
        if current /= systemDatabase
            then do
                reply400 "bad_request" "the account table lives in the system database"
                finish
            else pure True

-- | 账号行：平铺的各列
accountRowsOf :: [Account] -> [Row]
accountRowsOf = map accountRow
  where
    -- | 一个账号转成一行
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
    -- | 一列转成线上 JSON
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

-- | 读一遍账号表，读不到就按 server 给的码回
requireAccounts :: WebSession -> ActionM [Account]
requireAccounts ws = do
    result <- liftIO (FrontSession.accounts (wsSession ws))
    either serverError pure result

-- | 口令列不接受排序与筛选
rejectSecretColumn :: Maybe (Text, Bool) -> [(Text, Value)] -> Either Text ()
rejectSecretColumn order filters
    | Just (col, _) <- order, col == T.pack passwordColumn = Left "the password column cannot be sorted"
    | any ((== T.pack passwordColumn) . fst) filters = Left "the password column cannot be filtered"
    | otherwise = Right ()

-- | 账号表的一页数据（排序与筛选都在内存里做）
baseRowsH :: WebSession -> AppEnv -> ActionM ()
baseRowsH ws env = do
    adminOnly ws
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
                accounts <- requireAccounts ws
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

-- | 按用户名排序（其余列不支持排序）
sortAccountRows :: Maybe (Text, Bool) -> [Row] -> [Row]
sortAccountRows Nothing rows = rows
sortAccountRows (Just (column, ascending)) rows =
    sortBy compareUser rows
  where
    -- | 比两个账号单元
    compareUser a b = case (accountCell a column, accountCell b column) of
        (Just x, Just y) -> if ascending then compare x y else compare y x
        _ -> EQ
    -- | 取账号行里某列的文本值
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
    -- | 行是否满足一条等值条件
    matches row (column, wanted) = case lookup (T.unpack column) row of
        Just value -> value == wanted
        Nothing -> False

-- | 账号表写入：插一行 = CREATE USER
baseInsertH :: AppEnv -> WebSession -> ActionM ()
baseInsertH env ws = do
    adminOnly ws
    withJsonObject env $ \o -> case valuesField o >>= readRowValues False accountColumns of
        Left err -> reply400 "bad_request" err
        Right values -> case (lookupValue "user" values, lookupValue (T.pack passwordColumn) values) of
            (Just (VStr user), Just (VStr password)) -> case createUserSql (T.pack user) (T.pack password) of
                Left err -> reply400 "bad_request" (T.pack err)
                Right sql -> runWrite ws sql
            _ -> reply400 "bad_request" "user and password are required"

-- | 账号表写入：改一行 = ALTER USER
baseUpdateH :: AppEnv -> WebSession -> Text -> ActionM ()
baseUpdateH env ws target = do
    adminOnly ws
    withJsonObject env $ \o -> case valuesField o >>= readRowValues False accountColumns of
        Left err -> reply400 "bad_request" err
        Right values -> case (values, lookupValue (T.pack passwordColumn) values) of
            ([(_, VStr password)], Just _) -> case alterUserSql target (T.pack password) of
                Left err -> reply400 "bad_request" (T.pack err)
                Right sql -> runWrite ws sql
            _ -> reply400 "bad_request" "only the password of an existing account can be changed"

-- | 账号表写入：删一行 = DROP USER
baseDeleteH :: AppEnv -> WebSession -> Text -> ActionM ()
baseDeleteH _env ws target = do
    adminOnly ws
    case dropUserSql target of
        Left err -> reply400 "bad_request" (T.pack err)
        Right sql -> runWrite ws sql

-- | 取一个列值
lookupValue :: Text -> [(String, Value)] -> Maybe Value
lookupValue name values = lookup (T.unpack name) values

-- | 错误码 + 消息 → 状态码 + 统一错误体
apiError :: Text -> Text -> ActionM a
apiError code message = do
    status (statusOf code)
    json (apiErrorJson code message)
    finish

-- | 拆开 server 的 code: message 并映射
serverError :: Text -> ActionM a
serverError message
    | code `elem` knownCodes = apiError code detail
    | otherwise = apiError "internal" message
  where
    (rawCode, rest) = T.breakOn ":" message
    code = T.strip rawCode
    detail = T.strip (T.drop 1 rest)
    knownCodes = ["unauthorized", "forbidden", "not_found", "conflict", "bad_request", "no_database", "query_error", "storage_error", "too_large"]

-- | 取错误消息里的错误码
errorCodeOf :: Text -> Text
errorCodeOf message = T.strip (fst (T.breakOn ":" message))

-- | 错误码 → HTTP 状态码
statusOf :: Text -> Status
statusOf code = case code of
    "unauthorized" -> status401
    "forbidden" -> status403
    "not_found" -> status404
    "conflict" -> status409
    "too_many_attempts" -> status429
    "too_large" -> status413
    "storage_error" -> status500
    "internal" -> status500
    _ -> status400

-- | 按 key 找目录项
findSetting :: Text -> Maybe SettingItem
findSetting key = case [i | i <- settingCatalogue, siKey i == key] of
    (i : _) -> Just i
    [] -> Nothing

-- | 把热改设置应用到当前进程
applyLiveKeys :: AppEnv -> Map.Map Text Text -> IO [Text]
applyLiveKeys env updates = do
    before <- readLive env
    let wanted = [k | k <- liveKeys, Map.member k updates]
        -- | 取热改项的整数值
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
    setPayloadPolicy
        (aeSessions env)
        (SessionPolicy (fromIntegral (lvSessionIdle after)) (fromIntegral (lvSessionMax after)))
    setRateLimit (aeLimiter env) (lvLoginMaxAttempts after) (fromIntegral (lvLoginWindow after))
    pure wanted

-- | 某项无配置时的实际生效值
effectiveValueOf :: Map.Map Text Text -> Text -> Text
effectiveValueOf effectiveValues key = fromMaybe (defaultOf key) (Map.lookup key effectiveValues)

-- | 把字符串解析成整数，坏值给 0
intFromText :: Text -> Int
intFromText given = fromMaybe 0 (readMaybe (T.unpack (T.strip given)))

-- | 表清单；管理员额外看到账号表
tablesH :: AppEnv -> ActionM ()
tablesH env = do
    ws <- requireSession env
    current <- requireDatabaseName ws
    result <- liftIO (catalog (wsSession ws))
    case result of
        Left message -> serverError message
        Right infos -> do
            let tables = [info | info <- infos, not (isAccountTable (T.pack (tiTable info)))]
            extra <- if current == systemDatabase && wsAdmin ws
                then (: []) . systemTableJson . length <$> requireAccounts ws
                else pure []
            json (map tableInfoJson tables ++ extra)

-- | 单表结构
tableH :: AppEnv -> ActionM ()
tableH env = do
    ws <- requireSession env
    _ <- requireDatabaseName ws
    name <- pathParam "t"
    account <- accountRoute ws name
    if account
        then do
            adminOnly ws
            accounts <- requireAccounts ws
            json (systemTableJson (length accounts))
        else
            if not (isIdentifier name)
                then reply400 "bad_request" "invalid table name"
                else do
                    result <- liftIO (catalog (wsSession ws))
                    case result of
                        Left message -> serverError message
                        Right infos -> case findTable name infos of
                            Nothing -> reply404
                            Just info -> json (tableInfoJson info)

-- | 分页浏览一张表，支持排序与等值过滤；账号表走内存里的账号行
rowsH :: AppEnv -> ActionM ()
rowsH env = do
    ws <- requireSession env
    _ <- requireDatabaseName ws
    name <- pathParam "t"
    account <- accountRoute ws name
    if account
        then baseRowsH ws env
        else
            if not (isIdentifier name)
                then reply400 "bad_request" "invalid table name"
                else do
                    info <- requireTable ws name
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
                                result <- liftIO (runStatement (wsSession ws) (T.pack sql))
                                case result of
                                    Left message -> serverError message
                                    Right res -> do
                                        let cols = map T.unpack (qrColumns res)
                                            page = take limit (drop offset (qrRows res))
                                            (sortColumn, ascending) = splitSort sortOrder
                                        json
                                            ( object
                                                [ "table" .= tiTable info
                                                , "columns" .= cols
                                                , "rows" .= map (map valueJson) page
                                                , "total" .= qrRowCount res
                                                , "limit" .= limit
                                                , "offset" .= offset
                                                , "sort" .= maybe A.Null A.String sortColumn
                                                , "dir" .= (if ascending then "asc" else "desc" :: Text)
                                                , "filters" .= map fst filters
                                                ]
                                            )

-- | 等值过滤条件：列名与原值
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
    -- | 一条过滤条件转成列与引擎值
    toPair (RowFilter column rawValue)
        | T.null rawValue = Right Nothing
        | otherwise = do
            schema <- maybe (Left ("unknown column: " <> column)) Right (lookupColumn column)
            ty <- maybe (Left ("unsupported column type for: " <> column)) Right (columnTypeOf (T.pack (scType schema)))
            value <- either (Left . T.pack) Right (coerceValue ty (A.String rawValue))
            pure (Just (column, value))
    -- | 按列名找表结构里的列
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
    -- | dir 参数转排序方向
    dirOf given = case fmap (T.toLower . T.strip) given of
        Just "desc" -> False
        _ -> True

-- | 建表：按列结构拼 CREATE TABLE
createTableH :: AppEnv -> ActionM ()
createTableH env = do
    ws <- requireSession env
    _ <- requireDatabaseName ws
    adminOnly ws
    withJsonObject env $ \o -> case decodeCreateTable o of
        Left err -> reply400 "bad_request" err
        Right spec -> case createTableSql spec of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite ws sql

-- | 删表；账号表不许删
dropTableH :: AppEnv -> ActionM ()
dropTableH env = do
    ws <- requireSession env
    _ <- requireDatabaseName ws
    adminOnly ws
    name <- pathParam "t"
    account <- accountRoute ws name
    if account
        then reply400 "bad_request" "the account table cannot be dropped"
        else case dropTableSql name of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite ws sql

-- | 插一行；账号表上等于 CREATE USER
insertRowH :: AppEnv -> ActionM ()
insertRowH env = do
    ws <- requireSession env
    _ <- requireDatabaseName ws
    name <- pathParam "t"
    account <- accountRoute ws name
    if account
        then baseInsertH env ws
        else do
            info <- requireTable ws name
            withJsonObject env $ \o -> case valuesField o >>= readRowValues False (tiColumns info) of
                Left err -> reply400 "bad_request" err
                Right row -> case insertRowSql name row of
                    Left err -> reply400 "bad_request" (T.pack err)
                    Right sql -> runWrite ws sql

-- | 改一行，只改给出的列
updateRowH :: AppEnv -> ActionM ()
updateRowH env = do
    ws <- requireSession env
    _ <- requireDatabaseName ws
    name <- pathParam "t"
    account <- accountRoute ws name
    if account
        then do
            target <- pathParam "id"
            baseUpdateH env ws target
        else do
            key <- rowIdParam
            info <- requireTable ws name
            withJsonObject env $ \o -> case valuesField o >>= readRowValues False (tiColumns info) of
                Left err -> reply400 "bad_request" err
                Right assigns -> case updateRowSql name key assigns of
                    Left err -> reply400 "bad_request" (T.pack err)
                    Right sql -> runWrite ws sql

-- | 删一行；账号表上等于 DROP USER
deleteRowH :: AppEnv -> ActionM ()
deleteRowH env = do
    ws <- requireSession env
    _ <- requireDatabaseName ws
    name <- pathParam "t"
    account <- accountRoute ws name
    if account
        then do
            target <- pathParam "id"
            baseDeleteH env ws target
        else do
            key <- rowIdParam
            _ <- requireTable ws name
            case deleteRowSql name key of
                Left err -> reply400 "bad_request" (T.pack err)
                Right sql -> runWrite ws sql

-- | 建索引，索引列必须整数且唯一；账号表没有索引可建
createIndexH :: AppEnv -> ActionM ()
createIndexH env = do
    ws <- requireSession env
    _ <- requireDatabaseName ws
    adminOnly ws
    name <- pathParam "t"
    account <- accountRoute ws name
    if account
        then reply400 "bad_request" "the account table has no indexes"
        else do
            info <- requireTable ws name
            withJsonObject env $ \o -> case textField "column" o of
                Left err -> reply400 "bad_request" err
                Right column -> case columnTypeOf (columnTypeText (tiColumns info) column) of
                    Nothing -> reply400 "bad_request" ("unknown column: " <> column)
                    Just _ -> case createIndexSql name column of
                        Left err -> reply400 "bad_request" (T.pack err)
                        Right sql -> runWrite ws sql

-- | 删索引，内建 id 索引不放行
dropIndexH :: AppEnv -> ActionM ()
dropIndexH env = do
    ws <- requireSession env
    _ <- requireDatabaseName ws
    adminOnly ws
    name <- pathParam "t"
    column <- pathParam "col"
    account <- accountRoute ws name
    if account
        then reply400 "bad_request" "the account table has no indexes"
        else do
            _ <- requireTable ws name
            if column == T.pack builtInIndexColumn
                then reply400 "bad_request" "the built-in id index cannot be dropped"
                else case dropIndexSql name column of
                    Left err -> reply400 "bad_request" (T.pack err)
                    Right sql -> runWrite ws sql

-- | 删列，内建 id 列不放行；账号表的列是固定的
dropColumnH :: AppEnv -> ActionM ()
dropColumnH env = do
    ws <- requireSession env
    _ <- requireDatabaseName ws
    adminOnly ws
    name <- pathParam "t"
    column <- pathParam "col"
    account <- accountRoute ws name
    if account
        then reply400 "bad_request" "the account table has no columns to drop"
        else do
            _ <- requireTable ws name
            if column == T.pack builtInIndexColumn
                then reply400 "bad_request" "the built-in id column cannot be dropped"
                else case dropColumnSql name column of
                    Left err -> reply400 "bad_request" (T.pack err)
                    Right sql -> runWrite ws sql

-- | 一键灌演示数据（缺什么补什么，可以反复点）
demoDataH :: AppEnv -> ActionM ()
demoDataH env = do
    ws <- requireAdminSession env
    _ <- requireDatabaseName ws
    result <- liftIO (seedDemo (wsSession ws))
    case result of
        Left message -> serverError message
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
    ws <- requireAdminSession env
    result <- liftIO (roleViews (wsSession ws))
    case result of
        Left message -> serverError message
        Right views -> json (map roleViewJson views)

-- | 一个角色的线上信息
roleViewJson :: RoleView -> A.Value
roleViewJson view =
    object
        [ "name" .= roleName view
        , "grants" .= map grantJson (roleGrants view)
        , "members" .= roleMembers view
        ]

-- | 一条授权的线上信息
grantJson :: Grant -> A.Value
grantJson grant =
    object
        [ "privilege" .= grantPrivilege grant
        , "object" .= grantObject grant
        ]

-- | 建角色
createRoleH :: AppEnv -> ActionM ()
createRoleH env = do
    ws <- requireAdminSession env
    withJsonObject env $ \o -> case textField "name" o of
        Left err -> reply400 "bad_request" err
        Right name -> runPrivilegeSql ws (createRoleSql name)

-- | 删角色（连带清掉它的授权与成员）
dropRoleH :: AppEnv -> ActionM ()
dropRoleH env = do
    ws <- requireAdminSession env
    name <- pathParam "name"
    runPrivilegeSql ws (dropRoleSql name)

-- | 给角色加权限
grantRoleH :: AppEnv -> ActionM ()
grantRoleH env = do
    ws <- requireAdminSession env
    role <- pathParam "name"
    withJsonObject env $ \o -> case grantFields o of
        Left err -> reply400 "bad_request" err
        Right (privileges, objectName) -> runPrivilegeSql ws (grantPrivilegesSql privileges objectName role)

-- | 收角色的权限
revokeRoleH :: AppEnv -> ActionM ()
revokeRoleH env = do
    ws <- requireAdminSession env
    role <- pathParam "name"
    withJsonObject env $ \o -> case grantFields o of
        Left err -> reply400 "bad_request" err
        Right (privileges, objectName) -> runPrivilegeSql ws (revokePrivilegesSql privileges objectName role)

-- | 把用户加进角色
addRoleMemberH :: AppEnv -> ActionM ()
addRoleMemberH env = do
    ws <- requireAdminSession env
    role <- pathParam "name"
    withJsonObject env $ \o -> case memberFields o of
        Left err -> reply400 "bad_request" err
        Right users -> runPrivilegeSql ws (grantRoleSql role users)

-- | 把用户移出角色
removeRoleMemberH :: AppEnv -> ActionM ()
removeRoleMemberH env = do
    ws <- requireAdminSession env
    role <- pathParam "name"
    user <- pathParam "user"
    runPrivilegeSql ws (revokeRoleSql role [user])

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

-- | 只接受字符串的取值
stringValue :: A.Value -> Either Text Text
stringValue (A.String value) = Right value
stringValue _ = Left "values must be strings"

-- | 角色与授权命令：拼 SQL 交给 server
runPrivilegeSql :: WebSession -> Either String String -> ActionM ()
runPrivilegeSql ws statement = case statement of
    Left err -> reply400 "bad_request" (T.pack err)
    Right sql -> do
        result <- liftIO (runStatement (wsSession ws) (T.pack sql))
        case result of
            Left message -> serverError message
            Right _ -> json (object ["ok" .= True])

-- | 写操作统一回执
runWrite :: WebSession -> String -> ActionM ()
runWrite ws sql = do
    result <- liftIO (runStatement (wsSession ws) (T.pack sql))
    case result of
        Left message -> serverError message
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
requireTable :: WebSession -> Text -> ActionM TableInfo
requireTable ws name
    | not (isIdentifier name) = do
        reply400 "bad_request" "invalid table name"
        finish
    | otherwise = do
        result <- liftIO (catalog (wsSession ws))
        case result of
            Left message -> serverError message
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
    -- | 一个设置值转成文本
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
    -- | 一列 {name, type} 转成列结构
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
    -- | 一个列值按结构转成引擎值
    convert (k, v) = do
        let name = K.toString k
        schema <- maybe (Left ("unknown column: " <> T.pack name)) Right (lookupSchema name)
        ty <- maybe (Left ("unsupported column type for: " <> T.pack name)) Right (columnTypeOf (T.pack (scType schema)))
        value <- either (Left . T.pack) Right (coerceValue ty v)
        pure (name, value)
    -- | 按列名找表结构里的列
    lookupSchema name = case [c | c <- cols, scName c == name] of
        (c : _) -> Just c
        [] -> Nothing

-- | 某列在数据字典里的类型字符串（找不到就给空串）
columnTypeText :: [SchemaColumn] -> Text -> Text
columnTypeText cols name = case [c | c <- cols, scName c == T.unpack name] of
    (c : _) -> T.pack (scType c)
    [] -> ""

-- | 查询接口：校验请求体后执行 SQL
queryH :: AppEnv -> ActionM ()
queryH env = do
    ws <- requireSession env
    let user = wsUser ws
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
                                executeSql ws live sql

-- | 把 SQL 交给 server，结果按控制台口径回执
executeSql :: WebSession -> Live -> Text -> ActionM ()
executeSql ws live sql = do
    result <- liftIO (runStatement (wsSession ws) sql)
    case result of
        Left message -> serverError message
        Right res -> do
            let shown = take (lvMaxRows live) (qrRows res)
            json
                ( object
                    [ "columns" .= qrColumns res
                    , "rows" .= map (map valueJson) shown
                    , "rowCount" .= qrRowCount res
                    , "truncated" .= (qrTruncated res || qrRowCount res > lvMaxRows live)
                    , "database" .= qrDatabase res
                    ]
                )

-- | 拿当前 Session 用户；没有就 401
requireUser :: AppEnv -> ActionM Text
requireUser env = wsUser <$> requireSession env

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
    -- | 从一段 Cookie 里取指定键的值
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

-- | 一张表的线上信息：列、行数、索引与统计
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
    -- | 一列转成线上 JSON
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

-- | 在字典里按名字找表
findTable :: Text -> [TableInfo] -> Maybe TableInfo
findTable name = go
  where
    -- | 顺序找表
    go [] = Nothing
    go (i : is) = if tiTable i == T.unpack name then Just i else go is

-- | 登录请求体
data LoginRequest = LoginRequest
    { lrUser :: Text
    , lrPassword :: Text
    }

instance FromJSON LoginRequest where
    parseJSON = withObject "LoginRequest" $ \o ->
        LoginRequest <$> o .: "user" <*> o .: "password"

-- | 查询请求体
data QueryRequest = QueryRequest
    { qrSql :: Text
    }

instance FromJSON QueryRequest where
    parseJSON = withObject "QueryRequest" $ \o -> QueryRequest <$> o .: "sql"
