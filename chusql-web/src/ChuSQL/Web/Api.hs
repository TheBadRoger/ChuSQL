{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.Api (
    AppEnv (..),
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
    apiErrorJson,
    newAppEnv,
) where

import ChuSQL.Model (Column (..), Row, Value (..))
import ChuSQL.Storage.IPC (SchemaColumn (..), TableInfo (..))
import ChuSQL.Web.Actions (
    ColumnSpec (..),
    CreateTableSpec (..),
    coerceValue,
    columnTypeOf,
    createIndexSql,
    createTableSql,
    deleteRowSql,
    dropColumnSql,
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
    createSession,
    deleteOtherSessions,
    deleteSession,
    hashLooksValid,
    hashPassword,
    lookupSession,
    setSessionPolicy,
    sessionToken,
    verifyPassword,
 )
import ChuSQL.Web.Backend (Backend, StatementResult (..), beCatalog, bePing, beStatement)
import ChuSQL.Web.Demo (SeedReport (..), seedDemo)
import ChuSQL.Web.RateLimit (RateLimiter, rateLimitBlock, rateLimitClear, rateLimitRecord, setRateLimit)
import ChuSQL.Web.Secure (bodyLimitDynamic, sameOriginOnly, securityHeaders, contentSecurityPolicy)
import ChuSQL.Web.Settings (
    SettingItem (..),
    applySettings,
    defaultOf,
    liveKeys,
    readSettingsFile,
    resolveSettingsFile,
    settingCatalogue,
    settingsFileCandidates,
    writeSettingsFile,
 )
import ChuSQL.Web.Static (contentTypeOf, readStatic, safeRelative)
import ChuSQL.Web.UiSettings (
    readUiSettings,
    resolveUiSettingsFile,
    uiSettingsFileCandidates,
    validateUiSettings,
    writeUiSettings,
 )
import Control.Exception (IOException, try)
import Data.Aeson (FromJSON (..), eitherDecode, object, withObject, (.:), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Vector as V
import Network.HTTP.Types (status204, status400, status401, status403, status404, status413, status429, status500)
import Network.Wai (Application)
import System.Environment (lookupEnv)
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
    , aeCredential :: IORef Credential
    , aeLimiter :: RateLimiter
    , aeStaticDir :: FilePath
    , aeCookieSecure :: Bool
    , aeLive :: IORef Live
    , aeSettingsFile :: FilePath
    , aeUiSettingsFile :: FilePath
    , aeEffective :: Map.Map Text Text
    , aeLog :: Text -> IO ()
    }

-- | 会话 Cookie 名
sessionCookieName :: Text
sessionCookieName = "chusql_session"

-- | 参数齐全地造一份环境（各项上限用内置默认，调用方按配置改）
newAppEnv :: Backend -> SessionStore -> Credential -> RateLimiter -> FilePath -> IO AppEnv
newAppEnv backend sessions cred limiter staticDir = do
    credRef <- newIORef cred
    liveRef <- newIORef defaultLive
    settingsFile <- resolveSettingsFileSafe
    uiSettingsFile <- resolveUiSettingsFileSafe
    pure
        AppEnv
            { aeBackend = backend
            , aeSessions = sessions
            , aeCredential = credRef
            , aeLimiter = limiter
            , aeStaticDir = staticDir
            , aeCookieSecure = False
            , aeLive = liveRef
            , aeSettingsFile = settingsFile
            , aeUiSettingsFile = uiSettingsFile
            , aeEffective = Map.empty
            , aeLog = const (pure ())
            }

-- | 定位设置文件，找不到就定下首个候选路径
resolveSettingsFileSafe :: IO FilePath
resolveSettingsFileSafe = do
    found <- try resolveSettingsFile :: IO (Either IOException FilePath)
    pure (either (const fallbackSettingsFile) id found)
  where
    -- | 候选清单里的第一个
    fallbackSettingsFile = case settingsFileCandidates of
        (p : _) -> p
        [] -> "chusql.settings.json"

-- | 定位 IDE 设置文件，找不到就定下首个候选路径
resolveUiSettingsFileSafe :: IO FilePath
resolveUiSettingsFileSafe = do
    found <- try resolveUiSettingsFile :: IO (Either IOException FilePath)
    pure (either (const fallbackUiSettingsFile) id found)
  where
    -- | 候选清单里的第一个
    fallbackUiSettingsFile = case uiSettingsFileCandidates of
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
    put "/api/ui-settings" (putUiSettingsH env)
    post "/api/account/password" (changePasswordH env)
    get "/api/tables" (tablesH env)
    post "/api/tables" (createTableH env)
    delete "/api/tables/:t" (dropTableH env)
    get "/api/tables/:t" (tableH env)
    get "/api/tables/:t/rows" (rowsH env)
    post "/api/tables/:t/rows" (insertRowH env)
    patch "/api/tables/:t/rows/:id" (updateRowH env)
    delete "/api/tables/:t/rows/:id" (deleteRowH env)
    post "/api/tables/:t/indexes" (createIndexH env)
    delete "/api/tables/:t/indexes/:col" (dropIndexH env)
    delete "/api/tables/:t/columns/:col" (dropColumnH env)
    post "/api/demo-data" (demoDataH env)
    post "/api/query" (queryH env)
    notFound (notFoundH env)

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
                if T.null user || T.length user > 64 || T.null password || T.length password > 256
                    then reply400 "bad_request" "user name or password is empty or too long"
                    else do
                        blocked <- liftIO (rateLimitBlock (aeLimiter env) user)
                        case blocked of
                            Just left -> do
                                setHeader "Retry-After" (TL.fromStrict (T.pack (show (ceiling left :: Int))))
                                status status429
                                json (apiErrorJson "too_many_attempts" "too many failed sign-in attempts, try again later")
                            Nothing -> do
                                cred <- liftIO (readCredential env)
                                live <- liftIO (readLive env)
                                let hashOk = verifyPassword (credEncoded cred) password
                                    userOk = T.toLower user == T.toLower (credUser cred)
                                if userOk && hashOk
                                    then do
                                        liftIO (rateLimitClear (aeLimiter env) user)
                                        token <- liftIO (createSession (aeSessions env) (credUser cred))
                                        setHeader "Set-Cookie" (TL.fromStrict (sessionCookie env token (Just (lvSessionMax live))))
                                        liftIO (aeLog env ("sign-in ok user=" <> credUser cred))
                                        status status204
                                    else do
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
    user <- requireUser env
    json (object ["user" .= user])

-- | 读设置：文件值、启动生效值与内置默认一起给出
settingsH :: AppEnv -> ActionM ()
settingsH env = do
    user <- requireUser env
    cred <- liftIO (readCredential env)
    fileValues <- liftIO (readSettingsFile (aeSettingsFile env))
    envSet <- liftIO (mapM (isSetEnv . siEnv) settingCatalogue)
    let owner = sameAccount user (credUser cred)
    json
        ( object
            [ "file" .= aeSettingsFile env
            , "account" .= credUser cred
            , "owner" .= owner
            , "items" .= zipWith (itemJson (aeEffective env) fileValues owner) settingCatalogue envSet
            ]
        )

-- | 一个配置项在界面上的样子
itemJson :: Map.Map Text Text -> Map.Map Text Text -> Bool -> SettingItem -> Bool -> A.Value
itemJson effectiveValues fileValues owner spec envSet =
    object
        [ "key" .= siKey spec
        , "label" .= siLabel spec
        , "group" .= siGroup spec
        , "kind" .= siKind spec
        , "default" .= siDefault spec
        , "value" .= shownValue
        , "source" .= source
        , "rootOnly" .= siRootOnly spec
        , "editable" .= (owner || not (siRootOnly spec))
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
    hasValue = fromFile /= Nothing || envSet
    source :: Text
    source
        | secret = if hasValue then "configured" else "not set"
        | fromFile /= Nothing = "settings file"
        | envSet = "environment"
        | otherwise = "default"

-- | 这个环境变量现在是设着的吗
isSetEnv :: Text -> IO Bool
isSetEnv name = do
    found <- lookupEnv (T.unpack name)
    pure (maybe False (not . null) found)

-- | 改设置：写配置文件并让可热改项立即生效
putSettingsH :: AppEnv -> ActionM ()
putSettingsH env = do
    user <- requireUser env
    withJsonObject env $ \o -> case settingsUpdates o of
        Left err -> reply400 "bad_request" err
        Right updates -> do
            cred <- liftIO (readCredential env)
            let forbidden =
                    [ k
                    | k <- Map.keys updates
                    , maybe False siRootOnly (findSetting k)
                    , not (sameAccount user (credUser cred))
                    ]
            if not (null forbidden)
                then reply403 ("only the configured account (" <> credUser cred <> ") may change: " <> T.intercalate ", " forbidden)
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
    value <- liftIO (readUiSettings (aeUiSettingsFile env))
    json value

-- | 整份覆盖前端 IDE 设置，校验不过一律 400
putUiSettingsH :: AppEnv -> ActionM ()
putUiSettingsH env = do
    _ <- requireUser env
    rawBody <- body
    limit <- liftIO (liveBodyLimit env)
    if BL.length rawBody > fromIntegral limit
        then reply413
        else case eitherDecode rawBody of
            Left _ -> reply400 "bad_request" "request body is not valid JSON"
            Right payload -> case validateUiSettings payload of
                Left err -> reply400 "bad_request" (T.pack err)
                Right valid -> do
                    written <- liftIO (writeUiSettings (aeUiSettingsFile env) valid)
                    case written of
                        Left err -> reply500 (T.pack err)
                        Right () -> status status204

-- | 改口令：验原口令，并踢掉其它会话
changePasswordH :: AppEnv -> ActionM ()
changePasswordH env = do
    user <- requireUser env
    token <- requestSessionToken
    withJsonObject env $ \o -> do
        cred <- liftIO (readCredential env)
        case (textField "current" o, textField "next" o) of
            (Left err, _) -> reply400 "bad_request" err
            (_, Left err) -> reply400 "bad_request" err
            (Right current, Right newPassword)
                | not (sameAccount user (credUser cred)) -> reply403 "only the configured account can change its password"
                | not (verifyPassword (credEncoded cred) current) -> reply400 "bad_password" "the current password is incorrect"
                | T.length newPassword < minPasswordLength -> reply400 "bad_request" (T.pack ("the new password must be at least " ++ show minPasswordLength ++ " characters"))
                | T.length newPassword > 256 -> reply400 "bad_request" "the new password is too long"
                | newPassword == current -> reply400 "bad_request" "the new password must differ from the current one"
                | otherwise -> do
                    encoded <- liftIO (hashPassword newPassword)
                    if not (hashLooksValid encoded)
                        then reply500 "could not hash the new password"
                        else do
                            currentFile <- liftIO (readSettingsFile (aeSettingsFile env))
                            let merged = Map.insert "password-hash" encoded (Map.insert "password" "" currentFile)
                            written <- liftIO (writeSettingsFile (aeSettingsFile env) merged)
                            case written of
                                Left err -> reply500 (T.pack err)
                                Right () -> do
                                    liftIO (writeCredential env cred{credEncoded = encoded})
                                    removed <- case token of
                                        Nothing -> pure 0
                                        Just t -> liftIO (deleteOtherSessions (aeSessions env) t)
                                    liftIO (aeLog env ("password changed by " <> user <> " (" <> T.pack (show removed) <> " other sessions signed out)"))
                                    status status204

-- | 口令最短长度（太短的口令不值得存）
minPasswordLength :: Int
minPasswordLength = 8

-- | 两个账号名是不是同一个（大小写不敏感）
sameAccount :: Text -> Text -> Bool
sameAccount a b = T.toLower a == T.toLower b

-- | 现在用的凭据
readCredential :: AppEnv -> IO Credential
readCredential = readIORef . aeCredential

-- | 换掉进程里的凭据（改口令）
writeCredential :: AppEnv -> Credential -> IO ()
writeCredential env = writeIORef (aeCredential env)

-- | 按 key 找目录项
findSetting :: Text -> Maybe SettingItem
findSetting key = case [i | i <- settingCatalogue, siKey i == key] of
    (i : _) -> Just i
    [] -> Nothing

-- | 把能热改的设置应用到当前进程
applyLiveKeys :: AppEnv -> Map.Map Text Text -> IO [Text]
applyLiveKeys env updates = do
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

-- | 表清单（列 / 行数 / 索引 / 列统计）
tablesH :: AppEnv -> ActionM ()
tablesH env = do
    _ <- requireUser env
    result <- liftIO (beCatalog (aeBackend env))
    case result of
        Left e -> engineError e
        Right infos -> json (map tableInfoJson infos)

-- | 单表结构
tableH :: AppEnv -> ActionM ()
tableH env = do
    _ <- requireUser env
    name <- pathParam "t"
    if not (isIdentifier name)
        then reply400 "bad_request" "invalid table name"
        else do
            result <- liftIO (beCatalog (aeBackend env))
            case result of
                Left e -> engineError e
                Right infos -> case findTable name infos of
                    Nothing -> reply404
                    Just info -> json (tableInfoJson info)

-- | 分页浏览一张表，支持排序与等值过滤
rowsH :: AppEnv -> ActionM ()
rowsH env = do
    _ <- requireUser env
    name <- pathParam "t"
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
    -- | 只认 asc/desc，否则算 asc
    dirOf given = case fmap (T.toLower . T.strip) given of
        Just "desc" -> False
        _ -> True

-- | 建表：按列结构拼 CREATE TABLE
createTableH :: AppEnv -> ActionM ()
createTableH env = do
    _ <- requireUser env
    withJsonObject env $ \o -> case decodeCreateTable o of
        Left err -> reply400 "bad_request" err
        Right spec -> case createTableSql spec of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite env sql

-- | 删表
dropTableH :: AppEnv -> ActionM ()
dropTableH env = do
    _ <- requireUser env
    name <- pathParam "t"
    case dropTableSql name of
        Left err -> reply400 "bad_request" (T.pack err)
        Right sql -> runWrite env sql

-- | 插一行，所有列必须给全
insertRowH :: AppEnv -> ActionM ()
insertRowH env = do
    _ <- requireUser env
    name <- pathParam "t"
    info <- requireTable env name
    withJsonObject env $ \o -> case valuesField o >>= readRowValues True (tiColumns info) of
        Left err -> reply400 "bad_request" err
        Right row -> case insertRowSql name row of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite env sql

-- | 改一行，只改给出来的列
updateRowH :: AppEnv -> ActionM ()
updateRowH env = do
    _ <- requireUser env
    name <- pathParam "t"
    key <- rowIdParam
    info <- requireTable env name
    withJsonObject env $ \o -> case valuesField o >>= readRowValues False (tiColumns info) of
        Left err -> reply400 "bad_request" err
        Right assigns -> case updateRowSql name key assigns of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite env sql

-- | 删一行
deleteRowH :: AppEnv -> ActionM ()
deleteRowH env = do
    _ <- requireUser env
    name <- pathParam "t"
    key <- rowIdParam
    _ <- requireTable env name
    case deleteRowSql name key of
        Left err -> reply400 "bad_request" (T.pack err)
        Right sql -> runWrite env sql

-- | 建索引，索引列必须整数且唯一
createIndexH :: AppEnv -> ActionM ()
createIndexH env = do
    _ <- requireUser env
    name <- pathParam "t"
    info <- requireTable env name
    withJsonObject env $ \o -> case textField "column" o of
        Left err -> reply400 "bad_request" err
        Right column -> case columnTypeOf (columnTypeText (tiColumns info) column) of
            Nothing -> reply400 "bad_request" ("unknown column: " <> column)
            Just ty
                | ty /= TInt -> reply400 "bad_request" "only integer columns can be indexed"
                | otherwise -> case createIndexSql name column of
                    Left err -> reply400 "bad_request" (T.pack err)
                    Right sql -> runWrite env sql

-- | 删索引（内建索引不放行：存储层也会拒，这里先给一句清楚的话）
dropIndexH :: AppEnv -> ActionM ()
dropIndexH env = do
    _ <- requireUser env
    name <- pathParam "t"
    column <- pathParam "col"
    _ <- requireTable env name
    if column == T.pack builtInIndexColumn
        then reply400 "bad_request" "the built-in id index cannot be dropped"
        else case dropIndexSql name column of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite env sql

-- | 删列，内建 id 列不放行
dropColumnH :: AppEnv -> ActionM ()
dropColumnH env = do
    _ <- requireUser env
    name <- pathParam "t"
    column <- pathParam "col"
    _ <- requireTable env name
    if column == T.pack builtInIndexColumn
        then reply400 "bad_request" "the built-in id column cannot be dropped"
        else case dropColumnSql name column of
            Left err -> reply400 "bad_request" (T.pack err)
            Right sql -> runWrite env sql

-- | 一键灌演示数据（缺什么补什么，可以反复点）
demoDataH :: AppEnv -> ActionM ()
demoDataH env = do
    _ <- requireUser env
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

-- | 执行任意一条语句
queryH :: AppEnv -> ActionM ()
queryH env = do
    user <- requireUser env
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
                                                ]
                                            )

-- | 拿当前 Session 用户；没有就 401
requireUser :: AppEnv -> ActionM Text
requireUser env = do
    token <- requestSessionToken
    found <- case token of
        Nothing -> pure Nothing
        Just t -> liftIO (lookupSession (aeSessions env) t)
    case found of
        Just user -> pure user
        Nothing -> do
            status status401
            json (apiErrorJson "unauthorized" "sign in first")
            finish

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
    -- | 这一对是不是 name=value，是且值非空就取出值
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
valueJson (VInt n) = A.Number (fromIntegral n)
valueJson (VStr s) = A.String (T.pack s)
valueJson (VBool b) = A.Bool b

-- | 一张表的线上信息：列、行数、索引与统计
tableInfoJson :: TableInfo -> A.Value
tableInfoJson info =
    object
        [ "table" .= tiTable info
        , "rowCount" .= tiRows info
        , "indexes" .= [object ["column" .= c, "builtIn" .= (c == builtInIndexColumn)] | c <- tiIndexes info]
        , "columns" .= map columnJson (tiColumns info)
        ]
  where
    -- | 一列的完整描述：角色与统计
    columnJson sc =
        object
            [ "name" .= scName sc
            , "type" .= scType sc
            , "primaryKey" .= isPrimaryKey
            , "indexed" .= isIndexed
            , "prime" .= (isPrimaryKey || isIndexed)
            , "distinct" .= distinctCount
            , "statsCapped" .= statsCapped
            ]
      where
        name = scName sc
        isPrimaryKey = name == builtInIndexColumn && scType sc == "int"
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
