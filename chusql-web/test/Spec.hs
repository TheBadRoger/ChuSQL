{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.Interface.Link (closeClient, connectClient)
import ChuSQL.Interface.Auth (SessionPolicy (..), defaultSessionPolicy, lookupPayload, setPayloadPolicy)
import ChuSQL.Interface.RateLimit (newRateLimiter)
import ChuSQL.Interface.Session (authenticateSession, newSession, runStatement)
import qualified ChuSQL.Interface.Actions as Actions
import qualified ChuSQL.Core.Model as Model
import ChuSQL.Core.Runtime
import ChuSQL.Interface.Settings (
    applySettings,
    defaultOf,
    isLockedSetting,
    isRestartRequired,
    liveKeys,
    readSettingsFile,
    settingCatalogue,
    writeSettingsFile,
 )
import ChuSQL.Web.API (
    AppEnv (..),
    WebSession (..),
    Live (..),
    defaultLive,
    isIdentifier,
    newAppEnvAt,
    parseCookieHeader,
    sessionCookieName,
    webApp,
 )
import ChuSQL.Web.Static (contentTypeOf, safeRelative)
import ChuSQL.Web.UISettings (validateUISettings)
import Control.Concurrent (forkFinally, forkIO)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, throwIO, try)
import Control.Monad (when)
import Data.Aeson (encode, object, (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Char (isDigit)
import Data.List (isInfixOf)
import Data.IORef (readIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (getCurrentTime)
import Data.Unique (hashUnique, newUnique)
import qualified Data.Vector as V
import Gates (gateSpec)
import Network.HTTP.Client (
    Manager,
    Request (method, requestBody, requestHeaders),
    RequestBody (RequestBodyLBS),
    Response (responseBody, responseHeaders, responseStatus),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
 )
import Network.HTTP.Types (Header, Method, methodGet, methodPost, statusCode)
import qualified Network.Wai as Wai
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Test as WT
import System.Directory (
    createDirectoryIfMissing,
    findExecutable,
    getTemporaryDirectory,
    removeFile,
    removePathForcibly,
 )
import System.Environment (lookupEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.Info (os)
import System.IO (Handle, hGetLine)
import System.Process (
    CreateProcess (std_err, std_out),
    ProcessHandle,
    StdStream (CreatePipe, NoStream),
    createProcess,
    proc,
    readProcessWithExitCode,
    terminateProcess,
    waitForProcess,
 )
import Test.Hspec
import Test.Hspec.Wai hiding (pendingWith)

-- ChuSQL Web 测试：纯函数、静态交付、门禁，以及打到真 server 的 HTTP 与 socket 用例。

-- | 夹具库名：测试默认选中的库
testDatabaseName :: Text
testDatabaseName = "test"

-- | 夹具管理员
adminUser :: Text
adminUser = "admin"

-- | 夹具管理员口令
adminPassword :: Text
adminPassword = "s3cret"

-- | 换成别的库（同名的旧头去掉）
withDb :: Text -> [Header] -> [Header]
withDb name hs = ("X-ChuSQL-Database", TE.encodeUtf8 name) : filter ((/= "X-ChuSQL-Database") . fst) hs

-- | with 块的会话别名，状态放 Application
type TestSession a = WaiSession Wai.Application a

-- | with 的变体：把 Application 放进会话状态
withApp :: IO Wai.Application -> SpecWith (Arg (WaiExpectation Wai.Application)) -> Spec
withApp mkApp = withState ((\app -> (app, app)) <$> mkApp)

-- | 纯断言抬进 WaiSession
check :: IO () -> TestSession ()
check = liftIO

-- | 加上 JSON 内容类型头
jsonHeaders :: [Header] -> [Header]
jsonHeaders hs = ("Content-Type", "application/json") : hs

-- | 拼出会话 Cookie 头
cookieHeader :: Text -> Header
cookieHeader token = ("Cookie", TE.encodeUtf8 (sessionCookieName <> "=" <> token))

-- | 发独立会话的 WAI 请求，不共享 cookie jar
requestAs :: Method -> BS.ByteString -> [Header] -> BL.ByteString -> TestSession WT.SResponse
requestAs verb path hs payload = do
    app <- getState
    liftIO $ WT.runSession send app
  where
    -- | 清空 cookie jar 后发出请求
    send = do
        WT.modifyClientCookies (const Map.empty)
        WT.srequest $
            WT.SRequest
                ( WT.setPath
                    ( WT.defaultRequest
                        { Wai.requestMethod = verb
                        , Wai.requestHeaders = hs
                        , Wai.requestBodyLength = Wai.KnownLength (fromIntegral (BL.length payload))
                        }
                    )
                    path
                )
                payload

-- | 带头发 GET 请求
getAs :: [Header] -> BS.ByteString -> TestSession WT.SResponse
getAs hs path = requestAs methodGet path hs BL.empty

-- | 带 JSON 头发 POST 请求
postAs :: [Header] -> BS.ByteString -> BL.ByteString -> TestSession WT.SResponse
postAs hs path payload = requestAs methodPost path (jsonHeaders hs) payload

-- | 带 JSON 头发 PUT 请求
putAs :: [Header] -> BS.ByteString -> BL.ByteString -> TestSession WT.SResponse
putAs hs path payload = requestAs "PUT" path (jsonHeaders hs) payload

-- | 带 JSON 头发 PATCH 请求
patchAs :: [Header] -> BS.ByteString -> BL.ByteString -> TestSession WT.SResponse
patchAs hs path payload = requestAs "PATCH" path (jsonHeaders hs) payload

-- | 带头发 DELETE 请求
deleteAs :: [Header] -> BS.ByteString -> TestSession WT.SResponse
deleteAs hs path = requestAs "DELETE" path hs BL.empty

-- | 用账号口令登录
loginAs :: Text -> Text -> TestSession WT.SResponse
loginAs user password =
    requestAs
        methodPost
        "/api/login"
        (jsonHeaders [])
        (encode (object ["user" .= user, "password" .= password]))

-- | 登录并返回会话 Cookie 请求头
signIn :: Text -> Text -> TestSession [Header]
signIn user password = do
    res <- loginAs user password
    expectStatusWith "sign-in" res 204
    case sessionCookieOf res of
        Just token -> pure [cookieHeader token]
        Nothing -> do
            check (expectationFailure "sign-in succeeded without a session cookie")
            pure []

-- | 以管理员登录并取请求头
adminHeaders :: TestSession [Header]
adminHeaders = signIn adminUser adminPassword

-- | 已登录的普通账号
ordinaryHeaders :: Text -> Text -> TestSession [Header]
ordinaryHeaders user password = do
    res <- loginAs user password
    expectStatusWith "ordinary sign-in" res 204
    case sessionCookieOf res of
        Just token -> pure [cookieHeader token]
        Nothing -> do
            check (expectationFailure "ordinary sign-in without a session cookie")
            pure []

-- | 建库，已存在就算就位
ensureDatabase :: [Header] -> Text -> TestSession ()
ensureDatabase hs name = do
    listed <- getAs hs "/api/databases"
    expectStatusWith "list databases" listed 200
    let names = map asText (items (jsonBody listed))
    when (name `notElem` names) $ do
        created <- postAs hs "/api/databases" (encode (object ["name" .= name]))
        expectStatusWith ("create database " <> T.unpack name) created 200

-- | 建表：已存在就当就位
ensureTable :: [Header] -> Text -> BL.ByteString -> TestSession ()
ensureTable hs name body = do
    listed <- getAs (withDb testDatabaseName hs) "/api/tables"
    expectStatusWith "list tables" listed 200
    let names = map (asText . at "table") (items (jsonBody listed))
    when (name `notElem` names) $ do
        created <- postAs (withDb testDatabaseName hs) "/api/tables" body
        expectStatusWith ("create table " <> T.unpack name) created 200

-- | 建账号：重名（409）也算就位
ensureAccount :: [Header] -> Text -> Text -> TestSession ()
ensureAccount hs user password = do
    created <-
        postAs
            (withDb "system" hs)
            "/api/tables/__system_users/rows"
            (encode (object ["values" .= object ["user" .= user, "password" .= password]]))
    check (statusCode (WT.simpleStatus created) `shouldSatisfy` (`elem` [200, 409]))

-- | 列定义
column :: Text -> Text -> A.Value
column name ty = object ["name" .= name, "type" .= ty]

-- | 响应体重解成 JSON
jsonBody :: WT.SResponse -> A.Value
jsonBody res = fromMaybe A.Null (A.decode (WT.simpleBody res))

-- | 取字段
field :: Text -> A.Value -> Maybe A.Value
field name (A.Object o) = KM.lookup (K.fromText name) o
field _ _ = Nothing

-- | 取字段（没有给 Null）
at :: Text -> A.Value -> A.Value
at name value = fromMaybe A.Null (field name value)

-- | 当数组看
items :: A.Value -> [A.Value]
items (A.Array xs) = V.toList xs
items _ = []

-- | 当文本看
asText :: A.Value -> Text
asText (A.String t) = t
asText _ = ""

-- | 当整数看
asInt :: A.Value -> Int
asInt (A.Number n) = round n
asInt _ = -1

-- | 当布尔看
asBool :: A.Value -> Bool
asBool (A.Bool b) = b
asBool _ = False

-- | 第 i 个元素，越界给 Nothing
nth :: Int -> [a] -> Maybe a
nth i xs = case drop i xs of
    (x : _) -> Just x
    [] -> Nothing

-- | 数组字段的长度
arrayLen :: A.Value -> Int
arrayLen = length . items

-- | 第一行（位置化数组）
firstRow :: WT.SResponse -> [A.Value]
firstRow res = case items (at "rows" (jsonBody res)) of
    (A.Array xs : _) -> V.toList xs
    _ -> []

-- | items 数组里按键找一项（找不到给 Null）
headEntry :: Text -> [A.Value] -> A.Value
headEntry key entries = case [e | e <- entries, asText (at "key" e) == key] of
    (e : _) -> e
    [] -> A.Null

-- | 响应头
responseHeader :: Header -> WT.SResponse -> Maybe BS.ByteString
responseHeader (name, _) res = lookup name (WT.simpleHeaders res)

-- | 取 Set-Cookie 里的会话令牌
sessionCookieOf :: WT.SResponse -> Maybe Text
sessionCookieOf res = do
    raw <- lookup "Set-Cookie" (WT.simpleHeaders res)
    parseCookieHeader sessionCookieName (TE.decodeUtf8 raw)

-- | 状态码断言，失败时把响应体和状态码一起报出来（定位用）
expectStatusWith :: String -> WT.SResponse -> Int -> TestSession ()
expectStatusWith label res code =
    check $
        if statusCode (WT.simpleStatus res) == code
            then pure ()
            else
                expectationFailure
                    ( label
                        ++ ": expected "
                        ++ show code
                        ++ " but got "
                        ++ show (statusCode (WT.simpleStatus res))
                        ++ " body="
                        ++ show (WT.simpleBody res)
                    )

-- | Either 判成
isRightE :: Either a b -> Bool
isRightE = either (const False) (const True)

-- | Either 判错
isLeftE :: Either a b -> Bool
isLeftE = either (const True) (const False)

-- | 临时文件
tempFilePath :: String -> IO FilePath
tempFilePath name = do
    tmp <- getTemporaryDirectory
    uniq <- uniqueSuffix
    pure (tmp </> ("chusql-web-test-" ++ name ++ "-" ++ uniq ++ ".toml"))

-- | 非重复后缀
uniqueSuffix :: IO String
uniqueSuffix = do
    u <- newUnique
    pure (show (hashUnique u))

-- | 文件不存在也算成功
removeIfExists :: FilePath -> IO ()
removeIfExists path = do
    _ <- try (removeFile path) :: IO (Either IOException ())
    pure ()

-- | 临时静态目录，内容固定便于断言
makeStaticDir :: IO FilePath
makeStaticDir = do
    tmp <- getTemporaryDirectory
    uniq <- uniqueSuffix
    let dir = tmp </> ("chusql-web-static-" ++ uniq)
    createDirectoryIfMissing True dir
    BS.writeFile (dir </> "index.html") "<!DOCTYPE html><html lang=\"zh-CN\"><head></head><body>ChuSQL Console</body></html>"
    BS.writeFile (dir </> "app.js") "// test bundle\n"
    BS.writeFile (dir </> "style.css") "body{}\n"
    BS.writeFile (dir </> "secret.token") "do-not-serve\n"
    pure dir

-- | 只做静态交付的环境，不连 server
staticOnlyEnv :: FilePath -> IO AppEnv
staticOnlyEnv staticDir = do
    limiter <- newRateLimiter getCurrentTime 5 (5 * 60)
    settings <- tempFilePath "static"
    removeIfExists settings
    newAppEnvAt settings "127.0.0.1" 1 limiter staticDir

-- 真 server 世界 --------------------------------------------------------------

-- | 一个真 server 进程及其端口、配置和数据目录
data LiveWorld = LiveWorld
    { lwProcess :: ProcessHandle
    , lwPort :: Int
    , lwConfig :: FilePath
    , lwDataDir :: FilePath
    }

-- | 起 server 进程并从横幅读回端口
startLiveWorld :: String -> IO LiveWorld
startLiveWorld label = do
    bin <- locateServerExe
    tmp <- getTemporaryDirectory
    uniq <- uniqueSuffix
    let root = tmp </> ("chusql-web-live-" ++ label ++ "-" ++ uniq)
        dataDir = root </> "data"
        config = root </> "settings.toml"
    createDirectoryIfMissing True dataDir
    writeFile config (serverConfigText dataDir)
    seedSystemCatalog config
    (_, Just out, _, process) <-
        createProcess (proc bin ["--config", config, "--user", T.unpack adminUser]){std_out = CreatePipe, std_err = NoStream}
    port <- readListeningPort out
    _ <- forkIO (drainHandle out)
    setAdminPassword port
    pure (LiveWorld process port config dataDir)

-- | 首启的管理员还没有口令：先用空口令登录，再按业务流程设一个
setAdminPassword :: Int -> IO ()
setAdminPassword port = do
    opened <- connectClient "127.0.0.1" port
    case opened of
        Left err -> fail ("fixture server refused the connection: " ++ T.unpack err)
        Right client -> do
            session <- newSession client
            signed <- authenticateSession session adminUser ""
            case signed of
                Left err -> fail ("fixture administrator cannot sign in: " ++ T.unpack err)
                Right () -> do
                    changed <- runStatement session ("ALTER USER " <> adminUser <> " IDENTIFIED BY '" <> adminPassword <> "'")
                    case changed of
                        Left err -> fail ("fixture administrator keeps no password: " ++ T.unpack err)
                        Right _ -> pure ()
            closeClient client

-- | 生成夹具 server 的配置文本
serverConfigText :: FilePath -> String
serverConfigText dataDir =
    unlines
        [ "[web]"
        , "password-min-length = 8"
        , "password-classes = 2"
        , ""
        , "[server]"
        , "listen_host = \"127.0.0.1\""
        , "port = 0"
        , ""
        , "[storage]"
        , "data_dir = \"" ++ slashed dataDir ++ "\""
        ]
  where
    -- | 反斜杠换成正斜杠
    slashed = map (\c -> if c == '\\' then '/' else c)

-- | 逐行读 stdout 直到读出端口
readListeningPort :: Handle -> IO Int
readListeningPort handle = do
    line <- hGetLine handle
    case portFromBanner line of
        Just port -> pure port
        Nothing -> readListeningPort handle

-- | 取一行里最后一串数字
portFromBanner :: String -> Maybe Int
portFromBanner line
    | not ("listening:" `isInfixOf` line) = Nothing
    | otherwise = case span isDigit (dropWhile (not . isDigit) (reverse line)) of
        ([], _) -> Nothing
        (digits, _) -> Just (read (reverse digits))

-- | 抽干 server 的 stdout
drainHandle :: Handle -> IO ()
drainHandle handle = do
    result <- try (hGetLine handle) :: IO (Either IOException String)
    case result of
        Left _ -> pure ()
        Right _ -> drainHandle handle

-- | 按平台补上 .exe 后缀
binaryName :: String -> String
binaryName stem = if os == "mingw32" then stem ++ ".exe" else stem

-- | 在 PATH 上查找 chusql-server
locateServerExe :: IO FilePath
locateServerExe = do
    found <- findExecutable (binaryName "chusql-server")
    case found of
        Just path -> pure path
        Nothing -> fail "chusql-server was not found on PATH: run `stack build` first"

-- | 全新的数据目录还没有系统目录：先跑引导程序建表并补一个免密管理员，
-- server 才肯启动（未引导的目录会被它拒绝）。
seedSystemCatalog :: FilePath -> IO ()
seedSystemCatalog config = do
    bootstrap <- locateBootstrapExe
    (code, out, errOut) <- readProcessWithExitCode bootstrap ["--config", config, "--passwordless", "--user", T.unpack adminUser] ""
    case code of
        ExitSuccess -> pure ()
        _ -> fail ("the fixture bootstrap program failed (exit " ++ show code ++ "): " ++ out ++ errOut)

-- | 找引导程序：先环境变量，再 PATH
locateBootstrapExe :: IO FilePath
locateBootstrapExe = do
    fromEnv <- lookupEnv "CHUSQL_BOOTSTRAP_EXE"
    case fromEnv of
        Just path -> pure path
        Nothing -> do
            found <- findExecutable (binaryName "csql-bootstrap")
            case found of
                Just path -> pure path
                Nothing -> fail "csql-bootstrap was not found on PATH: run `stack build` first"

-- | 构造指向真 server 的 Web 环境
liveEnv :: LiveWorld -> FilePath -> Int -> IO AppEnv
liveEnv world staticDir attempts = do
    limiter <- newRateLimiter getCurrentTime attempts (5 * 60)
    uiPath <- tempFilePath "ui-settings"
    removeIfExists uiPath
    base <- newAppEnvAt (lwConfig world) "127.0.0.1" (lwPort world) limiter staticDir
    pure base{aeUISettingsFile = uiPath}

-- | 收摊：杀进程、清数据目录
stopServer :: LiveWorld -> IO ()
stopServer world = do
    terminateProcess (lwProcess world)
    _ <- try (waitForProcess (lwProcess world)) :: IO (Either IOException ExitCode)
    _ <- try (removePathForcibly (lwDataDir world)) :: IO (Either IOException ())
    pure ()

-- 纯函数 ---------------------------------------------------------------------

-- | 纯函数用例
unitSpec :: Spec
unitSpec = describe "pure helpers" $ do
    it "rejects malformed runtime values before producing write SQL" $ do
        case resolveType builtinTypes (listType intType) of
            Left err -> expectationFailure err
            Right descriptor ->
                Actions.insertRowSql "t" [("xs", Model.VRuntime (descriptorId descriptor) (RList [RString "wrong"]))]
                    `shouldSatisfy` either (const True) (const False)
    it "escapes nested runtime string literals" $ do
        case resolveType builtinTypes (listType stringType) of
            Left err -> expectationFailure err
            Right descriptor -> Actions.sqlLiteral (Model.VRuntime (descriptorId descriptor) (RList [RString "a'b"]))
                `shouldBe` Right "LIST<String>('a''b')"
    it "parses the session cookie out of a Cookie header" $ do
        parseCookieHeader sessionCookieName "a=1; chusql_session=abc123; b=2" `shouldBe` Just "abc123"
        parseCookieHeader sessionCookieName "other=1" `shouldBe` Nothing
        parseCookieHeader sessionCookieName "" `shouldBe` Nothing
    it "accepts plain identifiers and rejects everything else" $ do
        isIdentifier "users" `shouldBe` True
        isIdentifier "_9" `shouldBe` True
        isIdentifier "" `shouldBe` False
        isIdentifier "1users" `shouldBe` False
        isIdentifier "user-name" `shouldBe` False
        isIdentifier (T.replicate 65 "a") `shouldBe` False
    it "keeps static delivery inside the asset whitelist" $ do
        safeRelative "index.html" `shouldBe` Just "index.html"
        safeRelative "app.js" `shouldBe` Just "app.js"
        safeRelative "../secret.html" `shouldBe` Nothing
        safeRelative "asset/app.js" `shouldBe` Nothing
        safeRelative "secret.token" `shouldBe` Nothing
        safeRelative "index.htmlx" `shouldBe` Nothing
    it "maps extensions to content types" $ do
        contentTypeOf "index.html" `shouldBe` "text/html; charset=utf-8"
        contentTypeOf "app.js" `shouldBe` "text/javascript; charset=utf-8"
        contentTypeOf "x.woff2" `shouldBe` "font/woff2"
        contentTypeOf "x.bin" `shouldBe` "application/octet-stream"
    it "ships the documented live defaults" $ do
        lvMaxRows defaultLive `shouldBe` 1000
        lvPageSize defaultLive `shouldBe` 25
        lvBodyLimit defaultLive `shouldBe` 65536
    it "round-trips settings through a real TOML file" $ do
        path <- tempFilePath "settings"
        removeIfExists path
        writeSettingsFile path (Map.fromList [("rows-per-page", "50"), ("log-files", "./logs")])
            >>= (`shouldSatisfy` isRightE)
        values <- readSettingsFile path
        Map.lookup "rows-per-page" values `shouldBe` Just "50"
        Map.lookup "log-files" values `shouldBe` Just "./logs"
    it "rejects unknown settings and out-of-range limits" $ do
        applySettings Map.empty (Map.fromList [("nope", "1")]) `shouldSatisfy` isLeftE
        applySettings Map.empty (Map.fromList [("password-min-length", "3")]) `shouldSatisfy` isLeftE
        applySettings Map.empty (Map.fromList [("password-min-length", "12")]) `shouldSatisfy` isRightE
    it "lists the live keys and locks nothing" $ do
        isLockedSetting "user" `shouldBe` False
        isLockedSetting "rows-per-page" `shouldBe` False
        ("rows-per-page" `elem` liveKeys) `shouldBe` True
        length settingCatalogue `shouldSatisfy` (> 20)
        defaultOf "rows-per-page" `shouldBe` "25"
        defaultOf "log-files" `shouldBe` "./logs"
        isRestartRequired "log-files" `shouldBe` True
    it "validates IDE settings" $ do
        validateUISettings (object ["sqlFontSize" .= (13 :: Int), "minimap" .= False])
            `shouldSatisfy` isRightE
        validateUISettings (object ["nope" .= (1 :: Int)]) `shouldSatisfy` isLeftE
        validateUISettings (A.String "nope") `shouldSatisfy` isLeftE

-- 静态交付 -------------------------------------------------------------------

-- | 静态交付用例
staticSpec :: FilePath -> Spec
staticSpec staticDir = describe "static delivery" $ do
    env <- runIO (staticOnlyEnv staticDir)
    withApp (webApp env) $ do
        it "serves the console page with a content-security-policy" $ do
            res <- requestAs methodGet "/" [] BL.empty
            expectStatusWith "index" res 200
            check (responseHeader ("Content-Type", "") res `shouldBe` Just "text/html; charset=utf-8")
            check (responseHeader ("Content-Security-Policy", "") res `shouldSatisfy` (/= Nothing))
            check (BSC.isInfixOf "chusql-style-nonce" (BL.toStrict (WT.simpleBody res)) `shouldBe` True)
        it "serves an asset inside the whitelist" $ do
            res <- requestAs methodGet "/static/app.js" [] BL.empty
            expectStatusWith "app.js" res 200
            check (responseHeader ("Content-Type", "") res `shouldBe` Just "text/javascript; charset=utf-8")
        it "refuses assets outside the whitelist" $ do
            res <- requestAs methodGet "/static/secret.token" [] BL.empty
            expectStatusWith "secret.token" res 404
        it "answers unknown routes with a JSON 404" $ do
            res <- requestAs methodGet "/api/nope" [] BL.empty
            expectStatusWith "unknown route" res 404
            check (asText (at "error" (jsonBody res)) `shouldBe` "not_found")
        it "reports the process is alive without touching the storage link" $ do
            res <- requestAs methodGet "/api/health" [] BL.empty
            expectStatusWith "health" res 200
            check (at "status" (jsonBody res) `shouldBe` A.String "ok")

-- 会话 -----------------------------------------------------------------------

-- | 登录与会话用例
sessionSpec :: AppEnv -> Spec
sessionSpec env = describe "sign-in and sessions (web -> TCP -> server)" $ do
    withApp (webApp env) $ do
        it "refuses protected endpoints without a cookie" $ do
            res <- requestAs methodGet "/api/databases" [] BL.empty
            expectStatusWith "no cookie" res 401
            check (asText (at "error" (jsonBody res)) `shouldBe` "unauthorized")
        it "refuses a wrong password" $ do
            res <- loginAs adminUser "not-the-password"
            expectStatusWith "wrong password" res 401
        it "signs in, reports the session policy from the server, and signs out" $ do
            res <- loginAs adminUser adminPassword
            expectStatusWith "sign-in" res 204
            token <- case sessionCookieOf res of
                Just t -> pure t
                Nothing -> do
                    check (expectationFailure "no session cookie after sign-in")
                    pure ""
            me <- getAs [cookieHeader token] "/api/session"
            expectStatusWith "session" me 200
            check (at "user" (jsonBody me) `shouldBe` A.String adminUser)
            check (at "administrator" (jsonBody me) `shouldBe` A.Bool True)
            check (asInt (at "minLength" (at "policy" (jsonBody me))) `shouldSatisfy` (>= 8))
            out <- requestAs "POST" "/api/logout" [cookieHeader token] BL.empty
            expectStatusWith "sign-out" out 204
            afterSignOut <- getAs [cookieHeader token] "/api/session"
            expectStatusWith "session after sign-out" afterSignOut 401
        it "reports the storage link through a fresh connection" $ do
            res <- requestAs methodGet "/api/status" [] BL.empty
            expectStatusWith "status" res 200
            check (at "storage" (jsonBody res) `shouldBe` A.String "up")

-- 门禁 -----------------------------------------------------------------------

-- | 门禁用例
gatingSpec :: AppEnv -> Spec
gatingSpec env = describe "gate keeping" $ do
    withApp (webApp env) $ do
        it "rejects an illegal database name in the request header" $ do
            hs <- adminHeaders
            res <- requestAs methodGet "/api/tables" (withDb "bad-name" hs) BL.empty
            expectStatusWith "illegal database" res 400
        it "rejects the account table outside the system database" $ do
            hs <- adminHeaders
            ensureDatabase hs testDatabaseName
            res <- getAs (withDb testDatabaseName hs) "/api/tables/__system_users/rows"
            expectStatusWith "account table outside system" res 400
        it "rejects role management without a cookie" $ do
            res <- getAs [] "/api/roles"
            expectStatusWith "roles without a cookie" res 401

-- 库、表、行 -----------------------------------------------------------------

-- | 库、表、行用例
catalogSpec :: AppEnv -> Spec
catalogSpec env = describe "databases, tables and rows" $ do
    withApp (webApp env) $ do
        it "creates a database, lists it and drops it" $ do
            hs <- adminHeaders
            created <- postAs hs "/api/databases" (encode (object ["name" .= ("shop" :: Text)]))
            expectStatusWith "create shop" created 200
            listed <- getAs hs "/api/databases"
            expectStatusWith "list databases" listed 200
            check (("shop" `elem` map asText (items (jsonBody listed))) `shouldBe` True)
            dropped <- deleteAs hs "/api/databases/shop"
            expectStatusWith "drop shop" dropped 200
        it "refuses to drop the system database" $ do
            hs <- adminHeaders
            res <- deleteAs hs "/api/databases/system"
            expectStatusWith "drop system" res 400
        it "creates a table, inserts a row, reads it back and updates it" $ do
            hs <- adminHeaders
            ensureDatabase hs testDatabaseName
            ensureTable
                hs
                "people"
                ( encode
                    ( object
                        [ "name" .= ("people" :: Text)
                        , "columns"
                            .= [ column "id" "int"
                               , column "name" "varchar(64)"
                               , column "note" "varchar(64)"
                               ]
                        ]
                    )
                )
            inserted <-
                postAs
                    (withDb testDatabaseName hs)
                    "/api/tables/people/rows"
                    ( encode
                        ( object
                            [ "values"
                                .= object
                                    [ "id" .= (1 :: Int)
                                    , "name" .= ("alice" :: Text)
                                    , "note" .= ("first" :: Text)
                                    ]
                            ]
                        )
                    )
            expectStatusWith "insert row" inserted 200
            rows <- getAs (withDb testDatabaseName hs) "/api/tables/people/rows?limit=10"
            expectStatusWith "read rows" rows 200
            check (asText (at "table" (jsonBody rows)) `shouldBe` "people")
            check (arrayLen (at "columns" (jsonBody rows)) `shouldBe` 3)
            check (arrayLen (at "rows" (jsonBody rows)) `shouldBe` 1)
            check (asInt (at "total" (jsonBody rows)) `shouldBe` 1)
            check (asInt (maybe A.Null id (nth 0 (firstRow rows))) `shouldBe` 1)
            check (asText (maybe A.Null id (nth 1 (firstRow rows))) `shouldBe` "alice")
            updated <-
                patchAs
                    (withDb testDatabaseName hs)
                    "/api/tables/people/rows/1"
                    (encode (object ["values" .= object ["note" .= ("changed" :: Text)]]))
            expectStatusWith "update row" updated 200
            again <- getAs (withDb testDatabaseName hs) "/api/tables/people/rows?limit=10"
            check (asText (maybe A.Null id (nth 2 (firstRow again))) `shouldBe` "changed")
        it "deletes the row and then the table" $ do
            hs <- adminHeaders
            ensureDatabase hs testDatabaseName
            ensureTable hs "scratch" (encode (object ["name" .= ("scratch" :: Text), "columns" .= [column "id" "int", column "label" "varchar(64)"]]))
            inserted <- postAs (withDb testDatabaseName hs) "/api/tables/scratch/rows" (encode (object ["values" .= object ["id" .= (2 :: Int), "label" .= ("x" :: Text)]]))
            expectStatusWith "insert scratch row" inserted 200
            deleted <- deleteAs (withDb testDatabaseName hs) "/api/tables/scratch/rows/2"
            expectStatusWith "delete row" deleted 200
            rows <- getAs (withDb testDatabaseName hs) "/api/tables/scratch/rows"
            check (asInt (at "total" (jsonBody rows)) `shouldBe` 0)
            dropped <- deleteAs (withDb testDatabaseName hs) "/api/tables/scratch"
            expectStatusWith "drop table" dropped 200
        it "manages indexes and columns of a table" $ do
            hs <- adminHeaders
            ensureDatabase hs testDatabaseName
            ensureTable hs "metrics" (encode (object ["name" .= ("metrics" :: Text), "columns" .= [column "id" "int", column "score" "int", column "tag" "varchar(64)"]]))
            made <- postAs (withDb testDatabaseName hs) "/api/tables/metrics/indexes" (encode (object ["column" .= ("score" :: Text)]))
            expectStatusWith "create index" made 200
            builtIn <- deleteAs (withDb testDatabaseName hs) "/api/tables/metrics/indexes/id"
            expectStatusWith "built-in index is refused" builtIn 400
            dropped <- deleteAs (withDb testDatabaseName hs) "/api/tables/metrics/indexes/score"
            expectStatusWith "drop index" dropped 200
            idColumn <- deleteAs (withDb testDatabaseName hs) "/api/tables/metrics/columns/id"
            expectStatusWith "built-in column is refused" idColumn 400
            columnDropped <- deleteAs (withDb testDatabaseName hs) "/api/tables/metrics/columns/tag"
            expectStatusWith "drop column" columnDropped 200

-- 设置与口令策略 -------------------------------------------------------------

-- | 设置与口令策略用例
settingsSpec :: AppEnv -> Spec
settingsSpec env = describe "settings and password policy" $ do
    withApp (webApp env) $ do
        it "shows the catalogue to the administrator" $ do
            hs <- adminHeaders
            res <- getAs hs "/api/settings"
            expectStatusWith "settings" res 200
            let entries = items (at "items" (jsonBody res))
            check (length entries `shouldSatisfy` (> 20))
            check (asText (at "value" (headEntry "rows-per-page" entries)) `shouldBe` "25")
            -- 设置目录里没有任何只读键（管理员名已不写进配置），新的日志目录项在列
            check (at "locked" (headEntry "rows-per-page" entries) `shouldBe` A.Bool False)
            check (asText (at "value" (headEntry "log-files" entries)) `shouldBe` "./logs")
        it "writes a setting, makes the server reload the policy, and reports it applied" $ do
            hs <- adminHeaders
            res <-
                putAs
                    hs
                    "/api/settings"
                    ( encode
                        ( object
                            [ "values"
                                .= object
                                    [ "password-min-length" .= ("16" :: Text)
                                    , "password-classes" .= ("3" :: Text)
                                    ]
                            ]
                        )
                    )
            expectStatusWith "write settings" res 200
            let applied = map asText (items (at "applied" (jsonBody res)))
            check (("password-min-length" `elem` applied) `shouldBe` True)
            me <- getAs hs "/api/session"
            check (asInt (at "minLength" (at "policy" (jsonBody me))) `shouldBe` 16)
            values <- liftIO (readSettingsFile (aeSettingsFile env))
            check (Map.lookup "password-min-length" values `shouldBe` Just "16")
            -- 管理员名不再落进配置文件：它由 --user 给
            check (Map.member "user" values `shouldBe` False)
        it "refuses unknown settings, including the retired administrator name" $ do
            hs <- adminHeaders
            unknown <- putAs hs "/api/settings" (encode (object ["values" .= object ["nope" .= ("1" :: Text)]]))
            expectStatusWith "unknown setting" unknown 400
            retired <- putAs hs "/api/settings" (encode (object ["values" .= object ["user" .= ("hacked" :: Text)]]))
            expectStatusWith "retired administrator name" retired 400

-- 账号表 ---------------------------------------------------------------------

-- | 账号表用例
accountSpec :: AppEnv -> Spec
accountSpec env = describe "the account table (system database, admin only)" $ do
    withApp (webApp env) $ do
        it "lists the account table in the system database" $ do
            hs <- adminHeaders
            res <- getAs (withDb "system" hs) "/api/tables"
            expectStatusWith "system tables" res 200
            check (("__system_users" `elem` map (asText . at "table") (items (jsonBody res))) `shouldBe` True)
        it "creates an account, changes its password, signs in as it and drops it" $ do
            hs <- adminHeaders
            ensureAccount hs "alice" "alice-Secret-1234"
            duplicate <-
                postAs
                    (withDb "system" hs)
                    "/api/tables/__system_users/rows"
                    (encode (object ["values" .= object ["user" .= ("alice" :: Text), "password" .= ("alice-Secret-9999" :: Text)]]))
            expectStatusWith "duplicate account" duplicate 409
            alice <- ordinaryHeaders "alice" "alice-Secret-1234"
            forbidden <- getAs (withDb "system" alice) "/api/tables"
            expectStatusWith "ordinary account cannot enter system" forbidden 403
            visible <- getAs alice "/api/databases"
            expectStatusWith "ordinary account lists databases" visible 200
            check (("system" `elem` map asText (items (jsonBody visible))) `shouldBe` False)
            changed <-
                patchAs
                    (withDb "system" hs)
                    "/api/tables/__system_users/rows/alice"
                    (encode (object ["values" .= object ["password" .= ("alice-Secret-5678" :: Text)]]))
            expectStatusWith "change password" changed 200
            _ <- ordinaryHeaders "alice" "alice-Secret-5678"
            dropped <- deleteAs (withDb "system" hs) "/api/tables/__system_users/rows/alice"
            expectStatusWith "drop account" dropped 200
        it "refuses account writes without a cookie" $ do
            res <- getAs (withDb "system" []) "/api/tables/__system_users/rows"
            expectStatusWith "account rows without a cookie" res 401

-- 角色 -----------------------------------------------------------------------

-- | 角色与授权用例
roleSpec :: AppEnv -> Spec
roleSpec env = describe "roles and grants" $ do
    withApp (webApp env) $ do
        it "uses database scopes and owner permissions through REST and rejects private data" $ do
            admin <- adminHeaders
            ensureDatabase admin testDatabaseName
            ensureAccount admin "acl_web_owner" "Owner-Secret-1234"
            ensureAccount admin "acl_web_reader" "Reader-Secret-1234"
            ensureTable admin "acl_private" (encode (object ["name" .= ("acl_private" :: Text), "columns" .= [column "id" "int"]]))
            permission <- postAs admin "/api/roles/acl_web_owner/grants" (encode (object ["object" .= ("database:" <> testDatabaseName), "privileges" .= (["connect", "create"] :: [Text])]))
            expectStatusWith "database object grant" permission 200
            owner <- ordinaryHeaders "acl_web_owner" "Owner-Secret-1234"
            created <- postAs (withDb testDatabaseName owner) "/api/tables" (encode (object ["name" .= ("acl_owned" :: Text), "columns" .= [column "id" "int"]]))
            expectStatusWith "owner creates table" created 200
            denied <- testSql owner "SELECT * FROM acl_private"
            expectStatusWith "private table denied" denied 403
            grantDenied <- postAs (withDb testDatabaseName owner) "/api/roles/acl_web_reader/grants" (encode (object ["object" .= (testDatabaseName <> ".acl_private"), "privileges" .= (["select"] :: [Text])]))
            expectStatusWith "missing grant option" grantDenied 403
            delegated <- postAs (withDb testDatabaseName owner) "/api/roles/acl_web_reader/grants" (encode (object ["object" .= (testDatabaseName <> ".acl_owned"), "privileges" .= (["select"] :: [Text])]))
            expectStatusWith "owner delegates via REST" delegated 200
            reader <- ordinaryHeaders "acl_web_reader" "Reader-Secret-1234"
            visible <- testSql reader "SELECT * FROM acl_owned"
            expectStatusWith "delegated query" visible 200
            revoked <- testSql admin "REVOKE SELECT ON acl_owned FROM acl_web_reader"
            expectStatusWith "root revokes owner delegation" revoked 200
            invalidated <- testSql reader "SELECT * FROM acl_owned"
            expectStatusWith "revoked Web session" invalidated 401
        it "filters database visibility after PUBLIC CONNECT is revoked" $ do
            admin <- adminHeaders
            ensureDatabase admin "acl_closed"
            ensureAccount admin "acl_web_guest" "Guest-Secret-1234"
            closed <- postAs admin "/api/query" (encode (object ["sql" .= ("REVOKE CONNECT ON DATABASE acl_closed FROM PUBLIC" :: Text)]))
            expectStatusWith "close default connection" closed 200
            guest <- ordinaryHeaders "acl_web_guest" "Guest-Secret-1234"
            listed <- getAs guest "/api/databases"
            expectStatusWith "filtered database list" listed 200
            check (("acl_closed" `elem` map asText (items (jsonBody listed))) `shouldBe` False)
            refused <- getAs (withDb "acl_closed" guest) "/api/tables"
            expectStatusWith "CONNECT denied through Web" refused 403
        it "creates a role, grants a privilege, adds a member and drops it" $ do
            hs <- adminHeaders
            ensureDatabase hs testDatabaseName
            ensureAccount hs "bob" "bob-Secret-123456"
            ensureTable (withDb testDatabaseName hs) "grant_probe" (encode (object ["name" .= ("grant_probe" :: Text), "columns" .= [column "id" "int"]]))
            created <- postAs hs "/api/roles" (encode (object ["name" .= ("analyst" :: Text)]))
            expectStatusWith "create role" created 200
            granted <-
                postAs
                    hs
                    "/api/roles/analyst/grants"
                    (encode (object ["object" .= (testDatabaseName <> ".grant_probe"), "privileges" .= (["select"] :: [Text])]))
            expectStatusWith "grant privilege" granted 200
            member <- postAs hs "/api/roles/analyst/members" (encode (object ["user" .= ("bob" :: Text)]))
            expectStatusWith "add member" member 200
            listed <- getAs hs "/api/roles"
            expectStatusWith "list roles" listed 200
            check (("analyst" `elem` map (asText . at "name") (items (jsonBody listed))) `shouldBe` True)
            removed <- deleteAs hs "/api/roles/analyst/members/bob"
            expectStatusWith "remove member" removed 200
            revoked <-
                deleteAs
                    hs
                    "/api/roles/analyst/grants"
            expectStatusWith "revoke without a body" revoked 400
            dropped <- deleteAs hs "/api/roles/analyst"
            expectStatusWith "drop role" dropped 200

        it "carries the grant option through the API" $ do
            hs <- adminHeaders
            ensureDatabase hs testDatabaseName
            ensureTable (withDb testDatabaseName hs) "grant_option_probe" (encode (object ["name" .= ("grant_option_probe" :: Text), "columns" .= [column "id" "int"]]))
            created <- postAs hs "/api/roles" (encode (object ["name" .= ("passer" :: Text)]))
            expectStatusWith "create passer" created 200
            granted <-
                postAs
                    (withDb testDatabaseName hs)
                    "/api/roles/passer/grants"
                    (encode (object ["object" .= (testDatabaseName <> ".grant_option_probe"), "privileges" .= (["select"] :: [Text]), "grantOption" .= True]))
            expectStatusWith "grant with option" granted 200
            listed <- getAs hs "/api/roles"
            expectStatusWith "list roles" listed 200
            let matching = [view | view <- items (jsonBody listed), asText (at "name" view) == "passer"]
            check ((concatMap (map (asBool . at "grantable") . items . at "grants") matching) `shouldBe` [True])
            refused <-
                postAs
                    hs
                    "/api/roles/passer/grants"
                    (encode (object ["object" .= testDatabaseName, "privileges" .= (["select"] :: [Text]), "grantOption" .= ("yes" :: Text)]))
            expectStatusWith "non-boolean grant option" refused 400

-- SQL 控制台 -----------------------------------------------------------------

-- | SQL 控制台用例
querySpec :: AppEnv -> Spec
querySpec env = describe "the SQL console" $ do
    withApp (webApp env) $ do
        it "returns typed compound values through HTTP and TCP" $ do
            hs <- adminHeaders
            ensureDatabase hs testDatabaseName
            result <- testSql hs "SELECT LIST<Int>(1, 2), length('abc')"
            expectStatusWith "typed SQL values" result 200
            let value = maybe A.Null id (nth 0 (firstRow result))
                envelope = at "__chusql_runtime_v1" value
            check (arrayLen envelope `shouldBe` 3)
            check (asInt (maybe A.Null id (nth 0 (items envelope))) `shouldBe` 1)
            check (asInt (maybe A.Null id (nth 1 (firstRow result))) `shouldBe` 3)
        it "runs DDL and DML and returns positional rows" $ do
            hs <- adminHeaders
            ensureDatabase hs testDatabaseName
            made <- postAs (withDb testDatabaseName hs) "/api/query" (encode (object ["sql" .= ("CREATE TABLE console_probe (id int, label varchar(64))" :: Text)]))
            expectStatusWith "create table by SQL" made 200
            inserted <- postAs (withDb testDatabaseName hs) "/api/query" (encode (object ["sql" .= ("INSERT INTO console_probe (id, label) VALUES (7, 'seven')" :: Text)]))
            expectStatusWith "insert by SQL" inserted 200
            res <- postAs (withDb testDatabaseName hs) "/api/query" (encode (object ["sql" .= ("SELECT * FROM console_probe" :: Text)]))
            expectStatusWith "select by SQL" res 200
            check (asInt (at "rowCount" (jsonBody res)) `shouldBe` 1)
            check (arrayLen (at "columns" (jsonBody res)) `shouldBe` 2)
            check (asText (maybe A.Null id (nth 1 (firstRow res))) `shouldBe` "seven")
        it "refuses an empty statement" $ do
            hs <- adminHeaders
            ensureDatabase hs testDatabaseName
            res <- postAs (withDb testDatabaseName hs) "/api/query" (encode (object ["sql" .= ("" :: Text)]))
            expectStatusWith "empty sql" res 400
        it "refuses an oversized body" $ do
            hs <- adminHeaders
            res <- postAs (withDb testDatabaseName hs) "/api/query" (BL.replicate 70000 32)
            expectStatusWith "oversized body" res 413
        it "reports a syntax error as a bad request" $ do
            hs <- adminHeaders
            ensureDatabase hs testDatabaseName
            res <- postAs (withDb testDatabaseName hs) "/api/query" (encode (object ["sql" .= ("SELEC nonsense" :: Text)]))
            expectStatusWith "syntax error" res 400
        it "redacts credentials from malformed SQL responses" $ do
            hs <- adminHeaders
            ensureDatabase hs testDatabaseName
            let secret = "Web-Secret-DoNotEcho123!" :: Text
            res <- postAs (withDb testDatabaseName hs) "/api/query" (encode (object ["sql" .= ("CREATE USER broken IDENTIFIED BY '" <> secret <> "' trailing")]))
            expectStatusWith "credential syntax error" res 400
            check (BSC.isInfixOf "Web-Secret-DoNotEcho123!" (BL.toStrict (WT.simpleBody res)) `shouldBe` False)
            check (BSC.isInfixOf "redacted" (BL.toStrict (WT.simpleBody res)) `shouldBe` True)

-- IDE 设置 -------------------------------------------------------------------

-- | 发出编辑事务控制请求。
editRequest :: [Header] -> BS.ByteString -> Text -> [A.Value] -> TestSession WT.SResponse
editRequest hs path owner changes = postAs hs path (encode (object
    ["id" .= owner, "database" .= testDatabaseName, "changes" .= changes]))

-- | 在测试库执行一条 SQL。
testSql :: [Header] -> Text -> TestSession WT.SResponse
testSql hs sql = postAs (withDb testDatabaseName hs) "/api/query" (encode (object ["sql" .= sql]))

-- | 生成整数行插入编辑。
editInsert :: Text -> Int -> A.Value
editInsert table key = object ["op" .= ("insert" :: Text), "table" .= table,
    "values" .= object ["id" .= key, "value" .= key]]

-- | 验证编辑原子性、隔离与失效清理。
editSpec :: AppEnv -> Spec
editSpec env = describe "web editing transactions" $ withApp (webApp env) $ do
    it "commits across tables atomically and deduplicates successful retries" $ do
        hs <- adminHeaders
        ensureDatabase hs testDatabaseName
        madeA <- testSql hs "CREATE TABLE edit_atomic_a (id int PRIMARY KEY, value int)"
        expectStatusWith "create first table" madeA 200
        madeB <- testSql hs "CREATE TABLE edit_atomic_b (id int PRIMARY KEY, value int)"
        expectStatusWith "create second table" madeB 200
        begun <- editRequest hs "/api/edit/begin" "atomic" []
        expectStatusWith "begin" begun 200
        let changes = [editInsert "edit_atomic_a" 1, editInsert "edit_atomic_b" 2]
        committed <- editRequest hs "/api/edit/commit" "atomic" changes
        expectStatusWith "commit" committed 200
        check (at "applied" (jsonBody committed) `shouldBe` A.Number 2)
        duplicate <- editRequest hs "/api/edit/commit" "atomic" changes
        expectStatusWith "duplicate commit" duplicate 200
        check (jsonBody duplicate `shouldBe` jsonBody committed)
        changed <- editRequest hs "/api/edit/commit" "atomic" [editInsert "edit_atomic_a" 9]
        expectStatusWith "changed duplicate" changed 409
        rowsA <- testSql hs "SELECT * FROM edit_atomic_a"
        rowsB <- testSql hs "SELECT * FROM edit_atomic_b"
        check (asInt (at "rowCount" (jsonBody rowsA)) `shouldBe` 1)
        check (asInt (at "rowCount" (jsonBody rowsB)) `shouldBe` 1)
    it "rolls back a mid-batch constraint error to the savepoint and permits correction" $ do
        hs <- adminHeaders
        ensureDatabase hs testDatabaseName
        made <- testSql hs "CREATE TABLE edit_constraints (id int PRIMARY KEY, value int)"
        expectStatusWith "create table" made 200
        begun <- editRequest hs "/api/edit/begin" "constraints" []
        expectStatusWith "begin" begun 200
        failed <- editRequest hs "/api/edit/commit" "constraints" [editInsert "edit_constraints" 1, editInsert "edit_constraints" 1]
        expectStatusWith "constraint failure" failed 400
        check (at "state" (jsonBody failed) `shouldBe` A.String "active")
        check (at "applied" (jsonBody failed) `shouldBe` A.Number 0)
        visible <- testSql hs "SELECT * FROM edit_constraints"
        check (asInt (at "rowCount" (jsonBody visible)) `shouldBe` 0)
        snapshot <- getAs (("X-ChuSQL-Edit", "constraints") : withDb testDatabaseName hs) "/api/tables/edit_constraints/rows"
        expectStatusWith "snapshot after failure" snapshot 200
        check (asInt (at "total" (jsonBody snapshot)) `shouldBe` 0)
        corrected <- editRequest hs "/api/edit/commit" "constraints" [editInsert "edit_constraints" 2]
        expectStatusWith "corrected replay" corrected 200
        visibleAgain <- testSql hs "SELECT id FROM edit_constraints"
        check (firstRow visibleAgain `shouldBe` [A.Number 2])
    it "preserves the initial snapshot and rejects a conflicting external update" $ do
        hs <- adminHeaders
        ensureDatabase hs testDatabaseName
        made <- testSql hs "CREATE TABLE edit_conflicts (id int PRIMARY KEY, value int)"
        expectStatusWith "create table" made 200
        seeded <- testSql hs "INSERT INTO edit_conflicts (id, value) VALUES (1, 10)"
        expectStatusWith "seed row" seeded 200
        begun <- editRequest hs "/api/edit/begin" "conflicts" []
        expectStatusWith "begin" begun 200
        observer <- adminHeaders
        updated <- testSql observer "UPDATE edit_conflicts SET value = 20 WHERE id = 1"
        expectStatusWith "concurrent update" updated 200
        snapshot <- getAs (("X-ChuSQL-Edit", "conflicts") : withDb testDatabaseName hs) "/api/tables/edit_conflicts/rows"
        check (firstRow snapshot `shouldBe` [A.Number 1, A.Number 10])
        let change = object ["op" .= ("update" :: Text), "table" .= ("edit_conflicts" :: Text),
                "pk" .= (1 :: Int), "set" .= object ["value" .= (30 :: Int)]]
        failed <- editRequest hs "/api/edit/commit" "conflicts" [change]
        expectStatusWith "serialization conflict" failed 409
        check (at "reloadRequired" (jsonBody failed) `shouldBe` A.Bool True)
        live <- testSql observer "SELECT value FROM edit_conflicts"
        check (firstRow live `shouldBe` [A.Number 20])
        retried <- editRequest hs "/api/edit/commit" "conflicts" [change]
        expectStatusWith "stale retry" retried 409
        reopened <- editRequest hs "/api/edit/begin" "conflicts" []
        expectStatusWith "stale begin" reopened 409
    it "binds editing to one page and database and makes rollback idempotent" $ do
        hs <- adminHeaders
        ensureDatabase hs testDatabaseName
        begun <- editRequest hs "/api/edit/begin" "owner" []
        expectStatusWith "begin" begun 200
        repeated <- editRequest hs "/api/edit/begin" "owner" []
        expectStatusWith "repeated begin" repeated 200
        otherPage <- editRequest hs "/api/edit/begin" "intruder" []
        expectStatusWith "another page" otherPage 409
        otherDatabase <- postAs hs "/api/edit/commit" (encode (object
            ["id" .= ("owner" :: Text), "database" .= ("system" :: Text), "changes" .= ([] :: [A.Value])]))
        expectStatusWith "another database" otherDatabase 409
        forged <- getAs (("X-ChuSQL-Edit", "intruder") : withDb testDatabaseName hs) "/api/tables"
        expectStatusWith "another page snapshot" forged 409
        rolled <- editRequest hs "/api/edit/rollback" "owner" []
        expectStatusWith "rollback" rolled 200
        duplicate <- editRequest hs "/api/edit/rollback" "owner" []
        expectStatusWith "repeated rollback" duplicate 200
        stale <- editRequest hs "/api/edit/begin" "owner" []
        expectStatusWith "ended identity" stale 409
    it "keeps SQL-console transactions separate from grid editing" $ do
        hs <- adminHeaders
        ensureDatabase hs testDatabaseName
        made <- testSql hs "CREATE TABLE edit_console (id int PRIMARY KEY, value int)"
        expectStatusWith "create table" made 200
        consoleBegin <- testSql hs "BEGIN"
        expectStatusWith "console begin" consoleBegin 200
        consoleWrite <- testSql hs "INSERT INTO edit_console (id, value) VALUES (1, 1)"
        expectStatusWith "console write" consoleWrite 200
        begun <- editRequest hs "/api/edit/begin" "console" []
        expectStatusWith "editor begin" begun 200
        committed <- editRequest hs "/api/edit/commit" "console" [editInsert "edit_console" 2]
        expectStatusWith "editor commit" committed 200
        consoleRollback <- testSql hs "ROLLBACK"
        expectStatusWith "console rollback" consoleRollback 200
        live <- testSql hs "SELECT id FROM edit_console"
        check (firstRow live `shouldBe` [A.Number 2])
    it "drops the editing connection on logout and rejects the old transaction after login" $ do
        hs <- adminHeaders
        ensureDatabase hs testDatabaseName
        begun <- editRequest hs "/api/edit/begin" "logout" []
        expectStatusWith "begin" begun 200
        gone <- postAs hs "/api/logout" "{}"
        expectStatusWith "logout" gone 204
        rejected <- editRequest hs "/api/edit/commit" "logout" []
        expectStatusWith "old cookie" rejected 401
        newHs <- adminHeaders
        stale <- editRequest newHs "/api/edit/commit" "logout" []
        expectStatusWith "old transaction on new connection" stale 409
    it "rejects system identity transactions" $ do
        hs <- adminHeaders
        res <- postAs hs "/api/edit/begin" (encode (object ["id" .= ("identities" :: Text), "database" .= ("system" :: Text)]))
        expectStatusWith "identity transaction" res 400
    it "invalidates editing when the account loses its permission" $ do
        hs <- adminHeaders
        ensureDatabase hs testDatabaseName
        made <- testSql hs "CREATE TABLE edit_permissions (id int PRIMARY KEY, value int)"
        expectStatusWith "create table" made 200
        ensureAccount hs "edit_user" "Editor-Secret-1234"
        granted <- testSql hs "GRANT SELECT, INSERT ON edit_permissions TO edit_user"
        expectStatusWith "grant editing" granted 200
        editor <- ordinaryHeaders "edit_user" "Editor-Secret-1234"
        begun <- editRequest editor "/api/edit/begin" "permissions" []
        expectStatusWith "begin" begun 200
        revoked <- testSql hs "REVOKE INSERT ON edit_permissions FROM edit_user"
        expectStatusWith "revoke insert" revoked 200
        rejected <- editRequest editor "/api/edit/commit" "permissions" [editInsert "edit_permissions" 1]
        expectStatusWith "invalidated editor" rejected 401
        live <- testSql hs "SELECT * FROM edit_permissions"
        check (asInt (at "rowCount" (jsonBody live)) `shouldBe` 0)
    it "restricts an SQL transaction to its owning console and blocks unrelated REST writes" $ do
        hs <- adminHeaders
        ensureDatabase hs testDatabaseName
        let owner = ("X-ChuSQL-Console", "page:console-a") : hs
            intruder = ("X-ChuSQL-Console", "page:console-b") : hs
        begun <- testSql owner "/* owner */ BEGIN"
        expectStatusWith "console begin" begun 200
        rejected <- testSql intruder "ROLLBACK"
        expectStatusWith "other console rollback" rejected 409
        rest <- postAs (withDb testDatabaseName hs) "/api/tables" (encode (object
            ["name" .= ("edit_rest_intruder" :: Text), "columns" .= [column "id" "int"]]))
        expectStatusWith "REST write inside console transaction" rest 409
        closed <- postAs hs "/api/query/rollback" (encode (object ["console" .= ("page:console-a" :: Text)]))
        expectStatusWith "close owning console" closed 204
        next <- testSql intruder "SELECT 1"
        expectStatusWith "other console after rollback" next 200
    it "cleans the editing snapshot when its web session expires" $ do
        hs <- adminHeaders
        ensureDatabase hs testDatabaseName
        begun <- editRequest hs "/api/edit/begin" "expires" []
        expectStatusWith "begin" begun 200
        let token = fromMaybe "" (lookup "Cookie" hs >>= parseCookieHeader sessionCookieName . TE.decodeUtf8)
        stored <- liftIO (lookupPayload (aeSessions env) token)
        app <- getState
        res <- liftIO $ bracket
            (setPayloadPolicy (aeSessions env) (SessionPolicy (-1) 86400))
            (const (setPayloadPolicy (aeSessions env) defaultSessionPolicy))
            (const (editFromApp app hs "expires" []))
        expectStatusWith "expired commit" res 401
        case stored of
            Nothing -> check (expectationFailure "missing live web session")
            Just (_, ws) -> check (readIORef (wsEdit ws) >>= (`shouldBe` Nothing))
    it "serializes duplicate HTTP commits and applies the edit set once" $ do
        hs <- adminHeaders
        ensureDatabase hs testDatabaseName
        made <- testSql hs "CREATE TABLE edit_double_click (id int PRIMARY KEY, value int)"
        expectStatusWith "create table" made 200
        begun <- editRequest hs "/api/edit/begin" "double-click" []
        expectStatusWith "begin" begun 200
        app <- getState
        results <- liftIO $ do
            first <- newEmptyMVar
            second <- newEmptyMVar
            _ <- forkFinally (editFromApp app hs "double-click" [editInsert "edit_double_click" 1]) (putMVar first)
            _ <- forkFinally (editFromApp app hs "double-click" [editInsert "edit_double_click" 1]) (putMVar second)
            mapM (either throwIO pure) =<< sequence [takeMVar first, takeMVar second]
        mapM_ (\res -> expectStatusWith "duplicate concurrent commit" res 200) results
        live <- testSql hs "SELECT * FROM edit_double_click"
        check (asInt (at "rowCount" (jsonBody live)) `shouldBe` 1)

-- | 从独立请求上下文提交编辑事务。
editFromApp :: Wai.Application -> [Header] -> Text -> [A.Value] -> IO WT.SResponse
editFromApp app hs owner changes = WT.runSession send app
  where
    -- | 构造带 Cookie 的原子提交请求。
    send = WT.srequest (WT.SRequest
        (WT.setPath WT.defaultRequest {Wai.requestMethod = methodPost, Wai.requestHeaders = jsonHeaders hs} "/api/edit/commit")
        (encode (object ["id" .= owner, "database" .= testDatabaseName, "changes" .= changes])))

-- | IDE 设置用例
uiSettingsSpec :: AppEnv -> Spec
uiSettingsSpec env = describe "IDE settings" $ do
    withApp (webApp env) $ do
        it "round-trips a valid payload" $ do
            hs <- adminHeaders
            initialUi <- getAs hs "/api/ui-settings"
            expectStatusWith "read IDE settings" initialUi 200
            saved <- putAs hs "/api/ui-settings" (encode (object ["sqlFontSize" .= (13 :: Int), "minimap" .= False]))
            expectStatusWith "write IDE settings" saved 204
            updatedUi <- getAs hs "/api/ui-settings"
            check (at "sqlFontSize" (jsonBody updatedUi) `shouldBe` A.Number 13)
        it "refuses an unknown key" $ do
            hs <- adminHeaders
            bad <- putAs hs "/api/ui-settings" (encode (object ["nope" .= (1 :: Int)]))
            expectStatusWith "unknown IDE setting" bad 400
        it "needs a session" $ do
            res <- getAs [] "/api/ui-settings"
            expectStatusWith "IDE settings without a cookie" res 401

-- 限流 -----------------------------------------------------------------------

-- | 登录限流用例
rateLimitSpec :: AppEnv -> Spec
rateLimitSpec env = describe "sign-in rate limiting" $ do
    withApp (webApp env) $ do
        it "locks the account after the configured failures" $ do
            first <- loginAs adminUser "wrong-one"
            expectStatusWith "first failure" first 401
            second <- loginAs adminUser "wrong-two"
            expectStatusWith "locked out" second 429
            third <- loginAs adminUser adminPassword
            expectStatusWith "still locked out" third 429

-- 真 socket 端到端 -----------------------------------------------------------

-- | 真 socket 端到端用例
e2eSpec :: AppEnv -> Spec
e2eSpec env = describe "end to end (HTTP socket -> web -> TCP -> server -> storage)" $ do
    it "signs in over a real socket and lists databases" $ do
        Warp.testWithApplication (webApp env) $ \port -> do
            manager <- newManager defaultManagerSettings
            login <- httpJson manager port methodPost "/api/login" [] (encode (object ["user" .= adminUser, "password" .= adminPassword]))
            statusCode (responseStatus login) `shouldBe` 204
            let cookie = case cookieOf login of
                    Just token -> [cookieHeader token]
                    Nothing -> []
            listed <- httpJson manager port methodGet "/api/databases" cookie BL.empty
            statusCode (responseStatus listed) `shouldBe` 200
            let body = fromMaybe A.Null (A.decode (responseBody listed))
            ("system" `elem` map asText (items body)) `shouldBe` True

-- | 走真 HTTP 客户端打出去的一个请求
httpJson :: Manager -> Int -> Method -> BS.ByteString -> [Header] -> BL.ByteString -> IO (Response BL.ByteString)
httpJson manager port httpMethod path headers payload = do
    baseRequest <- parseRequest ("http://127.0.0.1:" ++ show port ++ BSC.unpack path)
    httpLbs
        baseRequest
            { method = httpMethod
            , requestHeaders = jsonHeaders headers
            , requestBody = RequestBodyLBS payload
            }
        manager

-- | 从响应里取会话令牌
cookieOf :: Response BL.ByteString -> Maybe Text
cookieOf res = do
    raw <- lookup "Set-Cookie" (responseHeaders res)
    parseCookieHeader sessionCookieName (TE.decodeUtf8 raw)

-- | 入口：起夹具并跑全部用例
main :: IO ()
main = do
    staticDir <- makeStaticDir
    -- server 用 bracket 兜底收摊：断言抛异常、用例被过滤器全跳过，都不会留下孤儿进程占着 exe
    bracket (startLiveWorld "main") stopServer $ \world ->
        bracket (startLiveWorld "limited") stopServer $ \limitedWorld -> do
            env <- liveEnv world staticDir 5
            limitedEnv <- liveEnv limitedWorld staticDir 1
            hspec (spec staticDir env limitedEnv)

-- | 执行前端事务状态回归。
frontendSpec :: Spec
frontendSpec = describe "browser transaction state" $ do
    it "passes the Node runtime value suite" $ do
        node <- findExecutable "node"
        case node of
            Nothing -> expectationFailure "Node.js is required for runtime value tests"
            Just executable -> do
                (exitCode, output, errors) <- readProcessWithExitCode executable ["test/runtime-values.mjs"] ""
                when (exitCode /= ExitSuccess) (expectationFailure (output ++ errors))
    it "passes the Node frontend transaction suite" $ do
        node <- findExecutable "node"
        case node of
            Nothing -> expectationFailure "Node.js is required for frontend transaction tests"
            Just executable -> do
                (exitCode, output, errors) <- readProcessWithExitCode executable ["--test", "--test-reporter=tap", "test/transactions.mjs"] ""
                when (exitCode /= ExitSuccess) (expectationFailure (output ++ errors))

-- | 汇总挂载全部用例组。
spec :: FilePath -> AppEnv -> AppEnv -> Spec
spec staticDir env limitedEnv = do
    unitSpec
    staticSpec staticDir
    gateSpec
    sessionSpec env
    gatingSpec env
    catalogSpec env
    settingsSpec env
    accountSpec env
    roleSpec env
    querySpec env
    editSpec env
    frontendSpec
    uiSettingsSpec env
    e2eSpec env
    rateLimitSpec limitedEnv
