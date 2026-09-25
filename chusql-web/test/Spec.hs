{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.Model (Column (..), Database, Table (..), Value (..))
import ChuSQL.Storage.IPC (SchemaColumn (..), TableInfo (..))
import ChuSQL.Syntax.AST (Expr (..), FromClause (..), Statement (..))
import ChuSQL.Web.Actions (
    ColumnSpec (..),
    CreateTableSpec (..),
    coerceValue,
    columnTypeOf,
    createIndexSql,
    createTableSql,
    deleteRowSql,
    dropIndexSql,
    dropTableSql,
    insertRowSql,
    insertRowsSql,
    isIdentifier,
    selectRowsSql,
    sqlLiteral,
    updateRowSql,
 )
import ChuSQL.Web.Api (
    AppEnv (..),
    Live (..),
    defaultLive,
    newAppEnv,
    parseCookieHeader,
    readLive,
    renderRow,
    setLive,
    webApp,
 )
import ChuSQL.Web.Auth (
    Credential (..),
    SessionPolicy (..),
    createSession,
    defaultSessionPolicy,
    deleteOtherSessions,
    deleteSession,
    hashLooksValid,
    hashPasswordWith,
    lookupSession,
    newSessionStore,
    sessionPolicy,
    sessionToken,
    setSessionPolicy,
    verifyPassword,
 )
import ChuSQL.Web.Backend (columnsFromStatement, ipcBackend, memoryBackend, tableInfoOf)
import ChuSQL.Web.Config (
    WebConfig (..),
    defaultPassword,
    defaultUser,
    defaultWebConfig,
    loadWebConfig,
    resolveCredential,
    usingDefaultCredentials,
 )
import ChuSQL.Web.RateLimit (newRateLimiter, rateLimitBlock, rateLimitRecord, setRateLimit)
import ChuSQL.Web.Settings (
    SettingItem (..),
    applySettings,
    defaultOf,
    effectiveSettings,
    findItem,
    isRestartRequired,
    isRootOnly,
    liveKeys,
    readSettingsFile,
    resolveSettingsFile,
    settingCatalogue,
    writeSettingsFile,
 )
import ChuSQL.Web.Static (contentTypeOf, readStatic, safeRelative)
import ChuSQL.Web.StorageProcess (StorageProcess (..), startStorageProcess, stopStorageProcess)
import ChuSQL.Web.UiSettings (
    readUiSettings,
    resolveUiSettingsFile,
    uiSettingsFileCandidates,
    validateUiSettings,
    writeUiSettings,
 )
import Control.Concurrent.MVar (newMVar)
import Control.Exception (IOException, bracket, finally, try)
import Data.Aeson (decode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isSuffixOf, nub, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, getCurrentTime)
import qualified Data.Vector as V
import Network.HTTP.Client (
    Manager,
    Request (method, requestBody, requestHeaders),
    RequestBody (..),
    Response (responseBody, responseHeaders, responseStatus),
    defaultManagerSettings,
    httpLbs,
    newManager,
    parseRequest,
 )
import Network.HTTP.Types (Header, Method, methodGet, methodPost, statusCode)
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Test as WT
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory)
import qualified System.Directory
import System.Environment (lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (Handle)
import System.Process (
    CreateProcess (cwd, std_err, std_out),
    ProcessHandle,
    StdStream (..),
    createProcess,
    proc,
    waitForProcess,
 )
import Test.Hspec
import Test.Hspec.Wai hiding (pendingWith)

-- ChuSQL Web 测试，覆盖三层：
-- 纯函数、路由级（内存后端）、端到端（真 Rust 存储进程）。

-- | users 5 行 + orders 1 行
testDb :: Database
testDb =
    [ ( "users"
      , Table
            "users"
            [("id", TInt), ("name", TStr), ("age", TInt)]
            [ [("id", VInt i), ("name", VStr ("user" ++ show i)), ("age", VInt (20 + i))]
            | i <- [1 .. 5]
            ]
      )
    , ( "orders"
      , Table
            "orders"
            [("id", TInt), ("user_id", TInt)]
            [[("id", VInt 1), ("user_id", VInt 1)]]
      )
    ]

-- | 测试账号：迭代次数压低（跑得快），口令 `s3cret`
testCredential :: Credential
testCredential = Credential "admin" (hashPasswordWith 1000 (BS.replicate 16 7) "s3cret")

-- | 可控时钟的起点
epoch :: UTCTime
epoch = UTCTime (fromGregorian 2026 1 1) 0

-- | 默认会话策略
policy :: SessionPolicy
policy = SessionPolicy (8 * 3600) (24 * 3600)

-- | 造一条 `SELECT *`
selectStar :: FromClause -> Statement
selectStar from = Select ["*"] from Nothing [] Nothing

-- | 造一条显式列名的 SELECT
selectThese :: [String] -> FromClause -> Statement
selectThese cols from = Select cols from Nothing [] Nothing

-- | with 块里的会话类型别名
type TestSession a = WaiSession () a

-- | 每个用例组一套：内存后端 + 可控时钟 + 临时静态目录
freshWorld :: FilePath -> IO AppEnv
freshWorld staticDir = do
    db <- newMVar testDb
    clock <- newIORef epoch
    sessions <- newSessionStore (readIORef clock) policy
    limiter <- newRateLimiter (readIORef clock) 5 (5 * 60)
    env <- newAppEnv (memoryBackend db) sessions testCredential limiter staticDir
    path <- tempSettingsPath "default"
    removeIfExists path
    uiPath <- tempUiSettingsPath "default"
    removeIfExists uiPath
    pure env{aeSettingsFile = path, aeUiSettingsFile = uiPath, aeEffective = Map.empty}

-- | 临时设置文件（每个用例组一个名字，互不干扰）
tempSettingsPath :: String -> IO FilePath
tempSettingsPath name = do
    tmp <- getTemporaryDirectory
    pure (tmp </> ("chusql-web-test-" ++ name ++ "-settings.json"))

-- | 临时 IDE 设置文件（同样按用例组取名）
tempUiSettingsPath :: String -> IO FilePath
tempUiSettingsPath name = do
    tmp <- getTemporaryDirectory
    pure (tmp </> ("chusql-web-test-" ++ name ++ "-ui-settings.json"))

-- | 文件不存在也算成功
removeIfExists :: FilePath -> IO ()
removeIfExists path = do
    there <- doesFileExist path
    if there then removeFileSafe path else pure ()
  where
    removeFileSafe p = do
        _ <- try (System.Directory.removeFile p) :: IO (Either IOException ())
        pure ()

-- | 带自己的设置文件的用例组
freshWorldAt :: FilePath -> String -> IO AppEnv
freshWorldAt staticDir name = do
    env <- freshWorld staticDir
    path <- tempSettingsPath name
    removeIfExists path
    uiPath <- tempUiSettingsPath name
    removeIfExists uiPath
    pure env{aeSettingsFile = path, aeUiSettingsFile = uiPath}

-- | 临时静态目录，内容固定便于断言
makeStaticDir :: IO FilePath
makeStaticDir = do
    tmp <- getTemporaryDirectory
    let dir = tmp </> "chusql-web-static-test"
    createDirectoryIfMissing True dir
    BS.writeFile (dir </> "index.html") "<!DOCTYPE html><html lang=\"zh-CN\"><head></head><body>ChuSQL Console</body></html>"
    BS.writeFile (dir </> "app.js") "// test bundle\n"
    BS.writeFile (dir </> "style.css") "body{}\n"
    BS.writeFile (dir </> "secret.token") "do-not-serve\n"
    pure dir

-- | 一个用例组一套环境
withWorld :: FilePath -> (AppEnv -> SpecWith ((), Application)) -> Spec
withWorld staticDir body = do
    appEnv <- runIO (freshWorld staticDir)
    with (webApp appEnv) (body appEnv)

-- | 同上，但带着自己的设置文件（改设置/改口令的用例不互相串）
withWorldAt :: FilePath -> String -> (AppEnv -> SpecWith ((), Application)) -> Spec
withWorldAt staticDir name body = do
    appEnv <- runIO (freshWorldAt staticDir name)
    with (webApp appEnv) (body appEnv)

-- | 响应体解成 JSON
jsonBody :: WT.SResponse -> A.Value
jsonBody res = fromMaybe A.Null (decode (WT.simpleBody res))

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

-- | 第 i 个元素（越界给 Nothing，不用 `!!`）
nth :: Int -> [a] -> Maybe a
nth i xs = case drop i xs of
    (x : _) -> Just x
    [] -> Nothing

-- | 取 Set-Cookie 里的会话令牌
sessionCookieOf :: WT.SResponse -> Maybe Text
sessionCookieOf res = do
    raw <- lookup "Set-Cookie" (WT.simpleHeaders res)
    parseCookieHeader "chusql_session" (TE.decodeUtf8 raw)

-- | 会话令牌变请求头
cookieHeaders :: WT.SResponse -> [Header]
cookieHeaders res = case sessionCookieOf res of
    Nothing -> []
    Just token -> [("Cookie", TE.encodeUtf8 ("chusql_session=" <> token))]

-- | 带 JSON 体的请求
jsonRequest :: Method -> BS.ByteString -> BL.ByteString -> TestSession WT.SResponse
jsonRequest httpMethod urlPath payload =
    request httpMethod urlPath [("Content-Type", "application/json")] payload

-- | 登录请求体
loginBody :: Text -> Text -> BL.ByteString
loginBody user password =
    BL.fromStrict . TE.encodeUtf8 $
        T.concat ["{\"user\":\"", user, "\",\"password\":\"", password, "\"}"]

-- | 登录
loginAs :: Text -> Text -> TestSession WT.SResponse
loginAs user password = jsonRequest methodPost "/api/login" (loginBody user password)

-- | 带请求头的 GET
getWith :: [Header] -> BS.ByteString -> TestSession WT.SResponse
getWith headers path = request methodGet path headers ""

-- | 带请求头的 JSON POST
postWith :: [Header] -> BS.ByteString -> BL.ByteString -> TestSession WT.SResponse
postWith headers path payload =
    request methodPost path (("Content-Type", "application/json") : headers) payload

-- | 带请求头的 JSON PUT（改设置用的是 PUT）
putWith :: [Header] -> BS.ByteString -> BL.ByteString -> TestSession WT.SResponse
putWith headers path payload =
    request "PUT" path (("Content-Type", "application/json") : headers) payload

-- | 在 items 数组里按键找一项（找不到给 Null）
headEntry :: Text -> [A.Value] -> A.Value
headEntry key entries = case [e | e <- entries, asText (at "key" e) == key] of
    (e : _) -> e
    [] -> A.Null

-- | 纯断言抬进 WaiSession
check :: IO () -> TestSession ()
check = liftIO

-- | 已经拿到响应对象时的状态码断言
hasStatus :: WT.SResponse -> Int -> IO ()
hasStatus res code = statusCode (WT.simpleStatus res) `shouldBe` code

-- | 临时设几个环境变量跑一段，跑完恢复原样（原来没有就删掉）
withEnv :: [(String, String)] -> IO a -> IO a
withEnv kvs act = do
    saved <- mapM (\(k, _) -> fmap ((,) k) (lookupEnv k)) kvs
    mapM_ (uncurry setEnv) kvs
    act `finally` mapM_ restore saved
  where
    restore (k, mv) = maybe (unsetEnv k) (setEnv k) mv

-- | 按 UTF-8 读文件（避免解码错）
readUtf8 :: FilePath -> IO Text
readUtf8 path = TE.decodeUtf8 <$> BS.readFile path

-- | Either 判空
isLeftE :: Either a b -> Bool
isLeftE (Left _) = True
isLeftE (Right _) = False

-- | Either 判成
isRightE :: Either a b -> Bool
isRightE = either (const False) (const True)

-- | 响应体里第一行（表格数据）
firstRow :: WT.SResponse -> [A.Value]
firstRow res = case items (at "rows" (jsonBody res)) of
    (r : _) -> case r of
        A.Array xs -> V.toList xs
        _ -> []
    [] -> []

-- | 数组字段的长度
arrayLen :: A.Value -> Int
arrayLen = length . items

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    staticDir <- runIO makeStaticDir
    unitSpec
    actionsSpec
    staticSpec staticDir
    frontendBundleSpec
    authSpec staticDir
    catalogSpec staticDir
    limitsSpec staticDir
    oneClickSpec staticDir
    sqlSpec staticDir
    settingsSpec staticDir
    uiSettingsSpec staticDir
    sortSpec staticDir
    securitySpec staticDir
    e2eSpec staticDir

unitSpec :: Spec
unitSpec = do
    describe "Password hashing" $ do
        it "accepts the correct password" $
            verifyPassword (hashPasswordWith 1000 (BS.replicate 16 1) "s3cret") "s3cret" `shouldBe` True
        it "rejects a wrong password" $
            verifyPassword (hashPasswordWith 1000 (BS.replicate 16 1) "s3cret") "wrong" `shouldBe` False
        it "treats a broken encoded hash as mismatch instead of throwing" $
            verifyPassword "garbage" "s3cret" `shouldBe` False
        it "same password with different salts yields different hashes" $
            hashPasswordWith 1000 (BS.replicate 16 1) "s3cret"
                `shouldNotBe` hashPasswordWith 1000 (BS.replicate 16 2) "s3cret"
        it "encoded hash has four parts: algo$iterations$salt$hash" $
            T.splitOn "$" (hashPasswordWith 1000 (BS.replicate 16 3) "s3cret")
                `shouldSatisfy` ((== 4) . length)

    describe "Sessions" $ do
        it "token is 64 hex chars and differs between calls" $ do
            a <- sessionToken
            b <- sessionToken
            T.length a `shouldBe` 64
            T.all (`elem` ("0123456789abcdef" :: String)) a `shouldBe` True
            a `shouldNotBe` b
        it "idle timeout drops the session" $ do
            clock <- newIORef epoch
            store <- newSessionStore (readIORef clock) policy
            token <- createSession store "admin"
            fresh <- lookupSession store token
            fresh `shouldBe` Just "admin"
            writeIORef clock (addUTCTime (9 * 3600) epoch)
            stale <- lookupSession store token
            stale `shouldBe` Nothing
        it "activity refreshes the idle timer but the absolute limit still expires" $ do
            clock <- newIORef epoch
            store <- newSessionStore (readIORef clock) policy
            token <- createSession store "admin"
            writeIORef clock (addUTCTime (7 * 3600) epoch)
            seen <- lookupSession store token
            seen `shouldBe` Just "admin"
            writeIORef clock (addUTCTime (13 * 3600) epoch)
            seenAgain <- lookupSession store token
            seenAgain `shouldBe` Just "admin"
            writeIORef clock (addUTCTime (25 * 3600) epoch)
            gone <- lookupSession store token
            gone `shouldBe` Nothing
        it "deleted session is no longer found" $ do
            clock <- newIORef epoch
            store <- newSessionStore (readIORef clock) policy
            token <- createSession store "admin"
            deleteSession store token
            found <- lookupSession store token
            found `shouldBe` Nothing
        it "a new expiry policy takes effect immediately (settings hot reload path)" $ do
            clock <- newIORef epoch
            store <- newSessionStore (readIORef clock) (SessionPolicy (8 * 3600) (24 * 3600))
            token <- createSession store "admin"
            setSessionPolicy store (SessionPolicy 10 20)
            now <- sessionPolicy store
            spIdleSeconds now `shouldBe` 10
            writeIORef clock (addUTCTime 11 epoch)
            gone <- lookupSession store token
            gone `shouldBe` Nothing
        it "password change drops other sessions but keeps the current one" $ do
            clock <- newIORef epoch
            store <- newSessionStore (readIORef clock) policy
            keep <- createSession store "admin"
            other <- createSession store "admin"
            removed <- deleteOtherSessions store keep
            kept <- lookupSession store keep
            dropped <- lookupSession store other
            removed `shouldBe` 1
            kept `shouldBe` Just "admin"
            dropped `shouldBe` Nothing
        it "no other sessions means zero removed (visible in logs)" $ do
            clock <- newIORef epoch
            store <- newSessionStore (readIORef clock) policy
            keep <- createSession store "admin"
            removed <- deleteOtherSessions store keep
            removed `shouldBe` 0
            stillThere <- lookupSession store keep
            stillThere `shouldBe` Just "admin"

    describe "Login rate limiting" $ do
        it "below the limit is not blocked" $ do
            clock <- newIORef epoch
            rl <- newRateLimiter (readIORef clock) 3 60
            rateLimitRecord rl "admin"
            rateLimitRecord rl "admin"
            open <- rateLimitBlock rl "admin"
            open `shouldBe` Nothing
        it "hitting the limit blocks and returns seconds to wait" $ do
            clock <- newIORef epoch
            rl <- newRateLimiter (readIORef clock) 3 60
            mapM_ (const (rateLimitRecord rl "admin")) [1 :: Int, 2, 3]
            blocked <- rateLimitBlock rl "admin"
            blocked `shouldSatisfy` maybe False (> 0)
        it "window expiry allows retries again" $ do
            clock <- newIORef epoch
            rl <- newRateLimiter (readIORef clock) 3 60
            mapM_ (const (rateLimitRecord rl "admin")) [1 :: Int, 2, 3]
            writeIORef clock (addUTCTime 61 epoch)
            open <- rateLimitBlock rl "admin"
            open `shouldBe` Nothing
        it "raising the threshold unlocks immediately (settings hot reload)" $ do
            clock <- newIORef epoch
            rl <- newRateLimiter (readIORef clock) 3 60
            mapM_ (const (rateLimitRecord rl "admin")) [1 :: Int, 2, 3]
            blocked <- rateLimitBlock rl "admin"
            blocked `shouldSatisfy` maybe False (> 0)
            setRateLimit rl 10 60
            unlocked <- rateLimitBlock rl "admin"
            unlocked `shouldBe` Nothing
        it "shortening the window allows immediately (settings hot reload)" $ do
            clock <- newIORef epoch
            rl <- newRateLimiter (readIORef clock) 3 3600
            mapM_ (const (rateLimitRecord rl "admin")) [1 :: Int, 2, 3]
            setRateLimit rl 3 1
            writeIORef clock (addUTCTime 2 epoch)
            open <- rateLimitBlock rl "admin"
            open `shouldBe` Nothing

    describe "Cookie parsing" $ do
        it "picks the session token out of many cookies" $
            parseCookieHeader "chusql_session" "a=1; chusql_session=abc123; b=2" `shouldBe` Just "abc123"
        it "missing key yields Nothing" $
            parseCookieHeader "chusql_session" "a=1; b=2" `shouldBe` Nothing
        it "empty value does not count as a value" $
            parseCookieHeader "chusql_session" "chusql_session=" `shouldBe` Nothing

    describe "Password hash validation (checked before changing the password)" $ do
        it "self-computed hash is valid" $
            hashLooksValid (hashPasswordWith 1000 (BS.replicate 16 5) "s3cret") `shouldBe` True
        it "broken hashes are invalid (bad length/algo/parts)" $ do
            hashLooksValid "garbage" `shouldBe` False
            hashLooksValid "pbkdf2-sha256$1000$00$00" `shouldBe` False
            hashLooksValid "md5$1000$00$00" `shouldBe` False

    describe "Settings catalogue (shared by the settings page and the server)" $ do
        it "keys are unique and kind/default are filled" $ do
            let keys = map siKey settingCatalogue
            length keys `shouldBe` length (nub keys)
            all (not . T.null . siKey) settingCatalogue `shouldBe` True
            all (not . T.null . siLabel) settingCatalogue `shouldBe` True
            all (\i -> siKind i `elem` ["text", "int", "bool", "secret"]) settingCatalogue `shouldBe` True
            let machineReadable = [i | i <- settingCatalogue, siKind i `elem` ["int", "bool"]]
            machineReadable `shouldSatisfy` (not . null)
            all (not . T.null . siDefault) machineReadable `shouldBe` True
            [siDefault i | i <- settingCatalogue, siKind i == "secret"] `shouldSatisfy` all T.null
            let envs = map siEnv settingCatalogue
            all (not . T.null) envs `shouldBe` True
            length envs `shouldBe` length (nub envs)
        it "every item has an env var and can be found by key" $ do
            all (not . T.null . siEnv) settingCatalogue `shouldBe` True
            map (fmap siKey . findItem . siKey) settingCatalogue `shouldBe` map (Just . siKey) settingCatalogue
        it "critical settings are root-only, tuning ones are not" $ do
            isRootOnly "port" `shouldBe` True
            isRootOnly "host" `shouldBe` True
            isRootOnly "user" `shouldBe` True
            isRootOnly "password-hash" `shouldBe` True
            isRootOnly "rows-per-page" `shouldBe` False
            isRootOnly "body-limit" `shouldBe` False
        it "live keys exist in the catalogue and are all integers" $ do
            liveKeys `shouldNotSatisfy` null
            map findItem liveKeys `shouldSatisfy` all (/= Nothing)
            [siKind i | Just i <- map findItem liveKeys] `shouldSatisfy` all (== "int")
        it "restart and live flags do not conflict: port needs a restart, page size does not" $ do
            isRestartRequired "port" `shouldBe` True
            isRestartRequired "static-dir" `shouldBe` True
            isRestartRequired "rows-per-page" `shouldBe` False
            filter isRestartRequired liveKeys `shouldBe` []
        it "defaults are found, an unknown key gives an empty string" $ do
            defaultOf "port" `shouldBe` "7777"
            defaultOf "rows-per-page" `shouldBe` "25"
            defaultOf "nope" `shouldBe` ""
        it "the flattened startup config carries the real port/account" $ do
            let flat = effectiveSettings defaultWebConfig
            Map.lookup "port" flat `shouldBe` Just "7777"
            Map.lookup "user" flat `shouldBe` Just defaultUser
            Map.lookup "rows-per-page" flat `shouldBe` Just "25"
            Map.member "password" flat `shouldBe` False
            Map.member "password-hash" flat `shouldBe` False
        it "merge settings: known keys applied, unknown rejected, empty clears" $ do
            let merged = applySettings Map.empty (Map.fromList [("rows-per-page", "50"), ("port", "9000")])
            fmap (Map.lookup "rows-per-page") merged `shouldBe` Right (Just "50")
            applySettings Map.empty (Map.fromList [("nope", "1")]) `shouldSatisfy` isLeftE
            fmap (Map.lookup "port") (applySettings (Map.fromList [("port", "9000")]) (Map.fromList [("port", "")]))
                `shouldBe` Right (Just "")
        it "type validation: ints reject letters, bools accept only known spellings" $ do
            applySettings Map.empty (Map.fromList [("rows-per-page", "abc")]) `shouldSatisfy` isLeftE
            applySettings Map.empty (Map.fromList [("rows-per-page", "10")]) `shouldSatisfy` isRightE
            applySettings Map.empty (Map.fromList [("seed", "maybe")]) `shouldSatisfy` isLeftE
            applySettings Map.empty (Map.fromList [("seed", "on")]) `shouldSatisfy` isRightE
        it "write settings file: empty values are not stored and reads come back" $ do
            tmp <- getTemporaryDirectory
            let path = tmp </> "chusql-web-test-write-settings.json"
            removeIfExists path
            written <- writeSettingsFile path (Map.fromList [("port", "9000"), ("host", "")])
            written `shouldBe` Right ()
            stored <- readSettingsFile path
            Map.lookup "port" stored `shouldBe` Just "9000"
            Map.member "host" stored `shouldBe` False
        it "write settings file: reports failure when the directory cannot be created" $ do
            tmp <- getTemporaryDirectory
            let blocker = tmp </> "chusql-web-test-blocker"
            removeIfExists blocker
            BS.writeFile blocker "not a directory"
            failed <- writeSettingsFile (blocker </> "settings.json") (Map.fromList [("port", "1")])
            failed `shouldSatisfy` isLeftE
            removeIfExists blocker
        it "an unreadable file counts as empty config (not a crash)" $ do
            tmp <- getTemporaryDirectory
            missing <- readSettingsFile (tmp </> "chusql-web-test-does-not-exist.json")
            missing `shouldBe` Map.empty
        it "settings file lookup: uses an existing file" $ do
            tmp <- getTemporaryDirectory
            let web = tmp </> "chusql-web-test-resolve"
            createDirectoryIfMissing True (web </> "script")
            BS.writeFile (web </> "script" </> "chusql.settings.json") "{\"port\":\"9000\"}"
            System.Directory.withCurrentDirectory web $ do
                found <- resolveSettingsFile
                found `shouldBe` ("script" </> "chusql.settings.json")
        it "settings file lookup: picks the candidate in an existing dir when none exists" $ do
            tmp <- getTemporaryDirectory
            let root = tmp </> "chusql-web-test-resolve-root"
            let web = root </> "chusql-web"
            createDirectoryIfMissing True (root </> "script")
            createDirectoryIfMissing True web
            System.Directory.withCurrentDirectory web $ do
                found <- resolveSettingsFile
                found `shouldBe` (".." </> "script" </> "chusql.settings.json")

    describe "Browsing SELECTs (column header click / WHERE filter cells)" $ do
        it "no sort and no filter yields a plain SELECT" $
            selectRowsSql "users" Nothing [] `shouldBe` Right "SELECT * FROM users"
        it "column and direction produce ORDER BY" $ do
            selectRowsSql "users" (Just ("age", True)) [] `shouldBe` Right "SELECT * FROM users ORDER BY age ASC"
            selectRowsSql "users" (Just ("age", False)) [] `shouldBe` Right "SELECT * FROM users ORDER BY age DESC"
        it "equality filters become WHERE with AND, placed before ORDER BY" $ do
            selectRowsSql "users" Nothing [("name", VStr "Ann"), ("age", VInt 30)]
                `shouldBe` Right "SELECT * FROM users WHERE name = 'Ann' AND age = 30"
            selectRowsSql "users" (Just ("age", False)) [("name", VStr "O'Brien")]
                `shouldBe` Right "SELECT * FROM users WHERE name = 'O''Brien' ORDER BY age DESC"
            selectRowsSql "users" Nothing [("vip", VBool True)]
                `shouldBe` Right "SELECT * FROM users WHERE vip = TRUE"
        it "table and column names go through the whitelist (including filter columns)" $ do
            selectRowsSql "users; DROP TABLE users" Nothing [] `shouldSatisfy` isLeftE
            selectRowsSql "users" (Just ("age; DROP TABLE users", True)) [] `shouldSatisfy` isLeftE
            selectRowsSql "users" Nothing [("age; DROP TABLE users", VInt 1)] `shouldSatisfy` isLeftE

    describe "Identifier whitelist" $ do
        it "normal table names pass" $ do
            isIdentifier "users" `shouldBe` True
            isIdentifier "user_1" `shouldBe` True
            isIdentifier "_tmp" `shouldBe` True
        it "leading digit / space / semicolon / empty string are rejected" $ do
            isIdentifier "1users" `shouldBe` False
            isIdentifier "user name" `shouldBe` False
            isIdentifier "users;DROP TABLE users" `shouldBe` False
            isIdentifier "" `shouldBe` False
        it "over 64 characters is rejected" $
            isIdentifier (T.replicate 65 "a") `shouldBe` False

    describe "Static file name whitelist" $ do
        it "normal file names pass" $ do
            safeRelative "app.js" `shouldBe` Just "app.js"
            safeRelative "style.css" `shouldBe` Just "style.css"
            safeRelative "codicon.ttf" `shouldBe` Just "codicon.ttf"
            safeRelative "editor.worker-a1.js" `shouldBe` Just "editor.worker-a1.js"
        it "traversal / subdirectory / empty string are all rejected" $ do
            safeRelative "../secret" `shouldBe` Nothing
            safeRelative "sub/app.js" `shouldBe` Nothing
            safeRelative "a\\b" `shouldBe` Nothing
            safeRelative "" `shouldBe` Nothing
            safeRelative "a b.js" `shouldBe` Nothing
        it "disallowed extensions fail (.token / .db / no extension)" $ do
            safeRelative "secret.token" `shouldBe` Nothing
            safeRelative "catalog.json" `shouldBe` Nothing
            safeRelative "data.db" `shouldBe` Nothing
            safeRelative "README" `shouldBe` Nothing

    describe "Content-Type" $ do
        it "chosen by extension (case-insensitive)" $ do
            contentTypeOf "index.html" `shouldBe` "text/html; charset=utf-8"
            contentTypeOf "app.JS" `shouldBe` "text/javascript; charset=utf-8"
            contentTypeOf "style.css" `shouldBe` "text/css; charset=utf-8"
            contentTypeOf "codicon.ttf" `shouldBe` "font/ttf"
            contentTypeOf "x.bin" `shouldBe` "application/octet-stream"

    describe "Column inference (headers even for empty results)" $ do
        it "SELECT * expands to the table columns" $
            columnsFromStatement testDb (selectStar (FromTable Nothing "users"))
                `shouldBe` ["id", "name", "age"]
        it "aliased SELECT * prefixes the alias" $
            columnsFromStatement testDb (selectStar (FromTable (Just "u") "users"))
                `shouldBe` ["u.id", "u.name", "u.age"]
        it "JOIN SELECT * puts the left table first" $
            columnsFromStatement
                testDb
                (selectStar (FromJoin (FromTable Nothing "users") (Just "o") "orders" (Eq (Col "id") (Col "user_id"))))
                `shouldBe` ["id", "name", "age", "o.id", "o.user_id"]
        it "explicit column names are kept as-is" $
            columnsFromStatement testDb (selectThese ["name"] (FromTable Nothing "users")) `shouldBe` ["name"]
        it "write statements have no result columns" $
            columnsFromStatement testDb (DropTable "users") `shouldBe` []

    describe "Row rendering" $ do
        it "values follow the given column order, missing columns give null" $
            renderRow ["name", "id", "age"] [("id", VInt 1), ("name", VStr "a")]
                `shouldBe` [A.String "a", A.Number 1, A.Null]

    describe "In-memory backend catalogue" $ do
        it "row count, columns and stats all match" $ do
            case lookup "users" testDb of
                Nothing -> expectationFailure "testDb has no users"
                Just tbl -> do
                    let info = tableInfoOf ("users", tbl)
                    tiTable info `shouldBe` "users"
                    tiRows info `shouldBe` 5
                    map scName (tiColumns info) `shouldBe` ["id", "name", "age"]
                    tiStats info `shouldBe` [("id", 5, False), ("name", 5, False), ("age", 5, False)]

    describe "Default account and credential source" $ do
        it "the default account is root and uses the built-in demo password" $ do
            let cfg = defaultWebConfig
            defaultUser `shouldBe` "root"
            wcUser cfg `shouldBe` "root"
            usingDefaultCredentials cfg `shouldBe` True
            cred <- resolveCredential cfg
            credUser cred `shouldBe` defaultUser
            verifyPassword (credEncoded cred) defaultPassword `shouldBe` True
        it "a plain password wins and the default stops working" $ do
            let cfg = defaultWebConfig{wcPassword = Just "s3cret"}
            usingDefaultCredentials cfg `shouldBe` False
            cred <- resolveCredential cfg
            verifyPassword (credEncoded cred) "s3cret" `shouldBe` True
            verifyPassword (credEncoded cred) defaultPassword `shouldBe` False
        it "a provided hash is used directly (not re-hashed)" $ do
            let encoded = hashPasswordWith 1000 (BS.replicate 16 9) "from-hash"
            cred <- resolveCredential defaultWebConfig{wcPasswordHash = Just encoded}
            credEncoded cred `shouldBe` encoded
        it "the account name can be changed" $ do
            cred <- resolveCredential defaultWebConfig{wcUser = "tester"}
            credUser cred `shouldBe` "tester"

    describe "Config: built-in defaults" $ do
        it "port defaults to 7777, account defaults to admin/chusql" $ do
            wcPort defaultWebConfig `shouldBe` 7777
            wcHost defaultWebConfig `shouldBe` "127.0.0.1"
            wcUser defaultWebConfig `shouldBe` defaultUser
            wcCookieSecure defaultWebConfig `shouldBe` False
        it "session / rate limit / paging / row limits all have explicit defaults" $ do
            wcSessionIdle defaultWebConfig `shouldBe` 8 * 3600
            wcSessionMax defaultWebConfig `shouldBe` 24 * 3600
            wcLoginMaxAttempts defaultWebConfig `shouldBe` 5
            wcLoginWindow defaultWebConfig `shouldBe` 5 * 60
            wcPageSize defaultWebConfig `shouldBe` 25
            wcMaxPageSize defaultWebConfig `shouldBe` 500
            wcMaxRows defaultWebConfig `shouldBe` 1000
            wcMaxSqlLength defaultWebConfig `shouldBe` 20000
            wcBodyLimit defaultWebConfig `shouldBe` 65536

    describe "Config: environment variables override defaults" $ do
        it "port / host / static dir / account" $ do
            withEnv
                [ ("CHUSQL_WEB_PORT", "8123")
                , ("CHUSQL_WEB_HOST", "0.0.0.0")
                , ("CHUSQL_WEB_STATIC", "mysite")
                , ("CHUSQL_WEB_USER", "tester")
                ]
                $ do
                    cfg <- loadWebConfig
                    wcPort cfg `shouldBe` 8123
                    wcHost cfg `shouldBe` "0.0.0.0"
                    wcStaticDir cfg `shouldBe` "mysite"
                    wcUser cfg `shouldBe` "tester"
        it "session / rate limit / paging / rows / body limit" $ do
            withEnv
                [ ("CHUSQL_WEB_SESSION_IDLE", "60")
                , ("CHUSQL_WEB_SESSION_MAX", "120")
                , ("CHUSQL_WEB_LOGIN_MAX_ATTEMPTS", "3")
                , ("CHUSQL_WEB_LOGIN_WINDOW", "30")
                , ("CHUSQL_WEB_PAGE_SIZE", "7")
                , ("CHUSQL_WEB_MAX_PAGE_SIZE", "70")
                , ("CHUSQL_WEB_MAX_ROWS", "9")
                , ("CHUSQL_WEB_MAX_SQL_LENGTH", "1234")
                , ("CHUSQL_WEB_BODY_LIMIT", "2048")
                , ("CHUSQL_WEB_COOKIE_SECURE", "1")
                ]
                $ do
                    cfg <- loadWebConfig
                    wcSessionIdle cfg `shouldBe` 60
                    wcSessionMax cfg `shouldBe` 120
                    wcLoginMaxAttempts cfg `shouldBe` 3
                    wcLoginWindow cfg `shouldBe` 30
                    wcPageSize cfg `shouldBe` 7
                    wcMaxPageSize cfg `shouldBe` 70
                    wcMaxRows cfg `shouldBe` 9
                    wcMaxSqlLength cfg `shouldBe` 1234
                    wcBodyLimit cfg `shouldBe` 2048
                    wcCookieSecure cfg `shouldBe` True
        it "unsetting the env var restores the default port 7777" $ do
            withEnv [("CHUSQL_WEB_PORT", "8123")] $ do
                tuned <- loadWebConfig
                wcPort tuned `shouldBe` 8123
            restored <- loadWebConfig
            wcPort restored `shouldBe` 7777

    describe "Launcher script (script/chusql.ps1)" $ do
        it "script/ has chusql.ps1 / chusql.cmd, default port 7777, covering all three modules" $ do
            script <- readUtf8 (".." </> "script" </> "chusql.ps1")
            batch <- readUtf8 (".." </> "script" </> "chusql.cmd")
            batch `shouldSatisfy` T.isInfixOf "chusql.ps1"
            script `shouldSatisfy` T.isInfixOf "chusql-storage"
            script `shouldSatisfy` T.isInfixOf "chusql-web.exe"
            script `shouldSatisfy` T.isInfixOf "/api/status"
            script `shouldSatisfy` T.isInfixOf "7777"
        it "has web / cli / config commands, cli is a placeholder (dev notice + still starts web)" $ do
            script <- readUtf8 (".." </> "script" </> "chusql.ps1")
            script `shouldSatisfy` T.isInfixOf "CLI interface is under development."
            script `shouldSatisfy` T.isInfixOf "'web'"
            script `shouldSatisfy` T.isInfixOf "'cli'"
            script `shouldSatisfy` T.isInfixOf "config set"
        it "settings persist to a file (config set/unset/reset + precedence note)" $ do
            script <- readUtf8 (".." </> "script" </> "chusql.ps1")
            script `shouldSatisfy` T.isInfixOf "chusql.settings.json"
            script `shouldSatisfy` T.isInfixOf "command line > settings file > environment > default"
            script `shouldSatisfy` T.isInfixOf "Save-SavedSettings"
        it "child window hidden + logs flow back to the launcher window" $ do
            script <- readUtf8 (".." </> "script" </> "chusql.ps1")
            script `shouldSatisfy` T.isInfixOf "-WindowStyle Hidden"
            script `shouldSatisfy` T.isInfixOf "Show-NewLogLines"
            script `shouldSatisfy` T.isInfixOf "RedirectStandardOutput"
        it "the script exposes settings as switches (port/session/paging/storage tuning)" $ do
            script <- readUtf8 (".." </> "script" </> "chusql.ps1")
            script `shouldSatisfy` T.isInfixOf "CHUSQL_WEB_SESSION_IDLE"
            script `shouldSatisfy` T.isInfixOf "CHUSQL_WEB_MAX_ROWS"
            script `shouldSatisfy` T.isInfixOf "CHUSQL_BUFFER_POOL_SIZE"
            script `shouldSatisfy` T.isInfixOf "CHUSQL_PAGE_SIZE"
        it "the Rust artifact is named chusql-storage (no longer server)" $ do
            script <- readUtf8 (".." </> "script" </> "chusql.ps1")
            script `shouldSatisfy` T.isInfixOf "target\\release\\chusql-storage.exe"
            script `shouldSatisfy` (not . T.isInfixOf "target\\release\\server.exe")

staticSpec :: FilePath -> Spec
staticSpec staticDir = describe "Static files" $ do
    it "reads index.html from the temp dir" $ do
        content <- readStatic staticDir "index.html"
        content `shouldSatisfy` maybe False (not . BS.null)
    it "an unreadable file gives Nothing" $
        readStatic staticDir "nope.js" `shouldReturn` Nothing
    it "a traversal path is unreadable" $
        readStatic staticDir "../secret.token" `shouldReturn` Nothing

frontendBundleSpec :: Spec
frontendBundleSpec = describe "React frontend build artifacts" $ do
    present <- runIO (doesDirectoryExist "static")
    let asset name = "static" </> name
        readAsset name = readUtf8 (asset name)
        -- | static/ 是构建产物，没构建就跳过
        check body
            | present = body
            | otherwise = pendingWith "static/ is not built; run npm run build under chusql-web/frontend"
    it "entry HTML exists and is non-empty" $ check $ do
        html <- BS.readFile (asset "index.html")
        html `shouldSatisfy` (not . BS.null)
    it "entry declares the Chinese page language" $ check $ do
        html <- readAsset "index.html"
        html `shouldSatisfy` T.isInfixOf "lang=\"zh-CN\""
    it "entry loads the app script only from /static/" $ check $ do
        html <- readAsset "index.html"
        html `shouldSatisfy` T.isInfixOf "src=\"/static/app.js\""
    it "entry loads styles only from /static/" $ check $ do
        html <- readAsset "index.html"
        html `shouldSatisfy` T.isInfixOf "href=\"/static/style.css\""
    it "entry has no inline script" $ check $ do
        html <- readAsset "index.html"
        html `shouldSatisfy` (not . T.isInfixOf "<script>")
    it "entry has no inline style" $ check $ do
        html <- readAsset "index.html"
        html `shouldSatisfy` (not . T.isInfixOf "style=")
        html `shouldSatisfy` (not . T.isInfixOf "<style")
    it "app script exists and is non-empty" $ check $ do
        js <- BS.readFile (asset "app.js")
        js `shouldSatisfy` (not . BS.null)
    it "app script contains the centrally maintained Chinese UI copy" $ check $ do
        js <- readAsset "app.js"
        js `shouldSatisfy` T.isInfixOf "ChuSQL 数据库终端"
        js `shouldSatisfy` T.isInfixOf "待提交变更"
    it "main stylesheet uses the agreed Dark+ three-layer background" $ check $ do
        css <- readAsset "style.css"
        css `shouldSatisfy` T.isInfixOf "--bg-base:#1e1e1e"
        css `shouldSatisfy` T.isInfixOf "--bg-elevated:#252526"
        css `shouldSatisfy` T.isInfixOf "--bg-statusbar:#007acc"
    it "own styles have no gradients or shadows" $ check $ do
        css <- readAsset "style.css"
        css `shouldSatisfy` (not . T.isInfixOf "gradient(")
        css `shouldSatisfy` (not . T.isInfixOf "box-shadow")
    it "SQL language pack exists and is non-empty" $ check $ do
        sql <- BS.readFile (asset "sql.js")
        sql `shouldSatisfy` (not . BS.null)
    it "Monaco font exists and is non-empty" $ check $ do
        font <- BS.readFile (asset "codicon.ttf")
        font `shouldSatisfy` (not . BS.null)
    it "Monaco worker exists and is non-empty" $ check $ do
        names <- System.Directory.listDirectory "static"
        let workers = filter (\name -> "editor.worker-" `isInfixOf` name && ".js" `isSuffixOf` name) names
        workers `shouldSatisfy` (not . null)
        mapM_ (\name -> BS.readFile (asset name) >>= (`shouldSatisfy` (not . BS.null))) workers
    it "all build files pass the static file name whitelist" $ check $ do
        names <- System.Directory.listDirectory "static"
        mapM_ (\name -> safeRelative (T.pack name) `shouldBe` Just name) names

actionsSpec :: Spec
actionsSpec = describe "One-click actions -> one SQL statement (ChuSQL.Web.Actions)" $ do
    it "create table: column types and identifiers are validated" $ do
        createTableSql (CreateTableSpec "customers" [ColumnSpec "id" "int", ColumnSpec "name" "str"])
            `shouldBe` Right "CREATE TABLE customers (id int, name str)"
        createTableSql (CreateTableSpec "bad name" [ColumnSpec "id" "int"]) `shouldSatisfy` isLeftE
        createTableSql (CreateTableSpec "t" []) `shouldSatisfy` isLeftE
        createTableSql (CreateTableSpec "t" [ColumnSpec "id" "blob"]) `shouldSatisfy` isLeftE
        createTableSql (CreateTableSpec "t" [ColumnSpec "1bad" "int"]) `shouldSatisfy` isLeftE
        createTableSql (CreateTableSpec "t" [ColumnSpec "id" "BOOL"]) `shouldSatisfy` isRightE
    it "string literal escaping: single quotes doubled, the rest unchanged" $ do
        sqlLiteral (VStr "O'Brien") `shouldBe` "'O''Brien'"
        sqlLiteral (VStr "a\\b") `shouldBe` "'a\\b'"
        sqlLiteral (VInt 7) `shouldBe` "7"
        sqlLiteral (VBool True) `shouldBe` "TRUE"
        sqlLiteral (VBool False) `shouldBe` "FALSE"
    it "statement shapes for insert / update / delete / index" $ do
        insertRowSql "t" [("id", VInt 1), ("name", VStr "a")]
            `shouldBe` Right "INSERT INTO t (id, name) VALUES (1, 'a')"
        insertRowsSql "t" [[("id", VInt 1)], [("id", VInt 2)]]
            `shouldBe` Right "INSERT INTO t (id) VALUES (1), (2)"
        updateRowSql "t" 3 [("age", VInt 9)]
            `shouldBe` Right "UPDATE t SET age = 9 WHERE id = 3"
        deleteRowSql "t" 3 `shouldBe` Right "DELETE FROM t WHERE id = 3"
        createIndexSql "t" "sku" `shouldBe` Right "CREATE INDEX ON t (sku)"
        dropIndexSql "t" "sku" `shouldBe` Right "DROP INDEX ON t (sku)"
        dropTableSql "t" `shouldBe` Right "DROP TABLE t"
    it "injection shapes cannot get in (the whitelist runs before SQL is built)" $ do
        dropTableSql "users; DROP TABLE users" `shouldSatisfy` isLeftE
        createIndexSql "t" "c) --" `shouldSatisfy` isLeftE
        insertRowSql "t" [("id; DROP", VInt 1)] `shouldSatisfy` isLeftE
        insertRowSql "t" [] `shouldSatisfy` isLeftE
    it "JSON values coerce by column type: int takes numbers/numeric strings, str takes strings, bool takes booleans/true|false" $ do
        coerceValue TInt (A.Number 3) `shouldBe` Right (VInt 3)
        coerceValue TInt (A.String "42") `shouldBe` Right (VInt 42)
        coerceValue TInt (A.String "x") `shouldSatisfy` isLeftE
        coerceValue TStr (A.String "hi") `shouldBe` Right (VStr "hi")
        coerceValue TStr (A.Number 1) `shouldSatisfy` isLeftE
        coerceValue TBool (A.Bool True) `shouldBe` Right (VBool True)
        coerceValue TBool (A.String "false") `shouldBe` Right (VBool False)
        coerceValue TBool (A.String "maybe") `shouldSatisfy` isLeftE
    it "column type parsing: int/integer/str/text/varchar/bool/boolean are recognised" $ do
        map columnTypeOf ["int", "INTEGER", "str", "text", "varchar", "bool", "boolean"]
            `shouldBe` map Just [TInt, TInt, TStr, TStr, TStr, TBool, TBool]
        columnTypeOf "blob" `shouldBe` Nothing

oneClickSpec :: FilePath -> Spec
oneClickSpec staticDir = describe "One-click action endpoints" $
    withWorld staticDir $ \_ -> do
        it "create -> insert -> update -> delete -> drop through the structured endpoints (incl. bool columns and quote escaping)" $ do
            res <- loginAs "admin" "s3cret"
            let hs = cookieHeaders res
            created <-
                postWith
                    hs
                    "/api/tables"
                    "{\"name\":\"customers\",\"columns\":[{\"name\":\"id\",\"type\":\"int\"},{\"name\":\"name\",\"type\":\"str\"},{\"name\":\"vip\",\"type\":\"bool\"}]}"
            check (hasStatus created 200)
            inserted <- postWith hs "/api/tables/customers/rows" "{\"values\":{\"id\":1,\"name\":\"O'Brien\",\"vip\":true}}"
            check (hasStatus inserted 200)
            page <- getWith hs "/api/tables/customers/rows"
            check (asInt (at "total" (jsonBody page)) `shouldBe` 1)
            check (firstRow page `shouldBe` [A.Number 1, A.String "O'Brien", A.Bool True])
            patched <-
                request
                    "PATCH"
                    "/api/tables/customers/rows/1"
                    (("Content-Type", "application/json") : hs)
                    "{\"values\":{\"vip\":false,\"name\":\"O''Brien\"}}"
            check (hasStatus patched 200)
            afterEdit <- getWith hs "/api/tables/customers/rows"
            check (firstRow afterEdit `shouldBe` [A.Number 1, A.String "O''Brien", A.Bool False])
            removed <- request "DELETE" "/api/tables/customers/rows/1" hs ""
            check (hasStatus removed 200)
            empty <- getWith hs "/api/tables/customers/rows"
            check (asInt (at "total" (jsonBody empty)) `shouldBe` 0)
            dropped <- request "DELETE" "/api/tables/customers" hs ""
            check (hasStatus dropped 200)
            gone <- getWith hs "/api/tables/customers"
            check (hasStatus gone 404)
        it "bad input always gives 400/404, never a half-built SQL" $ do
            res <- loginAs "admin" "s3cret"
            let hs = cookieHeaders res
            badName <- postWith hs "/api/tables" "{\"name\":\"bad name\",\"columns\":[{\"name\":\"id\",\"type\":\"int\"}]}"
            check (hasStatus badName 400)
            unknownColumn <- postWith hs "/api/tables/users/rows" "{\"values\":{\"nope\":1}}"
            check (hasStatus unknownColumn 400)
            missingColumns <- postWith hs "/api/tables/users/rows" "{\"values\":{\"id\":99}}"
            check (hasStatus missingColumns 400)
            badBody <- postWith hs "/api/tables/users/rows" "not json"
            check (hasStatus badBody 400)
            badId <- request "PATCH" "/api/tables/users/rows/abc" (("Content-Type", "application/json") : hs) "{\"values\":{\"age\":1}}"
            check (hasStatus badId 400)
            noTable <- postWith hs "/api/tables/nope/rows" "{\"values\":{\"id\":1}}"
            check (hasStatus noTable 404)
            badType <- postWith hs "/api/tables/users/rows" "{\"values\":{\"id\":1,\"name\":\"x\",\"age\":\"old\"}}"
            check (hasStatus badType 400)
        it "index: integer columns work, string columns are rejected by the API (duplicate rejection is up to real storage, see e2e)" $ do
            res <- loginAs "admin" "s3cret"
            let hs = cookieHeaders res
            okIndex <- postWith hs "/api/tables/users/indexes" "{\"column\":\"age\"}"
            check (hasStatus okIndex 200)
            stringIndex <- postWith hs "/api/tables/users/indexes" "{\"column\":\"name\"}"
            check (hasStatus stringIndex 400)
            dropped <- request "DELETE" "/api/tables/users/indexes/age" hs ""
            check (hasStatus dropped 200)
            builtIn <- request "DELETE" "/api/tables/users/indexes/id" hs ""
            check (hasStatus builtIn 400)
        it "drop column: the column is gone but rows stay; built-in id and unknown column/table are rejected" $ do
            res <- loginAs "admin" "s3cret"
            let hs = cookieHeaders res
            created <- postWith hs "/api/tables" "{\"name\":\"drop_col_t\",\"columns\":[{\"name\":\"id\",\"type\":\"int\"},{\"name\":\"code\",\"type\":\"int\"},{\"name\":\"note\",\"type\":\"str\"}]}"
            check (hasStatus created 200)
            inserted <- postWith hs "/api/tables/drop_col_t/rows" "{\"values\":{\"id\":1,\"code\":7,\"note\":\"a\"}}"
            check (hasStatus inserted 200)
            droppedColumn <- request "DELETE" "/api/tables/drop_col_t/columns/code" hs ""
            check (hasStatus droppedColumn 200)
            info <- getWith hs "/api/tables/drop_col_t"
            check $ do
                let names = map (asText . at "name") (items (at "columns" (jsonBody info)))
                names `shouldBe` ["id", "note"]
            page <- getWith hs "/api/tables/drop_col_t/rows"
            check (hasStatus page 200)
            check (map asText (items (at "columns" (jsonBody page))) `shouldBe` ["id", "note"])
            check (firstRow page `shouldBe` [A.Number 1, A.String "a"])
            builtInColumn <- request "DELETE" "/api/tables/drop_col_t/columns/id" hs ""
            check (hasStatus builtInColumn 400)
            unknownColumn <- request "DELETE" "/api/tables/drop_col_t/columns/nope" hs ""
            check (hasStatus unknownColumn 400)
            unknownTable <- request "DELETE" "/api/tables/nope/columns/code" hs ""
            check (hasStatus unknownTable 404)
        it "demo data: fills what is missing, repeated clicks do not re-seed (users/orders already exist in memory)" $ do
            res <- loginAs "admin" "s3cret"
            let hs = cookieHeaders res
            first <- postWith hs "/api/demo-data" "{}"
            check (hasStatus first 200)
            check (arrayLen (at "created" (jsonBody first)) + arrayLen (at "skipped" (jsonBody first)) `shouldBe` 3)
            products <- getWith hs "/api/tables/products/rows?limit=100"
            check (asInt (at "total" (jsonBody products)) `shouldBe` 8)
            users <- getWith hs "/api/tables/users/rows?limit=100"
            check (asInt (at "total" (jsonBody users)) `shouldBe` 5)
            again <- postWith hs "/api/demo-data" "{}"
            check (hasStatus again 200)
            check (arrayLen (at "created" (jsonBody again)) `shouldBe` 0)
            check (arrayLen (at "skipped" (jsonBody again)) `shouldBe` 3)
            usersAgain <- getWith hs "/api/tables/users/rows?limit=100"
            check (asInt (at "total" (jsonBody usersAgain)) `shouldBe` 5)

authSpec :: FilePath -> Spec
authSpec staticDir = do
    describe "Auth: not logged in" $
        withWorld staticDir $ \_ -> do
            it "health check needs no login" $
                get "/api/health" `shouldRespondWith` 200
            it "table list gives 401" $
                get "/api/tables" `shouldRespondWith` 401
            it "table rows give 401" $
                get "/api/tables/users/rows" `shouldRespondWith` 401
            it "SQL console gives 401" $
                jsonRequest methodPost "/api/query" "{\"sql\":\"SELECT * FROM users\"}" `shouldRespondWith` 401
            it "session lookup gives 401" $
                get "/api/session" `shouldRespondWith` 401
            it "status needs no login and reports storage up (the memory backend is always up)" $ do
                res <- get "/api/status"
                check (hasStatus res 200)
                check (asText (at "storage" (jsonBody res)) `shouldBe` "up")
            it "the home page needs no login (the frontend switches to the login view)" $
                get "/" `shouldRespondWith` 200

    describe "Auth: login" $
        withWorld staticDir $ \_ -> do
            it "correct password: 204 + session cookie (HttpOnly / SameSite=Strict)" $ do
                res <- loginAs "admin" "s3cret"
                check (hasStatus res 204)
                let rawCookie = fmap TE.decodeUtf8 (lookup "Set-Cookie" (WT.simpleHeaders res))
                check (rawCookie `shouldSatisfy` maybe False (T.isInfixOf "HttpOnly"))
                check (rawCookie `shouldSatisfy` maybe False (T.isInfixOf "SameSite=Strict"))
                check (sessionCookieOf res `shouldSatisfy` maybe False (not . T.null))
            it "wrong password gives 401" $
                loginAs "admin" "nope" `shouldRespondWith` 401
            it "the username is case-insensitive but the password must match" $
                loginAs "ADMIN" "s3cret" `shouldRespondWith` 204
            it "non-JSON body gives 400" $
                jsonRequest methodPost "/api/login" "{not json" `shouldRespondWith` 400
            it "missing field gives 400" $
                jsonRequest methodPost "/api/login" "{\"user\":\"admin\"}" `shouldRespondWith` 400
            it "empty password gives 400" $
                jsonRequest methodPost "/api/login" "{\"user\":\"admin\",\"password\":\"\"}" `shouldRespondWith` 400
            it "a cookie allows session lookup" $ do
                res <- loginAs "admin" "s3cret"
                cs <- getWith (cookieHeaders res) "/api/session"
                check (hasStatus cs 200)
                check (asText (at "user" (jsonBody cs)) `shouldBe` "admin")
            it "logout invalidates the session immediately" $ do
                res <- loginAs "admin" "s3cret"
                out <- postWith (cookieHeaders res) "/api/logout" "{}"
                check (hasStatus out 204)
                gone <- getWith (cookieHeaders res) "/api/tables"
                check (hasStatus gone 401)
            it "a forged token gets nothing" $
                getWith [("Cookie", "chusql_session=deadbeef")] "/api/tables" `shouldRespondWith` 401

    describe "Auth: login failure rate limiting" $
        withWorld staticDir $ \_ -> do
            it "after 5 failures the 6th is 429 (even with the right password)" $ do
                mapM_ (const (loginAs "admin" "wrong")) [1 :: Int, 2, 3, 4, 5]
                blocked <- loginAs "admin" "s3cret"
                check (hasStatus blocked 429)
                check (lookup "Retry-After" (WT.simpleHeaders blocked) `shouldSatisfy` maybe False (not . BS.null))

catalogSpec :: FilePath -> Spec
catalogSpec staticDir = describe "Catalogue and paged browsing" $
    withWorld staticDir $ \_ -> do
        it "table list carries row counts, columns, indexes and stats" $ do
            res <- loginAs "admin" "s3cret"
            list <- getWith (cookieHeaders res) "/api/tables"
            check (hasStatus list 200)
            check $ do
                let tables = items (jsonBody list)
                map (asText . at "table") tables `shouldBe` ["users", "orders"]
                case tables of
                    [] -> expectationFailure "the table list is empty"
                    (usersInfo : _) -> do
                        asInt (at "rowCount" usersInfo) `shouldBe` 5
                        map (asText . at "name") (items (at "columns" usersInfo)) `shouldBe` ["id", "name", "age"]
                        let cols = items (at "columns" usersInfo)
                        case cols of
                            (firstCol : secondCol : _) -> do
                                at "primaryKey" firstCol `shouldBe` A.Bool True
                                at "prime" firstCol `shouldBe` A.Bool True
                                asInt (at "distinct" firstCol) `shouldBe` 5
                                at "primaryKey" secondCol `shouldBe` A.Bool False
                                at "prime" secondCol `shouldBe` A.Bool False
                                at "indexed" secondCol `shouldBe` A.Bool False
                            _ -> expectationFailure "users should have three columns"
        it "single table structure" $ do
            res <- loginAs "admin" "s3cret"
            info <- getWith (cookieHeaders res) "/api/tables/users"
            check (hasStatus info 200)
            check (asText (at "table" (jsonBody info)) `shouldBe` "users")
        it "missing table gives 404" $ do
            res <- loginAs "admin" "s3cret"
            missing <- getWith (cookieHeaders res) "/api/tables/nope"
            check (hasStatus missing 404)
        it "invalid table name gives 400 (never built into SQL)" $ do
            res <- loginAs "admin" "s3cret"
            bad <- getWith (cookieHeaders res) "/api/tables/1bad/rows"
            check (hasStatus bad 400)
        it "paging: limit/offset apply and total is the full row count" $ do
            res <- loginAs "admin" "s3cret"
            page <- getWith (cookieHeaders res) "/api/tables/users/rows?limit=2&offset=1"
            check (hasStatus page 200)
            check $ do
                let body = jsonBody page
                asInt (at "total" body) `shouldBe` 5
                asInt (at "limit" body) `shouldBe` 2
                asInt (at "offset" body) `shouldBe` 1
                let rows = items (at "rows" body)
                length rows `shouldBe` 2
                nth 0 rows `shouldBe` Just (A.Array (V.fromList [A.Number 2, A.String "user2", A.Number 22]))
        it "an out-of-range limit is clamped to the cap of 500" $ do
            res <- loginAs "admin" "s3cret"
            page <- getWith (cookieHeaders res) "/api/tables/users/rows?limit=99999"
            check (asInt (at "limit" (jsonBody page)) `shouldBe` 500)
        it "non-integer limit gives 400" $ do
            res <- loginAs "admin" "s3cret"
            bad <- getWith (cookieHeaders res) "/api/tables/users/rows?limit=abc"
            check (hasStatus bad 400)
        it "empty tables are browsable too (columns come from the catalogue)" $ do
            res <- loginAs "admin" "s3cret"
            created <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"CREATE TABLE empty_t (id int, name str)\"}"
            check (hasStatus created 200)
            emptyPage <- getWith (cookieHeaders res) "/api/tables/empty_t/rows"
            check (hasStatus emptyPage 200)
            check $ do
                asInt (at "total" (jsonBody emptyPage)) `shouldBe` 0
                map asText (items (at "columns" (jsonBody emptyPage))) `shouldBe` ["id", "name"]
        it "filter param: equality per column, multiple conditions are AND (the WHERE row above the data grid)" $ do
            res <- loginAs "admin" "s3cret"
            let hs = cookieHeaders res
            one <- getWith hs "/api/tables/users/rows?filter=%5B%7B%22column%22%3A%22name%22%2C%22value%22%3A%22user2%22%7D%5D"
            check (hasStatus one 200)
            check $ do
                asInt (at "total" (jsonBody one)) `shouldBe` 1
                firstRow one `shouldBe` [A.Number 2, A.String "user2", A.Number 22]
            two <-
                getWith
                    hs
                    "/api/tables/users/rows?filter=%5B%7B%22column%22%3A%22age%22%2C%22value%22%3A%2223%22%7D%2C%7B%22column%22%3A%22id%22%2C%22value%22%3A%223%22%7D%5D"
            check $ do
                asInt (at "total" (jsonBody two)) `shouldBe` 1
                firstRow two `shouldBe` [A.Number 3, A.String "user3", A.Number 23]
        it "filter param: a blank cell counts as unfilled; unknown column / uncoercible value / non-JSON give 400" $ do
            res <- loginAs "admin" "s3cret"
            let hs = cookieHeaders res
            blank <- getWith hs "/api/tables/users/rows?filter=%5B%7B%22column%22%3A%22name%22%2C%22value%22%3A%22%22%7D%5D"
            check (asInt (at "total" (jsonBody blank)) `shouldBe` 5)
            unknown <- getWith hs "/api/tables/users/rows?filter=%5B%7B%22column%22%3A%22nope%22%2C%22value%22%3A%22x%22%7D%5D"
            check (hasStatus unknown 400)
            badType <- getWith hs "/api/tables/users/rows?filter=%5B%7B%22column%22%3A%22age%22%2C%22value%22%3A%22old%22%7D%5D"
            check (hasStatus badType 400)
            notJson <- getWith hs "/api/tables/users/rows?filter=oops"
            check (hasStatus notJson 400)

-- | 换一组更小的上限再跑
limitsSpec :: FilePath -> Spec
limitsSpec staticDir = describe "Paging and row limits follow the config" $ do
    appEnv <- runIO (freshWorld staticDir)
    runIO (setLive appEnv (defaultLive{lvPageSize = 1, lvMaxPageSize = 2, lvMaxRows = 2}))
    with (webApp appEnv) $ do
        it "a missing limit uses the configured page size" $ do
            res <- loginAs "admin" "s3cret"
            page <- getWith (cookieHeaders res) "/api/tables/users/rows"
            check (hasStatus page 200)
            check (asInt (at "limit" (jsonBody page)) `shouldBe` 1)
        it "a limit above the cap is clamped to maxPageSize" $ do
            res <- loginAs "admin" "s3cret"
            page <- getWith (cookieHeaders res) "/api/tables/users/rows?limit=99"
            check (asInt (at "limit" (jsonBody page)) `shouldBe` 2)
        it "query results cap at maxRows rows and set truncated" $ do
            res <- loginAs "admin" "s3cret"
            out <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"SELECT * FROM users\"}"
            check (hasStatus out 200)
            check (length (items (at "rows" (jsonBody out))) `shouldBe` 2)
            check (at "truncated" (jsonBody out) `shouldBe` A.Bool True)

sqlSpec :: FilePath -> Spec
sqlSpec staticDir = describe "SQL console" $
    withWorld staticDir $ \_ -> do
        it "SELECT returns columns and rows" $ do
            res <- loginAs "admin" "s3cret"
            out <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"SELECT name, age FROM users WHERE age > 22 ORDER BY age\"}"
            check (hasStatus out 200)
            check $ do
                let body = jsonBody out
                map asText (items (at "columns" body)) `shouldBe` ["name", "age"]
                length (items (at "rows" body)) `shouldBe` 3
                asInt (at "rowCount" body) `shouldBe` 3
        it "inserted rows can be queried back" $ do
            res <- loginAs "admin" "s3cret"
            _ <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"INSERT INTO users (id, name, age) VALUES (9, 'newbie', 31)\"}"
            out <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"SELECT * FROM users WHERE id = 9\"}"
            check (length (items (at "rows" (jsonBody out))) `shouldBe` 1)
        it "multi-row INSERT is supported" $ do
            res <- loginAs "admin" "s3cret"
            _ <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"INSERT INTO users (id, name, age) VALUES (11, 'a', 1), (12, 'b', 2)\"}"
            out <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"SELECT * FROM users WHERE id > 10\"}"
            check (length (items (at "rows" (jsonBody out))) `shouldBe` 2)
        it "CREATE TABLE / DROP TABLE end-to-end" $ do
            res <- loginAs "admin" "s3cret"
            created <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"CREATE TABLE demo_t (id int, note str)\"}"
            check (hasStatus created 200)
            dropped <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"DROP TABLE demo_t\"}"
            check (hasStatus dropped 200)
        it "syntax error: 400 + the original message" $ do
            res <- loginAs "admin" "s3cret"
            bad <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"SELECT FROM WHERE\"}"
            check (hasStatus bad 400)
            check $ do
                asText (at "error" (jsonBody bad)) `shouldBe` "query_error"
                asText (at "message" (jsonBody bad)) `shouldSatisfy` (not . T.null)
        it "missing table gives 404" $ do
            res <- loginAs "admin" "s3cret"
            missing <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"SELECT * FROM nope\"}"
            check (hasStatus missing 404)
        it "empty sql gives 400" $ do
            res <- loginAs "admin" "s3cret"
            bad <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"   \"}"
            check (hasStatus bad 400)
        it "non-JSON body gives 400" $ do
            res <- loginAs "admin" "s3cret"
            bad <- postWith (cookieHeaders res) "/api/query" "oops"
            check (hasStatus bad 400)

-- | 设置接口：读写配置与改口令
settingsSpec :: FilePath -> Spec
settingsSpec staticDir = do
    describe "Settings endpoints" $
        withWorldAt staticDir "settings" $ \appEnv -> do
            it "not logged in gives 401" $
                get "/api/settings" `shouldRespondWith` 401
            it "read settings: every catalogue key is present and secrets only report whether set" $ do
                res <- loginAs "admin" "s3cret"
                body <- getWith (cookieHeaders res) "/api/settings"
                check (hasStatus body 200)
                check $ do
                    let payload = jsonBody body
                        entries = items (at "items" payload)
                    at "owner" payload `shouldBe` A.Bool True
                    T.null (asText (at "file" payload)) `shouldBe` False
                    length entries `shouldBe` length settingCatalogue
                    map (asText . at "key") entries `shouldBe` map siKey settingCatalogue
                    let secrets = [i | i <- entries, asText (at "kind" i) == "secret"]
                    secrets `shouldSatisfy` (not . null)
                    map (asText . at "value") secrets `shouldSatisfy` all T.null
                    let port = headEntry "port" entries
                    asText (at "source" port) `shouldBe` "default"
                    asText (at "value" port) `shouldBe` "7777"
                    at "rootOnly" port `shouldBe` A.Bool True
                    at "editable" port `shouldBe` A.Bool True
                    at "restart" port `shouldBe` A.Bool True
                    at "live" port `shouldBe` A.Bool False
                    let rows = headEntry "rows-per-page" entries
                    at "live" rows `shouldBe` A.Bool True
                    at "restart" rows `shouldBe` A.Bool False
            it "write settings: only catalogue keys are accepted and ints reject letters" $ do
                res <- loginAs "admin" "s3cret"
                unknown <- putWith (cookieHeaders res) "/api/settings" "{\"values\":{\"nope\":\"1\"}}"
                check (hasStatus unknown 400)
                badType <- putWith (cookieHeaders res) "/api/settings" "{\"values\":{\"rows-per-page\":\"abc\"}}"
                check (hasStatus badType 400)
                empty <- putWith (cookieHeaders res) "/api/settings" "{\"values\":{}}"
                check (hasStatus empty 400)
            it "write settings: live keys apply at once (page size + body limit)" $ do
                res <- loginAs "admin" "s3cret"
                saved <- putWith (cookieHeaders res) "/api/settings" "{\"values\":{\"rows-per-page\":\"2\",\"body-limit\":\"200\"}}"
                check (hasStatus saved 200)
                check (map asText (items (at "applied" (jsonBody saved))) `shouldMatchList` ["rows-per-page", "body-limit"])
                page <- getWith (cookieHeaders res) "/api/tables/users/rows"
                check (asInt (at "limit" (jsonBody page)) `shouldBe` 2)
                live <- liftIO (readLive appEnv)
                check (lvPageSize live `shouldBe` 2)
                check (lvBodyLimit live `shouldBe` 200)
                big <- jsonRequest methodPost "/api/query" (BL.fromStrict (BS.replicate 400 97))
                check (hasStatus big 413)
            it "write settings: restart-only keys are named" $ do
                res <- loginAs "admin" "s3cret"
                saved <- putWith (cookieHeaders res) "/api/settings" "{\"values\":{\"static-dir\":\"static\"}}"
                check (hasStatus saved 200)
                check (map asText (items (at "restartRequired" (jsonBody saved))) `shouldContain` ["static-dir"])
            it "write settings: persisted values survive the next start (read back) and are sourced as settings file" $ do
                res <- loginAs "admin" "s3cret"
                _ <- putWith (cookieHeaders res) "/api/settings" "{\"values\":{\"max-rows\":\"77\"}}"
                stored <- liftIO (readSettingsFile (aeSettingsFile appEnv))
                check (Map.lookup "max-rows" stored `shouldBe` Just "77")
                body <- getWith (cookieHeaders res) "/api/settings"
                check $ do
                    let entries = items (at "items" (jsonBody body))
                    asText (at "source" (headEntry "max-rows" entries)) `shouldBe` "settings file"
                    asText (at "value" (headEntry "max-rows" entries)) `shouldBe` "77"
            it "changing the max-rows setting truncates query results" $ do
                res <- loginAs "admin" "s3cret"
                _ <- putWith (cookieHeaders res) "/api/settings" "{\"values\":{\"max-rows\":\"2\"}}"
                out <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"SELECT * FROM users\"}"
                check (hasStatus out 200)
                check (length (items (at "rows" (jsonBody out))) `shouldBe` 2)
                check (at "truncated" (jsonBody out) `shouldBe` A.Bool True)
            it "non-config account: critical settings give 403, normal ones pass" $ do
                guest <- liftIO (createSession (aeSessions appEnv) "guest")
                let guestHeaders = [("Cookie", TE.encodeUtf8 ("chusql_session=" <> guest))]
                forbidden <- putWith guestHeaders "/api/settings" "{\"values\":{\"port\":\"9999\"}}"
                check (hasStatus forbidden 403)
                check (asText (at "error" (jsonBody forbidden)) `shouldBe` "forbidden")
                allowed <- putWith guestHeaders "/api/settings" "{\"values\":{\"rows-per-page\":\"4\"}}"
                check (hasStatus allowed 200)
                reloaded <- getWith guestHeaders "/api/settings"
                check $ do
                    let entries = items (at "items" (jsonBody reloaded))
                        port = headEntry "port" entries
                    asText (at "value" port) `shouldBe` "7777"
                    at "editable" port `shouldBe` A.Bool False
                    at "editable" (headEntry "rows-per-page" entries) `shouldBe` A.Bool True

    describe "Account: change password" $
        withWorldAt staticDir "password" $ \appEnv -> do
            it "not logged in gives 401" $
                postWith [] "/api/account/password" "{\"current\":\"s3cret\",\"next\":\"longenough\"}"
                    `shouldRespondWith` 401
            it "wrong current password: 400 bad_password" $ do
                res <- loginAs "admin" "s3cret"
                bad <- postWith (cookieHeaders res) "/api/account/password" "{\"current\":\"nope\",\"next\":\"longenough\"}"
                check (hasStatus bad 400)
                check (asText (at "error" (jsonBody bad)) `shouldBe` "bad_password")
            it "new password too short / same as the old one: 400" $ do
                res <- loginAs "admin" "s3cret"
                short <- postWith (cookieHeaders res) "/api/account/password" "{\"current\":\"s3cret\",\"next\":\"tiny\"}"
                check (hasStatus short 400)
                same <- postWith (cookieHeaders res) "/api/account/password" "{\"current\":\"s3cret\",\"next\":\"s3cret\"}"
                check (hasStatus same 400)
            it "after success: the new password logs in, the old one fails, other sessions are kicked and the hash is persisted" $ do
                tokenA <- liftIO (createSession (aeSessions appEnv) "admin")
                tokenB <- liftIO (createSession (aeSessions appEnv) "admin")
                let hdrs token = [("Cookie", TE.encodeUtf8 ("chusql_session=" <> token))]
                changed <- postWith (hdrs tokenA) "/api/account/password" "{\"current\":\"s3cret\",\"next\":\"brand-new-pass\"}"
                check (hasStatus changed 204)
                stillThere <- getWith (hdrs tokenA) "/api/session"
                check (hasStatus stillThere 200)
                kicked <- getWith (hdrs tokenB) "/api/session"
                check (hasStatus kicked 401)
                fresh <- loginAs "admin" "brand-new-pass"
                check (hasStatus fresh 204)
                stale <- loginAs "admin" "s3cret"
                check (hasStatus stale 401)
                stored <- liftIO (readSettingsFile (aeSettingsFile appEnv))
                check (Map.lookup "password-hash" stored `shouldSatisfy` maybe False hashLooksValid)
                check (Map.member "password" stored `shouldBe` False)

-- | 一份完整的合法 IDE 设置请求体
uiSettingsBody :: BL.ByteString
uiSettingsBody = "{\"uiFonts\":[\"JetBrains Mono\",\"Consolas\"],\"gridFonts\":[\"Consolas\"],\"sqlFonts\":[\"Mono\"],\"uiFontSize\":13,\"gridFontSize\":12,\"gridRowHeight\":24,\"sqlFontSize\":13,\"sqlLineHeight\":20,\"sqlTabSize\":2,\"pageSize\":200,\"nullText\":\"NULL\",\"sqlLineNumbers\":true,\"autocomplete\":true,\"minimap\":false,\"confirmBeforeCommit\":true}"

-- | 上面那份设置解出来的值
fullUiSettings :: A.Value
fullUiSettings = fromMaybe A.Null (A.decode uiSettingsBody)

-- | 在合法设置上换掉一个字段（试坏值用）
withUiField :: Text -> A.Value -> A.Value
withUiField key value = case fullUiSettings of
    A.Object o -> A.Object (KM.insert (K.fromText key) value o)
    other -> other

-- | n 项字体链
fontChainValue :: Int -> A.Value
fontChainValue n = A.Array (V.fromList (replicate n (A.String "Mono")))

uiSettingsSpec :: FilePath -> Spec
uiSettingsSpec staticDir = do
    describe "IDE settings validation" $ do
        it "accepts a complete valid object" $
            validateUiSettings fullUiSettings `shouldSatisfy` isRightE
        it "rejects an unknown key" $
            validateUiSettings (withUiField "nope" (A.Bool True)) `shouldSatisfy` isLeftE
        it "rejects a string where an integer is expected" $
            validateUiSettings (withUiField "uiFontSize" (A.String "13")) `shouldSatisfy` isLeftE
        it "rejects an integer outside its range" $ do
            validateUiSettings (withUiField "uiFontSize" (A.Number 10)) `shouldSatisfy` isLeftE
            validateUiSettings (withUiField "uiFontSize" (A.Number 17)) `shouldSatisfy` isLeftE
        it "rejects a font chain longer than ten entries" $
            validateUiSettings (withUiField "uiFonts" (fontChainValue 11)) `shouldSatisfy` isLeftE
        it "rejects a blank font name" $
            validateUiSettings (withUiField "gridFonts" (A.Array (V.fromList [A.String "  "]))) `shouldSatisfy` isLeftE
        it "rejects a number where a boolean is expected" $
            validateUiSettings (withUiField "minimap" (A.Number 1)) `shouldSatisfy` isLeftE
        it "names the offending key in the error" $
            case validateUiSettings (withUiField "pageSize" (A.Number 5)) of
                Left err -> err `shouldSatisfy` isInfixOf "pageSize"
                Right _ -> expectationFailure "expected a range error"

    describe "IDE settings file" $ do
        it "candidates look for the file in script/ first" $
            uiSettingsFileCandidates
                `shouldBe` [ "script" </> "chusql.ui.settings.json"
                           , ".." </> "script" </> "chusql.ui.settings.json"
                           , "chusql.ui.settings.json"
                           ]
        it "resolution picks one of the candidates" $ do
            path <- resolveUiSettingsFile
            path `shouldSatisfy` (`elem` uiSettingsFileCandidates)
        it "a missing file reads back as an empty object" $ do
            path <- tempUiSettingsPath "missing"
            removeIfExists path
            readUiSettings path `shouldReturn` A.Object KM.empty
        it "writing then reading round-trips the map and creates the directory" $ do
            tmp <- getTemporaryDirectory
            let dir = tmp </> "chusql-web-test-ui-file" </> "nested"
                path = dir </> "chusql.ui.settings.json"
                stored = Map.fromList [("minimap", A.Bool True), ("uiFontSize", A.Number 13)]
            removeIfExists path
            written <- writeUiSettings path stored
            written `shouldBe` Right ()
            readUiSettings path `shouldReturn` A.Object (KM.fromList [("minimap", A.Bool True), ("uiFontSize", A.Number 13)])

    describe "IDE settings endpoints" $
        withWorldAt staticDir "ui-settings" $ \appEnv -> do
            it "not logged in: GET and PUT both give 401" $ do
                get "/api/ui-settings" `shouldRespondWith` 401
                putWith [] "/api/ui-settings" uiSettingsBody `shouldRespondWith` 401
            it "a missing file reads back as an empty object" $ do
                res <- loginAs "admin" "s3cret"
                body <- getWith (cookieHeaders res) "/api/ui-settings"
                check (hasStatus body 200)
                check (jsonBody body `shouldBe` A.Object KM.empty)
            it "a valid body is stored whole and read back unchanged" $ do
                res <- loginAs "admin" "s3cret"
                saved <- putWith (cookieHeaders res) "/api/ui-settings" uiSettingsBody
                check (hasStatus saved 204)
                body <- getWith (cookieHeaders res) "/api/ui-settings"
                check (hasStatus body 200)
                check (jsonBody body `shouldBe` fullUiSettings)
            it "an unknown key gives 400 bad_request" $ do
                res <- loginAs "admin" "s3cret"
                bad <- putWith (cookieHeaders res) "/api/ui-settings" "{\"nope\":1}"
                check (hasStatus bad 400)
                check (asText (at "error" (jsonBody bad)) `shouldBe` "bad_request")
            it "a non-object JSON body gives 400" $ do
                res <- loginAs "admin" "s3cret"
                bad <- putWith (cookieHeaders res) "/api/ui-settings" "[]"
                check (hasStatus bad 400)
            it "a body over the live limit gives 413" $ do
                res <- loginAs "admin" "s3cret"
                check $ do
                    live <- readLive appEnv
                    setLive appEnv live{lvBodyLimit = 16}
                big <- putWith (cookieHeaders res) "/api/ui-settings" uiSettingsBody
                check (hasStatus big 413)

sortSpec :: FilePath -> Spec
sortSpec staticDir = describe "Sorting while browsing a table" $
    withWorld staticDir $ \_ -> do
        it "asc: the first row has the smallest age" $ do
            res <- loginAs "admin" "s3cret"
            page <- getWith (cookieHeaders res) "/api/tables/users/rows?sort=age&dir=asc"
            check (hasStatus page 200)
            check $ do
                asText (at "sort" (jsonBody page)) `shouldBe` "age"
                asText (at "dir" (jsonBody page)) `shouldBe` "asc"
                firstRow page `shouldBe` [A.Number 1, A.String "user1", A.Number 21]
        it "desc: the first row has the largest age" $ do
            res <- loginAs "admin" "s3cret"
            page <- getWith (cookieHeaders res) "/api/tables/users/rows?sort=age&dir=desc"
            check (hasStatus page 200)
            check (firstRow page `shouldBe` [A.Number 5, A.String "user5", A.Number 25])
        it "unknown column / invalid column name: 400" $ do
            res <- loginAs "admin" "s3cret"
            missing <- getWith (cookieHeaders res) "/api/tables/users/rows?sort=nope"
            check (hasStatus missing 400)
            invalid <- getWith (cookieHeaders res) "/api/tables/users/rows?sort=age%3B%20DROP%20TABLE%20users"
            check (hasStatus invalid 400)
        it "the direction accepts only asc/desc, anything else is asc" $ do
            res <- loginAs "admin" "s3cret"
            page <- getWith (cookieHeaders res) "/api/tables/users/rows?sort=age&dir=sideways"
            check (hasStatus page 200)
            check (asText (at "dir" (jsonBody page)) `shouldBe` "asc")

securitySpec :: FilePath -> Spec
securitySpec staticDir = do
    describe "Security: response headers and static files" $
        withWorld staticDir $ \_ -> do
            it "the per-page style nonce differs each time and matches the CSP" $ do
                first <- get "/"
                second <- get "/static/index.html"
                let responsePolicy res = fromMaybe "" (lookup "Content-Security-Policy" (WT.simpleHeaders res))
                    nonce res = BSC.takeWhile (/= '\'') (BSC.drop 7 (snd (BSC.breakSubstring "'nonce-" (responsePolicy res))))
                check $ do
                    BS.length (nonce first) `shouldBe` 64
                    nonce first `shouldNotBe` nonce second
                    BL.toStrict (WT.simpleBody first) `shouldSatisfy` BS.isInfixOf (nonce first)
                    BL.toStrict (WT.simpleBody second) `shouldSatisfy` BS.isInfixOf (nonce second)
                    responsePolicy first `shouldSatisfy` (not . BS.isInfixOf "unsafe-inline")
                    responsePolicy first `shouldSatisfy` BS.isInfixOf "script-src 'self';"
                    length (filter ((== "Content-Security-Policy") . fst) (WT.simpleHeaders first)) `shouldBe` 1
            it "every response carries CSP / nosniff / DENY / no-store" $ do
                res <- get "/api/health"
                let headerNames = map fst (WT.simpleHeaders res)
                check $ do
                    headerNames `shouldContain` ["Content-Security-Policy"]
                    headerNames `shouldContain` ["X-Content-Type-Options"]
                    headerNames `shouldContain` ["X-Frame-Options"]
                    headerNames `shouldContain` ["Cache-Control"]
            it "static JS uses the right Content-Type" $ do
                res <- get "/static/app.js"
                check (hasStatus res 200)
                check (lookup "Content-Type" (WT.simpleHeaders res) `shouldBe` Just "text/javascript; charset=utf-8")
            it "a missing static file gives 404" $
                get "/static/nope.js" `shouldRespondWith` 404
            it "URL-encoded traversal is rejected" $
                get "/static/%2e%2e%2fsecret.token" `shouldRespondWith` 404
            it "files outside the whitelist are unreadable" $
                get "/static/secret.token" `shouldRespondWith` 404
            it "an unmatched path gives 404 (JSON)" $ do
                res <- get "/api/nope"
                check (hasStatus res 404)
                check (asText (at "error" (jsonBody res)) `shouldBe` "not_found")
    describe "Security: cross-site and body size" $
        withWorld staticDir $ \_ -> do
            it "a cross-site Origin POST gives 403" $
                postWith
                    [("Origin", "http://evil.example"), ("Host", "127.0.0.1:8080")]
                    "/api/login"
                    (loginBody "admin" "s3cret")
                    `shouldRespondWith` 403
            it "a same-origin Origin POST works as usual (204 with the right password)" $ do
                res <-
                    postWith
                        [("Origin", "http://127.0.0.1:8080"), ("Host", "127.0.0.1:8080")]
                        "/api/login"
                        (loginBody "admin" "s3cret")
                check (hasStatus res 204)
            it "an oversized request with Content-Length gives 413" $
                postWith [("Content-Length", "999999")] "/api/login" (loginBody "admin" "s3cret")
                    `shouldRespondWith` 413
            it "a genuinely large body gives 413" $
                jsonRequest methodPost "/api/login" (BL.fromStrict (BS.replicate 70000 97))
                    `shouldRespondWith` 413

-- | 端到端：真 HTTP + 真 Rust 存储进程
e2eSpec :: FilePath -> Spec
e2eSpec staticDir = describe "End-to-end: browser -> Haskell -> Rust" $ do
    it "login -> create table -> insert -> query -> logout all the way through" $ do
        serverBin <- locateServerOrSkip
        started <- startStorageProcess serverBin
        running <- case started of
            Left err -> do
                expectationFailure err
                fail "the storage process did not start"
            Right ok -> pure ok
        bracket (pure running) stopStorageProcess $ \sp -> do
            backend <- ipcBackend
            clock <- newIORef =<< getCurrentTime
            sessions <- newSessionStore (readIORef clock) defaultSessionPolicy
            limiter <- newRateLimiter (readIORef clock) 50 300
            appEnv <- newAppEnv backend sessions testCredential limiter staticDir
            app <- webApp appEnv
            Warp.testWithApplication (pure app) $ \boundPort -> do
                mgr <- newManager defaultManagerSettings
                anon <- e2eGet mgr boundPort "/api/tables" []
                statusCode (responseStatus anon) `shouldBe` 401
                signedIn <- e2ePost mgr boundPort "/api/login" (loginBody "admin" "s3cret") []
                statusCode (responseStatus signedIn) `shouldBe` 204
                let cookie = e2eCookie signedIn
                cookie `shouldSatisfy` maybe False (not . BS.null)
                let auth = [("Cookie", fromMaybe "" cookie)]
                created <- e2ePost mgr boundPort "/api/query" "{\"sql\":\"CREATE TABLE web_e2e (id int, name str)\"}" auth
                statusCode (responseStatus created) `shouldBe` 200
                inserted <-
                    e2ePost
                        mgr
                        boundPort
                        "/api/query"
                        "{\"sql\":\"INSERT INTO web_e2e (id, name) VALUES (1, 'a'), (2, 'b'), (3, 'c')\"}"
                        auth
                statusCode (responseStatus inserted) `shouldBe` 200
                selected <- e2ePost mgr boundPort "/api/query" "{\"sql\":\"SELECT id, name FROM web_e2e ORDER BY id\"}" auth
                statusCode (responseStatus selected) `shouldBe` 200
                let body = fromMaybe A.Null (decode (responseBody selected))
                asInt (at "rowCount" body) `shouldBe` 3
                nth 2 (items (at "rows" body)) `shouldBe` Just (A.Array (V.fromList [A.Number 3, A.String "c"]))
                listed <- e2eGet mgr boundPort "/api/tables" auth
                statusCode (responseStatus listed) `shouldBe` 200
                let tableNames = map (asText . at "table") (items (fromMaybe A.Null (decode (responseBody listed))))
                tableNames `shouldContain` ["web_e2e"]
                home <- e2eGet mgr boundPort "/" []
                statusCode (responseStatus home) `shouldBe` 200
                BL.toStrict (responseBody home) `shouldSatisfy` BS.isInfixOf "ChuSQL"
                browsed <- e2eGet mgr boundPort "/api/tables/web_e2e/rows?limit=2" auth
                statusCode (responseStatus browsed) `shouldBe` 200
                asInt (at "total" (fromMaybe A.Null (decode (responseBody browsed)))) `shouldBe` 3
                uiCreated <- e2ePost mgr boundPort "/api/tables" "{\"name\":\"web_ui\",\"columns\":[{\"name\":\"id\",\"type\":\"int\"},{\"name\":\"code\",\"type\":\"int\"}]}" auth
                statusCode (responseStatus uiCreated) `shouldBe` 200
                uiIns <- e2ePost mgr boundPort "/api/tables/web_ui/rows" "{\"values\":{\"id\":1,\"code\":5}}" auth
                statusCode (responseStatus uiIns) `shouldBe` 200
                uiIdx <- e2ePost mgr boundPort "/api/tables/web_ui/indexes" "{\"column\":\"code\"}" auth
                statusCode (responseStatus uiIdx) `shouldBe` 200
                uiInfo <- e2eGet mgr boundPort "/api/tables/web_ui" auth
                let uiJson = fromMaybe A.Null (decode (responseBody uiInfo))
                sort (map (asText . at "column") (items (at "indexes" uiJson))) `shouldBe` ["code", "id"]
                map (at "builtIn") (items (at "indexes" uiJson)) `shouldSatisfy` elem (A.Bool True)
                _ <- e2ePost mgr boundPort "/api/tables/web_ui/rows" "{\"values\":{\"id\":2,\"code\":5}}" auth
                dupIdx <- e2ePost mgr boundPort "/api/tables/web_ui/indexes" "{\"column\":\"code\"}" auth
                statusCode (responseStatus dupIdx) `shouldBe` 400
                uiDel <- e2eSend mgr boundPort "DELETE" "/api/tables/web_ui/rows/2" "" auth
                statusCode (responseStatus uiDel) `shouldBe` 200
                uiRows <- e2eGet mgr boundPort "/api/tables/web_ui/rows" auth
                asInt (at "total" (fromMaybe A.Null (decode (responseBody uiRows)))) `shouldBe` 1
                droppedCol <- e2eSend mgr boundPort "DELETE" "/api/tables/web_ui/columns/code" "" auth
                statusCode (responseStatus droppedCol) `shouldBe` 200
                afterDrop <- e2eGet mgr boundPort "/api/tables/web_ui" auth
                let afterJson = fromMaybe A.Null (decode (responseBody afterDrop))
                map (asText . at "name") (items (at "columns" afterJson)) `shouldBe` ["id"]
                map (asText . at "column") (items (at "indexes" afterJson)) `shouldBe` ["id"]
                afterRows <- e2eGet mgr boundPort "/api/tables/web_ui/rows" auth
                let afterRowsJson = fromMaybe A.Null (decode (responseBody afterRows))
                asInt (at "total" afterRowsJson) `shouldBe` 1
                map asText (items (at "columns" afterRowsJson)) `shouldBe` ["id"]
                dropId <- e2eSend mgr boundPort "DELETE" "/api/tables/web_ui/columns/id" "" auth
                statusCode (responseStatus dropId) `shouldBe` 400
                demo <- e2ePost mgr boundPort "/api/demo-data" "{}" auth
                statusCode (responseStatus demo) `shouldBe` 200
                let demoJson = fromMaybe A.Null (decode (responseBody demo))
                arrayLen (at "created" demoJson) `shouldBe` 3
                demoProducts <- e2eGet mgr boundPort "/api/tables/products" auth
                let prodJson = fromMaybe A.Null (decode (responseBody demoProducts))
                map (asText . at "column") (items (at "indexes" prodJson)) `shouldContain` ["sku"]
                demoUsers <- e2eGet mgr boundPort "/api/tables/users/rows?limit=100" auth
                asInt (at "total" (fromMaybe A.Null (decode (responseBody demoUsers)))) `shouldBe` 10
                out <- e2ePost mgr boundPort "/api/logout" "{}" auth
                statusCode (responseStatus out) `shouldBe` 204
                afterLogout <- e2eGet mgr boundPort "/api/tables" auth
                statusCode (responseStatus afterLogout) `shouldBe` 401
                exists <- doesDirectoryExist (spDataDir sp)
                exists `shouldBe` True

-- | 找存储进程，cargo 不可用则 pending
locateServerOrSkip :: IO FilePath
locateServerOrSkip = do
    found <- firstDir [".." </> "chusql-storage", "chusql-storage"]
    case found of
        Nothing -> do
            expectationFailure "cannot find the chusql-storage directory (tests must run inside the repository)"
            pure ""
        Just storageDir -> do
            built <- buildServer storageDir
            case built of
                Left err
                    | "cannot run cargo" `isInfixOf` err -> do
                        pendingWith err
                        pure ""
                    | otherwise -> do
                        expectationFailure err
                        pure ""
                Right bin -> pure bin

-- | 编译 Rust 存储并找可执行文件
buildServer :: FilePath -> IO (Either String FilePath)
buildServer storageDir = do
    built <-
        try (createProcess (proc "cargo" ["build", "--bin", "chusql-storage"]){cwd = Just storageDir, std_out = NoStream, std_err = NoStream}) ::
            IO (Either IOException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
    case built of
        Left e -> pure (Left ("cannot run cargo (Rust is required for the IPC tests): " ++ show e))
        Right (_, _, _, ph) -> do
            code <- waitForProcess ph
            case code of
                ExitFailure n -> pure (Left ("cargo build --bin chusql-storage failed with exit code " ++ show n))
                ExitSuccess -> do
                    found <-
                        firstFile
                            [ storageDir </> "target" </> "debug" </> "chusql-storage.exe"
                            , storageDir </> "target" </> "debug" </> "chusql-storage"
                            ]
                    pure (maybe (Left "cargo build finished but no storage executable was found") Right found)

-- | 找第一个存在的目录
firstDir :: [FilePath] -> IO (Maybe FilePath)
firstDir [] = pure Nothing
firstDir (d : ds) = do
    ok <- doesDirectoryExist d
    if ok then pure (Just d) else firstDir ds

-- | 找第一个存在的文件
firstFile :: [FilePath] -> IO (Maybe FilePath)
firstFile [] = pure Nothing
firstFile (p : ps) = do
    ok <- doesFileExist p
    if ok then pure (Just p) else firstFile ps

-- | 从响应头里取会话 Cookie
e2eCookie :: Response BL.ByteString -> Maybe BS.ByteString
e2eCookie resp = case lookup "Set-Cookie" (responseHeaders resp) of
    Nothing -> Nothing
    Just raw -> Just (BSC.takeWhile (/= ';') raw)

-- | 端到端：任意方法的 JSON 请求
e2eSend :: Manager -> Int -> BS.ByteString -> BS.ByteString -> BL.ByteString -> [Header] -> IO (Response BL.ByteString)
e2eSend mgr port httpMethod path payload headers = do
    base <- parseRequest ("http://127.0.0.1:" ++ show port ++ BSC.unpack path)
    httpLbs
        base
            { method = httpMethod
            , requestBody = RequestBodyLBS payload
            , requestHeaders = ("Content-Type", "application/json") : headers
            }
        mgr

-- | 端到端 POST
e2ePost :: Manager -> Int -> BS.ByteString -> BL.ByteString -> [Header] -> IO (Response BL.ByteString)
e2ePost mgr port path payload headers = e2eSend mgr port "POST" path payload headers

-- | 端到端 GET
e2eGet :: Manager -> Int -> BS.ByteString -> [Header] -> IO (Response BL.ByteString)
e2eGet mgr port path headers = do
    base <- parseRequest ("http://127.0.0.1:" ++ show port ++ BSC.unpack path)
    httpLbs base{requestHeaders = headers} mgr
