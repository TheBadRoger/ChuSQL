{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import ChuSQL.Model (ColumnType (..), Database, Table (..), Value (..), plainColumn, pattern TInt, pattern TStr, pattern TBool)
import ChuSQL.Storage.IPC (Account (..), Request (..), SchemaColumn (..), TableInfo (..), setPipeName)
import ChuSQL.Web.Accounts
import Data.Either (isLeft, isRight)
import ChuSQL.Syntax.AST (Expr (..), FromClause (..), JoinKind (..), Statement (..))
import ChuSQL.Syntax.Parser (parseStatement)
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
import ChuSQL.Web.API (
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
import ChuSQL.Web.Backend (beAccounts, columnsFromStatement, inDatabase, ipcBackend, memoryBackend, tableInfoOf)
import ChuSQL.Web.Config (
    WebConfig (..),
    canonicalSettingKeys,
    defaultPassword,
    defaultUser,
    defaultWebConfig,
    loadWebConfigAt,
    resolveCredential,
    resolvePipeName,
    usingDefaultCredentials,
 )
import ChuSQL.Web.RateLimit (newRateLimiter, rateLimitBlock, rateLimitRecord, setRateLimit)
import ChuSQL.Web.Settings (
    SettingItem (..),
    applySettings,
    effectiveSettings,
    findItem,
    isRestartRequired,
    isRootOnly,
    liveKeys,
    readSettingsFile,
    settingCatalogue,
    writeSettingsFile,
 )
import ChuSQL.Web.Static (contentTypeOf, readStatic, safeRelative)
import ChuSQL.Web.StorageProcess (StorageProcess (..), platformBinaryName, startStorageProcess, storageChildArgs, stopStorageProcess, waitForStorage)
import ChuSQL.Web.TOML (defaultConfigFile, resolveConfigPath)
import ChuSQL.Web.UISettings (
    readUISettings,
    uiSettingsFileCandidates,
    validateUISettings,
    writeUISettings,
 )
import Control.Concurrent.MVar (newMVar)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (forM_)
import Data.Aeson (decode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.List (isInfixOf, isPrefixOf, nub, partition, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Calendar (fromGregorian)
import Data.Time.Clock (UTCTime (..), addUTCTime, getCurrentTime)
import Data.Unique (hashUnique, newUnique)
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
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, getXdgDirectory, removePathForcibly)
import qualified System.Directory
import System.Environment (getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (Handle)
import System.Info (os)
import System.Process (
    CreateProcess (cwd, std_err, std_out),
    ProcessHandle,
    StdStream (..),
    createProcess,
    proc,
    readCreateProcessWithExitCode,
    waitForProcess,
 )
import Test.Hspec
import qualified System.Process as Process
import Test.Hspec.Wai hiding (pendingWith)

-- ChuSQL Web 测试：纯函数、路由级（内存后端）、端到端（真 Rust 存储进程）。

-- | 夹具库名：服务不再有默认库，测试世界里这一个库扮演"已选中的库"。
--   它只是个普通库名（不再是保留名）——保留的只有 system。
testDatabaseName :: String
testDatabaseName = "test"

-- | 夹具库的请求头
testDatabaseHeader :: Header
testDatabaseHeader = ("X-ChuSQL-Database", TE.encodeUtf8 (T.pack testDatabaseName))

-- | 没有显式指定库的请求都落到夹具库；要测"没选库"的用例请直接用 `request`
fixtureHeaders :: [Header] -> [Header]
fixtureHeaders hs
    | any ((== "X-ChuSQL-Database") . fst) hs = hs
    | otherwise = testDatabaseHeader : hs

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
selectStar from = Select ["*"] from Nothing [] [] Nothing

-- | 造一条显式列名的 SELECT
selectThese :: [String] -> FromClause -> Statement
selectThese cols from = Select cols from Nothing [] [] Nothing

-- | with 块里的会话类型别名
type TestSession a = WaiSession () a

-- | 每个用例组一套：内存后端 + 可控时钟 + 临时静态目录
freshWorld :: FilePath -> IO AppEnv
freshWorld staticDir = freshWorldWith staticDir testCredential

-- | 同上，但换一个 root 凭据（免密用例用）
freshWorldWith :: FilePath -> Credential -> IO AppEnv
freshWorldWith staticDir credential = do
    db <- newMVar testDb
    clock <- newIORef epoch
    sessions <- newSessionStore (readIORef clock) policy
    limiter <- newRateLimiter (readIORef clock) 5 (5 * 60)
    env <- newAppEnv (memoryBackend testDatabaseName db) sessions credential limiter staticDir
    path <- tempSettingsPath "default"
    removeIfExists path
    uiPath <- tempUISettingsPath "default"
    removeIfExists uiPath
    pure env{aeSettingsFile = path, aeUISettingsFile = uiPath, aeEffective = Map.empty}

-- | 免密管理员的世界：空口令的凭据等价于"只允许管理员免密登录"
withPasswordlessWorld :: FilePath -> (AppEnv -> SpecWith ((), Application)) -> Spec
withPasswordlessWorld staticDir body = do
    appEnv <- runIO (freshWorldWith staticDir (Credential "admin" ""))
    with (webApp appEnv) (body appEnv)

-- | 临时设置文件（每个用例组一个名字，互不干扰）
tempSettingsPath :: String -> IO FilePath
tempSettingsPath name = do
    tmp <- getTemporaryDirectory
    pure (tmp </> ("chusql-web-test-" ++ name ++ "-settings.json"))

-- | 临时 IDE 设置文件（同样按用例组取名）
tempUISettingsPath :: String -> IO FilePath
tempUISettingsPath name = do
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
    uiPath <- tempUISettingsPath name
    removeIfExists uiPath
    pure env{aeSettingsFile = path, aeUISettingsFile = uiPath}

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

-- | 会话令牌变请求头；顺带带上夹具库——服务没有默认库，缺头就是"没选库"
cookieHeaders :: WT.SResponse -> [Header]
cookieHeaders res = case sessionCookieOf res of
    Nothing -> []
    Just token -> [("Cookie", TE.encodeUtf8 ("chusql_session=" <> token)), testDatabaseHeader]

-- | 把请求指到系统库 system（账号表与 system 库本身都在那里）
systemHeaders :: [Header] -> [Header]
systemHeaders hs = ("X-ChuSQL-Database", "system") : filter ((/= "X-ChuSQL-Database") . fst) hs

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
getWith headers path = request methodGet path (fixtureHeaders headers) ""

-- | 带请求头的 JSON POST
postWith :: [Header] -> BS.ByteString -> BL.ByteString -> TestSession WT.SResponse
postWith headers path payload =
    request methodPost path (fixtureHeaders (("Content-Type", "application/json") : headers)) payload

-- | 带请求头的 JSON PUT（改设置用的是 PUT）
putWith :: [Header] -> BS.ByteString -> BL.ByteString -> TestSession WT.SResponse
putWith headers path payload =
    request "PUT" path (fixtureHeaders (("Content-Type", "application/json") : headers)) payload

-- | 带请求头的 JSON PATCH（改行用的是 PATCH）
patchWith :: [Header] -> BS.ByteString -> BL.ByteString -> TestSession WT.SResponse
patchWith headers path payload =
    request "PATCH" path (fixtureHeaders (("Content-Type", "application/json") : headers)) payload

-- | 带请求头的 DELETE
deleteWith :: [Header] -> BS.ByteString -> TestSession WT.SResponse
deleteWith headers path = request "DELETE" path (fixtureHeaders headers) ""

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
    staticAssetSpec
    staticServeSpec
    authSpec staticDir
    passwordlessSpec staticDir
    catalogSpec staticDir
    limitsSpec staticDir
    oneClickSpec staticDir
    sqlSpec staticDir
    privilegesSpec staticDir
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

    describe "Account policy and lifecycle" $ do
        it "rejects invalid configurable policy limits" $ do
            mapM_ (\(key, value) -> applySettings Map.empty (Map.singleton key value) `shouldSatisfy` isLeft)
                [("password-min-length", "7"), ("password-classes", "5")]
        it "the admin is configuration only: no storage needed to sign in" $ do
            db <- newMVar testDb
            sessions <- newSessionStore (pure epoch) policy
            let backend = (memoryBackend testDatabaseName db){beAccounts = const (pure (Left "unavailable"))}
            service <- newAccounts backend sessions testCredential
            authenticate service "admin" "s3cret" >>= (`shouldSatisfy` isRight)
            listAccounts service >>= (`shouldSatisfy` isLeft)
        it "enforces password length and character classes" $ do
            let pol = defaultPasswordPolicy
            passwordAllowed pol "tiny" `shouldSatisfy` isLeft
            passwordAllowed pol "alllowercaseletters" `shouldSatisfy` isLeft
            passwordAllowed pol "long-password" `shouldBe` Right ()
        it "the admin name is reserved and ordinary names are validated" $ do
            db <- newMVar testDb
            sessions <- newSessionStore (pure epoch) policy
            service <- newAccounts (memoryBackend testDatabaseName db) sessions testCredential
            runAccountCommand service (Root "admin") (CreateAccount "ADMIN" "another-password") >>= (`shouldSatisfy` isLeft)
            runAccountCommand service (Root "admin") (CreateAccount "with space" "another-password") >>= (`shouldSatisfy` isLeft)
            runAccountCommand service (Root "admin") (CreateAccount "alice" "tiny") >>= (`shouldSatisfy` isLeft)
        it "creates, resets and drops ordinary accounts, and a reset revokes their sessions" $ do
            db <- newMVar testDb
            sessions <- newSessionStore (pure epoch) policy
            service <- newAccounts (memoryBackend testDatabaseName db) sessions testCredential
            let admin = Root "admin"
            runAccountCommand service admin (CreateAccount "Alice" "alice-password") >>= (`shouldBe` Right ())
            Right alice <- authenticate service "alice" "alice-password"
            Right bob <- authenticate service "admin" "s3cret"
            runAccountCommand service admin (ResetAccountPassword "alice" "alice-other-pass") >>= (`shouldBe` Right ())
            currentPrincipal service alice >>= (`shouldSatisfy` isLeft)
            currentPrincipal service bob >>= (`shouldSatisfy` isRight)
            authenticate service "alice" "alice-password" >>= (`shouldSatisfy` isLeft)
            authenticate service "alice" "alice-other-pass" >>= (`shouldSatisfy` isRight)
            runAccountCommand service admin (DropAccount "alice") >>= (`shouldBe` Right ())
            authenticate service "alice" "alice-other-pass" >>= (`shouldSatisfy` isLeft)
            runAccountCommand service admin (DropAccount "alice") >>= (`shouldSatisfy` isLeft)
        it "an ordinary account cannot administer accounts" $ do
            db <- newMVar testDb
            sessions <- newSessionStore (pure epoch) policy
            service <- newAccounts (memoryBackend testDatabaseName db) sessions testCredential
            let admin = Root "admin"
            runAccountCommand service admin (CreateAccount "alice" "alice-password") >>= (`shouldBe` Right ())
            Right token <- authenticate service "alice" "alice-password"
            Right principal <- currentPrincipal service token
            principalIsRoot principal `shouldBe` False
            principalName principal `shouldBe` "alice"
            runAccountCommand service principal (CreateAccount "bob" "bob-password") >>= (`shouldSatisfy` isLeft)
            runAccountCommand service principal (DropAccount "alice") >>= (`shouldSatisfy` isLeft)
            authenticate service "bob" "bob-password" >>= (`shouldSatisfy` isLeft)
        it "a failed account write keeps the old password" $ do
            db <- newMVar testDb
            sessions <- newSessionStore (pure epoch) policy
            let original = memoryBackend testDatabaseName db
                backend = original{beAccounts = \req -> case req of
                    ReqAccountReset {} -> pure (Left "write failed")
                    _ -> beAccounts original req}
            service <- newAccounts backend sessions testCredential
            let admin = Root "admin"
            runAccountCommand service admin (CreateAccount "alice" "alice-password") >>= (`shouldBe` Right ())
            runAccountCommand service admin (ResetAccountPassword "alice" "alice-other-pass") >>= (`shouldSatisfy` isLeft)
            authenticate service "alice" "alice-password" >>= (`shouldSatisfy` isRight)
            authenticate service "alice" "alice-other-pass" >>= (`shouldSatisfy` isLeft)
        it "a legacy row named like the administrator can be dropped, but not recreated" $ do
            db <- newMVar testDb
            sessions <- newSessionStore (pure epoch) policy
            _ <- beAccounts (memoryBackend testDatabaseName db) (ReqAccountCreate "admin" "legacy-hash")
            service <- newAccounts (memoryBackend testDatabaseName db) sessions testCredential
            runAccountCommand service (Root "admin") (CreateAccount "admin" "another-password") >>= (`shouldSatisfy` isLeft)
            runAccountCommand service (Root "admin") (ResetAccountPassword "admin" "another-password") >>= (`shouldSatisfy` isLeft)
            runAccountCommand service (Root "admin") (DropAccount "admin") >>= (`shouldBe` Right ())

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

    describe "Cookie parsing" $ do
        it "picks the session token out of many cookies" $
            parseCookieHeader "chusql_session" "a=1; chusql_session=abc123; b=2" `shouldBe` Just "abc123"
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
            let pairs = [(siSection i, siTomlKey i) | i <- settingCatalogue]
            [s | (s, _) <- pairs] `shouldSatisfy` all (not . T.null)
            [k | (_, k) <- pairs] `shouldSatisfy` all (not . T.null)
            length pairs `shouldBe` length (nub pairs)
            findItem "pipe-name" `shouldSatisfy` maybe False (\i -> (siSection i, siTomlKey i) == ("server", "pipe_name"))
            findItem "storage-page-size" `shouldSatisfy` maybe False (\i -> (siSection i, siTomlKey i) == ("page", "size"))
        it "critical settings are root-only, tuning ones are not" $ do
            isRootOnly "port" `shouldBe` True
            isRootOnly "host" `shouldBe` True
            isRootOnly "user" `shouldBe` True
            isRootOnly "password" `shouldBe` True
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
        it "the config file lives at the fixed place the installer writes" $ do
            fixed <- defaultConfigFile
            dir <- getXdgDirectory XdgConfig "ChuSQL"
            fixed `shouldBe` (dir </> "chusql.toml")
        it "an explicit --config path wins over the fixed place" $ do
            tmp <- getTemporaryDirectory
            let explicit = tmp </> "chusql-web-test-explicit.toml"
            resolveConfigPath (Just explicit) `shouldReturn` explicit
        it "without --config the fixed place is used" $ do
            fixed <- defaultConfigFile
            resolveConfigPath Nothing `shouldReturn` fixed

    describe "Browsing SELECTs (column header click / WHERE filter cells)" $ do
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
        it "normalizes default qualified headers" $ do
            fmap (columnsFromStatement testDb) (parseStatement "SELECT users.name FROM users WHERE FALSE") `shouldBe` Right ["name"]
        it "infers headers for a SELECT without FROM" $ do
            fmap (columnsFromStatement testDb) (parseStatement "SELECT 1 + 2 LIMIT 0") `shouldBe` Right ["1 + 2"]
        it "SELECT * expands to the table columns" $
            columnsFromStatement testDb (selectStar (FromTable Nothing "users"))
                `shouldBe` ["id", "name", "age"]
        it "aliased SELECT * prefixes the alias" $
            columnsFromStatement testDb (selectStar (FromTable (Just "u") "users"))
                `shouldBe` ["u.id", "u.name", "u.age"]
        it "JOIN SELECT * puts the left table first" $
            columnsFromStatement
                testDb
                (selectStar (FromJoin InnerJoin (FromTable Nothing "users") (Just "o") "orders" (Eq (Col "users.id") (Col "o.user_id"))))
                `shouldBe` ["users.id", "users.name", "users.age", "o.id", "o.user_id"]
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

    describe "Database qualification (inDatabase)" $ do
        let rewrite db sql = parseStatement sql >>= Right . inDatabase db

        it "prefixes the tables of a subquery as well" $ do
            case rewrite "sales" "SELECT name FROM users WHERE id IN (SELECT user_id FROM orders)" of
                Left err -> expectationFailure err
                Right stmt -> do
                    let text = show stmt
                    ("sales.users" `isInfixOf` text) `shouldBe` True
                    ("sales.orders" `isInfixOf` text) `shouldBe` True
                    ("\"orders\"" `isInfixOf` text) `shouldBe` False

        it "keeps the JOIN kind while prefixing both sides" $ do
            case rewrite "sales" "SELECT u.name FROM users u LEFT JOIN orders o ON u.id = o.user_id" of
                Left err -> expectationFailure err
                Right stmt -> do
                    let text = show stmt
                    ("LeftJoin" `isInfixOf` text) `shouldBe` True
                    ("sales.users" `isInfixOf` text) `shouldBe` True
                    ("sales.orders" `isInfixOf` text) `shouldBe` True
                    ("\"orders\"" `isInfixOf` text) `shouldBe` False

        it "qualifies a subquery that sits inside a JOIN condition" $ do
            case rewrite "sales" "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id AND EXISTS (SELECT 1 FROM orders x WHERE x.user_id = u.id)" of
                Left err -> expectationFailure err
                Right stmt -> do
                    let text = show stmt
                    ("sales.users" `isInfixOf` text) `shouldBe` True
                    ("sales.orders" `isInfixOf` text) `shouldBe` True
                    ("\"orders\"" `isInfixOf` text) `shouldBe` False

        it "leaves table names alone when no database is selected" $ do
            case rewrite "" "SELECT name FROM users WHERE id IN (SELECT user_id FROM orders)" of
                Left err -> expectationFailure err
                Right stmt -> do
                    let text = show stmt
                    ("\"users\"" `isInfixOf` text) `shouldBe` True
                    ("\"orders\"" `isInfixOf` text) `shouldBe` True
                    ("test." `isInfixOf` text) `shouldBe` False

    describe "Administrator credential source (settings file only)" $ do
        it "with nothing configured the built-in demo account is used" $ do
            defaultUser `shouldBe` "root"
            usingDefaultCredentials Map.empty `shouldBe` True
            cred <- resolveCredential defaultWebConfig Map.empty
            credUser cred `shouldBe` defaultUser
            verifyPassword (credEncoded cred) defaultPassword `shouldBe` True
        it "a plain password in the settings file wins" $ do
            let saved = Map.singleton "password" "s3cret"
            usingDefaultCredentials saved `shouldBe` False
            cred <- resolveCredential defaultWebConfig saved
            verifyPassword (credEncoded cred) "s3cret" `shouldBe` True
            verifyPassword (credEncoded cred) defaultPassword `shouldBe` False
        it "an explicitly empty password makes the administrator passwordless" $ do
            let saved = Map.singleton "password" ""
            usingDefaultCredentials saved `shouldBe` False
            cred <- resolveCredential defaultWebConfig saved
            credUser cred `shouldBe` defaultUser
            credEncoded cred `shouldBe` ""
            -- 空口令不是"能匹配任意口令"的哈希：它是一个明确的空编码串
            verifyPassword (credEncoded cred) "" `shouldBe` False
        it "the retired password-hash key is not a setting and never a credential" $ do
            let encoded = hashPasswordWith 1000 (BS.replicate 16 9) "from-hash"
            cred <- resolveCredential defaultWebConfig (Map.singleton "password-hash" encoded)
            verifyPassword (credEncoded cred) defaultPassword `shouldBe` True
            applySettings Map.empty (Map.singleton "password-hash" encoded) `shouldSatisfy` isLeftE
        it "the administrator name comes from the file, and --user still wins" $ do
            cred <- resolveCredential defaultWebConfig (Map.fromList [("user", "tester"), ("password", "s3cret")])
            credUser cred `shouldBe` "tester"
            fromCli <- resolveCredential defaultWebConfig{wcUser = "from-cli"} (Map.singleton "user" "tester")
            credUser fromCli `shouldBe` "from-cli"
        it "the password environment variables are no longer consulted" $ do
            tmp <- getTemporaryDirectory
            let missing = tmp </> "chusql-web-test-cred-env.toml"
            removeIfExists missing
            withEnv [("CHUSQL_WEB_PASSWORD", "from-env"), ("CHUSQL_WEB_USER", "env-user")] $ do
                cfg <- loadWebConfigAt missing
                wcUser cfg `shouldBe` ""
                cred <- resolveCredential cfg Map.empty
                credUser cred `shouldBe` defaultUser
                verifyPassword (credEncoded cred) "from-env" `shouldBe` False

    describe "Config: built-in defaults" $ do
        it "port defaults to 7777 and the administrator name is not baked into the config" $ do
            wcPort defaultWebConfig `shouldBe` 7777
            wcHost defaultWebConfig `shouldBe` "127.0.0.1"
            wcUser defaultWebConfig `shouldBe` ""
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

    describe "Config: environment variables are ignored (the file comes from --config)" $ do
        it "port / host / static dir / limits in the environment change nothing" $ do
            tmp <- getTemporaryDirectory
            let missing = tmp </> "chusql-web-test-ignored-env.toml"
            removeIfExists missing
            withEnv
                [ ("CHUSQL_WEB_PORT", "8123")
                , ("CHUSQL_WEB_HOST", "0.0.0.0")
                , ("CHUSQL_WEB_STATIC", "mysite")
                , ("CHUSQL_WEB_SESSION_IDLE", "60")
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
                    cfg <- loadWebConfigAt missing
                    cfg `shouldBe` defaultWebConfig
        it "the retired endpoint variables change nothing either" $ do
            tmp <- getTemporaryDirectory
            let missing = tmp </> "chusql-web-test-ignored-endpoint.toml"
            removeIfExists missing
            withEnv
                [ ("CHUSQL_PIPE", "from-env-pipe")
                , ("CHUSQL_DATA_DIR", "from-env-data")
                , ("CHUSQL_STORAGE_SERVER", "from-env-server")
                ]
                $ do
                    cfg <- loadWebConfigAt missing
                    wcPipeName cfg `shouldBe` Nothing
                    wcDataDir cfg `shouldBe` Nothing
                    wcStorageServer cfg `shouldBe` Nothing

    describe "Config: the sections of the global chusql.toml" $ do
        it "reads the section and accepts both underscore and hyphen spellings" $ do
            tmp <- getTemporaryDirectory
            let path = tmp </> "chusql-web-test-global.toml"
            writeFile path "[page]\nsize = 8192\n\n[web]\nstatic_dir = \"webroot\"\nsession_idle = 4242\nrows_per_page = 33\n"
            cfg <- loadWebConfigAt path
            wcStaticDir cfg `shouldBe` "webroot"
            wcSessionIdle cfg `shouldBe` 4242
            wcPageSize cfg `shouldBe` 33
            -- 别的层的分区不归 web 管，没给的键回到内置默认
            wcPort cfg `shouldBe` 7777
            wcMaxRows cfg `shouldBe` 1000
            saved <- readSettingsFile path
            Map.lookup "rows-per-page" saved `shouldBe` Just "33"
        it "the endpoint comes from [server] pipe_name and [storage] data_dir, the executable from [web] storage_server" $ do
            tmp <- getTemporaryDirectory
            let path = tmp </> "chusql-web-test-endpoint.toml"
            writeFile
                path
                "[server]\npipe_name = \"joint-pipe\"\n\n[storage]\ndata_dir = \"../localdata\"\n\n[web]\nstorage_server = \"chusql-storage.exe\"\n"
            cfg <- loadWebConfigAt path
            wcPipeName cfg `shouldBe` Just "joint-pipe"
            resolvePipeName cfg `shouldBe` "joint-pipe"
            wcDataDir cfg `shouldBe` Just "../localdata"
            wcStorageServer cfg `shouldBe` Just "chusql-storage.exe"
        it "without a file the pipe name falls back to the shared default" $ do
            resolvePipeName defaultWebConfig `shouldBe` "chusql-joint"
        it "a port in the environment no longer wins over the file" $ do
            tmp <- getTemporaryDirectory
            let path = tmp </> "chusql-web-test-global-env.toml"
            writeFile path "[web]\nport = 1234\nhost = \"0.0.0.0\"\n"
            withEnv [("CHUSQL_WEB_PORT", "4321")] $ do
                cfg <- loadWebConfigAt path
                wcPort cfg `shouldBe` 1234
                wcHost cfg `shouldBe` "0.0.0.0"
        it "the canonical hyphen key wins when both spellings are present" $ do
            let both = Map.fromList [("session-idle", "1"), ("session_idle", "2")]
            Map.lookup "session-idle" (canonicalSettingKeys both) `shouldBe` Just "1"
        it "a missing config file falls back to the built-in defaults" $ do
            tmp <- getTemporaryDirectory
            let missing = tmp </> "chusql-web-test-missing-config.toml"
            removeIfExists missing
            cfg <- loadWebConfigAt missing
            wcPort cfg `shouldBe` wcPort defaultWebConfig
            wcPageSize cfg `shouldBe` wcPageSize defaultWebConfig

    describe "Spawning the storage process" $ do
        it "points the child at the same config file on its command line" $ do
            storageChildArgs ("scripts" </> "chusql.toml")
                `shouldBe` ["--config", "scripts" </> "chusql.toml"]
        it "passes nothing else" $ do
            storageChildArgs "chusql.toml" `shouldBe` ["--config", "chusql.toml"]

    describe "Installer scripts (scripts/)" $ do
        it "install.sh releases files, writes chusql.toml, sets PATH and removes the package" $ do
            installer <- readUtf8 (".." </> "scripts" </> "install.sh")
            installer `shouldSatisfy` T.isInfixOf "chusql-storage"
            installer `shouldSatisfy` T.isInfixOf "chusql.toml"
            installer `shouldSatisfy` T.isInfixOf "PATH"
            installer `shouldSatisfy` T.isInfixOf "rm -rf"
        -- 一份脚本三种来源：包内（旁边有 bin/）、在线（管道进来时 $0 是 sh，按平台取 release 资源）、
        -- 显式归档或源码。去掉任何一条，从 GitHub 装的用法就断了。
        it "install.sh picks the package by platform and can fall back to source" $ do
            installer <- readUtf8 (".." </> "scripts" </> "install.sh")
            installer `shouldSatisfy` T.isInfixOf "bin/chusql-storage"
            installer `shouldSatisfy` T.isInfixOf "uname -s"
            installer `shouldSatisfy` T.isInfixOf "uname -m"
            installer `shouldSatisfy` T.isInfixOf "releases/latest/download"
            installer `shouldSatisfy` T.isInfixOf "chusql-$component-$label.$ext"
            installer `shouldSatisfy` T.isInfixOf ".sha256"
            installer `shouldSatisfy` T.isInfixOf "--url"
            installer `shouldSatisfy` T.isInfixOf "--from-source"
            installer `shouldSatisfy` T.isInfixOf "stack build --fast"
        -- static_dir 是相对路径，启动器必须先切到安装目录；少了这一句，从别处执行
        -- csql-web 就会因为找不到 static/ 直接退出（Windows 的 .ps1 靠 -WorkingDirectory）。
        it "csql-web.sh switches to the install dir before starting" $ do
            launcher <- readUtf8 (".." </> "scripts" </> "csql-web.sh")
            launcher `shouldSatisfy` T.isInfixOf "cd \"$home_dir\""
            launcher `shouldSatisfy` T.isInfixOf "chusql-storage"
        -- 默认数据目录按平台惯例算，四处必须说同一件事：Rust 代码、配置示例、模板、文档。
        -- 跟安装脚本装的位置也是一处：Windows 装到 %LOCALAPPDATA%\ChuSQL、Unix 装到 ~/.local/share/chusql，
        -- 数据都放在它下面的 data/ 里。
        it "the default data dir follows the platform, and code, template and docs agree" $ do
            rust <- readUtf8 (".." </> "chusql-storage" </> "src" </> "config.rs")
            rust `shouldSatisfy` T.isInfixOf "default_data_dir"
            rust `shouldSatisfy` T.isInfixOf "LOCALAPPDATA"
            rust `shouldSatisfy` T.isInfixOf "XDG_DATA_HOME"
            rust `shouldSatisfy` (not . T.isInfixOf "DEFAULT_DATA_DIR")

            example <- readUtf8 (".." </> "chusql-storage" </> "chusql-storage.toml.example")
            -- 示例文件说自己列的值就是默认值：data_dir 不能是能生效的一行
            example `shouldSatisfy` (not . T.isInfixOf "\ndata_dir =")
            example `shouldSatisfy` T.isInfixOf "%LOCALAPPDATA%"
            example `shouldSatisfy` T.isInfixOf "XDG_DATA_HOME"

            template <- readUtf8 (".." </> "scripts" </> "chusql.toml")
            -- 模板那行是安装时被 sed / -replace 替换的占位，必须保持能生效
            template `shouldSatisfy` T.isInfixOf "\ndata_dir = "
            template `shouldSatisfy` T.isInfixOf "%LOCALAPPDATA%"

            configDoc <- readUtf8 (".." </> "doc" </> "config.md")
            configDoc `shouldSatisfy` T.isInfixOf "%LOCALAPPDATA%\\ChuSQL\\data"
            configDoc `shouldSatisfy` T.isInfixOf "chusql/data"
        -- 端点规则两侧必须一致：Rust 服务端和 Haskell 前端算出来的必须是同一个路径。
        it "Rust and Haskell agree on where the Unix socket lives" $ do
            rustEndpoint <- readUtf8 (".." </> "chusql-storage" </> "src" </> "endpoint.rs")
            haskellIpc <- readUtf8 (".." </> "chusql-engine" </> "src" </> "ChuSQL" </> "Storage" </> "IPC.hs")
            mapM_
                ( \src -> do
                    src `shouldSatisfy` T.isInfixOf "XDG_RUNTIME_DIR"
                    src `shouldSatisfy` T.isInfixOf "TMPDIR"
                    src `shouldSatisfy` T.isInfixOf "/tmp"
                    src `shouldSatisfy` T.isInfixOf ".sock"
                )
                [rustEndpoint, haskellIpc]
        it "the Linux pipeline builds both toolchains and runs every test suite" $ do
            pipeline <- readUtf8 (".." </> ".github" </> "workflows" </> "linux.yml")
            pipeline `shouldSatisfy` T.isInfixOf "ubuntu-latest"
            pipeline `shouldSatisfy` T.isInfixOf "cargo test --release"
            pipeline `shouldSatisfy` T.isInfixOf "cargo clippy"
            pipeline `shouldSatisfy` T.isInfixOf "stack test --fast"
            -- 打包与安装冒烟只在发版流水线里做（tag 触发 / 手动 dispatch），这条流水线不再出包
            pipeline `shouldSatisfy` (not . T.isInfixOf "package.ps1")
            pipeline `shouldSatisfy` (not . T.isInfixOf "smoke-linux.sh")
            -- 仓库里没有 rustfmt.toml，历史代码不是按当前 rustfmt 排的：格式化不是门禁
            pipeline `shouldSatisfy` (not . T.isInfixOf "cargo fmt")
        -- 装的人不该被要求装两个工具链：发版流水线出预编译包，install.sh 按同一套平台标签去取。
        -- 装配与打归档原来在 scripts/package.ps1 里，脚本删掉后直接写在流水线里（两个平台各一段）。
        it "the release pipeline publishes one archive per platform, tagged the way install.sh looks it up" $ do
            release <- readUtf8 (".." </> ".github" </> "workflows" </> "release.yml")
            release `shouldSatisfy` T.isInfixOf "linux-x86_64"
            release `shouldSatisfy` T.isInfixOf "macos-arm64"
            release `shouldSatisfy` T.isInfixOf "macos-x86_64"
            release `shouldSatisfy` T.isInfixOf "windows-x86_64"
            release `shouldSatisfy` T.isInfixOf "cargo build --release"
            release `shouldSatisfy` T.isInfixOf "stack build --fast chusql-web:exe:chusql-web"
            release `shouldSatisfy` T.isInfixOf "stack build --fast chusql-cli:exe:csql"
            release `shouldSatisfy` T.isInfixOf "tar -czf"
            release `shouldSatisfy` T.isInfixOf "Compress-Archive"
            release `shouldSatisfy` T.isInfixOf "scripts/install.sh"
            release `shouldSatisfy` (not . T.isInfixOf "package.ps1")
            release `shouldSatisfy` T.isInfixOf "sha256"
            release `shouldSatisfy` T.isInfixOf "gh release upload"
        it "the storage binary name follows the platform (a stray .exe must not win on Linux)" $ do
            platformBinaryName "mingw32" "chusql-storage" `shouldBe` "chusql-storage.exe"
            platformBinaryName "linux" "chusql-storage" `shouldBe` "chusql-storage"
            -- 同一 target 目录里可能躺着一份别处交叉编译出来的 .exe：谁都不许再写
            -- 「.exe 优先、无后缀兜底」这种候选表
            engine <- readUtf8 (".." </> "chusql-engine" </> "test" </> "Spec.hs")
            bench <- readUtf8 (".." </> "benchmark" </> "src" </> "Main.hs")
            storage <- readUtf8 ("src" </> "ChuSQL" </> "Web" </> "StorageProcess.hs")
            mapM_
                (\src -> src `shouldSatisfy` (not . T.isInfixOf "\"chusql-storage.exe\", \"chusql-storage\""))
                [engine, bench, storage]

    -- 细节分到 doc/ 下四份，README 只留面向用户的关键内容；链接断了或者文档没了，等于没写。
    describe "Documentation (doc/)" $ do
        it "the README keeps only the essentials and links the split documents" $ do
            readme <- readUtf8 (".." </> "README.md")
            mapM_
                (\target -> readme `shouldSatisfy` T.isInfixOf ("doc/" <> target <> ".md"))
                ["install", "config", "commands", "architecture"]
            -- 安装选项与配置键的长表已经搬走，不该在 README 里再长回来
            readme `shouldSatisfy` (not . T.isInfixOf "--from-source")
            readme `shouldSatisfy` (not . T.isInfixOf "pool_size")
            -- 仓库布局这类开发者信息属于 doc/architecture.md，不属于 README
            readme `shouldSatisfy` (not . T.isInfixOf "chusql-engine/")
        it "each split document covers what its name promises" $ do
            let expectations =
                    [ ("install", ["install.sh", "install.ps1", "--component", "--install-dir", "--from-source", "rm -rf"])
                    , ("config", ["[web]", "[page]", "pipe_name", "data_dir", "host", "port"])
                    , ("commands", ["csql", "--format", "\\dt", "CREATE ROLE", "GRANT", "/api/roles"])
                    , ("architecture", ["chusql-storage", "chusql-web", "csql", "pipe_name", "data_dir", "CREATE DATABASE"])
                    ]
            mapM_
                ( \(name, needles) -> do
                    doc <- readUtf8 (".." </> "doc" </> name <> ".md")
                    mapM_ (\needle -> doc `shouldSatisfy` T.isInfixOf needle) needles
                )
                expectations
        -- doc/ 只写面向用户的内容：构建、测试、打包这些开发流程不进文档
        it "the split documents stay user-facing (no build, test or packaging recipes)" $ do
            let developerOnly =
                    [ "package.ps1"
                    , "stack build"
                    , "stack test"
                    , "cargo build"
                    , "cargo test"
                    , "cargo clippy"
                    , ".stack-work"
                    , ".github/workflows"
                    , "smoke-linux.sh"
                    ]
            mapM_
                ( \name -> do
                    doc <- readUtf8 (".." </> "doc" </> name <> ".md")
                    mapM_ (\needle -> doc `shouldSatisfy` (not . T.isInfixOf needle)) developerOnly
                )
                ["install", "config", "commands", "architecture"]

staticSpec :: FilePath -> Spec
staticSpec staticDir = describe "Static files" $ do
    it "an unreadable file gives Nothing" $
        readStatic staticDir "nope.js" `shouldReturn` Nothing

-- | 手写静态前端：源文件入库，没有打包器，也不该再有 Node 工具链
staticAssetSpec :: Spec
staticAssetSpec = describe "Static front end (hand-written, no bundler)" $ do
    let dir = "static"
        asset name = dir </> name
        readAsset name = readUtf8 (asset name)
    it "the app shell is present and loads both assets from /static/" $ do
        html <- readAsset "index.html"
        html `shouldSatisfy` T.isInfixOf "id=\"root\""
        html `shouldSatisfy` T.isInfixOf "src=\"/static/app.js\""
        html `shouldSatisfy` T.isInfixOf "href=\"/static/style.css\""
        -- 服务端把 nonce 元信息插在 </head> 前面，这个标记不能丢
        html `shouldSatisfy` T.isInfixOf "</head>"
    it "the shell has no inline script or style (the CSP allows neither)" $ do
        html <- readAsset "index.html"
        html `shouldSatisfy` (not . T.isInfixOf "<script>")
        html `shouldSatisfy` (not . T.isInfixOf "<style")
    it "no generated template keeps an inline style attribute (the CSP would block it)" $ do
        js <- readAsset "app.js"
        js `shouldSatisfy` (not . T.isInfixOf "style=\"")
    it "the hand-written modules and the stylesheet are non-empty" $
        mapM_ (\name -> BS.readFile (asset name) >>= (`shouldSatisfy` (not . BS.null)))
            ["app.js", "core.js", "api.js", "style.css"]
    it "the app carries the Chinese UI copy and the REST client keeps every endpoint" $ do
        js <- readAsset "app.js"
        js `shouldSatisfy` T.isInfixOf "待提交变更"
        js `shouldSatisfy` T.isInfixOf "新建查询"
        api <- readAsset "api.js"
        api `shouldSatisfy` T.isInfixOf "X-ChuSQL-Database"
        api `shouldSatisfy` T.isInfixOf "/api/tables/"
        api `shouldSatisfy` T.isInfixOf "/api/ui-settings"
        api `shouldSatisfy` T.isInfixOf "/api/query"
    it "every shipped asset passes the static file name whitelist" $ do
        names <- System.Directory.listDirectory dir
        mapM_ (\name -> safeRelative (T.pack name) `shouldBe` Just name) names
    it "content types are mapped for every shipped asset" $ do
        contentTypeOf "index.html" `shouldBe` "text/html; charset=utf-8"
        contentTypeOf "app.js" `shouldBe` "text/javascript; charset=utf-8"
        contentTypeOf "core.js" `shouldBe` "text/javascript; charset=utf-8"
        contentTypeOf "api.js" `shouldBe` "text/javascript; charset=utf-8"
        contentTypeOf "style.css" `shouldBe` "text/css; charset=utf-8"
    it "the Node toolchain is gone from the repository" $ do
        frontendDir <- doesDirectoryExist (".." </> "chusql-web" </> "frontend")
        nodeModules <- doesDirectoryExist (".." </> "chusql-web" </> "node_modules")
        viteConfig <- doesFileExist (".." </> "chusql-web" </> "frontend" </> "vite.config.ts")
        packageJson <- doesFileExist (".." </> "chusql-web" </> "frontend" </> "package.json")
        frontendDir `shouldBe` False
        nodeModules `shouldBe` False
        viteConfig `shouldBe` False
        packageJson `shouldBe` False

-- | 真静态目录里的资源由服务端交付（临时目录那套在 securitySpec 里另测）
staticServeSpec :: Spec
staticServeSpec = describe "Static front end served by the server" $
    withWorld "static" $ \_ -> do
        it "the home page carries the app root and the per-page nonce meta" $ do
            res <- get "/"
            check (hasStatus res 200)
            let body = BL.toStrict (WT.simpleBody res)
            check (body `shouldSatisfy` BS.isInfixOf "id=\"root\"")
            check (body `shouldSatisfy` BS.isInfixOf "chusql-style-nonce")
        it "the app script and stylesheet are served with their content types" $ do
            js <- get "/static/app.js"
            check (hasStatus js 200)
            check (lookup "Content-Type" (WT.simpleHeaders js) `shouldBe` Just "text/javascript; charset=utf-8")
            css <- get "/static/style.css"
            check (hasStatus css 200)
            check (lookup "Content-Type" (WT.simpleHeaders css) `shouldBe` Just "text/css; charset=utf-8")
        it "traversal and unknown files are still rejected" $ do
            get "/static/..%2fchusql.toml" `shouldRespondWith` 404
            get "/static/nope.js" `shouldRespondWith` 404

actionsSpec :: Spec
actionsSpec = describe "One-click actions -> one SQL statement (ChuSQL.Web.Actions)" $ do
    it "create table: column types and identifiers are validated" $ do
        createTableSql (CreateTableSpec "customers" [ColumnSpec "id" "int", ColumnSpec "name" "str"])
            `shouldBe` Right "CREATE TABLE customers (id int, name str)"
        createTableSql (CreateTableSpec "bad name" [ColumnSpec "id" "int"]) `shouldSatisfy` isLeftE
        createTableSql (CreateTableSpec "t" []) `shouldSatisfy` isLeftE
        createTableSql (CreateTableSpec "t" [ColumnSpec "id" "blob"])
            `shouldBe` Right "CREATE TABLE t (id blob)"
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
    it "column type parsing: int/bigint/smallint/str/text/varchar/char/float/double/decimal/date/timestamp/bool/blob are recognised" $ do
        map columnTypeOf ["int", "INTEGER", "str", "text", "varchar", "bool", "boolean"]
            `shouldBe` map Just [TInt, TInt, TStr, TStr, TStr, TBool, TBool]
        columnTypeOf "blob" `shouldBe` Just (plainColumn CBlob)
        -- 类型带参数时才认得出来，未知类型与空串都给 Nothing
        map columnTypeOf ["bigint", "smallint", "float", "double", "date", "timestamp"]
            `shouldBe` map Just [plainColumn CBigInt, plainColumn CSmallInt, plainColumn CFloat, plainColumn CDouble, plainColumn CDate, plainColumn CTimestamp]
        map columnTypeOf ["varchar(20)", "char(3)", "decimal(8,2)"]
            `shouldBe` map Just [plainColumn (CVarchar 20), plainColumn (CChar 3), plainColumn (CDecimal 8 2)]
        map columnTypeOf ["nope", ""] `shouldBe` [Nothing, Nothing]

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
            nullableTable <- postWith hs "/api/tables" "{\"name\":\"nullable_t\",\"columns\":[{\"name\":\"id\",\"type\":\"int\"},{\"name\":\"name\",\"type\":\"str\"},{\"name\":\"age\",\"type\":\"int\"}]}"
            check (hasStatus nullableTable 200)
            partial <- postWith hs "/api/tables/nullable_t/rows" "{\"values\":{\"id\":99}}"
            check (hasStatus partial 200)
            filled <- getWith hs "/api/tables/nullable_t/rows"
            check $ do
                asInt (at "total" (jsonBody filled)) `shouldBe` 1
                firstRow filled `shouldBe` [A.Number 99, A.Null, A.Null]
            badBody <- postWith hs "/api/tables/users/rows" "not json"
            check (hasStatus badBody 400)
            badId <- request "PATCH" "/api/tables/users/rows/abc" (("Content-Type", "application/json") : hs) "{\"values\":{\"age\":1}}"
            check (hasStatus badId 400)
            noTable <- postWith hs "/api/tables/nope/rows" "{\"values\":{\"id\":1}}"
            check (hasStatus noTable 404)
            badType <- postWith hs "/api/tables/users/rows" "{\"values\":{\"id\":1,\"name\":\"x\",\"age\":\"old\"}}"
            check (hasStatus badType 400)
        it "index: any column type can be indexed, the built-in id index cannot be dropped" $ do
            res <- loginAs "admin" "s3cret"
            let hs = cookieHeaders res
            okIndex <- postWith hs "/api/tables/users/indexes" "{\"column\":\"age\"}"
            check (hasStatus okIndex 200)
            stringIndex <- postWith hs "/api/tables/users/indexes" "{\"column\":\"name\"}"
            check (hasStatus stringIndex 200)
            droppedString <- request "DELETE" "/api/tables/users/indexes/name" hs ""
            check (hasStatus droppedString 200)
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

-- | 免密管理员：只有管理员能用空口令进来，普通账号一律 403
passwordlessSpec :: FilePath -> Spec
passwordlessSpec staticDir = describe "Auth: passwordless administrator" $
    withPasswordlessWorld staticDir $ \_ -> do
        it "the administrator signs in with an empty password" $ do
            res <- loginAs "admin" ""
            check (hasStatus res 204)
            check (sessionCookieOf res `shouldSatisfy` maybe False (not . T.null))
        it "a non-empty password is refused with 401" $
            loginAs "admin" "s3cret" `shouldRespondWith` 401
        it "an ordinary account is refused with 403 while the server is passwordless" $ do
            res <- loginAs "someone" "whatever"
            check (hasStatus res 403)
            check (asText (at "error" (jsonBody res)) `shouldBe` "forbidden")
        it "an unknown account with an empty password is refused too" $ do
            res <- loginAs "ghost" ""
            check (hasStatus res 403)

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
        it "multi-row INSERT is supported" $ do
            res <- loginAs "admin" "s3cret"
            _ <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"INSERT INTO users (id, name, age) VALUES (11, 'a', 1), (12, 'b', 2)\"}"
            out <- postWith (cookieHeaders res) "/api/query" "{\"sql\":\"SELECT * FROM users WHERE id > 10\"}"
            check (length (items (at "rows" (jsonBody out))) `shouldBe` 2)
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
                admin <- loginAs "admin" "s3cret"
                made <- postWith (systemHeaders (cookieHeaders admin)) "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"guest\",\"password\":\"guest-password\"}}"
                check (hasStatus made 200)
                guest <- loginAs "guest" "guest-password"
                check (hasStatus guest 204)
                let guestHeaders = cookieHeaders guest
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

    describe "Account table (system table, administrator only)" $
        withWorldAt staticDir "accounts" $ \appEnv -> do
            it "not logged in gives 401 on every account-table route" $ do
                getWith [] "/api/tables" `shouldRespondWith` 401
                getWith (systemHeaders []) "/api/tables/__chusql_users" `shouldRespondWith` 401
                getWith (systemHeaders []) "/api/tables/__chusql_users/rows" `shouldRespondWith` 401
                postWith (systemHeaders []) "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"guest\",\"password\":\"guest-password\"}}" `shouldRespondWith` 401
                patchWith (systemHeaders []) "/api/tables/__chusql_users/rows/guest" "{\"values\":{\"password\":\"guest-password\"}}" `shouldRespondWith` 401
                deleteWith (systemHeaders []) "/api/tables/__chusql_users/rows/guest" `shouldRespondWith` 401
            it "the retired account routes are gone" $ do
                res <- loginAs "admin" "s3cret"
                let hs = cookieHeaders res
                switched <- postWith hs "/api/account/switch" "{\"user\":\"someone\",\"password\":\"someone-password\"}"
                check (hasStatus switched 404)
                listed <- getWith hs "/api/accounts"
                check (hasStatus listed 404)
                selfService <- postWith hs "/api/account/password" "{\"current\":\"s3cret\",\"next\":\"longenough\"}"
                check (hasStatus selfService 404)
            it "the administrator sees the account table with a secret password column" $ do
                admin <- loginAs "admin" "s3cret"
                body <- getWith (systemHeaders (cookieHeaders admin)) "/api/tables"
                check (hasStatus body 200)
                check $ do
                    let entries = items (jsonBody body)
                        account = tableEntry "__chusql_users" entries
                        users = tableEntry "users" entries
                    at "table" account `shouldBe` A.String "__chusql_users"
                    at "kind" account `shouldBe` A.String "system"
                    map (asText . at "name") (items (at "columns" account)) `shouldBe` ["id", "user", "password", "registered_at", "last_login_at"]
                    map (asBool . at "secret") (items (at "columns" account)) `shouldBe` [False, False, True, False, False]
                    map (asBool . at "primaryKey") (items (at "columns" account)) `shouldBe` [True, False, False, False, False]
                    map (asText . at "type") (items (at "columns" account)) `shouldBe` ["int", "varchar(64)", "varchar(256)", "timestamp", "timestamp"]
                    map (asBool . at "indexed") (items (at "columns" account)) `shouldBe` [True, True, False, False, False]
                    at "kind" users `shouldBe` A.String "table"
                detail <- getWith (systemHeaders (cookieHeaders admin)) "/api/tables/__chusql_users"
                check (hasStatus detail 200)
                check (at "kind" (jsonBody detail) `shouldBe` A.String "system")
            it "insert, update and delete on the account table are CREATE, ALTER and DROP USER" $ do
                admin <- loginAs "admin" "s3cret"
                let hs = cookieHeaders admin
                made <- postWith (systemHeaders hs) "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"Alice\",\"password\":\"alice-password\"}}"
                check (hasStatus made 200)
                page <- getWith (systemHeaders hs) "/api/tables/__chusql_users/rows"
                check (hasStatus page 200)
                check $ do
                    let rows = items (at "rows" (jsonBody page))
                    map asText (items (at "columns" (jsonBody page))) `shouldBe` ["id", "user", "password", "registered_at", "last_login_at"]
                    map (asText . (V.! 1) . asArray) rows `shouldBe` ["alice"]
                    length rows `shouldBe` 1
                    case firstRow page of
                        [_, _, _, A.String registered, lastLogin] -> do
                            registered `shouldSatisfy` (/= "")
                            lastLogin `shouldBe` A.Null
                        other -> expectationFailure ("unexpected account row: " ++ show other)
                repeated <- postWith (systemHeaders hs) "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"alice\",\"password\":\"alice-password\"}}"
                check (hasStatus repeated 409)
                alice <- loginAs "alice" "alice-password"
                check (hasStatus alice 204)
                adminAgain <- loginAs "admin" "s3cret"
                let hsAgain = cookieHeaders adminAgain
                stamped <- getWith (systemHeaders hsAgain) "/api/tables/__chusql_users/rows"
                check $ do
                    hasStatus stamped 200
                    case firstRow stamped of
                        [_, _, _, _, lastLogin] -> lastLogin `shouldSatisfy` (/= A.Null)
                        other -> expectationFailure ("unexpected account row: " ++ show other)
                changed <- patchWith (systemHeaders hsAgain) "/api/tables/__chusql_users/rows/alice" "{\"values\":{\"password\":\"alice-other-pass\"}}"
                check (hasStatus changed 200)
                stalePassword <- loginAs "alice" "alice-password"
                check (hasStatus stalePassword 401)
                freshPassword <- loginAs "alice" "alice-other-pass"
                check (hasStatus freshPassword 204)
                adminLast <- loginAs "admin" "s3cret"
                let hsLast = cookieHeaders adminLast
                renamed <- patchWith (systemHeaders hsLast) "/api/tables/__chusql_users/rows/alice" "{\"values\":{\"user\":\"bob\"}}"
                check (hasStatus renamed 400)
                unknown <- deleteWith (systemHeaders hsLast) "/api/tables/__chusql_users/rows/nobody"
                check (hasStatus unknown 404)
                dropped <- deleteWith (systemHeaders hsLast) "/api/tables/__chusql_users/rows/alice"
                check (hasStatus dropped 200)
                gone <- loginAs "alice" "alice-other-pass"
                check (hasStatus gone 401)
            it "the account table can be sorted and filtered by user but not by password" $ do
                admin <- loginAs "admin" "s3cret"
                let hs = cookieHeaders admin
                _ <- postWith (systemHeaders hs) "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"bob\",\"password\":\"bob-password\"}}"
                _ <- postWith (systemHeaders hs) "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"amy\",\"password\":\"amy-password\"}}"
                sorted <- getWith (systemHeaders hs) "/api/tables/__chusql_users/rows?sort=user&dir=asc"
                check (hasStatus sorted 200)
                check $ do
                    let names = map (asText . (V.! 1) . asArray) (items (at "rows" (jsonBody sorted)))
                    names `shouldSatisfy` \ns -> ns == sort ns && all (`elem` ns) ["amy", "bob"]
                filtered <- getWith (systemHeaders hs) "/api/tables/__chusql_users/rows?filter=%5B%7B%22column%22%3A%22user%22%2C%22value%22%3A%22bob%22%7D%5D"
                check (hasStatus filtered 200)
                check (map (asText . (V.! 1) . asArray) (items (at "rows" (jsonBody filtered))) `shouldBe` ["bob"])
                byPassword <- getWith (systemHeaders hs) "/api/tables/__chusql_users/rows?sort=password"
                check (hasStatus byPassword 400)
                filterPassword <- getWith (systemHeaders hs) "/api/tables/__chusql_users/rows?filter=%5B%7B%22column%22%3A%22password%22%2C%22value%22%3A%22x%22%7D%5D"
                check (hasStatus filterPassword 400)
            it "the password column is stored as a hash, never in the settings file" $ do
                admin <- loginAs "admin" "s3cret"
                _ <- postWith (systemHeaders (cookieHeaders admin)) "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"dave\",\"password\":\"dave-password\"}}"
                stored <- liftIO (beAccounts (aeBackend appEnv) ReqAccountsList)
                check $ case stored of
                    Left err -> expectationFailure err
                    Right accounts -> case filter ((== "dave") . accountUser) accounts of
                        [account] -> do
                            accountRevision account `shouldBe` 1
                            hashLooksValid (accountHash account) `shouldBe` True
                            verifyPassword (accountHash account) "dave-password" `shouldBe` True
                        _ -> expectationFailure "dave was not stored in the system table"
                settings <- liftIO (readSettingsFile (aeSettingsFile appEnv))
                check (Map.member "password" settings `shouldBe` False)
                check (Map.member "password-hash" settings `shouldBe` False)
            it "the account table has no indexes, cannot be dropped and cannot be renamed" $ do
                admin <- loginAs "admin" "s3cret"
                let hs = cookieHeaders admin
                index <- postWith (systemHeaders hs) "/api/tables/__chusql_users/indexes" "{\"column\":\"user\"}"
                check (hasStatus index 400)
                dropIndex <- deleteWith (systemHeaders hs) "/api/tables/__chusql_users/indexes/user"
                check (hasStatus dropIndex 400)
                dropColumn <- deleteWith (systemHeaders hs) "/api/tables/__chusql_users/columns/password"
                check (hasStatus dropColumn 400)
                dropped <- deleteWith (systemHeaders hs) "/api/tables/__chusql_users"
                check (hasStatus dropped 400)
            it "an ordinary account never sees or touches the account table" $ do
                admin <- loginAs "admin" "s3cret"
                made <- postWith (systemHeaders (cookieHeaders admin)) "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"guest\",\"password\":\"guest-password\"}}"
                check (hasStatus made 200)
                guest <- loginAs "guest" "guest-password"
                check (hasStatus guest 204)
                let guestHeaders = cookieHeaders guest
                listed <- getWith guestHeaders "/api/tables"
                check (hasStatus listed 200)
                check (map (asText . at "table") (items (jsonBody listed)) `shouldNotContain` ["__chusql_users"])
                detail <- getWith (systemHeaders guestHeaders) "/api/tables/__chusql_users"
                check (hasStatus detail 403)
                rows <- getWith (systemHeaders guestHeaders) "/api/tables/__chusql_users/rows"
                check (hasStatus rows 403)
                created <- postWith (systemHeaders guestHeaders) "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"bob\",\"password\":\"bob-password\"}}"
                check (hasStatus created 403)
                reset <- patchWith (systemHeaders guestHeaders) "/api/tables/__chusql_users/rows/guest" "{\"values\":{\"password\":\"guest-other-pass\"}}"
                check (hasStatus reset 403)
                dropped <- deleteWith (systemHeaders guestHeaders) "/api/tables/__chusql_users/rows/guest"
                check (hasStatus dropped 403)
                stillThere <- loginAs "guest" "guest-password"
                check (hasStatus stillThere 204)
            it "CREATE USER / ALTER USER / DROP USER go through the SQL console" $ do
                admin <- loginAs "admin" "s3cret"
                created <- postWith (cookieHeaders admin) "/api/query" "{\"sql\":\"CREATE USER carol IDENTIFIED BY 'carol-password'\"}"
                check (hasStatus created 200)
                check (asInt (at "rowCount" (jsonBody created)) `shouldBe` 0)
                changed <- postWith (cookieHeaders admin) "/api/query" "{\"sql\":\"ALTER USER carol IDENTIFIED BY 'carol-other-pass'\"}"
                check (hasStatus changed 200)
                dropped <- postWith (cookieHeaders admin) "/api/query" "{\"sql\":\"DROP USER carol\"}"
                check (hasStatus dropped 200)
                gone <- loginAs "carol" "carol-other-pass"
                check (hasStatus gone 401)
            it "an ordinary account cannot run account statements in the SQL console" $ do
                admin <- loginAs "admin" "s3cret"
                _ <- postWith (cookieHeaders admin) "/api/query" "{\"sql\":\"CREATE USER erin IDENTIFIED BY 'erin-password'\"}"
                erin <- loginAs "erin" "erin-password"
                check (hasStatus erin 204)
                denials <- mapM (\sql -> postWith (cookieHeaders erin) "/api/query" (sqlBody sql))
                    [ "DROP USER erin"
                    , "ALTER USER erin IDENTIFIED BY 'erin-other-pass'"
                    , "CREATE USER frank IDENTIFIED BY 'frank-password'"
                    ]
                check (map (statusCode . WT.simpleStatus) denials `shouldBe` [403, 403, 403])
                stillThere <- loginAs "erin" "erin-password"
                check (hasStatus stillThere 204)
            it "the administrator credentials are locked in the settings API" $ do
                admin <- loginAs "admin" "s3cret"
                locked <- putWith (cookieHeaders admin) "/api/settings" "{\"values\":{\"password\":\"whatever\"}}"
                check (hasStatus locked 403)
                alsoLocked <- putWith (cookieHeaders admin) "/api/settings" "{\"values\":{\"user\":\"someone\"}}"
                check (hasStatus alsoLocked 403)
                body <- getWith (cookieHeaders admin) "/api/settings"
                check $ do
                    let entries = items (at "items" (jsonBody body))
                    at "editable" (headEntry "user" entries) `shouldBe` A.Bool False
                    at "locked" (headEntry "user" entries) `shouldBe` A.Bool True
                    at "locked" (headEntry "rows-per-page" entries) `shouldBe` A.Bool False

    describe "Databases (nothing is selected by default, system is administrator-only)" $
        withWorldAt staticDir "databases" $ \_ -> do
            it "lists exactly the databases that exist; the fixture database comes first" $ do
                admin <- loginAs "admin" "s3cret"
                body <- getWith (cookieHeaders admin) "/api/databases"
                check (hasStatus body 200)
                check (map asText (items (jsonBody body)) `shouldBe` ["test", "system"])
            it "the account table is listed in system, never in test" $ do
                admin <- loginAs "admin" "s3cret"
                let hs = cookieHeaders admin
                testList <- getWith hs "/api/tables"
                check (map (asText . at "table") (items (jsonBody testList)) `shouldNotContain` ["__chusql_users"])
                systemList <- getWith (systemHeaders hs) "/api/tables"
                check (hasStatus systemList 200)
                check (map (asText . at "table") (items (jsonBody systemList)) `shouldContain` ["__chusql_users"])
            it "naming the account table from another database is a 400, not a silent lookup" $ do
                admin <- loginAs "admin" "s3cret"
                outside <- getWith (cookieHeaders admin) "/api/tables/__chusql_users/rows"
                check (hasStatus outside 400)
                check (asText (at "message" (jsonBody outside)) `shouldBe` "the account table lives in the system database")
            it "an ordinary account gets 403 on system and never sees it in the list" $ do
                admin <- loginAs "admin" "s3cret"
                made <- postWith (systemHeaders (cookieHeaders admin)) "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"guest\",\"password\":\"guest-password\"}}"
                check (hasStatus made 200)
                guest <- loginAs "guest" "guest-password"
                let guestHeaders = cookieHeaders guest
                blocked <- getWith (systemHeaders guestHeaders) "/api/tables"
                check (hasStatus blocked 403)
                listed <- getWith guestHeaders "/api/databases"
                check (hasStatus listed 200)
                check (map asText (items (jsonBody listed)) `shouldBe` ["test"])
            it "only the system database is reserved: test is an ordinary name" $ do
                admin <- loginAs "admin" "s3cret"
                let hs = cookieHeaders admin
                reservedDrop <- deleteWith hs "/api/databases/system"
                check (hasStatus reservedDrop 400)
                check (asText (at "message" (jsonBody reservedDrop)) `shouldBe` "the system database cannot be dropped")
                ordinaryDrop <- deleteWith hs "/api/databases/test"
                check (hasStatus ordinaryDrop 400)
                check (asText (at "message" (jsonBody ordinaryDrop)) `shouldSatisfy` T.isInfixOf "unavailable")
            it "a request without a database header is a 400, not a silent default" $ do
                admin <- loginAs "admin" "s3cret"
                let _hs = cookieHeaders admin
                bareTables <- request methodGet "/api/tables" [] ""
                check (hasStatus bareTables 400)
                check (asText (at "message" (jsonBody bareTables)) `shouldBe` "no database selected")
                bareQuery <- request methodPost "/api/query" [("Content-Type", "application/json")] "{\"sql\":\"SELECT * FROM users\"}"
                check (hasStatus bareQuery 400)
                check (asText (at "error" (jsonBody bareQuery)) `shouldBe` "no_database")
                stillListed <- request methodGet "/api/databases" [] ""
                check (hasStatus stillListed 200)
            it "CREATE DATABASE in the SQL console still goes to the storage layer" $ do
                admin <- loginAs "admin" "s3cret"
                created <- postWith (cookieHeaders admin) "/api/query" "{\"sql\":\"CREATE DATABASE sales\"}"
                check (hasStatus created 400)
                check (asText (at "message" (jsonBody created)) `shouldSatisfy` T.isInfixOf "unavailable")

-- | 角色与权限：REST 端点、SQL 语句、越权口径、内部表不可见
privilegesSpec :: FilePath -> Spec
privilegesSpec staticDir = withWorldAt staticDir "privileges" $ \_ -> do
    -- 注意：wai-extra 的 Session 自带 Cookie 罐，罐里的令牌会盖过显式 Cookie 头，
    -- 所以每个身份都得在动手前重新登录一次（和既有用例组同一套写法）。
    let signIn user password = do
            res <- loginAs user password
            expectStatusWith ("sign in " <> T.unpack user) res 204
    it "the administrator creates a role, grants SELECT and adds a member" $ do
        signIn "admin" "s3cret"
        made <- postWith [] "/api/roles" "{\"name\":\"analyst\"}"
        check (hasStatus made 200)
        granted <- postWith [] "/api/roles/analyst/grants" "{\"object\":\"users\",\"privileges\":[\"select\"]}"
        check (hasStatus granted 200)
        _ <- postWith [] "/api/query" (sqlBody "CREATE USER dana IDENTIFIED BY 'dana-password'")
        joined <- postWith [] "/api/roles/analyst/members" "{\"user\":\"dana\"}"
        check (hasStatus joined 200)
        listed <- getWith [] "/api/roles"
        check (hasStatus listed 200)
        check $ do
            let analyst = roleEntry "analyst" (items (jsonBody listed))
            at "name" analyst `shouldBe` A.String "analyst"
            map (asText . at "privilege") (items (at "grants" analyst)) `shouldBe` ["select"]
            map (asText . at "object") (items (at "grants" analyst)) `shouldBe` ["test.users"]
            map asText (items (at "members" analyst)) `shouldBe` ["dana"]
        signIn "dana" "dana-password"
        visible <- getWith [] "/api/tables"
        check (hasStatus visible 200)
        check (map (asText . at "table") (items (jsonBody visible)) `shouldBe` ["users"])
        allowed <- postWith [] "/api/query" (sqlBody "SELECT * FROM users")
        check (hasStatus allowed 200)
        check (asInt (at "rowCount" (jsonBody allowed)) `shouldBe` 5)
        denied <- postWith [] "/api/query" (sqlBody "SELECT * FROM orders")
        check (hasStatus denied 403)
        check (asText (at "error" (jsonBody denied)) `shouldBe` "forbidden")
        joins <- postWith [] "/api/query" (sqlBody "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id")
        check (hasStatus joins 403)
        nested <- postWith [] "/api/query" (sqlBody "SELECT name FROM users WHERE id IN (SELECT user_id FROM orders)")
        check (hasStatus nested 403)
        detail <- getWith [] "/api/tables/orders/rows"
        check (hasStatus detail 403)
        rows <- getWith [] "/api/tables/users/rows"
        check (hasStatus rows 200)
        inserted <- postWith [] "/api/tables/users/rows" "{\"values\":{\"id\":9,\"name\":\"nine\",\"age\":30}}"
        check (hasStatus inserted 403)
    it "grants of insert/update/delete unlock the one-click writes, DDL stays administrator-only" $ do
        signIn "admin" "s3cret"
        _ <- postWith [] "/api/roles" "{\"name\":\"editor\"}"
        _ <- postWith [] "/api/roles/editor/grants" "{\"object\":\"users\",\"privileges\":[\"insert\",\"update\",\"delete\"]}"
        _ <- postWith [] "/api/query" (sqlBody "CREATE USER evan IDENTIFIED BY 'evan-password'")
        _ <- postWith [] "/api/roles/editor/members" "{\"users\":[\"evan\"]}"
        signIn "evan" "evan-password"
        inserted <- postWith [] "/api/tables/users/rows" "{\"values\":{\"id\":99,\"name\":\"nine\",\"age\":30}}"
        check (hasStatus inserted 200)
        changed <- patchWith [] "/api/tables/users/rows/99" "{\"values\":{\"age\":31}}"
        check (hasStatus changed 200)
        removed <- deleteWith [] "/api/tables/users/rows/99"
        check (hasStatus removed 200)
        visible <- getWith [] "/api/tables"
        check (hasStatus visible 200)
        check (length (items (jsonBody visible)) `shouldBe` 0)
        readDenied <- postWith [] "/api/query" (sqlBody "SELECT * FROM users")
        check (hasStatus readDenied 403)
        dropped <- deleteWith [] "/api/tables/users"
        check (hasStatus dropped 403)
        ddl <- postWith [] "/api/query" (sqlBody "CREATE TABLE t9 (id INT)")
        check (hasStatus ddl 403)
        roles <- getWith [] "/api/roles"
        check (hasStatus roles 403)
        roleSql <- postWith [] "/api/query" (sqlBody "CREATE ROLE sneak")
        check (hasStatus roleSql 403)
    it "a wildcard grant covers every table" $ do
        signIn "admin" "s3cret"
        _ <- postWith [] "/api/roles" "{\"name\":\"supervisor\"}"
        _ <- postWith [] "/api/roles/supervisor/grants" "{\"object\":\"*\",\"privileges\":[\"select\"]}"
        _ <- postWith [] "/api/query" (sqlBody "CREATE USER ivy IDENTIFIED BY 'ivy-password'")
        _ <- postWith [] "/api/roles/supervisor/members" "{\"user\":\"ivy\"}"
        signIn "ivy" "ivy-password"
        visible <- getWith [] "/api/tables"
        check (hasStatus visible 200)
        check (sort (map (asText . at "table") (items (jsonBody visible))) `shouldBe` ["orders", "users"])
        both <- postWith [] "/api/query" (sqlBody "SELECT * FROM orders")
        check (hasStatus both 200)
        innerJoin <- postWith [] "/api/query" (sqlBody "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id")
        check (hasStatus innerJoin 200)
    it "revoking membership and privileges closes the door again" $ do
        signIn "admin" "s3cret"
        _ <- postWith [] "/api/roles" "{\"name\":\"temp\"}"
        _ <- postWith [] "/api/roles/temp/grants" "{\"object\":\"users\",\"privileges\":[\"select\"]}"
        _ <- postWith [] "/api/query" (sqlBody "CREATE USER gina IDENTIFIED BY 'gina-password'")
        _ <- postWith [] "/api/roles/temp/members" "{\"user\":\"gina\"}"
        signIn "gina" "gina-password"
        grantedSelect <- postWith [] "/api/query" (sqlBody "SELECT * FROM users")
        expectStatusWith "gina selects after the grant" grantedSelect 200
        signIn "admin" "s3cret"
        evicted <- deleteWith [] "/api/roles/temp/members/gina"
        expectStatusWith "evict gina" evicted 200
        signIn "gina" "gina-password"
        afterEviction <- postWith [] "/api/query" (sqlBody "SELECT * FROM users")
        expectStatusWith "gina select after eviction" afterEviction 403
        signIn "admin" "s3cret"
        readded <- postWith [] "/api/roles/temp/members" "{\"user\":\"gina\"}"
        expectStatusWith "re-add gina" readded 200
        signIn "gina" "gina-password"
        back <- postWith [] "/api/query" (sqlBody "SELECT * FROM users")
        expectStatusWith "gina selects again" back 200
        signIn "admin" "s3cret"
        revoked <- deleteJsonWith [] "/api/roles/temp/grants" "{\"object\":\"users\",\"privileges\":[\"select\"]}"
        expectStatusWith "revoke the grant" revoked 200
        signIn "gina" "gina-password"
        empty <- getWith [] "/api/tables"
        expectStatusWith "gina table list after revoke" empty 200
        check (items (jsonBody empty) `shouldBe` [])
        afterRevoke <- postWith [] "/api/query" (sqlBody "SELECT * FROM users")
        check (hasStatus afterRevoke 403)
    it "role statements run in the SQL console and conflicts and unknown roles are reported" $ do
        signIn "admin" "s3cret"
        created <- postWith [] "/api/query" (sqlBody "CREATE ROLE auditor")
        check (hasStatus created 200)
        granted <- postWith [] "/api/query" (sqlBody "GRANT SELECT ON * TO auditor")
        check (hasStatus granted 200)
        listed <- getWith [] "/api/roles"
        check $ do
            let auditor = roleEntry "auditor" (items (jsonBody listed))
            map (asText . at "privilege") (items (at "grants" auditor)) `shouldBe` ["select"]
            map (asText . at "object") (items (at "grants" auditor)) `shouldBe` ["*"]
        duplicate <- postWith [] "/api/roles" "{\"name\":\"auditor\"}"
        check (hasStatus duplicate 409)
        unknownGrant <- postWith [] "/api/roles/nobody/grants" "{\"object\":\"users\",\"privileges\":[\"select\"]}"
        check (hasStatus unknownGrant 404)
        unknownMember <- postWith [] "/api/roles/nobody/members" "{\"user\":\"dana\"}"
        check (hasStatus unknownMember 404)
        badPrivilege <- postWith [] "/api/query" (sqlBody "GRANT EXECUTE ON users TO auditor")
        check (hasStatus badPrivilege 400)
        revoked <- postWith [] "/api/query" (sqlBody "REVOKE SELECT ON * FROM auditor")
        check (hasStatus revoked 200)
        removed <- postWith [] "/api/query" (sqlBody "DROP ROLE auditor")
        check (hasStatus removed 200)
        gone <- getWith [] "/api/roles"
        check (map (asText . at "name") (items (jsonBody gone)) `shouldNotContain` ["auditor"])
    it "ALL expands into the four concrete privileges" $ do
        signIn "admin" "s3cret"
        _ <- postWith [] "/api/roles" "{\"name\":\"owner\"}"
        _ <- postWith [] "/api/roles/owner/grants" "{\"object\":\"orders\",\"privileges\":[\"all\"]}"
        listed <- getWith [] "/api/roles"
        check $ do
            let owner = roleEntry "owner" (items (jsonBody listed))
            sort (map (asText . at "privilege") (items (at "grants" owner))) `shouldBe` ["delete", "insert", "select", "update"]
            map (asText . at "object") (items (at "grants" owner)) `shouldBe` replicate 4 "test.orders"
    it "the tables that hold roles and grants are invisible" $ do
        signIn "admin" "s3cret"
        testList <- getWith [] "/api/tables"
        check (hasStatus testList 200)
        check (map (asText . at "table") (items (jsonBody testList)) `shouldNotContain` ["sys_roles", "sys_grants", "sys_members"])
        systemList <- getWith (systemHeaders []) "/api/tables"
        check (hasStatus systemList 200)
        check (map (asText . at "table") (items (jsonBody systemList)) `shouldNotContain` ["sys_roles", "sys_grants", "sys_members"])
        hidden <- getWith [] "/api/tables/sys_roles"
        check (hasStatus hidden 404)
        peek <- postWith [] "/api/query" (sqlBody "SELECT * FROM sys_roles")
        check (statusCode (WT.simpleStatus peek) `shouldSatisfy` (`elem` [400, 403, 404]))
    it "an account with no role has no access at all" $ do
        signIn "admin" "s3cret"
        _ <- postWith [] "/api/query" (sqlBody "CREATE USER frank IDENTIFIED BY 'frank-password'")
        signIn "frank" "frank-password"
        visible <- getWith [] "/api/tables"
        check (hasStatus visible 200)
        check (items (jsonBody visible) `shouldBe` [])
        forM_
            [ "SELECT * FROM users"
            , "INSERT INTO users (id, name, age) VALUES (7, 'seven', 7)"
            , "UPDATE users SET age = 8 WHERE id = 7"
            , "DELETE FROM users WHERE id = 7"
            ]
            $ \sql -> do
                res <- postWith [] "/api/query" (sqlBody sql)
                check (hasStatus res 403)
        detail <- getWith [] "/api/tables/users/rows"
        check (hasStatus detail 403)
        databases <- getWith [] "/api/databases"
        check (map asText (items (jsonBody databases)) `shouldBe` ["test"])

-- | 在角色清单里按名字找一项
roleEntry :: Text -> [A.Value] -> A.Value
roleEntry name entries = case [e | e <- entries, asText (at "name" e) == name] of
    (e : _) -> e
    [] -> A.Null

-- | 带 JSON 体的 DELETE（撤销授权要用）
deleteJsonWith :: [Header] -> BS.ByteString -> BL.ByteString -> TestSession WT.SResponse
deleteJsonWith headers path payload =
    request "DELETE" path (fixtureHeaders (("Content-Type", "application/json") : headers)) payload

-- | 一条 SQL 请求体
sqlBody :: Text -> BL.ByteString
sqlBody sql = BL.fromStrict (TE.encodeUtf8 (T.concat ["{\"sql\":\"", sql, "\"}"]))

-- | 在表清单里按表名找一项（找不到给 Null）
tableEntry :: Text -> [A.Value] -> A.Value
tableEntry name entries = case [e | e <- entries, asText (at "table" e) == name] of
    (e : _) -> e
    [] -> A.Null

-- | JSON 当布尔看
asBool :: A.Value -> Bool
asBool (A.Bool b) = b
asBool _ = False

-- | JSON 当数组看（不是数组给空）
asArray :: A.Value -> V.Vector A.Value
asArray (A.Array xs) = xs
asArray _ = V.empty

-- | 一份完整的合法 IDE 设置请求体
uiSettingsBody :: BL.ByteString
uiSettingsBody = "{\"uiFonts\":[\"JetBrains Mono\",\"Consolas\"],\"gridFonts\":[\"Consolas\"],\"sqlFonts\":[\"Mono\"],\"uiFontSize\":13,\"gridFontSize\":12,\"gridRowHeight\":24,\"sqlFontSize\":13,\"sqlLineHeight\":20,\"sqlTabSize\":2,\"pageSize\":200,\"nullText\":\"NULL\",\"sqlLineNumbers\":true,\"autocomplete\":true,\"minimap\":false}"

-- | 上面那份设置解出来的值
fullUISettings :: A.Value
fullUISettings = fromMaybe A.Null (A.decode uiSettingsBody)

-- | 在合法设置上换掉一个字段（试坏值用）
withUiField :: Text -> A.Value -> A.Value
withUiField key value = case fullUISettings of
    A.Object o -> A.Object (KM.insert (K.fromText key) value o)
    other -> other

uiSettingsSpec :: FilePath -> Spec
uiSettingsSpec staticDir = do
    describe "IDE settings validation" $ do
        it "accepts a complete valid object" $
            validateUISettings fullUISettings `shouldSatisfy` isRightE
        it "rejects a string where an integer is expected" $
            validateUISettings (withUiField "uiFontSize" (A.String "13")) `shouldSatisfy` isLeftE
        it "rejects an integer outside its range" $ do
            validateUISettings (withUiField "uiFontSize" (A.Number 10)) `shouldSatisfy` isLeftE
            validateUISettings (withUiField "uiFontSize" (A.Number 17)) `shouldSatisfy` isLeftE
        it "rejects a blank font name" $
            validateUISettings (withUiField "gridFonts" (A.Array (V.fromList [A.String "  "]))) `shouldSatisfy` isLeftE
        it "rejects a number where a boolean is expected" $
            validateUISettings (withUiField "minimap" (A.Number 1)) `shouldSatisfy` isLeftE
        it "names the offending key in the error" $
            case validateUISettings (withUiField "pageSize" (A.Number 5)) of
                Left err -> err `shouldSatisfy` isInfixOf "pageSize"
                Right _ -> expectationFailure "expected a range error"

    describe "IDE settings file" $ do
        it "candidates look for the file in scripts/ first" $
            uiSettingsFileCandidates
                `shouldBe` [ "scripts" </> "chusql.ui.settings.json"
                           , ".." </> "scripts" </> "chusql.ui.settings.json"
                           , "chusql.ui.settings.json"
                           ]
        it "writing then reading round-trips the map and creates the directory" $ do
            tmp <- getTemporaryDirectory
            let dir = tmp </> "chusql-web-test-ui-file" </> "nested"
                path = dir </> "chusql.ui.settings.json"
                stored = Map.fromList [("minimap", A.Bool True), ("uiFontSize", A.Number 13)]
            removeIfExists path
            written <- writeUISettings path stored
            written `shouldBe` Right ()
            readUISettings path `shouldReturn` A.Object (KM.fromList [("minimap", A.Bool True), ("uiFontSize", A.Number 13)])

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
                check (jsonBody body `shouldBe` fullUISettings)
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
            it "a genuinely large body gives 413" $
                jsonRequest methodPost "/api/login" (BL.fromStrict (BS.replicate 70000 97))
                    `shouldRespondWith` 413

-- | 端到端：真 HTTP + 真 Rust 存储进程
e2eSpec :: FilePath -> Spec
e2eSpec staticDir = describe "End-to-end: browser -> Haskell -> Rust" $ do
    it "isolates database contexts across HTTP clients and supports qualified SQL" $ do
        serverBin <- locateServerOrSkip
        (pipe, _dataDir, cfg) <- makeServerConfig "iso"
        started <- startStorageProcess serverBin cfg
        running <- either fail pure started
        bracket (pure running) stopStorageProcess $ \_sp -> do
            backend <- ipcBackend
            sessions <- newSessionStore getCurrentTime defaultSessionPolicy
            limiter <- newRateLimiter getCurrentTime 50 300
            env <- newAppEnv backend sessions testCredential limiter staticDir
            app <- webApp env
            Warp.testWithApplication (pure app) $ \port -> do
                mgr <- newManager defaultManagerSettings
                signedInResponse <- e2ePost mgr port "/api/login" (loginBody "admin" "s3cret") []
                let auth = [("Cookie", fromMaybe "" (e2eCookie signedInResponse))]
                    alpha = ("X-ChuSQL-Database", "alpha") : auth
                    beta = ("X-ChuSQL-Database", "beta") : auth
                    query headers sql = e2ePost mgr port "/api/query" (sqlBody sql) headers
                mapM_ (\name -> do
                    created <- query auth ("CREATE DATABASE " <> name)
                    statusCode (responseStatus created) `shouldBe` 200) ["alpha", "beta"]
                mapM_ (\headers -> do
                    created <- query headers "CREATE TABLE items (id int)"
                    statusCode (responseStatus created) `shouldBe` 200) [alpha, beta]
                inserted <- query alpha "INSERT INTO items (id) VALUES (1)"
                statusCode (responseStatus inserted) `shouldBe` 200
                selected <- query beta "SELECT * FROM items"
                asInt (at "rowCount" (fromMaybe A.Null (decode (responseBody selected)))) `shouldBe` 0
                qualified <- query beta "SELECT * FROM alpha.items"
                asInt (at "rowCount" (fromMaybe A.Null (decode (responseBody qualified)))) `shouldBe` 1
                betaInsert <- query beta "INSERT INTO items (id) VALUES (1)"
                statusCode (responseStatus betaInsert) `shouldBe` 200
                joined <- query auth "SELECT a.id FROM alpha.items a JOIN beta.items b ON a.id = b.id"
                asInt (at "rowCount" (fromMaybe A.Null (decode (responseBody joined)))) `shouldBe` 1
                inherited <- getEnvironment
                -- 构建好的 chusql-cli 也要能一条命令跑通（没构建就跳过）
                locateCliExe >>= \found -> case found of
                    Nothing -> pendingWith "csql is not built (run stack build chusql-cli:exe:csql)"
                    Just cliExe -> do
                        cliConfig <- makeCliHome port pipe "admin" "s3cret"
                        (cliExit, cliOut, cliErr) <-
                            readCreateProcessWithExitCode
                                (proc cliExe ["--config", cliConfig, "--database", "alpha", "--format", "json", "-e", "SELECT * FROM items"])
                                    { Process.env = Just inherited
                                    }
                                ""
                        (cliExit, cliErr) `shouldBe` (ExitSuccess, "")
                        asInt (at "rowCount" (fromMaybe A.Null (decode (BL.fromStrict (BSC.pack cliOut))))) `shouldBe` 1
                switched <- query auth "USE alpha"
                at "database" (fromMaybe A.Null (decode (responseBody switched))) `shouldBe` A.String "alpha"
                -- 限定名不需要先选库；裸表名没有当前库就是 400
                qualifiedWithoutDatabase <- query auth "SELECT * FROM alpha.items"
                asInt (at "rowCount" (fromMaybe A.Null (decode (responseBody qualifiedWithoutDatabase)))) `shouldBe` 1
                bareWithoutDatabase <- query auth "SELECT * FROM items"
                statusCode (responseStatus bareWithoutDatabase) `shouldBe` 400
                at "error" (fromMaybe A.Null (decode (responseBody bareWithoutDatabase))) `shouldBe` A.String "no_database"
                listed <- e2eGet mgr port "/api/databases" auth
                items (fromMaybe A.Null (decode (responseBody listed))) `shouldBe` map A.String ["alpha", "beta", "system"]
                createdDb <- e2ePost mgr port "/api/databases" "{\"name\":\"gamma\"}" auth
                statusCode (responseStatus createdDb) `shouldBe` 200
                duplicateDb <- e2ePost mgr port "/api/databases" "{\"name\":\"gamma\"}" auth
                statusCode (responseStatus duplicateDb) `shouldBe` 400
                droppedDb <- e2eSend mgr port "DELETE" "/api/databases/gamma" "" auth
                statusCode (responseStatus droppedDb) `shouldBe` 200
                keptSystem <- e2eSend mgr port "DELETE" "/api/databases/system" "" auth
                statusCode (responseStatus keptSystem) `shouldBe` 400
                -- test 不再是保留名：能建也能删
                madeTest <- e2ePost mgr port "/api/databases" "{\"name\":\"test\"}" auth
                statusCode (responseStatus madeTest) `shouldBe` 200
                droppedTest <- e2eSend mgr port "DELETE" "/api/databases/test" "" auth
                statusCode (responseStatus droppedTest) `shouldBe` 200
                bad <- query (("X-ChuSQL-Database", "../escape") : auth) "SELECT 1"
                statusCode (responseStatus bad) `shouldBe` 400

    it "login -> create table -> insert -> query -> logout all the way through" $ do
        serverBin <- locateServerOrSkip
        (_pipe, dataDir, cfg) <- makeServerConfig "flow"
        started <- startStorageProcess serverBin cfg
        running <- case started of
            Left err -> do
                expectationFailure err
                fail "the storage process did not start"
            Right ok -> pure ok
        bracket (pure running) stopStorageProcess $ \_sp -> do
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
                let bare = [("Cookie", fromMaybe "" cookie)]
                -- 服务不再自带默认库：先建一个自己的库，再把它当请求上下文
                madeDb <- e2ePost mgr boundPort "/api/databases" "{\"name\":\"main\"}" bare
                statusCode (responseStatus madeDb) `shouldBe` 200
                let auth = ("X-ChuSQL-Database", "main") : bare
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
                exists <- doesDirectoryExist dataDir
                exists `shouldBe` True

    it "ordinary accounts survive restart, the administrator resets passwords and ordinary users cannot administer accounts" $ do
        serverBin <- locateServerOrSkip
        (_pipe, _dataDir, cfg) <- makeServerConfig "restart"
        Right running <- startStorageProcess serverBin cfg
        bracket (pure running) stopStorageProcess $ \sp -> do
            backend <- ipcBackend
            sessions <- newSessionStore getCurrentTime defaultSessionPolicy
            limiter <- newRateLimiter getCurrentTime 50 300
            env <- newAppEnv backend sessions testCredential limiter staticDir
            app <- webApp env
            Warp.testWithApplication (pure app) $ \port -> do
                mgr <- newManager defaultManagerSettings
                admin <- e2ePost mgr port "/api/login" (loginBody "admin" "s3cret") []
                let auth = [("Cookie", fromMaybe "" (e2eCookie admin))]
                    authSystem = ("X-ChuSQL-Database", "system") : auth
                created <- e2ePost mgr port "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"alice\",\"password\":\"alice-password\"}}" authSystem
                statusCode (responseStatus created) `shouldBe` 200
                duplicate <- e2ePost mgr port "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"ALICE\",\"password\":\"alice-password\"}}" authSystem
                statusCode (responseStatus duplicate) `shouldBe` 409
                goneSwitch <- e2ePost mgr port "/api/account/switch" (loginBody "alice" "alice-password") auth
                statusCode (responseStatus goneSwitch) `shouldBe` 404
                stillAdmin <- e2eGet mgr port "/api/session" auth
                statusCode (responseStatus stillAdmin) `shouldBe` 200
                alice <- e2ePost mgr port "/api/login" (loginBody "alice" "alice-password") []
                statusCode (responseStatus alice) `shouldBe` 204
                let aliceAuth = [("Cookie", fromMaybe "" (e2eCookie alice))]
                    aliceSystem = ("X-ChuSQL-Database", "system") : aliceAuth
                misplaced <- e2ePost mgr port "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"other\",\"password\":\"other-password\"}}" aliceAuth
                statusCode (responseStatus misplaced) `shouldBe` 400
                forbidden <- e2ePost mgr port "/api/tables/__chusql_users/rows" "{\"values\":{\"user\":\"other\",\"password\":\"other-password\"}}" aliceSystem
                statusCode (responseStatus forbidden) `shouldBe` 403
                listed <- e2eGet mgr port "/api/tables/__chusql_users/rows" aliceSystem
                statusCode (responseStatus listed) `shouldBe` 403
                blockedSystem <- e2eGet mgr port "/api/tables" aliceSystem
                statusCode (responseStatus blockedSystem) `shouldBe` 403
                aliceDbs <- e2eGet mgr port "/api/databases" aliceAuth
                items (fromMaybe A.Null (decode (responseBody aliceDbs))) `shouldSatisfy` not . elem (A.String "system")
                tableList <- e2eGet mgr port "/api/tables" aliceAuth
                statusCode (responseStatus tableList) `shouldBe` 400
                at "error" (fromMaybe A.Null (decode (responseBody tableList))) `shouldBe` A.String "no_database"
                _ <- e2ePost mgr port "/api/databases" "{\"name\":\"shop\"}" auth
                shopTables <- e2eGet mgr port "/api/tables" (("X-ChuSQL-Database", "shop") : aliceAuth)
                statusCode (responseStatus shopTables) `shouldBe` 200
                BL.toStrict (responseBody shopTables) `shouldSatisfy` (not . BS.isInfixOf "__chusql_users")
                settings <- e2eSend mgr port "PUT" "/api/settings" "{\"values\":{\"password-min-length\":\"8\"}}" aliceAuth
                statusCode (responseStatus settings) `shouldBe` 403
                hidden <- e2ePost mgr port "/api/query" "{\"sql\":\"SELECT * FROM __chusql_users\"}" aliceAuth
                statusCode (responseStatus hidden) `shouldSatisfy` (>= 400)
                BL.toStrict (responseBody hidden) `shouldSatisfy` (not . BS.isInfixOf "pbkdf2")
                selfService <- e2ePost mgr port "/api/account/password" "{\"current\":\"alice-password\",\"next\":\"alice-new-password\"}" aliceAuth
                statusCode (responseStatus selfService) `shouldBe` 404
                adminAgain <- e2ePost mgr port "/api/login" (loginBody "admin" "s3cret") []
                let authAgain = [("Cookie", fromMaybe "" (e2eCookie adminAgain))]
                reset <- e2eSend mgr port "PATCH" "/api/tables/__chusql_users/rows/alice" "{\"values\":{\"password\":\"alice-new-password\"}}" (("X-ChuSQL-Database", "system") : authAgain)
                statusCode (responseStatus reset) `shouldBe` 200
            stopStorageProcess sp
            (_, _, _, process) <- createProcess (proc serverBin ["--config", cfg]){std_out = NoStream, std_err = NoStream}
            let restarted = StorageProcess process (spPipe sp)
            bracket (pure restarted) stopStorageProcess $ \_ -> do
                waitForStorage 200 >>= (`shouldBe` True)
                backend2 <- ipcBackend
                sessions2 <- newSessionStore getCurrentTime defaultSessionPolicy
                limiter2 <- newRateLimiter getCurrentTime 50 300
                env2 <- newAppEnv backend2 sessions2 testCredential limiter2 staticDir
                app2 <- webApp env2
                Warp.testWithApplication (pure app2) $ \port -> do
                    mgr <- newManager defaultManagerSettings
                    fresh <- e2ePost mgr port "/api/login" (loginBody "alice" "alice-new-password") []
                    statusCode (responseStatus fresh) `shouldBe` 204
                    stale <- e2ePost mgr port "/api/login" (loginBody "alice" "alice-password") []
                    statusCode (responseStatus stale) `shouldBe` 401
                    root <- e2ePost mgr port "/api/login" (loginBody "admin" "s3cret") []
                    statusCode (responseStatus root) `shouldBe` 204

-- | 找构建好的 csql 可执行文件；没构建出可执行文件就返回 Nothing
locateCliExe :: IO (Maybe FilePath)
locateCliExe = do
    let name = platformBinaryName os "csql"
    firstFile . concat =<< mapM (\root -> findFileUnder 8 (root </> ".stack-work") name) cliRoots

-- | csql 可能落地的根：测试的 CWD 是 chusql-web/，所以两种相对位置都试
cliRoots :: [FilePath]
cliRoots = ["", "chusql-cli", ".." </> "chusql-cli", "chusql-web", ".." </> "chusql-web", ".."]

-- | 按名字在 .stack-work 里递进找可执行文件。
--   stack 会把它放在带平台、包哈希与编译器版本的多层目录里，写死路径会过期。
findFileUnder :: Int -> FilePath -> FilePath -> IO [FilePath]
findFileUnder depth dir name = do
    exists <- doesDirectoryExist dir
    if not exists || depth <= 0
        then pure []
        else do
            entries <- System.Directory.listDirectory dir
            let (named, rest) = partition (== name) entries
            deeper <- mapM (\entry -> findFileUnder (depth - 1) (dir </> entry) name) (filter diggable rest)
            pure ([dir </> entry | entry <- named] ++ concat deeper)
  where
    -- 只往可能放可执行文件的目录里钻，跳过编译中间产物与包索引
    diggable entry =
        not ("." `isPrefixOf` entry)
            && entry `notElem` ["work", "tmp", "package-index", "snapshots", "pantry", "indices"]

-- | 一次端到端用例用的管名后缀：同一进程内唯一
uniqueSuffix :: IO String
uniqueSuffix = do
    u <- newUnique
    pure (show (hashUnique u))

-- | 造一份临时 chusql.toml（[server] pipe_name + [storage] data_dir），路径由调用方显式交给存储进程
makeServerConfig :: String -> IO (String, FilePath, FilePath)
makeServerConfig label = do
    tmp <- getTemporaryDirectory
    suffix <- uniqueSuffix
    let pipe = "chusql-web-" ++ label ++ "-" ++ suffix
        dataDir = tmp </> pipe
        cfg = dataDir ++ ".toml"
        -- TOML 的普通字符串会吃反斜杠，路径统一用正斜杠
        slashed = map (\c -> if c == '\\' then '/' else c) dataDir
    -- 临时路径在不同轮次可能重名，先扫干净，别把上一次的库带进来
    removePathForcibly cfg
    removePathForcibly dataDir
    createDirectoryIfMissing True dataDir
    writeFile
        cfg
        ( unlines
            [ "[server]"
            , "pipe_name = \"" ++ pipe ++ "\""
            , "[storage]"
            , "data_dir = \"" ++ slashed ++ "\""
            ]
        )
    setPipeName pipe
    pure (pipe, dataDir, cfg)

-- | 给 CLI 子进程造一份临时 chusql.toml：管名与 root 凭据都在里面，返回配置文件路径
makeCliHome :: Int -> String -> String -> String -> IO FilePath
makeCliHome port pipe user password = do
    tmp <- getTemporaryDirectory
    let home = tmp </> ("chusql-cli-" ++ show port)
        cfg = home </> "chusql.toml"
    createDirectoryIfMissing True home
    writeFile
        cfg
        ( unlines
            [ "[server]"
            , "pipe_name = \"" ++ pipe ++ "\""
            , "[web]"
            , "user = \"" ++ user ++ "\""
            , "password = \"" ++ password ++ "\""
            ]
        )
    pure cfg

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
                            [ storageDir </> "target" </> "debug" </> platformBinaryName os "chusql-storage"
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
