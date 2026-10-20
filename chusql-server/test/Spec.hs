{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import ChuSQL.Core.Engine.Storage.IPC (
    Account (..),
    Request (ReqAccountCreate, ReqAccountReset, ReqAccountsList, ReqIdentityAlter),
    TableInfo (..),
    closeConnection,
    localStorageLink,
    setStorageLink,
 )
import ChuSQL.Core.Engine.Syntax.AST (Statement)
import ChuSQL.Core.Engine.Syntax.Parser (parseStatement)
import ChuSQL.Core.Model (Database, Histogram (..), Row, Table (..), TableIdentity (..), Value (..), pattern TInt, pattern TStr)
import ChuSQL.Core.Protocol (SchemaColumn (..), TxnOp (..))
import qualified ChuSQL.Driver.Protocol as Driver
import qualified ChuSQL.Driver.Result as DriverResult
import ChuSQL.Server.Accounts (Principal (..), ensureRootAccount)
import ChuSQL.Server.Auth (hashPasswordWith)
import ChuSQL.Server.Backend (Backend (..), StatementResult (..), applyTransactionMemory, ipcBackend, memoryBackend)
import qualified ChuSQL.Server.Config as Config
import ChuSQL.Server.Privileges (
    PrivilegeCommand (..),
    PrivilegeError (..),
    Privileges,
    affectedAccounts,
    authorize,
    authorizeConnect,
    claimObject,
    listRoleViews,
    listSelfGrants,
    newPrivileges,
    prepareObject,
    runPrivilegeCommand,
 )
import ChuSQL.Server.Protocol (ClientRequest (..), Grant (..), RoleView (..), ServerResponse (..), decodeRequest, encodeResponse)
import ChuSQL.Server.RateLimit (newRateLimiter)
import ChuSQL.Server.Security (databaseSecurity, writeSecurity)
import ChuSQL.Server.Session (QueryResult (..), Session, SessionError (..), authenticateSessionCoded, newSession, runStatementCoded, runStorageCoded)
import qualified ChuSQL.Server.Settings as Settings
import ChuSQL.Server.TCP (
    ServerConfig (..),
    ServerEnv (..),
    ServerHandle (..),
    defaultServerConfig,
    loadServerConfigAt,
    newServerEnv,
    startServer,
 )
import Control.Concurrent (forkFinally, forkIO, killThread, threadDelay)
import Control.Concurrent.MVar (MVar, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar)
import Control.Concurrent.QSem (signalQSem, waitQSem)
import Control.Exception (IOException, bracket, bracket_, evaluate, finally, try)
import Control.Monad (forM_, replicateM_, void, when, (>=>))
import Data.Aeson (decode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Either (lefts)
import Data.IORef (atomicModifyIORef', modifyIORef', newIORef, readIORef, writeIORef)
import Data.List (sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified Data.Vector as V
import Network.Socket (
    AddrInfo (..),
    SocketType (Stream),
    addrAddress,
    addrFamily,
    connect,
    defaultHints,
    defaultProtocol,
    getAddrInfo,
    socket,
    socketToHandle,
    withSocketsDo,
 )
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory, removeFile, removePathForcibly)
import System.FilePath ((</>))
import System.IO (
    BufferMode (LineBuffering),
    Handle,
    IOMode (ReadWriteMode),
    hClose,
    hFlush,
    hIsEOF,
    hSetBuffering,
    hSetNewlineMode,
    noNewlineTranslation,
    openTempFile,
 )
import System.Mem.StableName (makeStableName)
import System.Timeout (timeout)
#if defined(linux_HOST_OS)
import Foreign.C.Types (CInt (..))
import Network.Socket (Family (AF_UNIX), SockAddr (SockAddrUnix))
#endif
import Test.Hspec hiding (after, before)

-- TCP 服务器测试：协议编解码、配置读取与真连服务器的端到端用例。

-- | 测试入口
main :: IO ()
main = hspec spec

-- | 全部用例
spec :: Spec
spec = do
    describe "independent driver wire compatibility" $ do
        it "decodes core results without a shared model" $ do
            let result = QueryResult ["large", "label"] [[VInt 9007199254740993, VStr "中文"]] 1 True (Just "demo")
            Driver.decodeResponse (encodeResponse (RespResult result))
                `shouldBe` Right (Driver.RespResult (DriverResult.QueryResult ["large", "label"] [[A.Number 9007199254740993, A.String "中文"]] 1 True (Just "demo")))
        it "preserves catalog identities and opaque defaults" $ do
            let column = SchemaColumn "id" "int" False (Just (VInt 1)) False True True Nothing
                info = TableInfo "demo" [column] 0 ["id"] [] [] (Just (TableIdentity 9007199254740993 2)) (Just 3)
            case Driver.decodeResponse (encodeResponse (RespCatalog [info])) of
                Right (Driver.RespCatalog [decoded]) -> A.toJSON decoded `shouldBe` A.toJSON info
                other -> expectationFailure (show other)
    tcpSpec
    tcpServerSpec
    privilegeSpec
    objectPrivilegeSpec
    databaseGateSpec
    transactionSpec
    ipcConcurrencySpec
    identitySpec
#if defined(linux_HOST_OS)
    localRootLoginSpec
#endif

-- | 身份属性与权限边界的端到端测试
identitySpec :: Spec
identitySpec = describe "unified identities" $ do
    it "lets catalog managers manage ordinary identities without gaining superuser" $ withTcpServer defaultServerConfig $ \_ handle ->
        withConnection handle $ \admin -> withConnection handle $ \manager -> do
            _ <- tcpLogin admin "admin" "s3cret"
            _ <- tcpQuery admin "create user manager identified by 'Worker-Pass123!'"
            changed <- tcpQuery admin "alter user manager system_catalog_manager"
            asText (at "status" changed) `shouldBe` "result"
            signed <- tcpLogin manager "manager" "Worker-Pass123!"
            asBool (at "admin" signed) `shouldBe` False
            created <- tcpQuery manager "create user managed identified by 'Worker-Pass123!'"
            asText (at "status" created) `shouldBe` "result"
            shown <- tcpQuery manager "show roles"
            asText (at "status" shown) `shouldBe` "result"
            switched <- tcpQuery manager "use system"
            asText (at "status" switched) `shouldBe` "result"
            mapM_
                ( tcpQuery manager
                    >=> (\reply -> asText (at "code" reply) `shouldBe` "forbidden")
                )
                ["alter user manager superuser", "alter user managed system_catalog_manager", "alter user admin identified by 'Another-Pass123!'", "create database forbidden"]
    it "does not inherit catalog management through role membership" $ withTcpServer defaultServerConfig $ \_ handle ->
        withConnection handle $ \admin -> withConnection handle $ \worker -> do
            _ <- tcpLogin admin "admin" "s3cret"
            _ <- tcpQuery admin "create user worker identified by 'Worker-Pass123!'"
            _ <- tcpQuery admin "create role catalog_admin"
            _ <- tcpQuery admin "alter role catalog_admin system_catalog_manager"
            _ <- tcpQuery admin "grant catalog_admin to worker"
            _ <- tcpLogin worker "worker" "Worker-Pass123!"
            denied <- tcpQuery worker "create user elevated identified by 'Worker-Pass123!'"
            asText (at "code" denied) `shouldBe` "forbidden"
    it "shares names and shows safe identity attributes" $ withTcpServer defaultServerConfig $ \_ handle ->
        withConnection handle $ \h -> do
            _ <- tcpLogin h "admin" "s3cret"
            _ <- tcpQuery h "create role reader"
            conflict <- tcpQuery h "create user reader identified by 'Worker-Pass123!'"
            asText (at "code" conflict) `shouldBe` "conflict"
            _ <- tcpQuery h "create user alice identified by 'Worker-Pass123!'"
            other <- tcpQuery h "create role alice"
            asText (at "code" other) `shouldBe` "conflict"
            shown <- tcpQuery h "show roles"
            map asText (items (at "columns" shown)) `shouldBe` ["id", "name", "can_login", "is_superuser", "enabled", "system_catalog_manager", "created_at"]
            asInt (at "rowCount" shown) `shouldBe` 3
            reader <- tcpQuery h "alter role reader login nologin"
            asText (at "code" reader) `shouldBe` "bad_request"
    it "turns a role into a login identity and promotes by attribute" $ withTcpServer defaultServerConfig $ \_ handle ->
        withConnection handle $ \admin -> withConnection handle $ \worker -> do
            _ <- tcpLogin admin "admin" "s3cret"
            _ <- tcpQuery admin "create role worker"
            _ <- tcpQuery admin "alter role worker identified by 'Worker-Pass123!'"
            denied <- tcpLogin worker "worker" "Worker-Pass123!"
            asText (at "status" denied) `shouldBe` "error"
            changed <- tcpQuery admin "alter role worker login superuser"
            asText (at "status" changed) `shouldBe` "result"
            allowed <- tcpLogin worker "worker" "Worker-Pass123!"
            asBool (at "admin" allowed) `shouldBe` True
            created <- tcpQuery worker "create role managed"
            asText (at "status" created) `shouldBe` "result"
    it "does not inherit the superuser attribute through membership" $ withTcpServer defaultServerConfig $ \_ handle ->
        withConnection handle $ \admin -> withConnection handle $ \alice -> do
            _ <- tcpLogin admin "admin" "s3cret"
            _ <- tcpQuery admin "create user alice identified by 'Worker-Pass123!'"
            _ <- tcpQuery admin "create role powerful"
            _ <- tcpQuery admin "alter role powerful superuser"
            _ <- tcpQuery admin "grant powerful to alice"
            signed <- tcpLogin alice "alice" "Worker-Pass123!"
            asBool (at "admin" signed) `shouldBe` False
            denied <- tcpQuery alice "create role elevated"
            asText (at "code" denied) `shouldBe` "forbidden"
    it "treats a demoted configured administrator as ordinary" $ withTcpServer defaultServerConfig $ \_ handle ->
        withConnection handle $ \admin -> do
            _ <- tcpLogin admin "admin" "s3cret"
            _ <- tcpQuery admin "create user owner identified by 'Worker-Pass123!'"
            _ <- tcpQuery admin "alter user owner superuser"
            demoted <- tcpQuery admin "alter user admin nosuperuser"
            asText (at "status" demoted) `shouldBe` "result"
            withConnection handle $ \fresh -> do
                signed <- tcpLogin fresh "admin" "s3cret"
                asBool (at "admin" signed) `shouldBe` False
                denied <- tcpQuery fresh "show roles"
                asText (at "code" denied) `shouldBe` "forbidden"
    it "protects the last enabled login superuser" $ withTcpServer defaultServerConfig $ \_ handle ->
        withConnection handle $ \h -> do
            _ <- tcpLogin h "admin" "s3cret"
            mapM_
                ( tcpQuery h
                    >=> (\reply -> asText (at "code" reply) `shouldBe` "bad_request")
                )
                ["alter user admin disabled", "alter role admin nologin", "alter user admin nosuperuser", "drop user admin", "drop role admin"]
            shown <- tcpQuery h "show roles"
            asText (at "status" shown) `shouldBe` "result"
    it "stops disabled role inheritance and disconnects its members" $ withTcpServer defaultServerConfig $ \_ handle ->
        withConnection handle $ \admin -> withConnection handle $ \alice -> do
            _ <- tcpLogin admin "admin" "s3cret"
            _ <- tcpQuery admin "use test"
            _ <- tcpQuery admin "create user alice identified by 'Worker-Pass123!'"
            _ <- tcpQuery admin "create role reader"
            _ <- tcpQuery admin "grant select on users to reader"
            _ <- tcpQuery admin "grant reader to alice"
            _ <- tcpLogin alice "alice" "Worker-Pass123!"
            _ <- tcpQuery alice "use test"
            before <- tcpQuery alice "select * from users"
            asText (at "status" before) `shouldBe` "result"
            changed <- tcpQuery admin "alter role reader disabled"
            asText (at "status" changed) `shouldBe` "result"
            refused <- tcpQuery alice "use test"
            asText (at "code" refused) `shouldBe` "unauthorized"
            tcpClosed alice
            withConnection handle $ \fresh -> do
                _ <- tcpLogin fresh "alice" "Worker-Pass123!"
                _ <- tcpQuery fresh "use test"
                after <- tcpQuery fresh "select * from users"
                asText (at "code" after) `shouldBe` "forbidden"
                escalation <- tcpQuery fresh "alter user alice superuser"
                asText (at "code" escalation) `shouldBe` "forbidden"
    it "disables login identities and invalidates their connections" $ withTcpServer defaultServerConfig $ \_ handle ->
        withConnection handle $ \admin -> withConnection handle $ \alice -> do
            _ <- tcpLogin admin "admin" "s3cret"
            _ <- tcpQuery admin "create user alice identified by 'Worker-Pass123!'"
            _ <- tcpLogin alice "alice" "Worker-Pass123!"
            _ <- tcpQuery admin "alter user alice disabled"
            refused <- tcpQuery alice "use test"
            asText (at "code" refused) `shouldBe` "unauthorized"
            tcpClosed alice
            withConnection handle $ \fresh -> do
                denied <- tcpLogin fresh "alice" "Worker-Pass123!"
                asText (at "status" denied) `shouldBe` "error"
                _ <- tcpQuery admin "alter user alice enabled"
                allowed <- tcpLogin fresh "alice" "Worker-Pass123!"
                asText (at "status" allowed) `shouldBe` "ok"
    it "does not reuse dropped identity ids or permissions" $ withTcpServer defaultServerConfig $ \_ handle ->
        withConnection handle $ \admin -> do
            _ <- tcpLogin admin "admin" "s3cret"
            _ <- tcpQuery admin "use test"
            _ <- tcpQuery admin "create user alice identified by 'Worker-Pass123!'"
            _ <- tcpQuery admin "grant select on users to alice"
            before <- tcpQuery admin "show roles"
            _ <- tcpQuery admin "drop user alice"
            _ <- tcpQuery admin "create user alice identified by 'Worker-Pass123!'"
            after <- tcpQuery admin "show roles"
            at "rows" after `shouldNotBe` at "rows" before
            withConnection handle $ \alice -> do
                _ <- tcpLogin alice "alice" "Worker-Pass123!"
                _ <- tcpQuery alice "use test"
                denied <- tcpQuery alice "select * from users"
                asText (at "code" denied) `shouldBe` "forbidden"

-- 夹具：内存后端装在 MVar 里

-- | 夹具库名
testDatabaseName :: String
testDatabaseName = "test"

-- | users 5 行 + orders 1 行
testDb :: Database
testDb =
    [
        ( "users"
        , Table
            "users"
            [("id", TInt), ("name", TStr), ("age", TInt)]
            [ [("id", VInt i), ("name", VStr ("user" ++ show i)), ("age", VInt (20 + i))]
            | i <- [1 .. 5]
            ]
            Nothing
            Nothing
            Nothing
        )
    ,
        ( "orders"
        , Table
            "orders"
            [("id", TInt), ("user_id", TInt)]
            [[("id", VInt 1), ("user_id", VInt 1)]]
            Nothing
            Nothing
            Nothing
        )
    ]

-- | 测试管理员名字
testRootName :: Text
testRootName = "admin"

-- | 测试管理员口令
testRootPassword :: Text
testRootPassword = "s3cret"

-- | 口令哈希：迭代次数压低，跑得快
testPasswordHash :: Text -> Text
testPasswordHash = hashPasswordWith 1000 (BS.replicate 16 7)

-- 小工具

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

-- | JSON 当布尔看
asBool :: A.Value -> Bool
asBool (A.Bool b) = b
asBool _ = False

-- | 第 i 个元素（越界给 Nothing，不用 `!!`）
nth :: Int -> [a] -> Maybe a
nth i xs = case drop i xs of
    (x : _) -> Just x
    [] -> Nothing

-- | 临时 TOML
tcpConfigPath :: String -> IO FilePath
tcpConfigPath name = uniqueTempPath ("chusql-server-test-" ++ name) ".toml"

-- | 临时设置文件（新会话读口令策略用）
tempSettingsPath :: String -> IO FilePath
tempSettingsPath name = uniqueTempPath ("chusql-server-test-" ++ name ++ "-settings") ".json"

-- | 新建唯一临时文件路径
uniqueTempPath :: String -> String -> IO FilePath
uniqueTempPath prefix suffix = do
    tmp <- getTemporaryDirectory
    (path, handle) <- openTempFile tmp (prefix ++ "-XXXX" ++ suffix)
    hClose handle
    pure path

-- | 文件不存在也算成功
removeIfExists :: FilePath -> IO ()
removeIfExists path = do
    there <- doesFileExist path
    when there $
        void (try (removeFile path) :: IO (Either IOException ()))

-- | 起一台内存后端的测试服务器：管理员已设口令 `s3cret`
withTcpServer :: ServerConfig -> (ServerEnv -> ServerHandle -> IO a) -> IO a
withTcpServer = withTcpServerAs (Just testRootPassword)

-- | 起内存后端测试服务器；Nothing 表示首启未设口令
withTcpServerAs :: Maybe Text -> ServerConfig -> (ServerEnv -> ServerHandle -> IO a) -> IO a
withTcpServerAs password config body = do
    db <- newMVar testDb
    limiter <- newRateLimiter getCurrentTime 50 300
    path <- tempSettingsPath "tcp-server"
    removeIfExists path
    let backend = memoryBackend testDatabaseName db
    _ <- ensureRootAccount backend testRootName
    case password of
        Nothing -> pure ()
        Just raw -> void (beAccounts backend (ReqAccountReset testRootName (testPasswordHash raw)))
    env <- newServerEnv backend testRootName path config{scPort = 0} limiter
    handle <- startServer env
    body env handle `finally` shStop handle

-- | 连上服务器并配成行缓冲句柄
withConnection :: ServerHandle -> (Handle -> IO a) -> IO a
withConnection handle = bracket open closeQuietly
  where
    -- \| 关连接，忽略异常
    closeQuietly h = do
        _ <- try (hClose h) :: IO (Either IOException ())
        pure ()
    -- \| 建连接并配成行缓冲句柄
    open = withSocketsDo $ do
        infos <- getAddrInfo (Just defaultHints{addrSocketType = Stream}) (Just "127.0.0.1") (Just (show (shPort handle)))
        info <- case infos of
            (i : _) -> pure i
            [] -> ioError (userError "cannot resolve the test server address")
        sock <- socket (addrFamily info) Stream defaultProtocol
        connect sock (addrAddress info)
        h <- socketToHandle sock ReadWriteMode
        hSetBuffering h LineBuffering
        hSetNewlineMode h noNewlineTranslation
        pure h

-- | 发一行收一行；服务器不回话就判失败，别把用例挂死
tcpTalk :: Handle -> BS.ByteString -> IO A.Value
tcpTalk h raw = do
    BSC.hPutStr h raw
    BSC.hPutStr h "\n"
    hFlush h
    line <- timeout (5 * 1000000) (BSC.hGetLine h)
    case line of
        Just got -> pure (fromMaybe A.Null (decode (BL.fromStrict got)))
        Nothing -> do
            expectationFailure "the TCP server did not answer in time"
            pure A.Null

-- | 登录一行
tcpLogin :: Handle -> Text -> Text -> IO A.Value
tcpLogin h user password =
    tcpTalk h (BSC.pack ("{\"method\":\"login\",\"user\":\"" ++ T.unpack user ++ "\",\"password\":\"" ++ T.unpack password ++ "\"}"))

-- | 查询一行
tcpQuery :: Handle -> Text -> IO A.Value
tcpQuery h sql = tcpTalk h (BSC.pack ("{\"method\":\"query\",\"sql\":\"" ++ T.unpack sql ++ "\"}"))

-- | 断言对端已经收工（读到 EOF）
tcpClosed :: Handle -> IO ()
tcpClosed h = do
    done <- timeout (5 * 1000000) (hIsEOF h)
    done `shouldBe` Just True

-- 用例

-- | 协议与配置的用例
tcpSpec :: Spec
tcpSpec = do
    describe "TCP protocol: decoding a request line" $ do
        it "hello without a protocol number takes the current version" $
            decodeRequest "{\"method\":\"hello\"}" `shouldBe` Right (ReqHello 1)
        it "hello keeps the protocol number the client asked for" $
            decodeRequest "{\"method\":\"hello\",\"protocol\":2}" `shouldBe` Right (ReqHello 2)
        it "method names are case-insensitive" $
            decodeRequest "{\"method\":\"PING\"}" `shouldBe` Right ReqPing
        it "unknown extra fields are ignored" $
            decodeRequest "{\"method\":\"quit\",\"line\":7}" `shouldBe` Right ReqQuit
        it "login without a password is refused" $
            decodeRequest "{\"method\":\"login\",\"user\":\"admin\"}" `shouldBe` Left "login needs both user and password"
        it "query without sql is refused" $
            decodeRequest "{\"method\":\"query\"}" `shouldBe` Left "query needs sql"
        it "an unknown method is named in the error" $
            decodeRequest "{\"method\":\"frobnicate\"}" `shouldBe` Left "unknown method: frobnicate"
        it "a line that is not JSON is a bad request, not a crash" $
            decodeRequest "not json" `shouldBe` Left "request is not valid JSON"

    describe "TCP protocol: encoding a response" $ do
        it "hello carries the protocol version and the server name" $ do
            let value = fromMaybe A.Null (decode (encodeResponse (RespHello 1)))
            asText (at "status" value) `shouldBe` "hello"
            asText (at "server" value) `shouldBe` "chusql-server"
            asInt (at "protocol" value) `shouldBe` 1
        it "login reports the account and whether it is the administrator" $ do
            let value = fromMaybe A.Null (decode (encodeResponse (RespLogin "admin" True)))
            asText (at "user" value) `shouldBe` "admin"
            asBool (at "admin" value) `shouldBe` True
        it "an error carries a code and a message" $ do
            let value = fromMaybe A.Null (decode (encodeResponse (RespError "no_database" "no database selected")))
            asText (at "status" value) `shouldBe` "error"
            asText (at "code" value) `shouldBe` "no_database"
            asText (at "message" value) `shouldBe` "no database selected"
        it "a result set is the command line JSON with a status field added" $ do
            let sample = QueryResult ["id"] [[VInt 1]] 1 False (Just "test")
                value = fromMaybe A.Null (decode (encodeResponse (RespResult sample)))
            asText (at "status" value) `shouldBe` "result"
            map asText (items (at "columns" value)) `shouldBe` ["id"]
            asInt (at "rowCount" value) `shouldBe` 1
            asBool (at "truncated" value) `shouldBe` False
            asText (at "database" value) `shouldBe` "test"

    describe "memory backend: catalog statistics" $ do
        it "reports a histogram for a numeric column" $ do
            db <- newMVar testDb
            let backend = memoryBackend testDatabaseName db
            infos <- beCatalog backend
            case infos of
                Left err -> expectationFailure err
                Right tables -> case [i | i <- tables, tiTable i == "users"] of
                    [] -> expectationFailure "the catalog is missing the users table"
                    (info : _) -> do
                        fmap histLow (lookup "id" (tiHistograms info)) `shouldBe` Just 1
                        fmap histHigh (lookup "id" (tiHistograms info)) `shouldBe` Just 5
                        fmap (sum . histBuckets) (lookup "id" (tiHistograms info)) `shouldBe` Just 5
                        lookup "name" (tiHistograms info) `shouldBe` Nothing

    describe "server config ([server] in settings.toml)" $ do
        it "reads authentication policy and limits from the server section" $ do
            path <- tcpConfigPath "authentication-policy"
            BS.writeFile path "[server]\nlogin_max_attempts = 4\nlogin_window = 99\npassword_min_length = 20\npassword_classes = 3\n"
            config <- Config.loadRuntimeConfigAt path
            Config.rcLoginMaxAttempts config `shouldBe` 4
            Config.rcLoginWindow config `shouldBe` 99
            settings <- Settings.readSettingsFile path
            Map.lookup "password-min-length" settings `shouldBe` Just "20"
            Map.lookup "password-classes" settings `shouldBe` Just "3"
        it "falls back to the defaults when the file has no server section" $ do
            path <- tcpConfigPath "server-default"
            removeIfExists path
            loadServerConfigAt path `shouldReturn` defaultServerConfig
        it "reads listen_host, port, max_message and max_rows" $ do
            path <- tcpConfigPath "server-keys"
            BS.writeFile path "[server]\nlisten_host = \"0.0.0.0\"\nport = 7777\nmax_message = 4096\nmax_rows = 7\n"
            config <- loadServerConfigAt path
            scHost config `shouldBe` "0.0.0.0"
            scPort config `shouldBe` 7777
            scMaxMessage config `shouldBe` 4096
            scMaxRows config `shouldBe` 7
        it "keeps unrelated section keys out of the server config" $ do
            path <- tcpConfigPath "server-other-keys"
            BS.writeFile path "[other]\nport = 8888\n\n[storage]\ndata_dir = \"/tmp/chusql\"\n\n[page]\nsize = 8192\n"
            config <- loadServerConfigAt path
            scPort config `shouldBe` scPort defaultServerConfig
            scMaxRows config `shouldBe` scMaxRows defaultServerConfig

        it "reads the Unix socket path and the local root login switch" $ do
            path <- tcpConfigPath "server-local-login"
            BS.writeFile path "[server]\nsocket = \"/run/chusql/chusql.sock\"\nlocal_root_login = true\n"
            config <- loadServerConfigAt path
            scSocketPath config `shouldBe` Just "/run/chusql/chusql.sock"
            scLocalRootLogin config `shouldBe` True
        it "rejects local root login without a socket" $ do
            path <- tcpConfigPath "server-local-login-bad"
            BS.writeFile path "[server]\nlocal_root_login = true\n"
            loadServerConfigAt path `shouldThrow` anyIOException
        it "reads resource limits and rejects malformed or out-of-range values" $ do
            path <- tcpConfigPath "server-resources"
            BS.writeFile path "[server]\nworker_threads=2\nmax_concurrent_queries=2\nmax_connections=32\nconnection_idle=300\ntransaction_idle=60\ntransaction_max=300\n"
            config <- loadServerConfigAt path
            (scWorkerThreads config, scMaxConcurrentQueries config, scMaxConnections config) `shouldBe` (2, 2, 32)
            (scConnectionIdle config, scTransactionIdle config, scTransactionMax config) `shouldBe` (300, 60, 300)
            forM_ ["worker_threads=0", "worker_threads=257", "max_connections=-1", "transaction_idle=oops", "transaction_max=31536001"] $ \invalid -> do
                BS.writeFile path (BSC.pack ("[server]\n" ++ invalid ++ "\n"))
                loadServerConfigAt path `shouldThrow` anyIOException

-- | 真连 TCP 服务器的用例
tcpServerSpec :: Spec
tcpServerSpec = do
    describe "configurable resource limits" $ do
        it "closes excess connections and releases their capacity after disconnect" $
            withTcpServer defaultServerConfig{scMaxConnections = 1} $ \env server -> do
                withConnection server $ \first -> do
                    reply <- tcpTalk first "{\"method\":\"hello\",\"protocol\":1}"
                    asText (at "status" reply) `shouldBe` "hello"
                    withConnection server tcpClosed
                -- \| 等待断线的会话完成清理
                let released = do
                        count <- readMVar (srvConnectionCount env)
                        if count == 0 then pure () else threadDelay 1000 >> released
                timeout 1000000 released `shouldReturn` Just ()
                withConnection server $ \next -> do
                    reply <- tcpTalk next "{\"method\":\"hello\",\"protocol\":1}"
                    asText (at "status" reply) `shouldBe` "hello"
        it "queues queries while allowing COMMIT to release a transaction" $
            withTcpServer defaultServerConfig{scMaxConcurrentQueries = 1} $ \env server ->
                withConnection server $ \writer -> withConnection server $ \reader -> do
                    _ <- tcpLogin writer testRootName testRootPassword
                    _ <- tcpLogin reader testRootName testRootPassword
                    _ <- tcpQuery writer "USE test"
                    _ <- tcpQuery reader "USE test"
                    _ <- tcpQuery writer "BEGIN"
                    slots <- maybe (fail "missing query limiter") pure (srvQuerySlots env)
                    bracket_ (waitQSem slots) (signalQSem slots) $ do
                        BSC.hPutStr reader "{\"method\":\"query\",\"sql\":\"SELECT 1\"}\n"
                        hFlush reader
                        queued <- timeout 100000 (BSC.hGetLine reader)
                        queued `shouldBe` Nothing
                        committed <- tcpQuery writer "COMMIT"
                        asText (at "status" committed) `shouldBe` "result"
                    line <- timeout 1000000 (BSC.hGetLine reader)
                    fmap (at "status" . fromMaybe A.Null . decode . BL.fromStrict) line `shouldBe` Just (A.String "result")
        it "closes idle connections with an explicit timeout response" $
            withTcpServer defaultServerConfig{scConnectionIdle = 1} $ \_ server ->
                withConnection server $ \client -> do
                    line <- timeout 3000000 (BSC.hGetLine client)
                    fmap (at "code" . fromMaybe A.Null . decode . BL.fromStrict) line `shouldBe` Just (A.String "connection_timeout")
                    tcpClosed client
        forM_ [("transaction inactivity", \config -> config{scTransactionIdle = 1}), ("transaction lifetime", \config -> config{scTransactionMax = 1})] $ \(label, limit) ->
            it ("rolls back and closes a connection after " ++ label) $
                withTcpServer (limit defaultServerConfig) $ \_ server -> do
                    withConnection server $ \writer -> do
                        _ <- tcpLogin writer testRootName testRootPassword
                        _ <- tcpQuery writer "USE test"
                        _ <- tcpQuery writer "BEGIN"
                        _ <- tcpQuery writer "INSERT INTO users (id, name, age) VALUES (999, 'expired', 99)"
                        line <- timeout 3000000 (BSC.hGetLine writer)
                        fmap (at "code" . fromMaybe A.Null . decode . BL.fromStrict) line `shouldBe` Just (A.String "transaction_expired")
                        tcpClosed writer
                    withConnection server $ \reader -> do
                        _ <- tcpLogin reader testRootName testRootPassword
                        _ <- tcpQuery reader "USE test"
                        rows <- tcpQuery reader "SELECT id FROM users WHERE id = 999"
                        at "rows" rows `shouldBe` A.toJSON ([] :: [A.Value])

    describe "TCP server: handshake and login" $ do
        it "binds a real port when asked for port 0" $
            withTcpServer defaultServerConfig $ \_ handle ->
                shPort handle `shouldSatisfy` (> 0)
        it "answers hello with the same protocol version" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    reply <- tcpTalk h "{\"method\":\"hello\",\"protocol\":1}"
                    asText (at "status" reply) `shouldBe` "hello"
                    asInt (at "protocol" reply) `shouldBe` 1
        it "still answers a client that connects long after startup" $
            -- 回归点：早先 accept 外面套 timeout 轮询，超时异常会在 accept
            -- 返回后把刚拿到的连接丢掉，只有延迟连接的客户端能撞上
            withTcpServer defaultServerConfig $ \_ handle -> do
                threadDelay 400000
                withConnection handle $ \h -> do
                    reply <- tcpTalk h "{\"method\":\"hello\",\"protocol\":1}"
                    asText (at "status" reply) `shouldBe` "hello"
                    asText (at "server" reply) `shouldBe` "chusql-server"
        it "refuses a protocol version it does not speak" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    reply <- tcpTalk h "{\"method\":\"hello\",\"protocol\":9}"
                    asText (at "code" reply) `shouldBe` "bad_request"
                    asText (at "message" reply) `shouldBe` "unsupported protocol version: 9"
        it "refuses a query before login" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    reply <- tcpQuery h "select * from users"
                    asText (at "status" reply) `shouldBe` "error"
                    asText (at "code" reply) `shouldBe` "unauthorized"
                    asText (at "message" reply) `shouldBe` "sign in first"
        it "signs the administrator in with an empty password until one is set" $
            withTcpServerAs Nothing defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    wrong <- tcpLogin h "admin" "nope"
                    asText (at "code" wrong) `shouldBe` "unauthorized"
                    good <- tcpLogin h "admin" ""
                    asText (at "status" good) `shouldBe` "ok"
                    asBool (at "admin" good) `shouldBe` True
        it "refuses a wrong password and then accepts the right one" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    bad <- tcpLogin h "admin" "wrong"
                    asText (at "status" bad) `shouldBe` "error"
                    asText (at "code" bad) `shouldBe` "unauthorized"
                    good <- tcpLogin h "admin" "s3cret"
                    asText (at "status" good) `shouldBe` "ok"
                    asText (at "user" good) `shouldBe` "admin"
                    asBool (at "admin" good) `shouldBe` True

    describe "TCP server: SQL over the wire" $ do
        it "USE picks a database and the result reports it" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    _ <- tcpLogin h "admin" "s3cret"
                    reply <- tcpQuery h "use test"
                    asText (at "status" reply) `shouldBe` "result"
                    asText (at "database" reply) `shouldBe` "test"
        it "SELECT returns the fixture rows" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    _ <- tcpLogin h "admin" "s3cret"
                    _ <- tcpQuery h "use test"
                    reply <- tcpQuery h "select * from users"
                    asText (at "status" reply) `shouldBe` "result"
                    map asText (items (at "columns" reply)) `shouldBe` ["id", "name", "age"]
                    asInt (at "rowCount" reply) `shouldBe` 5
                    let first = case map items (items (at "rows" reply)) of
                            (r : _) -> r
                            [] -> []
                    length first `shouldBe` 3
                    asInt (fromMaybe A.Null (nth 0 first)) `shouldBe` 1
                    asText (fromMaybe A.Null (nth 1 first)) `shouldBe` "user1"
        it "an unknown table maps to not_found" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    _ <- tcpLogin h "admin" "s3cret"
                    _ <- tcpQuery h "use test"
                    reply <- tcpQuery h "select * from nope"
                    asText (at "code" reply) `shouldBe` "not_found"
        it "a query without a selected database maps to no_database" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    _ <- tcpLogin h "admin" "s3cret"
                    reply <- tcpQuery h "select * from users"
                    asText (at "code" reply) `shouldBe` "no_database"
        it "an empty sql is a bad request" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    _ <- tcpLogin h "admin" "s3cret"
                    reply <- tcpTalk h "{\"method\":\"query\",\"sql\":\"  \"}"
                    asText (at "code" reply) `shouldBe` "bad_request"
                    asText (at "message" reply) `shouldBe` "sql must not be empty"
        it "the row limit truncates the rows but keeps the total count" $
            withTcpServer defaultServerConfig{scMaxRows = 2} $ \_ handle ->
                withConnection handle $ \h -> do
                    _ <- tcpLogin h "admin" "s3cret"
                    _ <- tcpQuery h "use test"
                    reply <- tcpQuery h "select * from users"
                    length (items (at "rows" reply)) `shouldBe` 2
                    asInt (at "rowCount" reply) `shouldBe` 5
                    asBool (at "truncated" reply) `shouldBe` True

    describe "TCP server: framing and lifecycle" $ do
        it "rejects unsupported analysis and local authentication methods" $
            withTcpServer defaultServerConfig $ \_ handle -> withConnection handle $ \h -> do
                forM_ ["sudo_challenge", "sudo_login", "normalization_analyze"] $ \method -> do
                    reply <- tcpTalk h (BL.toStrict (A.encode (A.object ["method" A..= (method :: Text)])))
                    asText (at "code" reply) `shouldBe` "bad_request"
                pong <- tcpTalk h "{\"method\":\"ping\"}"
                asText (at "status" pong) `shouldBe` "pong"
        it "a bad JSON line does not kill the connection" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    bad <- tcpTalk h "not json"
                    asText (at "code" bad) `shouldBe` "bad_request"
                    pong <- tcpTalk h "{\"method\":\"ping\"}"
                    asText (at "status" pong) `shouldBe` "pong"
        it "a line over the message limit is refused as too_large" $
            withTcpServer defaultServerConfig{scMaxMessage = 32} $ \_ handle ->
                withConnection handle $ \h -> do
                    reply <- tcpTalk h (BSC.pack ("{\"method\":\"query\",\"sql\":\"" ++ replicate 40 'a' ++ "\"}"))
                    asText (at "code" reply) `shouldBe` "too_large"
        it "refuses oversized input before a newline and closes the connection" $
            withTcpServer defaultServerConfig{scMaxMessage = 32} $ \_ handle ->
                withConnection handle $ \h -> do
                    BSC.hPutStr h (BSC.replicate 33 'a')
                    hFlush h
                    line <- timeout 1000000 (BSC.hGetLine h)
                    case line of
                        Nothing -> expectationFailure "oversized input waited for a newline"
                        Just raw -> asText (at "code" (fromMaybe A.Null (decode (BL.fromStrict raw)))) `shouldBe` "too_large"
                    tcpClosed h
        it "accepts exact-limit CRLF frames and preserves pipelined requests" $
            withTcpServer defaultServerConfig{scMaxMessage = 17} $ \_ handle ->
                withConnection handle $ \h -> do
                    BSC.hPutStr h "{\"method\":\"ping\"}\r\n{\"method\":\"ping\"}\n"
                    hFlush h
                    replicateM_ 2 $ do
                        raw <- timeout 1000000 (BSC.hGetLine h)
                        fmap (asText . at "status" . fromMaybe A.Null . decode . BL.fromStrict) raw `shouldBe` Just "pong"
        it "ping answers pong and quit closes the connection" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    pong <- tcpTalk h "{\"method\":\"ping\"}"
                    asText (at "status" pong) `shouldBe` "pong"
                    bye <- tcpTalk h "{\"method\":\"quit\"}"
                    asText (at "status" bye) `shouldBe` "bye"
                    tcpClosed h
        it "one connection is one session" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \h -> do
                    reply <- tcpLogin h "admin" "s3cret"
                    asText (at "status" reply) `shouldBe` "ok"
                    withConnection handle $ \other -> do
                        blocked <- tcpQuery other "select * from users"
                        asText (at "code" blocked) `shouldBe` "unauthorized"

    describe "TCP server: account changes drop live sessions" $ do
        it "disconnects a member whose role was revoked" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \admin -> do
                    _ <- tcpLogin admin "admin" "s3cret"
                    _ <- tcpQuery admin "create user bob identified by 'bob-password-1'"
                    _ <- tcpQuery admin "create role reader"
                    _ <- tcpQuery admin "use test"
                    _ <- tcpQuery admin "grant select on users to reader"
                    _ <- tcpQuery admin "grant reader to bob"
                    withConnection handle $ \bob -> do
                        _ <- tcpLogin bob "bob" "bob-password-1"
                        _ <- tcpQuery bob "use test"
                        allowed <- tcpQuery bob "select name from users"
                        asText (at "status" allowed) `shouldBe` "result"
                        _ <- tcpQuery admin "revoke reader from bob"
                        refused <- tcpQuery bob "select name from users"
                        asText (at "code" refused) `shouldBe` "unauthorized"
                        tcpClosed bob
        it "disconnects every member of a role that was dropped" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \admin -> do
                    _ <- tcpLogin admin "admin" "s3cret"
                    _ <- tcpQuery admin "create user bob identified by 'bob-password-1'"
                    _ <- tcpQuery admin "create role reader"
                    _ <- tcpQuery admin "grant reader to bob"
                    withConnection handle $ \bob -> do
                        _ <- tcpLogin bob "bob" "bob-password-1"
                        _ <- tcpQuery admin "drop role reader"
                        refused <- tcpQuery bob "use test"
                        asText (at "code" refused) `shouldBe` "unauthorized"
                        tcpClosed bob
        it "disconnects an account whose password was reset" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \admin -> do
                    _ <- tcpLogin admin "admin" "s3cret"
                    _ <- tcpQuery admin "create user bob identified by 'bob-password-1'"
                    withConnection handle $ \bob -> do
                        _ <- tcpLogin bob "bob" "bob-password-1"
                        _ <- tcpQuery admin "alter user bob identified by 'bob-password-2'"
                        refused <- tcpQuery bob "use test"
                        asText (at "code" refused) `shouldBe` "unauthorized"
                        tcpClosed bob
                    withConnection handle $ \again -> do
                        relogin <- tcpLogin again "bob" "bob-password-2"
                        asText (at "status" relogin) `shouldBe` "ok"
        it "disconnects an account that was dropped" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \admin -> do
                    _ <- tcpLogin admin "admin" "s3cret"
                    _ <- tcpQuery admin "create user bob identified by 'bob-password-1'"
                    withConnection handle $ \bob -> do
                        _ <- tcpLogin bob "bob" "bob-password-1"
                        _ <- tcpQuery admin "drop user bob"
                        refused <- tcpQuery bob "use test"
                        asText (at "code" refused) `shouldBe` "unauthorized"
                        tcpClosed bob
        it "leaves the connections of untouched accounts alone" $
            withTcpServer defaultServerConfig $ \_ handle ->
                withConnection handle $ \admin -> do
                    _ <- tcpLogin admin "admin" "s3cret"
                    _ <- tcpQuery admin "create user bob identified by 'bob-password-1'"
                    withConnection handle $ \bob -> do
                        _ <- tcpLogin bob "bob" "bob-password-1"
                        _ <- tcpQuery admin "create user carol identified by 'carol-password-1'"
                        _ <- tcpQuery admin "alter user carol identified by 'carol-password-2'"
                        still <- tcpQuery bob "use test"
                        asText (at "status" still) `shouldBe` "result"

-- 权限服务夹具：内存后端上的系统库视角

-- | 一个普通账号
testAccount :: Account
testAccount = Account 0 "alice" "" 0 "" Nothing True False True False

-- | 权限用例传的当前库名
testDatabase :: Text
testDatabase = T.pack testDatabaseName

-- | 一个空的权限服务加管理员身份
withPrivileges :: (Privileges -> Principal -> IO a) -> IO a
withPrivileges body = do
    db <- newMVar testDb
    let backend = memoryBackend testDatabaseName db
    _ <- beAccounts backend (ReqAccountCreate "alice" "hash")
    _ <- beAccounts backend (ReqAccountCreate "bob" "hash")
    service <- newPrivileges backend
    body service (Root testRootName)

-- | 解析一条 SQL，失败就丢掉用例
parseOrFail :: String -> IO Statement
parseOrFail sql = case parseStatement sql of
    Left err -> expectationFailure err >> fail "statement did not parse"
    Right stmt -> pure stmt

-- | 用某个身份鉴权一条 SQL
authorizeSql :: Privileges -> Principal -> String -> IO (Either PrivilegeError ())
authorizeSql service principal sql = parseOrFail sql >>= authorize service principal testDatabase

-- | 权限与目录 API 用例
privilegeSpec :: Spec
privilegeSpec = describe "server privileges" $ do
    it "creates a role and lists it" $ withPrivileges $ \service root -> do
        created <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        created `shouldBe` Right ()
        views <- listRoleViews service
        fmap (map roleName) views `shouldBe` Right ["reader"]

    it "refuses a role that already exists" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        again <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        again `shouldBe` Left (PrivilegeError "conflict" "role already exists: reader")

    it "grants a privilege on a table and shows it in the role view" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        granted <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" False)
        granted `shouldBe` Right ()
        views <- listRoleViews service
        fmap (map roleGrants) views `shouldBe` Right [[Grant "reader" "select" "test.users" False]]

    it "expands ALL and keeps a star object as is" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["all"] "*" "reader" False)
        views <- listRoleViews service
        fmap (concatMap (map grantPrivilege . roleGrants)) views
            `shouldBe` Right ["select", "insert", "update", "delete"]
        fmap (concatMap (map grantObject . roleGrants)) views `shouldBe` Right (replicate 4 "test.*")

    it "revokes a privilege again" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" False)
        _ <- runPrivilegeCommand service root testDatabase (RevokePrivilegesCommand ["select"] "users" "reader")
        views <- listRoleViews service
        fmap (map roleGrants) views `shouldBe` Right [[]]

    it "marks a grant that carries the grant option" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" True)
        views <- listRoleViews service
        fmap (map roleGrants) views `shouldBe` Right [[Grant "reader" "select" "test.users" True]]

    it "refuses a non-administrator without the grant option" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "writer")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" False)
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice"])
        refused <- runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "writer" False)
        refused `shouldBe` Left (PrivilegeError "forbidden" "grant option required: SELECT ON users")

    it "lets a grant option holder pass the privilege on" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "writer")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" True)
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice"])
        passed <- runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "writer" False)
        passed `shouldBe` Right ()
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "writer" ["bob"])
        views <- listRoleViews service
        fmap (map roleGrants) views `shouldBe` Right [[Grant "reader" "select" "test.users" True], [Grant "writer" "select" "test.users" False]]

    it "stops the pass-on once the grant option is revoked" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "writer")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" True)
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice"])
        _ <- runPrivilegeCommand service root testDatabase (RevokePrivilegesCommand ["select"] "users" "reader")
        refused <- runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "writer" False)
        refused `shouldBe` Left (PrivilegeError "forbidden" "grant option required: SELECT ON users")
        views <- listRoleViews service
        fmap (map roleGrants) views `shouldBe` Right [[], []]

    it "adds and removes members" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice"])
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice", "bob"])
        views <- listRoleViews service
        fmap (map roleMembers) views `shouldBe` Right [["alice", "bob"]]
        _ <- runPrivilegeCommand service root testDatabase (RevokeRoleCommand "reader" ["alice"])
        remaining <- listRoleViews service
        fmap (map roleMembers) remaining `shouldBe` Right [["bob"]]

    it "drops a role together with its grants and members" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" False)
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice"])
        dropped <- runPrivilegeCommand service root testDatabase (DropRoleCommand "reader")
        dropped `shouldBe` Right ()
        views <- listRoleViews service
        views `shouldBe` Right []

    it "inherits the grants of another role" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "writer")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" False)
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["writer"])
        linked <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "writer" ["alice"])
        linked `shouldBe` Right ()
        -- 继承链上的授权跟着成员传上来
        authorizeSql service (Ordinary testAccount) "SELECT * FROM users" `shouldReturn` Right ()
        views <- listRoleViews service
        case views of
            Left err -> expectationFailure (show err)
            Right known -> do
                let membersOf name = concat [ms | view <- known, roleName view == name, ms <- [roleMembers view]]
                membersOf "reader" `shouldMatchList` ["writer"]
                membersOf "writer" `shouldMatchList` ["alice"]
        revoked <- runPrivilegeCommand service root testDatabase (RevokeRoleCommand "reader" ["writer"])
        revoked `shouldBe` Right ()
        authorizeSql service (Ordinary testAccount) "SELECT * FROM users"
            `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")

    it "refuses a role membership that would create a cycle" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "writer")
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["writer"])
        loop <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "writer" ["reader"])
        loop `shouldBe` Left (PrivilegeError "conflict" "role membership would create a cycle: writer and reader")
        self <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["reader"])
        self `shouldBe` Left (PrivilegeError "conflict" "role membership would create a cycle: reader and reader")

    it "drops the inherited edge together with the role" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "writer")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" False)
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["writer"])
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "writer" ["alice"])
        dropped <- runPrivilegeCommand service root testDatabase (DropRoleCommand "reader")
        dropped `shouldBe` Right ()
        authorizeSql service (Ordinary testAccount) "SELECT * FROM users"
            `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
        views <- listRoleViews service
        fmap (map roleMembers) views `shouldBe` Right [["alice"]]

    it "refuses a command on a role that does not exist" $ withPrivileges $ \service root -> do
        missing <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "ghost" False)
        missing `shouldBe` Left (PrivilegeError "not_found" "unknown role: ghost")

    it "refuses a table grant without a selected database" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        refused <- runPrivilegeCommand service root "" (GrantPrivilegesCommand ["select"] "users" "reader" False)
        refused `shouldBe` Left (PrivilegeError "no_database" "no database selected")

    it "refuses a non-administrator" $ withPrivileges $ \service _ -> do
        refused <- runPrivilegeCommand service (Ordinary testAccount) testDatabase (CreateRoleCommand "reader")
        refused `shouldBe` Left (PrivilegeError "forbidden" "administrator required")

    it "requires administrator privileges for domain commands" $ withPrivileges $ \service root -> do
        authorizeSql service root "CREATE DOMAIN code AS INT" `shouldReturn` Right ()
        authorizeSql service root "ALTER DOMAIN code SET NOT NULL" `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "ALTER DOMAIN code SET NOT NULL"
            `shouldReturn` Left (PrivilegeError "forbidden" "administrator required")
        authorizeSql service (Ordinary testAccount) "CREATE DOMAIN code AS INT"
            `shouldReturn` Left (PrivilegeError "forbidden" "administrator required")

    it "authorizes the administrator on any statement" $ withPrivileges $ \service root ->
        case parseStatement "SELECT * FROM users" of
            Left err -> expectationFailure err
            Right stmt -> authorize service root testDatabase stmt `shouldReturn` Right ()

    it "requires the privilege on the table inside a derived table" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" False)
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice"])
        authorizeSql service (Ordinary testAccount) "SELECT d.name FROM (SELECT name FROM users) d"
            `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "SELECT d.name FROM (SELECT product FROM orders) d"
            `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON orders")

    it "requires the privilege on the table inside a CTE" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" False)
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice"])
        authorizeSql service (Ordinary testAccount) "WITH d AS (SELECT name FROM users) SELECT d.name FROM d"
            `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "WITH d AS (SELECT product FROM orders) SELECT d.product FROM d"
            `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON orders")

    it "requires the privilege on the table inside a comparison subquery" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" False)
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice"])
        authorizeSql service (Ordinary testAccount) "SELECT name FROM users WHERE id >= (SELECT id FROM users)"
            `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "SELECT name FROM users WHERE id >= (SELECT id FROM orders)"
            `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON orders")

    it "requires the privilege on the table inside a quantifier subquery" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" False)
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice"])
        authorizeSql service (Ordinary testAccount) "SELECT name FROM users WHERE id > ANY (SELECT id FROM users)"
            `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "SELECT name FROM users WHERE id > ANY (SELECT id FROM orders)"
            `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON orders")

    it "requires SELECT on subqueries in HAVING and expression ordering" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" False)
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice"])
        mapM_
            ( \sql ->
                authorizeSql service (Ordinary testAccount) sql
                    `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON orders")
            )
            [ "SELECT COUNT(*) FROM users HAVING COUNT(*) > (SELECT COUNT(*) FROM orders)"
            , "SELECT name FROM users ORDER BY (SELECT COUNT(*) FROM orders) + id"
            ]

    it "lists the accounts a role change touches, members included" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "inner")
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "inner" ["reader"])
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice"])
        directly <- affectedAccounts service (RevokeRoleCommand "inner" ["bob"])
        fmap sort directly `shouldBe` Right ["bob"]
        granted <- affectedAccounts service (GrantRoleCommand "writer" ["inner"])
        fmap sort granted `shouldBe` Right (sort ["inner", "reader", "alice"])
        dropped <- affectedAccounts service (DropRoleCommand "inner")
        fmap sort dropped `shouldBe` Right (sort ["inner", "reader", "alice"])
        affectedAccounts service (CreateRoleCommand "other") `shouldReturn` Right []

    it "keeps the role tables on the real storage" $ withIpcStorage $ do
        base <- ipcBackend
        service <- newPrivileges base
        created <- runPrivilegeCommand service (Root testRootName) testDatabase (CreateRoleCommand "reader")
        created `shouldBe` Right ()
        views <- listRoleViews service
        fmap (map roleName) views `shouldBe` Right ["reader"]
        stored <- beAccounts base ReqAccountsList
        fmap (map (\a -> (accountUser a, accountCanLogin a))) stored `shouldBe` Right [("root", True), ("reader", False)]
        let system = beWithDatabase base "system"
        -- 账号表不给语句通道看到
        denied <- beStatement system "SELECT * FROM __system_users"
        case denied of
            Left _ -> pure ()
            Right result -> expectationFailure ("account table must stay hidden: " ++ show (srRows result))

    it "inherits a role on the real storage" $ withIpcStorage $ do
        base <- ipcBackend
        seedPrivilegeObjects base
        service <- newPrivileges base
        _ <- runPrivilegeCommand service (Root testRootName) testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service (Root testRootName) testDatabase (CreateRoleCommand "writer")
        _ <- runPrivilegeCommand service (Root testRootName) testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" False)
        _ <- runPrivilegeCommand service (Root testRootName) testDatabase (GrantRoleCommand "reader" ["writer"])
        linked <- runPrivilegeCommand service (Root testRootName) testDatabase (GrantRoleCommand "writer" ["alice"])
        linked `shouldBe` Right ()
        authorizeSql service (Ordinary testAccount) "SELECT * FROM users" `shouldReturn` Right ()

    it "keeps a grant option on the real storage" $ withIpcStorage $ do
        base <- ipcBackend
        seedPrivilegeObjects base
        service <- newPrivileges base
        _ <- runPrivilegeCommand service (Root testRootName) testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service (Root testRootName) testDatabase (CreateRoleCommand "writer")
        _ <- runPrivilegeCommand service (Root testRootName) testDatabase (GrantPrivilegesCommand ["select"] "users" "reader" True)
        _ <- runPrivilegeCommand service (Root testRootName) testDatabase (GrantRoleCommand "reader" ["alice"])
        views <- listRoleViews service
        fmap (map roleGrants) views `shouldBe` Right [[Grant "reader" "select" "test.users" True], []]
        passed <- runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "writer" False)
        passed `shouldBe` Right ()
        after <- listRoleViews service
        fmap (map roleGrants) after `shouldBe` Right [[Grant "reader" "select" "test.users" True], [Grant "writer" "select" "test.users" False]]

-- 真存储夹具：一个用例一份数据目录，跑完关链路删干净
-- 创建独立的对象授权内存夹具。
withObjectPrivileges :: (Backend -> Privileges -> Principal -> IO ()) -> IO ()
withObjectPrivileges body = do
    db <- newMVar testDb
    let backend = memoryBackend testDatabaseName db
    mapM_ (\name -> beAccounts backend (ReqAccountCreate name "hash") >>= either (fail . show) (const (pure ()))) ["alice", "bob", "carol"]
    service <- newPrivileges backend
    body backend service (Root testRootName)

-- 验证对象作用域、授权来源和外部入口。
objectPrivilegeSpec :: Spec
objectPrivilegeSpec = describe "stable object privileges" $ do
    it "lists only the caller's effective grants including roles and PUBLIC" $ withObjectPrivileges $ \backend service root -> do
        runPrivilegeCommand service root testDatabase (CreateRoleCommand "self_reader") `shouldReturn` Right ()
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "self_reader" True) `shouldReturn` Right ()
        runPrivilegeCommand service root testDatabase (GrantRoleCommand "self_reader" ["alice"]) `shouldReturn` Right ()
        grants <- listSelfGrants service (Ordinary testAccount)
        grants `shouldSatisfy` either (const False) (elem (Grant "alice" "select" "test.users" True))
        grants `shouldSatisfy` either (const False) (elem (Grant "alice" "connect" "database:test" False))
        grants `shouldSatisfy` either (const False) (all ((== "alice") . grantRole))
        runPrivilegeCommand service root testDatabase (RevokePrivilegesCommand ["select"] "users" "self_reader") `shouldReturn` Right ()
        after <- listSelfGrants service (Ordinary testAccount)
        after `shouldSatisfy` either (const False) (not . any ((== "test.users") . grantObject))
        _ <- beAccounts backend (ReqAccountReset "alice" (testPasswordHash "Alice-Secret-1234"))
        _ <- beAccounts backend (ReqAccountCreate testRootName (testPasswordHash testRootPassword))
        _ <- beAccounts backend (ReqIdentityAlter testRootName Nothing (Just True) Nothing Nothing)
        settings <- tempSettingsPath "self-grants"
        session <- newSession backend testRootName settings
        authenticateSessionCoded session "alice" "Alice-Secret-1234" `shouldReturn` Right ()
        result <- mustSql session "SHOW GRANTS"
        qrColumns result `shouldBe` ["identity", "privilege", "object", "grant_option"]
        qrRows result `shouldSatisfy` all (\row -> take 1 row == [VStr "alice"])
        rejected <- runStatementCoded session "SHOW GRANTS FOR admin"
        rejected `shouldSatisfy` either (const True) (const False)
    it "includes ownership permissions and excludes ungranted tables" $ withObjectPrivileges $ \backend service _ -> do
        mustRun (beStatement (beWithDatabase backend "test") "CREATE TABLE personal_owned (id INT)")
        claimObject service (Ordinary testAccount) testDatabase "personal_owned" `shouldReturn` Right ()
        grants <- listSelfGrants service (Ordinary testAccount)
        fmap (filter ((== "test.personal_owned") . grantObject)) grants
            `shouldBe` Right
                [Grant "alice" privilege "test.personal_owned" True | privilege <- ["select", "insert", "update", "delete"]]
        grants `shouldSatisfy` either (const False) (not . any ((== "test.users") . grantObject))

    it "checks DISTINCT table reads and nested subqueries" $ withObjectPrivileges $ \_ service root -> do
        authorizeSql service (Ordinary testAccount) "SELECT DISTINCT age FROM users"
            `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
        authorizeSql service (Ordinary testAccount) "SELECT DISTINCT d.age FROM (SELECT DISTINCT age FROM users) d"
            `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "alice" False) `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "SELECT DISTINCT age FROM users" `shouldReturn` Right ()

    it "migrates old table grants once and ignores later legacy writes" $ withObjectPrivileges $ \backend service _ -> do
        let system = beWithDatabase backend "system"
        mustRun (beStatement system "CREATE TABLE __system_grants (role VARCHAR(64), privilege VARCHAR(16), object VARCHAR(128))")
        mustRun (beStatement system "INSERT INTO __system_grants (role, privilege, object) VALUES ('alice', 'select', 'test.users')")
        authorizeSql service (Ordinary testAccount) "SELECT * FROM users" `shouldReturn` Right ()
        mustRun (beStatement system "INSERT INTO __system_grants (role, privilege, object) VALUES ('bob', 'select', 'test.users')")
        authorizeSql service (Ordinary testAccount{accountUser = "bob"}) "SELECT * FROM users" `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
    it "recovers a durable creator intent after table creation and cancels absent objects" $ withIpcStorage $ do
        backend <- ipcBackend
        seedPrivilegeObjects backend
        service <- newPrivileges backend
        prepareObject service (Ordinary testAccount) "test" "recovered_owned" `shouldReturn` Right ()
        mustRun (beStatement (beWithDatabase backend "test") "CREATE TABLE recovered_owned (id INT)")
        closeConnection
        dir <- ipcDataDir
        opened <- localStorageLink (Just (dir ++ ".toml"))
        link <- either fail pure opened
        setStorageLink link
        fresh <- ipcBackend >>= newPrivileges
        authorizeSql fresh (Ordinary testAccount) "SELECT * FROM recovered_owned" `shouldReturn` Right ()
        prepareObject fresh (Ordinary testAccount) "test" "never_created" `shouldReturn` Right ()
        authorizeSql fresh (Ordinary testAccount) "SELECT * FROM recovered_owned" `shouldReturn` Right ()
        mustRun (beStatement (beWithDatabase backend "test") "CREATE TABLE never_created (id INT)")
        authorizeSql fresh (Ordinary testAccount) "SELECT * FROM never_created" `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON never_created")
    it "holds authorization through a concurrent query and orders revocation after it" $ withObjectPrivileges $ \backend service root -> do
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "alice" False) `shouldReturn` Right ()
        _ <- beAccounts backend (ReqAccountCreate testRootName (testPasswordHash testRootPassword))
        _ <- beAccounts backend (ReqIdentityAlter testRootName Nothing (Just True) Nothing Nothing)
        _ <- beAccounts backend (ReqAccountReset "alice" (testPasswordHash "Alice-Secret-1234"))
        started <- newEmptyMVar
        release <- newEmptyMVar
        finished <- newEmptyMVar
        revoked <- newEmptyMVar
        let slow =
                backend
                    { beWithDatabase = \name ->
                        (beWithDatabase backend name)
                            { beStatement = \sql -> do
                                when (sql == "SELECT * FROM users") $
                                    putMVar started () >> takeMVar release
                                beStatement (beWithDatabase backend name) sql
                            }
                    }
        settings <- tempSettingsPath "permission-race"
        reader <- newSession slow testRootName settings
        admin <- newSession backend testRootName settings
        authenticateSessionCoded reader "alice" "Alice-Secret-1234" `shouldReturn` Right ()
        authenticateSessionCoded admin testRootName testRootPassword `shouldReturn` Right ()
        _ <- mustSql reader "USE test"
        _ <- mustSql admin "USE test"
        _ <- forkIO (runStatementCoded reader "SELECT * FROM users" >>= putMVar finished)
        takeMVar started
        _ <- forkIO (runStatementCoded admin "REVOKE SELECT ON users FROM alice" >>= putMVar revoked)
        blocked <- timeout 100000 (takeMVar revoked)
        case blocked of Nothing -> pure (); Just _ -> expectationFailure "revocation raced past an authorized query"
        putMVar release ()
        takeMVar finished >>= either (expectationFailure . show) (const (pure ()))
        takeMVar revoked >>= either (expectationFailure . show) (const (pure ()))
        result <- runStatementCoded reader "SELECT * FROM users"
        case result of Left err -> sessCode err `shouldBe` "forbidden"; Right _ -> expectationFailure "revoked privilege remained active"
    it "requires existing objects and validates database privilege scope" $ withObjectPrivileges $ \_ service root -> do
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "missing" "alice" False)
            `shouldReturn` Left (PrivilegeError "not_found" "unknown table")
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "database:test" "alice" False)
            `shouldReturn` Left (PrivilegeError "bad_request" "invalid privilege for object scope")
    it "applies PUBLIC CONNECT defaults and explicit inherited connections" $ withObjectPrivileges $ \_ service root -> do
        authorizeConnect service (Ordinary testAccount) "test" `shouldReturn` Right ()
        runPrivilegeCommand service root "" (RevokePrivilegesCommand ["connect"] "database:test" "public") `shouldReturn` Right ()
        authorizeConnect service (Ordinary testAccount) "test" `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: CONNECT ON database:test")
        runPrivilegeCommand service root "" (CreateRoleCommand "connector") `shouldReturn` Right ()
        runPrivilegeCommand service root "" (GrantPrivilegesCommand ["connect"] "database:test" "connector" False) `shouldReturn` Right ()
        runPrivilegeCommand service root "" (GrantRoleCommand "connector" ["alice"]) `shouldReturn` Right ()
        authorizeConnect service (Ordinary testAccount) "test" `shouldReturn` Right ()
        runPrivilegeCommand service root "" (GrantPrivilegesCommand ["connect"] "database:test" "public" False) `shouldReturn` Right ()
    it "grants CREATE without granting existing table access" $ withObjectPrivileges $ \_ service root -> do
        runPrivilegeCommand service root "" (GrantPrivilegesCommand ["create"] "database:test" "alice" False) `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "CREATE TABLE owned (id INT)" `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "SELECT * FROM users" `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
    it "gives table owners management and delegation without giving database management" $ withObjectPrivileges $ \_ service root -> do
        claimObject service (Ordinary testAccount) testDatabase "users" `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "DROP TABLE users" `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "INSERT INTO users (id) VALUES (10)" `shouldReturn` Right ()
        runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" False) `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "DROP DATABASE test" `shouldReturn` Left (PrivilegeError "forbidden" "object owner required")
        runPrivilegeCommand service root testDatabase (RevokePrivilegesCommand ["select"] "users" "alice") `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "SELECT * FROM users" `shouldReturn` Right ()
    it "executes grant option revocation over TCP and keeps SELECT" $
        withTcpServer defaultServerConfig $ \_ handle -> withConnection handle $ \admin -> do
            _ <- tcpLogin admin "admin" "s3cret"
            _ <- tcpQuery admin "USE test"
            _ <- tcpQuery admin "CREATE USER option_reader IDENTIFIED BY 'Option-Reader-1234'"
            tcpQuery admin "GRANT SELECT ON users TO option_reader WITH GRANT OPTION" >>= \reply -> at "status" reply `shouldBe` A.String "result"
            tcpQuery admin "REVOKE GRANT OPTION FOR SELECT ON users FROM option_reader" >>= \reply -> at "status" reply `shouldBe` A.String "result"
            withConnection handle $ \reader -> do
                _ <- tcpLogin reader "option_reader" "Option-Reader-1234"
                _ <- tcpQuery reader "USE test"
                tcpQuery reader "SELECT DISTINCT age FROM users ORDER BY 1" >>= \reply -> at "status" reply `shouldBe` A.String "result"
                tcpQuery reader "SHOW GRANTS" >>= \reply -> at "status" reply `shouldBe` A.String "result"
                tcpQuery reader "GRANT SELECT ON users TO admin" >>= \reply -> at "code" reply `shouldBe` A.String "forbidden"
    it "revokes grant option while preserving data access and independent sources" $ withObjectPrivileges $ \_ service root -> do
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "alice" True) `shouldReturn` Right ()
        runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" True) `shouldReturn` Right ()
        runPrivilegeCommand service (Ordinary testAccount{accountUser = "bob"}) testDatabase (GrantPrivilegesCommand ["select"] "users" "carol" False) `shouldReturn` Right ()
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "carol" False) `shouldReturn` Right ()
        affectedAccounts service (RevokeGrantOptionCommand ["select"] "users" "alice") `shouldReturn` Right ["alice", "bob", "carol"]
        runPrivilegeCommand service root testDatabase (RevokeGrantOptionCommand ["select"] "users" "alice") `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "SELECT * FROM users" `shouldReturn` Right ()
        grants <- listSelfGrants service (Ordinary testAccount)
        grants `shouldSatisfy` either (const False) (elem (Grant "alice" "select" "test.users" False))
        authorizeSql service (Ordinary testAccount{accountUser = "bob"}) "SELECT * FROM users"
            `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
        authorizeSql service (Ordinary testAccount{accountUser = "carol"}) "SELECT * FROM users" `shouldReturn` Right ()
        runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" False)
            `shouldReturn` Left (PrivilegeError "forbidden" "grant option required: SELECT ON users")
    it "revokes only the caller's grant option source" $ withObjectPrivileges $ \_ service root -> do
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "alice" True) `shouldReturn` Right ()
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" True) `shouldReturn` Right ()
        runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" True) `shouldReturn` Right ()
        runPrivilegeCommand service (Ordinary testAccount) testDatabase (RevokeGrantOptionCommand ["select"] "users" "bob") `shouldReturn` Right ()
        grants <- listSelfGrants service (Ordinary testAccount{accountUser = "bob"})
        grants `shouldSatisfy` either (const False) (elem (Grant "bob" "select" "test.users" True))
    it "cascades multihop delegations after the source grant is revoked" $ withObjectPrivileges $ \_ service root -> do
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "alice" True) `shouldReturn` Right ()
        runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" True) `shouldReturn` Right ()
        let bob = Ordinary testAccount{accountUser = "bob"}
        runPrivilegeCommand service bob testDatabase (GrantPrivilegesCommand ["select"] "users" "carol" False) `shouldReturn` Right ()
        affectedAccounts service (RevokePrivilegesCommand ["select"] "users" "alice") `shouldReturn` Right ["alice", "bob", "carol"]
        runPrivilegeCommand service root testDatabase (RevokePrivilegesCommand ["select"] "users" "alice") `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount{accountUser = "carol"}) "SELECT * FROM users"
            `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
    it "keeps an independent source when another delegation chain is revoked" $ withObjectPrivileges $ \_ service root -> do
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "alice" True) `shouldReturn` Right ()
        runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" False) `shouldReturn` Right ()
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" False) `shouldReturn` Right ()
        runPrivilegeCommand service root testDatabase (RevokePrivilegesCommand ["select"] "users" "alice") `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount{accountUser = "bob"}) "SELECT * FROM users" `shouldReturn` Right ()
    it "removes delegations whose grantor loses an inherited source" $ withObjectPrivileges $ \_ service root -> do
        runPrivilegeCommand service root testDatabase (CreateRoleCommand "delegator") `shouldReturn` Right ()
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "delegator" True) `shouldReturn` Right ()
        runPrivilegeCommand service root testDatabase (GrantRoleCommand "delegator" ["alice"]) `shouldReturn` Right ()
        runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" False) `shouldReturn` Right ()
        runPrivilegeCommand service root testDatabase (RevokeRoleCommand "delegator" ["alice"]) `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount{accountUser = "bob"}) "SELECT * FROM users" `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
    it "does not preserve circular delegation after its last independent root is revoked" $ withObjectPrivileges $ \_ service root -> do
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "alice" True) `shouldReturn` Right ()
        runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" True) `shouldReturn` Right ()
        runPrivilegeCommand service (Ordinary testAccount{accountUser = "bob"}) testDatabase (GrantPrivilegesCommand ["select"] "users" "alice" True) `shouldReturn` Right ()
        runPrivilegeCommand service root testDatabase (RevokePrivilegesCommand ["select"] "users" "alice") `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount{accountUser = "bob"}) "SELECT * FROM users" `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
    it "never transfers a dropped table's grant to a same-name replacement" $ withObjectPrivileges $ \backend service root -> do
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "alice" False) `shouldReturn` Right ()
        mustRun (beStatement backend "DROP TABLE users")
        mustRun (beStatement backend "CREATE TABLE users (id INT)")
        authorizeSql service (Ordinary testAccount) "SELECT * FROM users" `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
    it "does not let disabled grantors keep downstream grants alive" $ withObjectPrivileges $ \backend service root -> do
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "alice" True) `shouldReturn` Right ()
        runPrivilegeCommand service (Ordinary testAccount) testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" False) `shouldReturn` Right ()
        changed <- beAccounts backend (ReqIdentityAlter "alice" Nothing Nothing (Just False) Nothing)
        either expectationFailure (const (pure ())) changed
        authorizeSql service (Ordinary testAccount{accountUser = "bob"}) "SELECT * FROM users" `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
    it "preserves dormant direct grants and ownership across unrelated ACL writes" $ withObjectPrivileges $ \backend service root -> do
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "alice" False) `shouldReturn` Right ()
        mustRun (beStatement backend "CREATE TABLE owned (id INT)")
        claimObject service (Ordinary testAccount) testDatabase "owned" `shouldReturn` Right ()
        changed <- beAccounts backend (ReqIdentityAlter "alice" Nothing Nothing (Just False) Nothing)
        either expectationFailure (const (pure ())) changed
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" False) `shouldReturn` Right ()
        enabled <- beAccounts backend (ReqIdentityAlter "alice" Nothing Nothing (Just True) Nothing)
        either expectationFailure (const (pure ())) enabled
        authorizeSql service (Ordinary testAccount) "SELECT * FROM users" `shouldReturn` Right ()
        authorizeSql service (Ordinary testAccount) "DROP TABLE owned" `shouldReturn` Right ()
    it "keeps same-name tables in separate database scopes and persists the ACL" $ withIpcStorage $ do
        backend <- ipcBackend
        seedPrivilegeObjects backend
        mustRun (beStatement backend "CREATE DATABASE other")
        mustRun (beStatement (beWithDatabase backend "other") "CREATE TABLE users (id INT)")
        service <- newPrivileges backend
        runPrivilegeCommand service (Root testRootName) "test" (GrantPrivilegesCommand ["select"] "users" "alice" False) `shouldReturn` Right ()
        statement <- parseOrFail "SELECT * FROM users"
        authorize service (Ordinary testAccount) "other" statement `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
        closeConnection
        dir <- ipcDataDir
        opened <- localStorageLink (Just (dir ++ ".toml"))
        link <- either fail pure opened
        setStorageLink link
        fresh <- ipcBackend >>= newPrivileges
        authorize fresh (Ordinary testAccount) "test" statement `shouldReturn` Right ()
        authorize fresh (Ordinary testAccount) "other" statement `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
    it "checks CONNECT and CREATE through TCP and assigns ownership to the creator" $
        withTcpServer defaultServerConfig $ \_ handle -> withConnection handle $ \admin -> do
            _ <- tcpLogin admin "admin" "s3cret"
            _ <- tcpQuery admin "CREATE USER object_owner IDENTIFIED BY 'Owner-Secret-1234'"
            tcpQuery admin "REVOKE CONNECT ON DATABASE test FROM PUBLIC" >>= \reply -> at "status" reply `shouldBe` A.String "result"
            withConnection handle $ \client -> do
                _ <- tcpLogin client "object_owner" "Owner-Secret-1234"
                tcpQuery client "USE test" >>= \reply -> at "code" reply `shouldBe` A.String "forbidden"
            _ <- tcpQuery admin "GRANT CONNECT, CREATE ON DATABASE test TO object_owner"
            withConnection handle $ \client -> do
                _ <- tcpLogin client "object_owner" "Owner-Secret-1234"
                tcpQuery client "USE test" >>= \reply -> at "status" reply `shouldBe` A.String "result"
                tcpQuery client "CREATE TABLE owned_probe (id INT)" >>= \reply -> at "status" reply `shouldBe` A.String "result"
                tcpQuery client "INSERT INTO owned_probe (id) VALUES (1)" >>= \reply -> at "status" reply `shouldBe` A.String "result"
                tcpQuery client "SELECT * FROM users" >>= \reply -> at "code" reply `shouldBe` A.String "forbidden"
                tcpTalk client "{\"method\":\"storage\",\"request\":{\"method\":\"scan\",\"database\":\"test\",\"table\":\"users\"}}" >>= \reply -> at "code" reply `shouldBe` A.String "forbidden"
    it "disconnects downstream TCP grantees and protects staged commits after revocation" $
        withTcpServer defaultServerConfig $ \_ handle -> withConnection handle $ \admin -> do
            _ <- tcpLogin admin "admin" "s3cret"
            _ <- tcpQuery admin "USE test"
            _ <- tcpQuery admin "CREATE USER source_user IDENTIFIED BY 'Source-Secret-1234'"
            _ <- tcpQuery admin "CREATE USER target_user IDENTIFIED BY 'Target-Secret-1234'"
            _ <- tcpQuery admin "GRANT SELECT, UPDATE ON users TO source_user WITH GRANT OPTION"
            withConnection handle $ \source -> do
                _ <- tcpLogin source "source_user" "Source-Secret-1234"
                _ <- tcpQuery source "USE test"
                _ <- tcpQuery source "GRANT SELECT, UPDATE ON users TO target_user"
                withConnection handle $ \target -> do
                    _ <- tcpLogin target "target_user" "Target-Secret-1234"
                    _ <- tcpQuery target "USE test"
                    _ <- tcpQuery target "BEGIN"
                    _ <- tcpQuery target "UPDATE users SET age = 90 WHERE id = 1"
                    _ <- tcpQuery admin "REVOKE UPDATE ON users FROM source_user"
                    tcpQuery target "COMMIT" >>= \reply -> at "code" reply `shouldBe` A.String "unauthorized"
                    tcpClosed target
                    reply <- tcpQuery admin "SELECT age FROM users WHERE id = 1"
                    map items (items (at "rows" reply)) `shouldBe` [[A.Number 21]]
    it "rejects a stale local transaction after a granted object is replaced" $ withObjectPrivileges $ \backend service root -> do
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["all"] "users" "alice" False) `shouldReturn` Right ()
        _ <- beAccounts backend (ReqAccountReset "alice" (testPasswordHash "Alice-Secret-1234"))
        settings <- tempSettingsPath "object-transaction"
        _ <- beAccounts backend (ReqAccountCreate testRootName (testPasswordHash testRootPassword))
        _ <- beAccounts backend (ReqIdentityAlter testRootName Nothing (Just True) Nothing Nothing)
        session <- newSession backend testRootName settings
        authenticateSessionCoded session "alice" "Alice-Secret-1234" `shouldReturn` Right ()
        _ <- mustSql session "USE test"
        _ <- mustSql session "BEGIN"
        _ <- mustSql session "UPDATE users SET age = 90 WHERE id = 1"
        mustRun (beStatement backend "DROP TABLE users")
        mustRun (beStatement backend "CREATE TABLE users (id INT, name VARCHAR(32), age INT)")
        result <- runStatementCoded session "COMMIT"
        case result of Left err -> sessCode err `shouldBe` "forbidden"; Right _ -> expectationFailure "stale transaction committed"
    it "checks the current administrator identity before executing a raw storage request" $ withObjectPrivileges $ \backend _ _ -> do
        _ <- beAccounts backend (ReqAccountCreate testRootName (testPasswordHash testRootPassword))
        _ <- beAccounts backend (ReqIdentityAlter testRootName Nothing (Just True) Nothing Nothing)
        _ <- beAccounts backend (ReqAccountCreate "spare_admin" "hash")
        _ <- beAccounts backend (ReqIdentityAlter "spare_admin" Nothing (Just True) Nothing Nothing)
        executed <- newIORef False
        let guarded = backend{beStorage = \_ -> writeIORef executed True >> pure (Right A.Null)}
        settings <- tempSettingsPath "raw-storage-identity"
        session <- newSession guarded testRootName settings
        authenticateSessionCoded session testRootName testRootPassword `shouldReturn` Right ()
        changed <- beAccounts backend (ReqIdentityAlter testRootName Nothing (Just False) Nothing Nothing)
        either expectationFailure (const (pure ())) changed
        result <- runStorageCoded session (A.object ["method" A..= ("ping" :: Text)])
        case result of Left err -> sessCode err `shouldBe` "unauthorized"; Right _ -> expectationFailure "stale administrator executed storage request"
        readIORef executed `shouldReturn` False

-- 为权限测试创建真实对象和登录身份。
seedPrivilegeObjects :: Backend -> IO ()
seedPrivilegeObjects backend = do
    mustRun (beStatement backend "CREATE DATABASE test")
    mustRun (beStatement (beWithDatabase backend "test") "CREATE TABLE users (id INT)")
    result <- beAccounts backend (ReqAccountCreate "alice" "hash")
    case result of Left err -> expectationFailure err; Right _ -> pure ()

-- | 本次用例的数据目录（用例串行跑，固定名够用）
ipcDataDir :: IO FilePath
ipcDataDir = do
    tmp <- getTemporaryDirectory
    pure (tmp </> "chusql-server-test-ipc")

-- | 写一份只属于这次用例的存储配置：数据目录与日志级别都从文件走
writeIpcConfig :: FilePath -> IO FilePath
writeIpcConfig dir = do
    let cfgPath = dir ++ ".toml"
        -- TOML 的普通字符串会吃反斜杠，路径统一用正斜杠
        slashed = map (\c -> if c == '\\' then '/' else c) dir
    writeFile
        cfgPath
        ( unlines
            [ "[storage]"
            , "data_dir = \"" ++ slashed ++ "\""
            , "[log]"
            , "level = \"error\""
            ]
        )
    pure cfgPath

-- | 删掉这次用例的数据目录与配置，删不掉也不算错
cleanIpc :: FilePath -> IO ()
cleanIpc dir = do
    _ <- try (removePathForcibly dir) :: IO (Either IOException ())
    _ <- try (removePathForcibly (dir ++ ".toml")) :: IO (Either IOException ())
    pure ()

-- 事务用例：内存后端跑得快，真存储的提交再在 IPC 用例里验一遍

-- | 起一个内存后端会话，当前库已切到 test
withMemorySession :: (Session -> IO a) -> IO a
withMemorySession body = do
    db <- newMVar testDb
    settings <- tempSettingsPath "transaction"
    removeIfExists settings
    session <- newSession (memoryBackend testDatabaseName db) (T.pack testDatabaseName) settings
    _ <- runStatementCoded session "use test"
    body session

-- | 两个会话共用一份内存库，用来验会话隔离
withTwoSessions :: (Session -> Session -> IO a) -> IO a
withTwoSessions body = do
    db <- newMVar testDb
    settings <- tempSettingsPath "transaction-pair"
    removeIfExists settings
    let backend = memoryBackend testDatabaseName db
    first <- newSession backend (T.pack testDatabaseName) settings
    second <- newSession backend (T.pack testDatabaseName) settings
    _ <- runStatementCoded first "use test"
    _ <- runStatementCoded second "use test"
    body first second

-- | 跑一条语句；出错就算用例失败
mustSql :: Session -> Text -> IO QueryResult
mustSql session sql = runStatementCoded session sql >>= either failed pure
  where
    failed err = expectationFailure (T.unpack (sessMessage err)) >> pure (QueryResult [] [] 0 False Nothing)

-- | 取 users 的 id 列
sessionIds :: Session -> IO [Int]
sessionIds session = do
    result <- mustSql session "select * from users"
    pure [n | row <- qrRows result, Just (VInt n) <- [nth 0 row]]

-- | 取某个 id 的 age
sessionAge :: Session -> Int -> IO (Maybe Int)
sessionAge session wanted = do
    result <- mustSql session "select * from users"
    let ages = [(n, a) | row <- qrRows result, Just (VInt n) <- [nth 0 row], Just (VInt a) <- [nth 2 row]]
    pure (lookup wanted ages)

-- | 取失败里的文案
failureMessage :: Either SessionError QueryResult -> Text
failureMessage = either sessMessage (const "")

-- | 取失败里的错误码
failureCode :: Either SessionError QueryResult -> Text
failureCode = either sessCode (const "")

-- | 取结果第一列的整数
intValues :: [[Value]] -> [Int]
intValues rows = [n | row <- rows, Just (VInt n) <- [nth 0 row]]

-- 数据库执行门的并行、等待与取消回归。
databaseGateSpec :: Spec
databaseGateSpec = describe "database execution gates" $ do
    it "lets another database write while preserving same-database exclusion" $ do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        done <- newEmptyMVar
        bracket (forkFinally (databaseSecurity [("gate_a", True)] (putMVar entered () >> takeMVar release)) (putMVar done)) killThread $ \_ -> do
            takeMVar entered
            timeout 1000000 (databaseSecurity [("gate_b", True)] (pure ())) `shouldReturn` Just ()
            timeout 50000 (databaseSecurity [("gate_a", False)] (pure ())) `shouldReturn` Nothing
            timeout 50000 (writeSecurity (pure ())) `shouldReturn` Nothing
            putMVar release ()
            result <- timeout 1000000 (takeMVar done)
            case result of Just (Right ()) -> pure (); _ -> expectationFailure "database writer failed to finish"
        timeout 1000000 (databaseSecurity [("gate_a", True)] (pure ())) `shouldReturn` Just ()
        timeout 1000000 (writeSecurity (pure ())) `shouldReturn` Just ()

    it "orders opposite dependency lists and releases cancelled waiters" $ do
        entered <- newEmptyMVar
        release <- newEmptyMVar
        done <- newEmptyMVar
        bracket (forkFinally (databaseSecurity [("gate_y", True), ("gate_x", False)] (putMVar entered () >> takeMVar release)) (putMVar done)) killThread $ \_ -> do
            takeMVar entered
            timeout 50000 (databaseSecurity [("gate_x", True), ("gate_y", False)] (pure ())) `shouldReturn` Nothing
            putMVar release ()
            result <- timeout 1000000 (takeMVar done)
            case result of Just (Right ()) -> pure (); _ -> expectationFailure "ordered writer failed to finish"
        timeout 1000000 (databaseSecurity [("gate_x", True), ("gate_y", True)] (pure ())) `shouldReturn` Just ()

-- | 显式事务用例
transactionSpec :: Spec
transactionSpec = describe "server transactions" $ do
    it "previews legacy duplicate deletion exactly as storage does" $ do
        let old = Table "legacy" [("id", TInt), ("name", TStr)] [[("id", VInt 1), ("name", VStr "first")], [("id", VInt 1), ("name", VStr "second")]] Nothing Nothing Nothing
        case applyTransactionMemory [TxnDelete "legacy" [1, 1]] [("legacy", old)] of
            Left err -> expectationFailure err
            Right db -> fmap tableRows (lookup "legacy" db) `shouldBe` Just [[("id", VInt 1), ("name", VStr "second")]]

    it "rechecks UNIQUE on merged rows before publishing any table" $ withTwoSessions $ \first second -> do
        _ <- mustSql first "CREATE TABLE registrations (id INT PRIMARY KEY, username TEXT UNIQUE)"
        _ <- mustSql first "BEGIN"
        _ <- mustSql second "BEGIN"
        _ <- mustSql first "INSERT INTO registrations (id, username) VALUES (1, 'same')"
        _ <- mustSql second "INSERT INTO registrations (id, username) VALUES (2, 'same')"
        _ <- mustSql second "UPDATE users SET age = 99 WHERE id = 1"
        _ <- mustSql first "COMMIT"
        denied <- runStatementCoded second "COMMIT"
        failureCode denied `shouldBe` "serialization_failure"
        failureMessage denied `shouldBe` "UNIQUE: duplicate value in column username"
        _ <- mustSql second "ROLLBACK"
        sessionAge first 1 `shouldReturn` Just 21
        rows <- mustSql first "SELECT id FROM registrations"
        qrRows rows `shouldBe` [[VInt 1]]

    it "allows distinct unique values and multiple NULLs after merging" $ withTwoSessions $ \first second -> do
        _ <- mustSql first "CREATE TABLE registrations (id INT PRIMARY KEY, username TEXT UNIQUE)"
        forM_ [first, second] $ \session -> void (mustSql session "BEGIN")
        _ <- mustSql first "INSERT INTO registrations (id, username) VALUES (1, 'one'), (3, NULL)"
        _ <- mustSql second "INSERT INTO registrations (id, username) VALUES (2, 'two'), (4, NULL)"
        _ <- mustSql first "COMMIT"
        _ <- mustSql second "COMMIT"
        rows <- mustSql first "SELECT COUNT(*) FROM registrations"
        qrRows rows `shouldBe` [[VInt 4]]

    forM_ ["SELECT id FROM test.users WHERE id = 1", "SELECT id FROM users WHERE EXISTS (SELECT id FROM users WHERE age > 0)"] $ \readSql ->
        it ("protects serializable read dependencies: " ++ T.unpack readSql) $ withTwoSessions $ \first second -> do
            _ <- mustSql first "CREATE TABLE actions (id INT PRIMARY KEY)"
            _ <- mustSql first "BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE"
            _ <- mustSql first readSql
            _ <- mustSql first "INSERT INTO actions (id) VALUES (1)"
            _ <- mustSql second "INSERT INTO users (id, name, age) VALUES (7, 'new', 20)"
            denied <- runStatementCoded first "COMMIT"
            failureCode denied `shouldBe` "serialization_failure"
            _ <- mustSql first "ROLLBACK"
            rows <- mustSql second "SELECT COUNT(*) FROM actions"
            qrRows rows `shouldBe` [[VInt 0]]

    it "forgets rolled-back reads and ignores unrelated table changes" $ withTwoSessions $ \first second -> do
        _ <- mustSql first "CREATE TABLE actions (id INT PRIMARY KEY)"
        _ <- mustSql first "START TRANSACTION ISOLATION LEVEL SERIALIZABLE"
        _ <- mustSql first "SAVEPOINT before_read"
        _ <- mustSql first "SELECT * FROM users"
        _ <- mustSql first "ROLLBACK TO before_read"
        _ <- mustSql first "INSERT INTO actions (id) VALUES (1)"
        _ <- mustSql second "UPDATE users SET age = 99 WHERE id = 1"
        _ <- mustSql first "COMMIT"
        rows <- mustSql second "SELECT COUNT(*) FROM actions"
        qrRows rows `shouldBe` [[VInt 1]]

    it "allows a serializable read-only snapshot to finish" $ withTwoSessions $ \first second -> do
        _ <- mustSql first "BEGIN ISOLATION LEVEL SERIALIZABLE"
        _ <- mustSql first "SELECT * FROM users"
        _ <- mustSql second "UPDATE users SET age = 99 WHERE id = 1"
        void (mustSql first "COMMIT")

    forM_ [("another commit", True), ("an autocommit update", False)] $ \(label, transactional) ->
        it ("excludes " ++ label ++ " between conflict checking and publication") $ do
            db <- newMVar testDb
            settings <- tempSettingsPath "transaction-publication"
            removeIfExists settings
            armed <- newIORef False
            snapshots <- newIORef (0 :: Int)
            captured <- newEmptyMVar
            release <- newEmptyMVar
            secondSnapshot <- newEmptyMVar
            let
                -- 在首次提交取得快照后暂停发布。
                pausedBackend original =
                    original
                        { beWithDatabase = pausedBackend . beWithDatabase original
                        , beStatement = \sql -> do
                            enabled <- readIORef armed
                            when (enabled && not transactional && sql == "UPDATE users SET age = 32 WHERE id = 1") (putMVar secondSnapshot ())
                            beStatement original sql
                        , beSnapshot = do
                            result <- beSnapshot original
                            enabled <- readIORef armed
                            when enabled $ do
                                n <- atomicModifyIORef' snapshots (\count -> (count + 1, count))
                                when (n == 0) (putMVar captured () >> takeMVar release)
                                when (n == 1) (putMVar secondSnapshot ())
                            pure result
                        }
                backend = pausedBackend (memoryBackend testDatabaseName db)
            first <- newSession backend (T.pack testDatabaseName) settings
            second <- newSession backend (T.pack testDatabaseName) settings
            mapM_ (`mustSql` "USE test") [first, second]
            _ <- mustSql first "BEGIN"
            when transactional $ do
                _ <- mustSql second "BEGIN"
                void (mustSql second "UPDATE users SET age = 32 WHERE id = 1")
            _ <- mustSql first "UPDATE users SET age = 31 WHERE id = 1"
            writeIORef armed True
            firstDone <- newEmptyMVar
            secondDone <- newEmptyMVar
            bracket (forkFinally (runStatementCoded first "COMMIT") (putMVar firstDone)) killThread $ \_ -> do
                timeout 2000000 (takeMVar captured) `shouldReturn` Just ()
                bracket (forkFinally (runStatementCoded second (if transactional then "COMMIT" else "UPDATE users SET age = 32 WHERE id = 1")) (putMVar secondDone)) killThread $ \_ -> do
                    entered <- timeout 200000 (takeMVar secondSnapshot)
                    putMVar release ()
                    firstResult <- timeout 2000000 (takeMVar firstDone)
                    secondResult <- timeout 2000000 (takeMVar secondDone)
                    entered `shouldBe` Nothing
                    case firstResult of
                        Just (Right result) -> failureCode result `shouldBe` ""
                        _ -> expectationFailure "first commit did not complete"
                    case secondResult of
                        Just (Right result) -> failureCode result `shouldBe` (if transactional then "serialization_failure" else "")
                        _ -> expectationFailure "second commit did not complete"
            when transactional $ void (mustSql second "ROLLBACK")
            sessionAge first 1 `shouldReturn` Just (if transactional then 31 else 32)

    it "executes DISTINCT against the connection snapshot with output columns" $ withMemorySession $ \session -> do
        _ <- mustSql session "CREATE TABLE distinct_values (id INT, value INT)"
        _ <- mustSql session "INSERT INTO distinct_values (id,value) VALUES (1,7),(2,7),(3,8)"
        _ <- mustSql session "BEGIN"
        _ <- mustSql session "UPDATE distinct_values SET value = 9 WHERE id = 3"
        result <- mustSql session "SELECT DISTINCT value FROM distinct_values ORDER BY 1"
        qrColumns result `shouldBe` ["value"]
        qrRows result `shouldBe` [[VInt 7], [VInt 9]]
        _ <- mustSql session "ROLLBACK"
        pure ()

    it "preserves concurrent changes in a large untouched table" $ withTwoSessions $ \first second -> do
        _ <- mustSql first "CREATE TABLE untouched (id INT, value INT)"
        let values = T.intercalate "," [T.pack ("(" ++ show key ++ ",0)") | key <- [1 .. 5000 :: Int]]
        _ <- mustSql first ("INSERT INTO untouched (id,value) VALUES " <> values)
        _ <- mustSql first "BEGIN"
        _ <- mustSql first "UPDATE users SET age = 77 WHERE id = 1"
        _ <- mustSql second "UPDATE untouched SET value = 9 WHERE id = 5000"
        _ <- mustSql first "COMMIT"
        sessionAge first 1 `shouldReturn` Just 77
        result <- mustSql first "SELECT value FROM untouched WHERE id = 5000"
        qrRows result `shouldBe` [[VInt 9]]

    it "preserves staged row order for unsorted integer ids" $ withMemorySession $ \session -> do
        _ <- mustSql session "CREATE TABLE keys (id INT, value INT)"
        _ <- mustSql session "INSERT INTO keys (id,value) VALUES (7,0),(-3,0),(4,0)"
        _ <- mustSql session "BEGIN"
        _ <- mustSql session "UPDATE keys SET value = 1"
        _ <- mustSql session "COMMIT"
        result <- mustSql session "SELECT id FROM keys"
        qrRows result `shouldBe` [[VInt 7], [VInt (-3)], [VInt 4]]

    it "filters qualified memory tables and preserves other databases on writes" $ do
        let local = Table "users" [("id", TInt)] [[("id", VInt 1)]] Nothing Nothing Nothing
            otherTable = Table "users" [("id", TInt)] [[("id", VInt 2)]] Nothing Nothing Nothing
        ref <- newMVar [("test.users", local), ("other.users", otherTable)]
        let backend = memoryBackend "test" ref
        selected <- beStatement backend "SELECT id FROM users"
        fmap srRows selected `shouldBe` Right [[("id", VInt 1)]]
        tables <- beCatalog backend
        fmap (map tiTable) tables `shouldBe` Right ["users"]
        updated <- beStatement backend "UPDATE users SET id = 3"
        void updated `shouldBe` Right ()
        snapshot <- beSnapshot (memoryBackend "other" ref)
        fmap (map (tableRows . snd)) snapshot `shouldBe` Right [[[("id", VInt 2)]]]
    it "keeps catalog and snapshot identities stable across system views and replacements" $ do
        ref <- newMVar [("test.users", Table "users" [("id", TInt)] [] Nothing Nothing Nothing)]
        let backend = memoryBackend "test" ref
        first <- beCatalog backend
        fmap (map tiIdentity) first `shouldBe` Right [Just (TableIdentity 1 1)]
        _ <- beSnapshot (beWithDatabase backend "system")
        saved <- beSnapshot backend
        fmap (map (tableIdentity . snd)) saved `shouldBe` Right [Just (TableIdentity 1 1)]
        mustRun (beStatement backend "DROP TABLE users")
        mustRun (beStatement backend "CREATE TABLE users (id INT)")
        replaced <- beCatalog backend
        fmap (map tiIdentity) replaced `shouldBe` Right [Just (TableIdentity 1 2)]
    it "preserves memory schema versions in catalogs and snapshots" $ do
        ref <- newMVar []
        let backend = memoryBackend "test" ref
        void <$> beStatement backend "CREATE TABLE versioned (id INT)" `shouldReturn` Right ()
        first <- beCatalog backend
        fmap (map tiSchemaVersion) first `shouldBe` Right [Just 1]
        void <$> beStatement backend "ALTER TABLE versioned ADD COLUMN extra TEXT" `shouldReturn` Right ()
        void <$> beStatement backend "ALTER TABLE versioned DROP COLUMN extra" `shouldReturn` Right ()
        current <- beCatalog backend
        snapshot <- beSnapshot backend
        fmap (map tiSchemaVersion) current `shouldBe` Right [Just 3]
        fmap (map (tableSchemaVersion . snd)) snapshot `shouldBe` Right [Just 3]
    it "rejects cross-database reads and writes including nested queries" $ withMemorySession $ \session -> do
        mapM_
            ( \query -> do
                result <- runStatementCoded session query
                failureMessage result `shouldSatisfy` T.isInfixOf "outside the current database"
            )
            [ "SELECT DISTINCT age FROM other.users"
            , "SELECT * FROM other.users"
            , "UPDATE other.users SET age = 1"
            , "DELETE FROM other.users"
            , "INSERT INTO other.users (id) VALUES (99)"
            , "SELECT * FROM users WHERE id IN (SELECT id FROM other.users)"
            , "SELECT other.users.id FROM users"
            ]
        void (mustSql session "BEGIN")
        result <- runStatementCoded session "SELECT * FROM other.users"
        failureMessage result `shouldSatisfy` T.isInfixOf "outside the current database"
        void (mustSql session "ROLLBACK")
    it "redacts malformed password statements through the session" $ withMemorySession $ \session -> do
        result <- runStatementCoded session "ALTER USER alice IDENTIFIED BY 'private-secret' trailing"
        failureMessage result `shouldSatisfy` T.isInfixOf "syntax"
        failureMessage result `shouldSatisfy` (not . T.isInfixOf "private-secret")
    it "resolves current database qualifiers for memory DDL" $ withMemorySession $ \session -> do
        _ <- mustSql session "CREATE TABLE test.qualified (id INT)"
        _ <- mustSql session "ALTER TABLE test.qualified ADD COLUMN value INT DEFAULT 8"
        _ <- mustSql session "INSERT INTO qualified (id) VALUES (1)"
        rows <- mustSql session "SELECT value FROM test.qualified"
        intValues (qrRows rows) `shouldBe` [8]
        void (mustSql session "DROP TABLE test.qualified")

    it "uses domain columns in snapshot transactions and rejects domain DDL" $ withMemorySession $ \session -> do
        _ <- mustSql session "create domain code as int"
        _ <- mustSql session "create table typed (id code)"
        _ <- mustSql session "begin"
        alterDenied <- runStatementCoded session "alter domain code set not null"
        failureMessage alterDenied `shouldBe` "DDL is not allowed in a transaction"
        denied <- runStatementCoded session "drop domain code"
        failureMessage denied `shouldBe` "DDL is not allowed in a transaction"
        _ <- mustSql session "insert into typed (id) values (7)"
        _ <- mustSql session "commit"
        rows <- mustSql session "select id from typed"
        intValues (qrRows rows) `shouldBe` [7]

    it "rejects stale transaction writes after a domain constraint changes" $ withTwoSessions $ \writer administrator -> do
        void (mustSql administrator "CREATE DOMAIN quantity AS INT")
        void (mustSql administrator "CREATE DOMAIN nested AS quantity")
        void (mustSql administrator "CREATE TABLE domain_items (id INT, qty nested)")
        void (mustSql writer "BEGIN")
        void (mustSql writer "INSERT INTO domain_items (id, qty) VALUES (1, 0)")
        void (mustSql administrator "ALTER DOMAIN quantity ADD CHECK (VALUE > 0)")
        refused <- runStatementCoded writer "COMMIT"
        failureCode refused `shouldBe` "serialization_failure"
        void (mustSql writer "ROLLBACK")
        rows <- mustSql administrator "SELECT qty FROM domain_items"
        qrRows rows `shouldBe` []
        void (mustSql writer "BEGIN")
        void (mustSql writer "INSERT INTO domain_items (id, qty) VALUES (2, 2)")
        void (mustSql writer "COMMIT")
        saved <- mustSql administrator "SELECT qty FROM domain_items"
        intValues (qrRows saved) `shouldBe` [2]

    it "keeps uncommitted rows invisible to other sessions until COMMIT" $ withTwoSessions $ \writer reader -> do
        _ <- mustSql writer "begin"
        _ <- mustSql writer "insert into users (id, name, age) values (6, 'six', 26)"
        sessionIds writer `shouldReturn` [1, 2, 3, 4, 5, 6]
        sessionIds reader `shouldReturn` [1, 2, 3, 4, 5]
        _ <- mustSql writer "commit"
        ids <- sessionIds writer
        ids `shouldMatchList` [1, 2, 3, 4, 5, 6]
        seen <- sessionIds reader
        seen `shouldMatchList` [1, 2, 3, 4, 5, 6]

    it "drops every change on ROLLBACK" $ withMemorySession $ \session -> do
        _ <- mustSql session "begin"
        _ <- mustSql session "insert into users (id, name, age) values (6, 'six', 26)"
        _ <- mustSql session "update users set age = 99 where id = 1"
        _ <- mustSql session "delete from users where id = 2"
        rollback <- mustSql session "rollback"
        qrRowCount rollback `shouldBe` 0
        kept <- sessionIds session
        kept `shouldMatchList` [1, 2, 3, 4, 5]

    it "commits the whole write set at once" $ withMemorySession $ \session -> do
        _ <- mustSql session "begin"
        _ <- mustSql session "insert into users (id, name, age) values (6, 'six', 26)"
        _ <- mustSql session "update users set age = 99 where id = 1"
        _ <- mustSql session "delete from users where id = 2"
        _ <- mustSql session "commit"
        ids <- sessionIds session
        ids `shouldMatchList` [1, 3, 4, 5, 6]
        sessionAge session 1 `shouldReturn` Just 99

    it "rejects DDL inside a transaction and takes it after ROLLBACK" $ withMemorySession $ \session -> do
        _ <- mustSql session "begin"
        denied <- runStatementCoded session "create table blocked (id int)"
        failureMessage denied `shouldBe` "DDL is not allowed in a transaction"
        _ <- mustSql session "rollback"
        _ <- mustSql session "create table fresh (id int)"
        _ <- mustSql session "insert into fresh (id) values (1)"
        rows <- mustSql session "select * from fresh"
        qrRowCount rows `shouldBe` 1

    it "does not start a transaction when its snapshot cannot be read" $ do
        ref <- newMVar testDb
        settings <- tempSettingsPath "snapshot-failure"
        let base = memoryBackend testDatabaseName ref
            -- \| 保留测试后端并让快照读取失败
            failing name =
                (beWithDatabase base name)
                    { beSnapshot = pure (Left "data page unreadable")
                    , beWithDatabase = failing
                    }
        session <- newSession (failing testDatabaseName) (T.pack testDatabaseName) settings
        void (mustSql session "USE test")
        result <- runStatementCoded session "BEGIN"
        failureMessage result `shouldBe` "data page unreadable"
        late <- runStatementCoded session "COMMIT"
        failureMessage late `shouldBe` "no transaction in progress"

    it "refuses nested BEGIN and COMMIT outside a transaction" $ withMemorySession $ \session -> do
        _ <- mustSql session "begin"
        nested <- runStatementCoded session "begin"
        failureMessage nested `shouldBe` "already in a transaction"
        _ <- mustSql session "rollback"
        late <- runStatementCoded session "commit"
        failureMessage late `shouldBe` "no transaction in progress"

    it "rejects UPDATE constraints without changing or ending the transaction" $ withMemorySession $ \session -> do
        _ <- mustSql session "CREATE TABLE constrained_updates (id INT PRIMARY KEY, qty INT NOT NULL UNIQUE)"
        _ <- mustSql session "INSERT INTO constrained_updates (id, qty) VALUES (1, 2), (2, 3)"
        _ <- mustSql session "BEGIN"
        mapM_
            ( \(query, marker) -> do
                result <- runStatementCoded session query
                failureMessage result `shouldSatisfy` T.isInfixOf marker
            )
            [ ("UPDATE constrained_updates SET qty = NULL WHERE id = 1", "NOT NULL")
            , ("UPDATE constrained_updates SET qty = 3 WHERE id = 1", "UNIQUE")
            , ("UPDATE constrained_updates SET id = 2 WHERE id = 1", "UNIQUE")
            ]
        unchanged <- mustSql session "SELECT qty FROM constrained_updates ORDER BY id"
        intValues (qrRows unchanged) `shouldBe` [2, 3]
        _ <- mustSql session "UPDATE constrained_updates SET qty = qty + 1"
        _ <- mustSql session "COMMIT"
        committed <- mustSql session "SELECT qty FROM constrained_updates ORDER BY id"
        intValues (qrRows committed) `shouldBe` [3, 4]

    it "does not switch database inside a transaction" $ withMemorySession $ \session -> do
        _ <- mustSql session "begin"
        denied <- runStatementCoded session "use other"
        failureMessage denied `shouldBe` "cannot switch database inside a transaction"

    it "refuses COMMIT when another session changed the same row" $ withTwoSessions $ \first second -> do
        _ <- mustSql first "begin"
        _ <- mustSql second "begin"
        _ <- mustSql first "update users set age = 31 where id = 1"
        _ <- mustSql second "update users set age = 42 where id = 1"
        _ <- mustSql first "commit"
        denied <- runStatementCoded second "commit"
        failureCode denied `shouldBe` "serialization_failure"
        failureMessage denied `shouldBe` "write conflict on table users, id 1"
        -- 事务保留：回滚之后看到的还是先提交的那个值
        _ <- mustSql second "rollback"
        sessionAge second 1 `shouldReturn` Just 31

    it "refuses COMMIT when the row was deleted meanwhile" $ withTwoSessions $ \first second -> do
        _ <- mustSql first "begin"
        _ <- mustSql second "begin"
        _ <- mustSql first "delete from users where id = 2"
        _ <- mustSql second "update users set age = 42 where id = 2"
        _ <- mustSql first "commit"
        denied <- runStatementCoded second "commit"
        failureMessage denied `shouldBe` "write conflict on table users, id 2"
        _ <- mustSql second "rollback"
        ids <- sessionIds second
        ids `shouldMatchList` [1, 3, 4, 5]

    it "refuses COMMIT when deleting a row that changed meanwhile" $ withTwoSessions $ \first second -> do
        _ <- mustSql first "begin"
        _ <- mustSql second "begin"
        _ <- mustSql first "update users set age = 31 where id = 3"
        _ <- mustSql second "delete from users where id = 3"
        _ <- mustSql first "commit"
        denied <- runStatementCoded second "commit"
        failureMessage denied `shouldBe` "write conflict on table users, id 3"
        _ <- mustSql second "rollback"
        sessionAge second 3 `shouldReturn` Just 31

    it "refuses COMMIT when another session inserted the same id" $ withTwoSessions $ \first second -> do
        _ <- mustSql first "begin"
        _ <- mustSql second "begin"
        _ <- mustSql first "insert into users (id, name, age) values (7, 'seven', 27)"
        _ <- mustSql second "insert into users (id, name, age) values (7, 'other', 70)"
        _ <- mustSql first "commit"
        denied <- runStatementCoded second "commit"
        failureMessage denied `shouldBe` "write conflict on table users, id 7"
        _ <- mustSql second "rollback"
        ids <- sessionIds second
        ids `shouldMatchList` [1, 2, 3, 4, 5, 7]

    it "keeps other sessions' rows untouched when the write sets do not overlap" $ withTwoSessions $ \first second -> do
        _ <- mustSql first "begin"
        _ <- mustSql second "begin"
        _ <- mustSql first "insert into users (id, name, age) values (6, 'six', 26)"
        _ <- mustSql first "update users set age = 31 where id = 1"
        _ <- mustSql second "update users set age = 42 where id = 2"
        _ <- mustSql first "commit"
        _ <- mustSql second "commit"
        sessionAge second 1 `shouldReturn` Just 31
        sessionAge second 2 `shouldReturn` Just 42
        ids <- sessionIds second
        ids `shouldMatchList` [1, 2, 3, 4, 5, 6]

    it "lets both sessions delete the same row" $ withTwoSessions $ \first second -> do
        _ <- mustSql first "begin"
        _ <- mustSql second "begin"
        _ <- mustSql first "delete from users where id = 4"
        _ <- mustSql second "delete from users where id = 4"
        _ <- mustSql first "commit"
        _ <- mustSql second "commit"
        ids <- sessionIds second
        ids `shouldMatchList` [1, 2, 3, 5]

    it "keeps the work before a savepoint and drops the work after it" $ withMemorySession $ \session -> do
        _ <- mustSql session "begin"
        _ <- mustSql session "update users set age = 31 where id = 1"
        _ <- mustSql session "savepoint spot"
        _ <- mustSql session "insert into users (id, name, age) values (6, 'six', 26)"
        _ <- mustSql session "delete from users where id = 2"
        _ <- mustSql session "rollback to spot"
        inside <- sessionIds session
        inside `shouldMatchList` [1, 2, 3, 4, 5]
        sessionAge session 1 `shouldReturn` Just 31
        _ <- mustSql session "commit"
        ids <- sessionIds session
        ids `shouldMatchList` [1, 2, 3, 4, 5]
        sessionAge session 1 `shouldReturn` Just 31

    it "lets a savepoint be used again after rolling back to it" $ withMemorySession $ \session -> do
        _ <- mustSql session "begin"
        _ <- mustSql session "insert into users (id, name, age) values (6, 'six', 26)"
        _ <- mustSql session "savepoint spot"
        _ <- mustSql session "insert into users (id, name, age) values (7, 'seven', 27)"
        _ <- mustSql session "rollback to spot"
        _ <- mustSql session "insert into users (id, name, age) values (8, 'eight', 28)"
        ids <- sessionIds session
        ids `shouldMatchList` [1, 2, 3, 4, 5, 6, 8]

    it "drops the savepoints above the one rolled back to" $ withMemorySession $ \session -> do
        _ <- mustSql session "begin"
        _ <- mustSql session "savepoint one"
        _ <- mustSql session "savepoint two"
        _ <- mustSql session "rollback to one"
        missing <- runStatementCoded session "rollback to two"
        failureMessage missing `shouldBe` "no such savepoint: two"

    it "drops a released savepoint and the ones after it" $ withMemorySession $ \session -> do
        _ <- mustSql session "begin"
        _ <- mustSql session "savepoint one"
        _ <- mustSql session "savepoint two"
        released <- mustSql session "release one"
        qrRowCount released `shouldBe` 0
        first <- runStatementCoded session "rollback to one"
        failureMessage first `shouldBe` "no such savepoint: one"
        second <- runStatementCoded session "rollback to two"
        failureMessage second `shouldBe` "no such savepoint: two"

    it "clears the savepoints on COMMIT" $ withMemorySession $ \session -> do
        _ <- mustSql session "begin"
        _ <- mustSql session "savepoint spot"
        _ <- mustSql session "commit"
        missing <- runStatementCoded session "rollback to spot"
        failureMessage missing `shouldBe` "no transaction in progress"

    it "needs a transaction and a known name for savepoints" $ withMemorySession $ \session -> do
        outside <- runStatementCoded session "savepoint spot"
        failureMessage outside `shouldBe` "no transaction in progress"
        _ <- mustSql session "begin"
        unknown <- runStatementCoded session "rollback to nowhere"
        failureMessage unknown `shouldBe` "no such savepoint: nowhere"
        gone <- runStatementCoded session "release nowhere"
        failureMessage gone `shouldBe` "no such savepoint: nowhere"

    it "needs a current database to start" $ do
        db <- newMVar testDb
        settings <- tempSettingsPath "transaction-bare"
        removeIfExists settings
        session <- newSession (memoryBackend testDatabaseName db) (T.pack testDatabaseName) settings
        result <- runStatementCoded session "begin"
        failureMessage result `shouldBe` "no database selected"

-- | 起一份真存储并装成当前链路；动态库打不开就当用例待定
withIpcStorage :: IO () -> IO ()
withIpcStorage body = do
    dir <- ipcDataDir
    cleanIpc dir
    createDirectoryIfMissing True dir
    cfgPath <- writeIpcConfig dir
    opened <- localStorageLink (Just cfgPath)
    case opened of
        Left err -> cleanIpc dir >> pendingWith ("cannot open the storage library: " ++ err)
        Right link -> do
            setStorageLink link
            base <- ipcBackend
            seeded <- beStorage base (A.object ["method" A..= ("bootstrap_system" :: Text), "user" A..= ("root" :: Text), "password_hash" A..= ("hash" :: Text)])
            case seeded of
                Left err -> expectationFailure err
                Right value -> case value of
                    A.Object fields -> KM.lookup "initialized" fields `shouldBe` Just (A.Bool True)
                    _ -> expectationFailure "invalid bootstrap response"
            body `finally` (closeConnection >> cleanIpc dir)

-- | 跑一条语句，出错就算用例失败
mustRun :: IO (Either String StatementResult) -> IO ()
mustRun action = action >>= either expectationFailure (const (pure ()))

-- | 取结果行，出错就算用例失败
rowsOf :: IO (Either String StatementResult) -> IO [Row]
rowsOf action = action >>= either (\err -> expectationFailure err >> pure []) (pure . srRows)

-- | 取结果里某一列的整数
intColumn :: String -> [Row] -> [Int]
intColumn name rows = [n | row <- rows, Just (VInt n) <- [lookup name row]]

-- | 往 p 表连插一批行，结果放 MVar 交回主线程
writeRows :: Backend -> [Int] -> MVar [Either String StatementResult] -> IO ()
writeRows session ids done = do
    outs <- mapM (\i -> beStatement session ("INSERT INTO p (id) VALUES (" ++ show i ++ ")")) ids
    putMVar done outs

-- | 同一进程里开两个库：会话之间不串数据，也不互相挡路
ipcConcurrencySpec :: Spec
ipcConcurrencySpec = describe "server backend on the real storage" $ do
    it "resolves DDL qualifiers without crossing database boundaries" $ withIpcStorage $ do
        base <- ipcBackend
        mustRun (beStatement base "CREATE DATABASE alpha")
        mustRun (beStatement base "CREATE DATABASE beta")
        let alpha = beWithDatabase base "alpha"
            beta = beWithDatabase base "beta"
        unselected <- beStatement base "CREATE TABLE alpha.t (id INT)"
        void unselected `shouldBe` Left "no database selected"
        mustRun (beStatement alpha "CREATE TABLE alpha.t (id INT, value INT)")
        duplicate <- beStatement alpha "CREATE TABLE t (id INT)"
        void duplicate `shouldSatisfy` either (const True) (const False)
        mustRun (beStatement alpha "INSERT INTO t (id, value) VALUES (1, 7)")
        mustRun (beStatement alpha "CREATE INDEX ON alpha.t (value)")
        mustRun (beStatement alpha "DROP INDEX ON alpha.t (value)")
        mustRun (beStatement alpha "ALTER TABLE alpha.t RENAME COLUMN value TO score")
        mustRun (beStatement alpha "ALTER TABLE alpha.t ALTER COLUMN score TYPE BIGINT")
        mustRun (beStatement alpha "ALTER TABLE alpha.t ALTER COLUMN score SET DEFAULT 9")
        mustRun (beStatement alpha "ALTER TABLE alpha.t ALTER COLUMN score SET NOT NULL")
        rows <- rowsOf (beStatement alpha "SELECT score FROM t")
        intColumn "score" rows `shouldBe` [7]
        crossed <- beStatement beta "DROP TABLE alpha.t"
        void crossed `shouldBe` Left "DDL target is outside the current database: alpha.t"
        mustRun (beStatement alpha "ALTER TABLE alpha.t DROP COLUMN score")
        mustRun (beStatement alpha "DROP TABLE alpha.t")
        catalog <- beCatalog alpha >>= either fail pure
        map tiTable catalog `shouldBe` []

    it "isolates domain catalogs by database and hides them from table lists" $ withIpcStorage $ do
        base <- ipcBackend
        unselected <- beStatement base "SHOW DOMAINS"
        void unselected `shouldBe` Left "no database selected"
        mustRun (beStatement base "CREATE DATABASE alpha")
        mustRun (beStatement base "CREATE DATABASE beta")
        let alpha = beWithDatabase base "alpha"
            beta = beWithDatabase base "beta"
        mustRun (beStatement alpha "CREATE DOMAIN code AS INT")
        mustRun (beStatement beta "CREATE DOMAIN code AS VARCHAR(3)")
        mustRun (beStatement alpha "CREATE TABLE typed (id code)")
        mustRun (beStatement beta "CREATE TABLE typed (id code)")
        mustRun (beStatement alpha "INSERT INTO typed (id) VALUES (1)")
        mustRun (beStatement beta "INSERT INTO typed (id) VALUES ('abc')")
        catalog <- beCatalog alpha >>= either fail pure
        map tiTable catalog `shouldBe` ["typed"]
        domains <- rowsOf (beStatement beta "SHOW DOMAINS")
        domains `shouldBe` [[("domain", VStr "code"), ("base_type", VStr "varchar(3)")]]
        refused <- beStatement beta "INSERT INTO typed (id) VALUES (1)"
        void refused `shouldSatisfy` either (const True) (const False)

    it "resolves nested domains only inside their own database" $ withIpcStorage $ do
        base <- ipcBackend
        mustRun (beStatement base "CREATE DATABASE alpha")
        mustRun (beStatement base "CREATE DATABASE beta")
        let alpha = beWithDatabase base "alpha"
            beta = beWithDatabase base "beta"
        mustRun (beStatement alpha "CREATE DOMAIN label AS VARCHAR(3)")
        mustRun (beStatement alpha "CREATE DOMAIN product_label AS label")
        absent <- beStatement beta "CREATE DOMAIN product_label AS label"
        void absent `shouldBe` Left "unknown domain: label"
        mustRun (beStatement alpha "CREATE TABLE products (name product_label)")
        mustRun (beStatement alpha "INSERT INTO products (name) VALUES ('abc')")
        rowsOf (beStatement alpha "SELECT name FROM products") `shouldReturn` [[("name", VStr "abc")]]
        dependency <- beStatement alpha "DROP DOMAIN label"
        void dependency `shouldSatisfy` either (const True) (const False)

    it "keeps two databases apart" $ withIpcStorage $ do
        base <- ipcBackend
        mustRun (beStatement base "CREATE DATABASE alpha")
        mustRun (beStatement base "CREATE DATABASE beta")
        let alpha = beWithDatabase base "alpha"
            beta = beWithDatabase base "beta"
        mustRun (beStatement alpha "CREATE TABLE t (id int)")
        mustRun (beStatement beta "CREATE TABLE t (id int)")
        mustRun (beStatement alpha "INSERT INTO t (id) VALUES (1)")
        mustRun (beStatement beta "INSERT INTO t (id) VALUES (2)")
        rowsA <- rowsOf (beStatement alpha "SELECT id FROM t")
        rowsB <- rowsOf (beStatement beta "SELECT id FROM t")
        intColumn "id" rowsA `shouldBe` [1]
        intColumn "id" rowsB `shouldBe` [2]

    it "runs two sessions at once without mixing their rows" $ withIpcStorage $ do
        base <- ipcBackend
        mustRun (beStatement base "CREATE DATABASE alpha")
        mustRun (beStatement base "CREATE DATABASE beta")
        let alpha = beWithDatabase base "alpha"
            beta = beWithDatabase base "beta"
        mustRun (beStatement alpha "CREATE TABLE p (id int)")
        mustRun (beStatement beta "CREATE TABLE p (id int)")
        doneA <- newEmptyMVar
        doneB <- newEmptyMVar
        _ <- forkIO (writeRows alpha [1 .. 20] doneA)
        _ <- forkIO (writeRows beta [101 .. 120] doneB)
        outs <- sequence [takeMVar doneA, takeMVar doneB]
        lefts (concat outs) `shouldBe` []
        rowsA <- rowsOf (beStatement alpha "SELECT id FROM p")
        rowsB <- rowsOf (beStatement beta "SELECT id FROM p")
        intColumn "id" rowsA `shouldMatchList` [1 .. 20]
        intColumn "id" rowsB `shouldMatchList` [101 .. 120]

    it "shares a committed snapshot and keeps old versions immutable" $ withIpcStorage $ do
        base <- ipcBackend
        mustRun (beStatement base "CREATE DATABASE alpha")
        let alpha = beWithDatabase base "alpha"
        mustRun (beStatement alpha "CREATE TABLE versions (id INT PRIMARY KEY, value INT)")
        mustRun (beStatement alpha "INSERT INTO versions (id, value) VALUES (1, 10)")
        first <- beSnapshot alpha >>= either fail pure
        second <- beSnapshot alpha >>= either fail pure
        firstName <- evaluate first >>= makeStableName
        secondName <- evaluate second >>= makeStableName
        (firstName == secondName) `shouldBe` True
        mustRun (beStatement alpha "UPDATE versions SET value = 20 WHERE id = 1")
        latest <- beSnapshot alpha >>= either fail pure
        fmap tableRows (lookup "versions" first) `shouldBe` Just [[("id", VInt 1), ("value", VInt 10)]]
        fmap tableRows (lookup "versions" latest) `shouldBe` Just [[("id", VInt 1), ("value", VInt 20)]]
        mustRun (beStatement base "DROP DATABASE alpha")
        mustRun (beStatement base "CREATE DATABASE alpha")
        recreated <- beSnapshot alpha >>= either fail pure
        map fst recreated `shouldNotContain` ["versions"]

    it "commits an explicit transaction to the real storage" $ withIpcStorage $ do
        base <- ipcBackend
        mustRun (beStatement base "CREATE DATABASE alpha")
        let alpha = beWithDatabase base "alpha"
        settings <- tempSettingsPath "transaction-ipc"
        removeIfExists settings
        session <- newSession alpha (T.pack testDatabaseName) settings
        _ <- mustSql session "use alpha"
        _ <- mustSql session "create table t (id int)"
        _ <- mustSql session "begin"
        _ <- mustSql session "insert into t (id) values (1)"
        inside <- mustSql session "select * from t"
        intValues (qrRows inside) `shouldBe` [1]
        _ <- mustSql session "commit"
        rows <- rowsOf (beStatement alpha "SELECT id FROM t")
        intColumn "id" rows `shouldBe` [1]

    it "rolls back to a savepoint on the real storage" $ withIpcStorage $ do
        base <- ipcBackend
        mustRun (beStatement base "CREATE DATABASE alpha")
        let alpha = beWithDatabase base "alpha"
        settings <- tempSettingsPath "transaction-ipc-savepoint"
        removeIfExists settings
        session <- newSession alpha (T.pack testDatabaseName) settings
        _ <- mustSql session "use alpha"
        _ <- mustSql session "create table t (id int)"
        _ <- mustSql session "begin"
        _ <- mustSql session "insert into t (id) values (1)"
        _ <- mustSql session "savepoint spot"
        _ <- mustSql session "insert into t (id) values (2)"
        _ <- mustSql session "rollback to spot"
        inside <- mustSql session "select * from t"
        intValues (qrRows inside) `shouldBe` [1]
        _ <- mustSql session "commit"
        rows <- rowsOf (beStatement alpha "SELECT id FROM t")
        intColumn "id" rows `shouldBe` [1]

    it "merges disjoint row writes while retaining the old snapshot" $ withIpcStorage $ do
        base <- ipcBackend
        mustRun (beStatement base "CREATE DATABASE alpha")
        let alpha = beWithDatabase base "alpha"
        mustRun (beStatement alpha "CREATE TABLE items (id int, value int)")
        mustRun (beStatement alpha "INSERT INTO items (id, value) VALUES (1, 10), (2, 20)")
        settings <- tempSettingsPath "transaction-ipc-rows"
        removeIfExists settings
        first <- newSession alpha "alpha" settings
        second <- newSession alpha "alpha" settings
        _ <- mustSql first "use alpha"
        _ <- mustSql second "use alpha"
        _ <- mustSql first "begin"
        _ <- mustSql second "begin"
        _ <- mustSql first "update items set value = 11 where id = 1"
        _ <- mustSql second "update items set value = 22 where id = 2"
        _ <- mustSql first "commit"
        old <- mustSql second "select value from items where id = 1"
        intValues (qrRows old) `shouldBe` [10]
        _ <- mustSql second "commit"
        rows <- rowsOf (beStatement alpha "SELECT value FROM items ORDER BY id")
        intColumn "value" rows `shouldBe` [11, 22]

    it "loads snapshot columns on demand and upgrades them across savepoints" $ withIpcStorage $ do
        base <- ipcBackend
        mustRun (beStatement base "CREATE DATABASE alpha")
        let alpha = beWithDatabase base "alpha"
        mustRun (beStatement alpha "CREATE TABLE items (id INT, name TEXT, age INT)")
        mustRun (beStatement alpha "INSERT INTO items (id, name, age) VALUES (1, 'old', 21), (2, 'second', 22)")
        readRequests <- newIORef []
        -- \| 切库后继续记录事务实际读取列
        let tracked backend =
                backend
                    { beReadTransaction = \token names -> do
                        modifyIORef' readRequests (++ [names])
                        beReadTransaction backend token names
                    , beWithDatabase = tracked . beWithDatabase backend
                    }
        settings <- tempSettingsPath "lazy-snapshot-columns"
        removeIfExists settings
        session <- newSession (tracked alpha) (T.pack testDatabaseName) settings
        _ <- mustSql session "USE alpha"
        _ <- mustSql session "BEGIN"
        readIORef readRequests `shouldReturn` []
        _ <- mustSql session "SAVEPOINT before_read"
        mustRun (beStatement alpha "UPDATE items SET name = 'new' WHERE id = 1")
        first <- mustSql session "SELECT name FROM items"
        qrRows first `shouldBe` [[VStr "old"], [VStr "second"]]
        readIORef readRequests `shouldReturn` [[("items", Just ["name"])]]
        second <- mustSql session "SELECT name FROM items"
        qrRows second `shouldBe` qrRows first
        readIORef readRequests `shouldReturn` [[("items", Just ["name"])]]
        _ <- mustSql session "ROLLBACK TO before_read"
        expanded <- mustSql session "SELECT name, age FROM items"
        qrRows expanded `shouldBe` [[VStr "old", VInt 21], [VStr "second", VInt 22]]
        readIORef readRequests `shouldReturn` [[("items", Just ["name"])], [("items", Nothing)]]
        _ <- mustSql session "UPDATE items SET age = 99 WHERE id = 2"
        _ <- mustSql session "ROLLBACK TO before_read"
        restored <- mustSql session "SELECT name, age FROM items"
        qrRows restored `shouldBe` [[VStr "old", VInt 21], [VStr "second", VInt 22]]
        _ <- mustSql session "ROLLBACK"
        fresh <- mustSql session "SELECT name FROM items"
        qrRows fresh `shouldBe` [[VStr "new"], [VStr "second"]]
        mustRun (beStatement alpha "ALTER TABLE items RENAME COLUMN name TO title")
        stale <- runStatementCoded session "SELECT name FROM items"
        stale `shouldSatisfy` isLeftResult
        renamed <- mustSql session "SELECT title FROM items"
        qrRows renamed `shouldBe` [[VStr "new"], [VStr "second"]]

    it "refuses the second COMMIT on the real storage" $ withIpcStorage $ do
        base <- ipcBackend
        mustRun (beStatement base "CREATE DATABASE alpha")
        let alpha = beWithDatabase base "alpha"
        mustRun (beStatement alpha "CREATE TABLE users (id int, name text, age int)")
        mustRun (beStatement alpha "INSERT INTO users (id, name, age) VALUES (1, 'a', 21)")
        settings <- tempSettingsPath "transaction-ipc-conflict"
        removeIfExists settings
        first <- newSession alpha (T.pack testDatabaseName) settings
        second <- newSession alpha (T.pack testDatabaseName) settings
        _ <- mustSql first "use alpha"
        _ <- mustSql second "use alpha"
        _ <- mustSql first "begin"
        _ <- mustSql second "begin"
        _ <- mustSql first "update users set age = 31 where id = 1"
        _ <- mustSql first "commit"
        denied <- runStatementCoded second "update users set age = 42 where id = 1"
        failureCode denied `shouldBe` "serialization_failure"
        failureMessage denied `shouldSatisfy` T.isInfixOf "row version changed"
        _ <- mustSql second "rollback"
        rows <- rowsOf (beStatement alpha "SELECT id, age FROM users")
        intColumn "id" rows `shouldBe` [1]
        intColumn "age" rows `shouldBe` [31]

-- 判断明确错误返回。
isLeftResult :: Either a b -> Bool
isLeftResult (Left _) = True
isLeftResult _ = False

#if defined(linux_HOST_OS)
-- | 起带 Unix socket 的测试服务器
withUnixTcpServer :: Bool -> (ServerEnv -> ServerHandle -> IO a) -> IO a
withUnixTcpServer enabled body = do
    sockPath <- tempSettingsPath "login.sock"
    removeIfExists sockPath
    withTcpServerAs (Just testRootPassword) defaultServerConfig{scPort = 0, scSocketPath = Just sockPath, scLocalRootLogin = enabled} $ \env handle -> do
        shSocketPath handle `shouldBe` Just sockPath
        body env handle

-- | 连 Unix socket 并配成行缓冲句柄
withUnixConnection :: ServerHandle -> (Handle -> IO a) -> IO a
withUnixConnection handle = bracket open closeQuietly
  where
    -- \| 关连接，忽略异常
    closeQuietly h = void (try (hClose h) :: IO (Either IOException ()))
    -- \| 建 Unix socket 连接
    open = case shSocketPath handle of
        Nothing -> ioError (userError "test server has no Unix socket")
        Just path -> do
            sock <- socket AF_UNIX Stream defaultProtocol
            connect sock (SockAddrUnix path)
            h <- socketToHandle sock ReadWriteMode
            hSetBuffering h LineBuffering
            hSetNewlineMode h noNewlineTranslation
            pure h

-- | 非 root 进程验证不了本机 root 身份，跳过
requiresRoot :: IO () -> IO ()
requiresRoot action = do
    euid <- c_geteuid
    if euid == 0 then action else pendingWith "requires root"

foreign import ccall unsafe "geteuid"
    c_geteuid :: IO CInt

-- | 本机 root 免口令登录的用例
localRootLoginSpec :: Spec
localRootLoginSpec = describe "local root login over a Unix socket" $ do
    it "signs in as root with an empty password over the socket" $ requiresRoot $
        withUnixTcpServer True $ \_ handle -> withUnixConnection handle $ \h -> do
            signed <- tcpLogin h testRootName ""
            asText (at "status" signed) `shouldBe` "ok"
            asText (at "user" signed) `shouldBe` testRootName
            asBool (at "admin" signed) `shouldBe` True
            pong <- tcpTalk h "{\"method\":\"ping\"}"
            asText (at "status" pong) `shouldBe` "pong"
    it "does not apply the empty password to TCP" $ requiresRoot $
        withUnixTcpServer True $ \_ handle -> withConnection handle $ \h -> do
            refused <- tcpLogin h testRootName ""
            asText (at "code" refused) `shouldBe` "unauthorized"
    it "keeps the password required when the switch is off" $ requiresRoot $
        withUnixTcpServer False $ \_ handle -> withUnixConnection handle $ \h -> do
            refused <- tcpLogin h testRootName ""
            asText (at "code" refused) `shouldBe` "unauthorized"
#endif
