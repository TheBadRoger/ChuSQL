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
import Data.IORef (newIORef, readIORef, writeIORef)
import ChuSQL.Core.Engine.Syntax.Parser (parseStatement)
import ChuSQL.Core.Model (Database, Histogram (..), Row, Table (..), Value (..), pattern TInt, pattern TStr)
import ChuSQL.Interface.Auth (hashPasswordWith)
import ChuSQL.Interface.Protocol (ClientRequest (..), Grant (..), RoleView (..), ServerResponse (..), decodeRequest, encodeResponse)
import ChuSQL.Interface.RateLimit (newRateLimiter)
import ChuSQL.Interface.Link (connectClient, closeClient, clientSudoLogin, clientQuery)
import ChuSQL.Interface.Sudo (SudoCredential, newSudoCredential, sudoProof, verifySudoProof, validSudoChallenge,
    createSudoCredential, removeSudoCredential, readSudoCredential, sudoPrivileged)
import ChuSQL.Server.Accounts (Principal (..), ensureRootAccount)
import ChuSQL.Server.Backend (Backend (..), StatementResult (..), ipcBackend, memoryBackend)
import ChuSQL.Server.Privileges (
    PrivilegeCommand (..),
    PrivilegeError (..),
    Privileges,
    affectedAccounts,
    authorize,
    authorizeConnect,
    claimObject,
    prepareObject,
    listRoleViews,
    newPrivileges,
    runPrivilegeCommand,
 )
import ChuSQL.Server.Session (QueryResult (..), Session, SessionError (..), newSession, runStatementCoded, runStorageCoded, authenticateSessionCoded)
import ChuSQL.Server.TCP (
    ServerConfig (..),
    ServerEnv (..),
    ServerHandle (..),
    defaultServerConfig,
    loadServerConfigAt,
    newServerEnv,
    startServer,
    isLocalPeer,
 )
import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar)
import System.Timeout (timeout)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (void)
import Data.List (sort)
import Data.Aeson (decode)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Network.Socket (
    AddrInfo (..),
    SockAddr (..),
    tupleToHostAddress,
    tupleToHostAddress6,
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
 )
import Test.Hspec hiding (after, before)

-- TCP 服务器测试：协议编解码、配置读取与真连服务器的端到端用例。

-- | 测试入口
main :: IO ()
main = hspec spec

-- | 全部用例
spec :: Spec
spec = do
    describe "sudo authentication" $ do
        it "defaults to disabled and rejects unconfigured TCP authentication" $
            withTcpServer defaultServerConfig $ \_ handle -> withConnection handle $ \client -> do
                scSudoUser defaultServerConfig `shouldBe` ""
                challenge <- tcpTalk client "{\"method\":\"sudo_challenge\"}"
                at "code" challenge `shouldBe` A.String "forbidden"
                denied <- tcpTalk client "{\"method\":\"sudo_login\",\"user\":\"admin\",\"proof\":\"fake\"}"
                at "code" denied `shouldBe` A.String "unauthorized"
        it "loads an explicit normalized local identity mapping" $ do
            path <- tempSettingsPath "sudo-config"
            writeFile path (unlines ["[server]", "sudo_auth_user = " ++ show ("Local_User" :: String)])
            config <- loadServerConfigAt path `finally` removeIfExists path
            scSudoUser config `shouldBe` "local_user"
        it "supports the client challenge and login exchange" $
            withTestSudo $ \_ handle credential -> do
                connected <- connectClient "127.0.0.1" (shPort handle)
                case connected of
                    Left message -> expectationFailure (T.unpack message)
                    Right client -> (do
                        clientSudoLogin client credential "local_user" >>= (`shouldBe` Right False)
                        result <- clientQuery client "select 1"
                        result `shouldSatisfy` isRightResult) `finally` closeClient client
        it "only treats kernel loopback addresses as local" $ do
            isLocalPeer (SockAddrInet 7777 (tupleToHostAddress (127, 0, 0, 1))) `shouldBe` True
            isLocalPeer (SockAddrInet 7777 (tupleToHostAddress (192, 168, 1, 2))) `shouldBe` False
            isLocalPeer (SockAddrInet6 7777 0 (tupleToHostAddress6 (0,0,0,0,0,0,0,1)) 0) `shouldBe` True
            isLocalPeer (SockAddrInet6 7777 0 (tupleToHostAddress6 (0,0,0,0,0,65535,32512,1)) 0) `shouldBe` True
            isLocalPeer (SockAddrInet6 7777 0 (tupleToHostAddress6 (0,0,0,0,0,65535,49320,1)) 0) `shouldBe` False
        it "binds proofs to the instance, challenge, account and expiration" $ do
            credential <- newSudoCredential
            other <- newSudoCredential
            now <- getCurrentTime
            let proof = sudoProof credential "nonce" "local_user"
            verifySudoProof credential "nonce" "LOCAL_USER" proof `shouldBe` True
            verifySudoProof other "nonce" "local_user" proof `shouldBe` False
            verifySudoProof credential "other" "local_user" proof `shouldBe` False
            verifySudoProof credential "nonce" "admin" proof `shouldBe` False
            validSudoChallenge credential now (Just ("nonce", addUTCTime (-61) now)) "local_user" proof `shouldBe` False
            validSudoChallenge credential now (Just ("nonce", addUTCTime 1 now)) "local_user" proof `shouldBe` False
            validSudoChallenge credential now Nothing "local_user" proof `shouldBe` False
        it "requires an OS elevated identity to provision or read credentials" $ do
            privileged <- sudoPrivileged
            if privileged then do
                bracket (createSudoCredential 65534) (const (removeSudoCredential 65534)) $ \credential -> do
                    stored <- readSudoCredential 65534
                    case stored of
                        Left message -> expectationFailure (T.unpack message)
                        Right restored -> sudoProof restored "native" "local_user" `shouldBe` sudoProof credential "native" "local_user"
                    duplicate <- try (createSudoCredential 65534) :: IO (Either IOException SudoCredential)
                    duplicate `shouldSatisfy` isLeftResult
            else do
                created <- try (createSudoCredential 65534) :: IO (Either IOException SudoCredential)
                created `shouldSatisfy` isLeftResult
                readSudoCredential 65534 >>= (`shouldSatisfy` isLeftResult)
        it "logs in an ordinary mapped identity without elevating its privileges" $
            withTestSudo $ \_ handle credential -> withConnection handle $ \client -> do
                answer <- tcpSudo client credential "local_user"
                at "status" answer `shouldBe` A.String "ok"
                at "admin" answer `shouldBe` A.Bool False
                denied <- tcpQuery client "create database forbidden"
                at "code" denied `shouldBe` A.String "forbidden"
        it "rejects a forged proof and consumes its challenge" $
            withTestSudo $ \_ handle credential -> withConnection handle $ \client -> do
                nonce <- tcpTalk client "{\"method\":\"sudo_challenge\"}"
                denied <- tcpTalk client "{\"method\":\"sudo_login\",\"user\":\"local_user\",\"proof\":\"forged\"}"
                at "code" denied `shouldBe` A.String "unauthorized"
                replay <- tcpSudoProof client "local_user" (sudoProof credential (asText (at "challenge" nonce)) "local_user")
                at "code" replay `shouldBe` A.String "unauthorized"
        it "rejects proof reuse on another connection and account mapping" $
            withTestSudo $ \_ handle credential -> withConnection handle $ \first -> withConnection handle $ \second -> do
                nonce <- tcpTalk first "{\"method\":\"sudo_challenge\"}"
                let proof = sudoProof credential (asText (at "challenge" nonce)) "local_user"
                replay <- tcpSudoProof second "local_user" proof
                at "code" replay `shouldBe` A.String "unauthorized"
                mismatch <- tcpSudo first credential "admin"
                at "code" mismatch `shouldBe` A.String "unauthorized"
        it "revokes the attribute and drops existing sudo sessions" $
            withTestSudo $ \_ handle credential -> withConnection handle $ \admin -> withConnection handle $ \client -> do
                _ <- tcpLogin admin testRootName testRootPassword
                _ <- tcpSudo client credential "local_user"
                changed <- tcpQuery admin "alter user local_user noallow_sudo_auth"
                at "status" changed `shouldBe` A.String "result"
                expired <- tcpQuery client "select 1"
                at "code" expired `shouldBe` A.String "unauthorized"
                tcpClosed client
                withConnection handle $ \fresh -> do
                    denied <- tcpSudo fresh credential "local_user"
                    at "code" denied `shouldBe` A.String "unauthorized"
        it "rejects disabled and NOLOGIN identities" $
            withTestSudo $ \_ handle credential -> withConnection handle $ \admin -> do
                _ <- tcpLogin admin testRootName testRootPassword
                mapM_ (checkSudoDisabled admin handle credential)
                    ["alter user local_user disabled", "alter user local_user enabled nologin"]
        it "does not inherit sudo authentication from a role" $
            withTestSudo $ \_ handle credential -> withConnection handle $ \admin -> do
                _ <- tcpLogin admin testRootName testRootPassword
                _ <- tcpQuery admin "create role sudo_role"
                _ <- tcpQuery admin "alter role sudo_role allow_sudo_auth"
                _ <- tcpQuery admin "grant sudo_role to local_user"
                _ <- tcpQuery admin "alter user local_user noallow_sudo_auth"
                withConnection handle $ \client -> do
                    denied <- tcpSudo client credential "local_user"
                    at "code" denied `shouldBe` A.String "unauthorized"
        it "keeps password login independent of the sudo attribute" $
            withTestSudo $ \_ handle _ -> withConnection handle $ \client -> do
                denied <- tcpLogin client "local_user" ""
                at "code" denied `shouldBe` A.String "unauthorized"
                accepted <- tcpLogin client "local_user" "Local-Pass123!"
                at "status" accepted `shouldBe` A.String "ok"
    tcpSpec
    tcpServerSpec
    privilegeSpec
    objectPrivilegeSpec
    transactionSpec
    ipcConcurrencySpec
    identitySpec

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
            mapM_ (\sql -> tcpQuery manager sql >>= \reply -> asText (at "code" reply) `shouldBe` "forbidden")
                ["alter user manager superuser", "alter user managed system_catalog_manager", "alter user managed allow_sudo_auth", "alter user admin identified by 'Another-Pass123!'", "create database forbidden"]
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
            map asText (items (at "columns" shown)) `shouldBe` ["id", "name", "can_login", "is_superuser", "enabled", "system_catalog_manager", "allow_sudo_auth", "created_at"]
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
            mapM_ (\sql -> tcpQuery h sql >>= \reply -> asText (at "code" reply) `shouldBe` "bad_request")
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

-- 夹具：内存后端装在 MVar 里，跟 Web 测试同一套表

-- | 夹具库名
testDatabaseName :: String
testDatabaseName = "test"

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
            Nothing
      )
    , ( "orders"
      , Table
            "orders"
            [("id", TInt), ("user_id", TInt)]
            [[("id", VInt 1), ("user_id", VInt 1)]]
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
testPasswordHash raw = hashPasswordWith 1000 (BS.replicate 16 7) raw

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
tcpConfigPath name = do
    tmp <- getTemporaryDirectory
    pure (tmp </> ("chusql-server-test-" ++ name ++ ".toml"))

-- | 临时设置文件（新会话读口令策略用）
tempSettingsPath :: String -> IO FilePath
tempSettingsPath name = do
    tmp <- getTemporaryDirectory
    pure (tmp </> ("chusql-server-test-" ++ name ++ "-settings.json"))

-- | 文件不存在也算成功
removeIfExists :: FilePath -> IO ()
removeIfExists path = do
    there <- doesFileExist path
    if there then void (try (removeFile path) :: IO (Either IOException ())) else pure ()

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
withConnection handle body = bracket open closeQuietly body
  where
    -- | 关连接，忽略异常
    closeQuietly h = do
        _ <- try (hClose h) :: IO (Either IOException ())
        pure ()
    -- | 建连接并配成行缓冲句柄
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
        it "keeps the HTTP, storage and page keys out of the server config" $ do
            path <- tcpConfigPath "server-other-keys"
            BS.writeFile path "[web]\nport = 8888\n\n[storage]\ndata_dir = \"/tmp/chusql\"\n\n[page]\nsize = 8192\n"
            config <- loadServerConfigAt path
            scPort config `shouldBe` scPort defaultServerConfig
            scMaxRows config `shouldBe` scMaxRows defaultServerConfig

-- | 真连 TCP 服务器的用例
tcpServerSpec :: Spec
tcpServerSpec = do
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
testAccount = Account 0 "alice" "" 0 "" Nothing True False True False False

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
        fmap (concatMap (map grantPrivilege) . map roleGrants) views
            `shouldBe` Right ["select", "insert", "update", "delete"]
        fmap (concatMap (map grantObject) . map roleGrants) views `shouldBe` Right (replicate 4 "test.*")

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
        _ <- beAccounts backend (ReqIdentityAlter testRootName Nothing (Just True) Nothing Nothing Nothing)
        _ <- beAccounts backend (ReqAccountReset "alice" (testPasswordHash "Alice-Secret-1234"))
        started <- newEmptyMVar
        release <- newEmptyMVar
        finished <- newEmptyMVar
        revoked <- newEmptyMVar
        let slow = backend{beWithDatabase = \name -> (beWithDatabase backend name){beStatement = \sql -> do
                if sql == "SELECT * FROM users" then putMVar started () >> takeMVar release else pure ()
                beStatement (beWithDatabase backend name) sql}}
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
        changed <- beAccounts backend (ReqIdentityAlter "alice" Nothing Nothing (Just False) Nothing Nothing)
        either expectationFailure (const (pure ())) changed
        authorizeSql service (Ordinary testAccount{accountUser = "bob"}) "SELECT * FROM users" `shouldReturn` Left (PrivilegeError "forbidden" "permission denied: SELECT ON users")
    it "preserves dormant direct grants and ownership across unrelated ACL writes" $ withObjectPrivileges $ \backend service root -> do
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "alice" False) `shouldReturn` Right ()
        mustRun (beStatement backend "CREATE TABLE owned (id INT)")
        claimObject service (Ordinary testAccount) testDatabase "owned" `shouldReturn` Right ()
        changed <- beAccounts backend (ReqIdentityAlter "alice" Nothing Nothing (Just False) Nothing Nothing)
        either expectationFailure (const (pure ())) changed
        runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "bob" False) `shouldReturn` Right ()
        enabled <- beAccounts backend (ReqIdentityAlter "alice" Nothing Nothing (Just True) Nothing Nothing)
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
        _ <- beAccounts backend (ReqIdentityAlter testRootName Nothing (Just True) Nothing Nothing Nothing)
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
        _ <- beAccounts backend (ReqIdentityAlter testRootName Nothing (Just True) Nothing Nothing Nothing)
        _ <- beAccounts backend (ReqAccountCreate "spare_admin" "hash")
        _ <- beAccounts backend (ReqIdentityAlter "spare_admin" Nothing (Just True) Nothing Nothing Nothing)
        executed <- newIORef False
        let guarded = backend{beStorage = \_ -> writeIORef executed True >> pure (Right A.Null)}
        settings <- tempSettingsPath "raw-storage-identity"
        session <- newSession guarded testRootName settings
        authenticateSessionCoded session testRootName testRootPassword `shouldReturn` Right ()
        changed <- beAccounts backend (ReqIdentityAlter testRootName Nothing (Just False) Nothing Nothing Nothing)
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
failureMessage outcome = either sessMessage (const "") outcome

-- | 取失败里的错误码
failureCode :: Either SessionError QueryResult -> Text
failureCode outcome = either sessCode (const "") outcome

-- | 取结果第一列的整数
intValues :: [[Value]] -> [Int]
intValues rows = [n | row <- rows, Just (VInt n) <- [nth 0 row]]

-- | 显式事务用例
transactionSpec :: Spec
transactionSpec = describe "server transactions" $ do
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
        let local = Table "users" [("id", TInt)] [[("id", VInt 1)]] Nothing
            otherTable = Table "users" [("id", TInt)] [[("id", VInt 2)]] Nothing
        ref <- newMVar [("test.users", local), ("other.users", otherTable)]
        let backend = memoryBackend "test" ref
        selected <- beStatement backend "SELECT id FROM users"
        fmap srRows selected `shouldBe` Right [[("id", VInt 1)]]
        tables <- beCatalog backend
        fmap (map tiTable) tables `shouldBe` Right ["users"]
        updated <- beStatement backend "UPDATE users SET id = 3"
        fmap (const ()) updated `shouldBe` Right ()
        snapshot <- beSnapshot (memoryBackend "other" ref)
        fmap (map (tableRows . snd)) snapshot `shouldBe` Right [[[ ("id", VInt 2) ]]]
    it "rejects cross-database reads and writes including nested queries" $ withMemorySession $ \session -> do
        mapM_ (\query -> do
            result <- runStatementCoded session query
            failureMessage result `shouldSatisfy` T.isInfixOf "outside the current database")
            [ "SELECT * FROM other.users"
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
        denied <- runStatementCoded session "drop domain code"
        failureMessage denied `shouldBe` "DDL is not allowed in a transaction"
        _ <- mustSql session "insert into typed (id) values (7)"
        _ <- mustSql session "commit"
        rows <- mustSql session "select id from typed"
        intValues (qrRows rows) `shouldBe` [7]

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

    it "refuses nested BEGIN and COMMIT outside a transaction" $ withMemorySession $ \session -> do
        _ <- mustSql session "begin"
        nested <- runStatementCoded session "begin"
        failureMessage nested `shouldBe` "already in a transaction"
        _ <- mustSql session "rollback"
        late <- runStatementCoded session "commit"
        failureMessage late `shouldBe` "no transaction in progress"

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
        fmap (const ()) unselected `shouldBe` Left "no database selected"
        mustRun (beStatement alpha "CREATE TABLE alpha.t (id INT, value INT)")
        duplicate <- beStatement alpha "CREATE TABLE t (id INT)"
        fmap (const ()) duplicate `shouldSatisfy` either (const True) (const False)
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
        fmap (const ()) crossed `shouldBe` Left "DDL target is outside the current database: alpha.t"
        mustRun (beStatement alpha "ALTER TABLE alpha.t DROP COLUMN score")
        mustRun (beStatement alpha "DROP TABLE alpha.t")
        catalog <- beCatalog alpha >>= either fail pure
        map tiTable catalog `shouldBe` []

    it "isolates domain catalogs by database and hides them from table lists" $ withIpcStorage $ do
        base <- ipcBackend
        unselected <- beStatement base "SHOW DOMAINS"
        fmap (const ()) unselected `shouldBe` Left "no database selected"
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
        fmap (const ()) refused `shouldSatisfy` either (const True) (const False)

    it "resolves nested domains only inside their own database" $ withIpcStorage $ do
        base <- ipcBackend
        mustRun (beStatement base "CREATE DATABASE alpha")
        mustRun (beStatement base "CREATE DATABASE beta")
        let alpha = beWithDatabase base "alpha"
            beta = beWithDatabase base "beta"
        mustRun (beStatement alpha "CREATE DOMAIN label AS VARCHAR(3)")
        mustRun (beStatement alpha "CREATE DOMAIN product_label AS label")
        absent <- beStatement beta "CREATE DOMAIN product_label AS label"
        fmap (const ()) absent `shouldBe` Left "unknown domain: label"
        mustRun (beStatement alpha "CREATE TABLE products (name product_label)")
        mustRun (beStatement alpha "INSERT INTO products (name) VALUES ('abc')")
        rowsOf (beStatement alpha "SELECT name FROM products") `shouldReturn` [[("name", VStr "abc")]]
        dependency <- beStatement alpha "DROP DOMAIN label"
        fmap (const ()) dependency `shouldSatisfy` either (const True) (const False)

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
        [err | Left err <- concat outs] `shouldBe` []
        rowsA <- rowsOf (beStatement alpha "SELECT id FROM p")
        rowsB <- rowsOf (beStatement beta "SELECT id FROM p")
        intColumn "id" rowsA `shouldMatchList` [1 .. 20]
        intColumn "id" rowsB `shouldMatchList` [101 .. 120]

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
        _ <- mustSql second "update users set age = 42 where id = 1"
        _ <- mustSql first "commit"
        denied <- runStatementCoded second "commit"
        failureCode denied `shouldBe` "serialization_failure"
        failureMessage denied `shouldBe` "write conflict on table users, id 1"
        _ <- mustSql second "rollback"
        rows <- rowsOf (beStatement alpha "SELECT id, age FROM users")
        intColumn "id" rows `shouldBe` [1]
        intColumn "age" rows `shouldBe` [31]


-- 判断明确错误返回。
isLeftResult :: Either a b -> Bool
isLeftResult (Left _) = True
isLeftResult _ = False

-- 启动带独立本机认证凭据的测试会话。
withTestSudo :: (ServerEnv -> ServerHandle -> SudoCredential -> IO a) -> IO a
withTestSudo body = withTcpServer defaultServerConfig $ \env handle -> do
    credential <- newSudoCredential
    withConnection handle $ \admin -> do
        _ <- tcpLogin admin testRootName testRootPassword
        created <- tcpQuery admin "create user local_user identified by 'Local-Pass123!'"
        at "status" created `shouldBe` A.String "result"
        enabled <- tcpQuery admin "alter user local_user allow_sudo_auth"
        at "status" enabled `shouldBe` A.String "result"
    modifyMVar_ (srvSudo env) (const (pure (Just (0, "local_user", credential))))
    body env handle credential `finally` modifyMVar_ (srvSudo env) (const (pure Nothing))

-- 交换本机挑战并提交账号绑定证明。
tcpSudo :: Handle -> SudoCredential -> Text -> IO A.Value
tcpSudo client credential user = do
    challenge <- tcpTalk client "{\"method\":\"sudo_challenge\"}"
    tcpSudoProof client user (sudoProof credential (asText (at "challenge" challenge)) user)

-- 发送本机认证证明。
tcpSudoProof :: Handle -> Text -> Text -> IO A.Value
tcpSudoProof client user proof = tcpTalk client (BL.toStrict (A.encode (A.object
    ["method" A..= ("sudo_login" :: Text), "user" A..= user, "proof" A..= proof])))

-- 验证身份禁用或取消登录后拒绝本机认证。
checkSudoDisabled :: Handle -> ServerHandle -> SudoCredential -> Text -> IO ()
checkSudoDisabled admin handle credential sql = do
    changed <- tcpQuery admin sql
    at "status" changed `shouldBe` A.String "result"
    withConnection handle $ \client -> do
        denied <- tcpSudo client credential "local_user"
        at "code" denied `shouldBe` A.String "unauthorized"

-- 判断明确成功返回。
isRightResult :: Either a b -> Bool
isRightResult (Right _) = True
isRightResult _ = False
