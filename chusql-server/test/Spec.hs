{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import ChuSQL.Core.Engine.Storage.IPC (Account (..), Request (ReqAccountReset))
import ChuSQL.Core.Engine.Syntax.Parser (parseStatement)
import ChuSQL.Core.Model (Database, Table (..), Value (..), pattern TInt, pattern TStr)
import ChuSQL.Interface.Auth (hashPasswordWith)
import ChuSQL.Interface.Protocol (ClientRequest (..), Grant (..), RoleView (..), ServerResponse (..), decodeRequest, encodeResponse)
import ChuSQL.Interface.RateLimit (newRateLimiter)
import ChuSQL.Server.Accounts (Principal (..), ensureRootAccount)
import ChuSQL.Server.Backend (Backend (..), memoryBackend)
import ChuSQL.Server.Privileges (
    PrivilegeCommand (..),
    PrivilegeError (..),
    Privileges,
    authorize,
    listRoleViews,
    newPrivileges,
    runPrivilegeCommand,
 )
import ChuSQL.Server.Session (QueryResult (..))
import ChuSQL.Server.TCP (
    ServerConfig (..),
    ServerEnv,
    ServerHandle (..),
    defaultServerConfig,
    loadServerConfigAt,
    newServerEnv,
    startServer,
 )
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (newMVar)
import Control.Exception (IOException, bracket, finally, try)
import Control.Monad (void)
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
import Data.Time.Clock (getCurrentTime)
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
import System.Directory (doesFileExist, getTemporaryDirectory, removeFile)
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
import System.Timeout (timeout)
import Test.Hspec

-- TCP 服务器测试：协议编解码、配置读取与真连服务器的端到端用例。

-- | 测试入口
main :: IO ()
main = hspec spec

-- | 全部用例
spec :: Spec
spec = do
    tcpSpec
    tcpServerSpec
    privilegeSpec

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

    describe "server config ([server] in chusql.toml)" $ do
        it "falls back to the defaults when the file has no server section" $ do
            path <- tcpConfigPath "server-default"
            removeIfExists path
            loadServerConfigAt path `shouldReturn` defaultServerConfig
        it "reads host, port, max_message and max_rows" $ do
            path <- tcpConfigPath "server-keys"
            BS.writeFile path "[server]\nhost = \"0.0.0.0\"\nport = 7777\nmax_message = 4096\nmax_rows = 7\n"
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

-- 权限服务夹具：内存后端上的系统库视角

-- | 一个普通账号
testAccount :: Account
testAccount = Account 0 "alice" "" 0 "" Nothing

-- | 权限用例传的当前库名
testDatabase :: Text
testDatabase = T.pack testDatabaseName

-- | 一个空的权限服务加管理员身份
withPrivileges :: (Privileges -> Principal -> IO a) -> IO a
withPrivileges body = do
    db <- newMVar testDb
    service <- newPrivileges (memoryBackend testDatabaseName db)
    body service (Root testRootName)

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
        granted <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader")
        granted `shouldBe` Right ()
        views <- listRoleViews service
        fmap (map roleGrants) views `shouldBe` Right [[Grant "reader" "select" "test.users"]]

    it "expands ALL and keeps a star object as is" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["all"] "*" "reader")
        views <- listRoleViews service
        fmap (concatMap (map grantPrivilege) . map roleGrants) views
            `shouldBe` Right ["select", "insert", "update", "delete"]
        fmap (concatMap (map grantObject) . map roleGrants) views `shouldBe` Right (replicate 4 "*")

    it "revokes a privilege again" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader")
        _ <- runPrivilegeCommand service root testDatabase (RevokePrivilegesCommand ["select"] "users" "reader")
        views <- listRoleViews service
        fmap (map roleGrants) views `shouldBe` Right [[]]

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
        _ <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "reader")
        _ <- runPrivilegeCommand service root testDatabase (GrantRoleCommand "reader" ["alice"])
        dropped <- runPrivilegeCommand service root testDatabase (DropRoleCommand "reader")
        dropped `shouldBe` Right ()
        views <- listRoleViews service
        views `shouldBe` Right []

    it "refuses a command on a role that does not exist" $ withPrivileges $ \service root -> do
        missing <- runPrivilegeCommand service root testDatabase (GrantPrivilegesCommand ["select"] "users" "ghost")
        missing `shouldBe` Left (PrivilegeError "not_found" "unknown role: ghost")

    it "refuses a table grant without a selected database" $ withPrivileges $ \service root -> do
        _ <- runPrivilegeCommand service root testDatabase (CreateRoleCommand "reader")
        refused <- runPrivilegeCommand service root "" (GrantPrivilegesCommand ["select"] "users" "reader")
        refused `shouldBe` Left (PrivilegeError "no_database" "no database selected")

    it "refuses a non-administrator" $ withPrivileges $ \service _ -> do
        refused <- runPrivilegeCommand service (Ordinary testAccount) testDatabase (CreateRoleCommand "reader")
        refused `shouldBe` Left (PrivilegeError "forbidden" "administrator required")

    it "authorizes the administrator on any statement" $ withPrivileges $ \service root ->
        case parseStatement "SELECT * FROM users" of
            Left err -> expectationFailure err
            Right stmt -> authorize service root testDatabase stmt `shouldReturn` Right ()

