{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.TCP (
    ServerConfig (..),
    defaultServerConfig,
    loadServerConfigAt,
    ServerEnv (..),
    LiveConnections,
    newLiveConnections,
    newServerEnv,
    ServerHandle (..),
    startServer,
    runServer,
    isLocalPeer,
) where

import ChuSQL.Interface.Config (ServerConfig (..), defaultServerConfig, loadServerConfigAt)
import ChuSQL.Interface.Sudo (SudoCredential, createSudoCredential, removeSudoCredential, sudoChallenge, validSudoChallenge, isLocalPeer)
import Data.Time.Clock (UTCTime, getCurrentTime)
import ChuSQL.Interface.Policy (PasswordPolicy (..))
import ChuSQL.Interface.Protocol (ClientRequest (..), ServerResponse (..), decodeRequest, encodeResponse, protocolVersion)
import ChuSQL.Interface.RateLimit (RateLimiter, rateLimitBlock, rateLimitClear, rateLimitRecord)
import qualified ChuSQL.Core.Engine.Error as E
import ChuSQL.Core.Protocol (Account (..), Request (ReqIdentityInitialize))
import ChuSQL.Server.Backend (Backend (..))
import ChuSQL.Server.Session (QueryResult (..), Session, SessionError (..), accounts, authenticateSessionCoded, authenticateSudoSessionCoded, isPlainIdentifier, catalog, databases, newSessionWith, policyOf, reloadPolicy, roleViews, runStatementCoded, runStorageCoded, sessionIsAdmin, sessionUser)
import ChuSQL.Server.Security (writeSecurity)
import ChuSQL.Server.Privileges (newPrivileges, reconcileObjects)
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (IOException, bracket, finally, onException, try)
import Control.Monad (filterM, void)
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as BLC
import Data.IORef (IORef, atomicModifyIORef', newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Network.Socket (
    AddrInfo (..),
    AddrInfoFlag (AI_PASSIVE),
    SockAddr (SockAddrInet, SockAddrInet6),
    Socket,
    SocketOption (ReuseAddr),
    SocketType (Stream),
    accept,
    addrAddress,
    addrFamily,
    bind,
    close,
    defaultHints,
    defaultProtocol,
    getAddrInfo,
    getSocketName,
    getPeerName,
    listen,
    setSocketOption,
    socket,
    socketToHandle,
  )
import System.IO (BufferMode (LineBuffering), Handle, IOMode (ReadWriteMode), hClose, hFlush, hPutStrLn, hSetBuffering, hSetNewlineMode, noNewlineTranslation, stderr)
import System.IO.Error (eofErrorType, isEOFError, mkIOError)

-- TCP 服务器：监听端口，一连接一会话，按一行一 JSON 收发协议消息。

-- 在线连接与服务器环境

-- | 一条在线连接：登录账号与「已被断开」标记
data LiveEntry = LiveEntry
    { leWho :: IORef (Maybe Text)
    , leDropped :: IORef Bool
    , leLocal :: Bool
    , leChallenge :: IORef (Maybe (Text, UTCTime))
    }

-- | 在线连接登记：自增编号到连接条目
data LiveConnections = LiveConnections
    { lcNext :: IORef Int
    , lcLive :: MVar (Map.Map Int LiveEntry)
    }

-- | 建一张空的在线连接登记表
newLiveConnections :: IO LiveConnections
newLiveConnections = LiveConnections <$> newIORef 0 <*> newMVar Map.empty

-- | 新建一条连接条目
newLiveEntry :: Bool -> IO LiveEntry
newLiveEntry local = LiveEntry <$> newIORef Nothing <*> newIORef False <*> pure local <*> newIORef Nothing

-- | 登记一条连接，返回注销动作
registerConnection :: LiveConnections -> LiveEntry -> IO (IO ())
registerConnection live entry = do
    n <- atomicModifyIORef' (lcNext live) (\i -> (i + 1, i))
    modifyMVar_ (lcLive live) (pure . Map.insert n entry)
    pure (modifyMVar_ (lcLive live) (pure . Map.delete n))

-- | 断开这些账号的在线连接，返回断开的条数
dropConnectionsOf :: LiveConnections -> [Text] -> IO Int
dropConnectionsOf live names = do
    let wanted = map T.toLower (filter (not . T.null) names)
    entries <- readMVar (lcLive live)
    hits <- filterM (isOf wanted) (Map.elems entries)
    mapM_ markDropped hits
    pure (length hits)
  where
    -- | 这条连接登录的账号在名单里吗
    isOf wanted entry = do
        current <- readIORef (leWho entry)
        pure (maybe False (\name -> T.toLower name `elem` wanted) current)
    -- | 只做标记：连接在下一次请求应答后收工
    markDropped entry = writeIORef (leDropped entry) True

-- 服务器

data ServerEnv = ServerEnv
    { srvBackend :: Backend
    , srvRootName :: Text
    , srvSettingsFile :: FilePath
    , srvConfig :: ServerConfig
    , srvLimiter :: RateLimiter
    , srvLive :: LiveConnections
    , srvSudo :: MVar (Maybe (Int, Text, SudoCredential))
    }

-- | 组装服务器环境：每个连接再各开一个会话
newServerEnv :: Backend -> Text -> FilePath -> ServerConfig -> RateLimiter -> IO ServerEnv
newServerEnv backend rootName settingsFile config limiter = do
    migrated <- beAccounts backend (ReqIdentityInitialize rootName)
    case migrated of
        Left err -> ioError (userError ("identity migration failed: " ++ err ++ "; stop all processes and try csql-bootstrap repair --config <settings.toml>; use recover if data restoration is required"))
        Right identities
            | any (\a -> accountEnabled a && accountCanLogin a && accountIsSuperuser a) identities -> pure ()
            | otherwise -> ioError (userError "identity migration failed: no enabled login superuser; stop all processes and try csql-bootstrap recover --config <settings.toml>; reset replaces system identities and grants")
    live <- newLiveConnections
    privileges <- newPrivileges backend
    recovered <- writeSecurity (reconcileObjects privileges)
    case recovered of
        Left err -> ioError (userError ("object privilege recovery failed: " ++ show err))
        Right () -> pure ()
    sudo <- newMVar Nothing
    pure (ServerEnv backend rootName settingsFile config limiter live sudo)

data ServerHandle = ServerHandle
    { shPort :: Int
    , shStop :: IO ()
    }

-- | 起监听并在线程里收连接（测试与嵌入用）
startServer :: ServerEnv -> IO ServerHandle
startServer env = do
    sock <- listenSocket (scHost (srvConfig env)) (scPort (srvConfig env))
    port <- actualPort sock
    enableSudo env port `onException` close sock
    stopped <- newMVar False
    _ <- forkIO (acceptLoop env sock stopped)
    pure (ServerHandle port (stopListening sock stopped `finally` disableSudo env))

-- | 在调用线程里收连接（可执行文件用）；绑好端口先回调一次
runServer :: ServerEnv -> (Int -> IO ()) -> IO ()
runServer env announce = bracket (listenSocket (scHost (srvConfig env)) (scPort (srvConfig env))) close $ \sock -> do
    port <- actualPort sock
    enableSudo env port
    (do
        announce port
        stopped <- newMVar False
        acceptLoop env sock stopped) `finally` disableSudo env

-- 为显式配置的本机身份映射创建凭据。
enableSudo :: ServerEnv -> Int -> IO ()
enableSudo env port
    | T.null user = pure ()
    | not (isPlainIdentifier user) = ioError (userError "sudo_auth_user must be a plain identity name")
    | otherwise = modifyMVar_ (srvSudo env) $ \current -> case current of
        Just _ -> ioError (userError "sudo authentication is already running")
        Nothing -> do
            credential <- createSudoCredential port
            hPutStrLn stderr ("sudo_auth enabled: local elevated identity -> " ++ T.unpack user)
            pure (Just (port, user, credential))
  where
    user = scSudoUser (srvConfig env)

-- 清理本次服务实例的本机认证凭据。
disableSudo :: ServerEnv -> IO ()
disableSudo env = modifyMVar_ (srvSudo env) $ \current -> do
    case current of
        Nothing -> pure ()
        Just (port, _, _) -> removeSudoCredential port
    pure Nothing

-- | 置停止标记并关掉监听套接字
stopListening :: Socket -> MVar Bool -> IO ()
stopListening sock stopped = do
    modifyMVar_ stopped (const (pure True))
    void (try (close sock) :: IO (Either IOException ()))

-- | 建监听套接字；端口 0 表示由系统分配
listenSocket :: String -> Int -> IO Socket
listenSocket host port = do
    let hints = defaultHints{addrSocketType = Stream, addrFlags = [AI_PASSIVE]}
        node = if null host then Nothing else Just host
    infos <- getAddrInfo (Just hints) node (Just (show port))
    case infos of
        [] -> ioError (userError ("cannot resolve listen address: " ++ host))
        (info : _) -> do
            sock <- socket (addrFamily info) Stream defaultProtocol
            setSocketOption sock ReuseAddr 1
            bind sock (addrAddress info)
            listen sock 128
            pure sock

-- | 系统分配端口后读回真正的端口
actualPort :: Socket -> IO Int
actualPort sock = do
    address <- getSocketName sock
    pure $ case address of
        SockAddrInet port _ -> fromIntegral port
        SockAddrInet6 port _ _ _ -> fromIntegral port
        _ -> 0

-- | 循环 accept，把每个连接交给独立线程
acceptLoop :: ServerEnv -> Socket -> MVar Bool -> IO ()
acceptLoop env sock stopped = loop
  where
    -- | 等下一个连接
    loop = do
        stop <- readMVar stopped
        if stop
            then pure ()
            else do
                accepted <- try (accept sock) :: IO (Either IOException (Socket, SockAddr))
                case accepted of
                    Left err -> do
                        stopping <- readMVar stopped
                        if stopping then pure () else hPutStrLn stderr ("accept failed: " ++ show err)
                    Right (conn, _) -> do
                        void (forkIO (reportConnection env conn))
                        loop

-- | 连接线程：异常写到 stderr
reportConnection :: ServerEnv -> Socket -> IO ()
reportConnection env conn = do
    outcome <- try (serveConnection env conn) :: IO (Either IOException ())
    case outcome of
        Left err -> hPutStrLn stderr ("connection failed: " ++ show err)
        Right () -> pure ()

-- | 一个连接一个会话：一行一条 JSON，读到 EOF 就收工
serveConnection :: ServerEnv -> Socket -> IO ()
serveConnection env conn = do
    peer <- getPeerName conn
    h <- socketToHandle conn ReadWriteMode
    hSetBuffering h LineBuffering
    hSetNewlineMode h noNewlineTranslation
    entry <- newLiveEntry (isLocalPeer peer)
    unregister <- registerConnection (srvLive env) entry
    session <- newSessionWith (srvBackend env) (srvRootName env) (srvSettingsFile env) (\names -> void (dropConnectionsOf (srvLive env) names))
    outcome <- try (loop h session entry BSC.empty `finally` unregister) :: IO (Either IOException ())
    case outcome of
        Left err | not (isEOFError err) -> hPutStrLn stderr ("connection error: " ++ show err)
        _ -> pure ()
    void (try (hClose h) :: IO (Either IOException ()))
  where
    -- | 逐行读请求并应答
    loop h session entry pending = do
        frame <- readRequestLine h (scMaxMessage (srvConfig env)) pending
        case frame of
            Nothing -> send h (RespError "too_large" "request line too large")
            Just (cleaned, remaining) -> case decodeRequest cleaned of
                Left message -> send h (RespError "bad_request" message) >> loop h session entry remaining
                Right request -> do
                    expired <- readIORef (leDropped entry)
                    if expired
                        then send h (RespError "unauthorized" "session was dropped by an account change")
                        else do
                            (response, keepGoing) <- dispatch env session entry request
                            send h response
                            if keepGoing then loop h session entry remaining else pure ()
    -- | 发一条响应并冲缓冲
    send h response = do
        BLC.hPutStr h (encodeResponse response)
        BLC.hPutStr h "\n"
        hFlush h

-- | 限长读取一行并保留后续帧
readRequestLine :: Handle -> Int -> BSC.ByteString -> IO (Maybe (BSC.ByteString, BSC.ByteString))
readRequestLine handle limit = collect [] 0
  where
    -- | 按块读取至换行、超限或 EOF
    collect parts total bytes = case BSC.elemIndex '\n' bytes of
        Just offset -> finish (BSC.take offset bytes : parts) (BSC.drop (offset + 1) bytes)
        Nothing
            | used > limit && (used - limit > 1 || BSC.null bytes || BSC.last bytes /= '\r') -> pure Nothing
            | otherwise -> do
                next <- BSC.hGetSome handle (max 1 (min 4094 (limit - used) + 2))
                if BSC.null next
                    then if used == 0
                        then ioError (mkIOError eofErrorType "read request" (Just handle) Nothing)
                        else finish (bytes : parts) BSC.empty
                    else collect (bytes : parts) used next
          where
            used = total + BSC.length bytes
    -- | 合并并校验一行的有效长度
    finish parts remaining = do
        let line = BSC.dropWhileEnd (== '\r') (BSC.concat (reverse parts))
        pure (if BSC.length line > limit then Nothing else Just (line, remaining))

-- | 一条请求一条响应，quit 后停连接
dispatch :: ServerEnv -> Session -> LiveEntry -> ClientRequest -> IO (ServerResponse, Bool)
dispatch env session entry request = case request of
    ReqHello version
        | version == protocolVersion -> pure (RespHello protocolVersion, True)
        | otherwise -> pure (RespError "bad_request" ("unsupported protocol version: " <> tshow version), True)
    ReqLogin user password -> do
        writeIORef (leChallenge entry) Nothing
        response <- login env session user password
        case response of
            RespLogin who _ -> writeIORef (leWho entry) (Just who)
            _ -> pure ()
        pure (response, True)
    ReqSudoChallenge -> do
        configured <- readMVar (srvSudo env)
        who <- sessionUser session
        case configured of
            Just _ | leLocal entry && who == Nothing -> do
                nonce <- sudoChallenge
                now <- getCurrentTime
                writeIORef (leChallenge entry) (Just (nonce, now))
                pure (RespSudoChallenge nonce, True)
            _ -> pure (RespError "forbidden" "local sudo authentication is unavailable", True)
    ReqSudoLogin user proof -> do
        response <- sudoLogin env session entry user proof
        pure (response, True)
    ReqQuery sql -> do
        who <- sessionUser session
        response <- case who of
            Nothing -> pure (RespError "unauthorized" "sign in first")
            Just _
                | T.null (T.strip sql) -> pure (RespError "bad_request" "sql must not be empty")
                | otherwise -> do
                    result <- runStatementCoded session sql
                    pure $ case result of
                        Left err -> RespError (sessCode err) (sessMessage err)
                        Right queryResult -> RespResult (trimResult (scMaxRows (srvConfig env)) queryResult)
        pure (response, True)
    ReqStorage payload -> do
        result <- runStorageCoded session payload
        let response = case result of
                Left err -> RespError (sessCode err) (sessMessage err)
                Right value -> RespStorage value
        pure (response, True)
    ReqCatalog -> do
        response <- overSession session (catalog session) RespCatalog
        pure (response, True)
    ReqDatabases -> do
        response <- overSession session (databases session) RespDatabases
        pure (response, True)
    ReqRoles -> do
        response <- overSession session (roleViews session) RespRoles
        pure (response, True)
    ReqAccounts -> do
        response <- overSession session (accounts session) RespAccounts
        pure (response, True)
    ReqPolicy -> do
        response <- overSession session (policyOf session) policyResponse
        pure (response, True)
    ReqReloadPolicy -> do
        response <- overSession session (reloadPolicy session) policyResponse
        pure (response, True)
    ReqPing -> pure (RespPong, True)
    ReqQuit -> pure (RespBye, False)

-- 消费一次性证明并登录本机映射账号。
sudoLogin :: ServerEnv -> Session -> LiveEntry -> Text -> Text -> IO ServerResponse
sudoLogin env session entry user proof = do
    challenge <- atomicModifyIORef' (leChallenge entry) (\value -> (Nothing, value))
    configured <- readMVar (srvSudo env)
    now <- getCurrentTime
    who <- sessionUser session
    let key = T.toLower (T.strip user)
        mapped = maybe "" (\(_, name, _) -> name) configured
        valid = leLocal entry && who == Nothing && key == mapped
            && case configured of
                Just (_, _, credential) -> validSudoChallenge credential now challenge key proof
                Nothing -> False
    blocked <- rateLimitBlock (srvLimiter env) key
    response <- case blocked of
        Just _ -> pure (RespError "too_many_attempts" "too many failed sudo logins")
        Nothing | not valid -> do
            rateLimitRecord (srvLimiter env) key
            pure (RespError "unauthorized" "sudo authentication refused")
        Nothing -> do
            result <- authenticateSudoSessionCoded session key
            case result of
                Left err -> do
                    rateLimitRecord (srvLimiter env) key
                    pure (RespError (sessCode err) (sessMessage err))
                Right () -> do
                    rateLimitClear (srvLimiter env) key
                    writeIORef (leWho entry) (Just key)
                    admin <- sessionIsAdmin session
                    pure (RespLogin key admin)
    hPutStrLn stderr ("sudo_auth attempt: mapped_user=" ++ T.unpack mapped
        ++ " local=" ++ show (leLocal entry) ++ " result=" ++ auditResult response)
    pure response
  where
    -- 提取不包含凭据的认证审计结果。
    auditResult RespLogin{} = "accepted"
    auditResult (RespError code _) = T.unpack code
    auditResult _ = "refused"

-- | 口令策略在线上只有两个数字：最短长度与至少几类字符
policyResponse :: PasswordPolicy -> ServerResponse
policyResponse (PasswordPolicy minLength classes) = RespPolicy minLength classes

-- | 会话方法统一口径：没登录先挡，其余失败按码表落到响应
overSession :: Session -> IO (Either Text a) -> (a -> ServerResponse) -> IO ServerResponse
overSession session action wrap = do
    who <- sessionUser session
    case who of
        Nothing -> pure (RespError "unauthorized" "sign in first")
        Just _ -> do
            result <- action
            pure $ case result of
                Left message -> RespError (sessionCode message) message
                Right value -> wrap value

-- | 会话层的失败文案归一到协议错误码
sessionCode :: Text -> Text
sessionCode = T.pack . E.errorCode . T.unpack

-- | 登录：按账号名限流，成功清账
login :: ServerEnv -> Session -> Text -> Text -> IO ServerResponse
login env session user password = do
    let key = T.toLower (T.strip user)
    blocked <- rateLimitBlock (srvLimiter env) key
    case blocked of
        Just seconds -> pure (RespError "too_many_attempts" ("too many failed logins, retry in " <> tshow (ceiling seconds :: Integer) <> "s"))
        Nothing -> do
            result <- authenticateSessionCoded session key password
            case result of
                Left err -> do
                    rateLimitRecord (srvLimiter env) key
                    pure (RespError (sessCode err) (sessMessage err))
                Right () -> do
                    rateLimitClear (srvLimiter env) key
                    who <- sessionUser session
                    admin <- sessionIsAdmin session
                    pure (RespLogin (fromMaybe key who) admin)

-- | 按上限截行，总行数保持不变
trimResult :: Int -> QueryResult -> QueryResult
trimResult limit result
    | length (qrRows result) > limit = result{qrRows = take limit (qrRows result), qrTruncated = True}
    | otherwise = result

-- | show 成 Text
tshow :: Show a => a -> Text
tshow = T.pack . show
