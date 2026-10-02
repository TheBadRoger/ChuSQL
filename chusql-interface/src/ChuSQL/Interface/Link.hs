{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Interface.Link (
    Client,
    connectClient,
    closeClient,
    clientLogin,
    clientQuery,
    clientPing,
    clientCatalog,
    clientDatabases,
    clientRoles,
    clientAccounts,
    clientPolicy,
    clientReloadPolicy,
) where

import ChuSQL.Core.Protocol (Account, QueryResult (..), TableInfo (..))
import ChuSQL.Interface.Protocol (
    ClientRequest (..),
    RoleView,
    ServerResponse (..),
    decodeResponse,
    encodeRequest,
    protocolVersion,
 )
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Control.Exception (IOException, try)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Char8 as BSC
import Data.Text (Text)
import qualified Data.Text as T
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
 )
import System.IO (BufferMode (LineBuffering), Handle, IOMode (ReadWriteMode), hClose, hFlush, hSetBuffering, hSetNewlineMode, noNewlineTranslation)
import System.IO.Error (isEOFError)

-- 前端到 server 的 TCP 链路：一行一条 JSON，命令行与 Web 端共用。

-- | 一条已连上、已对过协议版本的连接
data Client = Client
    { clHandle :: Handle
    , clLock :: MVar ()
    }

-- | 连上 server 并对协议版本，失败给人话错误
connectClient :: Text -> Int -> IO (Either Text Client)
connectClient host port = do
    opened <- try (openConnection host port) :: IO (Either IOException Handle)
    case opened of
        Left err -> pure (Left ("cannot reach the server at " <> T.pack (address host port) <> ": " <> T.pack (describe err)))
        Right handle -> do
            lock <- newMVar ()
            let client = Client handle lock
            greeted <- request client (ReqHello protocolVersion)
            case greeted of
                Right (RespHello version)
                    | version == protocolVersion -> pure (Right client)
                    | otherwise ->
                        giveUp handle
                            ( "server "
                                <> T.pack (address host port)
                                <> " speaks protocol "
                                <> T.pack (show version)
                                <> ", this client needs "
                                <> T.pack (show protocolVersion)
                            )
                Right (RespError code message) -> giveUp handle (code <> ": " <> message)
                Right _ -> giveUp handle "unexpected response to hello"
                Left message -> giveUp handle message
  where
    -- | 对不上就关连接并回错误
    giveUp handle message = do
        _ <- try (hClose handle) :: IO (Either IOException ())
        pure (Left message)

-- | 断开连接；重复调用不报错
closeClient :: Client -> IO ()
closeClient client = do
    _ <- try (hClose (clHandle client)) :: IO (Either IOException ())
    pure ()

-- | 一行请求一行响应；同一条连接上的请求串起来发
request :: Client -> ClientRequest -> IO (Either Text ServerResponse)
request client message = withMVar (clLock client) $ \_ -> do
    outcome <- try (exchange (clHandle client) message) :: IO (Either IOException (Either Text ServerResponse))
    pure $ case outcome of
        Left err -> Left (T.pack (describe err))
        Right answer -> answer

-- | 在句柄上发一行请求、读回一行响应
exchange :: Handle -> ClientRequest -> IO (Either Text ServerResponse)
exchange handle message = do
    BLC.hPutStr handle (encodeRequest message)
    BLC.hPutStr handle "\n"
    hFlush handle
    line <- BSC.hGetLine handle
    pure (decodeResponse (BL.fromStrict line))

-- | 登录；成功返回是不是管理员
clientLogin :: Client -> Text -> Text -> IO (Either Text Bool)
clientLogin client user password = do
    answer <- request client (ReqLogin user password)
    case answer of
        Left message -> pure (Left message)
        Right (RespLogin _ admin) -> pure (Right admin)
        Right other -> pure (failed "login" other)

-- | 跑一条语句，拿回结果集
clientQuery :: Client -> Text -> IO (Either Text QueryResult)
clientQuery client sql = do
    answer <- request client (ReqQuery sql)
    case answer of
        Left message -> pure (Left message)
        Right (RespResult result) -> pure (Right result)
        Right other -> pure (failed "a query" other)

-- | 探活
clientPing :: Client -> IO (Either Text ())
clientPing client = do
    answer <- request client ReqPing
    case answer of
        Left message -> pure (Left message)
        Right RespPong -> pure (Right ())
        Right other -> pure (failed "a ping" other)

-- | 数据字典（\\dt 与 \\d 用）
clientCatalog :: Client -> IO (Either Text [TableInfo])
clientCatalog client = do
    answer <- request client ReqCatalog
    case answer of
        Left message -> pure (Left message)
        Right (RespCatalog infos) -> pure (Right infos)
        Right other -> pure (failed "a catalog" other)

-- | 库清单（\\l 用）
clientDatabases :: Client -> IO (Either Text [Text])
clientDatabases client = do
    answer <- request client ReqDatabases
    case answer of
        Left message -> pure (Left message)
        Right (RespDatabases names) -> pure (Right names)
        Right other -> pure (failed "a database list" other)

-- | 角色总览（\\dr 用）
clientRoles :: Client -> IO (Either Text [RoleView])
clientRoles client = do
    answer <- request client ReqRoles
    case answer of
        Left message -> pure (Left message)
        Right (RespRoles roles) -> pure (Right roles)
        Right other -> pure (failed "a role list" other)

-- | 账号清单（\\du 与管理页用）
clientAccounts :: Client -> IO (Either Text [Account])
clientAccounts client = do
    answer <- request client ReqAccounts
    case answer of
        Left message -> pure (Left message)
        Right (RespAccounts accounts) -> pure (Right accounts)
        Right other -> pure (failed "an account list" other)

-- | 当前生效的口令策略（最短长度、至少几类字符）
clientPolicy :: Client -> IO (Either Text (Int, Int))
clientPolicy client = do
    answer <- request client ReqPolicy
    case answer of
        Left message -> pure (Left message)
        Right (RespPolicy minLength classes) -> pure (Right (minLength, classes))
        Right other -> pure (failed "a password policy" other)

-- | 让 server 重读它自己的设置文件并换上新的口令策略
clientReloadPolicy :: Client -> IO (Either Text (Int, Int))
clientReloadPolicy client = do
    answer <- request client ReqReloadPolicy
    case answer of
        Left message -> pure (Left message)
        Right (RespPolicy minLength classes) -> pure (Right (minLength, classes))
        Right other -> pure (failed "a password policy reload" other)

-- | 应答形状对不上时的错误，error 应答取码与说明
failed :: Text -> ServerResponse -> Either Text a
failed what response = case response of
    RespError code message -> Left (code <> ": " <> message)
    _ -> Left ("unexpected response to " <> what)

-- | 建连接（套接字转成文本模式的行缓冲句柄）
openConnection :: Text -> Int -> IO Handle
openConnection host port = do
    let hints = defaultHints{addrSocketType = Stream}
    infos <- getAddrInfo (Just hints) (Just (T.unpack host)) (Just (show port))
    case infos of
        [] -> ioError (userError ("cannot resolve server address: " ++ address host port))
        (info : _) -> do
            sock <- socket (addrFamily info) Stream defaultProtocol
            connect sock (addrAddress info)
            handle <- socketToHandle sock ReadWriteMode
            hSetBuffering handle LineBuffering
            hSetNewlineMode handle noNewlineTranslation
            pure handle

-- | 把主机端口拼成地址字样
address :: Text -> Int -> String
address host port = T.unpack host ++ ":" ++ show port

-- | 断线给人的说法：EOF 就是 server 把连接关了
describe :: IOException -> String
describe err
    | isEOFError err = "the server closed the connection"
    | otherwise = show err
