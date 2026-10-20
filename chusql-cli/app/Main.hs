{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.CLI.Format (OutputFormat (..), formatName, parseFormat)
import ChuSQL.CLI.Password (readPasswordInput)
import ChuSQL.CLI.Script (stripTerminator)
import ChuSQL.Server.Config (ServerConfig (..), loadServerConfigAt, loadRuntimeConfigAt, rootUserName)
import ChuSQL.Driver.Link (Client, closeClient, connectClient)
import ChuSQL.Driver.Session (
    Session,
    authenticateSession,
    isPlainIdentifier,
    newSession,
    switchDatabase,
 )
import ChuSQL.Server.TOML (resolveConfigPath)
import Control.Exception (finally)
import Control.Monad (unless)
import Data.IORef (IORef, newIORef)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import Repl (reportError, runRepl, runStatementOnce)
import System.Console.Haskeline (defaultSettings, getPassword, runInputT)
import System.Directory (getHomeDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hFlush, hIsTerminalDevice, hPutStr, hSetEncoding, stderr, stdin, stdout, utf8)

-- 命令行客户端通过独立驱动连接数据库服务。

data Options = Options
    { optUser :: Maybe Text
    , optDatabase :: Text
    , optFormat :: OutputFormat
    , optExecute :: Maybe Text
    , optHistory :: Maybe FilePath
    , optConfig :: Maybe FilePath
    }

-- | 命令行参数解析器
optionsParser :: Parser Options
optionsParser =
    Options
        <$> optional (T.pack <$> strOption (long "user" <> short 'u' <> metavar "USER" <> help "Sign in as this account (default: root)"))
        <*> ( T.pack
                <$> strOption
                    ( long "database"
                        <> short 'd'
                        <> metavar "DB"
                        <> value ""
                        <> help "Database to use (default: none, select one with USE)"
                    )
            )
        <*> option
            (eitherReader formatReader)
            ( long "format"
                <> short 'f'
                <> metavar "FORMAT"
                <> value FormatTable
                <> showDefaultWith (T.unpack . formatName)
                <> help "Output format: table (or text), json or csv"
            )
        <*> optional (T.pack <$> strOption (long "execute" <> short 'e' <> metavar "SQL" <> help "Run one statement and exit"))
        <*> optional (strOption (long "history" <> metavar "FILE" <> help "SQL history file (default ~/.chusql_history)"))
        <*> optional (strOption (long "config" <> metavar "FILE" <> help "Config file to read (default: the fixed settings.toml under the system config directory)"))

-- | 把 --format 的取值解析成格式
formatReader :: String -> Either String OutputFormat
formatReader "text" = Right FormatTable
formatReader raw = maybe (Left "format must be table, text, json or csv") Right (parseFormat (T.pack raw))

-- | 命令行界面与帮助
parserInfo :: ParserInfo Options
parserInfo =
    info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Run SQL against the ChuSQL storage engine"
            <> header "csql - the ChuSQL command line client"
        )

-- | 解析参数、登录 server 后进入会话
main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    options <- execParser parserInfo
    configPath <- resolveConfigPath (optConfig options)
    base <- loadRuntimeConfigAt configPath
    config <- loadServerConfigAt configPath
    let rootName = rootUserName base
        userName = fromMaybe rootName (optUser options)
    password <- promptPassword userName
    connected <- connectClient (T.pack (scHost config)) (scPort config)
    case connected of
        Left message -> reportError message >> exitFailure
        Right client ->
            connect options client userName password
                `finally` closeClient client

-- | 连 server、登录，之后转入会话
connect :: Options -> Client -> Text -> Text -> IO ()
connect options client userName password = do
    session <- newSession client
    signedIn <- authenticateSession session userName password
    case signedIn of
        Left message -> reportError message >> exitFailure
        Right () -> runSession options session

-- | 执行 -e 的单条语句，否则进交互循环
runSession :: Options -> Session -> IO ()
runSession options session = do
    formatRef <- newIORef (optFormat options)
    ready <- selectDatabase session (T.strip (optDatabase options))
    if not ready
        then exitFailure
        else runSqlSession options session formatRef

-- 执行 SQL 或进入交互循环。
runSqlSession :: Options -> Session -> IORef OutputFormat -> IO ()
runSqlSession options session formatRef = case optExecute options of
    Just sql -> do
        ok <- runStatementOnce session (optFormat options) (stripTerminator sql)
        unless ok exitFailure
    Nothing -> do
        historyFile <- resolveHistory (optHistory options)
        runRepl session formatRef historyFile

-- | -d 指定了库才切，否则保持未选库
selectDatabase :: Session -> Text -> IO Bool
selectDatabase _ wanted | T.null wanted = pure True
selectDatabase session wanted
    | not (isPlainIdentifier wanted) = reportError ("not a plain database name: " <> wanted) >> pure False
    | otherwise = do
        switched <- switchDatabase session wanted
        case switched of
            Left message -> reportError message >> pure False
            Right () -> pure True

-- | 交互式读取口令；直接回车表示空口令
promptPassword :: Text -> IO Text
promptPassword user = do
    terminal <- hIsTerminalDevice stdin
    if terminal
        then do
            entered <- runInputT defaultSettings (getPassword (Just '*') (T.unpack user <> "'s password: "))
            pure (maybe "" T.pack entered)
        else do
            hPutStr stderr (T.unpack user <> "'s password: ")
            hFlush stderr
            readPasswordInput stdin

-- | 取历史文件路径，没给就用默认
resolveHistory :: Maybe FilePath -> IO (Maybe FilePath)
resolveHistory (Just path) = pure (Just path)
resolveHistory Nothing = do
    home <- getHomeDirectory
    pure (Just (home </> ".chusql_history"))
