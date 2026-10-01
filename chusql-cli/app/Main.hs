{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.CLI.Format (OutputFormat (..), formatName, parseFormat)
import ChuSQL.CLI.Script (stripTerminator)
import ChuSQL.CLI.Session (
    Session,
    authenticateSession,
    isPlainIdentifier,
    newSession,
    switchDatabase,
 )
import ChuSQL.Web.Auth (Credential (..))
import ChuSQL.Web.Backend (ipcBackend)
import ChuSQL.Web.Config (WebConfig (..), defaultPassword, loadWebConfigAt, resolveCredential, resolvePipeName)
import ChuSQL.Web.Settings (readSettingsFile)
import ChuSQL.Web.TOML (resolveConfigPath)
import ChuSQL.Web.StorageProcess (
    StorageProcess,
    findStorageServer,
    startStorageProcessAt,
    stopStorageProcess,
    waitForStorage,
 )
import ChuSQL.Storage.IPC (setPipeName)
import Control.Exception (finally)
import Control.Monad (unless)
import Data.IORef (newIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Options.Applicative
import Repl (reportError, runRepl, runStatementOnce)
import System.Console.Haskeline (defaultSettings, getPassword, runInputT)
import System.Directory (getHomeDirectory)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.IO (hSetEncoding, stderr, stdout, utf8)

-- chusql-cli：与 Web 管理端并列的前端，直连存储与引擎，账号与 Web 共用。

data Options = Options
    { optUser :: Maybe Text
    , optPromptPassword :: Bool
    , optDatabase :: Text
    , optFormat :: OutputFormat
    , optExecute :: Maybe Text
    , optHistory :: Maybe FilePath
    , optPipe :: Maybe String
    , optStorage :: Maybe FilePath
    , optConfig :: Maybe FilePath
    }

optionsParser :: Parser Options
optionsParser =
    Options
        <$> optional (T.pack <$> strOption (long "user" <> short 'u' <> metavar "USER" <> help "Sign in as this account (default: the configured administrator)"))
        <*> switch (long "password" <> short 'p' <> help "Always ask for the password, ignoring the configured one")
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
                <> help "Output format: table, json or csv"
            )
        <*> optional (T.pack <$> strOption (long "execute" <> short 'e' <> metavar "SQL" <> help "Run one statement and exit"))
        <*> optional (strOption (long "history" <> metavar "FILE" <> help "SQL history file (default ~/.chusql_history)"))
        <*> optional (strOption (long "pipe" <> metavar "NAME" <> help "Endpoint name of the storage process (Windows named pipe, Unix socket file; default: [server] pipe_name in chusql.toml)"))
        <*> optional (strOption (long "storage" <> metavar "SERVER" <> help "Rust storage executable to spawn when nothing answers the endpoint"))
        <*> optional (strOption (long "config" <> metavar "FILE" <> help "Config file to read (default: the fixed chusql.toml under the system config directory)"))

formatReader :: String -> Either String OutputFormat
formatReader raw = maybe (Left "format must be table, json or csv") Right (parseFormat (T.pack raw))

parserInfo :: ParserInfo Options
parserInfo =
    info
        (optionsParser <**> helper)
        ( fullDesc
            <> progDesc "Run SQL against the ChuSQL storage engine"
            <> header "csql - the ChuSQL command line client"
        )

main :: IO ()
main = do
    hSetEncoding stdout utf8
    hSetEncoding stderr utf8
    options <- execParser parserInfo
    configPath <- resolveConfigPath (optConfig options)
    base <- loadWebConfigAt configPath
    let settingsFile = configPath
    saved <- readSettingsFile settingsFile
    credential <- resolveCredential base saved
    let rootName = credUser credential
        userName = fromMaybe rootName (optUser options)
        pipeName = resolvePipe options base
    setPipeName pipeName
    password <- resolvePassword (optPromptPassword options) userName rootName saved
    storage <- acquireStorage options pipeName settingsFile
    case storage of
        Left message -> reportError message >> exitFailure
        Right spawned ->
            connect options credential settingsFile userName password
                `finally` maybe (pure ()) stopStorageProcess spawned

-- | 接上存储，登录，然后进交互或跑单条语句
connect :: Options -> Credential -> FilePath -> Text -> Text -> IO ()
connect options credential settingsFile userName password = do
    backend <- ipcBackend
    session <- newSession backend credential settingsFile
    signedIn <- authenticateSession session userName password
    case signedIn of
        Left message -> reportError message >> exitFailure
        Right () -> runSession options session

runSession :: Options -> Session -> IO ()
runSession options session = do
    formatRef <- newIORef (optFormat options)
    ready <- selectDatabase session (T.strip (optDatabase options))
    if not ready
        then exitFailure
        else case optExecute options of
            Just sql -> do
                ok <- runStatementOnce session (optFormat options) (stripTerminator sql)
                unless ok exitFailure
            Nothing -> do
                historyFile <- resolveHistory (optHistory options)
                runRepl session formatRef historyFile

-- | 只有 -d 明确给了一个库才切；没给就保持"未选库"（服务没有默认库）
selectDatabase :: Session -> Text -> IO Bool
selectDatabase _ wanted | T.null wanted = pure True
selectDatabase session wanted
    | not (isPlainIdentifier wanted) = reportError ("not a plain database name: " <> wanted) >> pure False
    | otherwise = do
        switched <- switchDatabase session wanted
        case switched of
            Left message -> reportError message >> pure False
            Right () -> pure True

-- | 口令：-p 强制交互输入；否则配置里的 root 明文（同 Web）> 交互输入
resolvePassword :: Bool -> Text -> Text -> Map.Map Text Text -> IO Text
resolvePassword forcePrompt userName rootName saved
    | forcePrompt = promptPassword userName
    | T.toLower userName == T.toLower rootName = pure (configuredPassword saved)
    | otherwise = promptPassword userName
  where
    configuredPassword values = case nonEmpty (Map.lookup "password" values) of
        Just plain -> plain
        Nothing -> defaultPassword

promptPassword :: Text -> IO Text
promptPassword user = do
    entered <- runInputT defaultSettings (getPassword (Just '*') (T.unpack user <> "'s password: "))
    pure (maybe "" T.pack entered)

-- | 先找已经在跑的存储；--storage 给了可执行文件才自己拉一个
acquireStorage :: Options -> String -> FilePath -> IO (Either Text (Maybe StorageProcess))
acquireStorage options pipeName configPath = do
    up <- waitForStorage 4
    case optStorage options of
        Nothing
            | up -> pure (Right Nothing)
            | otherwise ->
                pure
                    ( Left
                        ( "no storage process answered on pipe "
                            <> T.pack pipeName
                            <> "; start the server first, or pass --storage <path>"
                        )
                    )
        Just given
            | up -> pure (Right Nothing)
            | otherwise -> do
                found <- findStorageServer (Just given)
                case found of
                    Left err -> pure (Left (T.pack err))
                    Right binary -> fmap (either (Left . T.pack) (Right . Just)) (startStorageProcessAt binary pipeName configPath)

-- | 管名：--pipe > chusql.toml 的 [server] pipe_name（缺省 chusql-joint）
resolvePipe :: Options -> WebConfig -> String
resolvePipe options base = fromMaybe (resolvePipeName base) (optPipe options)

nonEmpty :: Maybe Text -> Maybe Text
nonEmpty given = case T.strip <$> given of
    Just text | not (T.null text) -> Just text
    _ -> Nothing

resolveHistory :: Maybe FilePath -> IO (Maybe FilePath)
resolveHistory (Just path) = pure (Just path)
resolveHistory Nothing = do
    home <- getHomeDirectory
    pure (Just (home </> ".chusql_history"))
