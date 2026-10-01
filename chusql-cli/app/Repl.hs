{-# LANGUAGE OverloadedStrings #-}

module Repl
    ( runRepl
    , runStatementOnce
    , reportError
    ) where

import ChuSQL.CLI.Format (OutputFormat, formatName, renderResult, renderRows)
import ChuSQL.CLI.Script (Meta (..), errorHint, metaHelp, parseMeta, takeStatement)
import ChuSQL.CLI.Session (Session, accountTable, catalog, databases, roleViews, runStatement, sessionDatabase, switchDatabase)
import ChuSQL.Storage.IPC (SchemaColumn (..), TableInfo (..))
import ChuSQL.Web.Privileges (Grant (..), RoleView (..))
import Control.Monad (forM_, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef (IORef, readIORef, writeIORef)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Console.Haskeline
import System.IO (hIsTerminalDevice, stderr, stdin)

-- 交互层：提示符、多行输入、历史、元命令与错误提示。

-- | 交互循环；historyPath 为 Nothing 就不落盘历史
runRepl :: Session -> IORef OutputFormat -> Maybe FilePath -> IO ()
runRepl session formatRef historyPath =
    runInputT (defaultSettings{historyFile = historyPath}) (loop session formatRef T.empty)

-- | 入口处直接报错（-e 与启动阶段用）
reportError :: Text -> IO ()
reportError message = do
    TIO.hPutStrLn stderr ("chusql: error: " <> message)
    forM_ (errorHint message) (\hint -> TIO.hPutStrLn stderr ("hint: " <> hint))

-- | 跑一条语句并按格式打印；成功与否当退出码
runStatementOnce :: Session -> OutputFormat -> Text -> IO Bool
runStatementOnce session format sql = do
    result <- runStatement session sql
    case result of
        Left message -> reportError message >> pure False
        Right answer -> TIO.putStrLn (renderResult format answer) >> pure True

loop :: Session -> IORef OutputFormat -> Text -> InputT IO ()
loop session formatRef buffer = do
    prompt <- liftIO (promptFor session buffer)
    entered <- getInputLine (T.unpack prompt)
    case entered of
        Nothing -> do
            interactive <- liftIO (hIsTerminalDevice stdin)
            when interactive (outputStrLn "")
        Just raw -> do
            let line = T.strip (T.pack raw)
            if T.null line
                then loop session formatRef buffer
                else case (T.null buffer, parseMeta line) of
                    (True, Just meta) -> do
                        again <- runMeta session formatRef meta
                        when again (loop session formatRef T.empty)
                    _ -> do
                        let extended = if T.null buffer then line else buffer <> "\n" <> line
                        remaining <- drain session formatRef extended
                        loop session formatRef remaining

-- | 提示符带上当前库；续行换个样子，方便看出语句还没写完；管道输入时不出提示符
promptFor :: Session -> Text -> IO Text
promptFor session buffer = do
    interactive <- hIsTerminalDevice stdin
    if not interactive
        then pure ""
        else
            if not (T.null (T.strip buffer))
                then pure "     -> "
                else do
                    current <- sessionDatabase session
                    pure (if T.null current then "chusql> " else "chusql [" <> current <> "]> ")

-- | 把攒下来的内容里所有完整语句依次执行，返回没写完的残余
drain :: Session -> IORef OutputFormat -> Text -> InputT IO Text
drain session formatRef buffer = case takeStatement buffer of
    Nothing -> pure (T.strip buffer)
    Just (statement, rest) -> do
        when (not (T.null statement)) (runOne session formatRef statement)
        drain session formatRef rest

runOne :: Session -> IORef OutputFormat -> Text -> InputT IO ()
runOne session formatRef statement = do
    result <- liftIO (runStatement session statement)
    case result of
        Left message -> liftIO (reportError message)
        Right answer -> do
            format <- liftIO (readIORef formatRef)
            outputStrLn (T.unpack (renderResult format answer))

runMeta :: Session -> IORef OutputFormat -> Meta -> InputT IO Bool
runMeta session formatRef meta = case meta of
    MetaQuit -> pure False
    MetaHelp -> outputStrLn (T.unpack metaHelp) >> pure True
    MetaDatabases -> listDatabases session >> pure True
    MetaTables -> listTables session >> pure True
    MetaDescribe name -> describeTable session name >> pure True
    MetaRoles -> listRoles session >> pure True
    MetaConnect name -> do
        switched <- liftIO (switchDatabase session name)
        case switched of
            Left message -> liftIO (reportError message)
            Right () -> outputStrLn ("database is now " <> T.unpack name)
        pure True
    MetaSetFormat format -> do
        liftIO (writeIORef formatRef format)
        outputStrLn ("output format is now " <> T.unpack (formatName format))
        pure True
    MetaUnknown name -> outputStrLn ("unknown command: \\" <> T.unpack name <> " (try \\?)") >> pure True

-- | \l：库清单
listDatabases :: Session -> InputT IO ()
listDatabases session = do
    answer <- liftIO (databases session)
    case answer of
        Left message -> liftIO (reportError message)
        Right names -> outputStrLn (T.unpack (renderRows ["database"] (map (: []) names)))

-- | \dt：表清单（base 里补上账号表）
listTables :: Session -> InputT IO ()
listTables session = do
    answer <- liftIO (catalog session)
    case answer of
        Left message -> liftIO (reportError message)
        Right tables -> do
            allTables <- liftIO (withAccountTable session tables)
            outputStrLn (T.unpack (renderRows headers (map tableRow allTables)))
  where
    headers = ["table", "columns", "rows", "indexes"]
    tableRow info =
        [ T.pack (tiTable info)
        , tshow (length (tiColumns info))
        , tshow (tiRows info)
        , if null (tiIndexes info) then "-" else T.intercalate ", " (map T.pack (tiIndexes info))
        ]

-- | \d <table>：列结构
describeTable :: Session -> Text -> InputT IO ()
describeTable session name = do
    answer <- liftIO (catalog session)
    case answer of
        Left message -> liftIO (reportError message)
        Right tables -> do
            allTables <- liftIO (withAccountTable session tables)
            case filter (matches name) allTables of
                [] -> liftIO (reportError ("unknown table: " <> name))
                (info : _) -> outputStrLn (T.unpack (renderRows headers (map columnRow (tiColumns info))))
  where
    headers = ["column", "type", "primary", "unique", "nullable", "auto"]
    matches wanted info = T.toLower (T.pack (tiTable info)) == T.toLower wanted
    columnRow column =
        [ T.pack (scName column)
        , T.pack (scType column)
        , yesNo (scPrimaryKey column)
        , yesNo (scUnique column)
        , yesNo (scNullable column)
        , yesNo (scAutoIncrement column)
        ]
    yesNo True = "yes"
    yesNo False = "no"

-- | \dr：角色清单（授权对象与成员）
listRoles :: Session -> InputT IO ()
listRoles session = do
    answer <- liftIO (roleViews session)
    case answer of
        Left message -> liftIO (reportError message)
        Right roles -> outputStrLn (T.unpack (renderRows ["role", "grants", "members"] (map roleRow roles)))
  where
    roleRow view =
        [ roleName view
        , if null (roleGrants view) then "-" else T.intercalate ", " (map grantText (roleGrants view))
        , if null (roleMembers view) then "-" else T.intercalate ", " (roleMembers view)
        ]
    grantText grant = grantPrivilege grant <> " on " <> grantObject grant

-- | 当前库是 system 时，把账号表也列出来（与 Web 端一致）
withAccountTable :: Session -> [TableInfo] -> IO [TableInfo]
withAccountTable session tables = do
    current <- sessionDatabase session
    if current /= "system"
        then pure tables
        else do
            extra <- accountTable session
            pure (maybe tables (: tables) extra)

tshow :: Show a => a -> Text
tshow = T.pack . show
