{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.Core.Engine.Storage.IPC (closeConnection, localStorageLink, sendRawRequest, setStorageLink)
import ChuSQL.Interface.Auth (hashPassword)
import ChuSQL.Interface.Config (loadWebConfigAt, rootUserName)
import ChuSQL.Interface.Settings (readSettingsFile)
import ChuSQL.Interface.TOML (resolveConfigPath)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

-- 引导程序：建系统库与管理员账号，装完即退，留在 bin 里备用。

data Options = Options
    { optConfig :: Maybe FilePath
    , optUser :: Maybe String
    , optPasswordStdin :: Bool
    , optPasswordless :: Bool
    , optHelp :: Bool
    }

-- | 全部选项都取默认值
emptyOptions :: Options
emptyOptions =
    Options
        { optConfig = Nothing
        , optUser = Nothing
        , optPasswordStdin = False
        , optPasswordless = False
        , optHelp = False
        }

-- | 解析参数
parseArgs :: [String] -> Either String Options
parseArgs = go emptyOptions
  where
    -- | 逐条消费参数列表
    go opts [] = Right opts
    go opts ("--help" : rest) = go opts{optHelp = True} rest
    go opts ("-h" : rest) = go opts{optHelp = True} rest
    go opts ("--config" : v : rest) = go opts{optConfig = Just v} rest
    go opts ("--user" : v : rest) = go opts{optUser = Just v} rest
    go opts ("--password-stdin" : rest) = go opts{optPasswordStdin = True} rest
    go opts ("--passwordless" : rest) = go opts{optPasswordless = True} rest
    go _ (a : _) = Left ("unrecognized argument or missing value: " ++ a)

-- | 打印用法说明
usage :: IO ()
usage = do
    putStrLn "usage: csql-bootstrap [options]"
    putStrLn ""
    putStrLn "config: chusql.toml sits at a fixed place (Windows: %APPDATA%\\ChuSQL\\chusql.toml;"
    putStrLn "        other: $XDG_CONFIG_HOME/ChuSQL/chusql.toml), the same file the server reads"
    putStrLn ""
    putStrLn "  --config FILE          read that config file instead"
    putStrLn "  --user NAME            administrator name          ([web] user, default root)"
    putStrLn "  --password-stdin       read the administrator password from one line of stdin"
    putStrLn "  --passwordless         leave the administrator without a password"
    putStrLn ""
    putStrLn "what it does: creates the system database, the __system_users table and the"
    putStrLn "  administrator row, then exits. It never changes an existing account, and it"
    putStrLn "  never deletes data, so the installer and later repairs can run it again."

-- | 入口
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    args <- getArgs
    case parseArgs args of
        Left err -> do
            putStrLn ("bad arguments: " ++ err)
            usage
            exitFailure
        Right opts
            | optHelp opts -> usage
            | otherwise -> run opts

-- | 读配置、算口令哈希、发一条引导请求
run :: Options -> IO ()
run opts = do
    configPath <- resolveConfigPath (optConfig opts)
    base <- loadWebConfigAt configPath
    saved <- readSettingsFile configPath
    let name = maybe (rootUserName base saved) T.pack (optUser opts)
    chosen <- readPassword opts
    case chosen of
        Left err -> do
            putStrLn ("!! " ++ err)
            usage
            exitFailure
        Right plain -> do
            encoded <- if T.null plain then pure "" else hashPassword plain
            putStrLn "================== ChuSQL Bootstrap ================="
            putStrLn ("config file:   " ++ configPath)
            putStrLn ("administrator: " ++ T.unpack name)
            putStrLn
                ( if T.null encoded
                    then "password:      none yet (sign in with an empty one, then set it)"
                    else "password:      set (stored hashed in the system table)"
                )
            opened <- localStorageLink (Just configPath)
            case opened of
                Left err -> failWith ("cannot open the storage library: " ++ err)
                Right link -> do
                    setStorageLink link
                    reply <- sendRawRequest (BL.toStrict (A.encode (bootstrapRequest name encoded)))
                    closeConnection
                    report reply

-- | 引导请求：建表、插管理员行
bootstrapRequest :: Text -> Text -> A.Value
bootstrapRequest name encoded =
    A.object
        [ "method" A..= ("bootstrap_system" :: Text)
        , "user" A..= name
        , "password_hash" A..= encoded
        ]

-- | 看应答：system 就是成功，其余按错误处理
report :: Either String BS.ByteString -> IO ()
report (Left err) = failWith ("cannot talk to the storage library: " ++ err)
report (Right raw) = case A.eitherDecodeStrict raw of
    Right (A.Object fields)
        | KM.lookup "status" fields == Just (A.String "system") ->
            putStrLn "system catalog: ready"
        | otherwise ->
            failWith ("bootstrap failed: " ++ message fields)
    Right _ -> failWith ("unexpected reply from the storage library: " ++ show raw)
    Left err -> failWith ("the storage library answered something that is not JSON: " ++ err)
  where
    -- | 取错误文本
    message fields = case KM.lookup "message" fields of
        Just (A.String text) -> T.unpack text
        _ -> show raw

-- | 读口令：stdin 一行，空行等于不设口令
readPassword :: Options -> IO (Either String Text)
readPassword opts
    | optPasswordless opts = pure (Right "")
    | optPasswordStdin opts = do
        line <- TIO.getLine
        pure (Right (T.dropWhileEnd (\c -> c == '\r' || c == '\n') line))
    | otherwise = pure (Left "pass --password-stdin or --passwordless")

-- | 打印错误并退出 1
failWith :: String -> IO a
failWith message = do
    putStrLn ("!! " ++ message)
    exitFailure
