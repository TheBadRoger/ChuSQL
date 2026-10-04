{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.Core.Engine.Storage.IPC (closeConnection, localStorageLink, sendRawRequest, setStorageLink)
import ChuSQL.Bootstrap (preinstalledTypes, runBootstrap)
import ChuSQL.Core.Engine.Storage.FFI (maintainStorage)
import ChuSQL.Interface.Auth (hashPassword)
import ChuSQL.Interface.Config (loadWebConfigAt, rootUserName)
import ChuSQL.Interface.TOML (resolveConfigPath)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (finally)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (BufferMode (LineBuffering), hSetBuffering, stdout)

-- 引导程序：初始化系统库、管理员身份与内置类型目录。

data Options = Options
    { optConfig :: Maybe FilePath
    , optUser :: Maybe String
    , optPasswordStdin :: Bool
    , optPasswordless :: Bool
    , optHelp :: Bool
    , optMode :: Maybe String
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
        , optMode = Nothing
        }

-- | 解析参数
parseArgs :: [String] -> Either String Options
parseArgs = go emptyOptions
  where
    -- | 逐条消费参数列表
    go opts [] = Right opts
    go opts (command : rest)
        | command `elem` ["repair", "recover", "recovery", "reset"] = case optMode opts of
            Nothing -> go opts{optMode = Just (if command == "recovery" then "recover" else command)} rest
            Just _ -> Left "only one maintenance command is allowed"
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
    putStrLn "usage: csql-bootstrap [repair|recover|recovery|reset] [options]"
    putStrLn "  repair    repair structures; abort if data recovery or initialization is needed"
    putStrLn "  recover   replay WAL and recover system metadata; recovery is an alias"
    putStrLn "  reset     replace system identities and grants with initial defaults"
    putStrLn "Stop all database processes first. Maintenance preserves the old system directory."
    putStrLn ""
    putStrLn "config: settings.toml sits at a fixed place (Windows: %APPDATA%\\ChuSQL\\settings.toml;"
    putStrLn "        other: $XDG_CONFIG_HOME/ChuSQL/settings.toml), the same file the server reads"
    putStrLn ""
    putStrLn "  --config FILE          read that config file instead"
    putStrLn "  --user NAME            administrator name          (default root)"
    putStrLn "  --password-stdin       read the administrator password from one line of stdin"
    putStrLn "  --passwordless         leave the administrator without a password"
    putStrLn ""
    putStrLn "what it does: initializes the system database, an enabled login superuser and"
    putStrLn "  the preinstalled builtin type directory, then exits. Existing passwords and"
    putStrLn "  matching type definitions are preserved; conflicting definitions fail."

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

-- | 读取配置和口令并完成安装引导
run :: Options -> IO ()
run opts | Just mode <- optMode opts = runMaintenance opts mode
run opts = do
    configPath <- resolveConfigPath (optConfig opts)
    base <- loadWebConfigAt configPath
    let name = maybe (rootUserName base) T.pack (optUser opts)
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
                    result <- runBootstrap send name encoded `finally` closeConnection
                    case result of
                        Left err -> failWith ("bootstrap failed: " ++ err)
                        Right count -> do
                            putStrLn "system catalog: ready"
                            putStrLn "administrator identity: LOGIN SUPERUSER ENABLED"
                            putStrLn ("preinstalled types: ready (" ++ show count ++ " definitions)")

-- | 在离线维护入口执行所选策略
runMaintenance :: Options -> String -> IO ()
runMaintenance opts mode = do
    path <- resolveConfigPath (optConfig opts)
    chosen <- if mode == "reset" || optPasswordStdin opts || optPasswordless opts then readPassword opts else pure (Right "")
    case chosen of
        Left err -> failWith err
        Right plain -> do
            encoded <- if T.null plain then pure "" else hashPassword plain
            let request = A.object ["mode" A..= mode, "user" A..= maybe "root" id (optUser opts), "password_hash" A..= encoded, "types" A..= preinstalledTypes]
            reply <- maintainStorage path (BL.toStrict (A.encode request))
            case reply of
                Left err -> failWith err
                Right value -> BL.putStrLn (BL.fromStrict value)

-- | 通过存储链路发送引导请求
send :: A.Value -> IO (Either String A.Value)
send request = do
    reply <- sendRawRequest (BL.toStrict (A.encode request))
    pure (reply >>= A.eitherDecodeStrict)

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
