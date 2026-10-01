{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.StorageProcess (
    StorageProcess (..),
    findStorageServer,
    startStorageProcess,
    startStorageProcessAt,
    platformBinaryName,
    storageChildArgs,
    stopStorageProcess,
    waitForStorage,
) where

import ChuSQL.Storage.IPC (Request (..), Response (..), closeConnection, sendRequest, setPipeName)
import ChuSQL.Web.Config (loadWebConfigAt, resolvePipeName)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import System.Directory (doesFileExist)
import System.Exit (ExitCode)
import System.FilePath ((</>))
import System.Info (os)
import System.IO (Handle)
import System.Process (
    CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    proc,
    terminateProcess,
    waitForProcess,
 )

-- 拉起并管理 Rust 存储进程，起来后等管道应答 ping 再继续。
-- 调参、管名与数据目录都在全局 chusql.toml 里，孩子自己读；这里只负责起进程、
-- 用 --config 把「读哪份配置文件」告诉它，并等就绪。

data StorageProcess = StorageProcess
    { spProcess :: ProcessHandle
    , spPipe :: String
    }

-- | 可执行文件名跟平台走：Windows 带 .exe，其它平台不带。同一个 target 目录里可能
-- 同时躺着一份别处交叉编译出来的 .exe（Windows PE），在 Linux 上误选它只会拿到
-- config file not found（Windows 进程看不到 /tmp/... 这样的路径）。目标平台当参数传，
-- 两个分支都能被测试直接钉住。
platformBinaryName :: String -> String -> FilePath
platformBinaryName targetOs stem = if targetOs == "mingw32" then stem ++ ".exe" else stem

-- | 找存储进程，没有就给路径提示
findStorageServer :: Maybe FilePath -> IO (Either String FilePath)
findStorageServer (Just p) = do
    ok <- doesFileExist p
    pure (if ok then Right p else Left ("storage server not found: " ++ p))
findStorageServer Nothing = do
    let dirs =
            [ "chusql-storage/target"
            , "../chusql-storage/target"
            , "../../chusql-storage/target"
            , "target"
            ]
        modes = ["release", "debug"]
        exes = [platformBinaryName os "chusql-storage"]
        candidates = [d </> m </> e | d <- dirs, m <- modes, e <- exes]
    found <- filterM doesFileExist candidates
    pure $ case found of
        (p : _) -> Right p
        [] -> Left "no Rust storage server found; run `cargo build --release` in chusql-storage, or pass --storage <path>"

-- | 按配置文件里的管名起存储进程
startStorageProcess :: FilePath -> FilePath -> IO (Either String StorageProcess)
startStorageProcess bin configPath = do
    cfg <- loadWebConfigAt configPath
    startStorageProcessAt bin (resolvePipeName cfg) configPath

-- | 给存储子进程的参数：配置文件位置是固定的，唯一的显式覆盖就是写进 --config
storageChildArgs :: FilePath -> [String]
storageChildArgs configPath = ["--config", configPath]

-- | 用给定的管名起存储进程，并把本进程的端点也指到它；环境原样继承
startStorageProcessAt :: FilePath -> String -> FilePath -> IO (Either String StorageProcess)
startStorageProcessAt bin pipe configPath = do
    let child = (proc bin (storageChildArgs configPath)){std_out = NoStream, std_err = Inherit}
    started <-
        try (createProcess child) ::
            IO (Either SomeException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
    case started of
        Left e -> pure (Left ("failed to start the storage process: " ++ show e))
        Right (_, _, _, ph) -> do
            setPipeName pipe
            ready <- waitForStorage (200 :: Int)
            if ready
                then pure (Right (StorageProcess ph pipe))
                else do
                    _ <- try (terminateProcess ph) :: IO (Either SomeException ())
                    pure (Left "storage process did not become ready within 5 seconds (no ping reply on the pipe)")

-- | 停掉存储进程，保留数据目录
stopStorageProcess :: StorageProcess -> IO ()
stopStorageProcess sp = do
    closeConnection
    _ <- try (terminateProcess (spProcess sp)) :: IO (Either SomeException ())
    _ <- try (waitForProcess (spProcess sp)) :: IO (Either SomeException ExitCode)
    pure ()

-- | 轮询 ping 直到对面应答
waitForStorage :: Int -> IO Bool
waitForStorage 0 = pure False
waitForStorage n = do
    r <- try (sendRequest ReqPing) :: IO (Either SomeException Response)
    case r of
        Right RespPong -> pure True
        _ -> threadDelay 25000 >> waitForStorage (n - 1)
