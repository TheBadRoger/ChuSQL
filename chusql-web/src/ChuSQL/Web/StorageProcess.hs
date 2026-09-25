{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.StorageProcess (
    StorageProcess (..),
    findStorageServer,
    startStorageProcess,
    stopStorageProcess,
    waitForStorage,
) where

import ChuSQL.Storage.IPC (Request (..), Response (..), closeConnection, sendRequest)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, try)
import Control.Monad (filterM)
import Data.Time.Clock.POSIX (getPOSIXTime)
import System.Directory (createDirectoryIfMissing, doesFileExist, getTemporaryDirectory)
import System.Environment (getEnvironment, setEnv)
import System.Exit (ExitCode)
import System.FilePath ((</>))
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

data StorageProcess = StorageProcess
    { spProcess :: ProcessHandle
    , spPipe :: String
    , spDataDir :: FilePath
    }

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
        exes = ["chusql-storage.exe", "chusql-storage"]
        candidates = [d </> m </> e | d <- dirs, m <- modes, e <- exes]
    found <- filterM doesFileExist candidates
    pure $ case found of
        (p : _) -> Right p
        [] -> Left "no Rust storage server found; run `cargo build --release` in chusql-storage, or pass --storage <path>"
-- | 起存储进程并等它就绪
startStorageProcess :: FilePath -> IO (Either String StorageProcess)
startStorageProcess bin = do
    stamp <- round . (* 1000000) <$> getPOSIXTime :: IO Integer
    tmp <- getTemporaryDirectory
    let pipe = "chusql-web-" ++ show stamp
        dir = tmp </> pipe
    createDirectoryIfMissing True dir
    envs <-
        childEnv
            [ ("CHUSQL_PIPE", pipe)
            , ("CHUSQL_DATA_DIR", dir)
            ]
    started <-
        try (createProcess (proc bin []){env = Just envs, std_out = NoStream, std_err = Inherit}) ::
            IO (Either SomeException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
    case started of
        Left e -> pure (Left ("failed to start the storage process: " ++ show e))
        Right (_, _, _, ph) -> do
            setEnv "CHUSQL_PIPE" pipe
            setEnv "CHUSQL_DATA_DIR" dir
            ready <- waitForStorage (200 :: Int)
            if ready
                then pure (Right (StorageProcess ph pipe dir))
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

-- | 子进程环境：继承并覆盖两个专用变量
childEnv :: [(String, String)] -> IO [(String, String)]
childEnv extra = do
    base <- getEnvironment
    let ours = ["CHUSQL_PIPE", "CHUSQL_DATA_DIR"]
        kept = filter (\(k, _) -> k `notElem` ours) base
        missing (k, _) = not (any ((== k) . fst) kept)
        defaults = filter missing storageDefaults
    pure (kept ++ defaults ++ extra)

-- | 存储进程的调参默认值（可以被用户环境里的同名变量覆盖）
storageDefaults :: [(String, String)]
storageDefaults =
    [ ("CHUSQL_PAGE_SIZE", "4096")
    , ("CHUSQL_BTREE_ORDER", "4")
    , ("CHUSQL_LOG", "info")
    ]
