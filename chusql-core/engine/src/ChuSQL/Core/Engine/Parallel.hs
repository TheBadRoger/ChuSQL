module ChuSQL.Core.Engine.Parallel (
    parallelShardLimit,
    Pool,
    workerPool,
    poolRun,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (SomeException, throwIO, try)
import System.IO.Unsafe (unsafePerformIO)

-- 并行执行：常驻工作线程池，结果按提交顺序收回。

-- | 单次并行扫描最多切几片
parallelShardLimit :: Int
parallelShardLimit = 8

-- | 池：待办队列与唤醒信号
data Pool = Pool
    { poolJobs :: MVar [IO ()]
    , poolReady :: MVar ()
    }

{-# NOINLINE poolRef #-}
-- | 进程全局的池，按容量复用
poolRef :: MVar (Maybe (Int, Pool))
poolRef = unsafePerformIO (newMVar Nothing)

-- | 按容量取池，不够就新建一个
workerPool :: Int -> IO Pool
workerPool size = modifyMVar poolRef reuse
  where
    -- | 现有池够大就留着，否则换新的
    reuse current = case current of
        Just (capacity, pool)
            | capacity >= wanted -> pure (current, pool)
        _ -> do
            pool <- startPool wanted
            pure (Just (wanted, pool), pool)

    -- | 池的线程数至少一个
    wanted = max 1 size

    -- | 起 wanted 个常驻线程吃队列
    startPool n = do
        jobs <- newMVar []
        ready <- newMVar ()
        sequence_ [forkIO (worker jobs ready) | _ <- [1 .. n]]
        pure (Pool jobs ready)

-- | 一个工作线程：被唤醒一次就取一个待办
worker :: MVar [IO ()] -> MVar () -> IO ()
worker jobs ready = do
    takeMVar ready
    job <- modifyMVar jobs pick
    case job of
        Nothing -> worker jobs ready
        Just act -> act >> worker jobs ready
  where
    -- | 取队首，空队列就回 Nothing
    pick [] = pure ([], Nothing)
    pick (j : js) = pure (js, Just j)

-- | 往池里丢一个待办
submit :: Pool -> IO () -> IO ()
submit pool job = do
    modifyMVar_ (poolJobs pool) (pure . (++ [job]))
    putMVar (poolReady pool) ()

-- | 跑一个动作，结果或异常都填进格子
fillCell :: MVar (Either SomeException a) -> IO a -> IO ()
fillCell cell act = try act >>= putMVar cell

-- | 并发跑一批动作，按输入顺序返回结果与异常
poolRun :: Pool -> [IO a] -> IO [a]
poolRun _ [] = pure []
poolRun pool actions = do
    cells <- mapM (const newEmptyMVar) actions
    sequence_ [submit pool (fillCell cell act) | (cell, act) <- zip cells actions]
    outcomes <- mapM takeMVar cells
    mapM (either throwIO pure) outcomes
