module ChuSQL.Core.Engine.Parallel (
    parallelShardLimit,
    Pool,
    workerPool,
    poolRun,
) where

import Control.Concurrent (forkIO)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar, newEmptyMVar, newMVar, putMVar, takeMVar)
import Control.Exception (SomeException, throwIO, try)
import Control.Monad (forever)
import System.IO.Unsafe (unsafePerformIO)

-- 并行执行：常驻工作线程池，结果按提交顺序收回。

-- | 单次并行扫描最多切几片
parallelShardLimit :: Int
parallelShardLimit = 8

-- | 池：工作线程共享的待办队列
newtype Pool = Pool (Chan (IO ()))

{-# NOINLINE poolRef #-}
-- | 进程全局的池，按容量复用
poolRef :: MVar (Maybe (Int, Pool))
poolRef = unsafePerformIO (newMVar Nothing)

-- | 按容量取池，不够就新建一个
workerPool :: Int -> IO Pool
workerPool size = modifyMVar poolRef reuse
  where
    -- | 复用队列并按需补充工作线程
    reuse current = case current of
        Just (capacity, pool@(Pool jobs))
            | capacity >= wanted -> pure (current, pool)
            | otherwise -> do
                sequence_ [forkIO (worker jobs) | _ <- [capacity + 1 .. wanted]]
                pure (Just (wanted, pool), pool)
        Nothing -> do
            pool <- startPool wanted
            pure (Just (wanted, pool), pool)

    -- | 池的线程数至少一个
    wanted = max 1 size

    -- | 起 wanted 个常驻线程吃队列
    startPool n = do
        jobs <- newChan
        sequence_ [forkIO (worker jobs) | _ <- [1 .. n]]
        pure (Pool jobs)

-- | 一个工作线程：等待并执行队首待办
worker :: Chan (IO ()) -> IO ()
worker jobs = forever (readChan jobs >>= id)

-- | 往池里丢一个待办
submit :: Pool -> IO () -> IO ()
submit (Pool jobs) = writeChan jobs

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
