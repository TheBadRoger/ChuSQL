module ChuSQL.Server.Security (readSecurity, writeSecurity, databaseSecurity, commitSecurity) where

import Control.Concurrent.MVar
import Control.Exception (bracket, bracket_, mask_)
import Control.Monad (when)
import qualified Data.Map.Strict as Map
import System.IO.Unsafe (unsafePerformIO)

-- 权限变更独占全局门，业务按库排序取门，允许不同库并行写。

data Gate = Gate (MVar ()) (MVar ()) (MVar Int)

-- 创建公平的共享读写门。
newGate :: IO Gate
newGate = Gate <$> newMVar () <*> newMVar () <*> newMVar 0

{-# NOINLINE securityGate #-}
securityGate :: Gate
securityGate = unsafePerformIO newGate

{-# NOINLINE databaseGates #-}
databaseGates :: MVar (Map.Map String (Gate, Int))
databaseGates = unsafePerformIO (newMVar Map.empty)

-- 在共享门内执行并保证取消时归还计数。
readGate :: Gate -> IO a -> IO a
readGate (Gate turn room readers) = bracket_ enter leave
  where
    -- 登记共享读取者。
    enter = withMVar turn $ \_ -> modifyMVar_ readers $ \count -> do
        when (count == 0) $ takeMVar room
        pure (count + 1)
    -- 释放最后一个共享读取者。
    leave = mask_ $ modifyMVar_ readers $ \count -> do
        when (count == 1) $ putMVar room ()
        pure (count - 1)

-- 独占执行写入及提交的校验与发布。
writeGate :: Gate -> IO a -> IO a
writeGate (Gate turn room _) action = withMVar turn $ \_ -> withMVar room (const action)

-- 并行执行权限稳定期间的查询。
readSecurity :: IO a -> IO a
readSecurity = readGate securityGate

-- 全局排除权限与对象变更期间的数据请求。
writeSecurity :: IO a -> IO a
writeSecurity = writeGate securityGate

-- 按名称取库门，固定顺序避免跨库等待环。
databaseSecurity :: [(String, Bool)] -> IO a -> IO a
databaseSecurity accesses action = readSecurity (keyedSecurity accesses action)

-- 只排序短暂提交检查，不阻塞行锁等待与准备。
commitSecurity :: String -> IO a -> IO a
commitSecurity database = keyedSecurity [("\0commit:" ++ database, True)]

-- 管理有序键门及引用生命周期。
keyedSecurity :: [(String, Bool)] -> IO a -> IO a
keyedSecurity accesses action = bracket register unregister $ \gates -> lock gates action
  where
    names = Map.toAscList (Map.fromListWith (||) (filter (not . null . fst) accesses))
    -- 先登记所有等待者，防止等待中的门被释放。
    register = modifyMVar databaseGates $ \existing -> do
        entries <- mapM (entry existing) names
        let updated = foldr (\(name, gate, count, _) -> Map.insert name (gate, count)) existing entries
        pure (updated, entries)
    -- 复用活跃门或创建首次访问的门。
    entry existing (name, exclusive) = case Map.lookup name existing of
        Just (gate, count) -> pure (name, gate, count + 1, exclusive)
        Nothing -> do
            gate <- newGate
            pure (name, gate, 1, exclusive)
    -- 最后一个持有者或等待者离开时回收门。
    unregister entries = modifyMVar_ databaseGates $ \existing -> pure (foldr release existing entries)
    -- 使用当前引用计数，避免覆盖并发登记。
    release (name, _, _, _) = Map.update (\(gate, count) -> if count == 1 then Nothing else Just (gate, count - 1)) name
    -- 递归持有有序门并在异常时逆序释放。
    lock [] body = body
    lock ((_, gate, _, exclusive) : rest) body = (if exclusive then writeGate else readGate) gate (lock rest body)
