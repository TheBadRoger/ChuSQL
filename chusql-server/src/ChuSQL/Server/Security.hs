module ChuSQL.Server.Security (readSecurity, writeSecurity) where

import Control.Concurrent.MVar
import Control.Exception (bracket_, mask_)
import System.IO.Unsafe (unsafePerformIO)

-- 共享权限门：查询并行读取，权限和对象变更独占执行。

{-# NOINLINE securityGate #-}
securityGate :: (MVar (), MVar (), MVar Int)
securityGate = unsafePerformIO $ do
    turn <- newMVar ()
    room <- newMVar ()
    readers <- newMVar 0
    pure (turn, room, readers)

-- 并行执行权限稳定期间的查询。
readSecurity :: IO a -> IO a
readSecurity = bracket_ enter leave
  where
    (turn, room, readers) = securityGate
    -- 登记共享读取者。
    enter = withMVar turn $ \_ -> modifyMVar_ readers $ \count -> do
        if count == 0 then takeMVar room else pure ()
        pure (count + 1)
    -- 释放最后一个共享读取者。
    leave = mask_ $ modifyMVar_ readers $ \count -> do
        if count == 1 then putMVar room () else pure ()
        pure (count - 1)

-- 独占执行权限或对象变更。
writeSecurity :: IO a -> IO a
writeSecurity action = withMVar turn $ \_ -> withMVar room (const action)
  where
    (turn, room, _) = securityGate
