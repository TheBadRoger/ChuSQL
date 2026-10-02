module ChuSQL.Interface.RateLimit (
    RateLimiter,
    newRateLimiter,
    setRateLimit,
    rateLimitBlock,
    rateLimitRecord,
    rateLimitClear,
) where

import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)

-- 登录失败限流：按用户名数一个滑动窗口里的失败次数。

-- | 一个键的失败计数与窗口起点
data Attempt = Attempt
    { attCount :: Int
    , attStart :: UTCTime
    }

-- | 限流阈值：次数上限与窗口长度
data RateLimitPolicy = RateLimitPolicy
    { rlpMax :: Int
    , rlpWindow :: NominalDiffTime
    }

-- | 限流器：计数表、阈值与时钟
data RateLimiter = RateLimiter
    { rlMap :: MVar (Map.Map Text Attempt)
    , rlPolicy :: IORef RateLimitPolicy
    , rlClock :: IO UTCTime
    }

-- | 造一个限流器：多少次失败、窗口多长
newRateLimiter :: IO UTCTime -> Int -> NominalDiffTime -> IO RateLimiter
newRateLimiter clock maxFailures window = do
    ref <- newMVar Map.empty
    policyRef <- newIORef (RateLimitPolicy maxFailures window)
    pure (RateLimiter ref policyRef clock)

-- | 换一套限流阈值（设置页热改）
setRateLimit :: RateLimiter -> Int -> NominalDiffTime -> IO ()
setRateLimit rl maxFailures window =
    writeIORef (rlPolicy rl) (RateLimitPolicy maxFailures window)

-- | 这个键被锁住了吗？锁住就返回还要等多少秒
rateLimitBlock :: RateLimiter -> Text -> IO (Maybe NominalDiffTime)
rateLimitBlock rl key = do
    now <- rlClock rl
    pol <- readIORef (rlPolicy rl)
    m <- readMVar (rlMap rl)
    pure $ case Map.lookup key m of
        Just a
            | attCount a >= rlpMax pol ->
                let left = rlpWindow pol - diffUTCTime now (attStart a)
                 in if left > 0 then Just left else Nothing
        _ -> Nothing

-- | 记一次失败
rateLimitRecord :: RateLimiter -> Text -> IO ()
rateLimitRecord rl key = do
    now <- rlClock rl
    pol <- readIORef (rlPolicy rl)
    modifyMVar_ (rlMap rl) $ \m ->
        let kept = Map.filter (\a -> diffUTCTime now (attStart a) < rlpWindow pol) m
            old = Map.findWithDefault (Attempt 0 now) key kept
            fresh = if attCount old == 0 then Attempt 1 now else old{attCount = attCount old + 1}
         in pure (Map.insert key fresh kept)

-- | 登录成功就把失败记录清掉
rateLimitClear :: RateLimiter -> Text -> IO ()
rateLimitClear rl key = modifyMVar_ (rlMap rl) (pure . Map.delete key)
