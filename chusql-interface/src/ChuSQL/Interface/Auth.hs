{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Interface.Auth (
    Credential (..),
    defaultIterations,
    hashPassword,
    hashPasswordWith,
    hashLooksValid,
    verifyPassword,
    SessionPolicy (..),
    defaultSessionPolicy,
    SessionStore,
    newSessionStore,
    sessionPolicy,
    setSessionPolicy,
    createSession,
    createVersionedSession,
    lookupVersionedSession,
    lookupSession,
    deleteSession,
    sessionToken,
    PayloadStore,
    newPayloadStore,
    payloadPolicy,
    setPayloadPolicy,
    createPayloadSession,
    lookupPayload,
    putPayload,
    dropPayload,
    sweepPayloads,
) where

import Control.Concurrent.MVar (MVar, modifyMVar, modifyMVar_, newMVar)
import Crypto.Hash (SHA256 (..))
import qualified Crypto.KDF.PBKDF2 as PBKDF2
import Crypto.Random (getRandomBytes)
import qualified Data.ByteString as BS
import Data.ByteArray (constEq)
import Data.ByteArray.Encoding (Base (Base16), convertFromBase, convertToBase)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import qualified Data.Map.Strict as Map
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (NominalDiffTime, UTCTime, diffUTCTime)
import Text.Read (readMaybe)

-- 口令哈希与会话仓库，服务端与 Web 管理端共用。

-- | 一条账号凭据：账号名 + 编码串
data Credential = Credential
    { credUser :: Text
    , credEncoded :: Text
    }
    deriving (Show, Eq)

-- | 默认迭代次数（10 万次）
defaultIterations :: Int
defaultIterations = 100000

-- | 派生密钥长度（字节）
keyLength :: Int
keyLength = 32

-- | 盐长度（字节）
saltLength :: Int
saltLength = 16

-- | 迭代次数的上界：配置串被人改坏时不至于把自己算死
maxIterations :: Int
maxIterations = 10000000

-- | PBKDF2-HMAC-SHA256 派生
deriveKey :: Int -> BS.ByteString -> BS.ByteString -> BS.ByteString
deriveKey iters salt password =
    PBKDF2.generate (PBKDF2.prfHMAC SHA256) (PBKDF2.Parameters iters keyLength) password salt

-- | 字节转十六进制
hexEncode :: BS.ByteString -> Text
hexEncode = TE.decodeUtf8 . convertToBase Base16

-- | 十六进制转字节（长度不对/有非法字符就给 Nothing）
hexDecode :: Text -> Maybe BS.ByteString
hexDecode t = case convertFromBase Base16 (TE.encodeUtf8 t) of
    Left _ -> Nothing
    Right bs -> Just bs

-- | 编码成 `pbkdf2-sha256$次数$盐$哈希`
encodeHash :: Int -> BS.ByteString -> BS.ByteString -> Text
encodeHash iters salt digest =
    T.intercalate "$" ["pbkdf2-sha256", T.pack (show iters), hexEncode salt, hexEncode digest]

-- | 解回 (次数, 盐, 哈希)，坏格式给 Nothing
parseHash :: Text -> Maybe (Int, BS.ByteString, BS.ByteString)
parseHash t = case T.splitOn "$" t of
    [alg, itersT, saltT, digestT]
        | alg == "pbkdf2-sha256" -> do
            iters <- readMaybe (T.unpack itersT) :: Maybe Int
            salt <- hexDecode saltT
            digest <- hexDecode digestT
            if iters < 1000 || iters > maxIterations || BS.null salt || BS.length digest /= keyLength
                then Nothing
                else Just (iters, salt, digest)
    _ -> Nothing

-- | 用给定盐生成编码串（给定盐是为了能写确定性测试）
hashPasswordWith :: Int -> BS.ByteString -> Text -> Text
hashPasswordWith iters salt password =
    encodeHash iters salt (deriveKey iters salt (TE.encodeUtf8 password))

-- | 给一个口令配随机盐，生成编码串
hashPassword :: Text -> IO Text
hashPassword password = do
    salt <- getRandomBytes saltLength
    pure (hashPasswordWith defaultIterations salt password)

-- | 恒定时间校验口令是否匹配
verifyPassword :: Text -> Text -> Bool
verifyPassword encoded candidate = case parseHash encoded of
    Nothing -> False
    Just (iters, salt, digest) -> constEq digest (deriveKey iters salt (TE.encodeUtf8 candidate))

-- | 编码串本身是否合法（不看口令）
hashLooksValid :: Text -> Bool
hashLooksValid = isJust . parseHash

-- | 一条会话：账号、创建/最后活动时间与账号版本号
data Session = Session
    { sessUser :: Text
    , sessCreated :: UTCTime
    , sessSeen :: UTCTime
    , sessRevision :: Maybe Integer
    }

-- | 会话过期策略：空闲上限与绝对上限
data SessionPolicy = SessionPolicy
    { spIdleSeconds :: NominalDiffTime
    , spAbsoluteSeconds :: NominalDiffTime
    }

-- | 默认：空闲 8 小时、绝对 24 小时
defaultSessionPolicy :: SessionPolicy
defaultSessionPolicy = SessionPolicy (8 * 3600) (24 * 3600)

-- | 会话仓库：令牌表、时钟与策略
data SessionStore = SessionStore
    { storeMap :: MVar (Map.Map Text Session)
    , storeClock :: IO UTCTime
    , storePolicy :: IORef SessionPolicy
    }

-- | 造一个空仓库
newSessionStore :: IO UTCTime -> SessionPolicy -> IO SessionStore
newSessionStore clock policy = do
    ref <- newMVar Map.empty
    policyRef <- newIORef policy
    pure (SessionStore ref clock policyRef)

-- | 现在用的是哪套过期策略
sessionPolicy :: SessionStore -> IO SessionPolicy
sessionPolicy = readIORef . storePolicy

-- | 换一套过期策略（设置页热改）
setSessionPolicy :: SessionStore -> SessionPolicy -> IO ()
setSessionPolicy store = writeIORef (storePolicy store)

-- | 这条会话过期了吗
expired :: UTCTime -> SessionPolicy -> Session -> Bool
expired now pol s =
    diffUTCTime now (sessSeen s) > spIdleSeconds pol
        || diffUTCTime now (sessCreated s) > spAbsoluteSeconds pol

-- | 清掉过期会话
prune :: UTCTime -> SessionPolicy -> Map.Map Text Session -> Map.Map Text Session
prune now pol = Map.filter (not . expired now pol)

-- | 随机令牌：32 字节，十六进制 64 个字符
sessionToken :: IO Text
sessionToken = hexEncode <$> getRandomBytes 32

-- | 开一条会话，返回令牌
createSession :: SessionStore -> Text -> IO Text
createSession store user = createVersionedSession store user Nothing

-- | 开一条会话并记下账号版本号，返回令牌
createVersionedSession :: SessionStore -> Text -> Maybe Integer -> IO Text
createVersionedSession store user revision = do
    token <- sessionToken
    now <- storeClock store
    pol <- sessionPolicy store
    modifyMVar_ (storeMap store) $ \m ->
        pure (Map.insert token (Session user now now revision) (prune now pol m))
    pure token

-- | 查令牌并刷新最后活动时间
lookupSession :: SessionStore -> Text -> IO (Maybe Text)
lookupSession store token = fmap (fmap fst) (lookupVersionedSession store token)

-- | 查令牌拿回账号名与账号版本号，并刷新最后活动时间
lookupVersionedSession :: SessionStore -> Text -> IO (Maybe (Text, Maybe Integer))
lookupVersionedSession store token = do
    now <- storeClock store
    pol <- sessionPolicy store
    modifyMVar (storeMap store) $ \m -> do
        let kept = prune now pol m
        pure $ case Map.lookup token kept of
            Nothing -> (kept, Nothing)
            Just s -> (Map.insert token s{sessSeen = now} kept, Just (sessUser s, sessRevision s))

-- | 删掉一条会话（退出登录）
deleteSession :: SessionStore -> Text -> IO ()
deleteSession store token = modifyMVar_ (storeMap store) (pure . Map.delete token)

-- | 一条带载荷的会话：账号、时间与载荷
data StoredSession a = StoredSession
    { psUser :: Text
    , psCreated :: UTCTime
    , psSeen :: UTCTime
    , psPayload :: a
    }

-- | 带载荷的会话仓库：令牌表、时钟与策略
data PayloadStore a = PayloadStore
    { psStoreMap :: MVar (Map.Map Text (StoredSession a))
    , psClock :: IO UTCTime
    , psPolicy :: IORef SessionPolicy
    }

-- | 造一个空的带载荷仓库
newPayloadStore :: IO UTCTime -> SessionPolicy -> IO (PayloadStore a)
newPayloadStore clock policy = do
    ref <- newMVar Map.empty
    policyRef <- newIORef policy
    pure (PayloadStore ref clock policyRef)

-- | 现在用的是哪套过期策略
payloadPolicy :: PayloadStore a -> IO SessionPolicy
payloadPolicy = readIORef . psPolicy

-- | 换一套过期策略
setPayloadPolicy :: PayloadStore a -> SessionPolicy -> IO ()
setPayloadPolicy store = writeIORef (psPolicy store)

-- | 这条带载荷的会话过期了吗
payloadExpired :: UTCTime -> SessionPolicy -> StoredSession a -> Bool
payloadExpired now pol s =
    diffUTCTime now (psSeen s) > spIdleSeconds pol
        || diffUTCTime now (psCreated s) > spAbsoluteSeconds pol

-- | 清掉过期的带载荷会话
prunePayloads :: UTCTime -> SessionPolicy -> Map.Map Text (StoredSession a) -> Map.Map Text (StoredSession a)
prunePayloads now pol = Map.filter (not . payloadExpired now pol)

-- | 上线一条带载荷的会话，返回令牌
createPayloadSession :: PayloadStore a -> Text -> a -> IO Text
createPayloadSession store user payload = do
    token <- sessionToken
    now <- psClock store
    pol <- payloadPolicy store
    modifyMVar_ (psStoreMap store) $ \m ->
        pure (Map.insert token (StoredSession user now now payload) (prunePayloads now pol m))
    pure token

-- | 查令牌并刷新最后活动时间，拿回账号名与载荷
lookupPayload :: PayloadStore a -> Text -> IO (Maybe (Text, a))
lookupPayload store token = do
    now <- psClock store
    pol <- payloadPolicy store
    modifyMVar (psStoreMap store) $ \m -> do
        let kept = prunePayloads now pol m
        pure $ case Map.lookup token kept of
            Nothing -> (kept, Nothing)
            Just s -> (Map.insert token s{psSeen = now} kept, Just (psUser s, psPayload s))

-- | 换掉载荷；会话已没了就返回 False
putPayload :: PayloadStore a -> Text -> a -> IO Bool
putPayload store token payload = do
    now <- psClock store
    pol <- payloadPolicy store
    modifyMVar (psStoreMap store) $ \m -> do
        let kept = prunePayloads now pol m
        pure $ case Map.lookup token kept of
            Nothing -> (kept, False)
            Just s -> (Map.insert token s{psSeen = now, psPayload = payload} kept, True)

-- | 摘掉会话并把载荷交还
dropPayload :: PayloadStore a -> Text -> IO (Maybe a)
dropPayload store token = do
    now <- psClock store
    pol <- payloadPolicy store
    modifyMVar (psStoreMap store) $ \m -> do
        let kept = prunePayloads now pol m
        pure $ case Map.lookup token kept of
            Nothing -> (kept, Nothing)
            Just s -> (Map.delete token kept, Just (psPayload s))

-- | 清掉过期会话并把它们的载荷交还；调用者负责关掉这些载荷
sweepPayloads :: PayloadStore a -> IO [a]
sweepPayloads store = do
    now <- psClock store
    pol <- payloadPolicy store
    modifyMVar (psStoreMap store) $ \m -> do
        let (gone, kept) = Map.partition (payloadExpired now pol) m
        pure (kept, map psPayload (Map.elems gone))
