{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Interface.Sudo (
    SudoCredential, createSudoCredential, removeSudoCredential,
    readSudoCredential, sudoProof, verifySudoProof, sudoChallenge, sudoPrivileged,
    newSudoCredential, isLocalPeer, validSudoChallenge,
) where

import Crypto.Hash (SHA256)
import Crypto.MAC.HMAC (HMAC, hmac)
import Crypto.Random (getRandomBytes)
import qualified Data.ByteArray as BA
import Data.ByteArray.Encoding (Base (Base16), convertToBase)
import qualified Data.ByteString as BS
import Data.Char (isHexDigit)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time.Clock (UTCTime, diffUTCTime)
import Foreign.C.String (CString)
import Foreign.C.Types (CInt (..))
import Foreign.Marshal.Alloc (allocaBytes)
import Network.Socket (SockAddr (..), hostAddressToTuple, hostAddress6ToTuple)

-- 用系统保护凭据与一次性挑战验证本机提升身份。

newtype SudoCredential = SudoCredential BS.ByteString

-- 隐藏本机凭据的诊断内容。
instance Show SudoCredential where
    show _ = "SudoCredential <redacted>"

-- 检查当前进程的操作系统提升身份。
foreign import ccall unsafe "chusql_sudo_privileged" nativePrivileged :: IO CInt
-- 创建操作系统保护的本机凭据。
foreign import ccall safe "chusql_sudo_create" nativeCreate :: CInt -> CString -> IO CInt
-- 读取操作系统保护的本机凭据。
foreign import ccall safe "chusql_sudo_read" nativeRead :: CInt -> CString -> IO CInt
-- 清理本次服务启动的凭据。
foreign import ccall safe "chusql_sudo_remove" nativeRemove :: CInt -> IO CInt

-- 判断当前进程是否以提升身份运行。
sudoPrivileged :: IO Bool
sudoPrivileged = (/= 0) <$> nativePrivileged

-- 生成随机挑战和服务实例凭据。
sudoChallenge :: IO Text
sudoChallenge = TE.decodeUtf8 . convertToBase Base16 <$> (getRandomBytes 32 :: IO BS.ByteString)

-- 创建本次服务实例的受保护凭据。
createSudoCredential :: Int -> IO SudoCredential
createSudoCredential port = do
    credential@(SudoCredential secret) <- newSudoCredential
    status <- BS.useAsCString secret (nativeCreate (fromIntegral port))
    if status == 0 then pure credential
        else ioError (userError (T.unpack (nativeError "create" status)))

-- 生成独立服务实例的随机凭据。
newSudoCredential :: IO SudoCredential
newSudoCredential = SudoCredential . TE.encodeUtf8 <$> sudoChallenge

-- 判断内核报告的连接地址是否为回环。
isLocalPeer :: SockAddr -> Bool
isLocalPeer (SockAddrInet _ host) = let (first, _, _, _) = hostAddressToTuple host in first == 127
isLocalPeer (SockAddrInet6 _ _ host _) = case hostAddress6ToTuple host of
    (0, 0, 0, 0, 0, 0, 0, 1) -> True
    (0, 0, 0, 0, 0, 65535, high, _) -> high >= 32512 && high <= 32767
    _ -> False
isLocalPeer _ = False

-- 校验挑战有效期及其账号绑定证明。
validSudoChallenge :: SudoCredential -> UTCTime -> Maybe (Text, UTCTime) -> Text -> Text -> Bool
validSudoChallenge credential now challenge user proof = case challenge of
    Just (nonce, issued) -> diffUTCTime now issued >= 0 && diffUTCTime now issued <= 60
        && verifySudoProof credential nonce user proof
    Nothing -> False

-- 删除本次服务实例的受保护凭据。
removeSudoCredential :: Int -> IO ()
removeSudoCredential port = do
    status <- nativeRemove (fromIntegral port)
    if status == 0 then pure () else ioError (userError (T.unpack (nativeError "remove" status)))

-- 验证操作系统身份并读取本机凭据。
readSudoCredential :: Int -> IO (Either Text SudoCredential)
readSudoCredential port = allocaBytes 64 $ \buffer -> do
    status <- nativeRead (fromIntegral port) buffer
    if status /= 0 then pure (Left (nativeError "read" status)) else do
        secret <- BS.packCStringLen (buffer, 64)
        pure $ if BS.all (isHexDigit . toEnum . fromIntegral) secret
            then Right (SudoCredential secret) else Left "invalid sudo credential"

-- 计算绑定挑战与目标账号的认证证明。
sudoProof :: SudoCredential -> Text -> Text -> Text
sudoProof (SudoCredential secret) challenge user = TE.decodeUtf8 (convertToBase Base16 digest)
  where
    digest = hmac secret (TE.encodeUtf8 ("chusql-sudo-v1:" <> challenge <> ":" <> T.toLower (T.strip user))) :: HMAC SHA256

-- 按固定长度常量时间比对认证证明。
verifySudoProof :: SudoCredential -> Text -> Text -> Text -> Bool
verifySudoProof credential challenge user proof = T.length proof == 64
    && BA.constEq (TE.encodeUtf8 (sudoProof credential challenge user)) (TE.encodeUtf8 proof)

-- 将原生错误转换为明确诊断。
nativeError :: Text -> CInt -> Text
nativeError operation status = "sudo credential " <> operation <> " failed (OS error "
    <> T.pack (show status) <> "); requires root or an elevated administrator; stale credential directories must be removed after stopping the server"
