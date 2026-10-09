{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE ImportQualifiedPost #-}

module ChuSQL.Core.Engine.Storage.FFI (
    StorageHandle,
    storageVersion,
    openStorage,
    closeStorage,
    storageRequest,
    maintainStorage,
) where

import Control.Concurrent (rtsSupportsBoundThreads, runInBoundThread)
import Control.Exception (SomeException, finally, mask_, try)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Foreign
import Foreign.C

-- Rust 存储库 cdylib 的 C ABI 绑定（ffi.rs），收发单行 JSON。
-- 错误读取绑定同一 OS 线程；响应复制期间屏蔽取消并归还缓冲。

-- | 存储实例句柄（Rust 侧的 *mut Storage）
newtype StorageHandle = StorageHandle (Ptr ())

foreign import ccall unsafe "chusql_storage_version"
    cStorageVersion :: IO CString

foreign import ccall safe "chusql_storage_open"
    cStorageOpen :: CString -> IO (Ptr ())

foreign import ccall safe "chusql_storage_close"
    cStorageClose :: Ptr () -> IO ()

foreign import ccall safe "chusql_storage_request"
    cStorageRequest :: Ptr () -> Ptr CChar -> CSize -> Ptr (Ptr CChar) -> Ptr CSize -> IO CInt

foreign import ccall unsafe "chusql_storage_free"
    cStorageFree :: Ptr CChar -> CSize -> IO ()

foreign import ccall unsafe "chusql_storage_last_error"
    cStorageLastError :: Ptr CSize -> IO (Ptr CChar)

foreign import ccall safe "chusql_storage_maintenance"
    cStorageMaintenance :: CString -> Ptr CChar -> CSize -> Ptr (Ptr CChar) -> Ptr CSize -> IO CInt

-- | 离线修复系统目录并返回结果
maintainStorage :: FilePath -> BS.ByteString -> IO (Either String BS.ByteString)
maintainStorage path payload = BS.useAsCString (TE.encodeUtf8 (T.pack path)) $ \config ->
    callStorage (cStorageMaintenance config) payload

-- | 库版本（Rust 侧的 CARGO_PKG_VERSION）
storageVersion :: IO String
storageVersion = cStorageVersion >>= peekCString

-- | 打开存储实例，失败给错误文本
openStorage :: Maybe FilePath -> IO (Either String StorageHandle)
openStorage path = onStorageThread $ mask_ $ do
    result <- try (withCStringOrNull path cStorageOpen) :: IO (Either SomeException (Ptr ()))
    case result of
        Left e -> pure (Left ("storage library call failed: " ++ show e))
        Right ptr
            | ptr == nullPtr -> Left <$> lastError
            | otherwise -> pure (Right (StorageHandle ptr))
  where
    -- | 没有路径就传空指针，否则转成 C 字符串
    withCStringOrNull Nothing f = f nullPtr
    withCStringOrNull (Just s) f = BS.useAsCString (TE.encodeUtf8 (T.pack s)) f

-- | 关掉存储实例
closeStorage :: StorageHandle -> IO ()
closeStorage (StorageHandle handle) = cStorageClose handle

-- | 一条 JSON 请求换一条 JSON 响应
storageRequest :: StorageHandle -> BS.ByteString -> IO (Either String BS.ByteString)
storageRequest (StorageHandle handle) = callStorage (cStorageRequest handle)

-- | 调用存储并复制和归还响应缓冲
callStorage :: (Ptr CChar -> CSize -> Ptr (Ptr CChar) -> Ptr CSize -> IO CInt)
    -> BS.ByteString -> IO (Either String BS.ByteString)
callStorage call payload = onStorageThread $ mask_ $
    BS.useAsCStringLen payload $ \(input, size) ->
        alloca $ \outRef -> alloca $ \lenRef -> do
            code <- call input (fromIntegral size) outRef lenRef
            if code /= 0 then Left <$> lastError else do
                output <- peek outRef
                len <- peek lenRef
                bytes <- BS.packCStringLen (output, fromIntegral len)
                    `finally` cStorageFree output len
                pure (Right bytes)

-- | 多线程运行时绑定存储调用线程
onStorageThread :: IO a -> IO a
onStorageThread = if rtsSupportsBoundThreads then runInBoundThread else id

-- | 当前线程最近一次失败的文本
lastError :: IO String
lastError = alloca $ \lenRef -> do
    ptr <- cStorageLastError lenRef
    if ptr == nullPtr
        then pure "storage: unknown error"
        else do
            len <- peek lenRef
            raw <- BS.packCStringLen (ptr, fromIntegral len)
            pure (T.unpack (TE.decodeUtf8With lenientDecode raw))
