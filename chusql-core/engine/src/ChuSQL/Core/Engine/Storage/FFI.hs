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

import Control.Exception (SomeException, try)
import Data.ByteString qualified as BS
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Encoding.Error (lenientDecode)
import Foreign
import Foreign.C

-- Rust 存储库 cdylib 的 C ABI 绑定（ffi.rs），收发单行 JSON。

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
maintainStorage path payload = withCString path $ \config ->
    BS.useAsCStringLen payload $ \(input, size) -> alloca $ \outRef -> alloca $ \lenRef -> do
        code <- cStorageMaintenance config input (fromIntegral size) outRef lenRef
        if code /= 0 then Left <$> lastError else do
            output <- peek outRef
            len <- peek lenRef
            bytes <- BS.packCStringLen (output, fromIntegral len)
            cStorageFree output len
            pure (Right bytes)

-- | 库版本（Rust 侧的 CARGO_PKG_VERSION）
storageVersion :: IO String
storageVersion = cStorageVersion >>= peekCString

-- | 打开存储实例，失败给错误文本
openStorage :: Maybe FilePath -> IO (Either String StorageHandle)
openStorage path = do
    result <- try (withCStringOrNull path cStorageOpen) :: IO (Either SomeException (Ptr ()))
    case result of
        Left e -> pure (Left ("storage library call failed: " ++ show e))
        Right ptr
            | ptr == nullPtr -> Left <$> lastError
            | otherwise -> pure (Right (StorageHandle ptr))
  where
    -- | 没有路径就传空指针，否则转成 C 字符串
    withCStringOrNull Nothing f = f nullPtr
    withCStringOrNull (Just s) f = withCString s f

-- | 关掉存储实例
closeStorage :: StorageHandle -> IO ()
closeStorage (StorageHandle handle) = cStorageClose handle

-- | 一条 JSON 请求换一条 JSON 响应
storageRequest :: StorageHandle -> BS.ByteString -> IO (Either String BS.ByteString)
storageRequest (StorageHandle handle) payload =
    BS.useAsCStringLen payload $ \(inPtr, inLen) ->
        alloca $ \outPtrRef ->
            alloca $ \outLenRef -> do
                code <- cStorageRequest handle (castPtr inPtr) (fromIntegral inLen) outPtrRef outLenRef
                if code /= 0
                    then Left <$> lastError
                    else do
                        outPtr <- peek outPtrRef
                        outLen <- peek outLenRef
                        bytes <- BS.packCStringLen (castPtr outPtr, fromIntegral outLen)
                        cStorageFree outPtr outLen
                        pure (Right bytes)

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
