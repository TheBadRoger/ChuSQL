{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.UISettings (
    uiSettingsFileCandidates,
    resolveUISettingsFile,
    validateUISettings,
    readUISettings,
    writeUISettings,
) where

import Control.Exception (IOException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import qualified Data.Map.Strict as Map
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory, (</>))

-- 前端 IDE 设置单独存一份文件，与全局启动配置 chusql.toml 互不干扰。

-- | IDE 设置文件的候选位置
uiSettingsFileCandidates :: [FilePath]
uiSettingsFileCandidates =
    [ "scripts" </> "chusql.ui.settings.json"
    , ".." </> "scripts" </> "chusql.ui.settings.json"
    , "chusql.ui.settings.json"
    ]

-- | 找现成的设置文件，否则挑目录已存在的候选
resolveUISettingsFile :: IO FilePath
resolveUISettingsFile = do
    found <- firstExistingFile uiSettingsFileCandidates
    case found of
        Just path -> pure path
        Nothing -> do
            ready <- firstExistingDir uiSettingsFileCandidates
            pure (maybe firstCandidate id ready)
  where
    firstCandidate = case uiSettingsFileCandidates of
        (p : _) -> p
        [] -> "chusql.ui.settings.json"

    firstExistingFile [] = pure Nothing
    firstExistingFile (p : ps) = do
        ok <- doesFileExist p
        if ok then pure (Just p) else firstExistingFile ps

    firstExistingDir [] = pure Nothing
    firstExistingDir (p : ps) = do
        let dir = takeDirectory p
        ok <- if null dir then pure True else doesDirectoryExist dir
        if ok then pure (Just p) else firstExistingDir ps

-- | 校验一份 IDE 设置，未知键与越界都给错
validateUISettings :: A.Value -> Either String (Map.Map Text A.Value)
validateUISettings (A.Object o) = Map.fromList <$> mapM checkField (KM.toList o)
  where
    checkField (rawKey, value) =
        let key = K.toText rawKey
         in case key of
                "uiFonts" -> pair key <$> fontChain key value
                "gridFonts" -> pair key <$> fontChain key value
                "sqlFonts" -> pair key <$> fontChain key value
                "uiFontSize" -> pair key <$> boundedInt key 11 16 value
                "gridFontSize" -> pair key <$> boundedInt key 10 18 value
                "gridRowHeight" -> pair key <$> boundedInt key 18 40 value
                "sqlFontSize" -> pair key <$> boundedInt key 10 20 value
                "sqlLineHeight" -> pair key <$> boundedInt key 16 40 value
                "sqlTabSize" -> pair key <$> boundedInt key 1 8 value
                "pageSize" -> pair key <$> boundedInt key 10 500 value
                "nullText" -> pair key <$> shortText key value
                "sqlLineNumbers" -> pair key <$> boolValue key value
                "autocomplete" -> pair key <$> boolValue key value
                "minimap" -> pair key <$> boolValue key value
                _ -> Left ("unknown setting: " ++ T.unpack key)

    pair key value = (key, value)

validateUISettings _ = Left "settings must be a JSON object"

fontChainMax :: Int
fontChainMax = 10

fontNameMax :: Int
fontNameMax = 64

nullTextMax :: Int
nullTextMax = 64

-- | 字体链：最多 10 项，每项非空且不超 64 字
fontChain :: Text -> A.Value -> Either String A.Value
fontChain key value = case value of
    A.Array xs
        | V.length xs > fontChainMax -> bad
        | otherwise -> A.Array . V.fromList <$> mapM item (V.toList xs)
    _ -> bad
  where
    bad =
        Left
            ( T.unpack key
                ++ " expects at most "
                ++ show fontChainMax
                ++ " non-empty font names of at most "
                ++ show fontNameMax
                ++ " characters"
            )
    item (A.String s) =
        let trimmed = T.strip s
         in if not (T.null trimmed) && T.length trimmed <= fontNameMax
                then Right (A.String trimmed)
                else bad
    item _ = bad

-- | 区间内的整数，顺便把它整成规范写法
boundedInt :: Text -> Int -> Int -> A.Value -> Either String A.Value
boundedInt key lo hi value = case value of
    A.Number n -> case floatingOrInteger n :: Either Double Integer of
        Right i
            | i >= fromIntegral lo && i <= fromIntegral hi -> Right (A.Number (fromIntegral i))
        _ -> bad
    _ -> bad
  where
    bad = Left (T.unpack key ++ " expects an integer in " ++ show lo ++ ".." ++ show hi)

-- | 空值占位文本：字符串且不超 64 字
shortText :: Text -> A.Value -> Either String A.Value
shortText key value = case value of
    A.String s
        | T.length s <= nullTextMax -> Right (A.String s)
    _ ->
        Left
            ( T.unpack key
                ++ " expects a string of at most "
                ++ show nullTextMax
                ++ " characters"
            )

-- | 布尔开关
boolValue :: Text -> A.Value -> Either String A.Value
boolValue key value = case value of
    A.Bool b -> Right (A.Bool b)
    _ -> Left (T.unpack key ++ " expects a boolean")

-- | 读 IDE 设置文件，缺文件或坏内容都给空对象
readUISettings :: FilePath -> IO A.Value
readUISettings path = do
    exists <- doesFileExist path
    if not exists
        then pure emptyObject
        else do
            raw <- try (BS.readFile path) :: IO (Either IOException BS.ByteString)
            pure $ case raw of
                Left _ -> emptyObject
                Right bytes -> case A.eitherDecodeStrict' bytes of
                    Right (A.Object o) -> A.Object o
                    _ -> emptyObject
  where
    emptyObject = A.Object KM.empty

-- | 写 IDE 设置文件，先建目录再落盘
writeUISettings :: FilePath -> Map.Map Text A.Value -> IO (Either String ())
writeUISettings path values = do
    prepared <- try (ensureDir (takeDirectory path)) :: IO (Either IOException ())
    case prepared of
        Left e -> pure (Left ("cannot create the settings directory " ++ takeDirectory path ++ ": " ++ show e))
        Right () -> do
            let encoded = A.encode (A.Object (KM.fromList [(K.fromText k, v) | (k, v) <- Map.toList values]))
            written <- try (BS.writeFile path (BL.toStrict encoded)) :: IO (Either IOException ())
            pure (either (Left . \e -> "cannot write " ++ path ++ ": " ++ show e) Right written)
  where
    ensureDir dir = if null dir then pure () else createDirectoryIfMissing True dir
