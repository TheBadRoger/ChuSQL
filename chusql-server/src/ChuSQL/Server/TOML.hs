{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.TOML (
    readSection,
    defaultConfigFile,
    resolveConfigPath,
) where

import qualified Data.ByteString as BS
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (XdgDirectory (XdgConfig), doesFileExist, getXdgDirectory)
import System.FilePath ((</>))

-- 全局 settings.toml 的分区读取：只认 [section] 与 key = value。

-- | 读一个分区；文件不存在或没有这个分区都当空表
readSection :: FilePath -> Text -> IO (Map Text Text)
readSection path section = do
    exists <- doesFileExist path
    if not exists
        then pure Map.empty
        else do
            raw <- BS.readFile path
            pure (Map.fromList (sectionEntries section (T.lines (TE.decodeUtf8 raw))))

-- | 分区里的键值对
sectionEntries :: Text -> [Text] -> [(Text, Text)]
sectionEntries wanted = go ""
  where
    -- \| 走到目标分区后收键值对
    go _ [] = []
    go current (line : rest)
        | Just name <- sectionName line = go name rest
        | current /= wanted = go current rest
        | otherwise = case assignment line of
            Nothing -> go current rest
            Just entry -> entry : go current rest

-- | 是不是分区头，返回分区名
sectionName :: Text -> Maybe Text
sectionName line = case T.strip line of
    stripped
        | "[" `T.isPrefixOf` stripped && "]" `T.isSuffixOf` stripped && T.length stripped > 2 ->
            Just (T.strip (T.drop 1 (T.dropEnd 1 stripped)))
    _ -> Nothing

-- | 是不是 key = value
assignment :: Text -> Maybe (Text, Text)
assignment line = case T.breakOn "=" (stripComment line) of
    (key, rest)
        | not (T.null rest)
        , let name = T.strip key
        , not (T.null name) ->
            Just (name, unquote (T.strip (T.drop 1 rest)))
    _ -> Nothing

-- | 去掉行尾注释（引号里的 # 不算）
stripComment :: Text -> Text
stripComment = T.pack . go False . T.unpack
  where
    -- \| 引号外遇到 # 就到此为止
    go _ [] = []
    go inString ('#' : rest)
        | not inString = []
        | otherwise = '#' : go inString rest
    go inString ('"' : rest) = '"' : go (not inString) rest
    go inString (ch : rest) = ch : go inString rest

-- | 去掉包裹的引号
unquote :: Text -> Text
unquote value
    | T.length value >= 2 && T.head value == '"' && T.last value == '"' = T.drop 1 (T.dropEnd 1 value)
    | T.length value >= 2 && T.head value == '\'' && T.last value == '\'' = T.drop 1 (T.dropEnd 1 value)
    | otherwise = value

-- | 全局配置路径：系统级优先 /etc/chusql
defaultConfigFile :: IO FilePath
defaultConfigFile = do
    systemWide <- doesFileExist "/etc/chusql/settings.toml"
    if systemWide
        then pure "/etc/chusql/settings.toml"
        else do
            dir <- getXdgDirectory XdgConfig "ChuSQL"
            pure (dir </> "settings.toml")

-- | 定位配置文件：命令行 --config 优先，否则用固定路径
resolveConfigPath :: Maybe FilePath -> IO FilePath
resolveConfigPath (Just path) = pure path
resolveConfigPath Nothing = defaultConfigFile
