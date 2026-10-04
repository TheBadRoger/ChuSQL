{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Interface.TOML
    ( readSection
    , writeSection
    , defaultConfigFile
    , resolveConfigPath
    ) where

import Control.Exception (IOException, try)
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import System.Directory (XdgDirectory (XdgConfig), createDirectoryIfMissing, doesFileExist, getXdgDirectory)
import System.FilePath (takeDirectory, (</>))

-- 全局 settings.toml 的最小读写：只认 [section] 与 key = value。

-- | 读一个分区；文件不存在或没有这个分区都当空表
readSection :: FilePath -> Text -> IO (Map Text Text)
readSection path section = do
    exists <- doesFileExist path
    if not exists
        then pure Map.empty
        else do
            raw <- BS.readFile path
            pure (Map.fromList (sectionEntries section (T.lines (TE.decodeUtf8 raw))))

-- | 只重写一个分区，其它行（含注释与别的层）原样保留
writeSection :: FilePath -> Text -> Map Text Text -> IO (Either String ())
writeSection path section values = do
    prepared <- prepareDirectory
    case prepared of
        Left err -> pure (Left err)
        Right () -> do
            exists <- doesFileExist path
            raw <- if exists then TE.decodeUtf8 <$> BS.readFile path else pure ""
            let updated = T.unlines (rewrite section values (T.lines raw))
            written <- tryWrite (TE.encodeUtf8 updated)
            pure written
  where
    -- | 目录不存在就先建
    prepareDirectory = do
        let dir = takeDirectory path
        if null dir
            then pure (Right ())
            else do
                done <- try (createDirectoryIfMissing True dir) :: IO (Either IOException ())
                pure (either (Left . show) (const (Right ())) done)
    -- | 写文件，异常转成错误串
    tryWrite bytes = do
        done <- try (BS.writeFile path bytes) :: IO (Either IOException ())
        pure (either (Left . show) (const (Right ())) done)

-- | 分区里的键值对
sectionEntries :: Text -> [Text] -> [(Text, Text)]
sectionEntries wanted = go ""
  where
    -- | 走到目标分区后收键值对
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
    -- | 引号外遇到 # 就到此为止
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

-- | 行级重写：已有键就地更新，空值删行，缺的补末尾
rewrite :: Text -> Map Text Text -> [Text] -> [Text]
rewrite section values = addMissing . replaceInSection . dropEmpties
  where
    -- | 删掉空值的键
    dropEmpties = filter (\line -> maybe True (not . T.null . snd) (assignment line))
    -- | 在目标分区里就地换行
    replaceInSection = go ""
    -- | 走到目标分区后逐行替换
    go _ [] = []
    go current (line : rest)
        | Just name <- sectionName line = line : go name rest
        | current /= section = line : go current rest
        | otherwise = case assignment line of
            Just (key, _) | Just value <- Map.lookup key values -> assignmentLine key value : go current rest
            _ -> line : go current rest
    -- | 分区里缺的键补在末尾
    addMissing lines' =
        let present = Set.fromList (keysOf section lines')
            missing = [(key, value) | (key, value) <- Map.toList values, not (Set.member key present), not (T.null value)]
         in if null missing then lines' else appendSection section missing lines'

-- | 拼一行 key = value
assignmentLine :: Text -> Text -> Text
assignmentLine key value = key <> " = " <> quote value

-- | 需要时才加引号
quote :: Text -> Text
quote value
    | T.all allowed value && not (T.null value) = value
    | otherwise = "\"" <> T.replace "\"" "\\\"" value <> "\""
  where
    -- | 可以裸写的字符集
    allowed ch = ch `elem` ("0123456789.-" :: String)

-- | 取某个分区里出现过的键
keysOf :: Text -> [Text] -> [Text]
keysOf wanted = go ""
  where
    -- | 走到目标分区后收键名
    go _ [] = []
    go current (line : rest)
        | Just name <- sectionName line = go name rest
        | current /= wanted = go current rest
        | otherwise = case assignment line of
            Just (key, _) -> key : go current rest
            Nothing -> go current rest

-- | 没有这个分区就在末尾补一个
appendSection :: Text -> [(Text, Text)] -> [Text] -> [Text]
appendSection section entries lines' =
    lines'
        ++ (if null lines' || not (T.null (last lines')) then [""] else [])
        ++ ("[" <> section <> "]") : [assignmentLine key value | (key, value) <- entries]

-- | 全局配置文件的固定路径，跟随系统惯例
defaultConfigFile :: IO FilePath
defaultConfigFile = do
    dir <- getXdgDirectory XdgConfig "ChuSQL"
    pure (dir </> "settings.toml")

-- | 定位配置文件：命令行 --config 优先，否则用固定路径
resolveConfigPath :: Maybe FilePath -> IO FilePath
resolveConfigPath (Just path) = pure path
resolveConfigPath Nothing = defaultConfigFile
