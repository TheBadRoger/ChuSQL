{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.Settings (readSettingsFile, applySettings, defaultOf) where

import ChuSQL.Server.TOML (readSection)
import Control.Monad (foldM)
import qualified Data.Map.Strict as Map
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- 读取并校验服务端口令策略。

-- 返回口令策略的默认值。
defaultOf :: Text -> Text
defaultOf "password-min-length" = "12"
defaultOf "password-classes" = "2"
defaultOf _ = ""

-- 读取服务端口令策略配置。
readSettingsFile :: FilePath -> IO (Map.Map Text Text)
readSettingsFile path = do
    server <- readSection path "server"
    pure (canonical server)
  where
    -- 统一口令策略键的下划线与连字符写法。
    canonical values = Map.fromList [(T.replace "_" "-" key, value) | (key, value) <- Map.toList values]

-- 校验口令长度与字符类别数并合并设置。
applySettings :: Map.Map Text Text -> Map.Map Text Text -> Either String (Map.Map Text Text)
applySettings current values = foldM apply current (Map.toList values)
  where
    -- 拒绝未知键和越界数值，空值使用默认。
    apply saved (key, value) = do
        (low, high) <- case key of
            "password-min-length" -> Right (8, 256)
            "password-classes" -> Right (1, 4)
            _ -> Left ("unknown setting: " ++ T.unpack key)
        case readMaybe (T.unpack (if T.null (T.strip value) then defaultOf key else T.strip value)) :: Maybe Int of
            Just number | number >= low && number <= high -> Right (Map.insert key value saved)
            _ -> Left (T.unpack key ++ " is outside the permitted range")
