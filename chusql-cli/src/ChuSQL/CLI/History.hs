{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.CLI.History (historySettings, rememberStatement) where

import Control.Monad (unless)
import Control.Monad.IO.Class (MonadIO)
import qualified Data.Text as T
import System.Console.Haskeline (InputT, Settings (..), defaultSettings, modifyHistory)
import System.Console.Haskeline.History (addHistory)

-- 关闭逐行历史，筛选完整语句后记录安全输入。

-- | 配置仅手动追加的输入历史
historySettings :: MonadIO m => Maybe FilePath -> Settings m
historySettings path = defaultSettings {historyFile = path, autoAddHistory = False}

-- | 排除含口令标记的完整语句
rememberStatement :: MonadIO m => T.Text -> InputT m ()
rememberStatement statement =
    unless (T.null (T.strip statement) || any (`T.isInfixOf` T.toCaseFold statement) ["password", "identified"])
        (modifyHistory (addHistory (T.unpack statement)))
