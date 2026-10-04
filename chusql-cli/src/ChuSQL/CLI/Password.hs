module ChuSQL.CLI.Password (readPasswordInput) where

import Data.Text (Text)
import qualified Data.Text.IO as T
import System.IO (Handle, hIsEOF, hSetNewlineMode, universalNewlineMode)

-- 管道口令输入：识别 LF 与 CRLF 行尾，保留口令内容。

-- | 从句柄读取一行口令
readPasswordInput :: Handle -> IO Text
readPasswordInput handle = do
    hSetNewlineMode handle universalNewlineMode
    empty <- hIsEOF handle
    if empty then pure mempty else T.hGetLine handle
