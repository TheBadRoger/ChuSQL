{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.Static (
    safeRelative,
    readStatic,
    contentTypeOf,
) where

import qualified Data.ByteString as BS
import Data.Char (isAsciiLower, isAsciiUpper, isDigit, toLower)
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesFileExist)
import System.FilePath ((</>), takeExtension)

-- 静态文件交付：文件名白名单加扩展名白名单，不做目录列举。

-- | 文件名长度上限
maxNameLength :: Int
maxNameLength = 128

-- | 允许交付的扩展名（只有前端资产）
allowedExtensions :: [String]
allowedExtensions = [".html", ".css", ".js", ".svg", ".png", ".ico", ".ttf", ".woff2"]

-- | 把请求里的文件名变成安全的相对路径；不合法给 Nothing
safeRelative :: Text -> Maybe FilePath
safeRelative t
    | T.null t = Nothing
    | T.length t > maxNameLength = Nothing
    | ".." `T.isInfixOf` t = Nothing
    | T.any badChar t = Nothing
    | map toLower (takeExtension (T.unpack t)) `notElem` allowedExtensions = Nothing
    | otherwise = Just (T.unpack t)
  where
    badChar c = not (isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("._-" :: String))

-- | 读一个静态文件；不存在（或是目录）给 Nothing
readStatic :: FilePath -> FilePath -> IO (Maybe BS.ByteString)
readStatic dir rel = case safeRelative (T.pack rel) of
    Nothing -> pure Nothing
    Just name -> do
        let path = dir </> name
        ok <- doesFileExist path
        if ok then Just <$> BS.readFile path else pure Nothing

-- | 按扩展名给 Content-Type
contentTypeOf :: FilePath -> Text
contentTypeOf path = case map toLower (takeExtension path) of
    ".html" -> "text/html; charset=utf-8"
    ".css" -> "text/css; charset=utf-8"
    ".js" -> "text/javascript; charset=utf-8"
    ".json" -> "application/json; charset=utf-8"
    ".svg" -> "image/svg+xml"
    ".png" -> "image/png"
    ".ico" -> "image/x-icon"
    ".ttf" -> "font/ttf"
    ".woff2" -> "font/woff2"
    ".txt" -> "text/plain; charset=utf-8"
    _ -> "application/octet-stream"
