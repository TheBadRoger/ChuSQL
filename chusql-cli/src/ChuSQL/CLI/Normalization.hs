{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.CLI.Normalization (normalizationArguments, loadNormalizationDeclaration, renderNormalization, runNormalizationOnce) where

import ChuSQL.Interface.Normalization
import ChuSQL.Interface.Session (Session, sessionDatabase, analyzeNormalization)
import Control.Exception (IOException, try)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import System.IO (IOMode (ReadMode), stderr, withBinaryFile)
import Text.ParserCombinators.ReadP

-- CLI 范式入口：读取严格声明文件、发送专用请求并显示报告。

-- 解析表、声明路径和独立显示格式。
normalizationArguments :: Text -> Either Text (Text, FilePath, Bool)
normalizationArguments input = case readP_to_S (skipSpaces *> sepBy token skipSpaces <* skipSpaces <* eof) (T.unpack input) of
    [(parts, "")] -> case parts of
        [table, "--declaration", file] -> Right (T.pack table, file, False)
        [table, "--declaration", file, "--format", "text"] -> Right (T.pack table, file, False)
        [table, "--declaration", file, "--format", "json"] -> Right (T.pack table, file, True)
        _ -> Left usage
    _ -> Left usage
  where
    usage = "usage: \\normalize TABLE --declaration FILE [--format text|json]"
    -- 保留引号内的 Windows 路径及空格。
    token = quoted '"' <++ quoted '\'' <++ munch1 (\c -> c /= ' ' && c /= '\t' && c /= '\n' && c /= '"' && c /= '\'')
    -- 读取不带转义解释的引号参数。
    quoted quote = char quote *> munch (/= quote) <* char quote

-- 有界读取 UTF-8 声明并拒绝重复字段。
loadNormalizationDeclaration :: FilePath -> IO (Either Text NormalizationDeclaration)
loadNormalizationDeclaration path = do
    loaded <- try (withBinaryFile path ReadMode (\handle -> BS.hGet handle 65537)) :: IO (Either IOException BS.ByteString)
    pure $ case loaded of
        Left err -> Left (T.pack (show err))
        Right raw -> do
            value <- decodeNormalizationJSON (BL.fromStrict raw)
            case A.fromJSON value of
                A.Error message -> Left (T.pack message)
                A.Success declaration -> Right declaration

-- 保留报告字段并选择文本或 JSON 显示。
renderNormalization :: Bool -> A.Value -> Text
renderNormalization True = jsonText
renderNormalization False = pretty 0

-- 将 JSON 编码为 UTF-8 文本。
jsonText :: A.Value -> Text
jsonText = TE.decodeUtf8 . BL.toStrict . A.encode

-- 缩进展示完整报告且保留所有证据。
pretty :: Int -> A.Value -> Text
pretty depth (A.Object fields) = T.intercalate "\n" [indent <> K.toText key <> ":\n" <> pretty (depth + 1) value | (key, value) <- KM.toList fields]
  where indent = T.replicate depth "  "
pretty depth value = T.replicate depth "  " <> jsonText value

-- 发送声明并让失败决定 CLI 退出码。
runNormalizationOnce :: Session -> Text -> FilePath -> Bool -> IO Bool
runNormalizationOnce session table path jsonOutput = do
    declaration <- loadNormalizationDeclaration path
    database <- sessionDatabase session
    result <- case declaration of
        Left message -> pure (Left message)
        Right d -> analyzeNormalization session (NormalizationRequest 1 database table d Nothing Nothing)
    case result of
        Left message -> TIO.hPutStrLn stderr ("chusql: error: " <> message) >> pure False
        Right analysis -> TIO.putStrLn (renderNormalization jsonOutput analysis) >> pure True
