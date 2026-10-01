{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.CLI.Format
    ( OutputFormat (..)
    , parseFormat
    , formatName
    , renderResult
    , renderTable
    , renderRows
    , renderCsv
    , renderJson
    , displayValue
    ) where

import ChuSQL.CLI.Session (QueryResult (..), queryResultJson)
import ChuSQL.Model (Value (..))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8)

-- 结果输出：表格、JSON、CSV 三种格式。

data OutputFormat = FormatTable | FormatJson | FormatCsv
    deriving (Eq, Show)

formatName :: OutputFormat -> Text
formatName FormatTable = "table"
formatName FormatJson = "json"
formatName FormatCsv = "csv"

-- | 认一个格式名
parseFormat :: Text -> Maybe OutputFormat
parseFormat raw = case T.toLower (T.strip raw) of
    "table" -> Just FormatTable
    "json" -> Just FormatJson
    "csv" -> Just FormatCsv
    _ -> Nothing

renderResult :: OutputFormat -> QueryResult -> Text
renderResult FormatTable = renderTable
renderResult FormatJson = renderJson
renderResult FormatCsv = renderCsv

-- | 紧凑 JSON，方便管道里再用
renderJson :: QueryResult -> Text
renderJson = decodeUtf8 . BL.toStrict . A.encode . queryResultJson

renderCsv :: QueryResult -> Text
renderCsv result = T.unlines (header : map rowLine (qrRows result))
  where
    header = T.intercalate "," (map csvField (qrColumns result))
    rowLine row = T.intercalate "," (map (csvField . csvCell) row)

-- | CSV 里的空值就是空字段
csvCell :: Value -> Text
csvCell VNull = ""
csvCell value = displayValue value

-- | 按 RFC 4180 给需要的字段加引号
csvField :: Text -> Text
csvField field
    | T.any (`elem` (",\"\n\r" :: String)) field = "\"" <> T.replace "\"" "\"\"" field <> "\""
    | otherwise = field

renderTable :: QueryResult -> Text
renderTable result
    | null (qrColumns result) = "OK"
    | otherwise = renderRows (qrColumns result) (map (map displayValue) (qrRows result))

-- | 画一张等宽表，列太宽就截断
renderRows :: [Text] -> [[Text]] -> Text
renderRows columns rawRows = T.unlines (border : headerLine : border : body ++ [border, summary])
  where
    widths = [columnWidth index | index <- [0 .. length columns - 1]]
    columnWidth index =
        min maxColumnWidth (maximum (T.length (columns !! index) : [T.length (cellAt row index) | row <- rawRows]))
    maxColumnWidth = 64
    cellAt row index = if index < length row then row !! index else ""
    cellLine cells = "| " <> T.intercalate " | " (zipWith pad widths (cells ++ repeat "")) <> " |"
    headerLine = cellLine columns
    body = map cellLine rawRows
    border = "+" <> T.intercalate "+" ["-" <> T.replicate (width + 2) "-" | width <- widths] <> "+"
    summary = "(" <> T.pack (show (length rawRows)) <> " row" <> (if length rawRows == 1 then "" else "s") <> ")"

-- | 补到指定宽度，超出的按 ... 截断
pad :: Int -> Text -> Text
pad width text = clipped <> T.replicate (width - T.length clipped) " "
  where
    clipped
        | T.length text <= width = text
        | width <= 3 = T.take width text
        | otherwise = T.take (width - 3) text <> "..."

displayValue :: Value -> Text
displayValue VNull = "NULL"
displayValue (VInt n) = T.pack (show n)
displayValue (VFloat d) = T.pack (show d)
displayValue (VStr s) = T.pack s
displayValue (VBool True) = "true"
displayValue (VBool False) = "false"
