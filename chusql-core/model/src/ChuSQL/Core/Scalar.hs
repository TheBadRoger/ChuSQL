module ChuSQL.Core.Scalar (
    ScalarType (..), scalarTypes, scalarName, normalizeScalar, scalarOrdering,
) where

import qualified Data.Aeson as A
import qualified Data.Aeson.Encoding as E
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Control.Monad (when)
import Data.Char (isDigit, isHexDigit, toLower)
import Data.List (intercalate, sortOn)
import Data.Maybe (fromMaybe, isJust, isNothing)
import Data.Scientific (normalize)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (UTCTime, ZonedTime, defaultTimeLocale, formatTime, zonedTimeToUTC)
import Data.Time.Format.ISO8601 (iso8601ParseM)
import qualified Data.Vector as V
import Numeric (readHex, showHex)
import Text.ParserCombinators.ReadP
import Text.Read (readMaybe)

-- 常用业务标量的输入校验、规范表示和能力边界。

data ScalarType
    = ScalarTime
    | ScalarTimeTZ
    | ScalarTimestampTZ
    | ScalarUUID
    | ScalarJSON
    | ScalarInet
    | ScalarCIDR
    | ScalarMAC
    | ScalarInterval
    deriving (Eq, Show)

-- 集中声明 SQL 类型名与兼容别名。
scalarTypes :: [(String, ScalarType)]
scalarTypes =
    [ ("time", ScalarTime), ("timetz", ScalarTimeTZ)
    , ("timestamptz", ScalarTimestampTZ), ("rfc3339", ScalarTimestampTZ)
    , ("uuid", ScalarUUID), ("json", ScalarJSON), ("jsonb", ScalarJSON)
    , ("inet", ScalarInet), ("cidr", ScalarCIDR), ("macaddr", ScalarMAC)
    , ("interval", ScalarInterval)
    ]

-- 返回落盘时使用的规范类型名。
scalarName :: ScalarType -> String
scalarName ScalarTime = "time"
scalarName ScalarTimeTZ = "timetz"
scalarName ScalarTimestampTZ = "timestamptz"
scalarName ScalarUUID = "uuid"
scalarName ScalarJSON = "json"
scalarName ScalarInet = "inet"
scalarName ScalarCIDR = "cidr"
scalarName ScalarMAC = "macaddr"
scalarName ScalarInterval = "interval"

-- 仅为规范文本可正确排序的类型开放顺序比较。
scalarOrdering :: ScalarType -> Bool
scalarOrdering kind = kind `elem` [ScalarTime, ScalarTimeTZ, ScalarTimestampTZ, ScalarUUID, ScalarMAC]

-- 校验输入并生成用于相等、哈希和存储的规范文本。
normalizeScalar :: ScalarType -> String -> Either String String
normalizeScalar kind input = maybe (Left ("invalid " ++ scalarName kind ++ " value")) Right $ case kind of
    ScalarTime -> clock input
    ScalarTimeTZ -> formatClock <$> zoned ("2000-01-02T" ++ input)
    ScalarTimestampTZ -> formatTimestamp <$> zoned input
    ScalarUUID -> uuid input
    ScalarJSON -> canonicalJSON input
    ScalarInet -> uncurry renderAddress <$> address input
    ScalarCIDR -> network input
    ScalarMAC -> mac input
    ScalarInterval -> duration input

-- 校验时分秒及最多六位小数，并固定微秒精度。
clock :: String -> Maybe String
clock input = case split '.' input of
    [whole] -> validate whole ""
    [whole, fraction] | not (null fraction), length fraction <= 6, all isDigit fraction -> validate whole fraction
    _ -> Nothing
  where
    -- 拒绝越界时间和未支持的闰秒。
    validate whole fraction = case split ':' whole of
        [hours, minutes, seconds]
            | bounded 2 23 hours, bounded 2 59 minutes, bounded 2 59 seconds ->
                Just (whole ++ "." ++ take 6 (fraction ++ repeat '0'))
        _ -> Nothing

-- 读取带明确时区的 RFC 3339 时间，规范为 UTC。
zoned :: String -> Maybe UTCTime
zoned input = do
    let (date, rest) = break (\ch -> ch == 'T' || ch == 't') input
    timestamp <- case rest of _ : body -> Just body; [] -> Nothing
    let (local, zone) = break (`elem` "Zz+-") timestamp
    _ <- clock local
    when (length date /= 10 || not (validZone zone)) Nothing
    let offset = if zone == "Z" || zone == "z" then "+00:00" else zone
    parsed <- iso8601ParseM (date ++ "T" ++ local ++ offset) :: Maybe ZonedTime
    let utc = zonedTimeToUTC parsed
    if length (formatTime defaultTimeLocale "%Y" utc) == 4 then Just utc else Nothing

-- 校验已知时区偏移，不把未知偏移当作 UTC。
validZone :: String -> Bool
validZone "Z" = True
validZone "z" = True
validZone "-00:00" = False
validZone (sign : hours) | sign `elem` "+-" = case split ':' hours of
    [h, m] -> bounded 2 23 h && bounded 2 59 m
    _ -> False
validZone _ = False

-- 输出固定微秒精度的 UTC 时间戳。
formatTimestamp :: UTCTime -> String
formatTimestamp time = formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%S" time ++ "." ++ take 6 (formatTime defaultTimeLocale "%q" time) ++ "Z"

-- 输出固定微秒精度的 UTC 日内时间。
formatClock :: UTCTime -> String
formatClock time = formatTime defaultTimeLocale "%H:%M:%S" time ++ "." ++ take 6 (formatTime defaultTimeLocale "%q" time) ++ "Z"

-- 校验固定宽度的十进制字段。
bounded :: Int -> Int -> String -> Bool
bounded width maximumValue value = length value == width && all isDigit value && maybe False (<= maximumValue) (readMaybe value)

-- 接受标准 UUID 形状并统一为小写。
uuid :: String -> Maybe String
uuid input
    | map length parts == [8, 4, 4, 4, 12], all (all isHexDigit) parts = Just (map toLower input)
    | otherwise = Nothing
  where
    parts = split '-' input

-- 解析 JSON 并统一对象键顺序及数值表示。
canonicalJSON :: String -> Maybe String
canonicalJSON input = do
    value <- either (const Nothing) Just (A.eitherDecodeStrict (TE.encodeUtf8 (T.pack input)))
    pure (T.unpack (TE.decodeUtf8 (BL.toStrict (E.encodingToLazyByteString (encodeJSON value)))))

-- 递归编码规范 JSON，保留数组顺序。
encodeJSON :: A.Value -> A.Encoding
encodeJSON (A.Object fields) = E.pairs (mconcat [E.pair key (encodeJSON value) | (key, value) <- sortOn (K.toText . fst) (KM.toList fields)])
encodeJSON (A.Array values) = E.list encodeJSON (V.toList values)
encodeJSON (A.Number number) = A.toEncoding (A.Number (normalize number))
encodeJSON value = A.toEncoding value

-- 解析 IPv4 或 IPv6，不执行 DNS 查询。
address :: String -> Maybe (Int, Integer)
address input
    | ':' `elem` input = (,) 128 <$> ipv6 input
    | otherwise = (,) 32 <$> ipv4 input

-- 校验四段 IPv4，拒绝有歧义的前导零。
ipv4 :: String -> Maybe Integer
ipv4 input = case split '.' input of
    parts | length parts == 4 -> foldl (\acc part -> (\value number -> value * 256 + number) <$> acc <*> octet part) (Just 0) parts
    _ -> Nothing
  where
    -- 读取一个十进制字节。
    octet part
        | null part || length part > 3 || not (all isDigit part) || (length part > 1 && take 1 part == "0") = Nothing
        | otherwise = do
            value <- readMaybe part
            if value <= 255 then Just value else Nothing

-- 展开 IPv6 压缩段及尾部嵌入的 IPv4。
ipv6 :: String -> Maybe Integer
ipv6 input = do
    words16 <- case map T.unpack (T.splitOn (T.pack "::") (T.pack input)) of
        [whole] -> do
            values <- groups whole
            if length values == 8 then Just values else Nothing
        [left, right] -> do
            when ('.' `elem` left) Nothing
            leading <- groups left
            trailing <- groups right
            if length leading + length trailing < 8
                then Just (leading ++ replicate (8 - length leading - length trailing) 0 ++ trailing)
                else Nothing
        _ -> Nothing
    pure (foldl (\value part -> value * 65536 + part) 0 words16)
  where
    -- 读取冒号分隔的非压缩片段。
    groups "" = Just []
    groups value = expand (split ':' value)
    -- 只允许最后一个片段使用点分 IPv4。
    expand [] = Just []
    expand [part] | '.' `elem` part = do
        value <- ipv4 part
        Just [value `div` 65536, value `mod` 65536]
    expand (part : rest) = do
        value <- hexGroup part
        (value :) <$> expand rest
    -- 读取一个不超过十六位的十六进制组。
    hexGroup part
        | null part || length part > 4 || not (all isHexDigit part) = Nothing
        | otherwise = case readHex part of [(value, "")] -> Just value; _ -> Nothing

-- 将地址输出为唯一的点分或展开十六进制文本。
renderAddress :: Int -> Integer -> String
renderAddress 32 value = intercalate "." [show ((value `div` (256 ^ power)) `mod` 256) | power <- [3, 2, 1, 0 :: Int]]
renderAddress _ value = intercalate ":" [pad 4 (showHex ((value `div` (65536 ^ power)) `mod` 65536) "") | power <- [7, 6 .. 0 :: Int]]

-- 校验 CIDR 前缀及零主机位，保持网络边界明确。
network :: String -> Maybe String
network input = case split '/' input of
    [host, rawPrefix] | not (null rawPrefix), length rawPrefix <= 3, all isDigit rawPrefix -> do
        (bits, value) <- address host
        prefix <- readMaybe rawPrefix
        if prefix > bits || value `mod` (2 ^ (bits - prefix)) /= 0
            then Nothing
            else Just (renderAddress bits value ++ "/" ++ show prefix)
    _ -> Nothing

-- 接受常用冒号或横线分隔的六字节 MAC 地址。
mac :: String -> Maybe String
mac input
    | length parts == 6, all (\part -> length part == 2 && all isHexDigit part) parts = Just (map toLower (intercalate ":" parts))
    | otherwise = Nothing
  where
    parts = split (if ':' `elem` input then ':' else '-') input

-- 解析 ISO 8601 时长，分别保留日历月及固定时长。
duration :: String -> Maybe String
duration input = case readP_to_S (parser <* eof) input of
    [(value, "")] -> Just value
    _ -> Nothing
  where
    -- 按固定顺序读取日历和时钟字段。
    parser = do
        sign <- option "" ((: []) <$> satisfy (`elem` "+-"))
        _ <- char 'P'
        years <- component 'Y'
        months <- component 'M'
        weeks <- component 'W'
        days <- component 'D'
        time <- option Nothing (Just <$> (char 'T' *> clockParts))
        when (not (any isJust [years, months, weeks, days]) && isNothing time) pfail
        when (isJust weeks && (any isJust [years, months, days] || isJust time)) pfail
        let calendar = amount years * 12 + amount months
            micros = amount weeks * 7 * dayMicros + amount days * dayMicros + fromMaybe 0 time
            negative = sign == "-" && (calendar /= 0 || micros /= 0)
            (wholeDays, remainder) = micros `divMod` dayMicros
            (seconds, fraction) = remainder `divMod` 1000000
        pure ((if negative then "-" else "") ++ "P" ++ show calendar ++ "M" ++ show wholeDays ++ "DT" ++ show seconds ++ "." ++ pad 6 (show fraction) ++ "S")
    -- 读取最多十八位无符号十进制数。
    number = do
        digits <- munch1 isDigit
        if length digits > 18 then pfail else pure (read digits :: Integer)
    -- 读取可选的带单位字段。
    component unit = option Nothing (Just <$> (number <* char unit))
    -- 读取时分秒，并把时间部分换算为微秒。
    clockParts = do
        hours <- component 'H'
        minutes <- component 'M'
        seconds <- option Nothing $ do
            whole <- number
            fraction <- option "" (char '.' *> munch1 isDigit)
            when (length fraction > 6) pfail
            _ <- char 'S'
            pure (Just (whole * 1000000 + read (take 6 (fraction ++ repeat '0'))))
        if any isJust [hours, minutes, seconds]
            then pure ((amount hours * 3600 + amount minutes * 60) * 1000000 + amount seconds)
            else pfail
    -- 把缺省字段解释为零。
    amount = fromMaybe 0
    dayMicros = 86400 * 1000000

-- 左侧补零，保留完整数值。
pad :: Int -> String -> String
pad width value = replicate (max 0 (width - length value)) '0' ++ value

-- 按分隔符切分并保留空片段供校验。
split :: Char -> String -> [String]
split separator value = case break (== separator) value of
    (part, []) -> [part]
    (part, _ : rest) -> part : split separator rest
