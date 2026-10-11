{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Interface.Normalization where

import ChuSQL.Core.Protocol (SchemaColumn)
import Control.Monad (unless, when)
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.:?), (.!=), (.=))
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy as BL
import Data.Char (isAlpha, isAlphaNum, isDigit, isHexDigit, isSpace)
import Data.List (nub)
import Data.Scientific (base10Exponent, toBoundedInteger)
import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Text.ParserCombinators.ReadP

-- 范式入口的版本化声明、对象身份与严格 JSON 校验。

data NormalizationDependency = NormalizationDependency [String] [String] deriving (Eq, Show)
data NormalizationDeclaration = NormalizationDeclaration
    { ndKeys :: [[String]], ndDependencies :: [NormalizationDependency]
    , ndComplete :: Bool, ndAtomic :: Maybe Bool
    } deriving (Eq, Show)
data ObjectIdentity = ObjectIdentity Integer Integer deriving (Eq, Show)
data NormalizationRequest = NormalizationRequest
    { nrVersion :: Int, nrDatabase :: Text, nrTable :: Text
    , nrDeclaration :: NormalizationDeclaration
    , nrExpectedObject :: Maybe ObjectIdentity, nrExpectedSchema :: Maybe Text
    } deriving (Eq, Show)

-- 拒绝对象中未定义的字段。
onlyFields :: [Text] -> A.Object -> AT.Parser ()
onlyFields names fields = unless (all ((`elem` names) . K.toText) (KM.keys fields)) (fail "normalization: unknown field")

instance FromJSON NormalizationDependency where
    -- 读取依赖两端的确切字段名。
    parseJSON = withObject "dependency" $ \o -> do
        onlyFields ["lhs", "rhs"] o
        lhs <- o .: "lhs"
        rhs <- o .: "rhs"
        when (null rhs) (fail "normalization: empty dependency rhs")
        pure (NormalizationDependency lhs rhs)
instance ToJSON NormalizationDependency where
    -- 编码声明依赖。
    toJSON (NormalizationDependency lhs rhs) = object ["lhs" .= lhs, "rhs" .= rhs]
instance FromJSON NormalizationDeclaration where
    -- 读取显式声明并限制数量。
    parseJSON = withObject "declaration" $ \o -> do
        onlyFields ["declared_keys", "dependencies", "dependencies_complete", "atomic_attributes"] o
        keys <- o .:? "declared_keys" .!= []
        deps <- o .:? "dependencies" .!= []
        when (length keys > 256 || length deps > 256) (fail "normalization: declaration exceeds 256 entries")
        NormalizationDeclaration keys deps <$> o .:? "dependencies_complete" .!= False <*> o .:? "atomic_attributes"
instance ToJSON NormalizationDeclaration where
    -- 编码全部声明标志。
    toJSON d = object ["declared_keys" .= ndKeys d, "dependencies" .= ndDependencies d,
        "dependencies_complete" .= ndComplete d, "atomic_attributes" .= ndAtomic d]
instance FromJSON ObjectIdentity where
    -- 校验稳定对象的正整数编号。
    parseJSON = withObject "object identity" $ \o -> do
        onlyFields ["database_id", "object_id"] o
        db <- o .: "database_id" >>= identityNumber
        table <- o .: "object_id" >>= identityNumber
        when (db <= 0 || table <= 0) (fail "normalization: invalid object identity")
        pure (ObjectIdentity db table)
-- 兼容目录数值并保留浏览器中的完整编号。
identityNumber :: A.Value -> AT.Parser Integer
identityNumber (A.String text)
    | not (T.null text) && T.length text <= 20 && T.all isDigit text = case reads (T.unpack text) of
        [(value, "")] | value <= toInteger (maxBound :: Word64) -> pure value
        _ -> fail "normalization: invalid identity number"
identityNumber (A.Number number)
    | base10Exponent number >= -20 && base10Exponent number <= 20 =
        maybe (fail "normalization: invalid identity number") (pure . toInteger) (toBoundedInteger number :: Maybe Word64)
identityNumber _ = fail "normalization: invalid identity number"

instance ToJSON ObjectIdentity where
    -- 编码对象的库及表编号。
    toJSON (ObjectIdentity db table) = object ["database_id" .= T.pack (show db), "object_id" .= T.pack (show table)]
instance FromJSON NormalizationRequest where
    -- 校验版本、范围和预期身份。
    parseJSON = withObject "normalization request" $ \o -> do
        onlyFields ["api_version", "database", "table", "declaration", "expected_object", "expected_schema_fingerprint"] o
        version <- o .: "api_version"
        when (version /= (1 :: Int)) (fail "normalization: unsupported api_version")
        db <- o .: "database"
        table <- o .: "table"
        unless (plainName db && case T.splitOn "." table of [name] -> plainName name; [scope, name] -> scope == db && plainName name; _ -> False)
            (fail "normalization: invalid or cross-database table name")
        NormalizationRequest version db table <$> o .: "declaration" <*> o .:? "expected_object" <*> o .:? "expected_schema_fingerprint"
instance ToJSON NormalizationRequest where
    -- 编码专用分析请求。
    toJSON r = object ["api_version" .= nrVersion r, "database" .= nrDatabase r, "table" .= nrTable r,
        "declaration" .= nrDeclaration r, "expected_object" .= nrExpectedObject r, "expected_schema_fingerprint" .= nrExpectedSchema r]

-- 校验与 SQL 一致的裸标识符。
plainName :: Text -> Bool
plainName name = not (T.null name) && T.length name <= 64 && (isAlpha (T.head name) || T.head name == '_') && T.all (\c -> isAlphaNum c || c == '_') name

-- 检查大小、编码及重复 JSON 字段。
decodeNormalizationJSON :: BL.ByteString -> Either Text A.Value
decodeNormalizationJSON raw
    | BL.length raw > 65536 = Left "normalization: request exceeds 64 KiB"
    | otherwise = do
        value <- either (Left . T.pack) Right (A.eitherDecode raw)
        text <- either (const (Left "normalization: invalid UTF-8")) Right (TE.decodeUtf8' (BL.toStrict raw))
        case readP_to_S (skipSpaces *> jsonNode 0 <* skipSpaces <* eof) (T.unpack text) of
            [(True, "")] -> Right value
            _ -> Left "normalization: duplicate JSON fields or nesting exceeds 64"

-- 遍历 JSON 结构并保留原始字段名。
jsonNode :: Int -> ReadP Bool
jsonNode depth
    | depth > 64 = pfail
    | otherwise = objectNode <++ arrayNode <++ (True <$ jsonString) <++ (True <$ munch1 (\c -> not (isSpace c) && c `notElem` (",]}" :: String)))
  where
    -- 验证对象及所有嵌套值。
    objectNode = do
        _ <- char '{'
        skipSpaces
        pairs <- sepBy pair (skipSpaces *> char ',' <* skipSpaces)
        skipSpaces
        _ <- char '}'
        pure (length (map fst pairs) == length (nub (map fst pairs)) && all snd pairs)
    -- 保留键名并验证对应值。
    pair = do
        key <- jsonString
        skipSpaces
        _ <- char ':'
        skipSpaces
        valid <- jsonNode (depth + 1)
        pure (key, valid)
    -- 验证数组的所有元素。
    arrayNode = do
        _ <- char '['
        skipSpaces
        values <- sepBy (jsonNode (depth + 1)) (skipSpaces *> char ',' <* skipSpaces)
        skipSpaces
        _ <- char ']'
        pure (and values)

-- 解码字符串以识别转义后的相同键。
jsonString :: ReadP Text
jsonString = do
    _ <- char '"'
    pieces <- many piece
    _ <- char '"'
    case A.eitherDecodeStrict (TE.encodeUtf8 (T.pack ('"' : concat pieces ++ "\""))) of
        Right value -> pure value
        Left _ -> pfail
  where
    -- 保留 JSON 字符串转义。
    piece = ((\c -> ['\\', c]) <$> (char '\\' *> get)) <++ munch1 (\c -> c /= '"' && c /= '\\')


-- 拒绝缺字段或未知标签的线上报告。
parseNormalizationReport :: A.Value -> AT.Parser A.Value
parseNormalizationReport value = withObject "normalization report" (\o -> do
    version <- o .: "api_version"
    conditional <- o .: "conditional"
    unless (version == (1 :: Int) && conditional) (fail "normalization: invalid report version or condition")
    _ <- (o .: "object" :: AT.Parser ObjectIdentity)
    fingerprint <- o .: "schema_fingerprint"
    unless (T.length fingerprint == 64 && T.all isHexDigit fingerprint) (fail "normalization: invalid schema fingerprint")
    view <- o .: "view"
    unless (view `elem` (["live_schema", "transaction_snapshot"] :: [Text])) (fail "normalization: unknown view")
    _ <- (o .: "schema" :: AT.Parser [SchemaColumn])
    _ <- (o .: "declaration" :: AT.Parser NormalizationDeclaration)
    _ <- (o .: "candidate_keys" :: AT.Parser [[String]])
    forms <- o .: "findings" >>= mapM parseFinding
    unless (forms == (["1nf", "2nf", "3nf", "bcnf"] :: [Text])) (fail "normalization: invalid findings")
    _ <- (o .: "suggestions" :: AT.Parser [A.Value])
    _ <- (o .: "condition" :: AT.Parser Text)
    pure value) value
  where
    -- 核对每项结论与证据的稳定形状。
    parseFinding = withObject "normalization finding" $ \o -> do
        form <- o .: "normal_form"
        verdict <- o .: "verdict"
        unless (verdict `elem` (["satisfied", "violated", "insufficient"] :: [Text])) (fail "normalization: unknown verdict")
        _ <- (o .: "witnesses" :: AT.Parser [NormalizationDependency])
        _ <- (o .: "condition" :: AT.Parser Text)
        pure form
