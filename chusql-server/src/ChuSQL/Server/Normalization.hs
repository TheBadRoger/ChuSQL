{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.Normalization (normalizationBudget, normalizationReport, schemaFingerprint) where

import qualified ChuSQL.Core.Engine.Normalization as N
import ChuSQL.Core.Model (Column (..), parseColumnType)
import ChuSQL.Core.Protocol (SchemaColumn (..), valueToJSON)
import ChuSQL.Interface.Normalization
import Control.Concurrent.MVar
import Control.Concurrent.QSem
import Control.Exception (bracket_, evaluate, mask)
import Crypto.Hash (Digest, SHA256, hash)
import Data.Aeson (object, (.=))
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import Data.List (nub, sort)
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

-- 范式服务：有界执行、稳定 schema 指纹与引擎报告的线上表示。

{-# NOINLINE analysisSlots #-}
-- 保存最多八个请求和两个执行槽。
analysisSlots :: (MVar Int, QSem)
analysisSlots = unsafePerformIO ((,) <$> newMVar 0 <*> newQSem 2)

-- 限制排队、执行时间及输出大小。
normalizationBudget :: IO (Either (Text, Text) A.Value) -> IO (Either (Text, Text) A.Value)
normalizationBudget action = mask $ \restore -> do
    let (queued, slots) = analysisSlots
    admitted <- modifyMVar queued $ \count -> pure (if count < 8 then (count + 1, True) else (count, False))
    if not admitted
        then pure (Left ("too_busy", "normalization queue is full"))
        else bracket_ (pure ()) (modifyMVar_ queued (pure . subtract 1)) $ do
            result <- restore $ timeout 5000000 $ bracket_ (waitQSem slots) (signalQSem slots) $ do
                outcome <- action
                case outcome of
                    Left err -> pure (Left err)
                    Right value -> do
                        size <- evaluate (BL.length (BL.take 64001 (A.encode value)))
                        pure (if size > 64000 then Left ("too_large", "normalization report exceeds transport budget") else Right value)
            pure (fromMaybe (Left ("timeout", "normalization exceeded its five second budget")) result)

-- 编码只影响语义的列信息。
fingerprintColumn :: SchemaColumn -> A.Value
fingerprintColumn c =
    A.toJSON
        [ A.toJSON (scName c)
        , A.toJSON (scType c)
        , A.toJSON (scNullable c)
        , maybe A.Null valueToJSON (scDefault c)
        , A.toJSON (scAutoIncrement c)
        , A.toJSON (scPrimaryKey c)
        , A.toJSON (scUnique c)
        , A.toJSON (scCheck c)
        ]

-- 将稳定编号与列结构绑定到 SHA256。
schemaFingerprint :: ObjectIdentity -> [SchemaColumn] -> Text
schemaFingerprint (ObjectIdentity database table) columns = T.pack (show (hash (BL.toStrict (A.encode (A.toJSON [A.toJSON [show database, show table], A.toJSON (map fingerprintColumn columns)]))) :: Digest SHA256))

-- 绑定 schema 并编码只读分析报告。
normalizationReport :: ObjectIdentity -> [SchemaColumn] -> Text -> NormalizationRequest -> Either (Text, Text) A.Value
normalizationReport identity columns view payload = do
    let fingerprint = schemaFingerprint identity columns
    if maybe False (/= identity) (nrExpectedObject payload) || maybe False (/= fingerprint) (nrExpectedSchema payload)
        then Left ("conflict", "normalization object or schema has changed")
        else Right ()
    schema <- mapM column columns
    let declaration = nrDeclaration payload
        axioms = N.Declaration (ndKeys declaration) [N.Dependency a b | NormalizationDependency a b <- ndDependencies declaration] (ndComplete declaration) (ndAtomic declaration)
    analysis <- either (Left . (,) "bad_request" . T.pack) Right (N.analyzeNormalization schema axioms)
    Right
        ( object
            [ "api_version" .= (1 :: Int)
            , "database" .= nrDatabase payload
            , "table" .= last (T.splitOn "." (nrTable payload))
            , "object" .= identity
            , "schema_fingerprint" .= fingerprint
            , "view" .= view
            , "schema" .= columns
            , "declaration" .= canonicalDeclaration declaration
            , "candidate_keys" .= N.candidateKeys analysis
            , "findings" .= map findingJSON (N.findings analysis)
            , "suggestions" .= map decompositionJSON (N.suggestions analysis)
            , "conditional" .= True
            , "condition" .= ("Analysis is conditional on the declarations; SQL NULL, UNIQUE and bag semantics are not proofs of classical keys or lossless data migration." :: Text)
            ]
        )
  where
    -- 还原列类型及声明约束而不读取数据。
    column c = case parseColumnType (scType c) of
        Nothing -> Left ("storage_error", "normalization: invalid schema type")
        Just ty -> Right (scName c, Column ty (scNullable c) (scDefault c) (scAutoIncrement c) (scUnique c) (scPrimaryKey c) (scCheck c))

-- 规范化经验证的声明属性集合。
canonicalDeclaration :: NormalizationDeclaration -> NormalizationDeclaration
canonicalDeclaration d =
    d
        { ndKeys = sort (nub (map N.canonical (ndKeys d)))
        , ndDependencies = [NormalizationDependency a b | (a, b) <- sort (nub [(N.canonical a, N.canonical b) | NormalizationDependency a b <- ndDependencies d])]
        }

-- 编码稳定范式标签及证据。
findingJSON :: N.Finding -> A.Value
findingJSON f =
    object
        [ "normal_form" .= formTag (N.normalForm f)
        , "verdict" .= verdictTag (N.verdict f)
        , "witnesses" .= [NormalizationDependency a b | N.Dependency a b <- N.witnesses f]
        , "condition" .= N.condition f
        ]

-- 编码一次分解的理论结论。
decompositionJSON :: N.Decomposition -> A.Value
decompositionJSON d = object ["relations" .= N.relations d, "lossless_join" .= verdictTag (N.losslessJoin d), "dependency_preserving" .= verdictTag (N.dependencyPreserving d)]

-- 输出稳定的范式名称。
formTag :: N.NormalForm -> Text
formTag N.NF1 = "1nf"
formTag N.NF2 = "2nf"
formTag N.NF3 = "3nf"
formTag N.BCNF = "bcnf"

-- 输出稳定的结论标签。
verdictTag :: N.Verdict -> Text
verdictTag N.Satisfied = "satisfied"
verdictTag N.Violated = "violated"
verdictTag N.Insufficient = "insufficient"
