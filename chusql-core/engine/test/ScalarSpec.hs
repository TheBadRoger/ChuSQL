module ScalarSpec (spec) where

import ChuSQL.Core.Engine (rowsOf, runStatement)
import ChuSQL.Core.Engine.Runtime.Query (bindQuery, executeQueryM)
import ChuSQL.Core.Engine.Storage.Memory (runMemoryStorage)
import ChuSQL.Core.Engine.Syntax.Parser (parseStatement)
import ChuSQL.Core.Model
import ChuSQL.Core.Runtime (TypeCapability (OrdCapability))
import ChuSQL.Core.Scalar (normalizeScalar)
import Control.Monad (foldM)
import Data.Either (isLeft)
import Data.List (isInfixOf)
import Test.Hspec

-- 业务标量的格式边界、规范表示及完整 SQL 行为。

-- 验证新增类型和别名的实际行为。
spec :: Spec
spec = do
    describe "native business scalars" $ do
        it "normalizes RFC 3339 offsets and fixed-precision fractional seconds" $ do
            normalizeScalar ScalarTimestampTZ "2026-01-01T00:00:00.25+08:00" `shouldBe` Right "2025-12-31T16:00:00.250000Z"
            normalizeScalar ScalarTimestampTZ "2025-12-31t16:00:00.250000z" `shouldBe` Right "2025-12-31T16:00:00.250000Z"
            normalizeScalar ScalarTime "12:34:56.1" `shouldBe` Right "12:34:56.100000"
            normalizeScalar ScalarTimeTZ "00:30:00+01:00" `shouldBe` Right "23:30:00.000000Z"
            map (normalizeScalar ScalarTimestampTZ)
                [ "2026-02-29T00:00:00Z", "2024-13-01T00:00:00Z", "2026-01-01T24:00:00Z"
                , "2026-01-01T00:00:60Z", "2026-01-01T00:00:00", "2026-01-01T00:00:00-00:00"
                , "2026-01-01T00:00:00+24:00", "2026-01-01T00:00:00.1234567Z"
                ] `shouldSatisfy` all isLeft
            normalizeScalar ScalarTimestampTZ "2024-02-29T00:00:00Z" `shouldBe` Right "2024-02-29T00:00:00.000000Z"
        it "validates and normalizes UUID and MAC addresses" $ do
            normalizeScalar ScalarUUID "550E8400-E29B-41D4-A716-446655440000" `shouldBe` Right "550e8400-e29b-41d4-a716-446655440000"
            normalizeScalar ScalarUUID "550e8400e29b41d4a716446655440000" `shouldSatisfy` isLeft
            normalizeScalar ScalarUUID "550g8400-e29b-41d4-a716-446655440000" `shouldSatisfy` isLeft
            normalizeScalar ScalarMAC "AA-BB-CC-00-11-22" `shouldBe` Right "aa:bb:cc:00:11:22"
            normalizeScalar ScalarMAC "aa:bb:cc:00:11:222" `shouldSatisfy` isLeft
        it "canonicalizes JSON keys and numbers while distinguishing JSON null from SQL NULL" $ do
            normalizeScalar ScalarJSON "{\"b\":2.0,\"a\":1}" `shouldBe` Right "{\"a\":1,\"b\":2}"
            normalizeScalar ScalarJSON "{\"nested\":{\"z\":0,\"a\":[true,null]}}" `shouldBe` Right "{\"nested\":{\"a\":[true,null],\"z\":0}}"
            normalizeScalar ScalarJSON "{\"a\":1,\"a\":2}" `shouldBe` Right "{\"a\":1}"
            coerceValue (CScalar ScalarJSON) (VStr "null") `shouldBe` Right (VStr "null")
            coerceValue (CScalar ScalarJSON) VNull `shouldBe` Right VNull
            normalizeScalar ScalarJSON "{bad}" `shouldSatisfy` isLeft
        it "normalizes IPv4, compressed IPv6, mapped addresses and CIDR network boundaries" $ do
            normalizeScalar ScalarInet "192.0.2.1" `shouldBe` Right "192.0.2.1"
            normalizeScalar ScalarInet "2001:DB8::1" `shouldBe` Right "2001:0db8:0000:0000:0000:0000:0000:0001"
            normalizeScalar ScalarInet "::ffff:192.0.2.1" `shouldBe` Right "0000:0000:0000:0000:0000:ffff:c000:0201"
            map (normalizeScalar ScalarInet) ["256.0.0.1", "01.2.3.4", "1::2::3", "1:2:3:4:5:6:7", "192.0.2.1::", "::1%eth0"] `shouldSatisfy` all isLeft
            normalizeScalar ScalarCIDR "192.0.2.0/24" `shouldBe` Right "192.0.2.0/24"
            normalizeScalar ScalarCIDR "2001:db8::/32" `shouldBe` Right "2001:0db8:0000:0000:0000:0000:0000:0000/32"
            map (normalizeScalar ScalarCIDR) ["192.0.2.1/24", "192.0.2.0/33", "::1/0", "::/-1"] `shouldSatisfy` all isLeft
        it "normalizes ISO durations without treating a calendar month as a fixed number of days" $ do
            normalizeScalar ScalarInterval "P1Y2M" `shouldBe` Right "P14M0DT0.000000S"
            normalizeScalar ScalarInterval "P2W" `shouldBe` Right "P0M14DT0.000000S"
            normalizeScalar ScalarInterval "PT25H" `shouldBe` Right "P0M1DT3600.000000S"
            normalizeScalar ScalarInterval "-PT0.25S" `shouldBe` Right "-P0M0DT0.250000S"
            normalizeScalar ScalarInterval "-P0D" `shouldBe` Right "P0M0DT0.000000S"
            map (normalizeScalar ScalarInterval) ["P", "PT", "P1WT1H", "PT1.1234567S", "P1D1Y"] `shouldSatisfy` all isLeft
        it "keeps type identities and only advertises meaningful ordering" $ do
            comparableTypes (CScalar ScalarUUID) (CScalar ScalarJSON) `shouldBe` False
            assignable (CScalar ScalarUUID) CStr `shouldBe` True
            assignable (CScalar ScalarUUID) (CScalar ScalarJSON) `shouldBe` False
            hasTypeCapability OrdCapability (CScalar ScalarJSON) `shouldBe` False
            hasTypeCapability OrdCapability (CScalar ScalarInet) `shouldBe` False
            hasTypeCapability OrdCapability (CScalar ScalarInterval) `shouldBe` False
            map (\(_, kind) -> parseColumnType (typeName (CScalar kind))) scalarTypes `shouldBe` map (Just . CScalar . snd) scalarTypes
    describe "business scalar SQL" $ do
        it "binds aliases and standard time zone declarations" $ do
            mapM_
                (\ty -> sqlCatalog ["CREATE TABLE sample (value " ++ ty ++ ")"] `shouldSatisfy` either (const False) (const True))
                ["UUID", "JSONB", "RFC3339", "TIME", "TIMETZ", "TIMESTAMP WITH TIME ZONE", "TIME WITH TIME ZONE", "TIME WITHOUT TIME ZONE", "INT2", "FLOAT8", "BYTEA"]
        it "normalizes writes, typed literals and contextual equality including IN" $ do
            let result = do
                    db <- fixture
                    query db "SELECT id FROM records WHERE uid = '550E8400-E29B-41D4-A716-446655440000' AND created = '2026-01-01T00:00:00+08:00' AND payload IN ('{\"b\":2,\"a\":1}')"
            result `shouldBe` Right [[VInt 1]]
            query [] "SELECT UUID '550E8400-E29B-41D4-A716-446655440000' AS uid" `shouldBe` Right [[VStr "550e8400-e29b-41d4-a716-446655440000"]]
        it "keeps bound and interpreted results equal and orders UTC microseconds correctly" $ do
            let result = do
                    db <- fixture
                    statement <- parseStatement "SELECT id FROM records ORDER BY created"
                    bound <- bindQuery db statement
                    interpreted <- rowsOf (runStatement db statement)
                    (executed, _) <- runMemoryStorage (executeQueryM bound) db
                    pure (interpreted, executed)
            result `shouldBe` Right ([[ ("id", VInt 1)], [("id", VInt 2)]], Right [[("id", VInt 1)], [("id", VInt 2)]])
        it "uses canonical JSON equality for grouping and refuses undefined JSON ordering" $ do
            let result = do
                    db <- fixture
                    query db "SELECT COUNT(*) AS n FROM records GROUP BY payload"
            result `shouldBe` Right [[VInt 2]]
            (fixture >>= \db -> query db "SELECT payload FROM records ORDER BY payload") `shouldSatisfy` either (isInfixOf "type capability") (const False)
        it "validates defaults and inherited domains, rejecting invalid inserts and updates" $ do
            let base = sqlCatalog ["CREATE DOMAIN public_id AS UUID NOT NULL", "CREATE TABLE ids (id INT, uid public_id DEFAULT '550E8400-E29B-41D4-A716-446655440000')", "INSERT INTO ids (id) VALUES (1)"]
            (base >>= \db -> query db "SELECT uid FROM ids") `shouldBe` Right [[VStr "550e8400-e29b-41d4-a716-446655440000"]]
            (fixture >>= \db -> parseStatement "UPDATE records SET payload = '{bad}'" >>= runStatement db) `shouldSatisfy` isLeft

-- 构造包含不同输入形状的同一规范数据。
fixture :: Either String Database
fixture = sqlCatalog
    [ "CREATE TABLE records (id INT, uid UUID, created TIMESTAMPTZ, payload JSONB)"
    , "INSERT INTO records (id, uid, created, payload) VALUES (1, '550E8400-E29B-41D4-A716-446655440000', '2026-01-01T00:00:00+08:00', '{\"b\":2,\"a\":1}')"
    , "INSERT INTO records (id, uid, created, payload) VALUES (2, '550e8400-e29b-41d4-a716-446655440001', '2025-12-31T16:00:00.1Z', '{\"a\":1.0,\"b\":2}')"
    ]

-- 按顺序执行内存数据库的准备语句。
sqlCatalog :: [String] -> Either String Database
sqlCatalog = foldM apply []
  where
    -- 更新语句返回的当前数据库。
    apply db source = parseStatement source >>= fmap fst . runStatement db

-- 通过正式 SQL 入口取得值列表。
query :: Database -> String -> Either String [[Value]]
query db source = map (map snd) <$> (parseStatement source >>= rowsOf . runStatement db)
