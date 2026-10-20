{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import ChuSQL.Core.Engine
import ChuSQL.Core.Engine.Algebra.Cost (rangeRows, selectivityOf)
import ChuSQL.Core.Engine.Algebra.Eval (evalRelOp, evalRelOpM)
import ChuSQL.Core.Engine.Algebra.Expr (colsInExpr, evalExpr)
import ChuSQL.Core.Engine.Algebra.Op (RelOp (..), renderPlan)
import ChuSQL.Core.Engine.Algebra.Optimize (optimize, pushProject)
import ChuSQL.Core.Engine.Algebra.Planner (translate)
import ChuSQL.Core.Engine.Algebra.Sort (sortRows, sortRowsTopK)
import ChuSQL.Core.Engine.Builtin
import ChuSQL.Core.Engine.Error
import ChuSQL.Core.Engine.Parallel (poolRun, workerPool)
import qualified ChuSQL.Core.Engine.Runtime.Query as Bound
import ChuSQL.Core.Engine.Semantic (prepare)
import ChuSQL.Core.Engine.Storage (IndexResult (..), MonadStorage (..))
import ChuSQL.Core.Engine.Storage.FFI (closeStorage, maintainStorage, openStorage)
import ChuSQL.Core.Engine.Storage.IPC (IPCStorage (..), StorageLink (..), closeConnection, defaultEnv, doListTables, envForDatabase, getDatabaseName, localStorageLink, runIPCStorage, sendRawRequest, setDatabaseName, setStorageLink)
import ChuSQL.Core.Engine.Storage.Memory (MemoryStorage (runMemoryStorage))
import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Engine.Syntax.Parser
import ChuSQL.Core.Model
import qualified ChuSQL.Core.Protocol as Wire
import Control.Concurrent (forkFinally, forkIO, killThread, threadDelay)
import Control.Concurrent.Chan (newChan, readChan, writeChan)
import Control.Concurrent.MVar (MVar, modifyMVar_, newEmptyMVar, newMVar, putMVar, readMVar, takeMVar, tryPutMVar)
import Control.Exception (ErrorCall, IOException, bracket_, finally, try)
import Control.Monad (replicateM_, void, when, (>=>))
import Data.Aeson.Types (parseEither)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import Data.Either (fromRight)
import Data.Hashable (hash)
import Data.List (isInfixOf, sortOn)
import Data.Unique (hashUnique, newUnique)
import GHC.IO.Encoding (getForeignEncoding, latin1, setForeignEncoding)
import qualified RuntimeSpec
import qualified ScalarSpec
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly, renameFile)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec

-- 引擎层的 hspec 测试：语法、执行、优化器与存储链路行为。

-- | 经实际 IPC 解析并执行领域测试语句
ipcDomainSql :: String -> IO (Either String [Row])
ipcDomainSql sql = case parseStatement sql of
    Left err -> pure (Left err)
    Right statement -> runIPCStorage (runStatementM statement)

-- | 测试表 users：id / name / age
users :: Table
users =
    Table
        { tableSchemaVersion = Nothing
        , tableIdentity = Nothing
        , tableName = "users"
        , tableCols = [("id", TInt), ("name", TStr), ("age", TInt)]
        , tableRows =
            [ [("id", VInt 1), ("name", VStr "Alice"), ("age", VInt 25)]
            , [("id", VInt 2), ("name", VStr "Bob"), ("age", VInt 17)]
            , [("id", VInt 3), ("name", VStr "Carol"), ("age", VInt 30)]
            ]
        , tableMeta = Nothing
        }

-- | 测试表 orders：id/user_id/product
orders :: Table
orders =
    Table
        { tableSchemaVersion = Nothing
        , tableIdentity = Nothing
        , tableName = "orders"
        , tableCols = [("id", TInt), ("user_id", TInt), ("product", TStr)]
        , tableRows =
            [ [("id", VInt 1), ("user_id", VInt 1), ("product", VStr "Book")]
            , [("id", VInt 2), ("user_id", VInt 2), ("product", VStr "Pen")]
            , [("id", VInt 3), ("user_id", VInt 1), ("product", VStr "Cup")]
            ]
        , tableMeta = Nothing
        }

-- | 两个测试表组成的默认库
testDB :: Database
testDB = [("users", users), ("orders", orders)]

-- | 连接链测试表 a：6 行，id 各不相同
chainA :: Table
chainA =
    Table
        { tableSchemaVersion = Nothing
        , tableIdentity = Nothing
        , tableName = "a"
        , tableCols = [("id", TInt), ("name", TStr)]
        , tableRows = [[("id", VInt n), ("name", VStr ("a" ++ show n))] | n <- [1 .. 6]]
        , tableMeta = Just (TableMeta 6 [("id", 6, False)] [] ["id"])
        }

-- | 连接链测试表 b：4 行，a_id 各不相同
chainB :: Table
chainB =
    Table
        { tableSchemaVersion = Nothing
        , tableIdentity = Nothing
        , tableName = "b"
        , tableCols = [("id", TInt), ("a_id", TInt)]
        , tableRows = [[("id", VInt n), ("a_id", VInt n)] | n <- [1 .. 4]]
        , tableMeta = Just (TableMeta 4 [("id", 4, False), ("a_id", 4, False)] [] ["id"])
        }

-- | 连接链测试表 c：2 行，b_id 各不相同
chainC :: Table
chainC =
    Table
        { tableSchemaVersion = Nothing
        , tableIdentity = Nothing
        , tableName = "c"
        , tableCols = [("id", TInt), ("b_id", TInt)]
        , tableRows = [[("id", VInt n), ("b_id", VInt n)] | n <- [1, 2]]
        , tableMeta = Just (TableMeta 2 [("id", 2, False), ("b_id", 2, False)] [] ["id"])
        }

-- | 三张表组成的连接链测试库
chainDB :: Database
chainDB = [("a", chainA), ("b", chainB), ("c", chainC)]

-- | 同一个连接链测试库，但没有统计
chainDBWithoutStats :: Database
chainDBWithoutStats = [(n, t{tableMeta = Nothing}) | (n, t) <- chainDB]

-- | 顺序执行命名类型测试语句
domainSql :: Database -> [String] -> Either String (Database, [Row])
domainSql db [] = Right (db, [])
domainSql db [sql] = parseStatement sql >>= runStatement db
domainSql db (sql : rest) = do
    statement <- parseStatement sql
    (next, _) <- runStatement db statement
    domainSql next rest

-- | 跑引擎层全部 hspec 用例
main :: IO ()
main = hspec $ do
    describe "REVOKE GRANT OPTION FOR" $ do
        it "parses object privileges and rejects membership options" $ do
            parseStatement "REVOKE GRANT OPTION FOR SELECT, UPDATE ON users FROM alice"
                `shouldBe` Right (RevokeGrantOption ["SELECT", "UPDATE"] "users" "alice")
            parseStatement "REVOKE GRANT OPTION FOR reader FROM alice" `shouldSatisfy` either (const True) (const False)
    describe "ORDER BY positions" $ do
        it "sorts the selected output position despite duplicate aliases" $ do
            let sql = "SELECT age AS x, -age AS x FROM users ORDER BY 2 LIMIT 1"
            rowsOf (parseStatement sql >>= runStatement testDB)
                `shouldBe` Right [[("x", VInt 30), ("x", VInt (-30))]]
            sameResultAsUnoptimized sql
            rowsOf (parseStatement "SELECT DISTINCT age AS x, -age AS x FROM users ORDER BY 2 LIMIT 1" >>= runStatement testDB)
                `shouldBe` Right [[("x", VInt 30), ("x", VInt (-30))]]
            rowsOf (parseStatement "SELECT DISTINCT user_id, user_id FROM orders ORDER BY 2 DESC" >>= runStatement testDB)
                `shouldBe` Right [[("user_id", VInt 2), ("user_id", VInt 2)], [("user_id", VInt 1), ("user_id", VInt 1)]]
            rowsOf (parseStatement "SELECT age, age FROM users ORDER BY age LIMIT 1" >>= runStatement testDB)
                `shouldBe` Right [[("age", VInt 17), ("age", VInt 17)]]
        it "preserves ordinal grouping, correlation and delayed projection errors" $ do
            rowsOf (parseStatement "SELECT user_id AS x, count(*) AS x FROM orders GROUP BY user_id ORDER BY 2 DESC" >>= runStatement testDB)
                `shouldBe` Right [[("x", VInt 1), ("x", VInt 2)], [("x", VInt 2), ("x", VInt 1)]]
            rowsOf (parseStatement "SELECT name AS x, 1 / (age - 25) AS x FROM users ORDER BY 1 DESC LIMIT 1" >>= runStatement testDB)
                `shouldBe` Right [[("x", VStr "Carol"), ("x", VInt 0)]]
            rowsOf (parseStatement "SELECT o.id, (SELECT i.age AS x FROM users i WHERE i.id = o.id ORDER BY 1 LIMIT 1) AS age FROM users o ORDER BY 1" >>= runStatement testDB)
                `shouldBe` Right [[("o.id", VInt 1), ("age", VInt 25)], [("o.id", VInt 2), ("age", VInt 17)], [("o.id", VInt 3), ("age", VInt 30)]]
            rowsOf (parseStatement "WITH t(a, b) AS (SELECT age AS x, -age AS x FROM users ORDER BY 2 LIMIT 1) SELECT * FROM t" >>= runStatement testDB)
                `shouldBe` Right [[("t.a", VInt 30), ("t.b", VInt (-30))]]
        it "resolves positions after wildcard expansion and before limiting" $ do
            rowsOf (parseStatement "SELECT * FROM orders ORDER BY 2 DESC, 1 DESC LIMIT 2" >>= runStatement testDB)
                `shouldBe` Right [tableRows orders !! 1, tableRows orders !! 2]
            rowsOf (parseStatement "SELECT DISTINCT user_id + 1 AS k FROM orders ORDER BY 1 DESC" >>= runStatement testDB)
                `shouldBe` Right [[("k", VInt 3)], [("k", VInt 2)]]
        it "rejects zero and positions beyond the output" $ do
            mapM_
                (\sql -> (parseStatement sql >>= runStatement testDB) `shouldSatisfy` either (isInfixOf "outside the select list") (const False))
                ["SELECT id FROM users ORDER BY 0", "SELECT id FROM users ORDER BY 2"]
        it "keeps NULL and equal-key row order when mixing ordinals and aliases" $ do
            let rows =
                    [ [("id", VInt n), ("name", VStr "same"), ("age", value)]
                    | (n, value) <- [(1, VInt 2), (2, VNull), (3, VInt 2)]
                    ]
                db = [("users", users{tableRows = rows})]
                sql = "SELECT id AS x, age AS x, name AS tag FROM users ORDER BY 2, tag"
            rowsOf (parseStatement sql >>= runStatement db)
                `shouldBe` Right
                    [ [("x", VInt 2), ("x", VNull), ("tag", VStr "same")]
                    , [("x", VInt 1), ("x", VInt 2), ("tag", VStr "same")]
                    , [("x", VInt 3), ("x", VInt 2), ("tag", VStr "same")]
                    ]
            fmap (map (drop 1 . map snd) . snd) (parseStatement "SELECT LIST<Int>(1) AS x, id AS x FROM users ORDER BY 1" >>= runStatement testDB)
                `shouldBe` Right [[VInt 1], [VInt 2], [VInt 3]]
            (parseStatement "SELECT age AS x, id AS x FROM users ORDER BY 1, x" >>= runStatement testDB)
                `shouldSatisfy` either (isInfixOf "ambiguous projection alias") (const False)
    describe "SELECT DISTINCT" $ do
        it "deduplicates projected values before ordering and limiting" $ do
            let db = [("orders", orders{tableRows = tableRows orders ++ tableRows orders})]
            rowsOf (parseStatement "SELECT DISTINCT user_id FROM orders ORDER BY user_id DESC LIMIT 2" >>= runStatement db)
                `shouldBe` Right [[("user_id", VInt 2)], [("user_id", VInt 1)]]
            rowsOf (parseStatement "SELECT DISTINCT user_id + 1 AS k FROM orders ORDER BY k LIMIT 2" >>= runStatement db)
                `shouldBe` Right [[("k", VInt 2)], [("k", VInt 3)]]
            rowsOf (parseStatement "SELECT ALL user_id FROM orders" >>= runStatement db)
                `shouldBe` Right (map (filter ((== "user_id") . fst)) (tableRows orders ++ tableRows orders))
        it "merges NULL and equal numeric values while retaining first rows" $ do
            evalRelOp [] (Distinct Unit) `shouldBe` Right [[]]
            let t = users{tableRows = [[("id", v), ("name", VStr "a"), ("age", VInt 1)] | v <- [VNull, VNull, VInt 1, VFloat 1, VInt 2]]}
            evalRelOp [("users", t)] (Distinct (Project ["id"] (Scan Nothing "users" Nothing)))
                `shouldBe` Right [[("id", VNull)], [("id", VInt 1)], [("id", VInt 2)]]
        it "keeps all distinct columns when an outer query projects fewer" $ do
            rowsOf (parseStatement "SELECT d.user_id FROM (SELECT DISTINCT user_id, product FROM orders) AS d" >>= runStatement testDB)
                `shouldBe` Right [[("d.user_id", VInt 1)], [("d.user_id", VInt 2)], [("d.user_id", VInt 1)]]
            rowsOf (parseStatement "WITH d(k) AS (SELECT DISTINCT user_id FROM orders) SELECT k FROM d" >>= runStatement testDB)
                `shouldBe` Right [[("d.k", VInt 1)], [("d.k", VInt 2)]]
        it "supports aggregates, empty inputs and output aliases" $ do
            rowsOf (parseStatement "SELECT DISTINCT COUNT(*) AS n FROM orders" >>= runStatement testDB)
                `shouldBe` Right [[("n", VInt 3)]]
            rowsOf (parseStatement "SELECT DISTINCT user_id FROM orders WHERE user_id = 99" >>= runStatement testDB)
                `shouldBe` Right []
            rowsOf (parseStatement "SELECT DISTINCT user_id AS k FROM orders ORDER BY user_id" >>= runStatement testDB)
                `shouldBe` Right [[("k", VInt 1)], [("k", VInt 2)]]
        it "accepts composite values with derived equality and hash capabilities" $ do
            let result = parseStatement "SELECT DISTINCT LIST<Int>(1) AS xs" >>= runStatement []
            fmap (length . snd) result `shouldBe` Right 1
        it "evaluates correlated DISTINCT subqueries with the outer binding" $ do
            rowsOf (parseStatement "SELECT u.id FROM users u WHERE u.id IN (SELECT DISTINCT o.user_id FROM orders o WHERE o.user_id = u.id)" >>= runStatement testDB)
                `shouldBe` Right [[("u.id", VInt 1)], [("u.id", VInt 2)]]
        it "rejects sorting by values excluded from the distinct output" $ do
            let result = parseStatement "SELECT DISTINCT user_id FROM orders ORDER BY product" >>= runStatement testDB
            result `shouldSatisfy` either (isInfixOf "must appear in the select list") (const False)

    describe "Storage read failures" $ do
        it "rejects storage that silently ignores a bound read guard" $ do
            setStorageLink
                StorageLink
                    { slRequest = \_ -> pure (Right (BSC.pack "{\"status\":\"rows\",\"rows\":[]}"))
                    , slClose = pure ()
                    }
            ( runIPCStorage (withSchemaGuard [] (scan "ignored"))
                    `shouldReturn` Left "storage did not confirm bound schema guard"
                )
                `finally` closeConnection
        it "rejects an ignored write handshake before sending the mutation" $ do
            requests <- newMVar []
            setStorageLink
                StorageLink
                    { slRequest = \payload -> do
                        modifyMVar_ requests (pure . (payload :))
                        pure (Right (BSC.pack "{\"status\":\"ok\"}"))
                    , slClose = pure ()
                    }
            flip finally closeConnection $ do
                runIPCStorage
                    ( withWriteSchemaGuard
                        [("ignored", users)]
                        (insert "ignored" [("id", VInt 1)])
                    )
                    `shouldReturn` Left "storage did not confirm bound schema guard"
                sent <- readMVar requests
                length sent `shouldBe` 1
                head sent `shouldSatisfy` BS.isInfixOf (BSC.pack "\"method\":\"validate_write\"")
        it "propagates catalog failures through snapshot, schema and SQL" $ do
            setStorageLink
                StorageLink
                    { slRequest = \_ -> pure (Right (BSC.pack "{\"status\":\"error\",\"message\":\"catalog unavailable\"}"))
                    , slClose = pure ()
                    }
            flip finally closeConnection $ do
                fmap (fmap (map fst)) (runIPCStorage snapshot) `shouldReturn` Left "catalog unavailable"
                fmap (fmap (map fst)) (runIPCStorage schema) `shouldReturn` Left "catalog unavailable"
                runIPCStorage (runStatementM (CreateTable "blocked" [("id", TInt)]))
                    `shouldReturn` Left "catalog unavailable"

        it "rejects the entire snapshot when a table scan fails" $ do
            setStorageLink
                StorageLink
                    { slRequest = \payload ->
                        pure $
                            Right $
                                BSC.pack $
                                    if BSC.pack "list_catalog" `BS.isInfixOf` payload
                                        then "{\"status\":\"catalog\",\"schemas\":[{\"table\":\"broken\",\"columns\":[],\"row_count\":1}]}"
                                        else "{\"status\":\"error\",\"message\":\"data page unreadable\"}"
                    , slClose = pure ()
                    }
            (fmap (fmap (map fst)) (runIPCStorage snapshot) `shouldReturn` Left "data page unreadable") `finally` closeConnection

    describe "Storage lifecycle and cancellation" $ do
        it "waits for concurrent requests before closing the storage link" $ do
            started <- newChan
            release <- newEmptyMVar
            finished <- newChan
            closing <- newEmptyMVar
            closed <- newEmptyMVar
            setStorageLink
                StorageLink
                    { slRequest = \_ -> writeChan started () >> readMVar release >> pure (Right BS.empty)
                    , slClose = putMVar closed ()
                    }
            flip finally (void (tryPutMVar release ()) >> closeConnection) $ do
                replicateM_ 2 (void (forkIO (sendRawRequest BS.empty >>= writeChan finished)))
                timeout 1000000 (replicateM_ 2 (readChan started)) `shouldReturn` Just ()
                void (forkIO (putMVar closing () >> closeConnection))
                takeMVar closing
                timeout 100000 (readMVar closed) `shouldReturn` Nothing
                putMVar release ()
                replicateM_ 2 (timeout 1000000 (readChan finished) `shouldReturn` Just (Right BS.empty))
                timeout 1000000 (readMVar closed) `shouldReturn` Just ()
                sendRawRequest BS.empty `shouldReturn` Left "storage not configured"
                closeConnection

        it "keeps the pool usable after cancellation while all workers are busy" $ do
            pool <- workerPool 4
            started <- newChan
            release <- newEmptyMVar
            finished <- newEmptyMVar
            void (forkIO (poolRun pool (replicate 4 (writeChan started () >> readMVar release)) >> putMVar finished ()))
            flip finally (void (tryPutMVar release ())) $ do
                timeout 1000000 (replicateM_ 4 (readChan started)) `shouldReturn` Just ()
                replicateM_ 2 (timeout 100000 (poolRun pool (replicate 3 (pure ()))) `shouldReturn` Nothing)
                putMVar release ()
                timeout 1000000 (takeMVar finished) `shouldReturn` Just ()
                timeout 1000000 (poolRun pool [pure ()]) `shouldReturn` Just [()]

        it "releases the storage link when an in-flight request is cancelled" $ do
            started <- newEmptyMVar
            release <- newEmptyMVar
            finished <- newEmptyMVar
            closed <- newEmptyMVar
            setStorageLink
                StorageLink
                    { slRequest = \_ -> putMVar started () >> takeMVar release >> pure (Right BS.empty)
                    , slClose = putMVar closed ()
                    }
            thread <- forkFinally (sendRawRequest BS.empty) (const (putMVar finished ()))
            flip finally (killThread thread >> closeConnection) $ do
                timeout 1000000 (takeMVar started) `shouldReturn` Just ()
                killThread thread
                timeout 1000000 (takeMVar finished) `shouldReturn` Just ()
                timeout 1000000 closeConnection `shouldReturn` Just ()
                readMVar closed `shouldReturn` ()

    RuntimeSpec.spec
    ScalarSpec.spec
    describe "persisted business scalars" $ do
        it "keeps canonical values, schema and indexed UUID equality after reopening" $ withTestSession $ \srv -> do
            withServerEnv srv $ do
                mapM_
                    (\source -> ipcDomainSql source `shouldReturn` Right [])
                    [ "CREATE TABLE scalar_values (uid UUID PRIMARY KEY, created TIMESTAMPTZ, payload JSONB, ip INET)"
                    , "INSERT INTO scalar_values (uid, created, payload, ip) VALUES ('550E8400-E29B-41D4-A716-446655440000', '2026-01-01T00:00:00+08:00', '{\"b\":2,\"a\":1}', '2001:DB8::1')"
                    , "CREATE INDEX ON scalar_values (uid)"
                    ]
                ipcDomainSql "UPDATE scalar_values SET created = '2026-02-29T00:00:00Z'" >>= (`shouldSatisfy` isLeft)
            closeConnection
            openSessionIn (dataDir srv)
            withServerEnv srv $
                ipcDomainSql "SELECT created, payload, ip FROM scalar_values WHERE uid = '550E8400-E29B-41D4-A716-446655440000'"
                    `shouldReturn` Right [[("created", VStr "2025-12-31T16:00:00.000000Z"), ("payload", VStr "{\"a\":1,\"b\":2}"), ("ip", VStr "2001:0db8:0000:0000:0000:0000:0000:0001")]]
    describe "identity syntax" $ do
        it "parses role and user attributes with their aliases" $ do
            parseStatement "ALTER ROLE worker LOGIN SUPERUSER ENABLED" `shouldBe` Right (AlterIdentity "worker" [("login", True), ("superuser", True), ("enabled", True)])
            parseStatement "ALTER USER worker NOLOGIN NOSUPERUSER DISABLED" `shouldBe` Right (AlterIdentity "worker" [("login", False), ("superuser", False), ("enabled", False)])
            parseStatement "ALTER ROLE worker IDENTIFIED BY 'Pass123!'" `shouldBe` Right (AlterUser "worker" "Pass123!")
            parseStatement "SHOW ROLES" `shouldBe` Right ShowRoles
        it "rejects empty unknown and repeated attributes" $ do
            parseStatement "ALTER ROLE worker" `shouldSatisfy` isLeft
            parseStatement "ALTER USER worker ACTIVE" `shouldSatisfy` isLeft
            prepare [] (AlterIdentity "worker" [("login", True), ("login", False)]) `shouldSatisfy` isLeft
    describe "ChuSQL.Core.Engine (ENUM)" $ do
        it "parses named enum declarations and rejects invalid labels" $ do
            parseStatement "CREATE TYPE role AS ENUM ('reader', 'AUTHOR', '管理员')"
                `shouldBe` Right (CreateDomain "role" (DomainDefinition (plainColumn (CEnum "role" ["reader", "AUTHOR", "管理员"])) []))
            parseStatement "DROP TYPE role" `shouldBe` Right (DropDomain "role")
            mapM_
                (\sql -> parseStatement sql `shouldSatisfy` isLeft)
                ["CREATE TYPE role AS ENUM ()", "CREATE TYPE role AS ENUM ('a', 'a')", "CREATE TYPE role AS ENUM (1)", "CREATE TYPE role AS ENUM ('" ++ replicate 64 'x' ++ "')"]
            parseStatement ("CREATE TYPE role AS ENUM ('" ++ replicate 63 'x' ++ "')") `shouldSatisfy` either (const False) (const True)
            validEnumLabels [replicate 21 '界'] `shouldBe` True
            validEnumLabels [replicate 22 '界'] `shouldBe` False
            validEnumLabels ["a\0b"] `shouldBe` False
        it "round trips nominal enum schemas and encoded values" $ do
            let labels = ["", "AUTHOR", "管理员", "a,b)", "quote'", "domain(other,str)"]
                ty = CDomain "role" (CEnum "role" labels)
                value = VEnum "role" labels "管理员"
            parseColumnType (typeName ty) `shouldBe` Just ty
            parseColumnType "enum([])" `shouldBe` Nothing
            parseColumnType "enum([\"role\",[\"a\",\"a\"]])" `shouldBe` Nothing
            parseEither Wire.valueFromJSON (Wire.valueToJSON value) `shouldBe` Right value
            parseEither Wire.valueFromJSON (Wire.valueToJSON (VEnum "role" labels "unknown")) `shouldSatisfy` isLeft
        it "enforces labels on inserts updates and defaults" $ do
            let labels = ["reader", "author", "admin"]
                setup = ["CREATE TYPE role AS ENUM ('reader', 'author', 'admin')", "CREATE TABLE members (id INT, role role DEFAULT 'reader')", "INSERT INTO members (id) VALUES (1)", "INSERT INTO members (id, role) VALUES (2, 'admin'), (3, NULL)"]
            case domainSql [] setup of
                Left err -> expectationFailure err
                Right (db, _) -> do
                    fmap snd (domainSql db ["SELECT role FROM members ORDER BY id"])
                        `shouldBe` Right [[("role", VEnum "role" labels "reader")], [("role", VEnum "role" labels "admin")], [("role", VNull)]]
                    mapM_
                        (\sql -> domainSql db [sql] `shouldSatisfy` isLeft)
                        ["INSERT INTO members (id, role) VALUES (4, 'Reader')", "INSERT INTO members (id, role) VALUES (4, 1)", "UPDATE members SET role = 'unknown' WHERE id = 1", "CREATE TABLE invalid (role role DEFAULT 'unknown')", "DROP TYPE role"]
                    fmap snd (domainSql db ["UPDATE members SET role = 'author' WHERE id = 1", "SELECT role FROM members WHERE id = 1"])
                        `shouldBe` Right [[("role", VEnum "role" labels "author")]]
                    domainSql db ["DROP TABLE members", "DROP TYPE role"] `shouldSatisfy` either (const False) (const True)
        it "validates existing text rows before conversion" $ do
            let setup = ["CREATE TYPE state AS ENUM ('a', 'b')", "CREATE TABLE items (id INT, value TEXT)", "INSERT INTO items (id, value) VALUES (1, 'a'), (2, 'unknown')"]
            case domainSql [] setup of
                Left err -> expectationFailure err
                Right (db, _) -> do
                    domainSql db ["ALTER TABLE items ALTER COLUMN value TYPE state"] `shouldSatisfy` isLeft
                    domainSql db ["CREATE TYPE state AS ENUM ('c')"] `shouldSatisfy` isLeft
                    fmap snd (domainSql db ["DELETE FROM items WHERE id = 2", "ALTER TABLE items ALTER COLUMN value TYPE state", "SELECT value FROM items"])
                        `shouldBe` Right [[("value", VEnum "state" ["a", "b"] "a")]]
        it "orders compares and aggregates by declaration order" $ do
            let labels = ["z", "a", "b"]
                setup = ["CREATE TYPE status AS ENUM ('z', 'a', 'b')", "CREATE TABLE items (id INT, state status UNIQUE)", "INSERT INTO items (id, state) VALUES (1, 'z'), (2, 'a'), (3, 'b'), (4, NULL), (5, NULL)"]
                value = VEnum "status" labels
            fmap snd (domainSql [] (setup ++ ["SELECT state FROM items WHERE state > 'z' ORDER BY state"]))
                `shouldBe` Right [[("state", value "a")], [("state", value "b")]]
            fmap snd (domainSql [] (setup ++ ["SELECT MIN(state), MAX(state) FROM items"]))
                `shouldBe` Right [[("MIN(state)", value "z"), ("MAX(state)", value "b")]]
            domainSql [] (setup ++ ["INSERT INTO items (id, state) VALUES (6, 'a')"]) `shouldSatisfy` isLeft
            fmap snd (domainSql [] (setup ++ ["SELECT state, COUNT(*) FROM items WHERE state = 'z' GROUP BY state"]))
                `shouldBe` Right [[("state", value "z"), ("COUNT(*)", VInt 1)]]
        it "rejects different enum identities and implicit text column comparisons" $ do
            let setup = ["CREATE TYPE first AS ENUM ('a', 'b')", "CREATE TYPE second AS ENUM ('a', 'b')", "CREATE TABLE items (a first, b second, text_value TEXT)"]
            mapM_
                (\query -> domainSql [] (setup ++ [query]) `shouldSatisfy` isLeft)
                ["SELECT * FROM items WHERE a = b", "SELECT * FROM items WHERE a = text_value", "UPDATE items SET a = b", "UPDATE items SET a = text_value", "SELECT * FROM items WHERE a = 'unknown'"]
        it "keeps enum identity through IN and quantified subqueries" $ do
            let setup = ["CREATE TYPE state AS ENUM ('z', 'a', 'b')", "CREATE TABLE items (id INT, value state)", "INSERT INTO items (id, value) VALUES (1, 'z'), (2, 'a'), (3, 'b')"]
                expected = Right [[("id", VInt 2)], [("id", VInt 3)]]
            mapM_
                (\query -> fmap snd (domainSql [] (setup ++ [query])) `shouldBe` expected)
                ["SELECT id FROM items WHERE value IN ('a', 'b') ORDER BY id", "SELECT id FROM items WHERE value IN (SELECT value FROM items WHERE id > 1) ORDER BY id", "SELECT id FROM items WHERE value > ANY (SELECT value FROM items WHERE id = 1) ORDER BY id"]
            fmap snd (domainSql [] (setup ++ ["SELECT id FROM items WHERE 'a' IN (SELECT value FROM items) ORDER BY id LIMIT 1"])) `shouldBe` Right [[("id", VInt 1)]]
        it "preserves native boolean expressions" $ do
            fmap snd (domainSql [] ["CREATE TABLE flags (active BOOL)", "INSERT INTO flags (active) VALUES (TRUE), (FALSE)", "SELECT not(active) AS inactive FROM flags WHERE active"])
                `shouldBe` Right [[("inactive", VBool False)]]

    describe "ChuSQL.Core.Engine (DOMAIN)" $ do
        it "parses domain commands and named columns" $ do
            parseStatement "CREATE DOMAIN amount AS DECIMAL(12,2)" `shouldBe` Right (CreateDomain "amount" (DomainDefinition (plainColumn (CDecimal 12 2)) []))
            parseStatement "DROP DOMAIN amount" `shouldBe` Right (DropDomain "amount")
            parseStatement "SHOW DOMAINS" `shouldBe` Right ShowDomains
            parseStatement "CREATE TABLE items (price amount)" `shouldBe` Right (CreateTable "items" [("price", plainColumn (CDomain "amount" CStr))])
        it "inherits defaults and non-null across nested domains without freezing column declarations" $ do
            let setup =
                    [ "CREATE DOMAIN positive AS INT DEFAULT 3 NOT NULL CHECK (VALUE > 0)"
                    , "CREATE DOMAIN small AS positive DEFAULT 4 CHECK (VALUE < 10)"
                    , "CREATE TABLE items (id INT, qty small, own small DEFAULT 6)"
                    , "INSERT INTO items (id) VALUES (1)"
                    , "ALTER DOMAIN small SET DEFAULT 5"
                    , "INSERT INTO items (id) VALUES (2)"
                    , "ALTER DOMAIN small DROP DEFAULT"
                    , "INSERT INTO items (id) VALUES (3)"
                    ]
            fmap snd (domainSql [] (setup ++ ["SELECT qty, own FROM items ORDER BY id"]))
                `shouldBe` Right [[("qty", VInt 4), ("own", VInt 6)], [("qty", VInt 5), ("own", VInt 6)], [("qty", VInt 3), ("own", VInt 6)]]
            case domainSql [] setup of
                Left err -> expectationFailure err
                Right (db, _) ->
                    mapM_
                        (\sql -> domainSql db [sql] `shouldSatisfy` isLeft)
                        [ "INSERT INTO items (qty) VALUES (NULL)"
                        , "INSERT INTO items (qty) VALUES (0)"
                        , "INSERT INTO items (qty) VALUES (10)"
                        , "UPDATE items SET qty = 0"
                        ]
        it "accepts UNKNOWN in domain checks and checks explicit null separately from defaults" $ do
            fmap
                snd
                ( domainSql
                    []
                    [ "CREATE DOMAIN positive AS INT DEFAULT 2 CHECK (VALUE > 0)"
                    , "CREATE TABLE items (qty positive)"
                    , "INSERT INTO items (qty) VALUES (NULL)"
                    , "ALTER DOMAIN positive SET NOT NULL"
                    ]
                )
                `shouldSatisfy` isLeft
            fmap
                snd
                ( domainSql
                    []
                    [ "CREATE DOMAIN positive AS INT DEFAULT 2 CHECK (VALUE > 0)"
                    , "CREATE TABLE items (qty positive)"
                    , "INSERT INTO items (qty) VALUES (NULL)"
                    , "SELECT qty FROM items"
                    ]
                )
                `shouldBe` Right [[("qty", VNull)]]
            fmap
                snd
                ( domainSql
                    []
                    [ "CREATE DOMAIN positive AS INT NOT NULL"
                    , "ALTER DOMAIN positive DROP NOT NULL"
                    , "CREATE TABLE items (qty positive)"
                    , "INSERT INTO items (qty) VALUES (NULL)"
                    , "SELECT qty FROM items"
                    ]
                )
                `shouldBe` Right [[("qty", VNull)]]
        it "validates definitions even for unused domains and empty tables" $ do
            mapM_
                (\statements -> domainSql [] statements `shouldSatisfy` isLeft)
                [ ["CREATE DOMAIN bad AS INT DEFAULT 0 CHECK (VALUE > 0)"]
                , ["CREATE DOMAIN bad AS INT DEFAULT NULL NOT NULL"]
                , ["CREATE DOMAIN bad AS INT CHECK (missing > 0)"]
                , ["CREATE DOMAIN bad AS INT CHECK (VALUE + 1)"]
                , ["CREATE DOMAIN bad AS INT CHECK (COUNT(*) > 0)"]
                , ["CREATE DOMAIN bad AS INT CHECK (VALUE > (SELECT 1))"]
                , ["CREATE DOMAIN bad AS INT CHECK (unknown_fn(VALUE))"]
                , ["CREATE DOMAIN bad AS INT CONSTRAINT same CHECK (VALUE > 0) CONSTRAINT SAME CHECK (VALUE < 9)"]
                , ["CREATE DOMAIN positive AS INT CHECK (VALUE > 0)", "CREATE TABLE items (qty positive DEFAULT 0)"]
                , ["CREATE DOMAIN positive AS INT CHECK (VALUE > 0)", "CREATE DOMAIN child AS positive DEFAULT 0"]
                ]
        it "fills added columns from inherited defaults and validates retyped values" $ do
            let setup =
                    [ "CREATE DOMAIN positive AS INT DEFAULT 7 NOT NULL CHECK (VALUE > 0)"
                    , "CREATE TABLE items (id INT)"
                    , "INSERT INTO items (id) VALUES (1)"
                    , "ALTER TABLE items ADD COLUMN qty positive"
                    ]
            fmap snd (domainSql [] (setup ++ ["SELECT qty FROM items"])) `shouldBe` Right [[("qty", VInt 7)]]
            domainSql
                []
                [ "CREATE DOMAIN positive AS INT CHECK (VALUE > 0)"
                , "CREATE TABLE items (id INT)"
                , "INSERT INTO items (id) VALUES (0)"
                , "ALTER TABLE items ALTER COLUMN id TYPE positive"
                ]
                `shouldSatisfy` isLeft
        it "checks existing rows, dependent defaults and all nested constraints before publication" $ do
            let setup =
                    [ "CREATE DOMAIN base AS INT"
                    , "CREATE DOMAIN child AS base DEFAULT 8"
                    , "CREATE TABLE items (id INT, qty child)"
                    , "INSERT INTO items (id, qty) VALUES (1, 4)"
                    ]
            case domainSql [] setup of
                Left err -> expectationFailure err
                Right (db, _) -> do
                    mapM_
                        (\sql -> domainSql db [sql] `shouldSatisfy` isLeft)
                        [ "ALTER DOMAIN base ADD CONSTRAINT large CHECK (VALUE > 5)"
                        , "ALTER DOMAIN base ADD CHECK (VALUE < 7)"
                        ]
                    fmap
                        snd
                        ( domainSql
                            db
                            [ "ALTER DOMAIN base ADD CONSTRAINT positive CHECK (VALUE > 0)"
                            , "ALTER DOMAIN child SET DEFAULT 0"
                            ]
                        )
                        `shouldSatisfy` isLeft
                    fmap snd (domainSql db ["SELECT qty FROM items"]) `shouldBe` Right [[("qty", VInt 4)]]
        it "preserves named constraints, drops one at a time and invalidates bound plans" $ do
            case domainSql
                []
                [ "CREATE DOMAIN qty AS INT CONSTRAINT positive CHECK (VALUE > 0) CHECK (VALUE < 10)"
                , "CREATE TABLE items (qty qty)"
                , "INSERT INTO items (qty) VALUES (4)"
                ] of
                Left err -> expectationFailure err
                Right (db, _) -> do
                    Right statement <- pure (parseStatement "INSERT INTO items (qty) VALUES (5)")
                    Right bound <- pure (bindWrite db statement)
                    Right query <- pure (parseStatement "SELECT qty FROM items")
                    Right boundQuery <- pure (Bound.bindQuery db query)
                    Right (changed, _) <- pure (domainSql db ["ALTER DOMAIN qty DROP CONSTRAINT positive"])
                    runMemoryStorage (executeWriteM bound) changed `shouldSatisfy` either (const False) (isLeft . fst)
                    runMemoryStorage (Bound.executeQueryM boundQuery) changed `shouldSatisfy` either (const False) (isLeft . fst)
                    domainSql changed ["INSERT INTO items (qty) VALUES (0)"] `shouldSatisfy` either (const False) (const True)
                    domainSql changed ["INSERT INTO items (qty) VALUES (10)"] `shouldSatisfy` isLeft
                    domainSql changed ["ALTER DOMAIN qty DROP CONSTRAINT missing"] `shouldSatisfy` isLeft
                    fmap
                        snd
                        ( domainSql
                            changed
                            [ "ALTER DOMAIN qty DROP CONSTRAINT domain_check_1"
                            , "INSERT INTO items (qty) VALUES (10)"
                            , "SELECT qty FROM items"
                            ]
                        )
                        `shouldBe` Right [[("qty", VInt 4)], [("qty", VInt 10)]]
        it "parses signed numeric defaults without changing their signs" $ do
            fmap
                snd
                ( domainSql
                    []
                    [ "CREATE DOMAIN plus AS DOUBLE DEFAULT 1.25"
                    , "CREATE DOMAIN minus AS DOUBLE DEFAULT -1.25"
                    , "CREATE TABLE items (id INT, a plus, b minus)"
                    , "INSERT INTO items (id) VALUES (-2)"
                    , "SELECT id, a, b FROM items"
                    ]
                )
                `shouldBe` Right [[("id", VInt (-2)), ("a", VFloat 1.25), ("b", VFloat (-1.25))]]
        it "keeps a named type while applying its base semantics" $ do
            let result = domainSql [] ["CREATE DOMAIN label AS VARCHAR(3)", "CREATE TABLE items (name label)", "INSERT INTO items (name) VALUES ('abc')"]
            case result of
                Left err -> expectationFailure err
                Right (db, _) -> do
                    fmap columnType (lookup "items" db >>= lookup "name" . tableCols) `shouldBe` Just (CDomain "label" (CVarchar 3))
                    fmap snd (domainSql db ["SELECT name FROM items"]) `shouldBe` Right [[("name", VStr "abc")]]
                    domainSql db ["INSERT INTO items (name) VALUES ('long')"] `shouldSatisfy` isLeft
        it "rejects unknown, duplicate and built-in domains" $ do
            mapM_
                (\sql -> domainSql [] sql `shouldSatisfy` isLeft)
                [ ["CREATE TABLE items (name missing)"]
                , ["CREATE DOMAIN INT AS INT"]
                , ["CREATE DOMAIN a AS INT", "CREATE DOMAIN A AS INT"]
                , ["CREATE DOMAIN a AS missing"]
                , ["CREATE DOMAIN bad AS VARCHAR(0)"]
                ]
        it "resolves nested domains and preserves their names and base checks" $ do
            let result = domainSql [] ["CREATE DOMAIN label AS VARCHAR(3)", "CREATE DOMAIN product_label AS label", "CREATE DOMAIN title AS product_label", "CREATE TABLE items (name title)", "INSERT INTO items (name) VALUES ('abc')"]
            case result of
                Left err -> expectationFailure err
                Right (db, _) -> do
                    fmap columnType (lookup "items" db >>= lookup "name" . tableCols) `shouldBe` Just (CDomain "title" (CDomain "product_label" (CDomain "label" (CVarchar 3))))
                    fmap snd (domainSql db ["SELECT name FROM items"]) `shouldBe` Right [[("name", VStr "abc")]]
                    domainSql db ["INSERT INTO items (name) VALUES ('long')"] `shouldSatisfy` isLeft
                    domainSql db ["DROP DOMAIN label"] `shouldSatisfy` isLeft
        it "protects domain dependencies even without user columns" $ do
            domainSql [] ["CREATE DOMAIN a AS INT", "CREATE DOMAIN b AS a", "DROP DOMAIN a"] `shouldSatisfy` isLeft
            fmap snd (domainSql [] ["CREATE DOMAIN a AS INT", "CREATE DOMAIN b AS a", "DROP DOMAIN b", "DROP DOMAIN a", "SHOW DOMAINS"]) `shouldBe` Right []
            domainSql [] ["CREATE DOMAIN self AS self"] `shouldSatisfy` isLeft
        it "uses nested domains in column additions and conversions" $ do
            fmap snd (domainSql [] ["CREATE DOMAIN a AS INT", "CREATE DOMAIN b AS a", "CREATE TABLE items (id INT)", "INSERT INTO items (id) VALUES (2)", "ALTER TABLE items ADD COLUMN qty b", "ALTER TABLE items ALTER COLUMN id TYPE b", "SELECT id + 1 AS next, qty FROM items"]) `shouldBe` Right [[("next", VInt 3), ("qty", VNull)]]
        it "rejects cyclic catalog definitions without recursing forever" $ do
            case domainSql [] ["CREATE DOMAIN a AS INT", "CREATE DOMAIN b AS a"] of
                Left err -> expectationFailure err
                Right (db, _) -> do
                    let cyclic = map (\(name, table) -> if name == domainTableName "a" then (name, table{tableCols = [("base", plainColumn (CDomain "b" CStr))]}) else (name, table)) db
                    fmap snd (domainSql cyclic ["CREATE TABLE items (id b)"]) `shouldBe` Left "cyclic domain definition: b"
        it "prevents dropping a domain until its columns are removed" $ do
            domainSql [] ["CREATE DOMAIN code AS INT", "CREATE TABLE items (code code)", "DROP DOMAIN code"] `shouldSatisfy` isLeft
            fmap snd (domainSql [] ["CREATE DOMAIN code AS INT", "CREATE TABLE items (code code)", "DROP TABLE items", "DROP DOMAIN code", "SHOW DOMAINS"]) `shouldBe` Right []
            domainSql [] ["DROP DOMAIN missing"] `shouldSatisfy` isLeft
        it "resolves domains in ADD COLUMN and ALTER COLUMN TYPE" $ do
            fmap snd (domainSql [] ["CREATE DOMAIN code AS INT", "CREATE TABLE items (id INT)", "INSERT INTO items (id) VALUES (1)", "ALTER TABLE items ADD COLUMN code code", "ALTER TABLE items ALTER COLUMN id TYPE code", "SELECT id, code FROM items"]) `shouldBe` Right [[("id", VInt 1), ("code", VNull)]]
        it "hides and protects the domain catalog from SQL" $ do
            domainSql [] ["CREATE DOMAIN code AS INT", "SELECT * FROM __system_domain_code"] `shouldSatisfy` isLeft
            domainSql [] ["CREATE DOMAIN code AS INT", "DROP TABLE __system_domain_code"] `shouldSatisfy` isLeft
            domainSql [] ["CREATE TABLE __system_domain_fake (base INT)"] `shouldSatisfy` isLeft
        it "round trips named types and compares using the base type" $ do
            let ty = CDomain "amount" (CDecimal 12 2)
            parseColumnType (typeName ty) `shouldBe` Just ty
            comparableTypes ty CInt `shouldBe` True
            integerType (CDomain "code" CInt) `shouldBe` True
            coerceValue (CDomain "code" CInt) (VStr "bad") `shouldSatisfy` isLeft
        it "uses numeric domain columns in expressions and sorting" $ do
            fmap snd (domainSql [] ["CREATE DOMAIN quantity AS INT", "CREATE TABLE items (qty quantity)", "INSERT INTO items (qty) VALUES (2), (1)", "SELECT qty + 1 AS next FROM items ORDER BY qty"]) `shouldBe` Right [[("next", VInt 2)], [("next", VInt 3)]]
        it "uses boolean domains as conditions and logical operands" $ do
            fmap snd (domainSql [] ["CREATE DOMAIN flag AS BOOL", "CREATE TABLE items (active flag)", "INSERT INTO items (active) VALUES (TRUE), (FALSE), (NULL)", "SELECT active FROM items WHERE active AND TRUE"]) `shouldBe` Right [[("active", VBool True)]]
    describe "ChuSQL.Core.Engine.Syntax.Parser" $ do
        it "resolves table-qualified columns inside database-qualified tables" $ do
            let db = [("sales.users", users)]
            rowsOf (parseStatement "SELECT users.name FROM sales.users WHERE users.id = 1" >>= runStatement db)
                `shouldBe` Right [[("name", VStr "Alice")]]
            rowsOf (parseStatement "SELECT sales.users.name FROM sales.users WHERE sales.users.id = 1" >>= runStatement db)
                `shouldBe` Right [[("name", VStr "Alice")]]
            let related = [("sales.users", users), ("sales.orders", orders)]
            rowsOf (parseStatement "SELECT sales.users.name FROM sales.users WHERE EXISTS (SELECT orders.id FROM sales.orders WHERE orders.user_id = sales.users.id)" >>= runStatement related)
                `shouldBe` Right [[("name", VStr "Alice")], [("name", VStr "Bob")]]
            rowsOf (parseStatement "SELECT sales.users.name FROM sales.users WHERE EXISTS (SELECT orders.id FROM sales.orders WHERE orders.user_id = intruder.users.id)" >>= runStatement related)
                `shouldSatisfy` either (isInfixOf "unknown column") (const False)

        it "derives the column prefix when the session schema is scoped to one database" $ do
            let scoped = [("users", users)]
            rowsOf (parseStatement "SELECT sales.users.name FROM sales.users WHERE sales.users.id = 1" >>= runStatement scoped)
                `shouldBe` Right [[("name", VStr "Alice")]]
            rowsOf (parseStatement "SELECT users.name FROM users WHERE users.id = 1" >>= runStatement scoped)
                `shouldBe` Right [[("name", VStr "Alice")]]

        it "derives the database prefix for a bare table name" $ do
            let catalog = [("sales.users", users)]
            rowsOf (parseStatement "SELECT name FROM users WHERE users.id = 1" >>= runStatement catalog)
                `shouldBe` Right [[("name", VStr "Alice")]]
            rowsOf (parseStatement "SELECT users.name FROM sales.users WHERE users.id = 1" >>= runStatement catalog)
                `shouldBe` Right [[("name", VStr "Alice")]]

        it "keeps derived prefixes short in joins" $ do
            let catalog = [("sales.users", users), ("sales.orders", orders)]
            fmap
                (map (map snd))
                (parseStatement "SELECT users.name, orders.product FROM sales.users JOIN sales.orders ON users.id = orders.user_id" >>= rowsOf . runStatement catalog)
                `shouldBe` Right [[VStr "Alice", VStr "Book"], [VStr "Alice", VStr "Cup"], [VStr "Bob", VStr "Pen"]]

        it "writes through the derived table key" $ do
            let scoped = [("users", users)]
            case parseStatement "UPDATE sales.users SET age = 26 WHERE id = 1" >>= runStatement scoped of
                Left err -> expectationFailure err
                Right (db', _) ->
                    rowsOf (runStatement db' (makeSelect ["age"] "users" (Just (Eq (Col "id") (LitInt 1)))))
                        `shouldBe` Right [[("age", VInt 26)]]

        it "rejects an ambiguous derived table name" $ do
            let catalog = [("sales.users", users), ("other.users", users)]
            (parseStatement "SELECT name FROM users" >>= rowsOf . runStatement catalog)
                `shouldSatisfy` either (isInfixOf "ambiguous table") (const False)

        it "parses database lifecycle statements" $ do
            map (fmap show . parseStatement) ["CREATE DATABASE sales", "DROP DATABASE sales", "USE sales", "SHOW DATABASES"]
                `shouldBe` map Right ["CreateDatabase \"sales\"", "DropDatabase \"sales\"", "UseDatabase \"sales\"", "ShowDatabases"]

        it "parses qualified tables for reads and writes" $ do
            parseStatement "SELECT * FROM sales.items" `shouldBe` Right (makeSelect ["*"] "sales.items" Nothing)
            parseStatement "INSERT INTO sales.items (id) VALUES (1)" `shouldBe` Right (Insert "sales.items" ["id"] [[LitInt 1]])
            parseStatement "CREATE TABLE sales.items (id int)" `shouldBe` Right (CreateTable "sales.items" [("id", TInt)])
            parseStatement "DROP TABLE sales.items" `shouldBe` Right (DropTable "sales.items")

        it "parses multiple columns" $ do
            parseStatement "SELECT name, age FROM users"
                `shouldBe` Right (makeSelect ["name", "age"] "users" Nothing)

        it "parses WHERE with >" $ do
            parseStatement "SELECT name FROM users WHERE age > 18"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Gt (Col "age") (LitInt 18)))
                    )

        it "parses AND" $ do
            parseStatement "SELECT name FROM users WHERE age > 18 AND age < 30"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        ( Just
                            ( And
                                (Gt (Col "age") (LitInt 18))
                                (Lt (Col "age") (LitInt 30))
                            )
                        )
                    )

        it "parses OR" $ do
            parseStatement "SELECT name FROM users WHERE age < 18 OR age > 60"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        ( Just
                            ( Or
                                (Lt (Col "age") (LitInt 18))
                                (Gt (Col "age") (LitInt 60))
                            )
                        )
                    )

        it "is case-insensitive for keywords" $ do
            parseStatement "select name from users where age > 18"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Gt (Col "age") (LitInt 18)))
                    )

        it "tolerates extra whitespace" $ do
            parseStatement "SELECT   name   FROM   users"
                `shouldBe` Right (makeSelect ["name"] "users" Nothing)

        it "uses standard SQL escaping: '' is one quote" $ do
            parseStatement "SELECT name FROM users WHERE name = 'It''s ok'"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Eq (Col "name") (LitStr "It's ok")))
                    )

        it "treats a backslash as an ordinary character" $ do
            parseStatement "SELECT name FROM users WHERE name = 'a\\b'"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Eq (Col "name") (LitStr "a\\b")))
                    )

        it "parses an empty string literal" $ do
            parseStatement "SELECT name FROM users WHERE name = ''"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Eq (Col "name") (LitStr "")))
                    )

        it "parses a string that is a single quote" $ do
            parseStatement "SELECT name FROM users WHERE name = ''''"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Eq (Col "name") (LitStr "'")))
                    )

        it "rejects an unterminated string literal" $ do
            parseStatement "SELECT name FROM users WHERE name = 'oops"
                `shouldSatisfy` isLeft

        it "honours parentheses and AND/OR precedence" $ do
            parseStatement "SELECT name FROM users WHERE (age > 18 OR age < 5) AND name = 'Bob'"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        ( Just
                            ( And
                                ( Or
                                    (Gt (Col "age") (LitInt 18))
                                    (Lt (Col "age") (LitInt 5))
                                )
                                (Eq (Col "name") (LitStr "Bob"))
                            )
                        )
                    )

        it "rejects trailing junk after a complete query" $ do
            parseStatement "SELECT name FROM users extra junk"
                `shouldSatisfy` isLeft

        it "returns Left on missing table name" $ do
            parseStatement "SELECT name FROM"
                `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (事务)" $ do
        it "parses transaction control statements" $ do
            parseStatement "BEGIN" `shouldBe` Right BeginTransaction
            parseStatement "BEGIN ISOLATION LEVEL SERIALIZABLE" `shouldBe` Right BeginSerializableTransaction
            parseStatement "START TRANSACTION ISOLATION LEVEL SERIALIZABLE" `shouldBe` Right BeginSerializableTransaction
            parseStatement "BEGIN TRANSACTION ISOLATION LEVEL SERIALIZABLE" `shouldBe` Right BeginSerializableTransaction
            parseStatement "BEGIN ISOLATION LEVEL READ COMMITTED" `shouldSatisfy` isLeft
            parseStatement "BEGIN TRANSACTION" `shouldBe` Right BeginTransaction
            parseStatement "START TRANSACTION" `shouldBe` Right BeginTransaction
            parseStatement "begin transaction" `shouldBe` Right BeginTransaction
            parseStatement "COMMIT" `shouldBe` Right CommitTransaction
            parseStatement "COMMIT TRANSACTION" `shouldBe` Right CommitTransaction
            parseStatement "ROLLBACK" `shouldBe` Right RollbackTransaction
            parseStatement "rollback transaction" `shouldBe` Right RollbackTransaction
            parseStatement "SAVEPOINT spot" `shouldBe` Right (Savepoint "spot")
            parseStatement "savepoint spot" `shouldBe` Right (Savepoint "spot")
            parseStatement "ROLLBACK TO spot" `shouldBe` Right (RollbackToSavepoint "spot")
            parseStatement "ROLLBACK TO SAVEPOINT spot" `shouldBe` Right (RollbackToSavepoint "spot")
            parseStatement "RELEASE spot" `shouldBe` Right (ReleaseSavepoint "spot")
            parseStatement "RELEASE SAVEPOINT spot" `shouldBe` Right (ReleaseSavepoint "spot")

        it "requires a name on savepoint statements" $ do
            parseStatement "SAVEPOINT" `shouldSatisfy` isLeft
            parseStatement "ROLLBACK TO" `shouldSatisfy` isLeft
            parseStatement "RELEASE" `shouldSatisfy` isLeft

        it "parses the bare words only as whole words" $ do
            parseStatement "BEGINNER" `shouldSatisfy` isLeft
            parseStatement "COMMITTED" `shouldSatisfy` isLeft

        it "reports transaction control as session work" $ do
            mapM_
                ( \sql ->
                    (runStatement [("users", users)] =<< parseStatement sql)
                        `shouldSatisfy` either (isInfixOf "executed by the session") (const False)
                )
                ["BEGIN", "COMMIT", "ROLLBACK", "SAVEPOINT spot", "ROLLBACK TO spot", "RELEASE spot"]

        it "puts transaction errors under the protocol category" $ do
            errorCode "BEGIN is executed by the session" `shouldBe` "query_error"
            errorCategory "BEGIN is executed by the session" `shouldBe` ProtocolError

    describe "ChuSQL.Core.Engine.Syntax.Parser (JOIN)" $ do
        it "parses a simple JOIN without aliases" $ do
            parseStatement "SELECT name FROM users JOIN orders ON id = user_id"
                `shouldBe` Right
                    ( Select
                        { selectItems = [("name", Col "name")]
                        , selectFrom =
                            FromJoin
                                InnerJoin
                                (FromTable Nothing "users")
                                (FromTable Nothing "orders")
                                (Eq (Col "id") (Col "user_id"))
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectHaving = Nothing
                        , selectOrderBy = []
                        , selectLimit = Nothing
                        }
                    )

        it "parses JOIN with aliases" $ do
            parseStatement "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id"
                `shouldBe` Right
                    ( Select
                        { selectItems = [("u.name", Col "u.name")]
                        , selectFrom =
                            FromJoin
                                InnerJoin
                                (FromTable (Just "u") "users")
                                (FromTable (Just "o") "orders")
                                (Eq (Col "u.id") (Col "o.user_id"))
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectHaving = Nothing
                        , selectOrderBy = []
                        , selectLimit = Nothing
                        }
                    )

        it "parses LEFT JOIN" $ do
            parseStatement "SELECT u.name FROM users u LEFT JOIN orders o ON u.id = o.user_id"
                `shouldBe` Right
                    ( Select
                        { selectItems = [("u.name", Col "u.name")]
                        , selectFrom =
                            FromJoin
                                LeftJoin
                                (FromTable (Just "u") "users")
                                (FromTable (Just "o") "orders")
                                (Eq (Col "u.id") (Col "o.user_id"))
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectHaving = Nothing
                        , selectOrderBy = []
                        , selectLimit = Nothing
                        }
                    )

        it "parses LEFT OUTER JOIN the same as LEFT JOIN" $ do
            fmap selectFrom (parseStatement "SELECT u.name FROM users u LEFT OUTER JOIN orders o ON u.id = o.user_id")
                `shouldBe` Right
                    ( FromJoin
                        LeftJoin
                        (FromTable (Just "u") "users")
                        (FromTable (Just "o") "orders")
                        (Eq (Col "u.id") (Col "o.user_id"))
                    )

    describe "ChuSQL.Core.Engine (派生表)" $ do
        -- sql：在默认库上跑 SQL 取结果行
        let sql input = parseStatement input >>= rowsOf . runStatement testDB
            -- values：只取结果里的值
            values input = fmap (map (map snd)) (sql input)
        it "parses a derived table with an alias" $ do
            parseStatement "SELECT d.name FROM (SELECT name FROM users) d"
                `shouldBe` Right
                    ( Select
                        { selectItems = [("d.name", Col "d.name")]
                        , selectFrom =
                            FromSubquery
                                (Just "d")
                                (Select [("name", Col "name")] (FromTable Nothing "users") Nothing [] Nothing [] Nothing)
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectHaving = Nothing
                        , selectOrderBy = []
                        , selectLimit = Nothing
                        }
                    )

        it "requires an alias for a derived table" $ do
            parseStatement "SELECT name FROM (SELECT name FROM users)" `shouldSatisfy` isLeft

        it "parses a derived table on the right of a JOIN" $ do
            fmap selectFrom (parseStatement "SELECT d.name FROM users u JOIN (SELECT name FROM users) d ON u.name = d.name")
                `shouldBe` Right
                    ( FromJoin
                        InnerJoin
                        (FromTable (Just "u") "users")
                        (FromSubquery (Just "d") (Select [("name", Col "name")] (FromTable Nothing "users") Nothing [] Nothing [] Nothing))
                        (Eq (Col "u.name") (Col "d.name"))
                    )

        it "parses a projection alias" $ do
            parseStatement "SELECT count(*) AS total FROM orders"
                `shouldBe` Right
                    ( Select
                        [("total", CountAll)]
                        (FromTable Nothing "orders")
                        Nothing
                        []
                        Nothing
                        []
                        Nothing
                    )

        it "runs a derived table as a source" $ do
            values "SELECT d.name FROM (SELECT name FROM users) d"
                `shouldBe` Right [[VStr "Alice"], [VStr "Bob"], [VStr "Carol"]]

        it "expands the star of a derived table" $ do
            sql "SELECT * FROM (SELECT id, name FROM users) d"
                `shouldBe` Right
                    [ [("d.id", VInt 1), ("d.name", VStr "Alice")]
                    , [("d.id", VInt 2), ("d.name", VStr "Bob")]
                    , [("d.id", VInt 3), ("d.name", VStr "Carol")]
                    ]

        it "joins a derived table with a table" $ do
            values "SELECT o.id FROM (SELECT id FROM users WHERE age > 20) d JOIN orders o ON o.user_id = d.id ORDER BY o.id"
                `shouldBe` Right [[VInt 1], [VInt 3]]

        it "runs an aggregate inside a derived table" $ do
            values "SELECT d.total FROM (SELECT user_id, count(*) AS total FROM orders GROUP BY user_id) d ORDER BY d.total"
                `shouldBe` Right [[VInt 1], [VInt 2]]

        it "runs a derived table inside a derived table" $ do
            values "SELECT inner_d.name FROM (SELECT d.name FROM (SELECT name FROM users) d WHERE d.name = 'Bob') inner_d"
                `shouldBe` Right [[VStr "Bob"]]

        it "keeps a projection alias outside a derived table" $ do
            sql "SELECT count(*) AS total FROM orders" `shouldBe` Right [[("total", VInt 3)]]

        it "rejects duplicate column names in a derived table" $ do
            sql "SELECT d.name FROM (SELECT name, name FROM users) d" `shouldSatisfy` isLeft

        it "does not let a derived table see outer columns" $ do
            sql "SELECT d.name FROM users u JOIN (SELECT name FROM users WHERE id = u.id) d ON u.name = d.name"
                `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (WITH)" $ do
        -- sql：在默认库上跑 SQL 取结果行
        let sql input = parseStatement input >>= rowsOf . runStatement testDB
            -- values：只取结果里的值
            values input = fmap (map (map snd)) (sql input)
        it "parses a CTE into a derived table" $ do
            parseStatement "WITH t AS (SELECT name FROM users) SELECT t.name FROM t"
                `shouldBe` Right
                    ( Select
                        { selectItems = [("t.name", Col "t.name")]
                        , selectFrom =
                            FromSubquery
                                (Just "t")
                                (Select [("name", Col "name")] (FromTable Nothing "users") Nothing [] Nothing [] Nothing)
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectHaving = Nothing
                        , selectOrderBy = []
                        , selectLimit = Nothing
                        }
                    )

        it "runs a CTE as a source" $ do
            values "WITH t AS (SELECT name FROM users) SELECT t.name FROM t"
                `shouldBe` Right [[VStr "Alice"], [VStr "Bob"], [VStr "Carol"]]

        it "refers to an earlier CTE inside the next one" $ do
            values "WITH young AS (SELECT name FROM users WHERE age < 20), picked AS (SELECT y.name FROM young y) SELECT p.name FROM picked p"
                `shouldBe` Right [[VStr "Bob"]]

        it "renames the output columns of a CTE" $ do
            values "WITH t(who) AS (SELECT name FROM users) SELECT t.who FROM t ORDER BY t.who"
                `shouldBe` Right [[VStr "Alice"], [VStr "Bob"], [VStr "Carol"]]

        it "keeps source ordering when renaming repeated CTE columns" $ do
            values "WITH t(first, second) AS (SELECT name, name FROM users ORDER BY name DESC LIMIT 1) SELECT first, second FROM t"
                `shouldBe` Right [[VStr "Carol", VStr "Carol"]]

        it "renames expression ordering aliases with CTE columns" $ do
            values "WITH t(next_age) AS (SELECT age + 1 AS years FROM users ORDER BY years DESC LIMIT 1) SELECT next_age FROM t"
                `shouldBe` Right [[VInt 31]]

        it "takes a bare column from a CTE" $ do
            values "WITH t(who) AS (SELECT name FROM users) SELECT who FROM t ORDER BY who"
                `shouldBe` Right [[VStr "Alice"], [VStr "Bob"], [VStr "Carol"]]

        it "runs a CTE inside an IN subquery" $ do
            values "WITH grown AS (SELECT id FROM users WHERE age > 20) SELECT name FROM users WHERE id IN (SELECT id FROM grown) ORDER BY name"
                `shouldBe` Right [[VStr "Alice"], [VStr "Carol"]]

        it "joins a CTE with itself" $ do
            values "WITH t AS (SELECT id, name FROM users) SELECT a.name FROM t a JOIN t b ON a.id = b.id ORDER BY a.name"
                `shouldBe` Right [[VStr "Alice"], [VStr "Bob"], [VStr "Carol"]]

        it "keeps a CTE inside a derived table" $ do
            values "SELECT d.name FROM (WITH t AS (SELECT name FROM users) SELECT t.name FROM t) d"
                `shouldBe` Right [[VStr "Alice"], [VStr "Bob"], [VStr "Carol"]]

        it "rejects a column list that does not match" $ do
            sql "WITH t(a, b) AS (SELECT name FROM users) SELECT t.a FROM t" `shouldSatisfy` isLeft

        it "reports an unknown table for a forward reference" $ do
            sql "WITH a AS (SELECT b.name FROM b), b AS (SELECT name FROM users) SELECT a.name FROM a"
                `shouldSatisfy` isLeft

        it "reports an unknown table for a name that is not a CTE" $ do
            sql "WITH t AS (SELECT name FROM users) SELECT x.name FROM x" `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (比较运算符)" $ do
        -- sql：在默认库上跑 SQL 取结果行
        let sql input = parseStatement input >>= rowsOf . runStatement testDB
            -- values：只取结果里的值
            values input = fmap (map (map snd)) (sql input)

        it "parses >= into GtE" $ do
            parseStatement "SELECT name FROM users WHERE age >= 18"
                `shouldBe` Right (Select [("name", Col "name")] (FromTable Nothing "users") (Just (GtE (Col "age") (LitInt 18))) [] Nothing [] Nothing)

        it "parses <= into LtE" $ do
            parseStatement "SELECT name FROM users WHERE age <= 30"
                `shouldBe` Right (Select [("name", Col "name")] (FromTable Nothing "users") (Just (LtE (Col "age") (LitInt 30))) [] Nothing [] Nothing)

        it "parses <> and != into the same node" $ do
            sql "SELECT name FROM users WHERE age <> 17"
                `shouldBe` sql "SELECT name FROM users WHERE age != 17"
            values "SELECT name FROM users WHERE age <> 17 ORDER BY name"
                `shouldBe` Right [[VStr "Alice"], [VStr "Carol"]]

        it "runs >= and <=" $ do
            values "SELECT name FROM users WHERE age >= 25 ORDER BY name"
                `shouldBe` Right [[VStr "Alice"], [VStr "Carol"]]
            values "SELECT name FROM users WHERE age <= 20" `shouldBe` Right [[VStr "Bob"]]

        it "keeps comparisons with NULL three-valued" $ do
            executeOperator OpGtE [VNull, VInt 1] `shouldBe` Right VNull
            executeOperator OpLtE [VInt 1, VNull] `shouldBe` Right VNull
            executeOperator OpNe [VNull, VInt 1] `shouldBe` Right VNull
            executeOperator OpGtE [VInt 1, VFloat 1.0] `shouldBe` Right (VBool True)
            executeOperator OpNe [VInt 1, VFloat 1.0] `shouldBe` Right (VBool False)

        it "rewrites >= and <= into an inclusive index range" $ do
            let stat rows distinct =
                    TableMeta rows [(c, d, False) | (c, d) <- distinct] []
                dbWith m = [("users", Table "users" [("id", TInt), ("name", TStr)] [] (Just m) Nothing Nothing)]
                indexed = dbWith (stat 10000 [("id", 10000)] ["id"])
                low = Filter (GtE (Col "id") (LitInt 2)) (Scan Nothing "users" Nothing)
                both = Filter (And (GtE (Col "id") (LitInt 1)) (LtE (Col "id") (LitInt 3))) (Scan Nothing "users" Nothing)
                halfOpenTop = Filter (And (GtE (Col "id") (LitInt 1)) (Lt (Col "id") (LitInt 3))) (Scan Nothing "users" Nothing)
                halfOpenBottom = Filter (And (Gt (Col "id") (LitInt 1)) (LtE (Col "id") (LitInt 3))) (Scan Nothing "users" Nothing)
            optimize indexed low `shouldBe` Range Nothing "users" "id" (Just (VInt 2, True)) Nothing
            optimize indexed both
                `shouldBe` Range Nothing "users" "id" (Just (VInt 1, True)) (Just (VInt 3, True))
            -- 开区间那一端标记取反
            optimize indexed halfOpenTop
                `shouldBe` Range Nothing "users" "id" (Just (VInt 1, True)) (Just (VInt 3, False))
            optimize indexed halfOpenBottom
                `shouldBe` Range Nothing "users" "id" (Just (VInt 1, False)) (Just (VInt 3, True))
            optimize indexed (Filter (LtE (Col "id") (LitInt 9)) (Scan Nothing "users" Nothing))
                `shouldBe` Range Nothing "users" "id" Nothing (Just (VInt 9, True))
            -- 统计说索引不划算时保持原样，交给全表扫
            optimize (dbWith (stat 10000 [("id", 10000)] [])) low `shouldBe` low

        it "uses an inclusive range on the real storage" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                _ <- runIPCStorage (runStatementM (CreateTable "rng_t" [("id", TInt), ("code", TInt)]))
                _ <-
                    runIPCStorage
                        ( runStatementM
                            ( Insert
                                "rng_t"
                                ["id", "code"]
                                [ [LitInt 1, LitInt 500]
                                , [LitInt 2, LitInt 501]
                                , [LitInt 3, LitInt 502]
                                , [LitInt 4, LitInt 503]
                                ]
                            )
                        )
                _ <- runIPCStorage (runStatementM (CreateIndex "rng_t" "code"))
                -- ids：按条件取 id，只比较值
                let ids cond =
                        fmap (map (map snd))
                            <$> runIPCStorage (runStatementM (makeSelect ["id"] "rng_t" (Just cond)))
                ids (And (GtE (Col "code") (LitInt 501)) (LtE (Col "code") (LitInt 502)))
                    `shouldReturn` Right [[VInt 2], [VInt 3]]
                -- 闭端含边界、开端不含边界
                ids (And (Gt (Col "code") (LitInt 501)) (LtE (Col "code") (LitInt 502)))
                    `shouldReturn` Right [[VInt 3]]
                ids (And (GtE (Col "code") (LitInt 501)) (Lt (Col "code") (LitInt 502)))
                    `shouldReturn` Right [[VInt 2]]
                ids (And (Gt (Col "code") (LitInt 501)) (Lt (Col "code") (LitInt 502)))
                    `shouldReturn` Right []

    describe "ChuSQL.Core.Engine (ANY / ALL)" $ do
        -- sql：在默认库上跑 SQL 取结果行
        let sql input = parseStatement input >>= rowsOf . runStatement testDB
            -- values：只取结果里的值
            values input = fmap (map (map snd)) (sql input)

        it "parses > ANY into a quantified comparison" $ do
            parseStatement "SELECT name FROM users WHERE age > ANY (SELECT age FROM orders)"
                `shouldBe` Right (Select [("name", Col "name")] (FromTable Nothing "users") (Just (QuantCmp CmpGt (Col "age") (Subquery (Select [("age", Col "age")] (FromTable Nothing "orders") Nothing [] Nothing [] Nothing) []) AnyQ)) [] Nothing [] Nothing)

        it "parses <> ALL into a quantified comparison" $ do
            parseStatement "SELECT name FROM users WHERE age <> ALL (SELECT age FROM orders)"
                `shouldBe` Right (Select [("name", Col "name")] (FromTable Nothing "users") (Just (QuantCmp CmpNe (Col "age") (Subquery (Select [("age", Col "age")] (FromTable Nothing "orders") Nothing [] Nothing [] Nothing) []) AllQ)) [] Nothing [] Nothing)

        it "still reads plain comparisons" $ do
            values "SELECT name FROM users WHERE age > 20 ORDER BY name"
                `shouldBe` Right [[VStr "Alice"], [VStr "Carol"]]

        it "satisfies ANY when at least one row matches" $ do
            values "SELECT name FROM users WHERE age > ANY (SELECT age FROM users WHERE name = 'Bob')"
                `shouldBe` Right [[VStr "Alice"], [VStr "Carol"]]
            values "SELECT name FROM users WHERE age = ANY (SELECT age FROM users WHERE name = 'Bob')"
                `shouldBe` Right [[VStr "Bob"]]

        it "satisfies ALL only when every row matches" $ do
            values "SELECT name FROM users WHERE age >= ALL (SELECT age FROM users)"
                `shouldBe` Right [[VStr "Carol"]]
            values "SELECT name FROM users WHERE age > ALL (SELECT age FROM users)" `shouldBe` Right []

        it "treats an empty subquery as true for ALL and false for ANY" $ do
            values "SELECT name FROM users WHERE age > ALL (SELECT age FROM users WHERE name = 'Nobody')"
                `shouldBe` Right [[VStr "Alice"], [VStr "Bob"], [VStr "Carol"]]
            values "SELECT name FROM users WHERE age > ANY (SELECT age FROM users WHERE name = 'Nobody')"
                `shouldBe` Right []

        it "keeps NULL in the subquery three-valued" $ do
            values "SELECT name FROM users WHERE age > ANY (SELECT NULL)" `shouldBe` Right []
            values "SELECT name FROM users WHERE age > ALL (SELECT NULL)" `shouldBe` Right []

        it "agrees with IN on the equality form" $ do
            sql "SELECT name FROM users WHERE id = ANY (SELECT id FROM orders)"
                `shouldBe` sql "SELECT name FROM users WHERE id IN (SELECT id FROM orders)"

        it "rejects a multi-column or mistyped quantifier subquery" $ do
            isLeft (sql "SELECT name FROM users WHERE age > ANY (SELECT id, name FROM users)")
                `shouldBe` True
            isLeft (sql "SELECT name FROM users WHERE name > ANY (SELECT id FROM users)")
                `shouldBe` True

    describe "ChuSQL.Core.Engine (SELECT)" $ do
        -- sql：在默认库上跑 SQL 取结果行
        let sql input = parseStatement input >>= rowsOf . runStatement testDB
            -- values：只取结果里的值
            values input = fmap (map (map snd)) (sql input)
        it "skips line comments between tokens and at EOF" $ do
            sql "-- start\nSELECT name -- list\nFROM users -- end" `shouldBe` sql "SELECT name FROM users"
        it "skips block comments in lists and WHERE" $ do
            sql "/* start */ SELECT name/* a */, age FROM users WHERE age/* b */>18/* end */"
                `shouldBe` sql "SELECT name, age FROM users WHERE age > 18"
        it "preserves comment markers in strings" $ do
            values "SELECT 'a--b', '/*x*/'" `shouldBe` Right [[VStr "a--b", VStr "/*x*/"]]
        it "reports the position of an unterminated block comment" $ do
            parseStatement "SELECT name FROM users /*"
                `shouldSatisfy` either (\err -> "<query>:1:" `isInfixOf` err && "end of input" `isInfixOf` err) (const False)
        it "evaluates multiplication before addition and comparison" $ do
            values "SELECT 1 + 2 * 3 = 7" `shouldBe` Right [[VBool True]]
        it "honours parentheses and unary minus" $ do
            values "SELECT -(1 + 2) * 3, 5 - -2" `shouldBe` Right [[VInt (-9), VInt 7]]
        it "evaluates arithmetic projections and predicates" $ do
            values "SELECT age + 1 FROM users WHERE age + 1 > 25" `shouldBe` Right [[VInt 26], [VInt 31]]
        it "uses left associative subtraction and integer division" $ do
            values "SELECT 10 - 3 - 2, -7 / 2, 12 / 3 * 2" `shouldBe` Right [[VInt 5, VInt (-3), VInt 8]]
        it "returns division by zero as Left" $ do
            sql "SELECT 1 / 0" `shouldSatisfy` either (isInfixOf "division by zero") (const False)
        it "rejects arithmetic type errors in SELECT" $ do
            sql "SELECT name + 1 FROM users" `shouldSatisfy` either (isInfixOf "SELECT: operator needs TInt") (const False)
        it "checks arithmetic columns even for empty tables" $ do
            (parseStatement "SELECT age + missing FROM users" >>= rowsOf . runStatement emptyDB)
                `shouldSatisfy` either (isInfixOf "unknown column") (const False)
        it "accepts default table prefixes with identical results" $ do
            sql "SELECT users.name FROM users WHERE users.id = 1" `shouldBe` sql "SELECT name FROM users WHERE id = 1"
        it "resolves default prefixes in JOIN" $ do
            values "SELECT users.name, orders.product FROM users JOIN orders ON users.id = orders.user_id"
                `shouldBe` Right [[VStr "Alice", VStr "Book"], [VStr "Alice", VStr "Cup"], [VStr "Bob", VStr "Pen"]]
        it "rejects ambiguous bare JOIN columns" $ do
            sql "SELECT id FROM users JOIN orders ON users.id = orders.user_id"
                `shouldSatisfy` either (isInfixOf "ambiguous column") (const False)
        it "accepts AS and keeps explicit aliases" $ do
            sql "SELECT u.name FROM users AS u WHERE u.id = 1" `shouldBe` Right [[("u.name", VStr "Alice")]]
        it "resolves bare columns in aliased queries" $ do
            sql "SELECT name FROM users u WHERE id = 1" `shouldBe` sql "SELECT u.name FROM users u WHERE u.id = 1"
        it "sorts by default qualified columns" $ do
            sql "SELECT name FROM users ORDER BY users.age" `shouldBe` sql "SELECT name FROM users ORDER BY age"
        it "finds every column inside arithmetic" $ do
            colsInExpr (Add (Neg (Col "a")) (Sub (Mul (Col "b") (Col "c")) (Div (Col "d") (Col "e"))))
                `shouldBe` ["a", "b", "c", "d", "e"]
        it "returns runtime arithmetic type errors" $ do
            evalExpr (Add (LitStr "x") (LitInt 1)) [] `shouldBe` Left "type error: expected two numbers"
            evalExpr (Div (LitInt 1) (LitBool False)) [] `shouldBe` Left "type error: expected two numbers"
            evalExpr (Neg (LitStr "x")) [] `shouldBe` Left "type error: expected a number"
        it "returns division overflow without throwing" $ do
            evalExpr (Div (LitInt minBound) (LitInt (-1))) [] `shouldBe` Left "integer division overflow"
        it "folds arithmetic constants in projections" $ do
            let plan = Compute [("answer", Add (LitInt 1) (Mul (LitInt 2) (LitInt 3)))] Unit
            optimize testDB plan `shouldBe` Compute [("answer", LitInt 7)] Unit
        it "preserves division errors during folding" $ do
            let plan = Compute [("bad", Div (LitInt 1) (Sub (LitInt 2) (LitInt 2)))] Unit
            evalRelOp testDB (optimize testDB plan) `shouldBe` Left "division by zero"
        it "preserves arithmetic JOIN results and order during optimization" $ do
            let plan = do
                    q <- parseStatement "SELECT u.name, o.product, u.age + 1 FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age + 1 > 20"
                    prepare testDB q >>= translate
            case plan of
                Left err -> expectationFailure err
                Right p -> do
                    let expected = Right [[("u.name", VStr "Alice"), ("o.product", VStr "Book"), ("u.age + 1", VInt 26)], [("u.name", VStr "Alice"), ("o.product", VStr "Cup"), ("u.age + 1", VInt 26)]]
                    evalRelOp testDB p `shouldBe` expected
                    evalRelOp testDB (optimize testDB p) `shouldBe` expected
        it "rejects ambiguous WHERE and ORDER BY columns" $ do
            sql "SELECT users.name FROM users JOIN orders ON users.id = orders.user_id WHERE id = 1"
                `shouldSatisfy` either (isInfixOf "ambiguous column") (const False)
            sql "SELECT users.name FROM users JOIN orders ON users.id = orders.user_id ORDER BY id"
                `shouldSatisfy` either (isInfixOf "ambiguous column") (const False)
        it "does not accept the table prefix after an explicit alias" $ do
            sql "SELECT users.name FROM users u" `shouldSatisfy` either (isInfixOf "unknown column") (const False)
        it "rejects duplicate qualifiers instead of selecting the first column" $ do
            sql "SELECT users.name FROM users JOIN users ON users.id = users.id"
                `shouldSatisfy` either (isInfixOf "ambiguous column") (const False)
        it "keeps default-qualified no-index lookup results" $ do
            sql "SELECT users.name FROM users WHERE users.age = 25" `shouldBe` Right [[("name", VStr "Alice")]]
        it "updates columns with arithmetic" $ do
            case parseStatement "UPDATE users SET age = age + 1 WHERE id = 1" >>= runStatement testDB of
                Left err -> expectationFailure err
                Right (db, _) -> rowsOf (runStatement db (makeSelect ["age"] "users" (Just (Eq (Col "id") (LitInt 1))))) `shouldBe` Right [[("age", VInt 26)]]
        it "inserts arithmetic and negative values" $ do
            case parseStatement "INSERT INTO users (id, name, age) VALUES (4, 'a--b/*c*/', -2 + 1)" >>= runStatement testDB of
                Left err -> expectationFailure err
                Right (db, _) -> rowsOf (runStatement db (makeSelect ["age"] "users" (Just (Eq (Col "id") (LitInt 4))))) `shouldBe` Right [[("age", VInt (-1))]]
        it "rejects arithmetic types in empty-table predicates" $ do
            (parseStatement "SELECT name FROM users WHERE name + 1 > 90" >>= rowsOf . runStatement emptyDB)
                `shouldSatisfy` either (isInfixOf "WHERE: operator needs TInt") (const False)
        it "rejects wildcard without FROM" $ do
            sql "SELECT *" `shouldBe` Left "SELECT: * requires FROM"
        it "applies WHERE and LIMIT to the unit row" $ do
            values "SELECT 1 + 2 WHERE FALSE" `shouldBe` Right []
            values "SELECT 1 + 2 LIMIT 0" `shouldBe` Right []
        it "does not move division errors ahead of an empty JOIN" $ do
            let db = [(n, if n == "orders" then t{tableRows = []} else t) | (n, t) <- testDB]
                plan = parseStatement "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age / 0 > 1" >>= prepare db >>= translate
            case plan of
                Left err -> expectationFailure err
                Right p -> do
                    evalRelOp db p `shouldBe` Right []
                    evalRelOp db (optimize db p) `shouldBe` Right []
        it "does not hide JOIN division errors with a pushed predicate" $ do
            let plan = parseStatement "SELECT u.name FROM users u JOIN orders o ON u.id / 0 = o.user_id WHERE u.age > 100" >>= prepare testDB >>= translate
            case plan of
                Left err -> expectationFailure err
                Right p -> do
                    evalRelOp testDB p `shouldBe` Left "division by zero"
                    evalRelOp testDB (optimize testDB p) `shouldBe` Left "division by zero"
        it "keeps default JOIN wildcard headers unambiguous" $ do
            fmap (map (map fst)) (sql "SELECT * FROM users JOIN orders ON users.id = orders.user_id LIMIT 1")
                `shouldBe` Right [["users.id", "users.name", "users.age", "orders.id", "orders.user_id", "orders.product"]]
        it "returns all rows without WHERE" $ do
            rowsOf (runStatement testDB (makeSelect ["*"] "users" Nothing))
                `shouldBe` Right
                    [ [("id", VInt 1), ("name", VStr "Alice"), ("age", VInt 25)]
                    , [("id", VInt 2), ("name", VStr "Bob"), ("age", VInt 17)]
                    , [("id", VInt 3), ("name", VStr "Carol"), ("age", VInt 30)]
                    ]

        it "filters with WHERE" $ do
            rowsOf
                ( runStatement
                    testDB
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Gt (Col "age") (LitInt 18)))
                    )
                )
                `shouldBe` Right
                    [ [("name", VStr "Alice")]
                    , [("name", VStr "Carol")]
                    ]

        it "projects only the requested columns" $ do
            rowsOf (runStatement testDB (makeSelect ["name"] "users" Nothing))
                `shouldBe` Right
                    [ [("name", VStr "Alice")]
                    , [("name", VStr "Bob")]
                    , [("name", VStr "Carol")]
                    ]

        it "returns Left for an unknown table" $ do
            runStatement testDB (makeSelect ["name"] "nonexistent" Nothing)
                `shouldSatisfy` isLeft

        it "returns Left for an unknown projected column" $ do
            runStatement testDB (makeSelect ["nope"] "users" Nothing)
                `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (INSERT)" $ do
        it "parses a simple INSERT" $ do
            parseStatement "INSERT INTO users (name, age) VALUES ('Dave', 22)"
                `shouldBe` Right (Insert "users" ["name", "age"] [[LitStr "Dave", LitInt 22]])

        it "executes INSERT and adds a row at the end" $ do
            case runStatement testDB (Insert "users" ["name", "age"] [[LitStr "Dave", LitInt 22]]) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runStatement db' (makeSelect ["name", "age"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "Alice"), ("age", VInt 25)]
                            , [("name", VStr "Bob"), ("age", VInt 17)]
                            , [("name", VStr "Carol"), ("age", VInt 30)]
                            , [("name", VStr "Dave"), ("age", VInt 22)]
                            ]

        it "does not modify the original database" $ do
            case runStatement testDB (Insert "users" ["name", "age"] [[LitStr "Dave", LitInt 22]]) of
                Left err -> expectationFailure err
                Right (_, _) -> do
                    rowsOf (runStatement testDB (makeSelect ["name"] "users" Nothing))
                        `shouldSatisfy` \case
                            Right rows -> length rows == 3
                            Left _ -> False

        it "returns Left when column count does not match value count" $ do
            runStatement testDB (Insert "users" ["name", "age"] [[LitStr "Dave"]])
                `shouldSatisfy` isLeft

        it "returns Left when a value has the wrong type for comparison" $ do
            runStatement testDB (Insert "users" ["name"] [[Col "other"]])
                `shouldSatisfy` isLeft
        it "writes id column to index and queries it back" $ do
            case parseStatement "INSERT INTO users (id, name, age) VALUES (42, 'Zoe', 20)" >>= runStatement testDB of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (parseStatement "SELECT name FROM users WHERE id = 42" >>= runStatement db')
                        `shouldBe` Right [[("name", VStr "Zoe")]]

        it "inserts all rows of a multi-row INSERT" $ do
            case parseStatement "INSERT INTO users (id, name, age) VALUES (7, 'A', 1), (8, 'B', 2)" >>= runStatement testDB of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (parseStatement "SELECT name FROM users WHERE age < 3" >>= runStatement db')
                        `shouldBe` Right [[("name", VStr "A")], [("name", VStr "B")]]
                    runStatement testDB (Insert "users" ["id", "name"] [[LitInt 1, LitStr "A"], [LitInt 2]])
                        `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (INDEX)" $ do
        it "parses CREATE INDEX and DROP INDEX" $ do
            parseStatement "CREATE INDEX ON users (age)"
                `shouldBe` Right (CreateIndex "users" "age")
            parseStatement "DROP INDEX ON users (age)"
                `shouldBe` Right (DropIndex "users" "age")

        it "rejects an index on an unknown column" $ do
            (parseStatement "CREATE INDEX ON users (nope)" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects an index on an unknown table" $ do
            (parseStatement "CREATE INDEX ON nope (age)" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "accepts index DDL in the memory storage (no-op there)" $ do
            rowsOf (parseStatement "CREATE INDEX ON users (age)" >>= runStatement testDB)
                `shouldBe` Right []
            rowsOf (parseStatement "DROP INDEX ON users (age)" >>= runStatement testDB)
                `shouldBe` Right []

    describe "ChuSQL.Core.Engine (DROP COLUMN)" $ do
        it "parses ALTER TABLE ... DROP COLUMN case-insensitively" $ do
            parseStatement "ALTER TABLE users DROP COLUMN age"
                `shouldBe` Right (DropColumn "users" "age")
            parseStatement "alter table users drop column age"
                `shouldBe` Right (DropColumn "users" "age")

        it "after execution the column definition and every row cell are gone, other columns untouched" $ do
            case runStatement testDB (DropColumn "users" "age") of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    case lookup "users" db' of
                        Nothing -> expectationFailure "users table is gone"
                        Just t -> do
                            map fst (tableCols t) `shouldBe` ["id", "name"]
                            tableRows t `shouldSatisfy` all (all (\(k, _) -> k /= "age"))
                            let rowCountBefore = maybe 0 (length . tableRows) (lookup "users" testDB)
                            length (tableRows t) `shouldBe` rowCountBefore
                            tableRows t `shouldBe` map (filter ((/= "age") . fst)) (maybe [] tableRows (lookup "users" testDB))

        it "unknown column or unknown table reports an error" $ do
            (parseStatement "ALTER TABLE users DROP COLUMN nope" >>= runStatement testDB)
                `shouldSatisfy` isLeft
            (parseStatement "ALTER TABLE nope DROP COLUMN age" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "the built-in id column cannot be dropped (the UI locates a row by it)" $ do
            (parseStatement "ALTER TABLE users DROP COLUMN id" >>= runStatement testDB)
                `shouldSatisfy` isLeft
    describe "ChuSQL.Core.Engine (USER)" $ do
        it "parses CREATE USER / ALTER USER / DROP USER case-insensitively" $ do
            parseStatement "CREATE USER alice IDENTIFIED BY 's3cret'"
                `shouldBe` Right (CreateUser "alice" "s3cret")
            parseStatement "create user alice identified by 's3cret'"
                `shouldBe` Right (CreateUser "alice" "s3cret")
            parseStatement "ALTER USER alice IDENTIFIED BY 'next-one'"
                `shouldBe` Right (AlterUser "alice" "next-one")
            parseStatement "alter user alice identified by 'next-one'"
                `shouldBe` Right (AlterUser "alice" "next-one")
            parseStatement "DROP USER alice"
                `shouldBe` Right (DropUser "alice")

        it "accepts a quoted account name so dot and dash are usable" $ do
            parseStatement "CREATE USER 'a.b-c' IDENTIFIED BY 'p'"
                `shouldBe` Right (CreateUser "a.b-c" "p")
            parseStatement "DROP USER 'a.b-c'"
                `shouldBe` Right (DropUser "a.b-c")

        it "keeps a doubled quote inside the password" $ do
            parseStatement "CREATE USER alice IDENTIFIED BY 'it''s ok'"
                `shouldBe` Right (CreateUser "alice" "it's ok")

        it "an empty password or a missing clause is an error" $ do
            (parseStatement "CREATE USER alice IDENTIFIED BY ''" >>= prepare testDB)
                `shouldSatisfy` isLeft
            parseStatement "CREATE USER alice" `shouldSatisfy` isLeft
            parseStatement "CREATE USER" `shouldSatisfy` isLeft
            parseStatement "DROP USER" `shouldSatisfy` isLeft

        it "refuses to invent account rows in the query engine" $ do
            (parseStatement "CREATE USER alice IDENTIFIED BY 's3cret'" >>= runStatement testDB)
                `shouldSatisfy` isLeft
            (parseStatement "ALTER USER alice IDENTIFIED BY 'next'" >>= runStatement testDB)
                `shouldSatisfy` isLeft
            (parseStatement "DROP USER alice" >>= runStatement testDB)
                `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (ROLE)" $ do
        it "parses CREATE ROLE / DROP ROLE case-insensitively" $ do
            parseStatement "CREATE ROLE analyst" `shouldBe` Right (CreateRole "analyst")
            parseStatement "create role analyst" `shouldBe` Right (CreateRole "analyst")
            parseStatement "DROP ROLE analyst" `shouldBe` Right (DropRole "analyst")
            parseStatement "drop role 'a.b'" `shouldBe` Right (DropRole "a.b")

        it "parses privilege grants on one table, on another database's table or on everything" $ do
            parseStatement "GRANT SELECT ON users TO analyst"
                `shouldBe` Right (GrantPrivileges ["SELECT"] "users" "analyst" False)
            parseStatement "grant select, insert on users to analyst"
                `shouldBe` Right (GrantPrivileges ["SELECT", "INSERT"] "users" "analyst" False)
            parseStatement "GRANT ALL ON sales.orders TO analyst"
                `shouldBe` Right (GrantPrivileges ["ALL"] "sales.orders" "analyst" False)
            parseStatement "GRANT DELETE ON * TO analyst"
                `shouldBe` Right (GrantPrivileges ["DELETE"] "*" "analyst" False)

        it "parses WITH GRANT OPTION on a privilege grant" $ do
            parseStatement "GRANT SELECT ON users TO analyst WITH GRANT OPTION"
                `shouldBe` Right (GrantPrivileges ["SELECT"] "users" "analyst" True)
            parseStatement "grant select, insert on * to analyst with grant option"
                `shouldBe` Right (GrantPrivileges ["SELECT", "INSERT"] "*" "analyst" True)
            parseStatement "GRANT SELECT ON users TO analyst WITH"
                `shouldSatisfy` isLeft

        it "parses database scopes, explicit tables and qualified wildcards" $ do
            parseStatement "GRANT CONNECT, CREATE ON DATABASE sales TO analyst"
                `shouldBe` Right (GrantPrivileges ["CONNECT", "CREATE"] "database:sales" "analyst" False)
            parseStatement "REVOKE CONNECT ON DATABASE sales FROM PUBLIC"
                `shouldBe` Right (RevokePrivileges ["CONNECT"] "database:sales" "PUBLIC")
            parseStatement "GRANT SELECT ON TABLE sales.orders TO analyst"
                `shouldBe` Right (GrantPrivileges ["SELECT"] "sales.orders" "analyst" False)
            parseStatement "REVOKE ALL ON sales.* FROM analyst"
                `shouldBe` Right (RevokePrivileges ["ALL"] "sales.*" "analyst")

        it "parses REVOKE symmetrically" $ do
            parseStatement "REVOKE SELECT ON users FROM analyst"
                `shouldBe` Right (RevokePrivileges ["SELECT"] "users" "analyst")
            parseStatement "revoke update, delete on * from analyst"
                `shouldBe` Right (RevokePrivileges ["UPDATE", "DELETE"] "*" "analyst")

        it "tells a role membership apart from a privilege grant" $ do
            parseStatement "GRANT analyst TO alice"
                `shouldBe` Right (GrantRole "analyst" ["alice"])
            parseStatement "grant analyst to alice, bob"
                `shouldBe` Right (GrantRole "analyst" ["alice", "bob"])
            parseStatement "REVOKE analyst FROM alice, bob"
                `shouldBe` Right (RevokeRole "analyst" ["alice", "bob"])

        it "an unknown privilege or a missing clause is an error" $ do
            parseStatement "GRANT EXECUTE ON users TO analyst" `shouldSatisfy` isLeft
            parseStatement "GRANT SELECT ON users" `shouldSatisfy` isLeft
            parseStatement "GRANT analyst TO" `shouldSatisfy` isLeft
            parseStatement "REVOKE analyst FROM" `shouldSatisfy` isLeft
            parseStatement "CREATE ROLE" `shouldSatisfy` isLeft
            parseStatement "DROP ROLE" `shouldSatisfy` isLeft

        it "checks role names, privileges and grant objects before execution" $ do
            (parseStatement "CREATE ROLE select" >>= prepare testDB) `shouldSatisfy` isLeft
            (parseStatement "DROP ROLE insert" >>= prepare testDB) `shouldSatisfy` isLeft
            (parseStatement "GRANT SELECT ON users TO analyst" >>= prepare testDB)
                `shouldBe` Right (GrantPrivileges ["SELECT"] "users" "analyst" False)
            (parseStatement "GRANT SELECT ON users TO analyst WITH GRANT OPTION" >>= prepare testDB)
                `shouldBe` Right (GrantPrivileges ["SELECT"] "users" "analyst" True)
            (parseStatement "GRANT SELECT ON users TO select" >>= prepare testDB) `shouldSatisfy` isLeft
            (parseStatement "REVOKE SELECT ON users FROM analyst" >>= prepare testDB)
                `shouldBe` Right (RevokePrivileges ["SELECT"] "users" "analyst")
            (parseStatement "GRANT analyst TO alice, bob" >>= prepare testDB)
                `shouldBe` Right (GrantRole "analyst" ["alice", "bob"])
            (parseStatement "REVOKE analyst FROM alice" >>= prepare testDB)
                `shouldBe` Right (RevokeRole "analyst" ["alice"])

        it "refuses to invent roles in the query engine (the server owns them)" $ do
            (parseStatement "CREATE ROLE analyst" >>= runStatement testDB) `shouldSatisfy` isLeft
            (parseStatement "DROP ROLE analyst" >>= runStatement testDB) `shouldSatisfy` isLeft
            (parseStatement "GRANT SELECT ON users TO analyst" >>= runStatement testDB) `shouldSatisfy` isLeft
            (parseStatement "REVOKE SELECT ON users FROM analyst" >>= runStatement testDB) `shouldSatisfy` isLeft
            (parseStatement "GRANT analyst TO alice" >>= runStatement testDB) `shouldSatisfy` isLeft
            (parseStatement "REVOKE analyst FROM alice" >>= runStatement testDB) `shouldSatisfy` isLeft

        it "user is reserved as an alias but still usable inside a longer name" $ do
            parseStatement "SELECT * FROM users user" `shouldSatisfy` isLeft
            parseStatement "SELECT user1 FROM users" `shouldBe` Right (Select [("user1", Col "user1")] (FromTable Nothing "users") Nothing [] Nothing [] Nothing)

    describe "ChuSQL.Core.Engine (DELETE)" $ do
        it "parses DELETE with WHERE" $ do
            parseStatement "DELETE FROM users WHERE age < 18"
                `shouldBe` Right (Delete "users" (Just (Lt (Col "age") (LitInt 18))))

        it "parses DELETE without WHERE" $ do
            parseStatement "DELETE FROM users"
                `shouldBe` Right (Delete "users" Nothing)

        it "executes DELETE with WHERE, removing matching rows" $ do
            case runStatement testDB (Delete "users" (Just (Lt (Col "age") (LitInt 18)))) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runStatement db' (makeSelect ["name"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "Alice")]
                            , [("name", VStr "Carol")]
                            ]

        it "executes DELETE without WHERE, removing all rows" $ do
            case runStatement testDB (Delete "users" Nothing) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runStatement db' (makeSelect ["name"] "users" Nothing))
                        `shouldBe` Right []

        it "returns Left when WHERE references an unknown column" $ do
            runStatement testDB (Delete "users" (Just (Gt (Col "unknown") (LitInt 18))))
                `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (UPDATE)" $ do
        it "parses UPDATE with WHERE" $ do
            parseStatement "UPDATE users SET age = 26 WHERE name = 'Alice'"
                `shouldBe` Right
                    ( Update
                        "users"
                        [("age", LitInt 26)]
                        (Just (Eq (Col "name") (LitStr "Alice")))
                    )

        it "parses UPDATE without WHERE" $ do
            parseStatement "UPDATE users SET age = 26"
                `shouldBe` Right
                    (Update "users" [("age", LitInt 26)] Nothing)

        it "parses multiple assignments" $ do
            parseStatement "UPDATE users SET age = 26, name = 'Dave'"
                `shouldBe` Right
                    ( Update
                        "users"
                        [("age", LitInt 26), ("name", LitStr "Dave")]
                        Nothing
                    )

        it "executes UPDATE with WHERE, modifying matching rows" $ do
            case runStatement testDB (Update "users" [("age", LitInt 99)] (Just (Eq (Col "name") (LitStr "Alice")))) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runStatement db' (makeSelect ["name", "age"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "Alice"), ("age", VInt 99)]
                            , [("name", VStr "Bob"), ("age", VInt 17)]
                            , [("name", VStr "Carol"), ("age", VInt 30)]
                            ]

        it "executes UPDATE without WHERE, modifying all rows" $ do
            case runStatement testDB (Update "users" [("age", LitInt 0)] Nothing) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runStatement db' (makeSelect ["name", "age"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "Alice"), ("age", VInt 0)]
                            , [("name", VStr "Bob"), ("age", VInt 0)]
                            , [("name", VStr "Carol"), ("age", VInt 0)]
                            ]

        it "applies multiple assignments to each row" $ do
            case runStatement testDB (Update "users" [("age", LitInt 99), ("name", LitStr "X")] Nothing) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runStatement db' (makeSelect ["name", "age"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "X"), ("age", VInt 99)]
                            , [("name", VStr "X"), ("age", VInt 99)]
                            , [("name", VStr "X"), ("age", VInt 99)]
                            ]

        it "returns Left when WHERE references an unknown column" $ do
            runStatement testDB (Update "users" [("age", LitInt 0)] (Just (Gt (Col "unknown") (LitInt 1))))
                `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (ORDER BY)" $ do
        it "sorts projection aliases with source keys and LIMIT" $ do
            let query = "SELECT age + 1 AS next, name FROM users ORDER BY next DESC, name LIMIT 2"
            rowsOf (parseStatement query >>= runStatement testDB)
                `shouldBe` Right [[("next", VInt 31), ("name", VStr "Carol")], [("next", VInt 26), ("name", VStr "Alice")]]
            sameResultAsUnoptimized query
        it "prefers output aliases over same-named source columns" $
            rowsOf (parseStatement "SELECT -age AS age FROM users ORDER BY age LIMIT 1" >>= runStatement testDB)
                `shouldBe` Right [[("age", VInt (-30))]]
        it "evaluates non-ordering projections after LIMIT" $ do
            rowsOf (parseStatement "SELECT name AS who, 1 / (age - 25) AS value FROM users ORDER BY who DESC LIMIT 1" >>= runStatement testDB)
                `shouldBe` Right [[("who", VStr "Carol"), ("value", VInt 0)]]
            rowsOf (parseStatement "SELECT 1 / 0 AS bad FROM users ORDER BY bad LIMIT 0" >>= runStatement testDB)
                `shouldSatisfy` isLeft
        it "sorts aggregate aliases and rejects ungrouped source sort keys" $ do
            rowsOf (parseStatement "SELECT user_id, count(*) AS total FROM orders GROUP BY user_id ORDER BY total DESC" >>= runStatement testDB)
                `shouldBe` Right [[("user_id", VInt 1), ("total", VInt 2)], [("user_id", VInt 2), ("total", VInt 1)]]
            (parseStatement "SELECT count(*) AS total FROM users ORDER BY age" >>= runStatement testDB)
                `shouldSatisfy` isLeft
        it "keeps ordering aliases when a CTE renames its outputs" $
            rowsOf (parseStatement "WITH t(x) AS (SELECT age + 1 AS next FROM users ORDER BY next DESC LIMIT 1) SELECT x FROM t" >>= runStatement testDB)
                `shouldBe` Right [[("t.x", VInt 31)]]
        it "rejects duplicate ordering aliases without changing duplicate outputs" $ do
            (parseStatement "SELECT age AS x, id AS x FROM users ORDER BY x" >>= runStatement testDB)
                `shouldSatisfy` either (isInfixOf "ambiguous projection alias") (const False)
            rowsOf (parseStatement "SELECT age AS x, id AS x, name AS who FROM users ORDER BY who LIMIT 1" >>= runStatement testDB)
                `shouldBe` Right [[("x", VInt 25), ("x", VInt 1), ("who", VStr "Alice")]]
        it "parses ORDER BY DESC" $ do
            case parseStatement "SELECT name FROM users ORDER BY age DESC" of
                Right q -> selectOrderBy q `shouldBe` [(OrderColumn "age", Desc)]
                Left err -> expectationFailure err

        it "defaults to ASC" $ do
            case parseStatement "SELECT name FROM users ORDER BY age" of
                Right q -> selectOrderBy q `shouldBe` [(OrderColumn "age", Asc)]
                Left err -> expectationFailure err

        it "parses multiple ORDER BY columns" $ do
            case parseStatement "SELECT name FROM users ORDER BY age DESC, name ASC" of
                Right q -> selectOrderBy q `shouldBe` [(OrderColumn "age", Desc), (OrderColumn "name", Asc)]
                Left err -> expectationFailure err

        it "executes ORDER BY age ASC" $ do
            let q =
                    Select
                        { selectItems = [("name", Col "name")]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectHaving = Nothing
                        , selectOrderBy = [(OrderColumn "age", Asc)]
                        , selectLimit = Nothing
                        }
            rowsOf (runStatement testDB q)
                `shouldBe` Right
                    [ [("name", VStr "Bob")]
                    , [("name", VStr "Alice")]
                    , [("name", VStr "Carol")]
                    ]

        it "executes ORDER BY age DESC" $ do
            let q =
                    Select
                        { selectItems = [("name", Col "name")]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectHaving = Nothing
                        , selectOrderBy = [(OrderColumn "age", Desc)]
                        , selectLimit = Nothing
                        }
            rowsOf (runStatement testDB q)
                `shouldBe` Right
                    [ [("name", VStr "Carol")]
                    , [("name", VStr "Alice")]
                    , [("name", VStr "Bob")]
                    ]

    describe "ChuSQL.Core.Engine (LIMIT)" $ do
        it "parses LIMIT" $ do
            case parseStatement "SELECT * FROM users LIMIT 2" of
                Right q -> selectLimit q `shouldBe` Just 2
                Left err -> expectationFailure err

        it "parses LIMIT after ORDER BY" $ do
            case parseStatement "SELECT * FROM users ORDER BY age DESC LIMIT 1" of
                Right q -> do
                    selectOrderBy q `shouldBe` [(OrderColumn "age", Desc)]
                    selectLimit q `shouldBe` Just 1
                Left err -> expectationFailure err

        it "executes LIMIT without ORDER BY" $ do
            let q =
                    Select
                        { selectItems = [("name", Col "name")]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectHaving = Nothing
                        , selectOrderBy = []
                        , selectLimit = Just 2
                        }
            rowsOf (runStatement testDB q)
                `shouldBe` Right
                    [ [("name", VStr "Alice")]
                    , [("name", VStr "Bob")]
                    ]

        it "executes LIMIT with ORDER BY" $ do
            let q =
                    Select
                        { selectItems = [("name", Col "name")]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectHaving = Nothing
                        , selectOrderBy = [(OrderColumn "age", Desc)]
                        , selectLimit = Just 2
                        }
            rowsOf (runStatement testDB q)
                `shouldBe` Right
                    [ [("name", VStr "Carol")]
                    , [("name", VStr "Alice")]
                    ]

        it "executes LIMIT 0" $ do
            let q =
                    Select
                        { selectItems = [("name", Col "name")]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectHaving = Nothing
                        , selectOrderBy = []
                        , selectLimit = Just 0
                        }
            rowsOf (runStatement testDB q)
                `shouldBe` Right []

        it "keeps errors when LIMIT wraps a projection" $ do
            let projection = Compute [("value", Div (LitInt 1) (Sub (Col "age") (LitInt 17)))] (Scan Nothing "users" Nothing)
            evalRelOp testDB (Limit 1 projection) `shouldBe` Left "division by zero"
            evalRelOp testDB (Limit 0 projection) `shouldBe` Left "division by zero"
            evalRelOp testDB (Limit 1 (Compute [("name", Col "name")] (Scan Nothing "users" Nothing)))
                `shouldBe` Right [[("name", VStr "Alice")]]

        it "keeps filter errors beyond LIMIT without changing matching row order" $ do
            -- \| 解析并执行过滤截断查询
            let query text = parseStatement text >>= rowsOf . runStatement testDB
            query "SELECT name FROM users WHERE 100 / (age - 17) > 0 LIMIT 1" `shouldBe` Left "division by zero"
            query "SELECT name FROM users WHERE 100 / (age - 17) > 0 LIMIT 0" `shouldBe` Left "division by zero"
            query "SELECT name FROM users WHERE age > 20 LIMIT 1" `shouldBe` Right [[("name", VStr "Alice")]]

        it "rejects negative LIMIT" $ do
            parseStatement "SELECT * FROM users LIMIT -1"
                `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (JOIN)" $ do
        it "preserves duplicate key order, numeric equality and NULL padding in hash joins" $ do
            let leftRows = [[("id", key)] | key <- [VInt 1, VInt 2, VNull, VInt 3, VInt 1]]
                rightRows =
                    [ [("user_id", key), ("id", VInt position)]
                    | (position, key) <- zip [1 ..] [VInt 1, VInt 2, VFloat 1, VNull, VInt 2, VInt 1]
                    ]
                db = [("users", users{tableRows = leftRows, tableCols = [("id", TInt)]}), ("orders", orders{tableRows = rightRows, tableCols = [("user_id", TInt), ("id", TInt)]})]
                -- \| 派生右表直接覆盖哈希连接路径
                plan kind = Join kind (Scan (Just "u") "users" Nothing) (Derived (Just "o") (Scan Nothing "orders" Nothing)) (Eq (Col "u.id") (Col "o.user_id"))
                -- \| 构造预期输出行
                output key other position = [("u.id", key), ("o.user_id", other), ("o.id", position)]
                ones = [output (VInt 1) other (VInt position) | (other, position) <- [(VInt 1, 1), (VFloat 1, 3), (VInt 1, 6)]]
                twos = [output (VInt 2) (VInt 2) (VInt position) | position <- [2, 5]]
            evalRelOp db (plan InnerJoin) `shouldBe` Right (ones ++ twos ++ ones)
            evalRelOp db (plan LeftJoin)
                `shouldBe` Right (ones ++ twos ++ [output VNull VNull VNull, output (VInt 3) VNull VNull] ++ ones)

        it "executes a two-table JOIN with aliases" $ do
            rowsOf (parseStatement "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id" >>= runStatement testDB)
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    , [("u.name", VStr "Bob"), ("o.product", VStr "Pen")]
                    ]

        it "executes JOIN with WHERE" $ do
            rowsOf (parseStatement "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 20" >>= runStatement testDB)
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    ]

        it "executes JOIN with ORDER BY" $ do
            rowsOf (parseStatement "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id ORDER BY o.product DESC" >>= runStatement testDB)
                `shouldBe` Right
                    [ [("u.name", VStr "Bob"), ("o.product", VStr "Pen")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    ]

        it "executes JOIN with LIMIT" $ do
            rowsOf (parseStatement "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id LIMIT 2" >>= runStatement testDB)
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    ]

        it "returns Left when JOIN table not found" $ do
            runStatement
                testDB
                ( Select
                    { selectItems = [("u.name", Col "u.name")]
                    , selectFrom =
                        FromJoin
                            InnerJoin
                            (FromTable (Just "u") "users")
                            (FromTable (Just "x") "nonexistent")
                            (Eq (Col "u.id") (Col "x.id"))
                    , selectWhere = Nothing
                    , selectGroupBy = []
                    , selectHaving = Nothing
                    , selectOrderBy = []
                    , selectLimit = Nothing
                    }
                )
                `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (LEFT JOIN)" $ do
        -- sql：在默认库上跑 SQL 取结果行
        let sql input = parseStatement input >>= rowsOf . runStatement testDB

        it "keeps unmatched left rows and fills the right side with NULL" $ do
            sql "SELECT u.name, o.product FROM users u LEFT JOIN orders o ON u.id = o.user_id"
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    , [("u.name", VStr "Bob"), ("o.product", VStr "Pen")]
                    , [("u.name", VStr "Carol"), ("o.product", VNull)]
                    ]

        it "emits NULL rows even when the right table is empty" $ do
            let db = [("users", users), ("orders", orders{tableRows = []})]
            (parseStatement "SELECT u.name, o.product FROM users u LEFT JOIN orders o ON u.id = o.user_id" >>= rowsOf . runStatement db)
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VNull)]
                    , [("u.name", VStr "Bob"), ("o.product", VNull)]
                    , [("u.name", VStr "Carol"), ("o.product", VNull)]
                    ]

        it "a WHERE on the right side drops the NULL rows again" $ do
            sql "SELECT u.name, o.product FROM users u LEFT JOIN orders o ON u.id = o.user_id WHERE o.product IS NOT NULL"
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    , [("u.name", VStr "Bob"), ("o.product", VStr "Pen")]
                    ]

        it "a WHERE on the left side keeps the NULL rows" $ do
            sql "SELECT u.name, o.product FROM users u LEFT JOIN orders o ON u.id = o.user_id WHERE u.age > 18"
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    , [("u.name", VStr "Carol"), ("o.product", VNull)]
                    ]

        it "matches INNER JOIN when every left row has a match" $ do
            sql "SELECT u.name, o.product FROM users u LEFT JOIN orders o ON u.id = o.user_id WHERE o.product IS NOT NULL ORDER BY o.product"
                `shouldBe` sql "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id ORDER BY o.product"

        it "keeps LEFT JOIN results identical after optimization" $ do
            sameResultAsUnoptimized "SELECT u.name, o.product FROM users u LEFT JOIN orders o ON u.id = o.user_id"
            sameResultAsUnoptimized "SELECT u.name, o.product FROM users u LEFT JOIN orders o ON u.id = o.user_id WHERE u.age > 18"

        it "keeps the LeftJoin node and does not push a right-side predicate across it" $ do
            fmap (leftJoinKinds . optimize testDB) (parseStatement "SELECT u.name FROM users u LEFT JOIN orders o ON u.id = o.user_id WHERE o.product = 'Book'" >>= translate)
                `shouldBe` Right [True]

        it "a non-equi ON goes through the nested loop path and still fills NULLs" $ do
            sql "SELECT u.name, o.product FROM users u LEFT JOIN orders o ON u.id > o.user_id"
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VNull)]
                    , [("u.name", VStr "Bob"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Bob"), ("o.product", VStr "Cup")]
                    , [("u.name", VStr "Carol"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Carol"), ("o.product", VStr "Pen")]
                    , [("u.name", VStr "Carol"), ("o.product", VStr "Cup")]
                    ]

    describe "ChuSQL.Core.Engine (JOIN 嵌套循环)" $ do
        -- sql：在默认库上跑 SQL 取结果行
        let sql input = parseStatement input >>= rowsOf . runStatement testDB

        it "keeps columns from both sides when the ON has no equi key" $ do
            sql "SELECT u.name, o.product FROM users u JOIN orders o ON u.id > o.user_id"
                `shouldBe` Right
                    [ [("u.name", VStr "Bob"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Bob"), ("o.product", VStr "Cup")]
                    , [("u.name", VStr "Carol"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Carol"), ("o.product", VStr "Pen")]
                    , [("u.name", VStr "Carol"), ("o.product", VStr "Cup")]
                    ]

    describe "ChuSQL.Core.Engine (子查询)" $ do
        -- sql：在默认库上跑 SQL 取结果行
        let sql input = parseStatement input >>= rowsOf . runStatement testDB

        it "IN (subquery) keeps the rows whose value the subquery returns" $ do
            sql "SELECT name FROM users WHERE id IN (SELECT user_id FROM orders)"
                `shouldBe` Right [[("name", VStr "Alice")], [("name", VStr "Bob")]]

        it "NOT IN (subquery) keeps the other rows" $ do
            sql "SELECT name FROM users WHERE id NOT IN (SELECT user_id FROM orders)"
                `shouldBe` Right [[("name", VStr "Carol")]]

        it "EXISTS works as a correlated subquery" $ do
            sql "SELECT u.name FROM users u WHERE EXISTS (SELECT * FROM orders o WHERE o.user_id = u.id)"
                `shouldBe` Right [[("u.name", VStr "Alice")], [("u.name", VStr "Bob")]]

        it "NOT EXISTS keeps the rows with no match" $ do
            sql "SELECT u.name FROM users u WHERE NOT EXISTS (SELECT * FROM orders o WHERE o.user_id = u.id)"
                `shouldBe` Right [[("u.name", VStr "Carol")]]

        it "a scalar subquery is compared as a single value" $ do
            sql "SELECT name FROM users WHERE age > (SELECT AVG(age) FROM users)"
                `shouldBe` Right [[("name", VStr "Alice")], [("name", VStr "Carol")]]

        it "a scalar subquery with no rows behaves like NULL" $ do
            sql "SELECT name FROM users WHERE age > (SELECT age FROM users WHERE id = 99)"
                `shouldBe` Right []

        it "rejects a scalar subquery that returns more than one row" $ do
            (parseStatement "SELECT name FROM users WHERE age > (SELECT age FROM users)" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects a scalar subquery with more than one column" $ do
            (parseStatement "SELECT name FROM users WHERE age > (SELECT id, age FROM users)" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column inside a subquery" $ do
            (parseStatement "SELECT name FROM users WHERE id IN (SELECT nope FROM orders)" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects a subquery that is not a SELECT" $ do
            runStatement testDB (Delete "users" (Just (ExistsSub (Subquery (DropTable "orders") []) False)))
                `shouldSatisfy` isLeft

        it "runs a subquery in a JOIN condition" $ do
            sql "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id AND o.product IN (SELECT product FROM orders WHERE user_id = 1)"
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    ]

        it "keeps subquery results identical after optimization" $ do
            sameResultAsUnoptimized "SELECT name FROM users WHERE id IN (SELECT user_id FROM orders)"
            sameResultAsUnoptimized "SELECT u.name FROM users u WHERE EXISTS (SELECT * FROM orders o WHERE o.user_id = u.id)"

    describe "ChuSQL.Core.Engine (子查询去相关)" $ do
        it "reuses an uncorrelated JOIN subquery across all pairs" $ do
            let query = "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id AND o.product IN (SELECT product FROM orders WHERE user_id = 1)"
                (result, scans) = countedRun query
            fmap (map (map snd)) result `shouldBe` Right [[VStr "Alice", VStr "Book"], [VStr "Alice", VStr "Cup"]]
            scanTimes "orders" scans `shouldBe` 2
            sameResultAsUnoptimized query

        it "reuses JOIN subqueries by values from both sides" $ do
            let query = "SELECT o.product FROM orders o JOIN users u ON o.user_id = u.id AND EXISTS (SELECT id FROM users x WHERE x.id = o.user_id AND x.age = u.age)"
                (result, scans) = countedRun query
            fmap (map (map snd)) result `shouldBe` Right [[VStr "Book"], [VStr "Pen"], [VStr "Cup"]]
            scanTimes "users" scans `shouldBe` 7
            sameResultAsUnoptimized query

        it "keeps unmatched LEFT JOIN rows with a cached subquery" $ do
            let query = "SELECT u.name, o.product FROM users u LEFT JOIN orders o ON u.id = o.user_id AND EXISTS (SELECT id FROM users WHERE id = 1)"
                (result, scans) = countedRun query
            fmap (map (map snd)) result `shouldBe` Right [[VStr "Alice", VStr "Book"], [VStr "Alice", VStr "Cup"], [VStr "Bob", VStr "Pen"], [VStr "Carol", VNull]]
            scanTimes "users" scans `shouldBe` 2
            sameResultAsUnoptimized query

        it "does not evaluate JOIN subqueries when there are no pairs" $ do
            let query = "SELECT u.name FROM users u LEFT JOIN (SELECT id FROM orders WHERE id < 0) o ON EXISTS (SELECT 1 / 0 FROM orders)"
                (result, scans) = countedRun query
            fmap (map (map snd)) result `shouldBe` Right [[VStr "Alice"], [VStr "Bob"], [VStr "Carol"]]
            scanTimes "orders" scans `shouldBe` 1
            sameResultAsUnoptimized query

        it "propagates JOIN subquery errors before subsequent pairs" $ do
            fst (countedRun "SELECT u.name FROM users u JOIN orders o ON EXISTS (SELECT 1 / 0 FROM orders)")
                `shouldSatisfy` either (isInfixOf "division by zero") (const False)

        it "runs a non-correlated subquery only once" $ do
            let (result, scans) = countedRun "SELECT name FROM users WHERE id IN (SELECT user_id FROM orders)"
            fmap (map (map snd)) result `shouldBe` Right [[VStr "Alice"], [VStr "Bob"]]
            scanTimes "orders" scans `shouldBe` 1

        it "scans simple correlated equality once through a semi join" $ do
            let (result, scans) =
                    countedRun "SELECT o.product FROM orders o WHERE EXISTS (SELECT * FROM users u WHERE u.id = o.user_id)"
            fmap (map (map snd)) result `shouldBe` Right [[VStr "Book"], [VStr "Pen"], [VStr "Cup"]]
            scanTimes "users" scans `shouldBe` 1
            scanTimes "orders" scans `shouldBe` 1

        it "runs a non-correlated subquery in a computed column only once" $ do
            let (result, scans) = countedRun "SELECT name, (SELECT COUNT(*) FROM orders) FROM users"
            length (fromRight [] result) `shouldBe` 3
            scanTimes "orders" scans `shouldBe` 1

    describe "semi joins" $ do
        let keysDB =
                [
                    ( "left_keys"
                    , users
                        { tableName = "left_keys"
                        , tableCols = [("k", TInt)]
                        , tableRows = [[("k", value)] | value <- [VInt 1, VInt 1, VInt 2, VNull, VInt 3]]
                        }
                    )
                ,
                    ( "right_keys"
                    , users
                        { tableName = "right_keys"
                        , tableCols = [("k", TInt)]
                        , tableRows = [[("k", value)] | value <- [VInt 1, VInt 1, VNull]]
                        }
                    )
                ]
            -- \| 在键边界数据上执行查询
            sql input = fmap (map (map snd)) (rowsOf (parseStatement input >>= runStatement keysDB))
            -- \| 检查经过语义解析的优化计划
            plan input = renderPlan . optimize testDB <$> (parseStatement input >>= prepare testDB >>= translate)

        it "keeps left duplicates without multiplying right matches" $ do
            sql "SELECT k FROM left_keys WHERE k IN (SELECT k FROM right_keys)"
                `shouldBe` Right [[VInt 1], [VInt 1]]
            sql "SELECT l.k FROM left_keys l WHERE EXISTS (SELECT 1 FROM right_keys r WHERE r.k = l.k)"
                `shouldBe` Right [[VInt 1], [VInt 1]]

        it "distinguishes NOT EXISTS from NULL-aware NOT IN" $ do
            sql "SELECT l.k FROM left_keys l WHERE NOT EXISTS (SELECT 1 FROM right_keys r WHERE l.k = r.k)"
                `shouldBe` Right [[VInt 2], [VNull], [VInt 3]]
            sql "SELECT k FROM left_keys WHERE k NOT IN (SELECT k FROM right_keys)" `shouldBe` Right []
            sql "SELECT k FROM left_keys WHERE k NOT IN (SELECT k FROM right_keys WHERE k = 1)"
                `shouldBe` Right [[VInt 2], [VInt 3]]

        it "applies empty-set rules even to NULL left values" $ do
            sql "SELECT k FROM left_keys WHERE k IN (SELECT k FROM right_keys WHERE k < 0)" `shouldBe` Right []
            sql "SELECT k FROM left_keys WHERE k NOT IN (SELECT k FROM right_keys WHERE k < 0)"
                `shouldBe` Right [[VInt 1], [VInt 1], [VInt 2], [VNull], [VInt 3]]
            sql "SELECT l.k FROM left_keys l WHERE NOT EXISTS (SELECT r.k FROM right_keys r WHERE r.k = l.k AND r.k < 0)"
                `shouldBe` Right [[VInt 1], [VInt 1], [VInt 2], [VNull], [VInt 3]]

        it "plans eligible predicates as semi joins and scans the right once" $ do
            mapM_
                ( \query -> do
                    plan query `shouldSatisfy` either (const False) (isInfixOf "SemiJoin")
                    sameResultAsUnoptimized query
                )
                [ "SELECT name FROM users WHERE id IN (SELECT user_id FROM orders)"
                , "SELECT name FROM users WHERE id NOT IN (SELECT user_id FROM orders)"
                , "SELECT o.product FROM orders o WHERE EXISTS (SELECT 1 FROM users u WHERE u.id = o.user_id)"
                , "SELECT o.product FROM orders o WHERE NOT EXISTS (SELECT 1 FROM users u WHERE o.user_id = u.id)"
                ]
            let (_, scans) = countedRun "SELECT o.product FROM orders o WHERE EXISTS (SELECT 1 FROM users u WHERE u.id = o.user_id)"
            scanTimes "users" scans `shouldBe` 1

        it "keeps complex correlation and risky left keys on the original path" $ do
            mapM_
                (\query -> plan query `shouldSatisfy` either (const False) (not . isInfixOf "SemiJoin"))
                [ "SELECT name FROM users WHERE 1 / (id - 1) IN (SELECT id FROM orders)"
                , "SELECT u.name FROM users u WHERE EXISTS (SELECT 1 / 0 FROM orders o WHERE o.user_id = u.id)"
                , "SELECT u.name FROM users u WHERE EXISTS (SELECT 1 FROM orders o WHERE o.user_id > u.id)"
                , "SELECT u.name FROM users u WHERE EXISTS (SELECT COUNT(*) FROM orders o WHERE o.user_id = u.id)"
                ]
            sameResultAsUnoptimized "SELECT u.name FROM users u WHERE EXISTS (SELECT COUNT(*) FROM orders o WHERE o.user_id = u.id)"

        it "skips the right side for empty left input and preserves right errors" $ do
            let (empty, scans) = countedRun "SELECT d.id FROM (SELECT id FROM users WHERE id < 0) d WHERE d.id IN (SELECT 1 / 0 FROM orders)"
            empty `shouldBe` Right []
            scanTimes "orders" scans `shouldBe` 0
            fst (countedRun "SELECT id FROM users WHERE id IN (SELECT 1 / (id - 2) FROM orders)")
                `shouldBe` Left "division by zero"
            fst (countedRun "SELECT u.id FROM users u WHERE EXISTS (SELECT 1 / 0 FROM orders o WHERE o.user_id = u.id)")
                `shouldBe` Left "division by zero"

        it "retains mixed numeric comparison at the floating precision boundary" $ do
            let db =
                    [
                        ( "big"
                        , users
                            { tableName = "big"
                            , tableCols = [("id", TInt)]
                            , tableRows = [[("id", VInt value)] | value <- [9007199254740995, 9007199254740996, 9007199254740997, 9007199254740998]]
                            }
                        )
                    ,
                        ( "floats"
                        , users
                            { tableName = "floats"
                            , tableCols = [("value", plainColumn CFloat)]
                            , tableRows = [[("value", VFloat 9007199254740996)]]
                            }
                        )
                    ]
                result = do
                    statement <- parseStatement "SELECT id FROM big WHERE id IN (SELECT value FROM floats)"
                    original <- prepare db statement >>= translate
                    unoptimized <- evalRelOp db original
                    optimized <- rowsOf (runStatement db statement)
                    Right (unoptimized, optimized)
                expected = [[("id", VInt 9007199254740995)], [("id", VInt 9007199254740996)], [("id", VInt 9007199254740997)]]
            result `shouldBe` Right (expected, expected)

        it "keeps nested enclosing scopes and repeated bindings distinct" $ do
            sameResultAsUnoptimized "SELECT u.id FROM users u WHERE EXISTS (SELECT o.id FROM orders o WHERE EXISTS (SELECT x.id FROM users x WHERE x.id = u.id))"
            sameResultAsUnoptimized "SELECT u.id FROM users u WHERE EXISTS (SELECT o.id FROM orders o WHERE EXISTS (SELECT x.id FROM users x WHERE x.id = o.user_id))"

        it "shares independent IN results with other subquery consumers" $ do
            let (result, scans) = countedRun "SELECT name, (SELECT COUNT(*) FROM orders) AS n FROM users WHERE id IN (SELECT COUNT(*) FROM orders)"
            result `shouldBe` Right [[("name", VStr "Carol"), ("n", VInt 3)]]
            scanTimes "orders" scans `shouldBe` 1

        it "preserves grouping, DISTINCT, ordering and LIMIT around semi joins" $ do
            mapM_
                sameResultAsUnoptimized
                [ "SELECT DISTINCT user_id FROM orders WHERE user_id IN (SELECT id FROM users) ORDER BY user_id DESC LIMIT 1"
                , "SELECT user_id, COUNT(*) FROM orders WHERE user_id IN (SELECT id FROM users) GROUP BY user_id HAVING COUNT(*) > 1 ORDER BY COUNT(*) DESC"
                , "SELECT name FROM users WHERE id IN (SELECT user_id FROM orders ORDER BY id LIMIT 1) ORDER BY age + 1"
                ]
            case parseStatement "SELECT name FROM users WHERE id IN (SELECT user_id FROM orders)" >>= prepare testDB >>= translate of
                Left err -> expectationFailure err
                Right original -> optimize testDB (optimize testDB original) `shouldBe` optimize testDB original

        it "executes semi and anti joins with NULL through storage IPC" $
            withTestSession $ \srv -> withServerEnv srv $ do
                -- \| 在真实存储执行半连接查询
                let query input = case parseStatement input of
                        Left err -> pure (Left err)
                        Right statement -> runIPCStorage (runStatementM statement)
                mapM_
                    (\input -> query input `shouldReturn` Right [])
                    [ "CREATE TABLE semi_left (k int)"
                    , "CREATE TABLE semi_right (k int)"
                    , "INSERT INTO semi_left (k) VALUES (1), (1), (2), (NULL)"
                    , "INSERT INTO semi_right (k) VALUES (1), (1), (NULL)"
                    ]
                query "SELECT l.k FROM semi_left l WHERE EXISTS (SELECT 1 FROM semi_right r WHERE r.k = l.k)"
                    `shouldReturn` Right [[("l.k", VInt 1)], [("l.k", VInt 1)]]
                query "SELECT l.k FROM semi_left l WHERE NOT EXISTS (SELECT 1 FROM semi_right r WHERE r.k = l.k)"
                    `shouldReturn` Right [[("l.k", VInt 2)], [("l.k", VNull)]]
                query "SELECT k FROM semi_left WHERE k NOT IN (SELECT k FROM semi_right)" `shouldReturn` Right []
                query "SELECT k FROM semi_left WHERE k NOT IN (SELECT k FROM semi_right WHERE k < 0)"
                    `shouldReturn` Right [[("k", VInt 1)], [("k", VInt 1)], [("k", VInt 2)], [("k", VNull)]]

    describe "NULL membership in empty subqueries" $ do
        -- \| 在测试库运行并取值
        let values input = fmap (map (map snd)) (rowsOf (parseStatement input >>= runStatement testDB))
        it "uses the same empty-set rule for membership and quantified comparison" $ do
            inValues VNull [] `shouldBe` Right (VBool False)
            values "SELECT NULL IN (SELECT id FROM orders WHERE id < 0), NULL NOT IN (SELECT id FROM orders WHERE id < 0), NULL = ANY (SELECT id FROM orders WHERE id < 0), NULL <> ALL (SELECT id FROM orders WHERE id < 0)"
                `shouldBe` Right [[VBool False, VBool True, VBool False, VBool True]]
        it "retains unknown results for nonempty sets" $ do
            values "SELECT NULL IN (SELECT id FROM orders), NULL NOT IN (SELECT id FROM orders), NULL IN (NULL), NULL NOT IN (NULL)"
                `shouldBe` Right [[VNull, VNull, VNull, VNull]]
        it "keeps optimized filtering identical to the original plan" $ do
            mapM_
                sameResultAsUnoptimized
                [ "SELECT name FROM users WHERE NULL IN (SELECT id FROM orders WHERE id < 0)"
                , "SELECT name FROM users WHERE NULL NOT IN (SELECT id FROM orders WHERE id < 0)"
                ]

    describe "query materialization" $ do
        it "shares one subquery across predicates, ordering and output" $ do
            let query = "SELECT name, (SELECT COUNT(*) FROM orders) AS n FROM users WHERE (SELECT COUNT(*) FROM orders) > 0 ORDER BY (SELECT COUNT(*) FROM orders) + age DESC"
                (result, scans) = countedRun query
            fmap (map (map snd)) result `shouldBe` Right [[VStr "Carol", VInt 3], [VStr "Alice", VInt 3], [VStr "Bob", VInt 3]]
            scanTimes "orders" scans `shouldBe` 1
            sameResultAsUnoptimized query

        it "shares correlated results by referenced values across operators" $ do
            let query = "SELECT o.product, (SELECT COUNT(*) FROM users u WHERE u.id = o.user_id) AS n FROM orders o WHERE (SELECT COUNT(*) FROM users u WHERE u.id = o.user_id) > 0 ORDER BY o.product"
                (result, scans) = countedRun query
            fmap (map (map snd)) result `shouldBe` Right [[VStr "Book", VInt 1], [VStr "Cup", VInt 1], [VStr "Pen", VInt 1]]
            scanTimes "users" scans `shouldBe` 2
            sameResultAsUnoptimized query

        it "shares rows while preserving each subquery consumer's semantics" $ do
            let (result, scans) = countedRun "SELECT EXISTS (SELECT id FROM orders) AS found, 1 IN (SELECT id FROM orders) AS member, 0 < ALL (SELECT id FROM orders) AS positive"
            fmap (map (map snd)) result `shouldBe` Right [[VBool True, VBool True, VBool True]]
            scanTimes "orders" scans `shouldBe` 1
            fst (countedRun "SELECT EXISTS (SELECT id FROM orders), (SELECT id FROM orders)")
                `shouldBe` Left "scalar subquery returned more than one row"

        it "shares empty results without confusing NULL and existence" $ do
            let (result, scans) = countedRun "SELECT (SELECT id FROM orders WHERE id < 0) AS empty, EXISTS (SELECT id FROM orders WHERE id < 0) AS found, 1 IN (SELECT id FROM orders WHERE id < 0) AS member"
            fmap (map (map snd)) result `shouldBe` Right [[VNull, VBool False, VBool False]]
            scanTimes "orders" scans `shouldBe` 1

        it "materializes repeated CTE bodies before applying source aliases" $ do
            let query = "WITH t AS (SELECT id, name FROM users) SELECT a.name, b.name FROM t a JOIN t b ON a.id = b.id ORDER BY a.name"
                (result, scans) = countedRun query
            fmap (map (map snd)) result `shouldBe` Right [[VStr "Alice", VStr "Alice"], [VStr "Bob", VStr "Bob"], [VStr "Carol", VStr "Carol"]]
            scanTimes "users" scans `shouldBe` 1
            sameResultAsUnoptimized query

        it "materializes repeated derived bodies and preserves duplicate rows" $ do
            let query = "SELECT a.user_id, b.user_id FROM (SELECT user_id FROM orders) a JOIN (SELECT user_id FROM orders) b ON a.user_id = b.user_id"
                (result, scans) = countedRun query
            fmap (map (map snd)) result `shouldBe` Right [[VInt 1, VInt 1], [VInt 1, VInt 1], [VInt 2, VInt 2], [VInt 1, VInt 1], [VInt 1, VInt 1]]
            scanTimes "orders" scans `shouldBe` 1
            sameResultAsUnoptimized query

        it "does not materialize unused subqueries or hide execution errors" $ do
            let (result, scans) = countedRun "SELECT (SELECT 1 / 0 FROM orders) FROM users WHERE id < 0"
            result `shouldBe` Right []
            scanTimes "orders" scans `shouldBe` 0
            fst (countedRun "WITH t AS (SELECT 1 / 0 AS bad FROM users) SELECT a.bad FROM t a JOIN t b ON a.bad = b.bad")
                `shouldBe` Left "division by zero"

        it "clears materialization between executions of the same bound query" $ do
            case parseStatement "SELECT (SELECT COUNT(*) FROM orders) AS n" >>= Bound.bindQuery testDB of
                Left err -> expectationFailure err
                Right bound -> do
                    -- \| 两次执行之间修改来源数据
                    let action = do
                            first <- Bound.executeQueryM bound
                            changed <- replaceAll "orders" []
                            second <- Bound.executeQueryM bound
                            pure (first, changed, second)
                    case runCounting action testDB of
                        Left err -> expectationFailure err
                        Right ((first, changed, second), _, scans) -> do
                            first `shouldBe` Right [[("n", VInt 3)]]
                            changed `shouldBe` Right ()
                            second `shouldBe` Right [[("n", VInt 0)]]
                            scanTimes "orders" scans `shouldBe` 2

        it "keeps enclosing rows distinct for raw name-based subquery calls" $ do
            let statement = Select [("value", Col "id")] FromUnit Nothing [] Nothing [] Nothing
                plan = Compute [("value", ScalarSub (Subquery statement ["id"]))] (Scan Nothing "users" Nothing)
            evalRelOp testDB plan `shouldBe` Right [[("value", VInt 1)], [("value", VInt 2)], [("value", VInt 3)]]

        it "executes repeated CTE references through real storage" $
            withTestSession $ \srv -> withServerEnv srv $ do
                -- \| 在实际存储执行数据和查询语句
                let query input = case parseStatement input of
                        Left err -> pure (Left err)
                        Right statement -> runIPCStorage (runStatementM statement)
                query "CREATE TABLE materialized (id int, amount int)" `shouldReturn` Right []
                query "INSERT INTO materialized (id, amount) VALUES (1, 10), (2, 20)" `shouldReturn` Right []
                query "WITH t AS (SELECT id, amount FROM materialized) SELECT a.id, a.amount + b.amount AS total FROM t a JOIN t b ON a.id = b.id ORDER BY a.id"
                    `shouldReturn` Right [[("a.id", VInt 1), ("total", VInt 20)], [("a.id", VInt 2), ("total", VInt 40)]]

    describe "ChuSQL.Core.Engine (聚合与 GROUP BY)" $ do
        -- sql：在默认库上跑 SQL 取结果行
        let sql input = parseStatement input >>= rowsOf . runStatement testDB
            -- sqlDb：在指定库上跑 SQL
            sqlDb db input = parseStatement input >>= rowsOf . runStatement db
            -- vals：只取结果里的值
            vals input = fmap (map (map snd)) (sql input)

        it "counts rows without GROUP BY" $ do
            vals "SELECT COUNT(*) FROM users" `shouldBe` Right [[VInt 3]]

        it "counts only the non-null values of a column" $ do
            let db = [(n, if n == "users" then t{tableRows = [[("id", VInt 1), ("name", VStr "a"), ("age", VNull)]]} else t) | (n, t) <- testDB]
            fmap (map (map snd)) (sqlDb db "SELECT COUNT(age), COUNT(*) FROM users")
                `shouldBe` Right [[VInt 0, VInt 1]]

        it "sums, averages and takes extremes" $ do
            vals "SELECT SUM(age), AVG(age), MIN(age), MAX(age) FROM users"
                `shouldBe` Right [[VInt 72, VFloat 24, VInt 17, VInt 30]]

        it "ignores nulls in the aggregates that skip them" $ do
            let db = [(n, if n == "users" then t{tableRows = [[("id", VInt 1), ("name", VStr "a"), ("age", VNull)], [("id", VInt 2), ("name", VStr "b"), ("age", VInt 4)]]} else t) | (n, t) <- testDB]
            fmap (map (map snd)) (sqlDb db "SELECT SUM(age), AVG(age), MIN(age), MAX(age) FROM users")
                `shouldBe` Right [[VInt 4, VFloat 4, VInt 4, VInt 4]]

        it "groups by a column and counts each group" $ do
            vals "SELECT user_id, COUNT(*) FROM orders GROUP BY user_id ORDER BY user_id"
                `shouldBe` Right [[VInt 1, VInt 2], [VInt 2, VInt 1]]

        it "keeps the group key in the output row" $ do
            fmap (map (map fst)) (sql "SELECT age, COUNT(*) FROM users GROUP BY age ORDER BY age")
                `shouldBe` Right [["age", "COUNT(*)"], ["age", "COUNT(*)"], ["age", "COUNT(*)"]]

        it "computes expressions on top of aggregate results" $ do
            vals "SELECT SUM(age) / 2 FROM users" `shouldBe` Right [[VInt 36]]

        it "uses the same aggregate only once" $ do
            case parseStatement "SELECT SUM(age), SUM(age) + 1 FROM users" of
                Left err -> expectationFailure err
                Right q -> case translate =<< prepare testDB q of
                    Left err -> expectationFailure err
                    Right plan -> case plan of
                        Compute _ (Aggregate _ aggs _) -> length aggs `shouldBe` 1
                        other -> expectationFailure ("unexpected plan: " ++ renderPlan other)

        it "returns one row with null sum for an empty table" $ do
            let empty = [(n, if n == "users" then t{tableRows = []} else t) | (n, t) <- testDB]
            fmap (map (map snd)) (sqlDb empty "SELECT COUNT(*), SUM(age) FROM users")
                `shouldBe` Right [[VInt 0, VNull]]

        it "returns no groups for an empty table when grouping" $ do
            let empty = [(n, if n == "users" then t{tableRows = []} else t) | (n, t) <- testDB]
            sqlDb empty "SELECT age, COUNT(*) FROM users GROUP BY age" `shouldBe` Right []

        it "rejects a bare column that is not grouped" $ do
            sql "SELECT name, COUNT(*) FROM users GROUP BY age"
                `shouldSatisfy` either (isInfixOf "must appear in GROUP BY") (const False)

        it "rejects an aggregate in WHERE" $ do
            sql "SELECT name FROM users WHERE COUNT(*) > 1"
                `shouldSatisfy` either (isInfixOf "aggregate functions are not allowed here") (const False)

        it "rejects nested aggregates" $ do
            sql "SELECT SUM(COUNT(*)) FROM users"
                `shouldSatisfy` either (isInfixOf "nested aggregates") (const False)

        it "rejects SUM on a text column" $ do
            sql "SELECT SUM(name) FROM users"
                `shouldSatisfy` either (isInfixOf "needs a number") (const False)

        it "rejects an unknown GROUP BY column" $ do
            sql "SELECT age, COUNT(*) FROM users GROUP BY nope"
                `shouldSatisfy` either (isInfixOf "unknown column") (const False)

        it "parses GROUP BY and the aggregate calls" $ do
            case parseStatement "SELECT age, COUNT(*), SUM(age) FROM users GROUP BY age" of
                Left err -> expectationFailure err
                Right q -> do
                    selectGroupBy q `shouldBe` ["age"]
                    selectItems q
                        `shouldBe` [ ("age", Col "age")
                                   , ("COUNT(*)", CountAll)
                                   , ("SUM(age)", SumOf (Col "age"))
                                   ]

        it "keeps grouped results identical after optimization" $ do
            let query = "SELECT user_id, COUNT(*) FROM orders GROUP BY user_id ORDER BY user_id"
            case parseStatement query >>= prepare testDB >>= translate of
                Left err -> expectationFailure err
                Right plan -> do
                    evalRelOp testDB plan `shouldBe` Right [[("user_id", VInt 1), ("COUNT(*)", VInt 2)], [("user_id", VInt 2), ("COUNT(*)", VInt 1)]]
                    evalRelOp testDB (optimize testDB plan) `shouldBe` evalRelOp testDB plan

    describe "HAVING and expression ordering" $ do
        -- \| 在固定测试库执行查询
        let sql text = rowsOf (parseStatement text >>= runStatement testDB)
        it "filters groups using aggregates absent from the projection" $ do
            sql "SELECT user_id FROM orders GROUP BY user_id HAVING COUNT(*) > 1 ORDER BY SUM(id) DESC"
                `shouldBe` Right [[("user_id", VInt 1)]]

        it "creates one implicit group, including an empty input" $ do
            sql "SELECT 7 AS n FROM users HAVING COUNT(*) = 3" `shouldBe` Right [[("n", VInt 7)]]
            sql "SELECT COUNT(*) AS n FROM users WHERE id < 0 HAVING COUNT(*) = 0"
                `shouldBe` Right [[("n", VInt 0)]]
            sql "SELECT 7 AS n FROM users WHERE id < 0 HAVING TRUE" `shouldBe` Right [[("n", VInt 7)]]
            sql "SELECT COUNT(*) FROM users HAVING NULL" `shouldBe` Right []
            sql "SELECT COUNT(*) FROM users HAVING FALSE" `shouldBe` Right []

        it "checks grouping, boolean predicates and nested aggregates" $ do
            mapM_
                (\query -> sql query `shouldSatisfy` isLeft)
                [ "SELECT name FROM users HAVING TRUE"
                , "SELECT COUNT(*) FROM users HAVING age > 20"
                , "SELECT COUNT(*) FROM users HAVING 1"
                , "SELECT COUNT(*) FROM users HAVING SUM(COUNT(*)) > 0"
                , "SELECT name FROM users ORDER BY COUNT(*)"
                , "SELECT COUNT(*) AS n FROM users HAVING n > 0"
                ]

        it "rewrites aggregate calls inside IN predicates" $ do
            sql "SELECT user_id FROM orders GROUP BY user_id HAVING COUNT(*) IN (1, 2) ORDER BY COUNT(*) DESC"
                `shouldBe` Right [[("user_id", VInt 1)], [("user_id", VInt 2)]]
            sql "SELECT COUNT(*) AS n FROM users HAVING 3 IN (COUNT(*), SUM(age))"
                `shouldBe` Right [[("n", VInt 3)]]

        it "combines expressions, aliases and output positions" $ do
            sql "SELECT name AS n, age FROM users ORDER BY age - 25, 1 DESC LIMIT 2"
                `shouldBe` Right [[("n", VStr "Bob"), ("age", VInt 17)], [("n", VStr "Alice"), ("age", VInt 25)]]
            sql "SELECT name FROM users ORDER BY length(name), name DESC"
                `shouldBe` Right [[("name", VStr "Bob")], [("name", VStr "Carol")], [("name", VStr "Alice")]]
            sql "SELECT name AS n FROM users ORDER BY n + 1" `shouldSatisfy` isLeft

        it "keeps projection errors deferred until after LIMIT" $ do
            sql "SELECT 1 / (age - 17) AS n FROM users ORDER BY age + 0 DESC LIMIT 1"
                `shouldSatisfy` either (const False) ((== 1) . length)
            sql "SELECT name FROM users ORDER BY 1 / (age - 17) LIMIT 0"
                `shouldBe` Left "division by zero"

        it "sorts DISTINCT only by expressions present in its output" $ do
            sql "SELECT DISTINCT age + 1 AS n FROM users ORDER BY age + 1 DESC LIMIT 1"
                `shouldBe` Right [[("n", VInt 31)]]
            sql "SELECT DISTINCT name FROM users ORDER BY age + 1" `shouldSatisfy` isLeft

        it "expands CTEs inside HAVING and ORDER BY subqueries" $ do
            sql "WITH x AS (SELECT id FROM users) SELECT name FROM users ORDER BY (SELECT MAX(id) FROM x) - age"
                `shouldBe` Right [[("name", VStr "Carol")], [("name", VStr "Alice")], [("name", VStr "Bob")]]
            sql "WITH x AS (SELECT id FROM users) SELECT COUNT(*) AS n FROM users HAVING COUNT(*) = (SELECT COUNT(*) FROM x)"
                `shouldBe` Right [[("n", VInt 3)]]

        it "binds correlated HAVING and ordering to their enclosing rows" $ do
            sql "SELECT u.id FROM users u WHERE EXISTS (SELECT COUNT(*) FROM orders o HAVING COUNT(*) >= u.id) ORDER BY u.id"
                `shouldBe` Right [[("u.id", VInt 1)], [("u.id", VInt 2)], [("u.id", VInt 3)]]
            sql "SELECT u.name FROM users u ORDER BY (SELECT COUNT(*) FROM orders o WHERE o.user_id = u.id) DESC, u.name"
                `shouldBe` Right [[("u.name", VStr "Alice")], [("u.name", VStr "Bob")], [("u.name", VStr "Carol")]]

        it "preserves semantics through optimizer rewrites" $ do
            mapM_
                sameResultAsUnoptimized
                [ "SELECT user_id FROM orders GROUP BY user_id HAVING COUNT(*) > 1 ORDER BY SUM(id) + 1 LIMIT 1"
                , "SELECT name FROM users ORDER BY age + 1 DESC, name LIMIT 2"
                , "SELECT DISTINCT age + 1 FROM users ORDER BY age + 1"
                ]

        it "executes grouped HAVING and expression sorting through storage IPC" $
            withTestSession $ \srv -> withServerEnv srv $ do
                -- \| 通过真实存储执行 SQL
                let query text = case parseStatement text of
                        Left err -> pure (Left err)
                        Right statement -> runIPCStorage (runStatementM statement)
                query "CREATE TABLE grouped (agg1 int, amount int)" `shouldReturn` Right []
                query "INSERT INTO grouped (agg1, amount) VALUES (1, 10), (1, 20), (2, 5)" `shouldReturn` Right []
                query "SELECT agg1, SUM(amount) AS total FROM grouped GROUP BY agg1 HAVING SUM(amount) > 5 ORDER BY SUM(amount) + agg1 DESC LIMIT 1"
                    `shouldReturn` Right [[("agg1", VInt 1), ("total", VInt 30)]]

    describe "ChuSQL end-to-end" $ do
        it "parses and executes a complex query" $ do
            rowsOf
                ( parseStatement "SELECT name FROM users WHERE age < 18 OR age > 28"
                    >>= runStatement testDB
                )
                `shouldBe` Right
                    [ [("name", VStr "Bob")]
                    , [("name", VStr "Carol")]
                    ]

        it "WHERE ORDER BY LIMIT end-to-end" $ do
            rowsOf (parseStatement "SELECT name FROM users WHERE age > 15 ORDER BY age DESC LIMIT 1" >>= runStatement testDB)
                `shouldBe` Right
                    [ [("name", VStr "Carol")]
                    ]

    describe "ChuSQL.Core.Engine.Algebra.Cost (直方图)" $ do
        it "builds a 16-bucket histogram from the numeric values" $ do
            let hist = histogramOf [VInt n | n <- [0 .. 99]]
            fmap histLow hist `shouldBe` Just 0
            fmap histHigh hist `shouldBe` Just 99
            fmap (length . histBuckets) hist `shouldBe` Just 16
            fmap (sum . histBuckets) hist `shouldBe` Just 100
            -- 非数值与 NULL 不参与；只有一个值都落第 0 桶
            histogramOf [VStr "a", VNull] `shouldBe` Nothing
            histogramOf [VInt 5, VNull, VStr "a"] `shouldBe` Just (Histogram 5 5 (1 : replicate 15 0))

        it "estimates a range from the histogram buckets" $ do
            let hist = Histogram 0 1600 (replicate 16 100)
                meta = TableMeta 1600 [] [("id", hist)] ["id"]
                plain = TableMeta 1600 [] [] []
            rangeRows meta "id" (Just (VInt 0, True)) (Just (VInt 100, True)) `shouldBe` 100
            rangeRows meta "id" Nothing Nothing `shouldBe` 1600
            selectivityOf meta "id" (Just (VInt 0, True)) (Just (VInt 100, True)) `shouldBe` Just 0.0625
            -- 没有直方图退回四分之一
            rangeRows plain "id" Nothing Nothing `shouldBe` 400
            selectivityOf plain "id" Nothing Nothing `shouldBe` Nothing

        it "uses the histogram to reject a range that would scan most rows" $ do
            let buckets = replicate 8 63 ++ replicate 8 62
                withHist = TableMeta 1000 [("id", 1000, False)] [("id", Histogram 0 1600 buckets)] ["id"]
                plain = TableMeta 1000 [("id", 1000, False)] [] ["id"]
                dbWith m = [("users", Table "users" [("id", TInt), ("name", TStr)] [] (Just m) Nothing Nothing)]
                narrow = Filter (And (GtE (Col "id") (LitInt 10)) (LtE (Col "id") (LitInt 20))) (Scan Nothing "users" Nothing)
                wide = Filter (Gt (Col "id") (LitInt 0)) (Scan Nothing "users" Nothing)
            -- 直方图说这个窄范围只命中十几行：改写
            optimize (dbWith withHist) narrow
                `shouldBe` Range Nothing "users" "id" (Just (VInt 10, True)) (Just (VInt 20, True))
            -- 直方图说这条范围覆盖全表：索引不比全表扫便宜，不改写
            optimize (dbWith withHist) wide `shouldBe` wide
            -- 没有直方图按四分之一算，仍然改写
            optimize (dbWith plain) wide
                `shouldBe` Range Nothing "users" "id" (Just (VInt 0, False)) Nothing

    describe "ChuSQL.Core.Engine.Algebra.Optimize" $ do
        it "keeps single-table WHERE results identical" $ do
            sameResultAsUnoptimized "SELECT name FROM users WHERE age > 18"

        it "keeps JOIN with WHERE results identical" $ do
            sameResultAsUnoptimized "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 20"

        it "keeps a JOIN without an equi key (nested loop path) results identical" $ do
            sameResultAsUnoptimized "SELECT u.name, o.product FROM users u JOIN orders o ON u.id > o.user_id"

        it "keeps JOIN with WHERE, ORDER BY and LIMIT results identical" $ do
            sameResultAsUnoptimized "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 15 ORDER BY o.product DESC LIMIT 2"

        it "pushes a single-side predicate into the left side of a JOIN" $ do
            case parseStatement "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 20" of
                Left err -> expectationFailure err
                Right q ->
                    case translate q of
                        Left err -> expectationFailure err
                        Right relOp ->
                            filterPushedIntoLeft (optimize testDB relOp) `shouldBe` True

        it "removes a redundant SELECT * projection" $ do
            case parseStatement "SELECT * FROM users" of
                Left err -> expectationFailure err
                Right q ->
                    case translate q of
                        Left err -> expectationFailure err
                        Right relOp ->
                            optimize testDB relOp `shouldBe` Scan Nothing "users" Nothing

        it "does not push a filter through LIMIT" $ do
            let cond = Gt (Col "age") (LitInt 18)
                relOp = Filter cond (Limit 2 (Scan Nothing "users" Nothing))
            optimize testDB relOp `shouldBe` relOp

        it "projects derived outputs without dropping expression errors" $ do
            let original = Project ["d.name"] (Derived (Just "d") (Compute [("name", Col "name"), ("bad", Div (LitInt 1) (LitInt 0))] (Scan Nothing "users" Nothing)))
            evalRelOp testDB (optimize testDB original) `shouldBe` evalRelOp testDB original
            evalRelOp testDB original `shouldBe` Left "division by zero"
            sameResultAsUnoptimized "SELECT d.name FROM (SELECT name, age FROM users) d WHERE d.age > 20 ORDER BY d.name"

        it "supplies memory schema statistics without exposing rows" $ do
            case runMemoryStorage schema testDB of
                Left err -> expectationFailure err
                Right (Left err, _) -> expectationFailure err
                Right (Right structures, _) -> do
                    map (tableRows . snd) structures `shouldBe` [[], []]
                    fmap (fmap metaRowCount . tableMeta) (lookup "users" structures) `shouldBe` Just (Just 3)

        it "uses derived schema estimates for safe join reordering" $ do
            let query = "SELECT d.name FROM (SELECT id, name FROM a) d JOIN b ON d.id = b.a_id JOIN c ON b.id = c.b_id ORDER BY d.name"
            case parseStatement query >>= prepare chainDB >>= translate of
                Left err -> expectationFailure err
                Right original -> do
                    let optimized = optimize chainDB original
                    planTables optimized `shouldBe` ["b", "c", "a"]
                    evalRelOp chainDB optimized `shouldBe` evalRelOp chainDB original

        it "is idempotent after reaching a fixed point" $ do
            case parseStatement "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 20" of
                Left err -> expectationFailure err
                Right q ->
                    case translate q of
                        Left err -> expectationFailure err
                        Right relOp ->
                            optimize testDB (optimize testDB relOp) `shouldBe` optimize testDB relOp

        it "folds a constant-false predicate into LitBool False" $ do
            fmap firstFilterCond (optimizedPlan "SELECT name FROM users WHERE 1 > 2")
                `shouldBe` Right (Just (LitBool False))

        it "picks the index only when the statistics make it cheaper" $ do
            let stat rows distinct =
                    TableMeta rows [(c, d, False) | (c, d) <- distinct] []
                dbWith m = [("users", Table "users" [("id", TInt), ("name", TStr)] [] (Just m) Nothing Nothing)]
                unknown = [("users", Table "users" [("id", TInt), ("name", TStr)] [] Nothing Nothing Nothing)]
                point = Filter (Eq (Col "id") (LitInt 1)) (Scan Nothing "users" Nothing)
                range = Filter (Gt (Col "id") (LitInt 5)) (Scan Nothing "users" Nothing)
            -- 索引在、点查命中一行：改写
            optimize (dbWith (stat 10000 [("id", 10000)] ["id"])) point
                `shouldBe` Lookup Nothing "users" "id" (VInt 1)
            -- 拿不到统计：保持原有改写行为
            optimize unknown point `shouldBe` Lookup Nothing "users" "id" (VInt 1)
            -- 没有索引：不改写，省掉一次白跑的存储层查询
            optimize (dbWith (stat 10000 [("id", 10000)] [])) point `shouldBe` point
            -- 有索引但整列一个值：点查等于全表，全表扫更便宜
            optimize (dbWith (stat 10000 [("name", 1)] ["name"])) (Filter (Eq (Col "name") (LitStr "a")) (Scan Nothing "users" Nothing))
                `shouldBe` Filter (Eq (Col "name") (LitStr "a")) (Scan Nothing "users" Nothing)
            -- 范围扫描同理
            optimize (dbWith (stat 10000 [("id", 10000)] ["id"])) range
                `shouldBe` Range Nothing "users" "id" (Just (VInt 5, False)) Nothing
            optimize (dbWith (stat 10000 [("id", 10000)] [])) range `shouldBe` range

        it "reorders an inner join chain only when the statistics say it is cheaper" $ do
            let sql =
                    "SELECT a.name FROM a JOIN b ON a.id = b.a_id JOIN c ON b.id = c.b_id ORDER BY a.name"
                unordered = "SELECT a.name FROM a JOIN b ON a.id = b.a_id JOIN c ON b.id = c.b_id"
            -- 统计说先接最小的 c 更省：文本顺序 a / b / c 换成一个更便宜的顺序
            fmap planTables (optimizedPlanIn chainDB sql) `shouldBe` Right ["b", "c", "a"]
            -- 拿不到统计：保持文本顺序
            fmap planTables (optimizedPlanIn chainDBWithoutStats sql) `shouldBe` Right ["a", "b", "c"]
            -- 行序可观察（没有 ORDER BY）：不动顺序
            fmap planTables (optimizedPlanIn chainDB unordered) `shouldBe` Right ["a", "b", "c"]

        it "keeps an inner join chain result identical when it reorders it" $ do
            let sql =
                    "SELECT a.name FROM a JOIN b ON a.id = b.a_id JOIN c ON b.id = c.b_id ORDER BY a.name"
            case parseStatement sql >>= prepare chainDB of
                Left err -> expectationFailure err
                Right q -> case translate q of
                    Left err -> expectationFailure err
                    Right relOp -> evalRelOp chainDB (optimize chainDB relOp) `shouldBe` evalRelOp chainDB relOp

        it "removes a filter whose predicate folds to true" $ do
            fmap anyFilter (optimizedPlan "SELECT name FROM users WHERE 1 = 1")
                `shouldBe` Right False

        it "removes a constant-true filter sitting above a join" $ do
            fmap anyFilter (optimizedPlan "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE 1 = 1")
                `shouldBe` Right False

        it "leaves an expression predicate as a filter" $ do
            fmap firstFilterCond (optimizedPlan "SELECT name FROM users WHERE age + 1 > 18")
                `shouldBe` Right (Just (Gt (Add (Col "age") (LitInt 1)) (LitInt 18)))

        it "folds the constant part of a predicate that cannot fold as a whole" $ do
            fmap firstFilterCond (optimizedPlan "SELECT name FROM users WHERE age > 18 AND 1 = 1")
                `shouldBe` Right (Just (And (Gt (Col "age") (LitInt 18)) (LitBool True)))

        it "keeps a constant predicate that fails to evaluate" $ do
            fmap firstFilterCond (optimizedPlan "SELECT name FROM users WHERE 'abc' > 1")
                `shouldBe` Right (Just (Gt (LitStr "abc") (LitInt 1)))

        it "returns no rows for a constant-false predicate" $ do
            rowsOf (parseStatement "SELECT name FROM users WHERE 1 > 2" >>= runStatement testDB)
                `shouldBe` Right []

        it "returns every row for a constant-true predicate" $ do
            rowsOf (parseStatement "SELECT name FROM users WHERE 1 = 1" >>= runStatement testDB)
                `shouldBe` Right
                    [ [("name", VStr "Alice")]
                    , [("name", VStr "Bob")]
                    , [("name", VStr "Carol")]
                    ]

        it "keeps both sides of a join" $ do
            let plan =
                    Project
                        ["u.name"]
                        ( Join
                            InnerJoin
                            (Scan (Just "u") "users" Nothing)
                            (Scan (Just "o") "orders" Nothing)
                            (Eq (Col "u.id") (Col "o.user_id"))
                        )
            stripProjects (pushProject testDB ["*"] plan) `shouldBe` stripProjects plan

        it "keeps nested projections in place" $ do
            let plan = Project ["name"] (Project ["name", "age"] (Scan Nothing "users" Nothing))
            stripProjects (pushProject testDB ["*"] plan) `shouldBe` stripProjects plan

        it "keeps the whole spine of a full pipeline" $ do
            let plan =
                    Project
                        ["name"]
                        ( Limit
                            2
                            ( Sort
                                [("age", Desc)]
                                (Filter (Gt (Col "age") (LitInt 18)) (Scan Nothing "users" Nothing))
                            )
                        )
            stripProjects (pushProject testDB ["*"] plan) `shouldBe` stripProjects plan

        it "merges two predicates pushed into the same side into one filter" $ do
            fmap leftSideFilters (optimizedPlan "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 20 AND u.name = 'Alice'")
                `shouldBe` Right (Just 1)

        it "pushes projection into a single-table scan as one layer" $ do
            optimizedPlan "SELECT name FROM users"
                `shouldBe` Right (Project ["name"] (Scan Nothing "users" (Just ["name"])))

        it "carries required columns in the scan plan" $ do
            fmap renderPlan (optimizedPlan "SELECT name FROM users")
                `shouldBe` Right "Project [\"name\"]\n  Scan Nothing \"users\" columns=[\"name\"]"

        it "preserves row count when an expression needs no columns" $ do
            sameResultAsUnoptimized "SELECT 1 FROM users"

        it "preserves duplicate projection labels and ordering dependencies" $ do
            sameResultAsUnoptimized "SELECT name, name FROM users ORDER BY age DESC LIMIT 2"

        it "does not prune anything when every column is taken" $ do
            fmap joinSideCols (projectedPlan "SELECT * FROM users u JOIN orders o ON u.id = o.user_id")
                `shouldBe` Right (Nothing, Nothing)

        it "activates projection pushdown inside optimize" $ do
            fmap joinSideCols (optimizedPlan "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id")
                `shouldBe` Right (Just ["u.name", "u.id"], Just ["o.user_id"])

        it "keeps projection with ORDER BY and LIMIT results identical" $ do
            sameResultAsUnoptimized "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id ORDER BY o.product DESC LIMIT 2"

        it "returns projected columns in the order the SELECT list asks for" $ do
            rowsOf (parseStatement "SELECT age, name FROM users WHERE id = 1" >>= runStatement testDB)
                `shouldBe` Right [[("age", VInt 25), ("name", VStr "Alice")]]
            rowsOf (parseStatement "SELECT u.age, u.name FROM users u WHERE u.id = 1" >>= runStatement testDB)
                `shouldBe` Right [[("u.age", VInt 25), ("u.name", VStr "Alice")]]
            rowsOf (parseStatement "SELECT * FROM orders WHERE id = 1" >>= runStatement testDB)
                `shouldBe` Right [[("id", VInt 1), ("user_id", VInt 1), ("product", VStr "Book")]]
        it "rewrites Filter id = k into Lookup" $ do
            fmap planRoot (optimizedPlan "SELECT name FROM users WHERE id = 2")
                `shouldBe` Right (Lookup Nothing "users" "id" (VInt 2))
        it "keeps Lookup result identical to unoptimized" $ do
            sameResultAsUnoptimized "SELECT name FROM users WHERE id = 2"

        it "rewrites an equality on any column into Lookup" $ do
            fmap planRoot (optimizedPlan "SELECT name FROM users WHERE age = 25")
                `shouldBe` Right (Lookup Nothing "users" "age" (VInt 25))

        it "rewrites a string equality into Lookup too" $ do
            fmap planRoot (optimizedPlan "SELECT name FROM users WHERE name = 'Alice'")
                `shouldBe` Right (Lookup Nothing "users" "name" (VStr "Alice"))
            sameResultAsUnoptimized "SELECT name FROM users WHERE name = 'Alice'"

        it "rewrites a one-sided range into Range" $ do
            fmap planRoot (optimizedPlan "SELECT name FROM users WHERE age > 20")
                `shouldBe` Right (Range Nothing "users" "age" (Just (VInt 20, False)) Nothing)
            fmap planRoot (optimizedPlan "SELECT name FROM users WHERE age < 20")
                `shouldBe` Right (Range Nothing "users" "age" Nothing (Just (VInt 20, False)))
            sameResultAsUnoptimized "SELECT name FROM users WHERE age > 20"

        it "rewrites a two-sided range into Range" $ do
            fmap planRoot (optimizedPlan "SELECT name FROM users WHERE age > 20 AND age < 30")
                `shouldBe` Right (Range Nothing "users" "age" (Just (VInt 20, False)) (Just (VInt 30, False)))
            sameResultAsUnoptimized "SELECT name FROM users WHERE age > 20 AND age < 30"

        it "keeps a range on two different columns as a filter" $ do
            fmap anyFilter (optimizedPlan "SELECT name FROM users WHERE age > 20 AND id < 3")
                `shouldBe` Right True

        it "returns the row for an aliased point lookup" $ do
            rowsOf (parseStatement "SELECT u.name FROM users u WHERE u.id = 1" >>= runStatement testDB)
                `shouldBe` Right [[("u.name", VStr "Alice")]]

    describe "ChuSQL.Core.Engine.Semantic" $ do
        it "rejects an unknown column in WHERE" $ do
            (parseStatement "SELECT name FROM users WHERE nope > 18" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in WHERE even when the table is empty" $ do
            (parseStatement "SELECT name FROM users WHERE nope > 18" >>= runStatement emptyDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in an ON condition" $ do
            (parseStatement "SELECT u.name FROM users u JOIN orders o ON u.nope = o.user_id" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects a type mismatch inside a comparison" $ do
            (parseStatement "SELECT name FROM users WHERE name > 18" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects a type mismatch even when the table is empty" $ do
            (parseStatement "SELECT name FROM users WHERE name > 18" >>= runStatement emptyDB)
                `shouldSatisfy` isLeft

        it "rejects a WHERE clause that is not a condition" $ do
            (parseStatement "SELECT name FROM users WHERE age" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects a non-boolean WHERE even when the table is empty" $ do
            (parseStatement "SELECT name FROM users WHERE age" >>= runStatement emptyDB)
                `shouldSatisfy` isLeft

        it "rejects a value whose type does not match the column" $ do
            (parseStatement "INSERT INTO users (name, age) VALUES ('Dave', 'abc')" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in UPDATE SET" $ do
            (parseStatement "UPDATE users SET nope = 1" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in DELETE WHERE" $ do
            (parseStatement "DELETE FROM users WHERE nope > 1" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "still accepts a valid query" $ do
            rowsOf (parseStatement "SELECT name FROM users WHERE age > 18" >>= runStatement testDB)
                `shouldBe` Right
                    [ [("name", VStr "Alice")]
                    , [("name", VStr "Carol")]
                    ]

        it "rejects an UPDATE whose value has the wrong type" $ do
            (parseStatement "UPDATE users SET age = 'abc'" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects a type mismatch in an ON condition" $ do
            (parseStatement "SELECT u.name FROM users u JOIN orders o ON u.name = o.user_id" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in DELETE WHERE even when the table is empty" $ do
            (parseStatement "DELETE FROM users WHERE nope > 1" >>= runStatement emptyDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in UPDATE SET even when the table is empty" $ do
            (parseStatement "UPDATE users SET nope = 1" >>= runStatement emptyDB)
                `shouldSatisfy` isLeft

        it "currently allows an INSERT that leaves a column out (known gap)" $ do
            (parseStatement "INSERT INTO users (name) VALUES ('Eve')" >>= runStatement testDB)
                `shouldSatisfy` isRight

        it "says which clause is wrong and which columns are available" $ do
            case parseStatement "SELECT name FROM users WHERE nope > 18" >>= runStatement testDB of
                Right _ -> expectationFailure "expected a Left"
                Left err -> do
                    err `shouldContain` "unknown column in WHERE"
                    err `shouldContain` "available: id, name, age"

        it "names INSERT when a target column does not exist" $ do
            case parseStatement "INSERT INTO users (nickname) VALUES (1)" >>= runStatement testDB of
                Right _ -> expectationFailure "expected a Left"
                Left err -> err `shouldContain` "unknown column in INSERT"

        it "names ORDER BY when a sort key does not exist" $ do
            case parseStatement "SELECT name FROM users ORDER BY nope" >>= runStatement testDB of
                Right _ -> expectationFailure "expected a Left"
                Left err -> err `shouldContain` "unknown column in ORDER BY"

        it "names ON and both types when a join condition mismatches" $ do
            case parseStatement "SELECT u.name FROM users u JOIN orders o ON u.name = o.user_id" >>= runStatement testDB of
                Right _ -> expectationFailure "expected a Left"
                Left err -> do
                    err `shouldContain` "ON: both sides of ="
                    err `shouldContain` "TStr and TInt"

        it "names the column and the expected type when an assignment has the wrong type" $ do
            case parseStatement "UPDATE users SET age = 'abc'" >>= runStatement testDB of
                Right _ -> expectationFailure "expected a Left"
                Left err -> err `shouldContain` "UPDATE: column age needs TInt, got TStr"

        it "accepts SELECT * across a join (the sentinel is not a column)" $ do
            (parseStatement "SELECT * FROM users u JOIN orders o ON u.id = o.user_id" >>= runStatement testDB)
                `shouldSatisfy` isRight

    describe "ChuSQL.Core.Engine.Algebra.Op" $ do
        it "renders a plan as indented text" $ do
            case parseStatement "SELECT name FROM users WHERE age > 18" >>= translate of
                Left err -> expectationFailure err
                Right plan ->
                    renderPlan plan
                        `shouldBe` "Project [\"name\"]\n  Filter Gt (Col \"age\") (LitInt 18)\n    Scan Nothing \"users\""

        it "renders a join plan with indentation" $ do
            case parseStatement "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id" >>= translate of
                Left err -> expectationFailure err
                Right plan ->
                    lines (renderPlan plan)
                        `shouldBe` [ "Project [\"u.name\"]"
                                   , "  Join on Eq (Col \"u.id\") (Col \"o.user_id\")"
                                   , "    Scan Just \"u\" \"users\""
                                   , "    Scan Just \"o\" \"orders\""
                                   ]

    describe "ChuSQL.Core.Engine.Storage.IPC" $ do
        it "uses UTF-8 config paths regardless of the foreign string encoding" $ do
            tmp <- getTemporaryDirectory
            stamp <- uniqueStamp
            let dir = tmp </> ("chusql-utf8-" ++ stamp)
                config = dir </> "settings-é.toml"
            createDirectoryIfMissing True dir
            flip finally (cleanSessionDir dir) $ do
                original <- writeSessionConfig dir
                renameFile original config
                encoding <- getForeignEncoding
                bracket_ (setForeignEncoding latin1) (setForeignEncoding encoding) $ do
                    opened <- openStorage (Just config)
                    case opened of
                        Left err -> expectationFailure err
                        Right handle -> closeStorage handle
                    maintainStorage config (BSC.pack "{\"mode\":\"unknown\"}") `shouldReturn` Left "invalid maintenance mode"

        it "preserves each worker's storage open error" $ do
            tmp <- getTemporaryDirectory
            stamp <- uniqueStamp
            pool <- workerPool 4
            let paths = [tmp </> ("missing-ffi-" ++ stamp ++ "-" ++ show n) | n <- [1 .. 16 :: Int]]
                -- \| 检查打开失败并归还意外成功的句柄
                checkOpen path = do
                    opened <- openStorage (Just path)
                    case opened of
                        Left err -> do
                            err `shouldContain` "config file not found"
                            err `shouldContain` path
                        Right handle -> closeStorage handle >> expectationFailure "expected missing config error"
            void (poolRun pool (map checkOpen paths))

        it "returns FFI input errors from workers and remains usable" $ do
            withTestSession $ \srv -> do
                pool <- workerPool 4
                -- \| 检查无效 UTF-8 的请求与维护错误
                let checkErrors = do
                        requested <- sendRawRequest (BS.pack [255])
                        requested `shouldSatisfy` either (isInfixOf "request is not valid UTF-8") (const False)
                        maintained <- maintainStorage (dataDir srv ++ ".toml") (BS.pack [255])
                        maintained `shouldSatisfy` either (isInfixOf "invalid utf-8") (const False)
                void (poolRun pool (replicate 16 checkErrors))
                sendRawRequest (BSC.pack "{\"method\":\"ping\"}")
                    `shouldReturn` Right (BSC.pack "{\"status\":\"pong\"}")

        it "executes bound writes and CHECKs through real storage" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                -- \| 解析并通过真实存储执行写语句
                let query source = case parseStatement source of
                        Left err -> expectationFailure err >> pure (Left err)
                        Right statement -> runIPCStorage (runStatementM statement)
                query "CREATE DOMAIN quantity AS INT" `shouldReturn` Right []
                query "CREATE TABLE bound_writes (qty quantity DEFAULT 2, id INT, name TEXT CHECK (length(name) > 1))"
                    `shouldReturn` Right []
                query "INSERT INTO bound_writes (id, name) VALUES (1, 'abc'), (2, 'def')" `shouldReturn` Right []
                rejectedInsert <- query "INSERT INTO bound_writes (id, qty, qty, name) VALUES (99, 1, 9, 'ok')"
                rejectedInsert `shouldSatisfy` either (isInfixOf "INSERT: duplicate target column: qty") (const False)
                query "SELECT id, qty FROM bound_writes"
                    `shouldReturn` Right
                        [[("id", VInt 1), ("qty", VInt 2)], [("id", VInt 2), ("qty", VInt 2)]]
                query "UPDATE bound_writes SET qty = qty + (SELECT MIN(id) FROM bound_writes) WHERE id = 1"
                    `shouldReturn` Right []
                query "UPDATE bound_writes SET qty = qty + 1, qty = qty + id WHERE id = 1"
                    `shouldReturn` Right []
                rejected <- query "UPDATE bound_writes SET qty = qty + 10, name = 'x' WHERE id = 1"
                rejected `shouldSatisfy` either (isInfixOf "CHECK on name failed") (const False)
                query "DELETE FROM bound_writes WHERE qty = 2" `shouldReturn` Right []
                query "SELECT id, qty, name FROM bound_writes"
                    `shouldReturn` Right
                        [[("id", VInt 1), ("qty", VInt 5), ("name", VStr "abc")]]

        it "keeps enclosing scopes distinct through real storage" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                -- \| 解析并通过真实存储执行相关查询
                let query source = case parseStatement source of
                        Left err -> expectationFailure err >> pure (Left err)
                        Right statement -> runIPCStorage (runStatementM statement)
                query "CREATE TABLE scope_far (id INT)" `shouldReturn` Right []
                query "CREATE TABLE scope_near (id INT)" `shouldReturn` Right []
                query "INSERT INTO scope_far (id) VALUES (1), (2)" `shouldReturn` Right []
                query "INSERT INTO scope_near (id) VALUES (10), (11)" `shouldReturn` Right []
                query "SELECT o.id, (SELECT (SELECT id) FROM scope_near n WHERE n.id = o.id + 9) AS nearest FROM scope_far o"
                    `shouldReturn` Right
                        [ [("o.id", VInt 1), ("nearest", VInt 10)]
                        , [("o.id", VInt 2), ("nearest", VInt 11)]
                        ]

        it "keeps composite comparisons, hashes and join fallback consistent after restart" $ do
            withTestSession $ \srv -> do
                withServerEnv srv $
                    mapM_
                        (\source -> ipcDomainSql source `shouldReturn` Right [])
                        [ "CREATE DOMAIN int_list AS [Int]"
                        , "CREATE TABLE composite_keys (id int_list, xs [Double])"
                        , "INSERT INTO composite_keys (id, xs) VALUES (LIST<Int>(10), LIST<Double>(-0.0)), (LIST<Int>(2), LIST<Double>(0.0)), (LIST<Int>(2), LIST<Double>(0.0))"
                        ]
                closeConnection
                openSessionIn (dataDir srv)
                withServerEnv srv $ do
                    ipcDomainSql "SELECT COUNT(*) AS n FROM composite_keys a INNER JOIN composite_keys b ON a.id = b.id"
                        `shouldReturn` Right [[("n", VInt 5)]]
                    ipcDomainSql "SELECT COUNT(*) AS n FROM composite_keys WHERE id = LIST<Int>(2)"
                        `shouldReturn` Right [[("n", VInt 2)]]
                    ipcDomainSql "SELECT COUNT(*) AS n FROM composite_keys GROUP BY id ORDER BY id"
                        `shouldReturn` Right [[("n", VInt 2)], [("n", VInt 1)]]
                    ipcDomainSql "SELECT COUNT(*) AS n FROM (SELECT DISTINCT xs FROM composite_keys) d"
                        `shouldReturn` Right [[("n", VInt 1)]]
                    ipcDomainSql "UPDATE composite_keys SET xs = LIST<Double>(2.5) WHERE id = LIST<Int>(10)"
                        `shouldReturn` Right []
                    ipcDomainSql "DELETE FROM composite_keys WHERE id = LIST<Int>(2)" `shouldReturn` Right []
                    ipcDomainSql "SELECT COUNT(*) AS n FROM composite_keys" `shouldReturn` Right [[("n", VInt 1)]]
                    ipcDomainSql "CREATE INDEX ON composite_keys (id)" >>= (`shouldSatisfy` isLeft)

        it "persists runtime type identities and nested values across restart" $ do
            withTestSession $ \srv -> do
                -- \| 解析并通过真实存储执行 SQL
                let query source = case parseStatement source of
                        Left err -> expectationFailure err >> pure (Left err)
                        Right statement -> runIPCStorage (runStatementM statement)
                originalRows <- withServerEnv srv $ do
                    query "CREATE TABLE compound (id Int, xs [Int], opt Maybe String, pair (Int, String), text String)" `shouldReturn` Right []
                    query "INSERT INTO compound (id, xs, opt, pair, text) VALUES (1, LIST<Int>(1, 2), MAYBE<String>('x'), TUPLE<Int, String>(3, 'y'), 'abc')" `shouldReturn` Right []
                    query "SELECT xs, opt, pair, length(text) FROM compound"
                originalRows `shouldSatisfy` isRight
                closeConnection
                openSessionIn (dataDir srv)
                reloadedRows <- withServerEnv srv $ query "SELECT xs, opt, pair, length(text) FROM compound"
                reloadedRows `shouldBe` originalRows
                withServerEnv srv $ do
                    query "INSERT INTO compound (xs) VALUES (LIST<String>('wrong'))" >>= (`shouldSatisfy` isLeft)
                    query "UPDATE compound SET xs = LIST<Int>(9) WHERE id = 1" `shouldReturn` Right []
                    updated <- query "SELECT xs FROM compound WHERE id = 1"
                    updated `shouldSatisfy` isRight

        it "projects scan columns and preserves constant rows" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (insertMany "projected" [[("id", VInt 1), ("name", VStr "one")], [("id", VInt 2), ("name", VNull)]]) `shouldReturn` Right ()
                runIPCStorage (scanColumns "projected" ["name"]) `shouldReturn` Right [[("name", VStr "one")], [("name", VNull)]]
                runIPCStorage (scanColumns "projected" []) `shouldReturn` Right [[], []]
                -- query：解析并执行一条 SQL
                let query sql = case parseStatement sql of
                        Left err -> expectationFailure err >> pure (Left err)
                        Right stmt -> runIPCStorage (runStatementM stmt)
                result <- query "SELECT 1 FROM projected"
                fmap (map (map snd)) result `shouldBe` Right [[VInt 1], [VInt 1]]
                query "SELECT p.name FROM projected p ORDER BY p.id DESC" `shouldReturn` Right [[("p.name", VNull)], [("p.name", VStr "one")]]

        it "recreates a dropped column without resurrecting old values" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                -- query：解析并执行一条 SQL
                let query sql = case parseStatement sql of
                        Left err -> expectationFailure err >> pure (Left err)
                        Right stmt -> runIPCStorage (runStatementM stmt)
                mapM_
                    (\sql -> query sql `shouldReturn` Right [])
                    [ "CREATE TABLE reused (id int, secret str)"
                    , "INSERT INTO reused (id, secret) VALUES (1, 'old')"
                    , "ALTER TABLE reused DROP COLUMN secret"
                    , "ALTER TABLE reused ADD COLUMN secret str DEFAULT 'new'"
                    ]
                query "SELECT secret FROM reused" `shouldReturn` Right [[("secret", VStr "new")]]
                query "SELECT secret FROM reused WHERE id = 1" `shouldReturn` Right [[("secret", VStr "new")]]

        it "scans stored value types and reports missing tables" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                let row = [("id", VInt 7), ("name", VStr "Alice"), ("flag", VBool True)]
                runIPCStorage (insert "users" row) `shouldReturn` Right ()
                rows <- runIPCStorage (scan "users")
                (map sortRow <$> rows) `shouldBe` Right [sortRow row]
                result <- runIPCStorage (scan "no_such_table")
                result `shouldSatisfy` isLeft

        it "scan returns all 20 inserted rows in order" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (insertMany "many" [[("id", VInt i)] | i <- [1 .. 20 :: Int]]) `shouldReturn` Right ()
                rows <- runIPCStorage (scan "many")
                fmap length rows `shouldBe` Right 20
                fmap (map (lookup "id")) rows `shouldBe` Right (map (Just . VInt) [1 .. 20])

        it "looks up a string index" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "names" [("id", VInt 1), ("name", VStr "alice")]) `shouldReturn` Right ()
                runIPCStorage (insert "names" [("id", VInt 2), ("name", VStr "bob")]) `shouldReturn` Right ()
                runIPCStorage (createIndex "names" "name") `shouldReturn` Right ()
                found <- runIPCStorage (lookupByColumn "names" "name" (VStr "bob"))
                fmap (lookup "id") (indexRow found) `shouldBe` Just (Just (VInt 2))
                missing <- runIPCStorage (lookupByColumn "names" "name" (VStr "dave"))
                missing `shouldBe` Right (IndexRows [])

        it "range scans an indexed column" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "rng" [("id", VInt 1), ("code", VInt 100)]) `shouldReturn` Right ()
                runIPCStorage (insert "rng" [("id", VInt 2), ("code", VInt 200)]) `shouldReturn` Right ()
                runIPCStorage (insert "rng" [("id", VInt 3), ("code", VInt 300)]) `shouldReturn` Right ()
                runIPCStorage (createIndex "rng" "code") `shouldReturn` Right ()
                hit <- runIPCStorage (scanRange "rng" "code" (Just (VInt 200, True)) (Just (VInt 300, True)))
                fmap (fmap (map (lookup "id"))) hit `shouldBe` Right (Just [Just (VInt 2), Just (VInt 3)])
                open <- runIPCStorage (scanRange "rng" "code" (Just (VInt 200, False)) (Just (VInt 300, True)))
                fmap (fmap (map (lookup "id"))) open `shouldBe` Right (Just [Just (VInt 3)])
                runIPCStorage (scanRange "rng" "nope" Nothing Nothing) `shouldReturn` Right Nothing

        it "looks up inserted ids and replaces rows with rebuilt indexes" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                let original = [("id", VInt 1), ("name", VStr "Alice")]
                    replacement = [("id", VInt 9), ("name", VStr "Zoe")]
                runIPCStorage (insert "rb" original) `shouldReturn` Right ()
                found <- runIPCStorage (lookupByColumn "rb" "id" (VInt 1))
                fmap sortRow (indexRow found) `shouldBe` Just (sortRow original)
                runIPCStorage (lookupByColumn "rb" "id" (VInt 8)) `shouldReturn` Right (IndexRows [])
                runIPCStorage (replaceAll "rb" [replacement, [("id", VInt 8)]]) `shouldReturn` Right ()
                rows <- runIPCStorage (scan "rb")
                fmap (map sortRow) rows `shouldBe` Right [sortRow replacement, [("id", VInt 8)]]
                old <- runIPCStorage (lookupByColumn "rb" "id" (VInt 1))
                old `shouldBe` Right (IndexRows [])
                new <- runIPCStorage (lookupByColumn "rb" "id" (VInt 9))
                fmap sortRow (indexRow new) `shouldBe` Just (sortRow replacement)

        it "snapshot lists tables and scans each one to build the database" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "a" [("id", VInt 1)]) `shouldReturn` Right ()
                runIPCStorage (insert "b" [("id", VInt 2)]) `shouldReturn` Right ()
                Right db <- runIPCStorage snapshot
                map fst db `shouldBe` ["a", "b"]
                map (length . tableRows . snd) db `shouldBe` [1, 1]
                map (map (lookup "id") . tableRows . snd) db `shouldBe` [[Just (VInt 1)], [Just (VInt 2)]]
        it "rejects a bound query after a same-schema persistent table recreation" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (createTable "rebuilt" [("id", TInt)]) `shouldReturn` Right ()
                Right catalog <- runIPCStorage schema
                let bound = parseStatement "SELECT id FROM rebuilt" >>= Bound.bindQuery catalog
                case bound of
                    Left err -> expectationFailure err
                    Right query -> do
                        runIPCStorage (Bound.executeQueryM query) `shouldReturn` Right []
                        runIPCStorage (dropTable "rebuilt") `shouldReturn` Right ()
                        runIPCStorage (createTable "rebuilt" [("id", TInt)]) `shouldReturn` Right ()
                        changed <- runIPCStorage (Bound.executeQueryM query)
                        changed `shouldSatisfy` either (isInfixOf "bound query object changed") (const False)
        it "preserves persistent versions in snapshots and rejects restored schema" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (createTable "versioned" [("id", TInt)]) `shouldReturn` Right ()
                Right catalog <- runIPCStorage schema
                Right contents <- runIPCStorage snapshot
                fmap tableSchemaVersion (lookup "versioned" catalog) `shouldBe` Just (Just 1)
                fmap tableSchemaVersion (lookup "versioned" contents) `shouldBe` Just (Just 1)
                case parseStatement "SELECT id FROM versioned" >>= Bound.bindQuery catalog of
                    Left err -> expectationFailure err
                    Right query -> do
                        runIPCStorage (replaceSchema "versioned" [("id", TInt), ("extra", TStr)] []) `shouldReturn` Right ()
                        runIPCStorage (replaceSchema "versioned" [("id", TInt)] []) `shouldReturn` Right ()
                        changed <- runIPCStorage (Bound.executeQueryM query)
                        changed `shouldSatisfy` either (isInfixOf "bound query schema changed") (const False)
        it "rejects a DDL race between schema validation and the actual read" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (createTable "raced" [("id", TInt)]) `shouldReturn` Right ()
                Right catalog <- runIPCStorage schema
                closeConnection
                Right link <- localStorageLink (Just (dataDir srv ++ ".toml"))
                setStorageLink
                    link
                        { slRequest = \payload -> do
                            when (BSC.pack "schema_guard" `BS.isInfixOf` payload) $
                                do
                                    changed <-
                                        slRequest
                                            link
                                            ( BSC.pack
                                                "{\"method\":\"replace_schema\",\"database\":\"ipctest\",\"table\":\"raced\",\"columns\":[{\"name\":\"id\",\"ty\":\"int\"},{\"name\":\"extra\",\"ty\":\"str\"}],\"rows\":[]}"
                                            )
                                    changed
                                        `shouldSatisfy` either (const False) (BS.isInfixOf (BSC.pack "ok"))
                            slRequest link payload
                        }
                case parseStatement "SELECT id FROM raced" >>= Bound.bindQuery catalog of
                    Left err -> expectationFailure err
                    Right query -> do
                        result <- runIPCStorage (Bound.executeQueryM query)
                        result `shouldSatisfy` either (isInfixOf "bound query schema changed") (const False)

        it "rejects restored-schema races at the actual INSERT UPDATE DELETE and ALTER write" $ do
            mapM_
                ( \(method, sql) -> withTestSession $ \srv -> withServerEnv srv $ do
                    runIPCStorage (createTable "write_race" [("id", TInt), ("qty", TInt)]) `shouldReturn` Right ()
                    runIPCStorage (insert "write_race" [("id", VInt 1), ("qty", VInt 5)]) `shouldReturn` Right ()
                    closeConnection
                    Right link <- localStorageLink (Just (dataDir srv ++ ".toml"))
                    setStorageLink
                        link
                            { slRequest = \payload -> do
                                when
                                    ( BSC.pack "schema_guard" `BS.isInfixOf` payload
                                        && BSC.pack ("\"method\":\"" ++ method ++ "\"") `BS.isInfixOf` payload
                                    )
                                    $ do
                                        added <- slRequest link (BSC.pack "{\"method\":\"replace_schema\",\"database\":\"ipctest\",\"table\":\"write_race\",\"columns\":[{\"name\":\"id\",\"ty\":\"int\"},{\"name\":\"qty\",\"ty\":\"int\"},{\"name\":\"extra\",\"ty\":\"str\"}],\"rows\":[{\"id\":1,\"qty\":5,\"extra\":\"x\"}]}")
                                        added `shouldSatisfy` either (const False) (BS.isInfixOf (BSC.pack "ok"))
                                        restored <- slRequest link (BSC.pack "{\"method\":\"replace_schema\",\"database\":\"ipctest\",\"table\":\"write_race\",\"columns\":[{\"name\":\"id\",\"ty\":\"int\"},{\"name\":\"qty\",\"ty\":\"int\"}],\"rows\":[{\"id\":1,\"qty\":5}]}")
                                        restored `shouldSatisfy` either (const False) (BS.isInfixOf (BSC.pack "ok"))
                                slRequest link payload
                            }
                    case parseStatement sql of
                        Left err -> expectationFailure err
                        Right statement -> do
                            result <- runIPCStorage (runStatementM statement)
                            result `shouldSatisfy` either (isInfixOf "bound query schema changed") (const False)
                            runIPCStorage (scan "write_race") `shouldReturn` Right [[("id", VInt 1), ("qty", VInt 5)]]
                )
                [ ("insert_batch", "INSERT INTO write_race (id, qty) VALUES (2, 9)")
                , ("replace_all", "UPDATE write_race SET qty = qty + 1")
                , ("delete_keys", "DELETE FROM write_race WHERE id = 1")
                , ("replace_schema", "ALTER TABLE write_race ADD COLUMN note TEXT")
                ]

        it "rechecks cross-database subquery dependencies before publishing a write" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (createTable "write_target" [("id", TInt)]) `shouldReturn` Right ()
                runIPCStorage (insert "write_target" [("id", VInt 1)]) `shouldReturn` Right ()
                runIPCStorage (createDatabase "write_source") `shouldReturn` Right ()
                runIPCStorage (createTable "write_source.source" [("id", TInt)]) `shouldReturn` Right ()
                runIPCStorage (insert "write_source.source" [("id", VInt 2)]) `shouldReturn` Right ()
                closeConnection
                Right link <- localStorageLink (Just (dataDir srv ++ ".toml"))
                setStorageLink
                    link
                        { slRequest = \payload -> do
                            when
                                ( BSC.pack "schema_guard" `BS.isInfixOf` payload
                                    && BSC.pack "\"method\":\"replace_all\"" `BS.isInfixOf` payload
                                )
                                $ do
                                    changed <- slRequest link (BSC.pack "{\"method\":\"replace_schema\",\"database\":\"write_source\",\"table\":\"source\",\"columns\":[{\"name\":\"id\",\"ty\":\"bigint\"}],\"rows\":[{\"id\":2}]}")
                                    changed `shouldSatisfy` either (const False) (BS.isInfixOf (BSC.pack "ok"))
                            slRequest link payload
                        }
                case parseStatement "UPDATE ipctest.write_target SET id = (SELECT id FROM write_source.source)" of
                    Left err -> expectationFailure err
                    Right statement -> do
                        result <- runIPCStorageIn (runStatementM statement) (envForDatabase Nothing)
                        result `shouldSatisfy` either (isInfixOf "bound query schema changed") (const False)
                        runIPCStorage (scan "write_target") `shouldReturn` Right [[("id", VInt 1)]]

        it "guards newly referenced DOMAIN definitions during ALTER" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (runStatementM (CreateDomain "new_quantity" (DomainDefinition (plainColumn CInt) []))) `shouldReturn` Right []
                runIPCStorage (createTable "domain_target" [("id", TInt)]) `shouldReturn` Right ()
                closeConnection
                Right link <- localStorageLink (Just (dataDir srv ++ ".toml"))
                setStorageLink
                    link
                        { slRequest = \payload -> do
                            when
                                ( BSC.pack "schema_guard" `BS.isInfixOf` payload
                                    && BSC.pack "\"method\":\"replace_schema\"" `BS.isInfixOf` payload
                                )
                                $ do
                                    changed <- slRequest link (BSC.pack "{\"method\":\"replace_schema\",\"database\":\"ipctest\",\"table\":\"__system_domain_new_quantity\",\"columns\":[{\"name\":\"base\",\"ty\":\"bigint\"}],\"rows\":[]}")
                                    changed `shouldSatisfy` either (const False) (BS.isInfixOf (BSC.pack "ok"))
                            slRequest link payload
                        }
                case parseStatement "ALTER TABLE domain_target ADD COLUMN qty new_quantity" of
                    Left err -> expectationFailure err
                    Right statement -> do
                        result <- runIPCStorage (runStatementM statement)
                        result `shouldSatisfy` either (isInfixOf "bound query schema changed") (const False)
                        Right catalog <- runIPCStorage schema
                        fmap (map fst . tableCols) (lookup "domain_target" catalog) `shouldBe` Just ["id"]

        it "doListTables returns the tables that exist" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "t1" [("id", VInt 1)]) `shouldReturn` Right ()
                runIPCStorage (insert "t2" [("id", VInt 2)]) `shouldReturn` Right ()
                env <- defaultEnv
                doListTables env `shouldReturn` Right ["t1", "t2"]

        it "two sessions keep their own database" $ do
            withTestSession $ \_ -> do
                let alpha = envForDatabase (Just "alpha")
                    beta = envForDatabase (Just "beta")
                runIPCStorageIn (createDatabase "alpha") alpha `shouldReturn` Right ()
                runIPCStorageIn (createDatabase "beta") beta `shouldReturn` Right ()
                runIPCStorageIn (insert "iso" [("id", VInt 1)]) alpha `shouldReturn` Right ()
                runIPCStorageIn (insert "iso" [("id", VInt 2)]) beta `shouldReturn` Right ()
                runIPCStorageIn (scan "iso") alpha `shouldReturn` Right [[("id", VInt 1)]]
                runIPCStorageIn (scan "iso") beta `shouldReturn` Right [[("id", VInt 2)]]

        it "persists altered domain defaults and inherited checks through actual FFI" $ do
            withTestSession $ \srv -> do
                withServerEnv srv $ do
                    mapM_
                        (\sql -> ipcDomainSql sql `shouldReturn` Right [])
                        [ "CREATE DOMAIN quantity AS INT DEFAULT 2 CHECK (VaLuE > 0)"
                        , "CREATE DOMAIN child AS quantity NOT NULL"
                        , "CREATE TABLE items (id INT, qty child)"
                        , "INSERT INTO items (id) VALUES (1)"
                        , "ALTER DOMAIN quantity SET DEFAULT 3"
                        , "ALTER DOMAIN QUANTITY ADD CONSTRAINT BOUNDED CHECK (VALUE < 10)"
                        ]
                    ipcDomainSql "ALTER DOMAIN quantity ADD CHECK (VALUE > 5)" >>= (`shouldSatisfy` isLeft)
                closeConnection
                openSessionIn (dataDir srv)
                withServerEnv srv $ do
                    ipcDomainSql "INSERT INTO items (id) VALUES (2)" `shouldReturn` Right []
                    ipcDomainSql "SELECT qty FROM items ORDER BY id"
                        `shouldReturn` Right [[("qty", VInt 2)], [("qty", VInt 3)]]
                    mapM_
                        (ipcDomainSql >=> (`shouldSatisfy` isLeft))
                        [ "UPDATE items SET qty = 10"
                        , "INSERT INTO items (qty) VALUES (NULL)"
                        , "ALTER DOMAIN quantity SET DEFAULT 0"
                        ]
                    ipcDomainSql "ALTER DOMAIN QUANTITY DROP CONSTRAINT BOUNDED" `shouldReturn` Right []
                    ipcDomainSql "UPDATE items SET qty = 10" `shouldReturn` Right []
        it "rejects domain alteration when a writer changes the validated snapshot" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                mapM_
                    (\sql -> ipcDomainSql sql `shouldReturn` Right [])
                    [ "CREATE DOMAIN quantity AS INT"
                    , "CREATE TABLE items (qty quantity)"
                    , "INSERT INTO items (qty) VALUES (4)"
                    ]
                closeConnection
                Right link <- localStorageLink (Just (dataDir srv ++ ".toml"))
                setStorageLink
                    link
                        { slRequest = \payload -> do
                            when (BSC.pack "\"method\":\"alter_domain\"" `BS.isInfixOf` payload) $ do
                                written <- slRequest link (BSC.pack "{\"method\":\"insert\",\"database\":\"ipctest\",\"table\":\"items\",\"row\":{\"qty\":0}}")
                                written `shouldSatisfy` either (const False) (BS.isInfixOf (BSC.pack "ok"))
                            slRequest link payload
                        }
                result <- ipcDomainSql "ALTER DOMAIN quantity ADD CHECK (VALUE > 0)"
                fmap errorCode (either Right (const (Left "expected conflict")) result) `shouldBe` Right "serialization_failure"
                ipcDomainSql "SELECT qty FROM items"
                    `shouldReturn` Right [[("qty", VInt 4)], [("qty", VInt 0)]]
                ipcDomainSql "INSERT INTO items (qty) VALUES (-1)" `shouldReturn` Right []
        it "guards domain constraints while creating a dependent table" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                ipcDomainSql "CREATE DOMAIN quantity AS INT" `shouldReturn` Right []
                closeConnection
                Right link <- localStorageLink (Just (dataDir srv ++ ".toml"))
                setStorageLink
                    link
                        { slRequest = \payload -> do
                            when
                                ( BSC.pack "schema_guard" `BS.isInfixOf` payload
                                    && BSC.pack "\"method\":\"create_table\"" `BS.isInfixOf` payload
                                )
                                $ do
                                    changed <- slRequest link (BSC.pack "{\"method\":\"replace_schema\",\"database\":\"ipctest\",\"table\":\"__system_domain_quantity\",\"columns\":[{\"name\":\"base\",\"ty\":\"int\",\"nullable\":false}],\"rows\":[]}")
                                    changed `shouldSatisfy` either (const False) (BS.isInfixOf (BSC.pack "ok"))
                            slRequest link payload
                        }
                result <- ipcDomainSql "CREATE TABLE items (qty quantity DEFAULT NULL)"
                result `shouldSatisfy` either (isInfixOf "bound query schema changed") (const False)
                runIPCStorage (scan "items") >>= (`shouldSatisfy` isLeft)

        it "persists enum labels defaults and indexes across actual FFI restart" $ do
            let labels = ["READER", "作者", "q'uote", "a,b)", "domain(unrelated,str)"]
            withTestSession $ \srv -> do
                withServerEnv srv $ do
                    mapM_
                        (\sql -> ipcDomainSql sql `shouldReturn` Right [])
                        [ "CREATE TYPE role AS ENUM ('READER', '作者', 'q''uote', 'a,b)', 'domain(unrelated,str)')"
                        , "CREATE TABLE enum_members (id INT, role role DEFAULT 'READER' UNIQUE)"
                        , "INSERT INTO enum_members (id) VALUES (1)"
                        , "INSERT INTO enum_members (id, role) VALUES (2, '作者'), (3, 'q''uote'), (4, 'a,b)')"
                        , "CREATE INDEX ON enum_members (role)"
                        , "CREATE DOMAIN unrelated AS TEXT"
                        , "DROP DOMAIN unrelated"
                        ]
                closeConnection
                openSessionIn (dataDir srv)
                withServerEnv srv $ do
                    ipcDomainSql "SELECT role FROM enum_members WHERE role = '作者'" `shouldReturn` Right [[("role", VEnum "role" labels "作者")]]
                    ipcDomainSql "SELECT role FROM enum_members ORDER BY id" `shouldReturn` Right [[("role", VEnum "role" labels value)] | value <- ["READER", "作者", "q'uote", "a,b)"]]
                    ipcDomainSql "UPDATE enum_members SET role = 'reader' WHERE id = 1" >>= (`shouldSatisfy` isLeft)
                    ipcDomainSql "INSERT INTO enum_members (id, role) VALUES (5, '作者')" >>= (`shouldSatisfy` isLeft)
                    ipcDomainSql "DROP TYPE role" >>= (`shouldSatisfy` isLeft)
                    ipcDomainSql "DELETE FROM enum_members WHERE id = 1" `shouldReturn` Right []
                    ipcDomainSql "INSERT INTO enum_members (id) VALUES (5)" `shouldReturn` Right []
                    ipcDomainSql "SELECT role FROM enum_members WHERE id = 5" `shouldReturn` Right [[("role", VEnum "role" labels "READER")]]

        it "persists nested domains and named columns across restart" $ do
            withTestSession $ \srv -> do
                withServerEnv srv $ do
                    runIPCStorage (runStatementM (CreateDomain "label" (DomainDefinition (plainColumn (CVarchar 3)) []))) `shouldReturn` Right []
                    runIPCStorage (runStatementM (CreateDomain "product_label" (DomainDefinition (plainColumn (CDomain "label" CStr)) []))) `shouldReturn` Right []
                    runIPCStorage (runStatementM (CreateTable "typed" [("name", plainColumn (CDomain "product_label" CStr))])) `shouldReturn` Right []
                    runIPCStorage (runStatementM (Insert "typed" ["name"] [[LitStr "abc"]])) `shouldReturn` Right []
                closeConnection
                openSessionIn (dataDir srv)
                withServerEnv srv $ do
                    runIPCStorage (runStatementM ShowDomains) `shouldReturn` Right [[("domain", VStr "label"), ("base_type", VStr "varchar(3)")], [("domain", VStr "product_label"), ("base_type", VStr "domain(label,varchar(3))")]]
                    runIPCStorage (runStatementM (Insert "typed" ["name"] [[LitStr "long"]])) >>= (`shouldSatisfy` isLeft)
                    runIPCStorage (runStatementM (DropDomain "label")) >>= (`shouldSatisfy` isLeft)

        it "rows survive a restart on the same data directory" $ do
            withTestSession $ \srv -> do
                withServerEnv srv $ runIPCStorage (insert "persist" [("id", VInt 42)]) `shouldReturn` Right ()
                closeConnection
                openSessionIn (dataDir srv)
                rows <- withServerEnv srv $ runIPCStorage (scan "persist")
                (map sortRow <$> rows) `shouldBe` Right [sortRow [("id", VInt 42)]]

        it "an unusable config path yields Left without throwing" $ do
            opened <- localStorageLink (Just "no/such/dir/chusql-core-storage.toml")
            case opened of
                Left err -> err `shouldSatisfy` (not . null)
                Right _ -> expectationFailure "opening a missing config file should fail"

        it "without a link the storage layer reports Left instead of throwing" $ do
            closeConnection
            result <- runIPCStorage (scan "users")
            result `shouldSatisfy` isLeft
        it "INSERT then SELECT WHERE id = k" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                _ <-
                    runIPCStorage
                        ( runStatementM
                            (CreateTable "ipc_idx" [("id", TInt), ("name", TStr)])
                        )
                _ <-
                    runIPCStorage
                        ( runStatementM
                            (Insert "ipc_idx" ["id", "name"] [[LitInt 42, LitStr "Zoe"]])
                        )
                result <-
                    runIPCStorage
                        ( runStatementM
                            ( Select
                                { selectItems = [("name", Col "name")]
                                , selectFrom = FromTable Nothing "ipc_idx"
                                , selectWhere = Just (Eq (Col "id") (LitInt 42))
                                , selectGroupBy = []
                                , selectHaving = Nothing
                                , selectOrderBy = []
                                , selectLimit = Nothing
                                }
                            )
                        )
                result `shouldBe` Right [[("name", VStr "Zoe")]]
        -- 存储层的日志现在直接打在本进程的输出里，没有单独的 debug.log 可读；
        -- 「走索引而不是全表扫」由下面 CREATE INDEX 那组用例与 Rust 侧 runtime 测试覆盖。

        it "keeps correlated outer references out of physical scan columns" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (runStatementM (CreateTable "outer_t" [("id", TInt), ("name", TStr)])) `shouldReturn` Right []
                runIPCStorage (runStatementM (CreateTable "inner_t" [("id", TInt), ("user_id", TInt), ("unused", TStr)])) `shouldReturn` Right []
                runIPCStorage (runStatementM (Insert "outer_t" ["id", "name"] [[LitInt 1, LitStr "one"], [LitInt 2, LitStr "two"]])) `shouldReturn` Right []
                runIPCStorage (runStatementM (Insert "inner_t" ["id", "user_id", "unused"] [[LitInt 10, LitInt 1, LitStr "x"]])) `shouldReturn` Right []
                case parseStatement "SELECT u.name FROM outer_t u WHERE EXISTS (SELECT id FROM inner_t o WHERE o.user_id = u.id)" of
                    Left err -> expectationFailure err
                    Right statement -> runIPCStorage (runStatementM statement) `shouldReturn` Right [[("u.name", VStr "one")]]

        it "CREATE INDEX makes a query on that column use the index" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                _ <- runIPCStorage (runStatementM (CreateTable "ix_t" [("id", TInt), ("code", TInt)]))
                _ <-
                    runIPCStorage
                        ( runStatementM
                            (Insert "ix_t" ["id", "code"] [[LitInt 1, LitInt 500], [LitInt 2, LitInt 501]])
                        )

                withoutIndex <- runIPCStorage (lookupByColumn "ix_t" "code" (VInt 501))
                withoutIndex `shouldBe` Right NoIndex
                scanned <- runIPCStorage (runStatementM (makeSelect ["code"] "ix_t" (Just (Eq (Col "code") (LitInt 501)))))
                scanned `shouldBe` Right [[("code", VInt 501)]]

                created <- runIPCStorage (runStatementM (CreateIndex "ix_t" "code"))
                created `shouldBe` Right []
                withIndex <- runIPCStorage (lookupByColumn "ix_t" "code" (VInt 501))
                fmap sortRow (indexRow withIndex)
                    `shouldBe` Just (sortRow [("id", VInt 2), ("code", VInt 501)])
                indexed <- runIPCStorage (runStatementM (makeSelect ["code"] "ix_t" (Just (Eq (Col "code") (LitInt 501)))))
                indexed `shouldBe` Right [[("code", VInt 501)]]

                _ <- runIPCStorage (runStatementM (Insert "ix_t" ["id", "code"] [[LitInt 3, LitInt 502]]))
                fresh <- runIPCStorage (lookupByColumn "ix_t" "code" (VInt 502))
                fmap sortRow (indexRow fresh)
                    `shouldBe` Just (sortRow [("id", VInt 3), ("code", VInt 502)])

        it "schema carries the row count, index list and column statistics" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                _ <- runIPCStorage (runStatementM (CreateTable "meta_t" [("id", TInt), ("code", TInt)]))
                _ <-
                    runIPCStorage
                        ( runStatementM
                            (Insert "meta_t" ["id", "code"] [[LitInt 1, LitInt 500], [LitInt 2, LitInt 501]])
                        )
                _ <- runIPCStorage (runStatementM (CreateIndex "meta_t" "code"))
                Right db <- runIPCStorage schema
                case lookup "meta_t" db of
                    Nothing -> expectationFailure "meta_t missing from schema"
                    Just t -> do
                        fmap metaRowCount (tableMeta t) `shouldBe` Just 2
                        fmap metaIndexes (tableMeta t) `shouldBe` Just ["id", "code"]
                        fmap (sortOn fst . map (\(c, d, _) -> (c, d)) . metaDistinct) (tableMeta t)
                            `shouldBe` Just [("code", 2), ("id", 2)]

        it "CREATE INDEX accepts duplicates and returns every matching row" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                _ <- runIPCStorage (runStatementM (CreateTable "dup_ix" [("id", TInt), ("age", TInt)]))
                _ <-
                    runIPCStorage
                        ( runStatementM
                            (Insert "dup_ix" ["id", "age"] [[LitInt 1, LitInt 30], [LitInt 2, LitInt 30]])
                        )
                res <- runIPCStorage (runStatementM (CreateIndex "dup_ix" "age"))
                res `shouldBe` Right []
                found <- runIPCStorage (lookupByColumn "dup_ix" "age" (VInt 30))
                case found of
                    Right (IndexRows rows) -> map (lookup "id") rows `shouldBe` [Just (VInt 1), Just (VInt 2)]
                    _ -> expectationFailure "expected an indexed lookup with rows"

        it "DELETE removes only the matching rows" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                _ <- runIPCStorage (runStatementM (CreateTable "del_t" [("id", TInt), ("name", TStr)]))
                _ <-
                    runIPCStorage
                        ( runStatementM
                            ( Insert
                                "del_t"
                                ["id", "name"]
                                [[LitInt 1, LitStr "A"], [LitInt 2, LitStr "B"], [LitInt 3, LitStr "C"]]
                            )
                        )
                _ <- runIPCStorage (runStatementM (Delete "del_t" (Just (Eq (Col "id") (LitInt 2)))))
                rows <- runIPCStorage (runStatementM (makeSelect ["name"] "del_t" Nothing))
                rows `shouldBe` Right [[("name", VStr "A")], [("name", VStr "C")]]
                gone <- runIPCStorage (lookupByColumn "del_t" "id" (VInt 2))
                gone `shouldBe` Right (IndexRows [])

        it "multi-row INSERT goes in one statement" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                _ <- runIPCStorage (runStatementM (CreateTable "many_t" [("id", TInt)]))
                _ <-
                    runIPCStorage
                        ( runStatementM
                            (Insert "many_t" ["id"] [[LitInt i] | i <- [1 .. 50]])
                        )
                rows <- runIPCStorage (runStatementM (makeSelect ["id"] "many_t" Nothing))
                fmap length rows `shouldBe` Right 50
        it "parses CREATE TABLE with two columns" $ do
            parseStatement "CREATE TABLE users (id INT, name TEXT)"
                `shouldBe` Right (CreateTable "users" [("id", TInt), ("name", TStr)])

        it "parses CREATE TABLE with BOOL column" $ do
            parseStatement "CREATE TABLE t (flag BOOL)"
                `shouldBe` Right (CreateTable "t" [("flag", TBool)])

        it "parses TRUE / FALSE as boolean literals (case-insensitive)" $ do
            parseStatement "INSERT INTO t (flag) VALUES (TRUE)"
                `shouldBe` Right (Insert "t" ["flag"] [[LitBool True]])
            parseStatement "INSERT INTO t (flag) VALUES (false)"
                `shouldBe` Right (Insert "t" ["flag"] [[LitBool False]])

        it "boolean literals work in WHERE (and don't need a column named true)" $ do
            parseStatement "SELECT * FROM t WHERE flag = TRUE"
                `shouldBe` Right (makeSelect ["*"] "t" (Just (Eq (Col "flag") (LitBool True))))

        it "inserts and reads back a boolean column end-to-end" $ do
            let db = [("t", Table "t" [("id", TInt), ("flag", TBool)] [] Nothing Nothing Nothing)]
            case runStatement db (Insert "t" ["id", "flag"] [[LitInt 1, LitBool True]]) of
                Left e -> expectationFailure e
                Right (db', _) ->
                    rowsOf (runStatement db' (makeSelect ["flag"] "t" Nothing))
                        `shouldBe` Right [[("flag", VBool True)]]

        it "parses CREATE TABLE with VARCHAR as TStr" $ do
            parseStatement "CREATE TABLE t (name VARCHAR)"
                `shouldBe` Right (CreateTable "t" [("name", TStr)])

        it "rejects CREATE TABLE with missing type" $ do
            parseStatement "CREATE TABLE t (id)"
                `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (CREATE TABLE)" $ do
        it "creates a table in memory" $ do
            case runStatement testDB (CreateTable "newt" [("id", TInt), ("name", TStr)]) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runStatement db' (makeSelect ["*"] "newt" Nothing))
                        `shouldBe` Right []

        it "rejects creating an existing table" $ do
            runStatement testDB (CreateTable "users" [("id", TInt)])
                `shouldSatisfy` isLeft

        it "rejects duplicate column names" $ do
            runStatement testDB (CreateTable "dup" [("id", TInt), ("id", TStr)])
                `shouldSatisfy` isLeft

        it "rejects empty column list at parse time" $ do
            parseStatement "CREATE TABLE t ()"
                `shouldSatisfy` isLeft
        it "parses DROP TABLE" $ do
            parseStatement "DROP TABLE users"
                `shouldBe` Right (DropTable "users")

        it "drops a table in memory" $ do
            case runStatement testDB (DropTable "users") of
                Left e -> expectationFailure e
                Right (db', _) -> do
                    runStatement db' (makeSelect ["*"] "users" Nothing)
                        `shouldSatisfy` isLeft

        it "rejects dropping a nonexistent table" $ do
            runStatement testDB (DropTable "nope")
                `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (NULL)" $ do
        -- rows：在给定库上跑 SQL 取结果行
        let rows db input = parseStatement input >>= rowsOf . runStatement db
            -- vals：只取结果里的值
            vals db input = fmap (map (map snd)) (rows db input)
            seeded = either (const testDB) fst (parseStatement "INSERT INTO users (id, name) VALUES (9, 'Nine')" >>= runStatement testDB)

        it "parses IS NULL and IS NOT NULL" $ do
            parseStatement "SELECT name FROM users WHERE age IS NULL"
                `shouldBe` Right (makeSelect ["name"] "users" (Just (IsNull (Col "age"))))
            parseStatement "SELECT name FROM users WHERE age IS NOT NULL"
                `shouldBe` Right (makeSelect ["name"] "users" (Just (IsNotNull (Col "age"))))

        it "fills omitted columns with NULL" $ do
            rows seeded "SELECT name FROM users WHERE age IS NULL"
                `shouldBe` Right [[("name", VStr "Nine")]]

        it "IS NOT NULL drops NULL rows" $ do
            vals seeded "SELECT name FROM users WHERE age IS NOT NULL"
                `shouldBe` Right [[VStr "Alice"], [VStr "Bob"], [VStr "Carol"]]

        it "compares nothing with NULL" $ do
            vals seeded "SELECT name FROM users WHERE age = NULL" `shouldBe` Right []
            vals seeded "SELECT name FROM users WHERE NULL IS NULL" `shouldBe` Right [[VStr "Alice"], [VStr "Bob"], [VStr "Carol"], [VStr "Nine"]]

        it "keeps NULL through three-valued AND/OR" $ do
            vals seeded "SELECT name FROM users WHERE age > 18 OR age IS NULL"
                `shouldBe` Right [[VStr "Alice"], [VStr "Carol"], [VStr "Nine"]]
            vals seeded "SELECT name FROM users WHERE age > 18 AND age < 30"
                `shouldBe` Right [[VStr "Alice"]]

        it "propagates NULL through arithmetic and projection" $ do
            vals seeded "SELECT age + 1 FROM users WHERE name = 'Nine'" `shouldBe` Right [[VNull]]
            vals seeded "SELECT age * 2 FROM users WHERE name = 'Nine'" `shouldBe` Right [[VNull]]

    describe "ChuSQL.Core.Engine (TYPES)" $ do
        it "checks rounded decimal precision, finite values and domains" $ do
            coerceValue (CDecimal 4 2) (VFloat 99.994) `shouldBe` Right (VFloat 99.99)
            mapM_
                (\value -> coerceValue (CDecimal 4 2) value `shouldSatisfy` isLeft)
                [VFloat 99.995, VFloat (-99.995), VInt 100, VFloat (0 / 0), VFloat (1 / 0)]
            coerceValue (CDecimal 2 0) (VFloat 2.5) `shouldBe` Right (VFloat 2)
            coerceValue (CDomain "amount" (CDecimal 4 2)) (VInt 100) `shouldSatisfy` isLeft
            coerceValue (CDecimal 309 0) (VInt 1) `shouldSatisfy` isLeft
        it "rejects invalid decimal definitions and overflowing defaults" $ do
            mapM_
                (\query -> (parseStatement query >>= runStatement testDB) `shouldSatisfy` isLeft)
                ["CREATE TABLE bad (v DECIMAL(2,3))", "CREATE TABLE bad (v DECIMAL(309,0))", "CREATE TABLE bad (v DECIMAL(2,0) DEFAULT 100)"]
        it "does not expose password input in syntax errors" $ do
            let query = "ALTER USER alice IDENTIFIED BY 'never-expose-this' trailing"
            parseStatement query `shouldSatisfy` either (\err -> "syntax" `isInfixOf` err && not ("never-expose-this" `isInfixOf` err)) (const False)
        it "rejects arbitrary prefixes for unqualified schema columns" $ do
            resolveColumn "intruder.id" [("id", TInt)] `shouldSatisfy` isLeft
            resolveColumn "wrong.u.id" [("id", TInt)] `shouldSatisfy` isLeft
            mapM_
                (\query -> (parseStatement query >>= runStatement testDB) `shouldSatisfy` isLeft)
                ["SELECT wrong.users.id FROM users", "SELECT wrong.u.id FROM users u", "SELECT users.id FROM users JOIN orders ON wrong.users.id = orders.user_id"]
        it "preserves untyped NULL through derived tables and CTEs" $ do
            mapM_
                (\query -> rowsOf (parseStatement query >>= runStatement testDB) `shouldBe` Right [[("answer", VNull)]])
                [ "SELECT d.n + 1 AS answer FROM (SELECT NULL AS n) d"
                , "SELECT d.n AND TRUE AS answer FROM (SELECT NULL AS n) d"
                , "WITH t AS (SELECT NULL AS n) SELECT t.n + 1 AS answer FROM t"
                , "SELECT d.n + 1 AS answer FROM (SELECT x.n FROM (SELECT NULL AS n) x) d"
                ]
        -- vals：在给定库上跑 SQL 只取值
        let vals db input = fmap (map (map snd)) (parseStatement input >>= rowsOf . runStatement db)
            -- withTable：先建表，返回新库
            withTable ddl = either (const testDB) fst (parseStatement ddl >>= runStatement testDB)

        it "parses every column type" $ do
            parseStatement "CREATE TABLE t (a bigint, b smallint, c varchar(5), d char(2), e float, f double, g decimal(6,2), h date, i timestamp, j blob, k bool)"
                `shouldBe` Right
                    ( CreateTable
                        "t"
                        [ ("a", plainColumn CBigInt)
                        , ("b", plainColumn CSmallInt)
                        , ("c", plainColumn (CVarchar 5))
                        , ("d", plainColumn (CChar 2))
                        , ("e", plainColumn CFloat)
                        , ("f", plainColumn CDouble)
                        , ("g", plainColumn (CDecimal 6 2))
                        , ("h", plainColumn CDate)
                        , ("i", plainColumn CTimestamp)
                        , ("j", plainColumn CBlob)
                        , ("k", plainColumn CBool)
                        ]
                    )
            fmap snd (domainSql [] ["CREATE TABLE t (a nope)"]) `shouldBe` Left "unknown domain: nope"

        it "reads and writes float literals" $ do
            vals testDB "SELECT 1.5 + 1" `shouldBe` Right [[VFloat 2.5]]
            vals testDB "SELECT 1 / 2, 1.0 / 2" `shouldBe` Right [[VInt 0, VFloat 0.5]]

        it "stores dates, timestamps and blobs as validated strings" $ do
            let db = withTable "CREATE TABLE typed (id int, d date, ts timestamp, p blob)"
            case parseStatement "INSERT INTO typed (id, d, ts, p) VALUES (1, DATE '2024-01-02', TIMESTAMP '2024-01-02 03:04:05', X'4869')" >>= runStatement db of
                Left e -> expectationFailure e
                Right (db', _) -> do
                    vals db' "SELECT d FROM typed" `shouldBe` Right [[VStr "2024-01-02"]]
                    vals db' "SELECT p FROM typed" `shouldBe` Right [[VStr "4869"]]
            runStatement db (Insert "typed" ["d"] [[LitStr "not-a-date"]])
                `shouldSatisfy` either (isInfixOf "date") (const False)

        it "rejects values that do not fit the column" $ do
            let db = withTable "CREATE TABLE sized (id int, code varchar(3), d date)"
            runStatement db (Insert "sized" ["code"] [[LitStr "toolong"]])
                `shouldSatisfy` either (isInfixOf "longer") (const False)
            runStatement db (Insert "sized" ["d"] [[LitInt 5]])
                `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (CONSTRAINTS)" $ do
        -- vals：在给定库上跑 SQL 只取值
        let vals db input = fmap (map (map snd)) (parseStatement input >>= rowsOf . runStatement db)
            -- withTable：先建表，返回新库
            withTable ddl = either (const testDB) fst (parseStatement ddl >>= runStatement testDB)
            -- step：执行一条语句，失败就返回原库
            step db input = either (const db) fst (parseStatement input >>= runStatement db)

        it "rejects omitting a NOT NULL column without a default" $ do
            let db = withTable "CREATE TABLE nn (id int NOT NULL, name str)"
            runStatement db (Insert "nn" ["name"] [[LitStr "x"]])
                `shouldSatisfy` either (isInfixOf "NOT NULL") (const False)
            runStatement db (Insert "nn" ["id"] [[LitNull]])
                `shouldSatisfy` either (isInfixOf "NOT NULL") (const False)

        it "fills the DEFAULT when a column is omitted" $ do
            let db = withTable "CREATE TABLE dft (id int, tag str DEFAULT 'x')"
            vals (step db "INSERT INTO dft (id) VALUES (1)") "SELECT tag FROM dft" `shouldBe` Right [[VStr "x"]]

        it "assigns AUTO_INCREMENT values" $ do
            let db = step (withTable "CREATE TABLE ai (id int AUTO_INCREMENT PRIMARY KEY, name str)") "INSERT INTO ai (name) VALUES ('a'), ('b')"
            vals db "SELECT id FROM ai" `shouldBe` Right [[VInt 1], [VInt 2]]

        it "rejects duplicates in a UNIQUE column" $ do
            let db = step (withTable "CREATE TABLE uq (id int UNIQUE, name str)") "INSERT INTO uq (id, name) VALUES (1, 'a')"
            runStatement db (Insert "uq" ["id", "name"] [[LitInt 1, LitStr "b"]])
                `shouldSatisfy` either (isInfixOf "UNIQUE") (const False)

        it "enforces CHECK constraints" $ do
            let db = withTable "CREATE TABLE ck (age int CHECK (age > 0))"
            runStatement db (Insert "ck" ["age"] [[LitInt (-1)]])
                `shouldSatisfy` either (isInfixOf "CHECK") (const False)
            vals (step db "INSERT INTO ck (age) VALUES (5)") "SELECT age FROM ck" `shouldBe` Right [[VInt 5]]

        it "validates CHECK types and row-only expressions before creating an empty table" $ do
            mapM_
                (\ddl -> (parseStatement ddl >>= runStatement testDB) `shouldSatisfy` isLeft)
                [ "CREATE TABLE invalid_ck (age int CHECK (age + 1))"
                , "CREATE TABLE invalid_ck (age int CHECK (length(age) > 0))"
                , "CREATE TABLE invalid_ck (age int CHECK (missing(age) > 0))"
                , "CREATE TABLE invalid_ck (age int CHECK (COUNT(*) > 0))"
                , "CREATE TABLE invalid_ck (age int CHECK (EXISTS (SELECT 1)))"
                , "CREATE TABLE invalid_ck (age int CHECK (unknown > 0))"
                ]

        it "validates added CHECKs against existing and new column types" $ do
            let db = withTable "CREATE TABLE add_ck (name text)"
                run input = parseStatement input >>= runStatement db
            run "ALTER TABLE add_ck ADD COLUMN age int CHECK (length(age) > 0)"
                `shouldSatisfy` isLeft
            run "ALTER TABLE add_ck ADD COLUMN age int CHECK (name > 0)"
                `shouldSatisfy` isLeft
            case run "ALTER TABLE add_ck ADD COLUMN age int CHECK (age > length(name))" of
                Left err -> expectationFailure err
                Right (updated, _) -> do
                    vals (step updated "INSERT INTO add_ck (name, age) VALUES ('ab', 3)") "SELECT age FROM add_ck"
                        `shouldBe` Right [[VInt 3]]
                    (parseStatement "INSERT INTO add_ck (name, age) VALUES ('ab', 1)" >>= runStatement updated)
                        `shouldSatisfy` either (isInfixOf "CHECK") (const False)

    describe "ChuSQL.Core.Engine (ALTER TABLE)" $ do
        -- vals：在给定库上跑 SQL 只取值
        let vals db input = fmap (map (map snd)) (parseStatement input >>= rowsOf . runStatement db)
            -- run：在给定库上执行语句
            run db input = parseStatement input >>= runStatement db
            -- withTable：先建表，返回新库
            withTable ddl = either (const testDB) fst (parseStatement ddl >>= runStatement testDB)
            -- step：执行一条语句，失败就返回原库
            step db input = either (const db) fst (run db input)

        it "ADD COLUMN fills existing rows with the default or NULL" $ do
            let seeded = step (withTable "CREATE TABLE ac (id int)") "INSERT INTO ac (id) VALUES (1)"
                withDefault = step seeded "ALTER TABLE ac ADD COLUMN tag str DEFAULT 'n'"
                withNull = step seeded "ALTER TABLE ac ADD COLUMN note str"
            vals withDefault "SELECT tag FROM ac" `shouldBe` Right [[VStr "n"]]
            vals withNull "SELECT note FROM ac" `shouldBe` Right [[VNull]]

        it "rejects ADD COLUMN NOT NULL without a default on a non-empty table" $ do
            let seeded = step (withTable "CREATE TABLE ac2 (id int)") "INSERT INTO ac2 (id) VALUES (1)"
            run seeded "ALTER TABLE ac2 ADD COLUMN req int NOT NULL" `shouldSatisfy` isLeft
            vals (step (withTable "CREATE TABLE ac3 (id int)") "ALTER TABLE ac3 ADD COLUMN req int NOT NULL") "SELECT * FROM ac3" `shouldBe` Right []

        it "RENAME COLUMN moves the data to the new name" $ do
            let seeded = step (withTable "CREATE TABLE rn (id int, name str)") "INSERT INTO rn (id, name) VALUES (1, 'a')"
                renamed = step seeded "ALTER TABLE rn RENAME COLUMN name TO label"
            vals renamed "SELECT label FROM rn" `shouldBe` Right [[VStr "a"]]
            run renamed "SELECT name FROM rn" `shouldSatisfy` isLeft
            run seeded "ALTER TABLE rn RENAME COLUMN nope TO other" `shouldSatisfy` isLeft

        it "ALTER COLUMN TYPE re-reads the stored values" $ do
            let seeded = step (withTable "CREATE TABLE ty (id int)") "INSERT INTO ty (id) VALUES (7)"
                widened = step seeded "ALTER TABLE ty ALTER COLUMN id TYPE bigint"
            vals widened "SELECT id FROM ty" `shouldBe` Right [[VInt 7]]
            let texty = step (withTable "CREATE TABLE ty2 (name str)") "INSERT INTO ty2 (name) VALUES ('x')"
            run texty "ALTER TABLE ty2 ALTER COLUMN name TYPE int" `shouldSatisfy` isLeft

        it "rejects structural changes that invalidate CHECKs even on empty tables" $ do
            let empty = withTable "CREATE TABLE checked_ddl (name text, qty int CHECK (qty > length(name)))"
                filled = step empty "INSERT INTO checked_ddl (name, qty) VALUES ('ab', 3)"
            mapM_
                ( \(db, expected) -> do
                    run db "ALTER TABLE checked_ddl DROP COLUMN name" `shouldSatisfy` isLeft
                    run db "ALTER TABLE checked_ddl RENAME COLUMN name TO label" `shouldSatisfy` isLeft
                    run db "ALTER TABLE checked_ddl ALTER COLUMN name TYPE int" `shouldSatisfy` isLeft
                    run db "ALTER TABLE checked_ddl ALTER COLUMN qty TYPE text" `shouldSatisfy` isLeft
                    vals db "SELECT name, qty FROM checked_ddl"
                        `shouldBe` Right expected
                )
                [(empty, []), (filled, [[VStr "ab", VInt 3]])]
            case run filled "ALTER TABLE checked_ddl DROP COLUMN qty" of
                Left err -> expectationFailure err
                Right (remaining, _) -> vals remaining "SELECT name FROM checked_ddl" `shouldBe` Right [[VStr "ab"]]
            run filled "ALTER TABLE checked_ddl ALTER COLUMN qty TYPE bigint" `shouldSatisfy` isRight

        it "rejects type changes that invalidate defaults or key capabilities" $ do
            let defaults = withTable "CREATE TABLE default_ddl (name text DEFAULT 'hello')"
                keys = withTable "CREATE TABLE key_ddl (qty int UNIQUE)"
            run defaults "ALTER TABLE default_ddl ALTER COLUMN name TYPE int" `shouldSatisfy` isLeft
            run keys "ALTER TABLE key_ddl ALTER COLUMN qty TYPE LIST<Int>" `shouldSatisfy` isLeft

        it "ALTER COLUMN SET/DROP DEFAULT changes what omitted columns get" $ do
            let base = withTable "CREATE TABLE df (id int, tag str)"
                withDefault = step base "ALTER TABLE df ALTER COLUMN tag SET DEFAULT 'y'"
                dropped = step withDefault "ALTER TABLE df ALTER COLUMN tag DROP DEFAULT"
            vals (step withDefault "INSERT INTO df (id) VALUES (1)") "SELECT tag FROM df" `shouldBe` Right [[VStr "y"]]
            vals (step dropped "INSERT INTO df (id) VALUES (2)") "SELECT tag FROM df" `shouldBe` Right [[VNull]]

        it "ALTER COLUMN SET/DROP NOT NULL checks the stored rows" $ do
            let nulls = step (withTable "CREATE TABLE nl (id int, tag str)") "INSERT INTO nl (id) VALUES (1)"
                strict = step (withTable "CREATE TABLE nl2 (id int, tag str NOT NULL)") "INSERT INTO nl2 (id, tag) VALUES (1, 'a')"
            run nulls "ALTER TABLE nl ALTER COLUMN tag SET NOT NULL" `shouldSatisfy` isLeft
            run nulls "ALTER TABLE nl ALTER COLUMN tag DROP NOT NULL" `shouldSatisfy` isRight
            run strict "ALTER TABLE nl2 ALTER COLUMN tag DROP NOT NULL" `shouldSatisfy` isRight

    describe "ChuSQL.Core.Model (值语义唯一权威)" $ do
        it "treats an integral float and an int as the same value" $ do
            valuesEqual (VInt 1) (VFloat 1.0) `shouldBe` True
            compareValue (VInt 1) (VFloat 1.0) `shouldBe` EQ
            hash (VInt 1) `shouldBe` hash (VFloat 1.0)
            hashValue (VInt 1) `shouldBe` hashValue (VFloat 1.0)

        it "keeps null, text and boolean in their own buckets" $ do
            hash VNull `shouldNotBe` hash (VInt 0)
            hash (VInt 1) `shouldNotBe` hash (VStr "1")
            hash (VStr "a") `shouldNotBe` hash (VBool True)

        it "orders null below every value and compares across families" $ do
            compareValue VNull (VInt minBound) `shouldBe` LT
            compareValue (VInt maxBound) VNull `shouldBe` GT
            compareValue VNull VNull `shouldBe` EQ
            compareValue (VInt 1) (VStr "a") `shouldBe` LT
            compareValue (VStr "a") (VBool True) `shouldBe` LT
            compareValue (VBool True) VNull `shouldBe` GT

        it "propagates null through comparisons" $ do
            evalExpr (Eq (Col "v") LitNull) [("v", VInt 1)] `shouldBe` Right VNull
            evalExpr (Gt LitNull (LitInt 1)) [] `shouldBe` Right VNull

        it "uses one comparability rule and one canonical form" $ do
            comparableTypes CInt CFloat `shouldBe` True
            assignable CInt CFloat `shouldBe` True
            assignable CStr CInt `shouldBe` False
            comparableTypes CInt CStr `shouldBe` False
            comparableTypes CStr CDate `shouldBe` True
            canonicalValue (VFloat 3.0) `shouldBe` VInt 3
            canonicalValue (VFloat 3.5) `shouldBe` VFloat 3.5

    describe "比较语义统一（排序 / 聚合 / 哈希连接）" $ do
        -- vals：在给定库上跑 SQL 只取值
        let vals db input = fmap (map (map snd)) (parseStatement input >>= rowsOf . runStatement db)
            -- seed：建一张两列空表
            seed name = case parseStatement ("CREATE TABLE " ++ name ++ " (id int, v int)") >>= runStatement emptyDB of
                Left e -> error e
                Right (db, _) -> db
            -- ins：插入一行，失败直接判测试失败
            ins db input = case parseStatement input >>= runStatement db of
                Left e -> error e
                Right (db', _) -> db'
            rows3 =
                ins
                    (ins (ins (seed "m") "INSERT INTO m (id, v) VALUES (1, 2)") "INSERT INTO m (id, v) VALUES (2, 1)")
                    "INSERT INTO m (id, v) VALUES (3, 2)"
            withNull =
                ins
                    (ins (ins (seed "m2") "INSERT INTO m2 (id, v) VALUES (1, 2)") "INSERT INTO m2 (id) VALUES (2)")
                    "INSERT INTO m2 (id, v) VALUES (3, 1)"

        it "sorts with the same rule that aggregates use" $ do
            map (map snd) (sortRows [("v", Asc)] [[("v", VFloat 1.5)], [("v", VNull)], [("v", VInt 1)]])
                `shouldBe` [[VNull], [VInt 1], [VFloat 1.5]]
            vals rows3 "SELECT v FROM m ORDER BY v, id" `shouldBe` Right [[VInt 1], [VInt 2], [VInt 2]]
            vals rows3 "SELECT v FROM m ORDER BY v DESC, id" `shouldBe` Right [[VInt 2], [VInt 2], [VInt 1]]
            vals rows3 "SELECT MIN(v), MAX(v) FROM m" `shouldBe` Right [[VInt 1, VInt 2]]

        it "keeps the first k rows of the same stable order when limiting a sort" $ do
            let rows = [[("id", VInt n), ("v", VInt (n `mod` 4))] | n <- [1 .. 33]]
                spec = [("v", Asc), ("id", Desc)]
                flat = [("v", Asc)]
                values = map (map snd)
            values (sortRowsTopK spec 5 rows) `shouldBe` take 5 (values (sortRows spec rows))
            values (sortRowsTopK flat 7 rows) `shouldBe` take 7 (values (sortRows flat rows))
            values (sortRowsTopK spec 33 rows) `shouldBe` values (sortRows spec rows)
            values (sortRowsTopK spec 0 rows) `shouldBe` []
            vals rows3 "SELECT id FROM m ORDER BY v LIMIT 2" `shouldBe` Right [[VInt 2], [VInt 1]]

        it "sorts the catalog names that are unknown into one code" $ do
            errorCode "unknown database: d" `shouldBe` "not_found"
            errorCode "unknown role: r" `shouldBe` "not_found"
            errorCode "unknown account" `shouldBe` "not_found"

        it "puts null first when sorting and skips it in aggregates" $ do
            vals withNull "SELECT v FROM m2 ORDER BY v" `shouldBe` Right [[VNull], [VInt 1], [VInt 2]]
            vals withNull "SELECT MIN(v), MAX(v), SUM(v), COUNT(v), COUNT(*) FROM m2"
                `shouldBe` Right [[VInt 1, VInt 2, VInt 3, VInt 2, VInt 3]]

        it "uses 1 and 1.0 as one key in predicates and unique columns" $ do
            let one = ins (seed "f1") "INSERT INTO f1 (id, v) VALUES (1, 1)"
            vals one "SELECT id FROM f1 WHERE v = 1.0" `shouldBe` Right [[VInt 1]]
            vals one "SELECT id FROM f1 WHERE v = 1" `shouldBe` Right [[VInt 1]]
            let uniqueTable = case parseStatement "CREATE TABLE uq (id int, v int UNIQUE)" >>= runStatement emptyDB of
                    Left e -> error e
                    Right (db, _) -> db
                seeded = ins uniqueTable "INSERT INTO uq (id, v) VALUES (1, 1)"
            case parseStatement "INSERT INTO uq (id, v) VALUES (2, 1.0)" >>= runStatement seeded of
                Left err -> err `shouldBe` "UNIQUE: duplicate value in column v"
                Right _ -> expectationFailure "1.0 must clash with 1 on a unique column"

    describe "ChuSQL.Core.Engine.Builtin (解析与调用唯一入口)" $ do
        it "resolves aggregate names case-insensitively" $ do
            resolveBuiltin "COUNT" `shouldBe` Just BCount
            resolveBuiltin "Sum" `shouldBe` Just BSum
            resolveBuiltin "median" `shouldBe` Nothing
            map fst builtinNames `shouldBe` ["count", "sum", "avg", "min", "max"]

        it "invokes every aggregate from one place" $ do
            invokeBuiltin BCountAll 3 [] `shouldBe` Right (VInt 3)
            invokeBuiltin BCount 3 [VInt 1, VNull, VInt 2] `shouldBe` Right (VInt 2)
            invokeBuiltin BSum 0 [] `shouldBe` Right VNull
            invokeBuiltin BSum 3 [VInt 1, VNull, VInt 2] `shouldBe` Right (VInt 3)
            invokeBuiltin BAvg 2 [VInt 1, VInt 3] `shouldBe` Right (VFloat 2)
            invokeBuiltin BMin 2 [VNull, VInt 4] `shouldBe` Right (VInt 4)
            invokeBuiltin BMax 2 [VNull, VInt 4] `shouldBe` Right (VInt 4)

        it "maps names to syntax nodes in one table" $ do
            builtinOfExpr CountAll `shouldBe` Just BCountAll
            builtinNode BCount (Col "age") `shouldBe` CountOf (Col "age")
            builtinNode BAvg (Col "age") `shouldBe` AvgOf (Col "age")
            aggregateArg (SumOf (Col "age")) `shouldBe` Just (Col "age")
            aggregateArg CountAll `shouldBe` Nothing
            unsupportedAggregate `shouldBe` "only COUNT, SUM, AVG, MIN and MAX can be aggregated"

        it "exposes one operator table and one executor" $ do
            operatorSymbol <$> operatorOfExpr (Add (LitInt 1) (LitInt 2)) `shouldBe` Just "+"
            operatorSymbol <$> operatorOfExpr (And (LitBool True) (LitBool False)) `shouldBe` Just "AND"
            executeOperator OpAdd [VInt 1, VInt 2] `shouldBe` Right (VInt 3)
            executeOperator OpDiv [VInt 1, VInt 0] `shouldBe` Left "division by zero"
            executeOperator OpNeg [VFloat 2] `shouldBe` Right (VFloat (-2))
            executeOperator OpEq [VInt 1, VFloat 1.0] `shouldBe` Right (VBool True)
            executeOperator OpGt [VNull, VInt 1] `shouldBe` Right VNull
            executeOperator OpAnd [VBool True, VNull] `shouldBe` Right VNull
            executeOperator OpOr [VBool True, VNull] `shouldBe` Right (VBool True)
            executeOperator OpAdd [VInt 1] `shouldSatisfy` isLeft
            executeOperator OpNeg [VInt 1, VInt 2] `shouldSatisfy` isLeft

        it "keeps null comparisons three-valued" $ do
            inValues VNull [VInt 1] `shouldBe` Right VNull
            inValues (VInt 1) [VInt 1, VNull] `shouldBe` Right (VBool True)
            inValues (VInt 2) [VInt 1, VNull] `shouldBe` Right VNull
            threeValuedNot VNull `shouldBe` VNull
            threeValuedNot (VBool True) `shouldBe` VBool False

    describe "ChuSQL.Core.Engine.Parallel (分片调度与分片选择)" $ do
        it "keeps the submission order of finished jobs" $ do
            pool <- workerPool 4
            results <- poolRun pool [delayed 30000 (1 :: Int), delayed 1000 2, delayed 20000 3]
            results `shouldBe` [1, 2, 3]

        it "dispatches every job without waiting for the slow one" $ do
            pool <- workerPool 4
            finished <- timeout 800000 (poolRun pool (replicate 4 (delayed 300000 ())))
            finished `shouldBe` Just (replicate 4 ())

        it "rethrows job failures in the submission order" $ do
            pool <- workerPool 4
            let failed = error "first bad job" :: IO ()
                later = error "second bad job" :: IO ()
            outcome <- try (poolRun pool (pure () : failed : [later])) :: IO (Either ErrorCall [()])
            case outcome of
                Left err -> show err `shouldContain` "first bad job"
                Right _ -> expectationFailure "poolRun returned instead of rethrowing the job failure"

        it "scans a small table in one sequential request" $ do
            probe <- newProbe users 4
            let op = Scan Nothing "users" Nothing
            runProbeStorage (evalRelOpM testDB op) probe `shouldReturn` evalRelOp testDB op
            recordedCalls probe `shouldReturn` ["scan:users"]

        it "splits a large table into the available shards" $ do
            probe <- newProbe bigTable 4
            let op = Scan Nothing "big" Nothing
            runProbeStorage (evalRelOpM bigDB op) probe `shouldReturn` evalRelOp bigDB op
            recordedCalls probe `shouldReturn` ["scan_shards:4:big"]

        it "projects columns through the shard path" $ do
            probe <- newProbe bigTable 3
            let op = Scan Nothing "big" (Just ["name"])
            runProbeStorage (evalRelOpM bigDB op) probe `shouldReturn` evalRelOp bigDB op
            recordedCalls probe `shouldReturn` ["scan_shards:3:big"]

        it "reads shards of one table through the wire" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                let rows = [[("id", VInt i), ("name", VStr ("n" ++ show i))] | i <- [1 .. 400 :: Int]]
                runIPCStorage (insertMany "wide" rows) `shouldReturn` Right ()
                whole <- runIPCStorage (scan "wide")
                fmap length whole `shouldBe` Right 400
                shardRows <- runIPCStorage (scanShards 4 "wide" Nothing)
                shardRows `shouldBe` whole
                projected <- runIPCStorage (scanShards 4 "wide" (Just ["name"]))
                fmap (map (map fst)) projected `shouldBe` Right (map (const ["name"]) rows)
                fmap (concatMap (map snd)) projected
                    `shouldBe` Right (map (VStr . ("n" ++) . show) [1 .. 400 :: Int])

    describe "ChuSQL.Core.Engine.Error (错误分类唯一入口)" $ do
        it "maps messages to wire codes without changing them" $ do
            errorCode "no database selected" `shouldBe` "no_database"
            errorCode "administrator required" `shouldBe` "forbidden"
            errorCode "unknown table: t" `shouldBe` "not_found"
            errorCode "unknown column: c" `shouldBe` "query_error"
            errorCode "type error: expected two numbers" `shouldBe` "query_error"
            errorCode "something else" `shouldBe` "query_error"

        it "keeps the account service codes in the same module" $ do
            accountErrorCode "account already exists" `shouldBe` "conflict"
            accountErrorCode "unknown account" `shouldBe` "not_found"
            accountErrorCode "disk gone" `shouldBe` "storage_error"

        it "classifies the same messages into categories" $ do
            errorCategory "no database selected" `shouldBe` NoDatabaseError
            errorCategory "administrator required" `shouldBe` PermissionError
            errorCategory "unknown table: t" `shouldBe` CatalogError
            errorCategory "type error: expected two numbers" `shouldBe` TypeError
            errorCategory "storage library call failed: x" `shouldBe` StorageError
            errorCategory "unsupported protocol version" `shouldBe` ProtocolError
            errorCategory "something else" `shouldBe` UnknownError

    describe "Worker pool capacity" $ do
        it "grows the existing queue so old references use the additional workers" $ do
            original <- workerPool 4
            _ <- workerPool 8
            started <- newChan
            release <- newEmptyMVar
            finished <- newEmptyMVar
            void (forkIO (poolRun original (replicate 8 (writeChan started () >> readMVar release)) >> putMVar finished ()))
            flip finally (void (tryPutMVar release ())) $ do
                timeout 1000000 (replicateM_ 8 (readChan started)) `shouldReturn` Just ()
                putMVar release ()
                timeout 1000000 (takeMVar finished) `shouldReturn` Just ()

-- | 每个测试表都清空行后的库
emptyDB :: Database
emptyDB = [(n, t{tableRows = []}) | (n, t) <- testDB]

-- | 一次进程内存储会话：进程内直连存储动态库
newtype StorageSession = StorageSession
    { dataDir :: FilePath
    }

-- | 写一份只属于这次会话的配置：数据目录与日志级别都从文件走
writeSessionConfig :: FilePath -> IO FilePath
writeSessionConfig dir = do
    let cfgPath = dir ++ ".toml"
        -- TOML 的普通字符串会吃反斜杠，路径统一用正斜杠
        slashed = map (\c -> if c == '\\' then '/' else c) dir
    writeFile
        cfgPath
        ( unlines
            [ "[storage]"
            , "data_dir = \"" ++ slashed ++ "\""
            , "[log]"
            , "level = \"debug\""
            ]
        )
    pure cfgPath

-- | 打开一份存储，并把它装成引擎当前用的链路
openSessionAt :: FilePath -> IO (Either String StorageSession)
openSessionAt dir = do
    cfgPath <- writeSessionConfig dir
    opened <- localStorageLink (Just cfgPath)
    case opened of
        Left e -> pure (Left ("cannot open the chusql_core_storage library: " ++ e))
        Right link -> do
            setStorageLink link
            pure (Right (StorageSession dir))

-- | 用同一份数据目录再开一次（模拟重启）
openSessionIn :: FilePath -> IO ()
openSessionIn dir = openSessionAt dir >>= either fail (const (pure ()))

-- | 每次调用都不同的后缀：CPU 时间加唯一编号
uniqueStamp :: IO String
uniqueStamp = do
    cpu <- getCPUTime
    u <- hashUnique <$> newUnique
    pure (show cpu ++ "-" ++ show u)

-- | 删掉会话的数据目录与配置文件
cleanSessionDir :: FilePath -> IO ()
cleanSessionDir dir = do
    _ <- try (removePathForcibly dir) :: IO (Either IOException ())
    _ <- try (removePathForcibly (dir ++ ".toml")) :: IO (Either IOException ())
    pure ()

-- | 每例独立目录，存储打开失败则测试失败
withTestSession :: (StorageSession -> IO ()) -> IO ()
withTestSession act = do
    tmp <- getTemporaryDirectory
    stamp <- uniqueStamp
    let dir = tmp </> "chusql-hs-IPC" </> stamp
    createDirectoryIfMissing True dir
    opened <- openSessionAt dir
    case opened of
        Left err -> cleanSessionDir dir >> expectationFailure err
        Right session -> act session `finally` (closeConnection >> cleanSessionDir dir)

-- | 语句不带库名，先建好测试库再切进去
withServerEnv :: StorageSession -> IO a -> IO a
withServerEnv _srv act = do
    oldDb <- getDatabaseName
    ensureTestDatabase
    setDatabaseName (Just testDatabase)
    act `finally` setDatabaseName oldDb

-- | IPC 用例使用的库名
testDatabase :: String
testDatabase = "ipctest"

-- | 保证测试库已存在
ensureTestDatabase :: IO ()
ensureTestDatabase = do
    listed <- runIPCStorage listDatabases
    case listed of
        Left e -> fail ("cannot list databases through the storage library: " ++ e)
        Right names
            | testDatabase `elem` names -> pure ()
            | otherwise -> do
                created <- runIPCStorage (createDatabase testDatabase)
                case created of
                    Right () -> pure ()
                    Left e -> fail ("cannot create the test database: " ++ e)

-- | 按列名把一行排序
sortRow :: Row -> Row
sortRow = sortOn fst

-- | 取出索引点查返回的那一行，取不到给 Nothing
indexRow :: Either String IndexResult -> Maybe Row
indexRow (Right (IndexRows (r : _))) = Just r
indexRow _ = Nothing

-- | 判断 Either 是 Left
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

-- | 延时后返回一个值
delayed :: Int -> a -> IO a
delayed micros value = threadDelay micros >> pure value

-- | 造一个记录调用序列的探针
newProbe :: Table -> Int -> IO Probe
newProbe table width = do
    calls <- newMVar []
    pure Probe{probeTable = table, probeCalls = calls, probeWidth = width}

-- | 记一条存储调用
recordCall :: Probe -> String -> IO ()
recordCall probe name = modifyMVar_ (probeCalls probe) (\calls -> pure (calls ++ [name]))

-- | 取回调用序列
recordedCalls :: Probe -> IO [String]
recordedCalls = readMVar . probeCalls

-- | 按列投影（空清单保留行）
projectColumns :: [String] -> [Row] -> [Row]
projectColumns cols = map (\row -> [(c, v) | c <- cols, Just v <- [lookup c row]])

-- | 按行数切成连续分片
rowShards :: Int -> [a] -> [[a]]
rowShards width rows
    | width <= 1 = [rows]
    | otherwise = go rows
  where
    -- \| 每片行数
    chunk = (length rows + width - 1) `div` width
    -- \| 反复切出前一片
    go rest
        | null rest = []
        | otherwise = let (part, more) = splitAt chunk rest in part : go more

-- | 声明 1000 行的大表
bigTable :: Table
bigTable = users{tableName = "big", tableMeta = Just (TableMeta 1000 [("id", 1000, False)] [] ["id"])}

-- | 只装大表的库
bigDB :: Database
bigDB = [("big", bigTable)]

-- | 记录存储调用、按行分片的假存储
data Probe = Probe
    { probeTable :: Table
    , probeCalls :: MVar [String]
    , probeWidth :: Int
    }

-- | 探针单子
newtype ProbeStorage a = ProbeStorage {runProbeStorage :: Probe -> IO a}

instance Functor ProbeStorage where
    fmap f (ProbeStorage act) = ProbeStorage (fmap f . act)

instance Applicative ProbeStorage where
    pure value = ProbeStorage (\_ -> pure value)
    ProbeStorage fn <*> ProbeStorage act = ProbeStorage (\probe -> fn probe <*> act probe)

instance Monad ProbeStorage where
    ProbeStorage act >>= next =
        ProbeStorage (\probe -> act probe >>= \value -> runProbeStorage (next value) probe)

instance MonadStorage ProbeStorage where
    -- \| 测试视图不支持并发结构变更
    withSchemaGuard _ action = action

    -- \| 测试视图不支持并发结构变更
    withWriteSchemaGuard _ action = action
    scan table = ProbeStorage $ \probe -> do
        recordCall probe ("scan:" ++ table)
        pure (Right (tableRows (probeTable probe)))

    scanColumns table columns = ProbeStorage $ \probe -> do
        recordCall probe ("scan_columns:" ++ table)
        pure (Right (projectColumns columns (tableRows (probeTable probe))))

    parallelShards = ProbeStorage (pure . probeWidth)

    scanShards width table columns = ProbeStorage $ \probe -> do
        recordCall probe ("scan_shards:" ++ show width ++ ":" ++ table)
        let rows = maybe id projectColumns columns (tableRows (probeTable probe))
        pure (Right (concat (rowShards width rows)))

    insert _ _ = ProbeStorage (\_ -> pure (Left "probe: insert is not supported"))
    replaceAll _ _ = ProbeStorage (\_ -> pure (Left "probe: replaceAll is not supported"))
    createTable _ _ = ProbeStorage (\_ -> pure (Left "probe: createTable is not supported"))
    dropTable _ = ProbeStorage (\_ -> pure (Left "probe: dropTable is not supported"))
    dropColumn _ _ = ProbeStorage (\_ -> pure (Left "probe: dropColumn is not supported"))
    replaceSchema _ _ _ = ProbeStorage (\_ -> pure (Left "probe: replaceSchema is not supported"))
    snapshot = ProbeStorage (\_ -> pure (Right []))

-- | 判断 Either 是 Right
isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

-- | 断言优化前后执行结果一致
sameResultAsUnoptimized :: String -> Expectation
sameResultAsUnoptimized sql =
    case parseStatement sql >>= prepare testDB of
        Left err -> expectationFailure err
        Right q ->
            case translate q of
                Left err -> expectationFailure err
                Right relOp ->
                    evalRelOp testDB (optimize testDB relOp) `shouldBe` evalRelOp testDB relOp

-- | 判断谓词是否下推到 Join 左子树
filterPushedIntoLeft :: RelOp -> Bool
filterPushedIntoLeft (Project _ (Join _ l _ _)) = pushedIntoLeft l
filterPushedIntoLeft _ = False

-- | 左侧子树里出现了下推下来的谓词（点查 / 范围 / 过滤）
pushedIntoLeft :: RelOp -> Bool
pushedIntoLeft (Filter _ _) = True
pushedIntoLeft (Range{}) = True
pushedIntoLeft (Project _ x) = pushedIntoLeft x
pushedIntoLeft _ = False

-- | 解析并优化 SQL，返回计划
optimizedPlan :: String -> Either String RelOp
optimizedPlan sql = do
    q <- parseStatement sql
    optimize testDB <$> translate q

-- | 在给定库上解析并优化 SQL，返回计划
optimizedPlanIn :: Database -> String -> Either String RelOp
optimizedPlanIn db sql = do
    q <- parseStatement sql
    optimize db <$> translate q

-- | 计划里出现的表名（先序）
planTables :: RelOp -> [String]
planTables op = case op of
    Scan _ t _ -> [t]
    Lookup _ t _ _ -> [t]
    Range _ t _ _ _ -> [t]
    Filter _ x -> planTables x
    Project _ x -> planTables x
    Compute _ x -> planTables x
    Aggregate _ _ x -> planTables x
    BoundAggregate _ _ x -> planTables x
    Distinct x -> planTables x
    Sort _ x -> planTables x
    BoundSort _ x -> planTables x
    Limit _ x -> planTables x
    Join _ l r _ -> planTables l ++ planTables r
    Derived _ x -> planTables x
    Unit -> []

-- | 计划里每个 Join 节点是不是 LEFT JOIN（先序）
leftJoinKinds :: RelOp -> [Bool]
leftJoinKinds (Join kind l r _) = (kind == LeftJoin) : (leftJoinKinds l ++ leftJoinKinds r)
leftJoinKinds (Project _ x) = leftJoinKinds x
leftJoinKinds (Compute _ x) = leftJoinKinds x
leftJoinKinds (Aggregate _ _ x) = leftJoinKinds x
leftJoinKinds (BoundAggregate _ _ x) = leftJoinKinds x
leftJoinKinds (Filter _ x) = leftJoinKinds x
leftJoinKinds (Distinct x) = leftJoinKinds x
leftJoinKinds (Sort _ x) = leftJoinKinds x
leftJoinKinds (BoundSort _ x) = leftJoinKinds x
leftJoinKinds (Limit _ x) = leftJoinKinds x
leftJoinKinds (Derived _ x) = leftJoinKinds x
leftJoinKinds _ = []

-- | 取计划里第一个过滤条件
firstFilterCond :: RelOp -> Maybe Expr
firstFilterCond (Filter p _) = Just p
firstFilterCond (Project _ x) = firstFilterCond x
firstFilterCond (Compute _ x) = firstFilterCond x
firstFilterCond (Aggregate _ _ x) = firstFilterCond x
firstFilterCond (BoundAggregate _ _ x) = firstFilterCond x
firstFilterCond Unit = Nothing
firstFilterCond (Distinct x) = firstFilterCond x
firstFilterCond (Sort _ x) = firstFilterCond x
firstFilterCond (BoundSort _ x) = firstFilterCond x
firstFilterCond (Limit _ x) = firstFilterCond x
firstFilterCond (Join _ l r _) = case firstFilterCond l of
    Just p -> Just p
    Nothing -> firstFilterCond r
firstFilterCond (Scan{}) = Nothing
firstFilterCond (Lookup{}) = Nothing
firstFilterCond (Range{}) = Nothing
firstFilterCond (Derived _ x) = firstFilterCond x

-- | 判断计划里是否还有过滤节点
anyFilter :: RelOp -> Bool
anyFilter (Filter _ _) = True
anyFilter (Project _ x) = anyFilter x
anyFilter (Compute _ x) = anyFilter x
anyFilter (Aggregate _ _ x) = anyFilter x
anyFilter (BoundAggregate _ _ x) = anyFilter x
anyFilter Unit = False
anyFilter (Distinct x) = anyFilter x
anyFilter (Sort _ x) = anyFilter x
anyFilter (BoundSort _ x) = anyFilter x
anyFilter (Limit _ x) = anyFilter x
anyFilter (Join _ l r _) = anyFilter l || anyFilter r
anyFilter (Scan{}) = False
anyFilter (Lookup{}) = False
anyFilter (Range{}) = False
anyFilter (Derived _ x) = anyFilter x

-- | 去掉计划里的投影节点
stripProjects :: RelOp -> RelOp
stripProjects (Project _ x) = stripProjects x
stripProjects (Compute items x) = Compute items (stripProjects x)
stripProjects (Aggregate keys aggs x) = Aggregate keys aggs (stripProjects x)
stripProjects (BoundAggregate keys aggs x) = BoundAggregate keys aggs (stripProjects x)
stripProjects Unit = Unit
stripProjects (Filter p x) = Filter p (stripProjects x)
stripProjects (Distinct x) = Distinct (stripProjects x)
stripProjects (Sort spec x) = Sort spec (stripProjects x)
stripProjects (BoundSort spec x) = BoundSort spec (stripProjects x)
stripProjects (Limit n x) = Limit n (stripProjects x)
stripProjects (Join k l r c) = Join k (stripProjects l) (stripProjects r) c
stripProjects (Scan a t _) = Scan a t Nothing
stripProjects (Lookup a t c k) = Lookup a t c k
stripProjects (Range a t c lo hi) = Range a t c lo hi
stripProjects (Derived a x) = Derived a (stripProjects x)

-- | 解析并做投影下推，返回计划
projectedPlan :: String -> Either String RelOp
projectedPlan sql = do
    q <- parseStatement sql
    plan <- translate q
    Right (pushProject testDB ["*"] plan)

-- | 取节点输出的列名
sideCols :: RelOp -> Maybe [String]
sideCols (Project cols _) = Just cols
sideCols (Scan a _ cols) = fmap (map (qualify a)) cols
sideCols _ = Nothing

-- | 取 Join 左右两侧的输出列名
joinSideCols :: RelOp -> (Maybe [String], Maybe [String])
joinSideCols (Project _ (Join _ l r _)) = (sideCols l, sideCols r)
joinSideCols _ = (Nothing, Nothing)

-- | 数左子树里过滤节点的个数
countFilters :: RelOp -> Int
countFilters (Filter _ x) = 1 + countFilters x
countFilters _ = 0

-- | 取 Join 左侧过滤节点数
leftSideFilters :: RelOp -> Maybe Int
leftSideFilters (Project _ (Join _ l _ _)) = Just (countFilters l)
leftSideFilters _ = Nothing

-- | 去掉外层投影，返回真正的根节点
planRoot :: RelOp -> RelOp
planRoot (Project _ x) = planRoot x
planRoot x = x

-- | 记下扫过哪些表的存储：语义与内存实现一致，只是扫描多记一笔
newtype Counting a = Counting {runCounting :: Database -> Either String (a, Database, [String])}

instance Functor Counting where
    fmap f (Counting m) = Counting $ \db -> fmap (\(a, db', log') -> (f a, db', log')) (m db)

instance Applicative Counting where
    pure a = Counting $ \db -> Right (a, db, [])
    Counting mf <*> Counting ma = Counting $ \db -> do
        (f, db1, l1) <- mf db
        (a, db2, l2) <- ma db1
        Right (f a, db2, l1 ++ l2)

instance Monad Counting where
    Counting m >>= k = Counting $ \db -> do
        (a, db1, l1) <- m db
        (b, db2, l2) <- runCounting (k a) db1
        Right (b, db2, l1 ++ l2)

-- | 内存实现的某个动作，转发给 Counting
memory :: MemoryStorage a -> Counting a
memory m = Counting $ \db -> case runMemoryStorage m db of
    Left e -> Left e
    Right (a, db') -> Right (a, db', [])

-- | 内存实现的扫描
memScan :: String -> MemoryStorage (Either String [Row])
memScan = scan

-- | 内存实现的插入
memInsert :: String -> Row -> MemoryStorage (Either String ())
memInsert = insert

-- | 内存实现的整表替换
memReplaceAll :: String -> [Row] -> MemoryStorage (Either String ())
memReplaceAll = replaceAll

-- | 内存实现的建表
memCreateTable :: String -> [(String, Column)] -> MemoryStorage (Either String ())
memCreateTable = createTable

-- | 内存实现的删表
memDropTable :: String -> MemoryStorage (Either String ())
memDropTable = dropTable

-- | 内存实现的删列
memDropColumn :: String -> String -> MemoryStorage (Either String ())
memDropColumn = dropColumn

-- | 内存实现的换结构
memReplaceSchema :: String -> [(String, Column)] -> [Row] -> MemoryStorage (Either String ())
memReplaceSchema = replaceSchema

instance MonadStorage Counting where
    -- \| 纯测试状态在求值期间保持稳定
    withSchemaGuard _ action = action

    -- \| 纯测试状态不受外部结构变更影响
    withWriteSchemaGuard _ action = action

    -- \| 扫描：记一笔表名，其余走内存实现
    scan t = Counting $ \db -> case runMemoryStorage (memScan t) db of
        Left e -> Left e
        Right (r, db') -> Right (r, db', [t])

    -- \| 插入：走内存实现
    insert t r = memory (memInsert t r)

    -- \| 整表替换：走内存实现
    replaceAll t rs = memory (memReplaceAll t rs)

    -- \| 建表：走内存实现
    createTable name cols = memory (memCreateTable name cols)

    -- \| 删表：走内存实现
    dropTable name = memory (memDropTable name)

    -- \| 删列：走内存实现
    dropColumn name col = memory (memDropColumn name col)

    -- \| 换结构：走内存实现
    replaceSchema name cols rows = memory (memReplaceSchema name cols rows)

    -- \| 快照：把当前库原样交出去
    snapshot = Counting $ \db -> Right (Right db, db, [])

-- | 解析一条 SQL 并在计数存储上跑，返回结果与扫描过的表
countedRun :: String -> (Either String [Row], [String])
countedRun input = case parseStatement input of
    Left err -> (Left err, [])
    Right stmt -> case runCounting (runStatementM stmt) testDB of
        Left err -> (Left err, [])
        Right (result, _, scans) -> (result, scans)

-- | 结果里出现了几张表被扫过几次
scanTimes :: String -> [String] -> Int
scanTimes t scans = length [name | name <- scans, name == t]
