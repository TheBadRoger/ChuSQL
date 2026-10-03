{-# LANGUAGE CPP #-}

module Main where

import ChuSQL.Core.Engine.Algebra.Eval (evalRelOp, evalRelOpM)
import ChuSQL.Core.Engine.Algebra.Expr (colsInExpr, evalExpr)
import ChuSQL.Core.Engine.Algebra.Op (RelOp (..), renderPlan)
import ChuSQL.Core.Engine.Algebra.Optimize (optimize, pushProject)
import ChuSQL.Core.Engine.Algebra.Planner (translate)
import ChuSQL.Core.Engine.Algebra.Sort (sortRows)
import ChuSQL.Core.Engine.Builtin
import ChuSQL.Core.Engine.Error
import ChuSQL.Core.Engine
import ChuSQL.Core.Engine.Parallel (poolRun, workerPool)
import ChuSQL.Core.Model
import ChuSQL.Core.Engine.Semantic (prepare)
import ChuSQL.Core.Engine.Storage (IndexResult (..), MonadStorage (..))
import ChuSQL.Core.Engine.Storage.IPC (IPCStorage (..), closeConnection, defaultEnv, doListTables, envForDatabase, getDatabaseName, localStorageLink, runIPCStorage, setDatabaseName, setStorageLink)
import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Engine.Syntax.Parser
import Control.Concurrent (threadDelay)
import Control.Concurrent.MVar (MVar, modifyMVar_, newMVar, readMVar)
import Control.Exception (ErrorCall, IOException, finally, try)
import Data.Hashable (hash)
import Data.List (isInfixOf, sortOn)
import Data.Unique (hashUnique, newUnique)
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import System.Timeout (timeout)
import Test.Hspec

-- 引擎层的 hspec 测试：语法、执行、优化器与存储链路行为。

-- | 测试表 users：id / name / age
users :: Table
users =
    Table
        { tableName = "users"
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
        { tableName = "orders"
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
        { tableName = "a"
        , tableCols = [("id", TInt), ("name", TStr)]
        , tableRows = [[("id", VInt n), ("name", VStr ("a" ++ show n))] | n <- [1 .. 6]]
        , tableMeta = Just (TableMeta 6 [("id", 6, False)] ["id"])
        }

-- | 连接链测试表 b：4 行，a_id 各不相同
chainB :: Table
chainB =
    Table
        { tableName = "b"
        , tableCols = [("id", TInt), ("a_id", TInt)]
        , tableRows = [[("id", VInt n), ("a_id", VInt n)] | n <- [1 .. 4]]
        , tableMeta = Just (TableMeta 4 [("id", 4, False), ("a_id", 4, False)] ["id"])
        }

-- | 连接链测试表 c：2 行，b_id 各不相同
chainC :: Table
chainC =
    Table
        { tableName = "c"
        , tableCols = [("id", TInt), ("b_id", TInt)]
        , tableRows = [[("id", VInt n), ("b_id", VInt n)] | n <- [1, 2]]
        , tableMeta = Just (TableMeta 2 [("id", 2, False), ("b_id", 2, False)] ["id"])
        }

-- | 三张表组成的连接链测试库
chainDB :: Database
chainDB = [("a", chainA), ("b", chainB), ("c", chainC)]

-- | 同一个连接链测试库，但没有统计
chainDBWithoutStats :: Database
chainDBWithoutStats = [(n, t { tableMeta = Nothing }) | (n, t) <- chainDB]

-- | 跑引擎层全部 hspec 用例
main :: IO ()
main = hspec $ do
    describe "ChuSQL.Core.Engine.Syntax.Parser" $ do
        it "resolves table-qualified columns inside database-qualified tables" $ do
            let db = [("sales.users", users)]
            rowsOf (parseStatement "SELECT users.name FROM sales.users WHERE users.id = 1" >>= runStatement db)
                `shouldBe` Right [[("name", VStr "Alice")]]
            rowsOf (parseStatement "SELECT sales.users.name FROM sales.users WHERE sales.users.id = 1" >>= runStatement db)
                `shouldBe` Right [[("name", VStr "Alice")]]

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
                        { selectCols = ["name"]
                        , selectFrom =
                            FromJoin
                                InnerJoin
                                (FromTable Nothing "users")
                                (FromTable Nothing "orders")
                                (Eq (Col "id") (Col "user_id"))
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectOrderBy = []
                        , selectLimit = Nothing
                        }
                    )

        it "parses JOIN with aliases" $ do
            parseStatement "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id"
                `shouldBe` Right
                    ( Select
                        { selectCols = ["u.name"]
                        , selectFrom =
                            FromJoin
                                InnerJoin
                                (FromTable (Just "u") "users")
                                (FromTable (Just "o") "orders")
                                (Eq (Col "u.id") (Col "o.user_id"))
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectOrderBy = []
                        , selectLimit = Nothing
                        }
                    )

        it "parses LEFT JOIN" $ do
            parseStatement "SELECT u.name FROM users u LEFT JOIN orders o ON u.id = o.user_id"
                `shouldBe` Right
                    ( Select
                        { selectCols = ["u.name"]
                        , selectFrom =
                            FromJoin
                                LeftJoin
                                (FromTable (Just "u") "users")
                                (FromTable (Just "o") "orders")
                                (Eq (Col "u.id") (Col "o.user_id"))
                        , selectWhere = Nothing
                        , selectGroupBy = []
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
                        { selectCols = ["d.name"]
                        , selectFrom =
                            FromSubquery
                                (Just "d")
                                (Select ["name"] (FromTable Nothing "users") Nothing [] [] Nothing)
                        , selectWhere = Nothing
                        , selectGroupBy = []
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
                        (FromSubquery (Just "d") (Select ["name"] (FromTable Nothing "users") Nothing [] [] Nothing))
                        (Eq (Col "u.name") (Col "d.name"))
                    )

        it "parses a projection alias" $ do
            parseStatement "SELECT count(*) AS total FROM orders"
                `shouldBe` Right
                    ( SelectExpr
                        [("total", CountAll)]
                        (FromTable Nothing "orders")
                        Nothing
                        []
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
                        { selectCols = ["t.name"]
                        , selectFrom =
                            FromSubquery
                                (Just "t")
                                (Select ["name"] (FromTable Nothing "users") Nothing [] [] Nothing)
                        , selectWhere = Nothing
                        , selectGroupBy = []
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
                        `shouldSatisfy` \r -> case r of
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

        it "refuses to invent roles in the query engine (the web services own them)" $ do
            (parseStatement "CREATE ROLE analyst" >>= runStatement testDB) `shouldSatisfy` isLeft
            (parseStatement "DROP ROLE analyst" >>= runStatement testDB) `shouldSatisfy` isLeft
            (parseStatement "GRANT SELECT ON users TO analyst" >>= runStatement testDB) `shouldSatisfy` isLeft
            (parseStatement "REVOKE SELECT ON users FROM analyst" >>= runStatement testDB) `shouldSatisfy` isLeft
            (parseStatement "GRANT analyst TO alice" >>= runStatement testDB) `shouldSatisfy` isLeft
            (parseStatement "REVOKE analyst FROM alice" >>= runStatement testDB) `shouldSatisfy` isLeft

        it "user is reserved as an alias but still usable inside a longer name" $ do
            parseStatement "SELECT * FROM users user" `shouldSatisfy` isLeft
            parseStatement "SELECT user1 FROM users" `shouldBe` Right (Select ["user1"] (FromTable Nothing "users") Nothing [] [] Nothing)

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
        it "parses ORDER BY DESC" $ do
            case parseStatement "SELECT name FROM users ORDER BY age DESC" of
                Right q -> selectOrderBy q `shouldBe` [("age", Desc)]
                Left err -> expectationFailure err

        it "defaults to ASC" $ do
            case parseStatement "SELECT name FROM users ORDER BY age" of
                Right q -> selectOrderBy q `shouldBe` [("age", Asc)]
                Left err -> expectationFailure err

        it "parses multiple ORDER BY columns" $ do
            case parseStatement "SELECT name FROM users ORDER BY age DESC, name ASC" of
                Right q -> selectOrderBy q `shouldBe` [("age", Desc), ("name", Asc)]
                Left err -> expectationFailure err

        it "executes ORDER BY age ASC" $ do
            let q =
                    Select
                        { selectCols = ["name"]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectOrderBy = [("age", Asc)]
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
                        { selectCols = ["name"]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectOrderBy = [("age", Desc)]
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
                    selectOrderBy q `shouldBe` [("age", Desc)]
                    selectLimit q `shouldBe` Just 1
                Left err -> expectationFailure err

        it "executes LIMIT without ORDER BY" $ do
            let q =
                    Select
                        { selectCols = ["name"]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectGroupBy = []
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
                        { selectCols = ["name"]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectOrderBy = [("age", Desc)]
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
                        { selectCols = ["name"]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectGroupBy = []
                        , selectOrderBy = []
                        , selectLimit = Just 0
                        }
            rowsOf (runStatement testDB q)
                `shouldBe` Right []

        it "rejects negative LIMIT" $ do
            parseStatement "SELECT * FROM users LIMIT -1"
                `shouldSatisfy` isLeft

    describe "ChuSQL.Core.Engine (JOIN)" $ do
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
                    { selectCols = ["u.name"]
                    , selectFrom =
                        FromJoin
                            InnerJoin
                            (FromTable (Just "u") "users")
                            (FromTable (Just "x") "nonexistent")
                            (Eq (Col "u.id") (Col "x.id"))
                    , selectWhere = Nothing
                    , selectGroupBy = []
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
            let db = [("users", users), ("orders", orders {tableRows = []})]
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
            let db = [(n, if n == "users" then t {tableRows = [[("id", VInt 1), ("name", VStr "a"), ("age", VNull)]]} else t) | (n, t) <- testDB]
            fmap (map (map snd)) (sqlDb db "SELECT COUNT(age), COUNT(*) FROM users")
                `shouldBe` Right [[VInt 0, VInt 1]]

        it "sums, averages and takes extremes" $ do
            vals "SELECT SUM(age), AVG(age), MIN(age), MAX(age) FROM users"
                `shouldBe` Right [[VInt 72, VFloat 24, VInt 17, VInt 30]]

        it "ignores nulls in the aggregates that skip them" $ do
            let db = [(n, if n == "users" then t {tableRows = [[("id", VInt 1), ("name", VStr "a"), ("age", VNull)], [("id", VInt 2), ("name", VStr "b"), ("age", VInt 4)]]} else t) | (n, t) <- testDB]
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
            let empty = [(n, if n == "users" then t {tableRows = []} else t) | (n, t) <- testDB]
            fmap (map (map snd)) (sqlDb empty "SELECT COUNT(*), SUM(age) FROM users")
                `shouldBe` Right [[VInt 0, VNull]]

        it "returns no groups for an empty table when grouping" $ do
            let empty = [(n, if n == "users" then t {tableRows = []} else t) | (n, t) <- testDB]
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
            let stat rows distinct indexes = TableMeta rows [(c, d, False) | (c, d) <- distinct] indexes
                dbWith m = [("users", Table "users" [("id", TInt), ("name", TStr)] [] (Just m))]
                unknown = [("users", Table "users" [("id", TInt), ("name", TStr)] [] Nothing)]
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
                mapM_ (\sql -> query sql `shouldReturn` Right [])
                    [ "CREATE TABLE reused (id int, secret str)"
                    , "INSERT INTO reused (id, secret) VALUES (1, 'old')"
                    , "ALTER TABLE reused DROP COLUMN secret"
                    , "ALTER TABLE reused ADD COLUMN secret str DEFAULT 'new'"
                    ]
                query "SELECT secret FROM reused" `shouldReturn` Right [[("secret", VStr "new")]]
                query "SELECT secret FROM reused WHERE id = 1" `shouldReturn` Right [[("secret", VStr "new")]]

        it "scan returns rows with all three value types after insert" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                let row = [("id", VInt 7), ("name", VStr "Alice"), ("flag", VBool True)]
                runIPCStorage (insert "users" row) `shouldReturn` Right ()
                rows <- runIPCStorage (scan "users")
                (map sortRow <$> rows) `shouldBe` Right [sortRow row]

        it "scan on a missing table returns Left" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                result <- runIPCStorage (scan "no_such_table")
                result `shouldSatisfy` isLeft

        it "scan returns all 20 inserted rows in order" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                mapM_ (\i -> runIPCStorage (insert "many" [("id", VInt i)])) [1 .. 20 :: Int]
                rows <- runIPCStorage (scan "many")
                fmap length rows `shouldBe` Right 20
                fmap (sortOn show . map (lookup "id")) rows
                    `shouldBe` Right (sortOn show (map (Just . VInt) [1 .. 20]))

        it "replaceAll leaves only the new rows" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "t" [("id", VInt 1)]) `shouldReturn` Right ()
                runIPCStorage (replaceAll "t" [[("id", VInt 9)], [("id", VInt 8)]]) `shouldReturn` Right ()
                rows <- runIPCStorage (scan "t")
                fmap (sortOn show . map (lookup "id")) rows
                    `shouldBe` Right (sortOn show [Just (VInt 9), Just (VInt 8)])

        it "lookupByColumn finds a row inserted with an id" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                let row = [("id", VInt 7), ("name", VStr "Zoe")]
                runIPCStorage (insert "keyed" row) `shouldReturn` Right ()
                found <- runIPCStorage (lookupByColumn "keyed" "id" (VInt 7))
                fmap sortRow (indexRow found) `shouldBe` Just (sortRow row)
                missing <- runIPCStorage (lookupByColumn "keyed" "id" (VInt 8))
                missing `shouldBe` Right (IndexRows [])

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

        it "replaceAll rebuilds the index" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "rb" [("id", VInt 1)]) `shouldReturn` Right ()
                runIPCStorage (replaceAll "rb" [[("id", VInt 9), ("name", VStr "Zoe")]]) `shouldReturn` Right ()
                old <- runIPCStorage (lookupByColumn "rb" "id" (VInt 1))
                old `shouldBe` Right (IndexRows [])
                new <- runIPCStorage (lookupByColumn "rb" "id" (VInt 9))
                fmap sortRow (indexRow new)
                    `shouldBe` Just (sortRow [("id", VInt 9), ("name", VStr "Zoe")])

        it "snapshot lists tables and scans each one to build the database" $ do
            withTestSession $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "a" [("id", VInt 1)]) `shouldReturn` Right ()
                runIPCStorage (insert "b" [("id", VInt 2)]) `shouldReturn` Right ()
                db <- runIPCStorage snapshot
                map fst db `shouldBe` ["a", "b"]
                map (length . tableRows . snd) db `shouldBe` [1, 1]
                map (map (lookup "id") . tableRows . snd) db `shouldBe` [[Just (VInt 1)], [Just (VInt 2)]]

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
                                { selectCols = ["name"]
                                , selectFrom = FromTable Nothing "ipc_idx"
                                , selectWhere = Just (Eq (Col "id") (LitInt 42))
                                , selectGroupBy = []
                                , selectOrderBy = []
                                , selectLimit = Nothing
                                }
                            )
                        )
                result `shouldBe` Right [[("name", VStr "Zoe")]]
                -- 存储层的日志现在直接打在本进程的输出里，没有单独的 debug.log 可读；
                -- 「走索引而不是全表扫」由下面 CREATE INDEX 那组用例与 Rust 侧 runtime 测试覆盖。

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
                db <- runIPCStorage schema
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
            let db = [("t", Table "t" [("id", TInt), ("flag", TBool)] [] Nothing)]
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
            parseStatement "CREATE TABLE t (a nope)" `shouldSatisfy` isLeft

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

-- | 每个测试表都清空行后的库
emptyDB :: Database
emptyDB = [(n, t{tableRows = []}) | (n, t) <- testDB]

-- | 一次进程内存储会话：进程内直连存储动态库
data StorageSession = StorageSession
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

-- | 一个用例一份干净数据目录；动态库打不开则 pending
withTestSession :: (StorageSession -> IO ()) -> IO ()
withTestSession act = do
    tmp <- getTemporaryDirectory
    stamp <- uniqueStamp
    let dir = tmp </> "chusql-hs-IPC" </> stamp
    createDirectoryIfMissing True dir
    opened <- openSessionAt dir
    case opened of
        Left err -> cleanSessionDir dir >> pendingWith err
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
    pure Probe {probeTable = table, probeCalls = calls, probeWidth = width}

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
    -- | 每片行数
    chunk = (length rows + width - 1) `div` width
    -- | 反复切出前一片
    go rest
        | null rest = []
        | otherwise = let (part, more) = splitAt chunk rest in part : go more

-- | 声明 1000 行的大表
bigTable :: Table
bigTable = users {tableName = "big", tableMeta = Just (TableMeta 1000 [("id", 1000, False)] ["id"])}

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
    snapshot = ProbeStorage (\_ -> pure [])

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
pushedIntoLeft (Range _ _ _ _ _) = True
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
    Sort _ x -> planTables x
    Limit _ x -> planTables x
    Join _ l r _ -> planTables l ++ planTables r
    Unit -> []

-- | 计划里每个 Join 节点是不是 LEFT JOIN（先序）
leftJoinKinds :: RelOp -> [Bool]
leftJoinKinds (Join kind l r _) = (kind == LeftJoin) : (leftJoinKinds l ++ leftJoinKinds r)
leftJoinKinds (Project _ x) = leftJoinKinds x
leftJoinKinds (Compute _ x) = leftJoinKinds x
leftJoinKinds (Aggregate _ _ x) = leftJoinKinds x
leftJoinKinds (Filter _ x) = leftJoinKinds x
leftJoinKinds (Sort _ x) = leftJoinKinds x
leftJoinKinds (Limit _ x) = leftJoinKinds x
leftJoinKinds _ = []

-- | 取计划里第一个过滤条件
firstFilterCond :: RelOp -> Maybe Expr
firstFilterCond (Filter p _) = Just p
firstFilterCond (Project _ x) = firstFilterCond x
firstFilterCond (Compute _ x) = firstFilterCond x
firstFilterCond (Aggregate _ _ x) = firstFilterCond x
firstFilterCond Unit = Nothing
firstFilterCond (Sort _ x) = firstFilterCond x
firstFilterCond (Limit _ x) = firstFilterCond x
firstFilterCond (Join _ l r _) = case firstFilterCond l of
    Just p -> Just p
    Nothing -> firstFilterCond r
firstFilterCond (Scan _ _ _) = Nothing
firstFilterCond (Lookup _ _ _ _) = Nothing
firstFilterCond (Range _ _ _ _ _) = Nothing

-- | 判断计划里是否还有过滤节点
anyFilter :: RelOp -> Bool
anyFilter (Filter _ _) = True
anyFilter (Project _ x) = anyFilter x
anyFilter (Compute _ x) = anyFilter x
anyFilter (Aggregate _ _ x) = anyFilter x
anyFilter Unit = False
anyFilter (Sort _ x) = anyFilter x
anyFilter (Limit _ x) = anyFilter x
anyFilter (Join _ l r _) = anyFilter l || anyFilter r
anyFilter (Scan _ _ _) = False
anyFilter (Lookup _ _ _ _) = False
anyFilter (Range _ _ _ _ _) = False

-- | 去掉计划里的投影节点
stripProjects :: RelOp -> RelOp
stripProjects (Project _ x) = stripProjects x
stripProjects (Compute items x) = Compute items (stripProjects x)
stripProjects (Aggregate keys aggs x) = Aggregate keys aggs (stripProjects x)
stripProjects Unit = Unit
stripProjects (Filter p x) = Filter p (stripProjects x)
stripProjects (Sort spec x) = Sort spec (stripProjects x)
stripProjects (Limit n x) = Limit n (stripProjects x)
stripProjects (Join k l r c) = Join k (stripProjects l) (stripProjects r) c
stripProjects (Scan a t _) = Scan a t Nothing
stripProjects (Lookup a t c k) = Lookup a t c k
stripProjects (Range a t c lo hi) = Range a t c lo hi

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
