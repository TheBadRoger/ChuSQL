{-# LANGUAGE CPP #-}

module Main where

import ChuSQL.Algebra.Eval (evalRelOp)
import ChuSQL.Algebra.Expr (colsInExpr, evalExpr)
import ChuSQL.Algebra.Op (RelOp (..), renderPlan)
import ChuSQL.Algebra.Optimize (optimize, pushProject)
import ChuSQL.Algebra.Planner (translate)
import ChuSQL.Engine
import ChuSQL.Model
import ChuSQL.Semantic (prepare)
import ChuSQL.Storage (IndexResult (..), MonadStorage (..))
import ChuSQL.Storage.IPC (IPCStorage (runIPCStorage), Request (ReqPing), Response (RespPong), doListTables, getPipeName, sendRequest, setPipeName)
import ChuSQL.Syntax.AST
import ChuSQL.Syntax.Parser
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, finally, try)
import Data.List (isInfixOf, sortOn)
import Data.Unique (hashUnique, newUnique)
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (Handle, IOMode (WriteMode), withFile)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (NoStream, UseHandle), createProcess, proc, terminateProcess, waitForProcess)
import Test.Hspec

-- 引擎层的 hspec 测试：语法、执行、优化器与 IPC 行为。

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
        }

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
        }

testDB :: Database
testDB = [("users", users), ("orders", orders)]

main :: IO ()
main = hspec $ do
    describe "ChuSQL.Syntax.Parser" $ do
        it "resolves table-qualified columns inside database-qualified tables" $ do
            let db = [("sales.users", users)]
            rowsOf (parseStatement "SELECT users.name FROM sales.users WHERE users.id = 1" >>= runStatement db)
                `shouldBe` Right [[("name", VStr "Alice")]]
            rowsOf (parseStatement "SELECT sales.users.name FROM sales.users WHERE sales.users.id = 1" >>= runStatement db)
                `shouldBe` Right [[("name", VStr "Alice")]]

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

    describe "ChuSQL.Syntax.Parser (JOIN)" $ do
        it "parses a simple JOIN without aliases" $ do
            parseStatement "SELECT name FROM users JOIN orders ON id = user_id"
                `shouldBe` Right
                    ( Select
                        { selectCols = ["name"]
                        , selectFrom =
                            FromJoin
                                InnerJoin
                                (FromTable Nothing "users")
                                Nothing
                                "orders"
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
                                (Just "o")
                                "orders"
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
                                (Just "o")
                                "orders"
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
                        (Just "o")
                        "orders"
                        (Eq (Col "u.id") (Col "o.user_id"))
                    )

    describe "ChuSQL.Engine (SELECT)" $ do
        let sql input = parseStatement input >>= rowsOf . runStatement testDB
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

    describe "ChuSQL.Engine (INSERT)" $ do
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

    describe "ChuSQL.Engine (INDEX)" $ do
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

    describe "ChuSQL.Engine (DROP COLUMN)" $ do
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
    describe "ChuSQL.Engine (USER)" $ do
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

    describe "ChuSQL.Engine (ROLE)" $ do
        it "parses CREATE ROLE / DROP ROLE case-insensitively" $ do
            parseStatement "CREATE ROLE analyst" `shouldBe` Right (CreateRole "analyst")
            parseStatement "create role analyst" `shouldBe` Right (CreateRole "analyst")
            parseStatement "DROP ROLE analyst" `shouldBe` Right (DropRole "analyst")
            parseStatement "drop role 'a.b'" `shouldBe` Right (DropRole "a.b")

        it "parses privilege grants on one table, on another database's table or on everything" $ do
            parseStatement "GRANT SELECT ON users TO analyst"
                `shouldBe` Right (GrantPrivileges ["SELECT"] "users" "analyst")
            parseStatement "grant select, insert on users to analyst"
                `shouldBe` Right (GrantPrivileges ["SELECT", "INSERT"] "users" "analyst")
            parseStatement "GRANT ALL ON sales.orders TO analyst"
                `shouldBe` Right (GrantPrivileges ["ALL"] "sales.orders" "analyst")
            parseStatement "GRANT DELETE ON * TO analyst"
                `shouldBe` Right (GrantPrivileges ["DELETE"] "*" "analyst")

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
                `shouldBe` Right (GrantPrivileges ["SELECT"] "users" "analyst")
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

    describe "ChuSQL.Engine (DELETE)" $ do
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

    describe "ChuSQL.Engine (UPDATE)" $ do
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

    describe "ChuSQL.Engine (ORDER BY)" $ do
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

    describe "ChuSQL.Engine (LIMIT)" $ do
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

    describe "ChuSQL.Engine (JOIN)" $ do
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
                            (Just "x")
                            "nonexistent"
                            (Eq (Col "u.id") (Col "x.id"))
                    , selectWhere = Nothing
                    , selectGroupBy = []
                    , selectOrderBy = []
                    , selectLimit = Nothing
                    }
                )
                `shouldSatisfy` isLeft

    describe "ChuSQL.Engine (LEFT JOIN)" $ do
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

    describe "ChuSQL.Engine (JOIN 嵌套循环)" $ do
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

    describe "ChuSQL.Engine (子查询)" $ do
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

    describe "ChuSQL.Engine (聚合与 GROUP BY)" $ do
        let sql input = parseStatement input >>= rowsOf . runStatement testDB
            sqlDb db input = parseStatement input >>= rowsOf . runStatement db
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

    describe "ChuSQL.Algebra.Optimize" $ do
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

    describe "ChuSQL.Semantic" $ do
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

    describe "ChuSQL.Algebra.Op" $ do
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

    describe "ChuSQL.Storage.IPC" $ do
        it "projects scan columns and preserves constant rows over the pipe" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
                runIPCStorage (insertMany "projected" [[("id", VInt 1), ("name", VStr "one")], [("id", VInt 2), ("name", VNull)]]) `shouldReturn` Right ()
                runIPCStorage (scanColumns "projected" ["name"]) `shouldReturn` Right [[("name", VStr "one")], [("name", VNull)]]
                runIPCStorage (scanColumns "projected" []) `shouldReturn` Right [[], []]
                let query sql = case parseStatement sql of
                        Left err -> expectationFailure err >> pure (Left err)
                        Right stmt -> runIPCStorage (runStatementM stmt)
                result <- query "SELECT 1 FROM projected"
                fmap (map (map snd)) result `shouldBe` Right [[VInt 1], [VInt 1]]
                query "SELECT p.name FROM projected p ORDER BY p.id DESC" `shouldReturn` Right [[("p.name", VNull)], [("p.name", VStr "one")]]

        it "recreates a dropped column without resurrecting old values over the pipe" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
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
            withTestServer $ \srv -> withServerEnv srv $ do
                let row = [("id", VInt 7), ("name", VStr "Alice"), ("flag", VBool True)]
                runIPCStorage (insert "users" row) `shouldReturn` Right ()
                rows <- runIPCStorage (scan "users")
                (map sortRow <$> rows) `shouldBe` Right [sortRow row]

        it "scan on a missing table returns Left" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
                result <- runIPCStorage (scan "no_such_table")
                result `shouldSatisfy` isLeft

        it "scan returns all 20 inserted rows in order" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
                mapM_ (\i -> runIPCStorage (insert "many" [("id", VInt i)])) [1 .. 20 :: Int]
                rows <- runIPCStorage (scan "many")
                fmap length rows `shouldBe` Right 20
                fmap (sortOn show . map (lookup "id")) rows
                    `shouldBe` Right (sortOn show (map (Just . VInt) [1 .. 20]))

        it "replaceAll leaves only the new rows" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "t" [("id", VInt 1)]) `shouldReturn` Right ()
                runIPCStorage (replaceAll "t" [[("id", VInt 9)], [("id", VInt 8)]]) `shouldReturn` Right ()
                rows <- runIPCStorage (scan "t")
                fmap (sortOn show . map (lookup "id")) rows
                    `shouldBe` Right (sortOn show [Just (VInt 9), Just (VInt 8)])

        it "lookupByColumn finds a row inserted with an id over the pipe" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
                let row = [("id", VInt 7), ("name", VStr "Zoe")]
                runIPCStorage (insert "keyed" row) `shouldReturn` Right ()
                found <- runIPCStorage (lookupByColumn "keyed" "id" (VInt 7))
                fmap sortRow (indexRow found) `shouldBe` Just (sortRow row)
                missing <- runIPCStorage (lookupByColumn "keyed" "id" (VInt 8))
                missing `shouldBe` Right (IndexRows [])

        it "looks up a string index over the pipe" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "names" [("id", VInt 1), ("name", VStr "alice")]) `shouldReturn` Right ()
                runIPCStorage (insert "names" [("id", VInt 2), ("name", VStr "bob")]) `shouldReturn` Right ()
                runIPCStorage (createIndex "names" "name") `shouldReturn` Right ()
                found <- runIPCStorage (lookupByColumn "names" "name" (VStr "bob"))
                fmap (lookup "id") (indexRow found) `shouldBe` Just (Just (VInt 2))
                missing <- runIPCStorage (lookupByColumn "names" "name" (VStr "dave"))
                missing `shouldBe` Right (IndexRows [])

        it "range scans an indexed column over the pipe" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "rng" [("id", VInt 1), ("code", VInt 100)]) `shouldReturn` Right ()
                runIPCStorage (insert "rng" [("id", VInt 2), ("code", VInt 200)]) `shouldReturn` Right ()
                runIPCStorage (insert "rng" [("id", VInt 3), ("code", VInt 300)]) `shouldReturn` Right ()
                runIPCStorage (createIndex "rng" "code") `shouldReturn` Right ()
                hit <- runIPCStorage (scanRange "rng" "code" (Just (VInt 200, True)) (Just (VInt 300, True)))
                fmap (fmap (map (lookup "id"))) hit `shouldBe` Right (Just [Just (VInt 2), Just (VInt 3)])
                open <- runIPCStorage (scanRange "rng" "code" (Just (VInt 200, False)) (Just (VInt 300, True)))
                fmap (fmap (map (lookup "id"))) open `shouldBe` Right (Just [Just (VInt 3)])
                runIPCStorage (scanRange "rng" "nope" Nothing Nothing) `shouldReturn` Right Nothing

        it "replaceAll rebuilds the index over the pipe" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "rb" [("id", VInt 1)]) `shouldReturn` Right ()
                runIPCStorage (replaceAll "rb" [[("id", VInt 9), ("name", VStr "Zoe")]]) `shouldReturn` Right ()
                old <- runIPCStorage (lookupByColumn "rb" "id" (VInt 1))
                old `shouldBe` Right (IndexRows [])
                new <- runIPCStorage (lookupByColumn "rb" "id" (VInt 9))
                fmap sortRow (indexRow new)
                    `shouldBe` Just (sortRow [("id", VInt 9), ("name", VStr "Zoe")])

        it "snapshot lists tables and scans each one to build the database" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "a" [("id", VInt 1)]) `shouldReturn` Right ()
                runIPCStorage (insert "b" [("id", VInt 2)]) `shouldReturn` Right ()
                db <- runIPCStorage snapshot
                map fst db `shouldBe` ["a", "b"]
                map (length . tableRows . snd) db `shouldBe` [1, 1]
                map (map (lookup "id") . tableRows . snd) db `shouldBe` [[Just (VInt 1)], [Just (VInt 2)]]

        it "doListTables returns the tables that exist" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "t1" [("id", VInt 1)]) `shouldReturn` Right ()
                runIPCStorage (insert "t2" [("id", VInt 2)]) `shouldReturn` Right ()
                doListTables `shouldReturn` Right ["t1", "t2"]

        it "rows survive a restart on the same data directory" $ do
            located <- locateServer
            case located of
                Left err -> pendingWith err
                Right bin -> do
                    srv1 <- startServer bin
                    let dir = dataDir srv1
                    withServerEnv srv1 $ runIPCStorage (insert "persist" [("id", VInt 42)]) `shouldReturn` Right ()
                    stopServer srv1
                    srv2 <- startServerAt bin dir
                    rows <- withServerEnv srv2 $ runIPCStorage (scan "persist")
                    stopServer srv2
                    cleanServerDir srv1
                    (map sortRow <$> rows) `shouldBe` Right [sortRow [("id", VInt 42)]]

        it "a missing server yields Left without throwing" $ do
            withPipeName "chusql-no-such-server" $ do
                result <- runIPCStorage (scan "users")
                result `shouldSatisfy` isLeft
#if !defined(mingw32_HOST_OS)
        it "an over-long socket path is reported instead of a raw connect failure" $ do
            withPipeName (replicate 128 'x') $ do
                result <- runIPCStorage (scan "users")
                show result `shouldSatisfy` isInfixOf "socket path too long"
#endif
        it "INSERT then SELECT WHERE id = k via IPC" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
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
                stopServer srv
                logs <- readFile (dataDir srv </> "debug.log")
                logs `shouldContain` "lookup_by_index table=ipc_idx column=id key=42"
                logs `shouldNotContain` "scan table="

        it "CREATE INDEX makes a query on that column use the index" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
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

        it "CREATE INDEX over the pipe accepts duplicates and returns every matching row" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
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

        it "DELETE removes only the matching rows over the pipe" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
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

        it "multi-row INSERT goes over the pipe in one statement" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
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
            let db = [("t", Table "t" [("id", TInt), ("flag", TBool)] [])]
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

    describe "ChuSQL.Engine (CREATE TABLE)" $ do
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

    describe "ChuSQL.Engine (NULL)" $ do
        let rows db input = parseStatement input >>= rowsOf . runStatement db
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

    describe "ChuSQL.Engine (TYPES)" $ do
        let vals db input = fmap (map (map snd)) (parseStatement input >>= rowsOf . runStatement db)
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

    describe "ChuSQL.Engine (CONSTRAINTS)" $ do
        let vals db input = fmap (map (map snd)) (parseStatement input >>= rowsOf . runStatement db)
            withTable ddl = either (const testDB) fst (parseStatement ddl >>= runStatement testDB)
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

    describe "ChuSQL.Engine (ALTER TABLE)" $ do
        let vals db input = fmap (map (map snd)) (parseStatement input >>= rowsOf . runStatement db)
            run db input = parseStatement input >>= runStatement db
            withTable ddl = either (const testDB) fst (parseStatement ddl >>= runStatement testDB)
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

emptyDB :: Database
emptyDB = [(n, t{tableRows = []}) | (n, t) <- testDB]

data IPCServer = IPCServer
    { pipeName :: String
    , dataDir :: FilePath
    , processHandle :: ProcessHandle
    }

locateServer :: IO (Either String FilePath)
locateServer = do
    mdir <- firstDir [".." </> "chusql-storage", "chusql-storage"]
    case mdir of
        Nothing -> pure (Left "chusql-storage directory not found (run these tests inside the repository)")
        Just dir -> do
            built <-
                try (createProcess (proc "cargo" ["build", "--bin", "chusql-storage"]){cwd = Just dir, std_out = NoStream, std_err = NoStream}) ::
                    IO (Either IOException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
            case built of
                Left e -> pure (Left ("cannot run cargo (Rust is required for the IPC tests): " ++ show e))
                Right (_, _, _, ph) -> do
                    ec <- waitForProcess ph
                    case ec of
                        ExitFailure n -> pure (Left ("cargo build --bin chusql-storage failed with exit code " ++ show n))
                        ExitSuccess -> do
                            -- 二进制名跟平台走：Linux 上没有 .exe，而 target/debug 里可能
                            -- 残留一份别处交叉编译出来的 .exe。误选它会以 Windows 的视角
                            -- 读配置（/tmp/x.toml 变成 C:\tmp\x.toml），报 config file not found。
#if defined(mingw32_HOST_OS)
                            let candidate = dir </> "target" </> "debug" </> "chusql-storage.exe"
#else
                            let candidate = dir </> "target" </> "debug" </> "chusql-storage"
#endif
                            ok <- doesFileExist candidate
                            pure
                                ( if ok
                                    then Right candidate
                                    else Left ("cargo build finished but no storage executable was found at " ++ candidate)
                                )
  where
    firstDir [] = pure Nothing
    firstDir (d : ds) = do
        ok <- doesDirectoryExist d
        if ok then pure (Just d) else firstDir ds

startServer :: FilePath -> IO IPCServer
startServer bin = do
    stamp <- uniqueStamp
    tmp <- getTemporaryDirectory
    let dir = tmp </> "chusql-hs-IPC" </> stamp
    createDirectoryIfMissing True dir
    startServerAt bin dir

startServerAt :: FilePath -> FilePath -> IO IPCServer
startServerAt bin dir = do
    stamp <- uniqueStamp
    let name = "chusql-hs-test-" ++ stamp
    cfgPath <- writeServerConfig dir name
    (_, _, _, ph) <- withFile (dir </> "debug.log") WriteMode $ \logHandle ->
        createProcess
            (proc bin ["--config", cfgPath])
                { std_out = UseHandle logHandle
                , std_err = UseHandle logHandle
                }
    let srv = IPCServer name dir ph
    waitUntilReady srv 100
    pure srv

-- | 给存储进程写一份顺手可用的配置：管名、数据目录与日志级别都从文件走
writeServerConfig :: FilePath -> String -> IO FilePath
writeServerConfig dir name = do
    let cfgPath = dir ++ ".toml"
        -- TOML 的普通字符串会吃反斜杠，路径统一用正斜杠
        slashed = map (\c -> if c == '\\' then '/' else c) dir
    writeFile
        cfgPath
        ( unlines
            [ "[storage]"
            , "data_dir = \"" ++ slashed ++ "\""
            , "[server]"
            , "pipe_name = \"" ++ name ++ "\""
            , "[log]"
            , "level = \"debug\""
            ]
        )
    pure cfgPath

-- | 每次调用都不同的后缀：CPU 时间加唯一编号
uniqueStamp :: IO String
uniqueStamp = do
    cpu <- getCPUTime
    u <- hashUnique <$> newUnique
    pure (show cpu ++ "-" ++ show u)

waitUntilReady :: IPCServer -> Int -> IO ()
waitUntilReady srv tries = do
    probe <- try (withServerEnv srv (sendRequest ReqPing)) :: IO (Either IOException Response)
    case probe of
        Right RespPong -> pure ()
        _ | tries > 0 -> threadDelay 50000 >> waitUntilReady srv (tries - 1)
        Right _ -> ioError (userError "server is up but the ping answer was not pong")
        Left e -> ioError e

stopServer :: IPCServer -> IO ()
stopServer srv = do
    _ <- try (terminateProcess (processHandle srv)) :: IO (Either IOException ())
    _ <- try (waitForProcess (processHandle srv)) :: IO (Either IOException ExitCode)
    pure ()

cleanServerDir :: IPCServer -> IO ()
cleanServerDir srv = do
    _ <- try (removePathForcibly (dataDir srv)) :: IO (Either IOException ())
    _ <- try (removePathForcibly (dataDir srv ++ ".toml")) :: IO (Either IOException ())
    pure ()

withTestServer :: (IPCServer -> IO ()) -> IO ()
withTestServer act = do
    located <- locateServer
    case located of
        Left err
            | "cannot run cargo" `isInfixOf` err -> pendingWith err
            | otherwise -> expectationFailure err
        Right bin -> bracket (startServer bin) (\s -> stopServer s >> cleanServerDir s) act

withServerEnv :: IPCServer -> IO a -> IO a
withServerEnv srv act = do
    old <- getPipeName
    setPipeName (pipeName srv)
    act `finally` setPipeName old

withPipeName :: String -> IO a -> IO a
withPipeName name act = do
    old <- getPipeName
    setPipeName name
    r <- act
    setPipeName old
    pure r

sortRow :: Row -> Row
sortRow = sortOn fst

-- | 取出索引点查返回的那一行，取不到给 Nothing
indexRow :: Either String IndexResult -> Maybe Row
indexRow (Right (IndexRows (r : _))) = Just r
indexRow _ = Nothing

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

sameResultAsUnoptimized :: String -> Expectation
sameResultAsUnoptimized sql =
    case parseStatement sql >>= prepare testDB of
        Left err -> expectationFailure err
        Right q ->
            case translate q of
                Left err -> expectationFailure err
                Right relOp ->
                    evalRelOp testDB (optimize testDB relOp) `shouldBe` evalRelOp testDB relOp

filterPushedIntoLeft :: RelOp -> Bool
filterPushedIntoLeft (Project _ (Join _ l _ _)) = pushedIntoLeft l
filterPushedIntoLeft _ = False

-- | 左侧子树里出现了下推下来的谓词（点查 / 范围 / 过滤）
pushedIntoLeft :: RelOp -> Bool
pushedIntoLeft (Filter _ _) = True
pushedIntoLeft (Range _ _ _ _ _) = True
pushedIntoLeft (Project _ x) = pushedIntoLeft x
pushedIntoLeft _ = False

optimizedPlan :: String -> Either String RelOp
optimizedPlan sql = do
    q <- parseStatement sql
    optimize testDB <$> translate q

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

projectedPlan :: String -> Either String RelOp
projectedPlan sql = do
    q <- parseStatement sql
    plan <- translate q
    Right (pushProject testDB ["*"] plan)

sideCols :: RelOp -> Maybe [String]
sideCols (Project cols _) = Just cols
sideCols (Scan a _ cols) = fmap (map (qualify a)) cols
sideCols _ = Nothing

joinSideCols :: RelOp -> (Maybe [String], Maybe [String])
joinSideCols (Project _ (Join _ l r _)) = (sideCols l, sideCols r)
joinSideCols _ = (Nothing, Nothing)

countFilters :: RelOp -> Int
countFilters (Filter _ x) = 1 + countFilters x
countFilters _ = 0

leftSideFilters :: RelOp -> Maybe Int
leftSideFilters (Project _ (Join _ l _ _)) = Just (countFilters l)
leftSideFilters _ = Nothing

planRoot :: RelOp -> RelOp
planRoot (Project _ x) = planRoot x
planRoot x = x
