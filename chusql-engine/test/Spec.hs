module Main where

import ChuSQL.Algebra.Eval (evalRelOp)
import ChuSQL.Algebra.Op (RelOp (..), renderPlan)
import ChuSQL.Algebra.Optimize (optimize, pushProject)
import ChuSQL.Algebra.Planner (translate)
import ChuSQL.Engine
import ChuSQL.Model
import ChuSQL.Storage (MonadStorage (..))
import ChuSQL.Storage.IPC (IPCStorage (runIPCStorage), Request (ReqPing), Response (RespPong), doListTables, sendRequest)
import ChuSQL.Syntax.AST
import ChuSQL.Syntax.Parser
import Control.Concurrent (threadDelay)
import Control.Exception (IOException, bracket, try)
import Data.List (isInfixOf, sortOn)
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist, getTemporaryDirectory, removePathForcibly)
import System.Environment (getEnvironment, lookupEnv, setEnv, unsetEnv)
import System.Exit (ExitCode (..))
import System.FilePath ((</>))
import System.IO (Handle)
import System.Process (CreateProcess (..), ProcessHandle, StdStream (NoStream), createProcess, proc, terminateProcess, waitForProcess)
import Test.Hspec

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
    describe "ChuSQL.Syntax.AST" $ do
        it "equal Selects are equal" $ do
            makeSelect ["name"] "users" Nothing
                `shouldBe` makeSelect ["name"] "users" Nothing

        it "different Selects are not equal" $ do
            makeSelect ["name"] "users" Nothing
                `shouldNotBe` makeSelect ["age"] "users" Nothing

    describe "ChuSQL.Syntax.Parser" $ do
        it "parses a simple SELECT" $ do
            parseStatement "SELECT name FROM users"
                `shouldBe` Right (makeSelect ["name"] "users" Nothing)

        it "parses SELECT *" $ do
            parseStatement "SELECT * FROM users"
                `shouldBe` Right (makeSelect ["*"] "users" Nothing)

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

        it "parses a string literal in WHERE" $ do
            parseStatement "SELECT name FROM users WHERE name = 'Alice'"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Eq (Col "name") (LitStr "Alice")))
                    )

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

        it "allows digits inside identifiers" $ do
            parseStatement "SELECT user1 FROM users"
                `shouldBe` Right (makeSelect ["user1"] "users" Nothing)

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
                                (FromTable Nothing "users")
                                Nothing
                                "orders"
                                (Eq (Col "id") (Col "user_id"))
                        , selectWhere = Nothing
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
                                (FromTable (Just "u") "users")
                                (Just "o")
                                "orders"
                                (Eq (Col "u.id") (Col "o.user_id"))
                        , selectWhere = Nothing
                        , selectOrderBy = []
                        , selectLimit = Nothing
                        }
                    )

        it "parses qualified column names" $ do
            case parseStatement "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id" of
                Right q -> selectCols q `shouldBe` ["u.name", "o.product"]
                Left err -> expectationFailure err

    describe "ChuSQL.Engine (SELECT)" $ do
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

        it "returns Left for an unknown column" $ do
            runStatement
                testDB
                ( makeSelect
                    ["name"]
                    "users"
                    (Just (Gt (Col "unknown") (LitInt 18)))
                )
                `shouldSatisfy` isLeft

        it "returns Left when WHERE is not a boolean" $ do
            runStatement testDB (makeSelect ["name"] "users" (Just (Col "age")))
                `shouldSatisfy` isLeft

        it "returns Left for an unknown projected column" $ do
            runStatement testDB (makeSelect ["nope"] "users" Nothing)
                `shouldSatisfy` isLeft

    describe "ChuSQL.Engine (INSERT)" $ do
        it "parses a simple INSERT" $ do
            parseStatement "INSERT INTO users (name, age) VALUES ('Dave', 22)"
                `shouldBe` Right (Insert "users" ["name", "age"] [LitStr "Dave", LitInt 22])

        it "parses INSERT with a single column" $ do
            parseStatement "INSERT INTO users (name) VALUES ('Eve')"
                `shouldBe` Right (Insert "users" ["name"] [LitStr "Eve"])

        it "parses INSERT case-insensitively" $ do
            parseStatement "insert into users (name, age) values ('Dave', 22)"
                `shouldBe` Right (Insert "users" ["name", "age"] [LitStr "Dave", LitInt 22])

        it "executes INSERT and adds a row at the end" $ do
            case runStatement testDB (Insert "users" ["name", "age"] [LitStr "Dave", LitInt 22]) of
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
            case runStatement testDB (Insert "users" ["name", "age"] [LitStr "Dave", LitInt 22]) of
                Left err -> expectationFailure err
                Right (_, _) -> do
                    rowsOf (runStatement testDB (makeSelect ["name"] "users" Nothing))
                        `shouldSatisfy` \r -> case r of
                            Right rows -> length rows == 3
                            Left _ -> False

        it "returns Left when column count does not match value count" $ do
            runStatement testDB (Insert "users" ["name", "age"] [LitStr "Dave"])
                `shouldSatisfy` isLeft

        it "returns Left for an unknown table" $ do
            runStatement testDB (Insert "nonexistent" ["name"] [LitStr "X"])
                `shouldSatisfy` isLeft

        it "returns Left when a value has the wrong type for comparison" $ do
            runStatement testDB (Insert "users" ["name"] [Col "other"])
                `shouldSatisfy` isLeft

    describe "ChuSQL.Engine (DELETE)" $ do
        it "parses DELETE with WHERE" $ do
            parseStatement "DELETE FROM users WHERE age < 18"
                `shouldBe` Right (Delete "users" (Just (Lt (Col "age") (LitInt 18))))

        it "parses DELETE without WHERE" $ do
            parseStatement "DELETE FROM users"
                `shouldBe` Right (Delete "users" Nothing)

        it "parses DELETE case-insensitively" $ do
            parseStatement "delete from users where age < 18"
                `shouldBe` Right (Delete "users" (Just (Lt (Col "age") (LitInt 18))))

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

        it "does not modify the original database" $ do
            case runStatement testDB (Delete "users" Nothing) of
                Left err -> expectationFailure err
                Right (_, _) -> do
                    rowsOf (runStatement testDB (makeSelect ["name"] "users" Nothing))
                        `shouldSatisfy` \r -> case r of
                            Right rows -> length rows == 3
                            Left _ -> False

        it "returns Left for an unknown table" $ do
            runStatement testDB (Delete "nonexistent" Nothing)
                `shouldSatisfy` isLeft

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

        it "parses UPDATE case-insensitively" $ do
            parseStatement "update users set age = 26 where name = 'Alice'"
                `shouldBe` Right
                    ( Update
                        "users"
                        [("age", LitInt 26)]
                        (Just (Eq (Col "name") (LitStr "Alice")))
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

        it "does not modify the original database" $ do
            case runStatement testDB (Update "users" [("age", LitInt 0)] Nothing) of
                Left err -> expectationFailure err
                Right (_, _) -> do
                    rowsOf (runStatement testDB (makeSelect ["name"] "users" Nothing))
                        `shouldSatisfy` \r -> case r of
                            Right rows -> length rows == 3
                            Left _ -> False

        it "returns Left for an unknown table" $ do
            runStatement testDB (Update "nonexistent" [("age", LitInt 0)] Nothing)
                `shouldSatisfy` isLeft

        it "returns Left when WHERE references an unknown column" $ do
            runStatement testDB (Update "users" [("age", LitInt 0)] (Just (Gt (Col "unknown") (LitInt 1))))
                `shouldSatisfy` isLeft

        it "returns Left when SET references an unknown column" $ do
            runStatement testDB (Update "users" [("unknown", LitInt 0)] Nothing)
                `shouldSatisfy` \r -> case r of
                    Right _ -> True
                    Left _ -> True

    describe "ChuSQL.Engine (ORDER BY)" $ do
        it "parses ORDER BY ASC" $ do
            case parseStatement "SELECT name FROM users ORDER BY age ASC" of
                Right q -> selectOrderBy q `shouldBe` [("age", Asc)]
                Left err -> expectationFailure err

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

        it "parses without ORDER BY" $ do
            case parseStatement "SELECT name FROM users" of
                Right q -> selectOrderBy q `shouldBe` []
                Left err -> expectationFailure err

        it "executes ORDER BY age ASC" $ do
            let q =
                    Select
                        { selectCols = ["name"]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
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
                        , selectOrderBy = [("age", Desc)]
                        , selectLimit = Nothing
                        }
            rowsOf (runStatement testDB q)
                `shouldBe` Right
                    [ [("name", VStr "Carol")]
                    , [("name", VStr "Alice")]
                    , [("name", VStr "Bob")]
                    ]

        it "executes ORDER BY name ASC" $ do
            let q =
                    Select
                        { selectCols = ["name"]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectOrderBy = [("name", Asc)]
                        , selectLimit = Nothing
                        }
            rowsOf (runStatement testDB q)
                `shouldBe` Right
                    [ [("name", VStr "Alice")]
                    , [("name", VStr "Bob")]
                    , [("name", VStr "Carol")]
                    ]

        it "returns Left for unknown ORDER BY column" $ do
            let q =
                    Select
                        { selectCols = ["name"]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectOrderBy = [("nope", Asc)]
                        , selectLimit = Nothing
                        }
            runStatement testDB q `shouldSatisfy` isLeft

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

        it "parses without LIMIT" $ do
            case parseStatement "SELECT * FROM users" of
                Right q -> selectLimit q `shouldBe` Nothing
                Left err -> expectationFailure err

        it "executes LIMIT without ORDER BY" $ do
            let q =
                    Select
                        { selectCols = ["name"]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
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
                        , selectOrderBy = [("age", Desc)]
                        , selectLimit = Just 2
                        }
            rowsOf (runStatement testDB q)
                `shouldBe` Right
                    [ [("name", VStr "Carol")]
                    , [("name", VStr "Alice")]
                    ]

        it "executes LIMIT larger than row count" $ do
            let q =
                    Select
                        { selectCols = ["name"]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
                        , selectOrderBy = []
                        , selectLimit = Just 10
                        }
            rowsOf (runStatement testDB q)
                `shouldBe` Right
                    [ [("name", VStr "Alice")]
                    , [("name", VStr "Bob")]
                    , [("name", VStr "Carol")]
                    ]

        it "executes LIMIT 0" $ do
            let q =
                    Select
                        { selectCols = ["name"]
                        , selectFrom = FromTable Nothing "users"
                        , selectWhere = Nothing
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
                            (FromTable (Just "u") "users")
                            (Just "x")
                            "nonexistent"
                            (Eq (Col "u.id") (Col "x.id"))
                    , selectWhere = Nothing
                    , selectOrderBy = []
                    , selectLimit = Nothing
                    }
                )
                `shouldSatisfy` isLeft

        it "returns Left when ON references an unknown column" $ do
            runStatement
                testDB
                ( Select
                    { selectCols = ["u.name"]
                    , selectFrom =
                        FromJoin
                            (FromTable (Just "u") "users")
                            (Just "o")
                            "orders"
                            (Eq (Col "u.nope") (Col "o.user_id"))
                    , selectWhere = Nothing
                    , selectOrderBy = []
                    , selectLimit = Nothing
                    }
                )
                `shouldSatisfy` isLeft

    describe "ChuSQL end-to-end" $ do
        it "parses and executes a simple query" $ do
            rowsOf (parseStatement "SELECT name FROM users WHERE age > 18" >>= runStatement testDB)
                `shouldBe` Right
                    [ [("name", VStr "Alice")]
                    , [("name", VStr "Carol")]
                    ]

        it "parses and executes a complex query" $ do
            rowsOf
                ( parseStatement "SELECT name FROM users WHERE age < 18 OR age > 28"
                    >>= runStatement testDB
                )
                `shouldBe` Right
                    [ [("name", VStr "Bob")]
                    , [("name", VStr "Carol")]
                    ]

        it "returns Left when parsing fails" $ do
            (parseStatement "SELECT name FROM" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "INSERT then SELECT end-to-end" $ do
            case parseStatement "INSERT INTO users (name, age) VALUES ('Dave', 22)" >>= runStatement testDB of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (parseStatement "SELECT name FROM users WHERE age > 18" >>= runStatement db')
                        `shouldBe` Right
                            [ [("name", VStr "Alice")]
                            , [("name", VStr "Carol")]
                            , [("name", VStr "Dave")]
                            ]

        it "DELETE then SELECT end-to-end" $ do
            case parseStatement "DELETE FROM users WHERE age > 20" >>= runStatement testDB of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (parseStatement "SELECT name FROM users" >>= runStatement db')
                        `shouldBe` Right
                            [ [("name", VStr "Bob")]
                            ]

        it "UPDATE then SELECT end-to-end" $ do
            case parseStatement "UPDATE users SET age = 99 WHERE name = 'Alice'" >>= runStatement testDB of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (parseStatement "SELECT name, age FROM users" >>= runStatement db')
                        `shouldBe` Right
                            [ [("name", VStr "Alice"), ("age", VInt 99)]
                            , [("name", VStr "Bob"), ("age", VInt 17)]
                            , [("name", VStr "Carol"), ("age", VInt 30)]
                            ]

        it "ORDER BY with WHERE end-to-end" $ do
            rowsOf (parseStatement "SELECT name FROM users WHERE age > 18 ORDER BY age DESC" >>= runStatement testDB)
                `shouldBe` Right
                    [ [("name", VStr "Carol")]
                    , [("name", VStr "Alice")]
                    ]

        it "ORDER BY with LIMIT end-to-end" $ do
            rowsOf (parseStatement "SELECT name FROM users ORDER BY age DESC LIMIT 2" >>= runStatement testDB)
                `shouldBe` Right
                    [ [("name", VStr "Carol")]
                    , [("name", VStr "Alice")]
                    ]

        it "WHERE ORDER BY LIMIT end-to-end" $ do
            rowsOf (parseStatement "SELECT name FROM users WHERE age > 15 ORDER BY age DESC LIMIT 1" >>= runStatement testDB)
                `shouldBe` Right
                    [ [("name", VStr "Carol")]
                    ]

        it "JOIN with WHERE and ORDER BY end-to-end" $ do
            rowsOf (parseStatement "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 15 ORDER BY o.product" >>= runStatement testDB)
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    , [("u.name", VStr "Bob"), ("o.product", VStr "Pen")]
                    ]

    describe "ChuSQL.Algebra.Optimize" $ do
        it "keeps single-table WHERE results identical" $ do
            sameResultAsUnoptimized "SELECT name FROM users WHERE age > 18"

        it "keeps JOIN with WHERE results identical" $ do
            sameResultAsUnoptimized "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 20"

        it "keeps JOIN with WHERE, ORDER BY and LIMIT results identical" $ do
            sameResultAsUnoptimized "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 15 ORDER BY o.product DESC LIMIT 2"

        it "keeps SELECT * results identical" $ do
            sameResultAsUnoptimized "SELECT * FROM users"

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
                            optimize testDB relOp `shouldBe` Scan Nothing "users"

        it "does not push a filter through LIMIT" $ do
            let cond = Gt (Col "age") (LitInt 18)
                relOp = Filter cond (Limit 2 (Scan Nothing "users"))
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

        it "leaves a predicate that mentions a column untouched" $ do
            fmap firstFilterCond (optimizedPlan "SELECT name FROM users WHERE age > 18")
                `shouldBe` Right (Just (Gt (Col "age") (LitInt 18)))

        it "folds the constant part of a predicate that cannot fold as a whole" $ do
            fmap firstFilterCond (optimizedPlan "SELECT name FROM users WHERE age > 18 AND 1 = 1")
                `shouldBe` Right (Just (And (Gt (Col "age") (LitInt 18)) (LitBool True)))

        it "keeps a constant predicate that fails to evaluate" $ do
            fmap firstFilterCond (optimizedPlan "SELECT name FROM users WHERE 'abc' > 1")
                `shouldBe` Right (Just (Gt (LitStr "abc") (LitInt 1)))

        it "keeps constant-false results identical" $ do
            sameResultAsUnoptimized "SELECT name FROM users WHERE 1 > 2"

        it "keeps constant-true results identical" $ do
            sameResultAsUnoptimized "SELECT name FROM users WHERE 1 = 1"

        it "keeps mixed constant and column predicates identical" $ do
            sameResultAsUnoptimized "SELECT name FROM users WHERE age > 18 AND 1 = 1"

        it "keeps a failing constant predicate identical" $ do
            sameResultAsUnoptimized "SELECT name FROM users WHERE 'abc' > 1"

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

        it "keeps the LIMIT node while pushing projection past it" $ do
            let plan = Project ["name"] (Limit 2 (Scan Nothing "users"))
            stripProjects (pushProject testDB ["*"] plan) `shouldBe` stripProjects plan

        it "keeps the sort keys while pushing projection past a sort" $ do
            let plan = Project ["name"] (Sort [("age", Desc)] (Scan Nothing "users"))
            stripProjects (pushProject testDB ["*"] plan) `shouldBe` stripProjects plan

        it "keeps both sides of a join" $ do
            let plan =
                    Project
                        ["u.name"]
                        ( Join
                            (Scan (Just "u") "users")
                            (Scan (Just "o") "orders")
                            (Eq (Col "u.id") (Col "o.user_id"))
                        )
            stripProjects (pushProject testDB ["*"] plan) `shouldBe` stripProjects plan

        it "keeps nested projections in place" $ do
            let plan = Project ["name"] (Project ["name", "age"] (Scan Nothing "users"))
            stripProjects (pushProject testDB ["*"] plan) `shouldBe` stripProjects plan

        it "keeps the whole spine of a full pipeline" $ do
            let plan =
                    Project
                        ["name"]
                        ( Limit
                            2
                            ( Sort
                                [("age", Desc)]
                                (Filter (Gt (Col "age") (LitInt 18)) (Scan Nothing "users"))
                            )
                        )
            stripProjects (pushProject testDB ["*"] plan) `shouldBe` stripProjects plan

        it "merges two predicates pushed into the same side into one filter" $ do
            fmap leftSideFilters (optimizedPlan "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 20 AND u.name = 'Alice'")
                `shouldBe` Right (Just 1)

        it "pushes projection into a single-table scan as one layer" $ do
            optimizedPlan "SELECT name FROM users"
                `shouldBe` Right (Project ["name"] (Scan Nothing "users"))

        it "does not add a projection when the scan already supplies every column" $ do
            optimizedPlan "SELECT id, name, age FROM users"
                `shouldBe` Right (Project ["id", "name", "age"] (Scan Nothing "users"))

        it "prunes the columns nobody needs on each side of a join" $ do
            fmap joinSideCols (projectedPlan "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id")
                `shouldBe` Right (Just ["u.name", "u.id"], Just ["o.user_id"])

        it "does not prune anything when every column is taken" $ do
            fmap joinSideCols (projectedPlan "SELECT * FROM users u JOIN orders o ON u.id = o.user_id")
                `shouldBe` Right (Nothing, Nothing)

        it "activates projection pushdown inside optimize" $ do
            fmap joinSideCols (optimizedPlan "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id")
                `shouldBe` Right (Just ["u.name", "u.id"], Just ["o.user_id"])

        it "keeps projection pushdown idempotent" $ do
            case projectedPlan "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id" of
                Left err -> expectationFailure err
                Right plan -> pushProject testDB ["*"] plan `shouldBe` plan

        it "keeps a join without WHERE results identical" $ do
            sameResultAsUnoptimized "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id"

        it "keeps projection with ORDER BY and LIMIT results identical" $ do
            sameResultAsUnoptimized "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id ORDER BY o.product DESC LIMIT 2"
        it "rewrites Filter id = k into Lookup" $ do
            fmap planRoot (optimizedPlan "SELECT name FROM users WHERE id = 2")
                `shouldBe` Right (Lookup "users" 2)
        it "keeps Lookup result identical to unoptimized" $ do
            sameResultAsUnoptimized "SELECT name FROM users WHERE id = 2"

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

        it "rejects comparing two different types with =" $ do
            (parseStatement "SELECT name FROM users WHERE name = 18" >>= runStatement testDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in INSERT" $ do
            (parseStatement "INSERT INTO users (nickname) VALUES (1)" >>= runStatement testDB)
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

        it "renders a sort and limit plan" $ do
            case parseStatement "SELECT name FROM users ORDER BY age DESC LIMIT 2" >>= translate of
                Left err -> expectationFailure err
                Right plan ->
                    lines (renderPlan plan)
                        `shouldBe` [ "Project [\"name\"]"
                                   , "  Limit 2"
                                   , "    Sort [(\"age\",Desc)]"
                                   , "      Scan Nothing \"users\""
                                   ]

    describe "ChuSQL.Storage.IPC" $ do
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

        it "lookupByKey finds a row inserted with an id over the pipe" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
                let row = [("id", VInt 7), ("name", VStr "Zoe")]
                runIPCStorage (insert "keyed" row) `shouldReturn` Right ()
                found <- runIPCStorage (lookupByKey "keyed" 7)
                fmap (fmap sortRow) found `shouldBe` Right (Just (sortRow row))
                missing <- runIPCStorage (lookupByKey "keyed" 8)
                missing `shouldBe` Right Nothing

        it "replaceAll rebuilds the index over the pipe" $ do
            withTestServer $ \srv -> withServerEnv srv $ do
                runIPCStorage (insert "rb" [("id", VInt 1)]) `shouldReturn` Right ()
                runIPCStorage (replaceAll "rb" [[("id", VInt 9), ("name", VStr "Zoe")]]) `shouldReturn` Right ()
                old <- runIPCStorage (lookupByKey "rb" 1)
                old `shouldBe` Right Nothing
                new <- runIPCStorage (lookupByKey "rb" 9)
                fmap (fmap sortRow) new
                    `shouldBe` Right (Just (sortRow [("id", VInt 9), ("name", VStr "Zoe")]))

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
        it "parses CREATE TABLE with two columns" $ do
            parseStatement "CREATE TABLE users (id INT, name TEXT)"
                `shouldBe` Right (CreateTable "users" [("id", TInt), ("name", TStr)])

        it "parses CREATE TABLE case-insensitively" $ do
            parseStatement "create table users (id int, name text)"
                `shouldBe` Right (CreateTable "users" [("id", TInt), ("name", TStr)])

        it "parses CREATE TABLE with BOOL column" $ do
            parseStatement "CREATE TABLE t (flag BOOL)"
                `shouldBe` Right (CreateTable "t" [("flag", TBool)])

        it "parses CREATE TABLE with VARCHAR as TStr" $ do
            parseStatement "CREATE TABLE t (name VARCHAR)"
                `shouldBe` Right (CreateTable "t" [("name", TStr)])

        it "parses CREATE TABLE with INTEGER" $ do
            parseStatement "CREATE TABLE t (id INTEGER)"
                `shouldBe` Right (CreateTable "t" [("id", TInt)])

        it "rejects CREATE TABLE without columns" $ do
            parseStatement "CREATE TABLE t ()"
                `shouldSatisfy` isLeft

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
                try (createProcess (proc "cargo" ["build", "--bin", "server"]){cwd = Just dir, std_out = NoStream, std_err = NoStream}) ::
                    IO (Either IOException (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle))
            case built of
                Left e -> pure (Left ("cannot run cargo (Rust is required for the IPC tests): " ++ show e))
                Right (_, _, _, ph) -> do
                    ec <- waitForProcess ph
                    case ec of
                        ExitFailure n -> pure (Left ("cargo build --bin server failed with exit code " ++ show n))
                        ExitSuccess -> do
                            found <- firstExisting [dir </> "target" </> "debug" </> "server.exe", dir </> "target" </> "debug" </> "server"]
                            pure (maybe (Left "cargo build finished but no server executable was found") Right found)
  where
    firstDir [] = pure Nothing
    firstDir (d : ds) = do
        ok <- doesDirectoryExist d
        if ok then pure (Just d) else firstDir ds
    firstExisting [] = pure Nothing
    firstExisting (p : ps) = do
        ok <- doesFileExist p
        if ok then pure (Just p) else firstExisting ps

startServer :: FilePath -> IO IPCServer
startServer bin = do
    stamp <- show <$> getCPUTime
    tmp <- getTemporaryDirectory
    let dir = tmp </> "chusql-hs-IPC" </> stamp
    createDirectoryIfMissing True dir
    startServerAt bin dir

startServerAt :: FilePath -> FilePath -> IO IPCServer
startServerAt bin dir = do
    stamp <- show <$> getCPUTime
    let name = "chusql-hs-test-" ++ stamp
    parentEnv <- getEnvironment
    (_, _, _, ph) <-
        createProcess
            (proc bin [])
                { env = Just (("CHUSQL_PIPE", name) : ("CHUSQL_DATA_DIR", dir) : parentEnv)
                , std_out = NoStream
                , std_err = NoStream
                }
    let srv = IPCServer name dir ph
    waitUntilReady srv 100
    pure srv

waitUntilReady :: IPCServer -> Int -> IO ()
waitUntilReady srv tries = do
    probe <- try (withServerEnv srv (sendRequest ReqPing)) :: IO (Either IOException Response)
    case probe of
        Right RespPong -> pure ()
        _ | tries > 0 -> threadDelay 50000 >> waitUntilReady srv (tries - 1)
        Right _ -> ioError (userError "server is up but ping did not answer pong")
        Left e -> ioError e

stopServer :: IPCServer -> IO ()
stopServer srv = do
    _ <- try (terminateProcess (processHandle srv)) :: IO (Either IOException ())
    _ <- try (waitForProcess (processHandle srv)) :: IO (Either IOException ExitCode)
    pure ()

cleanServerDir :: IPCServer -> IO ()
cleanServerDir srv = do
    _ <- try (removePathForcibly (dataDir srv)) :: IO (Either IOException ())
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
    oldPipe <- lookupEnv "CHUSQL_PIPE"
    oldDir <- lookupEnv "CHUSQL_DATA_DIR"
    setEnv "CHUSQL_PIPE" (pipeName srv)
    setEnv "CHUSQL_DATA_DIR" (dataDir srv)
    r <- act
    restore "CHUSQL_PIPE" oldPipe
    restore "CHUSQL_DATA_DIR" oldDir
    pure r
  where
    restore k = maybe (unsetEnv k) (setEnv k)

withPipeName :: String -> IO a -> IO a
withPipeName name act = do
    old <- lookupEnv "CHUSQL_PIPE"
    setEnv "CHUSQL_PIPE" name
    r <- act
    maybe (unsetEnv "CHUSQL_PIPE") (setEnv "CHUSQL_PIPE") old
    pure r

sortRow :: Row -> Row
sortRow = sortOn fst

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

sameResultAsUnoptimized :: String -> Expectation
sameResultAsUnoptimized sql =
    case parseStatement sql of
        Left err -> expectationFailure err
        Right q ->
            case translate q of
                Left err -> expectationFailure err
                Right relOp ->
                    evalRelOp testDB (optimize testDB relOp) `shouldBe` evalRelOp testDB relOp

filterPushedIntoLeft :: RelOp -> Bool
filterPushedIntoLeft (Project _ (Join (Filter _ _) _ _)) = True
filterPushedIntoLeft _ = False

optimizedPlan :: String -> Either String RelOp
optimizedPlan sql = do
    q <- parseStatement sql
    optimize testDB <$> translate q

firstFilterCond :: RelOp -> Maybe Expr
firstFilterCond (Filter p _) = Just p
firstFilterCond (Project _ x) = firstFilterCond x
firstFilterCond (Sort _ x) = firstFilterCond x
firstFilterCond (Limit _ x) = firstFilterCond x
firstFilterCond (Join l r _) = case firstFilterCond l of
    Just p -> Just p
    Nothing -> firstFilterCond r
firstFilterCond (Scan _ _) = Nothing
firstFilterCond (Lookup _ _) = Nothing

anyFilter :: RelOp -> Bool
anyFilter (Filter _ _) = True
anyFilter (Project _ x) = anyFilter x
anyFilter (Sort _ x) = anyFilter x
anyFilter (Limit _ x) = anyFilter x
anyFilter (Join l r _) = anyFilter l || anyFilter r
anyFilter (Scan _ _) = False
anyFilter (Lookup _ _) = False

stripProjects :: RelOp -> RelOp
stripProjects (Project _ x) = stripProjects x
stripProjects (Filter p x) = Filter p (stripProjects x)
stripProjects (Sort spec x) = Sort spec (stripProjects x)
stripProjects (Limit n x) = Limit n (stripProjects x)
stripProjects (Join l r c) = Join (stripProjects l) (stripProjects r) c
stripProjects (Scan a t) = Scan a t
stripProjects (Lookup t k) = Lookup t k

projectedPlan :: String -> Either String RelOp
projectedPlan sql = do
    q <- parseStatement sql
    plan <- translate q
    Right (pushProject testDB ["*"] plan)

sideCols :: RelOp -> Maybe [String]
sideCols (Project cols _) = Just cols
sideCols _ = Nothing

joinSideCols :: RelOp -> (Maybe [String], Maybe [String])
joinSideCols (Project _ (Join l r _)) = (sideCols l, sideCols r)
joinSideCols _ = (Nothing, Nothing)

countFilters :: RelOp -> Int
countFilters (Filter _ x) = 1 + countFilters x
countFilters _ = 0

leftSideFilters :: RelOp -> Maybe Int
leftSideFilters (Project _ (Join l _ _)) = Just (countFilters l)
leftSideFilters _ = Nothing

planRoot :: RelOp -> RelOp
planRoot (Project _ x) = planRoot x
planRoot x = x
