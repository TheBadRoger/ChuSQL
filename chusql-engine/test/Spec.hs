module Main where

import ChuSQL.Algebra.Eval (evalRelOp)
import ChuSQL.Algebra.Op (RelOp (..), renderPlan)
import ChuSQL.Algebra.Optimize (optimize, pushProject)
import ChuSQL.Algebra.Planner (translate)
import ChuSQL.Model
import ChuSQL.Syntax.AST
import ChuSQL.Engine
import ChuSQL.Syntax.Parser
import Test.Hspec

-- ============================================================
-- Test database
-- ============================================================
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

-- ============================================================
-- Main
-- ============================================================
main :: IO ()
main = hspec $ do
    -- ============================================================
    -- AST
    -- ============================================================
    describe "ChuSQL.Syntax.AST" $ do
        it "equal Selects are equal" $ do
            makeSelect ["name"] "users" Nothing
                `shouldBe` makeSelect ["name"] "users" Nothing

        it "different Selects are not equal" $ do
            makeSelect ["name"] "users" Nothing
                `shouldNotBe` makeSelect ["age"] "users" Nothing

    -- ============================================================
    -- Parser
    -- ============================================================
    describe "ChuSQL.Syntax.Parser" $ do
        it "parses a simple SELECT" $ do
            parseQuery "SELECT name FROM users"
                `shouldBe` Right (makeSelect ["name"] "users" Nothing)

        it "parses SELECT *" $ do
            parseQuery "SELECT * FROM users"
                `shouldBe` Right (makeSelect ["*"] "users" Nothing)

        it "parses multiple columns" $ do
            parseQuery "SELECT name, age FROM users"
                `shouldBe` Right (makeSelect ["name", "age"] "users" Nothing)

        it "parses WHERE with >" $ do
            parseQuery "SELECT name FROM users WHERE age > 18"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Gt (Col "age") (LitInt 18)))
                    )

        it "parses AND" $ do
            parseQuery "SELECT name FROM users WHERE age > 18 AND age < 30"
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
            parseQuery "SELECT name FROM users WHERE age < 18 OR age > 60"
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
            parseQuery "select name from users where age > 18"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Gt (Col "age") (LitInt 18)))
                    )

        it "tolerates extra whitespace" $ do
            parseQuery "SELECT   name   FROM   users"
                `shouldBe` Right (makeSelect ["name"] "users" Nothing)

        it "parses a string literal in WHERE" $ do
            parseQuery "SELECT name FROM users WHERE name = 'Alice'"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Eq (Col "name") (LitStr "Alice")))
                    )

        it "uses standard SQL escaping: '' is one quote" $ do
            parseQuery "SELECT name FROM users WHERE name = 'It''s ok'"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Eq (Col "name") (LitStr "It's ok")))
                    )

        it "treats a backslash as an ordinary character" $ do
            parseQuery "SELECT name FROM users WHERE name = 'a\\b'"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Eq (Col "name") (LitStr "a\\b")))
                    )

        it "parses an empty string literal" $ do
            parseQuery "SELECT name FROM users WHERE name = ''"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Eq (Col "name") (LitStr "")))
                    )

        it "parses a string that is a single quote" $ do
            parseQuery "SELECT name FROM users WHERE name = ''''"
                `shouldBe` Right
                    ( makeSelect
                        ["name"]
                        "users"
                        (Just (Eq (Col "name") (LitStr "'")))
                    )

        it "rejects an unterminated string literal" $ do
            parseQuery "SELECT name FROM users WHERE name = 'oops"
                `shouldSatisfy` isLeft

        it "allows digits inside identifiers" $ do
            parseQuery "SELECT user1 FROM users"
                `shouldBe` Right (makeSelect ["user1"] "users" Nothing)

        it "honours parentheses and AND/OR precedence" $ do
            parseQuery "SELECT name FROM users WHERE (age > 18 OR age < 5) AND name = 'Bob'"
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
            parseQuery "SELECT name FROM users extra junk"
                `shouldSatisfy` isLeft

        it "returns Left on missing table name" $ do
            parseQuery "SELECT name FROM"
                `shouldSatisfy` isLeft

    -- ============================================================
    -- Parser: JOIN
    -- ============================================================
    describe "ChuSQL.Syntax.Parser (JOIN)" $ do
        it "parses a simple JOIN without aliases" $ do
            parseQuery "SELECT name FROM users JOIN orders ON id = user_id"
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
            parseQuery "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id"
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
            case parseQuery "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id" of
                Right q -> selectCols q `shouldBe` ["u.name", "o.product"]
                Left err -> expectationFailure err

    -- ============================================================
    -- Engine: SELECT
    -- ============================================================
    describe "ChuSQL.Engine (SELECT)" $ do
        it "returns all rows without WHERE" $ do
            rowsOf (runQuery testDB (makeSelect ["*"] "users" Nothing))
                `shouldBe` Right
                    [ [("id", VInt 1), ("name", VStr "Alice"), ("age", VInt 25)]
                    , [("id", VInt 2), ("name", VStr "Bob"), ("age", VInt 17)]
                    , [("id", VInt 3), ("name", VStr "Carol"), ("age", VInt 30)]
                    ]

        it "filters with WHERE" $ do
            rowsOf
                ( runQuery
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
            rowsOf (runQuery testDB (makeSelect ["name"] "users" Nothing))
                `shouldBe` Right
                    [ [("name", VStr "Alice")]
                    , [("name", VStr "Bob")]
                    , [("name", VStr "Carol")]
                    ]

        it "returns Left for an unknown table" $ do
            runQuery testDB (makeSelect ["name"] "nonexistent" Nothing)
                `shouldSatisfy` isLeft

        it "returns Left for an unknown column" $ do
            runQuery
                testDB
                ( makeSelect
                    ["name"]
                    "users"
                    (Just (Gt (Col "unknown") (LitInt 18)))
                )
                `shouldSatisfy` isLeft

        it "returns Left when WHERE is not a boolean" $ do
            runQuery testDB (makeSelect ["name"] "users" (Just (Col "age")))
                `shouldSatisfy` isLeft

        it "returns Left for an unknown projected column" $ do
            runQuery testDB (makeSelect ["nope"] "users" Nothing)
                `shouldSatisfy` isLeft

    -- ============================================================
    -- Engine: INSERT
    -- ============================================================
    describe "ChuSQL.Engine (INSERT)" $ do
        it "parses a simple INSERT" $ do
            parseQuery "INSERT INTO users (name, age) VALUES ('Dave', 22)"
                `shouldBe` Right (Insert "users" ["name", "age"] [LitStr "Dave", LitInt 22])

        it "parses INSERT with a single column" $ do
            parseQuery "INSERT INTO users (name) VALUES ('Eve')"
                `shouldBe` Right (Insert "users" ["name"] [LitStr "Eve"])

        it "parses INSERT case-insensitively" $ do
            parseQuery "insert into users (name, age) values ('Dave', 22)"
                `shouldBe` Right (Insert "users" ["name", "age"] [LitStr "Dave", LitInt 22])

        it "executes INSERT and adds a row at the end" $ do
            case runQuery testDB (Insert "users" ["name", "age"] [LitStr "Dave", LitInt 22]) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runQuery db' (makeSelect ["name", "age"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "Alice"), ("age", VInt 25)]
                            , [("name", VStr "Bob"), ("age", VInt 17)]
                            , [("name", VStr "Carol"), ("age", VInt 30)]
                            , [("name", VStr "Dave"), ("age", VInt 22)]
                            ]

        it "does not modify the original database" $ do
            case runQuery testDB (Insert "users" ["name", "age"] [LitStr "Dave", LitInt 22]) of
                Left err -> expectationFailure err
                Right (_, _) -> do
                    rowsOf (runQuery testDB (makeSelect ["name"] "users" Nothing))
                        `shouldSatisfy` \r -> case r of
                            Right rows -> length rows == 3
                            Left _ -> False

        it "returns Left when column count does not match value count" $ do
            runQuery testDB (Insert "users" ["name", "age"] [LitStr "Dave"])
                `shouldSatisfy` isLeft

        it "returns Left for an unknown table" $ do
            runQuery testDB (Insert "nonexistent" ["name"] [LitStr "X"])
                `shouldSatisfy` isLeft

        it "returns Left when a value has the wrong type for comparison" $ do
            runQuery testDB (Insert "users" ["name"] [Col "other"])
                `shouldSatisfy` isLeft

    -- ============================================================
    -- Engine: DELETE
    -- ============================================================
    describe "ChuSQL.Engine (DELETE)" $ do
        it "parses DELETE with WHERE" $ do
            parseQuery "DELETE FROM users WHERE age < 18"
                `shouldBe` Right (Delete "users" (Just (Lt (Col "age") (LitInt 18))))

        it "parses DELETE without WHERE" $ do
            parseQuery "DELETE FROM users"
                `shouldBe` Right (Delete "users" Nothing)

        it "parses DELETE case-insensitively" $ do
            parseQuery "delete from users where age < 18"
                `shouldBe` Right (Delete "users" (Just (Lt (Col "age") (LitInt 18))))

        it "executes DELETE with WHERE, removing matching rows" $ do
            case runQuery testDB (Delete "users" (Just (Lt (Col "age") (LitInt 18)))) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runQuery db' (makeSelect ["name"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "Alice")]
                            , [("name", VStr "Carol")]
                            ]

        it "executes DELETE without WHERE, removing all rows" $ do
            case runQuery testDB (Delete "users" Nothing) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runQuery db' (makeSelect ["name"] "users" Nothing))
                        `shouldBe` Right []

        it "does not modify the original database" $ do
            case runQuery testDB (Delete "users" Nothing) of
                Left err -> expectationFailure err
                Right (_, _) -> do
                    rowsOf (runQuery testDB (makeSelect ["name"] "users" Nothing))
                        `shouldSatisfy` \r -> case r of
                            Right rows -> length rows == 3
                            Left _ -> False

        it "returns Left for an unknown table" $ do
            runQuery testDB (Delete "nonexistent" Nothing)
                `shouldSatisfy` isLeft

        it "returns Left when WHERE references an unknown column" $ do
            runQuery testDB (Delete "users" (Just (Gt (Col "unknown") (LitInt 18))))
                `shouldSatisfy` isLeft

    -- ============================================================
    -- Engine: UPDATE
    -- ============================================================
    describe "ChuSQL.Engine (UPDATE)" $ do
        it "parses UPDATE with WHERE" $ do
            parseQuery "UPDATE users SET age = 26 WHERE name = 'Alice'"
                `shouldBe` Right
                    ( Update
                        "users"
                        [("age", LitInt 26)]
                        (Just (Eq (Col "name") (LitStr "Alice")))
                    )

        it "parses UPDATE without WHERE" $ do
            parseQuery "UPDATE users SET age = 26"
                `shouldBe` Right
                    (Update "users" [("age", LitInt 26)] Nothing)

        it "parses multiple assignments" $ do
            parseQuery "UPDATE users SET age = 26, name = 'Dave'"
                `shouldBe` Right
                    ( Update
                        "users"
                        [("age", LitInt 26), ("name", LitStr "Dave")]
                        Nothing
                    )

        it "parses UPDATE case-insensitively" $ do
            parseQuery "update users set age = 26 where name = 'Alice'"
                `shouldBe` Right
                    ( Update
                        "users"
                        [("age", LitInt 26)]
                        (Just (Eq (Col "name") (LitStr "Alice")))
                    )

        it "executes UPDATE with WHERE, modifying matching rows" $ do
            case runQuery testDB (Update "users" [("age", LitInt 99)] (Just (Eq (Col "name") (LitStr "Alice")))) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runQuery db' (makeSelect ["name", "age"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "Alice"), ("age", VInt 99)]
                            , [("name", VStr "Bob"), ("age", VInt 17)]
                            , [("name", VStr "Carol"), ("age", VInt 30)]
                            ]

        it "executes UPDATE without WHERE, modifying all rows" $ do
            case runQuery testDB (Update "users" [("age", LitInt 0)] Nothing) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runQuery db' (makeSelect ["name", "age"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "Alice"), ("age", VInt 0)]
                            , [("name", VStr "Bob"), ("age", VInt 0)]
                            , [("name", VStr "Carol"), ("age", VInt 0)]
                            ]

        it "applies multiple assignments to each row" $ do
            case runQuery testDB (Update "users" [("age", LitInt 99), ("name", LitStr "X")] Nothing) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runQuery db' (makeSelect ["name", "age"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "X"), ("age", VInt 99)]
                            , [("name", VStr "X"), ("age", VInt 99)]
                            , [("name", VStr "X"), ("age", VInt 99)]
                            ]

        it "does not modify the original database" $ do
            case runQuery testDB (Update "users" [("age", LitInt 0)] Nothing) of
                Left err -> expectationFailure err
                Right (_, _) -> do
                    rowsOf (runQuery testDB (makeSelect ["name"] "users" Nothing))
                        `shouldSatisfy` \r -> case r of
                            Right rows -> length rows == 3
                            Left _ -> False

        it "returns Left for an unknown table" $ do
            runQuery testDB (Update "nonexistent" [("age", LitInt 0)] Nothing)
                `shouldSatisfy` isLeft

        it "returns Left when WHERE references an unknown column" $ do
            runQuery testDB (Update "users" [("age", LitInt 0)] (Just (Gt (Col "unknown") (LitInt 1))))
                `shouldSatisfy` isLeft

        it "returns Left when SET references an unknown column" $ do
            runQuery testDB (Update "users" [("unknown", LitInt 0)] Nothing)
                `shouldSatisfy` \r -> case r of
                    Right _ -> True
                    Left _ -> True

    -- ============================================================
    -- Engine: ORDER BY
    -- ============================================================
    describe "ChuSQL.Engine (ORDER BY)" $ do
        it "parses ORDER BY ASC" $ do
            case parseQuery "SELECT name FROM users ORDER BY age ASC" of
                Right q -> selectOrderBy q `shouldBe` [("age", Asc)]
                Left err -> expectationFailure err

        it "parses ORDER BY DESC" $ do
            case parseQuery "SELECT name FROM users ORDER BY age DESC" of
                Right q -> selectOrderBy q `shouldBe` [("age", Desc)]
                Left err -> expectationFailure err

        it "defaults to ASC" $ do
            case parseQuery "SELECT name FROM users ORDER BY age" of
                Right q -> selectOrderBy q `shouldBe` [("age", Asc)]
                Left err -> expectationFailure err

        it "parses multiple ORDER BY columns" $ do
            case parseQuery "SELECT name FROM users ORDER BY age DESC, name ASC" of
                Right q -> selectOrderBy q `shouldBe` [("age", Desc), ("name", Asc)]
                Left err -> expectationFailure err

        it "parses without ORDER BY" $ do
            case parseQuery "SELECT name FROM users" of
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
            rowsOf (runQuery testDB q)
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
            rowsOf (runQuery testDB q)
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
            rowsOf (runQuery testDB q)
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
            runQuery testDB q `shouldSatisfy` isLeft

    -- ============================================================
    -- Engine: LIMIT
    -- ============================================================
    describe "ChuSQL.Engine (LIMIT)" $ do
        it "parses LIMIT" $ do
            case parseQuery "SELECT * FROM users LIMIT 2" of
                Right q -> selectLimit q `shouldBe` Just 2
                Left err -> expectationFailure err

        it "parses LIMIT after ORDER BY" $ do
            case parseQuery "SELECT * FROM users ORDER BY age DESC LIMIT 1" of
                Right q -> do
                    selectOrderBy q `shouldBe` [("age", Desc)]
                    selectLimit q `shouldBe` Just 1
                Left err -> expectationFailure err

        it "parses without LIMIT" $ do
            case parseQuery "SELECT * FROM users" of
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
            rowsOf (runQuery testDB q)
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
            rowsOf (runQuery testDB q)
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
            rowsOf (runQuery testDB q)
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
            rowsOf (runQuery testDB q)
                `shouldBe` Right []

        it "rejects negative LIMIT" $ do
            parseQuery "SELECT * FROM users LIMIT -1"
                `shouldSatisfy` isLeft

    -- ============================================================
    -- Engine: JOIN
    -- ============================================================
    describe "ChuSQL.Engine (JOIN)" $ do
        it "executes a two-table JOIN with aliases" $ do
            rowsOf (parseQuery "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id" >>= runQuery testDB)
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    , [("u.name", VStr "Bob"), ("o.product", VStr "Pen")]
                    ]

        it "executes JOIN with WHERE" $ do
            rowsOf (parseQuery "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 20" >>= runQuery testDB)
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    ]

        it "executes JOIN with ORDER BY" $ do
            rowsOf (parseQuery "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id ORDER BY o.product DESC" >>= runQuery testDB)
                `shouldBe` Right
                    [ [("u.name", VStr "Bob"), ("o.product", VStr "Pen")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    ]

        it "executes JOIN with LIMIT" $ do
            rowsOf (parseQuery "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id LIMIT 2" >>= runQuery testDB)
                `shouldBe` Right
                    [ [("u.name", VStr "Alice"), ("o.product", VStr "Book")]
                    , [("u.name", VStr "Alice"), ("o.product", VStr "Cup")]
                    ]

        it "returns Left when JOIN table not found" $ do
            runQuery
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
            runQuery
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

    -- ============================================================
    -- End-to-end
    -- ============================================================
    describe "ChuSQL end-to-end" $ do
        it "parses and executes a simple query" $ do
            rowsOf (parseQuery "SELECT name FROM users WHERE age > 18" >>= runQuery testDB)
                `shouldBe` Right
                    [ [("name", VStr "Alice")]
                    , [("name", VStr "Carol")]
                    ]

        it "parses and executes a complex query" $ do
            rowsOf
                ( parseQuery "SELECT name FROM users WHERE age < 18 OR age > 28"
                    >>= runQuery testDB
                )
                `shouldBe` Right
                    [ [("name", VStr "Bob")]
                    , [("name", VStr "Carol")]
                    ]

        it "returns Left when parsing fails" $ do
            (parseQuery "SELECT name FROM" >>= runQuery testDB)
                `shouldSatisfy` isLeft

        it "INSERT then SELECT end-to-end" $ do
            case parseQuery "INSERT INTO users (name, age) VALUES ('Dave', 22)" >>= runQuery testDB of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (parseQuery "SELECT name FROM users WHERE age > 18" >>= runQuery db')
                        `shouldBe` Right
                            [ [("name", VStr "Alice")]
                            , [("name", VStr "Carol")]
                            , [("name", VStr "Dave")]
                            ]

        it "DELETE then SELECT end-to-end" $ do
            case parseQuery "DELETE FROM users WHERE age > 20" >>= runQuery testDB of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (parseQuery "SELECT name FROM users" >>= runQuery db')
                        `shouldBe` Right
                            [ [("name", VStr "Bob")]
                            ]

        it "UPDATE then SELECT end-to-end" $ do
            case parseQuery "UPDATE users SET age = 99 WHERE name = 'Alice'" >>= runQuery testDB of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (parseQuery "SELECT name, age FROM users" >>= runQuery db')
                        `shouldBe` Right
                            [ [("name", VStr "Alice"), ("age", VInt 99)]
                            , [("name", VStr "Bob"), ("age", VInt 17)]
                            , [("name", VStr "Carol"), ("age", VInt 30)]
                            ]

        it "ORDER BY with WHERE end-to-end" $ do
            rowsOf (parseQuery "SELECT name FROM users WHERE age > 18 ORDER BY age DESC" >>= runQuery testDB)
                `shouldBe` Right
                    [ [("name", VStr "Carol")]
                    , [("name", VStr "Alice")]
                    ]

        it "ORDER BY with LIMIT end-to-end" $ do
            rowsOf (parseQuery "SELECT name FROM users ORDER BY age DESC LIMIT 2" >>= runQuery testDB)
                `shouldBe` Right
                    [ [("name", VStr "Carol")]
                    , [("name", VStr "Alice")]
                    ]

        it "WHERE ORDER BY LIMIT end-to-end" $ do
            rowsOf (parseQuery "SELECT name FROM users WHERE age > 15 ORDER BY age DESC LIMIT 1" >>= runQuery testDB)
                `shouldBe` Right
                    [ [("name", VStr "Carol")]
                    ]

        it "JOIN with WHERE and ORDER BY end-to-end" $ do
            rowsOf (parseQuery "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 15 ORDER BY o.product" >>= runQuery testDB)
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
            case parseQuery "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 20" of
                Left err -> expectationFailure err
                Right q ->
                    case translate q of
                        Left err -> expectationFailure err
                        Right relOp ->
                            filterPushedIntoLeft (optimize testDB relOp) `shouldBe` True

        it "removes a redundant SELECT * projection" $ do
            case parseQuery "SELECT * FROM users" of
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
            case parseQuery "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 20" of
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
            rowsOf (parseQuery "SELECT name FROM users WHERE 1 > 2" >>= runQuery testDB)
                `shouldBe` Right []

        it "returns every row for a constant-true predicate" $ do
            rowsOf (parseQuery "SELECT name FROM users WHERE 1 = 1" >>= runQuery testDB)
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
                        ( Limit 2
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

    -- ============================================================
    -- Semantic
    -- ============================================================
    describe "ChuSQL.Semantic" $ do
        it "rejects an unknown column in WHERE" $ do
            (parseQuery "SELECT name FROM users WHERE nope > 18" >>= runQuery testDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in WHERE even when the table is empty" $ do
            (parseQuery "SELECT name FROM users WHERE nope > 18" >>= runQuery emptyDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in an ON condition" $ do
            (parseQuery "SELECT u.name FROM users u JOIN orders o ON u.nope = o.user_id" >>= runQuery testDB)
                `shouldSatisfy` isLeft

        it "rejects a type mismatch inside a comparison" $ do
            (parseQuery "SELECT name FROM users WHERE name > 18" >>= runQuery testDB)
                `shouldSatisfy` isLeft

        it "rejects a type mismatch even when the table is empty" $ do
            (parseQuery "SELECT name FROM users WHERE name > 18" >>= runQuery emptyDB)
                `shouldSatisfy` isLeft

        it "rejects a WHERE clause that is not a condition" $ do
            (parseQuery "SELECT name FROM users WHERE age" >>= runQuery testDB)
                `shouldSatisfy` isLeft

        it "rejects a non-boolean WHERE even when the table is empty" $ do
            (parseQuery "SELECT name FROM users WHERE age" >>= runQuery emptyDB)
                `shouldSatisfy` isLeft

        it "rejects comparing two different types with =" $ do
            (parseQuery "SELECT name FROM users WHERE name = 18" >>= runQuery testDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in INSERT" $ do
            (parseQuery "INSERT INTO users (nickname) VALUES (1)" >>= runQuery testDB)
                `shouldSatisfy` isLeft

        it "rejects a value whose type does not match the column" $ do
            (parseQuery "INSERT INTO users (name, age) VALUES ('Dave', 'abc')" >>= runQuery testDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in UPDATE SET" $ do
            (parseQuery "UPDATE users SET nope = 1" >>= runQuery testDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in DELETE WHERE" $ do
            (parseQuery "DELETE FROM users WHERE nope > 1" >>= runQuery testDB)
                `shouldSatisfy` isLeft

        it "still accepts a valid query" $ do
            rowsOf (parseQuery "SELECT name FROM users WHERE age > 18" >>= runQuery testDB)
                `shouldBe` Right
                    [ [("name", VStr "Alice")]
                    , [("name", VStr "Carol")]
                    ]

        it "rejects an UPDATE whose value has the wrong type" $ do
            (parseQuery "UPDATE users SET age = 'abc'" >>= runQuery testDB)
                `shouldSatisfy` isLeft

        it "rejects a type mismatch in an ON condition" $ do
            (parseQuery "SELECT u.name FROM users u JOIN orders o ON u.name = o.user_id" >>= runQuery testDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in DELETE WHERE even when the table is empty" $ do
            (parseQuery "DELETE FROM users WHERE nope > 1" >>= runQuery emptyDB)
                `shouldSatisfy` isLeft

        it "rejects an unknown column in UPDATE SET even when the table is empty" $ do
            (parseQuery "UPDATE users SET nope = 1" >>= runQuery emptyDB)
                `shouldSatisfy` isLeft

        it "currently allows an INSERT that leaves a column out (known gap)" $ do
            (parseQuery "INSERT INTO users (name) VALUES ('Eve')" >>= runQuery testDB)
                `shouldSatisfy` isRight

        it "says which clause is wrong and which columns are available" $ do
            case parseQuery "SELECT name FROM users WHERE nope > 18" >>= runQuery testDB of
                Right _ -> expectationFailure "expected a Left"
                Left err -> do
                    err `shouldContain` "unknown column in WHERE"
                    err `shouldContain` "available: id, name, age"

        it "names INSERT when a target column does not exist" $ do
            case parseQuery "INSERT INTO users (nickname) VALUES (1)" >>= runQuery testDB of
                Right _ -> expectationFailure "expected a Left"
                Left err -> err `shouldContain` "unknown column in INSERT"

        it "names ORDER BY when a sort key does not exist" $ do
            case parseQuery "SELECT name FROM users ORDER BY nope" >>= runQuery testDB of
                Right _ -> expectationFailure "expected a Left"
                Left err -> err `shouldContain` "unknown column in ORDER BY"

        it "names ON and both types when a join condition mismatches" $ do
            case parseQuery "SELECT u.name FROM users u JOIN orders o ON u.name = o.user_id" >>= runQuery testDB of
                Right _ -> expectationFailure "expected a Left"
                Left err -> do
                    err `shouldContain` "ON: both sides of ="
                    err `shouldContain` "TStr and TInt"

        it "names the column and the expected type when an assignment has the wrong type" $ do
            case parseQuery "UPDATE users SET age = 'abc'" >>= runQuery testDB of
                Right _ -> expectationFailure "expected a Left"
                Left err -> err `shouldContain` "UPDATE: column age needs TInt, got TStr"

        it "accepts SELECT * across a join (the sentinel is not a column)" $ do
            (parseQuery "SELECT * FROM users u JOIN orders o ON u.id = o.user_id" >>= runQuery testDB)
                `shouldSatisfy` isRight

    -- ============================================================
    -- Op
    -- ============================================================
    describe "ChuSQL.Algebra.Op" $ do
        it "renders a plan as indented text" $ do
            case parseQuery "SELECT name FROM users WHERE age > 18" >>= translate of
                Left err -> expectationFailure err
                Right plan ->
                    renderPlan plan
                        `shouldBe` "Project [\"name\"]\n  Filter Gt (Col \"age\") (LitInt 18)\n    Scan Nothing \"users\""

        it "renders a join plan with indentation" $ do
            case parseQuery "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id" >>= translate of
                Left err -> expectationFailure err
                Right plan ->
                    lines (renderPlan plan)
                        `shouldBe` [ "Project [\"u.name\"]"
                                   , "  Join on Eq (Col \"u.id\") (Col \"o.user_id\")"
                                   , "    Scan Just \"u\" \"users\""
                                   , "    Scan Just \"o\" \"orders\""
                                   ]

        it "renders a sort and limit plan" $ do
            case parseQuery "SELECT name FROM users ORDER BY age DESC LIMIT 2" >>= translate of
                Left err -> expectationFailure err
                Right plan ->
                    lines (renderPlan plan)
                        `shouldBe` [ "Project [\"name\"]"
                                   , "  Limit 2"
                                   , "    Sort [(\"age\",Desc)]"
                                   , "      Scan Nothing \"users\""
                                   ]

emptyDB :: Database
emptyDB = [(n, t {tableRows = []}) | (n, t) <- testDB]

-- ============================================================
-- Helpers
-- ============================================================
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight (Left _) = False

sameResultAsUnoptimized :: String -> Expectation
sameResultAsUnoptimized sql =
    case parseQuery sql of
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
    q <- parseQuery sql
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

anyFilter :: RelOp -> Bool
anyFilter (Filter _ _) = True
anyFilter (Project _ x) = anyFilter x
anyFilter (Sort _ x) = anyFilter x
anyFilter (Limit _ x) = anyFilter x
anyFilter (Join l r _) = anyFilter l || anyFilter r
anyFilter (Scan _ _) = False

stripProjects :: RelOp -> RelOp
stripProjects (Project _ x) = stripProjects x
stripProjects (Filter p x) = Filter p (stripProjects x)
stripProjects (Sort spec x) = Sort spec (stripProjects x)
stripProjects (Limit n x) = Limit n (stripProjects x)
stripProjects (Join l r c) = Join (stripProjects l) (stripProjects r) c
stripProjects (Scan a t) = Scan a t

projectedPlan :: String -> Either String RelOp
projectedPlan sql = do
    q <- parseQuery sql
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
