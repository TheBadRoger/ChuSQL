module Main where

import ChuSQL.Eval.Executor
import ChuSQL.Model
import ChuSQL.Syntax.Ast
import ChuSQL.Syntax.Parser
import Test.Hspec

-- Test database
testDB :: Database
testDB = [("users", users)]
  where
    users =
        Table
            { tableName = "users"
            , tableCols = ["name", "age"]
            , tableRows =
                [ [("name", VStr "Alice"), ("age", VInt 25)]
                , [("name", VStr "Bob"), ("age", VInt 17)]
                , [("name", VStr "Carol"), ("age", VInt 30)]
                ]
            }

main :: IO ()
main = hspec $ do
    -- ============================================================
    -- AST
    -- ============================================================
    describe "ChuSQL.Syntax.Ast" $ do
        it "shows a Select without WHERE" $ do
            show (makeSelect ["name"] "users" Nothing)
                `shouldBe` "Select {selectCols = [\"name\"], selectTable = \"users\", \
                           \selectWhere = Nothing, selectOrderBy = [], selectLimit = Nothing}"

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
            parseQuery "SELECT name FROM users extra"
                `shouldSatisfy` isLeft

        it "returns Left on missing table name" $ do
            parseQuery "SELECT name FROM"
                `shouldSatisfy` isLeft

    -- ============================================================
    -- Engine: SELECT
    -- ============================================================
    describe "ChuSQL.Eval.Engine (SELECT)" $ do
        it "returns all rows without WHERE" $ do
            rowsOf (runQuery testDB (makeSelect ["*"] "users" Nothing))
                `shouldBe` Right
                    [ [("name", VStr "Alice"), ("age", VInt 25)]
                    , [("name", VStr "Bob"), ("age", VInt 17)]
                    , [("name", VStr "Carol"), ("age", VInt 30)]
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
    describe "ChuSQL.Eval.Engine (INSERT)" $ do
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
                    rowsOf (runQuery db' (makeSelect ["*"] "users" Nothing))
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
                    rowsOf (runQuery testDB (makeSelect ["*"] "users" Nothing))
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
    describe "ChuSQL.Eval.Engine (DELETE)" $ do
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
                    rowsOf (runQuery db' (makeSelect ["*"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "Alice"), ("age", VInt 25)]
                            , [("name", VStr "Carol"), ("age", VInt 30)]
                            ]

        it "executes DELETE without WHERE, removing all rows" $ do
            case runQuery testDB (Delete "users" Nothing) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runQuery db' (makeSelect ["*"] "users" Nothing))
                        `shouldBe` Right []

        it "does not modify the original database" $ do
            case runQuery testDB (Delete "users" Nothing) of
                Left err -> expectationFailure err
                Right (_, _) -> do
                    rowsOf (runQuery testDB (makeSelect ["*"] "users" Nothing))
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
    describe "ChuSQL.Eval.Engine (UPDATE)" $ do
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
                    rowsOf (runQuery db' (makeSelect ["*"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "Alice"), ("age", VInt 99)]
                            , [("name", VStr "Bob"), ("age", VInt 17)]
                            , [("name", VStr "Carol"), ("age", VInt 30)]
                            ]

        it "executes UPDATE without WHERE, modifying all rows" $ do
            case runQuery testDB (Update "users" [("age", LitInt 0)] Nothing) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runQuery db' (makeSelect ["*"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "Alice"), ("age", VInt 0)]
                            , [("name", VStr "Bob"), ("age", VInt 0)]
                            , [("name", VStr "Carol"), ("age", VInt 0)]
                            ]

        it "applies multiple assignments to each row" $ do
            case runQuery testDB (Update "users" [("age", LitInt 99), ("name", LitStr "X")] Nothing) of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (runQuery db' (makeSelect ["*"] "users" Nothing))
                        `shouldBe` Right
                            [ [("name", VStr "X"), ("age", VInt 99)]
                            , [("name", VStr "X"), ("age", VInt 99)]
                            , [("name", VStr "X"), ("age", VInt 99)]
                            ]

        it "does not modify the original database" $ do
            case runQuery testDB (Update "users" [("age", LitInt 0)] Nothing) of
                Left err -> expectationFailure err
                Right (_, _) -> do
                    rowsOf (runQuery testDB (makeSelect ["*"] "users" Nothing))
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
    describe "ChuSQL.Eval.Engine (ORDER BY)" $ do
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
                        , selectTable = "users"
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
                        , selectTable = "users"
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
                        , selectTable = "users"
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
                        , selectTable = "users"
                        , selectWhere = Nothing
                        , selectOrderBy = [("nope", Asc)]
                        , selectLimit = Nothing
                        }
            runQuery testDB q `shouldSatisfy` isLeft

    -- ============================================================
    -- Engine: LIMIT
    -- ============================================================
    describe "ChuSQL.Eval.Engine (LIMIT)" $ do
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
                        , selectTable = "users"
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
                        , selectTable = "users"
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
                        , selectTable = "users"
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
                        , selectTable = "users"
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
                    rowsOf (parseQuery "SELECT * FROM users" >>= runQuery db')
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

        it "ORDER BY then INSERT then SELECT end-to-end" $ do
            case parseQuery "INSERT INTO users (name, age) VALUES ('Dave', 99)" >>= runQuery testDB of
                Left err -> expectationFailure err
                Right (db', _) -> do
                    rowsOf (parseQuery "SELECT name FROM users ORDER BY age DESC" >>= runQuery db')
                        `shouldBe` Right
                            [ [("name", VStr "Dave")]
                            , [("name", VStr "Carol")]
                            , [("name", VStr "Alice")]
                            , [("name", VStr "Bob")]
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

-- ============================================================
-- Helpers
-- ============================================================
isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

rowsOf :: Either String (Database, [Row]) -> Either String [Row]
rowsOf = fmap snd
