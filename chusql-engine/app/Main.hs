module Main where

import ChuSQL.Algebra.Op (renderPlan)
import ChuSQL.Algebra.Optimize (optimize)
import ChuSQL.Algebra.Planner (translate)
import ChuSQL.Engine
import ChuSQL.Model
import ChuSQL.Syntax.AST
import ChuSQL.Syntax.Parser

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

db :: Database
db = [("users", users), ("orders", orders)]

main :: IO ()
main = do
    putStrLn "==================== Engine ====================="
    let q1 = makeSelect ["name"] "users" (Just (Gt (Col "age") (LitInt 18)))
    print (rowsOf (runQuery db q1))

    let q2 = makeSelect ["*"] "users" Nothing
    print (rowsOf (runQuery db q2))

    let q3 = makeSelect ["name"] "nonexistent" Nothing
    print (rowsOf (runQuery db q3))

    let q4 =
            makeSelect
                ["name"]
                "users"
                ( Just
                    ( Or
                        (Lt (Col "age") (LitInt 18))
                        (Gt (Col "age") (LitInt 30))
                    )
                )
    print (rowsOf (runQuery db q4))

    -- 解析 SQL 文本，再交给执行引擎
    let parsed = parseQuery "SELECT name FROM users WHERE age > 18 AND name = 'Alice'"
    print (rowsOf (parsed >>= runQuery db))

    putStrLn "\n==================== Optimizer ====================="
    let planSql =
            "SELECT u.name, o.product FROM users u \
            \JOIN orders o ON u.id = o.user_id WHERE u.age > 18"
    case parseQuery planSql of
        Left err -> putStrLn err
        Right query -> case translate query of
            Left err -> putStrLn err
            Right plan -> do
                putStrLn "--- plan before optimization ---"
                putStrLn (renderPlan plan)
                putStrLn "--- plan after optimization ---"
                putStrLn (renderPlan (optimize db plan))
                putStrLn "--- rows ---"
                print (rowsOf (runQuery db query))
