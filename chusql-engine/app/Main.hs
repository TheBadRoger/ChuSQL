module Main where

import ChuSQL.Algebra.Op (RelOp (..))
import ChuSQL.Algebra.Optimize (optimize)
import ChuSQL.Algebra.Planner (translate)
import ChuSQL.Model
import ChuSQL.SQLSyntax.AST
import ChuSQL.SQLSyntax.Executor
import ChuSQL.SQLSyntax.Parser

users :: Table
users =
    Table
        { tableName = "users"
        , tableCols = ["id", "name", "age"]
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
        , tableCols = ["id", "user_id", "product"]
        , tableRows =
            [ [("id", VInt 1), ("user_id", VInt 1), ("product", VStr "Book")]
            , [("id", VInt 2), ("user_id", VInt 2), ("product", VStr "Pen")]
            , [("id", VInt 3), ("user_id", VInt 1), ("product", VStr "Cup")]
            ]
        }

db :: Database
db = [("users", users), ("orders", orders)]

-- runQuery 返回 (更新后的数据库, 结果行)；演示只关心结果行
rowsOf :: Either String (Database, [Row]) -> Either String [Row]
rowsOf = fmap snd

main :: IO ()
main = do
    putStrLn "==================== Parser Test ====================="
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
        Right query -> case translate db query of
            Left err -> putStrLn err
            Right plan -> do
                putStrLn "--- plan before optimization ---"
                putStrLn (dump 0 plan)
                putStrLn "--- plan after optimization ---"
                putStrLn (dump 0 (optimize db plan))
                putStrLn "--- rows ---"
                print (rowsOf (runQuery db query))

dump :: Int -> RelOp -> String
dump ind op = case op of
    Scan a t ->
        pad ++ "Scan " ++ show a ++ " " ++ show t
    Filter e x ->
        pad ++ "Filter " ++ show e ++ "\n" ++ dump (ind + 1) x
    Project c x ->
        pad ++ "Project " ++ show c ++ "\n" ++ dump (ind + 1) x
    Sort s x ->
        pad ++ "Sort " ++ show s ++ "\n" ++ dump (ind + 1) x
    Limit n x ->
        pad ++ "Limit " ++ show n ++ "\n" ++ dump (ind + 1) x
    Join l r c ->
        pad
            ++ "Join on "
            ++ show c
            ++ "\n"
            ++ dump (ind + 1) l
            ++ "\n"
            ++ dump (ind + 1) r
  where
    pad = replicate (ind * 2) ' '
