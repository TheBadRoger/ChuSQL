module Main where

import ChuSQL.Eval.Executor
import ChuSQL.Model
import ChuSQL.Syntax.Ast
import ChuSQL.Syntax.Parser

users :: Table
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

db :: Database
db = [("users", users)]

-- runQuery 返回 (更新后的数据库, 结果行)；演示只关心结果行
rowsOf :: Either String (Database, [Row]) -> Either String [Row]
rowsOf = fmap snd

main :: IO ()
main = do
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
