module Main where

import ChuSQL.Algebra.Eval (evalRelOp)
import ChuSQL.Algebra.Optimize (optimize)
import ChuSQL.Algebra.Planner (translate)
import ChuSQL.Model
import ChuSQL.SQLSyntax.Parser (parseQuery)
import Control.Exception (evaluate)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf (printf)

mkUsers :: Int -> Table
mkUsers n =
    Table
        { tableName = "users"
        , tableCols = ["id", "name", "age"]
        , tableRows =
            [ [("id", VInt i), ("name", VStr ("user" ++ show i)), ("age", VInt (i `mod` 100))]
            | i <- [1 .. n]
            ]
        }

mkOrders :: Int -> Int -> Table
mkOrders n m =
    Table
        { tableName = "orders"
        , tableCols = ["id", "user_id", "product"]
        , tableRows =
            [ [("id", VInt j), ("user_id", VInt (1 + (j - 1) `mod` n)), ("product", VStr ("item" ++ show (j `mod` 50)))]
            | j <- [1 .. m]
            ]
        }

checksum :: Either String [Row] -> Int
checksum (Left _) = -1
checksum (Right rows) = go 0 rows
  where
    go acc [] = acc
    go acc (r : rs) = go (acc + length r) rs

rowCount :: Either String [Row] -> Int
rowCount (Left _) = -1
rowCount (Right rows) = length rows

runOnce :: Database -> String -> Bool -> IO Int
runOnce db sql useOpt = do
    let result = case parseQuery sql >>= translate db of
            Left e -> Left e
            Right plan -> evalRelOp db (if useOpt then optimize db plan else plan)
    forced <- evaluate (checksum result)
    return forced

resultRows :: Database -> String -> Bool -> IO Int
resultRows db sql useOpt = do
    let result = case parseQuery sql >>= translate db of
            Left e -> Left e
            Right plan -> evalRelOp db (if useOpt then optimize db plan else plan)
    _ <- evaluate (checksum result)
    return (rowCount result)

timed :: IO a -> IO Double
timed act = do
    t0 <- getCPUTime
    _ <- act
    t1 <- getCPUTime
    return (fromIntegral (t1 - t0) / 1e12)

benchOp :: Double -> IO Int -> IO (Double, Int)
benchOp target act = loop 1
  where
    loop k = do
        t <- timed (mapM_ (const act) [1 .. k])
        if t < target then loop (k * 2) else return (t / fromIntegral k, k)

report :: Database -> Double -> String -> IO ()
report db target sql = do
    nUn <- resultRows db sql False
    nOp <- resultRows db sql True
    (tUn, kUn) <- benchOp target (runOnce db sql False)
    (tOp, kOp) <- benchOp target (runOnce db sql True)
    let speedup = if tOp <= 0 then 0 else tUn / tOp
        mismatch = if nUn == nOp then "" else "   <<< 结果不一致！"
    printf "%s\n" sql
    printf
        "    结果行数 %-6d | 未优化 %9.3f ms | 优化后 %9.3f ms | 加速 %6.1fx%s\n"
        nOp
        (tUn * 1000)
        (tOp * 1000)
        speedup
        mismatch
    printf "    （实测批次：未优化 %d 次/批，优化后 %d 次/批）\n\n" kUn kOp

queries :: [String]
queries =
    [ "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 90"
    , "SELECT u.name, o.product FROM users u JOIN orders o ON u.id = o.user_id"
    , "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 90 ORDER BY o.product LIMIT 10"
    , "SELECT name FROM users WHERE age > 90"
    ]

warmupSql :: String
warmupSql = "SELECT name FROM users"

main :: IO ()
main = do
    args <- getArgs
    let pick i d = if length args > i then read (args !! i) else d
        n = pick 0 500
        m = pick 1 500
        target = pick 2 (0.3 :: Double)
        db = [("users", mkUsers n), ("orders", mkOrders n m)]
    printf "数据规模：users = %d 行，orders = %d 行\n" n m
    printf "计时方式：CPU 时间；每个用例自动加倍批量次数，直到一批累计耗时超过 %.2f 秒，再折算成单次耗时\n" target
    printf "（先跑一遍预热，避免把首次构造数据的开销算进去）\n\n"
    _ <- runOnce db warmupSql False
    _ <- runOnce db warmupSql True
    mapM_ (report db target) queries
