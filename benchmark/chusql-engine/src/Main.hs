module Main where

import ChuSQL.Algebra.Eval (evalRelOp)
import ChuSQL.Algebra.Optimize (optimize)
import ChuSQL.Algebra.Planner (translate)
import ChuSQL.Model
import ChuSQL.Syntax.Parser (parseStatement)
import Control.Exception (evaluate)
import Data.IORef (IORef, newIORef, readIORef)
import System.CPUTime (getCPUTime)
import System.Environment (getArgs)
import Text.Printf (printf)

-- 性能基准：同一批语句，比较优化前后的耗时。

-- | 造一张 users 表
mkUsers :: Int -> Table
mkUsers n =
    Table
        { tableName = "users"
        , tableCols = [("id", TInt), ("name", TStr), ("age", TInt)]
        , tableRows =
            [ [("id", VInt i), ("name", VStr ("user" ++ show i)), ("age", VInt (i `mod` 100))]
            | i <- [1 .. n]
            ]
        }

-- | 造一张 orders 表
mkOrders :: Int -> Int -> Table
mkOrders n m =
    Table
        { tableName = "orders"
        , tableCols = [("id", TInt), ("user_id", TInt), ("product", TStr)]
        , tableRows =
            [ [("id", VInt j), ("user_id", VInt (1 + (j - 1) `mod` n)), ("product", VStr ("item" ++ show (j `mod` 50)))]
            | j <- [1 .. m]
            ]
        }

-- | 结果校验和（防被优化掉）
checksum :: Either String [Row] -> Int
checksum (Left _) = -1
checksum (Right rows) = go 0 rows
  where
    -- \| 累加行长
    go acc [] = acc
    go acc (r : rs) = go (acc + length r) rs

-- | 结果行数
rowCount :: Either String [Row] -> Int
rowCount (Left _) = -1
rowCount (Right rows) = length rows

-- | 跑一次，返回校验和
runOnce :: IORef String -> Database -> String -> Bool -> IO Int
runOnce saltRef db sql useOpt = do
    salt <- readIORef saltRef
    let result = case parseStatement (sql ++ salt) >>= translate of
            Left e -> Left e
            Right plan -> evalRelOp db (if useOpt then optimize db plan else plan)
    forced <- evaluate (checksum result)
    return forced

-- | 跑一次，返回行数
resultRows :: IORef String -> Database -> String -> Bool -> IO Int
resultRows saltRef db sql useOpt = do
    salt <- readIORef saltRef
    let result = case parseStatement (sql ++ salt) >>= translate of
            Left e -> Left e
            Right plan -> evalRelOp db (if useOpt then optimize db plan else plan)
    _ <- evaluate (checksum result)
    return (rowCount result)

-- | 量一段 IO 的 CPU 时间
timed :: IO a -> IO Double
timed act = do
    t0 <- getCPUTime
    _ <- act
    t1 <- getCPUTime
    return (fromIntegral (t1 - t0) / 1e12)

-- | 加倍批量次数再折算单次
benchOp :: Double -> IO Int -> IO (Double, Int)
benchOp target act = loop 1
  where
    -- \| 批次翻倍直到超过目标
    loop k = do
        t <- timed (mapM_ (const act) [1 .. k])
        if t < target then loop (k * 2) else return (t / fromIntegral k, k)

-- | 打印一个用例的对比
report :: IORef String -> Database -> Double -> String -> IO ()
report saltRef db target sql = do
    nUn <- resultRows saltRef db sql False
    nOp <- resultRows saltRef db sql True
    (tUn, kUn) <- benchOp target (runOnce saltRef db sql False)
    (tOp, kOp) <- benchOp target (runOnce saltRef db sql True)
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

-- | 基准入口
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
    saltRef <- newIORef ""
    _ <- runOnce saltRef db warmupSql False
    _ <- runOnce saltRef db warmupSql True
    mapM_ (report saltRef db target) queries
