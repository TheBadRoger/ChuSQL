{-# LANGUAGE OverloadedStrings #-}

module Main where

import ChuSQL.Core.Protocol (QueryResult (..), TableInfo (..))
import ChuSQL.Interface.Link (Client, clientPing, closeClient, connectClient)
import ChuSQL.Interface.Session (
    Session,
    authenticateSession,
    catalog,
    newSession,
    runStatement,
    switchDatabase,
 )
import Control.Concurrent (forkIO, forkFinally, threadDelay)
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (IOException, bracket, evaluate, throwIO, try)
import Control.Monad (forM, forM_, replicateM, unless, when)
import Data.List (intercalate, sort)
import ChuSQL.Core.Model (Value (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import System.Directory (
    createDirectoryIfMissing,
    findExecutable,
    getTemporaryDirectory,
    removeDirectoryRecursive,
    removeFile,
 )
import System.Environment (getArgs)
import System.Exit (ExitCode (..), exitFailure)
import System.FilePath ((</>))
import System.Info (os)
import System.IO (Handle, hGetLine)
import System.Process (
    CreateProcess (std_err, std_out),
    ProcessHandle,
    StdStream (CreatePipe, NoStream),
    createProcess,
    proc,
    readProcessWithExitCode,
    terminateProcess,
    waitForProcess,
 )
import Text.Printf (printf)
import Text.Read (readMaybe)

-- 全链路基准：客户端经 TCP 连 chusql-server，落盘到进程内 Rust 存储。
-- 每条语句都走完整链路：解析、语义检查、提交。
-- 对照项：ping 与 catalog。

-- | 一个基准世界：真 server 进程 + 一条 TCP 会话
data Bench = Bench
    { bProcess :: ProcessHandle
    , bDataDir :: FilePath
    , bClient :: Client
    , bSession :: Session
    , bPort :: Int
    , bConfigPath :: FilePath
    }

-- | 跑一条 SQL 文本（整条链路）
runSql :: Bench -> String -> IO (Either Text QueryResult)
runSql bench sql = do
    result <- runStatement (bSession bench) (T.pack sql)
    case result of
        Left err -> ioError (userError (T.unpack err))
        Right _ -> pure result

-- | 量一次墙上耗时并强制求出行数
timedQuery :: Bench -> String -> IO (Double, Either Text Int)
timedQuery bench sql = do
    t0 <- getMonotonicTime
    r <- runSql bench sql
    n <- case r of
        Left _ -> pure (-1)
        Right result -> evaluate (qrRowCount result)
    t1 <- getMonotonicTime
    pure (t1 - t0, either Left (const (Right n)) r)

-- | 打印一行结果
report :: String -> Either Text Int -> Double -> IO ()
report label result dt = case result of
    Left e -> printf "%-38s %10s   !! %s\n" label ("FAILED" :: String) (T.unpack e)
    Right n -> printf "%-38s %8.1f ms %8d rows\n" label (dt * 1000) n

-- | 单独量一条可重复的 SQL（先预热）
benchSql :: Bench -> String -> String -> IO (Either Text Int)
benchSql bench label sql = do
    _ <- runSql bench sql
    (dt, r) <- timedQuery bench sql
    report label r dt
    pure r

-- | 只跑一次的语句（重跑会报错）
benchSqlOnce :: Bench -> String -> String -> IO (Either Text Int)
benchSqlOnce bench label sql = do
    (dt, r) <- timedQuery bench sql
    report label r dt
    pure r

-- | 批量插入并报平均耗时
batchInsert :: Bench -> String -> Int -> (Int -> String) -> IO (Either Text ())
batchInsert bench label n mkSql = do
    t0 <- getMonotonicTime
    results <- mapM (runSql bench . mkSql) [1 .. n]
    t1 <- getMonotonicTime
    let total = t1 - t0
        errs = [e | Left e <- results]
    case errs of
        [] -> do
            printf
                "%-38s %8.1f ms %7.3f ms/row\n"
                label
                (total * 1000)
                (total * 1000 / fromIntegral n)
            pure (Right ())
        (e : _) -> do
            printf "%-38s %10s   !! %s\n" label ("FAILED" :: String) (T.unpack e)
            pure (Left e)

-- | 纯 TCP 往返：只过协议层，不过引擎与存储
benchPing :: Bench -> Int -> IO ()
benchPing bench times = do
    _ <- clientPing (bClient bench)
    t0 <- getMonotonicTime
    results <- mapM (const (clientPing (bClient bench))) [1 .. times]
    t1 <- getMonotonicTime
    let total = t1 - t0
        hits = length [() | Right _ <- results]
    printf
        "%-38s %8.1f ms %7.3f ms/op (pongs %d/%d)\n"
        ("tcp round trip (ping, no engine)" :: String)
        (total * 1000)
        (total * 1000 / fromIntegral times)
        hits
        times
    unless (hits == times) (ioError (userError "ping failed during benchmark"))

-- | 一次点查（走语句，等于把 id 索引那一支也量进去）
benchLookup :: Bench -> Int -> Int -> IO ()
benchLookup bench key times = do
    let sql = printf "SELECT name FROM users WHERE id = %d" key
    _ <- runSql bench sql
    t0 <- getMonotonicTime
    results <- mapM (const (runSql bench sql)) [1 .. times]
    t1 <- getMonotonicTime
    let total = t1 - t0
        hits = length [() | Right result <- results, qrRowCount result > 0]
    printf
        "%-38s %8.1f ms %7.3f ms/op (hits %d/%d)\n"
        ("point lookup by id (statement, over TCP)" :: String)
        (total * 1000)
        (total * 1000 / fromIntegral times)
        hits
        times
    unless (hits == times) (ioError (userError "point lookup failed during benchmark"))

-- | 量 list_catalog（只取结构，不取行）
benchCatalog :: Bench -> Int -> IO ()
benchCatalog bench n = do
    _ <- catalog (bSession bench)
    t0 <- getMonotonicTime
    results <- mapM (const (catalog (bSession bench))) [1 .. n]
    t1 <- getMonotonicTime
    let total = t1 - t0
        hits = length [() | Right _ <- results]
    printf
        "%-38s %8.1f ms %7.3f ms/op (%d/%d ok)\n"
        ("list_catalog (schema only, over TCP)" :: String)
        (total * 1000)
        (total * 1000 / fromIntegral n)
        hits
        n
    unless (hits == n) (ioError (userError "catalog failed during benchmark"))

-- | 依次跑一批语句，返回 (耗时秒, 错误清单)
runBatch :: Bench -> [String] -> IO (Double, [Text])
runBatch bench sqls = do
    t0 <- getMonotonicTime
    results <- mapM (runSql bench) sqls
    t1 <- getMonotonicTime
    pure (t1 - t0, [e | Left e <- results])

-- | 把 1..n 按 size 切成一段段
chunked :: Int -> Int -> [[Int]]
chunked size n = go [1 .. n]
  where
    -- | 递归切分
    go [] = []
    go xs = let (a, b) = splitAt size xs in a : go b

-- | 一条带多行的 INSERT（一条语句 N 行，只落一次盘）
multiInsertSql :: [Int] -> String
multiInsertSql ids =
    "INSERT INTO batch_t (id, name, age) VALUES "
        ++ intercalate ", " [printf "(%d, 'b%d', %d)" i i (i `mod` 100) | i <- ids]

-- | 打印一张表的统计
printStats :: TableInfo -> IO ()
printStats i =
    printf
        "  %-12s %6d rows  indexes=%s\n             stats=%s\n"
        (tiTable i)
        (tiRows i)
        (if null (tiIndexes i) then "none" else unwords (tiIndexes i))
        ( unwords
            [ n ++ ":" ++ show d ++ (if c then "+ (lower bound)" else "")
            | (n, d, c) <- tiStats i
            ]
        )

-- | 主场景
runScenarios :: Bench -> Int -> Int -> IO ()
runScenarios bench u o = do
    putStrLn "\n[create tables]"
    r1 <- benchSqlOnce bench "CREATE TABLE users" "CREATE TABLE users (id int, name str, age int)"
    r2 <- benchSqlOnce bench "CREATE TABLE orders" "CREATE TABLE orders (id int, user_id int, product str)"
    case (r1, r2) of
        (Right _, Right _) -> pure ()
        _ -> do
            putStrLn "table creation failed; stopping here"
            exitFailure

    putStrLn "\n[write: 500 rows per durable INSERT commit]"
    loadBatch bench "users" "id,name,age" u (\i -> printf "(%d,'user%d',%d)" i i (i `mod` 100))
    loadBatch bench "orders" "id,user_id,product" o (\i -> printf "(%d,%d,'item%d')" i (1 + (i - 1) `mod` u) (i `mod` 50))
    checkCount bench "users" u
    checkCount bench "orders" o

    putStrLn "\n[breakdown: protocol round trip vs one catalog call, both over TCP]"
    benchPing bench 200
    benchCatalog bench 200

    putStrLn "\n[read/query: full statement, including parsing and semantic checks]"
    _ <- benchSql bench "SELECT * FROM users" "SELECT * FROM users"
    ageR <- benchSql bench "SELECT name FROM users WHERE age > 90" "SELECT name FROM users WHERE age > 90"

    putStrLn "\n[read/query: join]"
    joinR <-
        benchSql
            bench
            "JOIN + WHERE (hash join)"
            "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 90"

    putStrLn "\n[point lookup]"
    benchLookup bench (u `div` 2) 100
    pressureScenarios bench u o

    putStrLn "\n[batch insert: one statement with N rows, one durable commit]"
    _ <- benchSqlOnce bench "CREATE TABLE batch_t" "CREATE TABLE batch_t (id int, name str, age int)"
    let groups = chunked 100 u
        perStmt = maximum (0 : map length groups)
    (bt, batchErrs) <- runBatch bench (map multiInsertSql groups)
    case batchErrs of
        [] ->
            printf
                "%-38s %8.1f ms %7.3f ms/row (%d rows/stmt x %d statements)\n"
                ("multi-row INSERT" :: String)
                (bt * 1000)
                (bt * 1000 / fromIntegral u)
                (perStmt :: Int)
                (length groups)
        (e : _) -> printf "%-38s %10s   !! %s\n" ("multi-row INSERT" :: String) ("FAILED" :: String) (T.unpack e)

    putStrLn "\n[row delete: cost tracks the scan plus one commit, not the row count]"
    _ <- benchSqlOnce bench "DELETE FROM users WHERE id = k (1 row)" (printf "DELETE FROM users WHERE id = %d" (u `div` 2))
    _ <- benchSqlOnce bench "DELETE FROM users (all)" "DELETE FROM users"

    putStrLn "\n[secondary index: falls back to scan, then uses the index once built]"
    _ <- benchSqlOnce bench "CREATE TABLE items" "CREATE TABLE items (id int, code int)"
    let itemGroups = chunked 100 o
        itemSql ids =
            "INSERT INTO items (id, code) VALUES "
                ++ intercalate ", " [printf "(%d, %d)" i (i * 7 + 1) | i <- ids]
    (it, itemErrs) <- runBatch bench (map itemSql itemGroups)
    case itemErrs of
        [] -> pure ()
        (e : _) -> printf "  !! %s\n" (T.unpack e)
    printf "  (loaded %d rows into items: %.1f ms)\n" o (it * 1000)
    let targetCode = (o `div` 2) * 7 + 1
    _ <-
        benchSql
            bench
            "SELECT ... WHERE code = k (no index)"
            (printf "SELECT id FROM items WHERE code = %d" targetCode)
    _ <- benchSqlOnce bench "CREATE INDEX ON items (code)" "CREATE INDEX ON items (code)"
    _ <-
        benchSql
            bench
            "SELECT ... WHERE code = k (via index)"
            (printf "SELECT id FROM items WHERE code = %d" targetCode)

    putStrLn "\n[catalog stats]"
    cat <- catalog (bSession bench)
    case cat of
        Left e -> printf "  failed to read stats: %s\n" (T.unpack e)
        Right infos -> mapM_ printStats infos

    putStrLn "\n[checks]"
    let expectAge = 9 * (u `div` 100) + max 0 (u `mod` 100 - 90)
    printf
        "age > 90 matched %s (expected %d rows from the data distribution)\n"
        (either T.unpack show ageR)
        expectAge
    when (u == o) $
        printf
            "JOIN matched %s (one order per user, should equal the line above = %d)\n"
            (either T.unpack show joinR)
            expectAge
    unless (u == o) $
        putStrLn "(users and orders differ in size; no simple formula, printing only)"
    loadedCheck <- runSql bench "SELECT id FROM batch_t"
    printf
        "multi-row INSERT loaded %s rows (expected %d)\n"
        (either T.unpack (show . qrRowCount) loadedCheck)
        u
    printf "secondary index: code = %d should find id = %d\n" targetCode (o `div` 2)
    checkCount bench "batch_t" u
    putStrLn "BENCHMARK_OK all scenarios completed"

-- | 基准用的库名
benchDatabase :: String
benchDatabase = "bench"

-- | 服务端没有默认库：先开一个库，再把之后的语句都落进它
prepareDatabase :: Session -> IO ()
prepareDatabase session = do
    created <- runStatement session ("CREATE DATABASE " <> T.pack benchDatabase)
    case created of
        Left e -> die ("cannot create the benchmark database: " ++ T.unpack e)
        Right _ -> do
            used <- switchDatabase session (T.pack benchDatabase)
            case used of
                Left e -> die ("cannot select the benchmark database: " ++ T.unpack e)
                Right () -> pure ()
  where
    -- | 报错退出
    die message = do
        putStrLn message
        exitFailure

-- | 按平台补上 .exe 后缀
binaryName :: String -> String
binaryName stem = if os == "mingw32" then stem ++ ".exe" else stem

-- | 在 PATH 里找 server 可执行文件
locateServerExe :: IO FilePath
locateServerExe = do
    found <- findExecutable (binaryName "chusql-server")
    case found of
        Just path -> pure path
        Nothing -> do
            putStrLn "chusql-server was not found on PATH: run `stack build` first"
            exitFailure

-- | 在 PATH 里找引导程序（server 需要先引导）
locateBootstrapExe :: IO FilePath
locateBootstrapExe = do
    found <- findExecutable (binaryName "csql-bootstrap")
    case found of
        Just path -> pure path
        Nothing -> do
            putStrLn "csql-bootstrap was not found on PATH: run `stack build` first"
            exitFailure

-- | 从 server 输出里读出监听端口
readListeningPort :: Handle -> IO Int
readListeningPort handle = do
    line <- hGetLine handle
    case portFromBanner line of
        Just port -> pure port
        Nothing -> readListeningPort handle

-- | 取一行里最后一串数字
portFromBanner :: String -> Maybe Int
portFromBanner line
    | not ("listening:" `isInfixOf` line) = Nothing
    | otherwise = case span isDigit (dropWhile (not . isDigit) (reverse line)) of
        ([], _) -> Nothing
        (digits, _) -> Just (read (reverse digits))

-- | 判断子串是否出现
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (prefix needle) (tails haystack)
  where
    -- | 前缀匹配
    prefix [] _ = True
    prefix _ [] = False
    prefix (x : xs) (y : ys) = x == y && prefix xs ys
    -- | 所有后缀
    tails [] = [[]]
    tails s@(_ : rest) = s : tails rest

-- | 判断是不是数字字符
isDigit :: Char -> Bool
isDigit ch = ch >= '0' && ch <= '9'

-- | 把 server 的 stdout 抽干，免得管道塞满
drainHandle :: Handle -> IO ()
drainHandle handle = do
    result <- try (hGetLine handle) :: IO (Either IOException String)
    case result of
        Left _ -> pure ()
        Right _ -> drainHandle handle

-- | 起 server、连上去、登录、开库
startBench :: IO Bench
startBench = do
    bin <- locateServerExe
    tmp <- getTemporaryDirectory
    stamp <- getMonotonicTime
    let name = "benchmark4csql-" ++ show (round (stamp * 1e6) :: Int)
        dataDir = tmp </> name
        configPath = tmp </> (name ++ ".toml")
        -- TOML 的普通字符串会吃反斜杠，路径统一用正斜杠
        slashed = map (\c -> if c == '\\' then '/' else c) dataDir
    createDirectoryIfMissing True dataDir
    writeFile configPath (benchConfigText slashed)
    printf "config   : %s\n" configPath
    printf "data dir : %s\n" dataDir
    bootstrap <- locateBootstrapExe
    (bootCode, bootOut, bootErr) <- readProcessWithExitCode bootstrap ["--config", configPath, "--passwordless", "--user", "admin"] ""
    case bootCode of
        ExitSuccess -> printf "bootstrap: system catalog ready\n"
        _ -> do
            putStrLn ("the bootstrap program failed (exit " ++ show bootCode ++ "): " ++ bootOut ++ bootErr)
            exitFailure
    (_, Just out, _, process) <-
        createProcess (proc bin ["--config", configPath, "--user", "admin"]){std_out = CreatePipe, std_err = NoStream}
    port <- readListeningPort out
    _ <- forkIO (drainHandle out)
    printf "server   : pid launched, tcp port %d\n" port
    linked <- connectClient "127.0.0.1" port
    case linked of
        Left err -> do
            putStrLn ("cannot connect to the server: " ++ T.unpack err)
            stopProcess process
            exitFailure
        Right client -> do
            session <- newSession client
            auth <- authenticateSession session (T.pack "admin") (T.pack "")
            case auth of
                Left err -> do
                    putStrLn ("cannot sign in: " ++ T.unpack err)
                    closeClient client
                    stopProcess process
                    exitFailure
                Right () -> do
                    prepareDatabase session
                    pure (Bench process dataDir client session port configPath)

-- | 基准用的配置：随机端口、临时数据目录
benchConfigText :: String -> String
benchConfigText dataDir =
    unlines
        [ "[server]"
        , "port = 0"
        , ""
        , "[storage]"
        , "data_dir = \"" ++ dataDir ++ "\""
        , ""
        , "[log]"
        , "level = \"error\""
        ]

-- | 收摊：关会话、停 server、清数据目录
stopBench :: Bench -> IO ()
stopBench bench = do
    putStrLn "\n[teardown] closing the TCP session, stopping the server, removing the data dir"
    closeClient (bClient bench)
    stopProcess (bProcess bench)
    removeDirectoryRecursive (bDataDir bench)
    removeFile (bConfigPath bench)

-- | 杀掉 server 进程
stopProcess :: ProcessHandle -> IO ()
stopProcess process = do
    terminateProcess process
    _ <- waitForProcess process
    pure ()

-- | 主入口
main :: IO ()
main = do
    args <- getArgs
    unless (length args <= 2) (ioError (userError "usage: benchmark4csql [users=10000] [orders=50000]"))
    let values = take 2 (args ++ drop (length args) ["10000", "50000"])
    sizes <- mapM positiveSize values
    (u, o) <- case sizes of
        [usersSize, ordersSize] -> pure (usersSize, ordersSize)
        _ -> ioError (userError "expected two benchmark sizes")
    printf "full-chain benchmark: TCP client --> chusql-server --> in-process Rust storage\n"
    printf "dataset  : users = %d rows, orders = %d rows\n" u o
    printf "timing   : wall clock (monotonic); every statement runs the full chain over TCP\n"
    printf "note     : the client no longer links the engine or the storage library\n"
    bracket
        (do
            t1 <- getMonotonicTime
            bench <- startBench
            t2 <- getMonotonicTime
            printf "\n[startup] server up, storage ready, signed in: %.2f s\n" (t2 - t1)
            threadDelay 25000
            pure bench
        )
        stopBench
        (\bench -> runScenarios bench u o)

-- | 校验压力规模是至少一百的整数
positiveSize :: String -> IO Int
positiveSize raw = case readMaybe raw of
    Just size | size >= 100 -> pure size
    _ -> ioError (userError "benchmark row counts must be integers >= 100")

-- | 批量加载并报告每秒写入行数
loadBatch :: Bench -> String -> String -> Int -> (Int -> String) -> IO ()
loadBatch bench table columns count row = do
    (milliseconds, ()) <- measure $ forM_ (chunked 500 count) $ \ids -> do
        _ <- runSql bench ("INSERT INTO " ++ table ++ " (" ++ columns ++ ") VALUES " ++ intercalate "," (map row ids))
        pure ()
    printf "LOAD %-16s rows=%d total_ms=%.3f rows_s=%.2f\n" table count milliseconds (fromIntegral count * 1000 / milliseconds)

-- | 校验完整数据加载数量
checkCount :: Bench -> String -> Int -> IO ()
checkCount bench table expected = do
    result <- runSql bench ("SELECT COUNT(*) FROM " ++ table)
    case result of
        Right rows | qrRows rows == [[VInt expected]] -> pure ()
        _ -> ioError (userError ("count mismatch in " ++ table ++ ": " ++ show result))

-- | 用单调时钟测量动作耗时
measure :: IO a -> IO (Double, a)
measure action = do
    start <- getMonotonicTime
    result <- action >>= evaluate
    end <- getMonotonicTime
    pure ((end - start) * 1000, result)

-- | 打印均值分位数和请求吞吐量
latencies :: String -> Int -> [Double] -> IO ()
latencies label rows times = do
    let ordered = sort times
        count = length times
        total = sum times
        -- | 按最近秩取分位数
        percentile fraction = ordered !! (ceiling (fraction * fromIntegral count) - 1)
    printf "CASE %-24s n=%d rows=%d mean_ms=%.3f p50_ms=%.3f p95_ms=%.3f max_ms=%.3f ops_s=%.2f\n" label count rows (total / fromIntegral count) (percentile (0.5 :: Double)) (percentile (0.95 :: Double)) (last ordered) (fromIntegral count * 1000 / total)

-- | 预热后反复测量并校验结果行数
sampleQuery :: Bench -> String -> String -> Int -> IO ()
sampleQuery bench label statement expected = do
    -- | 执行一次并校验结果数量
    let action = do
            result <- runSql bench statement
            case result of
                Right rows | qrRowCount rows == expected -> pure ()
                _ -> ioError (userError (label ++ ": result mismatch " ++ show result))
    _ <- action
    times <- replicateM 7 (fst <$> measure action)
    latencies label expected times

-- | 运行索引聚合连接事务与并发压力
pressureScenarios :: Bench -> Int -> Int -> IO ()
pressureScenarios bench u o = do
    putStrLn "\n[pressure: one warmup, seven measured samples per read case]"
    let selected = length [() | i <- [1 .. o], (1 + (i - 1) `mod` u) `mod` 100 > 90]
        point = printf "SELECT id FROM users WHERE name = 'user%d'" (u `div` 2)
    sampleQuery bench "scan_users" "SELECT * FROM users" u
    sampleQuery bench "scan_orders" "SELECT * FROM orders" o
    sampleQuery bench "id_point" (printf "SELECT id FROM users WHERE id = %d" (u `div` 2)) 1
    sampleQuery bench "name_scan" point 1
    _ <- benchSqlOnce bench "CREATE INDEX users(name)" "CREATE INDEX ON users (name)"
    sampleQuery bench "name_index" point 1
    sampleQuery bench "id_range100" "SELECT id FROM users WHERE id >= 1 AND id <= 100" 100
    sampleQuery bench "sort_top100" "SELECT name FROM users ORDER BY age DESC LIMIT 100" 100
    sampleQuery bench "group100" "SELECT age, COUNT(*) FROM users GROUP BY age" 100
    sampleQuery bench "filtered_join" "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 90" selected
    sampleQuery bench "derived_join" "SELECT d.name FROM (SELECT id,name FROM users WHERE age > 90) d JOIN orders o ON d.id = o.user_id" selected
    sampleQuery bench "cte_group" "WITH ages AS (SELECT age FROM users) SELECT age, COUNT(*) FROM ages GROUP BY age" 100
    sampleQuery bench "scalar_subquery" "SELECT name, (SELECT COUNT(*) FROM orders) FROM users LIMIT 100" 100
    sampleQuery bench "correlated_exists20" "SELECT u.name FROM (SELECT id,name FROM users WHERE id <= 20) u WHERE EXISTS (SELECT id FROM orders o WHERE o.user_id = u.id)" 20
    _ <- runSql bench "CREATE TABLE wide (id int, payload str)"
    loadBatch bench "wide" "id,payload" (min 5000 u) (\i -> printf "(%d,'%s')" i (replicate 512 'x'))
    checkCount bench "wide" (min 5000 u)
    sampleQuery bench "scan_wide512B" "SELECT * FROM wide" (min 5000 u)
    mapM_ (concurrentLookup bench) [1, 4, 8]
    _ <- runSql bench "CREATE TABLE writes (id int, name str, age int)"
    _ <- batchInsert bench "200 individual durable INSERTs" 200 (\i -> printf "INSERT INTO writes (id,name,age) VALUES (%d,'w%d',%d)" i i i)
    (milliseconds, ()) <- measure $ do
        _ <- runSql bench "BEGIN"
        forM_ [201 .. 400 :: Int] $ \i -> runSql bench (printf "INSERT INTO writes (id,name,age) VALUES (%d,'w%d',%d)" i i i) >> pure ()
        _ <- runSql bench "COMMIT"
        pure ()
    printf "WRITE transaction200 total_ms=%.3f rows_s=%.2f\n" milliseconds (200000 / milliseconds)
    checkCount bench "writes" 400
    _ <- benchSqlOnce bench "UPDATE writes (400 rows)" "UPDATE writes SET age = age + 1"
    _ <- runSql bench "BEGIN"
    _ <- benchSqlOnce bench "transaction UPDATE (400 rows)" "UPDATE writes SET age = 0"
    _ <- benchSqlOnce bench "ROLLBACK 400 changed rows" "ROLLBACK"
    restored <- runSql bench "SELECT age FROM writes WHERE id = 1"
    unless (fmap qrRows restored == Right [[VInt 2]]) (ioError (userError "rollback value mismatch"))
    _ <- benchSqlOnce bench "DELETE writes (200 rows)" "DELETE FROM writes WHERE id > 200"
    checkCount bench "writes" 200
    putStrLn "PRESSURE_OK all result checks passed"

-- | 用多条独立会话同步开始点查
concurrentLookup :: Bench -> Int -> IO ()
concurrentLookup bench workers = do
    completions <- forM [1 .. workers] $ \_ -> do
        start <- newEmptyMVar
        done <- newEmptyMVar
        client <- connectClient "127.0.0.1" (bPort bench) >>= either (ioError . userError . T.unpack) pure
        session <- newSession client
        authenticateSession session "admin" "" >>= either (ioError . userError . T.unpack) pure
        switchDatabase session "bench" >>= either (ioError . userError . T.unpack) pure
        _ <- forkFinally (takeMVar start >> bracket (pure client) closeClient (\_ -> replicateM 100 (fst <$> measure (do
            result <- runStatement session "SELECT id FROM users WHERE id = 1"
            unless (fmap qrRows result == Right [[VInt 1]]) (ioError (userError "concurrent lookup mismatch")))))) (putMVar done)
        pure (start, done)
    (milliseconds, results) <- measure $ do
        mapM_ (\(start, _) -> putMVar start ()) completions
        mapM (takeMVar . snd) completions
    times <- concat <$> mapM (either throwIO pure) results
    latencies ("concurrent" ++ show workers ++ "_point") 1 times
    printf "CONCURRENT workers=%d ops=%d wall_ms=%.3f aggregate_ops_s=%.2f\n" workers (workers * 100) milliseconds (fromIntegral (workers * 100) * 1000 / milliseconds)
