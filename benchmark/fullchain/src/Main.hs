module Main where

import ChuSQL.Engine (runStatementM)
import ChuSQL.Model (Row, Value (..))
import ChuSQL.Storage (IndexResult (..))
import ChuSQL.Storage.IPC (
    IPCStorage (..),
    Request (..),
    Response (..),
    TableInfo (..),
    closeConnection,
    doInsert,
    doListCatalog,
    doLookupByColumn,
    sendRequest,
 )
import ChuSQL.Syntax.Parser (parseStatement)
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, evaluate, try)
import Control.Monad (filterM, unless, when)
import Data.List (intercalate, isInfixOf, isPrefixOf)
import GHC.Clock (getMonotonicTime)
import System.Directory (
    createDirectoryIfMissing,
    doesFileExist,
    getTemporaryDirectory,
    removeDirectoryRecursive,
 )
import System.Environment (getArgs, getEnvironment, setEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.Process (
    CreateProcess (..),
    ProcessHandle,
    StdStream (..),
    createProcess,
    proc,
    terminateProcess,
    waitForProcess,
 )
import Text.Printf (printf)

-- 全链路基准：Haskell 引擎与 Rust 存储进程一起跑，
-- 语句走完整链路（解析、优化、命名管道、WAL、落盘），
-- 用墙上时钟计时，结果打印到标准输出。

-- | 找 Rust 存储进程：优先 release
findServer :: FilePath -> IO FilePath
findServer "" = do
    let dirs =
            [ "chusql-storage/target"
            , "../../chusql-storage/target"
            , "../chusql-storage/target"
            , "target"
            ]
        modes = ["release", "debug"]
        exes = ["chusql-storage.exe", "chusql-storage"]
        candidates = [d </> m </> e | d <- dirs, m <- modes, e <- exes]
    found <- filterM doesFileExist candidates
    case found of
        (p : _) -> do
            unless ("release" `isInfixOf` p) $
                putStrLn "!! using the debug storage binary; numbers will be slower, run: cargo build --release"
            pure p
        [] -> do
            putStrLn "Rust storage binary not found. Build it first:"
            putStrLn "    cd chusql-storage && cargo build --release"
            exitFailure
findServer p = do
    ok <- doesFileExist p
    if ok
        then pure p
        else do
            putStrLn ("storage binary not found: " ++ p)
            exitFailure

-- | 子进程环境：继承并覆盖 CHUSQL_*
childEnv :: [(String, String)] -> IO [(String, String)]
childEnv extra = do
    base <- getEnvironment
    let stripped = filter (\(k, _) -> not ("CHUSQL_" `isPrefixOf` k)) base
    pure (stripped ++ extra)

-- | 起存储进程并等管道可连
startServer :: FilePath -> String -> FilePath -> IO ProcessHandle
startServer serverPath pipe dataDir = do
    envs <-
        childEnv
            [ ("CHUSQL_PIPE", pipe)
            , ("CHUSQL_DATA_DIR", dataDir)
            , ("CHUSQL_PAGE_SIZE", "4096")
            , ("CHUSQL_BTREE_ORDER", "4")
            , ("CHUSQL_LOG", "error")
            ]
    (_, _, _, ph) <-
        createProcess
            (proc serverPath []) {env = Just envs, std_out = NoStream, std_err = Inherit}
    ready <- waitReady (200 :: Int)
    unless ready $ do
        putStrLn "storage process was not ready within 5 s"
        exitFailure
    pure ph

-- | 轮询 ping，直到对面应答
waitReady :: Int -> IO Bool
waitReady 0 = pure False
waitReady n = do
    r <- try (sendRequest ReqPing) :: IO (Either SomeException Response)
    case r of
        Right RespPong -> pure True
        _ -> threadDelay 25000 >> waitReady (n - 1)

-- | 停存储进程并清掉缓存连接
stopServer :: ProcessHandle -> FilePath -> IO ()
stopServer ph dataDir = do
    closeConnection
    terminateProcess ph
    _ <- waitForProcess ph
    _ <- try (removeDirectoryRecursive dataDir) :: IO (Either SomeException ())
    pure ()

-- | 量一次墙上耗时并强制求出行数
timedRows :: IO (Either String [Row]) -> IO (Double, Either String Int)
timedRows act = do
    t0 <- getMonotonicTime
    r <- act
    n <- case r of
        Left _ -> pure (-1)
        Right rows -> evaluate (length rows)
    t1 <- getMonotonicTime
    pure (t1 - t0, either Left (const (Right n)) r)

-- | 跑一条 SQL 文本（整条链路）
runSql :: String -> IO (Either String [Row])
runSql sql = case parseStatement sql of
    Left e -> pure (Left e)
    Right stmt -> runIPCStorage (runStatementM stmt)

-- | 打印一行结果
report :: String -> Either String Int -> Double -> IO ()
report label result dt = case result of
    Left e -> printf "%-38s %10s   !! %s\n" label "FAILED" e
    Right n -> printf "%-38s %8.1f ms %8d rows\n" label (dt * 1000) n

-- | 单独量一条可重复的 SQL（先预热）
benchSql :: String -> String -> IO (Either String Int)
benchSql label sql = do
    _ <- runSql sql
    (dt, r) <- timedRows (runSql sql)
    report label r dt
    pure r

-- | 只跑一次的语句（重跑会报错）
benchSqlOnce :: String -> String -> IO (Either String Int)
benchSqlOnce label sql = do
    (dt, r) <- timedRows (runSql sql)
    report label r dt
    pure r

-- | 造一条 INSERT 的 SQL 文本
userSql :: Int -> String
userSql i =
    printf "INSERT INTO users (id, name, age) VALUES (%d, 'user%d', %d)" i i (i `mod` 100)

-- | 造一条订单的 INSERT
orderSql :: Int -> Int -> String
orderSql u j =
    printf
        "INSERT INTO orders (id, user_id, product) VALUES (%d, %d, 'item%d')"
        j
        (1 + (j - 1) `mod` u)
        (j `mod` 50)

-- | 批量插入并报平均耗时
batchInsert :: String -> Int -> (Int -> String) -> IO (Either String ())
batchInsert label n mkSql = do
    t0 <- getMonotonicTime
    results <- mapM (runSql . mkSql) [1 .. n]
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
            printf "%-38s %10s   !! %s\n" label "FAILED" e
            pure (Left e)

-- | 一次点查（原始 IPC，不经过引擎）
benchLookup :: String -> String -> Int -> Int -> IO ()
benchLookup table column key times = do
    _ <- doLookupByColumn table column key
    t0 <- getMonotonicTime
    results <- mapM (const (doLookupByColumn table column key)) [1 .. times]
    t1 <- getMonotonicTime
    let total = t1 - t0
        hit = length [() | Right (IndexRow (Just _)) <- results]
    printf
        "%-38s %8.1f ms %7.3f ms/op (hits %d/%d)\n"
        "lookup_by_index (raw IPC, by id)"
        (total * 1000)
        (total * 1000 / fromIntegral times)
        hit
        times

-- | 量 list_catalog（只取结构，不取行）
benchListCatalog :: Int -> IO ()
benchListCatalog n = do
    t0 <- getMonotonicTime
    _ <- mapM (const doListCatalog) [1 .. n]
    t1 <- getMonotonicTime
    let total = t1 - t0
    printf
        "%-38s %8.1f ms %7.3f ms/op\n"
        "list_catalog (schema only)"
        (total * 1000)
        (total * 1000 / fromIntegral n)

-- | 量原始 IPC 插入（不经过引擎）
benchRawInsert :: String -> [Row] -> IO ()
benchRawInsert table rows = do
    t0 <- getMonotonicTime
    results <- mapM (\r -> doInsert table r) rows
    t1 <- getMonotonicTime
    let total = t1 - t0
        n = length rows
        errs = [e | Left e <- results]
    case errs of
        [] ->
            printf
                "%-38s %8.1f ms %7.3f ms/row\n"
                ("raw IPC insert x " ++ show n)
                (total * 1000)
                (total * 1000 / fromIntegral n)
        (e : _) -> printf "%-38s %10s   !! %s\n" "raw IPC insert" "FAILED" e

-- | 造一行 users 数据（给原始 IPC 用）
userRow :: Int -> Row
userRow i = [("id", VInt i), ("name", VStr ("user" ++ show i)), ("age", VInt (i `mod` 100))]

-- | 依次跑一批语句，返回 (耗时秒, 错误清单)
runBatch :: [String] -> IO (Double, [String])
runBatch sqls = do
    t0 <- getMonotonicTime
    results <- mapM runSql sqls
    t1 <- getMonotonicTime
    pure (t1 - t0, [e | Left e <- results])

-- | 把 1..n 按 size 切成一段段
chunked :: Int -> Int -> [[Int]]
chunked size n = go [1 .. n]
  where
    -- | 每次切下 size 个
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
runScenarios :: Int -> Int -> IO ()
runScenarios u o = do
    putStrLn "\n[create tables]"
    r1 <- benchSqlOnce "CREATE TABLE users" "CREATE TABLE users (id int, name str, age int)"
    r2 <- benchSqlOnce "CREATE TABLE orders" "CREATE TABLE orders (id int, user_id int, product str)"
    case (r1, r2) of
        (Right _, Right _) -> pure ()
        _ -> do
            putStrLn "table creation failed; stopping here"
            exitFailure

    putStrLn "\n[write: every INSERT is a full durable commit]"
    _ <- batchInsert (printf "%d INSERT INTO users" u) u userSql
    _ <- batchInsert (printf "%d INSERT INTO orders" o) o (orderSql u)

    putStrLn "\n[breakdown: same INSERT split in two, both via raw IPC, no engine]"
    _ <- benchSqlOnce "CREATE TABLE raw_users" "CREATE TABLE raw_users (id int, name str, age int)"
    benchListCatalog 200
    benchRawInsert "raw_users" [userRow i | i <- [1 .. u]]

    putStrLn "\n[read/query: full statement, including parsing and semantic checks]"
    _ <- benchSql "SELECT * FROM users" "SELECT * FROM users"
    ageR <- benchSql "SELECT name FROM users WHERE age > 90" "SELECT name FROM users WHERE age > 90"

    putStrLn "\n[read/query: join]"
    joinR <-
        benchSql
            "JOIN + WHERE (hash join)"
            "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 90"

    putStrLn "\n[point lookup]"
    benchLookup "users" "id" (u `div` 2) 100
    _ <- benchSql "SELECT ... WHERE id = k (via Lookup)" (printf "SELECT name FROM users WHERE id = %d" (u `div` 2))

    putStrLn "\n[batch insert: one statement with N rows, one durable commit]"
    _ <- benchSqlOnce "CREATE TABLE batch_t" "CREATE TABLE batch_t (id int, name str, age int)"
    let groups = chunked 100 u
    (bt, batchErrs) <- runBatch (map multiInsertSql groups)
    case batchErrs of
        [] ->
            printf
                "%-38s %8.1f ms %7.3f ms/row (%d rows/stmt x %d statements)\n"
                "multi-row INSERT"
                (bt * 1000)
                (bt * 1000 / fromIntegral u)
                (100 :: Int)
                (length groups)
        (e : _) -> printf "%-38s %10s   !! %s\n" "multi-row INSERT" "FAILED" e

    putStrLn "\n[row delete: cost tracks the scan plus one commit, not the row count]"
    _ <- benchSqlOnce "DELETE FROM users WHERE id = k (1 row)" (printf "DELETE FROM users WHERE id = %d" (u `div` 2))
    _ <- benchSqlOnce "DELETE FROM users (all)" "DELETE FROM users"

    putStrLn "\n[secondary index: falls back to scan, then uses the index once built]"
    _ <- benchSqlOnce "CREATE TABLE items" "CREATE TABLE items (id int, code int)"
    let itemGroups = chunked 100 o
        itemSql ids =
            "INSERT INTO items (id, code) VALUES "
                ++ intercalate ", " [printf "(%d, %d)" i (i * 7 + 1) | i <- ids]
    (it, itemErrs) <- runBatch (map itemSql itemGroups)
    case itemErrs of
        [] -> pure ()
        (e : _) -> printf "  !! %s\n" e
    printf "  (loaded %d rows into items: %.1f ms)\n" o (it * 1000)
    let targetCode = (o `div` 2) * 7 + 1
    _ <-
        benchSql
            "SELECT ... WHERE code = k (no index)"
            (printf "SELECT id FROM items WHERE code = %d" targetCode)
    _ <- benchSqlOnce "CREATE INDEX ON items (code)" "CREATE INDEX ON items (code)"
    _ <-
        benchSql
            "SELECT ... WHERE code = k (via index)"
            (printf "SELECT id FROM items WHERE code = %d" targetCode)

    putStrLn "\n[catalog stats]"
    cat <- doListCatalog
    case cat of
        Left e -> printf "  failed to read stats: %s\n" e
        Right infos -> mapM_ printStats infos

    putStrLn "\n[checks]"
    let expectAge = 9 * (u `div` 100) + max 0 (u `mod` 100 - 90)
    printf
        "age > 90 matched %s (expected %d rows from the data distribution)\n"
        (either show show ageR)
        expectAge
    when (u == o) $
        printf
            "JOIN matched %s (one order per user, should equal the line above = %d)\n"
            (either show show joinR)
            expectAge
    unless (u == o) $
        putStrLn "(users and orders differ in size; no simple formula, printing only)"
    printf "multi-row INSERT loaded %d rows (expected %d)\n" u u
    printf "secondary index: code = %d should find id = %d\n" targetCode (o `div` 2)

-- | 主入口
main :: IO ()
main = do
    args <- getArgs
    let pick i d = if length args > i then args !! i else d
        u = read (pick 0 "200") :: Int
        o = read (pick 1 "200") :: Int
        serverArg = pick 2 ""
    serverPath <- findServer serverArg

    tmp <- getTemporaryDirectory
    stamp <- getMonotonicTime
    let pipe = "chusql-fullchain-" ++ show (round (stamp * 1e6) :: Int)
        dataDir = tmp </> pipe
    createDirectoryIfMissing True dataDir
    setEnv "CHUSQL_PIPE" pipe

    printf "full-chain benchmark: Haskell engine <--named pipe--> Rust storage\n"
    printf "storage  : %s\n" serverPath
    printf "pipe     : %s\n" pipe
    printf "data dir : %s\n" dataDir
    printf "dataset  : users = %d rows, orders = %d rows\n" u o
    printf "timing   : wall clock (monotonic); every statement runs the full chain\n"

    bracket
        (do
            t1 <- getMonotonicTime
            ph <- startServer serverPath pipe dataDir
            t2 <- getMonotonicTime
            printf "\n[startup] storage ready in %.2f s\n" (t2 - t1)
            pure ph
        )
        (\ph -> do
            putStrLn "\n[teardown] stopping storage and removing the data dir"
            stopServer ph dataDir
        )
        (const (runScenarios u o))
