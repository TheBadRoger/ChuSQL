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
import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (IOException, bracket, evaluate, try)
import Control.Monad (unless, when)
import Data.List (intercalate)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Clock (getMonotonicTime)
import System.Directory (
    createDirectoryIfMissing,
    findExecutable,
    getTemporaryDirectory,
    removeDirectoryRecursive,
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
    terminateProcess,
    waitForProcess,
 )
import Text.Printf (printf)

-- 全链路基准：客户端经 TCP 连 chusql-server，落盘到进程内 Rust 存储。
-- 每条语句都走完整链路：解析、语义检查、提交。
-- 对照项：ping 与 catalog。

-- | 一个基准世界：真 server 进程 + 一条 TCP 会话
data Bench = Bench
    { bProcess :: ProcessHandle
    , bDataDir :: FilePath
    , bClient :: Client
    , bSession :: Session
    }

-- | 跑一条 SQL 文本（整条链路）
runSql :: Bench -> String -> IO (Either Text QueryResult)
runSql bench sql = runStatement (bSession bench) (T.pack sql)

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

    putStrLn "\n[write: every INSERT is a full durable commit]"
    _ <- batchInsert bench (printf "%d INSERT INTO users" u) u userSql
    _ <- batchInsert bench (printf "%d INSERT INTO orders" o) o (orderSql u)

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
    let name = "chusql-fullchain-" ++ show (round (stamp * 1e6) :: Int)
        dataDir = tmp </> name
        configPath = tmp </> (name ++ ".toml")
        -- TOML 的普通字符串会吃反斜杠，路径统一用正斜杠
        slashed = map (\c -> if c == '\\' then '/' else c) dataDir
    createDirectoryIfMissing True dataDir
    writeFile configPath (benchConfigText slashed)
    printf "config   : %s\n" configPath
    printf "data dir : %s\n" dataDir
    (_, Just out, _, process) <-
        createProcess (proc bin ["--config", configPath]){std_out = CreatePipe, std_err = NoStream}
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
            auth <- authenticateSession session (T.pack "admin") (T.pack "s3cret")
            case auth of
                Left err -> do
                    putStrLn ("cannot sign in: " ++ T.unpack err)
                    closeClient client
                    stopProcess process
                    exitFailure
                Right () -> do
                    prepareDatabase session
                    pure (Bench process dataDir client session)

-- | 基准用的配置：管理员凭据、随机端口、临时数据目录
benchConfigText :: String -> String
benchConfigText dataDir =
    unlines
        [ "[web]"
        , "user = \"admin\""
        , "password = \"s3cret\""
        , ""
        , "[server]"
        , "host = \"127.0.0.1\""
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
    _ <- try (removeDirectoryRecursive (bDataDir bench)) :: IO (Either IOException ())
    pure ()

-- | 杀掉 server 进程
stopProcess :: ProcessHandle -> IO ()
stopProcess process = do
    terminateProcess process
    _ <- try (waitForProcess process) :: IO (Either IOException ExitCode)
    pure ()

-- | 主入口
main :: IO ()
main = do
    args <- getArgs
    let pick i d = if length args > i then args !! i else d
        u = read (pick 0 "200") :: Int
        o = read (pick 1 "200") :: Int
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
