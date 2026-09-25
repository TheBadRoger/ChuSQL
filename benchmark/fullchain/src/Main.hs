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

-- 全链路基准：Haskell 引擎和 Rust 存储进程**一起启动**，跑真实情景。
--
-- 一条语句真正走过的路：
--   SQL 文本 → 词法/语法分析 → 语义检查 → 关系代数 → 命名管道（JSON 行）
--            → Rust：WAL（fsync）→ 堆表/B+ 树 → 页缓冲池 → 落盘
--            → 结果原路返回
--
-- 计时用**墙上时钟（单调时钟）**，不是 CPU 时间：这条链路大部分工夫花在等 IO
-- 和跨进程往返上，用 CPU 时间会把这些等待整个漏掉。
--
-- 跑法（先把 Rust 那边编译成 release）：
--   cd chusql-storage && cargo build --release
--   cd benchmark/fullchain && stack run              # 默认 users=orders=200
--   stack run -- 500 500                             # 换规模
--   stack run -- 200 200 ../../chusql-storage/target/release/server.exe   # 指定存储进程

-- * 命令行与存储进程

-- | 找 Rust 存储进程：优先 release，其次 debug
findServer :: FilePath -> IO FilePath
findServer "" = do
    let dirs =
            [ "chusql-storage/target"
            , "../../chusql-storage/target"
            , "../chusql-storage/target"
            , "target"
            ]
        modes = ["release", "debug"]
        exes = ["server.exe", "server"]
        candidates = [d </> m </> e | d <- dirs, m <- modes, e <- exes]
    found <- filterM doesFileExist candidates
    case found of
        (p : _) -> do
            unless ("release" `isInfixOf` p) $
                putStrLn "!! 找到的是 debug 版存储进程，数字会偏慢；建议 cargo build --release"
            pure p
        [] -> do
            putStrLn "找不到 Rust 存储进程。先编译："
            putStrLn "    cd chusql-storage && cargo build --release"
            exitFailure
findServer p = do
    ok <- doesFileExist p
    if ok
        then pure p
        else do
            putStrLn ("指定的存储进程不存在：" ++ p)
            exitFailure

-- | 子进程环境：继承当前环境，但把 CHUSQL_* 换成本次专用的
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
        putStrLn "存储进程 5 秒内没就绪"
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

-- * 计时

-- | 量一次墙上耗时，并把结果行数强制出来（Haskell 是惰性的，不强制就等于没测）
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
    Left e -> printf "%-38s %10s   !! %s\n" label "失败" e
    Right n -> printf "%-38s %8.1f ms %8d 行\n" label (dt * 1000) n

-- | 单独量一条 SQL（先预热一遍）：只适合可以重复跑的语句（SELECT）
benchSql :: String -> String -> IO (Either String Int)
benchSql label sql = do
    _ <- runSql sql -- 预热：连接、页缓存、优化器都先热一次
    (dt, r) <- timedRows (runSql sql)
    report label r dt
    pure r

-- | 只跑一次的语句（CREATE TABLE 这类重跑会报错的）
benchSqlOnce :: String -> String -> IO (Either String Int)
benchSqlOnce label sql = do
    (dt, r) <- timedRows (runSql sql)
    report label r dt
    pure r

-- * 场景

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
                "%-38s %8.1f ms %7.3f ms/条\n"
                label
                (total * 1000)
                (total * 1000 / fromIntegral n)
            pure (Right ())
        (e : _) -> do
            printf "%-38s %10s   !! %s\n" label "失败" e
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
        "%-38s %8.1f ms %7.3f ms/次（命中 %d/%d）\n"
        "lookup_by_index（原始 IPC，按 id）"
        (total * 1000)
        (total * 1000 / fromIntegral times)
        hit
        times

-- | 量 list_catalog：现在每条语句都要先来一发（只取结构，不取行）
benchListCatalog :: Int -> IO ()
benchListCatalog n = do
    t0 <- getMonotonicTime
    _ <- mapM (const doListCatalog) [1 .. n]
    t1 <- getMonotonicTime
    let total = t1 - t0
    printf
        "%-38s %8.1f ms %7.3f ms/次\n"
        "list_catalog（取结构）"
        (total * 1000)
        (total * 1000 / fromIntegral n)

-- | 量原始 IPC 插入：不经过引擎，把"协议 + 存储"和"引擎"分开
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
                "%-38s %8.1f ms %7.3f ms/条\n"
                ("原始 IPC insert × " ++ show n)
                (total * 1000)
                (total * 1000 / fromIntegral n)
        (e : _) -> printf "%-38s %10s   !! %s\n" "原始 IPC insert" "失败" e

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
    -- \| 每次切下 size 个
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
        "  %-12s %6d 行  索引=%s\n             统计=%s\n"
        (tiTable i)
        (tiRows i)
        (if null (tiIndexes i) then "无" else unwords (tiIndexes i))
        ( unwords
            [ n ++ ":" ++ show d ++ (if c then "+（下界）" else "")
            | (n, d, c) <- tiStats i
            ]
        )

-- | 主场景
runScenarios :: Int -> Int -> IO ()
runScenarios u o = do
    putStrLn "\n[建表]"
    r1 <- benchSqlOnce "CREATE TABLE users" "CREATE TABLE users (id int, name str, age int)"
    r2 <- benchSqlOnce "CREATE TABLE orders" "CREATE TABLE orders (id int, user_id int, product str)"
    case (r1, r2) of
        (Right _, Right _) -> pure ()
        _ -> do
            putStrLn "建表失败，后面不用跑了"
            exitFailure

    putStrLn "\n[写入：每条 INSERT 都是一次完整落盘]"
    _ <- batchInsert (printf "%d 条 INSERT INTO users" u) u userSql
    _ <- batchInsert (printf "%d 条 INSERT INTO orders" o) o (orderSql u)

    putStrLn "\n[分解：同一条 INSERT 拆成两半，都用原始 IPC，不经过引擎]"
    _ <- benchSqlOnce "CREATE TABLE raw_users" "CREATE TABLE raw_users (id int, name str, age int)"
    benchListCatalog 200
    benchRawInsert "raw_users" [userRow i | i <- [1 .. u]]

    putStrLn "\n[读取与查询：整条语句，含引擎侧的解析与语义检查]"
    _ <- benchSql "SELECT * FROM users" "SELECT * FROM users"
    ageR <- benchSql "SELECT name FROM users WHERE age > 90" "SELECT name FROM users WHERE age > 90"

    putStrLn "\n[读取与查询：连接]"
    joinR <-
        benchSql
            "JOIN + WHERE（哈希连接）"
            "SELECT u.name FROM users u JOIN orders o ON u.id = o.user_id WHERE u.age > 90"

    putStrLn "\n[点查]"
    benchLookup "users" "id" (u `div` 2) 100
    _ <- benchSql "SELECT ... WHERE id = k（走 Lookup）" (printf "SELECT name FROM users WHERE id = %d" (u `div` 2))

    putStrLn "\n[批量插入：一条语句带多行，N 行只落一次盘]"
    _ <- benchSqlOnce "CREATE TABLE batch_t" "CREATE TABLE batch_t (id int, name str, age int)"
    let groups = chunked 100 u
    (bt, batchErrs) <- runBatch (map multiInsertSql groups)
    case batchErrs of
        [] ->
            printf
                "%-38s %8.1f ms %7.3f ms/行（%d 行/条 × %d 条语句）\n"
                "多行 INSERT"
                (bt * 1000)
                (bt * 1000 / fromIntegral u)
                (100 :: Int)
                (length groups)
        (e : _) -> printf "%-38s %10s   !! %s\n" "多行 INSERT" "失败" e

    putStrLn "\n[行级删除：代价和删几行无关，只和扫描 + 一次落盘有关]"
    _ <- benchSqlOnce "DELETE FROM users WHERE id = k（1 行）" (printf "DELETE FROM users WHERE id = %d" (u `div` 2))
    _ <- benchSqlOnce "DELETE FROM users（全部）" "DELETE FROM users"

    putStrLn "\n[二级索引：没有索引回退扫描，建完索引走索引]"
    _ <- benchSqlOnce "CREATE TABLE items" "CREATE TABLE items (id int, code int)"
    let itemGroups = chunked 100 o
        itemSql ids =
            "INSERT INTO items (id, code) VALUES "
                ++ intercalate ", " [printf "(%d, %d)" i (i * 7 + 1) | i <- ids]
    (it, itemErrs) <- runBatch (map itemSql itemGroups)
    case itemErrs of
        [] -> pure ()
        (e : _) -> printf "  !! %s\n" e
    printf "  （先把 %d 行灌进 items：%.1f ms）\n" o (it * 1000)
    let targetCode = (o `div` 2) * 7 + 1
    _ <-
        benchSql
            "SELECT ... WHERE code = k（无索引）"
            (printf "SELECT id FROM items WHERE code = %d" targetCode)
    _ <- benchSqlOnce "CREATE INDEX ON items (code)" "CREATE INDEX ON items (code)"
    _ <-
        benchSql
            "SELECT ... WHERE code = k（走索引）"
            (printf "SELECT id FROM items WHERE code = %d" targetCode)

    putStrLn "\n[数据字典统计]"
    cat <- doListCatalog
    case cat of
        Left e -> printf "  取统计失败：%s\n" e
        Right infos -> mapM_ printStats infos

    putStrLn "\n[校验]"
    let expectAge = 9 * (u `div` 100) + max 0 (u `mod` 100 - 90)
    printf
        "age > 90 命中 %s（按数据分布应为 %d 行）\n"
        (either show show ageR)
        expectAge
    when (u == o) $
        printf
            "JOIN 命中 %s（每个用户恰好 1 张订单，应与上一行相同 = %d）\n"
            (either show show joinR)
            expectAge
    unless (u == o) $
        putStrLn "（users 与 orders 行数不同，JOIN 行数没有简单公式，只打印不校验）"
    printf "多行 INSERT 灌进去 %d 行（应为 %d）\n" u u
    printf "二级索引那一行：code = %d 应该查到 id = %d\n" targetCode (o `div` 2)

-- | 主入口
main :: IO ()
main = do
    args <- getArgs
    let pick i d = if length args > i then args !! i else d
        u = read (pick 0 "200") :: Int
        o = read (pick 1 "200") :: Int
        serverArg = pick 2 ""
    serverPath <- findServer serverArg

    -- 每次跑用独立的管道名与数据目录，避免和别的进程抢
    tmp <- getTemporaryDirectory
    stamp <- getMonotonicTime
    let pipe = "chusql-fullchain-" ++ show (round (stamp * 1e6) :: Int)
        dataDir = tmp </> pipe
    createDirectoryIfMissing True dataDir
    setEnv "CHUSQL_PIPE" pipe

    printf "全链路基准：Haskell 引擎 ←命名管道→ Rust 存储进程\n"
    printf "存储进程：%s\n" serverPath
    printf "管道：%s\n" pipe
    printf "数据目录：%s\n" dataDir
    printf "数据规模：users = %d 行，orders = %d 行\n" u o
    printf "计时：墙上时钟（单调）；每条语句都走完整链路\n"

    bracket
        (do
            t1 <- getMonotonicTime
            ph <- startServer serverPath pipe dataDir
            t2 <- getMonotonicTime
            printf "\n[启动] 存储进程就绪，用时 %.2f s\n" (t2 - t1)
            pure ph
        )
        (\ph -> do
            putStrLn "\n[收尾] 停存储进程、清理数据目录"
            stopServer ph dataDir
        )
        (const (runScenarios u o))
