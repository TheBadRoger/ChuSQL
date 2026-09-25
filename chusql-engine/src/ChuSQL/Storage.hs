module ChuSQL.Storage (
    MonadStorage (..),
    IndexResult (..),
) where

import ChuSQL.Model (Column, Database, Row, Table (..), Value (..))

-- 存储层抽象：上层只认 domain 类型，不关心底层实现。

-- | 一次索引点查的结果
data IndexResult
    = -- \| 有索引，查过了：Just 命中一行，Nothing 没这行
      IndexRow (Maybe Row)
    | -- \| 没有这个索引 —— 调用方应该退回全表扫描
      NoIndex
    deriving (Show, Eq)

-- | 存储接口：读写表、按索引取行、取快照
class (Monad m) => MonadStorage m where
    -- \| 全表扫描
    scan :: String -> m (Either String [Row])

    -- \| 追加一行。
    -- 要写哪些索引由**存储层按自己的元数据**决定（表上有哪些索引它自己清楚），
    -- 调用方不需要指定 key。
    insert :: String -> Row -> m (Either String ())

    -- \| 一批行一起追加。
    -- 默认逐行调 `insert`；能批量的实现（比如走 WAL 的 IPC 存储）会覆盖它，
    -- 好处是 N 行只落一次盘。
    insertMany :: String -> [Row] -> m (Either String ())
    insertMany t = go
      where
        -- \| 一行一行来，遇到错就停
        go [] = pure (Right ())
        go (r : rs) = do
            x <- insert t r
            case x of
                Left e -> pure (Left e)
                Right _ -> go rs

    -- \| 按 `id` 批量删行。
    -- 默认实现是"全读出来、筛掉、整表写回"（对内存实现足够）；
    -- 真正能在原地删的实现会覆盖它。
    deleteKeys :: String -> [Int] -> m (Either String ())
    deleteKeys t ks = do
        rows <- scan t
        case rows of
            Left e -> pure (Left e)
            Right rs -> replaceAll t [r | r <- rs, not (doomed r)]
      where
        -- \| 这一行的 id 在待删清单里吗
        doomed r = case lookup "id" r of
            Just (VInt k) -> k `elem` ks
            _ -> False

    -- \| 整表替换
    replaceAll :: String -> [Row] -> m (Either String ())

    -- \| 按某一列的索引取一行。
    -- 默认回答"没有索引"（内存实现不需要索引），于是上层会自己扫一遍。
    lookupByColumn :: String -> String -> Int -> m (Either String IndexResult)
    lookupByColumn _ _ _ = pure (Right NoIndex)

    -- \| 建一张空表
    createTable :: String -> [(String, Column)] -> m (Either String ())

    -- \| 删除表
    dropTable :: String -> m (Either String ())

    -- \| 给某一列建索引（要求这一列整数取值唯一）
    createIndex :: String -> String -> m (Either String ())
    createIndex _ _ = pure (Right ())

    -- \| 去掉某一列的索引
    dropIndex :: String -> String -> m (Either String ())
    dropIndex _ _ = pure (Right ())

    -- \| 取全库快照（表名 + 列 + 全部行）
    snapshot :: m Database

    -- \| 取全库**结构**：只要表名和列，不要行。
    --
    -- 语义检查和查询优化都只看列名/列类型，一行数据都不碰；
    -- 而"取全库快照"要把所有行拉一遍，在单语句路径上是白花的钱。
    -- 默认实现退化成整库快照再抹掉行，有更便宜做法的实现（比如走数据字典）可以覆盖。
    schema :: m Database
    schema = fmap (map withoutRows) snapshot
      where
        -- \| 抹掉行，只留表名与列定义
        withoutRows (name, t) = (name, t {tableRows = []})
