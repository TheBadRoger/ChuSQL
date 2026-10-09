module ChuSQL.Core.Engine.Storage (
    MonadStorage (..),
    IndexResult (..),
) where

import ChuSQL.Core.Model (Column, Database, Row, Table (..), Value (..))

-- 存储层抽象：MonadStorage 类与 IndexResult。

data IndexResult
    = IndexRows [Row]
    | NoIndex
    deriving (Show, Eq)

class (Monad m) => MonadStorage m where
    -- \| 建一个库
    createDatabase :: String -> m (Either String ())
    createDatabase _ = pure (Left "database management is unavailable in this storage")
    -- \| 删一个库
    dropDatabase :: String -> m (Either String ())
    dropDatabase _ = pure (Left "database management is unavailable in this storage")
    -- \| 切当前库
    useDatabase :: String -> m (Either String ())
    useDatabase _ = pure (Left "database selection is unavailable in this storage")
    -- \| 列出现有库
    listDatabases :: m (Either String [String])
    listDatabases = pure (Right [])

    -- \| 全表扫描
    scan :: String -> m (Either String [Row])

    -- \| 只返回指定列；空清单保留每行的存在性。
    scanColumns :: String -> [String] -> m (Either String [Row])
    scanColumns t cols = do
        result <- scan t
        pure (fmap (map (\row -> [(c, v) | c <- cols, Just v <- [lookup c row]])) result)

    -- \| 本存储能用的并行分片数；不支持并行的存储返回 1
    parallelShards :: m Int
    parallelShards = pure 1

    -- \| 按分片并行扫描并按分片序拼接；默认退回顺序扫描
    scanShards :: Int -> String -> Maybe [String] -> m (Either String [Row])
    scanShards _ t cols = maybe (scan t) (scanColumns t) cols

    -- \| 追加一行
    insert :: String -> Row -> m (Either String ())

    -- \| 一批行一起追加
    insertMany :: String -> [Row] -> m (Either String ())
    insertMany t = go
      where
        -- | 逐行插入，出错就停
        go [] = pure (Right ())
        go (r : rs) = do
            x <- insert t r
            case x of
                Left e -> pure (Left e)
                Right _ -> go rs

    -- \| 按 id 批量删行
    deleteKeys :: String -> [Int] -> m (Either String ())
    deleteKeys t ks = do
        rows <- scan t
        case rows of
            Left e -> pure (Left e)
            Right rs -> replaceAll t [r | r <- rs, not (doomed r)]
      where
        -- | 这一行要不要删
        doomed r = case lookup "id" r of
            Just (VInt k) -> k `elem` ks
            _ -> False

    -- \| 整表替换
    replaceAll :: String -> [Row] -> m (Either String ())

    -- \| 按某一列的索引取一行（键可以是整数或字符串）
    lookupByColumn :: String -> String -> Value -> m (Either String IndexResult)
    lookupByColumn _ _ _ = pure (Right NoIndex)

    -- \| 按某一列的索引做范围扫描；没有可用索引就回 Nothing
    scanRange ::
        String ->
        String ->
        Maybe (Value, Bool) ->
        Maybe (Value, Bool) ->
        m (Either String (Maybe [Row]))
    scanRange _ _ _ _ = pure (Right Nothing)

    -- \| 建一张空表
    createTable :: String -> [(String, Column)] -> m (Either String ())

    -- \| 删除表
    dropTable :: String -> m (Either String ())

    -- \| 给某一列建索引（id 列要求唯一）
    createIndex :: String -> String -> m (Either String ())
    createIndex _ _ = pure (Right ())

    -- \| 去掉某一列的索引
    dropIndex :: String -> String -> m (Either String ())
    dropIndex _ _ = pure (Right ())

    -- \| 删一列，连它的索引与数据一起去掉
    dropColumn :: String -> String -> m (Either String ())

    -- \| ALTER 用：整表换列定义与全部行
    replaceSchema :: String -> [(String, Column)] -> [Row] -> m (Either String ())

    -- \| 取全库快照（表名 + 列 + 全部行）
    snapshot :: m (Either String Database)

    -- \| 取全库结构：只要表名和列，不要行
    schema :: m (Either String Database)
    schema = fmap (fmap (map withoutRows)) snapshot
      where
        -- | 表定义去掉所有行
        withoutRows (name, t) = (name, t {tableRows = []})
