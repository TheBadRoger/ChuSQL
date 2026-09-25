module ChuSQL.Storage (
    MonadStorage (..),
    IndexResult (..),
) where

import ChuSQL.Model (Column, Database, Row, Table (..), Value (..))

-- 存储层抽象：上层只认 domain 类型，不关心底层实现。

data IndexResult
    = IndexRow (Maybe Row)
    | NoIndex
    deriving (Show, Eq)

class (Monad m) => MonadStorage m where
    -- \| 全表扫描
    scan :: String -> m (Either String [Row])

    -- \| 追加一行
    insert :: String -> Row -> m (Either String ())

    -- \| 一批行一起追加
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

    -- \| 按 id 批量删行
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

    -- \| 按某一列的索引取一行
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

    -- \| 删一列，连它的索引与数据一起去掉
    dropColumn :: String -> String -> m (Either String ())

    -- \| 取全库快照（表名 + 列 + 全部行）
    snapshot :: m Database

    -- \| 取全库结构：只要表名和列，不要行
    schema :: m Database
    schema = fmap (map withoutRows) snapshot
      where
        -- \| 抹掉行，只留表名与列定义
        withoutRows (name, t) = (name, t {tableRows = []})
