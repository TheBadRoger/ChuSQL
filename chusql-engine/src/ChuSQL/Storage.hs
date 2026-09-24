module ChuSQL.Storage (
    MonadStorage (..),
) where

import ChuSQL.Model (Column, Database, Row)

-- 存储层抽象：上层只认 domain 类型，不关心底层实现。

-- | 存储接口：读写表、按主键取行、取快照
class (Monad m) => MonadStorage m where
    -- \| 全表扫描
    scan :: String -> m (Either String [Row])

    -- \| 追加一行
    insert :: String -> Row -> Maybe Int -> m (Either String ())

    -- \| 整表替换
    replaceAll :: String -> [Row] -> m (Either String ())

    -- \| 按主键取一行
    lookupByKey :: String -> Int -> m (Either String (Maybe Row))

    -- \| 建一张空表
    createTable :: String -> [(String, Column)] -> m (Either String ())

    -- \| 删除表
    dropTable :: String -> m (Either String ())

    -- \| 取全库快照
    snapshot :: m Database
