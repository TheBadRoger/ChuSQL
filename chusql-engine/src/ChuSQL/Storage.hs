module ChuSQL.Storage (MonadStorage (..)) where

import ChuSQL.Model (Database, Row)

-- 存储层抽象：上层只认 domain 类型，不关心底层是内存还是 IPC。
class (Monad m) => MonadStorage m where
    -- 全表扫描；表不存在返回 Left。
    scan :: String -> m (Either String [Row])

    -- 追加一行到表尾；表不存在返回 Left。
    insert :: String -> Row -> m (Either String ())

    -- 用新行列表整体替换一张表。
    replaceAll :: String -> [Row] -> m (Either String ())

    -- 按主键查一行；命中返回 Just，未命中返回 Nothing。
    lookupByKey :: String -> Int -> m (Either String (Maybe Row))

    -- 取全库快照，喂给还接收 Database 的语义检查 / 优化器 / evalRelOp。
    snapshot :: m Database
