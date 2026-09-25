module ChuSQL.Storage.Memory (MemoryStorage (runMemoryStorage)) where

import ChuSQL.Model
import ChuSQL.Storage

-- 内存实现：状态是 Database，错误通道是 Either String。
--
-- 它只实现"必须有"的那几个：批量插入、行级删除、索引、统计全走类的默认实现
-- （内存里数据本来就在手上，扫一遍就是最快的做法）。

-- * 实例

-- | 内存存储：Database 上的状态
newtype MemoryStorage a = MemoryStorage {runMemoryStorage :: Database -> Either String (a, Database)}

-- | Functor：转发给底层状态
instance Functor MemoryStorage where
    fmap f (MemoryStorage m) = MemoryStorage $ \db -> do
        (a, db') <- m db
        Right (f a, db')

-- | Applicative：转发 pure 和 <*>
instance Applicative MemoryStorage where
    pure x = MemoryStorage $ \db -> Right (x, db)
    MemoryStorage mf <*> MemoryStorage ma = MemoryStorage $ \db -> do
        (f, db1) <- mf db
        (a, db2) <- ma db1
        Right (f a, db2)

-- | Monad：顺序执行
instance Monad MemoryStorage where
    MemoryStorage m >>= k = MemoryStorage $ \db -> do
        (a, db') <- m db
        runMemoryStorage (k a) db'

-- | MonadStorage：直接改内存库
instance MonadStorage MemoryStorage where
    -- \| 全表扫描
    scan t = MemoryStorage $ \db -> do
        tbl <- lookupTable db t
        Right (Right (tableRows tbl), db)

    -- \| 追加一行
    insert t r = MemoryStorage $ \db -> do
        tbl <- lookupTable db t
        Right (Right (), replaceTable t tbl{tableRows = tableRows tbl ++ [r]} db)

    -- \| 整表替换
    replaceAll t rows = MemoryStorage $ \db -> do
        tbl <- lookupTable db t
        Right (Right (), replaceTable t tbl{tableRows = rows} db)

    -- \| 建空表；已存在报错
    createTable name cols = MemoryStorage $ \db ->
        if any ((== name) . fst) db
            then Right (Left ("table already exists: " ++ name), db)
            else Right (Right (), db ++ [(name, Table name cols [])])

    -- \| 删表；不存在报错
    dropTable name = MemoryStorage $ \db ->
        if any ((== name) . fst) db
            then Right (Right (), filter ((/= name) . fst) db)
            else Right (Left ("unknown table: " ++ name), db)

    -- \| 原样返回当前库
    snapshot = MemoryStorage $ \db -> Right (db, db)

-- * 工具

-- | 用给定表替换同名表
replaceTable :: String -> Table -> Database -> Database
replaceTable name table =
    map (\(n, t) -> if n == name then (n, table) else (n, t))

