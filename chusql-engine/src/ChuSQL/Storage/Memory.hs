module ChuSQL.Storage.Memory (MemoryStorage (runMemoryStorage)) where

import ChuSQL.Model
import ChuSQL.Storage

-- 内存实现：状态是 Database，错误通道是 Either String。

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

    -- \| 按主键找第一行
    lookupByKey t k = MemoryStorage $ \db -> do
        tbl <- lookupTable db t
        Right (Right (findRow tbl k), db)

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
-- | 按 id 列找一行
findRow :: Table -> Int -> Maybe Row
findRow tbl k = case filter matches (tableRows tbl) of
    (r : _) -> Just r
    [] -> Nothing
  where
    -- \| 这行的 id 是否等于 k
    matches r = lookup "id" r == Just (VInt k)

-- | 用给定表替换同名表
replaceTable :: String -> Table -> Database -> Database
replaceTable name table =
    map (\(n, t) -> if n == name then (n, table) else (n, t))
