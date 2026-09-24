module ChuSQL.Storage.Memory (MemoryStorage (runMemoryStorage)) where

import ChuSQL.Model
import ChuSQL.Storage

-- | 内存实现：本质是 Database 上的 State，错误通道是 Either String。
newtype MemoryStorage a = MemoryStorage {runMemoryStorage :: Database -> Either String (a, Database)}

-- | Functor：把 fmap 转发给底下的 State。
instance Functor MemoryStorage where
    fmap f (MemoryStorage m) = MemoryStorage $ \db -> do
        (a, db') <- m db
        Right (f a, db')

-- | Applicative：转发 pure / <*>。
instance Applicative MemoryStorage where
    pure x = MemoryStorage $ \db -> Right (x, db)
    MemoryStorage mf <*> MemoryStorage ma = MemoryStorage $ \db -> do
        (f, db1) <- mf db
        (a, db2) <- ma db1
        Right (f a, db2)

-- | Monad：顺序执行，把动作转发给底下的 State。
instance Monad MemoryStorage where
    MemoryStorage m >>= k = MemoryStorage $ \db -> do
        (a, db') <- m db
        runMemoryStorage (k a) db'

-- | MonadStorage 实例：内存里直接改 Database。
instance MonadStorage MemoryStorage where
    -- \| 按名查表，返回所有行。
    scan t = MemoryStorage $ \db -> do
        tbl <- lookupTable db t
        Right (Right (tableRows tbl), db)

    -- \| 追加一行到表尾。
    insert t r = MemoryStorage $ \db -> do
        tbl <- lookupTable db t
        Right (Right (), replaceTable t tbl{tableRows = tableRows tbl ++ [r]} db)

    -- \| 用新行列表替换整表内容。
    replaceAll t rows = MemoryStorage $ \db -> do
        tbl <- lookupTable db t
        Right (Right (), replaceTable t tbl{tableRows = rows} db)

    -- \| 找 id 列等于 k 的第一行。
    lookupByKey t k = MemoryStorage $ \db -> do
        tbl <- lookupTable db t
        Right (Right (findRow tbl k), db)

    -- \| 原样返回当前数据库。
    snapshot = MemoryStorage $ \db -> Right (db, db)

-- | 按 id 列匹配一个整数。
findRow :: Table -> Int -> Maybe Row
findRow tbl k = case filter matches (tableRows tbl) of
    (r : _) -> Just r
    [] -> Nothing
  where
    -- \| 这行的 id 列是否等于 k。
    matches r = lookup "id" r == Just (VInt k)

-- | 用给定表替换同名表；不存在就原样返回。
replaceTable :: String -> Table -> Database -> Database
replaceTable name table =
    map (\(n, t) -> if n == name then (n, table) else (n, t))
