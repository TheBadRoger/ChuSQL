module ChuSQL.Core.Engine.Storage.Memory (MemoryStorage (runMemoryStorage)) where

import ChuSQL.Core.Model
import ChuSQL.Core.Engine.Storage

-- 内存实现：Database 上的状态，错误通道是 Either String。

newtype MemoryStorage a = MemoryStorage {runMemoryStorage :: Database -> Either String (a, Database)}

instance Functor MemoryStorage where
    fmap f (MemoryStorage m) = MemoryStorage $ \db -> do
        (a, db') <- m db
        Right (f a, db')

instance Applicative MemoryStorage where
    pure x = MemoryStorage $ \db -> Right (x, db)
    MemoryStorage mf <*> MemoryStorage ma = MemoryStorage $ \db -> do
        (f, db1) <- mf db
        (a, db2) <- ma db1
        Right (f a, db2)

instance Monad MemoryStorage where
    MemoryStorage m >>= k = MemoryStorage $ \db -> do
        (a, db') <- m db
        runMemoryStorage (k a) db'

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

    -- \| 删一列：表定义里去掉这一列，每一行里的那一格也一起去掉
    dropColumn name col = MemoryStorage $ \db ->
        case lookup name db of
            Nothing -> Right (Left ("unknown table: " ++ name), db)
            Just tbl ->
                if col `notElem` map fst (tableCols tbl)
                    then Right (Left ("unknown column: " ++ col), db)
                    else
                        Right
                            ( Right ()
                            , replaceTable
                                name
                                tbl
                                    { tableCols = filter ((/= col) . fst) (tableCols tbl)
                                    , tableRows = map (filter ((/= col) . fst)) (tableRows tbl)
                                    }
                                db
                            )

    -- \| 原样返回当前库
    snapshot = MemoryStorage $ \db -> Right (db, db)

    -- \| 改表结构：列定义与行一起换
    replaceSchema name cols rows = MemoryStorage $ \db -> case lookup name db of
        Nothing -> Right (Left ("unknown table: " ++ name), db)
        Just tbl -> Right (Right (), replaceTable name tbl{tableCols = cols, tableRows = rows} db)

-- | 用给定表替换同名表
replaceTable :: String -> Table -> Database -> Database
replaceTable name table =
    map (\(n, t) -> if n == name then (n, table) else (n, t))

