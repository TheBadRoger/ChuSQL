{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.Backend (
    StatementResult (..),
    Backend (..),
    columnsFromStatement,
    ipcBackend,
    memoryBackend,
    tableInfoOf,
) where

import ChuSQL.Engine (runStatement, runStatementM)
import ChuSQL.Model
import ChuSQL.Storage (MonadStorage (schema))
import ChuSQL.Storage.IPC (
    IPCStorage (..),
    Request (..),
    Response (..),
    SchemaColumn (..),
    TableInfo (..),
    doListCatalog,
    sendRequest,
 )
import ChuSQL.Syntax.AST
import ChuSQL.Syntax.Parser (parseStatement)
import Control.Concurrent.MVar (MVar, modifyMVar, newMVar, readMVar, withMVar)
import Data.List (nub)

-- 后端：跑一条语句与取数据字典，分 IPC（真存储）和内存（测试）两套。

data StatementResult = StatementResult
    { srColumns :: [String]
    , srRows :: [Row]
    }

data Backend = Backend
    { beStatement :: String -> IO (Either String StatementResult)
    , beCatalog :: IO (Either String [TableInfo])
    , bePing :: IO Bool
    }

-- | 按给定列名取列，固定列顺序
columnsFromStatement :: Database -> Statement -> [String]
columnsFromStatement db stmt = case stmt of
    Select cols from _ _ _
        | cols == [allColumns] -> wildcard from
        | otherwise -> cols
    _ -> []
  where
    -- | 展开 *：单表按列序，JOIN 左表在前
    wildcard (FromTable mAlias tbl) = case lookup tbl db of
        Nothing -> []
        Just t -> [qualify mAlias c | c <- colNames t]
    wildcard (FromJoin left mAlias tbl _) = wildcard left ++ wildcard (FromTable mAlias tbl)

-- | 走命名管道的后端（真存储）
ipcBackend :: IO Backend
ipcBackend = do
    lock <- newMVar ()
    pure
        Backend
            { beStatement = \sql -> withMVar lock $ \_ -> runIpc sql
            , beCatalog = withMVar lock $ \_ -> do
                result <- doListCatalog
                pure (either Left Right result)
            , bePing = withMVar lock $ \_ -> do
                resp <- sendRequest ReqPing
                pure (case resp of RespPong -> True; _ -> False)
            }
  where
    -- | 解析并执行，空结果时补一次表头
    runIpc :: String -> IO (Either String StatementResult)
    runIpc sql = case parseStatement sql of
        Left e -> pure (Left e)
        Right stmt -> do
            result <- runIPCStorage (runStatementM stmt)
            case result of
                Left e -> pure (Left e)
                Right rows -> do
                    cols <- case rows of
                        [] -> do
                            db <- runIPCStorage (schema :: IPCStorage Database)
                            pure (columnsFromStatement db stmt)
                        (r : _) -> pure (map fst r)
                    pure (Right (StatementResult cols rows))

-- | 内存后端（测试）：状态就是一个 `Database`
memoryBackend :: MVar Database -> Backend
memoryBackend ref =
    Backend
        { beStatement = \sql -> modifyMVar ref $ \db -> case parseStatement sql of
            Left e -> pure (db, Left e)
            Right stmt -> case runStatement db stmt of
                Left e -> pure (db, Left e)
                Right (db', rows) -> pure (db', Right (StatementResult (colsOf db stmt rows) rows))
        , beCatalog = do
            db <- readMVar ref
            pure (Right (map tableInfoOf db))
        , bePing = pure True
        }
  where
    -- | 有行用行上的键，空结果回退语法树
    colsOf db stmt rows = case rows of
        [] -> columnsFromStatement db stmt
        (r : _) -> map fst r

-- | 一张表的线上信息（内存实现：没有索引，统计现算）
tableInfoOf :: (String, Table) -> TableInfo
tableInfoOf (name, tbl) =
    TableInfo
        { tiTable = name
        , tiColumns = [SchemaColumn c (wireType ty) | (c, ty) <- tableCols tbl]
        , tiRows = length (tableRows tbl)
        , tiIndexes = []
        , tiStats = [(c, distinctOf c, False) | (c, _) <- tableCols tbl]
        }
  where
    -- | 某一列有几个不同值
    distinctOf c = length (nub [v | r <- tableRows tbl, Just v <- [lookup c r]])

-- | 本地列类型转线上字符串（和 IPC 那边同一套写法）
wireType :: Column -> String
wireType TInt = "int"
wireType TStr = "str"
wireType TBool = "bool"
