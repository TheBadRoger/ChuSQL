{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Web.Demo (
    SeedReport (..),
    demoTableSpecs,
    demoRows,
    demoIndexes,
    demoNames,
    demoPlan,
    seedDemo,
) where

import ChuSQL.Core.Model (Value (..))
import ChuSQL.Core.Protocol (TableInfo (..))
import ChuSQL.Interface.Actions (
    ColumnSpec (..),
    CreateTableSpec (..),
    createIndexSql,
    createTableSql,
    insertRowsSql,
 )
import ChuSQL.Interface.Session (Session, catalog, runStatement)
import Data.Text (Text)
import qualified Data.Text as T

-- 演示数据：三张表加一个二级索引，语句与界面走同一套拼 SQL 的代码。

-- | 演示表（建表顺序即依赖顺序；没有外键约束，纯展示）
demoTableSpecs :: [CreateTableSpec]
demoTableSpecs =
    [ CreateTableSpec "users" [ColumnSpec "id" "int", ColumnSpec "name" "str", ColumnSpec "age" "int"]
    , CreateTableSpec "orders" [ColumnSpec "id" "int", ColumnSpec "user_id" "int", ColumnSpec "product" "str", ColumnSpec "amount" "int"]
    , CreateTableSpec "products" [ColumnSpec "id" "int", ColumnSpec "name" "str", ColumnSpec "sku" "int", ColumnSpec "price" "int"]
    ]

-- | 每张表的演示行
demoRows :: [(T.Text, [[(String, Value)]])]
demoRows =
    [ ( "users"
      , [ [("id", VInt i), ("name", VStr name), ("age", VInt age)]
        | (i, (name, age)) <- zip [1 :: Int ..] userRecords
        ]
      )
    , ( "orders"
      , [ [("id", VInt i), ("user_id", VInt uid), ("product", VStr p), ("amount", VInt a)]
        | (i, (uid, p, a)) <- zip [1 :: Int ..] orderRecords
        ]
      )
    , ( "products"
      , [ [("id", VInt i), ("name", VStr n), ("sku", VInt sku), ("price", VInt price)]
        | (i, (n, sku, price)) <- zip [1 :: Int ..] productRecords
        ]
      )
    ]

-- | 演示索引：products.sku 取值唯一
demoIndexes :: [(T.Text, T.Text)]
demoIndexes = [("products", "sku")]

-- | 演示数据涉及的表名
demoNames :: [T.Text]
demoNames = map ctsTable demoTableSpecs

-- | 完整的一套语句（建表 + 插行 + 建索引），按顺序执行
demoPlan :: Either String [String]
demoPlan = do
    creates <- mapM createTableSql demoTableSpecs
    inserts <- mapM (uncurry insertRowsSql) demoRows
    indexes <- mapM (uncurry createIndexSql) demoIndexes
    pure (creates ++ inserts ++ indexes)

data SeedReport = SeedReport
    { srCreated :: [String]
    , srSkipped :: [String]
    , srIndexes :: [String]
    }
    deriving (Show, Eq)

-- | 按需灌演示数据：缺表补表、缺索引补索引
seedDemo :: Session -> IO (Either Text SeedReport)
seedDemo session = do
    catalogResult <- catalog session
    case catalogResult of
        Left message -> pure (Left message)
        Right infos -> do
            let existingTables = map tiTable infos
                missing = [spec | spec <- demoTableSpecs, T.unpack (ctsTable spec) `notElem` existingTables]
                skipped = [T.unpack (ctsTable spec) | spec <- demoTableSpecs, T.unpack (ctsTable spec) `elem` existingTables]
                missingNames = map ctsTable missing
                wantedIndexes =
                    [ (t, c)
                    | (t, c) <- demoIndexes
                    , t `elem` missingNames || indexMissing infos t c
                    ]
            created <- runAll (map createTableSql missing)
            case created of
                Left message -> pure (Left message)
                Right () -> do
                    rowsDone <- runAll [insertRowsSql t rs | (t, rs) <- demoRows, t `elem` missingNames]
                    case rowsDone of
                        Left message -> pure (Left message)
                        Right () -> do
                            indexes <- runAll [createIndexSql t c | (t, c) <- wantedIndexes]
                            pure $ fmap (const (SeedReport (map T.unpack missingNames) skipped (map (T.unpack . snd) wantedIndexes))) indexes
  where
    -- | 顺序跑完一串语句，首错即停
    runAll :: [Either String String] -> IO (Either Text ())
    runAll [] = pure (Right ())
    runAll (sql : rest) = case sql of
        Left e -> pure (Left (T.pack e))
        Right statement -> do
            result <- runStatement session (T.pack statement)
            case result of
                Left message -> pure (Left message)
                Right _ -> runAll rest

    -- | 目录里这张表缺不缺这个索引
    indexMissing :: [TableInfo] -> Text -> Text -> Bool
    indexMissing infos t c = case lookup (T.unpack t) [(tiTable i, tiIndexes i) | i <- infos] of
        Nothing -> True
        Just cols -> T.unpack c `notElem` cols

-- | 演示用户数据
userRecords :: [(String, Int)]
userRecords =
    [ ("Alice", 24)
    , ("Bob", 31)
    , ("Carol", 45)
    , ("Dave", 19)
    , ("Erin", 38)
    , ("Frank", 52)
    , ("Grace", 27)
    , ("Heidi", 33)
    , ("Ivan", 41)
    , ("Judy", 29)
    ]

-- | 演示订单数据
orderRecords :: [(Int, String, Int)]
orderRecords =
    [ (1, "Laptop", 1299)
    , (1, "Mouse", 25)
    , (2, "Keyboard", 79)
    , (3, "Monitor", 349)
    , (3, "Webcam", 89)
    , (4, "Headset", 119)
    , (5, "SSD", 139)
    , (5, "RAM", 99)
    , (6, "Dock", 199)
    , (7, "Cable", 12)
    , (8, "Stand", 45)
    , (9, "Lamp", 39)
    ]

-- | 演示产品数据
productRecords :: [(String, Int, Int)]
productRecords =
    [ ("Laptop", 1001, 1299)
    , ("Mouse", 1002, 25)
    , ("Keyboard", 1003, 79)
    , ("Monitor", 1004, 349)
    , ("Webcam", 1005, 89)
    , ("Headset", 1006, 119)
    , ("SSD", 1007, 139)
    , ("RAM", 1008, 99)
    ]
