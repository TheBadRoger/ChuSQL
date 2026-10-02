module ChuSQL.Core.Engine.Algebra.Cost (tableMetaOf, probeCost, pointRows, rangeRows, indexCheaper) where

import ChuSQL.Core.Model

-- 统计与成本估算：行数、列级不同值数与索引探路页数。

-- | 表统计，拿不到就是 Nothing
tableMetaOf :: Database -> String -> Maybe TableMeta
tableMetaOf db t = lookup t db >>= tableMeta

-- | 索引探路的页数：B+Tree 高度
probeCost :: Int -> Int
probeCost rows = max 1 (ceiling (logBase 2 (fromIntegral (max 2 rows) :: Double)))

-- | 等值点查的估算命中行数：行数除以该列不同值数
pointRows :: TableMeta -> String -> Int
pointRows m col = case [d | (c, d, _) <- metaDistinct m, c == col, d > 1] of
    (d : _) -> max 1 (rows `div` d)
    [] -> rows
  where
    rows = max 0 (metaRowCount m)

-- | 范围扫描的估算命中行数：没有直方图，按四分之一算
rangeRows :: TableMeta -> Int
rangeRows m = max 0 (metaRowCount m) `div` 4

-- | 有索引且探路加命中比全表扫便宜
indexCheaper :: TableMeta -> String -> Int -> Bool
indexCheaper m col est = col `elem` metaIndexes m && probeCost rows + est < rows
  where
    rows = max 0 (metaRowCount m)
