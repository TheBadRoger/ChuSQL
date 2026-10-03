module ChuSQL.Core.Engine.Algebra.Cost (tableMetaOf, probeCost, pointRows, rangeRows, selectivityOf, indexCheaper) where

import ChuSQL.Core.Model

-- 统计与成本估算：行数、不同值数、列直方图与索引探路页数。

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

-- | 范围扫描的估算命中行数：有直方图按桶算，没有按四分之一
rangeRows :: TableMeta -> String -> Maybe (Value, Bool) -> Maybe (Value, Bool) -> Int
rangeRows m col lo hi = case histogramFor m col of
    Just h -> estimateRows h lo hi
    Nothing -> max 0 (metaRowCount m) `div` 4

-- | 直方图估算区间命中行数：按桶算交集占比
estimateRows :: Histogram -> Maybe (Value, Bool) -> Maybe (Value, Bool) -> Int
estimateRows h lo hi = max 0 (min total (round (sum (zipWith overlap [0 ..] (histBuckets h)))))
  where
    total = sum (histBuckets h)
    width = if histHigh h > histLow h then (histHigh h - histLow h) / fromIntegral (length (histBuckets h)) else 0
    lower = maybe (-1 / 0) id (boundOf lo)
    upper = maybe (1 / 0) id (boundOf hi)
    overlap i count
        | width <= 0 = if lower <= histLow h && histLow h <= upper then fromIntegral count else 0
        | otherwise = fromIntegral count * hit / width
      where
        start = histLow h + width * fromIntegral i
        hit = max 0 (min (start + width) upper - max start lower)

-- | 区间选择率：直方图命中的比例；没有直方图给 Nothing
selectivityOf :: TableMeta -> String -> Maybe (Value, Bool) -> Maybe (Value, Bool) -> Maybe Double
selectivityOf m col lo hi = case histogramFor m col of
    Nothing -> Nothing
    Just h
        | total <= 0 -> Nothing
        | otherwise -> Just (fromIntegral (estimateRows h lo hi) / fromIntegral total)
      where
        total = sum (histBuckets h)

-- | 边界值转成参与估值的小数；非数值给 Nothing
boundOf :: Maybe (Value, Bool) -> Maybe Double
boundOf (Just (VInt n, _)) = Just (fromIntegral n)
boundOf (Just (VFloat d, _)) = Just d
boundOf _ = Nothing

-- | 有索引且探路加命中比全表扫便宜
indexCheaper :: TableMeta -> String -> Int -> Bool
indexCheaper m col est = col `elem` metaIndexes m && probeCost rows + est < rows
  where
    rows = max 0 (metaRowCount m)
