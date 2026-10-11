module ChuSQL.Core.Engine.Algebra.Sort (sortRows, sortBoundRows, compareValue) where

import ChuSQL.Core.Model
import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Engine.Algebra.Expr (evalExpr)
import Data.List (sortBy)

-- 排序：按多列规格排出结果行，每列可升可降。

-- | 反转比较结果
flipOrdering :: Ordering -> Ordering
flipOrdering LT = GT
flipOrdering GT = LT
flipOrdering EQ = EQ

-- | 取出排序键（按 spec 顺序，每行只查一次）
sortKey :: [(String, SortDir)] -> Row -> [Maybe Value]
sortKey spec row = [lookup col row | (col, _) <- spec]

-- | 比较两个排序键
compareKeys :: [SortDir] -> [Maybe Value] -> [Maybe Value] -> Ordering
compareKeys [] _ _ = EQ
compareKeys (dir : rest) (a : as) (b : bs) =
    case (a, b) of
        (Just v1, Just v2) ->
            case compareValue v1 v2 of
                EQ -> compareKeys rest as bs
                o -> if dir == Desc then flipOrdering o else o
        _ -> compareKeys rest as bs
compareKeys _ _ _ = EQ

-- | 按排序规格排序若干行
sortRows :: [(String, SortDir)] -> [Row] -> [Row]
sortRows [] rows = rows
sortRows spec rows = map snd (sortBy cmp (map decorate rows))
  where
    -- | 取键并带上原行
    decorate row = (sortKey spec row, row)
    -- | 比较两个带键的行
    cmp (k1, _) (k2, _) = compareKeys (map snd spec) k1 k2

-- | 用绑定槽位取键并稳定排序
sortBoundRows :: [(Expr, SortDir)] -> Row -> [Row] -> Either String [Row]
sortBoundRows [] _ rows = Right rows
sortBoundRows spec outer rows = do
    decorated <- mapM decorate rows
    Right (map snd (sortBy compareRows decorated))
  where
    directions = map snd spec
    -- | 每行只计算一次绑定排序键
    decorate row = do
        keys <- mapM (\(expression, _) -> evalExpr expression (row ++ outer)) spec
        Right (map Just keys, row)
    -- | 比较排序键并保留同键行序
    compareRows (left, _) (right, _) = compareKeys directions left right
