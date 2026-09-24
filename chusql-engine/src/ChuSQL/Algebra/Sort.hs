module ChuSQL.Algebra.Sort (sortRows) where

import ChuSQL.Model
import ChuSQL.Syntax.AST
import Data.List (sortBy)

-- 排序：按多列排序，每列可升可降。

-- | 反转比较结果
flipOrdering :: Ordering -> Ordering
flipOrdering LT = GT
flipOrdering GT = LT
flipOrdering EQ = EQ

-- | 比较两个值
compareValue :: Value -> Value -> Ordering
compareValue (VInt a) (VInt b) = compare a b
compareValue (VStr a) (VStr b) = compare a b
compareValue (VBool a) (VBool b) = compare a b
compareValue _ _ = EQ

-- | 取出排序键（按 spec 顺序，每行只查一次）
sortKey :: [(String, SortDir)] -> Row -> [Maybe Value]
sortKey spec row = [lookup col row | (col, _) <- spec]

-- | 比较两个排序键
compareKeys :: [(String, SortDir)] -> [Maybe Value] -> [Maybe Value] -> Ordering
compareKeys [] _ _ = EQ
compareKeys ((_, dir) : rest) (a : as) (b : bs) =
    case (a, b) of
        (Just v1, Just v2) ->
            case compareValue v1 v2 of
                EQ -> compareKeys rest as bs
                o -> if dir == Desc then flipOrdering o else o
        _ -> compareKeys rest as bs
compareKeys _ _ _ = EQ

-- | 排序若干行（先取键再排，避免每次比较都查列）
sortRows :: [(String, SortDir)] -> [Row] -> [Row]
sortRows [] rows = rows
sortRows spec rows = map snd (sortBy cmp (map decorate rows))
  where
    decorate row = (sortKey spec row, row)
    cmp (k1, _) (k2, _) = compareKeys spec k1 k2
