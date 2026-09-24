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

-- | 按排序要求比较两行
compareRows :: [(String, SortDir)] -> Row -> Row -> Ordering
compareRows [] _ _ = EQ
compareRows ((col, dir) : rest) r1 r2 =
    case (lookup col r1, lookup col r2) of
        (Just v1, Just v2) ->
            case compareValue v1 v2 of
                EQ -> compareRows rest r1 r2
                o ->
                    if dir == Desc
                        then flipOrdering o
                        else o
        _ -> compareRows rest r1 r2

-- | 排序若干行
sortRows :: [(String, SortDir)] -> [Row] -> [Row]
sortRows [] rows = rows
sortRows spec rows = sortBy (compareRows spec) rows
