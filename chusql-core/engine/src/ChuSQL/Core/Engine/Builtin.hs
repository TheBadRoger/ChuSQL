module ChuSQL.Core.Engine.Builtin
    ( Builtin (..)
    , builtinNames
    , resolveBuiltin
    , builtinName
    , invokeBuiltin
    , builtinOfExpr
    , builtinNode
    , aggregateArg
    , unsupportedAggregate
    , Operator (..)
    , operatorOfExpr
    , operatorSymbol
    , executeOperator
    , threeValuedNot
    , inValues
    ) where

import ChuSQL.Core.Model (Value (..), compareValue)
import ChuSQL.Core.Engine.Syntax.AST (Expr (..))
import Control.Monad (foldM)
import Data.Char (toLower)

-- 内置函数与运算符的唯一权威：名称解析和求值都只在这里实现一次。

-- | 内置聚合函数
data Builtin = BCountAll | BCount | BSum | BAvg | BMin | BMax
    deriving (Show, Eq, Enum, Bounded)

-- | 名字到内置函数的对照表，解析只认这张表
builtinNames :: [(String, Builtin)]
builtinNames =
    [ ("count", BCount)
    , ("sum", BSum)
    , ("avg", BAvg)
    , ("min", BMin)
    , ("max", BMax)
    ]

-- | 按名字解析内置函数，名字不分大小写
resolveBuiltin :: String -> Maybe Builtin
resolveBuiltin name = lookup (map toLower name) builtinNames

-- | 内置函数在 SQL 里的名字
builtinName :: Builtin -> String
builtinName BCountAll = "count"
builtinName b = case [n | (n, x) <- builtinNames, x == b] of
    (n : _) -> n
    [] -> "count"

-- | 在一组上调用内置函数：给出行数与各行的参数值
invokeBuiltin :: Builtin -> Int -> [Value] -> Either String Value
invokeBuiltin BCountAll rows _ = Right (VInt rows)
invokeBuiltin BCount _ vs = Right (VInt (length (nonNull vs)))
invokeBuiltin BSum _ vs = case nonNull vs of
    [] -> Right VNull
    xs -> foldM addValue (VInt 0) xs
invokeBuiltin BAvg _ vs = case nonNull vs of
    [] -> Right VNull
    xs -> do
        total <- foldM addValue (VInt 0) xs
        sumValue <- numberOf total
        Right (VFloat (sumValue / fromIntegral (length xs)))
invokeBuiltin BMin _ vs = extremeOf lower (nonNull vs)
invokeBuiltin BMax _ vs = extremeOf higher (nonNull vs)

-- | 去掉 NULL
nonNull :: [Value] -> [Value]
nonNull = filter (/= VNull)

-- | 聚合节点对应的内置函数；其余节点给 Nothing
builtinOfExpr :: Expr -> Maybe Builtin
builtinOfExpr CountAll = Just BCountAll
builtinOfExpr (CountOf _) = Just BCount
builtinOfExpr (SumOf _) = Just BSum
builtinOfExpr (AvgOf _) = Just BAvg
builtinOfExpr (MinOf _) = Just BMin
builtinOfExpr (MaxOf _) = Just BMax
builtinOfExpr _ = Nothing

-- | 内置函数对应的 AST 节点
builtinNode :: Builtin -> Expr -> Expr
builtinNode BCountAll _ = CountAll
builtinNode BCount e = CountOf e
builtinNode BSum e = SumOf e
builtinNode BAvg e = AvgOf e
builtinNode BMin e = MinOf e
builtinNode BMax e = MaxOf e

-- | 聚合调用的参数表达式，COUNT(*) 没有参数
aggregateArg :: Expr -> Maybe Expr
aggregateArg (CountOf x) = Just x
aggregateArg (SumOf x) = Just x
aggregateArg (AvgOf x) = Just x
aggregateArg (MinOf x) = Just x
aggregateArg (MaxOf x) = Just x
aggregateArg _ = Nothing

-- | 只支持这几个聚合函数
unsupportedAggregate :: String
unsupportedAggregate = "only COUNT, SUM, AVG, MIN and MAX can be aggregated"

-- | 数值相加：整数配整数还是整数
addValue :: Value -> Value -> Either String Value
addValue (VInt a) (VInt b) = Right (VInt (a + b))
addValue (VInt a) (VFloat b) = Right (VFloat (fromIntegral a + b))
addValue (VFloat a) (VInt b) = Right (VFloat (a + fromIntegral b))
addValue (VFloat a) (VFloat b) = Right (VFloat (a + b))
addValue _ _ = Left "type error: expected two numbers"

-- | 能当数用的值
numberOf :: Value -> Either String Double
numberOf (VInt n) = Right (fromIntegral n)
numberOf (VFloat d) = Right d
numberOf _ = Left "type error: expected a number"

-- | 一组值里的极值，空组给 NULL
extremeOf :: (Value -> Value -> Value) -> [Value] -> Either String Value
extremeOf _ [] = Right VNull
extremeOf pick (x : xs) = Right (foldl pick x xs)

-- | 取更小的那个
lower :: Value -> Value -> Value
lower a b = if compareValue b a == LT then b else a

-- | 取更大的那个
higher :: Value -> Value -> Value
higher a b = if compareValue b a == GT then b else a

-- | 运算符：AST 里的每个运算节点对应一个
data Operator
    = OpAdd
    | OpSub
    | OpMul
    | OpDiv
    | OpNeg
    | OpGt
    | OpLt
    | OpEq
    | OpAnd
    | OpOr
    deriving (Show, Eq, Enum, Bounded)

-- | 运算符的 SQL 写法
operatorSymbol :: Operator -> String
operatorSymbol OpAdd = "+"
operatorSymbol OpSub = "-"
operatorSymbol OpMul = "*"
operatorSymbol OpDiv = "/"
operatorSymbol OpNeg = "-"
operatorSymbol OpGt = ">"
operatorSymbol OpLt = "<"
operatorSymbol OpEq = "="
operatorSymbol OpAnd = "AND"
operatorSymbol OpOr = "OR"

-- | 运算节点对应的运算符；其余节点给 Nothing
operatorOfExpr :: Expr -> Maybe Operator
operatorOfExpr (Add _ _) = Just OpAdd
operatorOfExpr (Sub _ _) = Just OpSub
operatorOfExpr (Mul _ _) = Just OpMul
operatorOfExpr (Div _ _) = Just OpDiv
operatorOfExpr (Neg _) = Just OpNeg
operatorOfExpr (Gt _ _) = Just OpGt
operatorOfExpr (Lt _ _) = Just OpLt
operatorOfExpr (Eq _ _) = Just OpEq
operatorOfExpr (And _ _) = Just OpAnd
operatorOfExpr (Or _ _) = Just OpOr
operatorOfExpr _ = Nothing

-- | 运算符求值：算错个数是内部错误
executeOperator :: Operator -> [Value] -> Either String Value
executeOperator op vs = case (op, vs) of
    (OpNeg, [x]) -> evalNeg x
    (OpGt, [x, y]) -> compareValues (== GT) x y
    (OpLt, [x, y]) -> compareValues (< EQ) x y
    (OpEq, [x, y]) -> compareValues (== EQ) x y
    (OpAnd, [x, y]) -> andValues x y
    (OpOr, [x, y]) -> orValues x y
    (_, [x, y]) | Just a <- arithOf op -> evalArith a x y
    _ -> Left ("internal error: wrong operand count for operator " ++ operatorSymbol op)

-- | 取负
evalNeg :: Value -> Either String Value
evalNeg VNull = Right VNull
evalNeg (VInt n) = Right (VInt (negate n))
evalNeg (VFloat d) = Right (VFloat (negate d))
evalNeg _ = Left "type error: expected a number"

-- | 整数算术与浮点算术成对出现
data Arith = Arith
    { arithInt :: Int -> Int -> Either String Int
    , arithFloat :: Double -> Double -> Either String Double
    }

-- | 二元算术运算符对应的实现
arithOf :: Operator -> Maybe Arith
arithOf OpAdd = Just (Arith (\x y -> Right (x + y)) (\x y -> Right (x + y)))
arithOf OpSub = Just (Arith (\x y -> Right (x - y)) (\x y -> Right (x - y)))
arithOf OpMul = Just (Arith (\x y -> Right (x * y)) (\x y -> Right (x * y)))
arithOf OpDiv = Just divArith
arithOf _ = Nothing

-- | 一边是 NULL 就出 NULL，两边都是整数就出整数
evalArith :: Arith -> Value -> Value -> Either String Value
evalArith _ VNull _ = Right VNull
evalArith _ _ VNull = Right VNull
evalArith ops (VInt x) (VInt y) = VInt <$> arithInt ops x y
evalArith ops x y = case (asDouble x, asDouble y) of
    (Just p, Just q) -> VFloat <$> arithFloat ops p q
    _ -> Left "type error: expected two numbers"

-- | 整数除法向零截断，溢出返回错误；浮点除法不能除以零
divArith :: Arith
divArith = Arith intDiv floatDiv
  where
    -- | 整数除法：除零与溢出都报错
    intDiv _ 0 = Left "division by zero"
    intDiv x y
        | x == minBound && y == -1 = Left "integer division overflow"
        | otherwise = Right (x `quot` y)
    -- | 浮点除法：不能除以零
    floatDiv _ 0 = Left "division by zero"
    floatDiv x y = Right (x / y)

-- | 能当数用的值
asDouble :: Value -> Maybe Double
asDouble (VInt n) = Just (fromIntegral n)
asDouble (VFloat d) = Just d
asDouble _ = Nothing

-- | 比较两个值：NULL 给 NULL，其余按全序比
compareValues :: (Ordering -> Bool) -> Value -> Value -> Either String Value
compareValues rel x y
    | x == VNull || y == VNull = Right VNull
    | comparableValue x y = Right (VBool (rel (compareValue x y)))
    | otherwise = Left "type error: cannot compare these values"

-- | 两个值是否落在可以用全序比较的同一族里
comparableValue :: Value -> Value -> Bool
comparableValue (VInt _) (VFloat _) = True
comparableValue (VFloat _) (VInt _) = True
comparableValue (VInt _) (VInt _) = True
comparableValue (VFloat _) (VFloat _) = True
comparableValue (VStr _) (VStr _) = True
comparableValue (VBool _) (VBool _) = True
comparableValue _ _ = False

-- | 逻辑与的三值真值表
andValues :: Value -> Value -> Either String Value
andValues (VBool False) _ = Right (VBool False)
andValues _ (VBool False) = Right (VBool False)
andValues (VBool True) (VBool True) = Right (VBool True)
andValues VNull (VBool True) = Right VNull
andValues (VBool True) VNull = Right VNull
andValues VNull VNull = Right VNull
andValues _ _ = Left "type error: expected two booleans"

-- | 逻辑或的三值真值表
orValues :: Value -> Value -> Either String Value
orValues (VBool True) _ = Right (VBool True)
orValues _ (VBool True) = Right (VBool True)
orValues (VBool False) (VBool False) = Right (VBool False)
orValues VNull (VBool False) = Right VNull
orValues (VBool False) VNull = Right VNull
orValues VNull VNull = Right VNull
orValues _ _ = Left "type error: expected two booleans"

-- | NOT 的三值取反
threeValuedNot :: Value -> Value
threeValuedNot (VBool b) = VBool (not b)
threeValuedNot _ = VNull

-- | IN 三值逻辑：命中为真，碰 NULL 得 NULL
inValues :: Value -> [Value] -> Either String Value
inValues VNull _ = Right VNull
inValues v vs = do
    results <- mapM (\x -> executeOperator OpEq [v, x]) vs
    Right $
        if VBool True `elem` results
            then VBool True
            else if VNull `elem` results then VNull else VBool False
