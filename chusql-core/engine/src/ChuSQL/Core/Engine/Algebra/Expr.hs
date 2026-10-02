module ChuSQL.Core.Engine.Algebra.Expr (evalExpr, evalCondForRow, colsInExpr, aggregatesIn, hasAggregate, bareColumns, hasSubquery, inValues, threeValuedNot) where

import ChuSQL.Core.Model
import ChuSQL.Core.Engine.Syntax.AST
import Control.Monad (join)

-- 表达式求值：三值逻辑，NULL 参与运算结果仍是 NULL。

-- | 在一行上求出表达式的值
evalExpr :: Expr -> Row -> Either String Value
evalExpr (Col name) row =
    maybe (Left ("unknown column: " ++ name)) Right (lookup name row)
evalExpr LitNull _ = Right VNull
evalExpr (LitInt n) _ = Right (VInt n)
evalExpr (LitFloat d) _ = Right (VFloat d)
evalExpr (LitStr s) _ = Right (VStr s)
evalExpr (LitBool b) _ = Right (VBool b)
evalExpr (LitDate s) _ = Right (VStr s)
evalExpr (LitTimestamp s) _ = Right (VStr s)
evalExpr (LitBlob s) _ = Right (VStr s)
evalExpr (Add a b) row = liftArith addArith a b row
evalExpr (Sub a b) row = liftArith subArith a b row
evalExpr (Mul a b) row = liftArith mulArith a b row
evalExpr (Div a b) row = liftArith divArith a b row
evalExpr (Neg a) row = do
    v <- evalExpr a row
    case v of
        VNull -> Right VNull
        VInt n -> Right (VInt (negate n))
        VFloat d -> Right (VFloat (negate d))
        _ -> Left "type error: expected a number"
evalExpr (Gt a b) row = liftCompare (> EQ) a b row
evalExpr (Lt a b) row = liftCompare (< EQ) a b row
evalExpr (Eq a b) row = liftCompare (== EQ) a b row
evalExpr (And a b) row = liftBool andValues a b row
evalExpr (Or a b) row = liftBool orValues a b row
evalExpr (IsNull a) row = do
    v <- evalExpr a row
    Right (VBool (v == VNull))
evalExpr (IsNotNull a) row = do
    v <- evalExpr a row
    Right (VBool (v /= VNull))
evalExpr (InList a es negated) row = do
    v <- evalExpr a row
    vs <- mapM (\e -> evalExpr e row) es
    r <- inValues v vs
    Right (if negated then threeValuedNot r else r)
evalExpr (ScalarSub _) _ = Left subqueryNeedsDatabase
evalExpr (InSub _ _ _) _ = Left subqueryNeedsDatabase
evalExpr (ExistsSub _ _) _ = Left subqueryNeedsDatabase
evalExpr agg _ = Left ("aggregate functions are not allowed here: " ++ show agg)

-- | 子查询要连上存储才能跑
subqueryNeedsDatabase :: String
subqueryNeedsDatabase = "subquery needs a database to run"

-- | IN 三值逻辑：命中为真，碰 NULL 得 NULL
inValues :: Value -> [Value] -> Either String Value
inValues VNull _ = Right VNull
inValues v vs = do
    results <- mapM (compareValues (== EQ) v) vs
    Right $
        if VBool True `elem` results
            then VBool True
            else if VNull `elem` results then VNull else VBool False

-- | NOT 的三值取反
threeValuedNot :: Value -> Value
threeValuedNot (VBool b) = VBool (not b)
threeValuedNot _ = VNull

-- | 表达式用到了哪些列
colsInExpr :: Expr -> [String]
colsInExpr (Col c) = [c]
colsInExpr (Add a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Sub a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Mul a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Div a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Neg a) = colsInExpr a
colsInExpr (Gt a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Lt a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Eq a b) = colsInExpr a ++ colsInExpr b
colsInExpr (And a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Or a b) = colsInExpr a ++ colsInExpr b
colsInExpr (IsNull a) = colsInExpr a
colsInExpr (IsNotNull a) = colsInExpr a
colsInExpr (CountOf a) = colsInExpr a
colsInExpr (SumOf a) = colsInExpr a
colsInExpr (AvgOf a) = colsInExpr a
colsInExpr (MinOf a) = colsInExpr a
colsInExpr (MaxOf a) = colsInExpr a
colsInExpr (ScalarSub sq) = subqueryRefs sq
colsInExpr (InSub a sq _) = colsInExpr a ++ subqueryRefs sq
colsInExpr (InList a es _) = colsInExpr a ++ concatMap colsInExpr es
colsInExpr (ExistsSub sq _) = subqueryRefs sq
colsInExpr _ = []

-- | 表达式里有没有子查询
hasSubquery :: Expr -> Bool
hasSubquery e = case e of
    ScalarSub _ -> True
    InSub _ _ _ -> True
    InList a es _ -> any hasSubquery (a : es)
    ExistsSub _ _ -> True
    Add a b -> both a b
    Sub a b -> both a b
    Mul a b -> both a b
    Div a b -> both a b
    Neg a -> hasSubquery a
    Gt a b -> both a b
    Lt a b -> both a b
    Eq a b -> both a b
    And a b -> both a b
    Or a b -> both a b
    IsNull a -> hasSubquery a
    IsNotNull a -> hasSubquery a
    CountOf a -> hasSubquery a
    SumOf a -> hasSubquery a
    AvgOf a -> hasSubquery a
    MinOf a -> hasSubquery a
    MaxOf a -> hasSubquery a
    _ -> False
  where
    -- | 两个子表达式里任意一个有
    both a b = hasSubquery a || hasSubquery b

-- | 表达式里出现的聚合调用（按出现顺序）
aggregatesIn :: Expr -> [Expr]
aggregatesIn e = case e of
    a@CountAll -> [a]
    a@(CountOf _) -> [a]
    a@(SumOf _) -> [a]
    a@(AvgOf _) -> [a]
    a@(MinOf _) -> [a]
    a@(MaxOf _) -> [a]
    Add a b -> both a b
    Sub a b -> both a b
    Mul a b -> both a b
    Div a b -> both a b
    Neg a -> aggregatesIn a
    Gt a b -> both a b
    Lt a b -> both a b
    Eq a b -> both a b
    And a b -> both a b
    Or a b -> both a b
    IsNull a -> aggregatesIn a
    IsNotNull a -> aggregatesIn a
    _ -> []
  where
    -- | 两个子表达式的聚合拼起来
    both a b = aggregatesIn a ++ aggregatesIn b

-- | 表达式里有没有聚合调用
hasAggregate :: Expr -> Bool
hasAggregate = not . null . aggregatesIn

-- | 聚合之外的列引用（聚合参数里的列不算）
bareColumns :: Expr -> [String]
bareColumns e = case e of
    Col c -> [c]
    Add a b -> both a b
    Sub a b -> both a b
    Mul a b -> both a b
    Div a b -> both a b
    Neg a -> bareColumns a
    Gt a b -> both a b
    Lt a b -> both a b
    Eq a b -> both a b
    And a b -> both a b
    Or a b -> both a b
    IsNull a -> bareColumns a
    IsNotNull a -> bareColumns a
    ScalarSub sq -> subqueryRefs sq
    InSub a sq _ -> bareColumns a ++ subqueryRefs sq
    InList a es _ -> bareColumns a ++ concatMap bareColumns es
    ExistsSub sq _ -> subqueryRefs sq
    _ -> []
  where
    -- | 两个子表达式的裸列拼起来
    both a b = bareColumns a ++ bareColumns b

-- | 判断条件真假：只有 TRUE 通过，NULL 当作不通过
evalCondForRow :: Expr -> Row -> Either String Bool
evalCondForRow e row = do
    v <- evalExpr e row
    case v of
        VBool b -> Right b
        VNull -> Right False
        _ -> Left "type error: WHERE condition must be a boolean"

-- | 把二元运算提升到 Either
liftBinOp ::
    (Value -> Value -> Either String Value) ->
    Expr ->
    Expr ->
    Row ->
    Either String Value
liftBinOp f a b row = join (f <$> evalExpr a row <*> evalExpr b row)

-- | 整数算术与浮点算术成对出现
data Arith = Arith
    { arithInt :: Int -> Int -> Either String Int
    , arithFloat :: Double -> Double -> Either String Double
    }

-- | 一边是 NULL 就出 NULL，两边都是整数就出整数
liftArith :: Arith -> Expr -> Expr -> Row -> Either String Value
liftArith ops a b row = liftBinOp go a b row
  where
    -- | NULL 出 NULL，整数配整数走整数算术
    go VNull _ = Right VNull
    go _ VNull = Right VNull
    go (VInt x) (VInt y) = VInt <$> arithInt ops x y
    go x y = case (asDouble x, asDouble y) of
        (Just p, Just q) -> VFloat <$> arithFloat ops p q
        _ -> Left "type error: expected two numbers"

-- | 能当数用的值
asDouble :: Value -> Maybe Double
asDouble (VInt n) = Just (fromIntegral n)
asDouble (VFloat d) = Just d
asDouble _ = Nothing

-- | 加法
addArith :: Arith
addArith = Arith (\x y -> Right (x + y)) (\x y -> Right (x + y))

-- | 减法
subArith :: Arith
subArith = Arith (\x y -> Right (x - y)) (\x y -> Right (x - y))

-- | 乘法
mulArith :: Arith
mulArith = Arith (\x y -> Right (x * y)) (\x y -> Right (x * y))

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

-- | 比较：任一侧是 NULL 就给 NULL
liftCompare ::
    (Ordering -> Bool) ->
    Expr ->
    Expr ->
    Row ->
    Either String Value
liftCompare rel a b row = liftBinOp (compareValues rel) a b row

-- | 比较两个值：NULL 给 NULL，其余按数、串、布尔比
compareValues :: (Ordering -> Bool) -> Value -> Value -> Either String Value
compareValues rel x y
    | x == VNull || y == VNull = Right VNull
    | Just p <- asDouble x, Just q <- asDouble y = Right (VBool (rel (compare p q)))
    | (VStr p, VStr q) <- (x, y) = Right (VBool (rel (compare p q)))
    | (VBool p, VBool q) <- (x, y) = Right (VBool (rel (compare p q)))
    | otherwise = Left "type error: cannot compare these values"

-- | 布尔二元运算
liftBool ::
    (Value -> Value -> Either String Value) ->
    Expr ->
    Expr ->
    Row ->
    Either String Value
liftBool f = liftBinOp f

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
