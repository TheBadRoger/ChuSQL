module ChuSQL.Core.Engine.Algebra.Expr (evalExpr, evalCondForRow, colsInExpr, subqueryRefsIn, aggregatesIn, hasAggregate, bareColumns, hasSubquery, hasDivision, inValues, threeValuedNot, quantifiedCompare) where

import ChuSQL.Core.Model
import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Engine.Builtin (Operator (..), compareNode, executeOperator, inValues, threeValuedNot)
import ChuSQL.Core.Engine.Runtime.SQL (invokeSQLFunction, constructSQLValue)

-- 表达式求值：三值逻辑，NULL 参与运算结果仍是 NULL。

-- | 在一行上求出表达式的值
evalExpr :: Expr -> Row -> Either String Value
evalExpr (Col name) row =
    maybe (Left ("unknown column: " ++ name)) Right (lookup name row)
evalExpr (FunctionCall name _) _ = Left ("unbound function call: " ++ name)
evalExpr (Construct _ _) _ = Left "unbound runtime constructor"
evalExpr (BoundConstruct tid arguments) row = mapM (\argument -> evalExpr argument row) arguments >>= constructSQLValue tid
evalExpr (RuntimeLiteral tid value) _ = coerceValue (CRuntime tid) (VRuntime tid value)
evalExpr (BoundFunction fid tid arguments) row =
    mapM (\argument -> evalExpr argument row) arguments >>= invokeSQLFunction fid tid
evalExpr LitNull _ = Right VNull
evalExpr (LitInt n) _ = Right (VInt n)
evalExpr (LitFloat d) _ = Right (VFloat d)
evalExpr (LitStr s) _ = Right (VStr s)
evalExpr (LitBool b) _ = Right (VBool b)
evalExpr (LitDate s) _ = Right (VStr s)
evalExpr (LitTimestamp s) _ = Right (VStr s)
evalExpr (LitBlob s) _ = Right (VStr s)
evalExpr (Add a b) row = liftOperator OpAdd a b row
evalExpr (Sub a b) row = liftOperator OpSub a b row
evalExpr (Mul a b) row = liftOperator OpMul a b row
evalExpr (Div a b) row = liftOperator OpDiv a b row
evalExpr (Neg a) row = do
    v <- evalExpr a row
    executeOperator OpNeg [v]
evalExpr (Gt a b) row = liftOperator OpGt a b row
evalExpr (Lt a b) row = liftOperator OpLt a b row
evalExpr (Eq a b) row = liftOperator OpEq a b row
evalExpr (GtE a b) row = liftOperator OpGtE a b row
evalExpr (LtE a b) row = liftOperator OpLtE a b row
evalExpr (NotEq a b) row = liftOperator OpNe a b row
evalExpr (And a b) row = liftOperator OpAnd a b row
evalExpr (Or a b) row = liftOperator OpOr a b row
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
evalExpr (QuantCmp _ _ _ _) _ = Left subqueryNeedsDatabase
evalExpr agg _ = Left ("aggregate functions are not allowed here: " ++ show agg)

-- | 子查询要连上存储才能跑
subqueryNeedsDatabase :: String
subqueryNeedsDatabase = "subquery needs a database to run"

-- | 量词比较摊成 AND / OR 链
quantifiedCompare :: CompareOp -> Expr -> [Expr] -> Quantifier -> Expr
quantifiedCompare op lhs vs q = case vs of
    [] -> LitBool (q == AllQ)
    _ -> foldr1 (if q == AllQ then And else Or) (map (compareNode op lhs) vs)

-- | 表达式用到了哪些列
colsInExpr :: Expr -> [String]
colsInExpr (Col c) = [c]
colsInExpr (Construct _ arguments) = concatMap colsInExpr arguments
colsInExpr (BoundConstruct _ arguments) = concatMap colsInExpr arguments
colsInExpr (FunctionCall _ arguments) = concatMap colsInExpr arguments
colsInExpr (BoundFunction _ _ arguments) = concatMap colsInExpr arguments
colsInExpr (Add a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Sub a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Mul a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Div a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Neg a) = colsInExpr a
colsInExpr (Gt a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Lt a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Eq a b) = colsInExpr a ++ colsInExpr b
colsInExpr (GtE a b) = colsInExpr a ++ colsInExpr b
colsInExpr (LtE a b) = colsInExpr a ++ colsInExpr b
colsInExpr (NotEq a b) = colsInExpr a ++ colsInExpr b
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
colsInExpr (QuantCmp _ a sq _) = colsInExpr a ++ subqueryRefs sq
colsInExpr _ = []

-- | 表达式里子查询引用的外层列
subqueryRefsIn :: Expr -> [String]
subqueryRefsIn e = case e of
    Construct _ arguments -> concatMap subqueryRefsIn arguments
    BoundConstruct _ arguments -> concatMap subqueryRefsIn arguments
    FunctionCall _ arguments -> concatMap subqueryRefsIn arguments
    BoundFunction _ _ arguments -> concatMap subqueryRefsIn arguments
    ScalarSub sq -> subqueryRefs sq
    InSub a sq _ -> subqueryRefsIn a ++ subqueryRefs sq
    InList a es _ -> concatMap subqueryRefsIn (a : es)
    ExistsSub sq _ -> subqueryRefs sq
    QuantCmp _ a sq _ -> subqueryRefsIn a ++ subqueryRefs sq
    Add a b -> both a b
    Sub a b -> both a b
    Mul a b -> both a b
    Div a b -> both a b
    Neg a -> subqueryRefsIn a
    Gt a b -> both a b
    Lt a b -> both a b
    Eq a b -> both a b
    GtE a b -> both a b
    LtE a b -> both a b
    NotEq a b -> both a b
    And a b -> both a b
    Or a b -> both a b
    IsNull a -> subqueryRefsIn a
    IsNotNull a -> subqueryRefsIn a
    CountOf a -> subqueryRefsIn a
    SumOf a -> subqueryRefsIn a
    AvgOf a -> subqueryRefsIn a
    MinOf a -> subqueryRefsIn a
    MaxOf a -> subqueryRefsIn a
    _ -> []
  where
    -- | 两个子表达式的外层列拼起来
    both a b = subqueryRefsIn a ++ subqueryRefsIn b

-- | 表达式里有没有除法（除法可能报错，不能提前或延后求值）
hasDivision :: Expr -> Bool
hasDivision e = case e of
    Construct _ arguments -> any hasDivision arguments
    BoundConstruct _ arguments -> any hasDivision arguments
    FunctionCall _ arguments -> any hasDivision arguments
    BoundFunction _ _ arguments -> any hasDivision arguments
    Div _ _ -> True
    Neg a -> hasDivision a
    Add a b -> both a b
    Sub a b -> both a b
    Mul a b -> both a b
    Gt a b -> both a b
    Lt a b -> both a b
    Eq a b -> both a b
    GtE a b -> both a b
    LtE a b -> both a b
    NotEq a b -> both a b
    And a b -> both a b
    Or a b -> both a b
    QuantCmp _ a _ _ -> hasDivision a
    _ -> False
  where
    -- | 两个子表达式里任意一个有除法
    both a b = hasDivision a || hasDivision b

-- | 表达式里有没有子查询
hasSubquery :: Expr -> Bool
hasSubquery e = case e of
    Construct _ arguments -> any hasSubquery arguments
    BoundConstruct _ arguments -> any hasSubquery arguments
    FunctionCall _ arguments -> any hasSubquery arguments
    BoundFunction _ _ arguments -> any hasSubquery arguments
    ScalarSub _ -> True
    InSub _ _ _ -> True
    InList a es _ -> any hasSubquery (a : es)
    ExistsSub _ _ -> True
    QuantCmp _ _ _ _ -> True
    Add a b -> both a b
    Sub a b -> both a b
    Mul a b -> both a b
    Div a b -> both a b
    Neg a -> hasSubquery a
    Gt a b -> both a b
    Lt a b -> both a b
    Eq a b -> both a b
    GtE a b -> both a b
    LtE a b -> both a b
    NotEq a b -> both a b
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
    Construct _ arguments -> concatMap aggregatesIn arguments
    BoundConstruct _ arguments -> concatMap aggregatesIn arguments
    FunctionCall _ arguments -> concatMap aggregatesIn arguments
    BoundFunction _ _ arguments -> concatMap aggregatesIn arguments
    a@CountAll -> [a]
    a@(CountOf _) -> [a]
    a@(SumOf _) -> [a]
    a@(AvgOf _) -> [a]
    a@(MinOf _) -> [a]
    a@(MaxOf _) -> [a]
    QuantCmp _ a _ _ -> aggregatesIn a
    Add a b -> both a b
    Sub a b -> both a b
    Mul a b -> both a b
    Div a b -> both a b
    Neg a -> aggregatesIn a
    Gt a b -> both a b
    Lt a b -> both a b
    Eq a b -> both a b
    GtE a b -> both a b
    LtE a b -> both a b
    NotEq a b -> both a b
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
    Construct _ arguments -> concatMap bareColumns arguments
    BoundConstruct _ arguments -> concatMap bareColumns arguments
    FunctionCall _ arguments -> concatMap bareColumns arguments
    BoundFunction _ _ arguments -> concatMap bareColumns arguments
    Col c -> [c]
    Add a b -> both a b
    Sub a b -> both a b
    Mul a b -> both a b
    Div a b -> both a b
    Neg a -> bareColumns a
    Gt a b -> both a b
    Lt a b -> both a b
    Eq a b -> both a b
    GtE a b -> both a b
    LtE a b -> both a b
    NotEq a b -> both a b
    And a b -> both a b
    Or a b -> both a b
    IsNull a -> bareColumns a
    IsNotNull a -> bareColumns a
    ScalarSub sq -> subqueryRefs sq
    InSub a sq _ -> bareColumns a ++ subqueryRefs sq
    InList a es _ -> bareColumns a ++ concatMap bareColumns es
    ExistsSub sq _ -> subqueryRefs sq
    QuantCmp _ a sq _ -> bareColumns a ++ subqueryRefs sq
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

-- | 二元运算节点统一走运算符权威
liftOperator :: Operator -> Expr -> Expr -> Row -> Either String Value
liftOperator op a b row = do
    x <- evalExpr a row
    y <- evalExpr b row
    executeOperator op [x, y]
