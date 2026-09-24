module ChuSQL.Algebra.Expr (evalExpr, evalCondForRow, colsInExpr) where

import ChuSQL.Model
import ChuSQL.Syntax.AST
import Control.Monad (join)

-- 表达式求值：在一行（列名 → 值）上算出单个值；另外提供"静态收集表达式里用到哪些列"的工具。

-- 表达式求值
evalExpr :: Expr -> Row -> Either String Value
evalExpr (Col name) row =
    maybe (Left ("unknown column: " ++ name)) Right (lookup name row)
evalExpr (LitInt n) _ = Right (VInt n)
evalExpr (LitStr s) _ = Right (VStr s)
evalExpr (LitBool b) _ = Right (VBool b)
evalExpr (Gt a b) row = liftBinOp (intOp (>)) a b row
evalExpr (Lt a b) row = liftBinOp (intOp (<)) a b row
evalExpr (Eq a b) row = liftBinOp eqOp a b row
evalExpr (And a b) row = liftBinOp (boolOp (&&)) a b row
evalExpr (Or a b) row = liftBinOp (boolOp (||)) a b row

-- 收集表达式里用到的列名
colsInExpr :: Expr -> [String]
colsInExpr (Col c) = [c]
colsInExpr (Gt a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Lt a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Eq a b) = colsInExpr a ++ colsInExpr b
colsInExpr (And a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Or a b) = colsInExpr a ++ colsInExpr b
colsInExpr _ = []

-- 对行计算条件语句
evalCondForRow :: Expr -> Row -> Either String Bool
evalCondForRow e row = do
    v <- evalExpr e row
    case v of
        VBool b -> Right b
        _ -> Left "type error: WHERE condition must be a boolean"

-- 固定模式：对两个子表达式求值，并返还给二元操作符
liftBinOp ::
    (Value -> Value -> Either String Value) ->
    Expr ->
    Expr ->
    Row ->
    Either String Value
liftBinOp f a b row = join (f <$> evalExpr a row <*> evalExpr b row)

-- 整数比较
intOp :: (Int -> Int -> Bool) -> Value -> Value -> Either String Value
intOp f (VInt x) (VInt y) = Right (VBool (f x y))
intOp _ _ _ = Left "type error: expected two integers"

-- 布尔运算
boolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Either String Value
boolOp f (VBool x) (VBool y) = Right (VBool (f x y))
boolOp _ _ _ = Left "type error: expected two booleans"

-- 相等比较
eqOp :: Value -> Value -> Either String Value
eqOp x y = Right (VBool (x == y))
