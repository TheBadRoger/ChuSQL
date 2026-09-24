module ChuSQL.Algebra.Expr (evalExpr, evalCondForRow, colsInExpr) where

import ChuSQL.Model
import ChuSQL.Syntax.AST
import Control.Monad (join)

-- 表达式求值：拿一行数据算出一个值。

-- * 求值
-- | 在一行上求出表达式的值
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

-- * 列引用
-- | 表达式用到了哪些列
colsInExpr :: Expr -> [String]
colsInExpr (Col c) = [c]
colsInExpr (Gt a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Lt a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Eq a b) = colsInExpr a ++ colsInExpr b
colsInExpr (And a b) = colsInExpr a ++ colsInExpr b
colsInExpr (Or a b) = colsInExpr a ++ colsInExpr b
colsInExpr _ = []

-- | 判断条件真假
evalCondForRow :: Expr -> Row -> Either String Bool
evalCondForRow e row = do
    v <- evalExpr e row
    case v of
        VBool b -> Right b
        _ -> Left "type error: WHERE condition must be a boolean"

-- * 内部
-- | 把二元运算提升到 Either
liftBinOp ::
    (Value -> Value -> Either String Value) ->
    Expr ->
    Expr ->
    Row ->
    Either String Value
liftBinOp f a b row = join (f <$> evalExpr a row <*> evalExpr b row)

-- | 整数二元运算
intOp :: (Int -> Int -> Bool) -> Value -> Value -> Either String Value
intOp f (VInt x) (VInt y) = Right (VBool (f x y))
intOp _ _ _ = Left "type error: expected two integers"

-- | 布尔二元运算
boolOp :: (Bool -> Bool -> Bool) -> Value -> Value -> Either String Value
boolOp f (VBool x) (VBool y) = Right (VBool (f x y))
boolOp _ _ _ = Left "type error: expected two booleans"

-- | 相等比较
eqOp :: Value -> Value -> Either String Value
eqOp x y = Right (VBool (x == y))
