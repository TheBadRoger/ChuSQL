module ChuSQL.Semantic (check) where

import ChuSQL.Algebra.Expr (colsInExpr)
import ChuSQL.Model
import ChuSQL.Syntax.AST
import Data.List (intercalate)

-- 语义检查：执行前的关卡 —— 表和列存不存在、表达式类型对不对、写入的值类型是否匹配。

-- * 表与列名解析

-- | 给一批列加上"别名."前缀
prefixColumns :: Maybe String -> [(String, Column)] -> [(String, Column)]
prefixColumns mAlias cols = [(prefix ++ c, ty) | (c, ty) <- cols]
  where
    prefix = maybe "" (++ ".") mAlias

-- | 走一遍 FROM：算出可用列（带别名前缀），顺手把每个 ON 条件也查了
checkFrom :: Database -> FromClause -> Either String [(String, Column)]
checkFrom db (FromTable mAlias tbl) = do
    t <- lookupTable db tbl
    Right (prefixColumns mAlias (tableCols t))
checkFrom db (FromJoin left mAlias tbl cond) = do
    lcols <- checkFrom db left
    t <- lookupTable db tbl
    let env = lcols ++ prefixColumns mAlias (tableCols t)
    checkBool "ON" env cond
    Right env

-- * 列检查与报错

-- | 统一拼一条带"可用列清单"的报错
missingColumn :: String -> String -> [(String, Column)] -> String
missingColumn place c env =
    "unknown column in " ++ place ++ ": " ++ c ++ hint
  where
    hint
        | null env = ""
        | otherwise = " (available: " ++ intercalate ", " (map fst env) ++ ")"

-- | 请求的列必须都在可用清单里；哨兵 allColumns 表示"全部列"，放行
checkColumns :: String -> [(String, Column)] -> [String] -> Either String ()
checkColumns place env requested =
    case [c | c <- requested, c /= allColumns, c `notElem` map fst env] of
        [] -> Right ()
        (c : _) -> Left (missingColumn place c env)

-- * 表达式类型推导

-- | 推出一个表达式的结果类型；列不存在、或类型对不上，都报错
inferExpr :: String -> [(String, Column)] -> Expr -> Either String Column
inferExpr place env (Col c) = maybe (Left (missingColumn place c env)) Right (lookup c env)
inferExpr _ _ (LitInt _) = Right TInt
inferExpr _ _ (LitStr _) = Right TStr
inferExpr _ _ (LitBool _) = Right TBool
inferExpr place env (Gt a b) = operands place env TInt a b
inferExpr place env (Lt a b) = operands place env TInt a b
inferExpr place env (And a b) = operands place env TBool a b
inferExpr place env (Or a b) = operands place env TBool a b
inferExpr place env (Eq a b) = do
    ta <- inferExpr place env a
    tb <- inferExpr place env b
    if ta == tb
        then Right TBool
        else Left (place ++ ": both sides of = must have the same type, got " ++ show ta ++ " and " ++ show tb)

-- | 二元运算的通用检查：两边都必须是同一种期望类型，结果都是布尔
operands :: String -> [(String, Column)] -> Column -> Expr -> Expr -> Either String Column
operands place env want a b = do
    ta <- inferExpr place env a
    tb <- inferExpr place env b
    if ta == want && tb == want
        then Right TBool
        else Left (place ++ ": operator needs " ++ show want ++ " on both sides, got " ++ show ta ++ " and " ++ show tb)

-- | 这个表达式必须能算成一个条件（布尔）
checkBool :: String -> [(String, Column)] -> Expr -> Either String ()
checkBool place env e = do
    checkColumns place env (colsInExpr e)
    t <- inferExpr place env e
    if t == TBool
        then Right ()
        else Left (place ++ ": condition must be a boolean, got " ++ show t)

-- * 写入语句

-- | 一条赋值的通用检查：目标列必须存在，右边表达式的类型要对得上
checkTyped :: String -> [(String, Column)] -> Table -> (String, Expr) -> Either String ()
checkTyped place env t (c, e) = do
    want <- maybe (Left (missingColumn place c (tableCols t))) Right (colType t c)
    got <- inferExpr place env e
    if want == got
        then Right ()
        else Left (place ++ ": column " ++ c ++ " needs " ++ show want ++ ", got " ++ show got)

-- | INSERT 的一个值：环境是空的 —— 值必须是字面量，写列名会被拦下
checkValue :: Table -> (String, Expr) -> Either String ()
checkValue = checkTyped "INSERT" []

-- | UPDATE 的一个赋值：环境是整张表 —— 右边可以引用本行的列
checkAssign :: Table -> (String, Expr) -> Either String ()
checkAssign t = checkTyped "UPDATE" (tableCols t) t

-- * 总入口

-- | 语义检查总入口：合法返回 Right ()，不合法返回 Left（带人话解释）
check :: Database -> Query -> Either String ()
check db q = case q of
    Select
        { selectCols = cols
        , selectFrom = fromC
        , selectWhere = mWhere
        , selectOrderBy = orderBy
        } -> do
            env <- checkFrom db fromC
            checkColumns "SELECT" env cols
            checkColumns "ORDER BY" env (map fst orderBy)
            mapM_ (checkBool "WHERE" env) mWhere
    Insert tbl cols vals -> do
        t <- lookupTable db tbl
        checkColumns "INSERT" (tableCols t) cols
        mapM_ (checkValue t) (zip cols vals)
    Delete tbl mWhere -> do
        t <- lookupTable db tbl
        mapM_ (checkBool "WHERE" (tableCols t)) mWhere
    Update tbl assigns mWhere -> do
        t <- lookupTable db tbl
        checkColumns "UPDATE" (tableCols t) (map fst assigns)
        mapM_ (checkAssign t) assigns
        mapM_ (checkBool "WHERE" (tableCols t)) mWhere
