module ChuSQL.Semantic (check) where

import ChuSQL.Algebra.Expr (colsInExpr)
import ChuSQL.Model
import ChuSQL.Syntax.AST
import Data.List (intercalate, nub)

-- 语义检查：表和列在不在、类型对不对，全部在执行前查。

-- * 环境
-- | 给列名加上别名前缀
prefixColumns :: Maybe String -> [(String, Column)] -> [(String, Column)]
prefixColumns mAlias cols = [(prefix ++ c, ty) | (c, ty) <- cols]
  where
    prefix = maybe "" (++ ".") mAlias

-- | 收集 FROM 能提供的列
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


-- * 检查
-- | 拼“列不存在”的报错
missingColumn :: String -> String -> [(String, Column)] -> String
missingColumn place c env =
    "unknown column in " ++ place ++ ": " ++ c ++ hint
  where
    hint
        | null env = ""
        | otherwise = " (available: " ++ intercalate ", " (map fst env) ++ ")"

-- | 检查列是否都在环境里
checkColumns :: String -> [(String, Column)] -> [String] -> Either String ()
checkColumns place env requested =
    case [c | c <- requested, c /= allColumns, c `notElem` map fst env] of
        [] -> Right ()
        (c : _) -> Left (missingColumn place c env)


-- | 推导表达式的类型
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

-- | 检查二元运算两边的类型
operands :: String -> [(String, Column)] -> Column -> Expr -> Expr -> Either String Column
operands place env want a b = do
    ta <- inferExpr place env a
    tb <- inferExpr place env b
    if ta == want && tb == want
        then Right TBool
        else Left (place ++ ": operator needs " ++ show want ++ " on both sides, got " ++ show ta ++ " and " ++ show tb)

-- | 要求条件能算出布尔
checkBool :: String -> [(String, Column)] -> Expr -> Either String ()
checkBool place env e = do
    checkColumns place env (colsInExpr e)
    t <- inferExpr place env e
    if t == TBool
        then Right ()
        else Left (place ++ ": condition must be a boolean, got " ++ show t)


-- * 写入语句
-- | 检查赋值列和类型
checkTyped :: String -> [(String, Column)] -> Table -> (String, Expr) -> Either String ()
checkTyped place env t (c, e) = do
    want <- maybe (Left (missingColumn place c (tableCols t))) Right (colType t c)
    got <- inferExpr place env e
    if want == got
        then Right ()
        else Left (place ++ ": column " ++ c ++ " needs " ++ show want ++ ", got " ++ show got)

-- | 检查插入值的类型
checkValue :: Table -> (String, Expr) -> Either String ()
checkValue = checkTyped "INSERT" []

-- | 检查 UPDATE 的赋值
checkAssign :: Table -> (String, Expr) -> Either String ()
checkAssign t = checkTyped "UPDATE" (tableCols t) t


-- * 入口
-- | 按语句类型分派检查
check :: Database -> Statement -> Either String ()
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
    CreateTable name cols -> do
        if null name
            then Left "CREATE TABLE: empty table name"
            else do
                let names = map fst cols
                if length names /= length (nub names)
                    then Left ("CREATE TABLE: duplicate column names in " ++ name)
                    else Right ()
    DropTable name ->
        if null name
            then Left "DROP TABLE: empty table name"
            else Right ()
