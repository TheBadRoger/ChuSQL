module ChuSQL.Core.Engine.Semantic (check, prepare, prepareRowExpression, querySchema) where

import ChuSQL.Core.Engine.Algebra.Expr (bareColumns, colsInExpr, hasAggregate)
import ChuSQL.Core.Model
import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Engine.Syntax.Parser (parseExpression)
import ChuSQL.Core.Engine.Runtime.Functions (FunctionSignature (..), builtinFunctions, functionSignature)
import ChuSQL.Core.Engine.Runtime.SQL (resolveSQLFunction, sqlResultType, sqlType)
import ChuSQL.Core.Engine.Runtime.Types
import Data.Char (isAlphaNum, isAscii, toLower)
import Data.List (intercalate, isInfixOf, isPrefixOf, isSuffixOf, nub)

-- 语义检查：表和列在不在、类型对不对，全部在执行前查。

-- | 推导出来的类型：NULL 没有类型，可以放进任何列
data InferredType
    = InferNull
    | InferType ColumnType
    deriving (Show, Eq)

-- | 给列名加上别名前缀
prefixColumns :: Maybe String -> [(String, Column)] -> [(String, Column)]
prefixColumns mAlias cols = [(prefix ++ c, ty) | (c, ty) <- cols]
  where
    prefix = maybe "" (++ ".") mAlias

-- | 从 schema 推导查询输出列
querySchema :: Database -> Statement -> Either String [(String, Column)]
querySchema db statement = do
    columns <- inferSubqueryOutput db (Scope [] []) (Subquery statement [])
    Right [(name, plainColumn (derivedType ty)) | (name, ty) <- columns]

-- | 检查期的作用域：本层优先，外层兜底
data Scope = Scope
    { scopeInner :: [(String, Column)]
    , scopeOuter :: [(String, Column)]
    }

-- | 本层先查，查不到再落外层；本层同名引起歧义时不往外层让
scopeAt :: String -> Scope -> String -> Either String (String, Column)
scopeAt place scope c
    | null (scopeOuter scope) = columnAt place (scopeInner scope) c
    | otherwise = case columnAt place (scopeInner scope) c of
        Right entry -> Right entry
        Left err
            | "ambiguous column:" `isInfixOf` err -> Left err
            | otherwise -> columnAt place (scopeOuter scope) c

-- | 收集 FROM 的列，列前缀由表名推导
checkFrom :: Database -> [(String, Column)] -> FromClause -> Either String [(String, Column)]
checkFrom _ _ FromUnit = Right []
checkFrom db _ (FromTable mAlias tbl) = do
    (key, t) <- resolveTable db tbl
    Right (prefixColumns (Just (deriveQualifier mAlias key)) (tableCols t))
checkFrom db _ (FromSubquery mAlias stmt) = do
    out <- inferSubqueryOutput db (Scope [] []) (Subquery stmt [])
    Right (prefixColumns mAlias [(name, plainColumn (derivedType t)) | (name, t) <- out])
checkFrom db outer (FromJoin _ left right cond) = do
    lcols <- checkFrom db outer left
    rcols <- checkFrom db outer right
    let env = lcols ++ rcols
    checkBool db "ON" (Scope env outer) cond
    Right env

-- | 保留派生表输出的类型或未定型 NULL
derivedType :: InferredType -> ColumnType
derivedType (InferType t) = t
derivedType InferNull = CNull

-- | 将列类型还原为表达式推导类型
inferredColumnType :: ColumnType -> InferredType
inferredColumnType CNull = InferNull
inferredColumnType ty = InferType ty

-- | 合并两层列，本层优先且不重复
scopedEnv :: [(String, Column)] -> [(String, Column)] -> [(String, Column)]
scopedEnv env outer = env ++ [o | o <- outer, o `notElem` env]

-- | 把作用域摊平成一层的列清单
scopeFlat :: Scope -> [(String, Column)]
scopeFlat scope = scopedEnv (scopeInner scope) (scopeOuter scope)

-- | 拼“列不存在”的报错
missingColumn :: String -> String -> [(String, Column)] -> String
missingColumn place c env =
    "unknown column in " ++ place ++ ": " ++ c ++ hint
  where
    hint
        | null env = ""
        | otherwise = " (available: " ++ intercalate ", " (map (reverse . takeWhile (/= '.') . reverse . fst) env) ++ "; qualified: " ++ intercalate ", " (map fst env) ++ ")"

-- | 检查列是否都在环境里
checkColumns :: String -> Scope -> [String] -> Either String ()
checkColumns place env = mapM_ (\c -> if c == allColumns then Right () else () <$ scopeAt place env c)

-- | 在一层里查一列，报错时带上子句名与可用列
columnAt :: String -> [(String, Column)] -> String -> Either String (String, Column)
columnAt place env c = case resolveColumn c env of
    Right entry -> Right entry
    Left err | "ambiguous column:" `isPrefixOf` err -> Left (place ++ ": " ++ err)
    Left _ -> Left (missingColumn place c env)

-- | 聚合不能出现在 WHERE 里
checkNoAggregate :: String -> Expr -> Either String ()
checkNoAggregate place e
    | hasAggregate e = Left (place ++ ": aggregate functions are not allowed here")
    | otherwise = Right ()

-- | 分组查询里，没被聚合的列必须出现在 GROUP BY 里
checkGrouping :: Scope -> [String] -> [(String, Expr)] -> Either String ()
checkGrouping env keys items
    | null keys && not (any (hasAggregate . snd) items) = Right ()
    | otherwise = do
        checkColumns "GROUP BY" env keys
        mapM_ (checkRuntimeColumn "GROUP BY" env) keys
        mapM_ okCol (nub (concatMap (bareColumns . snd) items))
  where
    -- | 这一列能不能出现在分组查询里
    okCol c
        | c == allColumns = Left "SELECT: * cannot be selected together with GROUP BY"
        | c `elem` keys = Right ()
        | otherwise = Left ("SELECT: column " ++ c ++ " must appear in GROUP BY or be aggregated")

-- | 渲染一个推导类型
renderType :: InferredType -> String
renderType InferNull = "NULL"
renderType (InferType t) = typeLabel t

-- | 推导表达式的类型（子查询也在这里整条查一遍）
inferExpr :: Database -> String -> Scope -> Expr -> Either String InferredType
inferExpr _ place env (Col c) = inferredColumnType . columnType . snd <$> scopeAt place env c
inferExpr _ _ _ LitNull = Right InferNull
inferExpr _ _ _ (LitInt _) = Right (InferType CInt)
inferExpr _ _ _ (LitFloat _) = Right (InferType CFloat)
inferExpr _ _ _ (LitStr _) = Right (InferType CStr)
inferExpr _ _ _ (LitBool _) = Right (InferType CBool)
inferExpr _ _ _ (LitDate _) = Right (InferType CDate)
inferExpr _ _ _ (LitTimestamp _) = Right (InferType CTimestamp)
inferExpr _ _ _ (LitBlob _) = Right (InferType CBlob)
inferExpr db place env (Construct ty arguments) = InferType . CRuntime <$> inferConstruction db place env ty arguments
inferExpr db place env (BoundConstruct tid arguments) = do
    inferred <- inferConstruction db place env (typeExpression tid) arguments
    if inferred == tid then Right (InferType (CRuntime tid)) else Left "constructor result identity mismatch"
inferExpr _ _ _ (RuntimeLiteral tid value) = validateValue builtinTypes tid value >> Right (InferType (CRuntime tid))
inferExpr db place env (FunctionCall name arguments) = do
    inferred <- mapM (inferExpr db place env) arguments
    sig <- resolveSQLFunction name (map inferredArgument inferred)
    InferType <$> sqlResultType (signatureResult sig)
inferExpr db place env (BoundFunction fid tid arguments) = do
    functions <- builtinFunctions builtinTypes
    expected <- functionSignature functions fid
    inferred <- mapM (inferExpr db place env) arguments
    resolved <- resolveSQLFunction (signatureName expected) (map inferredArgument inferred)
    if signatureId resolved == fid && signatureResult resolved == tid
        then InferType <$> sqlResultType tid
        else Left "bound function signature identity mismatch"
inferExpr db place env (Add a b) = arithmetic db place env a b
inferExpr db place env (Sub a b) = arithmetic db place env a b
inferExpr db place env (Mul a b) = arithmetic db place env a b
inferExpr db place env (Div a b) = arithmetic db place env a b
inferExpr db place env (Neg a) = do
    t <- inferExpr db place env a
    case t of
        InferNull -> Right InferNull
        InferType x
            | numericType x -> Right t
            | otherwise -> Left (place ++ ": operator needs TInt on both sides, got TInt and " ++ typeLabel x)
inferExpr db place env (Gt a b) = comparison db place env a b
inferExpr db place env (Lt a b) = comparison db place env a b
inferExpr db place env (Eq a b) = comparison db place env a b
inferExpr db place env (GtE a b) = comparison db place env a b
inferExpr db place env (LtE a b) = comparison db place env a b
inferExpr db place env (NotEq a b) = comparison db place env a b
inferExpr db place env (And a b) = boolean db place env a b
inferExpr db place env (Or a b) = boolean db place env a b
inferExpr db place env (IsNull a) = inferExpr db place env a >> Right (InferType CBool)
inferExpr db place env (IsNotNull a) = inferExpr db place env a >> Right (InferType CBool)
inferExpr _ _ _ CountAll = Right (InferType CInt)
inferExpr db place env (CountOf a) = do
    _ <- aggregateArg db place env a
    Right (InferType CInt)
inferExpr db place env (SumOf a) = aggregateArg db place env a >>= numericResult "SUM" place
inferExpr db place env (AvgOf a) = do
    t <- aggregateArg db place env a
    case t of
        InferNull -> Right InferNull
        InferType x
            | numericType x -> Right (InferType CFloat)
            | otherwise -> Left (place ++ ": AVG needs a number, got " ++ typeLabel x)
inferExpr db place env (MinOf a) = aggregateArg db place env a >>= requireOrdering "MIN"
inferExpr db place env (MaxOf a) = aggregateArg db place env a >>= requireOrdering "MAX"
inferExpr db place env (ScalarSub sq) = do
    cols <- inferSubqueryOutput db env sq
    case cols of
        [(_, t)] -> Right t
        _ -> Left (place ++ ": scalar subquery must return exactly one column")
inferExpr db place env (InSub a sq _) = do
    ta <- inferExpr db place env a
    cols <- inferSubqueryOutput db env sq
    case cols of
        [(_, t)]
            | compatible ta t -> Right (InferType CBool)
            | otherwise -> Left (place ++ ": IN subquery column has type " ++ renderType t ++ ", expected " ++ renderType ta)
        _ -> Left (place ++ ": IN subquery must return exactly one column")
inferExpr db place env (InList a es _) = do
    ta <- inferExpr db place env a
    ts <- mapM (inferExpr db place env) es
    mapM_ (itemType place ta) ts
    Right (InferType CBool)
inferExpr _ _ _ (ExistsSub _ _) = Right (InferType CBool)
inferExpr db place env (QuantCmp _ a sq q) = do
    ta <- inferExpr db place env a
    cols <- inferSubqueryOutput db env sq
    case cols of
        [(_, t)]
            | compatible ta t -> Right (InferType CBool)
            | otherwise -> Left (place ++ ": " ++ quantLabel q ++ " subquery column has type " ++ renderType t ++ ", expected " ++ renderType ta)
        _ -> Left (place ++ ": " ++ quantLabel q ++ " subquery must return exactly one column")

-- | 量词在错误文本里的名字
quantLabel :: Quantifier -> String
quantLabel AnyQ = "ANY"
quantLabel AllQ = "ALL"

-- | IN 列表里的每一项都要跟左边的类型对得上
itemType :: String -> InferredType -> InferredType -> Either String ()
itemType place want got
    | compatible want got = Right ()
    | otherwise = Left (place ++ ": IN list item has type " ++ renderType got ++ ", expected " ++ renderType want)

-- | 推导子查询的输出列与类型
inferSubqueryOutput :: Database -> Scope -> Subquery -> Either String [(String, InferredType)]
inferSubqueryOutput db outerScope sq = case subqueryStatement sq of
    q@Select { selectCols = cols } -> do
        env <- checkFrom db enclosing (selectFrom q)
        checkResolvedWith db enclosing q
        mapM (columnTypeOf env) cols
    q@SelectExpr { selectItems = items } -> do
        env <- checkFrom db enclosing (selectFrom q)
        checkResolvedWith db enclosing q
        mapM (\(l, e) -> (\t -> (l, t)) <$> inferExpr db "SELECT" (Scope env enclosing) e) items
    _ -> Left "subquery must be a SELECT"
  where
    enclosing = scopeFlat outerScope
    -- | 查这一列的推导类型
    columnTypeOf env c = do
        (_, col) <- scopeAt "SELECT" (Scope env enclosing) c
        Right (c, inferredColumnType (columnType col))

-- | 聚合的参数：列要在，且不能嵌套聚合
aggregateArg :: Database -> String -> Scope -> Expr -> Either String InferredType
aggregateArg db place env a
    | hasAggregate a = Left (place ++ ": nested aggregates are not allowed")
    | otherwise = inferExpr db place env a

-- | 数值聚合的结果类型：整数进整数出，其它往浮点靠
numericResult :: String -> String -> InferredType -> Either String InferredType
numericResult fn place t = case t of
    InferNull -> Right InferNull
    InferType x
        | numericType x -> Right (if integerType x then InferType x else InferType CFloat)
        | otherwise -> Left (place ++ ": " ++ fn ++ " needs a number, got " ++ typeLabel x)

-- | 算术结果：有小数就出小数，否则是整数
arithResult :: InferredType -> InferredType -> InferredType
arithResult InferNull _ = InferNull
arithResult _ InferNull = InferNull
arithResult (InferType a) (InferType b)
    | integerType a && integerType b = InferType CInt
    | otherwise = InferType CFloat

-- | 算术两边的类型检查
arithmetic :: Database -> String -> Scope -> Expr -> Expr -> Either String InferredType
arithmetic db place env a b = do
    ta <- inferExpr db place env a
    tb <- inferExpr db place env b
    let want = if any isFloatSide [ta, tb] then CFloat else CInt
    mapM_ (numericOperand place (InferType want) (InferType want)) [ta, tb]
    Right (arithResult ta tb)
  where
    -- | 是不是浮点类型
    isFloatSide (InferType x) = numericType x && not (integerType x)
    isFloatSide _ = False

-- | 一侧必须是数
numericOperand :: String -> InferredType -> InferredType -> InferredType -> Either String ()
numericOperand place want other t = case t of
    InferNull -> Right ()
    InferType x | numericType x -> Right ()
    _ -> Left (place ++ ": operator needs " ++ renderType want ++ " on both sides, got " ++ renderType other ++ " and " ++ renderType t)

-- | 比较两边的类型检查
comparison :: Database -> String -> Scope -> Expr -> Expr -> Either String InferredType
comparison db place env a b = do
    ta <- inferExpr db place env a
    tb <- inferExpr db place env b
    if compatible ta tb
        then Right (InferType CBool)
        else Left (place ++ ": both sides of = must have the same type, got " ++ renderType ta ++ " and " ++ renderType tb)

-- | 两类能不能比，类型族规则来自 Model
compatible :: InferredType -> InferredType -> Bool
compatible InferNull _ = True
compatible _ InferNull = True
compatible (InferType a) (InferType b) = comparableTypes a b

-- | 逻辑运算两边的类型检查
boolean :: Database -> String -> Scope -> Expr -> Expr -> Either String InferredType
boolean db place env a b = do
    ta <- inferExpr db place env a
    tb <- inferExpr db place env b
    mapM_ (boolOperand place ta) [ta, tb]
    Right (InferType CBool)
  where
    -- | 挨个查操作数是不是布尔
    boolOperand _ _ InferNull = Right ()
    boolOperand _ _ (InferType ty) | typeClassOf ty == BooleanClass = Right ()
    boolOperand p other t = Left (p ++ ": operator needs TBool on both sides, got " ++ renderType other ++ " and " ++ renderType t)

-- | 要求条件能算出布尔
checkBool :: Database -> String -> Scope -> Expr -> Either String ()
checkBool db place env e = do
    checkColumns place env (colsInExpr e)
    t <- inferExpr db place env e
    case t of
        InferNull -> Right ()
        InferType x | typeClassOf x == BooleanClass -> Right ()
        InferType x -> Left (place ++ ": condition must be a boolean, got " ++ typeLabel x)

-- | 检查赋值列和类型
checkTyped :: Database -> String -> Scope -> Table -> (String, Expr) -> Either String ()
checkTyped db place env t (c, e) = do
    want <- maybe (Left (missingColumn place c (tableCols t))) Right (colType t c)
    got <- inferExpr db place env e
    case got of
        InferNull -> Right ()
        InferType x
            | assignable (columnType want) x -> Right ()
            | otherwise -> Left (place ++ ": column " ++ c ++ " needs " ++ show want ++ ", got " ++ typeLabel x)

-- | 检查插入值的类型
checkValue :: Database -> Table -> (String, Expr) -> Either String ()
checkValue db = checkTyped db "INSERT" (Scope [] [])

-- | 检查 UPDATE 的赋值
checkAssign :: Database -> Table -> (String, Expr) -> Either String ()
checkAssign db t = checkTyped db "UPDATE" (Scope (tableCols t) []) t

-- | 按语句类型分派检查
check :: Database -> Statement -> Either String ()
check db q = () <$ prepare db q

-- | 执行前解析列名并检查类型
prepare :: Database -> Statement -> Either String Statement
prepare db q = do
    typed <- resolveDomainTypes db q
    let visible = filter (not . isDomainTable . fst) db
    resolved <- resolveStatement visible typed
    case resolved of
        CreateDomain name base -> do
            checkDomainName name
            checkTypeParameters base
            case base of
                CVarchar n | n <= 0 -> Left "CREATE DOMAIN: length must be positive"
                CChar n | n <= 0 -> Left "CREATE DOMAIN: length must be positive"
                CDecimal p s | p <= 0 || p > 308 || s < 0 || s > p -> Left "CREATE DOMAIN: invalid precision or scale"
                _ -> Right ()
            if any ((== domainTableName name) . fst) db
                then Left ("domain already exists: " ++ name)
                else Right ()
        DropDomain name -> do
            _ <- maybe (Left ("unknown domain: " ++ name)) Right (lookup (domainTableName name) db)
            if any (any (usesDomain name . columnType . snd) . tableCols . snd) db
                then Left ("domain is still used by a column: " ++ name)
                else Right ()
        ShowDomains -> Right ()
        _ -> do
            if any (isDomainTable . lastTablePart) (ddlTargets resolved)
                then Left "reserved domain catalog"
                else Right ()
            checkResolved visible resolved
    pure resolved

-- | 检查命名类型的名称
checkDomainName :: String -> Either String ()
checkDomainName name
    | null name || length name > 48 = Left "CREATE DOMAIN: name must contain 1 to 48 characters"
    | any (\c -> not (isAscii c && (isAlphaNum c || c == '_'))) name = Left "CREATE DOMAIN: invalid name"
    | Just _ <- parseColumnType name = Left "CREATE DOMAIN: built-in type name is reserved"
    | "__system_" `isPrefixOf` map toLower name = Left "CREATE DOMAIN: reserved name"
    | otherwise = Right ()

-- | 判断列类型是否引用指定命名类型
usesDomain :: String -> ColumnType -> Bool
usesDomain name (CDomain other base) = map toLower name == map toLower other || usesDomain name base
usesDomain _ _ = False

-- | 列出结构语句的目标表
ddlTargets :: Statement -> [String]
ddlTargets statement = case statement of
    CreateTable name _ -> [name]
    DropTable name -> [name]
    CreateIndex name _ -> [name]
    DropIndex name _ -> [name]
    AddColumn name _ -> [name]
    DropColumn name _ -> [name]
    RenameColumn name _ _ -> [name]
    AlterColumnType name _ _ -> [name]
    AlterColumnDefault name _ _ -> [name]
    AlterColumnNull name _ _ -> [name]
    _ -> []

-- | 从 schema 解析列上的命名类型
resolveDomainTypes :: Database -> Statement -> Either String Statement
resolveDomainTypes db statement = case statement of
    CreateDomain name base -> CreateDomain name <$> resolveDomainType base
    CreateTable name cols -> CreateTable name <$> mapM resolveColumnType cols
    AddColumn name col -> AddColumn name <$> resolveColumnType col
    AlterColumnType name col ty -> AlterColumnType name col <$> resolveDomainType ty
    _ -> Right statement
  where
    -- | 解析一列的类型
    resolveColumnType (name, col) = do
        ty <- resolveDomainType (columnType col)
        Right (name, col {columnType = ty})
    -- | 从类型目录取基础类型
    resolveDomainType = resolveWith []
    -- | 递归解析类型并拒绝循环定义
    resolveWith seen (CDomain name _)
        | map toLower name `elem` seen = Left ("cyclic domain definition: " ++ name)
        | otherwise = do
            table <- maybe (Left ("unknown domain: " ++ name)) Right (lookup (domainTableName name) db)
            base <- maybe (Left ("invalid domain catalog: " ++ name)) Right (lookup "base" (tableCols table))
            resolved <- resolveWith (map toLower name : seen) (columnType base)
            Right (CDomain (map toLower name) resolved)
    resolveWith _ ty = Right ty

-- | 检查已解析的语句
checkResolved :: Database -> Statement -> Either String ()
checkResolved db = checkResolvedWith db []

-- | 检查已解析的语句；outer 是相关子查询能看见的外层列
checkResolvedWith :: Database -> [(String, Column)] -> Statement -> Either String ()
checkResolvedWith db outer q = case q of
    Select
        { selectCols = cols
        , selectFrom = fromC
        , selectWhere = mWhere
        , selectGroupBy = groupBy
        , selectOrderBy = orderBy
        } -> do
            env <- checkFrom db outer fromC
            let full = Scope env outer
            checkColumns "SELECT" full cols
            checkGrouping full groupBy (map (\c -> (c, Col c)) cols)
            checkColumns "ORDER BY" full (map fst orderBy)
            mapM_ (checkRuntimeColumn "ORDER BY" full . fst) orderBy
            mapM_ (checkNoAggregate "WHERE") mWhere
            mapM_ (checkBool db "WHERE" full) mWhere
    SelectExpr items fromC mWhere groupBy orderBy _ -> do
        env <- checkFrom db outer fromC
        let full = Scope env outer
        mapM_ (\item -> () <$ inferExpr db "SELECT" full (snd item)) items
        checkGrouping full groupBy (items ++ [(c, Col c) | (c, _) <- orderBy, c `notElem` map fst items])
        mapM_ (checkOrder db full items . fst) orderBy
        mapM_ (checkNoAggregate "WHERE") mWhere
        mapM_ (checkBool db "WHERE" full) mWhere
    Insert tbl cols rows -> do
        t <- lookupTable db tbl
        checkColumns "INSERT" (Scope (tableCols t) []) cols
        if null rows
            then Left "INSERT: no values"
            else mapM_ (mapM_ (checkValue db t) . zip cols) rows
        checkInsertTargets t cols
    Delete tbl mWhere -> do
        t <- lookupTable db tbl
        mapM_ (checkBool db "WHERE" (Scope (tableCols t) [])) mWhere
    Update tbl assigns mWhere -> do
        t <- lookupTable db tbl
        checkColumns "UPDATE" (Scope (tableCols t) []) (map fst assigns)
        mapM_ (checkAssign db t) assigns
        mapM_ (checkBool db "WHERE" (Scope (tableCols t) [])) mWhere
    CreateTable name cols -> do
        if null name
            then Left "CREATE TABLE: empty table name"
            else do
                let names = map fst cols
                if length names /= length (nub names)
                    then Left ("CREATE TABLE: duplicate column names in " ++ name)
                    else Right ()
                checkColumnDefs (map fst cols) cols
    DropTable name ->
        if null name
            then Left "DROP TABLE: empty table name"
            else Right ()
    CreateIndex tbl col -> do
        t <- lookupTable db tbl
        if col `elem` tableCols' t
            then checkRuntimeColumn "CREATE INDEX" (Scope (tableCols t) []) col
            else Left ("CREATE INDEX: unknown column: " ++ col)
    DropIndex tbl col -> do
        t <- lookupTable db tbl
        if col `elem` tableCols' t
            then Right ()
            else Left ("DROP INDEX: unknown column: " ++ col)
    DropColumn tbl col -> do
        t <- lookupTable db tbl
        if col `notElem` tableCols' t
            then Left ("DROP COLUMN: unknown column: " ++ col)
            else
                if col == idColumn
                    then Left "DROP COLUMN: the built-in id column cannot be dropped"
                    else Right ()
    AddColumn tbl def -> do
        t <- lookupTable db tbl
        if fst def `elem` tableCols' t
            then Left ("ADD COLUMN: column already exists: " ++ fst def)
            else Right ()
        checkColumnDefs (tableCols' t ++ [fst def]) [def]
    RenameColumn tbl old new -> do
        t <- lookupTable db tbl
        if old `notElem` tableCols' t
            then Left ("RENAME COLUMN: unknown column: " ++ old)
            else Right ()
        if old == idColumn
            then Left "RENAME COLUMN: the built-in id column cannot be renamed"
            else
                if new `elem` tableCols' t
                    then Left ("RENAME COLUMN: column already exists: " ++ new)
                    else Right ()
    AlterColumnType tbl col ty -> do
        t <- lookupTable db tbl
        c <- requireColumn "ALTER COLUMN" t col
        checkTypeParameters ty
        if columnAutoIncrement c && not (integerType ty)
            then Left "ALTER COLUMN: AUTO_INCREMENT column must stay an integer type"
            else Right ()
    AlterColumnDefault tbl col def -> do
        t <- lookupTable db tbl
        c <- requireColumn "ALTER COLUMN" t col
        mapM_ (\v -> checkDefault c v) def
    AlterColumnNull tbl col nullable -> do
        t <- lookupTable db tbl
        c <- requireColumn "ALTER COLUMN" t col
        if not nullable && columnAutoIncrement c
            then Left "ALTER COLUMN: AUTO_INCREMENT column cannot be nullable"
            else Right ()
    CreateUser name password -> checkAccount name password
    AlterUser name password -> checkAccount name password
    AlterIdentity name attributes -> do
        checkRoleName name
        if length (nub (map fst attributes)) == length attributes
            then Right ()
            else Left "duplicate identity attribute"
    ShowRoles -> Right ()
    DropUser name -> checkUserName name
    CreateRole name -> checkRoleName name
    DropRole name -> checkRoleName name
    GrantPrivileges privs obj role _ -> do
        mapM_ checkPrivilege privs
        checkGrantObject obj
        checkRoleName role
    RevokePrivileges privs obj role -> do
        mapM_ checkPrivilege privs
        checkGrantObject obj
        checkRoleName role
    GrantRole role members -> do
        checkRoleName role
        mapM_ checkUserName members
    RevokeRole role members -> do
        checkRoleName role
        mapM_ checkUserName members
    CreateDatabase name -> checkDatabaseName name
    DropDatabase name -> checkDatabaseName name
    UseDatabase name -> checkDatabaseName name
    ShowDatabases -> Right ()
    CreateDomain{} -> Right ()
    DropDomain{} -> Right ()
    ShowDomains -> Right ()
    -- | 事务控制由会话层执行，语义层不做列检查
    BeginTransaction -> Right ()
    CommitTransaction -> Right ()
    RollbackTransaction -> Right ()
    -- | 保存点名字只是会话层的记号，不做列检查
    Savepoint _ -> Right ()
    RollbackToSavepoint _ -> Right ()
    ReleaseSavepoint _ -> Right ()
  where
    tableCols' = map fst . tableCols

-- | 查一列，没有就报错
requireColumn :: String -> Table -> String -> Either String Column
requireColumn place t c = maybe (Left (place ++ ": unknown column: " ++ c)) Right (colType t c)

-- | 插入时没提到的列能不能自己补上
checkInsertTargets :: Table -> [String] -> Either String ()
checkInsertTargets t cols = mapM_ checkTarget (tableCols t)
  where
    checkTarget (name, c)
        | name `elem` cols = Right ()
        | columnAutoIncrement c = Right ()
        | columnNullable c = Right ()
        | Just _ <- columnDefault c = Right ()
        | otherwise = Left ("INSERT: column " ++ name ++ " is NOT NULL and has no default")

-- | 检查一列定义是否合法（names 给 CHECK 用）
checkColumnDefs :: [String] -> [(String, Column)] -> Either String ()
checkColumnDefs names cols = do
    mapM_ (checkTypeParameters . columnType . snd) cols
    mapM_ (\(_, c) -> mapM_ (checkDefault c) (columnDefault c)) cols
    mapM_ (\(_, c) -> checkAutoIncrement c) cols
    mapM_ checkRuntimeKey cols
    if length [() | (_, c) <- cols, columnAutoIncrement c] > 1
        then Left "CREATE TABLE: at most one AUTO_INCREMENT column is allowed"
        else Right ()
    checkChecks names cols
  where
    -- | 拒绝缺少能力实现的运行时键
    checkRuntimeKey (_, column)
        | typeClassOf (columnType column) == RuntimeClass
        , columnUnique column || columnPrimaryKey column = Left "runtime type key capability is not registered"
        | otherwise = Right ()

-- | 校验列类型的长度和精度参数
checkTypeParameters :: ColumnType -> Either String ()
checkTypeParameters (CDomain _ base) = checkTypeParameters base
checkTypeParameters (CRuntime tid) = () <$ describeType builtinTypes tid
checkTypeParameters CNull = Left "NULL is only valid as an inferred query output"
checkTypeParameters (CDecimal p s)
    | p <= 0 || p > 308 || s < 0 || s > p = Left "invalid decimal precision or scale"
checkTypeParameters (CVarchar n) | n <= 0 = Left "character length must be positive"
checkTypeParameters (CChar n) | n <= 0 = Left "character length must be positive"
checkTypeParameters _ = Right ()

-- | 自增列必须是整数列
checkAutoIncrement :: Column -> Either String ()
checkAutoIncrement c
    | columnAutoIncrement c && not (integerType (columnType c)) = Left "AUTO_INCREMENT: column must be an integer type"
    | otherwise = Right ()

-- | 默认值必须能放进这一列
checkDefault :: Column -> Value -> Either String ()
checkDefault c v
    | VNull <- v = Right ()
    | valueFits (columnType c) v = () <$ coerceValue (columnType c) v
    | otherwise = Left ("DEFAULT: value does not fit column type " ++ typeLabel (columnType c))

-- | CHECK 里的列必须存在
checkChecks :: [String] -> [(String, Column)] -> Either String ()
checkChecks names = mapM_ checkOne
  where
    -- | 查这一列的 CHECK
    checkOne (_, c) = case columnCheck c of
        Nothing -> Right ()
        Just text -> case parseExpression text of
            Left err -> Left ("CHECK: " ++ firstLine err)
            Right e -> mapM_ (known e) (colsInExpr e)
    -- | 这个列名在不在定义里
    known _ col
        | col `elem` names = Right ()
        | otherwise = Left ("CHECK: unknown column: " ++ col)
    firstLine = takeWhile (/= '\n')

-- | 账号名上限
accountNameLimit :: Int
accountNameLimit = 64

-- | 检查库名是否合法
checkDatabaseName :: String -> Either String ()
checkDatabaseName name
    | null name || length name > 64 || not (all (\c -> isAscii c && (isAlphaNum c || c == '_')) name) = Left "invalid database name"
    | otherwise = Right ()

-- | 口令上限
passwordLimit :: Int
passwordLimit = 256

-- | 账号名：ASCII 字母数字加点、短横线、下划线
checkUserName :: String -> Either String ()
checkUserName name
    | null name = Left "USER: empty account name"
    | length name > accountNameLimit = Left "USER: account name is too long"
    | not (all allowed name) = Left "USER: account name may only contain ASCII letters, digits, dot, dash or underscore"
    | otherwise = Right ()
  where
    -- | 账号名字符规则
    allowed c = isAscii c && (isAlphaNum c || c `elem` ("_.-" :: String))

-- | 建号 / 改口令：名字合法且口令不空
checkAccount :: String -> String -> Either String ()
checkAccount name password = do
    checkUserName name
    if null password
        then Left "USER: password must not be empty"
        else
            if length password > passwordLimit
                then Left "USER: password is too long"
                else Right ()

-- | 检查角色名是否合法（不许用权限关键字）
checkRoleName :: String -> Either String ()
checkRoleName name
    | null name = Left "ROLE: empty role name"
    | length name > accountNameLimit = Left "ROLE: role name is too long"
    | not (all allowed name) = Left "ROLE: role name may only contain ASCII letters, digits, dot, dash or underscore"
    | map toLower name `elem` privilegeNames = Left "ROLE: role name must not be a privilege keyword"
    | otherwise = Right ()
  where
    -- | 角色名字符规则
    allowed c = isAscii c && (isAlphaNum c || c `elem` ("_.-" :: String))

-- | 权限关键字（小写，只用于比较）
privilegeNames :: [String]
privilegeNames = ["select", "insert", "update", "delete", "all"]

-- | 权限名只能是这几个
checkPrivilege :: String -> Either String ()
checkPrivilege name
    | map toLower name `elem` privilegeNames = Right ()
    | otherwise = Left ("GRANT: unknown privilege: " ++ name)

-- | 授权对象：* 表示全部表，否则是一张表（可带一个库前缀）
checkGrantObject :: String -> Either String ()
checkGrantObject "*" = Right ()
checkGrantObject obj
    | null obj = Left "GRANT: object name must not be empty"
    | length obj > 128 = Left "GRANT: object name is too long"
    | not (all allowed obj) = Left "GRANT: object name may only contain ASCII letters, digits, dot, dash or underscore"
    | otherwise = Right ()
  where
    -- | 授权对象字符规则
    allowed c = isAscii c && (isAlphaNum c || c `elem` ("_.-" :: String))

-- | 内置 id 列名
idColumn :: String
idColumn = "id"
-- | 把列引用统一为执行期名字（子查询一起解析）
resolveExpr ::
    Database ->
    [(String, Column)] ->
    [(String, Column)] ->
    (String -> Either String String) ->
    Expr ->
    Either String Expr
resolveExpr db env outer f e = case e of
    Construct ty arguments -> do
        tid <- inferConstruction db "constructor" (Scope env outer) ty arguments
        bound <- mapM (resolveExpr db env outer f) arguments
        Right (BoundConstruct tid bound)
    BoundConstruct tid arguments -> BoundConstruct tid <$> mapM (resolveExpr db env outer f) arguments
    FunctionCall name arguments -> do
        inferred <- mapM (inferExpr db "function" (Scope env outer)) arguments
        sig <- resolveSQLFunction name (map inferredArgument inferred)
        bound <- mapM (resolveExpr db env outer f) arguments
        Right (BoundFunction (signatureId sig) (signatureResult sig) bound)
    BoundFunction fid tid arguments -> BoundFunction fid tid <$> mapM (resolveExpr db env outer f) arguments
    Col c -> Col <$> f c
    Add a b -> binary Add a b
    Sub a b -> binary Sub a b
    Mul a b -> binary Mul a b
    Div a b -> binary Div a b
    Neg a -> Neg <$> resolveExpr db env outer f a
    Gt a b -> binary Gt a b
    Lt a b -> binary Lt a b
    Eq a b -> binary Eq a b
    GtE a b -> binary GtE a b
    LtE a b -> binary LtE a b
    NotEq a b -> binary NotEq a b
    And a b -> binary And a b
    Or a b -> binary Or a b
    IsNull a -> IsNull <$> resolveExpr db env outer f a
    IsNotNull a -> IsNotNull <$> resolveExpr db env outer f a
    CountAll -> Right CountAll
    CountOf a -> CountOf <$> resolveExpr db env outer f a
    SumOf a -> SumOf <$> resolveExpr db env outer f a
    AvgOf a -> AvgOf <$> resolveExpr db env outer f a
    MinOf a -> MinOf <$> resolveExpr db env outer f a
    MaxOf a -> MaxOf <$> resolveExpr db env outer f a
    ScalarSub sq -> ScalarSub <$> resolveSubquery db (env ++ outer) sq
    InSub a sq negated -> InSub <$> resolveExpr db env outer f a <*> resolveSubquery db (env ++ outer) sq <*> pure negated
    InList a es negated -> InList <$> resolveExpr db env outer f a <*> mapM (resolveExpr db env outer f) es <*> pure negated
    ExistsSub sq negated -> ExistsSub <$> resolveSubquery db (env ++ outer) sq <*> pure negated
    QuantCmp op a sq q -> QuantCmp op <$> resolveExpr db env outer f a <*> resolveSubquery db (env ++ outer) sq <*> pure q
    _ -> Right e
  where
    -- | 递归处理两个子表达式
    binary ctor a b = ctor <$> resolveExpr db env outer f a <*> resolveExpr db env outer f b

-- | 校验具体构造器和参数逻辑类型
inferConstruction :: Database -> String -> Scope -> TypeExpr -> [Expr] -> Either String TypeId
inferConstruction db place env ty@(TypeApply cid parameters) arguments = do
    descriptor <- resolveType builtinTypes ty
    expected <- case (cid, parameters) of
        (TypeConstructorId 5, [child]) -> Right (replicate (length arguments) child)
        (TypeConstructorId 6, [child]) | length arguments <= 1 -> Right (replicate (length arguments) child)
        (TypeConstructorId 7, fields) | length fields == length arguments -> Right fields
        _ -> Left "composite constructor argument count mismatch"
    inferred <- mapM (inferExpr db place env) arguments
    supplied <- mapM argumentIdentity inferred
    required <- mapM (fmap descriptorId . resolveType builtinTypes) expected
    if supplied == required then Right (descriptorId descriptor) else Left "composite constructor argument type mismatch"
  where
    -- | 解析非空参数的逻辑类型身份
    argumentIdentity InferNull = Left "SQL NULL needs Maybe construction inside a runtime value"
    argumentIdentity (InferType column) = sqlType column

-- | 使用单行 schema 绑定表达式
prepareRowExpression :: [(String, Column)] -> Expr -> Either String Expr
prepareRowExpression columns expression = do
    bound <- resolveExpr [] columns [] (nameIn columns "expression") expression
    _ <- inferExpr [] "expression" (Scope columns []) bound
    Right bound

-- | 将推导类型转换为可空参数类型
inferredArgument :: InferredType -> Maybe ColumnType
inferredArgument InferNull = Nothing
inferredArgument (InferType ty) = Just ty

-- | 解析子查询，并记下它引用的外层列
resolveSubquery :: Database -> [(String, Column)] -> Subquery -> Either String Subquery
resolveSubquery db outer sq = do
    stmt <- resolveSubqueryStatement db outer (subqueryStatement sq)
    pure (Subquery stmt (outerRefsOf db stmt))

-- | 子查询里指向外层作用域的列（内层查不到的列就是外层引用）
outerRefsOf :: Database -> Statement -> [String]
outerRefsOf db stmt = nub [c | c <- statementColumns stmt, not (inInner c)]
  where
    inner = case checkFrom db [] (statementFrom stmt) of
        Right env -> env
        Left _ -> []
    -- | 这一列能不能在内层解析到
    inInner c = case resolveColumn c inner of
        Right _ -> True
        Left _ -> False

-- | 一条语句里出现的所有列引用
statementColumns :: Statement -> [String]
statementColumns stmt = case stmt of
    Select cols fromC mWhere groupBy orderBy _ ->
        cols ++ fromColumns fromC ++ concatMap colsInExpr mWhere ++ groupBy ++ map fst orderBy
    SelectExpr items fromC mWhere groupBy orderBy _ ->
        concatMap (colsInExpr . snd) items ++ fromColumns fromC ++ concatMap colsInExpr mWhere ++ groupBy ++ map fst orderBy
    _ -> []
  where
    -- | 取 FROM 里出现的列
    fromColumns FromUnit = []
    fromColumns (FromTable _ _) = []
    fromColumns (FromSubquery _ _) = []
    fromColumns (FromJoin _ l r c) = fromColumns l ++ fromColumns r ++ colsInExpr c

-- | 取语句的 FROM 子句，没有就是 FromUnit
statementFrom :: Statement -> FromClause
statementFrom Select { selectFrom = fromC } = fromC
statementFrom SelectExpr { selectFrom = fromC } = fromC
statementFrom _ = FromUnit

-- | 解析子查询里的 SELECT
resolveSubqueryStatement :: Database -> [(String, Column)] -> Statement -> Either String Statement
resolveSubqueryStatement db outer q@Select{} = resolveQuery db outer q (map (\c -> (c, Col c)) (selectCols q))
resolveSubqueryStatement db outer q@SelectExpr{} = resolveQuery db outer q (selectItems q)
resolveSubqueryStatement _ _ _ = Left "subquery must be a SELECT"

-- | 列名解析入口：查询走连接解析，改数据的语句按本表列解析
resolveStatement :: Database -> Statement -> Either String Statement
resolveStatement db q = case q of
    Select{} -> resolveSelect db q
    SelectExpr{} -> resolveSelect db q
    Update tbl assigns mWhere -> do
        (key, t) <- resolveTable db tbl
        let env = tableCols t
            name = nameIn env "UPDATE"
        assigns' <- mapM (\(c, e) -> (\e' -> (c, e')) <$> resolveExpr db env [] name e) assigns
        w <- mapM (resolveExpr db env [] (nameIn env "WHERE")) mWhere
        pure (Update key assigns' w)
    Delete tbl mWhere -> do
        (key, t) <- resolveTable db tbl
        w <- mapM (resolveExpr db (tableCols t) [] (nameIn (tableCols t) "WHERE")) mWhere
        pure (Delete key w)
    Insert tbl cols rows -> do
        (key, t) <- resolveTable db tbl
        let env = tableCols t
        rows' <- mapM (mapM (resolveExpr db env [] (nameIn env "INSERT"))) rows
        pure (Insert key cols rows')
    other -> Right other

-- | 查一列的物理名
nameIn :: [(String, Column)] -> String -> String -> Either String String
nameIn env place c = fst <$> columnAt place env c

-- | 内层先查，查不到再落到外层；内层同名引起的歧义不往外层让
scopedColumnAt :: String -> [(String, Column)] -> [(String, Column)] -> String -> Either String (String, Column)
scopedColumnAt place inner outer c = scopeAt place (Scope inner outer) c

-- | 单表保留原表头，连接使用唯一前缀
resolveSelect :: Database -> Statement -> Either String Statement
resolveSelect db q@Select{} = resolveQuery db [] q (map (\c -> (c, Col c)) (selectCols q))
resolveSelect db q@SelectExpr{} = resolveQuery db [] q (selectItems q)
resolveSelect _ q = Right q

-- | 检查排序输出别名或来源列
checkOrder :: Database -> Scope -> [(String, Expr)] -> String -> Either String ()
checkOrder db env items column = case [e | (label, e) <- items, label == column] of
    [] -> checkRuntimeColumn "ORDER BY" env column
    [expression] -> () <$ (inferExpr db "ORDER BY" env expression >>= requireOrdering "ORDER BY")
    _ -> Left ("ORDER BY: ambiguous projection alias: " ++ column)

-- | 校验列已有排序能力
checkRuntimeColumn :: String -> Scope -> String -> Either String ()
checkRuntimeColumn place env column = do
    (_, definition) <- scopeAt place env column
    () <$ requireOrdering place (InferType (columnType definition))

-- | 拒绝尚未注册能力的复合类型
requireOrdering :: String -> InferredType -> Either String InferredType
requireOrdering place inferred@(InferType ty)
    | typeClassOf ty == RuntimeClass = Left (place ++ ": runtime type capability is not registered")
    | otherwise = Right inferred
requireOrdering _ inferred = Right inferred

-- | 解析投影、条件、排序与连接引用
resolveQuery :: Database -> [(String, Column)] -> Statement -> [(String, Expr)] -> Either String Statement
resolveQuery db outer q items = do
    source <- resolveSource db outer (selectFrom q)
    env <- checkFrom db outer source
    -- | 单表无别名时把裸列名换成物理列名；带别名时表头保留别名限定名
    let physical c = case selectFrom q of
            FromTable Nothing tbl -> unqualify (Just (deriveQualifier Nothing tbl)) c
            _ -> c
        -- | 查一列并换算成物理名
        name place c
            | length (filter (== '.') c) == 2, not (qualifiedReference c) = Left (missingColumn place c env)
            | otherwise = physical . fst <$> scopedColumnAt place env outer c
        -- | 库限定列只引用来源中的真实表名
        qualifiedReference c = qualifiedReferenceIn db (selectFrom q) c
            || (any (\(key, _) -> length (filter (== '.') key) == 1 && ("." ++ key) `isSuffixOf` c) outer
                && (not (any (elem '.' . fst) db) || any (\(table, _) -> (table ++ ".") `isPrefixOf` c) db))
        -- | 换算一个投影项：带别名时保留别名做输出列名
        output (label, Col c) = do
            k <- name "SELECT" c
            pure (if label == c then (k, Col k) else (label, Col k))
        output (label, e) = do
            e' <- resolveExpr db env outer (name "SELECT") e
            pure (label, e')
    projected <- if items == [("*", Col "*")]
        then if null env && selectFrom q == FromUnit
            then Left "SELECT: * requires FROM"
            else Right [(physical c, Col (physical c)) | (c, _) <- env]
        else mapM output items
    condition <- mapM (resolveExpr db env outer (name "WHERE")) (selectWhere q)
    grouping <- mapM (name "GROUP BY") (selectGroupBy q)
    -- | 优先解析唯一投影别名，再查来源列
    let orderName c = case [label | (label, _) <- projected, label == c, qIsExpression] of
            [] -> name "ORDER BY" c
            [label] -> Right label
            _ -> Left ("ORDER BY: ambiguous projection alias: " ++ c)
        qIsExpression = case q of SelectExpr{} -> True; _ -> False
    ordering <- mapM (\(c, d) -> (\k -> (k, d)) <$> orderName c) (selectOrderBy q)
    pure $ case q of
        Select{} -> Select (map fst projected) source condition grouping ordering (selectLimit q)
        _ -> SelectExpr projected source condition grouping ordering (selectLimit q)

-- | 收集没有别名遮蔽的来源表名
sourceTableNames :: FromClause -> [String]
sourceTableNames (FromTable Nothing table) = [table]
sourceTableNames (FromJoin _ left right _) = sourceTableNames left ++ sourceTableNames right
sourceTableNames _ = []

-- | 库限定引用必须使用当前来源的表名
qualifiedReferenceIn :: Database -> FromClause -> String -> Bool
qualifiedReferenceIn db source c = any (\table -> (table ++ ".") `isPrefixOf` c) tables
  where
    tables = sourceTableNames source ++ [key | table <- sourceTableNames source, Right (key, _) <- [resolveTable db table]]

-- | 单表不加限定名，其余来源交给 resolveFrom
resolveSource :: Database -> [(String, Column)] -> FromClause -> Either String FromClause
resolveSource db outer fromC = case fromC of
    FromTable mAlias tbl -> do
        (key, _) <- resolveTable db tbl
        Right (FromTable mAlias key)
    other -> resolveFrom db outer other

-- | 连接两侧与派生表都要先解析，再取环境解析 ON
resolveFrom :: Database -> [(String, Column)] -> FromClause -> Either String FromClause
resolveFrom _ _ FromUnit = Right FromUnit
resolveFrom db _ (FromTable alias tbl) = do
    (key, _) <- resolveTable db tbl
    Right (FromTable (Just (deriveQualifier alias key)) key)
resolveFrom db _ (FromSubquery mAlias stmt) = do
    inner <- resolveSubqueryStatement db [] stmt
    named <- nameDerivedColumns inner
    Right (FromSubquery mAlias named)
resolveFrom db outer (FromJoin kind left right cond) = do
    l <- resolveFrom db outer left
    r <- resolveFrom db outer right
    env <- checkFrom db outer (FromJoin kind l r cond)
    -- | 连接条件也拒绝不存在的库前缀
    let joinName column
            | length (filter (== '.') column) == 2, not (qualifiedReferenceIn db (FromJoin kind left right cond) column) = Left (missingColumn "ON" column env)
            | otherwise = fst <$> columnAt "ON" env column
    c <- resolveExpr db env outer joinName cond
    pure (FromJoin kind l r c)

-- | 派生表的输出列名：列取裸名，表达式保留原标签
nameDerivedColumns :: Statement -> Either String Statement
nameDerivedColumns q@Select { selectCols = cols, selectFrom = fromC } = do
    let names = map lastTablePart cols
    checkDerivedNames names
    Right
        ( SelectExpr
            [(n, Col c) | (n, c) <- zip names cols]
            fromC
            (selectWhere q)
            (selectGroupBy q)
            (selectOrderBy q)
            (selectLimit q)
        )
nameDerivedColumns q@SelectExpr { selectItems = items } = do
    let names = map derivedItemName items
    checkDerivedNames names
    Right q { selectItems = zip names (map snd items) }
nameDerivedColumns _ = Left "FROM: subquery must be a SELECT"

-- | 派生表里一个投影项的输出名：没写别名的列取裸名
derivedItemName :: (String, Expr) -> String
derivedItemName (label, Col c)
    | label == c = lastTablePart c
    | otherwise = label
derivedItemName (label, _) = label

-- | 派生表列名不能重复
checkDerivedNames :: [String] -> Either String ()
checkDerivedNames = go []
  where
    -- | 逐个查有没有见过
    go _ [] = Right ()
    go seen (n : ns)
        | n `elem` seen = Left ("FROM: duplicate column name in subquery: " ++ n)
        | otherwise = go (n : seen) ns
