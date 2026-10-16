module ChuSQL.Core.Engine.Domain (
    domainColumns,
    domainDefinition,
    canonicalDomain,
    effectiveColumns,
    domainCheckPlan,
    runDomainChecks,
    validateDomainDefaults,
    prepareDomainAlter,
) where

import ChuSQL.Core.Engine.Algebra.Expr (evalExpr)
import ChuSQL.Core.Engine.Runtime.Relation (orderRow)
import ChuSQL.Core.Engine.Semantic (prepareDomainCheckExpression)
import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Engine.Syntax.Parser (parseExpression)
import ChuSQL.Core.Model
import Control.Applicative ((<|>))
import Control.Monad (when)
import Data.Char (isAlphaNum, isAscii, toLower)
import Data.List (isPrefixOf, nub)
import Data.Maybe (fromMaybe)

-- 领域元数据、约束继承、默认值及结构变更的纯校验。

-- | 将领域定义编码为目录列
domainColumns :: DomainDefinition -> [(String, Column)]
domainColumns (DomainDefinition base checks) =
    ("base", base{columnCheck = Nothing})
        : [("check_" ++ name, (plainColumn CBool){columnCheck = Just text}) | (name, text) <- checks]

-- | 从新旧目录恢复领域定义
domainDefinition :: Table -> Either String DomainDefinition
domainDefinition table = do
    base <- maybe (Left "invalid domain catalog: missing base") Right (lookup "base" (tableCols table))
    checks <- mapM readCheck [(name, col) | (name, col) <- tableCols table, name /= "base"]
    let legacy = maybe [] (\text -> [("domain_check_0", text)]) (columnCheck base)
    Right (DomainDefinition base{columnCheck = Nothing} (legacy ++ checks))
  where
    -- \| 恢复一条具名检查条件
    readCheck (name, col)
        | "check_" `isPrefixOf` name, Just text <- columnCheck col = Right (drop 6 name, text)
        | otherwise = Left "invalid domain constraint catalog"

-- | 为匿名约束命名并验证完整定义
canonicalDomain :: Database -> String -> DomainDefinition -> Either String DomainDefinition
canonicalDomain db name definition = do
    normalized <- normalize definition
    let candidate = install name normalized db
    validateOwn candidate name
    Right normalized
  where
    -- \| 规范化约束名称并拒绝重复声明
    normalize (DomainDefinition base checks) = do
        when
            (columnAutoIncrement base || columnUnique base || columnPrimaryKey base)
            (Left "DOMAIN does not support row-set constraints")
        let named = [map toLower key | (key, _) <- checks, not (null key)]
        when (length named /= length (nub named)) (Left "duplicate domain constraint name")
        mapM_ validName named
        assigned <- assign named 1 checks
        Right (DomainDefinition base assigned)
    -- \| 验证约束标识符
    validName key
        | null key || length key > 48 || not (all (\c -> isAscii c && (isAlphaNum c || c == '_')) key) =
            Left "invalid domain constraint name"
        | otherwise = Right ()
    -- \| 为未命名约束分配稳定名称
    assign _ _ [] = Right []
    assign used index ((key, text) : rest)
        | null key =
            let (chosen, next) = fresh used index
             in ((chosen, text) :) <$> assign (chosen : used) next rest
        | otherwise = ((map toLower key, text) :) <$> assign used index rest
    -- \| 跳过已有的自动约束名称
    fresh used index
        | chosen `elem` used = fresh used (index + 1)
        | otherwise = (chosen, index + 1)
      where
        chosen = "domain_check_" ++ show (index :: Int)

-- | 在纯目录中安装候选领域定义
install :: String -> DomainDefinition -> Database -> Database
install name definition db = (key, table{tableCols = domainColumns definition}) : filter ((/= key) . fst) db
  where
    key = domainTableName name
    table = fromMaybe (Table key [] [] Nothing Nothing Nothing) (lookup key db)

-- | 沿目录解析领域链并拒绝循环
chain :: Database -> ColumnType -> Either String [(String, DomainDefinition)]
chain db = go []
  where
    -- \| 逐层读取当前定义而非缓存属性
    go seen (CDomain name _)
        | name `elem` seen = Left ("cyclic domain definition: " ++ name)
        | otherwise = do
            table <- maybe (Left ("unknown domain: " ++ name)) Right (lookup (domainTableName name) db)
            definition@(DomainDefinition base _) <- domainDefinition table
            rest <- go (name : seen) (columnType base)
            Right ((name, definition) : rest)
    go _ _ = Right []

-- | 合并列声明与继承的非空和默认值
effectiveColumns :: Database -> [(String, Column)] -> Either String [(String, Column)]
effectiveColumns db = mapM effective
  where
    -- \| 保留最近默认值并收紧可空属性
    effective (name, column) = do
        definitions <- chain db (columnType column)
        let bases = [base | (_, DomainDefinition base _) <- definitions]
        Right
            ( name
            , column
                { columnNullable = columnNullable column && all columnNullable bases
                , columnDefault = foldl (<|>) (columnDefault column) (map columnDefault bases)
                }
            )

-- | 绑定领域 VALUE 到独立的单值布局
domainCheckPlan :: Database -> [(String, Column)] -> Either String [(String, Int, Expr)]
domainCheckPlan db columns = concat <$> mapM checks (zip [0 ..] columns)
  where
    -- \| 按基础领域到外层领域绑定条件
    checks (slot, (_, column)) = do
        definitions <- chain db (columnType column)
        mapM
            (bind slot column)
            [ (name ++ "." ++ key, text)
            | (name, DomainDefinition _ constraints) <- reverse definitions
            , (key, text) <- constraints
            ]
    -- \| 拒绝聚合及子查询并固定类型槽位
    bind slot column (name, text) = do
        expression <- parseExpression text
        bound <- prepareDomainCheckExpression (columnType column) expression
        Right (name, slot, bound)

-- | SQL CHECK 的 UNKNOWN 与 TRUE 都通过
runDomainChecks :: [(String, Int, Expr)] -> Row -> Either String ()
runDomainChecks plan row = mapM_ check plan
  where
    -- \| 在单值作用域执行一条领域约束
    check (name, slot, expression) = case drop slot row of
        (_, value) : _ -> do
            result <- evalExpr expression [("value", value)]
            case result of
                VBool True -> Right ()
                VNull -> Right ()
                VBool False -> Left ("CHECK on DOMAIN " ++ name ++ " failed")
                _ -> Left ("CHECK on DOMAIN " ++ name ++ " is not boolean")
        [] -> Left "missing bound domain value"

-- | 检查有效默认值是否满足领域约束
validateDomainDefaults :: Database -> [(String, Column)] -> Either String ()
validateDomainDefaults db columns = do
    effective <- effectiveColumns db columns
    mapM_ validate [entry | entry@(_, column) <- effective, CDomain{} <- [columnType column]]
  where
    -- \| 默认值不改变已有行但必须可赋值
    validate entry@(name, column) = case columnDefault column of
        Nothing -> Right ()
        Just value -> do
            coerced <- coerceValue (columnType column) value
            checkNull name column coerced
            plan <- domainCheckPlan db [entry]
            runDomainChecks plan [(name, coerced)]

-- | 拒绝领域或列要求非空时的 NULL
checkNull :: String -> Column -> Value -> Either String ()
checkNull name column value
    | value == VNull && not (columnNullable column) = Left ("DOMAIN NOT NULL: " ++ name)
    | otherwise = Right ()

-- | 检查领域本身的定义及继承默认值
validateOwn :: Database -> String -> Either String ()
validateOwn db name = do
    table <- maybe (Left ("unknown domain: " ++ name)) Right (lookup (domainTableName name) db)
    DomainDefinition base _ <- domainDefinition table
    let columns = [("value", plainColumn (CDomain name (columnType base)))]
    _ <- domainCheckPlan db columns
    validateDomainDefaults db columns

-- | 在完整快照上校验并生成领域替换定义
prepareDomainAlter :: String -> DomainAlter -> Database -> Either String [(String, Column)]
prepareDomainAlter rawName action db = do
    table <- maybe (Left ("unknown domain: " ++ name)) Right (lookup (domainTableName name) db)
    definition <- domainDefinition table >>= change
    normalized <- canonicalDomain db name definition
    let candidate = install name normalized db
    mapM_ (validateDependent candidate) [key | (key, _) <- candidate, isDomainTable key]
    mapM_ (validateTable candidate) [(key, value) | (key, value) <- candidate, not (isDomainTable key)]
    Right (domainColumns normalized)
  where
    name = map toLower rawName
    -- \| 修改单个属性且保留其他约束
    change (DomainDefinition base checks) = case action of
        DomainDefault value -> Right (DomainDefinition base{columnDefault = value} checks)
        DomainNullable nullable -> Right (DomainDefinition base{columnNullable = nullable} checks)
        DomainAddCheck key text -> Right (DomainDefinition base (checks ++ [(fromMaybe "" key, text)]))
        DomainDropCheck key
            | map toLower key `elem` map fst checks -> Right (DomainDefinition base (filter ((/= map toLower key) . fst) checks))
            | otherwise -> Left ("unknown domain constraint: " ++ key)
    -- \| 判断传递引用是否包含目标领域
    affected candidate ty = any ((== name) . fst) <$> chain candidate ty
    -- \| 校验依赖领域的继承默认值
    validateDependent candidate key = do
        let domain = drop (length (domainTableName "")) key
        applies <- affected candidate (CDomain domain CStr)
        when applies (validateOwn candidate domain)
    -- \| 只校验受领域变化影响的列与数据
    validateTable candidate (_, table) = do
        selected <-
            mapM
                ( \entry@(_, column) -> do
                    applies <- affected candidate (columnType column)
                    Right [entry | applies]
                )
                (tableCols table)
        let columns = concat selected
        effective <- effectiveColumns candidate columns
        validateDomainDefaults candidate columns
        plan <- domainCheckPlan candidate columns
        mapM_ (validateRow effective plan) (tableRows table)
    -- \| 按列布局检查非空与全部继承条件
    validateRow columns plan row = do
        selected <- orderRow (map fst columns) row
        values <-
            mapM
                ( \((key, column), (_, value)) -> do
                    coerced <- coerceValue (columnType column) value
                    checkNull key column coerced
                    Right (key, coerced)
                )
                (zip columns selected)
        runDomainChecks plan values
