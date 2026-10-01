module ChuSQL.Algebra.Eval (evalRelOp, evalRelOpM, evalExprM, evalCondForRowM) where

import ChuSQL.Algebra.Expr (evalCondForRow, evalExpr)
import ChuSQL.Algebra.Optimize (optimize, relOpCols)
import ChuSQL.Algebra.Op
import ChuSQL.Algebra.Planner (translate)
import ChuSQL.Algebra.Sort (compareValue, sortRows)
import ChuSQL.Model
import ChuSQL.Storage (IndexResult (..), MonadStorage (..))
import ChuSQL.Storage.Memory (runMemoryStorage)
import ChuSQL.Syntax.AST (Expr (..), JoinKind (..), Subquery (..))
import Control.Monad (filterM, foldM)
import qualified Data.HashMap.Strict as HM

-- 执行：按算子树算出结果行（Scan 阶段会加别名前缀）。
-- 外层行 outer 给相关子查询用：子查询里的列先在本层行里找，找不到再落到外层行。

-- | 只保留清单里的列，顺序按清单（SELECT 的列序就是结果的列序）
project :: [String] -> Row -> Row
project ["*"] row = row
project cs row = [(c, v) | c <- cs, Just v <- [lookup c row]]

-- | 给列名加别名前缀
addPrefix :: Maybe String -> Row -> Row
addPrefix mAlias = map (\(k, v) -> (qualify mAlias k, v))

prefixRows :: Maybe String -> [Row] -> [Row]
prefixRows mAlias = map (addPrefix mAlias)

-- | 值 → 字面量
valueLiteral :: Value -> Expr
valueLiteral (VInt n) = LitInt n
valueLiteral (VFloat d) = LitFloat d
valueLiteral (VStr s) = LitStr s
valueLiteral (VBool b) = LitBool b
valueLiteral VNull = LitNull

-- | 点查退化成扫描时用的条件
lookupCondition :: Maybe String -> String -> Value -> Expr
lookupCondition mAlias col k = Eq (Col (qualify mAlias col)) (valueLiteral k)

-- | 范围扫描退化成扫描时用的条件（闭端写成 > 或 = 的并）
rangeCondition :: Maybe String -> String -> Maybe (Value, Bool) -> Maybe (Value, Bool) -> Expr
rangeCondition mAlias col lo hi = case (lo, hi) of
    (Just l, Just h) -> And (bound Gt l) (bound Lt h)
    (Just l, Nothing) -> bound Gt l
    (Nothing, Just h) -> bound Lt h
    (Nothing, Nothing) -> LitBool True
  where
    c = Col (qualify mAlias col)
    bound rel (v, inclusive)
        | inclusive = Or (rel c (valueLiteral v)) (Eq c (valueLiteral v))
        | otherwise = rel c (valueLiteral v)

-- | 一批行的列名（空批给空清单）
rowsCols :: [Row] -> [String]
rowsCols (r : _) = map fst r
rowsCols [] = []

-- | 参与等值匹配的键：NULL 不参与匹配，整数值的浮点归一到整数
-- （嵌套循环走 compareValues，哈希要对齐它）
joinKey :: Value -> Maybe Value
joinKey VNull = Nothing
joinKey (VFloat d)
    | not (isNaN d)
    , not (isInfinite d)
    , fromIntegral (rounded :: Integer) == d =
        Just (VInt (fromIntegral rounded))
  where
    rounded = round d :: Integer
joinKey v = Just v

joinKeyOf :: String -> Row -> Maybe Value
joinKeyOf c row = lookup c row >>= joinKey

data EquiKeys = EquiKeys
    { ekLeft :: String
    , ekRight :: String
    }

-- | 认出 a = b 这种两列等值条件，并且能分清谁在左谁在右
equiKeys :: [String] -> [String] -> Expr -> Maybe EquiKeys
equiKeys lcols rcols (Eq (Col a) (Col b))
    | a /= b, a `elem` lcols, b `elem` rcols = Just (EquiKeys a b)
    | a /= b, b `elem` lcols, a `elem` rcols = Just (EquiKeys b a)
equiKeys _ _ _ = Nothing

-- | 连接：能认出等值键就走哈希，否则退回嵌套循环；左连接没配上的左行补 NULL
joinRowsIn ::
    (MonadStorage m) =>
    Database ->
    Row ->
    JoinKind ->
    [String] ->
    Expr ->
    [Row] ->
    [Row] ->
    m (Either String [Row])
joinRowsIn db outer kind rcols cond lrows rrows =
    case equiKeys (rowsCols lrows) rcols cond of
        Just ks -> pure (Right (hashJoin kind ks ncols lrows rrows))
        Nothing -> filterPairsM db outer kind ncols cond lrows rrows
  where
    -- 右表为空时行上没有列名，用算子推导出来的列名补 NULL
    ncols = if null rrows then rcols else rowsCols rrows

-- | 左连接里配不上的左行补右表 NULL；内连接里没有匹配就是没行
unmatched :: JoinKind -> Row -> [String] -> [Row]
unmatched InnerJoin _ _ = []
unmatched LeftJoin l cols = [l ++ [(c, VNull) | c <- cols]]

-- | 哈希连接，输出顺序同嵌套循环
hashJoin :: JoinKind -> EquiKeys -> [String] -> [Row] -> [Row] -> [Row]
hashJoin kind ks ncols lrows rrows = concatMap probe lrows
  where
    buckets = HM.fromListWith (flip (++)) [(v, [r]) | r <- rrows, Just v <- [joinKeyOf (ekRight ks) r]]
    probe l =
        let miss = case kind of
                InnerJoin -> []
                LeftJoin -> [l ++ nullRow]
         in case joinKeyOf (ekLeft ks) l of
                Nothing -> miss
                Just v -> case HM.lookupDefault [] v buckets of
                    [] -> miss
                    rs -> [l ++ r | r <- rs]
    nullRow = [(c, VNull) | c <- ncols]

-- | 把两边的行按条件配对（边配边筛，不先建整张积表）
filterPairsM ::
    (MonadStorage m) =>
    Database ->
    Row ->
    JoinKind ->
    [String] ->
    Expr ->
    [Row] ->
    [Row] ->
    m (Either String [Row])
filterPairsM db outer kind ncols cond lrows rrows = go lrows []
  where
    go [] acc = pure (Right (reverse acc))
    go (l : ls) acc = do
        hits <- keep l rrows []
        case hits of
            Left e -> pure (Left e)
            Right hs -> do
                let hs' = case (kind, hs) of
                        (LeftJoin, []) -> [l ++ nullRow]
                        _ -> hs
                go ls (reverse hs' ++ acc)
    keep _ [] acc = pure (Right (reverse acc))
    keep l (r : rs) acc = do
        ok <- evalCondForRowM db cond (l ++ r ++ outer)
        case ok of
            Left e -> pure (Left e)
            Right True -> keep l rs ((l ++ r) : acc)
            Right False -> keep l rs acc
    nullRow = [(c, VNull) | c <- ncols]

-- | 索引连接：右表是裸 Scan 时逐左行点查，返回 Nothing 表示该走通用路径
probeIndexJoin ::
    (MonadStorage m) =>
    Database ->
    Row ->
    JoinKind ->
    Maybe String ->
    String ->
    EquiKeys ->
    [String] ->
    Expr ->
    Row ->
    m (Either String (Maybe [Row]))
probeIndexJoin db outer kind mAlias t ks rcols cond l = case lookup (ekLeft ks) l of
    Nothing -> pure (Right (Just (unmatched kind l rcols)))
    Just k
        | not (indexSafe db t (unqualify mAlias (ekRight ks)) k) -> pure (Right Nothing)
        | otherwise -> do
            res <- lookupByColumn t (unqualify mAlias (ekRight ks)) k
            case res of
                Left e -> pure (Left e)
                Right NoIndex -> pure (Right Nothing)
                Right (IndexRows rows) -> do
                    pairs <- joinRowsIn db outer kind rcols cond [l] (prefixRows mAlias rows)
                    pure (fmap Just pairs)

-- | 索引按值的相等匹配，而数值比较会把 2 和 2.0 看成相等，
-- 所以只有非浮点列 + 非浮点键才敢用索引，保证优化不改变结果语义
indexSafe :: Database -> String -> String -> Value -> Bool
indexSafe db t col k = case lookupTable db t of
    Left _ -> False
    Right tbl -> case lookup col (tableCols tbl) of
        Nothing -> False
        Just c -> not (floatType (columnType c)) && not (floatValue k)

floatType :: ColumnType -> Bool
floatType CFloat = True
floatType CDouble = True
floatType (CDecimal _ _) = True
floatType _ = False

floatValue :: Value -> Bool
floatValue (VFloat _) = True
floatValue _ = False

-- | 分组键的值
groupKey :: [String] -> Row -> Either String [Value]
groupKey keys row = mapM get keys
  where
    get c = maybe (Left ("unknown column: " ++ c)) Right (lookup c row)

-- | 按分组键把行分组，保留分组首次出现的顺序
groupRows :: [String] -> [Row] -> Either String [([Value], [Row])]
groupRows keys rows = go rows (HM.empty, [])
  where
    go [] (acc, order) = Right [(k, reverse (HM.lookupDefault [] k acc)) | k <- reverse order]
    go (r : rs) (acc, order) = do
        k <- groupKey keys r
        let order' = if HM.member k acc then order else k : order
        go rs (HM.insertWith (++) k [r] acc, order')

-- | 一组行上求同一个表达式（外层列从 outer 取）
valuesOf :: Row -> Expr -> [Row] -> Either String [Value]
valuesOf outer e rows = mapM (evalExpr e . (++ outer)) rows

-- | 去掉 NULL
nonNull :: [Value] -> [Value]
nonNull = filter (/= VNull)

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

-- | 一组值里的极值（NULL 已经先剔掉）
extremeOf :: (Value -> Value -> Value) -> [Value] -> Either String Value
extremeOf _ [] = Right VNull
extremeOf pick (x : xs) = Right (foldl pick x xs)

-- | 一个聚合算子在一组行上的结果
evalAgg :: Row -> Expr -> [Row] -> Either String Value
evalAgg outer agg rows = case agg of
    CountAll -> Right (VInt (length rows))
    CountOf e -> do
        vs <- valuesOf outer e rows
        Right (VInt (length (nonNull vs)))
    SumOf e -> do
        vs <- valuesOf outer e rows
        case nonNull vs of
            [] -> Right VNull
            xs -> foldM addValue (VInt 0) xs
    AvgOf e -> do
        vs <- valuesOf outer e rows
        case nonNull vs of
            [] -> Right VNull
            xs -> do
                total <- foldM addValue (VInt 0) xs
                sumOf <- numberOf total
                Right (VFloat (sumOf / fromIntegral (length xs)))
    MinOf e -> valuesOf outer e rows >>= extremeOf lower . nonNull
    MaxOf e -> valuesOf outer e rows >>= extremeOf higher . nonNull
    _ -> Left "only COUNT, SUM, AVG, MIN and MAX can be aggregated"
  where
    lower a b = if compareValue b a == LT then b else a
    higher a b = if compareValue b a == GT then b else a

-- | 一组的输出行：分组列 + 各聚合值
oneGroup :: Row -> [String] -> [(String, Expr)] -> ([Value], [Row]) -> Either String Row
oneGroup outer keys aggs (keyValues, rows) = do
    values <- mapM (\(_, e) -> evalAgg outer e rows) aggs
    Right (zip keys keyValues ++ zip (map fst aggs) values)

-- | 分组求值：没有分组列时，即使没有行也要出一行
evalAggregate :: Row -> [String] -> [(String, Expr)] -> [Row] -> Either String [Row]
evalAggregate outer keys aggs rows = do
    groups <- groupRows keys rows
    let top = if null keys && null groups then [([], [])] else groups
    mapM (oneGroup outer keys aggs) top

-- | 单子过滤：条件本身是单子求值的
filterRowsM :: (Monad m) => (Row -> m (Either String Bool)) -> [Row] -> m (Either String [Row])
filterRowsM p = go []
  where
    go acc [] = pure (Right (reverse acc))
    go acc (r : rs) = do
        keep <- p r
        case keep of
            Left e -> pure (Left e)
            Right True -> go (r : acc) rs
            Right False -> go acc rs

-- | 按投影顺序逐项求值（可以带子查询）
computeM :: (MonadStorage m) => Database -> Row -> [(String, Expr)] -> Row -> m (Either String Row)
computeM db outer items row = go items []
  where
    go [] acc = pure (Right (reverse acc))
    go ((label, e) : rest) acc = do
        v <- evalExprM db e (row ++ outer)
        case v of
            Left err -> pure (Left err)
            Right val -> go rest ((label, val) : acc)

-- | 表达式求值（先把子查询求成字面量）
evalExprM :: (MonadStorage m) => Database -> Expr -> Row -> m (Either String Value)
evalExprM db e env = do
    e' <- substSubqueries db e env
    pure (e' >>= \x -> evalExpr x env)

-- | 条件求值（先把子查询求成字面量）
evalCondForRowM :: (MonadStorage m) => Database -> Expr -> Row -> m (Either String Bool)
evalCondForRowM db e env = do
    e' <- substSubqueries db e env
    pure (e' >>= \x -> evalCondForRow x env)

-- | 子查询结果里唯一那一列的值
rowValue :: Row -> Either String Value
rowValue [(_, v)] = Right v
rowValue _ = Left "subquery must return exactly one column"

-- | 标量子查询：0 行给 NULL，多行报错
scalarResult :: Either String [Row] -> Either String Expr
scalarResult rows = do
    rs <- rows
    case rs of
        [] -> Right LitNull
        [r] -> valueLiteral <$> rowValue r
        _ -> Left "scalar subquery returned more than one row"

-- | 把子查询求值成字面量；子查询的错直接往上传
substSubqueries :: (MonadStorage m) => Database -> Expr -> Row -> m (Either String Expr)
substSubqueries db e env = case e of
    ScalarSub sq -> scalarResult <$> runSubqueryRows db sq env
    InSub a sq negated -> do
        lhs <- substSubqueries db a env
        rows <- runSubqueryRows db sq env
        pure (do
            a' <- lhs
            rs <- rows
            vs <- mapM rowValue rs
            Right (InList a' (map valueLiteral vs) negated))
    ExistsSub sq negated -> do
        rows <- runSubqueryRows db sq env
        pure (fmap (\rs -> LitBool (if negated then null rs else not (null rs))) rows)
    InList a es negated -> do
        lhs <- substSubqueries db a env
        es' <- mapM (\x -> substSubqueries db x env) es
        pure (do
            a' <- lhs
            items <- sequence es'
            Right (InList a' items negated))
    Add a b -> bin Add a b
    Sub a b -> bin Sub a b
    Mul a b -> bin Mul a b
    Div a b -> bin Div a b
    Neg a -> un Neg a
    Gt a b -> bin Gt a b
    Lt a b -> bin Lt a b
    Eq a b -> bin Eq a b
    And a b -> bin And a b
    Or a b -> bin Or a b
    IsNull a -> un IsNull a
    IsNotNull a -> un IsNotNull a
    CountOf a -> un CountOf a
    SumOf a -> un SumOf a
    AvgOf a -> un AvgOf a
    MinOf a -> un MinOf a
    MaxOf a -> un MaxOf a
    _ -> pure (Right e)
  where
    bin ctor a b = do
        ra <- substSubqueries db a env
        case ra of
            Left err -> pure (Left err)
            Right a' -> do
                rb <- substSubqueries db b env
                pure ((ctor a') <$> rb)
    un ctor a = do
        ra <- substSubqueries db a env
        pure (ctor <$> ra)

-- | 跑子查询：翻译成算子树再求值，外层行做相关子查询的绑定
runSubqueryRows :: (MonadStorage m) => Database -> Subquery -> Row -> m (Either String [Row])
runSubqueryRows db sq outer = case translate (subqueryStatement sq) of
    Left e -> pure (Left e)
    Right op -> evalRelOpIn db outer (optimize db op)

-- | 单子求值：Scan/点查/范围查都问存储
evalRelOpM :: (MonadStorage m) => Database -> RelOp -> m (Either String [Row])
evalRelOpM db = evalRelOpIn db []

-- | 纯求值（内存存储，子查询走同一条路径）
evalRelOp :: Database -> RelOp -> Either String [Row]
evalRelOp db op = do
    (result, _) <- runMemoryStorage (evalRelOpIn db [] op) db
    result

-- | 带外层行的执行器
evalRelOpIn :: (MonadStorage m) => Database -> Row -> RelOp -> m (Either String [Row])
evalRelOpIn _ _ Unit = pure (Right [[]])
evalRelOpIn db outer (Compute items op) = do
    rows <- evalRelOpIn db outer op
    case rows of
        Left e -> pure (Left e)
        Right rs -> do
            computed <- mapM (computeM db outer items) rs
            pure (sequence computed)
evalRelOpIn db outer (Aggregate keys aggs op) = do
    rows <- evalRelOpIn db outer op
    pure (rows >>= evalAggregate outer keys aggs)
evalRelOpIn _ _ (Scan mAlias tbl cols) = do
    res <- maybe (scan tbl) (scanColumns tbl) cols
    pure (fmap (prefixRows mAlias) res)
evalRelOpIn db outer (Lookup mAlias tbl col k) = do
    res <- lookupByColumn tbl col k
    case res of
        Left e -> pure (Left e)
        Right NoIndex -> scanFiltered db outer mAlias tbl (lookupCondition mAlias col k)
        Right (IndexRows rows) -> pure (Right (prefixRows mAlias rows))
evalRelOpIn db outer (Range mAlias tbl col lo hi) = do
    res <- scanRange tbl col lo hi
    case res of
        Left e -> pure (Left e)
        Right Nothing -> scanFiltered db outer mAlias tbl (rangeCondition mAlias col lo hi)
        Right (Just rows) -> pure (Right (prefixRows mAlias rows))
evalRelOpIn db outer (Filter e op) = do
    rows <- evalRelOpIn db outer op
    case rows of
        Left err -> pure (Left err)
        Right rs -> filterRowsM (\r -> evalCondForRowM db e (r ++ outer)) rs
evalRelOpIn db outer (Project cols op) = do
    rows <- evalRelOpIn db outer op
    pure (fmap (map (project cols)) rows)
evalRelOpIn db outer (Sort spec op) = do
    rows <- evalRelOpIn db outer op
    pure (fmap (sortRows spec) rows)
evalRelOpIn db outer (Limit n op) = do
    rows <- evalRelOpIn db outer op
    pure (fmap (take n) rows)
evalRelOpIn db outer (Join kind l r c) = do
    lres <- evalRelOpIn db outer l
    case lres of
        Left e -> pure (Left e)
        Right ls -> do
            let nested = do
                    rres <- evalRelOpIn db outer r
                    case rres of
                        Left e -> pure (Left e)
                        Right rs -> joinRowsIn db outer kind (relOpCols db r) c ls rs
            case (r, equiKeys (relOpCols db l) (relOpCols db r) c) of
                (Scan mAlias t _, Just ks) -> do
                    probed <- mapM (probeIndexJoin db outer kind mAlias t ks (relOpCols db r) c) ls
                    case sequence probed of
                        Left e -> pure (Left e)
                        Right ms -> case sequence ms of
                            Just rows -> pure (Right (concat rows))
                            Nothing -> nested
                _ -> nested

-- | 扫描后按条件过滤（点查/范围查退化成扫描时用）
scanFiltered ::
    (MonadStorage m) =>
    Database ->
    Row ->
    Maybe String ->
    String ->
    Expr ->
    m (Either String [Row])
scanFiltered _ outer mAlias tbl cond = do
    res <- scan tbl
    pure $ do
        rows <- res
        filterM (evalCondForRow cond . (++ outer)) (prefixRows mAlias rows)
