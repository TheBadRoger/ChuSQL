{-# LANGUAGE PatternSynonyms #-}

module ChuSQL.Core.Model (
    ColumnType (..),
    ScalarType (..),
    scalarTypes,
    scalarDefinition,
    Column (..),
    pattern TInt,
    pattern TStr,
    pattern TBool,
    TypeClass (..),
    Value (..),
    Row,
    TableMeta (..),
    Histogram (..),
    Table (..),
    TableIdentity (..),
    Database,
    histogramOf,
    histogramFor,
    colNames,
    colType,
    lookupTable,
    resolveTable,
    lastTablePart,
    deriveQualifier,
    allColumns,
    qualify,
    unqualify,
    resolveColumn,
    plainColumn,
    typeLabel,
    typeName,
    parseColumnType,
    validEnumLabels,
    enumDefinition,
    domainTableName,
    isDomainTable,
    typeClassOf,
    assignable,
    comparableTypes,
    hasTypeCapability,
    integerType,
    numericType,
    valueFits,
    coerceValue,
    canonicalValue,
    compareValue,
    valuesEqual,
    hashValue,
) where

import ChuSQL.Core.Runtime
import ChuSQL.Core.Scalar
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.Char (isDigit, isSpace, toLower)
import Data.Hashable (Hashable (..))
import Data.List (elemIndex, isPrefixOf, isSuffixOf, nub, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TLE
import Data.Word (Word64)

-- 数据模型：列类型、列约束、值、行、表、数据库，外加查表小工具。

-- | 列的基础类型
data ColumnType
    = CInt
    | CBigInt
    | CSmallInt
    | CStr
    | CEnum String [String]
    | CVarchar Int
    | CChar Int
    | CBool
    | CFloat
    | CDouble
    | CDecimal Int Int
    | CDate
    | CTimestamp
    | CBlob
    | CScalar ScalarType
    | CNull
    | CDomain String ColumnType
    | CRuntime TypeId
    deriving (Show, Eq)

-- | 列定义：类型 + 约束；约束全部取默认值就是普通可空列。
data Column = Column
    { columnType :: ColumnType
    , columnNullable :: Bool
    , columnDefault :: Maybe Value
    , columnAutoIncrement :: Bool
    , columnUnique :: Bool
    , columnPrimaryKey :: Bool
    , columnCheck :: Maybe String
    }
    deriving (Eq)

-- | 普通可空列
plainColumn :: ColumnType -> Column
plainColumn t = Column t True Nothing False False False Nothing

-- | 普通整型列的匹配模式
pattern TInt :: Column
pattern TInt <- Column CInt _ _ _ _ _ _
    where
        TInt = plainColumn CInt

-- | 普通字符串列的匹配模式
pattern TStr :: Column
pattern TStr <- Column CStr _ _ _ _ _ _
    where
        TStr = plainColumn CStr

-- | 普通布尔列的匹配模式
pattern TBool :: Column
pattern TBool <- Column CBool _ _ _ _ _ _
    where
        TBool = plainColumn CBool

-- | 错误消息里用的类型名
typeLabel :: ColumnType -> String
typeLabel CInt = "TInt"
typeLabel CBigInt = "TBigInt"
typeLabel CSmallInt = "TSmallInt"
typeLabel CStr = "TStr"
typeLabel (CEnum name _) = name
typeLabel (CVarchar n) = "TVarchar " ++ show n
typeLabel (CChar n) = "TChar " ++ show n
typeLabel CBool = "TBool"
typeLabel CFloat = "TFloat"
typeLabel CDouble = "TDouble"
typeLabel (CDecimal p s) = "TDecimal " ++ show p ++ " " ++ show s
typeLabel CDate = "TDate"
typeLabel CTimestamp = "TTimestamp"
typeLabel CBlob = "TBlob"
typeLabel (CScalar kind) = scalarName kind
typeLabel CNull = "NULL"
typeLabel (CDomain name _) = name
typeLabel (CRuntime tid) = renderTypeExpr (typeExpression tid)

-- | 线上/磁盘上的类型名
typeName :: ColumnType -> String
typeName CInt = "int"
typeName CBigInt = "bigint"
typeName CSmallInt = "smallint"
typeName CStr = "str"
typeName (CEnum name labels) = "enum(" ++ TL.unpack (TLE.decodeUtf8 (A.encode (name, labels))) ++ ")"
typeName (CVarchar n) = "varchar(" ++ show n ++ ")"
typeName (CChar n) = "char(" ++ show n ++ ")"
typeName CBool = "bool"
typeName CFloat = "float"
typeName CDouble = "double"
typeName (CDecimal p s) = "decimal(" ++ show p ++ "," ++ show s ++ ")"
typeName CDate = "date"
typeName CTimestamp = "timestamp"
typeName CBlob = "blob"
typeName (CScalar kind) = scalarName kind
typeName CNull = "null"
typeName (CDomain name base) = "domain(" ++ name ++ "," ++ typeName base ++ ")"
typeName (CRuntime tid) = "runtime(" ++ BS.unpack (encodeType tid) ++ ")"

-- | 生成类型定义的目录键
domainTableName :: String -> String
domainTableName name = "__system_domain_" ++ map toLower name

-- | 判断类型定义目录键
isDomainTable :: String -> Bool
isDomainTable name = "__system_domain_" `isPrefixOf` map toLower name

-- | 解析类型名，可带长度/精度参数；不认识的给 Nothing
parseColumnType :: String -> Maybe ColumnType
parseColumnType raw
    | Just inner <- stripPrefix "enum(" raw
    , Just body <- stripSuffixMaybe ")" inner = do
        (name, labels) <- either (const Nothing) Just (A.eitherDecodeStrict (TE.encodeUtf8 (T.pack body)))
        if not (null name) && validEnumLabels labels then Just (CEnum name labels) else Nothing
    | Just inner <- stripPrefix "runtime(" raw
    , Just body <- stripSuffixMaybe ")" inner =
        either (const Nothing) (Just . CRuntime) (decodeType builtinTypes (BS.pack body))
    | Just inner <- stripPrefix "domain(" raw
    , Just body <- stripSuffixMaybe ")" inner
    , (name, ',' : definition) <- break (== ',') body =
        CDomain name <$> parseColumnType definition
    | otherwise = case (base, args) of
        ("int", []) -> Just CInt
        ("integer", []) -> Just CInt
        ("bigint", []) -> Just CBigInt
        ("smallint", []) -> Just CSmallInt
        ("int2", []) -> Just CSmallInt
        ("int4", []) -> Just CInt
        ("int8", []) -> Just CBigInt
        ("str", []) -> Just CStr
        ("text", []) -> Just CStr
        ("varchar", []) -> Just CStr
        ("varchar", [n]) -> CVarchar <$> positive n
        ("char", []) -> Just (CChar 1)
        ("char", [n]) -> CChar <$> positive n
        ("bool", []) -> Just CBool
        ("boolean", []) -> Just CBool
        ("float", []) -> Just CFloat
        ("real", []) -> Just CFloat
        ("float4", []) -> Just CFloat
        ("float8", []) -> Just CDouble
        ("double", []) -> Just CDouble
        ("decimal", []) -> Just (CDecimal 10 0)
        ("numeric", []) -> Just (CDecimal 10 0)
        ("decimal", [p]) -> flip CDecimal 0 <$> positive p
        ("numeric", [p]) -> flip CDecimal 0 <$> positive p
        ("decimal", [p, s]) -> CDecimal <$> positive p <*> plain s
        ("numeric", [p, s]) -> CDecimal <$> positive p <*> plain s
        ("date", []) -> Just CDate
        ("timestamp", []) -> Just CTimestamp
        ("datetime", []) -> Just CTimestamp
        ("blob", []) -> Just CBlob
        ("bytea", []) -> Just CBlob
        ("binary", []) -> Just CBlob
        ("varbinary", []) -> Just CBlob
        (name, []) | Just kind <- lookup name scalarTypes -> Just (CScalar kind)
        _ -> either (const Nothing) (Just . CRuntime . descriptorId) (parseTypeExpr raw >>= resolveType builtinTypes)
  where
    -- \| 去掉空白的类型名
    cleaned = map toLower (filter (not . isSpace) raw)
    -- \| 基础名与参数段
    (base, rest) = span (/= '(') cleaned
    -- \| 括号里的参数表
    args = case stripSuffixMaybe ")" (drop 1 rest) of
        Nothing -> []
        Just inner -> map (filter (not . isSpace)) (splitOn ',' inner)

-- | 验证枚举标签非空集合且没有重复
validEnumLabels :: [String] -> Bool
validEnumLabels labels = not (null labels) && length labels == length (nub labels) && all validLabel labels
  where
    -- 检查标签的 UTF-8 长度和文本边界。
    validLabel label = '\0' `notElem` label && BS.length (BS.fromStrict (TE.encodeUtf8 (T.pack label))) <= 63

-- 取得领域包装后的枚举身份与标签。
enumDefinition :: ColumnType -> Maybe (String, [String])
enumDefinition (CDomain _ base) = enumDefinition base
enumDefinition (CEnum name labels) = Just (name, labels)
enumDefinition _ = Nothing

-- 取得领域包装后的业务标量类型。
scalarDefinition :: ColumnType -> Maybe ScalarType
scalarDefinition (CDomain _ base) = scalarDefinition base
scalarDefinition (CScalar kind) = Just kind
scalarDefinition _ = Nothing

-- | 去掉结尾
stripSuffixMaybe :: String -> String -> Maybe String
stripSuffixMaybe suffix s
    | suffix `isSuffixOf` s = Just (take (length s - length suffix) s)
    | otherwise = Nothing

-- | 按逗号切开
splitOn :: Char -> String -> [String]
splitOn sep s = case break (== sep) s of
    (part, []) -> [part]
    (part, _ : rest) -> part : splitOn sep rest

-- | 正数参数
positive :: String -> Maybe Int
positive s
    | not (null s) && all isDigit s = Just (read s)
    | otherwise = Nothing

-- | 非负整数参数
plain :: String -> Maybe Int
plain = positive

-- | 写出普通列就只说类型名，带约束才补在后面
instance Show Column where
    show c = typeLabel (columnType c) ++ notNull ++ def ++ auto ++ uni ++ key ++ check
      where
        notNull = if columnNullable c then "" else " NOT NULL"
        def = maybe "" (\v -> " DEFAULT " ++ show v) (columnDefault c)
        auto = if columnAutoIncrement c then " AUTO_INCREMENT" else ""
        uni = if columnUnique c then " UNIQUE" else ""
        key = if columnPrimaryKey c then " PRIMARY KEY" else ""
        check = maybe "" (\s -> " CHECK (" ++ s ++ ")") (columnCheck c)

-- | 类型大类
data TypeClass
    = NumericClass
    | TextClass
    | TemporalClass
    | BooleanClass
    | NullClass
    | RuntimeClass
    | EnumClass
    | ScalarClass
    deriving (Show, Eq)

-- | 整型大类判断
typeClassOf :: ColumnType -> TypeClass
typeClassOf CInt = NumericClass
typeClassOf CBigInt = NumericClass
typeClassOf CSmallInt = NumericClass
typeClassOf CFloat = NumericClass
typeClassOf CDouble = NumericClass
typeClassOf CDecimal{} = NumericClass
typeClassOf CStr = TextClass
typeClassOf CEnum{} = EnumClass
typeClassOf CVarchar{} = TextClass
typeClassOf CChar{} = TextClass
typeClassOf CBlob = TextClass
typeClassOf CDate = TemporalClass
typeClassOf CTimestamp = TemporalClass
typeClassOf CScalar{} = ScalarClass
typeClassOf CBool = BooleanClass
typeClassOf CNull = NullClass
typeClassOf (CDomain _ base) = typeClassOf base
typeClassOf CRuntime{} = RuntimeClass

-- | 同大类可赋值；日期/时间列接受字符串字面量
assignable :: ColumnType -> ColumnType -> Bool
assignable (CDomain _ base) source = assignable base source
assignable target (CDomain _ base) = assignable target base
assignable target source
    | source == CNull = True
    | target == source = True
    | typeClassOf target == ScalarClass || typeClassOf source == ScalarClass =
        typeClassOf target == ScalarClass && typeClassOf source == TextClass
    | typeClassOf target == EnumClass || typeClassOf source == EnumClass = False
    | CRuntime tid <- target = Just (typeExpression tid) == scalarTypeExpression source
    | typeClassOf target == RuntimeClass || typeClassOf source == RuntimeClass = False
    | typeClassOf target == typeClassOf source = True
    | typeClassOf target == TemporalClass && typeClassOf source == TextClass = True
    | otherwise = False

-- | 是不是整数类型
integerType :: ColumnType -> Bool
integerType CInt = True
integerType CBigInt = True
integerType CSmallInt = True
integerType (CDomain _ base) = integerType base
integerType _ = False

-- | 比较用的类型族：同大类可比，日期时间与文本互比
comparableTypes :: ColumnType -> ColumnType -> Bool
comparableTypes (CDomain _ base) other = comparableTypes base other
comparableTypes other (CDomain _ base) = comparableTypes other base
comparableTypes a b
    | a == CNull || b == CNull = True
    | typeClassOf a == ScalarClass || typeClassOf b == ScalarClass = a == b
    | typeClassOf a == EnumClass || typeClassOf b == EnumClass = a == b
    | typeClassOf a == RuntimeClass || typeClassOf b == RuntimeClass = a == b && hasTypeCapability EqCapability a
    | otherwise = a == CNull || b == CNull || sameClass || crossTemporal
  where
    -- \| 两个类型同属一个大类
    sameClass = typeClassOf a == typeClassOf b
    -- \| 时间与文本放在一起比
    crossTemporal = (temporal a && textual b) || (textual a && temporal b)
    -- \| 是不是时间类型
    temporal t = typeClassOf t == TemporalClass
    -- \| 是不是文本类型
    textual t = typeClassOf t == TextClass

-- | 领域沿用基础类型的已注册值运算能力
hasTypeCapability :: TypeCapability -> ColumnType -> Bool
hasTypeCapability capability (CDomain _ base) = hasTypeCapability capability base
hasTypeCapability OrdCapability (CScalar kind) = scalarOrdering kind
hasTypeCapability capability (CRuntime tid) =
    either (const False) (elem capability . descriptorCapabilities) (describeType builtinTypes tid)
hasTypeCapability _ _ = True

-- | 是不是数值类型
numericType :: ColumnType -> Bool
numericType t = typeClassOf t == NumericClass

-- | 值能不能放进这一列（先不看可空）
valueFits :: ColumnType -> Value -> Bool
valueFits (CDomain _ base) value = valueFits base value
valueFits (CRuntime expected) (VRuntime supplied value) = expected == supplied && validateValue builtinTypes expected value == Right ()
valueFits (CRuntime expected) value = case scalarRuntimeValue value of
    Just runtime -> validateValue builtinTypes expected runtime == Right ()
    Nothing -> value == VNull
valueFits _ VNull = True
valueFits CInt (VInt _) = True
valueFits CBigInt (VInt _) = True
valueFits CSmallInt (VInt _) = True
valueFits CFloat (VInt _) = True
valueFits CFloat (VFloat _) = True
valueFits CDouble (VInt _) = True
valueFits CDouble (VFloat _) = True
valueFits CDecimal{} (VInt _) = True
valueFits CDecimal{} (VFloat _) = True
valueFits CStr (VStr _) = True
valueFits (CEnum name labels) (VEnum other supplied label) = name == other && labels == supplied && label `elem` labels
valueFits CVarchar{} (VStr _) = True
valueFits CChar{} (VStr _) = True
valueFits CBool (VBool _) = True
valueFits CDate (VStr _) = True
valueFits CTimestamp (VStr _) = True
valueFits CBlob (VStr _) = True
valueFits (CScalar kind) (VStr text) = either (const False) (const True) (normalizeScalar kind text)
valueFits _ _ = False

-- | 把值收进列类型：NULL 原样通过，类型不符报错
coerceValue :: ColumnType -> Value -> Either String Value
coerceValue _ VNull = Right VNull
coerceValue t v = case t of
    CInt -> toInt t v
    CBigInt -> toInt t v
    CSmallInt -> toInt t v
    CFloat -> toFloat t v
    CDouble -> toFloat t v
    CDecimal p s -> toDecimal p s v
    CEnum name labels -> case v of
        VStr label | label `elem` labels -> Right (VEnum name labels label)
        VEnum{} | valueFits t v -> Right v
        _ -> Left ("invalid ENUM value for " ++ name ++ ": " ++ show v ++ "; expected one of " ++ show labels)
    CStr -> toText t v
    CVarchar n -> toSized n v
    CChar n -> toSized n v
    CBool -> toBool t v
    CDate -> toDate v
    CTimestamp -> toTimestamp v
    CBlob -> toText t v
    CScalar kind -> case v of
        VStr text -> VStr <$> normalizeScalar kind text
        _ -> Left (scalarName kind ++ " must be a string")
    CNull -> Left "a NULL output cannot contain a non-NULL value"
    CDomain _ base -> coerceValue base v
    CRuntime expected -> case v of
        VRuntime supplied value | supplied == expected -> do
            validateValue builtinTypes expected value
            Right v
        _ -> case scalarRuntimeValue v of
            Just value -> do
                validateValue builtinTypes expected value
                Right (VRuntime expected value)
            Nothing -> Left "runtime value type identity mismatch"

-- | 整数收进整型列
toInt :: ColumnType -> Value -> Either String Value
toInt _ (VInt n) = Right (VInt n)
toInt t (VFloat d) = case properFraction d :: (Int, Double) of
    (n, frac)
        | frac == 0 -> Right (VInt n)
        | otherwise -> Left ("cannot put a fractional number into " ++ typeLabel t)
toInt t _ = Left ("cannot put this value into " ++ typeLabel t)

-- | 数值收进浮点列
toFloat :: ColumnType -> Value -> Either String Value
toFloat _ (VInt n) = Right (VFloat (fromIntegral n))
toFloat _ (VFloat d) = Right (VFloat d)
toFloat t _ = Left ("cannot put this value into " ++ typeLabel t)

-- | DECIMAL 按小数位四舍五入
toDecimal :: Int -> Int -> Value -> Either String Value
toDecimal p s v
    | p <= 0 || p > 308 || s < 0 || s > p = Left "invalid decimal precision or scale"
    | otherwise = case v of
        VInt n -> rounded (fromIntegral n)
        VFloat d -> rounded d
        _ -> Left ("cannot put this value into " ++ typeLabel (CDecimal p s))
  where
    -- \| 缩放因子
    scale = (10 :: Double) ^ s
    -- \| 按比例四舍五入
    rounded d
        | isNaN d || isInfinite d || isInfinite (d * scale) = Left "decimal value is not finite or exceeds precision"
        | abs result >= (10 :: Double) ^ (p - s) = Left "decimal value exceeds precision"
        | otherwise = Right (VFloat result)
      where
        result = fromIntegral (round (d * scale) :: Integer) / scale

-- | 字符串收进文本列
toText :: ColumnType -> Value -> Either String Value
toText _ (VStr s) = Right (VStr s)
toText _ (VEnum _ _ label) = Right (VStr label)
toText t _ = Left ("cannot put this value into " ++ typeLabel t)

-- | 按列长度限制收字符串
toSized :: Int -> Value -> Either String Value
toSized n (VStr s)
    | length s <= n = Right (VStr s)
    | otherwise = Left ("value is longer than " ++ show n ++ " characters")
toSized _ _ = Left "cannot put this value into a character column"

-- | 布尔收进布尔列
toBool :: ColumnType -> Value -> Either String Value
toBool _ (VBool b) = Right (VBool b)
toBool t _ = Left ("cannot put this value into " ++ typeLabel t)

-- | 字符串收进日期列
toDate :: Value -> Either String Value
toDate (VStr s)
    | validDateText s = Right (VStr s)
    | otherwise = Left "date must look like YYYY-MM-DD"
toDate _ = Left "date must be a string"

-- | 字符串收进时间戳列
toTimestamp :: Value -> Either String Value
toTimestamp (VStr s)
    | validTimestampText s = Right (VStr s)
    | otherwise = Left "timestamp must look like YYYY-MM-DD HH:MM:SS"
toTimestamp _ = Left "timestamp must be a string"

-- | YYYY-MM-DD
validDateText :: String -> Bool
validDateText s = case splitOn '-' s of
    [y, m, d] -> allOf 4 y && allOf 2 m && allOf 2 d
    _ -> False

-- | 日期时间格式；中间允许空格或 T
validTimestampText :: String -> Bool
validTimestampText s = case break (\c -> c == ' ' || c == 'T') s of
    (d, _ : tm) -> validDateText d && validTimeText tm
    _ -> False

-- | HH:MM:SS
validTimeText :: String -> Bool
validTimeText t = case splitOn ':' t of
    [h, m, sec] -> allOf 2 h && allOf 2 m && allOf 2 sec
    _ -> False

-- | 一段字符串是不是固定长度的数字
allOf :: Int -> String -> Bool
allOf n s = length s == n && all isDigit s

-- | 引擎里的一行值
data Value
    = VNull
    | VInt Int
    | VFloat Double
    | VStr String
    | VBool Bool
    | VEnum String [String] String
    | VRuntime TypeId RuntimeValue
    deriving (Show, Eq)

-- | 值的比较关键字：整值浮点折成整数
canonicalValue :: Value -> Value
canonicalValue (VFloat d)
    | wholeDouble d = VInt (truncate d)
canonicalValue v = v

-- | 浮点是不是能精确折成整数的整值
wholeDouble :: Double -> Bool
wholeDouble d =
    not (isNaN d)
        && not (isInfinite d)
        && abs d <= 9007199254740992
        && d == fromIntegral (truncate d :: Int)

-- | 值的全序：NULL 最小，数值跨类型比大小，异族按族的先后
compareValue :: Value -> Value -> Ordering
compareValue a b = case (canonicalValue a, canonicalValue b) of
    (VNull, VNull) -> EQ
    (VNull, _) -> LT
    (_, VNull) -> GT
    (VInt x, VInt y) -> compare x y
    (VInt x, VFloat y) -> compare (fromIntegral x) y
    (VFloat x, VInt y) -> compare x (fromIntegral y)
    (VFloat x, VFloat y) -> compare x y
    (VStr x, VStr y) -> compare x y
    (VBool x, VBool y) -> compare x y
    (VEnum nx xs x, VEnum ny ys y) -> compare (nx, xs, elemIndex x xs) (ny, ys, elemIndex y ys)
    (VRuntime tx x, VRuntime ty y) -> compare (tx, x) (ty, y)
    (x, y) -> compare (valueRank x) (valueRank y)

-- | 族的先后，只用来给异族值定序
valueRank :: Value -> Int
valueRank VNull = 0
valueRank (VInt _) = 1
valueRank (VFloat _) = 1
valueRank (VStr _) = 2
valueRank (VBool _) = 3
valueRank VRuntime{} = 4
valueRank VEnum{} = 5

-- | 相等语义：比较关键字相同即相等，与 hashValue 配套
valuesEqual :: Value -> Value -> Bool
valuesEqual a b = canonicalValue a == canonicalValue b

-- | 值的关键字哈希，与 valuesEqual 用同一套关键字
hashValue :: Value -> Int
hashValue v = case canonicalValue v of
    VNull -> hashWithSalt 0 (0 :: Int)
    VInt n -> hashWithSalt 1 n
    VFloat d -> hashWithSalt 1 d
    VStr t -> hashWithSalt 2 t
    VBool b -> hashWithSalt 3 b
    VEnum name labels label -> hashWithSalt 5 (name, labels, label)
    VRuntime tid value -> hashWithSalt 4 (BS.unpack (encodeType tid), value)

-- | 解析现有标量列的逻辑类型
scalarTypeExpression :: ColumnType -> Maybe TypeExpr
scalarTypeExpression (CDomain _ base) = scalarTypeExpression base
scalarTypeExpression CInt = Just intType
scalarTypeExpression CBigInt = Just intType
scalarTypeExpression CSmallInt = Just intType
scalarTypeExpression CDouble = Just doubleType
scalarTypeExpression CFloat = Just doubleType
scalarTypeExpression CBool = Just boolType
scalarTypeExpression CStr = Just stringType
scalarTypeExpression _ = Nothing

-- | 转换现有标量值
scalarRuntimeValue :: Value -> Maybe RuntimeValue
scalarRuntimeValue (VInt n) = Just (RInt n)
scalarRuntimeValue (VFloat n) = Just (RDouble n)
scalarRuntimeValue (VBool b) = Just (RBool b)
scalarRuntimeValue (VStr text) = Just (RString text)
scalarRuntimeValue _ = Nothing

-- | 值按类型打散搅拌
instance Hashable Value where
    hashWithSalt s = hashWithSalt s . hashValue

-- | 一行数据
type Row = [(String, Value)]

-- | 一列的等宽直方图：区间下界、上界与每桶计数
data Histogram = Histogram
    { histLow :: Double
    , histHigh :: Double
    , histBuckets :: [Int]
    }
    deriving (Show, Eq)

-- | 一张表的统计：行数、各列不同值数、列直方图与索引
data TableMeta = TableMeta
    { metaRowCount :: Int
    , metaDistinct :: [(String, Int, Bool)]
    , metaHistograms :: [(String, Histogram)]
    , metaIndexes :: [String]
    }
    deriving (Show)

-- | 保存持久化数据库与表编号
data TableIdentity = TableIdentity Word64 Word64 deriving (Show, Eq)

-- | 一张表：列、行、统计与独立对象身份
data Table = Table
    { tableName :: String
    , tableCols :: [(String, Column)]
    , tableRows :: [Row]
    , tableMeta :: Maybe TableMeta
    , tableIdentity :: Maybe TableIdentity
    , tableSchemaVersion :: Maybe Word64
    }
    deriving (Show)

-- | 库：库名到表的映射
type Database = [(String, Table)]

-- | 这一列的直方图
histogramFor :: TableMeta -> String -> Maybe Histogram
histogramFor m col = lookup col (metaHistograms m)

-- | 数值列按 16 个等宽桶统计，无数值给 Nothing
histogramOf :: [Value] -> Maybe Histogram
histogramOf values = case mapMaybe numericOf values of
    [] -> Nothing
    vs -> let lo = minimum vs; hi = maximum vs in Just (Histogram lo hi (bucketCounts lo hi vs))

-- | 值转成用于分桶的数；非数值给 Nothing
numericOf :: Value -> Maybe Double
numericOf (VInt n) = Just (fromIntegral n)
numericOf (VFloat d) = Just d
numericOf _ = Nothing

-- | 每个桶里落了多少个值
bucketCounts :: Double -> Double -> [Double] -> [Int]
bucketCounts lo hi vs = [length [v | v <- vs, bucketIndexOf lo hi v == i] | i <- [0 .. bucketTotal - 1]]

-- | 值落在第几个桶；区间退化时都算第 0 桶
bucketIndexOf :: Double -> Double -> Double -> Int
bucketIndexOf lo hi v
    | hi <= lo = 0
    | otherwise = max 0 (min (bucketTotal - 1) (floor ((v - lo) / (hi - lo) * fromIntegral bucketTotal) :: Int))

-- | 直方图的桶数
bucketTotal :: Int
bucketTotal = 16

-- | 取表的列名
colNames :: Table -> [String]
colNames = map fst . tableCols

-- | 查某列的定义
colType :: Table -> String -> Maybe Column
colType t c = lookup c (tableCols t)

-- | 按名查表，没有就报错
lookupTable :: Database -> String -> Either String Table
lookupTable db t = maybe (Left ("unknown table: " ++ t)) Right (lookup t db)

-- | 取表名引用的最后一段
lastTablePart :: String -> String
lastTablePart = reverse . takeWhile (/= '.') . reverse

-- | 推导列前缀：有别名用别名，否则取表名末段
deriveQualifier :: Maybe String -> String -> String
deriveQualifier (Just a) _ = a
deriveQualifier Nothing t = lastTablePart t

-- | 按名查表并返回 schema 键：精确、前缀、唯一后缀
resolveTable :: Database -> String -> Either String (String, Table)
resolveTable db name = case lookup name db of
    Just table -> Right (name, table)
    Nothing
        | '.' `elem` name, Just table <- lookup bare db -> Right (bare, table)
        | [(key, table)] <- suffixed -> Right (key, table)
        | otherwise -> Left (miss suffixed)
  where
    -- \| 引用里去掉库前缀的那一段
    bare = lastTablePart name
    -- \| schema 里以这个表名收尾的键：`sales.users` 能推出裸表名引用
    suffixed = [(key, table) | (key, table) <- db, ('.' : name) `isSuffixOf` key]
    -- \| 一个候选都没有就是查无此表，多于一个就是有歧义
    miss [] = "unknown table: " ++ name
    miss _ = "ambiguous table: " ++ name

-- | 通配列名
allColumns :: String
allColumns = "*"

-- | 给列名加别名前缀
qualify :: Maybe String -> String -> String
qualify Nothing c = c
qualify (Just a) c = a ++ "." ++ c

-- | 去掉别名前缀
unqualify :: Maybe String -> String -> String
unqualify Nothing c = c
unqualify (Just a) c = fromMaybe c (stripPrefix (a ++ ".") c)

-- | 裸列仅允许唯一匹配，限定名必须精确匹配
resolveColumn :: String -> [(String, a)] -> Either String (String, a)
resolveColumn name env = case [(k, v) | (k, v) <- env, matches k] of
    [entry] -> Right entry
    [] -> Left ("unknown column: " ++ name)
    _ -> Left ("ambiguous column: " ++ name)
  where
    -- \| 裸名匹配末段，库限定名只补一层前缀
    matches k =
        k == name
            || ('.' : name) `isSuffixOf` k
            || (length (filter (== '.') name) == 2 && length (filter (== '.') k) == 1 && ('.' : k) `isSuffixOf` name)
