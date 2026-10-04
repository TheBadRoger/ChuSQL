{-# LANGUAGE PatternSynonyms #-}

module ChuSQL.Core.Model
    ( ColumnType (..)
    , Column (..)
    , pattern TInt
    , pattern TStr
    , pattern TBool
    , TypeClass (..)
    , Value (..)
    , Row
    , TableMeta (..)
    , Histogram (..)
    , Table (..)
    , Database
    , histogramOf
    , histogramFor
    , colNames
    , colType
    , lookupTable
    , resolveTable
    , lastTablePart
    , deriveQualifier
    , allColumns
    , qualify
    , unqualify
    , resolveColumn
    , plainColumn
    , typeLabel
    , typeName
    , parseColumnType
    , domainTableName
    , isDomainTable
    , typeClassOf
    , assignable
    , comparableTypes
    , integerType
    , numericType
    , valueFits
    , coerceValue
    , canonicalValue
    , compareValue
    , valuesEqual
    , hashValue
    ) where

import Data.Char (isDigit, isSpace, toLower)
import Data.Hashable (Hashable (..))
import Data.List (isPrefixOf, isSuffixOf, stripPrefix)
import Data.Maybe (fromMaybe, mapMaybe)

-- 数据模型：列类型、列约束、值、行、表、数据库，外加查表小工具。

-- | 列的基础类型
data ColumnType
    = CInt
    | CBigInt
    | CSmallInt
    | CStr
    | CVarchar Int
    | CChar Int
    | CBool
    | CFloat
    | CDouble
    | CDecimal Int Int
    | CDate
    | CTimestamp
    | CBlob
    | CNull
    | CDomain String ColumnType
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
typeLabel (CVarchar n) = "TVarchar " ++ show n
typeLabel (CChar n) = "TChar " ++ show n
typeLabel CBool = "TBool"
typeLabel CFloat = "TFloat"
typeLabel CDouble = "TDouble"
typeLabel (CDecimal p s) = "TDecimal " ++ show p ++ " " ++ show s
typeLabel CDate = "TDate"
typeLabel CTimestamp = "TTimestamp"
typeLabel CBlob = "TBlob"
typeLabel CNull = "NULL"
typeLabel (CDomain name _) = name

-- | 线上/磁盘上的类型名
typeName :: ColumnType -> String
typeName CInt = "int"
typeName CBigInt = "bigint"
typeName CSmallInt = "smallint"
typeName CStr = "str"
typeName (CVarchar n) = "varchar(" ++ show n ++ ")"
typeName (CChar n) = "char(" ++ show n ++ ")"
typeName CBool = "bool"
typeName CFloat = "float"
typeName CDouble = "double"
typeName (CDecimal p s) = "decimal(" ++ show p ++ "," ++ show s ++ ")"
typeName CDate = "date"
typeName CTimestamp = "timestamp"
typeName CBlob = "blob"
typeName CNull = "null"
typeName (CDomain name base) = "domain(" ++ name ++ "," ++ typeName base ++ ")"

-- | 生成类型定义的目录键
domainTableName :: String -> String
domainTableName name = "__system_domain_" ++ map toLower name

-- | 判断类型定义目录键
isDomainTable :: String -> Bool
isDomainTable name = "__system_domain_" `isPrefixOf` map toLower name

-- | 解析类型名，可带长度/精度参数；不认识的给 Nothing
parseColumnType :: String -> Maybe ColumnType
parseColumnType raw
    | Just inner <- stripPrefix "domain(" raw
    , Just body <- stripSuffixMaybe ")" inner
    , (name, ',' : definition) <- break (== ',') body =
        CDomain name <$> parseColumnType definition
    | otherwise = case (base, args) of
        ("int", []) -> Just CInt
        ("integer", []) -> Just CInt
        ("bigint", []) -> Just CBigInt
        ("smallint", []) -> Just CSmallInt
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
        ("double", []) -> Just CDouble
        ("decimal", []) -> Just (CDecimal 10 0)
        ("numeric", []) -> Just (CDecimal 10 0)
        ("decimal", [p]) -> flip CDecimal 0 <$> positive p
        ("numeric", [p]) -> flip CDecimal 0 <$> positive p
        ("decimal", [p, s]) -> CDecimal <$> positive p <*> plain s
        ("numeric", [p, s]) -> CDecimal <$> positive p <*> plain s
        ("date", []) -> Just CDate
        ("timestamp", []) -> Just CTimestamp
        ("blob", []) -> Just CBlob
        _ -> Nothing
  where
    -- | 去掉空白的类型名
    cleaned = map toLower (filter (not . isSpace) raw)
    -- | 基础名与参数段
    (base, rest) = span (/= '(') cleaned
    -- | 括号里的参数表
    args = case stripSuffixMaybe ")" (drop 1 rest) of
        Nothing -> []
        Just inner -> map (filter (not . isSpace)) (splitOn ',' inner)

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
typeClassOf CVarchar{} = TextClass
typeClassOf CChar{} = TextClass
typeClassOf CBlob = TextClass
typeClassOf CDate = TemporalClass
typeClassOf CTimestamp = TemporalClass
typeClassOf CBool = BooleanClass
typeClassOf CNull = NullClass
typeClassOf (CDomain _ base) = typeClassOf base

-- | 同大类可赋值；日期/时间列接受字符串字面量
assignable :: ColumnType -> ColumnType -> Bool
assignable target source
    | source == CNull = True
    | target == source = True
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
comparableTypes a b = a == CNull || b == CNull || sameClass || crossTemporal
  where
    -- | 两个类型同属一个大类
    sameClass = typeClassOf a == typeClassOf b
    -- | 时间与文本放在一起比
    crossTemporal = (temporal a && textual b) || (textual a && temporal b)
    -- | 是不是时间类型
    temporal t = typeClassOf t == TemporalClass
    -- | 是不是文本类型
    textual t = typeClassOf t == TextClass

-- | 是不是数值类型
numericType :: ColumnType -> Bool
numericType t = typeClassOf t == NumericClass

-- | 值能不能放进这一列（先不看可空）
valueFits :: ColumnType -> Value -> Bool
valueFits (CDomain _ base) value = valueFits base value
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
valueFits CVarchar{} (VStr _) = True
valueFits CChar{} (VStr _) = True
valueFits CBool (VBool _) = True
valueFits CDate (VStr _) = True
valueFits CTimestamp (VStr _) = True
valueFits CBlob (VStr _) = True
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
    CStr -> toText t v
    CVarchar n -> toSized n v
    CChar n -> toSized n v
    CBool -> toBool t v
    CDate -> toDate v
    CTimestamp -> toTimestamp v
    CBlob -> toText t v
    CNull -> Left "a NULL output cannot contain a non-NULL value"
    CDomain _ base -> coerceValue base v

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
    -- | 缩放因子
    scale = (10 :: Double) ^ s
    -- | 按比例四舍五入
    rounded d
        | isNaN d || isInfinite d || isInfinite (d * scale) = Left "decimal value is not finite or exceeds precision"
        | abs result >= (10 :: Double) ^ (p - s) = Left "decimal value exceeds precision"
        | otherwise = Right (VFloat result)
      where
        result = fromIntegral (round (d * scale) :: Integer) / scale

-- | 字符串收进文本列
toText :: ColumnType -> Value -> Either String Value
toText _ (VStr s) = Right (VStr s)
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
    (x, y) -> compare (valueRank x) (valueRank y)

-- | 族的先后，只用来给异族值定序
valueRank :: Value -> Int
valueRank VNull = 0
valueRank (VInt _) = 1
valueRank (VFloat _) = 1
valueRank (VStr _) = 2
valueRank (VBool _) = 3

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

-- | 一张表：名字、列定义与全部行；meta 为存储层统计，拿不到是 Nothing
data Table = Table
    { tableName :: String
    , tableCols :: [(String, Column)]
    , tableRows :: [Row]
    , tableMeta :: Maybe TableMeta
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
    -- | 引用里去掉库前缀的那一段
    bare = lastTablePart name
    -- | schema 里以这个表名收尾的键：`sales.users` 能推出裸表名引用
    suffixed = [(key, table) | (key, table) <- db, ('.' : name) `isSuffixOf` key]
    -- | 一个候选都没有就是查无此表，多于一个就是有歧义
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
    -- | 裸名匹配末段，库限定名只补一层前缀
    matches k = k == name || ('.' : name) `isSuffixOf` k
        || (length (filter (== '.') name) == 2 && length (filter (== '.') k) == 1 && ('.' : k) `isSuffixOf` name)
