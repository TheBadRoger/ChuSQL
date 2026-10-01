{-# LANGUAGE PatternSynonyms #-}

module ChuSQL.Model
    ( ColumnType (..)
    , Column (..)
    , pattern TInt
    , pattern TStr
    , pattern TBool
    , TypeClass (..)
    , Value (..)
    , Row
    , Table (..)
    , Database
    , colNames
    , colType
    , lookupTable
    , allColumns
    , qualify
    , unqualify
    , resolveColumn
    , plainColumn
    , typeLabel
    , typeName
    , parseColumnType
    , typeClassOf
    , assignable
    , integerType
    , numericType
    , valueFits
    , coerceValue
    ) where

import Data.Char (isDigit, isSpace, toLower)
import Data.Hashable (Hashable (..))
import Data.List (isSuffixOf, stripPrefix)
import Data.Maybe (fromMaybe)

-- 数据模型：列类型、列约束、值、行、表、数据库，外加几个查表小工具。

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

pattern TInt :: Column
pattern TInt <- Column CInt _ _ _ _ _ _
  where
    TInt = plainColumn CInt

pattern TStr :: Column
pattern TStr <- Column CStr _ _ _ _ _ _
  where
    TStr = plainColumn CStr

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

-- | 解析类型名，可带长度/精度参数；不认识的给 Nothing
parseColumnType :: String -> Maybe ColumnType
parseColumnType raw = case (base, args) of
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
    cleaned = map toLower (filter (not . isSpace) raw)
    (base, rest) = span (/= '(') cleaned
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
    deriving (Show, Eq)

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

-- | 同大类可赋值；日期/时间列接受字符串字面量
assignable :: ColumnType -> ColumnType -> Bool
assignable target source
    | target == source = True
    | typeClassOf target == typeClassOf source = True
    | typeClassOf target == TemporalClass && typeClassOf source == TextClass = True
    | otherwise = False

integerType :: ColumnType -> Bool
integerType CInt = True
integerType CBigInt = True
integerType CSmallInt = True
integerType _ = False

numericType :: ColumnType -> Bool
numericType t = typeClassOf t == NumericClass

-- | 值能不能放进这一列（先不看可空）
valueFits :: ColumnType -> Value -> Bool
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

toInt :: ColumnType -> Value -> Either String Value
toInt _ (VInt n) = Right (VInt n)
toInt t (VFloat d) = case properFraction d :: (Int, Double) of
    (n, frac)
        | frac == 0 -> Right (VInt n)
        | otherwise -> Left ("cannot put a fractional number into " ++ typeLabel t)
toInt t _ = Left ("cannot put this value into " ++ typeLabel t)

toFloat :: ColumnType -> Value -> Either String Value
toFloat _ (VInt n) = Right (VFloat (fromIntegral n))
toFloat _ (VFloat d) = Right (VFloat d)
toFloat t _ = Left ("cannot put this value into " ++ typeLabel t)

-- | DECIMAL 按小数位四舍五入
toDecimal :: Int -> Int -> Value -> Either String Value
toDecimal p s v = case v of
    VInt n -> rounded (fromIntegral n)
    VFloat d -> rounded d
    _ -> Left ("cannot put this value into " ++ typeLabel (CDecimal p s))
  where
    scale = (10 :: Double) ^ max 0 s
    rounded d = Right (VFloat (fromIntegral (round (d * scale) :: Integer) / scale))

toText :: ColumnType -> Value -> Either String Value
toText _ (VStr s) = Right (VStr s)
toText t _ = Left ("cannot put this value into " ++ typeLabel t)

toSized :: Int -> Value -> Either String Value
toSized n (VStr s)
    | length s <= n = Right (VStr s)
    | otherwise = Left ("value is longer than " ++ show n ++ " characters")
toSized _ _ = Left "cannot put this value into a character column"

toBool :: ColumnType -> Value -> Either String Value
toBool _ (VBool b) = Right (VBool b)
toBool t _ = Left ("cannot put this value into " ++ typeLabel t)

toDate :: Value -> Either String Value
toDate (VStr s)
    | validDateText s = Right (VStr s)
    | otherwise = Left "date must look like YYYY-MM-DD"
toDate _ = Left "date must be a string"

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

-- | YYYY-MM-DD HH:MM:SS，日期与时间之间允许空格或 T
validTimestampText :: String -> Bool
validTimestampText s = case break (\c -> c == ' ' || c == 'T') s of
    (d, _ : tm) -> validDateText d && validTimeText tm
    _ -> False

-- | HH:MM:SS
validTimeText :: String -> Bool
validTimeText t = case splitOn ':' t of
    [h, m, sec] -> allOf 2 h && allOf 2 m && allOf 2 sec
    _ -> False

allOf :: Int -> String -> Bool
allOf n s = length s == n && all isDigit s


data Value
    = VNull
    | VInt Int
    | VFloat Double
    | VStr String
    | VBool Bool
    deriving (Show, Eq)

instance Hashable Value where
    hashWithSalt s VNull = hashWithSalt (hashWithSalt s (0 :: Int)) ()
    hashWithSalt s (VInt n) = hashWithSalt (hashWithSalt s (1 :: Int)) n
    hashWithSalt s (VFloat d) = hashWithSalt (hashWithSalt s (2 :: Int)) d
    hashWithSalt s (VStr t) = hashWithSalt (hashWithSalt s (3 :: Int)) t
    hashWithSalt s (VBool b) = hashWithSalt (hashWithSalt s (4 :: Int)) b

type Row = [(String, Value)]

data Table = Table
    { tableName :: String
    , tableCols :: [(String, Column)]
    , tableRows :: [Row]
    }
    deriving (Show)

type Database = [(String, Table)]

-- | 取表的列名
colNames :: Table -> [String]
colNames = map fst . tableCols

-- | 查某列的定义
colType :: Table -> String -> Maybe Column
colType t c = lookup c (tableCols t)

-- | 按名查表，没有就报错
lookupTable :: Database -> String -> Either String Table
lookupTable db t = maybe (Left ("unknown table: " ++ t)) Right (lookup t db)

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
    matches k = k == name || ('.' : name) `isSuffixOf` k
