module ChuSQL.Core.Runtime
    ( TypeConstructorId (..), TypeId, TypeExpr (..), Kind (..)
    , TypeRegistry, TypeDescriptor (..), PhysicalType (..), RuntimeValue (..)
    , builtinTypes, resolveType, describeType, typeExpression
    , intType, doubleType, boolType, stringType, listType, maybeType, tupleType, renderTypeExpr, parseTypeExpr, renderRuntimeValue
    , validateValue, encodeValue, decodeValue, encodeType, decodeType, runtimeToJSON, runtimeFromJSON
    ) where

import Data.Aeson (Value (..), eitherDecode, encode)
import Data.Aeson.Types (Parser, parseEither)
import qualified Data.ByteString.Lazy as BS
import Data.Scientific (floatingOrInteger)
import qualified Data.Text as T
import qualified Data.Vector as V
import Data.Char (isAlphaNum, toLower)
import qualified Text.ParserCombinators.ReadP as P

-- 运行时类型身份、构造器注册及版本化物理编码。

-- | 标识稳定的类型构造器
newtype TypeConstructorId = TypeConstructorId Int deriving (Show, Eq, Ord)

-- | 表达逻辑类型应用
data TypeExpr = TypeApply TypeConstructorId [TypeExpr] deriving (Show, Eq, Ord)

-- | 标识完整的类型实例
newtype TypeId = TypeId TypeExpr deriving (Show, Eq, Ord)

-- | 描述构造器参数个数
data Kind = TypeKind | TypeArrow Kind Kind | ProductKind deriving (Show, Eq)

-- | 描述独立的物理表示
data PhysicalType = SignedInteger | FloatingPoint | Boolean | Utf8 | Sequence PhysicalType
    | Optional PhysicalType | Product [PhysicalType] deriving (Show, Eq)

-- | 保存类型实例描述
data TypeDescriptor = TypeDescriptor
    { descriptorId :: TypeId, descriptorName :: String, descriptorPhysical :: PhysicalType
    } deriving (Show, Eq)

-- | 保存构造器元数据
newtype TypeRegistry = TypeRegistry [(TypeConstructorId, String, Kind)] deriving (Show, Eq)

-- | 表达运行时值
data RuntimeValue = RInt Int | RDouble Double | RBool Bool | RString String
    | RList [RuntimeValue] | RMaybe (Maybe RuntimeValue) | RTuple [RuntimeValue]
    deriving (Show, Eq)

-- | 恢复固定编号的内建构造器
builtinTypes :: TypeRegistry
builtinTypes = TypeRegistry
    [(TypeConstructorId 1, "Int", TypeKind), (TypeConstructorId 2, "Double", TypeKind)
    ,(TypeConstructorId 3, "Bool", TypeKind), (TypeConstructorId 4, "String", TypeKind)
    ,(TypeConstructorId 5, "List", TypeArrow TypeKind TypeKind)
    ,(TypeConstructorId 6, "Maybe", TypeArrow TypeKind TypeKind)
    ,(TypeConstructorId 7, "Tuple", ProductKind)]

-- | 构造整数类型表达式
intType :: TypeExpr
intType = TypeApply (TypeConstructorId 1) []

-- | 构造浮点类型表达式
doubleType :: TypeExpr
doubleType = TypeApply (TypeConstructorId 2) []

-- | 构造布尔类型表达式
boolType :: TypeExpr
boolType = TypeApply (TypeConstructorId 3) []

-- | 构造字符串类型表达式
stringType :: TypeExpr
stringType = TypeApply (TypeConstructorId 4) []

-- | 构造列表类型表达式
listType :: TypeExpr -> TypeExpr
listType t = TypeApply (TypeConstructorId 5) [t]

-- | 构造可选类型表达式
maybeType :: TypeExpr -> TypeExpr
maybeType t = TypeApply (TypeConstructorId 6) [t]

-- | 构造元组类型表达式
tupleType :: [TypeExpr] -> TypeExpr
tupleType = TypeApply (TypeConstructorId 7)

-- | 渲染内建逻辑类型语法
renderTypeExpr :: TypeExpr -> String
renderTypeExpr ty@(TypeApply cid arguments) = case (cid, arguments) of
    (TypeConstructorId 1, []) -> "Int"
    (TypeConstructorId 2, []) -> "Double"
    (TypeConstructorId 3, []) -> "Bool"
    (TypeConstructorId 4, []) -> "String"
    (TypeConstructorId 5, [child]) -> "[" ++ renderTypeExpr child ++ "]"
    (TypeConstructorId 6, [child]) -> "Maybe " ++ renderTypeExpr child
    (TypeConstructorId 7, fields) -> "(" ++ joinTypes fields ++ ")"
    _ -> show ty
  where
    -- | 连接元组字段类型
    joinTypes [] = ""
    joinTypes [field] = renderTypeExpr field
    joinTypes (field : fields) = renderTypeExpr field ++ ", " ++ joinTypes fields

-- | 解析完整的内建逻辑类型文本
parseTypeExpr :: String -> Either String TypeExpr
parseTypeExpr source = case P.readP_to_S (P.skipSpaces *> expression <* P.skipSpaces <* P.eof) source of
    [(ty, "")] -> Right ty
    _ -> Left "invalid runtime type expression"
  where
    -- | 解析类型原子和构造器应用
    expression = (intType <$ token "int") P.<++ (doubleType <$ token "double")
        P.<++ (boolType <$ token "bool") P.<++ (stringType <$ token "string")
        P.<++ (listType <$> between "[" "]" expression)
        P.<++ (listType <$> (token "list" *> expression))
        P.<++ (maybeType <$> (token "maybe" *> expression))
        P.<++ (tupleType <$> between "(" ")" (P.sepBy expression (symbol ",")))
    -- | 解析具有名称边界的类型关键字
    token word = do
        _ <- mapM character word
        rest <- P.look
        case rest of
            c : _ | isAlphaNum c || c == '_' -> P.pfail
            _ -> P.skipSpaces
    -- | 不区分大小写匹配关键字字符
    character c = P.satisfy ((== c) . toLower)
    -- | 解析类型分隔符
    symbol text = P.string text <* P.skipSpaces
    -- | 解析括号包围的类型
    between open close = P.between (symbol open) (symbol close)

-- | 渲染运行时值的可读文本
renderRuntimeValue :: RuntimeValue -> String
renderRuntimeValue (RInt n) = show n
renderRuntimeValue (RDouble n) = show n
renderRuntimeValue (RBool b) = show b
renderRuntimeValue (RString text) = show text
renderRuntimeValue (RList values) = "[" ++ joinValues values ++ "]"
renderRuntimeValue (RMaybe Nothing) = "Nothing"
renderRuntimeValue (RMaybe (Just value)) = "Just (" ++ renderRuntimeValue value ++ ")"
renderRuntimeValue (RTuple values) = "(" ++ joinValues values ++ ")"

-- | 连接复合值的字段文本
joinValues :: [RuntimeValue] -> String
joinValues [] = ""
joinValues [value] = renderRuntimeValue value
joinValues (value : values) = renderRuntimeValue value ++ ", " ++ joinValues values

-- | 提取身份中的逻辑类型
typeExpression :: TypeId -> TypeExpr
typeExpression (TypeId t) = t

-- | 解析类型实例并校验 kind
resolveType :: TypeRegistry -> TypeExpr -> Either String TypeDescriptor
resolveType registry@(TypeRegistry constructors) expression@(TypeApply cid args) = do
    (name, kind) <- case [(n, k) | (c, n, k) <- constructors, c == cid] of
        [entry] -> Right entry
        _ -> Left ("unknown type constructor: " ++ show cid)
    children <- mapM (resolveType registry) args
    physical <- case (cid, kind, map descriptorPhysical children) of
        (TypeConstructorId 1, TypeKind, []) -> Right SignedInteger
        (TypeConstructorId 2, TypeKind, []) -> Right FloatingPoint
        (TypeConstructorId 3, TypeKind, []) -> Right Boolean
        (TypeConstructorId 4, TypeKind, []) -> Right Utf8
        (TypeConstructorId 5, TypeArrow TypeKind TypeKind, [child]) -> Right (Sequence child)
        (TypeConstructorId 6, TypeArrow TypeKind TypeKind, [child]) -> Right (Optional child)
        (TypeConstructorId 7, ProductKind, fields) -> Right (Product fields)
        _ -> Left ("invalid type constructor application: " ++ name)
    Right (TypeDescriptor (TypeId expression) name physical)

-- | 按身份查询类型描述
describeType :: TypeRegistry -> TypeId -> Either String TypeDescriptor
describeType registry = resolveType registry . typeExpression

-- | 校验逻辑类型对应的值
validateValue :: TypeRegistry -> TypeId -> RuntimeValue -> Either String ()
validateValue registry tid value = do
    descriptor <- describeType registry tid
    validatePhysical (descriptorPhysical descriptor) value

-- | 校验值的物理结构
validatePhysical :: PhysicalType -> RuntimeValue -> Either String ()
validatePhysical SignedInteger RInt{} = Right ()
validatePhysical FloatingPoint (RDouble n)
    | not (isNaN n || isInfinite n) = Right ()
validatePhysical Boolean RBool{} = Right ()
validatePhysical Utf8 RString{} = Right ()
validatePhysical (Sequence child) (RList values) = mapM_ (validatePhysical child) values
validatePhysical (Optional _) (RMaybe Nothing) = Right ()
validatePhysical (Optional child) (RMaybe (Just value)) = validatePhysical child value
validatePhysical (Product fields) (RTuple values)
    | length fields == length values = sequence_ (zipWith validatePhysical fields values)
validatePhysical physical _ = Left ("value does not fit physical type: " ++ show physical)

-- | 编码带版本与类型身份的值
encodeValue :: TypeRegistry -> TypeId -> RuntimeValue -> Either String BS.ByteString
encodeValue registry tid value = do
    validateValue registry tid value
    Right (encode (runtimeToJSON tid value))

-- | 表达协议中的类型化值封装
runtimeToJSON :: TypeId -> RuntimeValue -> Value
runtimeToJSON tid value = array [Number 1, typeJson (typeExpression tid), valueJson value]

-- | 解析并校验协议类型化值
runtimeFromJSON :: TypeRegistry -> Value -> Either String (TypeId, RuntimeValue)
runtimeFromJSON registry json = do
    (expression, payload) <- parseEither parseEnvelope json
    descriptor <- resolveType registry expression
    value <- parseEither (parseValue (descriptorPhysical descriptor)) payload
    validateValue registry (descriptorId descriptor) value
    Right (descriptorId descriptor, value)

-- | 解码并校验版本和类型身份
decodeValue :: TypeRegistry -> TypeId -> BS.ByteString -> Either String RuntimeValue
decodeValue registry expected bytes = do
    json <- eitherDecode bytes
    (expression, payload) <- parseEither parseEnvelope json
    descriptor <- resolveType registry expression
    if descriptorId descriptor /= expected then Left "encoded value type identity mismatch" else do
        value <- parseEither (parseValue (descriptorPhysical descriptor)) payload
        validateValue registry expected value
        Right value

-- | 编码版本化类型身份
encodeType :: TypeId -> BS.ByteString
encodeType tid = encode (array [Number 1, typeJson (typeExpression tid)])

-- | 解码并解析类型身份
decodeType :: TypeRegistry -> BS.ByteString -> Either String TypeId
decodeType registry bytes = do
    json <- eitherDecode bytes
    expression <- parseEither parseTypeEnvelope json
    descriptorId <$> resolveType registry expression

-- | 生成 JSON 数组
array :: [Value] -> Value
array = Array . V.fromList

-- | 编码构造器应用
typeJson :: TypeExpr -> Value
typeJson (TypeApply (TypeConstructorId cid) args) = array [Number (fromIntegral cid), array (map typeJson args)]

-- | 解析构造器应用
parseTypeJson :: Value -> Parser TypeExpr
parseTypeJson (Array values) = case V.toList values of
    [Number n, Array args] -> case floatingOrInteger n :: Either Double Integer of
        Right cid | cid > 0, cid <= fromIntegral (maxBound :: Int) -> TypeApply (TypeConstructorId (fromInteger cid)) <$> mapM parseTypeJson (V.toList args)
        _ -> fail "invalid type constructor identity"
    _ -> fail "invalid type expression"
parseTypeJson _ = fail "invalid type expression"

-- | 解析类型版本封装
parseTypeEnvelope :: Value -> Parser TypeExpr
parseTypeEnvelope (Array values) = case V.toList values of
    [Number 1, expression] -> parseTypeJson expression
    _ -> fail "unsupported type encoding version or invalid envelope"
parseTypeEnvelope _ = fail "invalid type envelope"

-- | 解析值版本封装
parseEnvelope :: Value -> Parser (TypeExpr, Value)
parseEnvelope (Array values) = case V.toList values of
    [Number 1, expression, payload] -> do
        ty <- parseTypeJson expression
        pure (ty, payload)
    _ -> fail "unsupported value encoding version or invalid envelope"
parseEnvelope _ = fail "invalid value envelope"

-- | 编码物理值
valueJson :: RuntimeValue -> Value
valueJson (RInt n) = Number (fromIntegral n)
valueJson (RDouble n) = Number (realToFrac n)
valueJson (RBool b) = Bool b
valueJson (RString s) = String (T.pack s)
valueJson (RList values) = array (map valueJson values)
valueJson (RMaybe Nothing) = array []
valueJson (RMaybe (Just value)) = array [valueJson value]
valueJson (RTuple values) = array (map valueJson values)

-- | 按物理描述解析值
parseValue :: PhysicalType -> Value -> Parser RuntimeValue
parseValue SignedInteger (Number n) = case floatingOrInteger n :: Either Double Integer of
    Right i | i >= fromIntegral (minBound :: Int), i <= fromIntegral (maxBound :: Int) -> pure (RInt (fromInteger i))
    _ -> fail "integer is fractional or out of range"
parseValue FloatingPoint (Number n) = pure (RDouble (realToFrac n))
parseValue Boolean (Bool b) = pure (RBool b)
parseValue Utf8 (String s) = pure (RString (T.unpack s))
parseValue (Sequence child) (Array values) = RList <$> mapM (parseValue child) (V.toList values)
parseValue (Optional child) (Array values) = case V.toList values of
    [] -> pure (RMaybe Nothing)
    [value] -> RMaybe . Just <$> parseValue child value
    _ -> fail "invalid optional value"
parseValue (Product fields) (Array values)
    | length fields == V.length values = RTuple <$> sequence (zipWith parseValue fields (V.toList values))
parseValue _ _ = fail "invalid physical value"
