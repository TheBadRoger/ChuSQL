module ChuSQL.Core.Engine.Runtime.SQL (resolveSQLFunction, sqlResultType, invokeSQLFunction, sqlType, constructSQLValue) where

import ChuSQL.Core.Engine.Runtime.Types
import ChuSQL.Core.Engine.Runtime.Functions
import ChuSQL.Core.Model (ColumnType (..), Value (..))

-- SQL 现有标量表示与已解析运行时函数之间的类型边界。

-- | 解析 SQL 标量调用的固定签名
resolveSQLFunction :: String -> [Maybe ColumnType] -> Either String FunctionSignature
resolveSQLFunction name arguments = do
    functions <- builtinFunctions builtinTypes
    types <- mapM (traverse sqlType) arguments
    resolveNullableFunction functions name types

-- | 解析现有标量列的逻辑类型
sqlType :: ColumnType -> Either String TypeId
sqlType (CDomain _ base) = sqlType base
sqlType (CRuntime tid) = describeType builtinTypes tid >> Right tid
sqlType ty = do
    expression <- case ty of
        CInt -> Right intType
        CBigInt -> Right intType
        CSmallInt -> Right intType
        CFloat -> Right doubleType
        CDouble -> Right doubleType
        CDecimal{} -> Right doubleType
        CBool -> Right boolType
        CStr -> Right stringType
        CVarchar{} -> Right stringType
        CChar{} -> Right stringType
        _ -> Left ("unsupported runtime function argument type: " ++ show ty)
    descriptorId <$> resolveType builtinTypes expression

-- | 将函数输出类型映射为 SQL schema
sqlResultType :: TypeId -> Either String ColumnType
sqlResultType tid
    | typeExpression tid == intType = Right CInt
    | typeExpression tid == doubleType = Right CDouble
    | typeExpression tid == boolType = Right CBool
    | typeExpression tid == stringType = Right CStr
    | otherwise = Left "runtime function result needs a composite SQL representation"

-- | 执行固定身份的 SQL 标量函数
invokeSQLFunction :: FunctionId -> TypeId -> [Value] -> Either String Value
invokeSQLFunction fid expected values = do
    functions <- builtinFunctions builtinTypes
    sig <- functionSignature functions fid
    if signatureResult sig /= expected then Left "bound function result identity mismatch"
        else if length values /= length (signatureArguments sig) then Left "function argument count mismatch"
        else do
            sequence_ (zipWith validateArgument (signatureArguments sig) values)
            if VNull `elem` values then Right VNull else do
                arguments <- mapM runtimeValue values
                result <- invokeFunction builtinTypes functions fid arguments
                sqlValue result
  where
    -- | 校验具体参数并保留 SQL NULL
    validateArgument _ VNull = Right ()
    validateArgument tid value = runtimeValue value >>= validateValue builtinTypes tid

-- | 转换现有 SQL 标量值
runtimeValue :: Value -> Either String RuntimeValue
runtimeValue (VInt n) = Right (RInt n)
runtimeValue (VFloat n) = Right (RDouble n)
runtimeValue (VBool b) = Right (RBool b)
runtimeValue (VStr text) = Right (RString text)
runtimeValue (VRuntime tid value) = validateValue builtinTypes tid value >> Right value
runtimeValue VNull = Left "untyped SQL NULL has no runtime scalar value"

-- | 转换运行时标量结果
sqlValue :: RuntimeValue -> Either String Value
sqlValue (RInt n) = Right (VInt n)
sqlValue (RDouble n) = Right (VFloat n)
sqlValue (RBool b) = Right (VBool b)
sqlValue (RString text) = Right (VStr text)
sqlValue _ = Left "composite runtime result has no SQL scalar representation"

-- | 按已解析构造器生成类型化值
constructSQLValue :: TypeId -> [Value] -> Either String Value
constructSQLValue tid values = do
    arguments <- mapM runtimeValue values
    value <- case typeExpression tid of
        TypeApply (TypeConstructorId 5) [_] -> Right (RList arguments)
        TypeApply (TypeConstructorId 6) [_] -> case arguments of
            [] -> Right (RMaybe Nothing)
            [argument] -> Right (RMaybe (Just argument))
            _ -> Left "Maybe constructor expects zero or one argument"
        TypeApply (TypeConstructorId 7) _ -> Right (RTuple arguments)
        _ -> Left "type is not a composite constructor"
    validateValue builtinTypes tid value
    Right (VRuntime tid value)
