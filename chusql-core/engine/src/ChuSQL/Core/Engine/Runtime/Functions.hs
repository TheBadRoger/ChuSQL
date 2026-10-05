module ChuSQL.Core.Engine.Runtime.Functions
    ( FunctionId (..), FunctionSignature (..), FunctionRegistry
    , builtinFunctions, resolveFunction, resolveNullableFunction, functionSignature, invokeFunction
    ) where

import ChuSQL.Core.Engine.Runtime.Types
import Data.Char (toLower)

-- 内建函数的固定身份、签名解析和类型化调用。

-- | 标识稳定的内建函数
newtype FunctionId = FunctionId Int deriving (Show, Eq, Ord)

-- | 描述已解析的函数签名
data FunctionSignature = FunctionSignature
    { signatureId :: FunctionId, signatureName :: String
    , signatureArguments :: [TypeId], signatureResult :: TypeId
    } deriving (Show, Eq)

-- | 保存函数签名和实现
newtype FunctionRegistry = FunctionRegistry [(FunctionSignature, [RuntimeValue] -> Either String RuntimeValue)]

-- | 从内建元数据恢复函数注册表
builtinFunctions :: TypeRegistry -> Either String FunctionRegistry
builtinFunctions types = do
    int <- descriptorId <$> resolveType types intType
    bool <- descriptorId <$> resolveType types boolType
    string <- descriptorId <$> resolveType types stringType
    Right (FunctionRegistry
        [(FunctionSignature (FunctionId 1) "length" [string] int, stringLength)
        ,(FunctionSignature (FunctionId 2) "not" [bool] bool, booleanNot)])

-- | 按名称和完整参数类型解析函数
resolveFunction :: FunctionRegistry -> String -> [TypeId] -> Either String FunctionSignature
resolveFunction (FunctionRegistry functions) name arguments =
    case [sig | (sig, _) <- functions, map toLower (signatureName sig) == map toLower name
              , signatureArguments sig == arguments] of
        [sig] -> Right sig
        [] -> Left ("no matching function signature: " ++ name)
        _ -> Left ("ambiguous function signature: " ++ name)

-- | 按可空 SQL 参数解析确定签名
resolveNullableFunction :: FunctionRegistry -> String -> [Maybe TypeId] -> Either String FunctionSignature
resolveNullableFunction (FunctionRegistry functions) name arguments =
    case [sig | (sig, _) <- functions, map toLower (signatureName sig) == map toLower name
              , length arguments == length (signatureArguments sig)
              , and (zipWith matches arguments (signatureArguments sig))] of
        [sig] -> Right sig
        [] -> Left ("no matching function signature: " ++ name)
        _ -> Left ("ambiguous function signature: " ++ name)
  where
    -- | 匹配具体参数或未定型 NULL
    matches Nothing _ = True
    matches (Just supplied) expected = supplied == expected

-- | 按固定身份查找函数签名
functionSignature :: FunctionRegistry -> FunctionId -> Either String FunctionSignature
functionSignature (FunctionRegistry functions) fid = case [sig | (sig, _) <- functions, signatureId sig == fid] of
    [sig] -> Right sig
    _ -> Left ("unknown function identity: " ++ show fid)

-- | 校验参数并执行已解析的函数
invokeFunction :: TypeRegistry -> FunctionRegistry -> FunctionId -> [RuntimeValue] -> Either String RuntimeValue
invokeFunction types (FunctionRegistry functions) fid values =
    case [(sig, implementation) | (sig, implementation) <- functions, signatureId sig == fid] of
        [(sig, implementation)]
            | length values == length (signatureArguments sig) -> do
                sequence_ (zipWith (validateValue types) (signatureArguments sig) values)
                result <- implementation values
                validateValue types (signatureResult sig) result
                Right result
            | otherwise -> Left "function argument count mismatch"
        _ -> Left ("unknown function identity: " ++ show fid)

-- | 计算字符串字符个数
stringLength :: [RuntimeValue] -> Either String RuntimeValue
stringLength [RString text] = Right (RInt (length text))
stringLength _ = Left "length expects String"

-- | 计算布尔取反
booleanNot :: [RuntimeValue] -> Either String RuntimeValue
booleanNot [RBool value] = Right (RBool (not value))
booleanNot _ = Left "not expects Bool"
