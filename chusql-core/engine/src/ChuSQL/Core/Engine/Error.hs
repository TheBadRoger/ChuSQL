module ChuSQL.Core.Engine.Error
    ( ErrorCategory (..)
    , ErrorRule (..)
    , errorRules
    , errorCategory
    , errorCode
    , errorLike
    , accountErrorCode
    ) where

import Data.List (isInfixOf)

-- 引擎与存储错误文本的唯一分类表和协议错误码表

-- | 错误大类
data ErrorCategory
    = NoDatabaseError
    | PermissionError
    | CatalogError
    | TypeError
    | StorageError
    | ProtocolError
    | UnknownError
    deriving (Eq, Show)

-- | 一条识别规则：命中文本片段就算这一类，并给出协议错误码
data ErrorRule = ErrorRule
    { ruleMarker :: String
    , ruleCategory :: ErrorCategory
    , ruleCode :: String
    }

-- | 唯一的错误识别表，顺序即优先级
errorRules :: [ErrorRule]
errorRules =
    [ ErrorRule "no database selected" NoDatabaseError "no_database"
    , ErrorRule "administrator required" PermissionError "forbidden"
    , ErrorRule "unknown table" CatalogError "not_found"
    , ErrorRule "unknown column" CatalogError "query_error"
    , ErrorRule "unknown database" CatalogError "not_found"
    , ErrorRule "unknown role" CatalogError "not_found"
    , ErrorRule "unknown account" CatalogError "not_found"
    , ErrorRule "unknown index" CatalogError "query_error"
    , ErrorRule "unknown user" CatalogError "query_error"
    , ErrorRule "permission denied" PermissionError "query_error"
    , ErrorRule "already exists" CatalogError "query_error"
    , ErrorRule "type error" TypeError "query_error"
    , ErrorRule "cannot put this value into" TypeError "query_error"
    , ErrorRule "division by zero" TypeError "query_error"
    , ErrorRule "overflow" TypeError "query_error"
    , ErrorRule "storage" StorageError "query_error"
    , ErrorRule "storage library call failed" StorageError "query_error"
    , ErrorRule "unsupported protocol" ProtocolError "query_error"
    ]

-- | 命中的第一条规则
errorLike :: String -> Maybe ErrorRule
errorLike message = case [r | r <- errorRules, ruleMarker r `isInfixOf` message] of
    [] -> Nothing
    (r : _) -> Just r

-- | 错误文本属于哪一大类
errorCategory :: String -> ErrorCategory
errorCategory message = maybe UnknownError ruleCategory (errorLike message)

-- | 错误文本对应的协议错误码
errorCode :: String -> String
errorCode message = maybe "query_error" ruleCode (errorLike message)

-- | 账号服务对外的错误码
accountErrorCode :: String -> String
accountErrorCode "account already exists" = "conflict"
accountErrorCode "unknown account" = "not_found"
accountErrorCode _ = "storage_error"
