{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Interface.Policy (
    PasswordPolicy (..),
    defaultPasswordPolicy,
    passwordKeys,
    policyFromValues,
) where

import ChuSQL.Interface.Settings (defaultOf)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- 口令策略值与策略计算，服务端与 Web 管理端共用。

-- | 口令策略：最短长度与至少几类字符
data PasswordPolicy = PasswordPolicy
    { ppMinLength :: Int
    , ppClasses :: Int
    }
    deriving (Show, Eq)

-- | 默认：至少 12 个字符、至少两类字符
defaultPasswordPolicy :: PasswordPolicy
defaultPasswordPolicy = PasswordPolicy 12 2

-- | 设置文件里决定口令策略的键
passwordKeys :: [Text]
passwordKeys = ["password-min-length", "password-classes"]

-- | 策略值：缺项或空值回落到内置默认
policyFromValues :: PasswordPolicy -> Map.Map Text Text -> PasswordPolicy
policyFromValues before values = PasswordPolicy
    (number "password-min-length" (ppMinLength before)) (number "password-classes" (ppClasses before))
  where
    -- | 取一个键的数值，缺项或空值回落到给定默认
    number key fallback = case Map.lookup key values of
        Nothing -> fallback
        Just value -> fromMaybe fallback (readMaybe (T.unpack (if T.null value then defaultOf key else value)))
