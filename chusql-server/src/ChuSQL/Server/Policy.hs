{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.Policy (
    configurePasswordPolicy,
    policyFromValues,
    passwordKeys,
) where

import ChuSQL.Interface.Policy (defaultPasswordPolicy, passwordKeys, policyFromValues)
import ChuSQL.Interface.Settings (applySettings, defaultOf, readSettingsFile)
import ChuSQL.Server.Accounts (Accounts, setAccountsPolicy)
import qualified Data.Map.Strict as Map

-- 口令策略装配：从设置文件读 password-* 键算出生效策略，并装到账号服务上。

-- | 从设置文件装口令策略，坏值报错
configurePasswordPolicy :: Accounts -> FilePath -> IO ()
configurePasswordPolicy accounts path = do
    saved <- readSettingsFile path
    let values = Map.fromList [(key, Map.findWithDefault (defaultOf key) key saved) | key <- passwordKeys]
    case applySettings Map.empty values of
        Left err -> ioError (userError err)
        Right valid -> setAccountsPolicy accounts (policyFromValues defaultPasswordPolicy valid)
