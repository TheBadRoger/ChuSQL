{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Bootstrap (preinstalledTypes, runBootstrap) where

import ChuSQL.Core.Model (ColumnType (..), typeName)
import ChuSQL.Core.Protocol (Account (..))
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import Data.Foldable (toList)
import Data.Text (Text)
import qualified Data.Text as T

-- 引导系统身份与预装类型，校验已有定义并保留口令。

-- | 现有内置类型及其默认参数
preinstalledTypes :: [A.Value]
preinstalledTypes = map definition
    [ ("int", CInt, False), ("integer", CInt, False)
    , ("bigint", CBigInt, False), ("smallint", CSmallInt, False)
    , ("varchar", CStr, True), ("char", CChar 1, True)
    , ("text", CStr, False), ("str", CStr, False)
    , ("boolean", CBool, False), ("bool", CBool, False)
    , ("float", CFloat, False), ("real", CFloat, False), ("double", CDouble, False)
    , ("decimal", CDecimal 10 0, True), ("numeric", CDecimal 10 0, True)
    , ("date", CDate, False), ("timestamp", CTimestamp, False), ("blob", CBlob, False)
    ]
  where
    -- | 编码类型的名称与基础表示
    definition :: (Text, ColumnType, Bool) -> A.Value
    definition (name, base, parameterized) = A.object
        ["name" A..= name, "base_type" A..= typeName base, "parameterized" A..= parameterized]

-- | 依次引导身份、迁移目录并预装类型
runBootstrap :: (A.Value -> IO (Either String A.Value)) -> Text -> Text -> IO (Either String Int)
runBootstrap send name encoded = do
    initialized <- send (A.object ["method" A..= ("bootstrap_system" :: Text), "user" A..= name, "password_hash" A..= encoded])
    case expect "system" initialized >>= initializedCatalog of
        Left err -> pure (Left err)
        Right () -> do
            migrated <- send (A.object ["method" A..= ("identity_initialize" :: Text), "administrator" A..= name])
            case expect "accounts" migrated >>= administrator of
                Left err -> pure (Left err)
                Right () -> do
                    installed <- send (A.object ["method" A..= ("bootstrap_types" :: Text), "types" A..= preinstalledTypes])
                    pure (expect "rows" installed >>= installedTypes)
  where
    -- | 确认系统目录已初始化
    initializedCatalog fields
        | KM.lookup "initialized" fields == Just (A.Bool True) = Right ()
        | otherwise = Left "system catalog is not initialized"
    -- | 确认所选引导身份可以管理系统
    administrator fields = case KM.lookup "accounts" fields of
        Nothing -> Left "missing identity directory"
        Just value -> case A.fromJSON value :: A.Result [Account] of
            A.Error _ -> Left "invalid identity directory"
            A.Success accounts
                | any (\a -> accountUser a == T.toLower name && accountCanLogin a && accountEnabled a && accountIsSuperuser a) accounts -> Right ()
                | otherwise -> Left "bootstrap identity is not an enabled login superuser; existing identities were preserved"
    -- | 确认全部预装类型存在且定义一致
    installedTypes fields = case KM.lookup "rows" fields of
        Just (A.Array rows)
            | all (\definition -> any (matches definition) (toList rows)) preinstalledTypes -> Right (length rows)
            | otherwise -> Left "preinstalled type verification failed"
        _ -> Left "missing preinstalled type directory"
    -- | 比对类型定义并检查持久化编号
    matches (A.Object definition) (A.Object saved) = all (\(key, value) -> KM.lookup key saved == Just value) (KM.toList definition)
        && case KM.lookup "id" saved of Just (A.Number n) -> n > 0; _ -> False
    matches _ _ = False

-- | 校验响应状态并返回明确错误
expect :: Text -> Either String A.Value -> Either String (KM.KeyMap A.Value)
expect _ (Left err) = Left err
expect status (Right (A.Object fields))
    | KM.lookup "status" fields == Just (A.String status) = Right fields
    | Just (A.String err) <- KM.lookup "message" fields = Left (T.unpack err)
    | otherwise = Left ("unexpected bootstrap response; expected " ++ T.unpack status)
expect _ (Right _) = Left "invalid bootstrap response"
