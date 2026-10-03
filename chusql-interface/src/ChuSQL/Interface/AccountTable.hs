{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Interface.AccountTable (
    accountTableName,
    isAccountTable,
    passwordColumn,
    accountColumns,
    systemTableInfo,
) where

import ChuSQL.Core.Protocol (SchemaColumn (..), TableInfo (..))
import Data.Text (Text)
import qualified Data.Text as T

-- 账号表（系统表 __system_users）的结构定义，SQL 会话与 Web 管理端共用。

-- | 账号表的名字：网页里像普通表一样浏览与编辑，但写入走账号命令
accountTableName :: String
accountTableName = "__system_users"

-- | 这张表是不是账号表
isAccountTable :: Text -> Bool
isAccountTable name = T.toLower name == T.pack accountTableName

-- | 口令列：密文只在服务端产生，界面不支持排序与筛选
passwordColumn :: String
passwordColumn = "password"

-- | 账号表的列定义与约束
accountColumns :: [SchemaColumn]
accountColumns =
    [ accountColumnSpec "id" "int" False True True False
    , accountColumnSpec "user" "varchar(64)" False False False True
    , accountColumnSpec passwordColumn "varchar(256)" False False False False
    , accountColumnSpec "registered_at" "timestamp" False False False False
    , accountColumnSpec "last_login_at" "timestamp" True False False False
    ]

-- | 账号表的列（约束由账号服务管）
accountColumnSpec :: String -> String -> Bool -> Bool -> Bool -> Bool -> SchemaColumn
accountColumnSpec name ty nullable autoIncrement primary unique =
    SchemaColumn name ty nullable Nothing autoIncrement primary unique Nothing

-- | 账号表的结构（排序校验用）
systemTableInfo :: TableInfo
systemTableInfo = TableInfo accountTableName accountColumns 0 [] [] []
