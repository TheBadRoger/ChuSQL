module ChuSQL.Server.Catalog
    ( Catalog
    , newCatalog
    , normalizeRole
    , normalizeUser
    , normalizeObject
    , withCatalog
    , withCatalogLock
    , ensureCatalog
    , eachAction
    , readRoleNames
    , insertRole
    , dropRole
    , readGrants
    , addGrant
    , removeGrant
    , readMembers
    , addMember
    , removeMember
    ) where

import ChuSQL.Core.Model (Row, Value (..))
import ChuSQL.Interface.Actions (sqlLiteral)
import ChuSQL.Interface.Protocol (Grant (..))
import ChuSQL.Server.Backend (Backend (..), StatementResult (..), grantsTable, membersTable, rolesTable)
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.List (intercalate, isInfixOf)
import Data.Text (Text)
import qualified Data.Text as T

-- 内部目录（系统表）的唯一访问入口，调用方不拼 SQL

-- | 三张系统表的读写都在这里，外面只认下面这些操作
data Catalog = Catalog
    { catBackend :: Backend
    , catLock :: MVar ()
    }

-- | 建一个目录句柄
newCatalog :: Backend -> IO Catalog
newCatalog backend = Catalog backend <$> newMVar ()

-- | 角色名统一小写去空白
normalizeRole :: Text -> Text
normalizeRole = T.toLower . T.strip

-- | 用户名统一小写去空白
normalizeUser :: Text -> Text
normalizeUser = T.toLower . T.strip

-- | 授权对象落库口径：`*` 原样，裸表名按当前库补全
normalizeObject :: Text -> Text -> Text
normalizeObject database object
    | T.strip object == T.pack "*" = T.pack "*"
    | T.any (== '.') object = T.toLower (T.strip object)
    | otherwise = normalizeUser database <> T.pack "." <> normalizeUser object

-- | 加锁跑一段目录动作
withCatalogLock :: Catalog -> IO a -> IO a
withCatalogLock catalog action = withMVar (catLock catalog) (const action)

-- | 建表后跑一段目录动作
withCatalog :: Catalog -> IO (Either String a) -> IO (Either String a)
withCatalog catalog action = withCatalogLock catalog $ do
    ready <- ensureCatalog catalog
    case ready of
        Left err -> pure (Left err)
        Right () -> action

-- | 确保三张系统表存在，已存在算成功
ensureCatalog :: Catalog -> IO (Either String ())
ensureCatalog catalog = eachAction tableDefs (createTable catalog)
  where
    -- | 三张系统表的定义
    tableDefs =
        [ (rolesTable, "(name VARCHAR(64))")
        , (grantsTable, "(role VARCHAR(64), privilege VARCHAR(16), object VARCHAR(128))")
        , (membersTable, "(role VARCHAR(64), member VARCHAR(64))")
        ]

-- | 建一张系统表，已存在算成功
createTable :: Catalog -> (String, String) -> IO (Either String ())
createTable catalog (table, columns) = do
    result <- beStatement (catBackend catalog) ("CREATE TABLE " ++ table ++ " " ++ columns)
    pure $ case result of
        Right _ -> Right ()
        Left err
            | "exists" `isInfixOf` err -> Right ()
            | otherwise -> Left err

-- | 跑一条系统表写语句
exec :: Catalog -> String -> IO (Either String ())
exec catalog sql = fmap (either Left (const (Right ()))) (beStatement (catBackend catalog) sql)

-- | 读一张系统表
readRows :: Catalog -> String -> IO (Either String [Row])
readRows catalog table = fmap (fmap srRows) (beStatement (catBackend catalog) ("SELECT * FROM " ++ table))

-- | 角色名清单
readRoleNames :: Catalog -> IO (Either String [Text])
readRoleNames catalog = fmap (fmap (map (normalizeRole . cell "name"))) (readRows catalog rolesTable)

-- | 新建一个角色
insertRole :: Catalog -> Text -> IO (Either String ())
insertRole catalog role = exec catalog (insertSql rolesTable ["name"] [normalizeRole role])

-- | 删掉一个角色，连同它的授权与成员
dropRole :: Catalog -> Text -> IO (Either String ())
dropRole catalog role = do
    first <- exec catalog ("DELETE FROM " ++ rolesTable ++ " WHERE name = " ++ roleLiteral role)
    second <- exec catalog ("DELETE FROM " ++ grantsTable ++ " WHERE role = " ++ roleLiteral role)
    third <- exec catalog ("DELETE FROM " ++ membersTable ++ " WHERE role = " ++ roleLiteral role)
    pure (sequence_ [first, second, third])

-- | 全部授权
readGrants :: Catalog -> IO (Either String [Grant])
readGrants catalog = fmap (fmap (map grantOf)) (readRows catalog grantsTable)

-- | 记一条授权，同名的先清掉
addGrant :: Catalog -> Text -> Text -> Text -> IO (Either String ())
addGrant catalog role privilege object = do
    cleared <- removeGrant catalog role privilege object
    case cleared of
        Left err -> pure (Left err)
        Right () -> exec catalog (insertSql grantsTable ["role", "privilege", "object"] [normalizeRole role, privilege, object])

-- | 取消一条授权
removeGrant :: Catalog -> Text -> Text -> Text -> IO (Either String ())
removeGrant catalog role privilege object = exec catalog (deleteGrantSql role privilege object)

-- | 全部成员关系
readMembers :: Catalog -> IO (Either String [(Text, Text)])
readMembers catalog = fmap (fmap (map memberOf)) (readRows catalog membersTable)

-- | 记一条成员关系
addMember :: Catalog -> Text -> Text -> IO (Either String ())
addMember catalog role user = exec catalog (insertSql membersTable ["role", "member"] [normalizeRole role, normalizeUser user])

-- | 取消一条成员关系
removeMember :: Catalog -> Text -> Text -> IO (Either String ())
removeMember catalog role user =
    exec catalog ("DELETE FROM " ++ membersTable ++ " WHERE role = " ++ roleLiteral role ++ " AND member = " ++ literal (normalizeUser user))

-- | 一串动作挨着跑，遇到第一个错误就停
eachAction :: [a] -> (a -> IO (Either e ())) -> IO (Either e ())
eachAction [] _ = pure (Right ())
eachAction (item : rest) action = do
    result <- action item
    case result of
        Left err -> pure (Left err)
        Right () -> eachAction rest action

-- | 行里取一列文本
cell :: String -> Row -> Text
cell column row = case lookup column row of
    Just (VStr value) -> T.pack value
    _ -> T.empty

-- | 一行授权记录
grantOf :: Row -> Grant
grantOf row =
    Grant
        { grantRole = normalizeRole (cell "role" row)
        , grantPrivilege = T.toLower (T.strip (cell "privilege" row))
        , grantObject = T.toLower (T.strip (cell "object" row))
        }

-- | 一行成员关系
memberOf :: Row -> (Text, Text)
memberOf row = (normalizeRole (cell "role" row), normalizeUser (cell "member" row))

-- | 拼一条删授权的语句
deleteGrantSql :: Text -> Text -> Text -> String
deleteGrantSql role privilege object =
    "DELETE FROM " ++ grantsTable ++ " WHERE role = " ++ roleLiteral role
        ++ " AND privilege = " ++ literal privilege ++ " AND object = " ++ literal object

-- | 拼一条插入语句
insertSql :: String -> [String] -> [Text] -> String
insertSql table columns values =
    "INSERT INTO " ++ table ++ " (" ++ intercalate ", " columns ++ ") VALUES (" ++ intercalate ", " (map literal values) ++ ")"

-- | 角色名的 SQL 字面量
roleLiteral :: Text -> String
roleLiteral = literal . normalizeRole

-- | 文本值的 SQL 字面量
literal :: Text -> String
literal = sqlLiteral . VStr . T.unpack
