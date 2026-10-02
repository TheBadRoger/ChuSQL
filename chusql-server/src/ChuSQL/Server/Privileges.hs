{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.Privileges (
    Grant (..),
    Membership (..),
    PrivilegeCommand (..),
    PrivilegeError (..),
    Privileges,
    RoleView (..),
    authorize,
    authorizeTables,
    filterTables,
    listRoleViews,
    newPrivileges,
    normalizeObject,
    privilegeCommand,
    runPrivilegeCommand,
) where

import ChuSQL.Core.Model (Row, Value (..))
import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Server.Accounts (Principal, principalIsRoot, principalName)
import ChuSQL.Interface.Actions (sqlLiteral)
import ChuSQL.Interface.Protocol (Grant (..), RoleView (..))
import ChuSQL.Server.Backend (
    Backend (..),
    StatementResult (..),
    exprTables,
    grantsTable,
    membersTable,
    rolesTable,
    systemDatabaseName,
    tableRefsOf,
 )
import Control.Concurrent.MVar (MVar, newMVar, withMVar)
import Data.List (intercalate, isInfixOf, nub, sort)
import Data.Text (Text)
import qualified Data.Text as T

-- 角色与权限服务：角色、授权与成员关系落在 system 库的表里，并据此判权限。

-- | 出错口径与账号服务一致
data PrivilegeError = PrivilegeError Text Text
    deriving (Show, Eq)

-- 一条角色授权（定义在共享协议层）
-- | 角色成员资格
data Membership = Membership
    { memberRole :: Text
    , memberUser :: Text
    }
    deriving (Show, Eq, Ord)

-- | 一个角色的全貌：授权与成员

data PrivilegeCommand
    = CreateRoleCommand Text
    | DropRoleCommand Text
    | GrantPrivilegesCommand [Text] Text Text
    | RevokePrivilegesCommand [Text] Text Text
    | GrantRoleCommand Text [Text]
    | RevokeRoleCommand Text [Text]
    deriving (Show, Eq)

data Privileges = Privileges
    { pvBackend :: Backend
    , pvLock :: MVar ()
    }

-- | 建权限服务，固定在 system 库上
newPrivileges :: Backend -> IO Privileges
newPrivileges base = do
    lock <- newMVar ()
    pure (Privileges (beWithDatabase base systemDatabaseName) lock)

-- | 语句转管理命令，不认的给 Nothing
privilegeCommand :: Statement -> Maybe PrivilegeCommand
privilegeCommand stmt = case stmt of
    CreateRole name -> Just (CreateRoleCommand (T.pack name))
    DropRole name -> Just (DropRoleCommand (T.pack name))
    GrantPrivileges privs obj role ->
        Just (GrantPrivilegesCommand (map T.toLower (map T.pack privs)) (T.pack obj) (T.pack role))
    RevokePrivileges privs obj role ->
        Just (RevokePrivilegesCommand (map T.toLower (map T.pack privs)) (T.pack obj) (T.pack role))
    GrantRole role members -> Just (GrantRoleCommand (T.pack role) (map T.pack members))
    RevokeRole role members -> Just (RevokeRoleCommand (T.pack role) (map T.pack members))
    _ -> Nothing

-- | 执行管理命令：只有管理员能跑，命令提到的角色必须已经存在
runPrivilegeCommand :: Privileges -> Principal -> Text -> PrivilegeCommand -> IO (Either PrivilegeError ())
runPrivilegeCommand service principal database command
    | not (principalIsRoot principal) = pure (Left forbidden)
    | needsDatabase = pure (Left (PrivilegeError "no_database" "no database selected"))
    | otherwise = withTables service (applyCommand service database command)
  where
    -- | 授权到具体对象时需要先选库
    needsDatabase = T.null database && case command of
        GrantPrivilegesCommand _ object _ -> T.strip object /= "*"
        RevokePrivilegesCommand _ object _ -> T.strip object /= "*"
        _ -> False

-- | 语句鉴权：管理员全通，普通身份按需查
authorize :: Privileges -> Principal -> Text -> Statement -> IO (Either PrivilegeError ())
authorize service principal database stmt
    | principalIsRoot principal = pure (Right ())
    | otherwise = case requiredActions stmt of
        Nothing -> pure (Left forbidden)
        Just needed -> authorizeTables service principal database needed

-- | 直接查一组 (表, 权限)：一键接口用这个
authorizeTables :: Privileges -> Principal -> Text -> [(Text, Text)] -> IO (Either PrivilegeError ())
authorizeTables service principal database needed
    | principalIsRoot principal = pure (Right ())
    | otherwise = withTables service $ do
        grants <- grantsOfUser service (principalName principal)
        pure (grants >>= \found -> checkAll database found needed)

-- | 只留下用户有 SELECT 权的表
filterTables :: Privileges -> Principal -> Text -> [Text] -> IO (Either PrivilegeError [Text])
filterTables service principal database tables
    | principalIsRoot principal = pure (Right tables)
    | otherwise = withTables service $ do
        grants <- grantsOfUser service (principalName principal)
        pure ((\found -> [table | table <- tables, checkAll database found [(table, "select")] == Right ()]) <$> grants)

-- | 角色总览（REST 与测试用）
listRoleViews :: Privileges -> IO (Either PrivilegeError [RoleView])
listRoleViews service = withTables service $ do
    names <- roleNames service
    case names of
        Left err -> pure (Left err)
        Right known -> sequence <$> mapM view (sort known)
  where
    -- | 组装一个角色的总览
    view role = do
        grants <- roleGrantsOf service role
        members <- membersOfRole service role
        pure (RoleView role <$> grants <*> members)

-- | 授权对象落库口径：`*` 原样，裸表名按当前库补全
normalizeObject :: Text -> Text -> Text
normalizeObject database object
    | T.strip object == "*" = "*"
    | T.any (== '.') object = T.toLower (T.strip object)
    | otherwise = normalizeUser database <> "." <> normalizeUser object

-- | 一条语句需要哪些 (表, 权限)
requiredActions :: Statement -> Maybe [(Text, Text)]
requiredActions stmt = case stmt of
    Select{} -> Just (selectNeeds (selectFrom stmt) (maybe [] (: []) (selectWhere stmt)))
    SelectExpr{} ->
        Just (selectNeeds (selectFrom stmt) (maybe [] (: []) (selectWhere stmt) ++ map snd (selectItems stmt)))
    Insert table _ rows -> Just ((T.pack table, "insert") : selectsOf (concat rows))
    Update table assigns cond -> Just ((T.pack table, "update") : selectsOf (map snd assigns ++ maybe [] (: []) cond))
    Delete table cond -> Just ((T.pack table, "delete") : selectsOf (maybe [] (: []) cond))
    _ -> Nothing
  where
    -- | FROM 子句要的 SELECT 权限
    selectNeeds source exprs = [(ref, "select") | ref <- nub (tableRefsOf source)] ++ selectsOf exprs
    -- | 表达式里子查询要的 SELECT 权限
    selectsOf exprs = [(ref, "select") | ref <- nub (concatMap exprTables exprs)]

-- | 每项 (表, 权限) 都要被某条授权覆盖
checkAll :: Text -> [Grant] -> [(Text, Text)] -> Either PrivilegeError ()
checkAll database grants needed = case [pair | pair@(table, privilege) <- needed, not (covered table privilege)] of
    [] -> Right ()
    ((table, privilege) : _) ->
        Left (PrivilegeError "forbidden" ("permission denied: " <> T.toUpper privilege <> " ON " <> table))
  where
    -- | 某一项是否被授权覆盖
    covered table privilege =
        let target = normalizeObject database table
         in any
                (\grant -> (grantObject grant == "*" || grantObject grant == target) && grantPrivilege grant == privilege)
                grants

-- | 一个用户实际能用的授权：他所属角色身上的那些
grantsOfUser :: Privileges -> Text -> IO (Either PrivilegeError [Grant])
grantsOfUser service user = do
    members <- readRows service membersTable
    rows <- readRows service grantsTable
    pure $ do
        memberRows <- members
        grantRows <- rows
        let mine = [normalizeRole (cell "role" row) | row <- memberRows, normalizeUser (cell "member" row) == normalizeUser user]
        pure [grantOf row | row <- grantRows, normalizeRole (cell "role" row) `elem` mine]

-- | 一个角色挂着的授权
roleGrantsOf :: Privileges -> Text -> IO (Either PrivilegeError [Grant])
roleGrantsOf service role = do
    rows <- readRows service grantsTable
    pure $ fmap (map grantOf . filter (\row -> normalizeRole (cell "role" row) == normalizeRole role)) rows

-- | 角色名清单（统一小写）
roleNames :: Privileges -> IO (Either PrivilegeError [Text])
roleNames service = do
    rows <- readRows service rolesTable
    pure (map (normalizeRole . cell "name") <$> rows)

-- | 一个角色的成员清单
membersOfRole :: Privileges -> Text -> IO (Either PrivilegeError [Text])
membersOfRole service role = do
    rows <- readRows service membersTable
    pure $
        fmap
            (map (normalizeUser . cell "member") . filter (\row -> normalizeRole (cell "role" row) == normalizeRole role))
            rows

-- | 角色必须存在，否则命令不落地
requireRole :: Privileges -> Text -> IO (Either PrivilegeError ())
requireRole service role = do
    known <- roleNames service
    pure $ case known of
        Left err -> Left err
        Right names
            | normalizeRole role `elem` names -> Right ()
            | otherwise -> Left (unknownRole role)

-- | 真正落库（表已经保证存在）
applyCommand :: Privileges -> Text -> PrivilegeCommand -> IO (Either PrivilegeError ())
applyCommand service database command = case command of
    CreateRoleCommand role -> do
        known <- roleNames service
        case known of
            Left err -> pure (Left err)
            Right names
                | normalizeRole role `elem` names -> pure (Left (PrivilegeError "conflict" ("role already exists: " <> role)))
                | otherwise -> exec service (insertSql rolesTable ["name"] [normalizeRole role])
    DropRoleCommand role -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () -> do
                first <- exec service ("DELETE FROM " ++ rolesTable ++ " WHERE name = " ++ roleLiteral role)
                second <- exec service ("DELETE FROM " ++ grantsTable ++ " WHERE role = " ++ roleLiteral role)
                third <- exec service ("DELETE FROM " ++ membersTable ++ " WHERE role = " ++ roleLiteral role)
                pure (sequence_ [first, second, third])
    GrantPrivilegesCommand privileges object role -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () -> each (expandPrivileges privileges) $ \privilege -> do
                let target = normalizeObject database object
                cleared <- exec service (deleteGrantSql role privilege target)
                case cleared of
                    Left err -> pure (Left err)
                    Right () ->
                        exec service (insertSql grantsTable ["role", "privilege", "object"] [normalizeRole role, privilege, target])
    RevokePrivilegesCommand privileges object role -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () ->
                each (expandPrivileges privileges) $ \privilege ->
                    exec service (deleteGrantSql role privilege (normalizeObject database object))
    GrantRoleCommand role users -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () ->
                each users $ \user -> do
                    known <- membersOfRole service role
                    case known of
                        Left err -> pure (Left err)
                        Right members
                            | normalizeUser user `elem` members -> pure (Right ())
                            | otherwise ->
                                exec service (insertSql membersTable ["role", "member"] [normalizeRole role, normalizeUser user])
    RevokeRoleCommand role users -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () ->
                each users $ \user ->
                    exec
                        service
                        ( "DELETE FROM " ++ membersTable ++ " WHERE role = " ++ roleLiteral role
                            ++ " AND member = " ++ literal (normalizeUser user)
                        )

-- | 建表只在第一次用时做；已经有了就算成功
withTables :: Privileges -> IO (Either PrivilegeError a) -> IO (Either PrivilegeError a)
withTables service action = withMVar (pvLock service) $ \_ -> do
    ready <- ensureTables service
    case ready of
        Left err -> pure (Left err)
        Right () -> action

-- | 确保三张内部表存在
ensureTables :: Privileges -> IO (Either PrivilegeError ())
ensureTables service = each tableDefs (createTable service)
  where
    -- | 三张内部表的定义
    tableDefs =
        [ (rolesTable, "(name VARCHAR(64))")
        , (grantsTable, "(role VARCHAR(64), privilege VARCHAR(16), object VARCHAR(128))")
        , (membersTable, "(role VARCHAR(64), member VARCHAR(64))")
        ]

-- | 建一张内部表，已存在算成功
createTable :: Privileges -> (String, String) -> IO (Either PrivilegeError ())
createTable service (table, columns) = do
    result <- beStatement (pvBackend service) ("CREATE TABLE " ++ table ++ " " ++ columns)
    pure $ case result of
        Right _ -> Right ()
        Left err
            | "exists" `isInfixOf` err -> Right ()
            | otherwise -> Left (storageError err)

-- | 跑一条内部写语句
exec :: Privileges -> String -> IO (Either PrivilegeError ())
exec service sql = do
    result <- beStatement (pvBackend service) sql
    pure $ case result of
        Left err -> Left (storageError err)
        Right _ -> Right ()

-- | 读一张内部表
readRows :: Privileges -> String -> IO (Either PrivilegeError [Row])
readRows service table = do
    result <- beStatement (pvBackend service) ("SELECT * FROM " ++ table)
    pure $ case result of
        Left err -> Left (storageError err)
        Right res -> Right (srRows res)

-- | 行里取一列文本
cell :: String -> Row -> Text
cell column row = case lookup column row of
    Just (VStr value) -> T.pack value
    _ -> ""

-- | 一行记录转成授权
grantOf :: Row -> Grant
grantOf row =
    Grant
        { grantRole = normalizeRole (cell "role" row)
        , grantPrivilege = T.toLower (T.strip (cell "privilege" row))
        , grantObject = T.toLower (T.strip (cell "object" row))
        }

-- | 拼一条删授权的 SQL
deleteGrantSql :: Text -> Text -> Text -> String
deleteGrantSql role privilege object =
    "DELETE FROM " ++ grantsTable ++ " WHERE role = " ++ roleLiteral role
        ++ " AND privilege = " ++ literal privilege ++ " AND object = " ++ literal object

-- | 拼一条插入的 SQL
insertSql :: String -> [String] -> [Text] -> String
insertSql table columns values =
    "INSERT INTO " ++ table ++ " (" ++ intercalate ", " columns ++ ") VALUES (" ++ intercalate ", " (map literal values) ++ ")"

-- | 把 ALL 展开成四种具体权限
expandPrivileges :: [Text] -> [Text]
expandPrivileges privileges = nub (concatMap expand (map (T.toLower . T.strip) privileges))
  where
    -- | 展开一条权限名
    expand "all" = ["select", "insert", "update", "delete"]
    expand other = [other]

-- | 一串动作挨着跑，遇到第一个错误就停
each :: [a] -> (a -> IO (Either PrivilegeError ())) -> IO (Either PrivilegeError ())
each [] _ = pure (Right ())
each (item : rest) action = do
    result <- action item
    case result of
        Left err -> pure (Left err)
        Right () -> each rest action

-- | 角色名的 SQL 字面量
roleLiteral :: Text -> String
roleLiteral = literal . normalizeRole

-- | 文本值的 SQL 字面量
literal :: Text -> String
literal = sqlLiteral . VStr . T.unpack

-- | 角色名统一小写去空白
normalizeRole :: Text -> Text
normalizeRole = T.toLower . T.strip

-- | 用户名统一小写去空白
normalizeUser :: Text -> Text
normalizeUser = T.toLower . T.strip

forbidden :: PrivilegeError
forbidden = PrivilegeError "forbidden" "administrator required"

-- | 未知角色的错误
unknownRole :: Text -> PrivilegeError
unknownRole role = PrivilegeError "not_found" ("unknown role: " <> role)

-- | 存储错误转成权限错误
storageError :: String -> PrivilegeError
storageError message = PrivilegeError "storage_error" ("privilege storage: " <> T.pack message)
