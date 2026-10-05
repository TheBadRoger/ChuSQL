{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.Privileges
    ( Grant (..), Membership (..), PrivilegeCommand (..), PrivilegeError (..), Privileges, RoleView (..)
    , affectedAccounts, authorize, authorizeTables, authorizeConnect, filterTables, listRoleViews
    , newPrivileges, normalizeObject, privilegeCommand, runPrivilegeCommand, claimObject, prepareObject, reconcileObjects, affectedObject
    ) where

import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Protocol (Account (..))
import ChuSQL.Interface.Protocol (Grant (..), RoleView (..))
import ChuSQL.Server.Accounts (Principal, principalIsRoot, principalIsCatalogManager)
import ChuSQL.Server.Backend (Backend (..), exprTables, tableRefsOf, systemDatabaseName)
import ChuSQL.Server.Catalog
import qualified ChuSQL.Server.ObjectACL as OA
import Data.List (nub, sort)
import Data.Text (Text)
import qualified Data.Text as T

-- 权限服务：稳定对象授权、角色继承与受影响会话展开。

data PrivilegeError = PrivilegeError Text Text deriving (Show, Eq)
data Membership = Membership { memberRole :: Text, memberUser :: Text } deriving (Show, Eq, Ord)
data PrivilegeCommand = CreateRoleCommand Text | DropRoleCommand Text
    | GrantPrivilegesCommand [Text] Text Text Bool | RevokePrivilegesCommand [Text] Text Text
    | GrantRoleCommand Text [Text] | RevokeRoleCommand Text [Text] deriving (Show, Eq)
data Privileges = Privileges { pvCatalog :: Catalog, pvBackend :: Backend }

-- 创建权限目录服务。
newPrivileges :: Backend -> IO Privileges
newPrivileges backend = do
    directory <- newCatalog (beWithDatabase backend systemDatabaseName)
    pure (Privileges directory backend)

-- 将管理语句转换为权限命令。
privilegeCommand :: Statement -> Maybe PrivilegeCommand
privilegeCommand statement = case statement of
    CreateRole name -> Just (CreateRoleCommand (T.pack name))
    DropRole name -> Just (DropRoleCommand (T.pack name))
    GrantPrivileges items object role option -> Just (GrantPrivilegesCommand (map (T.toLower . T.pack) items) (T.pack object) (T.pack role) option)
    RevokePrivileges items object role -> Just (RevokePrivilegesCommand (map (T.toLower . T.pack) items) (T.pack object) (T.pack role))
    GrantRole role members -> Just (GrantRoleCommand (T.pack role) (map T.pack members))
    RevokeRole role members -> Just (RevokeRoleCommand (T.pack role) (map T.pack members))
    _ -> Nothing

-- 执行角色管理或有来源记录的对象授权。
runPrivilegeCommand :: Privileges -> Principal -> Text -> PrivilegeCommand -> IO (Either PrivilegeError ())
runPrivilegeCommand service principal database command = withTables service $ case command of
    GrantPrivilegesCommand items object role option -> catalog (OA.objectGrant backend directory principal database items object role option)
    RevokePrivilegesCommand items object role -> catalog (OA.objectRevoke backend directory principal database items object role)
    CreateRoleCommand role
        | principalIsCatalogManager principal -> do
            known <- catalog (readRoleNames directory)
            case known of
                Left err -> pure (Left err)
                Right names | normalizeRole role `elem` names -> pure (Left (PrivilegeError "conflict" ("role already exists: " <> role)))
                            | normalizeRole role == "public" -> pure (Left (PrivilegeError "bad_request" "public is reserved for default privileges"))
                            | otherwise -> catalog (insertRole directory role)
    DropRoleCommand role
        | principalIsRoot principal -> after (catalog (dropRole directory role))
    GrantRoleCommand role members
        | principalIsRoot principal -> do
            known <- catalog (readRoleNames directory)
            edges <- catalog (readMembers directory)
            case (,) <$> known <*> edges of
                Left err -> pure (Left err)
                Right (names, pairs) -> case validateMembers names pairs role members of
                    Left err -> pure (Left err)
                    Right () -> eachAction members $ \member -> if (normalizeRole role, normalizeUser member) `elem` pairs then pure (Right ()) else catalog (addMember directory role member)
    RevokeRoleCommand role members
        | principalIsRoot principal -> after (eachAction members (\member -> catalog (removeMember directory role member)))
    _ -> pure (Left forbidden)
  where
    backend = pvBackend service
    directory = pvCatalog service
    -- 清理成员或身份变更失效的授权链。
    after action = do
        result <- action
        case result of Left err -> pure (Left err); Right () -> catalog (OA.objectReconcile backend directory)

-- 在写入前校验全部成员和循环依赖。
validateMembers :: [Text] -> [(Text, Text)] -> Text -> [Text] -> Either PrivilegeError ()
validateMembers names pairs role members
    | parent `notElem` names = Left (PrivilegeError "not_found" ("unknown role: " <> role))
    | any ((`notElem` names) . normalizeUser) members = Left (PrivilegeError "not_found" "unknown role member")
    | otherwise = foldMembers pairs members
  where
    parent = normalizeRole role
    -- 逐项加入候选成员边并检查闭包。
    foldMembers _ [] = Right ()
    foldMembers edges (member : rest)
        | child `elem` closureUp edges [parent] = Left (PrivilegeError "conflict" ("role membership would create a cycle: " <> parent <> " and " <> child))
        | otherwise = foldMembers ((parent, child) : edges) rest
      where child = normalizeUser member

-- 展开角色的继承闭包。
closureUp :: [(Text, Text)] -> [Text] -> [Text]
closureUp edges known = let more = [parent | (parent, child) <- edges, child `elem` known, parent `notElem` known]
                       in if null more then known else closureUp edges (nub (known ++ more))

-- 展开成员和转授权变更影响的全部身份。
affectedAccounts :: Privileges -> PrivilegeCommand -> IO (Either PrivilegeError [Text])
affectedAccounts service command = withTables service (catalog (OA.objectAffected (pvBackend service) (pvCatalog service) direct))
  where
    direct = case command of
        CreateRoleCommand _ -> []
        DropRoleCommand role -> [role]
        GrantPrivilegesCommand _ _ role _ -> [role]
        RevokePrivilegesCommand _ _ role -> [role]
        GrantRoleCommand _ members -> members
        RevokeRoleCommand _ members -> members

-- 检查 SQL 的对象作用域权限。
authorize :: Privileges -> Principal -> Text -> Statement -> IO (Either PrivilegeError ())
authorize service principal database statement
    | principalIsRoot principal = pure (Right ())
    | otherwise = case statement of
        CreateTable{} -> authorizeTables service principal database [("database:" <> database, "create")]
        DropDatabase name -> owner ("database:" <> T.pack name)
        DropTable name -> owner (T.pack name)
        CreateIndex name _ -> owner (T.pack name)
        DropIndex name _ -> owner (T.pack name)
        DropColumn name _ -> owner (T.pack name)
        AddColumn name _ -> owner (T.pack name)
        RenameColumn name _ _ -> owner (T.pack name)
        AlterColumnType name _ _ -> owner (T.pack name)
        AlterColumnDefault name _ _ -> owner (T.pack name)
        AlterColumnNull name _ _ -> owner (T.pack name)
        ShowDomains -> authorizeConnect service principal database
        ShowDatabases -> pure (Right ())
        _ -> case requiredActions statement of Nothing -> pure (Left forbidden); Just needed -> authorizeTables service principal database needed
  where
    -- 检查结构操作的对象所有权。
    owner object = withTables service (catalog (OA.objectOwner (pvBackend service) (pvCatalog service) principal database object))

-- 检查数据库的 CONNECT 权限。
authorizeConnect :: Privileges -> Principal -> Text -> IO (Either PrivilegeError ())
authorizeConnect service principal database
    | principalIsRoot principal = pure (Right ())
    | otherwise = withTables service (catalog (OA.objectConnect (pvBackend service) (pvCatalog service) principal database))

-- 检查 CONNECT 和表上的业务权限。
authorizeTables :: Privileges -> Principal -> Text -> [(Text, Text)] -> IO (Either PrivilegeError ())
authorizeTables service principal database needed
    | principalIsRoot principal = pure (Right ())
    | T.null database && any (not . T.isPrefixOf "database:" . fst) needed = pure (Left (PrivilegeError "no_database" "no database selected"))
    | otherwise = withTables service $ do
        connected <- if T.null database then pure (Right ()) else catalog (OA.objectConnect (pvBackend service) (pvCatalog service) principal database)
        case connected of Left err -> pure (Left err); Right () -> catalog (OA.objectAuthorize (pvBackend service) (pvCatalog service) principal database needed)

-- 仅返回拥有 SELECT 权限的表名。
filterTables :: Privileges -> Principal -> Text -> [Text] -> IO (Either PrivilegeError [Text])
filterTables service principal database tables
    | principalIsRoot principal = pure (Right tables)
    | otherwise = do
        connected <- authorizeConnect service principal database
        case connected of
            Left err -> pure (Left err)
            Right () -> do
                results <- mapM (\table -> authorizeTables service principal database [(table, "select")]) tables
                pure $ case [err | Left err@(PrivilegeError code _) <- results, code /= "forbidden"] of
                    err : _ -> Left err
                    [] -> Right [table | (table, Right ()) <- zip tables results]

-- 返回角色的有效对象授权和直接成员。
listRoleViews :: Privileges -> IO (Either PrivilegeError [RoleView])
listRoleViews service = withTables service $ do
    names <- catalog (readIdentities (pvCatalog service))
    members <- catalog (readMembers (pvCatalog service))
    case (,) <$> names <*> members of
        Left err -> pure (Left err)
        Right (identities, pairs) -> fmap sequence $ mapM (view pairs) (sort (map accountUser (filter (not . accountCanLogin) identities)))
  where
    -- 组装一个角色的公开权限视图。
    view members name = do
        grants <- catalog (OA.objectViews (pvBackend service) (pvCatalog service) name)
        pure ((\rows -> RoleView name rows [member | (role, member) <- members, role == name]) <$> grants)

-- 登记创建对象的所有者。
claimObject :: Privileges -> Principal -> Text -> Text -> IO (Either PrivilegeError ())
claimObject service principal database object = withTables service (catalog (OA.objectClaim (pvBackend service) (pvCatalog service) principal database object))

-- 持久化即将建表的所有者声明。
prepareObject :: Privileges -> Principal -> Text -> Text -> IO (Either PrivilegeError ())
prepareObject service principal database object = withTables service (catalog (OA.objectPrepareCreate (pvBackend service) (pvCatalog service) principal database object))

-- 获取删除对象影响的授权身份。
affectedObject :: Privileges -> Text -> Text -> IO (Either PrivilegeError [Text])
affectedObject service database object = withTables service (catalog (OA.objectUsers (pvBackend service) (pvCatalog service) database object))

-- 清理身份或对象删除后的权限记录。
reconcileObjects :: Privileges -> IO (Either PrivilegeError ())
reconcileObjects service = withTables service (catalog (OA.objectReconcile (pvBackend service) (pvCatalog service)))

-- 收集数据语句及子查询所需的表权限。
requiredActions :: Statement -> Maybe [(Text, Text)]
requiredActions statement = case statement of
    Select{} -> Just (selectNeeds (selectFrom statement) (maybe [] (: []) (selectWhere statement)))
    SelectExpr{} -> Just (selectNeeds (selectFrom statement) (maybe [] (: []) (selectWhere statement) ++ map snd (selectItems statement)))
    Insert table _ rows -> Just ((T.pack table, "insert") : selectsOf (concat rows))
    Update table assignments condition -> Just ((T.pack table, "update") : selectsOf (map snd assignments ++ maybe [] (: []) condition))
    Delete table condition -> Just ((T.pack table, "delete") : selectsOf (maybe [] (: []) condition))
    _ -> Nothing
  where
    -- 收集来源树和表达式的读取权限。
    selectNeeds source expressions = [(ref, "select") | ref <- nub (tableRefsOf source)] ++ selectsOf expressions
    -- 收集表达式中子查询的读取权限。
    selectsOf expressions = [(ref, "select") | ref <- nub (concatMap exprTables expressions)]

-- 持目录锁并确保旧权限表存在。
withTables :: Privileges -> IO (Either PrivilegeError a) -> IO (Either PrivilegeError a)
withTables service action = withCatalogLock (pvCatalog service) $ do
    ready <- ensureCatalog (pvCatalog service)
    case ready of Left err -> pure (Left (storageError err)); Right () -> action

-- 将目录结果转换为权限错误。
catalog :: IO (Either String a) -> IO (Either PrivilegeError a)
catalog action = fmap (either (Left . storageError) Right) action

-- 分类稳定对象目录的明确错误。
storageError :: String -> PrivilegeError
storageError message = PrivilegeError code (T.pack message)
  where
    code | message == "no database selected" = "no_database"
         | any (`T.isPrefixOf` T.pack message) ["permission denied:", "grant option required:", "object owner required", "administrator required", "system objects cannot be granted"] = "forbidden"
         | any (`T.isPrefixOf` T.pack message) ["unknown role:", "unknown table", "unknown database"] = "not_found"
         | message == "object ACL changed concurrently" = "conflict"
         | message == "invalid privilege for object scope" = "bad_request"
         | message == "the last enabled login superuser cannot be removed" = "bad_request"
         | otherwise = "storage_error"

forbidden :: PrivilegeError
forbidden = PrivilegeError "forbidden" "administrator required"
