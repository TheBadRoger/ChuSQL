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

import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Server.Accounts (Principal, principalIsRoot, principalName)
import ChuSQL.Interface.Protocol (Grant (..), RoleView (..))
import ChuSQL.Server.Backend (
    Backend (..),
    exprTables,
    systemDatabaseName,
    tableRefsOf,
 )
import ChuSQL.Server.Catalog (
    Catalog,
    addGrant,
    addMember,
    dropRole,
    eachAction,
    insertRole,
    newCatalog,
    normalizeObject,
    normalizeRole,
    normalizeUser,
    readGrants,
    readMembers,
    readRoleNames,
    removeGrant,
    removeMember,
    withCatalogLock,
    ensureCatalog,
 )
import Data.List (nub, sort)
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
    { pvCatalog :: Catalog
    }

-- | 建权限服务，固定在 system 库上
newPrivileges :: Backend -> IO Privileges
newPrivileges base = Privileges <$> newCatalog (beWithDatabase base systemDatabaseName)

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

-- | 一个用户实际能用的授权：所属角色加上继承来的
grantsOfUser :: Privileges -> Text -> IO (Either PrivilegeError [Grant])
grantsOfUser service user = do
    members <- catalog (readMembers (pvCatalog service))
    grants <- catalog (readGrants (pvCatalog service))
    roles <- roleNames service
    pure $ do
        memberRows <- members
        grantRows <- grants
        known <- roles
        let edges = roleEdges memberRows known
            mine = closureUp edges [role | (role, member) <- memberRows, member == normalizeUser user]
        pure [grant | grant <- grantRows, grantRole grant `elem` mine]

-- | 角色到角色的成员边：父角色在前
roleEdges :: [(Text, Text)] -> [Text] -> [(Text, Text)]
roleEdges members roles = [pair | pair@(_, child) <- members, child `elem` roles]

-- | 向上闭包：这群角色直接或间接所属的全部角色
closureUp :: [(Text, Text)] -> [Text] -> [Text]
closureUp edges = go
  where
    -- | 展开一轮，没有再新增就停
    go known =
        let more = [parent | (parent, child) <- edges, child `elem` known, parent `notElem` known]
         in if null more then known else go (known ++ more)

-- | 一个角色挂着的授权
roleGrantsOf :: Privileges -> Text -> IO (Either PrivilegeError [Grant])
roleGrantsOf service role = do
    grants <- catalog (readGrants (pvCatalog service))
    pure (filter (\grant -> grantRole grant == normalizeRole role) <$> grants)

-- | 角色名清单（统一小写）
roleNames :: Privileges -> IO (Either PrivilegeError [Text])
roleNames service = catalog (readRoleNames (pvCatalog service))

-- | 一个角色的直接成员：用户与子角色
membersOfRole :: Privileges -> Text -> IO (Either PrivilegeError [Text])
membersOfRole service role = do
    members <- catalog (readMembers (pvCatalog service))
    pure (map snd . filter (\(name, _) -> name == normalizeRole role) <$> members)

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
                | otherwise -> catalog (insertRole (pvCatalog service) role)
    DropRoleCommand role -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () -> catalog (dropRole (pvCatalog service) role)
    GrantPrivilegesCommand privileges object role -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () ->
                eachAction (expandPrivileges privileges) $ \privilege ->
                    catalog (addGrant (pvCatalog service) role privilege (normalizeObject database object))
    RevokePrivilegesCommand privileges object role -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () ->
                eachAction (expandPrivileges privileges) $ \privilege ->
                    catalog (removeGrant (pvCatalog service) role privilege (normalizeObject database object))
    GrantRoleCommand role members -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () -> do
                known <- roleNames service
                case known of
                    Left err -> pure (Left err)
                    Right roles -> eachAction members (grantMember service roles role)
    RevokeRoleCommand role users -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () -> eachAction users $ \user -> catalog (removeMember (pvCatalog service) role user)

-- | 加一条成员边：名字是角色就查环，否则当用户加
grantMember :: Privileges -> [Text] -> Text -> Text -> IO (Either PrivilegeError ())
grantMember service roles role member = do
    current <- membersOfRole service role
    case current of
        Left err -> pure (Left err)
        Right known
            | normalizeUser member `elem` known -> pure (Right ())
            | normalizeRole member `notElem` roles -> catalog (addMember (pvCatalog service) role member)
            | otherwise -> do
                edges <- membershipEdges service
                case edges of
                    Left err -> pure (Left err)
                    Right pairs
                        | normalizeRole member `elem` closureUp pairs [normalizeRole role] ->
                            pure (Left (cycleMember role member))
                        | otherwise -> catalog (addMember (pvCatalog service) role member)

-- | 只保留两端都是角色的成员边
membershipEdges :: Privileges -> IO (Either PrivilegeError [(Text, Text)])
membershipEdges service = do
    members <- catalog (readMembers (pvCatalog service))
    roles <- roleNames service
    pure (roleEdges <$> members <*> roles)

-- | 全程持锁，先保证系统表存在再跑动作
withTables :: Privileges -> IO (Either PrivilegeError a) -> IO (Either PrivilegeError a)
withTables service action = withCatalogLock (pvCatalog service) $ do
    ready <- ensureCatalog (pvCatalog service)
    case ready of
        Left err -> pure (Left (storageError err))
        Right () -> action

-- | 目录操作转成权限错误
catalog :: IO (Either String a) -> IO (Either PrivilegeError a)
catalog action = fmap (either (Left . storageError) Right) action

-- | 把 ALL 展开成四种具体权限
expandPrivileges :: [Text] -> [Text]
expandPrivileges privileges = nub (concatMap expand (map (T.toLower . T.strip) privileges))
  where
    -- | 展开一条权限名
    expand "all" = ["select", "insert", "update", "delete"]
    expand other = [other]

forbidden :: PrivilegeError
forbidden = PrivilegeError "forbidden" "administrator required"

-- | 未知角色的错误
unknownRole :: Text -> PrivilegeError
unknownRole role = PrivilegeError "not_found" ("unknown role: " <> role)

-- | 角色成员成环的错误
cycleMember :: Text -> Text -> PrivilegeError
cycleMember role member =
    PrivilegeError "conflict" ("role membership would create a cycle: " <> normalizeRole role <> " and " <> normalizeRole member)

-- | 存储错误转成权限错误
storageError :: String -> PrivilegeError
storageError message = PrivilegeError "storage_error" ("privilege storage: " <> T.pack message)
