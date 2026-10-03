{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.Privileges (
    Grant (..),
    Membership (..),
    PrivilegeCommand (..),
    PrivilegeError (..),
    Privileges,
    RoleView (..),
    affectedAccounts,
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
    addGrantOption,
    addMember,
    dropRole,
    eachAction,
    insertRole,
    newCatalog,
    normalizeObject,
    normalizeRole,
    normalizeUser,
    readGrantOptions,
    readGrants,
    readMembers,
    readRoleNames,
    removeGrant,
    removeGrantOption,
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
    | GrantPrivilegesCommand [Text] Text Text Bool
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
    GrantPrivileges privs obj role withOption ->
        Just (GrantPrivilegesCommand (map T.toLower (map T.pack privs)) (T.pack obj) (T.pack role) withOption)
    RevokePrivileges privs obj role ->
        Just (RevokePrivilegesCommand (map T.toLower (map T.pack privs)) (T.pack obj) (T.pack role))
    GrantRole role members -> Just (GrantRoleCommand (T.pack role) (map T.pack members))
    RevokeRole role members -> Just (RevokeRoleCommand (T.pack role) (map T.pack members))
    _ -> Nothing

-- | 执行管理命令：管理员全通，普通身份只能转授自己拿到的权限
runPrivilegeCommand :: Privileges -> Principal -> Text -> PrivilegeCommand -> IO (Either PrivilegeError ())
runPrivilegeCommand service principal database command
    | needsDatabase = pure (Left (PrivilegeError "no_database" "no database selected"))
    | principalIsRoot principal = withTables service (applyCommand service database command)
    | otherwise = withTables service (delegateCommand service principal database command)
  where
    -- | 授权到具体对象时需要先选库
    needsDatabase = T.null database && case command of
        GrantPrivilegesCommand _ object _ _ -> T.strip object /= "*"
        RevokePrivilegesCommand _ object _ -> T.strip object /= "*"
        _ -> False

-- | 一条命令会影响的账号：角色连它的成员一起展开，账号名原样留下
affectedAccounts :: Privileges -> PrivilegeCommand -> IO [Text]
affectedAccounts service command = do
    known <- roleNames service
    case known of
        Left _ -> pure (direct command)
        Right roles -> walk roles [] (direct command)
  where
    -- | 命令直接点到的名字
    direct cmd = case cmd of
        CreateRoleCommand _ -> []
        DropRoleCommand role -> [role]
        GrantPrivilegesCommand _ _ role _ -> [role]
        RevokePrivilegesCommand _ _ role -> [role]
        GrantRoleCommand _ members -> members
        RevokeRoleCommand _ members -> members
    -- | 是角色就接着往下走它的成员，走过的名字不再走第二遍
    walk roles seen [] = pure (reverse seen)
    walk roles seen (name : rest)
        | name `elem` seen = walk roles seen rest
        | normalizeRole name `elem` roles = do
            members <- membersOfRole service name
            case members of
                Left _ -> walk roles (name : seen) rest
                Right found -> walk roles (name : seen) (found ++ rest)
        | otherwise = walk roles (name : seen) rest

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
checkAll database grants needed = checkCovered database grants needed "permission denied: "

-- | 转授权要查 grant option，报错口径与普通鉴权分开
checkOptions :: Text -> [Grant] -> [(Text, Text)] -> Either PrivilegeError ()
checkOptions database grants needed = checkCovered database grants needed "grant option required: "

-- | 覆盖检查的公共实现
checkCovered :: Text -> [Grant] -> [(Text, Text)] -> Text -> Either PrivilegeError ()
checkCovered database grants needed prefix = case [pair | pair@(table, privilege) <- needed, not (covered table privilege)] of
    [] -> Right ()
    ((table, privilege) : _) ->
        Left (PrivilegeError "forbidden" (prefix <> T.toUpper privilege <> " ON " <> table))
  where
    -- | 某一项是否被授权覆盖
    covered table privilege =
        let target = normalizeObject database table
         in any
                (\grant -> (grantObject grant == "*" || grantObject grant == target) && grantPrivilege grant == privilege)
                grants

-- | 一个用户实际能用的授权：含角色继承
grantsOfUser :: Privileges -> Text -> IO (Either PrivilegeError [Grant])
grantsOfUser service user = do
    members <- catalog (readMembers (pvCatalog service))
    grants <- catalog (readGrants (pvCatalog service))
    options <- catalog (readGrantOptions (pvCatalog service))
    roles <- roleNames service
    pure $ do
        memberRows <- members
        grantRows <- grants
        optionRows <- options
        known <- roles
        let edges = roleEdges memberRows known
            mine = closureUp edges [role | (role, member) <- memberRows, member == normalizeUser user]
        pure [grant | grant <- grantRows ++ optionRows, grantRole grant `elem` mine]

-- | 一个用户手里的 grant option，只有这些能再转授
optionsOfUser :: Privileges -> Text -> IO (Either PrivilegeError [Grant])
optionsOfUser service user = fmap (fmap (filter grantable)) (grantsOfUser service user)

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

-- | 一个角色挂着的授权，带 grant option 的会标出来
roleGrantsOf :: Privileges -> Text -> IO (Either PrivilegeError [Grant])
roleGrantsOf service role = do
    grants <- catalog (readGrants (pvCatalog service))
    options <- catalog (readGrantOptions (pvCatalog service))
    pure (do
        rows <- grants
        optionRows <- options
        let mine = filter (\grant -> grantRole grant == normalizeRole role) rows
        pure (markOptions mine optionRows))

-- | 给授权标上有没有对应的 grant option
markOptions :: [Grant] -> [Grant] -> [Grant]
markOptions grants options = [grant {grantable = hasOption grant} | grant <- grants]
  where
    -- | 有没有一条同角色同权限同对象的 option
    hasOption grant = any (same grant) options
    -- | 两条授权指向同一处
    same one other =
        grantRole one == grantRole other
            && grantPrivilege one == grantPrivilege other
            && grantObject one == grantObject other

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
    GrantPrivilegesCommand privileges object role withOption -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () ->
                eachAction (expandPrivileges privileges) $ \privilege ->
                    grantOne service withOption role privilege (normalizeObject database object)
    RevokePrivilegesCommand privileges object role -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () ->
                eachAction (expandPrivileges privileges) $ \privilege ->
                    revokeOne service role privilege (normalizeObject database object)
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

-- | 落一条授权，带 grant option 的再落一条转授权
grantOne :: Privileges -> Bool -> Text -> Text -> Text -> IO (Either PrivilegeError ())
grantOne service withOption role privilege object = do
    written <- catalog (addGrant (pvCatalog service) role privilege object)
    case written of
        Left err -> pure (Left err)
        Right ()
            | withOption -> catalog (addGrantOption (pvCatalog service) role privilege object)
            | otherwise -> pure (Right ())

-- | 收一条授权，连同它的 grant option
revokeOne :: Privileges -> Text -> Text -> Text -> IO (Either PrivilegeError ())
revokeOne service role privilege object = do
    dropped <- catalog (removeGrant (pvCatalog service) role privilege object)
    case dropped of
        Left err -> pure (Left err)
        Right () -> catalog (removeGrantOption (pvCatalog service) role privilege object)

-- | 普通身份转授权：只放行 GRANT
delegateCommand :: Privileges -> Principal -> Text -> PrivilegeCommand -> IO (Either PrivilegeError ())
delegateCommand service principal database command = case command of
    GrantPrivilegesCommand privileges object role _ -> do
        allowed <- requireRole service role
        case allowed of
            Left err -> pure (Left err)
            Right () -> do
                owned <- optionsOfUser service (principalName principal)
                case owned >>= \found -> checkOptions database found needed of
                    Left err -> pure (Left err)
                    Right () -> applyCommand service database command
    _ -> pure (Left forbidden)
  where
    -- | 命令里每个 (对象, 权限) 都要对得上
    needed = case command of
        GrantPrivilegesCommand privileges object _ _ -> [(object, privilege) | privilege <- expandPrivileges privileges]
        _ -> []

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
