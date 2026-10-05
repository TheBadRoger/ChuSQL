{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Server.ObjectACL
    ( objectAuthorize, objectGrant, objectRevoke, objectViews, objectClaim
    , objectReconcile, objectAffected, objectOwner, objectConnect, objectPrepareCreate, objectUsers
    ) where

import ChuSQL.Core.Protocol (Account (..), TableInfo (..))
import ChuSQL.Interface.Protocol (Grant (..))
import ChuSQL.Server.Accounts (Principal, principalIsRoot, principalName)
import ChuSQL.Server.Backend (Backend (..))
import ChuSQL.Server.Catalog (Catalog, readIdentities, readMembers, readGrants, readGrantOptions)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.Aeson as A
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (nub)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import GHC.Generics (Generic)

-- 稳定对象权限目录：保存所有者、授予来源并计算有效授权链。

data Target = Target { targetDatabase :: Text, targetTable :: Maybe Text, targetKind :: Text, targetName :: Text }
    deriving (Eq, Show, Generic)
instance ToJSON Target
instance FromJSON Target

data Edge = Edge { edgeRole :: Text, edgeGrantor :: Text, edgeTarget :: Target, edgePrivilege :: Text, edgeOption :: Bool, edgeRoot :: Bool }
    deriving (Eq, Show, Generic)
instance ToJSON Edge
instance FromJSON Edge

data Owner = Owner { ownerRole :: Text, ownerTarget :: Target }
    deriving (Eq, Show, Generic)
instance ToJSON Owner
instance FromJSON Owner

data Pending = Pending { pendingRole :: Text, pendingDatabase :: Text, pendingName :: Text }
    deriving (Eq, Show, Generic)
instance ToJSON Pending
instance FromJSON Pending

data ACL = ACL { aclVersion :: Int, aclEdges :: [Edge], aclOwners :: [Owner], aclClosed :: [Text], aclPending :: [Pending] }
    deriving (Eq, Show, Generic)
instance ToJSON ACL
instance FromJSON ACL where
    -- 读取权限快照并兼容无声明的旧记录。
    parseJSON = A.withObject "ACL" $ \fields -> ACL <$> fields A..: "aclVersion" <*> fields A..: "aclEdges"
        <*> fields A..: "aclOwners" <*> fields A..: "aclClosed" <*> (fields A..:? "aclPending" A..!= [])

data Context = Context { contextIdentities :: [Account], contextMembers :: [(Text, Text)], contextTargets :: [Target] }

-- 解析存储响应并保留明确错误。
request :: Backend -> A.Value -> IO (Either String A.Value)
request backend payload = fmap (>>= response) (beStorage backend payload)
  where
    -- 检查响应状态。
    response value@(A.Object fields) = case KM.lookup "status" fields of
        Just (A.String "error") -> case KM.lookup "message" fields of Just (A.String message) -> Left (T.unpack message); _ -> Left "invalid storage error"
        Just (A.String _) -> Right value
        _ -> Left "invalid storage response"
    response _ = Left "invalid storage response"

-- 从响应取必需字段。
field :: Text -> A.Value -> Either String A.Value
field name (A.Object fields) = maybe (Left ("missing field: " ++ T.unpack name)) Right (KM.lookup (K.fromText name) fields)
field _ _ = Left "invalid storage object"

-- 将存储编号规范为文本。
identifier :: A.Value -> Either String Text
identifier (A.Number number) | number > 0 = Right (T.pack (BL.unpack (A.encode (A.Number number))))
identifier _ = Left "invalid object identifier"

-- 按 catalog 解析数据库或表对象。
resolve :: Backend -> Text -> Text -> IO (Either String Target)
resolve backend database raw = do
    let object = T.toLower (T.strip raw)
        (selected, table, kind, label) =
            if "database:" `T.isPrefixOf` object then (T.drop 9 object, Nothing, "database", object)
            else if object == "*" || ".*" `T.isSuffixOf` object then
                let scope = if object == "*" then database else T.dropEnd 2 object
                in (scope, Nothing, "tables", scope <> ".*")
            else case T.splitOn "." object of
                [name] -> (database, Just name, "table", database <> "." <> name)
                [db, name] -> (db, Just name, "table", object)
                _ -> ("", Just "", "table", object)
    if T.null selected then pure (Left "no database selected") else do
        result <- request backend (A.object (["method" A..= ("resolve_object" :: Text), "database" A..= selected] ++ maybe [] (\name -> ["table" A..= name]) table))
        pure $ do
            value <- result
            dbId <- field "database_id" value >>= identifier
            tableId <- case table of Nothing -> Right Nothing; Just _ -> Just <$> (field "object_id" value >>= identifier)
            if selected == "system" then Left "system objects cannot be granted" else Right (Target dbId tableId kind label)

-- 展开当前 catalog 中的稳定业务对象。
targets :: Backend -> IO (Either String [Target])
targets backend = do
    names <- beDatabases backend
    case names of
        Left err -> pure (Left err)
        Right databases -> fmap (fmap concat . sequence) $ mapM one (filter (/= "system") databases)
  where
    -- 展开一个数据库和它的表。
    one name = do
        database <- resolve backend "" ("database:" <> T.pack name)
        catalog <- beCatalog (beWithDatabase backend name)
        case catalog of
            Left err -> pure (Left err)
            Right infos -> do
                tables <- mapM (resolve backend (T.pack name) . T.pack . tableNameOf) infos
                pure ((:) <$> database <*> sequence tables)
    -- 取数据字典的表名。
    tableNameOf = tiTable

-- 读取身份、成员边和对象字典。
context :: Backend -> Catalog -> IO (Either String Context)
context backend catalog = do
    identities <- readIdentities catalog
    members <- readMembers catalog
    objects <- targets backend
    pure (Context <$> identities <*> members <*> objects)

-- 查找身份的稳定编号。
roleId :: Context -> Text -> Either String Text
roleId env name = case [T.pack (show (accountId account)) | account <- contextIdentities env, accountUser account == T.toLower name] of
    [value] -> Right value
    [] | T.toLower (T.strip name) == "public" -> Right "public"
    _ -> Left ("unknown role: " ++ T.unpack name)

-- 从编号查回公开身份名。
roleName :: Context -> Text -> Text
roleName _ "public" = "public"
roleName env value = case [accountUser account | account <- contextIdentities env, T.pack (show (accountId account)) == value] of name : _ -> name; [] -> ""

-- 判断身份当前是否贡献权限。
active :: Context -> Text -> Bool
active _ "0" = True
active _ "public" = True
active env value = any (\account -> T.pack (show (accountId account)) == value && accountEnabled account) (contextIdentities env)

-- 判断稳定身份是否仍存在。
knownRole :: Context -> Text -> Bool
knownRole env value = value `elem` ["0", "public"] || any (\account -> T.pack (show (accountId account)) == value) (contextIdentities env)

-- 展开一个身份继承的有效角色编号。
roles :: Context -> Text -> [Text]
roles env value = nub ("public" : go [value])
  where
    pairs = [(parent, child) | (parentName, childName) <- contextMembers env, Right parent <- [roleId env parentName], Right child <- [roleId env childName], active env parent, active env child]
    -- 展开成员关系的向上闭包。
    go known = let more = [parent | (parent, child) <- pairs, child `elem` known, parent `notElem` known]
               in if null more then known else go (nub (known ++ more))

-- 判断一个对象范围是否覆盖目标。
covers :: Target -> Target -> Bool
covers source target = targetDatabase source == targetDatabase target &&
    (targetKind source == "tables" && targetKind target `elem` ["tables", "table"] || targetKind source == targetKind target && targetTable source == targetTable target)

-- 判断稳定对象当前是否存在。
exists :: Context -> Target -> Bool
exists env target = any same (contextTargets env)
  where
    -- 核对数据库或表的稳定编号。
    same current = targetDatabase current == targetDatabase target &&
        (targetKind target == "tables" && targetKind current == "database" || targetKind current == targetKind target && targetTable current == targetTable target)

-- 判断身份是否拥有指定对象。
owns :: Context -> ACL -> Text -> Target -> Bool
owns env acl identity target = any (\owner -> ownerRole owner `elem` roles env identity && ownerTarget owner == target && active env (ownerRole owner)) (aclOwners acl)

-- 计算仍有合法授予来源的授权闭包。
effective :: Context -> ACL -> ACL
effective env acl = acl {aclOwners = owners, aclEdges = grow roots, aclClosed = [value | value <- aclClosed acl, any (\target -> targetKind target == "database" && targetDatabase target == value) (contextTargets env)]}
  where
    owners = [owner | owner <- aclOwners acl, knownRole env (ownerRole owner), exists env (ownerTarget owner)]
    candidates = [edge | edge <- aclEdges acl, knownRole env (edgeRole edge), knownRole env (edgeGrantor edge), exists env (edgeTarget edge)]
    roots = [edge | edge <- candidates, edgeRoot edge]
    -- 从独立授予根展开转授权链。
    grow known = let more = [edge | edge <- candidates, edge `notElem` known, supported known edge]
                 in if null more then known else grow (known ++ more)
    -- 检查授予者的所有权或有效转授权。
    supported known edge = active env (edgeGrantor edge) && (owns env acl{aclOwners = owners} (edgeGrantor edge) (edgeTarget edge) || any (\source -> active env (edgeRole source) && edgeOption source && edgeRole source `elem` roles env (edgeGrantor edge) && edgePrivilege source == edgePrivilege edge && covers (edgeTarget source) (edgeTarget edge)) known)

-- 读取并校验权限快照。
readACL :: Backend -> IO (Either String (Maybe Text, Maybe ACL))
readACL backend = do
    result <- request backend (A.object ["method" A..= ("object_acl_read" :: Text)])
    pure $ do
        value <- result
        rows <- field "rows" value
        case rows of
            A.Array values -> case foldr (:) [] values of
              [] -> Right (Nothing, Nothing)
              [row] -> do
                payload <- field "payload" row
                case payload of
                    A.String text -> case A.eitherDecodeStrict (TE.encodeUtf8 text) of
                        Left err -> Left ("invalid object ACL: " ++ err)
                        Right acl | aclVersion acl == 1 -> Right (Just text, Just acl)
                        Right _ -> Left "unsupported object ACL version"
                    _ -> Left "invalid object ACL payload"
              _ -> Left "invalid object ACL rows"
            _ -> Left "invalid object ACL rows"

-- 原子写入权限快照并检查并发版本。
writeACL :: Backend -> Maybe Text -> ACL -> IO (Either String ())
writeACL backend expected acl = fmap (fmap (const ())) $ request backend (A.object ["method" A..= ("object_acl_replace" :: Text), "expected" A..= expected, "payload" A..= TE.decodeUtf8 (BL.toStrict (A.encode acl))])

-- 将旧名称授权一次性绑定到现存对象。
migrate :: Backend -> Catalog -> Context -> IO (Either String ACL)
migrate backend catalog env = do
    grants <- readGrants catalog
    options <- readGrantOptions catalog
    case (,) <$> grants <*> options of
        Left err -> pure (Left err)
        Right (rows, optionRows) -> fmap (fmap (\edges -> ACL 1 (concat edges) [] [] []) . sequence) (mapM (one optionRows) rows)
  where
    -- 转换一条旧授权记录。
    one options grant = case roleId env (grantRole grant) of
        Left _ -> pure (Right [])
        Right identity -> do
            objects <- if grantObject grant == "*" then pure (Right [target | target <- contextTargets env, targetKind target == "table"])
                else fmap (fmap (: [])) (resolve backend "" (grantObject grant))
            let option = any (\row -> grantRole row == grantRole grant && grantPrivilege row == grantPrivilege grant && grantObject row == grantObject grant) options
            pure $ case objects of
                Left "unknown table" -> Right []
                Left "unknown database" -> Right []
                Left err -> Left err
                Right found -> Right [Edge identity "0" target (grantPrivilege grant) option True | target <- found]

-- 取得迁移完成且来源有效的权限快照。
load :: Backend -> Catalog -> IO (Either String (Context, Maybe Text, ACL))
load backend catalog = do
    env <- context backend catalog
    stored <- readACL backend
    case (,) <$> env <*> stored of
        Left err -> pure (Left err)
        Right (current, (previous, Just acl)) | null (aclPending acl) -> pure (Right (current, previous, effective current acl))
        Right (current, (previous, Just acl)) -> do
            let recovered = [Owner (pendingRole pending) target | pending <- aclPending acl, target <- contextTargets current,
                    targetKind target == "table", targetDatabase target == pendingDatabase pending, targetName target == pendingName pending]
                next = acl{aclPending = [], aclOwners = recovered ++ [owner | owner <- aclOwners acl, ownerTarget owner `notElem` map ownerTarget recovered]}
            written <- writeACL backend previous (effective current next)
            case written of Left err -> pure (Left err); Right () -> load backend catalog
        Right (current, (previous, Nothing)) -> do
            migrated <- migrate backend catalog current
            case migrated of
                Left err -> pure (Left err)
                Right acl -> do
                    written <- writeACL backend previous acl
                    case written of
                        Left "object ACL changed concurrently" -> load backend catalog
                        Left err -> pure (Left err)
                        Right () -> load backend catalog

-- 检查作用域权限或所有权。
allowed :: Context -> ACL -> Text -> Target -> Text -> Bool -> Bool
allowed env acl identity target privilege option = owns env acl identity target ||
    (not option && privilege == "connect" && targetKind target == "database" && targetDatabase target `notElem` aclClosed acl) ||
    any (\edge -> active env (edgeRole edge) && edgeRole edge `elem` roles env identity && edgePrivilege edge == privilege && covers (edgeTarget edge) target && (not option || edgeOption edge)) (aclEdges acl)

-- 按对象类型展开和校验权限名称。
privileges :: Target -> [Text] -> Either String [Text]
privileges target names =
    if all (`elem` supported) expanded then Right expanded else Left "invalid privilege for object scope"
  where
    supported = if targetKind target == "database" then ["connect", "create"] else ["select", "insert", "update", "delete"]
    expanded = nub (concatMap (\name -> if name == "all" then supported else [name]) (map T.toLower names))

-- 检查一组稳定对象上的业务权限。
objectAuthorize :: Backend -> Catalog -> Principal -> Text -> [(Text, Text)] -> IO (Either String ())
objectAuthorize backend catalog principal database needed = do
    loaded <- load backend catalog
    objects <- mapM (resolve backend database . fst) needed
    pure $ do
        (env, _, acl) <- loaded
        found <- sequence objects
        identity <- roleId env (principalName principal)
        if active env identity then Right () else Left "permission denied: disabled identity"
        case [(name, privilege) | ((name, privilege), target) <- zip needed found, not (allowed env acl identity target privilege False)] of
            [] -> Right ()
            (name, privilege) : _ -> Left ("permission denied: " ++ T.unpack (T.toUpper privilege <> " ON " <> name))

-- 检查数据库连接权限。
objectConnect :: Backend -> Catalog -> Principal -> Text -> IO (Either String ())
objectConnect backend catalog principal database = objectAuthorize backend catalog principal "" [("database:" <> database, "connect")]

-- 授予权限并记录授予者的稳定身份。
objectGrant :: Backend -> Catalog -> Principal -> Text -> [Text] -> Text -> Text -> Bool -> IO (Either String ())
objectGrant backend catalog principal database names object role option = do
    loaded <- load backend catalog
    resolved <- resolve backend database object
    case (,) <$> loaded <*> resolved of
        Left err -> pure (Left err)
        Right ((env, previous, acl), target) -> case do
            recipient <- roleId env role
            items <- privileges target names
            grantor <- if principalIsRoot principal then Right (either (const "0") id (roleId env (principalName principal))) else roleId env (principalName principal)
            case [item | item <- items, not (principalIsRoot principal), not (allowed env acl grantor target item True)] of
                item : _ -> Left ("grant option required: " ++ T.unpack (T.toUpper item <> " ON " <> object))
                [] -> Right (recipient, grantor, items) of
                    Left err -> pure (Left err)
                    Right (recipient, grantor, items) -> do
                        let edges = [Edge recipient grantor target item option (principalIsRoot principal) | item <- items]
                            remaining = [edge | edge <- aclEdges acl, not (any (same edge) edges)]
                            promoted = [edge {edgeOption = edgeOption edge || any (\old -> same old edge && edgeOption old) (aclEdges acl)} | edge <- edges]
                        writeACL backend previous acl{aclEdges = remaining ++ promoted, aclClosed = if recipient == "public" && "connect" `elem` items then filter (/= targetDatabase target) (aclClosed acl) else aclClosed acl}
  where
    -- 比较同一授予来源的授权键。
    same one other = edgeRole one == edgeRole other && edgeGrantor one == edgeGrantor other && edgeTarget one == edgeTarget other && edgePrivilege one == edgePrivilege other

-- 撤销授权并收敛失去来源的下游授权。
objectRevoke :: Backend -> Catalog -> Principal -> Text -> [Text] -> Text -> Text -> IO (Either String ())
objectRevoke backend catalog principal database names object role = do
    loaded <- load backend catalog
    resolved <- resolve backend database object
    case (,) <$> loaded <*> resolved of
        Left err -> pure (Left err)
        Right ((env, previous, acl), target) -> case (,) <$> roleId env role <*> privileges target names of
            Left err -> pure (Left err)
            Right (recipient, items) -> do
                let grantor = either (const "") id (roleId env (principalName principal))
                    manager = principalIsRoot principal || owns env acl grantor target
                    -- 选择本次可撤销的授予来源。
                    matches edge = edgeRole edge == recipient && edgeTarget edge == target && edgePrivilege edge `elem` items && (manager || edgeGrantor edge == grantor)
                    next = acl{aclEdges = filter (not . matches) (aclEdges acl), aclClosed = if recipient == "public" && "connect" `elem` items then nub (targetDatabase target : aclClosed acl) else aclClosed acl}
                if recipient == "public" && "connect" `elem` items && not manager then pure (Left "administrator required")
                    else writeACL backend previous (effective env next)

-- 返回公开名称形式的有效授权。
objectViews :: Backend -> Catalog -> Text -> IO (Either String [Grant])
objectViews backend catalog role = do
    loaded <- load backend catalog
    pure $ do
        (env, _, acl) <- loaded
        identity <- roleId env role
        let mine = [edge | edge <- aclEdges acl, edgeRole edge == identity]
        Right (nub [Grant role (edgePrivilege edge) (targetName (edgeTarget edge)) (any (\other -> edgeTarget other == edgeTarget edge && edgePrivilege other == edgePrivilege edge && edgeOption other) mine) | edge <- mine])

-- 登记新对象的创建者为所有者。
objectClaim :: Backend -> Catalog -> Principal -> Text -> Text -> IO (Either String ())
objectClaim backend catalog principal database object = do
    loaded <- load backend catalog
    resolved <- resolve backend database object
    case (,) <$> loaded <*> resolved of
        Left err -> pure (Left err)
        Right ((env, previous, acl), target) -> case roleId env (principalName principal) of
            Left err -> pure (Left err)
            Right identity -> writeACL backend previous acl{aclOwners = Owner identity target : filter ((/= target) . ownerTarget) (aclOwners acl)}

-- 在建表前持久化创建者声明。
objectPrepareCreate :: Backend -> Catalog -> Principal -> Text -> Text -> IO (Either String ())
objectPrepareCreate backend catalog principal database raw = do
    loaded <- load backend catalog
    parent <- resolve backend "" ("database:" <> database)
    case (,) <$> loaded <*> parent of
        Left err -> pure (Left err)
        Right ((env, previous, acl), target) -> case roleId env (principalName principal) of
            Left err -> pure (Left err)
            Right identity -> do
                let name = if "." `T.isInfixOf` raw then T.toLower raw else database <> "." <> T.toLower raw
                if any ((== name) . targetName) (contextTargets env) then pure (Right ())
                    else writeACL backend previous acl{aclPending = [Pending identity (targetDatabase target) name]}

-- 清理失效身份、成员来源和已删除对象。
objectReconcile :: Backend -> Catalog -> IO (Either String ())
objectReconcile backend catalog = do
    loaded <- load backend catalog
    case loaded of Left err -> pure (Left err); Right (_, previous, acl) -> writeACL backend previous acl

-- 列出对象权限变更需失效的身份。
objectAffected :: Backend -> Catalog -> [Text] -> IO (Either String [Text])
objectAffected backend catalog names = do
    loaded <- load backend catalog
    pure $ do
        (env, _, acl) <- loaded
        let edges = [(roleName env (edgeGrantor edge), roleName env (edgeRole edge)) | edge <- aclEdges acl] ++ contextMembers env
            -- 展开成员和转授权的受影响身份。
            walk seen = let more = [child | (parent, child) <- edges, parent `elem` seen, child `notElem` seen, not (T.null child)]
                        in if null more then seen else walk (nub (seen ++ more))
        let normalized = map (T.toLower . T.strip) names
        Right (if "public" `elem` normalized then nub (walk normalized ++ map accountUser (filter (not . accountIsSuperuser) (contextIdentities env))) else walk normalized)

-- 检查稳定对象的所有者权限。
objectOwner :: Backend -> Catalog -> Principal -> Text -> Text -> IO (Either String ())
objectOwner backend catalog principal database object = do
    loaded <- load backend catalog
    resolved <- resolve backend database object
    pure $ do
        (env, _, acl) <- loaded
        target <- resolved
        identity <- roleId env (principalName principal)
        if owns env acl identity target then Right () else Left "object owner required"

-- 展开删除对象时需失效的授权身份。
objectUsers :: Backend -> Catalog -> Text -> Text -> IO (Either String [Text])
objectUsers backend catalog database object = do
    loaded <- load backend catalog
    resolved <- resolve backend database object
    case (,) <$> loaded <*> resolved of
        Left err -> pure (Left err)
        Right ((env, _, acl), target) -> do
            let related = [edgeRole edge | edge <- aclEdges acl, if targetKind target == "database" then targetDatabase (edgeTarget edge) == targetDatabase target else covers (edgeTarget edge) target]
                    ++ [ownerRole owner | owner <- aclOwners acl, ownerTarget owner == target]
            result <- objectAffected backend catalog (nub (map (roleName env) related))
            pure (fmap (filter (\name -> not (any (\account -> accountUser account == name && accountIsSuperuser account) (contextIdentities env)))) result)
