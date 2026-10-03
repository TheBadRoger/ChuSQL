{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.Interface.Actions (
    ColumnSpec (..),
    CreateTableSpec (..),
    isIdentifier,
    columnTypeOf,
    columnTypeName,
    coerceValue,
    sqlLiteral,
    escapeStringLiteral,
    createDatabaseSql,
    dropDatabaseSql,
    createTableSql,
    dropTableSql,
    createIndexSql,
    dropIndexSql,
    dropColumnSql,
    insertRowSql,
    insertRowsSql,
    updateRowSql,
    deleteRowSql,
    selectRowsSql,
    createUserSql,
    alterUserSql,
    dropUserSql,
    createRoleSql,
    dropRoleSql,
    grantPrivilegesSql,
    revokePrivilegesSql,
    grantRoleSql,
    revokeRoleSql,
) where

import ChuSQL.Core.Model (Column (..), ColumnType (..), Value (..), integerType, numericType, parseColumnType, plainColumn, typeName)
import qualified ChuSQL.Core.Model as M
import qualified Data.Aeson as A
import Data.Char (isAsciiLower, isAsciiUpper, isDigit)
import Data.List (intercalate)
import Data.Scientific (floatingOrInteger)
import Data.Text (Text)
import qualified Data.Text as T
import Text.Read (readMaybe)

-- 把界面上的一键操作翻译成安全 SQL：标识符过白名单，值一律转义。

-- | 界面上传来的一列：名字与类型名
data ColumnSpec = ColumnSpec
    { csName :: Text
    , csType :: Text
    }
    deriving (Show, Eq)

-- | 建表请求：表名 + 列
data CreateTableSpec = CreateTableSpec
    { ctsTable :: Text
    , ctsColumns :: [ColumnSpec]
    }
    deriving (Show, Eq)

-- | 表名/列名的白名单
isIdentifier :: Text -> Bool
isIdentifier t =
    not (T.null t)
        && T.length t <= 64
        && case T.unpack t of
            (c : cs) -> (isAsciiLower c || isAsciiUpper c || c == '_') && all restChar cs
            [] -> False
  where
    -- | 标识符里其余字符的合法集
    restChar c = isAsciiLower c || isAsciiUpper c || isDigit c || c == '_'

-- | 检查一个标识符，不合法就报错（带上"是表名还是列名"）
checkIdent :: Text -> Text -> Either String Text
checkIdent what name
    | isIdentifier name = Right name
    | otherwise = Left (T.unpack what ++ " name is not a plain identifier: " ++ T.unpack name)

-- | 列表不能空
requireNonEmpty :: String -> [a] -> Either String [a]
requireNonEmpty what xs
    | null xs = Left (what ++ " must not be empty")
    | otherwise = Right xs

-- | 线上列类型名（和数据字典里的写法一致）转本地类型
columnTypeOf :: Text -> Maybe Column
columnTypeOf = fmap plainColumn . parseColumnType . T.unpack . T.strip

-- | 本地类型转线上名字
columnTypeName :: Column -> Text
columnTypeName = T.pack . typeName . columnType

-- | 字符串字面量转义：单引号翻倍（引擎的词法器就是这么折回去的）
escapeStringLiteral :: String -> String
escapeStringLiteral = concatMap (\c -> if c == '\'' then "''" else [c])

-- | 值转 SQL 字面量
sqlLiteral :: Value -> String
sqlLiteral VNull = "NULL"
sqlLiteral (VInt n) = show n
sqlLiteral (VFloat d) = show d
sqlLiteral (VStr s) = "'" ++ escapeStringLiteral s ++ "'"
sqlLiteral (VBool True) = "TRUE"
sqlLiteral (VBool False) = "FALSE"

-- | JSON 值按列类型转成引擎的值；类型对不上就报错
coerceValue :: Column -> A.Value -> Either String Value
coerceValue col v = case v of
    A.Null -> Right VNull
    A.Bool b -> M.coerceValue t (VBool b)
    A.Number n -> case floatingOrInteger n :: Either Double Integer of
        Right i -> M.coerceValue t (VInt (fromIntegral i))
        Left d -> M.coerceValue t (VFloat d)
    A.String s
        | integerType t -> case readMaybe (T.unpack (T.strip s)) :: Maybe Int of
            Just i -> M.coerceValue t (VInt i)
            Nothing -> Left ("expected an integer, got: " ++ T.unpack s)
        | numericType t -> case readMaybe (T.unpack (T.strip s)) :: Maybe Double of
            Just d -> M.coerceValue t (VFloat d)
            Nothing -> Left ("expected a number, got: " ++ T.unpack s)
        | t == CBool -> case T.toLower (T.strip s) of
            "true" -> Right (VBool True)
            "false" -> Right (VBool False)
            _ -> Left ("expected true/false, got: " ++ T.unpack s)
        | otherwise -> M.coerceValue t (VStr (T.unpack s))
    _ -> Left "unsupported value"
  where
    t = columnType col

-- | 构造 CREATE DATABASE
createDatabaseSql :: Text -> Either String String
createDatabaseSql name = do
    db <- checkIdent "database" name
    pure ("CREATE DATABASE " ++ T.unpack db)

-- | 构造 DROP DATABASE
dropDatabaseSql :: Text -> Either String String
dropDatabaseSql name = do
    db <- checkIdent "database" name
    pure ("DROP DATABASE " ++ T.unpack db)

-- | 构造 CREATE TABLE 语句
createTableSql :: CreateTableSpec -> Either String String
createTableSql spec = do
    tbl <- checkIdent "table" (ctsTable spec)
    cols <- requireNonEmpty "columns" (ctsColumns spec)
    defs <- mapM columnDef cols
    pure ("CREATE TABLE " ++ T.unpack tbl ++ " (" ++ intercalate ", " defs ++ ")")
  where
    -- | 列类型不认时报错
    columnDef (ColumnSpec name ty) = do
        col <- checkIdent "column" name
        ctype <- maybe (Left ("unknown column type: " ++ T.unpack ty)) Right (columnTypeOf ty)
        pure (T.unpack col ++ " " ++ T.unpack (columnTypeName ctype))

-- | 构造 DROP TABLE 语句
dropTableSql :: Text -> Either String String
dropTableSql table = do
    tbl <- checkIdent "table" table
    pure ("DROP TABLE " ++ T.unpack tbl)

-- | 构造 CREATE INDEX 语句
createIndexSql :: Text -> Text -> Either String String
createIndexSql table column = do
    tbl <- checkIdent "table" table
    col <- checkIdent "column" column
    pure ("CREATE INDEX ON " ++ T.unpack tbl ++ " (" ++ T.unpack col ++ ")")

-- | 构造 DROP INDEX 语句
dropIndexSql :: Text -> Text -> Either String String
dropIndexSql table column = do
    tbl <- checkIdent "table" table
    col <- checkIdent "column" column
    pure ("DROP INDEX ON " ++ T.unpack tbl ++ " (" ++ T.unpack col ++ ")")

-- | 删列：列定义、索引与数据一起没
dropColumnSql :: Text -> Text -> Either String String
dropColumnSql table column = do
    tbl <- checkIdent "table" table
    col <- checkIdent "column" column
    pure ("ALTER TABLE " ++ T.unpack tbl ++ " DROP COLUMN " ++ T.unpack col)

-- | 构造单行 INSERT 语句
insertRowSql :: Text -> [(String, Value)] -> Either String String
insertRowSql table row = do
    tbl <- checkIdent "table" table
    cols <- requireNonEmpty "values" row
    names <- mapM (checkIdent "column" . T.pack . fst) cols
    pure
        ( "INSERT INTO "
            ++ T.unpack tbl
            ++ " ("
            ++ intercalate ", " (map T.unpack names)
            ++ ") VALUES ("
            ++ intercalate ", " (map (sqlLiteral . snd) cols)
            ++ ")"
        )

-- | 一次插多行，N 行只落一次盘
insertRowsSql :: Text -> [[(String, Value)]] -> Either String String
insertRowsSql table rows = do
    tbl <- checkIdent "table" table
    rs <- requireNonEmpty "rows" rows
    case rs of
        [] -> Left "rows must not be empty"
        (first : _) -> do
            names <- mapM (checkIdent "column" . T.pack . fst) first
            let widths = map length rs
            if any (/= length first) widths
                then Left "all rows must have the same columns"
                else
                    pure
                        ( "INSERT INTO "
                            ++ T.unpack tbl
                            ++ " ("
                            ++ intercalate ", " (map T.unpack names)
                            ++ ") VALUES "
                            ++ intercalate ", " ["(" ++ intercalate ", " (map (sqlLiteral . snd) r) ++ ")" | r <- rs]
                        )

-- | 构造按 id 改行的 UPDATE 语句
updateRowSql :: Text -> Int -> [(String, Value)] -> Either String String
updateRowSql table key assigns = do
    tbl <- checkIdent "table" table
    cols <- requireNonEmpty "values" assigns
    sets <- mapM (\(n, v) -> (\c -> T.unpack c ++ " = " ++ sqlLiteral v) <$> checkIdent "column" (T.pack n)) cols
    pure ("UPDATE " ++ T.unpack tbl ++ " SET " ++ intercalate ", " sets ++ " WHERE id = " ++ show key)

-- | 按 id 删一行，存储层原地删
deleteRowSql :: Text -> Int -> Either String String
deleteRowSql table key = do
    tbl <- checkIdent "table" table
    pure ("DELETE FROM " ++ T.unpack tbl ++ " WHERE id = " ++ show key)

-- | 浏览一张表的 SELECT，带排序与等值过滤
selectRowsSql :: Text -> Maybe (Text, Bool) -> [(Text, Value)] -> Either String String
selectRowsSql table order filters = do
    tbl <- checkIdent "table" table
    whereSql <- whereClause filters
    orderSql <- orderClause order
    pure ("SELECT * FROM " ++ T.unpack tbl ++ whereSql ++ orderSql)

-- | 拼 WHERE 子句，无条件给空串
whereClause :: [(Text, Value)] -> Either String String
whereClause filters = case filters of
    [] -> pure ""
    _ -> do
        terms <- mapM term filters
        pure (" WHERE " ++ intercalate " AND " terms)
  where
    -- | 一个等值条件
    term (column, value) = do
        col <- checkIdent "column" column
        pure (T.unpack col ++ " = " ++ sqlLiteral value)

-- | 拼 ORDER BY 子句，无排序给空串
orderClause :: Maybe (Text, Bool) -> Either String String
orderClause Nothing = pure ""
orderClause (Just (column, ascending)) = do
    col <- checkIdent "column" column
    pure (" ORDER BY " ++ T.unpack col ++ (if ascending then " ASC" else " DESC"))

-- | 账号名：标识符规则另允许点与短横线
accountName :: Text -> Either String Text
accountName name
    | T.null name || T.length name > 64 = Left "account name is empty or too long"
    | not (T.all allowed name) = Left ("account name may only contain ASCII letters, digits, dot, dash or underscore: " ++ T.unpack name)
    | otherwise = Right name
  where
    -- | 账号名允许的字符集
    allowed c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("_.-" :: String)

-- | 单引号字符串字面量
quoted :: Text -> String
quoted value = "'" ++ escapeStringLiteral (T.unpack value) ++ "'"

-- | 构造 CREATE USER
createUserSql :: Text -> Text -> Either String String
createUserSql name password = do
    user <- accountName name
    if T.null password || T.length password > 256
        then Left "password must not be empty or longer than 256 characters"
        else pure ("CREATE USER " ++ T.unpack user ++ " IDENTIFIED BY " ++ quoted password)

-- | 构造 ALTER USER（改一个普通账号的口令）
alterUserSql :: Text -> Text -> Either String String
alterUserSql name password = do
    user <- accountName name
    if T.null password || T.length password > 256
        then Left "password must not be empty or longer than 256 characters"
        else pure ("ALTER USER " ++ T.unpack user ++ " IDENTIFIED BY " ++ quoted password)

-- | 构造 DROP USER
dropUserSql :: Text -> Either String String
dropUserSql name = do
    user <- accountName name
    pure ("DROP USER " ++ T.unpack user)

-- | 角色名：与账号名同一套字符规则
roleName :: Text -> Either String Text
roleName name
    | T.null name || T.length name > 64 = Left "role name is empty or too long"
    | not (T.all allowed name) = Left ("role name may only contain ASCII letters, digits, dot, dash or underscore: " ++ T.unpack name)
    | otherwise = Right name
  where
    -- | 角色名允许的字符集
    allowed c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("_.-" :: String)

-- | 授权对象：`*`，或长度不超过 128 的裸表名/`库.表`
grantObject :: Text -> Either String Text
grantObject object
    | T.strip object == "*" = Right "*"
    | T.null object || T.length object > 128 = Left "object name is empty or too long"
    | not (T.all allowed object) = Left ("object name may only contain ASCII letters, digits, dot, dash or underscore: " ++ T.unpack object)
    | otherwise = Right object
  where
    -- | 授权对象允许的字符集
    allowed c = isAsciiLower c || isAsciiUpper c || isDigit c || c `elem` ("_.-" :: String)

-- | 权限清单：词要纯字母，认不认由 server 定
privilegeWords :: [Text] -> Either String [Text]
privilegeWords privileges
    | null privileges = Left "at least one privilege is required"
    | otherwise = mapM word privileges
  where
    -- | 单个权限词
    word one
        | T.null one || not (T.all (\c -> isAsciiLower c || isAsciiUpper c) one) = Left ("not a privilege keyword: " ++ T.unpack one)
        | otherwise = Right one

-- | 构造 CREATE ROLE
createRoleSql :: Text -> Either String String
createRoleSql name = do
    role <- roleName name
    pure ("CREATE ROLE " ++ T.unpack role)

-- | 构造 DROP ROLE
dropRoleSql :: Text -> Either String String
dropRoleSql name = do
    role <- roleName name
    pure ("DROP ROLE " ++ T.unpack role)

-- | 构造 GRANT 权限 ON 对象 TO 角色
grantPrivilegesSql :: [Text] -> Text -> Text -> Bool -> Either String String
grantPrivilegesSql privileges object role withOption = do
    words' <- privilegeWords privileges
    target <- grantObject object
    who <- roleName role
    pure
        ( "GRANT " ++ T.unpack (T.intercalate ", " words') ++ " ON " ++ T.unpack target
            ++ " TO " ++ T.unpack who ++ tailText
        )
  where
    -- | 转授权后缀
    tailText
        | withOption = " WITH GRANT OPTION"
        | otherwise = ""

-- | 构造 REVOKE 权限 ON 对象 FROM 角色
revokePrivilegesSql :: [Text] -> Text -> Text -> Either String String
revokePrivilegesSql privileges object role = do
    words' <- privilegeWords privileges
    target <- grantObject object
    who <- roleName role
    pure ("REVOKE " ++ T.unpack (T.intercalate ", " words') ++ " ON " ++ T.unpack target ++ " FROM " ++ T.unpack who)

-- | 构造 GRANT <角色> TO <用户>[, ...]
grantRoleSql :: Text -> [Text] -> Either String String
grantRoleSql role users = do
    who <- roleName role
    members <- requireNonEmpty "users" users >>= mapM accountName
    pure ("GRANT " ++ T.unpack who ++ " TO " ++ T.unpack (T.intercalate ", " members))

-- | 构造 REVOKE 角色 FROM 用户
revokeRoleSql :: Text -> [Text] -> Either String String
revokeRoleSql role users = do
    who <- roleName role
    members <- requireNonEmpty "users" users >>= mapM accountName
    pure ("REVOKE " ++ T.unpack who ++ " FROM " ++ T.unpack (T.intercalate ", " members))
