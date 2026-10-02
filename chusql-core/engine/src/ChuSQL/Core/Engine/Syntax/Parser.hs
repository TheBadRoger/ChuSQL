{-# LANGUAGE ImportQualifiedPost #-}

module ChuSQL.Core.Engine.Syntax.Parser (parseStatement, parseExpression) where

import ChuSQL.Core.Model (Column (..), ColumnType (..), Value (..), plainColumn)
import ChuSQL.Core.Engine.Syntax.AST
import Control.Monad (void)
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isAlpha, isAlphaNum, isHexDigit, isSpace, toLower, toUpper)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lxr

-- SQL 解析：词法 + 语法，把文本变成语句树。

type Parser = Parsec Void String

-- | 跳过空白
sc :: Parser ()
sc = Lxr.space space1 (Lxr.skipLineComment "--") (Lxr.skipBlockComment "/*" "*/")

-- | 词后面吃掉空白
lexeme :: Parser a -> Parser a
lexeme = Lxr.lexeme sc

-- | 读一个符号
symbol :: String -> Parser String
symbol = Lxr.symbol sc

-- | 读关键字（不分大小写）
keyword :: String -> Parser ()
keyword k = lexeme $ try $ do
    _ <- string' k
    notFollowedBy (satisfy isIdentChar)

-- | 读一个名字
identifier :: Parser String
identifier = lexeme $ do
    first <- satisfy (\ch -> isAlpha ch || ch == '_')
    rest <- many (satisfy isIdentChar)
    return (first : rest)

-- | 账号名：裸名字或单引号字符串
userName :: Parser String
userName = identifier <|> stringLit

-- | 名字里允许的字符
isIdentChar :: Char -> Bool
isIdentChar ch = isAlphaNum ch || ch == '_'

-- | 读一个整数
integer :: Parser Int
integer = lexeme Lxr.decimal

-- | 读一个浮点数（必须有小数点或指数）
floatLit :: Parser Double
floatLit = lexeme Lxr.float

-- | 读一个字符串字面量
stringLit :: Parser String
stringLit = lexeme $ do
    _ <- char '\''
    s <- many stringChar
    _ <- char '\''
    return s

-- | 字符串里的一个字符
stringChar :: Parser Char
stringChar =
    choice
        [ '\'' <$ try (string "''")
        , satisfy (/= '\'')
        ]

-- | X'ABCD' 形式的二进制字面量，统一存成大写十六进制文本
blobLit :: Parser String
blobLit = lexeme $ do
    _ <- char 'x' <|> char 'X'
    _ <- char '\''
    s <- many (satisfy isHexDigit)
    _ <- char '\''
    return (map toUpper s)

-- | 升序或降序
sortDir :: Parser SortDir
sortDir = (Desc <$ keyword "desc") <|> (Asc <$ keyword "asc") <|> pure Asc

-- | LIMIT 后面的整数
limitClause :: Parser Int
limitClause = do
    keyword "limit"
    integer

-- | 能带表名的列名
qualifiedName :: Parser String
qualifiedName = do
    first <- identifier
    rest <- many (symbol "." *> identifier)
    return (intercalate "." (first : rest))

-- | 读一个表达式
expr :: Parser Expr
expr = makeExprParser atom operatorTable

-- | 运算符优先级（从紧到松）
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [ [Prefix (Neg <$ symbol "-")]
    , [InfixL (Mul <$ symbol "*"), InfixL (Div <$ symbol "/")]
    , [InfixL (Add <$ symbol "+"), InfixL (Sub <$ symbol "-")]
    , [ InfixN (Gt <$ symbol ">")
        , InfixN (Lt <$ symbol "<")
        , InfixN (Eq <$ symbol "=")
        ]
    , [ Postfix inPredicate
        , Postfix (IsNotNull <$ try (keyword "is" *> keyword "not" *> keyword "null"))
        , Postfix (IsNull <$ (keyword "is" *> keyword "null"))
        ]
    , [InfixL (And <$ keyword "AND")]
    , [InfixL (Or <$ keyword "OR")]
    ]

-- | 最小的表达式单位
atom :: Parser Expr
atom =
    choice
        [ LitNull <$ keyword "null"
        , LitBool True <$ keyword "TRUE"
        , LitBool False <$ keyword "FALSE"
        , try (LitDate <$> (keyword "date" *> stringLit))
        , try (LitTimestamp <$> (keyword "timestamp" *> stringLit))
        , try (LitBlob <$> blobLit)
        , LitFloat <$> try floatLit
        , LitInt <$> integer
        , LitStr <$> stringLit
        , try aggregateCall
        , try existsPredicate
        , Col <$> qualifiedName
        , try scalarSubquery
        , between (symbol "(") (symbol ")") expr
        ]

-- | [NOT] EXISTS (SELECT ...)
existsPredicate :: Parser Expr
existsPredicate = do
    negated <- (True <$ try (keyword "not" *> keyword "exists")) <|> (False <$ keyword "exists")
    sq <- between (symbol "(") (symbol ")") subquerySelect
    return (ExistsSub sq negated)

-- | 标量子查询：(SELECT ...)
scalarSubquery :: Parser Expr
scalarSubquery = ScalarSub <$> between (symbol "(") (symbol ")") subquerySelect

-- | 读 [NOT] IN：子查询或值清单
inPredicate :: Parser (Expr -> Expr)
inPredicate = do
    negated <- (True <$ try (keyword "not" *> keyword "in")) <|> (False <$ keyword "in")
    body <-
        between (symbol "(") (symbol ")") $
            choice
                [ Left <$> try subquerySelect
                , Right <$> sepBy1 expr (symbol ",")
                ]
    return $ \e -> case body of
        Left sq -> InSub e sq negated
        Right es -> InList e es negated

-- | 括号里的子查询；对外层列的引用留给语义检查填
subquerySelect :: Parser Subquery
subquerySelect = Subquery <$> selectStatement <*> pure []

-- | 聚合调用：COUNT/SUM/AVG/MIN/MAX
aggregateCall :: Parser Expr
aggregateCall =
    choice
        [ CountAll <$ try (keyword "count" *> between (symbol "(") (symbol ")") (symbol "*"))
        , CountOf <$> aggArg "count"
        , SumOf <$> aggArg "sum"
        , AvgOf <$> aggArg "avg"
        , MinOf <$> aggArg "min"
        , MaxOf <$> aggArg "max"
        ]
  where
    -- | 读聚合函数的参数
    aggArg name = try (keyword name *> between (symbol "(") (symbol ")") expr)

-- | 只认字面量，给 DEFAULT 用
literalValue :: Parser Value
literalValue =
    choice
        [ VNull <$ keyword "null"
        , VBool True <$ keyword "TRUE"
        , VBool False <$ keyword "FALSE"
        , try (negateLit <$> floatLit)
        , VInt <$> integer
        , VFloat <$> try floatLit
        , VStr <$> stringLit
        ]
  where
    -- | 负浮点字面量
    negateLit d = VFloat (negate d)

-- | 一个排序列
orderItem :: Parser (String, SortDir)
orderItem = do
    col <- qualifiedName
    dir <- sortDir
    return (col, dir)

-- | ORDER BY 的列表
orderByClause :: Parser [(String, SortDir)]
orderByClause = do
    keyword "order"
    keyword "by"
    sepBy1 orderItem (symbol ",")

-- | GROUP BY 的列表
groupByClause :: Parser [String]
groupByClause = do
    keyword "group"
    keyword "by"
    sepBy1 qualifiedName (symbol ",")

-- | SET 里的一条赋值
assignment :: Parser (String, Expr)
assignment = do
    col <- identifier
    _ <- symbol "="
    e <- expr
    return (col, e)

-- | 保留字清单，别名不许用
reservedWords :: [String]
reservedWords =
    [ "select"
    , "as"
    , "from"
    , "where"
    , "order"
    , "by"
    , "limit"
    , "join"
    , "left"
    , "outer"
    , "inner"
    , "exists"
    , "in"
    , "on"
    , "and"
    , "or"
    , "asc"
    , "desc"
    , "insert"
    , "into"
    , "values"
    , "delete"
    , "update"
    , "set"
    , "create"
    , "table"
    , "index"
    , "drop"
    , "alter"
    , "add"
    , "rename"
    , "to"
    , "column"
    , "user"
    , "identified"
    , "null"
    , "is"
    , "not"
    , "default"
    , "check"
    , "unique"
    , "primary"
    , "key"
    , "auto_increment"
    , "type"
    , "group"
    ]

-- | 别名（不许用保留字）
aliasName :: Parser String
aliasName = try $ do
    name <- identifier
    if map toLower name `elem` reservedWords then empty else return name

-- | 表名 + 可选别名
tableRef :: Parser (Maybe String, String)
tableRef = do
    tbl <- tableName
    mAlias <- optional ((keyword "as" *> aliasName) <|> aliasName)
    return (mAlias, tbl)

-- | 连接类型：内连接 / 左外连接
joinKind :: Parser JoinKind
joinKind =
    try (keyword "left" *> optional (keyword "outer") *> keyword "join" *> return LeftJoin)
        <|> try (keyword "inner" *> keyword "join" *> return InnerJoin)
        <|> (keyword "join" *> return InnerJoin)

-- | 一个 JOIN ... ON
joinClause :: Parser (JoinKind, Maybe String, String, Expr)
joinClause = do
    kind <- joinKind
    (mAlias, tbl) <- tableRef
    keyword "on"
    cond <- expr
    return (kind, mAlias, tbl, cond)

-- | FROM 子句（可含多个 JOIN）
fromClause :: Parser FromClause
fromClause = do
    (mAlias, tbl) <- tableRef
    joins <- many joinClause
    return (foldl (\acc (k, a, t, c) -> FromJoin k acc a t c) (FromTable mAlias tbl) joins)

-- | SELECT 的列清单
selectList :: Parser [(String, Expr)]
selectList =
    choice
        [ [("*", Col "*")] <$ symbol "*"
        , sepBy1 selectItem (symbol ",")
        ]

-- | 投影标签保留表达式原文
selectItem :: Parser (String, Expr)
selectItem = do
    (source, e) <- match expr
    pure (case e of Col c -> (c, e); _ -> (reverse (dropWhile isSpace (reverse source)), e))

-- | 读 CREATE TABLE
createTableStatement :: Parser Statement
createTableStatement = do
    keyword "create"
    keyword "table"
    name <- tableName
    cols <- between (symbol "(") (symbol ")") (sepBy1 columnDef (symbol ","))
    return (CreateTable name cols)

-- | 读 DROP TABLE
dropTableStatement :: Parser Statement
dropTableStatement = do
    keyword "drop"
    keyword "table"
    name <- tableName
    return (DropTable name)

-- | 读 CREATE INDEX（列名即索引名）
createIndexStatement :: Parser Statement
createIndexStatement = do
    keyword "create"
    keyword "index"
    keyword "on"
    tbl <- tableName
    col <- between (symbol "(") (symbol ")") identifier
    return (CreateIndex tbl col)

-- | 读 DROP INDEX
dropIndexStatement :: Parser Statement
dropIndexStatement = do
    keyword "drop"
    keyword "index"
    keyword "on"
    tbl <- tableName
    col <- between (symbol "(") (symbol ")") identifier
    return (DropIndex tbl col)

-- | 读 ALTER TABLE 的加列、删列、改名与改属性
alterTableStatement :: Parser Statement
alterTableStatement = do
    keyword "alter"
    keyword "table"
    tbl <- tableName
    choice
        [ do
            keyword "add"
            void (optional (keyword "column"))
            (name, col) <- columnDef
            pure (AddColumn tbl (name, col))
        , do
            keyword "drop"
            void (optional (keyword "column"))
            DropColumn tbl <$> identifier
        , do
            keyword "rename"
            void (optional (keyword "column"))
            old <- identifier
            keyword "to"
            RenameColumn tbl old <$> identifier
        , do
            keyword "alter"
            void (optional (keyword "column"))
            col <- identifier
            choice
                [ AlterColumnType tbl col <$> (keyword "type" *> columnTypeP)
                , try (AlterColumnNull tbl col False <$ (keyword "set" *> keyword "not" *> keyword "null"))
                , try (AlterColumnNull tbl col True <$ (keyword "drop" *> keyword "not" *> keyword "null"))
                , try (AlterColumnDefault tbl col . Just <$> (keyword "set" *> keyword "default" *> literalValue))
                , AlterColumnDefault tbl col Nothing <$ (keyword "drop" *> keyword "default")
                ]
        ]

-- | 读 CREATE USER，口令是明文
createUserStatement :: Parser Statement
createUserStatement = do
    keyword "create"
    keyword "user"
    name <- userName
    pw <- identifiedBy
    return (CreateUser name pw)

-- | 读 IDENTIFIED BY '口令'
identifiedBy :: Parser String
identifiedBy = do
    keyword "identified"
    keyword "by"
    stringLit

-- | 读 ALTER USER 改口令
alterUserStatement :: Parser Statement
alterUserStatement = do
    keyword "alter"
    keyword "user"
    name <- userName
    pw <- identifiedBy
    return (AlterUser name pw)

-- | 读 DROP USER
dropUserStatement :: Parser Statement
dropUserStatement = do
    keyword "drop"
    keyword "user"
    name <- userName
    return (DropUser name)

-- | 读 CREATE ROLE
createRoleStatement :: Parser Statement
createRoleStatement = do
    keyword "create"
    keyword "role"
    CreateRole <$> userName

-- | 读 DROP ROLE
dropRoleStatement :: Parser Statement
dropRoleStatement = do
    keyword "drop"
    keyword "role"
    DropRole <$> userName

-- | 权限名，统一收成大写
privilegeName :: Parser String
privilegeName =
    choice
        [ "SELECT" <$ keyword "select"
        , "INSERT" <$ keyword "insert"
        , "UPDATE" <$ keyword "update"
        , "DELETE" <$ keyword "delete"
        , "ALL" <$ keyword "all"
        ]

-- | ON 后面的授权对象：* 或一张表
grantObject :: Parser String
grantObject = ("*" <$ symbol "*") <|> tableName

-- | 读 GRANT：带 ON 的授权限，不带的加角色
grantStatement :: Parser Statement
grantStatement = do
    keyword "grant"
    choice
        [ try $ do
            privs <- sepBy1 privilegeName (symbol ",")
            keyword "on"
            obj <- grantObject
            keyword "to"
            GrantPrivileges privs obj <$> userName
        , GrantRole <$> userName <*> (keyword "to" *> sepBy1 userName (symbol ","))
        ]

-- | 读 REVOKE：与 GRANT 对称
revokeStatement :: Parser Statement
revokeStatement = do
    keyword "revoke"
    choice
        [ try $ do
            privs <- sepBy1 privilegeName (symbol ",")
            keyword "on"
            obj <- grantObject
            keyword "from"
            RevokePrivileges privs obj <$> userName
        , RevokeRole <$> userName <*> (keyword "from" *> sepBy1 userName (symbol ","))
        ]

-- | 建表时的一列：类型后面跟若干约束
columnDef :: Parser (String, Column)
columnDef = do
    name <- identifier
    ty <- columnTypeP
    mods <- many columnConstraint
    return (name, foldl (\c f -> f c) (plainColumn ty) mods)

-- | 一条列约束
columnConstraint :: Parser (Column -> Column)
columnConstraint =
    choice
        [ try (keyword "not" *> keyword "null" *> pure (\c -> c{columnNullable = False}))
        , keyword "null" *> pure (\c -> c{columnNullable = True})
        , keyword "auto_increment" *> pure (\c -> c{columnAutoIncrement = True, columnNullable = False})
        , try (keyword "primary" *> keyword "key" *> pure (\c -> c{columnPrimaryKey = True, columnUnique = True, columnNullable = False}))
        , keyword "unique" *> pure (\c -> c{columnUnique = True})
        , keyword "default" *> ((\v c -> c{columnDefault = Just v}) <$> literalValue)
        , keyword "check" *> ((\s c -> c{columnCheck = Just s}) <$> checkText)
        ]

-- | CHECK 后面括号里的表达式原文，交给执行期再解析
checkText :: Parser String
checkText = between (symbol "(") (symbol ")") (match expr) >>= \(source, _) -> pure source

-- | 括号里的长度参数
sizeParen :: Parser Int
sizeParen = try (between (symbol "(") (symbol ")") integer)

-- | 列类型关键字
columnTypeP :: Parser ColumnType
columnTypeP =
    choice
        [ keyword "integer" >> pure CInt
        , keyword "int" >> pure CInt
        , keyword "bigint" >> pure CBigInt
        , keyword "smallint" >> pure CSmallInt
        , (keyword "varchar" >>) (maybe CStr CVarchar <$> optional sizeParen)
        , (keyword "char" >>) (maybe (CChar 1) CChar <$> optional sizeParen)
        , keyword "text" >> pure CStr
        , keyword "str" >> pure CStr
        , keyword "boolean" >> pure CBool
        , keyword "bool" >> pure CBool
        , keyword "float" >> pure CFloat
        , keyword "real" >> pure CFloat
        , try (keyword "double" *> keyword "precision" *> pure CDouble)
        , keyword "double" >> pure CDouble
        , (keyword "decimal" >>) decimalType
        , (keyword "numeric" >>) decimalType
        , keyword "date" >> pure CDate
        , keyword "timestamp" >> pure CTimestamp
        , keyword "blob" >> pure CBlob
        ]

-- | DECIMAL 的精度与小数位
decimalType :: Parser ColumnType
decimalType = do
    args <- optional (try (between (symbol "(") (symbol ")") decimalArgs))
    pure $ case args of
        Nothing -> CDecimal 10 0
        Just (p, s) -> CDecimal p s

-- | DECIMAL 的精度与小数位参数
decimalArgs :: Parser (Int, Int)
decimalArgs = do
    p <- integer
    s <- fromMaybe 0 <$> optional (symbol "," *> integer)
    pure (p, s)

-- | 读 SELECT
selectStatement :: Parser Statement
selectStatement = do
    keyword "select"
    cols <- selectList
    fromC <- fromMaybe FromUnit <$> optional (keyword "from" *> fromClause)
    mWhere <- optional (keyword "where" *> expr)
    groupBy <- fromMaybe [] <$> optional groupByClause
    orderBy <- fromMaybe [] <$> optional orderByClause
    mLimit <- optional limitClause
    pure $ if all (\(name, e) -> e == Col name) cols
        then Select (map fst cols) fromC mWhere groupBy orderBy mLimit
        else SelectExpr cols fromC mWhere groupBy orderBy mLimit

-- | 读 INSERT，可一次插多行
insertStatement :: Parser Statement
insertStatement = do
    keyword "INSERT"
    keyword "INTO"
    tbl <- tableName
    cols <-
        between
            (symbol "(")
            (symbol ")")
            (sepBy1 identifier (symbol ","))
    keyword "VALUES"
    rows <- sepBy1 valueRow (symbol ",")
    return (Insert tbl cols rows)

-- | 一行值：一对括号里的若干字面量
valueRow :: Parser [Expr]
valueRow = between (symbol "(") (symbol ")") (sepBy1 expr (symbol ","))

-- | 读 DELETE
deleteStatement :: Parser Statement
deleteStatement = do
    keyword "DELETE"
    keyword "FROM"
    tbl <- tableName
    mWhere <- optional (keyword "WHERE" *> expr)
    return (Delete tbl mWhere)

-- | 读 UPDATE
updateStatement :: Parser Statement
updateStatement = do
    keyword "update"
    tbl <- tableName
    keyword "set"
    assigns <- sepBy1 assignment (symbol ",")
    mWhere <- optional (keyword "where" *> expr)
    return (Update tbl assigns mWhere)

-- | 解析总入口
parseStatement :: String -> Either String Statement
parseStatement input =
    case runParser (sc *> statementP <* sc <* eof) "<query>" input of
        Left err -> Left (errorBundlePretty err)
        Right q -> Right q
  where
    -- | 按关键字分派到各语句解析器
    statementP =
        selectStatement
            <|> try (CreateDatabase <$> (keyword "create" *> keyword "database" *> identifier))
            <|> try (DropDatabase <$> (keyword "drop" *> keyword "database" *> identifier))
            <|> (UseDatabase <$> (keyword "use" *> identifier))
            <|> (ShowDatabases <$ (keyword "show" *> keyword "databases"))
            <|> insertStatement
            <|> deleteStatement
            <|> updateStatement
            <|> try createUserStatement
            <|> try alterUserStatement
            <|> try dropUserStatement
            <|> try createRoleStatement
            <|> try dropRoleStatement
            <|> try grantStatement
            <|> try revokeStatement
            <|> try createIndexStatement
            <|> try createTableStatement
            <|> try dropIndexStatement
            <|> try alterTableStatement
            <|> dropTableStatement

-- | 解析一个裸表达式（CHECK 约束原文用）
parseExpression :: String -> Either String Expr
parseExpression input =
    case runParser (sc *> expr <* sc <* eof) "<expr>" input of
        Left err -> Left (errorBundlePretty err)
        Right e -> Right e

-- | 表名至多有一个库前缀。
tableName :: Parser String
tableName = do
    first <- identifier
    databaseTable <- optional (symbol "." *> identifier)
    pure (maybe first (\t -> map toLower first ++ "." ++ t) databaseTable)
