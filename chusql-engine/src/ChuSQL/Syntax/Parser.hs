{-# LANGUAGE ImportQualifiedPost #-}

module ChuSQL.Syntax.Parser (parseStatement) where

import ChuSQL.Model (Column (..))
import ChuSQL.Syntax.AST
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isAlpha, isAlphaNum, toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lxr

-- SQL 解析：词法 + 语法，把文本变成语句树。

-- * 词法
-- | 解析器类型
type Parser = Parsec Void String


-- | 跳过空白
sc :: Parser ()
sc = Lxr.space space1 empty empty

-- | 词后面吃掉空白
lexeme :: Parser a -> Parser a
lexeme = Lxr.lexeme sc

-- | 读一个符号
symbol :: String -> Parser String
symbol = Lxr.symbol sc

{- | 大小写不敏感的关键字；其后不能紧跟标识符字符，避免把 @selection@ 读成 @select@。
整体用 'try' 包住：否则 @ORDER@ 会被 @keyword "or"@ 吃掉前缀 @OR@ 再报错，导致无法回溯。
-}
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

-- | 名字里允许的字符
isIdentChar :: Char -> Bool
isIdentChar ch = isAlphaNum ch || ch == '_'

-- | 读一个整数
integer :: Parser Int
integer = lexeme Lxr.decimal

-- | 读一个字符串字面量
stringLit :: Parser String
stringLit = lexeme $ do
    _ <- char '\''
    s <- many stringChar
    _ <- char '\''
    return s

{- | 字符串内的一个字符：@''@ 折叠成一个单引号，其余字符（含反斜杠）原样保留。
两个分支都不会在失败时消耗输入，因此可以安全地放进 'many'。
-}
-- | 字符串里的一个字符
stringChar :: Parser Char
stringChar =
    choice
        [ '\'' <$ try (string "''")
        , satisfy (/= '\'')
        ]

-- * 子句
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


-- * 表达式
-- | 读一个表达式
expr :: Parser Expr
expr = makeExprParser atom operatorTable

{- | 运算符表。注意 'makeExprParser' 要求各层按优先级从高到低排列：
比较运算符绑定最紧，'AND' 次之，'OR' 最松。
-}
-- | 运算符优先级（从紧到松）
operatorTable :: [[Operator Parser Expr]]
operatorTable =
    [
        [ InfixN (Gt <$ symbol ">") -- 最高优先级
        , InfixN (Lt <$ symbol "<")
        , InfixN (Eq <$ symbol "=")
        ]
    , [InfixL (And <$ keyword "AND")]
    , [InfixL (Or <$ keyword "OR")] -- 最低优先级
    ]

-- | 最小的表达式单位
atom :: Parser Expr
atom =
    choice
        [ LitInt <$> integer
        , LitStr <$> stringLit
        , Col <$> qualifiedName
        , between (symbol "(") (symbol ")") expr
        ]


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

-- | SET 里的一条赋值
assignment :: Parser (String, Expr)
assignment = do
    col <- identifier
    _ <- symbol "="
    e <- expr
    return (col, e)


-- | 保留字清单
reservedWords :: [String]
reservedWords =
    [ "select"
    , "from"
    , "where"
    , "order"
    , "by"
    , "limit"
    , "join"
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
    ]

{- | 表别名。别名不能是保留字：否则 @FROM users WHERE age > 18@ 会把 @WHERE@
当成 @users@ 的别名吃掉，后面真正的 WHERE 子句就再也解析不到。
用 'try' 包住，一旦命中保留字就整体回溯，让 'optional' 正常返回 'Nothing'。
-}
-- | 别名（不许用保留字）
aliasName :: Parser String
aliasName = try $ do
    name <- identifier
    if map toLower name `elem` reservedWords then empty else return name

-- | 表名 + 可选别名
tableRef :: Parser (Maybe String, String)
tableRef = do
    tbl <- identifier
    mAlias <- optional aliasName
    return (mAlias, tbl)

-- | 一个 JOIN ... ON
joinClause :: Parser (Maybe String, String, Expr)
joinClause = do
    keyword "join"
    (mAlias, tbl) <- tableRef
    keyword "on"
    cond <- expr
    return (mAlias, tbl, cond)

-- | FROM 子句（可含多个 JOIN）
fromClause :: Parser FromClause
fromClause = do
    (mAlias, tbl) <- tableRef
    joins <- many joinClause
    return (foldl (\acc (a, t, c) -> FromJoin acc a t c) (FromTable mAlias tbl) joins)

-- | SELECT 的列清单
selectList :: Parser [String]
selectList =
    choice
        [ ["*"] <$ symbol "*"
        , (:) <$> qualifiedName <*> many (symbol "," *> qualifiedName)
        ]


-- | 读 CREATE TABLE
createTableStatement :: Parser Statement
createTableStatement = do
    keyword "create"
    keyword "table"
    name <- identifier
    cols <- between (symbol "(") (symbol ")") (sepBy1 columnDef (symbol ","))
    return (CreateTable name cols)

-- | 读 DROP TABLE
dropTableStatement :: Parser Statement
dropTableStatement = do
    keyword "drop"
    keyword "table"
    name <- identifier
    return (DropTable name)

-- | 建表时的一列
columnDef :: Parser (String, Column)
columnDef = do
    name <- identifier
    ty <- columnType
    return (name, ty)

-- | 列类型关键字
columnType :: Parser Column
columnType =
    (keyword "integer" >> pure TInt)
        <|> (keyword "int" >> pure TInt)
        <|> (keyword "varchar" >> pure TStr)
        <|> (keyword "text" >> pure TStr)
        <|> (keyword "str" >> pure TStr)
        <|> (keyword "boolean" >> pure TBool)
        <|> (keyword "bool" >> pure TBool)

-- * 语句
-- | 读 SELECT
selectStatement :: Parser Statement
selectStatement = do
    keyword "select"
    cols <- selectList
    keyword "from"
    fromC <- fromClause
    mWhere <- optional (keyword "where" *> expr)
    orderBy <- fromMaybe [] <$> optional orderByClause
    mLimit <- optional limitClause
    return
        Select
            { selectCols = cols
            , selectFrom = fromC
            , selectWhere = mWhere
            , selectOrderBy = orderBy
            , selectLimit = mLimit
            }

-- | 读 INSERT
insertStatement :: Parser Statement
insertStatement = do
    keyword "INSERT"
    keyword "INTO"
    tbl <- identifier
    cols <-
        between
            (symbol "(")
            (symbol ")")
            (sepBy1 identifier (symbol ","))
    keyword "VALUES"
    vals <-
        between
            (symbol "(")
            (symbol ")")
            (sepBy1 atom (symbol ","))
    return (Insert tbl cols vals)

-- | 读 DELETE
deleteStatement :: Parser Statement
deleteStatement = do
    keyword "DELETE"
    keyword "FROM"
    tbl <- identifier
    mWhere <- optional (keyword "WHERE" *> expr)
    return (Delete tbl mWhere)

-- | 读 UPDATE
updateStatement :: Parser Statement
updateStatement = do
    keyword "update"
    tbl <- identifier
    keyword "set"
    assigns <- sepBy1 assignment (symbol ",")
    mWhere <- optional (keyword "where" *> expr)
    return (Update tbl assigns mWhere)

-- * 入口
-- | 解析总入口
parseStatement :: String -> Either String Statement
parseStatement input =
    case runParser (sc *> statementP <* sc <* eof) "<query>" input of
        Left err -> Left (errorBundlePretty err)
        Right q -> Right q
  where
    -- \| 依次尝试各种语句
    statementP =
        selectStatement
            <|> insertStatement
            <|> deleteStatement
            <|> updateStatement
            <|> createTableStatement
            <|> dropTableStatement
