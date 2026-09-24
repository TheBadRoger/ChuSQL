{-# LANGUAGE ImportQualifiedPost #-}

module ChuSQL.Syntax.Parser (parseQuery) where

import ChuSQL.Syntax.AST
import Control.Monad.Combinators.Expr (Operator (..), makeExprParser)
import Data.Char (isAlpha, isAlphaNum, toLower)
import Data.List (intercalate)
import Data.Maybe (fromMaybe)
import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer qualified as Lxr

-- SQL 解析器：把 SQL 文本解析成 AST。词法层管空白、关键字、标识符、字面量与符号；
-- 语法层按优先级 比较 > AND > OR 自底向上组装表达式。

type Parser = Parsec Void String

-- * 词法层

-- | 跳过空白（含换行）。
sc :: Parser ()
sc = Lxr.space space1 empty empty

-- | 消费一个词法单元以及其后的空白。
lexeme :: Parser a -> Parser a
lexeme = Lxr.lexeme sc

-- | 匹配一个符号（如 ","、"("），并吃掉其后的空白。
symbol :: String -> Parser String
symbol = Lxr.symbol sc

-- | 大小写不敏感的关键字；其后不能紧跟标识符字符，避免把 @selection@ 读成 @select@。
-- 整体用 'try' 包住：否则 @ORDER@ 会被 @keyword "or"@ 吃掉前缀 @OR@ 再报错，导致无法回溯。
keyword :: String -> Parser ()
keyword k = lexeme $ try $ do
    _ <- string' k
    notFollowedBy (satisfy isIdentChar)

-- | 标识符：字母或下划线开头，其后可跟字母、数字或下划线。
identifier :: Parser String
identifier = lexeme $ do
    first <- satisfy (\ch -> isAlpha ch || ch == '_')
    rest <- many (satisfy isIdentChar)
    return (first : rest)

-- | 标识符允许的字符：字母、数字或下划线。
isIdentChar :: Char -> Bool
isIdentChar ch = isAlphaNum ch || ch == '_'

-- | 十进制整数字面量。
integer :: Parser Int
integer = lexeme Lxr.decimal

-- | 单引号字符串字面量，遵循标准 SQL 转义规则：
stringLit :: Parser String
stringLit = lexeme $ do
    _ <- char '\''
    s <- many stringChar
    _ <- char '\''
    return s

-- | 字符串内的一个字符：@''@ 折叠成一个单引号，其余字符（含反斜杠）原样保留。
-- 两个分支都不会在失败时消耗输入，因此可以安全地放进 'many'。
stringChar :: Parser Char
stringChar =
    choice
        [ '\'' <$ try (string "''")
        , satisfy (/= '\'')
        ]

-- | 解析 ASC/DESC
sortDir :: Parser SortDir
sortDir = (Desc <$ keyword "desc") <|> (Asc <$ keyword "asc") <|> pure Asc

-- | 解析 LIMIT
limitClause :: Parser Int
limitClause = do
    keyword "limit"
    integer

-- | 解析JOIN和限定列名
qualifiedName :: Parser String
qualifiedName = do
    first <- identifier
    rest <- many (symbol "." *> identifier)
    return (intercalate "." (first : rest))

-- * 表达式层

-- | 表达式入口，最低优先级。
expr :: Parser Expr
expr = makeExprParser atom operatorTable

-- | 运算符表。注意 'makeExprParser' 要求各层按优先级从高到低排列：
-- 比较运算符绑定最紧，'AND' 次之，'OR' 最松。
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

-- | 原子表达式：字面量、列引用或括号表达式。
atom :: Parser Expr
atom =
    choice
        [ LitInt <$> integer
        , LitStr <$> stringLit
        , Col <$> qualifiedName
        , between (symbol "(") (symbol ")") expr
        ]

-- * 语句层

-- | 显式指定排序的语句
orderItem :: Parser (String, SortDir)
orderItem = do
    col <- qualifiedName
    dir <- sortDir
    return (col, dir)

-- | @ORDER BY col [ASC|DESC], ...@。
orderByClause :: Parser [(String, SortDir)]
orderByClause = do
    keyword "order"
    keyword "by"
    sepBy1 orderItem (symbol ",")

-- | @col = expr@，用于 UPDATE 的 SET 子句。
assignment :: Parser (String, Expr)
assignment = do
    col <- identifier
    _ <- symbol "="
    e <- expr
    return (col, e)

-- * 从句层

-- | SQL 保留字，不能用作表别名
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
    ]

-- | 表别名。别名不能是保留字：否则 @FROM users WHERE age > 18@ 会把 @WHERE@
-- 当成 @users@ 的别名吃掉，后面真正的 WHERE 子句就再也解析不到。
-- 用 'try' 包住，一旦命中保留字就整体回溯，让 'optional' 正常返回 'Nothing'。
aliasName :: Parser String
aliasName = try $ do
    name <- identifier
    if map toLower name `elem` reservedWords then empty else return name

-- | @表名 [别名]@。
tableRef :: Parser (Maybe String, String)
tableRef = do
    tbl <- identifier
    mAlias <- optional aliasName
    return (mAlias, tbl)

-- | @JOIN 表 [别名] ON 条件@。
joinClause :: Parser (Maybe String, String, Expr)
joinClause = do
    keyword "join"
    (mAlias, tbl) <- tableRef
    keyword "on"
    cond <- expr
    return (mAlias, tbl, cond)

-- | FROM 从句：一张表 + 任意多个 JOIN。
fromClause :: Parser FromClause
fromClause = do
    (mAlias, tbl) <- tableRef
    joins <- many joinClause
    return (foldl (\acc (a, t, c) -> FromJoin acc a t c) (FromTable mAlias tbl) joins)

-- | 选择列表：@*@ 或逗号分隔的列名。
selectList :: Parser [String]
selectList =
    choice
        [ ["*"] <$ symbol "*"
        , (:) <$> qualifiedName <*> many (symbol "," *> qualifiedName)
        ]

-- | @SELECT ... FROM ... [WHERE ...]@。
selectQuery :: Parser Query
selectQuery = do
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

-- | @INSERT ... INTO ... VALUES ...@。
insertQuery :: Parser Query
insertQuery = do
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

-- | @DELETE ... FROM ... [WHERE ...]@。
deleteQuery :: Parser Query
deleteQuery = do
    keyword "DELETE"
    keyword "FROM"
    tbl <- identifier
    mWhere <- optional (keyword "WHERE" *> expr)
    return (Delete tbl mWhere)

-- | UPDATE ... SET col = val, ... [WHERE ...]@
updateQuery :: Parser Query
updateQuery = do
    keyword "update"
    tbl <- identifier
    keyword "set"
    assigns <- sepBy1 assignment (symbol ",")
    mWhere <- optional (keyword "where" *> expr)
    return (Update tbl assigns mWhere)

-- | 解析一条完整的语句；失败时返回可直接展示的错误信息。
parseQuery :: String -> Either String Query
parseQuery input =
    case runParser (sc *> (selectQuery <|> insertQuery <|> deleteQuery <|> updateQuery) <* sc <* eof) "<query>" input of
        Left err -> Left (errorBundlePretty err)
        Right q -> Right q
