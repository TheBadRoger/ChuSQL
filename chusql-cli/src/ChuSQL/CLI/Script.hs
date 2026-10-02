{-# LANGUAGE OverloadedStrings #-}

module ChuSQL.CLI.Script
    ( Meta (..)
    , parseMeta
    , takeStatement
    , statementComplete
    , stripTerminator
    , errorHint
    , metaHelp
    ) where

import ChuSQL.CLI.Format (OutputFormat (..), formatName, parseFormat)
import Data.Char (isSpace)
import Data.Text (Text)
import qualified Data.Text as T

-- 交互层的纯逻辑：元命令、多行语句切分、错误提示。

data Meta
    = MetaQuit
    | MetaHelp
    | MetaDatabases
    | MetaTables
    | MetaDescribe Text
    | MetaRoles
    | MetaConnect Text
    | MetaSetFormat OutputFormat
    | MetaUnknown Text
    deriving (Eq, Show)

-- | 反斜杠开头的行是元命令，其余交给 SQL
parseMeta :: Text -> Maybe Meta
parseMeta raw = do
    body <- T.stripPrefix "\\" (T.strip raw)
    let (command, rest) = T.break isSpace body
        argument = T.strip rest
    pure $ case T.toLower command of
        "q" -> MetaQuit
        "quit" -> MetaQuit
        "exit" -> MetaQuit
        "?" -> MetaHelp
        "h" -> MetaHelp
        "help" -> MetaHelp
        "l" -> MetaDatabases
        "list" -> MetaDatabases
        "dt" -> MetaTables
        "tables" -> MetaTables
        "d" | T.null argument -> MetaTables
            | otherwise -> MetaDescribe argument
        "dr" -> MetaRoles
        "roles" -> MetaRoles
        "c" -> connectTo argument
        "connect" -> connectTo argument
        "use" -> connectTo argument
        "f" -> selectFormat argument
        "format" -> selectFormat argument
        other -> MetaUnknown other
  where
    -- | 拼出切库命令
    connectTo argument
        | T.null argument = MetaUnknown "c"
        | otherwise = MetaConnect argument
    -- | 解析格式参数
    selectFormat argument = maybe (MetaUnknown "format") MetaSetFormat (parseFormat argument)

-- | 取出第一条语句，忽略引号与注释里的分号
takeStatement :: Text -> Maybe (Text, Text)
takeStatement = go ScanNormal [] . T.unpack
  where
    -- | 逐字符扫描语句边界
    go _ _ [] = Nothing
    go ScanNormal acc (';' : rest) = Just (T.strip (T.pack (reverse acc)), T.pack rest)
    go ScanNormal acc ('-' : '-' : rest) = go ScanLineComment ('-' : '-' : acc) rest
    go ScanNormal acc ('/' : '*' : rest) = go ScanBlockComment ('*' : '/' : acc) rest
    go ScanNormal acc ('\'' : rest) = go ScanString ('\'' : acc) rest
    go ScanNormal acc (ch : rest) = go ScanNormal (ch : acc) rest
    go ScanString acc ('\'' : '\'' : rest) = go ScanString ('\'' : '\'' : acc) rest
    go ScanString acc ('\'' : rest) = go ScanNormal ('\'' : acc) rest
    go ScanString acc (ch : rest) = go ScanString (ch : acc) rest
    go ScanLineComment acc ('\n' : rest) = go ScanNormal ('\n' : acc) rest
    go ScanLineComment acc (ch : rest) = go ScanLineComment (ch : acc) rest
    go ScanBlockComment acc ('*' : '/' : rest) = go ScanNormal ('/' : '*' : acc) rest
    go ScanBlockComment acc (ch : rest) = go ScanBlockComment (ch : acc) rest

data ScanState = ScanNormal | ScanString | ScanLineComment | ScanBlockComment

-- | 攒下来的内容里有没有一条以分号结束的语句
statementComplete :: Text -> Bool
statementComplete = maybe False (const True) . takeStatement

-- | 去掉结尾的空白与分号（-e 直接执行时用）
stripTerminator :: Text -> Text
stripTerminator raw = T.stripEnd (T.dropWhileEnd (== ';') (T.stripEnd raw))

-- | 常见错误给一句怎么查
errorHint :: Text -> Maybe Text
errorHint message
    | "unknown table" `T.isInfixOf` lowered = Just "run \\dt to list the tables of the current database"
    | "unknown column" `T.isInfixOf` lowered = Just "run \\d <table> to see the columns of a table"
    | "unknown database" `T.isInfixOf` lowered = Just "run \\l to list the databases"
    | "syntax" `T.isInfixOf` lowered = Just "one statement at a time, and every statement ends with a semicolon"
    | "invalid user name or password" `T.isInfixOf` lowered = Just "check --user and the password; an empty password works only while the account has none"
    | "sign in first" `T.isInfixOf` lowered = Just "the session expired, restart the CLI"
    | "too many failed sign-in attempts" `T.isInfixOf` lowered = Just "wait a few minutes before trying again"
    | otherwise = Nothing
  where
    lowered = T.toLower message

-- | \? 的帮助文本
metaHelp :: Text
metaHelp =
    T.unlines
        [ "SQL statements end with a semicolon; a statement may span several lines."
        , ""
        , "  \\q, \\quit          leave the CLI"
        , "  \\?, \\h, \\help       show this help"
        , "  \\l, \\list           list databases"
        , "  \\dt, \\d             list tables"
        , "  \\d <table>          describe a table"
        , "  \\dr, \\roles         list roles with their grants and members"
        , "  \\c <database>       switch database"
        , "  \\f, \\format <fmt>   output format: " <> T.intercalate ", " (map formatName [FormatTable, FormatJson, FormatCsv])
        , ""
        , "At the chusql> prompt the arrow keys walk the SQL history."
        ]
