{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.CLI.Format (OutputFormat (..), parseFormat, renderCsv, renderJson, renderTable)
import ChuSQL.CLI.History (historySettings, rememberStatement)
import ChuSQL.CLI.Password (readPasswordInput)
import ChuSQL.CLI.Script (Meta (..), errorHint, parseMeta, statementComplete, stripTerminator, takeStatement)
import ChuSQL.Core.Model (Value (..))
import ChuSQL.Core.Protocol (QueryResult (..), queryResultJson)
import Data.Aeson (decode)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Control.Exception (bracket, finally)
import Control.Monad.IO.Class (liftIO)
import System.Directory (getTemporaryDirectory, removeFile)
import System.IO (Handle, SeekMode (AbsoluteSeek), hClose, hPutStr, hSeek, hSetNewlineMode, noNewlineTranslation, openTempFile)
import Test.Hspec
import System.Console.Haskeline (getHistory, getInputLine, runInputT, runInputTBehavior, useFileHandle)
import System.Console.Haskeline.History (historyLines)

-- chusql-cli 测试：输出格式、语句缓冲、元命令与口令输入。

-- | 测试入口
main :: IO ()
main = hspec spec

-- | 全部测试用例
spec :: Spec
spec = do
    formatSpec
    scriptSpec
    passwordSpec
    historySpec

-- | 历史过滤与持久化的用例
historySpec :: Spec
historySpec = describe "CLI: SQL history" $ do
    it "does not automatically record unfinished input lines" $
        withPasswordInput "CREATE USER alice IDENTIFIED BY\n'secret'\n" (\handle -> do
            entries <- runInputTBehavior (useFileHandle handle) (historySettings Nothing) $ do
                getInputLine "" >>= liftExpectation "CREATE USER alice IDENTIFIED BY"
                getInputLine "" >>= liftExpectation "'secret'"
                historyLines <$> getHistory
            entries `shouldBe` [])
    it "keeps ordinary statements and excludes multiline or mixed-case passwords" $ do
        entries <- runInputT (historySettings Nothing) $ do
            rememberStatement "SELECT 1;"
            rememberStatement "CREATE USER alice\nWITH PaSsWoRd\n'secret';"
            rememberStatement "ALTER USER alice PASSWORD 'new-secret';"
            rememberStatement "CREATE USER alice\nIdEnTiFiEd /* credential */ BY\n'secret';"
            rememberStatement "ALTER ROLE analyst IDENTIFIED BY 'new-secret';"
            rememberStatement "SELECT 'password';"
            rememberStatement "   "
            historyLines <$> getHistory
        entries `shouldBe` ["SELECT 1;"]
    it "persists only safe statements from a batch" $ do
        directory <- getTemporaryDirectory
        bracket (openTempFile directory "csql-history-test")
            (\(path, _) -> removeFile path)
            (\(path, handle) -> do
                hClose handle
                runInputT (historySettings (Just path)) $
                    rememberBatch "SELECT 1; CREATE USER alice IDENTIFIED BY 'secret'; SELECT 2;"
                entries <- runInputT (historySettings (Just path)) (historyLines <$> getHistory)
                entries `shouldBe` ["SELECT 2;", "SELECT 1;"])
  where
    -- | 核对实际读取的输入行
    liftExpectation expected actual = liftIO (actual `shouldBe` Just expected)
    -- | 按完整语句筛选批量输入
    rememberBatch input = case takeStatement input of
        Nothing -> pure ()
        Just (statement, rest) -> do
            rememberStatement (statement <> ";")
            rememberBatch rest

-- | 在临时句柄中写入口令输入
withPasswordInput :: String -> (Handle -> IO a) -> IO a
withPasswordInput input action = do
    directory <- getTemporaryDirectory
    bracket (openTempFile directory "csql-password-test")
        (\(path, handle) -> hClose handle `finally` removeFile path)
        (\(_, handle) -> do
            hSetNewlineMode handle noNewlineTranslation
            hPutStr handle input
            hSeek handle AbsoluteSeek 0
            action handle)

-- | 管道口令的行尾与内容用例
passwordSpec :: Spec
passwordSpec = describe "CLI: password input" $ do
    it "reads LF, CRLF and unterminated passwords identically" $ do
        mapM_ (\input -> withPasswordInput input (\handle -> readPasswordInput handle `shouldReturn` "secret"))
            ["secret\n", "secret\r\n", "secret"]
    it "preserves password spaces and internal carriage returns" $
        withPasswordInput " a\rb \r\n" (\handle -> readPasswordInput handle `shouldReturn` " a\rb ")
    it "accepts an empty password and consumes only one line" $
        withPasswordInput "\r\nsecond\n" (\handle -> do
            readPasswordInput handle `shouldReturn` ""
            readPasswordInput handle `shouldReturn` "second"
            readPasswordInput handle `shouldReturn` "")

-- | 输出格式的用例
formatSpec :: Spec
formatSpec = describe "CLI: output formats" $ do
    it "renders a boxed table and marks NULL" $
        renderTable sampleResult
            `shouldBe` T.unlines
                [ "+-----+--------+"
                , "| id | name  |"
                , "+-----+--------+"
                , "| 1  | Alice |"
                , "| 2  | NULL  |"
                , "+-----+--------+"
                , "(2 rows)"
                ]
    it "renders CSV with RFC 4180 quoting and empty NULL fields" $
        renderCsv
            QueryResult
                { qrColumns = ["id", "name"]
                , qrRows = [[VInt 1, VStr "a,b"], [VNull, VStr "say \"hi\""]]
                , qrRowCount = 2
                , qrTruncated = False
                , qrDatabase = Nothing
                }
            `shouldBe` "id,name\n1,\"a,b\"\n,\"say \"\"hi\"\"\"\n"
    it "renders JSON that parses back to the same result" $
        (decode (BL.fromStrict (encodeUtf8 (renderJson sampleResult))) :: Maybe A.Value)
            `shouldBe` Just (queryResultJson sampleResult)
    it "accepts table/json/csv in any case and rejects anything else" $ do
        parseFormat "TABLE" `shouldBe` Just FormatTable
        parseFormat " json " `shouldBe` Just FormatJson
        parseFormat "csv" `shouldBe` Just FormatCsv
        parseFormat "xml" `shouldBe` Nothing

-- | 语句缓冲与元命令的用例
scriptSpec :: Spec
scriptSpec = do
    describe "CLI: statement buffering" $ do
        it "a statement is complete only at an unquoted semicolon" $ do
            statementComplete "SELECT 1;" `shouldBe` True
            statementComplete "SELECT 1" `shouldBe` False
            statementComplete "SELECT ';'" `shouldBe` False
            statementComplete "SELECT 'a''b'" `shouldBe` False
            statementComplete "SELECT 1 -- ;" `shouldBe` False
            statementComplete "SELECT /* ; */ 1" `shouldBe` False
            statementComplete "SELECT /* ; */ 1;" `shouldBe` True
        it "splits the first statement off and keeps the rest" $
            takeStatement "SELECT 1; SELECT 2;"
                `shouldBe` Just ("SELECT 1", " SELECT 2;")
        it "drops the terminator before sending SQL to the server" $ do
            stripTerminator "SELECT 1 ; " `shouldBe` "SELECT 1"
            stripTerminator "SELECT 1" `shouldBe` "SELECT 1"
        it "suggests how to look around after a common error" $ do
            errorHint "unknown table: nope" `shouldBe` Just "run \\dt to list the tables of the current database"
            errorHint "unknown column: nope" `shouldBe` Just "run \\d <table> to see the columns of a table"
            errorHint "something else" `shouldBe` Nothing
    describe "CLI: meta commands" $ do
        it "parses the backslash commands" $ do
            parseMeta "\\q" `shouldBe` Just MetaQuit
            parseMeta "\\?" `shouldBe` Just MetaHelp
            parseMeta "\\l" `shouldBe` Just MetaDatabases
            parseMeta "\\dt" `shouldBe` Just MetaTables
            parseMeta "\\d" `shouldBe` Just MetaTables
            parseMeta "\\d users" `shouldBe` Just (MetaDescribe "users")
            parseMeta "\\dr" `shouldBe` Just MetaRoles
            parseMeta "\\roles" `shouldBe` Just MetaRoles
            parseMeta "\\c sales" `shouldBe` Just (MetaConnect "sales")
            parseMeta "\\format CSV" `shouldBe` Just (MetaSetFormat FormatCsv)
        it "leaves ordinary SQL alone" $ do
            parseMeta "SELECT 1" `shouldBe` Nothing
            parseMeta "\\nope" `shouldBe` Just (MetaUnknown "nope")
            parseMeta "\\f xml" `shouldBe` Just (MetaUnknown "format")

sampleResult :: QueryResult
sampleResult =
    QueryResult
        { qrColumns = ["id", "name"]
        , qrRows = [[VInt 1, VStr "Alice"], [VInt 2, VNull]]
        , qrRowCount = 2
        , qrTruncated = False
        , qrDatabase = Nothing
        }
