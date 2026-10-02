{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.CLI.Format (OutputFormat (..), parseFormat, renderCsv, renderJson, renderTable)
import ChuSQL.CLI.Script (Meta (..), errorHint, parseMeta, statementComplete, stripTerminator, takeStatement)
import ChuSQL.Core.Model (Value (..))
import ChuSQL.Core.Protocol (QueryResult (..), queryResultJson)
import Data.Aeson (decode)
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec

-- chusql-cli 测试：只覆盖本前端的纯逻辑（输出格式、语句缓冲与元命令）。

-- | 测试入口
main :: IO ()
main = hspec spec

-- | 全部测试用例
spec :: Spec
spec = do
    formatSpec
    scriptSpec

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
