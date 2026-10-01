{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Main (main) where

import ChuSQL.CLI.Format (OutputFormat (..), parseFormat, renderCsv, renderJson, renderTable)
import ChuSQL.CLI.Script (Meta (..), errorHint, parseMeta, statementComplete, stripTerminator, takeStatement)
import ChuSQL.CLI.Session (
    QueryResult (..),
    Session,
    authenticateSession,
    catalog,
    databases,
    newSession,
    queryResultJson,
    roleViews,
    runStatement,
    sessionDatabase,
    sessionIsAdmin,
    switchDatabase,
 )
import ChuSQL.Model (Database, Table (..), Value (..), pattern TInt, pattern TStr)
import ChuSQL.Storage.IPC (TableInfo (..))
import ChuSQL.Web.Auth (Credential (..), hashPasswordWith)
import ChuSQL.Web.Backend (memoryBackend)
import ChuSQL.Web.Privileges (Grant (..), RoleView (..))
import Control.Concurrent.MVar (newMVar)
import Data.Aeson (decode)
import qualified Data.Aeson as A
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.Either (isLeft, isRight)
import Data.List (nub, sort)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Test.Hspec

-- chusql-cli 测试：输出格式、交互层纯逻辑与共享账号会话。

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    formatSpec
    scriptSpec
    sessionSpec
    roleSpec

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

sessionSpec :: Spec
sessionSpec = describe "CLI: session over the shared account service" $ do
    it "signs in as the configured administrator and refuses a wrong password" $ do
        session <- cliSession
        authenticateSession session "admin" "wrong-password" >>= \answer -> isLeft answer `shouldBe` True
        authenticateSession session "admin" "s3cret" >>= \answer -> isRight answer `shouldBe` True
    it "runs SQL straight through the engine" $ do
        session <- cliSession
        signInRoot session
        result <- runStatement session "SELECT id, name FROM users ORDER BY id LIMIT 1"
        fmap qrColumns result `shouldBe` Right ["id", "name"]
        fmap qrRowCount result `shouldBe` Right 1
    it "starts with nothing selected, lists what exists and refuses an unknown USE" $ do
        session <- cliSession
        authenticateSession session "admin" "s3cret" >>= (`shouldBe` Right ())
        sessionDatabase session >>= (`shouldBe` "")
        runStatement session "SELECT id FROM users" >>= \answer -> answer `shouldBe` Left "no database selected"
        listed <- databases session
        listed `shouldBe` Right ["test", "system"]
        runStatement session "USE missing" >>= \answer -> isLeft answer `shouldBe` True
        sessionDatabase session >>= (`shouldBe` "")
        runStatement session "USE test" >>= \answer -> isRight answer `shouldBe` True
        sessionDatabase session >>= (`shouldBe` "test")
    it "keeps accounts on the same store and system administrator-only" $ do
        session <- cliSession
        signInRoot session
        runStatement session "CREATE USER alice IDENTIFIED BY 'alice-password-1'" >>= \answer -> isRight answer `shouldBe` True
        authenticateSession session "alice" "alice-password-1" >>= \answer -> isRight answer `shouldBe` True
        sessionIsAdmin session >>= (`shouldBe` False)
        switchDatabase session "system" >>= \answer -> isLeft answer `shouldBe` True
        runStatement session "DROP USER admin" >>= \answer -> isLeft answer `shouldBe` True

roleSpec :: Spec
roleSpec = describe "CLI: roles and grants over the shared privilege service" $ do
    it "runs role statements as the administrator and refuses them for anyone else" $ do
        session <- cliSession
        signInRoot session
        created <- runStatement session "CREATE ROLE analyst"
        isRight created `shouldBe` True
        duplicate <- runStatement session "CREATE ROLE analyst"
        duplicate `shouldBe` Left "role already exists: analyst"
        granted <- runStatement session "GRANT SELECT ON users TO analyst"
        isRight granted `shouldBe` True
        tallied <- runStatement session "GRANT SELECT, INSERT, UPDATE, DELETE ON users TO analyst"
        isRight tallied `shouldBe` True
        views <- roleViews session
        fmap (map roleName) views `shouldBe` Right ["analyst"]
        fmap (sort . nub . concatMap (map grantPrivilege . roleGrants)) views `shouldBe` Right ["delete", "insert", "select", "update"]
        fmap (sort . nub . concatMap (map grantObject . roleGrants)) views `shouldBe` Right ["test.users"]
        unknown <- runStatement session "GRANT SELECT ON users TO nobody"
        unknown `shouldBe` Left "unknown role: nobody"
        dropped <- runStatement session "DROP ROLE analyst"
        isRight dropped `shouldBe` True
        missing <- runStatement session "GRANT SELECT ON users TO analyst"
        missing `shouldBe` Left "unknown role: analyst"
    it "an ordinary account only gets what a grant allows" $ do
        session <- cliSession
        signInRoot session
        runStatement session "CREATE TABLE secrets (id INT)" >>= \answer -> isRight answer `shouldBe` True
        runStatement session "CREATE ROLE reader" >>= \answer -> isRight answer `shouldBe` True
        runStatement session "GRANT SELECT ON users TO reader" >>= \answer -> isRight answer `shouldBe` True
        runStatement session "CREATE USER bob IDENTIFIED BY 'bob-password-1'" >>= \answer -> isRight answer `shouldBe` True
        runStatement session "GRANT reader TO bob" >>= \answer -> isRight answer `shouldBe` True
        authenticateSession session "bob" "bob-password-1" >>= \answer -> isRight answer `shouldBe` True
        granted <- runStatement session "SELECT id, name FROM users"
        fmap qrRowCount granted `shouldBe` Right 3
        refused <- runStatement session "SELECT id FROM secrets"
        refused `shouldBe` Left "permission denied: SELECT ON secrets"
        write <- runStatement session "INSERT INTO users (id, name) VALUES (4, 'four')"
        write `shouldBe` Left "permission denied: INSERT ON users"
        admin <- runStatement session "CREATE ROLE other"
        admin `shouldBe` Left "administrator required"
        listed <- catalog session
        fmap (map tiTable) listed `shouldBe` Right ["users"]
        hidden <- roleViews session
        hidden `shouldBe` Left "administrator required"
    it "revoking the membership closes the door again" $ do
        session <- cliSession
        signInRoot session
        runStatement session "CREATE ROLE guest" >>= \answer -> isRight answer `shouldBe` True
        runStatement session "GRANT SELECT ON users TO guest" >>= \answer -> isRight answer `shouldBe` True
        runStatement session "CREATE USER carol IDENTIFIED BY 'carol-password-1'" >>= \answer -> isRight answer `shouldBe` True
        runStatement session "GRANT guest TO carol" >>= \answer -> isRight answer `shouldBe` True
        roster <- roleViews session
        fmap (concatMap roleMembers) roster `shouldBe` Right ["carol"]
        authenticateSession session "carol" "carol-password-1" >>= \answer -> isRight answer `shouldBe` True
        runStatement session "SELECT id FROM users" >>= \answer -> isRight answer `shouldBe` True
        signInRoot session
        runStatement session "REVOKE guest FROM carol" >>= \answer -> isRight answer `shouldBe` True
        authenticateSession session "carol" "carol-password-1" >>= \answer -> isRight answer `shouldBe` True
        refused <- runStatement session "SELECT id FROM users"
        refused `shouldBe` Left "permission denied: SELECT ON users"

cliSession :: IO Session
cliSession = do
    db <- newMVar testDb
    newSession (memoryBackend testDatabaseName db) testCredential "chusql-test-missing.settings.json"

-- | 夹具库名：服务不再有默认库，这个库要显式选
testDatabaseName :: String
testDatabaseName = "test"

-- | 登录管理员并选中夹具库（未选库时连表名都解析不了）
signInRoot :: Session -> Expectation
signInRoot session = do
    authenticateSession session "admin" "s3cret" >>= (`shouldBe` Right ())
    switchDatabase session (T.pack testDatabaseName) >>= (`shouldBe` Right ())

sampleResult :: QueryResult
sampleResult =
    QueryResult
        { qrColumns = ["id", "name"]
        , qrRows = [[VInt 1, VStr "Alice"], [VInt 2, VNull]]
        , qrRowCount = 2
        , qrTruncated = False
        , qrDatabase = Nothing
        }

-- | 测试库：三行 users
testDb :: Database
testDb =
    [ ( "users"
      , Table
            "users"
            [("id", TInt), ("name", TStr)]
            [[("id", VInt i), ("name", VStr ("user" ++ show i))] | i <- [1 .. 3]]
      )
    ]

-- | 测试账号：迭代次数压低（跑得快），口令 `s3cret`
testCredential :: Credential
testCredential = Credential "admin" (hashPasswordWith 1000 (BS.replicate 16 7) "s3cret")
