{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import ChuSQL.Bootstrap (preinstalledTypes, runBootstrap)
import ChuSQL.Core.Protocol (Account (..))
import ChuSQL.Core.Model (columnType, typeName)
import ChuSQL.Core.Engine.Syntax.AST (Statement (CreateTable))
import ChuSQL.Core.Engine.Syntax.Parser (parseStatement)
import ChuSQL.Core.Engine.Storage.IPC (closeConnection, localStorageLink, sendRawRequest, setStorageLink)
import Control.Exception (finally)
import qualified Data.Aeson as A
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString.Lazy as BL
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Text (Text)
import qualified Data.Text as T
import Data.Unique (hashUnique, newUnique)
import System.Directory (createDirectoryIfMissing, getTemporaryDirectory, removePathForcibly)
import System.FilePath ((</>))
import Test.Hspec

-- 引导测试：身份属性、类型清单、错误停止与真实持久化。

-- | 执行引导模块测试
main :: IO ()
main = hspec $ do
    describe "bootstrap plan" $ do
        it "registers every builtin alias with its parser defaults" $ mapM_ checkDefinition preinstalledTypes
        it "initializes identities before installing types" $ do
            (send, requests) <- mock [Right ready, Right (identity True), Right installed]
            runBootstrap send "root" "new-hash" `shouldReturn` Right 18
            calls <- requests
            map (field "method") calls `shouldBe` map A.String ["bootstrap_system", "identity_initialize", "bootstrap_types"]
        mapM_ (\(title, replies, err) -> it title (expectBootstrapFailure replies err))
            [ ("stops on storage errors", [Left "storage unavailable"], "storage unavailable")
            , ("rejects an uninitialized system response", [Right (A.object ["status" A..= ("system" :: Text), "initialized" A..= False])], "system catalog is not initialized")
            , ("preserves a demoted identity and stops before type installation", [Right ready, Right (identity False)], "bootstrap identity is not an enabled login superuser; existing identities were preserved")
            , ("reports type definition conflicts explicitly", [Right ready, Right (identity True), Right (A.object ["status" A..= ("error" :: Text), "message" A..= ("type conflict" :: Text)])], "type conflict")
            , ("refuses incomplete type registration", [Right ready, Right (identity True), Right (A.object ["status" A..= ("rows" :: Text), "rows" A..= ([] :: [A.Value])])], "preinstalled type verification failed")
            ]
    describe "bootstrap on real storage" $ do
        it "creates root and eighteen types before any server starts" $ withStorage $ do
            runBootstrap transport "root" "original-hash" `shouldReturn` Right 18
            result <- transport (A.object ["method" A..= ("accounts_list" :: Text)])
            case result of
                Left err -> expectationFailure err
                Right value -> case A.fromJSON (field "accounts" value) :: A.Result [Account] of
                    A.Error err -> expectationFailure err
                    A.Success [account] -> do
                        accountUser account `shouldBe` "root"
                        accountCanLogin account `shouldBe` True
                        accountEnabled account `shouldBe` True
                        accountIsSuperuser account `shouldBe` True
                        accountHash account `shouldBe` "original-hash"
                    A.Success _ -> expectationFailure "unexpected identities"
        it "keeps the existing password and type definitions on rerun" $ withStorage $ do
            runBootstrap transport "root" "original-hash" `shouldReturn` Right 18
            first <- transport (A.object ["method" A..= ("accounts_list" :: Text)])
            runBootstrap transport "root" "replacement-hash" `shouldReturn` Right 18
            transport (A.object ["method" A..= ("accounts_list" :: Text)]) `shouldReturn` first

-- | 校验引导错误及停止请求的位置
expectBootstrapFailure :: [Either String A.Value] -> String -> Expectation
expectBootstrapFailure replies err = do
    (send, requests) <- mock replies
    runBootstrap send "root" "hash" `shouldReturn` Left err
    fmap length requests `shouldReturn` length replies

-- | 校验预装定义与 SQL 默认类型一致
checkDefinition :: A.Value -> IO ()
checkDefinition value = case (field "name" value, field "base_type" value) of
    (A.String name, A.String base) -> case parseStatement ("CREATE TABLE seeded (value " ++ T.unpack name ++ ")") of
        Right (CreateTable _ [(_, column)]) -> T.pack (typeName (columnType column)) `shouldBe` base
        other -> expectationFailure (show other)
    _ -> expectationFailure "invalid builtin definition"

-- | 系统目录的成功响应
ready :: A.Value
ready = A.object ["status" A..= ("system" :: Text), "initialized" A..= True]

-- | 带最高权限属性的身份响应
identity :: Bool -> A.Value
identity super = A.object ["status" A..= ("accounts" :: Text), "accounts" A..= [Account 1 "root" "existing-hash" 1 "" Nothing True super True False]]

-- | 已写入编号的类型响应
installed :: A.Value
installed = A.object ["status" A..= ("rows" :: Text), "rows" A..= map numbered (zip [1 :: Int ..] preinstalledTypes)]
  where
    -- | 给预装定义添加持久化编号
    numbered (n, A.Object fields) = A.Object (KM.insert "id" (A.toJSON n) fields)
    numbered (_, value) = value

-- | 读取响应中的一个字段
field :: A.Key -> A.Value -> A.Value
field key (A.Object fields) = maybe A.Null id (KM.lookup key fields)
field _ _ = A.Null

-- | 记录请求并返回指定响应
mock :: [Either String A.Value] -> IO (A.Value -> IO (Either String A.Value), IO [A.Value])
mock replies = do
    calls <- newIORef []
    queuedReplies <- newIORef replies
    let
        -- | 消费一条模拟响应
        send request = do
            modifyIORef' calls (++ [request])
            remaining <- readIORef queuedReplies
            case remaining of
                [] -> pure (Left "unexpected request")
                reply : rest -> modifyIORef' queuedReplies (const rest) >> pure reply
    pure (send, readIORef calls)

-- | 通过动态库存储链路发送请求
transport :: A.Value -> IO (Either String A.Value)
transport request = do
    reply <- sendRawRequest (BL.toStrict (A.encode request))
    pure (reply >>= A.eitherDecodeStrict)

-- | 在独立临时目录运行真实引导
withStorage :: IO () -> IO ()
withStorage action = do
    tmp <- getTemporaryDirectory
    unique <- hashUnique <$> newUnique
    let directory = tmp </> ("chusql-bootstrap-" ++ show unique)
        config = directory </> "settings.toml"
    createDirectoryIfMissing True directory
    writeFile config ("[storage]\ndata_dir = \"" ++ map (\c -> if c == '\\' then '/' else c) directory ++ "\"\n")
    opened <- localStorageLink (Just config)
    case opened of
        Left err -> removePathForcibly directory >> expectationFailure err
        Right link -> do
            setStorageLink link
            action `finally` (closeConnection >> removePathForcibly directory)
