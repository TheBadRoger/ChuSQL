{-# LANGUAGE PatternSynonyms #-}

module NormalizationSpec (spec) where

import ChuSQL.Core.Engine.Normalization
import ChuSQL.Core.Model (pattern TInt)
import Test.Hspec

-- 范式分析回归：公理闭包、候选键、信息不足与分解性质。

-- 执行范式检查回归。
spec :: Spec
spec = do
    describe "Armstrong normalization" $ do
        it "computes reflexivity, augmentation and transitivity" $ do
            attributeClosure [Dependency ["a"] ["b"], Dependency ["b"] ["c"]] ["a"]
                `shouldBe` ["a", "b", "c"]
            implies [Dependency ["a"] ["b"]] (Dependency ["a", "c"] ["b", "c"])
                `shouldBe` True
            attributeClosure [] ["a"] `shouldBe` ["a"]
        it "accepts BCNF with a declared key" $ do
            fmap (map verdict . findings) (analyzeNormalization schema
                (Declaration [["a"]] [] True (Just True)))
                `shouldBe` Right (replicate 4 Satisfied)
        it "detects partial dependencies of composite keys" $ do
            fmap (map verdict . findings) (analyzeNormalization schema
                (Declaration [["a", "b"]] [Dependency ["a"] ["c"]] True (Just True)))
                `shouldBe` Right [Satisfied, Violated, Violated, Violated]
        it "detects transitive dependencies and a preserving split" $ do
            let analysis = analyzeNormalization schema
                    (Declaration [["a"]] [Dependency ["b"] ["c"]] True (Just True))
            fmap (map verdict . findings) analysis
                `shouldBe` Right [Satisfied, Satisfied, Violated, Violated]
            fmap suggestions analysis
                `shouldBe` Right [Decomposition [["b", "c"], ["a", "b"]] Satisfied Satisfied]
        it "distinguishes 3NF from BCNF and detects dependency loss" $ do
            let analysis = analyzeNormalization schema
                    (Declaration [["a", "b"]] [Dependency ["c"] ["b"]] True (Just True))
            fmap (map verdict . findings) analysis
                `shouldBe` Right [Satisfied, Satisfied, Satisfied, Violated]
            fmap suggestions analysis
                `shouldBe` Right [Decomposition [["b", "c"], ["a", "c"]] Satisfied Violated]
        it "does not certify incomplete dependencies" $ do
            fmap (map verdict . findings) (analyzeNormalization schema
                (Declaration [["a"]] [] False (Just True)))
                `shouldBe` Right [Satisfied, Insufficient, Insufficient, Insufficient]
        it "requires an atomicity declaration" $ do
            fmap (map verdict . findings) (analyzeNormalization schema
                (Declaration [] [] True Nothing)) `shouldBe` Right (replicate 4 Insufficient)
        it "propagates a declared 1NF violation" $ do
            fmap (map verdict . findings) (analyzeNormalization schema
                (Declaration [] [] True (Just False))) `shouldBe` Right (replicate 4 Violated)
        it "rejects unknown attributes" $ do
            analyzeNormalization schema (Declaration [] [Dependency ["z"] ["a"]] True (Just True))
                `shouldBe` Left "normalization: unknown or duplicate dependency attributes"
        it "rejects nonminimal declared candidate keys" $ do
            analyzeNormalization schema (Declaration [["a", "b"]]
                [Dependency ["a"] ["b", "c"]] True (Just True))
                `shouldBe` Left "normalization: declared candidate key is not minimal"
        it "includes the empty candidate key for constant relations" $ do
            fmap candidateKeys (analyzeNormalization schema
                (Declaration [] [Dependency [] ["a", "b", "c"]] True (Just True))) `shouldBe` Right [[]]
        it "rejects excessive exhaustive searches" $ do
            analyzeNormalization [(show n, TInt) | n <- [1 :: Int .. 13]]
                (Declaration [] [] True (Just True))
                `shouldBe` Left "normalization: exhaustive analysis supports at most 12 columns"
  where
    schema = [("a", TInt), ("b", TInt), ("c", TInt)]
