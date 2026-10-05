module ChuSQL.Core.Engine.Normalization where

import ChuSQL.Core.Model (Column)
import Data.List (nub, sort, subsequences, (\\))

-- 只读范式分析：声明验证、阿姆斯特朗闭包、候选键与分解建议。

type Attributes = [String]
data Dependency = Dependency Attributes Attributes deriving (Eq, Show)
data Verdict = Satisfied | Violated | Insufficient deriving (Eq, Show)
data NormalForm = NF1 | NF2 | NF3 | BCNF deriving (Eq, Show)
data Declaration = Declaration
    { declaredKeys :: [Attributes]
    , dependencies :: [Dependency]
    , dependenciesComplete :: Bool
    , atomicAttributes :: Maybe Bool
    } deriving (Eq, Show)
data Finding = Finding
    { normalForm :: NormalForm
    , verdict :: Verdict
    , witnesses :: [Dependency]
    , condition :: String
    } deriving (Eq, Show)
data Decomposition = Decomposition
    { relations :: [Attributes]
    , losslessJoin :: Verdict
    , dependencyPreserving :: Verdict
    } deriving (Eq, Show)
data Analysis = Analysis
    { candidateKeys :: [Attributes]
    , findings :: [Finding]
    , suggestions :: [Decomposition]
    } deriving (Eq, Show)

-- 规范化属性集合。
canonical :: Attributes -> Attributes
canonical = sort . nub

-- 判断属性集合包含关系。
subset :: Attributes -> Attributes -> Bool
subset xs ys = all (`elem` ys) xs

-- 用自反、增广和传递规则求闭包。
attributeClosure :: [Dependency] -> Attributes -> Attributes
attributeClosure f seed = fixed (canonical seed)
  where
    -- 迭代加入已蕴含的属性。
    fixed xs = let ys = canonical (xs ++ concat [b | Dependency a b <- f, subset a xs])
               in if xs == ys then xs else fixed ys

-- 判断声明是否蕴含依赖。
implies :: [Dependency] -> Dependency -> Bool
implies f (Dependency a b) = subset b (attributeClosure f a)

-- 枚举最小超键。
keysOf :: Attributes -> [Dependency] -> [Attributes]
keysOf attrs f = [x | x <- subsequences attrs, super x,
                     all (not . super) [x \\ [a] | a <- x]]
  where
    -- 判断闭包是否覆盖关系。
    super x = subset attrs (attributeClosure f x)

-- 投影关系上的全部非平凡依赖。
projectDependencies :: [Dependency] -> Attributes -> [Dependency]
projectDependencies f attrs =
    [Dependency x [a] | x <- subsequences attrs,
                       a <- attributeClosure f x, a `elem` attrs, a `notElem` x]

-- 验证声明并生成只读范式报告。
analyzeNormalization :: [(String, Column)] -> Declaration -> Either String Analysis
analyzeNormalization schema declaration
    | null attrs = Left "normalization: empty schema"
    | length attrs /= length schema = Left "normalization: duplicate columns"
    | length attrs > 12 = Left "normalization: exhaustive analysis supports at most 12 columns"
    | any invalidDependency supplied = Left "normalization: unknown or duplicate dependency attributes"
    | any invalidKey rawKeys = Left "normalization: unknown or duplicate key attributes"
    | any (`notElem` keys) normalizedKeys = Left "normalization: declared candidate key is not minimal"
    | otherwise = Right (Analysis keys results decompositions)
  where
    attrs = canonical (map fst schema)
    supplied = dependencies declaration
    rawKeys = declaredKeys declaration
    normalizedKeys = map canonical rawKeys
    f = supplied ++ [Dependency k attrs | k <- normalizedKeys]
    keys = keysOf attrs f
    prime = nub (concat keys)
    complete = dependenciesComplete declaration
    atomic = case atomicAttributes declaration of
        Just True -> Satisfied
        Just False -> Violated
        Nothing -> Insufficient
    allFDs = projectDependencies f attrs
    partial = [d | d@(Dependency x [a]) <- allFDs, a `notElem` prime,
                   any (\k -> x /= k && subset x k) keys]
    third = [d | d@(Dependency x [a]) <- allFDs, a `notElem` prime,
                 not (subset attrs (attributeClosure f x))]
    boyce = [d | d@(Dependency x _) <- allFDs,
                 not (subset attrs (attributeClosure f x))]
    results = Finding NF1 atomic [] "attributes must have atomic domains; requires an explicit declaration" :
        [classify NF2 partial, classify NF3 third, classify BCNF boyce]
    decompositions = [split d | d <- take 1 boyce, complete, atomic == Satisfied]
    -- 校验字段集合。
    valid xs = length xs == length (canonical xs) && subset xs attrs
    -- 校验依赖声明。
    invalidDependency (Dependency a b) = not (valid a && valid b) || null b
    -- 校验候选键声明。
    invalidKey k = not (valid k)
    -- 结合声明完整性判定范式。
    classify nf ds = Finding nf status (if complete then ds else []) (rule nf)
      where
        status | atomic == Violated = Violated
               | atomic == Insufficient || not complete = Insufficient
               | null ds = Satisfied
               | otherwise = Violated
    -- 说明各范式的必要条件。
    rule NF1 = "attributes must have atomic domains"
    rule NF2 = "no nonprime attribute depends on a proper subset of any candidate key"
    rule NF3 = "each nontrivial dependency has a superkey determinant or a prime dependent"
    rule BCNF = "each nontrivial dependency has a superkey determinant"
    -- 按违反依赖生成一次无损分解。
    split (Dependency x y) = Decomposition parts Satisfied preservation
      where
        parts = [canonical (x ++ y), attrs \\ (y \\ x)]
        projected = concatMap (projectDependencies f) parts
        preservation = if all (implies projected) f then Satisfied else Violated
