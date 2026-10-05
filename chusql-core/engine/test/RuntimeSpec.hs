module RuntimeSpec (spec) where

import ChuSQL.Core.Engine.Runtime.Types
import ChuSQL.Core.Engine.Runtime.Functions
import ChuSQL.Core.Engine.Runtime.Binding
import qualified ChuSQL.Core.Engine.Runtime.Query as Query
import ChuSQL.Core.Engine (rowsOf, runStatement)
import ChuSQL.Core.Engine.Semantic (prepare)
import ChuSQL.Core.Engine.Syntax.Parser (parseStatement)
import ChuSQL.Core.Engine.Syntax.AST (Expr (..), Statement (..))
import qualified ChuSQL.Core.Model as SQL
import ChuSQL.Core.Protocol (valueToJSON, valueFromJSON)
import Data.Aeson.Types (parseEither)
import Data.Hashable (hash)
import qualified Data.ByteString.Lazy.Char8 as BS
import Test.Hspec

-- 运行时类型、函数绑定、物理编码和类型化查询测试。

-- | 检查运行时类型基础契约
spec :: Spec
spec = do
    describe "runtime type registry" $ do
        it "keeps a fixed identity encoding across releases" $ withType (listType intType) $ \tid ->
            encodeType tid `shouldBe` BS.pack "[1,[5,[[1,[]]]]]"
        it "parses nested Haskell-style type expressions" $
            parseTypeExpr "(Int, [Maybe String])" `shouldBe` Right (tupleType [intType, listType (maybeType stringType)])
        it "rejects constructor names without a token boundary" $
            parseTypeExpr "MaybeInt" `shouldSatisfy` isFailure
        it "restores stable nested identities from bootstrap metadata" $ do
            let ty = tupleType [listType intType, maybeType stringType]
            resolveType builtinTypes ty `shouldBe` resolveType builtinTypes ty
            fmap descriptorPhysical (resolveType builtinTypes ty)
                `shouldBe` Right (Product [Sequence SignedInteger, Optional Utf8])
        it "distinguishes logical String from List Int" $
            fmap descriptorId (resolveType builtinTypes stringType)
                `shouldNotBe` fmap descriptorId (resolveType builtinTypes (listType intType))
        it "rejects unknown constructors" $
            resolveType builtinTypes (TypeApply (TypeConstructorId 99) []) `shouldSatisfy` isFailure
        it "rejects unapplied List" $
            resolveType builtinTypes (TypeApply (TypeConstructorId 5) []) `shouldSatisfy` isFailure
        it "rejects arguments to Int" $
            resolveType builtinTypes (TypeApply (TypeConstructorId 1) [intType]) `shouldSatisfy` isFailure
    describe "runtime encoding" $ do
        it "round trips nested lists, optional values and tuples" $ do
            let ty = tupleType [listType (maybeType intType), stringType, doubleType, boolType]
                value = RTuple [RList [RMaybe Nothing, RMaybe (Just (RInt 42))], RString "你好", RDouble 1.25, RBool True]
            withType ty $ \tid -> do
                (encodeValue builtinTypes tid value >>= decodeValue builtinTypes tid) `shouldBe` Right value
                decodeType builtinTypes (encodeType tid) `shouldBe` Right tid
        it "round trips the empty tuple" $ withType (tupleType []) $ \tid ->
            (encodeValue builtinTypes tid (RTuple []) >>= decodeValue builtinTypes tid) `shouldBe` Right (RTuple [])
        it "rejects mismatched list elements" $ withType (listType intType) $ \tid ->
            encodeValue builtinTypes tid (RList [RString "bad"]) `shouldSatisfy` isFailure
        it "rejects tuple arity mismatch" $ withType (tupleType [intType]) $ \tid ->
            encodeValue builtinTypes tid (RTuple []) `shouldSatisfy` isFailure
        it "rejects non-finite floating point values" $ withType doubleType $ \tid ->
            encodeValue builtinTypes tid (RDouble (1 / 0)) `shouldSatisfy` isFailure
        it "rejects unsupported type encoding versions" $
            decodeType builtinTypes (BS.pack "[2,[1,[]]]") `shouldSatisfy` isFailure
        it "rejects overflowed constructor identities" $
            decodeType builtinTypes (BS.pack "[1,[18446744073709551617,[]]]") `shouldSatisfy` isFailure
        it "rejects fractional integer payloads" $ withType intType $ \tid ->
            decodeValue builtinTypes tid (BS.pack "[1,[1,[]],1.5]") `shouldSatisfy` isFailure
        it "rejects encoded identity mismatch" $ withType intType $ \tid ->
            decodeValue builtinTypes tid (BS.pack "[1,[4,[]],\"x\"]") `shouldSatisfy` isFailure
    describe "runtime binder and executor" $ do
        it "resolves case-insensitive length to a stable FunctionId" $ withFunctions $ \functions ->
            withType stringType $ \string -> withType intType $ \int ->
                resolveFunction functions "LENGTH" [string]
                    `shouldBe` Right (FunctionSignature (FunctionId 1) "length" [string] int)
        it "binds and executes nested calls" $ withFunctions $ \functions -> do
            let expression = Apply "not" [Apply "not" [Literal boolType (RBool True)]]
            (bindExpression builtinTypes functions [] expression >>= evaluateExpression builtinTypes functions [])
                `shouldBe` Right (RBool True)
        it "rejects a wrong function argument type during binding" $ withFunctions $ \functions ->
            bindExpression builtinTypes functions [] (Apply "length" [Literal intType (RInt 1)]) `shouldSatisfy` isFailure
        it "rejects unknown function identities" $ withFunctions $ \functions ->
            invokeFunction builtinTypes functions (FunctionId 99) [] `shouldSatisfy` isFailure
        it "rejects wrong runtime argument types" $ withFunctions $ \functions ->
            invokeFunction builtinTypes functions (FunctionId 1) [RInt 1] `shouldSatisfy` isFailure
        it "binds empty List and Nothing using explicit element types" $ withFunctions $ \functions -> do
            let expression = TupleLiteral [ListLiteral intType [], NothingLiteral stringType]
            (bindExpression builtinTypes functions [] expression >>= evaluateExpression builtinTypes functions [])
                `shouldBe` Right (RTuple [RList [], RMaybe Nothing])
        it "rejects mixed logical list types" $ withFunctions $ \functions ->
            bindExpression builtinTypes functions [] (ListLiteral intType [Literal doubleType (RDouble 1)]) `shouldSatisfy` isFailure
        it "binds queries using schema without reading storage" $ withFunctions $ \functions -> do
            let catalog = [("items", [("name", stringType), ("enabled", boolType)])]
                query = Projection [("size", Apply "length" [Reference "name"])]
                    (Selection (Reference "enabled") (TableScan "items"))
                scan name = if name == "items" then Right
                    [[("name", RString "甲乙"), ("enabled", RBool True)]
                    ,[("name", RString "hidden"), ("enabled", RBool False)]] else Left "unknown storage table"
            (bindPlan builtinTypes functions catalog query >>= executePlan builtinTypes functions scan)
                `shouldBe` Right [[("size", RInt 2)]]
        it "uses catalog existence even when storage could return rows" $ withFunctions $ \functions ->
            bindPlan builtinTypes functions [] (TableScan "missing") `shouldSatisfy` isFailure
        it "rejects non-boolean filters" $ withFunctions $ \functions ->
            bindPlan builtinTypes functions [] (Selection (Literal intType (RInt 1)) SingleRow) `shouldSatisfy` isFailure
        it "rejects schema drift returned by storage" $ withFunctions $ \functions ->
            (bindPlan builtinTypes functions [("t", [("x", intType)])] (TableScan "t")
                >>= executePlan builtinTypes functions (const (Right [[("x", RString "wrong")]]))) `shouldSatisfy` isFailure
        it "propagates storage failures" $ withFunctions $ \functions ->
            (bindPlan builtinTypes functions [("t", [])] (TableScan "t")
                >>= executePlan builtinTypes functions (const (Left "storage unavailable"))) `shouldBe` Left "storage unavailable"
    describe "SQL runtime functions" $ do
        it "binds the SQL entry point to output TypeIds" $ withType intType $ \tid -> do
            let result = parseStatement "SELECT length('abc') AS size" >>= Query.bindQuery []
            fmap Query.queryTypes result `shouldBe` Right [("size", Query.RuntimeType tid)]
        it "retains compatibility for existing temporal SQL types" $ do
            let result = parseStatement "SELECT DATE '2026-10-05' AS today" >>= Query.bindQuery []
            fmap Query.queryTypes result `shouldBe` Right [("today", Query.CompatibilityType SQL.CDate)]
        it "rejects composite ordering without an Ord implementation" $
            sql "SELECT LIST<Int>(1) AS xs ORDER BY xs" `shouldSatisfy` isFailure
        it "rejects composite extrema without an Ord implementation" $
            sql "SELECT MIN(LIST<Int>(1))" `shouldSatisfy` isFailure
        it "rejects unique runtime columns without a key implementation" $
            (parseStatement "CREATE TABLE keyed (xs [Int] UNIQUE)" >>= runStatement []) `shouldSatisfy` isFailure
        it "rejects primary runtime keys without a key implementation" $
            (parseStatement "CREATE TABLE keyed (xs [Int] PRIMARY KEY)" >>= runStatement []) `shouldSatisfy` isFailure
        it "constructs nested compound values in SQL" $ withType (listType (maybeType intType)) $ \tid ->
            sql "SELECT LIST<Maybe Int>(MAYBE<Int>(), MAYBE<Int>(42))"
                `shouldBe` Right [[SQL.VRuntime tid (RList [RMaybe Nothing, RMaybe (Just (RInt 42))])]]
        it "constructs heterogeneous tuples with stable type identity" $ withType (tupleType [intType, stringType]) $ \tid ->
            sql "SELECT TUPLE<Int, String>(1, 'a')" `shouldBe` Right [[SQL.VRuntime tid (RTuple [RInt 1, RString "a"])]]
        it "rejects constructor argument type mismatch" $
            sql "SELECT LIST<Int>('bad')" `shouldSatisfy` isFailure
        it "rejects constructor arity mismatch" $
            sql "SELECT MAYBE<Int>(1, 2)" `shouldSatisfy` isFailure
        it "rejects SQL NULL inside a typed compound value" $
            sql "SELECT LIST<Int>(NULL)" `shouldSatisfy` isFailure
        it "round trips typed runtime values through the shared wire protocol" $
            withType (maybeType (listType intType)) $ \tid -> do
                let value = SQL.VRuntime tid (RMaybe (Just (RList [RInt 1, RInt 2])))
                parseEither valueFromJSON (valueToJSON value) `shouldBe` Right value
        it "round trips typed schema identities through catalog type names" $
            withType (tupleType [intType, listType stringType]) $ \tid ->
                SQL.parseColumnType (SQL.typeName (SQL.CRuntime tid)) `shouldBe` Just (SQL.CRuntime tid)
        it "keeps hashing consistent for signed floating zero" $ withType (listType doubleType) $ \tid -> do
            let positive = SQL.VRuntime tid (RList [RDouble 0])
                negative = SQL.VRuntime tid (RList [RDouble (-0)])
            positive `shouldBe` negative
            hash positive `shouldBe` hash negative
        it "creates and writes compound columns through SQL" $ do
            let result = do
                    create <- parseStatement "CREATE TABLE composite (xs [Int], opt Maybe String, pair (Int, String))"
                    (db, _) <- runStatement [] create
                    insert <- parseStatement "INSERT INTO composite (xs, opt, pair) VALUES (LIST<Int>(1, 2), MAYBE<String>(), TUPLE<Int, String>(3, 'x'))"
                    (updated, _) <- runStatement db insert
                    select <- parseStatement "SELECT xs, opt, pair FROM composite"
                    rowsOf (runStatement updated select)
            result `shouldSatisfy` either (const False) ((== 1) . length)
        it "stores logical String with the generic runtime encoding" $ do
            let result = do
                    create <- parseStatement "CREATE TABLE strings (text String)"
                    (db, _) <- runStatement [] create
                    insert <- parseStatement "INSERT INTO strings (text) VALUES ('abc')"
                    (updated, _) <- runStatement db insert
                    select <- parseStatement "SELECT length(text) FROM strings"
                    map (map snd) <$> rowsOf (runStatement updated select)
            result `shouldBe` Right [[SQL.VInt 3]]
        it "runs length and not through resolved identities" $
            sql "SELECT length('你好'), not(TRUE)" `shouldBe` Right [[SQL.VInt 2, SQL.VBool False]]
        it "preserves nullable SQL function semantics" $
            sql "SELECT length(NULL), not(NULL)" `shouldBe` Right [[SQL.VNull, SQL.VNull]]
        it "binds calls before execution" $ do
            let bound = parseStatement "SELECT length('abc')" >>= prepare []
            case bound of
                Right SelectExpr {selectItems = [(_, BoundFunction (FunctionId 1) _ [_])]} -> pure ()
                _ -> expectationFailure (show bound)
        it "rejects unknown SQL functions before scanning" $
            sql "SELECT missing('abc')" `shouldSatisfy` isFailure
        it "rejects wrong argument count" $
            sql "SELECT length('a', 'b')" `shouldSatisfy` isFailure
        it "rejects wrong SQL argument type" $
            sql "SELECT length(1)" `shouldSatisfy` isFailure
        it "finds references inside calls for projection pushdown" $
            sql "SELECT length(name) FROM t WHERE not(enabled)" `shouldBe` Right [[SQL.VInt 3]]
        it "rewrites aggregates nested in calls" $
            sql "SELECT length(MIN(name)) FROM t" `shouldBe` Right [[SQL.VInt 2]]
        it "executes subqueries nested in function arguments" $
            sql "SELECT length((SELECT MIN(name) FROM t))" `shouldBe` Right [[SQL.VInt 2]]
        it "expands CTEs inside function arguments" $
            sql "WITH names AS (SELECT name FROM t) SELECT length((SELECT MIN(name) FROM names))"
                `shouldBe` Right [[SQL.VInt 2]]
        it "binds CHECK functions using the row schema" $ do
            let result = do
                    statement <- parseStatement "CREATE TABLE checked (name TEXT CHECK (length(name) > 1))"
                    (db, _) <- runStatement [] statement
                    insert <- parseStatement "INSERT INTO checked (name) VALUES ('abc')"
                    runStatement db insert
            result `shouldSatisfy` either (const False) (const True)

-- | 通过正式入口执行测试 SQL
sql :: String -> Either String [[SQL.Value]]
sql source = do
    statement <- parseStatement source
    rows <- rowsOf (runStatement database statement)
    Right (map (map snd) rows)
  where
    -- | 提供函数调用测试表
    database = [("t", SQL.Table "t" [("name", SQL.plainColumn SQL.CStr), ("enabled", SQL.plainColumn SQL.CBool)]
        [[("name", SQL.VStr "ab"), ("enabled", SQL.VBool True)]
        ,[("name", SQL.VStr "xyz"), ("enabled", SQL.VBool False)]] Nothing)]

-- | 判断结果是否明确失败
isFailure :: Either a b -> Bool
isFailure (Left _) = True
isFailure _ = False

-- | 准备已解析类型身份
withType :: TypeExpr -> (TypeId -> Expectation) -> Expectation
withType ty action = case resolveType builtinTypes ty of
    Left err -> expectationFailure err
    Right descriptor -> action (descriptorId descriptor)

-- | 准备内建函数注册表
withFunctions :: (FunctionRegistry -> Expectation) -> Expectation
withFunctions action = case builtinFunctions builtinTypes of
    Left err -> expectationFailure err
    Right functions -> action functions
