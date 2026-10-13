module RuntimeSpec (spec) where

import ChuSQL.Core.Engine.Runtime.Types
import ChuSQL.Core.Engine.Runtime.Functions
import ChuSQL.Core.Engine.Runtime.Binding
import ChuSQL.Core.Engine.Runtime.Relation (bindRelOpColumns)
import qualified ChuSQL.Core.Engine.Algebra.Op as Rel
import ChuSQL.Core.Engine.Algebra.Expr (evalExpr)
import ChuSQL.Core.Engine.Algebra.Eval (evalRelOp)
import ChuSQL.Core.Engine.Algebra.Sort (sortBoundRows)
import ChuSQL.Core.Engine.Storage.Memory (runMemoryStorage)
import qualified ChuSQL.Core.Engine.Runtime.Query as Query
import ChuSQL.Core.Engine (rowsOf, runStatement)
import ChuSQL.Core.Engine.Semantic (prepare, prepareRowExpression)
import ChuSQL.Core.Engine.Syntax.Parser (parseStatement)
import ChuSQL.Core.Engine.Syntax.AST (Expr (..), Statement (..), SortDir (..))
import qualified ChuSQL.Core.Model as SQL
import ChuSQL.Core.Protocol (valueToJSON, valueFromJSON, TableInfo (..))
import qualified Data.Aeson as A
import Data.Word (Word64)
import Data.Aeson.Types (parseEither)
import Data.Hashable (hash)
import Control.Monad (foldM)
import qualified Data.ByteString.Lazy.Char8 as BS
import Test.Hspec

-- 运行时类型、函数绑定、物理编码和类型化查询测试。

-- | 执行语句建立测试目录
sqlCatalog :: [String] -> Either String SQL.Database
sqlCatalog = foldM apply []
  where
    -- | 更新当前测试数据库
    apply db source = do
        statement <- parseStatement source
        fst <$> runStatement db statement

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
        it "uses the bound column slot and rejects reordered expression input" $ withFunctions $ \functions ->
            withType intType $ \int -> withType stringType $ \string -> do
                let bound = bindExpression builtinTypes functions [("count", int), ("name", string)]
                        (Apply "length" [Reference "name"])
                (bound >>= evaluateExpression builtinTypes functions [("count", RInt 1), ("name", RString "abc")])
                    `shouldBe` Right (RInt 3)
                (bound >>= evaluateExpression builtinTypes functions [("name", RString "abc"), ("count", RInt 1)])
                    `shouldBe` Left "bound column position mismatch: name"
                (bound >>= evaluateExpression builtinTypes functions [("count", RInt 1)])
                    `shouldBe` Left "missing bound column: name"
        it "rejects ambiguous references before binding a slot" $ withFunctions $ \functions ->
            withType intType $ \int ->
                bindExpression builtinTypes functions [("id", int), ("id", int)] (Reference "id")
                    `shouldSatisfy` isFailure
        it "normalizes storage order at scan boundaries before evaluating slots" $ withFunctions $ \functions -> do
            let catalog = [("items", [("name", stringType), ("enabled", boolType)])]
                query = Projection [("size", Apply "length" [Reference "name"])]
                    (Selection (Reference "enabled") (TableScan "items"))
                scan _ = Right [[("enabled", RBool True), ("name", RString "abc")]]
            (bindPlan builtinTypes functions catalog query >>= executePlan builtinTypes functions scan)
                `shouldBe` Right [[("size", RInt 3)]]
        it "rebinds slots to each projection output schema" $ withFunctions $ \functions -> do
            let catalog = [("items", [("name", stringType), ("enabled", boolType)])]
                source = Projection [("enabled", Reference "enabled"), ("label", Reference "name")]
                    (TableScan "items")
                query = Projection [("size", Apply "length" [Reference "label"])] source
                scan _ = Right [[("name", RString "abc"), ("enabled", RBool True)]]
            (bindPlan builtinTypes functions catalog query >>= executePlan builtinTypes functions scan)
                `shouldBe` Right [[("size", RInt 3)]]
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
        it "validates transitive DOMAIN dependencies and ignores unrelated definitions" $ do
            let result = do
                    db <- sqlCatalog ["CREATE DOMAIN amount AS INT", "CREATE DOMAIN total AS amount"
                        , "CREATE DOMAIN unrelated AS TEXT", "CREATE TABLE amounts (value total)"]
                    statement <- parseStatement "SELECT value FROM amounts"
                    bound <- Query.bindQuery db statement
                    let change target = [(name, if name == target then table {SQL.tableCols = [("base", SQL.plainColumn SQL.CBool)]}
                            else table) | (name, table) <- db]
                    accepted <- fst <$> runMemoryStorage (Query.executeQueryM bound) (change "__system_domain_unrelated")
                    rejected <- fst <$> runMemoryStorage (Query.executeQueryM bound) (change "__system_domain_amount")
                    Right (accepted, rejected)
            result `shouldBe` Right (Right [], Left "bound query schema changed: __system_domain_amount")
        it "preserves exact catalog identities and rejects malformed identifiers" $ do
            let identity = SQL.TableIdentity (maxBound :: Word64) (maxBound :: Word64)
                info = TableInfo "items" [] 0 [] [] [] (Just identity) Nothing
                decode text = A.eitherDecode (BS.pack text) :: Either String TableInfo
            A.fromJSON (A.toJSON info) `shouldBe` A.Success info
            fmap tiIdentity (decode "{\"table\":\"items\",\"columns\":[],\"row_count\":0}") `shouldBe` Right Nothing
            fmap tiIdentity (decode "{\"table\":\"items\",\"columns\":[],\"row_count\":0,\"database_id\":0,\"object_id\":1}")
                `shouldBe` Right (Just (SQL.TableIdentity 0 1))
            mapM_ (\fields -> decode ("{\"table\":\"items\",\"columns\":[],\"row_count\":0," ++ fields ++ "}")
                `shouldSatisfy` isFailure)
                ["\"database_id\":1", "\"database_id\":1,\"object_id\":-1"
                , "\"database_id\":1,\"object_id\":1.5", "\"database_id\":1,\"object_id\":\"18446744073709551616\""
                , "\"database_id\":1,\"object_id\":1e99999999"]
        it "rejects a same-schema replacement or loss of object identity" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE bound_items (id INT)"]
                    let identified = [(name, table {SQL.tableIdentity = Just (SQL.TableIdentity 1 1)}) | (name, table) <- db]
                    statement <- parseStatement "SELECT id FROM bound_items"
                    bound <- Query.bindQuery identified statement
                    let replaced = [(name, table {SQL.tableIdentity = Just (SQL.TableIdentity 1 2)}) | (name, table) <- db]
                    changed <- fst <$> runMemoryStorage (Query.executeQueryM bound) replaced
                    lost <- fst <$> runMemoryStorage (Query.executeQueryM bound) db
                    Right (changed, lost)
            result `shouldBe` Right (Left "bound query object changed: bound_items", Left "bound query object changed: bound_items")
        it "preserves exact schema versions and rejects malformed versions" $ do
            let info = TableInfo "items" [] 0 [] [] [] Nothing (Just maxBound)
                decode version = A.eitherDecode (BS.pack ("{\"table\":\"items\",\"columns\":[],\"row_count\":0,\"schema_version\":" ++ version ++ "}")) :: Either String TableInfo
            A.fromJSON (A.toJSON info) `shouldBe` A.Success info
            fmap tiSchemaVersion (decode "0") `shouldBe` Right Nothing
            fmap tiSchemaVersion (decode "7") `shouldBe` Right (Just 7)
            mapM_ (\version -> decode version `shouldSatisfy` isFailure)
                ["-1", "1.5", "\"18446744073709551616\"", "\"+1\"", "1e99999999"]
        it "invalidates a bound query after restoring its original columns" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE versioned (id INT)"]
                    statement <- parseStatement "SELECT id FROM versioned"
                    bound <- Query.bindQuery db statement
                    added <- parseStatement "ALTER TABLE versioned ADD COLUMN extra TEXT" >>= runStatement db
                    restored <- parseStatement "ALTER TABLE versioned DROP COLUMN extra" >>= runStatement (fst added)
                    fst <$> runMemoryStorage (Query.executeQueryM bound) (fst restored)
            result `shouldBe` Right (Left "bound query schema changed: versioned")
        it "rejects loss of a bound schema version" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE versioned (id INT)"]
                    statement <- parseStatement "SELECT id FROM versioned"
                    bound <- Query.bindQuery db statement
                    fst <$> runMemoryStorage (Query.executeQueryM bound)
                        [(name, table {SQL.tableSchemaVersion = Nothing}) | (name, table) <- db]
            result `shouldBe` Right (Left "bound query schema changed: versioned")
        it "ignores unrelated table changes but validates nested query dependencies" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE referenced (id INT)", "CREATE TABLE unrelated (id INT)"]
                    statement <- parseStatement "SELECT length((SELECT MIN(name) FROM (SELECT 'abc' AS name FROM referenced) d)) AS n"
                    bound <- Query.bindQuery db statement
                    unrelated <- parseStatement "ALTER TABLE unrelated ADD COLUMN extra TEXT" >>= runStatement db
                    accepted <- fst <$> runMemoryStorage (Query.executeQueryM bound) (fst unrelated)
                    referenced <- parseStatement "ALTER TABLE referenced ADD COLUMN extra TEXT" >>= runStatement db
                    rejected <- fst <$> runMemoryStorage (Query.executeQueryM bound) (fst referenced)
                    Right (accepted, rejected)
            result `shouldBe` Right (Right [[("n", SQL.VNull)]], Left "bound query schema changed: referenced")
        it "rejects schema drift before executing an empty bound query" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE bound_items (id INT)"]
                    statement <- parseStatement "SELECT id FROM bound_items"
                    bound <- Query.bindQuery db statement
                    changed <- parseStatement "ALTER TABLE bound_items ADD COLUMN extra TEXT" >>= runStatement db
                    fst <$> runMemoryStorage (Query.executeQueryM bound) (fst changed)
            result `shouldBe` Right (Left "bound query schema changed: bound_items")
        it "keeps binding valid across row and statistics changes" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE bound_items (id INT)"]
                    statement <- parseStatement "SELECT id FROM bound_items"
                    bound <- Query.bindQuery db statement
                    changed <- parseStatement "INSERT INTO bound_items (id) VALUES (7)" >>= runStatement db
                    let current = [(name, table {SQL.tableMeta = Just (SQL.TableMeta 100 [] [] [])})
                            | (name, table) <- fst changed]
                    fst <$> runMemoryStorage (Query.executeQueryM bound) current
            result `shouldBe` Right (Right [[("id", SQL.VInt 7)]])
        it "binds grouping keys and then sorting to the grouped output" $ do
            let source = Rel.Compute [("first", LitInt 1), ("key", LitInt 2)] Rel.Unit
                plan = Rel.Sort [("key", Desc)] (Rel.Aggregate ["key"] [("n", CountAll)] source)
                bound = Rel.BoundSort [(ColumnSlot 0 "key", Desc)]
                    (Rel.BoundAggregate [("key", ColumnSlot 1 "key")] [("n", CountAll)] source)
            bindRelOpColumns [] [] plan `shouldBe` Right bound
            evalRelOp [] bound `shouldBe` Right [[("key", SQL.VInt 2), ("n", SQL.VInt 1)]]
            evalRelOp [] (Rel.BoundAggregate [("key", ColumnSlot 0 "key")] [] source)
                `shouldBe` Left "bound column position mismatch: key"
        it "sorts bound NULL keys stably and rejects wrong slots" $ do
            let a = [("id", SQL.VInt 1), ("key", SQL.VInt 2)]
                b = [("id", SQL.VInt 2), ("key", SQL.VNull)]
                c = [("id", SQL.VInt 3), ("key", SQL.VInt 2)]
            sortBoundRows [(ColumnSlot 1 "key", Asc)] [] [a, b, c] `shouldBe` Right [b, a, c]
            sortBoundRows [(ColumnSlot 1 "key", Desc)] [] [a, b, c] `shouldBe` Right [a, c, b]
            sortBoundRows [(ColumnSlot 0 "key", Asc)] [] [a]
                `shouldBe` Left "bound column position mismatch: key"
        it "preserves NULL group order and empty-input global aggregates" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE grouped_slots (id INT, tag TEXT)"
                        , "INSERT INTO grouped_slots (id, tag) VALUES (1, 'b'), (2, NULL), (3, 'b')"]
                    grouped <- parseStatement "SELECT tag, COUNT(*) AS n FROM grouped_slots GROUP BY tag"
                    groups <- rowsOf (runStatement db grouped)
                    empty <- parseStatement "SELECT COUNT(*) AS n, SUM(id) AS total FROM grouped_slots WHERE id < 0"
                    aggregates <- rowsOf (runStatement db empty)
                    Right (map (map snd) groups, map (map snd) aggregates)
            result `shouldBe` Right ([[SQL.VStr "b", SQL.VInt 2], [SQL.VNull, SQL.VInt 1]]
                , [[SQL.VInt 0, SQL.VNull]])
        it "binds SQL relation expressions to the optimized input slots" $ do
            let plan = Rel.Compute [("value", Col "second")]
                    (Rel.Compute [("first", LitInt 1), ("second", LitInt 2)] Rel.Unit)
            bindRelOpColumns [] [] plan `shouldBe` Right (Rel.Compute [("value", ColumnSlot 1 "second")]
                (Rel.Compute [("first", LitInt 1), ("second", LitInt 2)] Rel.Unit))
            bindRelOpColumns [] [] (Rel.Filter (Col "missing") Rel.Unit)
                `shouldBe` Left "unknown column: missing"
        it "checks SQL slot position instead of searching another column" $ do
            evalExpr (ColumnSlot 1 "name") [("name", SQL.VStr "abc"), ("id", SQL.VInt 1)]
                `shouldBe` Left "bound column position mismatch: name"
            evalExpr (ColumnSlot (-1) "name") [("name", SQL.VStr "abc")]
                `shouldBe` Left "bound column position mismatch: name"
        it "binds projections without losing repeated outputs or empty rows" $ do
            let source = Rel.Compute [("first", LitInt 1), ("second", LitInt 2)] Rel.Unit
            bindRelOpColumns [] [] (Rel.Project ["second", "first", "second"] source)
                `shouldBe` Right (Rel.Compute [("second", ColumnSlot 1 "second")
                    , ("first", ColumnSlot 0 "first"), ("second", ColumnSlot 1 "second")] source)
            bindRelOpColumns [] [] (Rel.Project [] source)
                `shouldBe` Right (Rel.Compute [] source)
            bindRelOpColumns [] [] (Rel.Project ["*"] source)
                `shouldBe` Right (Rel.Compute [("first", ColumnSlot 0 "first"), ("second", ColumnSlot 1 "second")] source)
        it "preserves repeated SQL outputs and correlated projection values" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE slot_items (id INT)"
                        , "INSERT INTO slot_items (id) VALUES (1), (2)"]
                    repeated <- parseStatement "SELECT id, id FROM slot_items"
                    repeatedRows <- rowsOf (runStatement db repeated)
                    correlated <- parseStatement "SELECT o.id, (SELECT o.id FROM slot_items i WHERE i.id = o.id) AS echoed FROM slot_items o"
                    correlatedRows <- rowsOf (runStatement db correlated)
                    Right (map (map snd) repeatedRows, map (map snd) correlatedRows)
            result `shouldBe` Right ([[SQL.VInt 1, SQL.VInt 1], [SQL.VInt 2, SQL.VInt 2]]
                , [[SQL.VInt 1, SQL.VInt 1], [SQL.VInt 2, SQL.VInt 2]])
        it "binds outer references after the local relation layout" $ do
            bindRelOpColumns [] ["outer.id"] (Rel.Compute [("id", Col "outer.id")] Rel.Unit)
                `shouldBe` Right (Rel.Compute [("id", OuterColumnSlot 0 0 "outer.id")] Rel.Unit)
        it "preserves outer scope identity when rebinding beside a local name" $ do
            let source = Rel.Compute [("id", LitInt 1)] Rel.Unit
                expression = OuterColumnSlot 0 0 "id"
                bound = bindRelOpColumns [] ["id"] (Rel.Compute [("outer", expression)] source)
            bound `shouldBe` Right (Rel.Compute [("outer", OuterColumnSlot 1 0 "id")] source)
            evalExpr (OuterColumnSlot 1 0 "id") [("id", SQL.VInt 1), ("id", SQL.VInt 9)]
                `shouldBe` Right (SQL.VInt 9)
            bindRelOpColumns [] [] (Rel.Compute [("outer", expression)] source)
                `shouldBe` Left "unknown column: id"
            prepareRowExpression [] expression `shouldBe` Left "execution column cannot be used in semantic input"
            prepareRowExpression [] (ColumnSlot 0 "id") `shouldBe` Left "execution column cannot be used in semantic input"
        it "keeps inner shadowing and multi-level correlated SQL references" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE scoped_items (id INT)"
                        , "INSERT INTO scoped_items (id) VALUES (1), (2)"]
                    statement <- parseStatement "SELECT o.id, (SELECT id FROM scoped_items i WHERE i.id = o.id) AS inner_id, (SELECT (SELECT o.id FROM scoped_items j WHERE j.id = i.id) FROM scoped_items i WHERE i.id = o.id) AS nested_id FROM scoped_items o"
                    rows <- rowsOf (runStatement db statement)
                    Right (map (map snd) rows)
            result `shouldBe` Right [[SQL.VInt 1, SQL.VInt 1, SQL.VInt 1], [SQL.VInt 2, SQL.VInt 2, SQL.VInt 2]]
        it "executes SQL against reversed physical column order" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE slot_items (id INT, name TEXT)"
                        , "INSERT INTO slot_items (id, name) VALUES (1, 'abc'), (2, 'xy')"]
                    let reordered = [(name, table {SQL.tableRows = map reverse (SQL.tableRows table)}) | (name, table) <- db]
                    statement <- parseStatement "SELECT length(name) AS size, id + 1 AS next FROM slot_items WHERE id > 0 ORDER BY size"
                    rowsOf (runStatement reordered statement)
            fmap (map (map snd)) result `shouldBe` Right [[SQL.VInt 2, SQL.VInt 3], [SQL.VInt 3, SQL.VInt 2]]
        it "binds the SQL entry point to output TypeIds" $ withType intType $ \tid -> do
            let result = parseStatement "SELECT length('abc') AS size" >>= Query.bindQuery []
            fmap Query.queryTypes result `shouldBe` Right [("size", Query.RuntimeType tid)]
        it "retains compatibility for existing temporal SQL types" $ do
            let result = parseStatement "SELECT DATE '2026-10-05' AS today" >>= Query.bindQuery []
            fmap Query.queryTypes result `shouldBe` Right [("today", Query.CompatibilityType SQL.CDate)]
        it "retains nested DOMAIN identities through derived aliases and DISTINCT" $ withType intType $ \tid -> do
            let result = do
                    db <- sqlCatalog ["CREATE DOMAIN quantity AS INT", "CREATE DOMAIN stock AS quantity"
                        , "CREATE TABLE inventory (qty stock)", "INSERT INTO inventory (qty) VALUES (2), (NULL)"]
                    statement <- parseStatement "SELECT DISTINCT d.qty AS available FROM (SELECT qty FROM inventory) d"
                    bound <- Query.bindQuery db statement
                    rows <- rowsOf (runStatement db statement)
                    Right (Query.queryTypes bound, map (map snd) rows)
            result `shouldBe` Right ([("available", Query.DomainType "stock"
                (Query.DomainType "quantity" (Query.RuntimeType tid)))], [[SQL.VInt 2], [SQL.VNull]])
        it "preserves temporal and binary DOMAIN wrappers" $ do
            let result = do
                    db <- sqlCatalog ["CREATE DOMAIN day AS DATE", "CREATE DOMAIN payload AS BLOB"
                        , "CREATE TABLE documents (created day, contents payload)"]
                    statement <- parseStatement "SELECT created, contents FROM documents"
                    Query.queryTypes <$> Query.bindQuery db statement
            result `shouldBe` Right [("created", Query.DomainType "day" (Query.CompatibilityType SQL.CDate))
                , ("contents", Query.DomainType "payload" (Query.CompatibilityType SQL.CBlob))]
        it "uses base function semantics without labeling function results as DOMAIN" $ withType intType $ \tid -> do
            let result = do
                    db <- sqlCatalog ["CREATE DOMAIN label AS VARCHAR(3)", "CREATE TABLE names (name label)"
                        , "INSERT INTO names (name) VALUES ('abc')"]
                    statement <- parseStatement "SELECT length(name) AS size FROM names"
                    bound <- Query.bindQuery db statement
                    rows <- rowsOf (runStatement db statement)
                    Right (Query.queryTypes bound, map (map snd) rows)
            result `shouldBe` Right ([("size", Query.RuntimeType tid)], [[SQL.VInt 3]])
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
                Right Select {selectItems = [(_, BoundFunction (FunctionId 1) _ [_])]} -> pure ()
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
        ,[("name", SQL.VStr "xyz"), ("enabled", SQL.VBool False)]] Nothing Nothing Nothing)]

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
