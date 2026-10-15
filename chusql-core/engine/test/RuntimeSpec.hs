module RuntimeSpec (spec) where

import ChuSQL.Core.Engine.Runtime.Types
import ChuSQL.Core.Engine.Runtime.Functions
import ChuSQL.Core.Engine.Runtime.Binding
import ChuSQL.Core.Engine.Runtime.Relation (bindRelOpColumns, bindWriteColumns)
import qualified ChuSQL.Core.Engine.Algebra.Op as Rel
import ChuSQL.Core.Engine.Algebra.Expr (evalExpr, subquerySlotsIn)
import ChuSQL.Core.Engine.Algebra.Eval (evalRelOp)
import ChuSQL.Core.Engine.Algebra.Sort (sortBoundRows)
import ChuSQL.Core.Engine.Storage.Memory (runMemoryStorage)
import qualified ChuSQL.Core.Engine.Runtime.Query as Query
import ChuSQL.Core.Engine (rowsOf, runStatement, runStatementM, bindWrite, executeWriteM)
import ChuSQL.Core.Engine.Semantic (prepare, prepareRowExpression)
import ChuSQL.Core.Engine.Syntax.Parser (parseStatement)
import ChuSQL.Core.Engine.Syntax.AST (Expr (..), Statement (..), Subquery (..), FromClause (..), SortDir (..))
import qualified ChuSQL.Core.Model as SQL
import ChuSQL.Core.Protocol (valueToJSON, valueFromJSON, TableInfo (..))
import qualified Data.Aeson as A
import Data.Word (Word64)
import Data.Aeson.Types (parseEither)
import Data.Hashable (hash)
import Control.Monad (foldM)
import qualified Data.ByteString.Lazy.Char8 as BS
import Data.List (isInfixOf)
import Data.Bifunctor (second)
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
    describe "bound write plans" $ do
        it "reuses INSERT targets and constraints while refreshing the auto counter" $ do
            let result = do
                    db <- sqlCatalog ["CREATE DOMAIN cached_quantity AS INT"
                        , "CREATE TABLE cached_insert (id INT AUTO_INCREMENT PRIMARY KEY, qty cached_quantity DEFAULT 2, tag TEXT CHECK (length(tag) > 1))"]
                    statement <- parseStatement "INSERT INTO cached_insert (tag) VALUES ('abc')"
                    bound <- bindWrite db statement
                    (first, inserted) <- runMemoryStorage (executeWriteM bound) db
                    first
                    let reversed = [(name, table {SQL.tableRows = map reverse (SQL.tableRows table)}) | (name, table) <- inserted]
                    (secondResult, final) <- runMemoryStorage (executeWriteM bound) reversed
                    secondResult
                    select <- parseStatement "SELECT id, qty, tag FROM cached_insert"
                    map (map snd) <$> rowsOf (runStatement final select)
            result `shouldBe` Right [[SQL.VInt 1, SQL.VInt 2, SQL.VStr "abc"], [SQL.VInt 2, SQL.VInt 2, SQL.VStr "abc"]]
        it "reuses ordered UPDATE targets and final-row uniqueness checks" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE cached_update (id INT PRIMARY KEY, qty INT NOT NULL UNIQUE CHECK (qty < 10))"
                        , "INSERT INTO cached_update (id, qty) VALUES (1, 1), (2, 8)"]
                    statement <- parseStatement "UPDATE cached_update SET qty = qty + 1, qty = qty + id WHERE id = 1"
                    bound <- bindWrite db statement
                    duplicate <- parseStatement "UPDATE cached_update SET qty = 8 WHERE id = 1" >>= bindWrite db
                    (first, updated) <- runMemoryStorage (executeWriteM bound) db
                    first
                    (secondResult, twice) <- runMemoryStorage (executeWriteM bound) updated
                    secondResult
                    (rejected, unchanged) <- runMemoryStorage (executeWriteM duplicate) twice
                    Right (SQL.tableRows <$> lookup "cached_update" twice, rejected
                        , SQL.tableRows <$> lookup "cached_update" unchanged)
                rows = Just [[("id", SQL.VInt 1), ("qty", SQL.VInt 5)], [("id", SQL.VInt 2), ("qty", SQL.VInt 8)]]
            result `shouldBe` Right (rows, Left "UNIQUE: duplicate value in column qty", rows)
        it "defers assignment errors until a reused plan matches a current row" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE deferred_write (id INT, qty INT CHECK (qty > 0))"]
                    statement <- parseStatement "UPDATE deferred_write SET qty = 1 / 0 WHERE id = 9"
                    bound <- bindWrite db statement
                    (empty, initial) <- runMemoryStorage (executeWriteM bound) db
                    empty
                    inserted <- parseStatement "INSERT INTO deferred_write (id, qty) VALUES (9, 5)" >>= runStatement initial
                    (rejected, final) <- runMemoryStorage (executeWriteM bound) (fst inserted)
                    Right (rejected, SQL.tableRows <$> lookup "deferred_write" final)
            result `shouldBe` Right (Left "division by zero", Just [[("id", SQL.VInt 9), ("qty", SQL.VInt 5)]])
        it "rejects stale INSERT UPDATE and DELETE plans before changing rows" $ do
            mapM_ (\source -> do
                let result = do
                        db <- sqlCatalog ["CREATE TABLE saved_write (id INT, qty INT)", "INSERT INTO saved_write (id, qty) VALUES (1, 5)"]
                        statement <- parseStatement source
                        bound <- bindWrite db statement
                        added <- parseStatement "ALTER TABLE saved_write ADD COLUMN extra TEXT" >>= runStatement db
                        restored <- parseStatement "ALTER TABLE saved_write DROP COLUMN extra" >>= runStatement (fst added)
                        (rejected, final) <- runMemoryStorage (executeWriteM bound) (fst restored)
                        Right (rejected, SQL.tableRows <$> lookup "saved_write" final)
                result `shouldBe` Right (Left "bound query schema changed: saved_write", Just [[("id", SQL.VInt 1), ("qty", SQL.VInt 5)]]))
                [ "INSERT INTO saved_write (id, qty) VALUES (2, 8)"
                , "UPDATE saved_write SET qty = qty + 1"
                , "DELETE FROM saved_write WHERE id = 1"
                ]
        it "reuses a write plan across data and unrelated schema changes" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE reusable_write (id INT, qty INT)", "CREATE TABLE unrelated_write (id INT)", "INSERT INTO reusable_write (id, qty) VALUES (1, 5)"]
                    statement <- parseStatement "UPDATE reusable_write SET qty = qty + 1 WHERE id = 1"
                    bound <- bindWrite db statement
                    changed <- parseStatement "INSERT INTO reusable_write (id, qty) VALUES (2, 9)" >>= runStatement db
                    unrelated <- parseStatement "ALTER TABLE unrelated_write ADD COLUMN extra TEXT" >>= runStatement (fst changed)
                    (first, updated) <- runMemoryStorage (executeWriteM bound) (fst unrelated)
                    first
                    (secondResult, final) <- runMemoryStorage (executeWriteM bound) updated
                    secondResult
                    Right (SQL.tableRows <$> lookup "reusable_write" final)
            result `shouldBe` Right (Just [[("id", SQL.VInt 1), ("qty", SQL.VInt 7)], [("id", SQL.VInt 2), ("qty", SQL.VInt 9)]])
        it "rejects replaced targets and changed subquery or DOMAIN dependencies" $ do
            let result = do
                    db <- sqlCatalog ["CREATE DOMAIN quantity AS INT", "CREATE TABLE dependent_write (id quantity)", "CREATE TABLE source_write (id INT)"]
                    statement <- parseStatement "UPDATE dependent_write SET id = (SELECT MAX(id) FROM source_write)"
                    let identified = [(name, table {SQL.tableIdentity = Just (SQL.TableIdentity 1 1)}) | (name, table) <- db]
                    bound <- bindWrite identified statement
                    let replaced = [(name, if name == "dependent_write" then table {SQL.tableIdentity = Just (SQL.TableIdentity 1 2)} else table) | (name, table) <- identified]
                        domainChanged = [(name, if name == SQL.domainTableName "quantity" then table {SQL.tableSchemaVersion = Just 2} else table) | (name, table) <- identified]
                    changed <- parseStatement "ALTER TABLE source_write ADD COLUMN extra TEXT" >>= runStatement identified
                    target <- fst <$> runMemoryStorage (executeWriteM bound) replaced
                    dependency <- fst <$> runMemoryStorage (executeWriteM bound) (fst changed)
                    domain <- fst <$> runMemoryStorage (executeWriteM bound) domainChanged
                    Right (target, dependency, domain)
            result `shouldBe` Right (Left "bound query object changed: dependent_write", Left "bound query schema changed: source_write", Left "bound query schema changed: __system_domain_quantity")
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
    describe "bound write expressions" $ do
        it "rejects missing existing key columns instead of skipping uniqueness checks" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE constraint_slots (id INT PRIMARY KEY, qty INT UNIQUE)"]
                    insert <- parseStatement "INSERT INTO constraint_slots (id, qty) VALUES (2, 3)"
                    let damaged = [(name, table {SQL.tableRows = [[("qty", SQL.VInt 9)]]}) | (name, table) <- db]
                    Right (runStatement damaged insert)
            case result of
                Right rejected -> rejected `shouldSatisfy` either (isInfixOf "missing bound column: id") (const False)
                Left err -> expectationFailure err
        it "checks existing INSERT keys and ALTER constraints with reversed physical columns" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE constraint_order (id INT PRIMARY KEY, qty INT UNIQUE)"
                        , "INSERT INTO constraint_order (id, qty) VALUES (1, 2), (2, 3)"]
                    let reversed = [(name, table {SQL.tableRows = map reverse (SQL.tableRows table)}) | (name, table) <- db]
                    insert <- parseStatement "INSERT INTO constraint_order (qty, id) VALUES (4, 3)"
                    (inserted, _) <- runStatement reversed insert
                    alter <- parseStatement "ALTER TABLE constraint_order ALTER COLUMN qty SET NOT NULL"
                    (altered, _) <- runStatement inserted alter
                    select <- parseStatement "SELECT id, qty FROM constraint_order"
                    map (map snd) <$> rowsOf (runStatement altered select)
            result `shouldBe` Right [[SQL.VInt 1, SQL.VInt 2], [SQL.VInt 2, SQL.VInt 3], [SQL.VInt 3, SQL.VInt 4]]
        it "rejects missing ALTER input before evaluating bound constraints" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE altered_missing (id INT, qty INT UNIQUE)"]
                    alter <- parseStatement "ALTER TABLE altered_missing ALTER COLUMN qty SET NOT NULL"
                    let damaged = [(name, table {SQL.tableRows = [[("id", SQL.VInt 1)]]}) | (name, table) <- db]
                    Right (runStatement damaged alter)
            case result of
                Right rejected -> rejected `shouldSatisfy` either (isInfixOf "missing bound column: qty") (const False)
                Left err -> expectationFailure err
        it "rejects UPDATE nullability and final-row uniqueness violations" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE update_constraints (id INT PRIMARY KEY, qty INT NOT NULL UNIQUE)"
                        , "INSERT INTO update_constraints (id, qty) VALUES (1, 2), (2, 3)"]
                    nullable <- parseStatement "UPDATE update_constraints SET qty = NULL WHERE id = 1"
                    duplicate <- parseStatement "UPDATE update_constraints SET qty = 3 WHERE id = 1"
                    primary <- parseStatement "UPDATE update_constraints SET id = 2 WHERE id = 1"
                    Right (db, [(nullable, "NOT NULL"), (duplicate, "UNIQUE"), (primary, "UNIQUE")])
            case result of
                Right (db, statements) -> mapM_ (\(statement, marker) ->
                    case runMemoryStorage (runStatementM statement) db of
                        Right (Left err, unchanged) -> do
                            err `shouldSatisfy` isInfixOf marker
                            map (second SQL.tableRows) unchanged
                                `shouldBe` map (second SQL.tableRows) db
                        other -> expectationFailure (show other)) statements
                Left err -> expectationFailure err
        it "checks UPDATE uniqueness after all rows rather than each replacement" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE update_unique (id INT PRIMARY KEY, qty INT UNIQUE)"
                        , "INSERT INTO update_unique (id, qty) VALUES (1, 2), (2, 3), (3, NULL), (4, NULL)"
                        , "UPDATE update_unique SET qty = qty + 1 WHERE id < 3"]
                    select <- parseStatement "SELECT id, qty FROM update_unique"
                    map (map snd) <$> rowsOf (runStatement db select)
            result `shouldBe` Right [[SQL.VInt 1, SQL.VInt 3], [SQL.VInt 2, SQL.VInt 4]
                , [SQL.VInt 3, SQL.VNull], [SQL.VInt 4, SQL.VNull]]
        it "rejects repeated INSERT targets instead of discarding later values" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE insert_targets (id INT, qty INT)"]
                    statement <- parseStatement "INSERT INTO insert_targets (id, qty, qty) VALUES (1, 2, 9)"
                    runStatement db statement
            result `shouldSatisfy` either (isInfixOf "INSERT: duplicate target column: qty") (const False)
        it "binds reordered INSERT inputs while retaining defaults, NULL and auto numbering" $ do
            let result = do
                    db <- sqlCatalog ["CREATE DOMAIN insert_label AS VARCHAR(4)"
                        , "CREATE TABLE insert_slots (id INT AUTO_INCREMENT PRIMARY KEY, label insert_label DEFAULT 'ok', qty INT DEFAULT 7)"
                        , "INSERT INTO insert_slots (qty, label, id) VALUES (NULL, 'ab', 10), (3, 'cd', NULL), (4, 'ef', 2)"
                        , "INSERT INTO insert_slots (label) VALUES ('gh')"
                        , "INSERT INTO insert_slots (qty) VALUES (8)"]
                    select <- parseStatement "SELECT id, label, qty FROM insert_slots"
                    map (map snd) <$> rowsOf (runStatement db select)
            result `shouldBe` Right [[SQL.VInt 10, SQL.VStr "ab", SQL.VNull]
                , [SQL.VInt 11, SQL.VStr "cd", SQL.VInt 3]
                , [SQL.VInt 2, SQL.VStr "ef", SQL.VInt 4]
                , [SQL.VInt 12, SQL.VStr "gh", SQL.VInt 7]
                , [SQL.VInt 13, SQL.VStr "ok", SQL.VInt 8]]
        it "binds write predicates and assignments to target schema positions" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE write_slots (id INT, qty INT)"]
                    statement <- parseStatement "UPDATE write_slots SET qty = qty + id WHERE id = 1"
                    prepare db statement >>= bindWriteColumns db
            result `shouldBe` Right (Update "write_slots"
                [("qty", Add (ColumnSlot 1 "qty") (ColumnSlot 0 "id"))]
                (Just (Eq (ColumnSlot 0 "id") (LitInt 1))))
        it "binds CHECK columns inside resolved function calls" $ do
            let columns = [("id", SQL.plainColumn SQL.CInt), ("name", SQL.plainColumn SQL.CStr)]
                result = prepareRowExpression columns (Gt (FunctionCall "length" [Col "name"]) (LitInt 1))
            case result of
                Right (Gt (BoundFunction (FunctionId 1) _ [ColumnSlot 1 "name"]) (LitInt 1)) -> pure ()
                _ -> expectationFailure (show result)
        it "preserves sequential updates and row order with reversed physical columns" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE write_slots (id INT, qty INT CHECK (qty >= 0))"
                        , "INSERT INTO write_slots (id, qty) VALUES (1, 2), (2, 10), (3, 5)"]
                    let reversed = [(name, table {SQL.tableRows = map reverse (SQL.tableRows table)}) | (name, table) <- db]
                    update <- parseStatement "UPDATE write_slots SET qty = qty + 1, qty = qty + id WHERE id = 1"
                    (updated, _) <- runStatement reversed update
                    delete <- parseStatement "DELETE FROM write_slots WHERE qty = 10"
                    (remaining, _) <- runStatement updated delete
                    select <- parseStatement "SELECT id, qty FROM write_slots"
                    map (map snd) <$> rowsOf (runStatement remaining select)
            result `shouldBe` Right [[SQL.VInt 1, SQL.VInt 4], [SQL.VInt 3, SQL.VInt 5]]
        it "keeps nested DOMAIN coercion, defaults and nullable writes" $ do
            let result = do
                    db <- sqlCatalog ["CREATE DOMAIN short_label AS VARCHAR(4)", "CREATE DOMAIN label AS short_label"
                        , "CREATE TABLE write_domains (id INT, name label CHECK (length(name) > 1), qty INT DEFAULT 2)"
                        , "INSERT INTO write_domains (id, name) VALUES (1, 'abc')"]
                    select <- parseStatement "SELECT id, name, qty FROM write_domains"
                    original <- map (map snd) <$> rowsOf (runStatement db select)
                    update <- parseStatement "UPDATE write_domains SET qty = NULL WHERE id = 1"
                    (updated, _) <- runStatement db update
                    rejected <- parseStatement "UPDATE write_domains SET name = 'abcde' WHERE id = 1"
                    rows <- map (map snd) <$> rowsOf (runStatement updated select)
                    Right (original, rows, runStatement updated rejected)
            case result of
                Right (original, rows, rejected) -> do
                    original `shouldBe` [[SQL.VInt 1, SQL.VStr "abc", SQL.VInt 2]]
                    rows `shouldBe` [[SQL.VInt 1, SQL.VStr "abc", SQL.VNull]]
                    rejected `shouldSatisfy` isFailure
                Left err -> expectationFailure err
        it "keeps correlated subqueries in bound assignments and DELETE predicates" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE write_outer (id INT, qty INT)"
                        , "CREATE TABLE write_inner (id INT)"
                        , "INSERT INTO write_outer (id, qty) VALUES (1, 2), (2, 10)"
                        , "INSERT INTO write_inner (id) VALUES (2)"]
                    update <- parseStatement "UPDATE write_outer SET qty = qty + (SELECT i.id FROM write_inner i WHERE i.id = qty) WHERE id = 1"
                    (updated, _) <- runStatement db update
                    select <- parseStatement "SELECT id, qty FROM write_outer"
                    changed <- map (map snd) <$> rowsOf (runStatement updated select)
                    delete <- parseStatement "DELETE FROM write_outer WHERE EXISTS (SELECT i.id FROM write_inner i WHERE i.id = qty - 2)"
                    (remaining, _) <- runStatement updated delete
                    kept <- map (map snd) <$> rowsOf (runStatement remaining select)
                    Right (changed, kept)
            result `shouldBe` Right ([[SQL.VInt 1, SQL.VInt 4], [SQL.VInt 2, SQL.VInt 10]]
                , [[SQL.VInt 2, SQL.VInt 10]])
        it "checks altered rows using schema order and keeps CHECK failures explicit" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE write_checks (id INT, qty INT CHECK (qty > id))"
                        , "INSERT INTO write_checks (id, qty) VALUES (1, 3)"]
                    let reversed = [(name, table {SQL.tableRows = map reverse (SQL.tableRows table)}) | (name, table) <- db]
                    alter <- parseStatement "ALTER TABLE write_checks ADD COLUMN note TEXT DEFAULT 'ok'"
                    (altered, _) <- runStatement reversed alter
                    invalid <- parseStatement "UPDATE write_checks SET qty = 0 WHERE id = 1"
                    Right (runStatement altered invalid)
            case result of
                Right (Left err) -> err `shouldSatisfy` isInfixOf "CHECK on qty failed"
                other -> expectationFailure (show other)
        it "does not evaluate skipped assignments or empty DELETE predicates" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE write_lazy (id INT, qty INT)"
                        , "INSERT INTO write_lazy (id, qty) VALUES (1, 2)"]
                    update <- parseStatement "UPDATE write_lazy SET qty = 1 / 0 WHERE id = 99"
                    (updated, _) <- runStatement db update
                    delete <- parseStatement "DELETE FROM write_lazy"
                    (empty, _) <- runStatement updated delete
                    invalid <- parseStatement "DELETE FROM write_lazy WHERE 1 / 0 = 1"
                    runStatement empty invalid
            result `shouldSatisfy` either (const False) (const True)
        it "rejects missing physical columns before a write" $ do
            let db = [("write_missing", SQL.Table "write_missing"
                    [("id", SQL.plainColumn SQL.CInt), ("qty", SQL.plainColumn SQL.CInt)]
                    [[("id", SQL.VInt 1)]] Nothing Nothing Nothing)]
            mapM_ (\source -> rowsOf (parseStatement source >>= runStatement db)
                `shouldBe` Left "missing bound column: qty")
                ["UPDATE write_missing SET id = 2", "DELETE FROM write_missing WHERE id = 1"]
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
        it "guards tables referenced only by HAVING or expression ordering" $ do
            let result query = do
                    db <- sqlCatalog ["CREATE TABLE main_rows (id INT)", "CREATE TABLE dependency_rows (id INT)"]
                    bound <- parseStatement query >>= Query.bindQuery db
                    changed <- parseStatement "ALTER TABLE dependency_rows ADD COLUMN extra TEXT" >>= runStatement db
                    fst <$> runMemoryStorage (Query.executeQueryM bound) (fst changed)
            mapM_ (\query -> result query `shouldBe` Right (Left "bound query schema changed: dependency_rows"))
                [ "SELECT COUNT(*) FROM main_rows HAVING COUNT(*) > (SELECT COUNT(*) FROM dependency_rows)"
                , "SELECT id FROM main_rows ORDER BY (SELECT COUNT(*) FROM dependency_rows) + id"
                ]
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
        it "binds subquery references to parent slots before execution" $ do
            let statement = Select [("value", Col "parent.id")] FromUnit Nothing [] Nothing [] Nothing
                query = Subquery statement ["parent.id"]
                source = Rel.Compute [("id", LitInt 1)] Rel.Unit
                plan = Rel.Compute [("value", ScalarSub query)] source
            bindRelOpColumns [] [] plan `shouldBe` Right (Rel.Compute
                [("value", ScalarSub (BoundSubquery statement ["parent.id"] [("parent.id", ColumnSlot 0 "id")]))] source)
            case bindRelOpColumns [] [] plan of
                Left err -> expectationFailure err
                Right bound -> evalRelOp [] bound `shouldBe` Right [[("value", SQL.VInt 1)]]
        it "keeps exact outer subquery references ahead of local physical fallbacks" $ do
            let statement = Select [("value", Col "parent.id")] FromUnit Nothing [] Nothing [] Nothing
                query = Subquery statement ["parent.id"]
                source = Rel.Compute [("id", LitInt 1)] Rel.Unit
            bindRelOpColumns [] ["parent.id"] (Rel.Compute [("value", ScalarSub query)] source)
                `shouldBe` Right (Rel.Compute [("value", ScalarSub (BoundSubquery statement ["parent.id"]
                    [("parent.id", OuterColumnSlot 1 0 "parent.id")]))] source)
        it "uses distinct bound positions when subqueries share a diagnostic name" $ do
            let statement = Select [("value", Col "x")] FromUnit Nothing [] Nothing [] Nothing
                first = ScalarSub (BoundSubquery statement ["x"] [("x", ColumnSlot 0 "a")])
                second = ScalarSub (BoundSubquery statement ["x"] [("x", ColumnSlot 1 "b")])
                table = SQL.Table "slot_cache" [("a", SQL.plainColumn SQL.CInt), ("b", SQL.plainColumn SQL.CInt)]
                    [[("a", SQL.VInt 1), ("b", SQL.VInt 2)], [("a", SQL.VInt 1), ("b", SQL.VInt 3)]] Nothing Nothing Nothing
                plan = Rel.Compute [("first", first), ("second", second)] (Rel.Scan Nothing "slot_cache" Nothing)
            subquerySlotsIn (Add first second) `shouldBe` [ColumnSlot 0 "a", ColumnSlot 1 "b"]
            evalRelOp [("slot_cache", table)] plan `shouldBe` Right
                [[("first", SQL.VInt 1), ("second", SQL.VInt 2)]
                , [("first", SQL.VInt 1), ("second", SQL.VInt 3)]]
        it "rejects bound subquery layout mismatches instead of looking up names" $ do
            let statement = Select [("value", Col "id")] FromUnit Nothing [] Nothing [] Nothing
                query = BoundSubquery statement ["id"] [("id", ColumnSlot 0 "id")]
                wrongLayout = Rel.Compute [("other", LitInt 2), ("id", LitInt 1)] Rel.Unit
            evalRelOp [] (Rel.Compute [("value", ScalarSub query)] wrongLayout)
                `shouldBe` Left "bound column position mismatch: id"
            prepareRowExpression [] (ScalarSub query)
                `shouldBe` Left "execution subquery cannot be used in semantic input"
            bindRelOpColumns [] [] (Rel.Compute [("value", ScalarSub
                (BoundSubquery statement ["id"] []))] Rel.Unit)
                `shouldBe` Left "bound subquery references mismatch"
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
        it "preserves duplicate column positions when rebinding local and outer slots" $ do
            let source = Rel.Compute [("id", LitInt 1), ("id", LitInt 9)] Rel.Unit
                plan = Rel.Compute [("value", ColumnSlot 1 "id")] source
            bindRelOpColumns [] [] plan `shouldBe` Right plan
            (bindRelOpColumns [] [] plan >>= evalRelOp [])
                `shouldBe` Right [[("value", SQL.VInt 9)]]
            bindRelOpColumns [] ["id", "id"] (Rel.Compute [("value", OuterColumnSlot 0 1 "id")] source)
                `shouldBe` Right (Rel.Compute [("value", OuterColumnSlot 2 1 "id")] source)
        it "rejects ambiguous slot relocation and invalid bound positions" $ do
            let source = Rel.Compute [("other", LitInt 0), ("id", LitInt 1), ("id", LitInt 9)] Rel.Unit
            bindRelOpColumns [] [] (Rel.Compute [("value", ColumnSlot 0 "id")] source)
                `shouldBe` Left "ambiguous bound column: id"
            bindRelOpColumns [] ["other", "id", "id"] (Rel.Compute [("value", OuterColumnSlot 0 0 "id")] Rel.Unit)
                `shouldBe` Left "ambiguous bound column: id"
            bindRelOpColumns [] [] (Rel.Compute [("value", ColumnSlot (-1) "id")] source)
                `shouldBe` Left "bound column position mismatch: id"
            bindRelOpColumns [] ["id"] (Rel.Compute [("value", OuterColumnSlot (-1) 0 "id")] Rel.Unit)
                `shouldBe` Left "bound column position mismatch: id"
            bindRelOpColumns [] [] (Rel.Compute [("value", ColumnSlot 0 "id")]
                (Rel.Compute [("other", LitInt 0), ("id", LitInt 9)] Rel.Unit))
                `shouldBe` Right (Rel.Compute [("value", ColumnSlot 1 "id")]
                    (Rel.Compute [("other", LitInt 0), ("id", LitInt 9)] Rel.Unit))
        it "keeps duplicate parent slots distinct through subquery rebinding and caching" $ do
            let statement = Select [("value", Col "x")] FromUnit Nothing [] Nothing [] Nothing
                first = ScalarSub (BoundSubquery statement ["x"] [("x", ColumnSlot 0 "id")])
                second = ScalarSub (BoundSubquery statement ["x"] [("x", ColumnSlot 1 "id")])
                source = Rel.Compute [("id", LitInt 1), ("id", LitInt 9)] Rel.Unit
                plan = Rel.Compute [("first", first), ("second", second)] source
            bindRelOpColumns [] [] plan `shouldBe` Right plan
            (bindRelOpColumns [] [] plan >>= evalRelOp [])
                `shouldBe` Right [[("first", SQL.VInt 1), ("second", SQL.VInt 9)]]
        it "keeps inner shadowing and multi-level correlated SQL references" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE scoped_items (id INT)"
                        , "INSERT INTO scoped_items (id) VALUES (1), (2)"]
                    statement <- parseStatement "SELECT o.id, (SELECT id FROM scoped_items i WHERE i.id = o.id) AS inner_id, (SELECT (SELECT o.id FROM scoped_items j WHERE j.id = i.id) FROM scoped_items i WHERE i.id = o.id) AS nested_id FROM scoped_items o"
                    rows <- rowsOf (runStatement db statement)
                    Right (map (map snd) rows)
            result `shouldBe` Right [[SQL.VInt 1, SQL.VInt 1, SQL.VInt 1], [SQL.VInt 2, SQL.VInt 2, SQL.VInt 2]]
        it "resolves the nearest of several enclosing column scopes" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE scope_far (id INT)", "CREATE TABLE scope_near (id INT)"
                        , "INSERT INTO scope_far (id) VALUES (1), (2)", "INSERT INTO scope_near (id) VALUES (10), (11)"]
                    statement <- parseStatement "SELECT o.id, (SELECT (SELECT id) FROM scope_near n WHERE n.id = o.id + 9) AS nearest FROM scope_far o"
                    map (map snd) <$> rowsOf (runStatement db statement)
            result `shouldBe` Right [[SQL.VInt 1, SQL.VInt 10], [SQL.VInt 2, SQL.VInt 11]]
        it "keeps enclosing scopes separate when aliases and DOMAIN types overlap" $ withType stringType $ \tid -> do
            let result = do
                    db <- sqlCatalog ["CREATE DOMAIN scope_label AS VARCHAR(4)"
                        , "CREATE TABLE scope_far (value INT)", "CREATE TABLE scope_near (value scope_label)"
                        , "INSERT INTO scope_far (value) VALUES (7)", "INSERT INTO scope_near (value) VALUES ('abc')"]
                    statement <- parseStatement "SELECT (SELECT (SELECT length(s.value)) FROM scope_near s) AS size FROM scope_far s"
                    rows <- map (map snd) <$> rowsOf (runStatement db statement)
                    domain <- parseStatement "SELECT (SELECT (SELECT s.value) FROM scope_near s) AS label FROM scope_far s"
                    bound <- Query.bindQuery db domain
                    Right (rows, Query.queryTypes bound)
            result `shouldBe` Right ([[SQL.VInt 3]], [("label", Query.DomainType "scope_label" (Query.RuntimeType tid))])
        it "skips empty or unrelated enclosing scopes and preserves NULL" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE scope_far (id INT)", "CREATE TABLE scope_near (name TEXT)"
                        , "INSERT INTO scope_far (id) VALUES (NULL), (4)", "INSERT INTO scope_near (name) VALUES ('n')"]
                    statement <- parseStatement "SELECT (SELECT (SELECT (SELECT id)) FROM scope_near n) AS echoed FROM scope_far o"
                    map (map snd) <$> rowsOf (runStatement db statement)
            result `shouldBe` Right [[SQL.VNull], [SQL.VInt 4]]
        it "does not resolve an ambiguous nearer scope against a farther one" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE scope_far (id INT)", "CREATE TABLE scope_near (id INT)"]
                    statement <- parseStatement "SELECT (SELECT (SELECT id) FROM scope_near a JOIN scope_near b ON a.id = b.id) FROM scope_far o"
                    rowsOf (runStatement db statement)
            result `shouldSatisfy` either (isInfixOf "ambiguous column") (const False)
        it "does not take a missing column from a shadowed source alias" $ do
            let result source = do
                    db <- sqlCatalog ["CREATE TABLE source_far (id INT)", "CREATE TABLE source_near (other INT)"
                        , "INSERT INTO source_far (id) VALUES (1)", "INSERT INTO source_near (other) VALUES (10)"]
                    statement <- parseStatement source
                    rowsOf (runStatement db statement)
            mapM_ (\source -> result source `shouldSatisfy` either (isInfixOf "unknown column") (const False))
                ["SELECT (SELECT s.id FROM source_near s) FROM source_far s"
                , "SELECT (SELECT (SELECT s.id) FROM source_near s) FROM source_far s"]
        it "keeps bare far columns visible when the nearer alias has no such column" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE source_far (id INT)", "CREATE TABLE source_near (other INT)"
                        , "INSERT INTO source_far (id) VALUES (1), (2)", "INSERT INTO source_near (other) VALUES (10)"]
                    statement <- parseStatement "SELECT (SELECT (SELECT id) FROM source_near s) AS inherited FROM source_far s"
                    map (map snd) <$> rowsOf (runStatement db statement)
            result `shouldBe` Right [[SQL.VInt 1], [SQL.VInt 2]]
        it "uses lexical scopes in IN, EXISTS and quantified subqueries" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE scope_far (id INT)", "CREATE TABLE scope_near (id INT)"
                        , "INSERT INTO scope_far (id) VALUES (1)", "INSERT INTO scope_near (id) VALUES (10)"]
                    statement <- parseStatement "SELECT (SELECT 10 IN (SELECT id) AND EXISTS (SELECT 1 WHERE id = 10) AND 10 = ALL (SELECT id) FROM scope_near n) AS matched FROM scope_far o"
                    map (map snd) <$> rowsOf (runStatement db statement)
            result `shouldBe` Right [[SQL.VBool True]]
        it "resolves JOIN conditions against the nearest enclosing scope" $ do
            let result = do
                    db <- sqlCatalog ["CREATE TABLE scope_far (value INT)", "CREATE TABLE scope_near (value INT)"
                        , "CREATE TABLE scope_join (id INT)", "INSERT INTO scope_far (value) VALUES (1)"
                        , "INSERT INTO scope_near (value) VALUES (10)", "INSERT INTO scope_join (id) VALUES (10)"]
                    statement <- parseStatement "SELECT (SELECT (SELECT a.id FROM scope_join a JOIN scope_join b ON a.id = value AND b.id = a.id) FROM scope_near n) AS matched FROM scope_far o"
                    map (map snd) <$> rowsOf (runStatement db statement)
            result `shouldBe` Right [[SQL.VInt 10]]
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
