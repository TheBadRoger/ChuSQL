module ChuSQL.Core.Engine.Runtime.Dependencies (queryTables, domainDependencies, typeDependencies, validateDependencies) where

import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Model (ColumnType (..), Database, columnType, domainTableName, lastTablePart, tableCols, tableIdentity, tableSchemaVersion)
import Data.List (nub)

-- 收集读写语句及嵌套表达式的表依赖，供执行期校验目录。

-- | 复查绑定依赖的身份、版本及列定义
validateDependencies :: Database -> Database -> Either String ()
validateDependencies expected current = mapM_ validate expected
  where
    -- \| 数据和统计变化不使结构绑定失效
    validate (name, table) = case lookup name current of
        Just actual | tableIdentity actual /= tableIdentity table -> Left ("bound query object changed: " ++ name)
        Just actual | tableSchemaVersion actual /= tableSchemaVersion table -> Left ("bound query schema changed: " ++ name)
        Just actual | tableCols actual == tableCols table -> Right ()
        _ -> Left ("bound query schema changed: " ++ name)

-- | 收集读写目标、来源及子查询依赖
queryTables :: Statement -> [String]
queryTables statement =
    nub
        ( case statement of
            SelectDistinct child -> queryTables child
            Select items source condition _ having ordering _ ->
                sourceTables source
                    ++ concatMap (expressionTables . snd) items
                    ++ concatMap expressionTables condition
                    ++ concatMap expressionTables having
                    ++ concatMap (maybe [] expressionTables . orderKeyExpression . fst) ordering
            Insert target _ rows -> target : concatMap (concatMap expressionTables) rows
            Update target assignments condition ->
                target
                    : concatMap (expressionTables . snd) assignments
                    ++ concatMap expressionTables condition
            Delete target condition -> target : concatMap expressionTables condition
            DropTable target -> [target]
            CreateIndex target _ -> [target]
            DropIndex target _ -> [target]
            DropColumn target _ -> [target]
            AddColumn target _ -> [target]
            RenameColumn target _ _ -> [target]
            AlterColumnType target _ _ -> [target]
            AlterColumnDefault target _ _ -> [target]
            AlterColumnNull target _ _ -> [target]
            _ -> []
        )

-- | 收集来源列依赖的领域定义
domainDependencies :: Database -> [String] -> [String]
domainDependencies catalog sources =
    typeDependencies
        catalog
        [ columnType column
        | (name, table) <- catalog
        , name `elem` sources
        , (_, column) <- tableCols table
        ]

-- | 收集声明类型依赖的领域目录
typeDependencies :: Database -> [ColumnType] -> [String]
typeDependencies catalog types = collect [] (concatMap typeDomains types)
  where
    -- \| 递归展开定义并防止重复访问
    collect seen [] = seen
    collect seen (domain : remaining) =
        let definitions =
                [ (name, table)
                | (name, table) <- catalog
                , lastTablePart name == domainTableName domain
                , name `notElem` seen
                ]
            names = map fst definitions
            nested =
                [ child
                | (_, table) <- definitions
                , (_, column) <- tableCols table
                , child <- typeDomains (columnType column)
                ]
         in collect (seen ++ names) (remaining ++ nested)
    -- \| 保留嵌套领域的每层依赖
    typeDomains (CDomain name base) = name : typeDomains base
    typeDomains _ = []

-- | 收集来源树与连接条件依赖
sourceTables :: FromClause -> [String]
sourceTables source = case source of
    FromTable _ name -> [name]
    FromSubquery _ statement -> queryTables statement
    FromJoin _ left right condition -> sourceTables left ++ sourceTables right ++ expressionTables condition
    FromUnit -> []

-- | 收集表达式内嵌套子查询依赖
expressionTables :: Expr -> [String]
expressionTables expression = case expression of
    ScalarSub query -> queryTables (subqueryStatement query)
    ExistsSub query _ -> queryTables (subqueryStatement query)
    InSub value query _ -> expressionTables value ++ queryTables (subqueryStatement query)
    QuantCmp _ value query _ -> expressionTables value ++ queryTables (subqueryStatement query)
    _ -> concatMap expressionTables (children expression)

-- | 列出当前表达式的直接子节点
children :: Expr -> [Expr]
children expression = case expression of
    FunctionCall _ arguments -> arguments
    BoundFunction _ _ arguments -> arguments
    Construct _ arguments -> arguments
    BoundConstruct _ arguments -> arguments
    InList value arguments _ -> value : arguments
    Add a b -> [a, b]
    Sub a b -> [a, b]
    Mul a b -> [a, b]
    Div a b -> [a, b]
    Gt a b -> [a, b]
    Lt a b -> [a, b]
    Eq a b -> [a, b]
    GtE a b -> [a, b]
    LtE a b -> [a, b]
    NotEq a b -> [a, b]
    And a b -> [a, b]
    Or a b -> [a, b]
    Neg a -> [a]
    IsNull a -> [a]
    IsNotNull a -> [a]
    CountOf a -> [a]
    SumOf a -> [a]
    AvgOf a -> [a]
    MinOf a -> [a]
    MaxOf a -> [a]
    _ -> []
