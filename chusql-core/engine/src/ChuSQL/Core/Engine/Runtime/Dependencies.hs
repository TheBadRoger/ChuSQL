module ChuSQL.Core.Engine.Runtime.Dependencies (queryTables, domainDependencies) where

import ChuSQL.Core.Engine.Syntax.AST
import ChuSQL.Core.Model (Database, ColumnType (..), columnType, tableCols, domainTableName, lastTablePart)
import Data.List (nub)

-- 收集查询及嵌套表达式的表依赖，供绑定执行前校验目录。

-- | 收集全部查询来源表
queryTables :: Statement -> [String]
queryTables statement = nub (case statement of
    SelectDistinct child -> queryTables child
    Select items source condition _ _ _ -> sourceTables source
        ++ concatMap (expressionTables . snd) items ++ concatMap expressionTables condition
    _ -> [])

-- | 收集来源列依赖的领域定义
domainDependencies :: Database -> [String] -> [String]
domainDependencies catalog sources = collect []
    [domain | (name, table) <- catalog, name `elem` sources
        , (_, column) <- tableCols table, domain <- typeDomains (columnType column)]
  where
    -- | 递归展开定义并防止重复访问
    collect seen [] = seen
    collect seen (domain : remaining) =
        let definitions = [(name, table) | (name, table) <- catalog
                , lastTablePart name == domainTableName domain, name `notElem` seen]
            names = map fst definitions
            nested = [child | (_, table) <- definitions, (_, column) <- tableCols table
                , child <- typeDomains (columnType column)]
        in collect (seen ++ names) (remaining ++ nested)
    -- | 保留嵌套领域的每层依赖
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
