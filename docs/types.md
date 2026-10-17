# 类型、函数与类型目录

[← 返回 README](../README.md)

本页帮助你选择列类型、理解类型限制、枚举和 DOMAIN 约束。

ChuSQL 支持基础类型、运行时复合类型以及按数据库保存的 ENUM 和 DOMAIN。DOMAIN 为基础类型命名，便于多个表列复用同一定义。

本页内容：

- [命名枚举 ENUM](#命名枚举-enum)
- [自定义类型 DOMAIN](#自定义类型-domain)
- [预定义类型的含义](#预定义类型的含义)
- [系统类型目录字段](#系统类型目录字段)
- [运行时类型与函数](#运行时类型与函数)
- [DOMAIN 约束与变更](#domain-约束与变更)

## 命名枚举 ENUM

`CREATE TYPE blog_role AS ENUM ('MEMBER', 'AUTHOR', 'MASTER')` 声明一组允许的文本标签。列使用 `blog_role` 后，默认值、插入、更新和已有列转换都必须满足标签集合；NULL 另外受列的可空约束控制。

标签区分大小写，每个标签最多 63 个 UTF-8 字节，集合至少包含一个标签，且各标签唯一。值以字符串返回，比较与排序按声明顺序，支持分组、唯一约束和索引。比较和赋值要求命名枚举类型一致；枚举列与普通文本列分别处理。ENUM 和 DOMAIN 共用库内类型名称空间，已有引用会阻止删除；使用方法见[命名枚举语法](syntax.md#命名枚举-enum)。

原生 BOOL / BOOLEAN 使用 TRUE、FALSE 和 SQL NULL，引导程序不额外安装布尔枚举。枚举内部保存类型身份、标签集合和值，因此较大的集合会增加单行开销，仍须满足[页内行大小限制](#行大小与索引键限制)。

## 自定义类型 DOMAIN

DOMAIN 按数据库保存，保留名称并复用基础类型的校验与运算语义。可引用内置类型或当前库已有 DOMAIN；删除前须解除列和其他 DOMAIN 的引用。

创建、使用、查询和删除的语法及限制见 [SQL 语法](syntax.md#自定义类型-domain)。

## 预定义类型的含义

安装期间，引导程序将下列 18 个名称及别名登记到系统类型目录。类型名称不区分大小写。

| SQL 名称 | 含义与默认行为 | 目录中的默认 `base_type` | 接受参数 |
| --- | --- | --- | --- |
| INT、INTEGER | 整数，两种名称等价 | `int` | 否 |
| BIGINT | 大整数类型名称 | `bigint` | 否 |
| SMALLINT | 小整数类型名称 | `smallint` | 否 |
| VARCHAR | 可变长文本；省略长度时不设声明长度上限 | `str` | 是：`VARCHAR(n)` |
| CHAR | 有长度限制的文本，默认长度 1 | `char(1)` | 是：`CHAR(n)` |
| TEXT、STR | 文本，两种名称等价 | `str` | 否 |
| BOOLEAN、BOOL | 布尔值 `TRUE` / `FALSE` | `bool` | 否 |
| FLOAT、REAL | 浮点数，两种名称等价 | `float` | 否 |
| DOUBLE | 双精度类型名称 | `double` | 否 |
| DECIMAL、NUMERIC | 指定精度和小数位的数值，默认 `(10, 0)` | `decimal(10,0)` | 是：`(p)` 或 `(p,s)` |
| DATE | 日期，使用 `YYYY-MM-DD` 格式 | `date` | 否 |
| TIMESTAMP | 日期与时间 | `timestamp` | 否 |
| BLOB | 二进制数据类型名称 | `blob` | 否 |

### 数值和字符行为

整数类型共用整数值表示；浮点类型共用浮点值表示；`CHAR(n)` 检查字符长度，保留原字符串长度。

`DECIMAL(p,s)` 要求 `1 <= p <= 308`、`0 <= s <= p`，使用有限浮点表示并按小数位舍入，恰好半值时取偶数；舍入后的绝对值达到 `10^(p-s)` 即报精度溢出。DECIMAL 使用浮点表示，不是精确十进制存储。

### 行大小与索引键限制

声明长度之外还有物理容量限制：整行的 JSON 编码须放入一个存储页（默认 4096 字节，包含页头和槽位开销）。

已建立索引的键在默认配置下最多 128 字节；有效上限为 `min(128, floor((page_size - 11) / (order - 1)) - 12)`，密集旧配置可能仍只支持 64 字节。字符串长度按 UTF-8 字节计数。非索引文本按声明长度和整行容量校验。超页行或超长索引键在写 WAL 前拒绝；为已有列创建索引时也会先检查现有值。

## 系统类型目录字段

全局预定义类型目录为 `system.__system_types`，每个名称或别名单独占一条记录。

| 字段 | 含义 |
| --- | --- |
| `id` | 类型记录的持久化编号 |
| `name` | 小写类型名称，例如 `integer` 或 `varchar` |
| `base_type` | 不带显式参数时的规范基础表示；例如 `integer` 对应 `int`，`varchar` 对应 `str` |
| `parameterized` | 该名称是否接受长度、精度等类型参数；具体列参数保存在列结构中 |

例如，目录中的 `varchar` 记录表示支持 `VARCHAR(n)`；实际列声明为 `VARCHAR(32)` 时，长度 32 保存在列结构中，全局目录记录保持原定义。

该目录由引导程序维护，类型语义由引擎实现。增加基础类型须扩展引擎实现；DOMAIN 由用户在工作库中创建，定义保存在本库 catalog。

首次安装完成管理员初始化后登记类型。重复引导保留一致的记录并补齐缺失项；同名定义冲突时停止安装启动流程并报错，保留现有定义。

## 运行时类型与函数

运行时复合类型包括列表、可选值和元组。以下示例展示声明、构造、读取和更新：

```sql
CREATE TABLE typed_values (
    id Int,
    name String,
    numbers [Int],
    label Maybe String,
    pair (Int, String)
);

INSERT INTO typed_values (id, name, numbers, label, pair)
VALUES (1, 'abc', LIST<Int>(1, 2), MAYBE<String>(), TUPLE<Int, String>(3, 'x'));

SELECT length(name), numbers, label, pair FROM typed_values;

UPDATE typed_values SET numbers = LIST<Int>(9), label = MAYBE<String>('ready') WHERE id = 1;

SELECT LIST<Maybe Int>(MAYBE<Int>(), MAYBE<Int>(42));
SELECT not(TRUE), length('你好');
```

| 类型或函数 | 写法与语义 |
| --- | --- |
| 标量 | `Int`、`Double`、`Bool`、`String` |
| 列表 | `[a]` / `List a`；`LIST<a>(...)` 按完整类型校验，元素类型须匹配声明 |
| 可选值 | `Maybe a`；`MAYBE<a>()` 表示无值（Nothing），`MAYBE<a>(value)` 表示有值（Just） |
| 元组 | `(a, b, ...)`；`TUPLE<a, b>(...)` 校验各字段类型 |
| `length` | `String -> Int` |
| `not` | `Bool -> Bool` |

空列表和 Nothing 也有确定类型。SQL `NULL` 表示列空值，Maybe 的 Nothing 表示该可选类型内的无值状态，两者分别处理。内建函数名称不区分大小写，NULL 参数传播为 NULL，非空参数仍校验类型。

### 比较、排序与分组

类型能力说明一种类型支持哪些运算。相等比较、排序和哈希分别记为 Eq、Ord 和 Hash；哈希用于 JOIN、GROUP BY 和 DISTINCT 的值分组。内建 Int、Double、Bool、String 提供这些能力，List、Maybe 和 Tuple 根据全部参数类型递归获得能力，DOMAIN 沿用基础类型的能力；ENUM 按自身身份与声明顺序实现比较和排序，哈希保留类型身份。

同一完整类型的复合值可以使用比较运算、IN、ANY / ALL、JOIN、ORDER BY、GROUP BY、DISTINCT 和 MIN / MAX。比较要求完整类型身份一致，例如 `[Int]` 与 `[Double]` 需分别处理，空列表同样按声明类型校验。SUM / AVG 仍只接受数值标量。

结构比较使用以下规则：

| 类型 | 升序规则 |
| --- | --- |
| List | 从首个不同元素比较大小；相同前缀下，较短列表在前 |
| Maybe | Nothing 在 Just 前；两个 Just 比较内部值 |
| Tuple | 按字段从左到右比较，取首个不同字段的结果 |
| SQL NULL | 升序在所有非空值之前；参与 SQL 比较时结果仍为 NULL |

以下查询都返回 TRUE：

```sql
SELECT LIST<Int>(2) < LIST<Int>(10);
SELECT LIST<Int>(2) < LIST<Int>(2, 9);
SELECT MAYBE<Int>() = MAYBE<Int>();
```

SQL NULL 与 Maybe 的 Nothing 不同，`MAYBE<Int>() = NULL` 返回 NULL。分组和去重将 SQL NULL 合并为一组，将 Nothing 作为另一个非空值处理。同键排序保留输入行序。

复合值按结构哈希，类型身份和各字段共同参与。有限浮点数的正负零相等，嵌套在列表、可选值或元组中也得到相同哈希。NaN、无穷大和载荷类型失配在读取或表达式边界明确拒绝。

复合值的查询能力不包含索引键编码。运行时复合类型的索引、UNIQUE 和 PRIMARY KEY 支持仍待实现；索引连接遇到复合键时使用原有扫描或哈希路径。Web 可以显示复合值及构造器预览，修改须通过 SQL 控制台。

### 类型身份与持久化

内建构造器编号固定为 `Int=1`、`Double=2`、`Bool=3`、`String=4`、`List=5`、`Maybe=6`、`Tuple=7`；函数编号固定为 `length=1`、`not=2`。完整类型身份 `TypeId` 与逻辑类型、物理表示分开；表结构（schema）和值使用版本化编码。未知版本、类型或载荷不匹配、非有限浮点数明确报错。

> [!IMPORTANT]
> 旧标量编码可继续读取；包含运行时复合类型的数据不能交给不支持该编码的旧客户端。

## DOMAIN 约束与变更

领域约束的继承、默认值和修改规则如下。

### 检查条件

DOMAIN 支持字面量 `DEFAULT`、`NOT NULL` 和多条 `CHECK`；语法见 [SQL 语法](syntax.md#自定义类型-domain)。

`CHECK` 使用单值变量 `VALUE`（不区分大小写），表达式范围为标量运算和函数。表列、聚合和子查询超出该范围。结果为 TRUE 或 SQL NULL 时通过，为 FALSE 时拒绝；非空由独立的 `NOT NULL` 保证。

### 继承与默认值

嵌套领域继承全部 CHECK 和非空限制。列自己的 DEFAULT 优先，其次取最近领域的 DEFAULT，再向基础领域查找；显式 NULL 按空值处理。领域的非空限制优先于列的可空声明。

绑定时按领域定义计算继承属性，默认值变化会影响后续省略该列的 INSERT；已有行保留原值。建表、加列、改列类型、INSERT 和 UPDATE 都使用这些规则；默认值在声明时也须满足完整领域约束。

### 修改定义与依赖失效

`ALTER DOMAIN` 支持 SET / DROP DEFAULT、SET / DROP NOT NULL、ADD [CONSTRAINT 名称] CHECK 和 DROP CONSTRAINT。未命名 CHECK 分配 `domain_check_1` 起的名称，跳过已有名称。变更发布前验证传递依赖领域、列默认值和已有行；失败保留原定义。

依赖定义的绑定查询与写计划在元数据变化后失效；显式事务提交也会复查修改表及其传递领域依赖，暂存写入也须满足提交时的约束。

### 持久化与并发边界

持久存储在库读门内取得同一提交点的完整快照，引擎完成校验后，存储在库写门内复查库身份和提交点，再通过现有 ReplaceSchema WAL 原子发布领域目录。并发写入或库替换导致 `serialization_failure`，调用方可重新执行；重试由调用方发起。

元数据沿用目录列编码，基础列保存类型、可空性和默认值，具名 CHECK 保存为独立目录列；已有只含基础类型的 DOMAIN 可继续读取。完整 SQL 类型校验由引擎负责，原始管理员存储接口仅执行存储层校验。

领域改名、基础类型修改、CASCADE、NOT VALID 和事务内领域 DDL 属于后续结构事务与对象依赖管理的扩展范围。
