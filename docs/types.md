# 类型、函数与类型目录

[← 返回 README](../README.md)

**本页导航：** [自定义类型 DOMAIN](#自定义类型-domain) · [预定义类型的含义](#预定义类型的含义) · [系统类型目录字段](#系统类型目录字段) · [运行时类型与函数](#运行时类型与函数) · [DOMAIN 约束设计（尚未实现）](#domain-约束设计尚未实现)

ChuSQL 支持基础类型、运行时复合类型和按数据库保存的 DOMAIN。DOMAIN 为基础类型命名，便于多个表列复用同一定义。

## 自定义类型 DOMAIN

DOMAIN 按数据库保存，保留名称并复用基础类型的校验与运算语义。可引用内置类型或当前库已有 DOMAIN；被列或其他 DOMAIN 引用时不能删除。

创建、使用、查询和删除的语法及限制见 [SQL 语法](syntax.md#自定义类型-domain)。

## 预定义类型的含义

安装期间，引导程序将下列 18 个名称及别名登记到系统类型目录。大小写不影响识别。

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

上述名称表达类型分类，不表示已经完整复刻 PostgreSQL 的存储和数值行为。当前整数类型共用整数值表示；浮点类型共用浮点值表示；`CHAR(n)` 检查字符长度，不自动补空格。`DECIMAL(p,s)` 要求 `1 <= p <= 308`、`0 <= s <= p`，使用有限浮点表示并按小数位舍入，恰好半值时取偶数；舍入后的绝对值达到 `10^(p-s)` 即报精度溢出。它不能视为精确十进制存储。

## 系统类型目录字段

全局预定义类型目录为 `system.__system_types`，每个名称或别名单独占一条记录。

| 字段 | 含义 |
| --- | --- |
| `id` | 类型记录的持久化编号 |
| `name` | 小写类型名称，例如 `integer` 或 `varchar` |
| `base_type` | 不带显式参数时的规范基础表示；例如 `integer` 对应 `int`，`varchar` 对应 `str` |
| `parameterized` | 该名称是否接受长度、精度等类型参数；不表示具体列已经指定了参数 |

例如，目录中的 `varchar` 记录表示支持 `VARCHAR(n)`；实际列声明为 `VARCHAR(32)` 时，长度 32 保存在列结构中，而不是修改全局目录记录。

该目录是受保护的系统元数据，普通 SQL 不直接展示或修改它。类型执行语义由引擎实现，不能通过手工增加目录行获得新的基础类型。安装登记不会创建任何预定义 DOMAIN；DOMAIN 定义另存于各工作库的 catalog。

首次安装完成管理员初始化后登记类型。重复引导保留一致的记录并补齐缺失项；同名定义冲突会报错，停止安装启动流程，不覆盖现有定义。

## 运行时类型与函数

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
| 列表 | `[a]` / `List a`；`LIST<a>(...)` 按完整类型校验，不隐式转换元素类型 |
| 可选值 | `Maybe a`；`MAYBE<a>()` 为 Nothing，`MAYBE<a>(value)` 为 Just |
| 元组 | `(a, b, ...)`；`TUPLE<a, b>(...)` 校验各字段类型 |
| `length` | `String -> Int` |
| `not` | `Bool -> Bool` |

空列表和 Nothing 也有确定类型。SQL 可空列的 `NULL` 与 Maybe 内部的 Nothing 不同，不能互相替代。内建函数名称不区分大小写，NULL 参数传播为 NULL，非空参数仍校验类型。

> [!NOTE]
> 复合类型尚无 SQL Eq / Ord / Hash 能力，不能自动用于比较、排序、分组、极值或索引。Web 可以显示复合值及构造器预览，修改须通过 SQL 控制台。

### 类型身份与持久化

内建构造器编号固定为 `Int=1`、`Double=2`、`Bool=3`、`String=4`、`List=5`、`Maybe=6`、`Tuple=7`；函数编号固定为 `length=1`、`not=2`。完整 `TypeId` 与逻辑类型、物理表示分开，schema 和值使用版本化编码。未知版本、类型或载荷不匹配、非有限浮点数明确报错。

> [!IMPORTANT]
> 旧标量编码可继续读取；包含运行时复合类型的数据不能交给不支持该编码的旧客户端。

## DOMAIN 约束设计（尚未实现）

当前 DOMAIN 只提供类型命名，不支持自身 `CHECK`、`NOT NULL`、`DEFAULT`、`ALTER DOMAIN` 或 `CASCADE`。后续约束绑定依赖统一 [bound IR](architecture.md#bound-ir)、对象依赖管理和原子结构事务；不能仅放开语法而绕过写入校验、旧数据验证及 WAL 恢复。
