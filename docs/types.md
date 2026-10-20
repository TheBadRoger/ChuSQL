# 数据类型

[← 返回 README](../README.md)

本页说明常用类型、自定义类型和容量限制。声明与修改语法见 [SQL 语法](syntax.md)。

## 预定义类型的含义

类型名不区分大小写：

| 类型 | 用途 |
| --- | --- |
| `INT` / `INTEGER` / `INT4`、`BIGINT` / `INT8`、`SMALLINT` / `INT2` | 整数，共用整数表示 |
| `FLOAT` / `REAL` / `FLOAT4`、`DOUBLE` / `FLOAT8` | 浮点数 |
| `DECIMAL(p,s)` / `NUMERIC(p,s)` | 按小数位舍入的浮点数，默认 `(10,0)`；不是精确十进制存储 |
| `TEXT` / `STR`、`VARCHAR(n)`、`CHAR(n)` | 文本；CHAR 默认长度 1，保留原字符串长度 |
| `BOOL` / `BOOLEAN` | TRUE、FALSE |
| `DATE`、`TIMESTAMP` / `DATETIME` | 日期及不带时区的时间戳 |
| `TIME`、`TIMETZ` | 日内时间，后者要求时区 |
| `TIMESTAMPTZ` / `RFC3339` | 带时区的 RFC 3339 时间戳 |
| `INTERVAL` | ISO 8601 时长，分别保存日历月和固定时长 |
| `UUID` | 标准 UUID 标识符 |
| `JSON` / `JSONB` | 校验并规范化 JSON；两者使用同一表示 |
| `INET`、`CIDR`、`MACADDR` | IP 地址、网络前缀和 MAC 地址 |
| `BLOB` / `BYTEA` / `BINARY` / `VARBINARY` | 沿用 BLOB 的内容表示，别名不接受长度参数 |

`DECIMAL(p,s)` 要求 `1 <= p <= 308`、`0 <= s <= p`，半值舍入取偶数，超出精度时报错。

## 时间与时区

`TIMESTAMPTZ` 接受含 `Z` 或已知偏移的 RFC 3339 输入，写入时转成 UTC，并固定输出六位小数秒。`TIME` 接受 `HH:MM:SS` 及可选小数秒；`TIMETZ` 还要求时区，按 UTC 日内时间保存。也可声明 `TIMESTAMP WITH TIME ZONE`、`TIME WITH TIME ZONE` 和对应的 `WITHOUT TIME ZONE` 写法。

新时间类型校验真实日期和时间范围，最多接受六位小数秒，超出精度时拒绝，不截断。当前不接受闰秒及表示未知偏移的 `-00:00`；`TIMESTAMPTZ` 的 UTC 年份限制为四位。原有 `TIMESTAMP` 的表示保持不变。

```sql
CREATE TABLE events (id UUID, occurred_at TIMESTAMPTZ, payload JSONB);
INSERT INTO events (id, occurred_at, payload) VALUES (
  '550E8400-E29B-41D4-A716-446655440000',
  '2026-01-01T00:00:00.25+08:00',
  '{"source":"cli","version":1}'
);
SELECT occurred_at FROM events;
```

查询返回 `2025-12-31T16:00:00.250000Z`。文本字面量在赋值、比较及 IN 条件中按目标类型校验；也可使用 `UUID '...'`、`TIMESTAMPTZ '...'` 等类型化字面量。

## 标识符、JSON 与网络类型

`UUID` 接受标准的 `8-4-4-4-12` 十六进制形状，统一为小写。`MACADDR` 接受六组十六进制字节，输入可用冒号或横线分隔，保存时统一为小写冒号格式。

`JSON` 与 `JSONB` 都会解析输入、排序对象键并规范数值，保留数组顺序；重复键保留第一个值。两者共用规范 JSON 文本，未提供独立二进制 JSON 格式或路径运算。JSON 文本 `null` 与 SQL NULL 不同。

`INET` 接受 IPv4、IPv6 及尾部嵌入 IPv4 的 IPv6。IPv6 保存为完整的小写八组地址；IPv4 拒绝有歧义的前导零。`CIDR` 要求地址带合法前缀长度，并拒绝非零主机位；不进行 DNS 查询，也不接受端口或接口区域标记。

这些类型沿用文本值传输，JSON 查询结果也是规范 JSON 字符串，由应用按需解析。相等、分组和去重依据规范值；时间、UUID 和 MACADDR 支持顺序比较，JSON、INET、CIDR 和 INTERVAL 暂不支持排序、大小比较及 MIN/MAX。

## 时长与业务约束

`INTERVAL` 接受 ISO 8601 的年月日、独立周数及小时、分钟、秒写法，可带整体正负号。秒最多六位小数，每个整数字段最多十八位；周数不能与其他字段混用。年份换算成月份，时钟部分换算成天与微秒，月份不折算为固定天数。

例如 `P1Y2M` 与 `P14M` 表示同一日历时长，`PT25H` 与 `P1DT1H` 表示同一固定时长。当前未提供日期加减或时长算术。邮箱、手机号、网址、金额精度等业务规则由表约束或 DOMAIN 定义，预设类型不会代替业务校验。

## 行大小与索引键限制

整行编码须放入单页，默认页大小为 4096 字节，包含存储开销。默认索引键上限为 128 字节，旧配置可能只有 64 字节；文本键按 UTF-8 字节计算。超限写入明确报错。

## 命名枚举 ENUM

枚举声明一组允许的标签，比较和排序按声明顺序，支持索引及唯一约束。标签区分大小写，每项最多 63 个 UTF-8 字节，集合非空且标签唯一。

不同命名枚举与普通文本分别处理。用法见[枚举语法](syntax.md#命名枚举-enum)。

## 自定义类型 DOMAIN

DOMAIN 为基础类型命名，可附加默认值、非空和 CHECK 约束，供多个列复用。定义按数据库保存，可基于已有 DOMAIN 嵌套；删除前先解除引用。

## DOMAIN 约束与变更

CHECK 用 `VALUE` 表示当前值，TRUE 或 NULL 通过，FALSE 拒绝；非空要求通过 NOT NULL 声明。表达式可使用标量运算和函数。

嵌套 DOMAIN 继承全部约束。默认值按“列、最近领域、基础领域”选择，仅在省略列时使用；已有行保留原值。

修改定义前会校验默认值及已有数据。并发变化可能返回 `serialization_failure`，由调用方重试。DOMAIN 管理在显式事务之外执行，语法见[自定义类型](syntax.md#自定义类型-domain)。

## 运行时类型与函数

列表、可选值和元组的写法如下：

| 类型 | 声明 | 构造示例 |
| --- | --- | --- |
| 列表 | `[Int]` / `List Int` | `LIST<Int>(1, 2)` |
| 可选值 | `Maybe String` | `MAYBE<String>()`、`MAYBE<String>('ready')` |
| 元组 | `(Int, String)` | `TUPLE<Int, String>(3, 'x')` |

内建函数 `length` 返回字符串长度，`not` 对布尔值取反。SQL NULL 与 Maybe 的无值状态 Nothing 分开处理。

### 比较、排序与分组

相同完整类型的复合值可用于比较、排序、JOIN、分组、去重和 MIN / MAX。列表逐项比较，元组按字段比较，Nothing 排在 Just 前；SUM / AVG 只接受数值标量。

运行时复合类型暂不支持索引和唯一键。CLI 可以显示这些值，修改使用 SQL。

## 系统类型目录

引导程序将 38 个预定义类型名及别名登记到受保护的 `__system_types` 系统目录中。再次引导会保留已有编号和定义，只补齐缺失项；同名定义冲突明确报错。该目录用于记录内建支持，不会通过插入一行目录数据自动创建新基础类型。

新标量复用现有文本编码；旧引擎可能无法识别新的类型名。使用新类型后回退版本，应恢复升级前的完整备份。含复合类型的数据仍需使用支持其编码的客户端。
