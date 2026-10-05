# 运行时类型与函数基础

阶段 A 提供固定身份的内建类型、函数注册表、schema 驱动的绑定和类型化查询入口。
完整 capability、运算符注册、转换、用户 ADT 与 Extension 留在后续阶段。

## SQL 使用

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

类型语法支持 `Int`、`Double`、`Bool`、`String`、`[a]` / `List a`、`Maybe a` 和 `(a, b, ...)`。
构造器中的类型参数是完整逻辑类型；空列表和 Nothing 也有确定类型。
`MAYBE<a>()` 表示 Nothing，`MAYBE<a>(value)` 表示 Just；不能用 SQL NULL 代替复合值内部的 Nothing。
SQL 可空列仍接受 NULL，和 Maybe 的 Nothing 保持区别。
构造器按完整类型校验，不隐式转换 List 的元素类型。

内建签名为 `length :: String -> Int` 和 `not :: Bool -> Bool`，名称不区分大小写。
SQL 参数为 NULL 时结果为 NULL；具体非空参数仍校验类型。
尚无运行时 Eq / Ord / Hash 字典，复合类型的比较、排序、分组、极值和索引不会自动取得能力。
Web 显示复合值及 SQL 构造器预览，复合字段通过 SQL 控制台修改。

## 身份与编码

`ChuSQL.Core.Runtime` 是共享模型中的权威定义；Engine 模块导出同一组身份。
内建构造器编号固定为 Int=1、Double=2、Bool=3、String=4、List=5、Maybe=6、Tuple=7。
TypeId 包含已校验的完整构造器应用，比较和恢复不依赖注册顺序或随机哈希。
FunctionId 固定为 length=1、not=2；内建元数据可直接重建注册表。

逻辑 TypeExpr 与 PhysicalType 分开；List、Maybe、Tuple 使用递归 sequence、optional、product 表示。
schema 保存版本化的 TypeId，协议值保存版本、类型身份和 payload；包括 String 在内的运行时值共用编码。
Rust 的通用行、目录和 WAL 路径保存该表示，读取时由共享模型验证版本、类型、元素和字段个数。
未知版本、身份溢出、类型不符、非有限浮点数及损坏 payload 明确返回错误。
内建类型不可删除，复合类型的依赖完整包含在 TypeExpr 中。
旧标量 schema 和协议继续读取；包含运行时类型的数据不能交给不支持该编码的旧客户端。

## 绑定与执行

`Runtime.Binding` 提供独立的 TypedExpression 和 TypedPlan，绑定只接受 catalog schema。
表达式和计划构造器隐藏，调用者必须经过 Binder；执行使用已解析的 TypeId / FunctionId，并校验存储行。
该 API 覆盖单行、表扫描、投影和过滤，可构造嵌套复合值及函数调用。

SQL SELECT 通过 `Runtime.Query.bindQuery` 生成不可直接构造的 BoundQuery，再由 `executeQueryM` 执行。
绑定计划保存输出 schema 与类型身份；现有 RelOp 作为关系执行和优化的兼容桥，输出按位置校验，保留重复别名。
类型化函数和构造器节点也进入 INSERT、UPDATE、CHECK、聚合参数和子查询。
日期、时间戳、Blob 等阶段 A 外的类型保留明确的 CompatibilityType。
旧 ColumnType 和运算符实现尚未整体移除；统一 capability、cast 和 rewrite 将在后续阶段收拢。

测试覆盖稳定编码、kind / 参数校验、共享协议、SQL 调用和复合列、实际存储重载、HTTP/TCP 与客户端显示。
