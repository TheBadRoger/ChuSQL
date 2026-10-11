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
列引用按当前输入 schema 的槽位绑定，列名保留用于诊断和位置校验。独立调用 `evaluateExpression`
必须按绑定时的 schema 顺序提供行；位置不符或类型不符明确报错。`executePlan` 在扫描边界
校验完整行并按 schema 排序，投影后的表达式重新绑定到子计划输出槽位。

SQL SELECT 通过 `Runtime.Query.bindQuery` 生成不可直接构造的 BoundQuery，再由 `executeQueryM` 执行。
绑定计划保存输出 schema 与类型身份；现有 RelOp 作为关系执行和优化的兼容桥，输出按位置校验，保留重复别名。
类型化函数和构造器节点也进入 INSERT、UPDATE、CHECK、聚合参数和子查询。
日期、时间戳、Blob 等阶段 A 外的类型保留明确的 CompatibilityType。
`queryTypes` 用 `DomainType 名称 基础类型` 保留每一层 DOMAIN 引用，包括嵌套领域和兼容类型。
直接列引用及派生表别名保留该身份；函数结果使用函数声明的返回类型，不继承参数的 DOMAIN。
领域名称来自当前库 schema 的解析结果；定义版本与约束绑定仍待完整 bound IR 实现。
SQL 计划在优化完成后由 `Runtime.Relation` 将投影、过滤、计算、聚合参数和连接条件中的列引用
绑定为 `ColumnSlot`。扫描、点查和范围查询在存储边界按算子布局排列返回行；相关子查询
按自己的局部布局和外层行布局绑定槽位。局部引用使用 ColumnSlot，外层引用使用 OuterColumnSlot，
保存局部布局宽度与外层相对位置；重新绑定时外层引用只在外层布局中查找，防止同名局部列捕获。
执行槽位不接受语义层再次推导，语义输入必须使用未绑定列。哈希/索引连接仍使用列名识别等值键。
投影单向转换为已有 Compute 算子，使用绑定列槽位取值；保留输出顺序、重复列及空投影的行数。
排序转换为 BoundSort，分组转换为 BoundAggregate；键使用绑定槽位，列名保留为输出标签和诊断。
排序每行计算一次键并保留同键行序，分组保留首次出现顺序及空输入全局聚合行为。
该步骤仍复用 Expr/RelOp 和扁平化外层行；多级词法作用域/来源编号及 schema 版本尚未引入。
绑定后的计划用于执行；结构改写必须在绑定前完成，或重新按新布局绑定。
执行前重新取得存储 schema，比较查询依赖表的完整列定义；依赖收集覆盖来源树、连接条件、
投影和过滤表达式里的嵌套查询，并递归追踪来源列使用的 DOMAIN 定义。缺表、列定义变化或 schema
读取失败明确返回错误，行数据、统计及无关表、无关 DOMAIN 的结构变化不使绑定失效。
Table 与 TableInfo 保留 catalog 的库编号和对象编号；执行前比较身份，可识别同名同结构的对象替换。
内存 Server 目录和存储快照同样保留身份。TableInfo 输出十进制字符串编号，读取兼容数字编号，
校验无符号 64 位范围；旧响应缺少编号或原始目录对象编号为零时，以列定义校验作为兼容路径。
身份与列定义校验不能替代服务端读写门或未来的原子 schema 版本校验。
旧 ColumnType 和运算符实现尚未整体移除；统一 capability、cast 和 rewrite 将在后续阶段收拢。

测试覆盖稳定编码、kind / 参数校验、共享协议、SQL 调用和复合列、实际存储重载、HTTP/TCP 与客户端显示。
