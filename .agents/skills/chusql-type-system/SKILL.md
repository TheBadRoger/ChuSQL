---
name: chusql-type-system
description: 修改 ChuSQL SQL 类型、值表示、赋值转换、类型推导、函数或表达式语义时使用；提供类型模块职责与一致性约束。仅调整界面样式或无关存储性能时不加载。
---

# ChuSQL 类型系统

## 适用范围

负责类型语义；落盘顺序交给 chusql-storage-integrity，传输契约交给 chusql-boundary-contracts。通用语言与测试规范使用已有 Skills，不重复加载。下列路径相对仓库根目录，只读本次相关模块。

## 模块与调用路径

- `chusql-core/model/src/ChuSQL/Core/Model.hs`：`ColumnType`、列约束、`Value`；集中提供 `assignable`、`comparableTypes`、`valueFits`、`coerceValue`、相等与哈希语义。
- `chusql-core/model/src/ChuSQL/Core/Runtime.hs`：`TypeExpr` / `TypeId`、`Kind`、独立 `PhysicalType`、`RuntimeValue`、注册表和版本化编码；Engine 的 `Runtime/Types.hs` 只是重新导出。
- `chusql-core/engine/src/ChuSQL/Core/Engine/Syntax/Parser.hs` 与 `Syntax/AST.hs` 解析声明和构造表达式；`Semantic.hs` 根据 schema 解析 DOMAIN、推导表达式并绑定身份。
- `chusql-core/engine/src/ChuSQL/Core/Engine/Runtime/Functions.hs` 定义固定 FunctionId 与签名，现有函数 length / not；`Runtime/SQL.hs` 连接 SQL 标量和运行时值。
- `chusql-core/engine/src/ChuSQL/Core/Engine/Runtime/Query.hs` 的 BoundQuery 保存 schema、输出类型和优化后的 RelOp，执行后按位置校验。

  `Runtime/Binding.hs` 另提供类型化表达式与基础计划，尚未替代完整 SQL 关系代数。
- `chusql-core/model/src/ChuSQL/Core/Protocol.hs` 共享值的 JSON 转换；Rust 的 `chusql-core/storage/src/protocol.rs` 使用 `HashMap<String, serde_json::Value>` 保存行。

## 必须保持的不变量

### 类型身份与值

- 构造器编号固定：Int=1、Double=2、Bool=3、String=4、List=5、Maybe=6、Tuple=7。使用 resolveType 检查参数 / kind，不把显示名当身份，不再维护第二份注册表。
- CRuntime / VRuntime 身份必须匹配；递归检查列表元素、Maybe 内容与元组字段数，Double 必须有限。保留逻辑类型与物理表示的区别。
- CEnum / VEnum 保留库内类型名、完整标签集合与当前标签；EnumType 不占用固定运行时构造器编号。声明顺序决定比较与排序，类型名和标签集合决定兼容性，不能退化为普通文本或混同不同枚举。
- SQL VNull 与 RMaybe Nothing 不同；CNull 是查询推导类型，不能声明为存储列。保留三值逻辑与函数 NULL 传播。

### 转换与数值语义

- 赋值兼容和比较兼容分别使用 Model 入口。coerceValue 负责值收敛；整值浮点转整数、字符串长度、日期格式、DECIMAL 舍入沿现有规则处理，不在 Parser / Eval / Web 重复实现转换。
- 整数与可规范化整值浮点保持相等 / 哈希一致。内建运行时类型的 Eq / Ord / Hash 由 TypeDescriptor 按参数递归派生，排序和哈希按结构实现；索引键编码仍独立，不能因已有查询能力放开索引或唯一键声明。

### 领域与兼容表示

- DOMAIN 使用 domainTableName 与目录定义解析基础类型、依赖及循环；不能拿解析器占位基础类型直接执行。系统内建类型目录也不是通过增加行即可扩展的插件注册表。
- 命名 ENUM 沿用领域目录及依赖守卫；EnumLiteral 在字符串常量的比较、IN / ANY 上下文绑定，枚举列与普通文本列不能自动转换。标签参数由 validEnumLabels 统一校验：非空集合、无重复、每项至多 63 个 UTF-8 字节且无 NUL。引导仅保留原生 Bool，不预装 BoolText / YesNo / OnOff。
- Date / Timestamp / Blob 在 QueryType 保留 CompatibilityType；ColumnType 与 RelOp 仍有真实消费者，不能作为无用兼容层删除。

### 编码与错误传播

- 类型 / 值编码版本为 1；身份、类型名、封装或值结构变化可能影响 catalog、WAL、FFI、TCP 和 Web，组合相关 Skills 核对消费者。
- 枚举使用版本 1 的 __chusql_enum_v1 编码，存储保留类型名、标签集合和值，TCP / Web / CLI 结果输出字符串标签。Rust heap::enum_rank 统一为枚举索引计算声明序号；更新编码同时验证索引与实际动态库。
- Rust 当前保存通用 JSON，只做局部校验，包括隐藏列、DOMAIN 依赖和 DECIMAL 规范化。它没有 Haskell validateValue 的完整等价检查，不要依赖原始存储请求完成所有 SQL 类型校验。
- 错误沿 Either String 返回；`chusql-core/engine/src/ChuSQL/Core/Engine/Error.hs` 用消息规则映射类别 / 协议码。修改错误文字须检查分类，不用默认值或 NULL 掩盖错误。

## 实现取舍与边界

复用 Model / Runtime 的语义入口，在现有类型桥上做最小修改。尚无完整 capability / operator / cast 注册机制、通用显式 CAST、用户 ADT 或 Extension；DOMAIN 的继承约束、默认值和变更校验集中在 Engine/Domain.hs；ALTER DOMAIN 通过一致快照和提交点复查发布。不要将规划当现有契约。
DECIMAL 是有限浮点表示，采用 ties-to-even 舍入与舍入后溢出检查，并非精确十进制存储。内建运行时复合类型已提供 SQL Eq / Ord / Hash 能力，尚无用户注册的运算实现或复合索引键编码。

## 验证入口

- 语义改动验证解析 → schema 绑定 → 执行，关注 NULL、边界值、明确错误与输出位置；说明修正只需核对源码。
- 复用 `chusql-core/engine/test/RuntimeSpec.hs`、`chusql-core/engine/test/Spec.hs`，按需覆盖固定编码、错误身份和嵌套值。
- 持久化类型变更验证 Rust 写入 / 重载与实际协议消费者；Web 值展示 / SQL 生成参考 `chusql-web/test/runtime-values.mjs`。
- 测试入口见 `gate-check/run-step.ps1`，不因加载 Skill 自动执行全量门禁或追求覆盖率。

## 相关资料

按需读 `docs/types.md`，区分当前类型用法与未来约束设计。以上源码模块是契约核对入口。
