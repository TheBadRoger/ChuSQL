# 数据库范式分析与迁移

引擎模块 `ChuSQL.Core.Engine.Normalization` 提供只读范式分析。输入只有 schema 与用户声明，不读取样本数据，不访问或修改存储。

## 接口与示例

```haskell
analyzeNormalization
    :: [(String, Column)]
    -> Declaration
    -> Either String Analysis
```

调用示例：

```haskell
analyzeNormalization [("student", TInt), ("course", TInt), ("name", TStr)]
    (Declaration
        { declaredKeys = [["student", "course"]]
        , dependencies = [Dependency ["student"] ["name"]]
        , dependenciesComplete = True
        , atomicAttributes = Just True
        })
```

这个声明违反 2NF、3NF、BCNF：非主属性 `name` 依赖复合候选键的真子集 `student`。

报告的 `witnesses` 给出相关字段，`condition` 给出范式条件。建议分为 `(student, name)` 与 `(course, student)`，无损连接并保持声明依赖。

## 声明与输入校验

`declaredKeys` 中每个键作为决定全部字段的依赖加入公理推导；检查器验证其最小性，并枚举所有最小超键。空键允许表达只有一行的关系约束。

字段名称使用 schema 中的确切名称，不自行处理 SQL 标识符解析。以下输入返回明确错误：

- 未知字段或重复字段。
- 空依赖右部。
- 非最小候选键。
- 空 schema。

## 公理推导与范式条件

属性闭包以输入属性实现自反性，逐轮加入已满足左部的依赖右部，得到阿姆斯特朗增广和传递规则蕴含的闭包。检查器枚举字段子集及其闭包，检查完整的依赖投影。

| 范式 | 检查条件 |
| --- | --- |
| 1NF | 根据用户声明判断领域值原子性。 |
| 2NF | 检查所有候选键的部分依赖。 |
| 3NF | 检查非超键左部与非主属性右部。 |
| BCNF | 检查非超键左部。 |

理论定义可参考[数据库课程讲义](https://documents.uow.edu.au/~jrg/235/slides/03databasenormalization/03databasenormalization.html)。

## 信息完整性与判定

### 领域值原子性

`atomicAttributes` 是用户关于领域值原子性的声明：

| 声明值 | 1NF 判定 |
| --- | --- |
| `Just True` | 满足。 |
| `Just False` | 违反。 |
| `Nothing` | 信息不足。 |

schema 类型不证明业务意义上的原子性。不满足 1NF 时更高范式也判违反；原子性未知时更高范式信息不足。

### 依赖完整性

只有 `dependenciesComplete = True` 才对更高范式给出满足或违反结论。这表示用户承诺给出了完整业务依赖的一个覆盖，而非必须列出所有推导依赖。完整的空依赖集有效。

未承诺完整时，更高范式返回 `Insufficient`，不输出违反证据或分解建议。此时 `candidateKeys` 仅是相对已知声明求得的键，不能视为完整业务候选键清单。

分析始终以声明为条件，不能证明数据库实际数据满足声明。

## 分解建议

分解建议仅针对第一条 BCNF 违反依赖 `X -> Y`，输出两个子关系：

- `X ∪ Y`
- `R − (Y − X)`

该二元分解由依赖保证无损；依赖保持通过两个子关系的完整依赖投影之并是否蕴含原依赖集判定。

建议只是一轮分解，子关系可能仍需继续分析，不承诺一次达到 BCNF。依赖不保持时需跨关系检查原约束。

## 第一阶段限制

- 最多接受 12 列，超过返回错误，避免指数枚举无界增长。
- 仅提供 Haskell 只读 API。
- CLI / Web 入口和自动迁移尚未实现。

## 后续入口设计（尚未实现）

以下 TCP、CLI 和 Web 接口尚未实现，拟议命令与端点目前不可用。实施状态以 project-plan.txt 为准。

### 协议与鉴权

在 `ChuSQL.Interface.Protocol` 增加专用客户端请求 `normalization_analyze`，由 server 调用 engine API；不经最高权限专用的原始 `ReqStorage` 入口。请求包含 `api_version=1`、数据库与表标识、声明，以及可选的预期对象编号和 schema 指纹。server 在现有权限读门内以 catalog 解析现存对象，检查 CONNECT 与 SELECT；系统对象拒绝访问。事务内取该连接的 schema 快照，返回视图来源和对象编号，不能混入其他连接的快照。

声明字段固定为 `declared_keys`、`dependencies`（`lhs` / `rhs`）、`dependencies_complete`、`atomic_attributes`。完整性缺省 false，原子性缺省 null；不把 PRIMARY KEY、UNIQUE 或样本自动当作完整业务依赖。前端可以展示实际 schema 约束供用户选择，确认后才成为声明。候选键的 nullable 与 SQL UNIQUE 语义单独提示，不自动证明经典关系模型的键。

响应包含对象编号、schema 指纹、规范化声明、候选键、四项 findings 和一次分解建议。稳定标签使用 `1nf/2nf/3nf/bcnf`、`satisfied/violated/insufficient`；条件说明与机器字段分开。所有结论标明“以声明为条件”，无损连接标签不代表现有 SQL 数据或袋语义已经验证。

字段名称必须与返回 schema 的确切列名一致；对象名称使用现有标识符规则，数据库限定名必须与请求数据库一致。指纹只覆盖列名、类型和约束等语义结构，排除行数与直方图；定义确定性编码并同时绑定稳定对象编号，避免同名重建复用报告。

拒绝未知协议版本、未知字段、重复字段、空右部、非最小声明键、空 schema 和超过 12 列。拟定请求上限 64 KiB、依赖条数 256、键声明 256；预算超限返回明确错误，不截断后给出完整报告。分析在有界工作队列运行，取消或超时释放权限读门与会话资源；输出也受传输预算约束，超限返回错误。实施时用最坏 12 列声明确定默认时间预算。

### CLI 与 Web

CLI 拟议元命令为 `\normalize TABLE --declaration FILE --format text|json`，通过当前登录连接和工作库请求。声明文件为 UTF-8 JSON，错误写 stderr；成功分析退出码 0，即使结论违反或信息不足；协议、权限、输入与运行错误退出非零。批处理增加独立 `--normalize` 模式，与 `--execute` 互斥，不把元命令发给 SQL 解析器。

Web 拟议 `POST /api/normalization/analyze`，复用登录、CSRF 和 Cookie 串行规则。入口位于表结构页：展示 schema、编辑声明、提交分析、显示证据和分解字段列表。完整性与原子性不预先勾选；没有自动执行分解按钮。编辑页分析必须携带既有页面事务标识，不能自动开启另一事务。对象或 schema 改变返回冲突并要求重新分析。

### 入口验收

验收覆盖 engine 报告的协议往返、CLI 文本/JSON 同值、Web 与 TCP 越权、跨库同名、删表重建、事务快照、超限及取消。沿用现有 14 项算法回归，不复制算法到客户端。

## 数据迁移设计（尚未实现）

先实现只读入口，再在 [DOMAIN 约束](types.md#domain-约束设计尚未实现)、依赖目录和原子结构发布具备后实施迁移。

### 规划条件

迁移首版限定单业务表、二元分解、显式目标名，保留原表，不自动改名或删除。请求必须引用报告对象编号、schema 指纹及声明摘要；报告本身不是执行授权。要求来源 SELECT、数据库 CREATE；切换旧表需要来源所有权。每个目标取得新编号和执行者所有权，原 ACL 不自动复制。

经典 FD 与无损分解适用于关系集合，当前 SQL 保留重复行且有 NULL。首版只接受所有参与字段 NOT NULL、全行无重复且声明确实成立的数据；否则明确拒绝，不能通过 DISTINCT 悄悄丢行。候选键也必须验证唯一和非空，包括空键最多一行。FD 验证按引擎规范值相等规则分组，禁止使用文本序列化代替值比较。

目标数据为各字段投影的集合。验证每条声明 FD、全部候选键、目标约束及两目标连接与来源的完整多重集相等；无损性理论证明和实际重建检查均须通过。若投影依赖不能保持所有原依赖，首版拒绝迁移；后续有跨表约束执行器后再开放，不只弹提示继续。

### 结构依赖与发布

规划器枚举索引、默认值、CHECK、DOMAIN 引用、外键及其他已登记依赖，逐项生成保留或拒绝理由。单目标可表达的约束绑定后迁移；跨目标 CHECK、无法保存的索引/唯一约束及任何无法枚举的受支持依赖阻止发布。不得根据 SQL 文本替换列名。客户端保存的任意 SQL 无法枚举，保留来源表可避免静默改坏；首版不提供透明替换。

执行流程为 prepare → validate → publish：prepare 创建不对业务 catalog 可见的暂存对象；validate 固定来源视图并生成目标、验证重建；publish 在短结构锁内复查身份、权限、对象编号、schema 与数据版本，原子登记全部目标。来源变化返回 serialization_failure，不自动重试。暂存对象需要空间预算、作业编号和取消清理。

现有事务禁止 DDL，不能拼接 CREATE/INSERT 伪装成原子迁移。实施前必须加入包含 catalog、页文件、索引和所有权声明的迁移 WAL 发布组；提交前暂存文件可清理，提交后故障进入 recovery_required 并重放到完整目标组。对象编号可以跳号但不得复用。恢复仅依据作业状态与 WAL 清理暂存对象，不能按名称猜测。

后续替换来源需独立设计：稳定对象引用、授权重绑定、完整依赖图与原子交换。保留原对象及其编号直到该方案验收；当前阶段不提供多轮自动 BCNF 化。

### 迁移验收

验收包含重复/NULL/不成立 FD 的拒绝、依赖不保持、跨目标约束、并发修改、撤权、名称占用、取消、空间不足，以及每个 WAL/文件发布边界的崩溃恢复；失败不得留下部分可见目标。
