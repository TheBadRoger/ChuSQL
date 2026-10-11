# AGENTS.md — ChuSQL 开发契约

参考：
- 唯一进度与验收来源：`.agents/context/progress.txt`（等价于 project-plan.txt）。
- 限制与设计：`.agents/context/known-issues.txt`、`.agents/context/design-decisions.txt`。

## 核心约束
- 测试必须通过，禁止绕过检查。
- 不吞错误，使用明确错误返回。
- FFI 响应由 Rust 分配并通过配套 free 归还；句柄关闭不得与请求并发。
- 表存在性以 catalog 为准。
- 语义检查和优化只使用 schema。
- 索引由存储层管理，优化器只判断可点查条件。
- 优化不能改变结果语义。
- 持久化、事务、并发、类型及 FFI 改动优先维护不变量、内存所有权与错误传播；保留必要的迁移和恢复约束。

## 开发方式
- 按任务选择已安装 Skills：编码用 ponytail / karpathy-guidelines，审查与简化按需用 ponytail-review；Rust、FFI、测试及覆盖率分别使用对应项目技能。
- 类型、值、转换或表达式语义用 `chusql-type-system`；存储、序列化、系统目录、初始化或恢复用 `chusql-storage-integrity`；FFI、跨组件协议、序列化契约或外部接口用 `chusql-boundary-contracts`（均在 `.agents/skills/`）。
- 跨领域组合相关 Skills，无关任务不加载；Skills 不覆盖更高优先级的安全和工程约束。
- 简单任务直接执行；采用最小必要改动，允许有明确收益且能验证的局部结构优化，优先删除重复和无用代码。
- 测试强度与实际风险及影响范围匹配，不机械新增测试，不盲目追求覆盖率；数据库内核微改动也须验证关键行为。
- 不强制 TDD、子代理或多阶段审查；减少无关读取、重复搜索、工具调用和输出，Skills 不应增加无收益的流程。
- 文档描述当前状态；不创建整改报告、修改流水账或无用途的兼容层，不修改 Git 历史。

## 注释规范
- 文件级：外部引用语句（Haskell `import`、Rust `use`、JS `import`、HTML 的 `<link>`/`<script>`、
  CSS `@import`、脚本的依赖/参数区）之后写一段注释，概括本文件的功能；没有引用语句的文件写在
  文件开头。长度不超过 3 行、合计不超过 90 字。
- 函数级：每个函数（含 Haskell 的 `where`/`let` 内函数、Rust 的私有 `fn`）前用一行不超过
  30 字的注释说明它做什么。
- 摘要说明做什么；仅在必要处补充不直观的不变量、所有权与安全约束，不记录历史和被删实现。

## 协作规则
- 全量门禁入口：`gate-check/run-gate.ps1`（三条车道并行，日志写仓库根的 `logs/`）。
- 门禁由项目所有者手动运行；agent 只跑改动模块的测试，拿到门禁结果后再提交。
- engine / web / storage 使用规定测试入口。
- 规定入口见 `gate-check/run-step.ps1`；Haskell 构建串行，存储协议改动须重建实际动态库。
- 改动说明只在交流中记录。
