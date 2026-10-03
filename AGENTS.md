# AGENTS.md — ChuSQL 开发契约（简版）

参考：
- 进度与待办：`project-plan.txt`
- 已知问题：`known-issues.txt`
- 设计依据：`design-decisions.txt`

## 核心约束
- project-plan.txt 是唯一进度来源。
- 测试必须通过，禁止绕过检查。
- 不吞错误，使用明确错误返回。
- 表存在性以 catalog 为准。
- 语义检查和优化只使用 schema。
- 索引由存储层管理，优化器只判断可点查条件。
- 优化不能改变结果语义。

## 注释规范
- 文件级：外部引用语句（Haskell `import`、Rust `use`、JS `import`、HTML 的 `<link>`/`<script>`、
  CSS `@import`、脚本的依赖/参数区）之后写一段注释，概括本文件的功能；没有引用语句的文件写在
  文件开头。长度不超过 3 行、合计不超过 90 字。
- 函数级：每个函数（含 Haskell 的 `where`/`let` 内函数、Rust 的私有 `fn`）前用一行不超过
  30 字的注释说明它做什么。
- 只写"做什么"，不写设计理由、历史与被删掉的旧实现；那些放 `design-decisions.txt` /
  `known-issues.txt`。

## 协作规则
- engine / web / storage 使用规定测试入口。
- 改动说明只在交流中记录。

## 当前重点
- 主线已完成：高级优化器 → 并行查询 → M1 会话隔离 → M2 快照事务 → M3 WAL 与崩溃恢复 → M4 表结构进 WAL → M5 系统表统一命名 → M6 检查点按 LSN 部分截断 → M7 事务写写冲突检测 → M8 事务保存点 → M9 角色继承 → M10 转授权 → M11 FROM 派生表与投影别名 → M12 `WITH` / CTE → M13 比较运算符补全。
- 进度与计数见 `agent_credientials/project-plan.txt`，限制与风险见 `agent_credientials/known-issues.txt`。
