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

## 协作规则
- engine / web / storage 使用规定测试入口。
- 注释、文案保持简短。
- 改动说明只在交流中记录。

## 当前重点
- 完善 WAL 重放幂等。
- 完整事务路线见 project-plan.txt 阶段 12。
