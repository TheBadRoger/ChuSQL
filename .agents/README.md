# AI agent 协作资料

本页帮助 AI agent 选择任务所需资料。按任务读取，不将全部资料作为每次工作的上下文。

## 资料入口

各文件的职责如下：

- 根目录 `AGENTS.md`：开发规则和测试入口。
- `context/progress.txt`：唯一进度、测试计数和验收来源。
- `context/known-issues.txt`：未解决限制；`design-decisions.txt`：现有设计约束。
- `context/main-roadmap.txt`：长期阶段；`side-branch-plan.txt`：近期任务及依赖。
- `skills/chusql-*/`：类型、存储、跨组件边界三个项目专用技能。

## 文档与技能

文档写作使用全局 `technical-documentation` 和 `plain-writing`，具体规则见根目录 `AGENTS.md` 的“文档编写”一节。

通用技能、安装锁文件及技能生成提示词仅保留在本地，不纳入版本控制。
`gate-check/` 为开发者和 CI 共用的门禁工具；`logs/` 为忽略的本地产物。
这些协作资料不参与应用构建、运行或发行打包。
