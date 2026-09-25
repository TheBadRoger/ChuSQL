# AGENTS.md — ChuSQL 开发契约

给 AI agent 的项目规则。**约束优先于便利**：任何"暂时放宽"都必须先在此文件里改数字，并说明理由，否则视为违约。
进度与待办的唯一事实来源是 `projectplan.txt`；本文件只管"什么算做完"。

---

## 1. 硬约束（CONSTRAINTS）

违反任一条 = 改动不算完成，不许提交、不许进 `projectplan.txt` 的 ✅。

| # | 约束 | 当前值 | 校验方式 |
|---|---|---|---|
| C1 | Haskell 测试全绿 | **381 条**（engine 200 + web 181） | `cd chusql-engine && stack test`、`cd chusql-web && stack test` |
| C2 | Rust 测试全绿 | **78 条** | `cd chusql-storage && cargo test` |
| C3 | Haskell 编译零警告 | `-Wall` 下 **0 warning** | `stack build` 输出无 `warning:`（engine 与 web 两个包都要看） |
| C4 | 不引入抑制手段 | 禁止 `{-# OPTIONS_GHC -Wno-... #-}`、`#[allow(...)]`、`@ts-ignore`、`--no-verify`、跳过/删除测试、注释掉断言 | 看 diff |
| C5 | 不吞错误 | Rust 拿不到 `unwrap()/expect()` 当控制流；Haskell 不用 `error`/`head` 兜底。能返回错误的走 `Result` / `Either` | 看 diff + clippy 提示 |
| C6 | 索引语义不得回退 | "索引列必须唯一"是**决策 14**，不是实现细节；放宽它必须同时改 `projectplan.txt` 的已知问题 6/9 | 看 diff |
| C7 | 性能结论必须同轮交叉测 | 机器漂移 15~25%（已知问题 4）；**小于 2 倍的差异不当结论**，改前改后必须同轮前后跑 | 基准记录 |
| C8 | 已实测的功能不许只留"应该能跑" | 新能力要么有测试断言，要么在 `projectplan.txt` 标 ⬜ 未做 | 看 `projectplan.txt` |
| C9 | 测试数下降 = 阻塞 | 新增测试可以增数；数字变小必须解释哪条被移除及原因 | 前后对比 |

**已知的历史违约模式**（别重复）：
- P1 遗留：`Spec.hs` 的 "INSERT ... via IPC" 用例只断言了结果，**没断言"没走 scan"** —— 需求被"测试绿了"掩盖。补法是检查服务端 debug 日志里不出现 `scan`。
- 若哪天要"为了跑通先注释掉断言" —— 停手，改成把这条写进 `projectplan.txt` 的 ⬜。

---

## 2. 改动规则

1. **改 `chusql-engine` 源码后**，跑全链路基准前必须先 `stack build --force-dirty`，否则基准可能链到旧引擎（plan 第 146 行）。
2. **表存在性判据**以 `catalog` 为准（不是文件长度），空表也是合法表；改这条要先搜它**所有**调用点（决策 7、8）。
3. **索引元数据只放一处**（数据字典）。优化器只把"单列等值条件"标成"可点查"，有没有索引由存储层回答（`no_index` → 上层退回全表扫描）。不许在优化器里缓存一份索引表（决策 13）。
4. **等价改造不许改结果**。典型：等值连接换哈希表，输出顺序必须和嵌套循环一致（左表序为主、右表序为辅，同键下右行保序）；认不出等值键或某侧空表时退回嵌套循环（决策 9）。
5. **存储接口分 `schema` 与 `snapshot`**：语义检查与优化只走 `schema`，不许再拉全库行（决策 10）。
6. **测试的位置约定**：Haskell 引擎测试全部在 `chusql-engine/test/Spec.hs`；Web 在 `chusql-web/test/Spec.hs`（web 是独立包，不能反向依赖 engine 的测试）；Rust 在 `chusql-storage/tests/*_test.rs`，另有 `src/log.rs` 里 3 条单元测试。新增测试放同一处，不另起目录。
7. **只留根目录一个 README**：性能基准的跑法/结果/踩坑都写进 `README.md`；设计长文写 `docs/ENGINE_INTRO.md`、`docs/STORAGE_INTRO.md`，P3 的规格写 `docs/WEB_SPEC.md`。
8. 改任何**决策**或**已知问题**，同一轮内更新 `projectplan.txt`。

---

## 3. 已知正确性缺口（最高优先级）

- **重放不幂等**（已知问题 1）：断电卡在"数据已落盘"与"日志清空"之间时，重启会把同一操作再作用一遍，`INSERT` 多一行。**这是当前唯一的正确性缺口**，不是功能缺失。
  做 P7 完整版 WAL 时，验收标准 = 给每次操作记 LSN、重放前比对是否已作用过，并有一条"中途断电重放两次、行数不变"的测试。

---

## 4. Skill 路由表

已安装 7 个相关 skill（源：`addyosmani/agent-skills`，全局位于 `~/.agents/skills`）。按阶段加载，**不要全部加载**。

| 阶段 | 先加载 | 用途 |
|---|---|---|
| 任何阶段开工 | `constraint-driven-development` | 对照本文件 C1–C9，检查本轮有没有偷偷降标准 |
| 写代码 / 修 bug（全程） | `test-driven-development` | 先红后绿；C1/C2 的 200/181/78 是底线 |
| 编译失败、测试失败、行为不符预期、IPC/FFI 异常 | `debugging-and-error-recovery` | 根因定位，禁止"试一下看看"式修改 |
| P3 Web（✅ 已完成，改动 Web 时仍照此走） | `spec-driven-development` → `api-and-interface-design` → `security-and-hardening` | 先出 spec 再定接口，最后过输入校验与 Session 安全 |
| P3 启动前（无 spec 时） | `spec-driven-development` | Web 涉及 5 块能力，必须先拆能力图 |
| P4 CLI | `api-and-interface-design` | 与 Web 共用 Haskell 后端，接口一次定稳 |
| P5 FFI（C ABI + 跨语言所有权） | `api-and-interface-design` → `debugging-and-error-recovery` | ABI 契约 + 内存所有权问题定位 |
| P2 遗留 A/B（字节键、重复键、范围扫描） | `test-driven-development` → `performance-optimization` | 重写比较器与分裂节点，必须有前后同轮数字 |
| P6 SQL 功能补齐 | `test-driven-development` | 每条语法/语义能力都要有用例 |
| P7 代价优化 / 统计信息被"用"上 | `performance-optimization` | 遵 C7；已知问题 3（池淘汰 O(池容量)）绝对值 2.8 µs/次，先证明值得改 |
| P7 完整版 WAL（含重放幂等） | `debugging-and-error-recovery` + 对抗式复核 | 见上面第 3 节，正确性问题 |

**明确不用**（本项目场景不匹配，别加载）：
`browser-testing-with-devtools`（需 chrome-devtools MCP，本机没配；Web 的端到端用
`stack test` 里那条真 HTTP 用例顶着）、
`deprecation-and-migration`、`observability-and-instrumentation`、`shipping-and-launch`（无生产部署）、
`planning-and-task-breakdown`（路线图已在 `projectplan.txt`）、
`git-workflow-and-versioning` / `code-review-and-quality` / `refactor` / `code-simplification`（与既有习惯重叠，只增流程噪音）。

---

## 5. 每轮收尾清单

- [ ] `cd chusql-engine && stack test` → 200 条全绿
- [ ] `cd chusql-web && stack test` → 181 条全绿（含一条真起 Rust 存储进程的端到端；`static/` 未构建时 14 条构建产物契约测试记 pending）
- [ ] `cd chusql-storage && cargo test` → 78 条全绿
- [ ] `stack build` 输出 0 warning（`-Wall`，engine 与 web 两个包）
- [ ] diff 里没有 C4 列的抑制手段
- [ ] 动过性能路径 → 同轮交叉测，数字进 `README.md` 表格
- [ ] 动过决策/已知问题 → `projectplan.txt` 同一轮更新
- [ ] 新能力的测试数只增不减（C9）

---

## 6. 常用命令

```bash
# Haskell 引擎测试
cd chusql-engine && stack test

# Web 测试（端到端那条会自己 cargo build --bin chusql-storage 并起真进程）
cd chusql-web && stack test

# Rust 测试
cd chusql-storage && cargo test

# 一键把三个模块都起来（Rust 存储 + Haskell Web + 浏览器）
#   .\script\chusql.ps1 web -Port 9000       临时端口（关掉即失效）
#   .\script\chusql.ps1 config set port 9000 存成设置（script\chusql.settings.json）
#   .\script\chusql.ps1 config / cli / help  看设置 / CLI 占位（会起 web）/ 帮助
#   （每个开关都能换成 CHUSQL_* 环境变量；清单见 README 的配置表）
# 手动分开起（一条命令连 Rust 存储进程一起拉）
cd chusql-storage && cargo build --release
cd chusql-web && stack run chusql-web -- --storage ../chusql-storage/target/release/chusql-storage.exe
#   浏览器打开 http://127.0.0.1:7777/ ；
#   端口被占或被系统保留时换一个：--port 7778
#   （Windows 查保留段：netsh interface ipv4 show excludedportrange protocol=tcp）

# 全链路基准（Haskell 引擎 + Rust 存储进程一起起）
cd chusql-storage && cargo build --release
cd benchmark/fullchain && stack run chusql-fullchain -- 1000 1000
#   ⚠️ 改过 chusql-engine 源码 → 先 stack build --force-dirty

# 分层基准
cd benchmark/chusql-engine && stack run      # 查询层（内存数据，隔离优化器）
cd benchmark/chusql-storage && cargo bench --offline   # 存储层微基准
```

> 上一轮评估：2026-09-25（P3 ⑰ 全部完成：按 DataGrip 截图把界面整个重做——主工具栏 + "Database Explorer" 工具窗（标题栏窗口动作/小工具栏/连接→库→schema→Database Objects→表→列的树）+ 标签页 + 数据编辑器工具栏 + 带行号槽与类型字形的网格 + 状态栏；图标换成 IDEA 风格线稿，配色换 IDEA 深浅两套；顺手做实了标签页、行号槽、单元格游标与方向键、工具栏加减行、DDL 只读查看、树过滤、折叠全部、工具窗三个窗口动作与下拉菜单）。下一项：P2 遗留 A/B（复合键 + 字节键 → 非唯一索引）。
