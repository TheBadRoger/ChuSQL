# 项目门禁

本页面向开发者，说明如何运行测试门禁和定位失败步骤。在仓库根目录使用 PowerShell 运行 `gate-check/run-gate.ps1`。

门禁先构建存储动态库，再并行运行 Rust、Haskell 和注释检查三个任务组（脚本中称为车道）。日志与临时目录写入仓库根的 `logs/`。

## 快速开始

先安装 PowerShell 7、Cargo、Stack 及项目指定的 GHC，确保这些命令可从终端调用。从仓库根目录按检查范围选择命令；同一工作区的 Haskell 构建须串行执行：

```powershell
pwsh -NoProfile -File gate-check/run-gate.ps1              # 全量
pwsh -NoProfile -File gate-check/run-gate.ps1 -Clean       # 先清各项目自己的构建工件
pwsh -NoProfile -File gate-check/run-gate.ps1 -Full        # 连依赖副本一起清（最慢，怀疑旧产物复用时才用）
pwsh -NoProfile -File gate-check/run-gate.ps1 -Only stack  # 只跑 Haskell 车道：cargo|stack|comments
```

## 车道与步骤

| 车道 | 步骤 | 说明 |
| --- | --- | --- |
| build | `cargo build --release` | 先单独跑：Haskell 测试要从 `chusql-core/storage/target/release` 加载动态库 |
| cargo | `cargo test --release`、`cargo clippy --all-targets -- -D warnings` | `-NoLint` 跳过 clippy |
| stack | engine → server → cli → bootstrap 的 `stack test <pkg> --fast` | 串行使用 Stack 与快照目录 |
| comments | `comment-check.ps1`、`comment-purity.ps1` | 注释长度与注释纯度 |

## 退出码与日志

- 每步打印一行 `STEP_EXIT <步骤> <码>`；任一非零则 `GATE_EXIT=1`，否则 `GATE_EXIT=0`。
- 末尾打印 `STEPS=...`（各步骤退出码）与总用时。
- 日志在仓库根的 `logs/<车道>.log`（stderr 在 `.err.log`），终端实时转发同一份内容；日志作为本地产物保存。
- `comment-check.ps1` 有问题数时退出 1；`comment-purity.ps1` 只报告差异，恒退出 0。

`-Clean` 清理本项目工件，`-Full` 连依赖一起清理，`-Only` 用于局部检查。完整验收包含 Clippy。

## 约定

- 局部改动运行受影响模块的规定测试；较大改动由 agent 运行全量门禁。
- 计数、验收状态与主线进度以 `.agents/context/progress.txt` 为准。
