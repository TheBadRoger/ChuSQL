# 项目门禁

本页面向开发者，说明如何运行测试门禁和定位失败步骤。在仓库根目录使用 PowerShell 运行 `gate-check/run-gate.ps1`。

门禁先构建存储动态库，再并行运行 Rust、Haskell 和注释检查三个任务组（脚本中称为车道）。日志与临时目录写入仓库根的 `logs/`。

## 快速开始

先安装 PowerShell 7、Cargo、Stack 及项目指定的 GHC，确保这些命令可从终端调用。从仓库根目录按检查范围选择命令；Haskell 构建应串行执行，不要同时启动另一组 Stack 构建：

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
| stack | engine → server → web → cli → bootstrap 的 `stack test <pkg> --fast` | 串行：stack 与快照目录不能并发写 |
| comments | `comment-check.ps1`、`comment-purity.ps1` | 注释长度与注释纯度 |

`build` 车道先跑完，再并行启动其余三条；这样动态库刷新与 Haskell 测试不会互相抢文件。

Server、CLI 和 Bootstrap 步骤先刷新各自工程里的 model、engine 与 interface 本地依赖；Web 步骤还刷新 server，再运行对应模块测试，避免复用旧的注册库。刷新失败会使对应步骤失败，不继续测试。

## 退出码与日志

- 每步打印一行 `STEP_EXIT <步骤> <码>`；任一非零则 `GATE_EXIT=1`，否则 `GATE_EXIT=0`。
- 末尾打印 `STEPS=...`（各步骤退出码）与总用时。
- 日志在仓库根的 `logs/<车道>.log`（stderr 在 `.err.log`），终端实时转发同一份内容；文件不进版本库。
- `comment-check.ps1` 有问题数时退出 1；`comment-purity.ps1` 只报告差异，恒退出 0。

## 清理与局部检查

按需要选择运行范围和清理程度：

| 选项 | 行为 |
| --- | --- |
| 不带清理选项 | 复用构建工件，不主动清理 |
| `-Clean` | 清理各项目自己的构建工件 |
| `-Full` | 连依赖副本一起清理，怀疑复用旧产物时使用 |
| `-Only cargo\|stack\|comments` | 运行指定任务组，不能代替要求执行的全量门禁 |

脚本也提供 `-NoLint` 跳过 Clippy；使用该选项的结果不包含 Clippy 验收，不得以此绕过项目要求的检查。输出逐行实时转发，无需等整步结束。

## 约定

- 局部改动运行受影响模块的规定测试；较大改动由 agent 运行全量门禁。
- 计数、验收状态与主线进度以 `.agents/context/progress.txt` 为准。
