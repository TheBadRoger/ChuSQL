# gate-check — 项目门禁

独立门禁目录：脚本与说明都在这一个目录，入口是 `run-gate.ps1`，三条车道并行；车道日志与临时目录写到仓库根的 `logs/`。

## 快速开始

```powershell
pwsh -NoProfile -File gate-check/run-gate.ps1              # 全量
pwsh -NoProfile -File gate-check/run-gate.ps1 -Clean       # 先清各项目自己的构建工件
pwsh -NoProfile -File gate-check/run-gate.ps1 -Full        # 连依赖副本一起清（最慢，怀疑旧产物复用时才用）
pwsh -NoProfile -File gate-check/run-gate.ps1 -NoLint      # 跳过 cargo clippy
pwsh -NoProfile -File gate-check/run-gate.ps1 -Only server # 只跑一条车道：cargo|stack|comments
```

## 车道与步骤

| 车道 | 步骤 | 说明 |
| --- | --- | --- |
| build | `cargo build --release` | 先单独跑：Haskell 测试要从 `chusql-core/storage/target/release` 加载动态库 |
| cargo | `cargo test --release`、`cargo clippy --all-targets -- -D warnings` | `-NoLint` 跳过 clippy |
| stack | engine → server → web → cli → bootstrap 的 `stack test <pkg> --fast` | 串行：stack 与快照目录不能并发写 |
| comments | `comment-check.ps1`、`comment-purity.ps1` | 注释长度与注释纯度 |

`build` 车道先跑完，再并行启动其余三条；这样动态库刷新与 Haskell 测试不会互相抢文件。

Web 步骤会先显式刷新该工程里的 model、engine、interface 与 server 本地依赖，再运行 Web 测试，避免构建工具依赖复用旧的注册库。刷新失败会使 Web 步骤失败，不继续测试。

## 退出码与日志

- 每步打印一行 `STEP_EXIT <步骤> <码>`；任一非零则 `GATE_EXIT=1`，否则 `GATE_EXIT=0`。
- 末尾打印 `STEPS=...`（各步骤退出码）与总用时。
- 日志在仓库根的 `logs/<车道>.log`（stderr 在 `.err.log`），终端实时转发同一份内容；文件不进版本库。
- `comment-check.ps1` 有问题数时退出 1；`comment-purity.ps1` 只报告差异，恒退出 0。

## 相对旧脚本的提速点

1. 默认不再 `stack clean --full`：改为零清理；`-Clean` 只清各项目自己的工件，`-Full` 才回到旧行为。
2. 车道并行：Rust 测试 + clippy、五个 Haskell 套件、注释检查同时跑。
3. 输出流式：旧脚本 `| Out-String` 要等整步跑完才吐字，这里逐行实时转发。
4. 单车道开关 `-Only`，改一个模块时不用全量。

## 约定

- 门禁由项目所有者手动运行；agent 只跑模块级测试，不跑全量门禁。
- 期望基线（随里程碑更新）：Engine 373 / Server 102 / Web 56 / CLI 13 / Bootstrap 9 / Rust 154。
- 计数与主线进度以 `agent-credientials/project-plan.txt` 为准。
