# ChuSQL

本项目是一个简单的数据库项目，服务端采用Haskell和Rust联合编写，采取B/S模式，管理客户端运行在浏览器网页上。

## 仓库布局

| 目录              | 语言                       | 组件                                                              |
| ----------------- | -------------------------- | ----------------------------------------------------------------- |
| `chusql-engine/`  | Haskell                    | 执行引擎：词法分析与AST、表达式求值、关系代数、内存表、服务端接口 |
| `chusql-storage/` | Rust                       | 存储引擎：磁盘IO、B+树、缓冲池、WAL、崩溃恢复、并发控制           |
| `chusql-web/`     | Haskell + React/TypeScript | Web 管理端：Scotty REST API + VS Code Dark+ 风格数据库 Web IDE    |
| `script/`         | PowerShell                 | `chusql.ps1` / `chusql.cmd`：一键启动 / 子命令 / 设置管理         |

# 架构分层

## Haskell部分

作为函数式编程语言，从语言层面契合SQL语句解析和代数数学运算，同时有强大的类型安全系统。

- SQL词法分析、AST
- 语义分析：表/列存在性与类型检查（执行前的关卡）
- 事务处理
- 查询优化、内存表管理
- 关系代数运算
- 服务端接口与请求相应
- Web 管理端：Scotty REST API、登录会话、静态页面交付

## Rust部分

高性能、内存安全且具有强大并行异步编程能力的现代编程语言。

- 磁盘IO
- B+树、页面管理
- 缓冲池
- WAL
- 事务日志和崩溃恢复
- 并发控制

# 性能基准

| 指标                                             | 200 × 200 | 500 × 500 |         1000 × 1000 |
| ------------------------------------------------ | --------: | --------: | ------------------: |
| 单条 `INSERT`（整条语句）                        |   2.66 ms |   2.86 ms |             2.88 ms |
| 同一条 `INSERT`（原始 IPC，不经引擎）            |   2.50 ms |   2.85 ms |             2.77 ms |
| **多行 `INSERT`（100 行/条）**                   |         — |         — |     **0.059 ms/行** |
| `SELECT * FROM users`                            |    0.4 ms |    1.0 ms |              1.9 ms |
| `SELECT name FROM users WHERE age > 90`          |    0.3 ms |    0.8 ms |              2.7 ms |
| `SELECT ... JOIN ... WHERE`                      |    0.9 ms |    2.4 ms |              5.3 ms |
| `SELECT name FROM users WHERE id = k`            |    0.2 ms |    0.2 ms |              0.2 ms |
| `DELETE FROM users WHERE id = k`（删 1 行）      |         — |         — |              4.8 ms |
| `DELETE FROM users`（删全部 1000 行）            |         — |         — |             20.2 ms |
| `SELECT ... WHERE code = k`（无索引 → 建索引后） |         — |         — | 1.8 ms → **0.2 ms** |

> 最近更新：2026-09-26
