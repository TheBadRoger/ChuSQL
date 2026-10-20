# 配置选项

[← 返回 README](../README.md)

本页帮助你查找配置项，并修改监听端口、数据路径和访问限制。ChuSQL 使用统一配置文件 `settings.toml`，默认位置如下：

| 平台 | 路径 |
| --- | --- |
| Windows | `%APPDATA%\ChuSQL\settings.toml` |
| Linux / macOS | `$XDG_CONFIG_HOME/ChuSQL/settings.toml`，默认 `~/.config/ChuSQL/settings.toml` |
| Linux 系统级安装 | `/etc/chusql/settings.toml` |

系统级安装（deb 软件包或安装器 `--system`）使用 `/etc/chusql/settings.toml`；该文件存在时，程序和客户端优先读取它，其余情况读取 XDG 路径。

`chusql-server` 和 `csql` 都可以用 `--config <path>` 临时指定其他配置文件。配置优先级为：**命令行参数 > 配置文件 > 内置默认值**。

## 存储配置

| 配置项 | 默认值 | 说明 |
| --- | --- | --- |
| `[page] size` | `4096` | 数据页大小（字节） |
| `[btree] order` | `4` | B+ 树阶数 |
| `[buffer] pool_size` | `1024` | 缓冲池页数 |
| `[storage] data_dir` | 平台默认数据目录 | 数据文件保存位置 |
| `[storage] log_files` | 安装时填写平台日志目录 | 诊断日志目录；WAL 位于数据库目录 |
| `[log] level` | `info` | 日志级别 |

正常运行时数据目录由 `chusql-server` 访问，离线初始化与修复由 `csql-bootstrap` 访问。安装器写入绝对路径，启动器和安装脚本共用日志目录。

## Server 配置

以下配置项位于 `[server]` 节，例如 `port = 7777`。

| 配置项 | 默认值 | 说明 |
| --- | --- | --- |
| `port` | `7777` | 数据库服务端口 |
| `listen_host` | `127.0.0.1` | 数据库服务监听地址 |
| `login_max_attempts` | `5` | 登录失败限制阈值 |
| `login_window` | `300` | 登录限制统计窗口（秒） |
| `password_min_length` | `12` | 普通账号最短口令长度 |
| `password_classes` | `2` | 口令至少包含的字符类别数 |
| `max_message` | `1048576` | 单条请求最大大小（字节） |
| `max_rows` | `1000` | 单次查询最大返回行数 |


## 账号与安全

账号口令保存在系统数据库中。管理员口令在安装或系统引导时设置，之后通过 SQL 修改。系统目录未初始化时，`chusql-server` 拒绝启动并提示运行 `csql-bootstrap`。

对外部署时让数据库服务只监听受控网络接口，远程访问使用可信网络、SSH 隧道或 VPN。

## 修改配置

直接编辑配置文件。修改后重启对应服务，并检查监听地址、端口和登录结果。Docker 的启动配置只读挂载，修改宿主文件后重新创建服务，见 [Docker 配置](docker.md#配置端口目录与资源)。

迁移数据目录时，先停止相关进程，再完整复制数据目录并更新 `data_dir`，保留原目录备份，启动后检查业务数据。已有数据的 `page.size` 和 `btree.order` 保持原值，磁盘格式转换使用专门的迁移流程。

安装器从 `resources/settings.toml.linux` 或 `resources/settings.toml.windows` 模板填入当前用户的绝对路径，写为 `settings.toml`；模板逐项提供英文注释。安装时加 `--low-performance-adapted`（Windows 为 `-LowPerformanceAdapted`），安装器会把 `resources/settings.low-performance.toml` 中的参数值覆盖进新写的 `settings.toml`，已有配置文件保持不变。

默认数据和日志路径如下：

| 平台 | 数据目录 | 日志目录 |
| --- | --- | --- |
| Linux | `${XDG_DATA_HOME:-$HOME/.local/share}/chusql/data` | `${XDG_STATE_HOME:-$HOME/.local/state}/chusql/logs` |
| Linux 系统级安装 | `/var/lib/chusql` | `/var/log/chusql` |
| Windows | `%LOCALAPPDATA%\ChuSQL\data` | `%LOCALAPPDATA%\ChuSQL\logs` |

系统级安装的配置文件属主为 `root:chusql`、权限 `0660`，数据与日志目录属主为 `chusql:chusql`、权限 `0750`。安装参数 `--data-dir` / `-DataDir` 可以覆盖数据目录，安装目录与数据目录分别设置。
