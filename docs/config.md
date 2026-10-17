# 配置选项

[← 返回 README](../README.md)

本页帮助你查找配置项、修改监听端口和数据路径。ChuSQL 使用统一配置文件 `settings.toml`，默认位置如下：

| 平台 | 路径 |
| --- | --- |
| Windows | `%APPDATA%\ChuSQL\settings.toml` |
| Linux / macOS | `$XDG_CONFIG_HOME/ChuSQL/settings.toml`；未设置时为 `~/.config/ChuSQL/settings.toml` |

`chusql-server`、`chusql-web` 和 `csql` 均可通过 `--config <path>` 临时指定其他配置文件。配置优先级为：**命令行参数 > 配置文件 > 内置默认值**。

本页内容：

- [存储配置](#存储配置)
- [Web 配置](#web-配置)
- [Server 配置](#server-配置)
- [账号与安全](#账号与安全)
- [修改配置](#修改配置)

## 存储配置

| 配置项 | 默认值 | 说明 |
| --- | --- | --- |
| `[page] size` | `4096` | 数据页大小（字节） |
| `[btree] order` | `4` | B+ 树阶数 |
| `[buffer] pool_size` | `1024` | 缓冲池页数 |
| `[storage] data_dir` | 平台默认数据目录 | 数据文件保存位置 |
| `[storage] log_files` | 安装时填写平台日志目录 | 诊断日志目录；WAL 位于数据库目录 |
| `[log] level` | `info` | 日志级别 |

正常运行时数据目录由 `chusql-server` 访问，离线初始化与修复由 `csql-bootstrap` 访问。安装器填写绝对路径，日志目录由启动器与安装脚本使用。

## Web 配置

以下配置项位于 `[web]` 节，例如 `port = 7778`。

| 配置项 | 默认值 | 说明 |
| --- | --- | --- |
| `port` | `7778` | Web 服务端口 |
| `listen_host` | `127.0.0.1` | Web 服务监听地址 |
| `cookie_secure` | `false` | HTTPS 部署时应启用 |
| `body_limit` | `65536` | 请求体大小上限（字节） |
| `session_idle` | `28800` | 会话空闲超时（秒） |
| `session_max` | `86400` | 会话最大存活时间（秒） |
| `login_max_attempts` | `5` | 登录失败限制阈值 |
| `login_window` | `300` | 登录限制统计窗口（秒） |
| `rows_per_page` | `25` | 默认分页大小 |
| `max_page_size` | `500` | 最大分页大小 |
| `max_rows` | `1000` | 单次查询最大返回行数 |
| `max_sql_length` | `20000` | SQL 最大字符数 |
| `password_min_length` | `12` | 普通账号最短口令长度 |
| `password_classes` | `2` | 口令至少包含的字符类别数 |

静态资源目录固定为安装目录下的 `static`。管理员账号名由启动参数 `--user` 指定，默认为 `root`；这两项分别由安装目录和启动参数决定。

## Server 配置

以下配置项位于 `[server]` 节，例如 `port = 7777`。Web 端口在独立的 `[web]` 节配置。

| 配置项 | 默认值 | 说明 |
| --- | --- | --- |
| `port` | `7777` | 数据库服务端口 |
| `listen_host` | `127.0.0.1` | 数据库服务监听地址 |
| `max_message` | `1048576` | 单条请求最大大小（字节） |
| `max_rows` | `1000` | 单次查询最大返回行数 |

本机 sudo 认证额外使用 `[server] sudo_auth_user`，默认空字符串，表示关闭。设置为一个已启用 `ALLOW_SUDO_AUTH` 的数据库身份后，必须以 Unix root 或 Windows 提升后的管理员运行 `chusql-server`。

这是明确的本机身份映射，与启动参数 `--user` 和最高权限属性分别设置。配置变更后重启服务生效，此项通过配置文件管理。操作步骤见 [用户与角色](users-and-roles.md#本机-sudo-认证)。

> [!NOTE]
> 修改 Server 配置后须重启 `chusql-server`。sudo 映射默认关闭，启用时还须满足身份属性和操作系统提升要求。

## 账号与安全

账号口令保存在系统数据库中。管理员口令在安装或系统引导时设置，之后可通过 SQL 或 Web 管理界面修改。

系统目录未完成初始化时，`chusql-server` 将拒绝启动并提示运行 `csql-bootstrap`。

对外部署时建议：

- 数据库服务仅监听受控网络接口；
- Web 服务通过 HTTPS 或反向代理提供；
- HTTPS 环境启用 `cookie_secure`。

## 修改配置

配置可通过以下方式修改：

- 直接编辑 `settings.toml`。
- 通过 Web 设置页面修改。

修改后重启对应服务，并检查监听地址、端口和登录结果。Docker 的启动配置只读挂载，需修改宿主文件后重新创建服务，见 [Docker 配置](docker.md#配置端口目录与资源)。

迁移数据目录时，先停止相关进程，再完整复制数据目录并更新 `data_dir`；保留原目录备份，启动后检查业务数据。已有数据的 `page.size` 和 `btree.order` 须保持原值；磁盘格式转换需专门的迁移流程。

安装器选择 `resources/settings.toml.linux` 或 `resources/settings.toml.windows`，填入当前用户的绝对路径后释放为 `settings.toml`。两份模板逐项提供英文注释。

默认数据和日志路径如下：

| 平台 | 数据目录 | 日志目录 |
| --- | --- | --- |
| Linux | `${XDG_DATA_HOME:-$HOME/.local/share}/chusql/data` | `${XDG_STATE_HOME:-$HOME/.local/state}/chusql/logs` |
| Windows | `%LOCALAPPDATA%\ChuSQL\data` | `%LOCALAPPDATA%\ChuSQL\logs` |

安装参数 `--data-dir` / `-DataDir` 可覆盖数据目录。安装目录与数据目录分别设置。
