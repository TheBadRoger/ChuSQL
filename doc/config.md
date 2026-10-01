# 配置选项

## 文件在哪

整条链路（存储进程 / Web / 命令行）共用一份 `chusql.toml`，位置固定，**没有环境变量可以覆盖**：

| 平台         | 路径 |
| ------------ | ---- |
| Windows      | `%APPDATA%\ChuSQL\chusql.toml`（`APPDATA` 没设时退到相对路径 `ChuSQL\chusql.toml`） |
| Linux / macOS | `$XDG_CONFIG_HOME/ChuSQL/chusql.toml`，没设就是 `~/.config/ChuSQL/chusql.toml`（`HOME` 也没有才退相对路径 `ChuSQL/chusql.toml`） |

安装脚本就是把 [`scripts/chusql.toml`](../scripts/chusql.toml) 这份模板写到上面的位置，并把安装时给的
`user` / `password` / `data_dir` 填进去。想临时读另一份文件，`chusql-storage`、`chusql-web`、`csql`
都认 `--config <path>`（启动器 `csql-web` 没有选项，只读固定路径）。

优先级：**命令行选项 > 配置文件 > 内置默认**，也就是配置文件写什么，命令行随时能盖掉
（选项表见 [commands.md](commands.md)）。

## 分区归属

| 分区 | 谁读 |
| ---- | ---- |
| `[page]` `[btree]` `[buffer]` `[storage]` `[server]` `[log]` | 存储进程 `chusql-storage` |
| `[web]` | Web 服务 `chusql-web` 与命令行 `csql`（账号、会话、限流、分页上限、静态目录、存储进程路径） |

端点与数据目录只有一份配置：前端读它来决定怎么连，存储进程读它来监听与落盘。

## 存储进程的分区

| 键 | 默认 | 说明 |
| -- | ---- | ---- |
| `[page] size` | `4096` | 页大小（字节） |
| `[btree] order` | `4` | B+ 树阶 |
| `[buffer] pool_size` | `1024` | 缓冲池页数 |
| `[storage] data_dir` | Windows `%LOCALAPPDATA%\ChuSQL\data`，Linux / macOS `${XDG_DATA_HOME:-~/.local/share}/chusql/data` | 数据落哪（模板里那行是安装时替换的占位） |
| `[server] pipe_name` | `chusql-joint` | 端点名（见下） |
| `[log] level` | `info` | 日志级别 |

数据目录跟着平台惯例走，跟安装脚本装的位置是同一处（Windows 装到 `%LOCALAPPDATA%\ChuSQL`、Unix 装到
`~/.local/share/chusql`，数据放在它下面的 `data/`）；`LOCALAPPDATA`/`XDG_DATA_HOME`/`HOME` 都没有时才退相对路径。
要固定就写绝对路径。

存储进程只认一个命令行开关：`--config <path>`（`--help` 看帮助）。

## `[web]` 分区

| 键 | 命令行（`chusql-web`） | 默认 | 说明 |
| -- | ---------------------- | ---- | ---- |
| `host` | `--host` | `127.0.0.1` | 监听地址 |
| `port` | `--port` | `7777` | 监听端口 |
| `static_dir` | `--static` | `static` | 静态资源目录（候选：给定值、`static`、`chusql-web/static`、`../chusql-web/static`、`../static`，取第一个存在的） |
| `user` | `--user` | 配置文件缺失时 `root` | 管理员账号名 |
| `password` | — | 文件里**没有**这一项时用内置演示口令 `root` / `chusql` | 管理员口令（明文，启动时哈希；见「口令与安全」） |
| `storage_server` | `--storage` | 空 | 存储进程可执行文件；留空＝存储已在别处运行，只按 `[server] pipe_name` 去连 |
| `cookie_secure` | `--cookie-secure` | `false` | 给会话 Cookie 加 `Secure`（HTTPS 部署时开） |
| `body_limit` | `--body-limit` | `65536` | 请求体上限（字节） |
| `session_idle` | `--session-idle` | `28800` | 会话空闲超时（秒） |
| `session_max` | `--session-max` | `86400` | 会话最大存活（秒） |
| `login_max_attempts` | `--login-max-attempts` | `5` | 连续失败几次后限流 |
| `login_window` | `--login-window` | `300` | 限流窗口（秒） |
| `rows_per_page` | `--page-size` | `25` | 默认每页行数 |
| `max_page_size` | `--max-page-size` | `500` | 每页行数上限 |
| `max_rows` | `--max-rows` | `1000` | 单次查询返回行数上限 |
| `max_sql_length` | `--max-sql-length` | `20000` | SQL 字符数上限 |
| `seed` | `--seed` | `false` | 库为空时灌演示数据 |
| `password_min_length` | — | `12` | 普通账号口令最短长度（允许 8–256） |
| `password_classes` | — | `2` | 口令至少包含几类字符（允许 1–4：小写/大写/数字/符号） |

### 键名两种写法都认

模板 `scripts/chusql.toml` 用下划线，Web 设置页写的是连字符（规范形式）。两种写法同时出现时**连字符优先**：

    [web]
    rows_per_page = 25      # 模板写法
    rows-per-page = 50      # 设置页写法，生效

对应关系：`static_dir`→`static-dir`、`session_idle`→`session-idle`、`session_max`→`session-max`、
`login_max_attempts`→`login-max-attempts`、`login_window`→`login-window`、`body_limit`→`body-limit`、
`rows_per_page`→`rows-per-page`、`max_page_size`→`max-page-size`、`max_rows`→`max-rows`、
`max_sql_length`→`max-sql-length`、`storage_server`→`storage-server`、
`password_min_length`→`password-min-length`、`password_classes`→`password-classes`。
`host`、`port`、`user`、`password`、`seed` 只有一种写法。

## 端点（`pipe_name`）

端点只有 `pipe_name` 一个配置，两个平台各一条规则：

| 平台 | 实际地址 |
| ---- | -------- |
| Windows | `\\.\pipe\<pipe_name>`（具名管道） |
| Linux / macOS | `<socket_dir>/<pipe_name>.sock`（文件系统套接字） |

`socket_dir` 按 `$XDG_RUNTIME_DIR` → `$TMPDIR` → `/tmp` 依次取。**前端和存储进程必须在同一套环境变量
下启动**，否则两边算出来的路径不一样。Unix 套接字路径上限约 100 字节，`$XDG_RUNTIME_DIR` 太长时会退到
`/tmp`；上一次崩溃残留的 `.sock` 会被下次启动清掉，若还有进程在听同一个端点，启动会报
`another storage server already listens on ...`。

## 口令与安全

- `password` 给了明文 → 启动时哈希；**显式留空**（`password = ""`）＝ 只允许管理员免密登录，普通账号一律拒绝登录；
- **整个文件里没有 `password` 这一项** → 回落到内置演示口令 `root` / `chusql`（仅用于本机试用，正式部署请写死）；
- 普通账号由管理员在 SQL 控制台用 `CREATE USER` / `ALTER USER` / `DROP USER` 管理，口令受
  `password_min_length` / `password_classes` 约束；
- 生产部署建议：`host = 127.0.0.1`（或放在反向代理后）、`cookie_secure = true`、给管理员设口令，
  并且别把数据目录放在共享盘上。

## 改配置的两种途径

1. 直接编辑 `chusql.toml`（新值在下次启动生效）；
2. Web 设置页（`/api/settings`）按分区把改动写回同一份文件——它会写成连字符形式。

每一项都带注释的模板在 [`scripts/chusql.toml`](../scripts/chusql.toml)。
