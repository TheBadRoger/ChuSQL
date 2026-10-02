# 命令与选项

## 一共有几个可执行文件

| 命令 | 作用 | 选项 |
| ---- | ---- | ---- |
| `chusql-server` | 数据库服务：装入引擎与存储动态库、独占数据目录，对外是 TCP | 见「`chusql-server` 选项」 |
| `chusql-web` | Web 管理端：REST 接口 + 浏览器里的数据库 IDE | 见「`chusql-web` 选项」 |
| `csql` | 命令行客户端，连数据库服务的 TCP 端点 | 见「`csql` 选项」 |
| `csql-web` | 一键启动器：按配置拉起 Web 服务，日志写 `<install-dir>/logs` | 无选项（`host`/`port` 从配置读） |

四个命令共用同一份 [`chusql.toml`](config.md)；`--config` 都是用来临时读另一份配置的。

## `csql` 选项

    csql [options]

| 选项 | 默认 | 说明 |
| ---- | ---- | ---- |
| `-u, --user USER` | 配置里的管理员 | 用这个账号登录 |
| `-p, --password` | — | 强制交互输入口令，忽略配置里的 |
| `-d, --database DB` | 不选（启动后没有默认库） | 登录后直接选中哪个库 |
| `-f, --format FORMAT` | `table` | 输出格式：`table` / `json` / `csv` |
| `-e, --execute SQL` | — | 跑一条语句就退出（脚本化用） |
| `--history FILE` | `~/.chusql_history` | 历史文件位置 |
| `--config FILE` | 固定位置的 `chusql.toml` | 读另一份配置 |
| `-h, --help` | — | 帮助 |

脚本化示例：

    csql -e "SELECT count(*) FROM orders" -f json
    csql -u analyst -p -d sales -f csv -e "SELECT * FROM orders LIMIT 5"

## 交互模式

- 提示符 `chusql>`；语句以**分号**结尾，可以跨多行；上下键翻历史（默认存 `~/.chusql_history`）。
- 退出码：语句出错时非 0，方便在脚本里判断。

### 元命令

| 元命令 | 作用 |
| ------ | ---- |
| `\q` `\quit` | 退出 |
| `\?` `\h` `\help` | 帮助 |
| `\l` `\list` | 列出数据库 |
| `\dt` `\d` | 列出当前库的表 |
| `\d <表>` | 看表结构 |
| `\dr` `\roles` | 角色总览：授权对象与成员（管理员专属，等价于 Web 的 `GET /api/roles`） |
| `\c <库>` | 切换数据库 |
| `\f` `\format <fmt>` | 切换输出格式：`table` / `json` / `csv` |

普通身份用 `\dt` 时只会看到自己有 `SELECT` 权限的表，系统表永远不出现。

## 账号与权限

账号：root（配置里的 `user` / `password` 决定，见 [config.md](config.md)）是管理员，其余都是普通账号，
默认**零权限**，只能看到被授权的表。系统库 `system`（账号表在里面）只有管理员能进。

服务启动后**只有系统库**，没有任何默认工作库：自己 `CREATE DATABASE shop` 再 `USE shop`。
没选库时只有"限定名"语句能跑（`SELECT * FROM shop.items`），裸表名（`SELECT * FROM items`）会报
`no database selected`——先把库选上。

    CREATE USER alice IDENTIFIED BY 'secret';    -- 普通账号，口令是明文，服务端加盐派生
    ALTER USER alice IDENTIFIED BY 'new-secret'; -- 改口令
    DROP USER alice;

`CREATE USER` / `ALTER USER` / `DROP USER` 只有管理员能执行（Web 端在 SQL 控制台，命令行直接跑）。

权限按**角色**给，三句话：

    CREATE ROLE analyst;                        -- 建角色（平的角色，不可继承）
    GRANT SELECT, INSERT ON orders TO analyst;  -- 授权：对象可以是表名或 *
    GRANT analyst TO alice;                     -- 把角色给账号
    REVOKE INSERT ON orders FROM analyst;       -- 收回权限
    REVOKE analyst FROM alice;                  -- 收回角色
    DROP ROLE analyst;

| 权限 | 覆盖 |
| ---- | ---- |
| `SELECT` | `SELECT`（含 JOIN、投影、`WHERE` 里的子查询） |
| `INSERT` | `INSERT`（含 `VALUES` 里的子查询） |
| `UPDATE` | `UPDATE`（含赋值与 `WHERE` 里的子查询） |
| `DELETE` | `DELETE`（含 `WHERE` 里的子查询） |
| `ALL` | 上面四种，等价于逐条授权 |

- 对象写 `*` 表示当前库里所有表；写表名时按**当前库**限定（在 `sales` 库执行 `GRANT ... ON orders`
  授权的是 `sales.orders`），换库要重新授权。
- 管理员（root）隐式拥有全部权限，不用授权；DDL（建表、建索引等）一律只有管理员能做。
- 角色语句本身只有管理员能跑；普通账号跑 `\dr` / `GET /api/roles` 会被拒（`administrator required`）。
- 授权变更立即生效；已经登录的会话不会被踢下线。
- 当前限制：没有列级授权，没有 `WITH GRANT OPTION`，角色不可继承，也没有自助查询（普通账号不能自己
  看自己的授权，得问管理员）。

Web 端对应的接口是 `/api/roles` 系列（`GET` 列表、`POST` 建角色、`DELETE /api/roles/:name` 删角色、
`POST/DELETE /api/roles/:name/grants` 授权/收权、`POST/DELETE /api/roles/:name/members[/:user]` 成员），
全部管理员专属。

## `chusql-web` 选项

    chusql-web [options]

| 选项 | 对应配置键 | 默认 |
| ---- | ---------- | ---- |
| `--config FILE` | — | 固定位置的 `chusql.toml` |
| `--host H` | `[web] host` | `127.0.0.1` |
| `--port N` | `[web] port` | `7778` |
| `--static DIR` | `[web] static_dir` | `static` |
| `--user NAME` | `[web] user` | `root` |
| `--cookie-secure BOOL` | `[web] cookie_secure` | `false` |
| `--body-limit N` | `[web] body_limit` | `65536` |
| `--session-idle N` | `[web] session_idle` | `28800` |
| `--session-max N` | `[web] session_max` | `86400` |
| `--login-max-attempts N` | `[web] login_max_attempts` | `5` |
| `--login-window N` | `[web] login_window` | `300` |
| `--page-size N` | `[web] rows_per_page` | `25` |
| `--max-page-size N` | `[web] max_page_size` | `500` |
| `--max-rows N` | `[web] max_rows` | `1000` |
| `--max-sql-length N` | `[web] max_sql_length` | `20000` |
| `--seed BOOL` | `[web] seed` | `false` |
| `-h, --help` | — | 帮助 |

REST 接口（界面的一键操作全走结构化接口）：

| 分组 | 接口 |
| ---- | ---- |
| 探活（无需登录） | `GET /api/health`、`GET /api/status` |
| 会话 | `POST /api/login`、`POST /api/logout`、`GET /api/session` |
| 设置 | `GET`/`PUT /api/settings`、`GET`/`PUT /api/ui-settings` |
| 库 | `GET`/`POST /api/databases`、`DELETE /api/databases/:name` |
| 表 | `GET`/`POST /api/tables`、`GET`/`DELETE /api/tables/:t` |
| 行 | `GET`/`POST /api/tables/:t/rows`、`PATCH`/`DELETE /api/tables/:t/rows/:id` |
| 索引与列 | `POST /api/tables/:t/indexes`、`DELETE /api/tables/:t/indexes/:col`、`DELETE /api/tables/:t/columns/:col` |
| 演示数据 | `POST /api/demo-data` |
| SQL 控制台 | `POST /api/query`（`CREATE USER` / `CREATE ROLE` / `GRANT` 也在这里跑） |
| 角色 | `GET`/`POST /api/roles`、`DELETE /api/roles/:name`、`POST`/`DELETE /api/roles/:name/grants`、`POST`/`DELETE /api/roles/:name/members[/:user]` |

除探活、登录和首页（`GET /`、`GET /static/:file`）外都要带会话 Cookie。选库用 `X-ChuSQL-Database` 头：
**不带这个头就是"没选库"**，此时只有限定名语句（`db.table`）能跑，裸表名会得到 400 `no_database`；
`GET /api/databases` 和 `POST`/`DELETE /api/databases/:name` 不需要选库。普通身份碰 `system` 库一律 403。

## `chusql-server` 选项

    chusql-server [options]

| 选项 | 对应配置键 | 默认 |
| ---- | ---------- | ---- |
| `--config FILE` | — | 固定位置的 `chusql.toml` |
| `--host H` | `[server] host` | `127.0.0.1` |
| `--port N` | `[server] port` | `7777` |
| `--user NAME` | `[web] user` | `root` |
| `--max-message N` | `[server] max_message` | `1048576` |
| `--max-rows N` | `[server] max_rows` | `1000` |
| `-h, --help` | — | 帮助 |

启动后打印实际监听地址，它同时是数据目录的唯一持有者：Web 与 `csql` 不开存储，把存储请求交给它转发。
客户端按**一行一个 JSON** 说话，方法与错误码见
[architecture.md](architecture.md) 的「TCP 数据库服务」；用 `nc` 手敲一个会话是这样：

    {"method":"hello","protocol":1}
    {"method":"login","user":"root","password":"..."}
    {"method":"query","sql":"USE test"}
    {"method":"query","sql":"SELECT * FROM users"}
    {"method":"quit"}

对话里每条 SQL 走的是和 Web、`csql` 同一套解析与权限检查，账号与库选择互不共享。
