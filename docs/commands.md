# 命令与选项

ChuSQL 的主要用户命令如下：

| 命令 | 作用 |
| --- | --- |
| `chusql-server` | 启动数据库服务 |
| `chusql-web` | 启动 Web 管理端 |
| `csql` | 启动命令行客户端 |
| `csql-web` | 按配置启动 Web 管理端 |

各组件默认使用同一份 `chusql.toml`。需要临时使用其他配置时，可通过 `--config FILE` 指定。

## csql

基本用法：

```sh
csql [options]
```

常用选项：

| 选项 | 说明 |
| --- | --- |
| `-u, --user USER` | 指定登录账号 |
| `-d, --database DB` | 登录后选择数据库 |
| `-f, --format FORMAT` | 输出格式：`table`、`json` 或 `csv` |
| `-e, --execute SQL` | 执行一条 SQL 后退出 |
| `--history FILE` | 指定历史记录文件 |
| `--config FILE` | 指定配置文件 |
| `-h, --help` | 显示帮助 |

示例：

```sh
csql -d sales -e "SELECT * FROM orders LIMIT 5"
csql -f json -e "SELECT count(*) FROM orders"
```

表名与列名的写法：

- 选定数据库后语句里直接写表名即可，例如 `SELECT name FROM orders WHERE orders.id = 1`。
- 表名可以带库前缀（`sales.orders`），列名可以带表限定符（`orders.id`）或库限定符（`sales.orders.id`），这几种写法指向同一张表和同一个列。
- 只有候选唯一时才自动推导前缀；同名的表或列出现在多个候选上会以 `ambiguous table` / `ambiguous column` 报错，需要写全。
- `FROM orders AS o` 之后只能用 `o.id`，不能再写 `orders.id`。

事务：

- `BEGIN`（或 `START TRANSACTION`）开始显式事务，`COMMIT` 提交，`ROLLBACK` 回滚。
- 事务内只允许数据语句；DDL 与 `USE` 会被拒绝，`SHOW DATABASES` 这类不碰表数据的语句照常可用。
- 事务里的读都看开事务那一刻的快照：别的连接在事务期间提交的改动不会出现在事务里，本会话未提交的改动别的连接也看不见。
- 同一连接同时只能有一个事务，没有保存点与嵌套事务；开事务前要先选好数据库。
- 提交时按行 `id` 写回本事务改动过的行，事务没碰过的行不受影响；没有整数 `id` 的表按整表替换提交。
- 提交是持久的：整批改动与一条提交标记一次落盘，提交后进程被强杀也会在重启时重放；提交前崩溃则整批丢弃，不会只落一半。

### 交互模式

SQL 语句以分号结束，可跨多行输入。

常用元命令：

| 命令 | 作用 |
| --- | --- |
| `\q` | 退出 |
| `\?` | 查看帮助 |
| `\l` | 列出数据库 |
| `\dt` | 列出当前数据库中的表 |
| `\d <表>` | 查看表结构 |
| `\c <库>` | 切换数据库 |
| `\f <格式>` | 修改输出格式 |
| `\dr` | 查看角色与授权信息（管理员） |

## 账号与权限

默认管理员账号为 `root`。普通账号创建后默认不拥有数据访问权限，需要管理员通过角色进行授权。

常用账号命令：

```sql
CREATE USER alice IDENTIFIED BY 'password';
ALTER USER alice IDENTIFIED BY 'new-password';
DROP USER alice;
```

常用角色和授权命令：

```sql
CREATE ROLE analyst;
GRANT SELECT ON orders TO analyst;
GRANT analyst TO alice;
REVOKE SELECT ON orders FROM analyst;
REVOKE analyst FROM alice;
DROP ROLE analyst;
```

当前表级权限包括：`SELECT`、`INSERT`、`UPDATE`、`DELETE` 和 `ALL`。

管理员拥有完整权限；数据库、表、索引等结构管理操作仅允许管理员执行。

系统数据库 `system` 仅允许管理员访问。

## chusql-web

基本用法：

```sh
chusql-web [options]
```

常用选项：

| 选项 | 说明 |
| --- | --- |
| `--host H` | 监听地址 |
| `--port N` | 监听端口 |
| `--static DIR` | 静态资源目录 |
| `--user NAME` | 管理员账号名 |
| `--config FILE` | 指定配置文件 |
| `-h, --help` | 显示帮助 |

其余请求大小、会话、登录限制、分页等参数通常通过 `chusql.toml` 配置即可。

## chusql-server

基本用法：

```sh
chusql-server [options]
```

常用选项：

| 选项 | 说明 |
| --- | --- |
| `--host H` | 数据库服务监听地址 |
| `--port N` | 数据库服务监听端口 |
| `--max-message N` | 单条请求大小上限 |
| `--max-rows N` | 单次查询返回行数上限 |
| `--config FILE` | 指定配置文件 |
| `-h, --help` | 显示帮助 |

`chusql-server` 是数据目录的唯一直接访问者。Web、CLI 及其他客户端均通过数据库服务访问数据。
