# 命令与选项

ChuSQL 的主要用户命令如下：

类型说明见 [自定义类型与预定义类型](types.md)，身份属性说明见 [用户、角色与 root 属性](users-and-roles.md)。

SQL 的查询、事务、类型、用户与授权语法见 [syntax.md](syntax.md)。

## 组件命令

安装脚本在启动服务前运行 `csql-bootstrap`：创建系统目录、初始化默认 `root`（LOGIN / SUPERUSER / ENABLED），再登记内置类型。管理员名称可通过安装选项修改。初始化失败时安装报错，停止启动服务。

预装目录 `system.__system_types` 登记 18 个名称与别名：INT、INTEGER、BIGINT、SMALLINT、VARCHAR、CHAR、TEXT、STR、BOOLEAN、BOOL、FLOAT、REAL、DOUBLE、DECIMAL、NUMERIC、DATE、TIMESTAMP、BLOB；记录基础表示及是否接受类型参数。该目录为受保护的系统元数据，不创建预定义 DOMAIN；DOMAIN 仍按工作库保存。

重复引导保留已有身份、密码及一致的类型记录，补齐缺失类型；同名定义冲突明确报错。已有引导身份失去管理员属性时拒绝继续，不自动提升权限。

修复或补齐安装引导时，可停止数据库服务后运行 `csql-bootstrap --config FILE --user root --password-stdin`，通过标准输入提供口令。

系统目录故障时，先停止所有数据库进程，再依次考虑 `csql-bootstrap repair --config FILE`、`recover` 或清除系统身份和授权的 `reset`。`repair` 仅修复结构，涉及数据修改时中止；完整说明见 [bootstrap.md](bootstrap.md)。

| 命令 | 作用 |
| --- | --- |
| `chusql-server` | 启动数据库服务 |
| `chusql-web` | 启动 Web 管理端 |
| `csql` | 启动命令行客户端 |
| `csql-web` | 按配置启动 Web 管理端 |
| `csql-bootstrap` | 初始化系统身份与内置类型目录 |

各组件默认使用同一份 `settings.toml`。需要临时使用其他配置时，可通过 `--config FILE` 指定。

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

CLI 历史只记录完整 SQL 和元命令。包含 `IDENTIFIED` 或 `PASSWORD`（忽略大小写）的输入不记录，多行口令输入和未完成语句也不会逐行写入历史；包含这些文本的普通字符串或标识符同样会被排除。已有历史文件不会自动清理。含这些文本的 SQL 解析错误会隐藏输入原文。

示例：

```sh
csql -d sales -e "SELECT * FROM orders LIMIT 5"
csql -f json -e "SELECT count(*) FROM orders"
```

在脚本里非交互使用（配合 `-e`）：

- 口令从标准输入读取，兼容 LF、CRLF 和无行尾；保留口令中的空格。
- 管道输入时口令提示和错误信息写在标准错误，标准输出保留查询结果，失败时退出码为 `1`。

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

## chusql-web

基本用法：

```sh
chusql-web [options]
```

常用选项：

| 选项 | 说明 |
| --- | --- |
| `--port N` | 监听端口 |
| `--listen-host H` | 监听地址 |
| `--user NAME` | 管理员账号名 |
| `--config FILE` | 指定配置文件 |
| `-h, --help` | 显示帮助 |

静态资源目录固定为 `static`。其余请求大小、会话、登录限制、分页等参数通常通过 `settings.toml` 配置即可。

## chusql-server

基本用法：

```sh
chusql-server [options]
```

常用选项：

| 选项 | 说明 |
| --- | --- |
| `--port N` | 数据库服务监听端口 |
| `--listen-host H` | 数据库服务监听地址 |
| `--user NAME` | 管理员账号名 |
| `--max-message N` | 单条请求大小上限 |
| `--max-rows N` | 单次查询返回行数上限 |
| `--config FILE` | 指定配置文件 |
| `-h, --help` | 显示帮助 |

`chusql-server` 是数据目录的唯一直接访问者。Web、CLI 及其他客户端均通过数据库服务访问数据。
