# 架构与组件

ChuSQL 由三个面向用户的程序、几个共享库与一份配置组成：

| 组件                            | 作用                                                                 |
| ------------------------------- | -------------------------------------------------------------------- |
| `chusql-server`                 | 数据库服务：进程内装入引擎与存储动态库、独占数据目录，对外只开一个 TCP 端口 |
| `chusql-web`                    | Web 管理端：浏览器里的数据库 IDE，自己开 HTTP 端口，后端经 TCP 连 `chusql-server` |
| `csql`                          | 命令行客户端，经 TCP 连 `chusql-server`                              |
| `chusql-core/engine`（库）      | SQL 解析、语义检查、优化与执行，链进 `chusql-server`                  |
| `chusql-core/storage`（动态库） | 存储：页面与索引、WAL、崩溃恢复                                      |
| `chusql-interface`（库）        | 接口层：线上协议、配置与设置、口令与限流、语句构造、TCP 客户端与会话，`chusql-web` 与 `csql` 共用 |
| `chusql-core/model`（库）       | 共享类型：表与列、结果集、权限枚举                                    |

安装时把可执行文件与存储动态库（`libchusql_core_storage.so` / `chusql_core_storage.dll`）放进同一个目录，
进程按 exe 所在目录就能找到库。

## 数据流

浏览器、`csql` 或别的程序 → TCP → `chusql-server` → 同一进程里调 `chusql-core/engine`
（经 FFI 再调 `chusql-core/storage` 动态库）→ 磁盘。

**数据目录只有 `chusql-server` 一个进程碰**：`chusql-web` 与 `csql` 自己不开存储，它们把 SQL 与数据字典请求
交给 TCP 服务执行，拿到结果再渲染。因此库文件同一时刻只有一个持有者；进程内的并发先用一把锁串起来，
完整事务与 MVCC 见后续计划。

`chusql-web` 是「自己开 HTTP 服务、同时又是 `chusql-server` 的 TCP 客户端」：浏览器只跟 Web 说话，
Web 后端在需要认证、鉴权、读数据或改数据的时候，才把请求发给 `chusql-server`。

三种用法共用同一份 [`chusql.toml`](config.md)、同一个管理员账号与同一张账号表——任意一端改的账号与权限，
另外两端立刻生效。

## 一次请求经过什么

1. 浏览器、命令行或别的程序发起 SQL；
2. 解析与语义检查（表/列是否存在、类型是否匹配、权限是否足够）在 `chusql-server` 里完成，不合格的语句不会落到磁盘；
3. 合格语句经 FFI 交给同一进程里的存储库；
4. 存储库读写页面与索引，写之前先记 WAL；
5. 结果原路返回。

## 数据在哪里

数据都在 `[storage] data_dir` 下。不写这个键就按平台惯例：Windows `%LOCALAPPDATA%\ChuSQL\data`，
Linux / macOS `${XDG_DATA_HOME:-~/.local/share}/chusql/data`。

| 目录                  | 内容                         |
| --------------------- | ---------------------------- |
| `system/`             | 系统库：账号表与角色授权表   |
| `databases/<库名>/`   | 各自建的库                   |

服务启动后只建 `system`，**没有默认工作库**；其余库由你自己 `CREATE DATABASE` 创建。

## TCP 数据库服务（`chusql-server`）

给别的程序用的接口：一条 TCP 连接一条 SQL 会话，请求和应答都是**一行一个 JSON 对象**，编码 UTF-8。

| 请求            | 字段                        | 应答 `status`                                                    |
| --------------- | --------------------------- | ---------------------------------------------------------------- |
| `hello`         | `protocol`（可选，默认 1）  | `hello`：带 `protocol`、`server`                                 |
| `login`         | `user`、`password`          | `ok`：带 `user`、`admin`                                         |
| `query`         | `sql`                       | `result`：带 `columns`、`rows`、`rowCount`、`truncated`、`database` |
| `catalog`       | —                           | `catalog`：带 `schemas`（表与列）                                |
| `databases`     | —                           | `databases`：带库名列表                                          |
| `roles`         | —                           | `roles`：带角色与授权视图                                        |
| `accounts`      | —                           | `accounts`：带账号表（仅管理员）                                 |
| `policy`        | —                           | `policy`：带 `minLength`、`classes`（仅管理员）                  |
| `reload-policy` | —                           | `policy`：重新读设置里的口令策略（仅管理员）                     |
| `storage`       | `request`（存储层的原始请求） | `storage`：带 `response`（存储层的原始应答，仅管理员）          |
| `ping`          | —                           | `pong`                                                           |
| `quit`          | —                           | `bye`，随后连接关闭                                              |

方法名大小写不敏感，认不出的字段忽略。任何一端出错都回 `{"status":"error","code":...,"message":...}`
且**连接不断**，客户端可以接着发下一条；`code` 取值有 `bad_request`（协议号不符、SQL 为空、行太长）、
`unauthorized`（没登录就查询、口令不对）、`forbidden`、`too_many_attempts`（登录失败被限流）、
`too_large`、`no_database`、`not_found`、`query_error`、`conflict`、`storage_error`。

登录之前只能 `hello` / `ping` / `quit`；`storage`、`accounts`、`policy`、`reload-policy` 与系统库只对管理员开放。
每条连接各有一份会话，数据库选择（`USE <库>`）不跨连接。监听参数在 `[server]` 分区（见 [config.md](config.md)），
默认只听本机 `127.0.0.1:7777`。
