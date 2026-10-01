# 架构与组件

ChuSQL 由三个可执行文件组成，共用同一份配置：

| 组件               | 作用                                                    |
| ------------------ | ------------------------------------------------------- |
| `chusql-storage`   | 存储进程：页面与索引、WAL、崩溃恢复                      |
| `chusql-web`       | Web 管理端：REST 接口 + 浏览器里的数据库 IDE             |
| `csql`             | 命令行客户端                                            |

## 数据流

SQL → 前端（`chusql-web` 或 `csql`）→ 本地端点 → `chusql-storage` → 磁盘。

Web 与命令行是两个并列的前端：Web 走 HTTP，命令行直连本地端点，两者共用同一份
[`chusql.toml`](config.md)、同一个管理员账号与同一张账号表——任意一端改的账号与权限，
另一端立刻生效。

## 一次请求经过什么

1. 浏览器或命令行发起 SQL；
2. 解析与语义检查（表/列是否存在、类型是否匹配、权限是否足够）在这里完成，不合格的语句不会落到磁盘；
3. 合格语句通过本地端点交给 `chusql-storage`；
4. 存储进程读写页面与索引，写之前先记 WAL；
5. 结果原路返回。

## 本地端点

前端与存储进程之间只有一个端点，由 `[server] pipe_name` 决定：Windows 是具名管道，
Linux / macOS 是套接字文件（规则见 [config.md](config.md) 的「端点」一节）。

## 数据在哪里

数据都在 `[storage] data_dir` 下。不写这个键就按平台惯例：Windows `%LOCALAPPDATA%\ChuSQL\data`，
Linux / macOS `${XDG_DATA_HOME:-~/.local/share}/chusql/data`。

| 目录                  | 内容                         |
| --------------------- | ---------------------------- |
| `system/`             | 系统库：账号表与角色授权表   |
| `databases/<库名>/`   | 各自建的库                   |

服务启动后只建 `system`，**没有默认工作库**；其余库由你自己 `CREATE DATABASE` 创建。
