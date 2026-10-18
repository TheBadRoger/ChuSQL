# ChuSQL

<p align="center">
  <img src="https://img.shields.io/badge/Haskell-5e5086?style=flat-square" alt="Haskell">
  <img src="https://img.shields.io/badge/Rust-dea584?style=flat-square" alt="Rust">
  <a href="https://github.com/TheBadRoger/ChuSQL/actions/workflows/build.yml"><img src="https://img.shields.io/github/actions/workflow/status/TheBadRoger/ChuSQL/build.yml?branch=main&amp;label=build&amp;style=flat-square" alt="Build"></a>
  <a href="https://github.com/TheBadRoger/ChuSQL/actions/workflows/linux.yml"><img src="https://img.shields.io/github/actions/workflow/status/TheBadRoger/ChuSQL/linux.yml?branch=main&amp;label=tests&amp;style=flat-square" alt="Tests"></a>
  <a href="https://github.com/TheBadRoger/ChuSQL/releases"><img src="https://img.shields.io/github/v/release/TheBadRoger/ChuSQL?style=flat-square" alt="Release"></a>
  <br>
</p>

ChuSQL 是一个用 Haskell 和 Rust 编写的数据库，提供命令行客户端和 Web 管理界面。本页帮助你完成安装、启动并执行第一条查询；SQL 语法和内部设计见文末的文档导航。

你可以用 SQL 查询和修改数据，通过事务提交或回滚修改，并管理用户与角色。数据库支持命名枚举（ENUM）和自定义类型（DOMAIN），并通过预写日志（WAL）在崩溃后恢复已提交的数据。

项目仓库：[GitHub](https://github.com/TheBadRoger/ChuSQL) · [ChuRepo](https://repo.chunana.cn/ChuNa7a/ChuSQL)。

## 快速开始

在 Linux 或 macOS 的终端运行安装脚本：

```sh
curl -fsSL https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.sh -o install.sh &&
sh install.sh --component both
```

脚本安装数据库、命令行客户端和 Web 管理端，中途提示设置管理员口令。Windows、apt 软件源、离线安装包和容器部署见[安装与卸载](docs/install.md)。

### 启动并验证

安装完成后，运行客户端并用管理员口令登录：

```sh
csql
```

登录后执行以下 SQL，创建示例库并验证读写：

```sql
CREATE DATABASE chusql_demo;
USE chusql_demo;
CREATE TABLE notes (id INT PRIMARY KEY, title TEXT);
INSERT INTO notes (id, title) VALUES (1, 'Hello ChuSQL');
SELECT id, title FROM notes ORDER BY id;
```

查询返回一行，`id` 为 `1`，`title` 为 `Hello ChuSQL`。示例库可以保留，也可以用 `DROP DATABASE chusql_demo;` 删除。

安装 Web 管理端时，另开终端启动：

```sh
csql-web
```

浏览器打开 <http://127.0.0.1:7778>。监听地址和端口的修改方法见[配置选项](docs/config.md)。

## 文档导航

按你要完成的任务选择文档：

| 任务 | 文档 |
| --- | --- |
| 用容器部署和备份 | [Docker 部署](docs/docker.md) |
| 安装、更新或卸载 | [安装与卸载](docs/install.md) |
| 修改端口、路径等设置 | [配置选项](docs/config.md) |
| 查找客户端命令和启动参数 | [命令与选项](docs/commands.md) |
| 编写查询和修改数据 | [SQL 语法](docs/syntax.md) |
| 选择数据类型、定义 ENUM 或 DOMAIN | [类型与类型目录](docs/types.md) |
| 管理账号和访问权限 | [用户、角色与属性](docs/users-and-roles.md) |
| 初始化或离线修复数据库 | [系统引导与恢复](docs/bootstrap.md) |
| 理解组件、查询执行和存储机制 | [架构与组件](docs/architecture.md) |
| 查看性能测量及瓶颈 | [性能基准与瓶颈分析](docs/performance.md) |
