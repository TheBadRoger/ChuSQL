# ChuSQL

<p align="center">
  <img src="https://img.shields.io/badge/Haskell-5e5086?style=flat-square" alt="Haskell">
  <img src="https://img.shields.io/badge/Rust-dea584?style=flat-square" alt="Rust">
  <a href="https://github.com/TheBadRoger/ChuSQL/actions/workflows/build.yml"><img src="https://img.shields.io/github/actions/workflow/status/TheBadRoger/ChuSQL/build.yml?branch=main&amp;label=build&amp;style=flat-square" alt="Build"></a>
  <a href="https://github.com/TheBadRoger/ChuSQL/actions/workflows/linux.yml"><img src="https://img.shields.io/github/actions/workflow/status/TheBadRoger/ChuSQL/linux.yml?branch=main&amp;label=tests&amp;style=flat-square" alt="Tests"></a>
  <a href="https://github.com/TheBadRoger/ChuSQL/releases"><img src="https://img.shields.io/github/v/release/TheBadRoger/ChuSQL?style=flat-square" alt="Release"></a>
  <br>
</p>

ChuSQL 是一个用 Haskell 和 Rust 编写的数据库，提供命令行客户端和 Web 管理界面。本页帮助你安装并启动客户端；SQL 写法和内部设计见文末的文档导航。

你可以用 SQL 查询和修改数据、通过事务提交或回滚修改，以及管理用户与角色。数据库还支持自定义类型（DOMAIN），并通过预写日志（WAL）在崩溃后恢复已提交的数据。

项目仓库：[GitHub](https://github.com/TheBadRoger/ChuSQL) · [ChuRepo](https://repo.chunana.cn/ChuNa7a/ChuSQL)。两处均可获取源码，按网络情况选择。

## 快速开始

按操作系统选择安装命令，再启动命令行客户端或 Web 界面。

### 安装

Linux / macOS：

```sh
(curl -fsSL --connect-timeout 10 \
  https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.sh -o install.sh ||
 curl -fsSL --connect-timeout 10 \
  https://repo.chunana.cn/ChuNa7a/ChuSQL/raw/branch/main/scripts/install.sh -o install.sh) &&
sh install.sh --component web
```

也可从 ChuRepo 下载后执行：

```sh
curl -fsSL \
  https://repo.chunana.cn/ChuNa7a/ChuSQL/raw/branch/main/scripts/install.sh \
  -o install.sh
sh install.sh --component web
```

Windows：在 PowerShell 中下载并执行安装脚本。

```powershell
try {
    irm https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.ps1 -OutFile "$env:TEMP\install.ps1" -TimeoutSec 60 -ErrorAction Stop
} catch {
    irm https://repo.chunana.cn/ChuNa7a/ChuSQL/raw/branch/main/scripts/install.ps1 -OutFile "$env:TEMP\install.ps1" -TimeoutSec 60 -ErrorAction Stop
}
& "$env:TEMP\install.ps1" -Component web
```

ChuRepo 下载方式：

```powershell
irm https://repo.chunana.cn/ChuNa7a/ChuSQL/raw/branch/main/scripts/install.ps1 -OutFile "$env:TEMP\install.ps1"
& "$env:TEMP\install.ps1" -Component web
```

首组命令优先从 GitHub 下载脚本，失败后尝试 ChuRepo。安装脚本在查询版本或下载发行包失败时，也会尝试 ChuRepo，并保持已选定的版本。校验失败时直接报错。

两站均无对应发行包时，使用包内安装或源码安装。显式指定其他仓库或归档 URL 时，不会回退到官方仓库。详细步骤见[安装与卸载](docs/install.md)。

### Docker 部署

已提供 Dockerfile 和 Compose 配置，可启动数据库并按需启用 Web。首次运行先创建管理员口令文件，再构建和启动；步骤与资源限制见 [Docker 部署](docs/docker.md)。

博客接入的数据模型、事务、迁移与验收步骤见[博客业务数据库接入](docs/blog-database.md)。

### 启动

已安装 CLI 时，运行以下命令打开命令行界面：

```sh
csql
```

已安装 Web 组件时，运行以下命令启动 Web 界面：

```sh
csql-web
```

启动后，在浏览器中打开 <http://127.0.0.1:7778>。监听地址和端口的修改方法见[配置选项](docs/config.md)。

## 文档导航

按你要完成的任务选择文档：

| 任务 | 文档 |
| --- | --- |
| 用容器部署和备份 | [Docker 部署](docs/docker.md) |
| 安装、更新或卸载 | [安装与卸载](docs/install.md) |
| 修改端口、路径等设置 | [配置选项](docs/config.md) |
| 查找客户端命令和启动参数 | [命令与选项](docs/commands.md) |
| 编写查询和修改数据 | [SQL 语法](docs/syntax.md) |
| 选择数据类型或定义 DOMAIN | [类型与类型目录](docs/types.md) |
| 管理账号和访问权限 | [用户、角色与属性](docs/users-and-roles.md) |
| 初始化或离线修复数据库 | [系统引导与恢复](docs/bootstrap.md) |
| 理解组件、查询执行和存储机制 | [架构与组件](docs/architecture.md) |
| 查看性能测量及瓶颈 | [性能基准与瓶颈分析](docs/performance.md) |
