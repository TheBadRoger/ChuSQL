# ChuSQL

<p align="center">
  <img src="https://img.shields.io/badge/Haskell-5e5086?style=flat-square" alt="Haskell">
  <img src="https://img.shields.io/badge/Rust-dea584?style=flat-square" alt="Rust">
  <a href="https://github.com/TheBadRoger/ChuSQL/actions/workflows/build.yml"><img src="https://img.shields.io/github/actions/workflow/status/TheBadRoger/ChuSQL/build.yml?branch=main&amp;label=build&amp;style=flat-square" alt="Build"></a>
  <a href="https://github.com/TheBadRoger/ChuSQL/actions/workflows/linux.yml"><img src="https://img.shields.io/github/actions/workflow/status/TheBadRoger/ChuSQL/linux.yml?branch=main&amp;label=tests&amp;style=flat-square" alt="Tests"></a>
  <a href="https://github.com/TheBadRoger/ChuSQL/releases"><img src="https://img.shields.io/github/v/release/TheBadRoger/ChuSQL?style=flat-square" alt="Release"></a>
  <br>
</p>

ChuSQL 是采用 Haskell SQL 引擎与 Rust 存储层的数据库项目，提供 TCP 服务、CLI 和 Web 管理端。支持查询优化、快照事务、WAL 崩溃恢复、DOMAIN 自定义类型及用户与角色管理。

## 快速开始

### 安装

Linux / macOS：

```sh
curl -fsSL https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.sh | sh -s -- --component web
```

Windows（PowerShell 里下下来再跑；也有下载 zip 解压的包内安装）：

```powershell
irm https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.ps1 -OutFile "$env:TEMP\install.ps1"
& "$env:TEMP\install.ps1" -Component web
```

### 启动

```sh
csql
```

启动CLI界面，或

```sh
csql-web
```

启动web界面，默认通过7778端口访问

## 更多参考文档

- [安装与卸载](docs/install.md)
- [配置选项](docs/config.md)
- [命令与选项](docs/commands.md)
- [SQL 语法](docs/syntax.md)
- [类型与类型目录](docs/types.md)
- [用户、角色与属性](docs/users-and-roles.md)
- [系统引导与恢复](docs/bootstrap.md)
- [架构与组件](docs/architecture.md)
- [性能基准与瓶颈分析](docs/performance.md)
