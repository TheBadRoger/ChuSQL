# ChuSQL

[![Release](docs/badges/release.svg)](https://github.com/TheBadRoger/ChuSQL/releases) [![Commit](docs/badges/commit.svg)](https://github.com/TheBadRoger/ChuSQL/commits/main) ![Haskell + Rust](docs/badges/languages.svg) [![Code lines](docs/badges/lines.svg)](scripts/update-code-lines.ps1)

[![Build](docs/badges/build.svg)](https://github.com/TheBadRoger/ChuSQL/actions/workflows/build.yml) [![Tests](docs/badges/tests.svg)](https://github.com/TheBadRoger/ChuSQL/actions/workflows/linux.yml) [![Gate](docs/badges/gate.svg)](https://github.com/TheBadRoger/ChuSQL/actions/workflows/gate.yml)

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
