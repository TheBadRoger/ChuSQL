# ChuSQL

本项目是一个简单的数据库项目，服务端采用 Haskell 和 Rust 联合编写

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
- [架构与组件](docs/architecture.md)
