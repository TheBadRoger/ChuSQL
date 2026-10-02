# ChuSQL

本项目是一个简单的数据库项目，服务端采用 Haskell 和 Rust 联合编写，采取 B/S 模式，管理客户端运行在浏览器网页上。

优点：

- 轻量：几个可执行文件加两个动态库、一份静态资源，解包即用，没有 Node / Python 之类的运行时依赖
- 部署容易：Linux 与 macOS 一条命令装完并启动，Windows 解压后跑一次安装脚本
- 配置统一：一个 `chusql.toml` 同时管住存储、Web 与命令行
- 账号统一：Web 管理端与命令行共用同一个管理员和同一张账号表

## 安装

Linux / macOS（自动识别当前平台，取对应发行包）：

```sh
curl -fsSL https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.sh | sh -s -- --component web
```

Windows：下载 `chusql-windows-x86_64.zip`，解压后在 PowerShell 里执行

```powershell
.\install.ps1 -Component web
```

不写 `--component` 时脚本会逐个问「装 cli 吗」「装 web 吗」（默认装 `csql`、不装 Web）；只装命令行
客户端就把 `web` 换成 `cli`，两个都要用 `both`。安装位置、目录结构与卸载方式见
[doc/install.md](doc/install.md)。

## 启动

```sh
csql-web
```

浏览器打开 http://127.0.0.1:7778 ，默认账号 `root`，口令是安装时给的那个。服务启动后只有系统库 `system`（里面是账号表），**没有默认工作库**：自己建一个库再用 `USE <库名>` 选中它。
换口令、换端口、改数据目录都在安装时写好的 `chusql.toml` 里。

## 文档

| 文档                              | 内容                                                                     |
| --------------------------------- | ------------------------------------------------------------------------ |
| [安装与卸载](doc/install.md)      | 三种安装方式、全部安装选项、装完的目录结构、从源码安装、卸载             |
| [配置选项](doc/config.md)         | 配置位置与优先级、全部配置键与默认值、数据目录、口令与安全               |
| [命令与选项](doc/commands.md)     | `csql` 与 `chusql-web` 的选项、元命令、账号与角色权限、REST 接口         |
| [架构与组件](doc/architecture.md) | 三个组件怎么分工、一次请求经过什么、数据落在哪                           |
