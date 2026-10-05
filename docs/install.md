# 安装与卸载

发行包已经包含编译后的程序，普通用户安装时无需安装 GHC 或 Rust 工具链。

## 在线安装

Linux / macOS：

```sh
curl -fsSL https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.sh | sh -s -- --component web
```

Windows PowerShell：

```powershell
irm https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.ps1 -OutFile "$env:TEMP\install.ps1"
& "$env:TEMP\install.ps1" -Component web
```

## 安装方式

ChuSQL 支持三种安装方式：

| 方式 | 适用场景 |
| --- | --- |
| 在线安装 | 有网络，希望安装发行版本 |
| 包内安装 | 已下载发行包，或处于离线环境 |
| 源码安装 | 当前平台没有发行包，或需要自行修改源码 |

安装时可以选择 Web 管理端、命令行客户端，或同时安装两者。数据库服务及核心组件会按需安装。

安装过程中必须为管理员账号设置口令。口令不会以明文写入配置文件。

完成系统目录初始化后，安装程序默认会启动 `chusql-server`；如不希望自动启动，可使用 `--no-start`（Windows 为 `-NoStart`）。

Linux 默认注册 `chusql-server.service` 为当前操作系统用户的 systemd 服务，并启用 linger 和开机启动，无需登录即可运行，异常退出后自动重启。需要可用的 systemd 用户会话；若启用 linger 时权限不足，按安装提示执行 `sudo loginctl enable-linger "$(id -un)"` 后重试。数据库管理员 `--user` 与运行服务的操作系统用户是两个独立身份。

`--no-start` 仅跳过本次启动，仍启用开机启动。非 systemd 环境可使用 `--no-service` 跳过服务注册，此时保留原有后台启动方式。

```sh
systemctl --user status chusql-server.service
systemctl --user restart chusql-server.service
systemctl --user stop chusql-server.service
journalctl --user -u chusql-server.service
```

## 包内安装

解压发行包后，在包根目录执行：

```sh
./install.sh --component web
```

Windows：

```powershell
.\install.ps1 -Component web
```

## 常用安装选项

| Linux / macOS | Windows | 说明 |
| --- | --- | --- |
| `--component web\|cli\|both` | `-Component` | 选择安装组件 |
| `--install-dir DIR` | `-InstallDir` | 指定安装目录 |
| `--data-dir DIR` | `-DataDir` | 指定数据目录 |
| `--user NAME` | `-RootUser` | 指定管理员账号名 |
| `--no-start` | `-NoStart` | 安装后不自动启动服务 |
| `--no-service` | — | 跳过 Linux 服务注册和开机启动 |
| `--version TAG` | `-Version` | 指定发行版本 |
| `--list-versions` | `-ListVersions` | 列出可安装版本 |
| `--from-source` | — | 从源码构建并安装 |

自动化安装时建议通过环境变量提供管理员口令，避免口令出现在 shell 历史中。

## 从源码安装

源码安装需要 `git`、`cargo`、`stack` 和 `ghc`；部分 Linux 环境还需要 zlib 开发包。

```sh
./install.sh --component web --from-source
```

源码构建完成后的配置、初始化和启动流程与发行包安装一致。

## 安装完成后

默认安装包含以下主要内容：

- `chusql-server`：数据库服务；
- `csql`：命令行客户端；
- `chusql-web`：Web 管理端；
- `csql-bootstrap`：系统目录初始化工具；
- `settings.toml`：全局配置文件；
- 数据目录与日志目录。

常用命令：

```sh
chusql-server
csql
csql-web
```

管理员使用安装时设置的口令登录。

## 卸载

Linux / macOS：

```sh
./uninstall.sh --yes
```

Windows：

```powershell
.\uninstall.ps1 -Yes
```

如需保留数据，可使用 `--keep-data`（Windows 为 `-KeepData`）；如需保留配置，可使用 `--keep-config`（Windows 为 `-KeepConfig`）。

Linux 卸载时会停止服务、取消开机启动并移除服务文件。用户的 linger 设置保留，其他用户服务仍可使用它。

## 发布真实性与后续签名方案

当前 SHA256 校验用于检测产物损坏，发布签名尚未实现。密钥管理、可信根、轮换、在线与离线验证的设计见[发布签名与验证](release-signing.md)；设计中的验证器和签名安装流程目前不可用。
