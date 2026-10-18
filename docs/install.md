# 安装与卸载

[← 返回 README](../README.md)

本页帮助你安装、更新和卸载 ChuSQL，并说明软件包源和系统级安装。发行包内含编译后的程序，解压即可安装。

项目仓库：[GitHub](https://github.com/TheBadRoger/ChuSQL) · [ChuRepo](https://repo.chunana.cn/ChuNa7a/ChuSQL)。使用容器部署时，按 [Docker 部署](docker.md) 创建口令文件并启动 Compose。

本页内容：

- [在线安装](#在线安装)
- [apt 软件源](#apt-软件源)
- [安装方式](#安装方式)
- [通过软件包源下载](#通过软件包源下载)
- [包内安装](#包内安装)
- [常用安装选项](#常用安装选项)
- [从源码安装](#从源码安装)
- [安装完成后](#安装完成后)
- [更新](#更新)
- [卸载](#卸载)
- [发布校验](#发布校验)

## 在线安装

Linux / macOS：

```sh
(curl -fsSL --connect-timeout 10 \
  https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.sh -o install.sh ||
 curl -fsSL --connect-timeout 10 \
  https://repo.chunana.cn/ChuNa7a/ChuSQL/raw/branch/main/scripts/install.sh -o install.sh) &&
sh install.sh --component both
```

Windows PowerShell：

```powershell
try {
    irm https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.ps1 -OutFile "$env:TEMP\install.ps1" -TimeoutSec 60 -ErrorAction Stop
} catch {
    irm https://repo.chunana.cn/ChuNa7a/ChuSQL/raw/branch/main/scripts/install.ps1 -OutFile "$env:TEMP\install.ps1" -TimeoutSec 60 -ErrorAction Stop
}
& "$env:TEMP\install.ps1" -Component both
```

命令先请求 GitHub，失败时改用 ChuRepo；安装和更新时下载发行包同样回退。`--component` 选择安装内容：`web` 安装 Web 管理端，`cli` 安装命令行客户端，`both` 同时安装。安装过程提示设置管理员口令。

## apt 软件源

ChuRepo 提供 Debian 软件包仓库，适用于 Debian、Ubuntu 及其衍生发行版。安装包把程序放到 `/usr/lib/chusql`，配置为 `/etc/chusql/settings.toml`，数据和日志分别在 `/var/lib/chusql`、`/var/log/chusql`，并注册系统级 `chusql-server.service`。

```sh
sudo install -d -m 0755 /etc/apt/keyrings
sudo curl -fsSL https://repo.chunana.cn/api/packages/ChuNa7a/debian/repository.key \
  -o /etc/apt/keyrings/gitea-chuna7a.asc
echo 'deb [signed-by=/etc/apt/keyrings/gitea-chuna7a.asc] https://repo.chunana.cn/api/packages/ChuNa7a/debian stable main' \
  | sudo tee /etc/apt/sources.list.d/chusql.list
sudo apt update
sudo apt install chusql
```

首次安装创建 `chusql` 服务账户，并生成管理员 `root` 的随机口令，保存在 `/etc/chusql/admin-password`（权限 `0600`）。用该口令登录，执行 `ALTER USER root PASSWORD '...'` 修改口令，然后删除该文件：

```sh
sudo cat /etc/chusql/admin-password
csql -u root
```

升级用 `sudo apt install --only-upgrade chusql`。卸载用 `sudo apt remove chusql`，保留数据与配置；`sudo apt purge chusql` 同时删除 `/var/lib/chusql`、`/var/log/chusql`、`/etc/chusql` 和服务账户。

## 安装方式

ChuSQL 支持五种安装方式：

| 方式 | 适用场景 |
| --- | --- |
| apt 软件源 | Debian 或 Ubuntu，希望系统级安装并用 `apt` 升级 |
| 在线安装 | 有网络，希望安装发行版本 |
| 软件包源下载 | 需要固定版本或脚本化下载，或从 ChuRepo 获取 |
| 包内安装 | 已下载发行包，或处于离线环境 |
| 源码安装 | 当前平台没有发行包，或需要自行修改源码 |

安装时选择组件。安装程序初始化系统目录，并为管理员账号设置口令；初始化完成后默认启动 `chusql-server`，需要稍后启动时用 `--no-start`（Windows 为 `-NoStart`）。

Linux 以 root 运行安装脚本时采用系统级安装：程序在 `/usr/lib/chusql`，配置为 `/etc/chusql/settings.toml`，数据与日志在 `/var/lib/chusql`、`/var/log/chusql`，`csql`、`csql-web`、`chusql-server` 链接到 `/usr/bin`，并注册系统级 `chusql-server.service`。用 `sudo ./install.sh --system --component both` 显式指定；打包和测试时可加 `--prefix DIR`，把同一布局写入 `DIR`。

普通用户安装注册 systemd 用户服务，安装器同时启用开机启动和 linger；需要时按提示执行 `sudo loginctl enable-linger "$(id -un)"`。常用命令：

```sh
systemctl --user status chusql-server.service
systemctl --user restart chusql-server.service
journalctl --user -u chusql-server.service
```

非 systemd 环境用 `--no-service` 跳过服务注册，数据库进程在后台启动。

## 通过软件包源下载

ChuRepo 的通用软件包仓库长期保存每个发行版本的发行包，文件与 GitHub 发行包相同。索引见 [ChuRepo 软件包](https://repo.chunana.cn/ChuNa7a/-/packages/generic/chusql)。

Linux：

```sh
VERSION=26.10.2
BASE="https://repo.chunana.cn/api/packages/ChuNa7a/generic/chusql/$VERSION"
curl -fL --compressed -O "$BASE/chusql-linux-x86_64.tar.gz"
curl -fL --compressed -O "$BASE/chusql-linux-x86_64.tar.gz.sha256"
sha256sum -c chusql-linux-x86_64.tar.gz.sha256
tar -xzf chusql-linux-x86_64.tar.gz
```

Windows：

```powershell
$version = "26.10.2"
$base = "https://repo.chunana.cn/api/packages/ChuNa7a/generic/chusql/$version"
Invoke-WebRequest "$base/chusql-windows-x86_64.zip" -OutFile chusql-windows-x86_64.zip
Invoke-WebRequest "$base/chusql-windows-x86_64.zip.sha256" -OutFile chusql-windows-x86_64.zip.sha256
(Get-FileHash chusql-windows-x86_64.zip -Algorithm SHA256).Hash
Expand-Archive chusql-windows-x86_64.zip
```

`VERSION` 和 `$version` 是不带 `v` 的版本号，例如 `26.10.2`。先比对校验值，再解压；解压后按[包内安装](#包内安装)运行安装器。

## 包内安装

发行包入口：[GitHub Releases](https://github.com/TheBadRoger/ChuSQL/releases) · [ChuRepo Releases](https://repo.chunana.cn/ChuNa7a/ChuSQL/releases) · [ChuRepo 软件包](https://repo.chunana.cn/ChuNa7a/-/packages/generic/chusql)。所选站点缺少对应版本或平台时，改用软件包源或从源码安装。

下载并解压对应平台的发行包后，在包根目录执行：

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
| `--system` | — | 系统级安装（Linux，root 默认启用） |
| `--prefix DIR` | — | 把系统级布局写入 DIR，跳过服务账户与 systemd |
| `--version TAG` | `-Version` | 指定发行版本 |
| `--list-versions` | `-ListVersions` | 列出可安装版本 |
| `--from-source` | — | 从源码构建并安装 |
| `--low-performance-adapted` | `-LowPerformanceAdapted` | 低性能机器沿用较小并发参数，见[配置](config.md#修改配置) |

自动化安装通过 `CHUSQL_ADMIN_PASSWORD` 提供管理员口令，两个平台的安装器都支持。由部署环境注入该变量，安装器通过标准输入交给引导程序。

## 从源码安装

源码安装需要 `git`、`cargo`、`stack` 和 `ghc`；部分 Linux 环境还需要 zlib 开发包。

选择一个源码地址克隆：

```sh
# GitHub
git clone https://github.com/TheBadRoger/ChuSQL.git
# 或 ChuRepo
git clone https://repo.chunana.cn/ChuNa7a/ChuSQL.git
```

Linux 进入仓库后使用本地源码构建：

```sh
cd ChuSQL
sh scripts/install.sh --component web --from-source --source-dir .
```

`--from-source` 面向 Linux；其他平台使用发行包。

## 安装完成后

安装器按所选组件安装 `chusql-server`、`csql`、`chusql-web`、`csql-bootstrap`，并生成 `settings.toml`、数据目录和日志目录。

安装完成后数据库已启动。选择 `cli` 或 `both` 时运行 `csql` 登录；选择 `web` 或 `both` 时运行 `csql-web`，浏览器打开 <http://127.0.0.1:7778>。完整的第一条查询见 [README 的启动步骤](../README.md#启动并验证)。

服务未运行时，Linux 用户服务用 `systemctl --user start chusql-server.service`，系统级安装用 `sudo systemctl start chusql-server.service`，未注册服务的环境直接运行 `chusql-server`。同一数据目录由一个服务进程使用。

系统级安装的配置文件为 `/etc/chusql/settings.toml`，管理员口令在首次安装时写入 `/etc/chusql/admin-password`。

## 更新

重新运行安装脚本，指定相同的安装目录和组件；用 `--version TAG` 选择版本：

```sh
./install.sh --component both --install-dir "$HOME/.local/share/chusql" --version TAG
```

安装器先停止该安装目录中的 Web 和数据库进程，保留 `settings.toml`、数据库和管理员口令，再检查系统目录并重启数据库。数据目录变更需编辑配置并迁移数据。

## 卸载

在安装目录执行卸载脚本：

```sh
./uninstall.sh --yes
```

Windows：

```powershell
.\uninstall.ps1 -Yes
```

`--keep-data`（Windows 为 `-KeepData`）保留数据，`--keep-config`（`-KeepConfig`）保留配置。系统级安装用 `sudo ./uninstall.sh --system`；deb 安装用 `sudo apt remove chusql` 或 `sudo apt purge chusql`。

## 发布校验

> [!IMPORTANT]
> SHA256 用于检测产物损坏；签名与可信根待实现。
