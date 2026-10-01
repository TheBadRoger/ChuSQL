# 安装与卸载

发行包里的程序是编好的：**装的人不需要 GHC，也不需要 Rust**。没有对应平台的发行包、或者要改代码时，
走「从源码安装」。

## 三种安装方式

| 方式     | 适用                       | 命令                                                     |
| -------- | -------------------------- | -------------------------------------------------------- |
| 在线安装 | 有网络，装最新发行版       | `curl -fsSL .../scripts/install.sh \| sh -s -- --component web` |
| 包内安装 | 手里已有归档（离线、内网） | 解压后在包根跑 `./install.sh --component web`             |
| 源码安装 | 没有对应平台资源，或要改代码 | `./install.sh --component web --from-source`            |

`--component web` 装 Web 管理端，`--component cli` 装命令行客户端 `csql`；两种都会带上存储进程
`chusql-storage`，因为它是后端。

## 一、发行包

归档由项目在打 tag 时按平台构建上传，名字是 `<component>-<os>-<arch>`，并附 `.sha256`：

| 平台         | 资源名                                                                    |
| ------------ | ------------------------------------------------------------------------- |
| Linux x86_64 | `chusql-web-linux-x86_64.tar.gz`、`chusql-cli-linux-x86_64.tar.gz`        |
| Windows x86_64 | `chusql-web-windows-x86_64.zip`、`chusql-cli-windows-x86_64.zip`        |
| macOS        | `chusql-web-macos-x86_64.tar.gz`、`chusql-web-macos-arm64.tar.gz`（cli 同理） |

### 在线安装

一行命令：脚本自己按 `uname` 挑资源、校验 sha256、解包、释放、写配置、加 PATH，最后清掉临时包：

    curl -fsSL https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.sh \
        | sh -s -- --component web

仓库与版本可以改：

    sh -s -- --component cli --repo OWNER/NAME --version v0.3.0

### 指定归档

不碰网络下载逻辑，直接指一个归档（本地路径、`file://` 或任意 URL）：

    ./install.sh --component web --url dist/chusql-web-linux-x86_64.tar.gz
    ./install.sh --component cli --url https://example.com/chusql-cli-linux-x86_64.tar.gz

### 包内安装

解压发行包后在包根目录直接跑安装脚本，就是最常见的本地安装：

    ./install.sh --component web --user root --password '...'        # Linux / macOS
    ./install.ps1 -Component web -RootUser root -RootPassword '...'   # Windows

Windows 的 `install.ps1` **只支持包内安装**（没有在线 / 源码模式）；要在线装就先手动下载归档。

## 二、安装选项

| `install.sh`            | `install.ps1`    | 默认            | 说明 |
| ----------------------- | ---------------- | --------------- | ---- |
| `--component web\|cli`  | `-Component`     | 必填            | 装哪个前端 |
| `--install-dir DIR`     | `-InstallDir`    | Linux/macOS `$HOME/.local/share/chusql`；Windows `%LOCALAPPDATA%\ChuSQL` | 程序装到哪 |
| `--data-dir DIR`        | `-DataDir`       | `<install-dir>/data` | 数据落哪 |
| `--user NAME`           | `-RootUser`      | `root`          | 管理员账号名 |
| `--password PW`         | `-RootPassword`  | 空              | 管理员口令；留空＝只允许管理员免密登录 |
| `--keep-package`        | `-KeepPackage`   | 否              | 装完不删安装包 |
| `--url URL\|PATH`       | —                | —               | 从这个归档装 |
| `--repo OWNER/NAME`     | —                | `TheBadRoger/ChuSQL` | 从哪个仓库取发行包 |
| `--version TAG`         | —                | `latest`        | 下载模式是 release tag，源码模式是 git ref |
| `--from-source`         | —                | —               | 从源码编译安装 |
| `--source-dir DIR`      | —                | —               | 用本地 checkout 编译，不克隆 |

## 三、从源码安装

需要 `git` + `cargo` + `stack` + `ghc`；Linux 上还要 zlib 开发头（`apt-get install zlib1g-dev`）：

    ./install.sh --component web --from-source                 # 克隆仓库再编
    ./install.sh --component web --from-source --source-dir ../ChuSQL   # 用本地 checkout

编完之后的释放、写配置、加 PATH 与包内安装完全相同，启动方式也一样。

## 四、装完是什么样

以 Linux、`--component web`、默认目录为例：

| 位置 | 内容 |
| ---- | ---- |
| `~/.local/share/chusql/bin/` | `chusql-storage`、`chusql-web` |
| `~/.local/share/chusql/static/` | Web 前端静态资源 |
| `~/.local/share/chusql/csql-web`、`csql-web.sh` | 启动器与命令入口（`cli` 组件则是 `csql`） |
| `~/.local/share/chusql/init.sql` | 初始化脚本 |
| `~/.local/share/chusql/logs/` | 日志 |
| `~/.local/share/chusql/data/` | 数据目录（Windows 是 `%LOCALAPPDATA%\ChuSQL\data`；可在 `[storage] data_dir` 改） |
| `~/.config/ChuSQL/chusql.toml` | 全局配置（Windows：`%APPDATA%\ChuSQL\chusql.toml`） |
| `~/.profile` | 追加一段 `# ChuSQL` 标记 + `PATH` 输出（Windows 改的是用户级 PATH） |

装完开新终端：

    csql-web        # 起 Web 前端，默认 http://127.0.0.1:7777
    csql            # 进命令行

口令留空时只有管理员能进。账号与权限见 [commands.md](commands.md)，配置项见 [config.md](config.md)。

## 五、卸载

没有卸载脚本，手工三步（位置按上面的表）：

    rm -rf ~/.local/share/chusql        # 程序与数据（数据另存过就只删程序目录）
    rm -f  ~/.config/ChuSQL/chusql.toml # 全局配置
    # 再编辑 ~/.profile，删掉 "# ChuSQL" 那三行

Windows：删 `%LOCALAPPDATA%\ChuSQL` 与 `%APPDATA%\ChuSQL\chusql.toml`，再到「环境变量」里把安装目录
从用户 PATH 移除。
