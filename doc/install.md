# 安装与卸载

发行包里的程序是编好的：**装的人不需要 GHC，也不需要 Rust**。没有对应平台的发行包、或者要改代码时，
走「从源码安装」。

## 三种安装方式

| 方式     | 适用                       | 命令                                                     |
| -------- | -------------------------- | -------------------------------------------------------- |
| 在线安装 | 有网络，装最新发行版       | `curl -fsSL .../scripts/install.sh \| sh -s -- --component web` |
| 包内安装 | 手里已有归档（离线、内网） | 解压后在包根跑 `./install.sh --component web`             |
| 源码安装 | 没有对应平台资源，或要改代码 | `./install.sh --component web --from-source`            |

Linux / macOS 用 `install.sh`，Windows 用 `install.ps1`；一个平台的发行包里 Web 管理端和命令行
客户端 `csql` 都有。`--component web` / `cli` / `both` 直接指定装哪几个；不写就**逐个问**
（英文提示，回答 `y` / `n`，直接回车取方括号里的默认值）：

    Install the command line client (csql)? [Y/n]
    Install the web front end (browser UI)? [y/N]

默认装 `csql`、不装 Web；可以都装、可以只装一个，也可以两个都不选（那就只装存储库
`libchusql_core_storage.so` / `chusql_core_storage.dll` 和 `chusql-server`）。**没选的组件一个文件都不会释放**。选完再提示设置管理员的初始口令
（`--password` / `-RootPassword` 给了就不问）。没有终端、又在跑脚本（比如 `curl | sh`）时问不了，
那时必须显式给 `--component`。

## 一、发行包

归档由项目在打 tag 时按平台构建上传，名字是 `chusql-<os>-<arch>`，并附 `.sha256`。一个归档里
Web 和 cli 两部分都有，装哪几个由包内脚本当场问：

| 平台           | 资源名                                                     |
| -------------- | ---------------------------------------------------------- |
| Linux x86_64   | `chusql-linux-x86_64.tar.gz`                               |
| Windows x86_64 | `chusql-windows-x86_64.zip`                                |
| macOS          | `chusql-macos-x86_64.tar.gz`、`chusql-macos-arm64.tar.gz`  |

### 在线安装

一行命令：脚本自己按 `uname` 挑资源、校验 sha256、解包、释放、写配置、加 PATH，最后清掉临时包：

    curl -fsSL https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.sh \
        | sh -s -- --component web

仓库与版本可以改：

    sh -s -- --component cli --repo OWNER/NAME --version v0.3.0

`--version` 不写就是 `latest`：脚本先问 GitHub API 要 release 列表，挑**最新的、正式版、且确实带本平台归档**
的那个；列表里一个都不合适就不猜。预发布版（prerelease）不会自动选中——要装就显式写 tag。先看有哪些版本：

    sh -s -- --list-versions

自建站点（GitHub Enterprise、镜像）用环境变量换基地址，取包和查版本都跟着走：

    CHUSQL_GITHUB_BASE=https://git.example.com sh -s -- --component web

查版本用的接口未认证时限制 60 次/小时，超了会自动退回 GitHub 的 `latest` 重定向（还能装，但挑不了"最新正式版"）。
CI 里或装得频繁时给个 token，两个脚本都认（`GITHUB_TOKEN` 也可以）：

    CHUSQL_GITHUB_TOKEN=ghp_xxx sh -s -- --list-versions

Windows 是同一套玩法，`install.ps1` 自己下 zip、校验 sha256、解包：

    irm https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.ps1 -OutFile install.ps1
    .\install.ps1 -Component web
    .\install.ps1 -ListVersions
    .\install.ps1 -Component cli -Repo OWNER/NAME -Version v0.3.0

在线模式只在脚本旁边没有包时才走：`bin\chusql_core_storage.dll` 在脚本旁边就是包内安装，不碰网络。

### 指定归档

不碰网络下载逻辑，直接指一个归档（本地路径、`file://` 或任意 URL）：

    ./install.sh --component web --url dist/chusql-linux-x86_64.tar.gz
    ./install.sh --component cli --url https://example.com/chusql-linux-x86_64.tar.gz

### 包内安装

解压发行包后在包根目录直接跑安装脚本，就是最常见的本地安装：

    ./install.sh --component web --user root --password '...'        # Linux / macOS
    ./install.ps1 -Component web -RootUser root -RootPassword '...'   # Windows

不给 `--component` 就是交互模式，脚本一个个问：

    ./install.sh
    .\install.ps1

Windows 的 `install.ps1` 两种模式都支持（包内 + 在线），只有源码模式没有——要改代码就在仓库里编。

## 二、安装选项

| `install.sh`            | `install.ps1`    | 默认            | 说明 |
| ----------------------- | ---------------- | --------------- | ---- |
| `--component web\|cli\|both` | `-Component` | 逐个问（没有终端时必填） | 装哪个前端 |
| `--interactive`         | `-Interactive`   | 有终端就问      | 强制从标准输入收答案（`curl \| sh`、自动化里用） |
| `--install-dir DIR`     | `-InstallDir`    | Linux/macOS `$HOME/.local/share/chusql`；Windows `%LOCALAPPDATA%\ChuSQL` | 程序装到哪 |
| `--data-dir DIR`        | `-DataDir`       | `<install-dir>/data` | 数据落哪 |
| `--user NAME`           | `-RootUser`      | `root`          | 管理员账号名 |
| `--password PW`         | `-RootPassword`  | 空              | 管理员口令；留空＝只允许管理员免密登录 |
| `--keep-package`        | `-KeepPackage`   | 否              | 装完不删安装包 |
| `--url URL\|PATH`       | —                | —               | 从这个归档装（只有 `install.sh` 有；Windows 直接解压 zip 走包内安装） |
| `--repo OWNER/NAME`     | `-Repo`          | `TheBadRoger/ChuSQL` | 从哪个仓库取发行包 |
| `--version TAG`         | `-Version`       | `latest`        | 下载模式是 release tag（`latest`＝最新的、带本平台包的正式版），源码模式是 git ref |
| `--list-versions`       | `-ListVersions`  | —               | 列出仓库里的发行 tag（预发布带标记），然后退出 |
| `--from-source`         | —                | —               | 从源码编译安装 |
| `--source-dir DIR`      | —                | —               | 用本地 checkout 编译，不克隆 |

## 三、从源码安装

需要 `git` + `cargo` + `stack` + `ghc`；Linux 上还要 zlib 开发头（`apt-get install zlib1g-dev`）：

    ./install.sh --component web --from-source                 # 克隆仓库再编
    ./install.sh --component web --from-source --source-dir ../ChuSQL   # 用本地 checkout

编完之后的释放、写配置、加 PATH 与包内安装完全相同，启动方式也一样。

## 四、装完是什么样

以 Linux、`--component web`、默认目录为例（选了 cli 就多一份 `bin/csql` 和一个 `csql` 命令入口）：

| 位置 | 内容 |
| ---- | ---- |
| `~/.local/share/chusql/bin/` | `libchusql_core_storage.so`（一定装）、`chusql-server`（一定装，TCP 数据库服务）、`chusql-web`（选了 Web 才有）、`csql`（选了 cli 才有） |
| `~/.local/share/chusql/static/` | Web 前端静态资源（选 Web 才有） |
| `~/.local/share/chusql/csql-web`、`csql-web.sh` | 启动器与命令入口（选 Web 才有；cli 则是 `csql`） |
| `~/.local/share/chusql/init.sql` | 初始化脚本 |
| `~/.local/share/chusql/logs/` | 日志 |
| `~/.local/share/chusql/data/` | 数据目录（Windows 是 `%LOCALAPPDATA%\ChuSQL\data`；可在 `[storage] data_dir` 改） |
| `~/.config/ChuSQL/chusql.toml` | 全局配置（Windows：`%APPDATA%\ChuSQL\chusql.toml`） |
| `~/.profile` | 追加一段 `# ChuSQL` 标记 + `PATH` 输出（Windows 改的是用户级 PATH） |

装完开新终端：

    csql-web        # 起 Web 前端，默认 http://127.0.0.1:7778
    csql            # 进命令行
    chusql-server   # 起 TCP 数据库服务，默认 127.0.0.1:7777（客户端连接方式见 commands.md）

口令留空时只有管理员能进。账号与权限见 [commands.md](commands.md)，配置项见 [config.md](config.md)。

## 五、卸载

没有卸载脚本，手工三步（位置按上面的表）：

    rm -rf ~/.local/share/chusql        # 程序与数据（数据另存过就只删程序目录）
    rm -f  ~/.config/ChuSQL/chusql.toml # 全局配置
    # 再编辑 ~/.profile，删掉 "# ChuSQL" 那三行

Windows：删 `%LOCALAPPDATA%\ChuSQL` 与 `%APPDATA%\ChuSQL\chusql.toml`，再到「环境变量」里把安装目录
从用户 PATH 移除。
