# 用 Docker 部署 ChuSQL

本页帮助你在 Linux 容器中启动数据库和可选的 Web 管理端，并保留数据、备份及升级。需要 Docker Engine 或启用 Linux 容器的 Docker Desktop，以及 Docker Compose v2 或更新版本。

以下命令在 Linux 或 WSL 的 Bash 中执行。WSL 或小内存机器应先按[受限构建说明](#在-wsl-或内存受限的机器上构建)生成镜像，再启动服务。

默认服务只向宿主机的 127.0.0.1 发布端口。镜像包含数据库、引导程序、CLI 和 Web，服务以 UID/GID 10001 运行。Web 通过 TCP 连接数据库，不直接打开数据库目录。

## 启动数据库

在仓库根目录执行以下步骤。首次构建需要联网下载 Rust、GHC、Stack 和依赖，耗时取决于网络与构建缓存。

1. 创建本机口令文件。目录仅允许当前用户访问；文件需要让容器中的非 root 用户可读。以下交互不将口令写入 shell 历史：

   ```sh
   install -d -m 700 secrets
   bash -c 'read -r -s -p "管理员口令: " password; printf "\n"; printf "%s\n" "$password" > secrets/admin_password'
   chmod 644 secrets/admin_password
   ```

   文件首行必须至少含一个非空白字符，建议使用强口令。不要将口令文件加入版本控制或镜像；仓库的 Git 与 Docker 忽略规则已排除 `secrets/`。

2. 检查配置，然后构建并启动：

   ```sh
   docker compose config --quiet
   docker compose build server
   docker compose up -d --wait server
   ```

   Compose 将文件作为 [Docker secret](https://docs.docker.com/compose/how-tos/use-secrets/) 挂载到数据库容器。启动脚本先执行现有引导程序，再启动数据库；引导失败时不启动服务。已有口令、业务数据和类型目录不会被重复初始化覆盖。

3. 检查状态并打开 CLI：

   ```sh
   docker compose ps
   docker compose logs --tail=100 server
   docker compose exec server csql --config /etc/chusql/web.toml --user root
   ```

   CLI 使用同一容器网络中的 `server:7777`。输入口令后执行 `SHOW DATABASES;`，应看到系统数据库。健康检查通过实际 TCP `ping` 响应判断服务是否可用，不需要管理员口令。

首次初始化的管理员默认为 `root`。修改口令文件不会重设已有账号口令；修改方法见[账号与权限命令](users-and-roles.md)。重启时仍需保留可读的口令文件。

Dockerfile 默认从腾讯云内网 Debian 镜像源 `http://mirrors.tencentyun.com` 安装系统依赖，继续使用 Debian 的软件包签名校验。其他网络环境可用构建参数覆盖，例如 `docker compose build --build-arg DEBIAN_MIRROR=http://deb.debian.org server`；切换该参数不改变运行配置。

Rust 依赖默认通过 rsproxy 下载，保留 `Cargo.lock` 的版本与校验和检查。其他镜像可通过 `--build-arg CARGO_REGISTRY=sparse+https://镜像地址/index/` 指定；该地址须提供 Cargo sparse 索引及其下载配置。

## 启用 Web 管理端

镜像构建完成后，启用 `web` profile：

```sh
docker compose --profile web up -d --wait
```

在浏览器打开 <http://127.0.0.1:7778>，使用数据库账号登录。Web 仅在数据库健康后启动，使用 HTTP 健康接口检查自身状态。Web 容器不挂载管理员口令文件。

停止全部服务但保留数据：

```sh
docker compose --profile web down
```

`down -v` 会删除本部署的命名卷，包括数据库。日常停止和更新不要添加 `-v`。

## 配置端口、目录与资源

以下变量可在执行命令前导出，或放入本机 `.env`。它们不是管理员口令：

| 变量 | 默认值 | 用途 |
| --- | --- | --- |
| `CHUSQL_IMAGE` | `chusql:local` | 运行镜像，可指定验收过的镜像 ID 或仓库 digest |
| `CHUSQL_ADMIN_USER` | `root` | 初始化及启动使用的管理员名 |
| `CHUSQL_ADMIN_PASSWORD_PATH` | `./secrets/admin_password` | 宿主机口令文件路径 |
| `CHUSQL_BIND_ADDRESS` | `127.0.0.1` | 宿主机发布地址 |
| `CHUSQL_TCP_PORT` | `7777` | 宿主机 TCP 端口 |
| `CHUSQL_WEB_PORT` | `7778` | 宿主机 HTTP 端口 |

例如，将 HTTP 端口改为 8080：

```sh
CHUSQL_WEB_PORT=8080 docker compose --profile web up -d --wait
```

容器内端口保持 7777 和 7778。更改宿主机发布地址可允许远程访问；TCP 和默认 HTTP 都不提供 TLS，应在可信网络使用，或通过 SSH 隧道、VPN、HTTPS 反向代理接入。使用 HTTPS 时在 `docker/web.toml` 中将 `cookie_secure` 设为 `true`。

启动配置分别在 `docker/server.toml` 和 `docker/web.toml`，由 Compose 只读挂载。数据库配置中的 `0.0.0.0` 是监听地址，Web 配置中的 `server` 是数据库连接地址。修改宿主文件后重新创建对应服务；Web 页面不能保存只读的启动配置。其余参数见[配置选项](config.md)，已有数据的页大小和 B+Tree 阶数不能随意更改。

Compose 创建以下命名卷，实际名称带项目名前缀，默认是 `chusql_`：

| 卷 | 容器路径 | 内容 |
| --- | --- | --- |
| `data` | `/var/lib/chusql` | 系统目录、业务库、索引、WAL 与检查点 |
| `logs` | `/var/log/chusql` | 存储诊断日志 |
| `web_settings` | `/opt/chusql/resources` | Web 编辑器偏好 |

数据库目录只允许一个 server 使用，不要扩容 server 副本或让另一进程同时打开此卷。若改为宿主目录绑定，先为 UID/GID 10001 设置写权限；命名卷会使用镜像目录的所有者。

从运行 UID 不同的旧镜像迁移时，已有卷的所有者不会自动变化。例如旧 ChuBlog 数据库镜像使用 UID/GID 10003。先停止所有访问该目录的服务并完成备份，确认 Compose 挂载的是要迁移的卷，再用新镜像调整所有者：

```sh
docker compose run --rm -T --no-deps --user 0:0 --entrypoint chown server \
  -R 10001:10001 /var/lib/chusql /var/log/chusql
```

随后启动服务，使用原口令检查原数据及新增写入。只在迁移所有者时使用 root，正常服务继续以 UID/GID 10001 运行。

每个运行中的服务默认限制为 1 核、512 MiB、128 个进程或线程，不使用额外交换空间。大数据量、整库快照事务或大型查询可能需要在 `compose.yaml` 中提高 `mem_limit` 和 `memswap_limit`；达到内存限制可能使容器被终止，重启后由 WAL 恢复。

### 在 WSL 或内存受限的机器上构建

运行容器的限制不限制镜像构建。Dockerfile 已将 Cargo 和 Stack 构建并发固定为 1，GHC 使用单个编译任务，并将 Haskell 构建进程的堆上限设为 1536 MiB。链接器、Rust 编译器、Stack 和 Docker 仍有额外内存开销；应为构建环境至少预留 4 GiB 可用内存并关闭其他大型构建。

需要在本机构建时，可使用 Docker Buildx 的[受限容器构建器](https://docs.docker.com/build/builders/drivers/docker-container/)。以下创建命令只需执行一次，后续复用同名构建器：

```sh
docker buildx create --name chusql-limited --driver docker-container \
  --driver-opt memory=3g,memory-swap=3g,cpu-period=100000,cpu-quota=100000
docker buildx build --builder chusql-limited --load -t chusql:local .
docker buildx stop chusql-limited
```

构建器限制为 1 核、3 GiB 内存且不使用额外交换空间。达到限制会导致构建失败；不要为完成构建而在内存紧张的 WSL 中取消限制。这里生成的标签与 Compose 一致，可直接执行 `docker compose up -d --wait server`。

若 WSL 曾因资源不足退出，优先在构建服务器上生成镜像，再导入部署机器。不要同时构建多个镜像或运行全量测试。构建机使用：

```sh
docker build -t chusql:local .
docker save chusql:local -o chusql-image.tar
```

将 `chusql-image.tar` 复制到部署机器后执行：

```sh
docker load -i chusql-image.tar
docker compose --profile web up -d --wait
```

镜像没有预先发布到公共仓库，`chusql:local` 是本地构建的标签。首次构建若下载失败，应检查 Docker 镜像源和依赖站点访问；已有宿主 Stack/Cargo 目录不会被发送到构建上下文。

容器构建器不自动继承 Docker Engine 的镜像源。若官方仓库不可达，按本机网络配置创建 BuildKit TOML 文件，并在 `docker buildx create` 时通过 `--buildkitd-config <文件路径>` 指定；在 `[registry."docker.io"]` 中设置可信 `mirrors`，在 `[worker.oci]` 中设置 `max-parallelism = 1`。切换镜像源时保留上述 CPU 与内存限制。

若代理在镜像源模式下返回错误，但直接访问正常，可添加 `--build-arg BASE_REGISTRY=docker.m.daocloud.io`，显式选择提供 `library/` 路径的可信 Docker Hub 代理。三个基础镜像均固定 digest，替换地址不会接受内容不同的镜像；更新基础镜像时须在 Dockerfile 中更新 digest 并重新验收。

## 验证部署镜像

构建完成后，在仓库根目录运行以下命令，需要 Python 3。验收使用随机命名的独立容器和数据卷，不连接已有部署；每个数据库容器限制为 1 核、512 MiB，结束后删除本次创建的容器和卷。

```sh
sh docker/test-entrypoint.sh
python3 docker/test-runtime.py --image chusql:local --seconds 60
```

脚本验证协议登录、账号与授权、事务冲突和回滚、持续关联写入、SIGKILL 后恢复，以及停机备份向空卷恢复。磁盘满场景只填满容器内 16 MiB 的临时文件系统，将测试库的检查点临时文件指向该文件系统，验证 WAL 提交后出现真实 ENOSPC 时封闭业务请求、保留日志并在重启时重放；不会填满宿主磁盘。只有明确返回事务冲突时才回滚并尝试新事务，断线后不自动重试结果未知的写入。

默认负载为 60 秒，仅用于回归验收，不能代替生产长期稳定性验证。可提高 `--seconds` 执行更长验证；测试不等同于真实断电，也不证明任意损坏页可恢复。

通过验收后固定运行镜像，避免后续构建覆盖标签：

```sh
export CHUSQL_IMAGE=$(docker image inspect chusql:local --format '{{.Id}}')
docker compose --profile web up -d --wait --no-build
```

将 `CHUSQL_IMAGE` 的值与源码版本、验收输出、部署配置及备份一起保留。镜像 ID 适用于本地导入的同一镜像；发布到镜像仓库后可改用 `仓库名@sha256:摘要`。更新时重新构建和验收，再切换该变量。

## 备份与恢复

先停止全部服务，确认数据库不再运行，再归档数据卷：

```sh
docker compose --profile web stop
docker compose ps --status running
docker compose run --rm -T --no-deps --entrypoint tar server \
  -C /var/lib/chusql -czf - . > chusql-data.tar.gz
docker compose --profile web up -d --wait
```

同时保留当前镜像、部署配置和管理员口令。数据库使用 WAL 和检查点；不要复制运行中的零散数据文件作为一致备份。

恢复时使用新的项目名创建空数据卷，避免旧目录残留文件。先加载与备份匹配的镜像，再执行：

```sh
docker compose -p chusql-restored run --rm -T --no-deps \
  --entrypoint tar server -C /var/lib/chusql -xzf - < chusql-data.tar.gz
docker compose -p chusql-restored up -d --wait server
```

原项目必须停止，或先更改宿主端口以避免冲突。使用原账号口令登录并查询业务表，确认恢复结果。离线维护及恢复限制见[系统引导与恢复](bootstrap.md)。

## 更新

更新前完成停机备份并保留原镜像。使用目标版本源码构建候选镜像，验收后切换运行镜像。以下沿用前文创建的受限构建器；网络环境需要的构建参数也应保留：

```sh
docker buildx build --builder chusql-limited --load -t chusql:candidate .
python3 docker/test-runtime.py --image chusql:candidate --seconds 60
export CHUSQL_IMAGE=$(docker image inspect chusql:candidate --format '{{.Id}}')
docker compose --profile web up -d --wait --no-build
docker compose --profile web logs --tail=100
```

命名卷保留数据，启动引导会保留已有口令。当前源码会为页文件增加 `.pages`、`.page-wal` 和可能残留的 `.pages.pending`；备份和恢复须复制整个数据目录，不能只选择 `.db`、`.idx` 或 `wal.log`。不要用旧版本程序直接打开已经升级的数据目录；回退需要原镜像和升级前备份。
