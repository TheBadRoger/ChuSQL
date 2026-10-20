# 用 Docker 部署 ChuSQL

[← 返回 README](../README.md)

本页帮助你在 Linux 容器中启动数据库，并完成备份与升级。需要 Docker Engine 或启用 Linux 容器的 Docker Desktop，以及 Docker Compose v2。

镜像基于 `debian:bookworm-slim`，用 apt 从 ChuRepo 的 [Debian 软件源](install.md#apt-软件源)安装发行包。默认基础镜像、系统软件源和 ChuSQL 软件源都指向国内地址，可在构建时覆盖。服务以 UID/GID 10001 运行。

以下命令在 Linux 或 WSL 的 Bash 中执行；默认只向宿主机 127.0.0.1 发布端口。

## 启动数据库

1. 创建本机口令文件，并让容器中的非 root 用户可读：

   ```sh
   install -d -m 700 secrets
   bash -c 'read -r -s -p "管理员口令: " password; printf "\n"; printf "%s\n" "$password" > secrets/admin_password'
   chmod 644 secrets/admin_password
   ```

   文件首行至少包含一个非空白字符。口令文件保存在宿主机的 `secrets/`，Git 与 Docker 的忽略规则已排除该目录。

2. 检查配置，构建并启动：

   ```sh
   docker compose config --quiet
   docker compose build server
   docker compose up -d --wait server
   ```

   Compose 把口令文件作为 [Docker secret](https://docs.docker.com/compose/how-tos/use-secrets/) 挂载到数据库容器。启动脚本先运行引导程序，再启动数据库；重复初始化保留已有口令、数据和类型目录。

3. 检查状态并打开客户端：

   ```sh
   docker compose ps
   docker compose logs --tail=100 server
   docker compose exec server csql --user root
   ```

   容器内的 `csql` 读取 `/etc/chusql/settings.toml`，数据目录为 `/var/lib/chusql`。登录后执行 `SHOW DATABASES;` 查看数据库。健康检查通过实际 TCP `ping` 响应判断服务是否可用。

首次初始化的管理员为 `root`，修改口令的方法见[账号与权限命令](users-and-roles.md)。

构建参数如下：

| 构建参数 | 默认值 | 用途 |
| --- | --- | --- |
| `BASE_REGISTRY` | `docker.m.daocloud.io` | 基础镜像仓库，国内加速地址 |
| `DEBIAN_MIRROR` | `http://mirrors.aliyun.com` | Debian 系统软件源 |
| `CHUSQL_APT_BASE` | `https://repo.chunana.cn/api/packages/ChuNa7a/debian` | ChuSQL 的 apt 软件源 |
| `CHUSQL_VERSION` | `26.10.2` | 安装的软件包版本，不带 `v` |

停止数据库时运行 `docker compose down`，数据卷会保留。`down -v` 同时删除命名卷和数据库。

## 配置端口、目录与资源

以下变量可在执行命令前导出，或放入本机 `.env`：

| 变量 | 默认值 | 用途 |
| --- | --- | --- |
| `CHUSQL_IMAGE` | `chusql:local` | 运行镜像，可指定验收过的镜像 ID 或仓库 digest |
| `CHUSQL_VERSION` | `26.10.2` | 构建时安装的软件包版本 |
| `CHUSQL_ADMIN_USER` | `root` | 初始化及启动使用的管理员名 |
| `CHUSQL_ADMIN_PASSWORD_PATH` | `./secrets/admin_password` | 宿主机口令文件路径 |
| `CHUSQL_BIND_ADDRESS` | `127.0.0.1` | 宿主机发布地址 |
| `CHUSQL_TCP_PORT` | `7777` | 宿主机 TCP 端口 |

容器内 TCP 端口保持 7777。远程接入使用可信网络、SSH 隧道或 VPN。

启动配置在 `docker/server.toml`，由 Compose 只读挂载。修改宿主文件后重新创建服务；其余参数见[配置选项](config.md)，已有数据的页大小和 B+Tree 阶数保持原值。

Compose 创建以下命名卷，实际名称带项目名前缀，默认是 `chusql_`：

| 卷 | 容器路径 | 内容 |
| --- | --- | --- |
| `data` | `/var/lib/chusql` | 系统目录、业务库、索引、WAL 与检查点 |
| `logs` | `/var/log/chusql` | 存储诊断日志 |

使用宿主目录绑定卷时，先为 UID/GID 10001 设置写权限。从运行 UID 不同的旧镜像迁移卷时，停止服务并完成备份后调整所有者：

```sh
docker compose run --rm -T --no-deps --user 0:0 --entrypoint chown server \
  -R 10001:10001 /var/lib/chusql /var/log/chusql
```

每个服务默认限制为 1 核、512 MiB 内存和 128 个进程或线程。大数据量、长事务或大型查询可在 `compose.yaml` 中提高 `mem_limit` 和 `memswap_limit`；达到内存限制时容器被终止，重启后由 WAL 恢复已提交的数据。

## 验收与更新

候选镜像先构建并验收，再投入运行。验收需要 Python 3，会使用独立的容器和数据卷，结束后自动清理：

```sh
docker compose build server
sh docker/test-entrypoint.sh
python3 docker/test-runtime.py --image chusql:local --seconds 60
export CHUSQL_IMAGE=$(docker image inspect chusql:local --format '{{.Id}}')
docker compose up -d --wait --no-build
```

`--seconds` 指定负载时长，回归验收默认 60 秒。把镜像 ID 与源码版本、部署配置和备份一起保留；发布到镜像仓库后可改用 `仓库名@sha256:摘要`。

更新时指定新的 `CHUSQL_VERSION`，构建并验收后按上述方式切换镜像。

命名卷保留数据，升级后的引导会保留已有口令。版本回退使用原镜像和升级前的备份。

## 备份与恢复

先停止全部服务并确认数据库进程退出，再归档数据卷：

```sh
docker compose stop
docker compose ps --status running
docker compose run --rm -T --no-deps --entrypoint tar server \
  -C /var/lib/chusql -czf - . > chusql-data.tar.gz
docker compose up -d --wait
```

备份时同时保留当前镜像、部署配置和管理员口令。数据库使用 WAL 和检查点，一致备份在停机后复制完整数据目录。

恢复到新的项目名和空数据卷：

```sh
docker compose -p chusql-restored run --rm -T --no-deps \
  --entrypoint tar server -C /var/lib/chusql -xzf - < chusql-data.tar.gz
docker compose -p chusql-restored up -d --wait server
```

原项目先停止，或改用其他宿主端口。用原账号口令登录并查询业务表，确认恢复结果。离线维护及恢复限制见[系统引导与恢复](bootstrap.md)。
