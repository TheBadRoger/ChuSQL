# syntax=docker/dockerfile:1

# 从 Gitea 的 Debian 源安装发行版，镜像只含运行时文件，不编译源码。
ARG BASE_REGISTRY=docker.m.daocloud.io
FROM ${BASE_REGISTRY}/library/debian:bookworm-slim@sha256:7c7b2c966bc9ee8cedfeef67e0e279108992c77681fa595db4a9d65c06ccc587

ARG DEBIAN_MIRROR=http://mirrors.aliyun.com
ARG CHUSQL_APT_BASE=https://repo.chunana.cn/api/packages/ChuNa7a/debian
ARG CHUSQL_VERSION=26.10.2
ARG CHUSQL_UID=10001

RUN sed -i "s|http://deb.debian.org|${DEBIAN_MIRROR}|g" /etc/apt/sources.list.d/debian.sources \
    && apt-get -o Acquire::Retries=2 -o Acquire::http::Timeout=30 update \
    && apt-get -o Acquire::Retries=2 -o Acquire::http::Timeout=30 install -y --no-install-recommends \
    ca-certificates curl netcat-openbsd \
    && rm -rf /var/lib/apt/lists/*

RUN install -d -m 0755 /etc/apt/keyrings \
    && curl -fsSL "$CHUSQL_APT_BASE/repository.key" -o /etc/apt/keyrings/gitea-chuna7a.asc \
    && printf 'deb [signed-by=/etc/apt/keyrings/gitea-chuna7a.asc] %s stable main\n' "$CHUSQL_APT_BASE" \
    > /etc/apt/sources.list.d/chusql.list

# 预建固定 UID 的服务账户，deb 安装时直接复用，容器用户保持一致。
RUN groupadd --gid "$CHUSQL_UID" chusql \
    && useradd --uid "$CHUSQL_UID" --gid chusql --home-dir /var/lib/chusql --no-create-home \
    --shell /usr/sbin/nologin chusql

RUN apt-get -o Acquire::Retries=2 -o Acquire::http::Timeout=30 update \
    && apt-get -o Acquire::Retries=2 -o Acquire::http::Timeout=30 install -y --no-install-recommends \
    "chusql=$CHUSQL_VERSION" \
    && rm -rf /var/lib/apt/lists/* /var/lib/chusql/* /var/lib/chusql/.[!.]* /etc/chusql/admin-password

# 启动目录 /opt/chusql 放配置与可写资源。
RUN install -d -o chusql -g chusql /opt/chusql/resources
WORKDIR /opt/chusql
COPY docker/server.toml /etc/chusql/
COPY --chmod=755 docker/entrypoint.sh docker/healthcheck.sh /usr/local/bin/
ENV PATH=/usr/lib/chusql/bin:${PATH} \
    LD_LIBRARY_PATH=/usr/lib/chusql/bin \
    LANG=C.UTF-8 \
    CHUSQL_ADMIN_USER=root
USER 10001:10001
EXPOSE 7777
HEALTHCHECK --interval=15s --timeout=5s --start-period=30s --retries=5 \
    CMD ["healthcheck.sh", "server"]
ENTRYPOINT ["entrypoint.sh"]
CMD ["server"]
