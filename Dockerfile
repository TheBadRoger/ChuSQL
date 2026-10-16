# syntax=docker/dockerfile:1

# 分阶段构建存储和四个程序，运行镜像不包含编译器。
ARG DEBIAN_MIRROR=http://mirrors.tencentyun.com
ARG BASE_REGISTRY=docker.io

FROM ${BASE_REGISTRY}/library/rust:1-bookworm@sha256:e12566d5abc17a0da741628782cdb504b68b8961f5bfcf677936bf3e8b32de29 AS storage
ARG CARGO_REGISTRY=sparse+https://rsproxy.cn/index/
ENV CARGO_BUILD_JOBS=1 CARGO_HTTP_TIMEOUT=30
WORKDIR /build/chusql-core/storage
COPY chusql-core/storage/Cargo.toml chusql-core/storage/Cargo.lock ./
COPY chusql-core/storage/src ./src
RUN --mount=type=cache,target=/usr/local/cargo/registry \
    cargo --config 'source.crates-io.replace-with="build-mirror"' \
    --config "source.build-mirror.registry=\"${CARGO_REGISTRY}\"" \
    build --release --locked --lib --jobs 1

FROM ${BASE_REGISTRY}/library/haskell:9.10.3-slim-bookworm@sha256:4f1f62fec9cea4cbb03e885c9ad45642609cadd3897b1e64a7f61d0862b575b1 AS haskell
ARG DEBIAN_MIRROR
RUN sed -i "s|http://deb.debian.org|${DEBIAN_MIRROR}|g" /etc/apt/sources.list.d/debian.sources \
    && apt-get -o Acquire::Retries=2 -o Acquire::http::Timeout=30 update \
    && apt-get -o Acquire::Retries=2 -o Acquire::http::Timeout=30 install -y --no-install-recommends libgmp-dev zlib1g-dev \
    && rm -rf /var/lib/apt/lists/*
ENV LD_LIBRARY_PATH=/build/chusql-core/storage/target/release \
    GHCRTS=-M1536m
WORKDIR /build
COPY --from=storage /build/chusql-core/storage/target/release/libchusql_core_storage.so ./chusql-core/storage/target/release/
COPY chusql-core/model ./chusql-core/model
COPY chusql-core/engine ./chusql-core/engine
COPY chusql-interface ./chusql-interface
COPY chusql-bootstrap ./chusql-bootstrap
COPY chusql-server ./chusql-server
COPY chusql-web ./chusql-web
COPY chusql-cli ./chusql-cli
RUN --mount=type=cache,target=/root/.stack \
    stack --stack-yaml chusql-cli/stack.yaml --system-ghc --no-install-ghc \
    build --fast --no-test --no-bench --jobs 1 --ghc-options=-j1 --copy-bins --local-bin-path /out/bin \
    chusql-server:exe:chusql-server chusql-bootstrap:exe:csql-bootstrap \
    chusql-web:exe:chusql-web chusql-cli:exe:csql

FROM ${BASE_REGISTRY}/library/debian:bookworm-slim@sha256:7c7b2c966bc9ee8cedfeef67e0e279108992c77681fa595db4a9d65c06ccc587 AS runtime
ARG DEBIAN_MIRROR
RUN sed -i "s|http://deb.debian.org|${DEBIAN_MIRROR}|g" /etc/apt/sources.list.d/debian.sources \
    && apt-get -o Acquire::Retries=2 -o Acquire::http::Timeout=30 update \
    && apt-get -o Acquire::Retries=2 -o Acquire::http::Timeout=30 install -y --no-install-recommends \
    ca-certificates libgmp10 libnuma1 libffi8 libtinfo6 zlib1g curl netcat-openbsd \
    && rm -rf /var/lib/apt/lists/* \
    && groupadd --gid 10001 chusql \
    && useradd --uid 10001 --gid chusql --create-home chusql \
    && install -d -o chusql -g chusql /var/lib/chusql /var/log/chusql /opt/chusql/resources
WORKDIR /opt/chusql
COPY --from=haskell /out/bin/ ./bin/
COPY --from=storage /build/chusql-core/storage/target/release/libchusql_core_storage.so ./bin/
COPY chusql-web/static ./static
COPY docker/server.toml docker/web.toml /etc/chusql/
COPY --chmod=755 docker/entrypoint.sh docker/healthcheck.sh /usr/local/bin/
ENV PATH=/opt/chusql/bin:${PATH} \
    LD_LIBRARY_PATH=/opt/chusql/bin \
    LANG=C.UTF-8 \
    CHUSQL_ADMIN_USER=root
USER 10001:10001
EXPOSE 7777 7778
HEALTHCHECK --interval=15s --timeout=5s --start-period=30s --retries=5 \
    CMD ["healthcheck.sh", "server"]
ENTRYPOINT ["entrypoint.sh"]
CMD ["server"]
