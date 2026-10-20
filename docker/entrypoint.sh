#!/bin/sh
set -eu

# 通过既有引导程序初始化目录，随后以当前非特权用户启动单个服务。
case "${1:-server}" in
    server)
        if [ "$#" -gt 0 ]; then shift; fi
        config=${CHUSQL_CONFIG:-/etc/chusql/server.toml}
        password_file=${CHUSQL_ADMIN_PASSWORD_FILE:-/run/secrets/admin_password}
        if [ ! -r "$password_file" ]; then
            echo "Cannot read administrator password file: $password_file" >&2
            exit 1
        fi
        password=$(head -n 1 "$password_file")
        case "$password" in
            *[![:space:]]*) ;;
            *)
                echo "Administrator password file must start with a non-blank password." >&2
                exit 1
                ;;
        esac
        printf '%s\n' "$password" | csql-bootstrap --config "$config" \
            --user "${CHUSQL_ADMIN_USER:-root}" --password-stdin
        unset password
        exec chusql-server --config "$config" --user "${CHUSQL_ADMIN_USER:-root}" "$@"
        ;;
    *)
        exec "$@"
        ;;
esac
