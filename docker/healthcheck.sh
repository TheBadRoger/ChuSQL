#!/bin/sh
set -eu

# 检查 TCP 协议响应，不读取账号口令。
case "${1:-server}" in
    server)
        response=$(printf '{"method":"ping"}\n{"method":"quit"}\n' | nc -w 3 127.0.0.1 7777)
        printf '%s\n' "$response" | grep -Eq '"status"[[:space:]]*:[[:space:]]*"pong"'
        ;;
    *)
        echo "Unknown healthcheck service: $1" >&2
        exit 1
        ;;
esac
