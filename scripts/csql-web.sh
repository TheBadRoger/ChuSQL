#!/bin/sh
# csql-web：只负责启动 Web 前端（先拉起存储进程，再起 Web 服务）。
# 配置走 chusql.toml，两层各读各的分区；这里只取 [web] 的 host/port 拼地址。
set -eu

home_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
bin="$home_dir/bin"

# 和在 Windows 的 csql-web.ps1（Start-Process -WorkingDirectory）保持一致：
# 先切到安装目录，配置里写相对的 static_dir / data_dir 才有确定的解析基准。
cd "$home_dir"

config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/ChuSQL"
config="$config_dir/chusql.toml"
[ -f "$config" ] || { echo "!! config not found: $config" >&2; exit 1; }

web_setting() {
    awk -v key="$1" -v def="$2" '
        /^[[:space:]]*\[/ { section = $0; gsub(/[][[:space:]]/, "", section); next }
        section != "web" { next }
        {
            line = $0
            sub(/#.*/, "", line)
            if (line ~ "^[[:space:]]*" key "[[:space:]]*=") {
                sub(/^[^=]*=[[:space:]]*/, "", line)
                gsub(/^[[:space:]]+|[[:space:]]+$/, "", line)
                gsub(/^"|"$/, "", line)
                print line
                found = 1
                exit
            }
        }
        END { if (!found) print def }
    ' "$config"
}

storage_exe="$bin/chusql-storage"
web_exe="$bin/chusql-web"
[ -x "$storage_exe" ] || { echo "!! storage binary not found in $bin" >&2; exit 1; }
[ -x "$web_exe" ] || { echo "!! web binary not found in $bin" >&2; exit 1; }

host=$(web_setting host 127.0.0.1)
port=$(web_setting port 7777)
log_dir="$home_dir/logs"
mkdir -p "$log_dir"

"$storage_exe" >>"$log_dir/storage.log" 2>>"$log_dir/storage.err.log" &
storage_pid=$!
"$web_exe" >>"$log_dir/web.log" 2>>"$log_dir/web.err.log" &
web_pid=$!

cleanup() {
    kill "$web_pid" "$storage_pid" 2>/dev/null || true
    wait "$web_pid" "$storage_pid" 2>/dev/null || true
    echo stopped.
}
trap cleanup INT TERM EXIT

echo "starting web ..."
echo "ready: http://$host:$port/"
echo "logs:  $log_dir"
wait "$web_pid"
