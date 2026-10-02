#!/bin/sh
set -eu

# csql-web：先起独占数据目录的 server，再起 Web 服务，配置读 chusql.toml。

home_dir=$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)
bin="$home_dir/bin"

# 和在 Windows 的 csql-web.ps1（Start-Process -WorkingDirectory）保持一致：
# 先切到安装目录，配置里写相对的 static_dir / data_dir 才有确定的解析基准。
cd "$home_dir"

config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/ChuSQL"
config="$config_dir/chusql.toml"
[ -f "$config" ] || { echo "!! config not found: $config" >&2; exit 1; }

# 从配置里取一个 [web] 段的值。
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

server_exe="$bin/chusql-server"
web_exe="$bin/chusql-web"
[ -x "$server_exe" ] || { echo "!! server binary not found in $bin" >&2; exit 1; }
[ -x "$web_exe" ] || { echo "!! web binary not found in $bin" >&2; exit 1; }

host=$(web_setting host 127.0.0.1)
port=$(web_setting port 7778)
log_dir="$home_dir/logs"
mkdir -p "$log_dir"

# server 先起来：它进程内装入存储库、独占数据目录，Web 的存储请求都经它转发
"$server_exe" --config "$config" >>"$log_dir/server.log" 2>>"$log_dir/server.err.log" &
server_pid=$!
"$web_exe" --config "$config" >>"$log_dir/web.log" 2>>"$log_dir/web.err.log" &
web_pid=$!

# 退出时杀掉 web 与 server 进程。
cleanup() {
    kill "$web_pid" "$server_pid" 2>/dev/null || true
    wait "$web_pid" "$server_pid" 2>/dev/null || true
    echo stopped.
}
trap cleanup INT TERM EXIT

echo "starting web ..."
echo "ready: http://$host:$port/"
echo "logs:  $log_dir"
wait "$web_pid"
