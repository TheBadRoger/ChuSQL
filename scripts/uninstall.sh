#!/bin/sh
set -eu

# ChuSQL 卸载脚本（Linux/macOS）：停服务、删程序与配置、
# 从 .profile 摘掉 PATH 段。数据目录默认一起删。

install_dir=''
data_dir=''
keep_data='no'
keep_config='no'
assume_yes='no'

usage() {
    cat <<'EOF'
Uninstall ChuSQL (Linux/macOS).

Usage: uninstall.sh [options]

  --install-dir DIR   program directory (default: $HOME/.local/share/chusql)
  --data-dir DIR      data directory (default: settings.toml, then XDG data directory)
  --keep-data         keep the data directory, remove programs only
  --keep-config       keep settings.toml
  --yes               do not ask (required when there is no terminal)
  -h, --help          show this help
EOF
}

step() { echo "== $1"; }
note() { echo "   $1"; }
fail() {
    echo "!! $1" >&2
    exit 1
}

# 停掉命令行里带这个路径的进程
stop_by_path() {
    target=$1
    [ -n "$target" ] || return 0
    if [ "$(uname -s)" = Linux ]; then
        for process in /proc/[0-9]*/exe; do
            executable=$(readlink "$process" 2>/dev/null) || continue
            [ "$executable" = "$target" ] || continue
            pid=${process#/proc/}; pid=${pid%/exe}
            kill "$pid" || fail "cannot stop $target ($pid)"
            attempts=0
            while [ -e "$process" ]; do
                attempts=$((attempts + 1))
                [ "$attempts" -lt 100 ] || fail "timed out stopping $target ($pid)"
                sleep 0.1
            done
        done
        return 0
    fi
    pids=$(ps -eo pid=,args= 2>/dev/null |
        awk -v t="$target" '{ pid = $1; $1 = ""; sub(/^ +/, ""); if (index($0, t) == 1) print pid }' || true)
    [ -n "$pids" ] || return 0
    for pid in $pids; do
        kill "$pid" 2>/dev/null || true
    done
    note "stopped: $pids"
    sleep 1
    for pid in $pids; do
        kill -9 "$pid" 2>/dev/null || true
    done
}

# 问一句 y/n，不是 y 就当没同意
ask_yes_no() {
    printf '%s ' "$1" >&2
    answer=''
    IFS= read -r answer || answer=''
    case "$answer" in
        y | Y | yes | YES | Yes) return 0 ;;
        *) return 1 ;;
    esac
}

while [ $# -gt 0 ]; do
    case "$1" in
        -h | --help) usage; exit 0 ;;
        --install-dir) install_dir="${2:-}"; shift 2 ;;
        --data-dir) data_dir="${2:-}"; shift 2 ;;
        --keep-data) keep_data='yes'; shift ;;
        --keep-config) keep_config='yes'; shift ;;
        --yes) assume_yes='yes'; shift ;;
        *) fail "unknown option: $1 (try --help)" ;;
    esac
done

install_dir=${install_dir:-$HOME/.local/share/chusql}
config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/ChuSQL"
config_file="$config_dir/settings.toml"
if [ -z "$data_dir" ] && [ -f "$config_file" ]; then
    data_dir=$(awk '/^[[:space:]]*\[storage\]/ { storage=1; next } /^[[:space:]]*\[/ { storage=0 } storage && /^[[:space:]]*data_dir[[:space:]]*=[[:space:]]*"[^"\\]*"[[:space:]]*(#.*)?$/ { sub(/^[^=]*=[[:space:]]*"/, ""); sub(/"[[:space:]]*(#.*)?$/, ""); print; exit }' "$config_file")
    [ -n "$data_dir" ] || fail 'settings.toml needs an explicit double-quoted storage.data_dir without escapes; pass --data-dir'
    case "$data_dir" in /*) ;; *) data_dir="$install_dir/$data_dir" ;; esac
fi
data_dir=${data_dir:-${XDG_DATA_HOME:-$HOME/.local/share}/chusql/data}
if [ "$(uname -s)" = Linux ]; then
    install_dir=$(realpath -m -- "$install_dir")
    data_dir=$(realpath -m -- "$data_dir")
else
    if [ -d "$install_dir" ]; then install_dir=$(CDPATH= cd -- "$install_dir" && pwd -P); fi
    if [ -d "$data_dir" ]; then data_dir=$(CDPATH= cd -- "$data_dir" && pwd -P); fi
fi
case "$install_dir" in /|"$HOME") fail 'refusing to remove the root or home directory' ;; esac
case "$data_dir" in /|"$HOME") fail 'refusing to remove the root or home data directory' ;; esac
case "$install_dir" in "$data_dir"|"$data_dir"/*) fail 'data directory must not contain the installation' ;; esac
for entry in bin static csql csql-web csql-web.sh uninstall.sh; do
    case "$data_dir" in "$install_dir/$entry"|"$install_dir/$entry"/*) fail 'data directory overlaps installed program files' ;; esac
done
profile="$HOME/.profile"

if [ "$assume_yes" = 'no' ]; then
    echo 'ChuSQL uninstaller'
    echo "  install dir  $install_dir"
    echo "  data dir     $data_dir"
    if [ "$keep_data" = 'yes' ]; then echo '  data         kept'; fi
    if [ -t 0 ]; then
        ask_yes_no 'Remove it? [y/N]' || {
            echo 'nothing was removed'
            exit 0
        }
    else
        fail 'no terminal: rerun with --yes to confirm'
    fi
fi

step 'Stopping the service'
service_file="${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user/chusql-server.service"
if [ "$(uname -s)" = 'Linux' ] && [ -f "$service_file" ]; then
    command -v systemctl >/dev/null 2>&1 || fail 'systemctl is required to remove the service'
    systemctl --user disable --now chusql-server.service || fail 'cannot stop and disable chusql-server.service'
    rm -f "$service_file"
    systemctl --user daemon-reload || fail 'cannot reload systemd units'
fi
stop_by_path "$install_dir/bin/chusql-server"
stop_by_path "$install_dir/bin/chusql-web"

step 'Removing files'
if [ ! -d "$install_dir" ]; then
    note "not found: $install_dir"
elif [ "$keep_data" = 'yes' ]; then
    for entry in "$install_dir"/bin "$install_dir"/static "$install_dir"/csql "$install_dir"/csql-web "$install_dir"/csql-web.sh "$install_dir"/uninstall.sh; do
        [ -e "$entry" ] || continue
        rm -rf "$entry"
    done
    note "kept data: $data_dir"
else
    rm -rf "$install_dir"
    note "removed $install_dir"
fi
if [ "$keep_data" = 'no' ] && [ -d "$data_dir" ]; then rm -rf "$data_dir"; fi

step 'Removing settings.toml'
if [ "$keep_config" = 'yes' ]; then
    note "kept config: $config_file"
else
    rm -f "$config_file"
    rmdir "$config_dir" 2>/dev/null || true
    note "removed config: $config_file"
fi

step 'Removing the PATH entry'
marker='# ChuSQL'
if [ ! -f "$profile" ]; then
    note "no $profile"
elif grep -q "^$marker$" "$profile"; then
    tmp="$profile.chusql-uninstall"
    awk -v m="$marker" '
        skip > 0 { skip--; next }
        $0 == m { skip = 2; next }
        { print }
    ' "$profile" >"$tmp" && mv "$tmp" "$profile"
    note "removed the $marker block from $profile"
else
    note "no $marker block in $profile"
fi

step 'Done'
note 'open a new terminal: the old PATH stays in the current one'
