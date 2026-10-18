#!/bin/sh
set -eu

# ChuSQL 安装脚本（Linux/macOS）：装 cli/web、写全局配置、
# 改 PATH、初始化系统目录，Linux 注册开机启动服务。

default_repo='TheBadRoger/ChuSQL'

# GitHub 站点：github.com 用 api.github.com；其它（GitHub Enterprise、镜像、本地夹具）
# 按 GHE 的约定走 <base>/api/v3，下载也走 <base>/<owner>/<repo>/releases/download/。
github_base="${CHUSQL_GITHUB_BASE:-https://github.com}"
github_base="${github_base%/}"
case "$github_base" in
    https://github.com|http://github.com) api_base='https://api.github.com' ;;
    *) api_base="$github_base/api/v3" ;;
esac

# 查版本走 API，未认证只有 60 次/小时；给了 token 就带上（CI 里 GITHUB_TOKEN 一般就有）
github_token="${CHUSQL_GITHUB_TOKEN:-${GITHUB_TOKEN:-}}"
churepo_base="${CHUSQL_CHUREPO_BASE:-https://repo.chunana.cn}"
churepo_base="${churepo_base%/}"
churepo_repo='ChuNa7a/ChuSQL'

# 打印用法、选项与环境变量说明。
usage() {
    cat <<'EOF'
usage: ./install.sh [--component web|cli|both] [options]

  --component web|cli|both
                          which parts to install. Leave it out and the installer
                          asks (English, y/n), command line client first:
                            command line client (csql)?   default yes [Y/n]
                            web front end (browser UI)?   default no  [y/N]
                          it then asks for the administrator password; it goes into the
                          system catalog and is never written to settings.toml

  --interactive           ask even when there is no terminal; the answers are
                          read from stdin (one per line)
  --install-dir DIR       where to install   (default: $HOME/.local/share/chusql)
  --data-dir DIR          where to keep data (default: XDG data directory)
  --system                install system-wide: /usr/lib/chusql, /etc/chusql,
                          /var/lib/chusql, /var/log/chusql, a "chusql" service
                          account and a systemd unit; the default when running
                          as root on Linux
  --prefix DIR            with --system: put the same layout under DIR and skip
                          the service account and systemd (packaging and tests)
  --user NAME             administrator name (default: root)
  --keep-package          do not delete the downloaded/extracted package
  --password VALUE        password of the administrator account (required when
                          there is no terminal to ask on; shows up in the shell
                          history, so CHUSQL_ADMIN_PASSWORD is safer)
  --low-performance-adapted
                          apply editable low performance resource limits
                          (default: high performance settings on a fresh install)
  --no-start              do not start the server after installing
  --no-service            skip Linux systemd registration and boot autostart

where the files come from:
  (default)               a local package, else GitHub releases with ChuRepo fallback
  --url URL|PATH          install from this archive (URL, file:// URL or local path)
  --repo OWNER/NAME       GitHub repository (default: TheBadRoger/ChuSQL)
  --version TAG           release tag to download / git ref to build
                          (default: latest = the newest release that has your package)
  --list-versions         list the release tags in the repository, then exit
  --from-source           clone and build locally (needs git, cargo, stack, ghc)
  --source-dir DIR        build from this checkout instead of cloning
  -h, --help              this text

environment:
  CHUSQL_ADMIN_PASSWORD   password of the administrator account, same as
                          --password
  CHUSQL_GITHUB_BASE      GitHub base URL: GitHub Enterprise, a mirror, or a
                          local test fixture (default: https://github.com)
  CHUSQL_CHUREPO_BASE     fallback base URL (default: https://repo.chunana.cn)
                          fallback only applies to the default repository
  CHUSQL_GITHUB_TOKEN     token for the version API (or GITHUB_TOKEN); without
                          one the API allows 60 requests per hour
EOF
}

component=''
install_dir=''
data_dir=''
root_user='root'
keep_package='no'
low_performance_adapted='no'
password_arg=''
no_start='no'
no_service='no'
repo="$default_repo"
version='latest'
url_arg=''
from_source='no'
source_dir=''
list_versions='no'
interactive_flag='no'
system_mode='no'
prefix=''
want_web='no'
want_cli='no'

while [ $# -gt 0 ]; do
    case "$1" in
        --component) component="${2:-}"; shift 2 ;;
        --install-dir) install_dir="${2:-}"; shift 2 ;;
        --data-dir) data_dir="${2:-}"; shift 2 ;;
        --user) root_user="${2:-}"; shift 2 ;;
        --keep-package) keep_package='yes'; shift ;;
        --low-performance-adapted) low_performance_adapted='yes'; shift ;;
        --password) password_arg="${2:-}"; shift 2 ;;
        --no-start) no_start='yes'; shift ;;
        --no-service) no_service='yes'; shift ;;
        --url) url_arg="${2:-}"; shift 2 ;;
        --repo) repo="${2:-}"; shift 2 ;;
        --version) version="${2:-}"; shift 2 ;;
        --list-versions) list_versions='yes'; shift ;;
        --interactive) interactive_flag='yes'; shift ;;
        --system) system_mode='yes'; shift ;;
        --prefix) prefix="${2:-}"; shift 2 ;;
        --from-source) from_source='yes'; shift ;;
        --source-dir) source_dir="${2:-}"; shift 2 ;;
        -h|--help) usage; exit 0 ;;
        *) echo "!! unknown option: $1" >&2; usage >&2; exit 1 ;;
    esac
done

# 打印错误并退出 1。
fail() { echo "!! $1" >&2; exit 1; }
# 打印步骤标题。
step() { echo; echo "==> $1"; }
# 打印缩进提示行。
note() { echo "  $1"; }

# 合并预设中的数值键，保留路径、端口与其它用户设置。
apply_performance_profile() {
    awk '
    function missing(section, i, pair) {
        for (i=1; i<=count; i++) {
            split(order[i], pair, SUBSEP)
            if (pair[1] == section && !seen[order[i]]) {
                print pair[2] " = " values[order[i]]
                seen[order[i]]=1
            }
        }
    }
    FNR == NR {
        if ($0 ~ /^[[:space:]]*\[/) { section=$0; sub(/^[[:space:]]*\[/,"",section); sub(/\].*$/,"",section); if (!known[section]++) sections[++total]=section }
        else if ($0 ~ /^[[:space:]]*[a-z_]+[[:space:]]*=[[:space:]]*[0-9]+[[:space:]]*$/) {
            key=$0; sub(/[[:space:]]*=.*$/,"",key); sub(/^[[:space:]]*/,"",key)
            value=$0; sub(/^.*=[[:space:]]*/,"",value); sub(/[[:space:]]*$/,"",value)
            id=section SUBSEP key; order[++count]=id; values[id]=value
        }
        else if ($0 !~ /^[[:space:]]*(#.*)?$/) { invalid=1; exit 1 }
        next
    }
    /^[[:space:]]*\[/ {
        missing(current); current=$0; sub(/^[[:space:]]*\[/,"",current); sub(/\].*$/,"",current); present[current]=1
    }
    {
        key=$0; sub(/[[:space:]]*=.*$/,"",key); sub(/^[[:space:]]*/,"",key)
        id=current SUBSEP key
        if (id in values) { comment=$0; if (comment ~ /#/) sub(/^[^#]*/,"",comment); else comment=""; print key " = " values[id] (comment == "" ? "" : " " comment); seen[id]=1 }
        else print
    }
    END {
        if (invalid || count == 0) exit 1
        missing(current)
        for (i=1; i<=total; i++) if (!present[sections[i]]) { print "\n[" sections[i] "]"; missing(sections[i]) }
    }' "$1" "$2"
}

# --list-versions 只问仓库有什么版本，不需要 --component
if [ "$list_versions" = 'no' ]; then
    case "$component" in
        web)  want_web='yes' ;;
        cli)  want_cli='yes' ;;
        both) want_web='yes'; want_cli='yes' ;;
        '')   : ;;   # 没给就交互询问（下面），问不了才报错
        *) fail "--component must be web, cli or both, got: $component" ;;
    esac
fi

# ---- 本脚本在哪（管道进来时 $0 是 sh，这时没有本地包）----
self=$0
script_dir=''
case "$self" in
    sh|bash|dash|ash|ksh|'-sh'|*/sh|*/bash|*/dash|*/ash|*/ksh) script_dir='' ;;
    *) script_dir=$(CDPATH= cd -- "$(dirname -- "$self")" 2>/dev/null && pwd) || script_dir='' ;;
esac

# 系统级安装：root 在 Linux 上默认走系统目录；--prefix 把同一布局放进别的根
if [ "$system_mode" = 'no' ] && [ "$list_versions" = 'no' ] && [ "$(id -u)" = '0' ] && [ "$(uname -s)" = 'Linux' ]; then
    system_mode='yes'
fi
while [ "${prefix%/}" != "$prefix" ]; do prefix="${prefix%/}"; done
[ -n "$prefix" ] || prefix='/'
case "$prefix" in
    /*) ;;
    *) prefix="$PWD/$prefix" ;;
esac
if [ "$system_mode" = 'yes' ]; then
    if [ "$(id -u)" != '0' ] && [ "$prefix" = '/' ]; then
        fail 'a system-wide install needs root; pass --prefix DIR to stage the layout elsewhere'
    fi
    service_user="${CHUSQL_SERVICE_USER:-chusql}"
    [ -n "$install_dir" ] || install_dir="$prefix/usr/lib/chusql"
    config_file="$prefix/etc/chusql/settings.toml"
    [ -n "$data_dir" ] || data_dir="$prefix/var/lib/chusql"
    log_dir="$prefix/var/log/chusql"
    service_file="$prefix/etc/systemd/system/chusql-server.service"
else
    [ -n "$install_dir" ] || install_dir="$HOME/.local/share/chusql"
    config_file="${XDG_CONFIG_HOME:-$HOME/.config}/ChuSQL/settings.toml"
fi
if [ "$list_versions" = 'no' ] && [ -f "$config_file" ]; then
    requested_data_dir=$data_dir
    data_dir=$(awk '/^[[:space:]]*\[storage\]/ { storage=1; next } /^[[:space:]]*\[/ { storage=0 } storage && /^[[:space:]]*data_dir[[:space:]]*=[[:space:]]*"[^"\\]*"[[:space:]]*(#.*)?$/ { sub(/^[^=]*=[[:space:]]*"/, ""); sub(/"[[:space:]]*(#.*)?$/, ""); print; exit }' "$config_file")
    [ -n "$data_dir" ] || fail 'existing settings.toml needs an explicit double-quoted storage.data_dir without escapes'
    case "$data_dir" in /*) ;; *) data_dir="$install_dir/$data_dir" ;; esac
    if [ -n "$requested_data_dir" ]; then
        requested_data_dir=$(CDPATH= cd -- "$requested_data_dir" && pwd -P) || fail 'requested data directory does not exist'
        existing_data_dir=$(CDPATH= cd -- "$data_dir" && pwd -P) || fail 'configured data directory does not exist'
        [ "$requested_data_dir" = "$existing_data_dir" ] ||
            fail 'existing settings.toml: edit its data_dir instead of passing a different --data-dir'
    fi
fi
if [ "$system_mode" = 'no' ]; then
    [ -n "$data_dir" ] || data_dir="${XDG_DATA_HOME:-$HOME/.local/share}/chusql/data"
    log_dir="${XDG_STATE_HOME:-$HOME/.local/state}/chusql/logs"
fi

# ---- 交互：先确定答案从哪来 ----
# 管道安装（curl ... | sh -s --）时 stdin 是脚本本身，所以优先开 /dev/tty；
# 没有控制终端时只认 --interactive（答案按行从 stdin 读），否则保持原来的非交互行为。
answers=''
can_prompt='no'
if [ "$interactive_flag" = 'yes' ]; then
    answers='stdin'
    can_prompt='yes'
elif (exec 3</dev/tty) 2>/dev/null; then
    answers='/dev/tty'
    can_prompt='yes'
elif [ -t 0 ]; then
    answers='stdin'
    can_prompt='yes'
fi

read_line() {   # -> $line（读到空行也算成功）
    if [ "$answers" = 'stdin' ]; then
        IFS= read -r line || line=''
    else
        IFS= read -r line < "$answers" || line=''
    fi
}

ask_yes_no() {  # ask_yes_no PROMPT DEFAULT(y|n) -> $reply = yes|no
    while :; do
        printf '%s ' "$1" >&2
        read_line
        case "$line" in
            '') reply="$2" ;;
            [Yy]|[Yy][Ee][Ss]) reply='yes' ;;
            [Nn]|[Nn][Oo]) reply='no' ;;
            *) echo '  please answer y or n' >&2; continue ;;
        esac
        return 0
    done
}

# 读一行口令 -> $line；从终端读时关掉回显（-interactive 从 stdin 读，没法关）。
read_secret() {
    if [ "$answers" = 'stdin' ]; then
        read_line
        return 0
    fi
    _saved=$(stty -g < "$answers" 2>/dev/null) || _saved=''
    stty -echo < "$answers" 2>/dev/null || true
    read_line
    if [ -n "$_saved" ]; then
        stty "$_saved" < "$answers" 2>/dev/null || true
    else
        stty echo < "$answers" 2>/dev/null || true
    fi
    printf '\n' >&2
    return 0
}

if [ "$component" = '' ] && [ "$list_versions" = 'no' ]; then
    if [ "$can_prompt" = 'no' ]; then
        fail '--component is required (web, cli or both) when there is no terminal to ask on'
    fi
    echo
    echo 'ChuSQL installer: which parts do you want?'
    ask_yes_no 'Install the command line client (csql)? [Y/n]' yes
    want_cli="$reply"
    ask_yes_no 'Install the web front end (browser UI)? [y/N]' no
    want_web="$reply"
    if [ "$want_web" = 'no' ] && [ "$want_cli" = 'no' ]; then
        echo '  neither selected: only the storage library and the server get installed' >&2
    fi
fi

# 管理员口令是装机必答项：交互时当场设，非交互时给 --password 或
# CHUSQL_ADMIN_PASSWORD。口令只经 stdin 传给 csql-bootstrap，不落 settings.toml。
admin_password="${CHUSQL_ADMIN_PASSWORD:-}"
if [ "$list_versions" = 'no' ]; then
    if [ -n "$password_arg" ]; then
        admin_password="$password_arg"
    fi
    if [ "$can_prompt" = 'yes' ] && [ -z "$admin_password" ]; then
        echo
        echo "The administrator account is $root_user."
        while :; do
            printf '  Password: ' >&2
            read_secret
            admin_password="$line"
            if [ -z "$admin_password" ]; then
                echo '  the password cannot be empty' >&2
                continue
            fi
            printf '  Repeat it: ' >&2
            read_secret
            if [ "$line" != "$admin_password" ]; then
                echo '  the two entries differ, try again' >&2
                continue
            fi
            break
        done
    fi
    if [ -z "$admin_password" ]; then
        fail 'no administrator password: pass --password PASSWORD or set CHUSQL_ADMIN_PASSWORD'
    fi
fi

# 选中的组件：空格分隔的清单，外加一个显示名（cli 或 web，两个就是 cli+web）
wanted=''
if [ "$want_cli" = 'yes' ]; then wanted='cli'; fi
if [ "$want_web" = 'yes' ]; then
    if [ -n "$wanted" ]; then wanted="$wanted web"; else wanted='web'; fi
fi
component_label=''
for comp in $wanted; do
    if [ -n "$component_label" ]; then component_label="$component_label+$comp"; else component_label="$comp"; fi
done
if [ -z "$component_label" ]; then component_label='none (storage only)'; fi

tmp=''
# 退出时删掉临时目录。
cleanup() { if [ -n "$tmp" ]; then rm -rf "$tmp"; fi; }
# 建临时目录并登记退出清理。
new_tmp() {
    tmp=$(mktemp -d "${TMPDIR:-/tmp}/chusql-install.XXXXXX") || fail 'cannot create a temporary directory'
    # --keep-package 时不注册清理，临时目录就留着
    if [ "$keep_package" = 'no' ]; then trap cleanup EXIT INT TERM HUP; fi
}

# 用 curl 或 wget 把 URL 下载到文件。
download() {
    if command -v curl >/dev/null 2>&1; then
        if [ -n "${3:-}" ]; then
            curl -fsSL --connect-timeout 10 --max-time 300 -H "$3" -H 'Accept: application/vnd.github+json' -o "$2" "$1" || return 1
        else
            curl -fsSL --connect-timeout 10 --max-time 300 -o "$2" "$1" || return 1
        fi
    elif command -v wget >/dev/null 2>&1; then
        if [ -n "${3:-}" ]; then
            wget -q --timeout=30 --tries=1 --header="$3" --header='Accept: application/vnd.github+json' -O "$2" "$1" || return 1
        else
            wget -q --timeout=30 --tries=1 -O "$2" "$1" || return 1
        fi
    else
        fail 'need curl or wget to download the package'
    fi
}

# 校验包的 sha256，没有就跳过。
verify_checksum() {
    sums="$tmp/asset.sha256"
    if ! download "$1.sha256" "$sums" 2>/dev/null; then
        note 'checksum   no .sha256 published for this asset (skipped)'
        return 0
    fi
    expected=$(awk 'NR==1 { print $1 }' "$sums")
    if command -v sha256sum >/dev/null 2>&1; then
        actual=$(sha256sum "$2" | awk '{ print $1 }')
    elif command -v shasum >/dev/null 2>&1; then
        actual=$(shasum -a 256 "$2" | awk '{ print $1 }')
    else
        fail 'no sha256sum/shasum available to verify the checksum'
    fi
    [ "$actual" = "$expected" ] || fail "checksum mismatch: expected $expected, got $actual"
    note "checksum   $expected (ok)"
}

# 按扩展名解包 zip 或 tar.gz。
unpack() {
    mkdir -p "$2"
    case "$1" in
        *.zip)
            command -v unzip >/dev/null 2>&1 || fail 'unzip is required to open a .zip package'
            unzip -q -o "$1" -d "$2"
            ;;
        *)
            tar -xzf "$1" -C "$2"
            ;;
    esac
}

# 探测平台标签（os、arch、ext、labels）。
detect_platform() {
    os=$(uname -s 2>/dev/null || echo unknown)
    case "$os" in
        Linux) os='linux' ;;
        Darwin) os='macos' ;;
        MINGW*|MSYS*|CYGWIN*) os='windows' ;;
    esac
    arch=$(uname -m 2>/dev/null || echo unknown)
    case "$arch" in
        x86_64|amd64) arch='x86_64' ;;
        aarch64|arm64) arch='arm64' ;;
    esac
    if [ "$os" = 'windows' ]; then ext='zip'; else ext='tar.gz'; fi
    labels="$os-$arch $os"
}

# ---- 发行 tag：问 GitHub API（不依赖 jq，用 awk 读 JSON）----
# 请求发行列表，默认仓库失败或无版本时回退。
api_get() {
    if [ -n "$github_token" ]; then
        download "$api_base$1" "$2" "Authorization: Bearer $github_token" &&
            grep -q '"tag_name"' "$2" && return 0
    else
        download "$api_base$1" "$2" && grep -q '"tag_name"' "$2" && return 0
    fi
    [ "$repo" = "$default_repo" ] || return 1
    note 'release API unavailable or empty; trying ChuRepo' >&2
    download "$churepo_base/api/v1/repos/$churepo_repo/releases?limit=100" "$2"
}

# 将紧凑 JSON 按字符串外的分隔符换行。
json_lines() {
    awk '
        {
            for (i = 1; i <= length($0); i++) {
                c = substr($0, i, 1)
                printf "%s", c
                if (quoted) {
                    if (escaped) escaped = 0
                    else if (c == "\\") escaped = 1
                    else if (c == "\"") quoted = 0
                } else if (c == "\"") quoted = 1
                else if (c ~ /[][{},]/) printf "\n"
            }
            printf "\n"
        }
    ' "$1"
}

# 把 release JSON 解析成 tag 与资源行。
parse_releases() {
    json_lines "$1" | awk '
        in_assets {
            if ($0 ~ /^[[:space:]]*\]/) { in_assets = 0; next }
            if ($0 ~ /"name":/) {
                s = $0; sub(/.*"name": *"/, "", s); sub(/".*/, "", s)
                if (s != "") { assets = assets " " s }
            }
            next
        }
        /"assets": *\[\]/ { next }
        /"assets": *\[/ { in_assets = 1; next }
        /"tag_name":/ {
            if (tag != "") { print tag "\t" pre "\t" assets }
            s = $0; sub(/.*"tag_name": *"/, "", s); sub(/".*/, "", s)
            tag = s; pre = 0; assets = ""
            next
        }
        /"prerelease":/ { pre = ($0 ~ /true/) ? 1 : 0; next }
        END { if (tag != "") { print tag "\t" pre "\t" assets } }
    '
}

# 这份 release 里有没有本平台的包。
tag_has_asset() {
    for label in $labels; do
        case " $1 " in
            *" chusql-$label.$ext "*) return 0 ;;
        esac
    done
    case " $1 " in
        *" chusql.$ext "*) return 0 ;;
    esac
    return 1
}

# 从 tags.txt 里挑第一个符合条件的 tag。
pick_tag() {
    [ -s "$tmp/tags.txt" ] || return 1
    while IFS="$(printf '\t')" read -r tag pre assets; do
        [ -n "$tag" ] || continue
        if [ "$1" = 'yes' ] && [ "$pre" = '1' ]; then continue; fi
        if [ "$2" = 'yes' ] && ! tag_has_asset "$assets"; then continue; fi
        printf '%s\n' "$tag"
        return 0
    done < "$tmp/tags.txt"
    return 1
}

# 列出仓库的 release tag 与预发布标记。
list_release_versions() {
    new_tmp
    json="$tmp/releases.json"
    if ! api_get "/repos/$repo/releases?per_page=100" "$json" 2>/dev/null ||
        ! parse_releases "$json" > "$tmp/tags.txt" 2>/dev/null ||
        [ ! -s "$tmp/tags.txt" ]; then
        echo "!! cannot list the releases of $repo from $api_base" >&2
        echo '   (offline, rate limited, or the repository has no release yet)' >&2
        echo "   open $github_base/$repo/releases and pass a tag: --version <tag>" >&2
        exit 1
    fi
    echo "ChuSQL releases in $repo"
    while IFS="$(printf '\t')" read -r tag pre assets; do
        [ -n "$tag" ] || continue
        marker=''
        if [ "$pre" = '1' ]; then marker=' (prerelease)'; fi
        if ! tag_has_asset "$assets"; then
            marker="$marker (no package for $os-$arch)"
        fi
        echo "  $tag$marker"
    done < "$tmp/tags.txt"
    echo
    echo "install one with: ./install.sh --version <tag>"
}

# 把 version=latest 解析成具体 tag。
resolve_version() {
    [ "$version" = 'latest' ] || return 0
    json="$tmp/releases.json"
    if ! api_get "/repos/$repo/releases?per_page=100" "$json" 2>/dev/null; then
        note "version    latest (cannot reach $api_base; using the latest release redirect)"
        return 0
    fi
    parse_releases "$json" > "$tmp/tags.txt" 2>/dev/null || true
    picked=''
    for want_stable in yes no; do
        picked=$(pick_tag "$want_stable" 'yes') && break
        picked=''
    done
    if [ -z "$picked" ]; then
        for want_stable in yes no; do
            picked=$(pick_tag "$want_stable" 'no') && break
            picked=''
        done
    fi
    if [ -n "$picked" ]; then
        version="$picked"
        note "version    $version"
    else
        note 'version    latest (no release lists a matching package; trying the latest release)'
    fi
}

# 下载本平台的发行包归档。
fetch_asset() {
    asset="$1"
    if [ "$version" = 'latest' ]; then
        url="$github_base/$repo/releases/latest/download/$asset"
    else
        url="$github_base/$repo/releases/download/$version/$asset"
    fi
    archive="$tmp/$asset"
    if download "$url" "$archive" 2>/dev/null; then
        note "asset      $asset"
        return 0
    fi
    [ "$repo" = "$default_repo" ] || return 1
    fallback_version="$version"
    if [ "$fallback_version" = 'latest' ]; then
        download "$churepo_base/api/v1/repos/$churepo_repo/releases/latest" "$tmp/latest.json" 2>/dev/null || return 1
        fallback_version=$(parse_releases "$tmp/latest.json" | cut -f1)
        [ -n "$fallback_version" ] || return 1
    fi
    url="$churepo_base/$churepo_repo/releases/download/$fallback_version/$asset"
    note "trying ChuRepo: $asset ($fallback_version)"
    if download "$url" "$archive" 2>/dev/null; then
        version="$fallback_version"
        return 0
    fi
    return 1
}

pack=''
url=''
archive=''

if [ "$list_versions" = 'yes' ]; then
    detect_platform
    list_release_versions
    exit 0
fi

if [ "$from_source" = 'yes' ]; then
    # ---------- 源码安装 ----------
    if [ -z "$source_dir" ]; then
        command -v git >/dev/null 2>&1 || fail 'git is required to clone the repository'
    fi
    command -v cargo >/dev/null 2>&1 || fail 'cargo (Rust) is required for --from-source'
    command -v stack >/dev/null 2>&1 || fail 'stack (Haskell) is required for --from-source'

    new_tmp
    if [ -n "$source_dir" ]; then
        src=$(CDPATH= cd -- "$source_dir" 2>/dev/null && pwd) || fail "no such directory: $source_dir"
        note "source     $src (using this checkout as is)"
    else
        src="$tmp/src"
        if [ "$version" = 'latest' ]; then
            step "Cloning $github_base/$repo.git"
            git clone --depth 1 "$github_base/$repo.git" "$src" >/dev/null 2>&1 ||
                fail "cannot clone $github_base/$repo.git"
        else
            step "Cloning $github_base/$repo.git at $version"
            git clone --depth 1 --branch "$version" "$github_base/$repo.git" "$src" >/dev/null 2>&1 ||
                fail "cannot clone $repo at $version"
        fi
        note "source     $src"
    fi
    [ -f "$src/chusql-core/storage/Cargo.toml" ] || fail "not a ChuSQL checkout (chusql-core/storage/Cargo.toml missing): $src"
    [ -f "$src/resources/settings.toml.linux" ] || fail "not a ChuSQL checkout (resources/settings.toml.linux missing): $src"

    if command -v pkg-config >/dev/null 2>&1 && ! pkg-config --exists zlib 2>/dev/null; then
        note 'hint       the Haskell zlib binding needs the C library: apt-get install zlib1g-dev (or dnf install zlib-devel)'
    fi

    step 'Building the Rust storage (cargo build --release)'
    ( cd "$src/chusql-core/storage" && cargo build --release ) || fail 'cargo build --release failed'
    storage_lib="$src/chusql-core/storage/target/release/libchusql_core_storage.so"
    [ -f "$storage_lib" ] || fail "storage library not found after the build: $storage_lib"

    step 'Staging the package'
    pack="$tmp/stage"
    mkdir -p "$pack/bin" "$pack/resources"
    cp "$storage_lib" "$pack/bin/"
    cp "$src/resources/settings.toml.linux" "$src/resources/settings.low-performance.toml" "$pack/resources/"
    cp "$src/scripts/install.sh" "$src/scripts/uninstall.sh" "$pack/"
    for comp in $wanted; do
        if [ "$comp" = 'web' ]; then
            comp_dir='chusql-web'; front_name='chusql-web'
        else
            comp_dir='chusql-cli'; front_name='csql'
        fi
        step "Building the Haskell binaries (stack build --fast in $comp_dir)"
        note 'the first build downloads the Hackage index (about 139 MB)'
        ( cd "$src/$comp_dir" && stack build --fast ) || fail "stack build failed in $comp_dir"
        stack_root=$( cd "$src/$comp_dir" && stack path --local-install-root 2>/dev/null | tr -d '\r' )
        [ -n "$stack_root" ] || fail 'cannot ask stack for --local-install-root'
        front_bin="$stack_root/bin/$front_name"
        [ -f "$front_bin" ] || fail "front end binary not found after the build: $front_bin"
        cp "$front_bin" "$pack/bin/"
        for extra in "$stack_root/bin/"*.so "$stack_root/bin/"*.dll; do
            if [ -f "$extra" ]; then cp "$extra" "$pack/bin/"; fi
        done
        if [ "$comp" = 'web' ]; then
            [ -d "$src/chusql-web/static" ] || fail "static assets not found: $src/chusql-web/static"
            mkdir -p "$pack/static"
            cp -R "$src/chusql-web/static/." "$pack/static/"
            cp "$src/scripts/csql-web.sh" "$pack/"
        fi
    done

    # TCP 数据库服务必装，跟组件选择无关：它住在 chusql-server 这个 stack 工程里
    step 'Building the TCP database server (stack build --fast chusql-server:exe:chusql-server)'
    ( cd "$src/chusql-server" && stack build --fast chusql-server:exe:chusql-server ) || fail 'stack build failed in chusql-server'
    server_root=$( cd "$src/chusql-server" && stack path --local-install-root 2>/dev/null | tr -d '\r' )
    [ -n "$server_root" ] || fail 'cannot ask stack for --local-install-root'
    [ -f "$server_root/bin/chusql-server" ] || fail "chusql-server not found after the build: $server_root/bin/chusql-server"
    cp "$server_root/bin/chusql-server" "$pack/bin/"

    # 引导程序独立成工程：建 system 目录不依赖服务本身，坏了还能单独重跑
    step 'Building the bootstrap program (stack build --fast chusql-bootstrap:exe:csql-bootstrap)'
    ( cd "$src/chusql-bootstrap" && stack build --fast chusql-bootstrap:exe:csql-bootstrap ) || fail 'stack build failed in chusql-bootstrap'
    bootstrap_root=$( cd "$src/chusql-bootstrap" && stack path --local-install-root 2>/dev/null | tr -d '\r' )
    [ -n "$bootstrap_root" ] || fail 'cannot ask stack for --local-install-root'
    [ -f "$bootstrap_root/bin/csql-bootstrap" ] || fail "csql-bootstrap not found after the build: $bootstrap_root/bin/csql-bootstrap"
    cp "$bootstrap_root/bin/csql-bootstrap" "$pack/bin/"
elif [ -n "$url_arg" ]; then
    # ---------- 指定 URL / 本地归档 ----------
    new_tmp
    archive="$tmp/package"
    case "$url_arg" in
        http://*|https://*|file://*) url="$url_arg" ;;
        *)
            [ -f "$url_arg" ] || fail "no such package: $url_arg"
            url="file://$(CDPATH= cd -- "$(dirname -- "$url_arg")" && pwd)/$(basename -- "$url_arg")"
            ;;
    esac
    step "Downloading $url"
    download "$url" "$archive" || fail "cannot download $url"
    case "$url_arg" in
        http://*|https://*|file://*) verify_checksum "$url" "$archive" ;;
    esac
    step 'Unpacking'
    unpack "$archive" "$tmp/pkg"
    pack="$tmp/pkg"
elif [ -n "$script_dir" ] && ls "$script_dir/bin/"libchusql_core_storage.* >/dev/null 2>&1; then
    # ---------- 包内安装 ----------
    pack="$script_dir"
else
    # ---------- 从 GitHub 发行版下载 ----------
    new_tmp
    detect_platform
    resolve_version
    mkdir -p "$tmp/pkg"
    step "Looking for a ChuSQL package for $os-$arch in $repo"
    found=''
    for label in $labels; do
        if fetch_asset "chusql-$label.$ext"; then
            found="$asset"
            break
        fi
    done
    if [ -z "$found" ]; then
        if fetch_asset "chusql.$ext"; then found="$asset"; fi
    fi
    [ -n "$found" ] || fail "no package for $os-$arch in $repo ($version); try --list-versions, --url or --from-source"
    verify_checksum "$url" "$archive"
    step "Unpacking $found"
    unpack "$archive" "$tmp/pkg"
    pack="$tmp/pkg"
fi

# ---- 校验包内容 ----
ls "$pack/bin/"libchusql_core_storage.* >/dev/null 2>&1 || fail "package is incomplete: bin/libchusql_core_storage.* not found in $pack"
[ -f "$pack/bin/chusql-server" ] || fail "package is incomplete: bin/chusql-server not found in $pack"
[ -f "$pack/bin/csql-bootstrap" ] || fail "package is incomplete: bin/csql-bootstrap not found in $pack"
if [ "$want_web" = 'yes' ]; then
    [ -f "$pack/bin/chusql-web" ] || fail 'package is incomplete: bin/chusql-web not found'
    [ -d "$pack/static" ] || fail 'package is incomplete: static/ not found'
fi
if [ "$want_cli" = 'yes' ]; then
    [ -f "$pack/bin/csql" ] || fail 'package is incomplete: bin/csql not found'
fi
[ -f "$pack/resources/settings.toml.linux" ] || fail 'package is incomplete: resources/settings.toml.linux not found'
if [ "$low_performance_adapted" = 'yes' ]; then
    [ -f "$pack/resources/settings.low-performance.toml" ] || fail 'package is incomplete: resources/settings.low-performance.toml not found'
fi

echo 'ChuSQL installer'
echo "  version    $version"
echo "  components $component_label"
echo "  install to $install_dir"
echo "  data dir   $data_dir"
echo "  root user  $root_user (password asked above, kept out of settings.toml)"

# ---- 释放文件 ----
# 包是合在一起的（web 和 cli 都在），这里按这次的选择逐个释放：没选的组件不落地
use_service='no'
ctl='systemctl'
if [ "$system_mode" = 'yes' ]; then
    if [ "$no_service" = 'no' ] && [ "$prefix" = '/' ]; then
        command -v systemctl >/dev/null 2>&1 || fail 'systemd is required; use --no-service to opt out'
        use_service='yes'
        if [ -f "$service_file" ]; then
            systemctl stop chusql-server.service 2>/dev/null || true
        fi
    elif [ "$prefix" != '/' ]; then
        note '--prefix: systemd registration skipped; register the unit yourself'
    fi
elif [ "$(uname -s)" = 'Linux' ] && [ "$no_service" = 'no' ]; then
    service_file="${XDG_CONFIG_HOME:-$HOME/.config}/systemd/user/chusql-server.service"
    ctl='systemctl --user'
    command -v systemctl >/dev/null 2>&1 || fail 'systemd is required; use --no-service to opt out'
    command -v loginctl >/dev/null 2>&1 || fail 'loginctl is required for boot autostart'
    $ctl show-environment >/dev/null || fail 'cannot connect to user systemd; run in a login session or use --no-service'
    os_user=$(id -un)
    linger=$(loginctl show-user "$os_user" -p Linger --value) || fail 'cannot query boot autostart for this OS user'
    if [ "$linger" != 'yes' ]; then
        loginctl enable-linger "$os_user" || fail 'cannot enable boot autostart; run sudo loginctl enable-linger for this OS user and retry'
    fi
    use_service='yes'
    if [ -f "$service_file" ]; then
        $ctl stop chusql-server.service || fail 'cannot stop the existing service'
    fi
fi
# 停止安装目录中的进程，避免更新运行中的文件。
stop_installed() {
    for executable in chusql-web chusql-server; do
        for process in /proc/[0-9]*/exe; do
            target=$(readlink "$process" 2>/dev/null) || continue
            [ "$target" = "$install_dir/bin/$executable" ] || continue
            pid=${process#/proc/}; pid=${pid%/exe}
            kill "$pid" || fail "cannot stop $executable ($pid)"
            attempts=0
            while [ -e "$process" ]; do
                attempts=$((attempts + 1))
                [ "$attempts" -lt 100 ] || fail "timed out stopping $executable ($pid)"
                sleep 0.1
            done
        done
    done
}
if [ "$system_mode" = 'yes' ]; then
    step 'Preparing the system layout'
    if [ "$prefix" = '/' ] && ! getent group "$service_user" >/dev/null 2>&1; then
        command -v adduser >/dev/null 2>&1 || fail 'adduser is required to create the service account'
        adduser --system --group --no-create-home --home "$data_dir" --shell /usr/sbin/nologin "$service_user" >/dev/null ||
            fail "cannot create the service account $service_user"
    fi
    mkdir -p "$prefix/usr/bin" "$prefix/etc/chusql"
fi
step 'Installing files'
mkdir -p "$install_dir/bin" "$log_dir" "$data_dir"
install_dir=$(CDPATH= cd -- "$install_dir" && pwd -P)
if [ "$(uname -s)" = Linux ]; then stop_installed; fi
data_dir=$(CDPATH= cd -- "$data_dir" && pwd)
log_dir=$(CDPATH= cd -- "$log_dir" && pwd)
cp "$pack/bin/chusql-server" "$install_dir/bin/"
cp "$pack/bin/csql-bootstrap" "$install_dir/bin/"
for lib in "$pack/bin/"libchusql_core_storage.*; do
    if [ -f "$lib" ]; then cp "$lib" "$install_dir/bin/"; fi
done
for lib in "$pack/bin/"*.so "$pack/bin/"*.dylib "$pack/bin/"*.dll; do
    if [ -f "$lib" ]; then cp "$lib" "$install_dir/bin/"; fi
done
if [ "$want_cli" = 'yes' ]; then
    cp "$pack/bin/csql" "$install_dir/bin/"
fi
if [ "$want_web" = 'yes' ]; then
    cp "$pack/bin/chusql-web" "$install_dir/bin/"
    mkdir -p "$install_dir/static"
    cp -R "$pack/static/." "$install_dir/static/"
    if [ -f "$pack/csql-web.sh" ]; then
        cp "$pack/csql-web.sh" "$install_dir/"
        chmod +x "$install_dir/csql-web.sh"
    fi
fi
chmod +x "$install_dir/bin/"*
if [ -f "$pack/uninstall.sh" ]; then
    cp "$pack/uninstall.sh" "$install_dir/"
    chmod +x "$install_dir/uninstall.sh"
fi

# ---- 写全局配置 ----
step 'Writing settings.toml'
config_dir=$(dirname -- "$config_file")
mkdir -p "$config_dir"
config_dir=$(CDPATH= cd -- "$config_dir" && pwd)
config_file="$config_dir/settings.toml"
if [ ! -f "$config_file" ]; then
    sed -e "s|@DATA_DIR@|$(printf '%s' "$data_dir" | sed 's/[\&|]/\\&/g')|g" \
        -e "s|@LOG_DIR@|$(printf '%s' "$log_dir" | sed 's/[\&|]/\\&/g')|g" \
        "$pack/resources/settings.toml.linux" > "$config_file"
fi
if [ "$low_performance_adapted" = 'yes' ]; then
    [ -f "$config_file.before-low-performance" ] || cp -p "$config_file" "$config_file.before-low-performance"
    profile_output=$(mktemp "$config_file.XXXXXX")
    if apply_performance_profile "$pack/resources/settings.low-performance.toml" "$config_file" > "$profile_output"; then
        mv "$profile_output" "$config_file"
    else
        fail 'cannot apply low performance profile; original settings.toml was preserved'
    fi
    note 'low performance settings applied; every resource limit can be edited in settings.toml'
fi
echo "  config file  $config_file"

# ---- 引导系统目录 ----
# 引导身份目录、root 与预装类型，口令通过标准输入传递
step 'Initializing the administrator identity and preinstalled types'
bootstrap="$install_dir/bin/csql-bootstrap"
[ -n "$admin_password" ] || fail 'the administrator password is required'
( cd "$install_dir" && printf '%s\n' "$admin_password" | "$bootstrap" --config "$config_file" --user "$root_user" --password-stdin ) ||
    fail 'the bootstrap program failed; identities or preinstalled types are not ready'
if [ "$system_mode" = 'yes' ] && [ "$prefix" = '/' ]; then
    chown -R "$service_user:$service_user" "$data_dir" "$log_dir"
    chmod 0750 "$data_dir" "$log_dir"
    chown root:"$service_user" "$config_dir" "$config_file"
    chmod 0750 "$config_dir"
    chmod 0660 "$config_file"
fi

# ---- 命令入口 ----
step 'Creating commands'
if [ "$want_web" = 'yes' ]; then
    cat > "$install_dir/csql-web" <<'EOF'
#!/bin/sh
exec "$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)/csql-web.sh" "$@"
EOF
    chmod +x "$install_dir/csql-web"
fi
if [ "$want_cli" = 'yes' ]; then
    cat > "$install_dir/csql" <<'EOF'
#!/bin/sh
exec "$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)/bin/csql" "$@"
EOF
    chmod +x "$install_dir/csql"
fi

if [ "$system_mode" = 'yes' ]; then
    if [ "$want_cli" = 'yes' ]; then ln -sf "$install_dir/bin/csql" "$prefix/usr/bin/csql"; else rm -f "$prefix/usr/bin/csql"; fi
    if [ "$want_web" = 'yes' ]; then ln -sf "$install_dir/csql-web.sh" "$prefix/usr/bin/csql-web"; else rm -f "$prefix/usr/bin/csql-web"; fi
    ln -sf "$install_dir/bin/chusql-server" "$prefix/usr/bin/chusql-server"
fi

# ---- 环境变量 ----
if [ "$system_mode" = 'yes' ]; then
    step 'Linking commands'
    system_commands='chusql-server'
    if [ "$want_cli" = 'yes' ]; then system_commands="csql $system_commands"; fi
    if [ "$want_web" = 'yes' ]; then system_commands="csql-web $system_commands"; fi
    note "commands: $system_commands (in $prefix/usr/bin)"
else
step 'Updating PATH'
profile="$HOME/.profile"
marker='# ChuSQL'
if [ -f "$profile" ] && grep -q "^$marker$" "$profile"; then
    awk -v m="$marker" 'skip > 0 { skip--; next } $0 == m { skip=2; next } { print }' "$profile" > "$profile.chusql-install"
    mv "$profile.chusql-install" "$profile"
fi
{
    echo ''
    echo "$marker"
    echo "PATH=\"\$PATH:$install_dir:$install_dir/bin\""
    echo "export PATH"
} >> "$profile"
echo "added $install_dir to PATH in $profile (login again or: . \"$profile\")"
fi

# ---- 删掉安装包 ----
if [ "$keep_package" = 'no' ]; then
    step 'Removing the package'
    case "$install_dir" in
        "$pack"*|"$pack") echo "kept the package (install dir is inside it): $pack" ;;
        *)
            if ls "$pack/bin/"libchusql_core_storage.* >/dev/null 2>&1; then
                ( cd / && rm -rf "$pack" )
                echo 'package removed'
            else
                echo "kept the package (not safe to delete automatically): $pack"
            fi
            ;;
    esac
else
    step 'Keeping the package'
    echo "  package kept at $pack"
fi

# ---- 拉起服务 ----
server_bin="$install_dir/bin/chusql-server"
server_pid=''
if [ "$use_service" = 'yes' ]; then
    step 'Registering the systemd service and boot autostart'
    for service_arg in "$install_dir" "$server_bin" "$config_file" "$root_user"; do
        case "$service_arg" in
            *"
"*) fail 'service arguments must not contain newlines' ;;
        esac
    done
    mkdir -p "$(dirname "$service_file")"
    # 转义 systemd 双引号参数与变量符号。
    unit_quote() {
        printf '%s' "$1" | sed 's/\\/\\\\/g; s/"/\\"/g; s/%/%%/g'
    }
    if [ "$system_mode" = 'yes' ]; then
        service_identity="User=$service_user
Group=$service_user
"
        unit_target='multi-user.target'
    else
        service_identity=''
        unit_target='default.target'
    fi
    cat > "$service_file" <<EOF
[Unit]
Description=ChuSQL database server

[Service]
Type=simple
${service_identity}WorkingDirectory=$(printf '%s' "$install_dir" | sed 's/%/%%/g')
ExecStart=:"$(unit_quote "$server_bin")" --config "$(unit_quote "$config_file")" --user "$(unit_quote "$root_user")"
Restart=on-failure
RestartSec=5
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=$unit_target
EOF
    $ctl daemon-reload || fail 'cannot reload systemd units'
    $ctl enable chusql-server.service || fail 'cannot enable boot autostart'
    if [ "$no_start" = 'yes' ]; then
        note '--no-start: boot autostart enabled; the server was not started'
    else
        $ctl restart chusql-server.service || fail 'cannot start chusql-server.service'
        sleep 1
        $ctl is-active --quiet chusql-server.service || fail 'the server stopped; run journalctl -u chusql-server.service'
        note 'server running under systemd'
    fi
elif [ "$no_start" = 'yes' ]; then
    note '--no-start: the server was not started'
else
    step 'Starting the server'
    mkdir -p "$log_dir"
    ( cd "$install_dir" && exec nohup "$server_bin" --config "$config_file" --user "$root_user" ) > "$log_dir/server.log" 2> "$log_dir/server.err.log" &
    server_pid=$!
    sleep 1
    if ! kill -0 "$server_pid" 2>/dev/null; then
        fail "the server stopped right away; look at $log_dir/server.err.log"
    fi
    note "server running, pid $server_pid"
    note "log            $log_dir/server.err.log"
fi

step 'Done'
echo "  settings.toml  $config_file"
echo "  data         $data_dir"
start_with=''
if [ "$want_web" = 'yes' ]; then start_with='csql-web'; fi
if [ "$want_cli" = 'yes' ]; then
    if [ -n "$start_with" ]; then start_with="$start_with, csql"; else start_with='csql'; fi
fi
echo "  start with   $start_with"
echo "  tcp server   $server_bin (listens on [server] listen_host/port, defaults 127.0.0.1:7777)"
if [ "$use_service" = 'yes' ] && [ "$system_mode" = 'yes' ]; then
    echo "  service      chusql-server.service (system-wide, boot autostart enabled)"
    echo "  manage       systemctl status|start|stop|restart chusql-server.service"
    echo "  logs         journalctl -u chusql-server.service"
elif [ "$use_service" = 'yes' ]; then
    echo '  service      chusql-server.service (boot autostart enabled)'
    echo '  manage       systemctl --user status|start|stop|restart chusql-server.service'
    echo '  logs         journalctl --user -u chusql-server.service'
elif [ -n "$server_pid" ]; then
    echo "  running      pid $server_pid (stop it with: kill $server_pid)"
else
    echo "  start it     $server_bin --config \"$config_file\" --user $root_user"
fi
