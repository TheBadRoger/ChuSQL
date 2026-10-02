#!/bin/sh
set -eu

# ChuSQL 安装脚本（Linux/macOS）：装 cli/web、写全局配置、
# 改 PATH、跑 csql-bootstrap 建系统目录、最后拉起服务。

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

# 打印用法、选项与环境变量说明。
usage() {
    cat <<'EOF'
usage: ./install.sh [--component web|cli|both] [options]

  --component web|cli|both
                          which parts to install. Leave it out and the installer
                          asks (English, y/n), command line client first:
                            command line client (csql)?   default yes [Y/n]
                            web front end (browser UI)?   default no  [y/N]
                          it then asks whether to give the administrator account a
                          password (default no); that password goes straight into the
                          system catalog and is never written to chusql.toml

  --interactive           ask even when there is no terminal; the answers are
                          read from stdin (one per line)
  --install-dir DIR       where to install   (default: $HOME/.local/share/chusql)
  --data-dir DIR          where to keep data (default: <install-dir>/data)
  --user NAME             administrator name (default: root)
  --keep-package          do not delete the downloaded/extracted package
  --password VALUE        password of the administrator account (required when
                          there is no terminal to ask on; shows up in the shell
                          history, so CHUSQL_ADMIN_PASSWORD is safer)
  --no-start              do not start the server after installing

where the files come from:
  (default)               a package next to this script, else the newest GitHub release
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
  CHUSQL_GITHUB_TOKEN     token for the version API (or GITHUB_TOKEN); without
                          one the API allows 60 requests per hour
EOF
}

component=''
install_dir=''
data_dir=''
root_user='root'
keep_package='no'
password_arg=''
no_start='no'
repo="$default_repo"
version='latest'
url_arg=''
from_source='no'
source_dir=''
list_versions='no'
interactive_flag='no'
want_web='no'
want_cli='no'

while [ $# -gt 0 ]; do
    case "$1" in
        --component) component="${2:-}"; shift 2 ;;
        --install-dir) install_dir="${2:-}"; shift 2 ;;
        --data-dir) data_dir="${2:-}"; shift 2 ;;
        --user) root_user="${2:-}"; shift 2 ;;
        --keep-package) keep_package='yes'; shift ;;
        --password) password_arg="${2:-}"; shift 2 ;;
        --no-start) no_start='yes'; shift ;;
        --url) url_arg="${2:-}"; shift 2 ;;
        --repo) repo="${2:-}"; shift 2 ;;
        --version) version="${2:-}"; shift 2 ;;
        --list-versions) list_versions='yes'; shift ;;
        --interactive) interactive_flag='yes'; shift ;;
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

[ -n "$install_dir" ] || install_dir="$HOME/.local/share/chusql"
[ -n "$data_dir" ] || data_dir="$install_dir/data"

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
# CHUSQL_ADMIN_PASSWORD。口令只经 stdin 传给 csql-bootstrap，不落 chusql.toml。
admin_password="${CHUSQL_ADMIN_PASSWORD:-}"
if [ "$list_versions" = 'no' ]; then
    if [ -n "$password_arg" ]; then
        admin_password="$password_arg"
    fi
    if [ "$can_prompt" = 'yes' ]; then
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
            curl -fsSL -H "$3" -H 'Accept: application/vnd.github+json' -o "$2" "$1" || return 1
        else
            curl -fsSL -o "$2" "$1" || return 1
        fi
    elif command -v wget >/dev/null 2>&1; then
        if [ -n "${3:-}" ]; then
            wget -q --header="$3" --header='Accept: application/vnd.github+json' -O "$2" "$1" || return 1
        else
            wget -q -O "$2" "$1" || return 1
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
# 请求 GitHub API 并落盘。
api_get() {
    if [ -n "$github_token" ]; then
        download "$api_base$1" "$2" "Authorization: Bearer $github_token"
    else
        download "$api_base$1" "$2"
    fi
}

# 把 release JSON 解析成 tag 与资源行。
parse_releases() {
    awk '
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
    ' "$1"
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
    [ -f "$src/scripts/chusql.toml" ] || fail "not a ChuSQL checkout (scripts/chusql.toml missing): $src"

    if command -v pkg-config >/dev/null 2>&1 && ! pkg-config --exists zlib 2>/dev/null; then
        note 'hint       the Haskell zlib binding needs the C library: apt-get install zlib1g-dev (or dnf install zlib-devel)'
    fi

    step 'Building the Rust storage (cargo build --release)'
    ( cd "$src/chusql-core/storage" && cargo build --release ) || fail 'cargo build --release failed'
    storage_lib="$src/chusql-core/storage/target/release/libchusql_core_storage.so"
    [ -f "$storage_lib" ] || fail "storage library not found after the build: $storage_lib"

    step 'Staging the package'
    pack="$tmp/stage"
    mkdir -p "$pack/bin" "$pack/scripts"
    cp "$storage_lib" "$pack/bin/"
    cp "$src/scripts/chusql.toml" "$pack/scripts/"
    cp "$src/scripts/install.sh" "$pack/"
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
[ -f "$pack/scripts/chusql.toml" ] || fail 'package is incomplete: scripts/chusql.toml not found'

echo 'ChuSQL installer'
echo "  version    $version"
echo "  components $component_label"
echo "  install to $install_dir"
echo "  data dir   $data_dir"
echo "  root user  $root_user (password asked above, kept out of chusql.toml)"

# ---- 释放文件 ----
# 包是合在一起的（web 和 cli 都在），这里按这次的选择逐个释放：没选的组件不落地
step 'Installing files'
mkdir -p "$install_dir/bin" "$install_dir/logs" "$data_dir"
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
chmod +x "$install_dir/bin/"* 2>/dev/null || true

# ---- 写全局配置 ----
step 'Writing chusql.toml'
config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/ChuSQL"
config_file="$config_dir/chusql.toml"
mkdir -p "$config_dir"
sed -e "s|^\(user[[:space:]]*=[[:space:]]*\).*|\1\"$root_user\"|" \
    -e "s|^\(data_dir[[:space:]]*=[[:space:]]*\).*|\1\"$data_dir\"|" \
    "$pack/scripts/chusql.toml" > "$config_file"
echo "  config file  $config_file"

# ---- 引导系统目录 ----
# csql-bootstrap 建 system 数据库和 __system_users 表；口令只走 stdin，不进 chusql.toml
step 'Creating the system catalog'
bootstrap="$install_dir/bin/csql-bootstrap"
[ -n "$admin_password" ] || fail 'the administrator password is required'
printf '%s\n' "$admin_password" | "$bootstrap" --config "$config_file" --user "$root_user" --password-stdin ||
    fail 'the bootstrap program failed; the system catalog is not ready'

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

# ---- 环境变量 ----
step 'Updating PATH'
profile="$HOME/.profile"
marker='# ChuSQL'
if [ -f "$profile" ] && grep -q "^$marker" "$profile"; then
    echo "PATH entry already present in $profile"
else
    {
        echo ''
        echo "$marker"
        echo "PATH=\"\$PATH:$install_dir\""
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
if [ "$no_start" = 'yes' ]; then
    note '--no-start: the server was not started'
else
    step 'Starting the server'
    mkdir -p "$install_dir/logs"
    nohup "$server_bin" --config "$config_file" > "$install_dir/logs/server.log" 2> "$install_dir/logs/server.err.log" &
    server_pid=$!
    sleep 1
    if ! kill -0 "$server_pid" 2>/dev/null; then
        fail "the server stopped right away; look at $install_dir/logs/server.err.log"
    fi
    note "server running, pid $server_pid"
    note "log            $install_dir/logs/server.err.log"
fi

step 'Done'
echo "  chusql.toml  $config_file"
echo "  data         $data_dir"
start_with=''
if [ "$want_web" = 'yes' ]; then start_with='csql-web'; fi
if [ "$want_cli" = 'yes' ]; then
    if [ -n "$start_with" ]; then start_with="$start_with, csql"; else start_with='csql'; fi
fi
echo "  start with   $start_with"
echo "  tcp server   $server_bin (listens on [server] host/port, defaults 127.0.0.1:7777)"
if [ -n "$server_pid" ]; then
    echo "  running      pid $server_pid (stop it with: kill $server_pid)"
else
    echo "  start it     $server_bin --config \"$config_file\""
fi
