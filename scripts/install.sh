#!/bin/sh
# ChuSQL 安装脚本（Linux / macOS / POSIX sh）。
#
# 三种来源，脚本自己判断走哪条：
#
#   1) 包内安装：解压一个发行包，脚本旁边就是 bin/，直接装本地文件
#        ./install.sh --component web
#
#   2) 在线安装：脚本从 GitHub 取发行包（也可用 --url 指任意 URL 或本地归档）
#        curl -fsSL https://raw.githubusercontent.com/TheBadRoger/ChuSQL/main/scripts/install.sh \
#            | sh -s -- --component web
#        sh install.sh --component web --url dist/chusql-web-linux-x86_64.tar.gz
#
#   3) 源码安装：拉仓库、本地编译（要有 git + cargo + stack + ghc）
#        ./install.sh --component web --from-source
#        ./install.sh --component web --from-source --source-dir ../ChuSQL
#
# 安装 = 释放到目标目录 + 配好环境变量 + 写全局配置，装完把包目录删掉。
# storage/engine 一定装；web 与 cli 二选一。
#
# 包目录结构（解压后就是这个样子）：
#   bin/         chusql-storage（必装）、chusql-web 或 csql（二选一）
#   static/      Web 前端静态资源（装 web 组件时带）
#   scripts/     chusql.toml、init.sql
#   csql-web.sh
#   install.sh

set -eu

default_repo='TheBadRoger/ChuSQL'

usage() {
    cat <<'EOF'
usage: ./install.sh --component web|cli [options]

  --component web|cli     which front end to install (required)

  --install-dir DIR       where to install   (default: $HOME/.local/share/chusql)
  --data-dir DIR          where to keep data (default: <install-dir>/data)
  --user NAME             administrator name (default: root)
  --password PW           administrator password, empty means no password
                          (administrator-only sign-in)
  --keep-package          do not delete the downloaded/extracted package

where the files come from:
  (default)               a package next to this script, else the newest GitHub release
  --url URL|PATH          install from this archive (URL, file:// URL or local path)
  --repo OWNER/NAME       GitHub repository (default: TheBadRoger/ChuSQL)
  --version TAG           release tag to download / git ref to build (default: latest)
  --from-source           clone and build locally (needs git, cargo, stack, ghc)
  --source-dir DIR        build from this checkout instead of cloning
  -h, --help              this text
EOF
}

component=''
install_dir=''
data_dir=''
root_user='root'
root_password=''
keep_package='no'
repo="$default_repo"
version='latest'
url_arg=''
from_source='no'
source_dir=''

while [ $# -gt 0 ]; do
    case "$1" in
        --component) component="${2:-}"; shift 2 ;;
        --install-dir) install_dir="${2:-}"; shift 2 ;;
        --data-dir) data_dir="${2:-}"; shift 2 ;;
        --user) root_user="${2:-}"; shift 2 ;;
        --password) root_password="${2:-}"; shift 2 ;;
        --keep-package) keep_package='yes'; shift ;;
        --url) url_arg="${2:-}"; shift 2 ;;
        --repo) repo="${2:-}"; shift 2 ;;
        --version) version="${2:-}"; shift 2 ;;
        --from-source) from_source='yes'; shift ;;
        --source-dir) source_dir="${2:-}"; shift 2 ;;
        -h|--help) usage; exit 0 ;;
        *) echo "!! unknown option: $1" >&2; usage >&2; exit 1 ;;
    esac
done

fail() { echo "!! $1" >&2; exit 1; }
step() { echo; echo "==> $1"; }
note() { echo "  $1"; }

case "$component" in
    web|cli) ;;
    '') fail '--component is required (web or cli)' ;;
    *) fail "--component must be web or cli, got: $component" ;;
esac

# ---- 本脚本在哪（管道进来时 $0 是 sh，这时没有本地包）----
self=$0
script_dir=''
case "$self" in
    sh|bash|dash|ash|ksh|'-sh'|*/sh|*/bash|*/dash|*/ash|*/ksh) script_dir='' ;;
    *) script_dir=$(CDPATH= cd -- "$(dirname -- "$self")" 2>/dev/null && pwd) || script_dir='' ;;
esac

[ -n "$install_dir" ] || install_dir="$HOME/.local/share/chusql"
[ -n "$data_dir" ] || data_dir="$install_dir/data"

tmp=''
cleanup() { if [ -n "$tmp" ]; then rm -rf "$tmp"; fi; }
new_tmp() {
    tmp=$(mktemp -d "${TMPDIR:-/tmp}/chusql-install.XXXXXX") || fail 'cannot create a temporary directory'
    # --keep-package 时不注册清理，临时目录就留着
    if [ "$keep_package" = 'no' ]; then trap cleanup EXIT INT TERM HUP; fi
}

download() {
    # download URL FILE
    if command -v curl >/dev/null 2>&1; then
        curl -fsSL -o "$2" "$1" || return 1
    elif command -v wget >/dev/null 2>&1; then
        wget -q -O "$2" "$1" || return 1
    else
        fail 'need curl or wget to download the package'
    fi
}

verify_checksum() {
    # verify_checksum ASSET_URL FILE —— 发布带 .sha256 就校验，没带就说明一下
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

unpack() {
    # unpack ARCHIVE DEST
    mkdir -p "$2"
    case "$1" in
        *.zip)
            command -v unzip >/dev/null 2>&1 || fail 'unzip is required to open a .zip package'
            unzip -q "$1" -d "$2"
            ;;
        *)
            tar -xzf "$1" -C "$2"
            ;;
    esac
}

# 平台标签（全局：os、arch、ext、labels）
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

# 发行资源（全局：url、archive），成功返回 0
fetch_asset() {
    asset="$1"
    if [ "$version" = 'latest' ]; then
        url="https://github.com/$repo/releases/latest/download/$asset"
    else
        url="https://github.com/$repo/releases/download/$version/$asset"
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
            step "Cloning https://github.com/$repo.git"
            git clone --depth 1 "https://github.com/$repo.git" "$src" >/dev/null 2>&1 ||
                fail "cannot clone https://github.com/$repo.git"
        else
            step "Cloning https://github.com/$repo.git at $version"
            git clone --depth 1 --branch "$version" "https://github.com/$repo.git" "$src" >/dev/null 2>&1 ||
                fail "cannot clone $repo at $version"
        fi
        note "source     $src"
    fi
    [ -f "$src/chusql-storage/Cargo.toml" ] || fail "not a ChuSQL checkout (chusql-storage/Cargo.toml missing): $src"
    [ -f "$src/scripts/chusql.toml" ] || fail "not a ChuSQL checkout (scripts/chusql.toml missing): $src"

    if command -v pkg-config >/dev/null 2>&1 && ! pkg-config --exists zlib 2>/dev/null; then
        note 'hint       the Haskell zlib binding needs the C library: apt-get install zlib1g-dev (or dnf install zlib-devel)'
    fi

    step 'Building the Rust storage (cargo build --release)'
    ( cd "$src/chusql-storage" && cargo build --release ) || fail 'cargo build --release failed'
    storage_bin="$src/chusql-storage/target/release/chusql-storage"
    [ -f "$storage_bin" ] || fail "storage binary not found after the build: $storage_bin"

    if [ "$component" = 'web' ]; then
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

    step 'Staging the package'
    pack="$tmp/stage"
    mkdir -p "$pack/bin" "$pack/scripts"
    cp "$storage_bin" "$pack/bin/"
    cp "$front_bin" "$pack/bin/"
    for extra in "$stack_root/bin/"*.so "$stack_root/bin/"*.dll; do
        if [ -f "$extra" ]; then cp "$extra" "$pack/bin/"; fi
    done
    cp "$src/scripts/chusql.toml" "$src/scripts/init.sql" "$pack/scripts/"
    cp "$src/scripts/install.sh" "$pack/"
    if [ "$component" = 'web' ]; then
        [ -d "$src/chusql-web/static" ] || fail "static assets not found: $src/chusql-web/static"
        mkdir -p "$pack/static"
        cp -R "$src/chusql-web/static/." "$pack/static/"
        cp "$src/scripts/csql-web.sh" "$pack/"
    fi
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
elif [ -n "$script_dir" ] && [ -f "$script_dir/bin/chusql-storage" ]; then
    # ---------- 包内安装 ----------
    pack="$script_dir"
else
    # ---------- 从 GitHub 发行版下载 ----------
    new_tmp
    detect_platform
    step "Looking for a ChuSQL package for $os-$arch (component $component) in $repo"
    found=''
    for label in $labels; do
        if fetch_asset "chusql-$component-$label.$ext"; then
            found="$asset"
            break
        fi
    done
    if [ -z "$found" ]; then
        if fetch_asset "chusql-$component.$ext"; then found="$asset"; fi
    fi
    [ -n "$found" ] || fail "no package for $os-$arch in $repo ($version); try --url or --from-source"
    verify_checksum "$url" "$archive"
    step 'Unpacking'
    unpack "$archive" "$tmp/pkg"
    pack="$tmp/pkg"
fi

# ---- 校验包内容 ----
[ -f "$pack/bin/chusql-storage" ] || fail "package is incomplete: bin/chusql-storage not found in $pack"
if [ "$component" = 'web' ]; then
    [ -f "$pack/bin/chusql-web" ] || fail 'package is incomplete: bin/chusql-web not found'
    [ -d "$pack/static" ] || fail 'package is incomplete: static/ not found'
else
    [ -f "$pack/bin/csql" ] || fail 'package is incomplete: bin/csql not found'
fi
[ -f "$pack/scripts/chusql.toml" ] || fail 'package is incomplete: scripts/chusql.toml not found'

echo 'ChuSQL installer'
echo "  component  $component"
echo "  install to $install_dir"
echo "  data dir   $data_dir"
if [ -n "$root_password" ]; then
    echo "  root user  $root_user"
else
    echo "  root user  $root_user (no password: administrator-only sign-in)"
fi

# ---- 释放文件 ----
step 'Installing files'
mkdir -p "$install_dir/bin" "$install_dir/logs" "$data_dir"
cp -R "$pack/bin/." "$install_dir/bin/"
chmod +x "$install_dir/bin/"* 2>/dev/null || true
if [ "$component" = 'web' ]; then
    mkdir -p "$install_dir/static"
    cp -R "$pack/static/." "$install_dir/static/"
    if [ -f "$pack/csql-web.sh" ]; then
        cp "$pack/csql-web.sh" "$install_dir/"
        chmod +x "$install_dir/csql-web.sh"
    fi
fi

# ---- 写全局配置 ----
step 'Writing chusql.toml'
config_dir="${XDG_CONFIG_HOME:-$HOME/.config}/ChuSQL"
config_file="$config_dir/chusql.toml"
mkdir -p "$config_dir"
sed -e "s|^\(user[[:space:]]*=[[:space:]]*\).*|\1\"$root_user\"|" \
    -e "s|^\(password[[:space:]]*=[[:space:]]*\).*|\1\"$root_password\"|" \
    -e "s|^\(data_dir[[:space:]]*=[[:space:]]*\).*|\1\"$data_dir\"|" \
    "$pack/scripts/chusql.toml" > "$config_file"
echo "  config file  $config_file"
if [ -f "$pack/scripts/init.sql" ]; then cp "$pack/scripts/init.sql" "$install_dir/"; fi

# ---- 命令入口 ----
step 'Creating commands'
if [ "$component" = 'web' ]; then
    cat > "$install_dir/csql-web" <<'EOF'
#!/bin/sh
exec "$(CDPATH= cd -- "$(dirname -- "$0")" && pwd)/csql-web.sh" "$@"
EOF
    chmod +x "$install_dir/csql-web"
else
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
            if [ -f "$pack/bin/chusql-storage" ]; then
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

step 'Done'
echo "  chusql.toml  $config_file"
echo "  data         $data_dir"
if [ "$component" = 'web' ]; then echo '  start with   csql-web'; else echo '  start with   csql'; fi
