#!/bin/sh
#   sh scripts/package.sh                          # 本机平台，web + cli 合成一个包
#   sh scripts/package.sh --no-build               # 用已有编译产物（快）
#   sh scripts/package.sh --no-verify              # 跳过「装一遍试试」
#   sh scripts/package.sh --out-dir /tmp/rel       # 换输出目录
#   sh scripts/package.sh --platform linux-x86_64  # 覆盖平台标签（默认按 uname 猜）
#   sh scripts/package.sh --version v0.1.0         # 只用于发布提示，不写进文件名
#
# 打完发出去（资源名必须跟这里一致，install.sh 才认得）：
#   gh release create v0.1.0 releases/* --generate-notes
#
# 收尾会按「只要 cli」「只要 web」各装一遍：核对落盘文件，也核对没选的组件没被释放。
# Windows 包用 scripts/package.ps1 打（这个脚本只出 tar.gz）。
set -eu

# 本机打发行包：构建、装配、打 tar.gz、出 sha256，产物落 releases/，命名与 CI 一致。
repo_root=$(cd "$(dirname "$0")/.." && pwd)
out_dir=$repo_root/releases
platform=''
version=''
build=yes
verify=yes

# 打印一行输出。
Say() { printf '%s\n' "$*"; }
# 打印步骤标题。
Step() { printf '\n== %s\n' "$*"; }
# 报错到 stderr 并退出 1。
Fail() {
    printf 'error: %s\n' "$*" >&2
    exit 1
}

while [ "$#" -gt 0 ]; do
    case "$1" in
        --platform)
            shift; [ "$#" -gt 0 ] || Fail '--platform needs a label like linux-x86_64'
            platform=$1; shift ;;
        --version)
            shift; [ "$#" -gt 0 ] || Fail '--version needs a tag like v0.1.0'
            version=$1; shift ;;
        --out-dir)
            shift; [ "$#" -gt 0 ] || Fail '--out-dir needs a path'
            out_dir=$1; shift ;;
        --no-build) build=no; shift ;;
        --no-verify) verify=no; shift ;;
        -h|--help) usage=yes; shift ;;
        *) Fail "unknown option: $1 (try --no-build, --out-dir DIR, --platform LABEL)" ;;
    esac
done

if [ "${usage:-no}" = 'yes' ]; then
    sed -n '2,/^set -eu$/p' "$0" | sed -e 's/^# \{0,1\}//' -e '$d'
    exit 0
fi

if [ -z "$platform" ]; then
    case "$(uname -s)" in
        Linux) os=linux ;;
        Darwin) os=macos ;;
        *) Fail "cannot tell the platform from $(uname -s); pass --platform linux-x86_64" ;;
    esac
    case "$(uname -m)" in
        x86_64|amd64) arch=x86_64 ;;
        arm64|aarch64) arch=arm64 ;;
        *) Fail "cannot tell the architecture from $(uname -m); pass --platform $os-x86_64" ;;
    esac
    platform=$os-$arch
fi

case "$platform" in
    windows-*) Fail 'the Windows package is scripts/package.ps1; this script only makes tar.gz archives' ;;
esac

# 用可用的工具算文件 sha256。
sha256_of() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | awk '{print $1}'
    elif command -v shasum >/dev/null 2>&1; then
        shasum -a 256 "$1" | awk '{print $1}'
    else
        openssl dgst -sha256 "$1" | awk '{print $NF}'
    fi
}

# 核对归档里必需的文件都在。
check_stage() {
    stage=$1
    ls "$stage/bin/"libchusql_core_storage.* >/dev/null 2>&1 || Fail 'bin/libchusql_core_storage.* is missing from the package'
    [ -f "$stage/bin/csql" ] || Fail 'bin/csql is missing from the package'
    [ -f "$stage/bin/chusql-web" ] || Fail 'bin/chusql-web is missing from the package'
    [ -f "$stage/bin/chusql-server" ] || Fail 'bin/chusql-server is missing from the package'
    [ -f "$stage/static/index.html" ] || Fail 'static/index.html is missing from the package'
    [ -f "$stage/csql-web.sh" ] || Fail 'csql-web.sh is missing from the package'
    [ -f "$stage/scripts/chusql.toml" ] || Fail 'scripts/chusql.toml is missing from the package'
    [ -f "$stage/install.sh" ] || Fail 'install.sh is missing from the package'
    [ -f "$stage/install.ps1" ] || Fail 'install.ps1 is missing from the package'
    # bin 里带 chusql 名字的文件只能是已知产物；改名前的残留或别的垃圾都算装配错误
    for f in "$stage"/bin/chusql* "$stage"/bin/libchusql*; do
        [ -e "$f" ] || continue
        case "$(basename "$f")" in
            libchusql_core_storage.so|libchusql_core_storage.dylib|libchusql_core_storage.dll|chusql-server|chusql-web) ;;
            *) Fail "unexpected artifact in bin/: $(basename "$f")" ;;
        esac
    done
}

# 按 cli / web 各装一遍，核对落地文件。
verify_package() {
    name=$1
    archive="$out_dir/$name.tar.gz"
    for comp in cli web; do
        vtmp=$(mktemp -d)
        mkdir -p "$vtmp/pkg" "$vtmp/home" "$vtmp/opt"
        if ! tar -xzf "$archive" -C "$vtmp/pkg"; then
            rm -rf "$vtmp"
            Fail "$name: the archive does not unpack"
        fi
        if ! ( cd "$vtmp/pkg" && HOME="$vtmp/home" sh install.sh --component "$comp" \
                --install-dir "$vtmp/opt" --data-dir "$vtmp/data" --user root --password package-check ) \
                >"$vtmp/log" 2>&1; then
            sed -n '1,120p' "$vtmp/log" >&2
            rm -rf "$vtmp"
            Fail "$name: installing the $comp part from the package failed (log above)"
        fi
        ok=yes
        ls "$vtmp/opt/bin/"libchusql_core_storage.* >/dev/null 2>&1 || ok=no
        [ -f "$vtmp/opt/bin/chusql-server" ] || ok=no
        [ -f "$vtmp/opt/init.sql" ] || ok=no
        [ -f "$vtmp/home/.config/ChuSQL/chusql.toml" ] || ok=no
        grep -q "data_dir = \"$vtmp/data\"" "$vtmp/home/.config/ChuSQL/chusql.toml" || ok=no
        if [ "$comp" = web ]; then
            [ -f "$vtmp/opt/bin/chusql-web" ] || ok=no
            [ -f "$vtmp/opt/static/index.html" ] || ok=no
            [ -f "$vtmp/opt/csql-web.sh" ] || ok=no
            [ -f "$vtmp/opt/csql-web" ] || ok=no
            # 没要 cli，就不许把 csql 释放出去
            if [ -f "$vtmp/opt/bin/csql" ] || [ -f "$vtmp/opt/csql" ]; then ok=no; fi
        else
            [ -f "$vtmp/opt/bin/csql" ] || ok=no
            [ -f "$vtmp/opt/csql" ] || ok=no
            # 没要 web，就不许把 chusql-web / static / 启动器释放出去
            if [ -f "$vtmp/opt/bin/chusql-web" ] || [ -e "$vtmp/opt/static" ] || [ -f "$vtmp/opt/csql-web" ]; then
                ok=no
            fi
        fi
        if [ "$ok" != yes ]; then
            sed -n '1,120p' "$vtmp/log" >&2
            rm -rf "$vtmp"
            Fail "$name: the $comp part is not what the installer promises (log above)"
        fi
        rm -rf "$vtmp"
        Say "  ok         $name installs the $comp part, and only the $comp part"
    done
}

# ---------- 构建 ----------
if [ "$build" = yes ]; then
    Step 'Building storage (Rust)'
    ( cd "$repo_root/chusql-core/storage" && cargo build --release )
    Step 'Building the web front end (Haskell)'
    ( cd "$repo_root/chusql-cli" && stack build --fast chusql-web:exe:chusql-web )
    Step 'Building the command line client (Haskell)'
    ( cd "$repo_root/chusql-cli" && stack build --fast chusql-cli:exe:csql )
    Step 'Building the TCP database server (Haskell)'
    ( cd "$repo_root/chusql-cli" && stack build --fast chusql-server:exe:chusql-server )
fi

# ---------- 装配 + 打归档 ----------
name="chusql-$platform"
stage="$out_dir/.stage/$name"
archive="$out_dir/$name.tar.gz"
mkdir -p "$out_dir"
trap 'rm -rf "$out_dir/.stage"' EXIT INT TERM

Step "Packing $name"
rm -rf "$out_dir/.stage"
mkdir -p "$stage/bin" "$stage/scripts"

cp "$repo_root/chusql-core/storage/target/release/"libchusql_core_storage.* "$stage/bin/" \
    || Fail "chusql-core/storage/target/release/libchusql_core_storage.* is not built (run without --no-build)"

stack_root=$(cd "$repo_root/chusql-cli" && stack path --local-install-root | tail -n 1 | tr -d '\r')
for front_name in chusql-web chusql-server csql; do
    front="$stack_root/bin/$front_name"
    if [ ! -f "$front" ]; then
        front=$(find "$repo_root/chusql-web/.stack-work" "$repo_root/chusql-cli/.stack-work" "$repo_root/chusql-server/.stack-work" \
            -name "$front_name" -type f 2>/dev/null | head -n 1 || true)
    fi
    [ -n "$front" ] && [ -f "$front" ] || Fail "$front_name is not built (run without --no-build)"
    cp "$front" "$stage/bin/"
done

# Haskell 一般静态链接，靶目录里真有共享库就一起带上；改名前的旧库（libchusql_storage.*）不能进包
for lib in "$repo_root"/chusql-core/storage/target/release/*.so "$repo_root"/chusql-core/storage/target/release/*.dylib; do
    [ -f "$lib" ] || continue
    case "$(basename "$lib")" in
        libchusql_storage.*|libchusql_storage-*) continue ;;
    esac
    cp "$lib" "$stage/bin/"
done

cp "$repo_root/scripts/chusql.toml" "$repo_root/scripts/init.sql" "$stage/scripts/"
cp "$repo_root/scripts/install.sh" "$repo_root/scripts/install.ps1" "$stage/"
mkdir -p "$stage/static"
cp -R "$repo_root"/chusql-web/static/. "$stage/static/"
cp "$repo_root/scripts/csql-web.sh" "$stage/"
if [ -f "$repo_root/scripts/csql-web.ps1" ]; then cp "$repo_root/scripts/csql-web.ps1" "$stage/"; fi
chmod +x "$stage/install.sh" "$stage/csql-web.sh"

check_stage "$stage"

rm -f "$archive" "$archive.sha256"
( cd "$stage" && tar -czf "$archive" . )
rm -rf "$out_dir/.stage"
hash=$(sha256_of "$archive")
printf '%s  %s\n' "$hash" "$name.tar.gz" > "$archive.sha256"
Say "  wrote      $archive"
Say "  checksum   $hash"

if [ "$verify" = yes ]; then
    verify_package "$name"
fi

# ---------- 收尾 ----------
Step 'Done'
ls -l "$out_dir/$name.tar.gz" "$out_dir/$name.tar.gz.sha256"
if [ -n "$version" ]; then
    Say ''
    Say "release $version with (tag 必须是真实存在的 git tag):"
    Say "  gh release create $version \"$out_dir\"/* --generate-notes"
else
    Say ''
    Say 'publish with (先用 --version 指定 tag):'
    Say "  gh release create <tag> \"$out_dir\"/* --generate-notes"
fi
