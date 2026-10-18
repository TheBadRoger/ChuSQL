#!/bin/sh
set -eu

# ChuSQL 的 deb 打包脚本：把发行包装配成系统级布局，用 dpkg-deb 打成 .deb。
# 用法：sh scripts/package-deb.sh --package <目录|tar.gz> --version v26.10.2 [--arch amd64] [--out-dir DIR]

package_path=''
version=''
arch=''
out_dir=''
maintainer='chuana <chunana@users.noreply.github.com>'
repo_root=$(cd "$(dirname "$0")/.." && pwd)

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
        --package)
            shift; [ "$#" -gt 0 ] || Fail '--package needs a directory or .tar.gz'
            package_path=$1; shift ;;
        --version)
            shift; [ "$#" -gt 0 ] || Fail '--version needs a tag like v26.10.2'
            version=$1; shift ;;
        --arch)
            shift; [ "$#" -gt 0 ] || Fail '--arch needs amd64 or arm64'
            arch=$1; shift ;;
        --out-dir)
            shift; [ "$#" -gt 0 ] || Fail '--out-dir needs a path'
            out_dir=$1; shift ;;
        -h|--help) usage=yes; shift ;;
        *) Fail "unknown option: $1 (try --package, --version, --arch, --out-dir)" ;;
    esac
done

if [ "${usage:-no}" = 'yes' ]; then
    sed -n '2,/^set -eu$/p' "$0" | sed -e 's/^# \{0,1\}//' -e '$d'
    exit 0
fi

[ -n "$version" ] || Fail '--version is required'
[ -n "$package_path" ] || Fail '--package is required'
[ -e "$package_path" ] || Fail "no such package: $package_path"
command -v dpkg-deb >/dev/null 2>&1 || Fail 'dpkg-deb is required to build a .deb'

arch=${arch:-amd64}
case "$arch" in
    amd64|arm64) ;;
    *) Fail "unsupported --arch: $arch (use amd64 or arm64)" ;;
esac
out_dir=${out_dir:-$repo_root/releases}
mkdir -p "$out_dir"

deb_version=${version#v}
case "$deb_version" in
    ''|*[!0-9A-Za-z.+~-]*) Fail "not a usable Debian version: $version" ;;
esac

work=$(mktemp -d "${TMPDIR:-/tmp}/chusql-deb.XXXXXX")
trap 'rm -rf "$work"' EXIT INT TERM HUP

if [ -d "$package_path" ]; then
    stage=$package_path
else
    mkdir -p "$work/stage"
    tar -xzf "$package_path" -C "$work/stage" || Fail 'cannot unpack the package archive'
    stage="$work/stage"
fi
for required in bin/chusql-server bin/csql-bootstrap bin/csql bin/chusql-web \
    static/index.html csql-web.sh resources/settings.toml.linux resources/settings.low-performance.toml; do
    [ -e "$stage/$required" ] || Fail "package is incomplete: $required"
done
ls "$stage/bin/"libchusql_core_storage.* >/dev/null 2>&1 || Fail 'package is incomplete: bin/libchusql_core_storage.*'

Step "Staging the system layout (chusql_${deb_version}_${arch})"
root="$work/root"
bin_dir="$root/usr/lib/chusql/bin"
mkdir -p "$root/DEBIAN" "$bin_dir" "$root/usr/lib/chusql/static" \
    "$root/etc/chusql" "$root/usr/lib/systemd/system" "$root/usr/share/doc/chusql" "$root/usr/share/chusql"
cp "$stage/bin/chusql-server" "$stage/bin/csql" "$stage/bin/chusql-web" "$stage/bin/csql-bootstrap" "$bin_dir/"
for lib in "$stage/bin/"libchusql_core_storage.*; do
    [ -f "$lib" ] && cp "$lib" "$bin_dir/"
done
chmod 0755 "$bin_dir/"*
cp -R "$stage/static/." "$root/usr/lib/chusql/static/"
cp "$stage/csql-web.sh" "$root/usr/lib/chusql/csql-web.sh"
chmod 0755 "$root/usr/lib/chusql/csql-web.sh"
mkdir -p "$root/usr/bin"
ln -s ../lib/chusql/bin/csql "$root/usr/bin/csql"
ln -s ../lib/chusql/bin/csql-bootstrap "$root/usr/bin/csql-bootstrap"
ln -s ../lib/chusql/bin/chusql-server "$root/usr/bin/chusql-server"
ln -s ../lib/chusql/bin/chusql-web "$root/usr/bin/chusql-web"
ln -s ../lib/chusql/csql-web.sh "$root/usr/bin/csql-web"
sed -e 's|@DATA_DIR@|/var/lib/chusql|g' -e 's|@LOG_DIR@|/var/log/chusql|g' \
    "$stage/resources/settings.toml.linux" > "$root/etc/chusql/settings.toml"
chmod 0644 "$root/etc/chusql/settings.toml"
cp "$stage/resources/settings.low-performance.toml" "$root/usr/share/chusql/settings.low-performance.toml"

cat > "$root/usr/lib/systemd/system/chusql-server.service" <<'EOF'
[Unit]
Description=ChuSQL database server
After=network.target

[Service]
Type=simple
User=chusql
Group=chusql
WorkingDirectory=/usr/lib/chusql
ExecStart=/usr/lib/chusql/bin/chusql-server --config /etc/chusql/settings.toml --user root
Restart=on-failure
RestartSec=5
StandardOutput=journal
StandardError=journal

[Install]
WantedBy=multi-user.target
EOF
chmod 0644 "$root/usr/lib/systemd/system/chusql-server.service"

{
    printf 'Format: https://www.debian.org/doc/packaging-manuals/copyright-format/1.0/\n'
    printf 'Upstream-Name: ChuSQL\nUpstream-Contact: %s\n' "$maintainer"
    printf 'Source: https://github.com/TheBadRoger/ChuSQL\n\n'
    printf 'Files: *\nCopyright: 2026 TheBadRoger\nLicense: MIT\n'
    sed 's/^/ /' "$repo_root/LICENSE"
} > "$root/usr/share/doc/chusql/copyright"

installed_size=$(du -sk "$root/usr" | awk '{print $1}')
cat > "$root/DEBIAN/control" <<EOF
Package: chusql
Version: $deb_version
Section: database
Priority: optional
Architecture: $arch
Depends: adduser, libc6 (>= 2.34), libgmp10, libgcc-s1
Installed-Size: $installed_size
Maintainer: $maintainer
Homepage: https://github.com/TheBadRoger/ChuSQL
Description: SQL database with a Haskell query engine and a Rust storage layer
 ChuSQL is a relational database whose main interface is SQL. This package
 installs the command line client, the TCP server and the web front end
 system-wide. Data lives in /var/lib/chusql, logs in /var/log/chusql and the
 editable settings in /etc/chusql/settings.toml.
EOF
printf '/etc/chusql/settings.toml\n' > "$root/DEBIAN/conffiles"

Step 'Writing the maintainer scripts'
cat > "$root/DEBIAN/postinst" <<'EOF'
#!/bin/sh
set -eu

config=/etc/chusql/settings.toml
data_dir=/var/lib/chusql
log_dir=/var/log/chusql
password_file=/etc/chusql/admin-password

# 建服务账户与组（已存在就跳过）。
create_account() {
    getent group chusql >/dev/null 2>&1 || addgroup --system chusql >/dev/null
    getent passwd chusql >/dev/null 2>&1 ||
        adduser --system --ingroup chusql --home "$data_dir" --no-create-home --shell /usr/sbin/nologin chusql >/dev/null
}

# 建数据、日志与配置目录并交回服务账户。
prepare_dirs() {
    mkdir -p "$data_dir" "$log_dir" "$(dirname "$config")"
    chown chusql:chusql "$data_dir" "$log_dir"
    chmod 0750 "$data_dir" "$log_dir"
    chown root:chusql "$config"
    chmod 0660 "$config"
}

# 生成随机管理员口令。
generate_password() {
    if command -v openssl >/dev/null 2>&1; then
        openssl rand -hex 12
    else
        head -c 18 /dev/urandom | base64 | tr -dc 'A-Za-z0-9' | head -c 24
        printf '\n'
    fi
}

# 首次安装时建立系统目录与 root 管理员。
bootstrap_once() {
    [ -f "$data_dir/system/catalog.json" ] && return 0
    password=''
    if [ -s "$password_file" ]; then
        password=$(cat "$password_file")
    fi
    if [ -z "$password" ]; then
        password=$(generate_password)
        printf '%s\n' "$password" > "$password_file"
        chmod 0600 "$password_file"
    fi
    printf '%s\n' "$password" | /usr/lib/chusql/bin/csql-bootstrap --config "$config" --user root --password-stdin
    chown -R chusql:chusql "$data_dir"
}

# 在 systemd 运行时启用并启动服务。
start_service() {
    [ -d /run/systemd/system ] || {
        echo 'ChuSQL: systemd is not running; start chusql-server.service manually' >&2
        return 0
    }
    systemctl daemon-reload
    systemctl enable chusql-server.service >/dev/null
    if ! systemctl restart chusql-server.service; then
        echo 'ChuSQL: the service did not start; run journalctl -u chusql-server.service' >&2
        return 0
    fi
}

case "${1:-configure}" in
    configure)
        create_account
        prepare_dirs
        bootstrap_once
        start_service
        if [ -s "$password_file" ]; then
            echo "ChuSQL: administrator 'root' is ready."
            echo "  password: stored in $password_file (mode 0600)"
            echo "  change it with ALTER USER root PASSWORD '...', then delete that file"
        fi
        ;;
esac
EOF

cat > "$root/DEBIAN/prerm" <<'EOF'
#!/bin/sh
set -eu

case "${1:-remove}" in
    remove|upgrade|deconfigure)
        if [ -d /run/systemd/system ]; then
            systemctl stop chusql-server.service 2>/dev/null || true
        fi
        ;;
esac
EOF

cat > "$root/DEBIAN/postrm" <<'EOF'
#!/bin/sh
set -eu

# purge 时删除数据、日志、配置与账户。
case "${1:-remove}" in
    purge)
        rm -rf /var/lib/chusql /var/log/chusql
        rm -f /etc/chusql/admin-password
        rmdir /etc/chusql 2>/dev/null || true
        if getent passwd chusql >/dev/null 2>&1; then
            deluser --system chusql 2>/dev/null || true
        fi
        if getent group chusql >/dev/null 2>&1; then
            delgroup chusql 2>/dev/null || true
        fi
        ;;
esac
EOF
chmod 0755 "$root/DEBIAN/postinst" "$root/DEBIAN/prerm" "$root/DEBIAN/postrm"

Step 'Building the package'
deb_name="chusql_${deb_version}_${arch}.deb"
deb_path="$out_dir/$deb_name"
dpkg-deb --root-owner-group --build "$root" "$deb_path" >/dev/null || Fail 'dpkg-deb failed to build the package'
if ! dpkg-deb --info "$deb_path" >/dev/null || ! dpkg-deb --contents "$deb_path" >/dev/null; then
    Fail "dpkg-deb cannot read the package back: $deb_path"
fi

if command -v sha256sum >/dev/null 2>&1; then
    checksum=$(sha256sum "$deb_path" | awk '{print $1}')
else
    checksum=$(shasum -a 256 "$deb_path" | awk '{print $1}')
fi
printf '%s  %s\n' "$checksum" "$deb_name" > "$deb_path.sha256"

Step 'Done'
Say "  package  $deb_path"
Say "  size     $(wc -c < "$deb_path") bytes"
Say "  sha256   $checksum"
