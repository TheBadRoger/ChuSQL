#!/bin/sh
set -eu

# 使用临时替身验证入口分派、口令文件及引导失败，不启动编译或数据库。
repo=$(CDPATH= cd -- "$(dirname -- "$0")/.." && pwd)
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT HUP INT TERM
export TEST_CALLS="$work/calls"
export TEST_PASSWORD="$work/received"
export CHUSQL_ADMIN_PASSWORD_FILE="$work/password"
export CHUSQL_CONFIG="$work/settings.toml"
export CHUSQL_ADMIN_USER=operator
mkdir "$work/bin"
export PATH="$work/bin:$PATH"

cat >"$work/bin/csql-bootstrap" <<'STUB'
#!/bin/sh
set -eu
printf 'bootstrap %s\n' "$*" >> "$TEST_CALLS"
cat > "$TEST_PASSWORD"
exit "${TEST_BOOTSTRAP_EXIT:-0}"
STUB
cat >"$work/bin/chusql-server" <<'STUB'
#!/bin/sh
printf 'server %s\n' "$*" >> "$TEST_CALLS"
STUB
chmod +x "$work/bin/"*

# 缺失或空白口令应在启动程序前拒绝。
for password_kind in missing empty blank; do
    rm -f "$CHUSQL_ADMIN_PASSWORD_FILE" "$TEST_CALLS"
    case "$password_kind" in
        empty) : >"$CHUSQL_ADMIN_PASSWORD_FILE" ;;
        blank) printf ' \r\n' >"$CHUSQL_ADMIN_PASSWORD_FILE" ;;
    esac
    if sh "$repo/docker/entrypoint.sh" server >"$work/output" 2>&1; then
        echo "Unexpected success for $password_kind password" >&2
        exit 1
    fi
    test ! -e "$TEST_CALLS"
done

# 保留空格与反斜杠，引导后按原样传参。
printf 'test password\\value' >"$CHUSQL_ADMIN_PASSWORD_FILE"
sh "$repo/docker/entrypoint.sh" server --max-rows 42
printf 'test password\\value\n' >"$work/expected"
cmp "$work/expected" "$TEST_PASSWORD"
grep -Fq "bootstrap --config $CHUSQL_CONFIG --user operator --password-stdin" "$TEST_CALLS"
grep -Fq "server --config $CHUSQL_CONFIG --user operator --max-rows 42" "$TEST_CALLS"
test "$(wc -l <"$TEST_CALLS")" -eq 2

# 引导失败必须阻止启动。
rm "$TEST_CALLS"
if TEST_BOOTSTRAP_EXIT=7 sh "$repo/docker/entrypoint.sh" server; then exit 1; fi
test "$(wc -l <"$TEST_CALLS")" -eq 1
rm "$CHUSQL_ADMIN_PASSWORD_FILE" "$TEST_CALLS"
test "$(sh "$repo/docker/entrypoint.sh" printf passthrough)" = passthrough
# 健康检查必须识别协议状态和连接失败。
cat >"$work/bin/nc" <<'STUB'
#!/bin/sh
cat > /dev/null
printf '%s\n' "$TEST_TCP_RESPONSE"
exit "${TEST_NC_EXIT:-0}"
STUB
chmod +x "$work/bin/nc"
TEST_TCP_RESPONSE='{"status":"pong"}' sh "$repo/docker/healthcheck.sh" server
if TEST_TCP_RESPONSE='{"status":"error"}' sh "$repo/docker/healthcheck.sh" server; then exit 1; fi
if TEST_TCP_RESPONSE='{"status":"pong"}' TEST_NC_EXIT=1 sh "$repo/docker/healthcheck.sh" server; then exit 1; fi
echo "entrypoint and healthcheck checks passed"
