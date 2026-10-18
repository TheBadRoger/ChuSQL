#!/bin/sh
# 把发行包发布到 ChuRepo：创建发行版、上传资产、通用软件包与 Debian 软件包。
# 用法：GITEA_TOKEN=... sh scripts/publish-gitea.sh v26.10.2 <发行包目录>
set -eu

REPO_API=${GITEA_REPO_API:-https://repo.chunana.cn/api/v1/repos/ChuNa7a/ChuSQL}
PKG_API=${GITEA_PKG_API:-https://repo.chunana.cn/api/packages/ChuNa7a/generic/chusql}
PKG_API_DELETE=${GITEA_PKG_DELETE_API:-$PKG_API}
DEB_API=${GITEA_DEB_API:-https://repo.chunana.cn/api/packages/ChuNa7a/debian}
DEB_POOL=${GITEA_DEB_POOL:-$DEB_API/pool/stable/main}
NOTES_DIR=${GITEA_NOTES_DIR:-docs/releases}

version=${1:?用法: publish-gitea.sh <vX.Y.Z> <发行包目录>}
dir=${2:?用法: publish-gitea.sh <vX.Y.Z> <发行包目录>}
[ -n "${GITEA_TOKEN:-}" ] || { echo 'publish-gitea: 需要 GITEA_TOKEN' >&2; exit 1; }

files='chusql-linux-x86_64.tar.gz chusql-linux-x86_64.tar.gz.sha256 chusql-windows-x86_64.zip chusql-windows-x86_64.zip.sha256'
deb_name=''
for candidate in "$dir"/chusql_*.deb; do
	[ -f "$candidate" ] || continue
	deb_name=$(basename "$candidate")
done
if [ -n "$deb_name" ]; then
	files="$files $deb_name $deb_name.sha256"
fi

echo 'publish-gitea: 校验发行包'
for f in $files; do
	[ -f "$dir/$f" ] || { echo "publish-gitea: 缺少 $dir/$f" >&2; exit 1; }
done
(cd "$dir" && sha256sum -c chusql-linux-x86_64.tar.gz.sha256 >/dev/null &&
	sha256sum -c chusql-windows-x86_64.zip.sha256 >/dev/null)
if [ -n "$deb_name" ]; then
	(cd "$dir" && sha256sum -c "$deb_name.sha256" >/dev/null)
fi

# 调用 Gitea API
api() {
	curl -sS -m 600 -H "Authorization: token $GITEA_TOKEN" "$@"
}

# 从 JSON 中取出 id 字段
json_id() {
	python3 -c 'import json,sys; print(json.load(sys.stdin).get("id", ""))'
}

echo "publish-gitea: 查找发行版 $version"
release_id=$(api "$REPO_API/releases/tags/$version" | json_id)
if [ -z "$release_id" ]; then
	echo "publish-gitea: 创建发行版 $version"
	body_file=$(mktemp)
	trap 'rm -f "$body_file"' EXIT
	if [ -f "$NOTES_DIR/$version.md" ]; then
		python3 - "$NOTES_DIR/$version.md" "$version" "$body_file" <<'PY'
import json, sys
notes, version, out = sys.argv[1:4]
body = open(notes, encoding='utf-8').read().rstrip()
body += '\n\n> 本版本的构建与测试由 GitHub Actions 完成；ChuRepo 不重复运行流水线。\n'
payload = {'tag_name': version, 'name': 'ChuSQL ' + version, 'body': body,
           'draft': False, 'prerelease': False}
open(out, 'w', encoding='utf-8').write(json.dumps(payload))
PY
	else
		printf '{"tag_name":"%s","name":"ChuSQL %s","draft":false,"prerelease":false}' \
			"$version" "$version" >"$body_file"
	fi
	release_id=$(curl -sS -m 120 -H "Authorization: token $GITEA_TOKEN" \
		-H 'Content-Type: application/json' --data-binary @"$body_file" \
		"$REPO_API/releases" | json_id)
fi
[ -n "$release_id" ] || { echo "publish-gitea: 创建发行版失败" >&2; exit 1; }

echo "publish-gitea: 上传发行版资产"
existing=$(api "$REPO_API/releases/$release_id" |
	python3 -c 'import json,sys; print(" ".join(a["name"] for a in json.load(sys.stdin).get("assets", [])))')
for f in $files; do
	case " $existing " in
	*" $f "*) echo "publish-gitea: 跳过已有资产 $f"; continue ;;
	esac
	code=$(curl -sS -m 900 -o /dev/null -w '%{http_code}' \
		-H "Authorization: token $GITEA_TOKEN" -F "attachment=@$dir/$f" \
		"$REPO_API/releases/$release_id/assets?name=$f")
	echo "publish-gitea: 资产 $f -> $code"
	[ "$code" = 201 ] || exit 1
done

echo "publish-gitea: 上传通用软件包"
pkg_version=${version#v}
for f in $files; do
	code=$(curl -sS -m 900 -o /dev/null -w '%{http_code}' -X PUT \
		-H "Authorization: token $GITEA_TOKEN" --upload-file "$dir/$f" \
		"$PKG_API/$pkg_version/$f")
	echo "publish-gitea: 软件包 $pkg_version/$f -> $code"
	if [ "$code" = 409 ]; then
		echo "publish-gitea: 软件包已存在，替换"
		curl -sS -m 60 -o /dev/null -w '' -X DELETE \
			-H "Authorization: token $GITEA_TOKEN" \
			"$PKG_API_DELETE/$pkg_version/$f"
		code=$(curl -sS -m 900 -o /dev/null -w '%{http_code}' -X PUT \
			-H "Authorization: token $GITEA_TOKEN" --upload-file "$dir/$f" \
			"$PKG_API/$pkg_version/$f")
		echo "publish-gitea: 软件包 $pkg_version/$f -> $code"
	fi
	[ "$code" = 201 ] || exit 1
done

if [ -n "$deb_name" ]; then
	echo "publish-gitea: 上传 Debian 软件包 $deb_name"
	deb_version=$(printf '%s' "$deb_name" | sed -e 's/^chusql_//' -e 's/_.*$//')
	code=$(curl -sS -m 900 -o /dev/null -w '%{http_code}' -X PUT \
		-H "Authorization: token $GITEA_TOKEN" \
		-H 'Content-Type: application/vnd.debian.binary-package' \
		--upload-file "$dir/$deb_name" "$DEB_POOL/upload")
	echo "publish-gitea: Debian 软件包 $deb_version -> $code"
	if [ "$code" = 409 ]; then
		echo "publish-gitea: Debian 软件包已存在，替换"
		curl -sS -m 60 -o /dev/null -w '' -X DELETE \
			-H "Authorization: token $GITEA_TOKEN" \
			"$DEB_POOL/chusql/$deb_version/amd64"
		code=$(curl -sS -m 900 -o /dev/null -w '%{http_code}' -X PUT \
			-H "Authorization: token $GITEA_TOKEN" \
			-H 'Content-Type: application/vnd.debian.binary-package' \
			--upload-file "$dir/$deb_name" "$DEB_POOL/upload")
		echo "publish-gitea: Debian 软件包 $deb_version -> $code"
	fi
	[ "$code" = 201 ] || exit 1
fi

echo "publish-gitea: 完成"
echo "  发行版: https://repo.chunana.cn/ChuNa7a/ChuSQL/releases/tag/$version"
echo "  软件包: https://repo.chunana.cn/ChuNa7a/-/packages/generic/chusql/$pkg_version"
if [ -n "$deb_name" ]; then
	echo "  apt 源: deb [signed-by=/etc/apt/keyrings/gitea-chuna7a.asc] https://repo.chunana.cn/api/packages/ChuNa7a/debian stable main"
fi
