#!/usr/bin/env python3
import hashlib
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
import json
import shlex
import tomllib
from pathlib import Path
import re
import shutil
import subprocess
import tempfile
import threading
import unittest

# 用本机 HTTP 夹具验证发行源回退、版本固定与令牌隔离。


class ReleaseFallback(unittest.TestCase):
    # 启动只响应测试路由的发行站。
    def setUp(self):
        self.routes = {}
        self.requests = []
        fixture = self

        class Handler(BaseHTTPRequestHandler):
            # 按夹具返回响应并记录认证头。
            def do_GET(self):
                fixture.requests.append((self.path, self.headers.get("Authorization")))
                status, body = fixture.routes.get(self.path, (404, b"missing"))
                self.send_response(status)
                self.end_headers()
                self.wfile.write(body)

            # 隐藏 HTTP 访问日志。
            def log_message(self, *args):
                pass

        self.server = ThreadingHTTPServer(("127.0.0.1", 0), Handler)
        self.thread = threading.Thread(target=self.server.serve_forever, daemon=True)
        self.thread.start()
        self.base = f"http://127.0.0.1:{self.server.server_port}"
        self.temp = tempfile.TemporaryDirectory()
        self.root = Path(self.temp.name)
        source = (Path(__file__).parent / "install.sh").read_text()
        self.functions = "\n".join(re.findall(r"^[a-z_]+\(\) \{[^\n]*\}$|^[a-z_]+\(\) \{\n.*?^\}", source, re.M | re.S))
        self.setup = f"""set -eu
{self.functions}
default_repo=TheBadRoger/ChuSQL
repo=$default_repo
github_base={self.base}/github
api_base=$github_base/api/v3
churepo_base={self.base}/churepo
churepo_repo=ChuNa7a/ChuSQL
github_token=secret-test-token
tmp={self.root}
version=v-test
labels=linux-x86_64
os=linux
arch=x86_64
ext=tar.gz
"""

    # 停止夹具并清理临时文件。
    def tearDown(self):
        self.server.shutdown()
        self.server.server_close()
        self.thread.join()
        self.temp.cleanup()

    # 执行真实安装脚本中的发行函数。
    def shell(self, command):
        return subprocess.run(["sh", "-c", self.setup + command], capture_output=True, text=True, timeout=15)

    # 预设只更新资源键，保留路径和其它设置且可重复合并。
    def test_low_performance_profile_merge(self):
        resources = Path(__file__).resolve().parent.parent / "resources"
        profile = resources / "settings.low-performance.toml"
        expected = tomllib.loads(profile.read_text())
        samples = ["[storage]\ndata_dir = '/srv/keep data'\n[server]\nport = 9999\nmax_connections = 77 # keep comment\n",
                   (resources / "settings.toml.linux").read_text(),
                   (resources / "settings.toml.windows").read_text()]
        for source in samples:
            target = self.root / "settings.toml"
            target.write_text(source)
            command = f"apply_performance_profile {shlex.quote(str(profile))} {shlex.quote(str(target))}"
            result = self.shell(command)
            self.assertEqual(result.returncode, 0, result.stderr)
            parsed = tomllib.loads(result.stdout)
            original = tomllib.loads(source)
            for section, values in expected.items():
                for key, value in values.items():
                    self.assertEqual(parsed[section][key], value)
            for section, values in original.items():
                for key, value in values.items():
                    if key not in expected.get(section, {}):
                        self.assertEqual(parsed[section][key], value)
            target.write_text(result.stdout)
            self.assertEqual(tomllib.loads(self.shell(command).stdout), parsed)

    # PowerShell 共用同一预设，并接受精确的双连字符选项。
    def test_powershell_profile_and_option(self):
        pwsh = shutil.which("pwsh")
        if not pwsh:
            self.skipTest("pwsh is unavailable")
        repo = Path(__file__).resolve().parent.parent
        source = (repo / "scripts/install.ps1").read_text()
        merge = re.search(r"^function Merge-PerformanceProfile.*?^\}", source, re.M | re.S).group()
        script = self.root / "merge.ps1"
        config = self.root / "config.toml"
        config.write_text("[server]\nport = 9001\nworker_threads = 12 # custom\n[storage]\ndata_dir = '/srv/keep'\n")
        script.write_text("param([string]$Config,[string]$Profile)\n$ErrorActionPreference = 'Stop'\n" + merge + "\nMerge-PerformanceProfile (Get-Content -Raw $Config) (Get-Content -Raw $Profile)\n")
        profile = repo / "resources/settings.low-performance.toml"
        result = subprocess.run([pwsh, "-NoProfile", "-File", str(script), "-Config", str(config), "-Profile", str(profile)], capture_output=True, text=True, timeout=15)
        self.assertEqual(result.returncode, 0, result.stderr)
        parsed = tomllib.loads(result.stdout)
        self.assertEqual(parsed["server"]["worker_threads"], 2)
        self.assertEqual(parsed["server"]["port"], 9001)
        self.assertEqual(parsed["storage"]["data_dir"], "/srv/keep")
        for section, values in tomllib.loads(profile.read_text()).items():
            for key, value in values.items():
                self.assertEqual(parsed[section][key], value)
        parameters = source[:source.index("$ErrorActionPreference")]
        script.write_text(parameters + "\nWrite-Output ([bool]$LowPerformanceAdapted)\n")
        for flag in ["--low-performance-adapted", "-LowPerformanceAdapted"]:
            result = subprocess.run([pwsh, "-NoProfile", "-File", str(script), flag], capture_output=True, text=True, timeout=15)
            self.assertEqual(result.returncode, 0, result.stderr)
            self.assertEqual(result.stdout.strip(), "True")

    # 验证紧凑 API、附件回退、同站校验与认证隔离。
    def test_api_and_asset_fallback(self):
        releases = [{"tag_name": "v-test", "prerelease": False,
                     "body": 'text with [brackets], \"quotes\"',
                     "assets": [{"name": "chusql-linux-x86_64.tar.gz"}]}]
        api = "/churepo/api/v1/repos/ChuNa7a/ChuSQL/releases?limit=100"
        self.routes[api] = (200, json.dumps(releases).encode())
        asset = "/churepo/ChuNa7a/ChuSQL/releases/download/v-test/chusql-linux-x86_64.tar.gz"
        self.routes[asset] = (200, b"release-package")
        digest = hashlib.sha256(b"release-package").hexdigest()
        self.routes[asset + ".sha256"] = (200, digest.encode())
        result = self.shell('version=latest; resolve_version; [ "$version" = v-test ]; fetch_asset chusql-linux-x86_64.tar.gz; verify_checksum "$url" "$archive"')
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("(ok)", result.stdout)
        self.assertEqual((self.root / "chusql-linux-x86_64.tar.gz").read_bytes(), b"release-package")
        self.assertTrue(any(auth for path, auth in self.requests if path.startswith("/github/")))
        self.assertTrue(all(auth is None for path, auth in self.requests if path.startswith("/churepo/")))

    # 列表不可用时从备用站解析 latest 的具体标签。
    def test_latest_redirect_fallback(self):
        self.routes["/churepo/api/v1/repos/ChuNa7a/ChuSQL/releases/latest"] = (
            200, json.dumps({"tag_name": "v-latest", "assets": []}).encode())
        asset = "/churepo/ChuNa7a/ChuSQL/releases/download/v-latest/package.tar.gz"
        self.routes[asset] = (200, b"latest-package")
        result = self.shell('version=latest; fetch_asset package.tar.gz; [ "$version" = v-latest ]')
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)

    # 两站缺少指定版本时不得静默换版。
    def test_missing_pinned_version_fails(self):
        result = self.shell('fetch_asset chusql-linux-x86_64.tar.gz')
        self.assertNotEqual(result.returncode, 0)
        self.assertTrue(all("latest" not in path for path, _ in self.requests))

    # 主站可用时无需访问备用站。
    def test_primary_success(self):
        path = "/github/TheBadRoger/ChuSQL/releases/download/v-test/package.tar.gz"
        self.routes[path] = (200, b"primary")
        result = self.shell('fetch_asset package.tar.gz')
        self.assertEqual(result.returncode, 0, result.stderr)
        self.assertEqual(self.requests, [(path, None)])

    # 显式仓库不回退至官方仓库。
    def test_custom_repo_does_not_fallback(self):
        result = self.shell('repo=custom/project; fetch_asset package.tar.gz')
        self.assertNotEqual(result.returncode, 0)
        self.assertTrue(all(path.startswith("/github/") for path, _ in self.requests))

    # 校验失败须中止，不能当作网络失败。
    def test_corrupt_checksum_fails(self):
        path = "/churepo/ChuNa7a/ChuSQL/releases/download/v-test/package.tar.gz"
        self.routes[path] = (200, b"corrupt")
        self.routes[path + ".sha256"] = (200, b"0" * 64)
        result = self.shell('fetch_asset package.tar.gz; verify_checksum "$url" "$archive"')
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("checksum mismatch", result.stderr)

    # 执行 PowerShell 的真实版本查询和下载校验代码。
    def powershell(self, command, download=False):
        if not shutil.which("pwsh"):
            self.skipTest("pwsh is required for PowerShell fallback tests")
        source = (Path(__file__).parent / "install.ps1").resolve()
        script = f"""$ErrorActionPreference = 'Stop'
$tokens = $null; $errors = $null
$ast = [System.Management.Automation.Language.Parser]::ParseFile('{source}', [ref]$tokens, [ref]$errors)
if ($errors.Count) {{ throw ($errors | Out-String) }}
$ast.FindAll({{ param($node) $node -is [System.Management.Automation.Language.FunctionDefinitionAst] }}, $true) | ForEach-Object {{ Invoke-Expression $_.Extent.Text }}
$Repo = 'TheBadRoger/ChuSQL'
$githubBase = '{self.base}/github'
$apiBase = \"$githubBase/api/v3\"
$churepoBase = '{self.base}/churepo'
$churepoRepo = 'ChuNa7a/ChuSQL'
$apiHeaders = @{{ Authorization = 'Bearer secret-test-token' }}
$Version = 'v-test'
$arch = 'x86_64'
$labels = @('windows-x86_64')
$tempDir = '{self.root}'
"""
        if download:
            text = source.read_text()
            script += text[text.index('    Step "Looking for a chusql package for windows-$arch"'):text.index("    Step 'Unpacking the package'")]
        script += "\n" + command
        self.root.mkdir(exist_ok=True)
        path = self.root / "test.ps1"
        path.write_text(script)
        return subprocess.run(["pwsh", "-NoProfile", "-File", str(path)], capture_output=True, text=True, timeout=20)

    # Windows 下载回退并校验，备用站不接收主站令牌。
    def test_powershell_fallback(self):
        api = "/churepo/api/v1/repos/ChuNa7a/ChuSQL/releases?limit=100"
        self.routes[api] = (200, json.dumps([{"tag_name": "v-test", "assets": []}]).encode())
        asset = "/churepo/ChuNa7a/ChuSQL/releases/download/v-test/chusql-windows-x86_64.zip"
        self.routes[asset] = (200, b"windows-package")
        digest = hashlib.sha256(b"windows-package").hexdigest()
        self.routes[asset + ".sha256"] = (200, digest.encode())
        result = self.powershell('if (@(Get-Releases)[0].tag_name -ne "v-test") { throw "wrong release" }')
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        result = self.powershell('if ($Version -ne "v-test") { throw "version changed" }', download=True)
        self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
        self.assertIn("(ok)", result.stdout)
        self.assertTrue(all(auth is None for path, auth in self.requests if path.startswith("/churepo/")))

    # Windows 双站缺包与校验错误均须失败。
    def test_powershell_errors(self):
        result = self.powershell('', download=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertTrue(all("latest" not in path for path, _ in self.requests))
        asset = "/churepo/ChuNa7a/ChuSQL/releases/download/v-test/chusql-windows-x86_64.zip"
        self.routes[asset] = (200, b"corrupt")
        self.routes[asset + ".sha256"] = (200, b"0" * 64)
        result = self.powershell('', download=True)
        self.assertNotEqual(result.returncode, 0)
        self.assertIn("checksum mismatch", result.stdout)


if __name__ == "__main__":
    unittest.main()
