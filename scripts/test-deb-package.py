#!/usr/bin/env python3
import hashlib
import os
from pathlib import Path
import shutil
import subprocess
import sys
import tempfile

# 用最小假负载验证 package-deb.sh 的布局、控制文件与维护者脚本。

# 执行命令并把失败输出带回异常。
def run(args, cwd=None):
    result = subprocess.run(args, cwd=cwd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
    if result.returncode:
        raise RuntimeError(result.stdout)
    return result.stdout

# 生成满足打包脚本校验的最小包目录。
def fake_package(root):
    payload = root / "payload"
    for name in ("chusql-server", "csql", "chusql-web", "csql-bootstrap"):
        (payload / "bin").mkdir(parents=True, exist_ok=True)
        (payload / "bin" / name).write_text("#!/bin/sh\nexit 0\n")
    (payload / "bin/libchusql_core_storage.so").write_text("")
    (payload / "static").mkdir()
    (payload / "static/index.html").write_text("<html></html>\n")
    (payload / "csql-web.sh").write_text("#!/bin/sh\nexit 0\n")
    (payload / "resources").mkdir()
    (payload / "resources/settings.toml.linux").write_text(
        "[storage]\ndata_dir = \"@DATA_DIR@\"\nlog_files = \"@LOG_DIR@\"\n")
    (payload / "resources/settings.low-performance.toml").write_text("[buffer]\npool_size = 256\n")
    return payload

# 主流程：打包、解包并核对系统布局。
def main():
    repo = Path(__file__).resolve().parent.parent
    if shutil.which("dpkg-deb") is None:
        print("SKIP package-deb tests (dpkg-deb is not installed)", flush=True)
        return
    with tempfile.TemporaryDirectory(prefix="chusql-deb-test-") as temporary:
        root = Path(temporary)
        payload = fake_package(root)
        out = root / "out"
        script = repo / "scripts/package-deb.sh"
        run(["sh", str(script), "--package", str(payload), "--version", "v9.9.9", "--out-dir", str(out)])
        deb = out / "chusql_9.9.9_amd64.deb"
        assert deb.is_file(), "package-deb.sh did not write the package"
        digest = hashlib.sha256(deb.read_bytes()).hexdigest()
        assert (out / "chusql_9.9.9_amd64.deb.sha256").read_text().startswith(digest)
        fields = run(["dpkg-deb", "-f", str(deb), "Package", "Version", "Architecture", "Depends"])
        assert "Package: chusql" in fields and "Version: 9.9.9" in fields, fields
        assert "Architecture: amd64" in fields and "libgmp10" in fields, fields
        print("PASS package-deb builds control metadata and a checksum", flush=True)
        (root / "root").mkdir()
        run(["dpkg-deb", "-x", str(deb), str(root / "root")])
        tree = root / "root"
        assert (tree / "usr/lib/chusql/bin/chusql-server").is_file()
        assert (tree / "usr/lib/chusql/csql-web.sh").is_file()
        assert os.access(tree / "usr/lib/chusql/bin/chusql-server", os.X_OK)
        for name in ("csql", "csql-web", "chusql-server"):
            link = tree / "usr/bin" / name
            assert link.is_symlink(), f"missing /usr/bin/{name} link"
            assert link.resolve(strict=False).is_file(), f"broken /usr/bin/{name} link"
        unit = (tree / "usr/lib/systemd/system/chusql-server.service").read_text()
        assert "User=chusql" in unit and "--config /etc/chusql/settings.toml" in unit, unit
        settings = (tree / "etc/chusql/settings.toml").read_text()
        assert "@DATA_DIR@" not in settings and 'data_dir = "/var/lib/chusql"' in settings, settings
        assert 'log_files = "/var/log/chusql"' in settings, settings
        print("PASS package-deb lays out programs, links, unit and settings", flush=True)
        control = root / "control"
        run(["dpkg-deb", "--control", str(deb), str(control)])
        assert (control / "conffiles").read_text().strip() == "/etc/chusql/settings.toml"
        for name in ("postinst", "prerm", "postrm"):
            run(["sh", "-n", str(control / name)])
        postinst = (control / "postinst").read_text()
        assert "adduser --system" in postinst and "--password-stdin" in postinst, postinst
        assert "systemctl restart chusql-server.service" in postinst, postinst
        postrm = (control / "postrm").read_text()
        assert "rm -rf /var/lib/chusql" in postrm and "deluser --system chusql" in postrm, postrm
        print("PASS package-deb maintainer scripts create and purge the system state", flush=True)
        broken = fake_package(root / "broken")
        (broken / "csql-web.sh").unlink()
        result = subprocess.run(["sh", str(script), "--package", str(broken), "--version", "v9.9.9",
                                 "--out-dir", str(root / "broken-out")],
                                text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        assert result.returncode != 0 and "csql-web.sh" in result.stdout, result.stdout
        print("PASS package-deb refuses an incomplete package", flush=True)


if __name__ == "__main__":
    try:
        main()
    except (AssertionError, RuntimeError) as error:
        print(f"FAIL {error}", file=sys.stderr)
        raise SystemExit(1)
