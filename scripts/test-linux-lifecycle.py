#!/usr/bin/env python3
import json
from functools import partial
from http.server import SimpleHTTPRequestHandler, ThreadingHTTPServer
import os
from pathlib import Path
import shutil
import socket
import subprocess
import sys
import tempfile
import threading
import time

# 用真实发行包验证 Linux 安装、更新及卸载，数据与配置隔离在临时目录。


# 执行命令并将失败日志原样输出。
def run(args, env, cwd=None):
    result = subprocess.run(args, env=env, cwd=cwd, text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT, timeout=60)
    if result.returncode:
        raise RuntimeError(result.stdout)
    return result.stdout


# 选择本机空闲端口。
def free_port():
    with socket.socket() as sock:
        sock.bind(("127.0.0.1", 0))
        return sock.getsockname()[1]


# 登录真实服务器并执行 SQL。
def query(port, statements):
    with socket.create_connection(("127.0.0.1", port), timeout=5) as sock:
        with sock.makefile("rwb") as stream:
            answers = []
            for request in [{"method": "hello", "protocol": 1},
                            {"method": "login", "user": "root", "password": "Lifecycle-test-123!"}] + [
                                {"method": "query", "sql": sql} for sql in statements]:
                stream.write(json.dumps(request).encode() + b"\n")
                stream.flush()
                answer = json.loads(stream.readline())
                assert answer["status"] != "error", answer
                answers.append(answer)
            return answers[2:]


# 等待服务器可接受登录。
def wait_server(port):
    deadline = time.monotonic() + 15
    while time.monotonic() < deadline:
        try:
            query(port, [])
            return
        except (OSError, ValueError):
            time.sleep(0.1)
    raise RuntimeError("server did not become ready")


# 验证系统级布局的暂存安装与卸载。
def test_system_layout(repo, archive, root, env):
    stage = root / "sys-stage"
    env = dict(env, CHUSQL_ADMIN_PASSWORD="Lifecycle-test-123!")
    run(["sh", str(repo / "scripts/install.sh"), "--url", archive, "--system",
         "--prefix", str(stage), "--component", "cli", "--no-service", "--no-start"], env)
    binary = stage / "usr/lib/chusql/bin/chusql-server"
    assert binary.is_file(), "system layout: missing the server binary"
    for name in ("csql", "chusql-server"):
        link = stage / "usr/bin" / name
        assert link.is_symlink(), f"system layout: {name} is not a link"
        assert link.resolve().is_file(), f"system layout: {name} points nowhere"
    settings = (stage / "etc/chusql/settings.toml").read_text()
    assert "@DATA_DIR@" not in settings and "@LOG_DIR@" not in settings
    assert f'data_dir = "{stage}/var/lib/chusql"' in settings, settings
    assert (stage / "var/lib/chusql/system/catalog.json").is_file()
    assert not (stage / "etc/systemd/system/chusql-server.service").exists(), "prefix staging must skip systemd"
    profile = root / "home/.profile"
    assert not profile.exists() or "# ChuSQL" not in profile.read_text(), "system layout must not edit .profile"
    print("PASS staged system install lays out /usr, /etc, /var and links", flush=True)
    run(["sh", str(repo / "scripts/uninstall.sh"), "--system", "--prefix", str(stage), "--yes"], env)
    assert not (stage / "usr/lib/chusql").exists()
    assert not (stage / "var/lib/chusql").exists()
    assert not (stage / "etc/chusql/settings.toml").exists()
    assert not (stage / "usr/bin/csql").exists()
    print("PASS staged system uninstall removes the layout", flush=True)


# 用真实用户 systemd 验证注册、更新启动和卸载。
def test_systemd(repo, archive, root, env):
    systemctl = shutil.which("systemctl")
    state = run([systemctl, "--user", "show", "chusql-server.service", "-p", "LoadState"], env)
    assert "LoadState=not-found" in state, "existing chusql-server.service: refusing to change it"
    runtime_unit = Path(os.environ["XDG_RUNTIME_DIR"]) / "systemd/user/chusql-server.service"
    assert not runtime_unit.is_symlink(), "existing runtime service link: refusing to change it"
    user = run(["id", "-un"], env).strip()
    linger = run(["loginctl", "show-user", user, "-p", "Linger"], env).strip()
    home = root / "service-home"
    home.mkdir()
    env = dict(env, HOME=str(home), XDG_CONFIG_HOME=str(home / ".config"),
               XDG_STATE_HOME=str(home / ".local/state"), CHUSQL_ADMIN_PASSWORD="Lifecycle-test-123!")
    install = root / "service program"
    unit = home / ".config/systemd/user/chusql-server.service"
    bridge = root / "service-tools"
    bridge.mkdir()
    wrapper = bridge / "systemctl"
    wrapper.write_text(f'''#!/bin/sh
# 将隔离配置中的单元链接到用户 systemd 的运行目录。
if [ "$*" = '--user daemon-reload' ] && [ -f "$CHUSQL_TEST_UNIT" ]; then
    "{systemctl}" --user link --runtime "$CHUSQL_TEST_UNIT" || exit 1
fi
exec "{systemctl}" "$@"
''')
    wrapper.chmod(0o755)
    env.update(PATH=str(bridge) + ":" + env["PATH"], CHUSQL_TEST_UNIT=str(unit))
    installer = ["sh", str(repo / "scripts/install.sh"), "--url", archive,
                 "--component", "cli", "--install-dir", str(install)]
    try:
        run(installer + ["--data-dir", str(root / "service-data"), "--no-start"], env)
        config = home / ".config/ChuSQL/settings.toml"
        port = free_port()
        config.write_text(config.read_text().replace("port = 7777", f"port = {port}"))
        run(installer, env)
        wait_server(port)
        run([systemctl, "--user", "is-active", "--quiet", "chusql-server.service"], env)
        run(["sh", str(install / "uninstall.sh"), "--install-dir", str(install), "--yes"], env)
        assert not unit.exists() and not (root / "service-data").exists()
        print("PASS real systemd registration, update/start and uninstall", flush=True)
    finally:
        state = run([systemctl, "--user", "show", "chusql-server.service", "-p", "LoadState"], env)
        if "LoadState=loaded" in state:
            run([systemctl, "--user", "stop", "chusql-server.service"], env)
        user_units = Path(os.environ["HOME"]) / ".config/systemd/user"
        for link in [runtime_unit, user_units / "chusql-server.service",
                     user_units / "default.target.wants/chusql-server.service"]:
            if link.is_symlink() and link.resolve() == unit.resolve():
                link.unlink()
        run([systemctl, "--user", "daemon-reload"], dict(env, CHUSQL_TEST_UNIT="/nonexistent"))
        if linger == "Linger=no" and run(["loginctl", "show-user", user, "-p", "Linger"], env).strip() == "Linger=yes":
            run(["loginctl", "disable-linger", user], env)


# 用本机发行站验证版本查询、HTTP 下载与校验。
def test_online(repo, archive, root, env):
    site = root / "release-site"
    api = site / "api/v3/repos/test/ChuSQL/releases"
    api.parent.mkdir(parents=True)
    api.write_text(json.dumps([{"tag_name": "v-test", "prerelease": False,
                               "assets": [{"name": "chusql-linux-x86_64.tar.gz"}]}], indent=2))
    assets = site / "test/ChuSQL/releases/download/v-test"
    assets.mkdir(parents=True)
    shutil.copy2(archive, assets / Path(archive).name)
    shutil.copy2(archive + ".sha256", assets / (Path(archive).name + ".sha256"))
    server = ThreadingHTTPServer(("127.0.0.1", 0), partial(SimpleHTTPRequestHandler, directory=str(site)))
    thread = threading.Thread(target=server.serve_forever, daemon=True)
    thread.start()
    install = root / "online-program"
    env = dict(env, CHUSQL_GITHUB_BASE=f"http://127.0.0.1:{server.server_port}")
    installer = ["sh", str(repo / "scripts/install.sh"), "--repo", "test/ChuSQL"]
    try:
        assert "v-test" in run(installer + ["--list-versions"], env)
        output = run(installer + ["--component", "cli", "--install-dir", str(install),
                                  "--data-dir", str(root / "online-data"), "--no-service", "--no-start"], env)
        assert "checksum" in output and "(ok)" in output
        assert (install / "bin/csql").exists()
        run(["sh", str(install / "uninstall.sh"), "--install-dir", str(install), "--yes"], env)
        assert not (root / "online-data").exists()
        print("PASS release API, latest version selection, HTTP download and checksum (local fixture)", flush=True)
    finally:
        server.shutdown()
        server.server_close()
        thread.join()


# 在隔离目录验证配置、SQL 数据和卸载保留选项。
def main():
    if sys.platform != "linux" or len(sys.argv) not in (2, 3) or (len(sys.argv) == 3 and sys.argv[2] != "--systemd"):
        raise SystemExit("usage: python3 scripts/test-linux-lifecycle.py <linux-package.tar.gz> [--systemd]")
    archive = str(Path(sys.argv[1]).resolve())
    repo = Path(__file__).resolve().parent.parent
    (repo / "logs").mkdir(exist_ok=True)
    with tempfile.TemporaryDirectory(prefix="linux-lifecycle-", dir=repo / "logs") as temporary:
        root = Path(temporary)
        home = root / "home"
        home.mkdir()
        env = dict(os.environ, HOME=str(home), XDG_CONFIG_HOME=str(home / ".config"),
                   XDG_DATA_HOME=str(home / ".local/share"), XDG_STATE_HOME=str(home / ".local/state"),
                   CHUSQL_ADMIN_PASSWORD="Lifecycle-test-123!")
        env.pop("LD_LIBRARY_PATH", None)
        install = root / "program"
        data = install / "custom/nested/data"
        config = home / ".config/ChuSQL/settings.toml"
        installer = ["sh", str(repo / "scripts/install.sh"), "--url", archive,
                     "--install-dir", str(install), "--component", "cli", "--no-service", "--no-start"]
        run(installer + ["--data-dir", str(data)], env)
        assert (data / "system/catalog.json").is_file()
        assert (install / "uninstall.sh").is_file()
        assert "@DATA_DIR@" not in config.read_text()
        assert run([str(install / "bin/csql-bootstrap"), "--help"], env)
        print("PASS clean installation and dynamic library lookup", flush=True)
        port = free_port()
        settings = config.read_text().replace("port = 7777", f"port = {port}").replace(str(data), "custom/nested/data") + "\n# preserve custom settings\n"
        config.write_text(settings)
        server = subprocess.Popen([str(install / "bin/chusql-server"), "--config", str(config)],
                                  env=env, cwd=install, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
        try:
            wait_server(port)
            query(port, ["create database lifecycle", "use lifecycle", "create table saved (id int)",
                         "insert into saved (id) values (42)"])
            env["CHUSQL_ADMIN_PASSWORD"] = "Different-update-password-123!"
            run(installer, env)
            server.wait(timeout=5)
            assert config.read_text() == settings
            assert (home / ".profile").read_text().count("# ChuSQL\n") == 1
            server = subprocess.Popen([str(install / "bin/chusql-server"), "--config", str(config)],
                                      env=env, cwd=install, stdout=subprocess.DEVNULL, stderr=subprocess.DEVNULL)
            wait_server(port)
            result = query(port, ["use lifecycle", "select id from saved"])[-1]
            assert result["rowCount"] == 1 and "42" in json.dumps(result["rows"]), result
            print("PASS running update preserves configuration, credentials and SQL data", flush=True)
            uninstall = ["sh", str(install / "uninstall.sh"), "--install-dir", str(install), "--yes"]
            run(uninstall + ["--keep-data", "--keep-config"], env)
            server.wait(timeout=5)
            assert data.is_dir() and config.read_text() == settings
            assert not (install / "bin").exists()
            assert "# ChuSQL" not in (home / ".profile").read_text()
            print("PASS uninstall stops server and keeps nested data and configuration", flush=True)
            run(installer, env)
            run(uninstall, env)
            assert not install.exists() and not data.exists() and not config.exists()
            print("PASS reinstall and full uninstall discover configured data path", flush=True)
        finally:
            if server.poll() is None:
                server.terminate()
                server.wait(timeout=10)
        test_online(repo, archive, root, env)
        test_system_layout(repo, archive, root, env)
        if len(sys.argv) == 3:
            with tempfile.TemporaryDirectory(prefix="chusql-systemd-") as service_temporary:
                test_systemd(repo, archive, Path(service_temporary), env)


if __name__ == "__main__":
    main()
