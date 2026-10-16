#!/usr/bin/env python3
import argparse
import concurrent.futures
import json
import pathlib
import secrets
import socket
import subprocess
import tempfile
import threading
import time

# 在隔离容器和数据卷验证持久化、冲突、崩溃、磁盘满及备份恢复。


# 执行有超时的 Docker 命令并保留失败诊断。
def docker(*args, **kwargs):
    try:
        return subprocess.run(
            ["docker", *args], check=True, capture_output=True, timeout=60, **kwargs
        ).stdout
    except subprocess.CalledProcessError as error:
        diagnostic = error.stderr.decode(errors="replace") if isinstance(error.stderr, bytes) else error.stderr
        raise RuntimeError(diagnostic) from error


class Client:
    # 建立独立会话并验证协议及登录。
    def __init__(self, port, password, user="root"):
        self.socket = socket.create_connection(("127.0.0.1", port), timeout=15)
        self.stream = self.socket.makefile("rwb")
        try:
            assert self.request(method="hello", protocol=1)["status"] == "hello"
            assert self.request(method="login", user=user, password=password)["status"] == "ok"
        except BaseException:
            self.close()
            raise

    # 发送一条请求，不重试结果未知的写入。
    def request(self, **request):
        self.stream.write(json.dumps(request).encode() + b"\n")
        self.stream.flush()
        raw = self.stream.readline(4 * 1024 * 1024)
        if not raw:
            raise ConnectionError("server closed the connection")
        return json.loads(raw)

    # 执行 SQL，保留错误码以识别事务冲突。
    def sql(self, sql):
        reply = self.request(method="query", sql=sql)
        if reply["status"] == "error":
            raise QueryError(reply)
        assert reply["status"] == "result", reply
        return reply

    # 关闭流及底层套接字。
    def close(self):
        try:
            self.stream.close()
        finally:
            self.socket.close()


class QueryError(Exception):
    # 保存服务端结构化错误。
    def __init__(self, reply):
        self.reply = reply
        super().__init__(str(reply))


# 仅等待真实服务登录成功，不用固定启动延时。
def ready(container, password):
    deadline = time.monotonic() + 45
    while time.monotonic() < deadline:
        try:
            port = int(docker("port", container, "7777/tcp", text=True).strip().rsplit(":", 1)[1])
            client = Client(port, password)
            client.close()
            return port
        except (OSError, ValueError, AssertionError, subprocess.CalledProcessError):
            time.sleep(0.2)
    raise TimeoutError(f"server did not become ready: {container}")


# 连接验收库。
def connect(port, password):
    client = Client(port, password)
    try:
        client.sql("USE acceptance")
        return client
    except BaseException:
        client.close()
        raise


# 验证事务冲突不会丢失更新，失败后显式回滚。
def conflict(port, password):
    first = connect(port, password)
    second = connect(port, password)
    try:
        for client in (first, second):
            client.sql("BEGIN")
            client.sql("UPDATE counters SET value = value + 1 WHERE id = 1")
        first.sql("COMMIT")
        try:
            second.sql("COMMIT")
        except QueryError as error:
            assert error.reply["code"] == "serialization_failure", error.reply
            second.sql("ROLLBACK")
        else:
            raise AssertionError("conflicting update unexpectedly committed")
        assert first.sql("SELECT value FROM counters WHERE id = 1")["rows"] == [[1]]
        first.sql("UPDATE counters SET value = 0 WHERE id = 1")
    finally:
        first.close()
        second.close()


# 在独立会话中持续提交关联写入并核对已确认提交。
def workload(port, password, seconds, stop, acknowledged, lock, worker):
    client = connect(port, password)
    deadline = time.monotonic() + seconds
    sequence = 0
    conflicts = 0
    try:
        while not stop.is_set() and time.monotonic() < deadline:
            sequence += 1
            identity = worker * 1_000_000 + sequence
            client.sql("BEGIN")
            client.sql(f"INSERT INTO events (id, actor) VALUES ({identity}, {worker})")
            client.sql("UPDATE counters SET value = value + 1 WHERE id = 1")
            try:
                client.sql("COMMIT")
            except QueryError as error:
                if error.reply["code"] != "serialization_failure":
                    raise
                conflicts += 1
                client.sql("ROLLBACK")
                continue
            with lock:
                acknowledged.add(identity)
    except (OSError, ConnectionError, ValueError):
        if not stop.is_set():
            raise
    finally:
        try:
            client.close()
        except OSError:
            if not stop.is_set():
                raise
    return conflicts


# 核对事务组完整性及已确认提交，允许断线提交结果未知。
def verify(port, password, acknowledged):
    client = connect(port, password)
    try:
        rows = []
        last = -1
        while True:
            page = client.sql(f"SELECT id FROM events WHERE id > {last} ORDER BY id LIMIT 500")
            assert not page["truncated"], page
            rows.extend(page["rows"])
            if len(page["rows"]) < 500:
                break
            last = page["rows"][-1][0]
        identities = {row[0] for row in rows}
        assert len(identities) == len(rows), "duplicate committed event"
        assert acknowledged <= identities, "acknowledged commits were lost"
        assert client.sql("SELECT value FROM counters WHERE id = 1")["rows"] == [[len(rows)]]
        return len(rows)
    finally:
        client.close()


# 验证恢复后的账号和最小授权，不授予写权限。
def verify_reader(port, password):
    client = Client(port, password, user="backup_reader")
    try:
        client.sql("USE acceptance")
        client.sql("SELECT id FROM events")
        reply = client.request(method="query", sql="DELETE FROM events")
        assert reply["status"] == "error" and reply["code"] == "forbidden", reply
    finally:
        client.close()


# 运行有资源上限的容器，并登记待清理名称。
def launch(image, name, volume, secret, containers):
    containers.append(name)
    docker(
        "run", "-d", "--name", name, "--cpus", "1", "--memory", "512m",
        "--memory-swap", "512m", "--pids-limit", "128",
        "--tmpfs", "/fault:rw,size=16m,uid=10001,gid=10001,mode=0700",
        "-p", "127.0.0.1::7777", "-v", f"{volume}:/var/lib/chusql",
        "-v", f"{secret}:/run/secrets/admin_password:ro", image,
    )


# 清理所有测试资源，缺失对象除外均明确报错。
def cleanup(containers, volumes):
    errors = []
    for kind, names in (("container", containers), ("volume", volumes)):
        for name in reversed(names):
            try:
                docker(kind, "rm", *(["-f"] if kind == "container" else []), name)
            except RuntimeError as error:
                if f"No such {kind}" not in str(error):
                    errors.append(str(error))
    if errors:
        raise RuntimeError("test resource cleanup failed: " + "; ".join(errors))


# 注入真实 ENOSPC，验证提交后故障封闭与重放。
def disk_full(container, port, password):
    directory = "/var/lib/chusql/databases/acceptance"
    docker("exec", container, "sh", "-ec",
           f"touch /fault/checkpoint; ln -s /fault/checkpoint {directory}/wal.checkpoint.pending")
    filled = subprocess.run(
        ["docker", "exec", container, "sh", "-c", "LC_ALL=C dd if=/dev/zero of=/fault/fill bs=4096"],
        capture_output=True, text=True, timeout=30,
    )
    assert filled.returncode != 0 and "No space left on device" in filled.stderr, filled.stderr
    client = connect(port, password)
    try:
        reply = client.request(method="query", sql="INSERT INTO faults (id) VALUES (1)")
        assert reply["status"] == "error" and "space" in reply["message"].lower(), reply
        blocked = client.request(method="query", sql="SELECT id FROM faults")
        assert blocked["status"] == "error" and "recovery" in blocked["message"].lower(), blocked
        docker("exec", container, "sh", "-ec", f"test -s {directory}/wal.log")
    finally:
        client.close()
        docker("exec", container, "sh", "-ec",
               f"rm -f /fault/fill {directory}/wal.checkpoint.pending")
    docker("restart", "-t", "10", container)
    port = ready(container, password)
    client = connect(port, password)
    try:
        assert client.sql("SELECT id FROM faults")["rows"] == [[1]]
        client.sql("INSERT INTO faults (id) VALUES (2)")
    finally:
        client.close()
    return port


# 验收完整镜像，清理仅限本次创建的容器及卷。
def main():
    parser = argparse.ArgumentParser(description="Validate an already built ChuSQL image in isolated containers.")
    parser.add_argument("--image", default="chusql:local")
    parser.add_argument("--seconds", type=int, default=60)
    args = parser.parse_args()
    if args.seconds < 5:
        parser.error("--seconds must be at least 5")
    name = "chusql-acceptance-" + secrets.token_hex(4)
    containers = []
    volumes = []
    password = "Aa1!" + secrets.token_urlsafe(30)
    reader_password = "Aa1!" + secrets.token_urlsafe(30)
    acknowledged = set()
    lock = threading.Lock()
    stop = threading.Event()
    with tempfile.TemporaryDirectory(prefix="chusql-acceptance-") as temporary:
        secret = pathlib.Path(temporary) / "password"
        secret.write_text(password + "\n")
        secret.chmod(0o644)
        try:
            image = json.loads(docker("image", "inspect", args.image))[0]
            assert image["Config"]["User"] == "10001:10001", image["Config"]["User"]
            print(f"image={image['Id']} repo_digests={image['RepoDigests']}", flush=True)
            for suffix in ("data", "restore"):
                volume = name + "-" + suffix
                docker("volume", "create", volume)
                volumes.append(volume)
            server = name + "-server"
            launch(args.image, server, volumes[0], secret, containers)
            port = ready(server, password)
            client = Client(port, password)
            try:
                for sql in (
                    "CREATE DATABASE acceptance", "USE acceptance",
                    "CREATE TABLE counters (id INT PRIMARY KEY, value INT)",
                    "CREATE TABLE events (id INT PRIMARY KEY, actor INT)",
                    "CREATE TABLE faults (id INT PRIMARY KEY)",
                    "INSERT INTO counters (id, value) VALUES (1, 0)",
                    f"CREATE USER backup_reader IDENTIFIED BY '{reader_password}'",
                    "GRANT SELECT ON acceptance.events TO backup_reader",
                ):
                    client.sql(sql)
            finally:
                client.close()
            conflict(port, password)
            verify_reader(port, reader_password)
            print("conflicting transactions and rollback: passed", flush=True)
            with concurrent.futures.ThreadPoolExecutor(max_workers=3) as pool:
                tasks = [pool.submit(workload, port, password, args.seconds, stop, acknowledged, lock, i)
                         for i in range(1, 4)]
                conflicts = sum(task.result(timeout=args.seconds + 30) for task in tasks)
            assert len(acknowledged) >= 3, "insufficient successful load transactions"
            count = verify(port, password, acknowledged)
            print(f"load: {args.seconds}s, {count} committed transactions, {conflicts} conflicts", flush=True)
            uncommitted = connect(port, password)
            uncommitted.sql("BEGIN")
            uncommitted.sql("INSERT INTO faults (id) VALUES (99)")
            with concurrent.futures.ThreadPoolExecutor(max_workers=2) as pool:
                tasks = [pool.submit(workload, port, password, 30, stop, acknowledged, lock, i)
                         for i in (10, 11)]
                deadline = time.monotonic() + 20
                while time.monotonic() < deadline:
                    with lock:
                        progressed = len(acknowledged) >= count + 5
                    if progressed:
                        break
                    if any(task.done() for task in tasks):
                        for task in tasks:
                            if task.done():
                                task.result()
                        raise AssertionError("crash workload ended before progress")
                    time.sleep(0.02)
                assert progressed, "crash workload made no progress"
                stop.set()
                docker("kill", "--signal", "KILL", server)
                for task in tasks:
                    task.result(timeout=20)
            uncommitted.close()
            docker("start", server)
            port = ready(server, password)
            count = verify(port, password, acknowledged)
            client = connect(port, password)
            try:
                assert client.sql("SELECT id FROM faults")["rows"] == [], "uncommitted data survived crash"
            finally:
                client.close()
            print(f"SIGKILL recovery: {count} complete transactions, all acknowledged commits retained", flush=True)
            port = disk_full(server, port, password)
            verify(port, password, acknowledged)
            print("ENOSPC after WAL commit, recovery fence and replay: passed", flush=True)
            secret.write_text(secrets.token_urlsafe(30) + "\n")
            docker("restart", "-t", "10", server)
            port = ready(server, password)
            verify(port, password, acknowledged)
            print("idempotent bootstrap preserves existing password and data: passed", flush=True)
            docker("stop", "-t", "10", server)
            archive = docker("run", "--rm", "--network", "none", "--memory", "128m", "--cpus", "1",
                             "-v", f"{volumes[0]}:/var/lib/chusql:ro", "--entrypoint", "tar",
                             args.image, "-C", "/var/lib/chusql", "-czf", "-", ".")
            docker("run", "--rm", "-i", "--network", "none", "--memory", "128m", "--cpus", "1",
                   "-v", f"{volumes[1]}:/var/lib/chusql", "--entrypoint", "tar", args.image,
                   "-C", "/var/lib/chusql", "-xzf", "-", input=archive)
            restored = name + "-restored"
            launch(args.image, restored, volumes[1], secret, containers)
            port = ready(restored, password)
            assert verify(port, password, acknowledged) == count
            verify_reader(port, reader_password)
            client = connect(port, password)
            try:
                assert client.sql("SELECT id FROM faults ORDER BY id")["rows"] == [[1], [2]]
            finally:
                client.close()
            print("cold backup restores into an empty volume with original credentials: passed", flush=True)
            for container in containers:
                state = json.loads(docker("inspect", container))[0]
                assert not state["State"]["OOMKilled"], container
                assert state["HostConfig"]["Memory"] == 512 * 1024 * 1024
            print("runtime acceptance: passed (1 CPU / 512 MiB per server)", flush=True)
        except BaseException:
            for container in containers:
                result = subprocess.run(["docker", "logs", "--tail", "40", container], capture_output=True, text=True, timeout=15)
                print(result.stdout, result.stderr, flush=True)
            raise
        finally:
            cleanup(containers, volumes)


if __name__ == "__main__":
    main()
