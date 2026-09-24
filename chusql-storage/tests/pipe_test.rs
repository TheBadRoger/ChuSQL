use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, Stdio};
use std::thread;
use std::time::Duration;

use interprocess::local_socket::{
    prelude::*,
    GenericNamespaced
};
use interprocess::TryClone;

// 命名管道集成测试：起 server 子进程，用唯一管道名，测试之间互不干扰。

/// 生成唯一管道名，避免测试之间抢占。
fn unique_pipe_name() -> String {
    let id = std::process::id();
    let ns = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    format!("chusql-test-{}-{}", id, ns)
}

/// 持有 server 子进程，Drop 时杀掉。
struct ServerProc(Child);

impl Drop for ServerProc {
    /// 测试结束时杀掉子进程并回收，避免留下孤儿进程。
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

/// 启动 server 子进程，并等管道可连（最多 5 秒）。
fn start_server(pipe: &str) -> (ServerProc, tempfile::TempDir) {
    let data = tempfile::tempdir().unwrap();

    let child = Command::new(env!("CARGO_BIN_EXE_server"))
        .env("CHUSQL_PIPE", pipe)
        .env("CHUSQL_DATA_DIR", data.path())
        .env("CHUSQL_PAGE_SIZE", "4096")
        .env("CHUSQL_BTREE_ORDER", "4")
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("failed to spawn server");

    let srv = ServerProc(child);

    for _ in 0..100 {
        if connect(pipe).is_ok() {
            return (srv, data);
        }
        thread::sleep(Duration::from_millis(50));
    }
    panic!("server did not start within 5s");
}

/// 连到指定管道。
fn connect(pipe: &str) -> std::io::Result<LocalSocketStream> {
    let name = pipe
        .to_ns_name::<GenericNamespaced>()
        .map_err(|e| std::io::Error::other(e.to_string()))?;
    LocalSocketStream::connect(name)
}

/// 发一行请求，读一行响应。
fn send(stream: &mut LocalSocketStream, line: &str) -> String {
    stream.write_all(line.as_bytes()).unwrap();
    stream.write_all(b"\n").unwrap();
    stream.flush().unwrap();

    let mut reader = BufReader::new(stream.try_clone().unwrap());
    let mut buf = String::new();
    reader.read_line(&mut buf).unwrap();
    buf.trim().to_string()
}

/// ping 应该回 pong。
#[test]
fn ping_pong() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);

    let mut c = connect(&pipe).unwrap();
    assert_eq!(send(&mut c, r#"{"method":"ping"}"#), r#"{"status":"pong"}"#);
}

/// 插一行再 scan，能扫到刚插进去的数据。
#[test]
fn insert_then_scan() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    let r1 = send(
        &mut c,
        r#"{"method":"insert","table":"users","row":{"id":1,"name":"Alice"}}"#,
    );
    assert!(r1.contains(r#""status":"ok""#), "insert response: {}", r1);

    let r2 = send(&mut c, r#"{"method":"scan","table":"users"}"#);
    assert!(r2.contains("Alice"), "scan response: {}", r2);
}

#[test]
fn scan_unknown_table_errors() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    let r = send(&mut c, r#"{"method":"scan","table":"nope"}"#);
    assert!(r.contains(r#""status":"error""#), "got {}", r);
    assert!(r.contains("unknown table"), "got {}", r);
}

#[test]
fn list_tables_returns_created_tables() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    send(&mut c, r#"{"method":"insert","table":"users","row":{"id":1}}"#);
    send(&mut c, r#"{"method":"insert","table":"orders","row":{"id":1}}"#);

    let r = send(&mut c, r#"{"method":"list_tables"}"#);
    assert!(r.contains("users"), "got {}", r);
    assert!(r.contains("orders"), "got {}", r);
}

#[test]
fn replace_all_replaces_rows() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    send(&mut c, r#"{"method":"insert","table":"users","row":{"id":1,"name":"Alice"}}"#);
    send(&mut c, r#"{"method":"insert","table":"users","row":{"id":2,"name":"Bob"}}"#);

    let r = send(
        &mut c,
        r#"{"method":"replace_all","table":"users","rows":[{"id":9,"name":"Zoe"}]}"#,
    );
    assert!(r.contains(r#""status":"ok""#), "replace_all response: {}", r);

    let r = send(&mut c, r#"{"method":"scan","table":"users"}"#);
    assert!(r.contains("Zoe"), "got {}", r);
    assert!(!r.contains("Alice"), "old row should be gone: {}", r);
}

#[test]
fn insert_with_key_then_lookup() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    // 带索引键插三行
    for (id, name) in [(1, "Alice"), (2, "Bob"), (3, "Carol")] {
        let req = format!(
            r#"{{"method":"insert","table":"idx_users","row":{{"id":{},"name":"{}"}},"key":{}}}"#,
            id, name, id
        );
        let r = send(&mut c, &req);
        assert!(r.contains(r#""status":"ok""#), "insert: {}", r);
    }

    // 命中
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"idx_users","key":2}"#);
    assert!(r.contains("Bob"), "lookup: {}", r);

    // 未命中
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"idx_users","key":99}"#);
    assert!(r.contains(r#""rows":[]"#), "miss: {}", r);
}

#[test]
fn insert_without_key_does_not_index() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    send(&mut c, r#"{"method":"insert","table":"nokey","row":{"id":1}}"#);
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"nokey","key":1}"#);
    assert!(r.contains(r#""rows":[]"#), "got: {}", r);
}

#[test]
fn lookup_survives_reopen() {
    let pipe = unique_pipe_name();
    let data = tempfile::tempdir().unwrap();

    // 第一次：写 3 行带 key
    {
        let exe = env!("CARGO_BIN_EXE_server");
        let child = Command::new(exe)
            .env("CHUSQL_PIPE", &pipe)
            .env("CHUSQL_DATA_DIR", data.path())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .unwrap();
        let srv = ServerProc(child);
        for _ in 0..100 {
            if connect(&pipe).is_ok() { break; }
            thread::sleep(Duration::from_millis(50));
        }
        let mut c = connect(&pipe).unwrap();
        for (id, name) in [(1, "Alice"), (2, "Bob"), (3, "Carol")] {
            let req = format!(
                r#"{{"method":"insert","table":"persist","row":{{"id":{},"name":"{}"}},"key":{}}}"#,
                id, name, id
            );
            send(&mut c, &req);
        }
        drop(srv);  // 杀 server
    }

    // 等一下，让文件句柄真正释放
    thread::sleep(Duration::from_millis(200));

    // 第二次：重开 server，从索引查到
    let (_srv, _data) = {
        let child = Command::new(env!("CARGO_BIN_EXE_server"))
            .env("CHUSQL_PIPE", &pipe)
            .env("CHUSQL_DATA_DIR", data.path())
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .unwrap();
        let srv = ServerProc(child);
        for _ in 0..100 {
            if connect(&pipe).is_ok() { break; }
            thread::sleep(Duration::from_millis(50));
        }
        (srv, data)
    };

    let mut c = connect(&pipe).unwrap();
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"persist","key":2}"#);
    assert!(r.contains("Bob"), "persist lookup: {}", r);
}

/// 配置文件里的管道名和数据目录要真的生效（这里不设 CHUSQL_PIPE / CHUSQL_DATA_DIR）。
#[test]
fn config_file_is_used() {
    let dir = tempfile::tempdir().unwrap();
    let pipe = unique_pipe_name();
    let data_dir = dir.path().join("data");
    let config_path = dir.path().join("chusql-storage.toml");

    // TOML 里写正斜杠，省得转义 Windows 路径
    let text = format!(
        "[server]\npipe_name = \"{}\"\n[storage]\ndata_dir = \"{}\"\n[log]\nlevel = \"debug\"\n",
        pipe,
        data_dir.display().to_string().replace('\\', "/")
    );
    std::fs::write(&config_path, text).unwrap();

    let child = Command::new(env!("CARGO_BIN_EXE_server"))
        .env("CHUSQL_CONFIG", &config_path)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("failed to spawn server");
    let _srv = ServerProc(child);
    for _ in 0..100 {
        if connect(&pipe).is_ok() {
            break;
        }
        thread::sleep(Duration::from_millis(50));
    }

    let mut c = connect(&pipe).unwrap();
    assert_eq!(send(&mut c, r#"{"method":"ping"}"#), r#"{"status":"pong"}"#);
    let r = send(&mut c, r#"{"method":"insert","table":"cfg","row":{"id":1}}"#);
    assert!(r.contains(r#""status":"ok""#), "insert: {}", r);

    // 数据真的落在配置文件指的那个目录里
    assert!(
        data_dir.join("cfg.db").exists(),
        "data dir from config file was not used"
    );
}
