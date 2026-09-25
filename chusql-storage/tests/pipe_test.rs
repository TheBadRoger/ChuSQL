use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, Stdio};
use std::thread;
use std::time::Duration;

use interprocess::local_socket::{
    prelude::*,
    GenericNamespaced
};
use interprocess::TryClone;

// 命名管道集成测试：起真 server，跑协议、索引与配置。

/// 唯一管道名，避免测试互抢
fn unique_pipe_name() -> String {
    let id = std::process::id();
    let ns = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    format!("chusql-test-{}-{}", id, ns)
}

/// 持有 server 子进程，退出时杀掉
struct ServerProc(Child);

impl Drop for ServerProc {
    /// 测试结束杀掉子进程
    fn drop(&mut self) {
        let _ = self.0.kill();
        let _ = self.0.wait();
    }
}

/// 起 server，等管道可连
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

/// 连到指定管道
fn connect(pipe: &str) -> std::io::Result<LocalSocketStream> {
    let name = pipe
        .to_ns_name::<GenericNamespaced>()
        .map_err(|e| std::io::Error::other(e.to_string()))?;
    LocalSocketStream::connect(name)
}

/// 发一行请求读一行响应
fn send(stream: &mut LocalSocketStream, line: &str) -> String {
    stream.write_all(line.as_bytes()).unwrap();
    stream.write_all(b"\n").unwrap();
    stream.flush().unwrap();

    let mut reader = BufReader::new(stream.try_clone().unwrap());
    let mut buf = String::new();
    reader.read_line(&mut buf).unwrap();
    buf.trim().to_string()
}

/// ping 回 pong
#[test]
fn ping_pong() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);

    let mut c = connect(&pipe).unwrap();
    assert_eq!(send(&mut c, r#"{"method":"ping"}"#), r#"{"status":"pong"}"#);
}

/// 插入后能扫到
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

/// 不存在的表报错
#[test]
fn scan_unknown_table_errors() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    let r = send(&mut c, r#"{"method":"scan","table":"nope"}"#);
    assert!(r.contains(r#""status":"error""#), "got {}", r);
    assert!(r.contains("unknown table"), "got {}", r);
}

/// 列出已建的表
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

/// 整表替换只剩新行
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

/// 按 id 查（列名走默认值 `id`，老客户端不用改）
#[test]
fn insert_then_lookup_by_index() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    for (id, name) in [(1, "Alice"), (2, "Bob"), (3, "Carol")] {
        let req = format!(
            r#"{{"method":"insert","table":"idx_users","row":{{"id":{},"name":"{}"}}}}"#,
            id, name
        );
        let r = send(&mut c, &req);
        assert!(r.contains(r#""status":"ok""#), "insert: {}", r);
    }

    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"idx_users","key":2}"#);
    assert!(r.contains("Bob"), "lookup: {}", r);

    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"idx_users","key":99}"#);
    assert!(r.contains(r#""rows":[]"#), "miss: {}", r);
}

/// 插入时索引由表自己维护：没给 key，id 也会进索引
#[test]
fn insert_indexes_the_id_column_automatically() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    send(&mut c, r#"{"method":"insert","table":"auto_idx","row":{"id":7,"name":"Zoe"}}"#);
    let r = send(
        &mut c,
        r#"{"method":"lookup_by_index","table":"auto_idx","column":"id","key":7}"#,
    );
    assert!(r.contains("Zoe"), "got: {}", r);
}

/// 同一个 id 插两次要报错（索引列是唯一的）
#[test]
fn duplicate_id_is_rejected() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    let req = r#"{"method":"insert","table":"dup_id","row":{"id":1,"name":"A"}}"#;
    assert!(send(&mut c, req).contains(r#""status":"ok""#));

    let r = send(&mut c, r#"{"method":"insert","table":"dup_id","row":{"id":1,"name":"B"}}"#);
    assert!(r.contains(r#""status":"error""#), "got: {}", r);
    assert!(r.contains("duplicate key"), "got: {}", r);
}

/// 批量插入一次全进
#[test]
fn insert_batch_writes_all_rows() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    let r = send(
        &mut c,
        r#"{"method":"insert_batch","table":"batch_t","rows":[{"id":1,"name":"A"},{"id":2,"name":"B"},{"id":3,"name":"C"}]}"#,
    );
    assert!(r.contains(r#""status":"ok""#), "batch: {}", r);

    let r = send(&mut c, r#"{"method":"scan","table":"batch_t"}"#);
    assert!(r.contains("A") && r.contains("B") && r.contains("C"), "scan: {}", r);

    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"batch_t","key":2}"#);
    assert!(r.contains("B"), "index after batch: {}", r);
}

/// 行级删除：行没了、索引条目也没了、别人不受影响
#[test]
fn delete_keys_removes_row_and_index_entry() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    for (id, name) in [(1, "Alice"), (2, "Bob"), (3, "Carol")] {
        let req = format!(
            r#"{{"method":"insert","table":"del_t","row":{{"id":{},"name":"{}"}}}}"#,
            id, name
        );
        send(&mut c, &req);
    }

    let r = send(&mut c, r#"{"method":"delete_keys","table":"del_t","keys":[2]}"#);
    assert!(r.contains(r#""status":"ok""#), "delete: {}", r);

    let r = send(&mut c, r#"{"method":"scan","table":"del_t"}"#);
    assert!(!r.contains("Bob"), "row should be gone: {}", r);
    assert!(r.contains("Alice") && r.contains("Carol"), "others stay: {}", r);

    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"del_t","key":2}"#);
    assert!(r.contains(r#""rows":[]"#), "index entry should be gone: {}", r);

    let r = send(&mut c, r#"{"method":"insert","table":"del_t","row":{"id":2,"name":"Bob2"}}"#);
    assert!(r.contains(r#""status":"ok""#), "id 2 应该能再插进来: {}", r);
}

/// 给第二列建索引后能按那一列查
#[test]
fn create_index_on_secondary_column() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    for (id, code) in [(1, 5001), (2, 5002), (3, 5003)] {
        let req = format!(
            r#"{{"method":"insert","table":"sec_t","row":{{"id":{},"code":{}}}}}"#,
            id, code
        );
        send(&mut c, &req);
    }

    // 没建之前：明确回 no_index，而不是回空结果
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"sec_t","column":"code","key":5002}"#);
    assert!(r.contains(r#""status":"no_index""#), "got: {}", r);

    let r = send(&mut c, r#"{"method":"create_index","table":"sec_t","column":"code"}"#);
    assert!(r.contains(r#""status":"ok""#), "create_index: {}", r);

    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"sec_t","column":"code","key":5002}"#);
    assert!(r.contains(r#""id":2"#), "lookup by code: {}", r);

    // 建完索引之后新插入的行也要进索引
    send(&mut c, r#"{"method":"insert","table":"sec_t","row":{"id":4,"code":5004}}"#);
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"sec_t","column":"code","key":5004}"#);
    assert!(r.contains(r#""id":4"#), "lookup new row: {}", r);

    // schema 里能看到这条索引
    let r = send(&mut c, r#"{"method":"describe_table","table":"sec_t"}"#);
    assert!(r.contains(r#""column":"code""#), "describe: {}", r);

    // 删掉索引后回到 no_index
    let r = send(&mut c, r#"{"method":"drop_index","table":"sec_t","column":"code"}"#);
    assert!(r.contains(r#""status":"ok""#), "drop_index: {}", r);
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"sec_t","column":"code","key":5002}"#);
    assert!(r.contains(r#""status":"no_index""#), "after drop: {}", r);
}

/// 有重复值的列不给建索引（不然"走索引"和"全表扫"会给出不同答案）
#[test]
fn create_index_rejects_duplicate_values() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    send(&mut c, r#"{"method":"insert","table":"dup_t","row":{"id":1,"age":30}}"#);
    send(&mut c, r#"{"method":"insert","table":"dup_t","row":{"id":2,"age":30}}"#);

    let r = send(&mut c, r#"{"method":"create_index","table":"dup_t","column":"age"}"#);
    assert!(r.contains(r#""status":"error""#), "got: {}", r);
    assert!(r.contains("duplicate value"), "got: {}", r);

    // 失败之后不留半成品：查这一列仍然是 no_index
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"dup_t","column":"age","key":30}"#);
    assert!(r.contains(r#""status":"no_index""#), "got: {}", r);
}

/// 数据字典里的统计：不同值个数
#[test]
fn stats_report_distinct_values() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    for (id, age) in [(1, 30), (2, 30), (3, 41)] {
        let req = format!(
            r#"{{"method":"insert","table":"stat_t","row":{{"id":{},"age":{}}}}}"#,
            id, age
        );
        send(&mut c, &req);
    }

    let r = send(&mut c, r#"{"method":"describe_table","table":"stat_t"}"#);
    assert!(r.contains(r#""row_count":3"#), "row_count: {}", r);
    assert!(r.contains(r#""name":"age","distinct":2"#), "age 有 2 个不同值: {}", r);
    assert!(r.contains(r#""name":"id","distinct":3"#), "id 有 3 个不同值: {}", r);

    // 删掉一行，统计跟着掉
    send(&mut c, r#"{"method":"delete_keys","table":"stat_t","keys":[1]}"#);
    let r = send(&mut c, r#"{"method":"describe_table","table":"stat_t"}"#);
    assert!(r.contains(r#""row_count":2"#), "after delete: {}", r);
    assert!(r.contains(r#""name":"age","distinct":1"#), "after delete: {}", r);
}

/// 重开服务后索引仍在
#[test]
fn lookup_survives_reopen() {
    let pipe = unique_pipe_name();
    let data = tempfile::tempdir().unwrap();

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
        drop(srv);
    }

    thread::sleep(Duration::from_millis(200));

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

/// 配置文件真的生效
#[test]
fn config_file_is_used() {
    let dir = tempfile::tempdir().unwrap();
    let pipe = unique_pipe_name();
    let data_dir = dir.path().join("data");
    let config_path = dir.path().join("chusql-storage.toml");

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

    assert!(
        data_dir.join("cfg.db").exists(),
        "data dir from config file was not used"
    );
}

/// 建表后能查 schema
#[test]
fn create_table_then_describe() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    let r = send(
        &mut c,
        r#"{"method":"create_table","table":"ct_users","columns":[{"name":"id","ty":"int"},{"name":"name","ty":"str"}]}"#,
    );
    assert!(r.contains(r#""status":"ok""#), "create: {}", r);

    let r = send(&mut c, r#"{"method":"describe_table","table":"ct_users"}"#);
    assert!(r.contains("id"), "describe: {}", r);
    assert!(r.contains("name"), "describe: {}", r);
    assert!(r.contains(r#""ty":"int""#), "describe: {}", r);
    assert!(r.contains(r#""ty":"str""#), "describe: {}", r);
}

/// 重复建表报错
#[test]
fn create_table_twice_errors() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    let req = r#"{"method":"create_table","table":"dup","columns":[{"name":"id","ty":"int"}]}"#;
    let r1 = send(&mut c, req);
    assert!(r1.contains(r#""status":"ok""#), "first: {}", r1);

    let r2 = send(&mut c, req);
    assert!(r2.contains(r#""status":"error""#), "second: {}", r2);
    assert!(r2.contains("already exists"), "second: {}", r2);
}

/// 新建空表能扫描
#[test]
fn scan_empty_table_after_create() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    let r = send(
        &mut c,
        r#"{"method":"create_table","table":"empty_t","columns":[{"name":"id","ty":"int"}]}"#,
    );
    assert!(r.contains(r#""status":"ok""#), "create: {}", r);

    let r = send(&mut c, r#"{"method":"scan","table":"empty_t"}"#);
    assert!(r.contains(r#""status":"rows""#), "scan: {}", r);
    assert!(r.contains(r#""rows":[]"#), "scan: {}", r);
}
