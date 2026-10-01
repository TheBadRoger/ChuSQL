use std::io::{BufRead, BufReader, Write};
use std::process::{Child, Command, Stdio};
use std::thread;
use std::time::Duration;

use interprocess::local_socket::prelude::*;
use interprocess::TryClone;

// 端点集成测试：起真 server，跑协议、索引与配置（端点由 endpoint 模块统一算）。

#[test]
fn scan_projects_columns_and_preserves_empty_rows() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"items","columns":[{"name":"id","ty":"int"},{"name":"name","ty":"str"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert_batch","table":"items","rows":[{"id":1,"name":"one"},{"id":2,"name":null}]}))?;
    for (columns, expected) in [
        (serde_json::json!(["name"]), serde_json::json!([{"name":"one"},{"name":null}])),
        (serde_json::json!([]), serde_json::json!([{},{}])),
    ] {
        let result: serde_json::Value = serde_json::from_str(&send(&mut c, &serde_json::json!({"method":"scan","table":"items","columns":columns}).to_string()))?;
        assert_eq!(result["rows"], expected);
    }
    let bad: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items","columns":["missing"]}"#))?;
    assert_eq!(bad["status"], "error");
    Ok(())
}

#[test]
fn drop_column_keeps_heap_bytes_and_hides_values_after_restart() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (server, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"items","columns":[{"name":"id","ty":"int"},{"name":"name","ty":"str"},{"name":"age","ty":"int"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert_batch","table":"items","rows":[{"id":1,"name":"old","age":20},{"id":2,"name":"secret","age":30}]}))?;
    for column in ["name", "age"] {
        request_ok(&mut c, serde_json::json!({"method":"create_index","table":"items","column":column}))?;
    }
    let heap = data.path().join("databases/main/items.db");
    let before = std::fs::read(&heap)?;
    request_ok(&mut c, serde_json::json!({"method":"drop_column","table":"items","column":"name"}))?;
    assert_eq!(std::fs::read(&heap)?, before, "DROP COLUMN must not rewrite heap pages");
    drop(c);
    drop(server);
    let _server = start_server_in(&pipe, data.path());
    let mut c = connect(&pipe)?;
    for request in [
        serde_json::json!({"method":"scan","table":"items"}),
        serde_json::json!({"method":"range_by_index","table":"items","column":"age","lo":0}),
    ] {
        let result: serde_json::Value = serde_json::from_str(&send(&mut c, &request.to_string()))?;
        assert_eq!(result["rows"], serde_json::json!([{"id":1,"age":20},{"id":2,"age":30}]));
    }
    let lookup: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"lookup_by_index","table":"items","column":"id","key":1}"#))?;
    assert_eq!(lookup["rows"], serde_json::json!([{"id":1,"age":20}]));
    let schema: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"describe_table","table":"items"}"#))?;
    assert_eq!(schema["row_count"], 2);
    assert!(schema["columns"].as_array().unwrap().iter().all(|col| col["name"] != "name"));
    assert!(schema["stats"].as_array().unwrap().iter().all(|col| col["name"] != "name"));
    let bad: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"insert","table":"items","row":{"id":3,"name":"resurrect"}}"#))?;
    assert_eq!(bad["status"], "error");
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":3,"age":40}}))?;
    Ok(())
}

#[test]
fn databases_isolate_catalog_and_rows() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (_srv, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    let request = |c: &mut LocalSocketStream, value: serde_json::Value| -> Result<serde_json::Value, serde_json::Error> {
        serde_json::from_str(&send(c, &value.to_string()))
    };
    for database in ["alpha", "beta"] {
        assert_eq!(request(&mut c, serde_json::json!({"method":"create_database","database":database}))?["status"], "ok");
        assert_eq!(request(&mut c, serde_json::json!({"method":"create_table","database":database,"table":"items","columns":[{"name":"id","ty":"int"}]}))?["status"], "ok");
    }
    assert_eq!(request(&mut c, serde_json::json!({"method":"insert","database":"alpha","table":"items","row":{"id":1}}))?["status"], "ok");
    assert_eq!(request(&mut c, serde_json::json!({"method":"scan","database":"alpha","table":"items"}))?["rows"], serde_json::json!([{"id":1}]));
    assert_eq!(request(&mut c, serde_json::json!({"method":"scan","database":"beta","table":"items"}))?["rows"], serde_json::json!([]));
    assert_eq!(request(&mut c, serde_json::json!({"method":"list_tables"}))?["tables"], serde_json::json!([]));
    assert!(data.path().join("databases/main/catalog.json").is_file());
    assert!(data.path().join("databases/alpha/catalog.json").is_file());
    assert!(data.path().join("databases/beta/catalog.json").is_file());
    assert_eq!(request(&mut c, serde_json::json!({"method":"drop_database","database":"alpha"}))?["status"], "ok");
    assert_eq!(request(&mut c, serde_json::json!({"method":"scan","database":"alpha","table":"items"}))?["status"], "error");
    assert_eq!(request(&mut c, serde_json::json!({"method":"scan","database":"beta","table":"items"}))?["rows"], serde_json::json!([]));
    Ok(())
}


#[test]
fn hidden_column_wal_replays_twice_without_touching_heap() -> Result<(), Box<dyn std::error::Error>> {
    use chusql_storage::wal::{Wal, WalOp};
    let pipe = unique_pipe_name();
    let (server, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":1,"secret":"old"}}))?;
    drop(c);
    drop(server);
    let heap = data.path().join("databases/main/items.db");
    let mut compacted: Option<Vec<u8>> = None;
    for round in 0..2 {
        let wal = Wal::new(data.path().join("databases/main/wal.log"));
        let op = WalOp::HideColumn { table: "items".into(), column: "secret".into() };
        wal.truncate()?;
        wal.append(&op)?;
        assert_eq!(wal.read()?, Some(op));
        assert!(std::fs::metadata(wal.path())?.len() < 100);
        drop(wal);
        let server = start_server_in(&pipe, data.path());
        let mut c = connect(&pipe)?;
        let result: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items"}"#))?;
        assert_eq!(result["rows"], serde_json::json!([{"id":1}]));
        let after = std::fs::read(&heap)?;
        assert!(
            !after.windows(6).any(|w| w == &b"secret"[..]),
            "启动整理要把隐藏列的字节清掉"
        );
        if round == 1 {
            assert_eq!(Some(after.clone()), compacted, "整理过的表第二次启动不该再重写");
        }
        compacted = Some(after);
        assert_eq!(std::fs::metadata(data.path().join("databases/main/wal.log"))?.len(), 0);
        drop(c);
        drop(server);
    }
    Ok(())
}

#[test]
fn hidden_column_failure_preserves_wal_until_recovery() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (server, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":1,"secret":"old"}}))?;
    let index = data.path().join("databases/main/items.secret.idx");
    std::fs::create_dir(&index)?;
    let failed: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"drop_column","table":"items","column":"secret"}"#))?;
    assert_eq!(failed["status"], "error");
    assert!(std::fs::metadata(data.path().join("databases/main/wal.log"))?.len() > 0);
    let blocked: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"insert","table":"items","row":{"id":2}}"#))?;
    assert_eq!(blocked["status"], "error");
    assert!(blocked["message"].as_str().unwrap().contains("recovery required"));
    drop(c);
    drop(server);
    std::fs::remove_dir(index)?;
    let _server = start_server_in(&pipe, data.path());
    let mut c = connect(&pipe)?;
    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":1}]));
    Ok(())
}

#[test]
fn drop_column_with_a_live_index_removes_the_index_file() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (_srv, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":1,"code":7}}))?;
    request_ok(&mut c, serde_json::json!({"method":"create_index","table":"items","column":"code"}))?;
    let index = data.path().join("databases/main/items.code.idx");
    assert!(index.is_file(), "index file exists after create_index");

    let dropped: serde_json::Value = serde_json::from_str(&send(
        &mut c,
        r#"{"method":"drop_column","table":"items","column":"code"}"#,
    ))?;
    assert_eq!(dropped["status"], "ok", "drop_column with a live index: {dropped}");
    assert!(!index.exists(), "index file is removed");

    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":1}]));
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":2}}))?;
    Ok(())
}

#[test]
fn compact_reclaims_hidden_column_bytes() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (server, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    for id in 1..=3 {
        request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":id,"secret":"padding","note":"n"}}))?;
    }
    request_ok(&mut c, serde_json::json!({"method":"drop_column","table":"items","column":"secret"}))?;
    let heap = data.path().join("databases/main/items.db");
    assert!(std::fs::read(&heap)?.windows(6).any(|w| w == &b"secret"[..]), "fast path keeps the bytes");

    request_ok(&mut c, serde_json::json!({"method":"compact","table":"items"}))?;
    assert!(!std::fs::read(&heap)?.windows(6).any(|w| w == &b"secret"[..]), "compaction rewrites the pages");
    assert_eq!(std::fs::metadata(data.path().join("databases/main/wal.log"))?.len(), 0);

    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items"}"#))?;
    assert_eq!(
        rows["rows"],
        serde_json::json!([{"id":1,"note":"n"},{"id":2,"note":"n"},{"id":3,"note":"n"}])
    );
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":4,"note":"x"}}))?;
    drop(c);
    drop(server);
    let _server = start_server_in(&pipe, data.path());
    let mut c = connect(&pipe)?;
    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items"}"#))?;
    assert_eq!(rows["rows"].as_array().map(Vec::len), Some(4), "rows survive compaction: {}", rows);
    Ok(())
}

#[test]
fn startup_compaction_reclaims_dropped_columns() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (server, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":1,"secret":"padding"}}))?;
    request_ok(&mut c, serde_json::json!({"method":"drop_column","table":"items","column":"secret"}))?;
    let heap = data.path().join("databases/main/items.db");
    assert!(std::fs::read(&heap)?.windows(6).any(|w| w == &b"secret"[..]));
    drop(c);
    drop(server);

    let _server = start_server_in(&pipe, data.path());
    let mut c = connect(&pipe)?;
    assert!(
        !std::fs::read(&heap)?.windows(6).any(|w| w == &b"secret"[..]),
        "startup must compact tables that still carry hidden columns"
    );
    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":1}]));
    Ok(())
}

#[test]
fn databases_reject_unsafe_names_and_keep_only_system_reserved() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (_srv, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    for database in ["", "../escape", "a/b", "a.b", "CON", "__chusql_users"] {
        let response: serde_json::Value = serde_json::from_str(&send(&mut c, &serde_json::json!({"method":"create_database","database":database}).to_string()))?;
        assert_eq!(response["status"], "error", "{database}: {response}");
    }
    // system 是唯一的保留库：不能建、不能删
    for method in ["create_database", "drop_database"] {
        let response: serde_json::Value = serde_json::from_str(&send(&mut c, &serde_json::json!({"method":method,"database":"system"}).to_string()))?;
        assert_eq!(response["status"], "error", "{method}: {response}");
    }
    // test 不再是保留名：能建、也能删（不再有默认工作库）
    let created: serde_json::Value = serde_json::from_str(&send(&mut c, &serde_json::json!({"method":"create_database","database":"test"}).to_string()))?;
    assert_eq!(created["status"], "ok", "{created}");
    assert!(data.path().join("databases/test/catalog.json").is_file());
    let dropped: serde_json::Value = serde_json::from_str(&send(&mut c, &serde_json::json!({"method":"drop_database","database":"test"}).to_string()))?;
    assert_eq!(dropped["status"], "ok", "{dropped}");
    assert!(!data.path().join("databases/test").exists());
    assert!(data.path().join("system/catalog.json").is_file());
    Ok(())
}

#[test]
fn system_database_owns_accounts_and_nothing_is_default() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (_srv, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    // 启动只建 system，另一个是测试自己建的 main
    let listed: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"list_databases"}"#))?;
    assert_eq!(listed["tables"], serde_json::json!(["main", "system"]));
    let created: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"account_create","user":"alice","password_hash":"hash-1"}"#))?;
    assert_eq!(created["status"], "accounts", "{created}");
    assert!(data.path().join("system/__chusql_users.db").is_file());
    // 账号是全局的：请求里带的库名对账号操作不起作用
    let accounts: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"accounts_list","database":"sales"}"#))?;
    assert_eq!(accounts["status"], "accounts");
    assert_eq!(accounts["accounts"][0]["user"], "alice");
    // 裸表名落到选中的库（测试补的是 main）
    assert_eq!(request_ok(&mut c, serde_json::json!({"method":"create_table","table":"items","columns":[{"name":"id","ty":"int"}]}))?, ());
    assert!(data.path().join("databases/main/items.db").is_file());
    assert!(!data.path().join("items.db").exists());
    assert!(!data.path().join("test").exists());
    Ok(())
}

/// 没选库的裸表名请求会被拒；限定名（db.table）不用选库
#[test]
fn unqualified_requests_need_a_selected_database() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    let response: serde_json::Value = serde_json::from_str(&send_raw(&mut c, r#"{"method":"scan","table":"items"}"#))?;
    assert_eq!(response["status"], "error", "{response}");
    assert!(response["message"].as_str().unwrap_or("").contains("no database selected"), "{response}");
    let created: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"create_table","table":"items","columns":[{"name":"id","ty":"int"}]}"#))?;
    assert_eq!(created["status"], "ok", "{created}");
    let qualified: serde_json::Value = serde_json::from_str(&send_raw(&mut c, r#"{"method":"scan","table":"main.items"}"#))?;
    assert_eq!(qualified["status"], "rows", "{qualified}");
    let unknown: serde_json::Value = serde_json::from_str(&send_raw(&mut c, r#"{"method":"scan","table":"ghost.items"}"#))?;
    assert_eq!(unknown["status"], "error", "{unknown}");
    assert!(unknown["message"].as_str().unwrap_or("").contains("unknown database"), "{unknown}");
    Ok(())
}

/// 发一条请求并断言状态是 ok
fn request_ok(c: &mut LocalSocketStream, value: serde_json::Value) -> Result<(), Box<dyn std::error::Error>> {
    let response: serde_json::Value = serde_json::from_str(&send(c, &value.to_string()))?;
    assert_eq!(response["status"], "ok", "{value}: {response}");
    Ok(())
}

/// 唯一管道名，避免测试互抢
fn unique_pipe_name() -> String {
    let id = std::process::id();
    let ns = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    format!("chusql-test-{}-{}", id, ns)
}

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
    let server = start_server_in(pipe, data.path());
    (server, data)
}

fn start_server_in(pipe: &str, data: &std::path::Path) -> ServerProc {
    let config = write_config(data, pipe);
    let child = Command::new(env!("CARGO_BIN_EXE_chusql-storage"))
        .arg("--config")
        .arg(&config)
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("failed to spawn server");

    let srv = ServerProc(child);

    for _ in 0..100 {
        if connect(pipe).is_ok() {
            // 服务启动只建 system 库，测试库在这里显式建
            create_fixture_database(pipe);
            return srv;
        }
        thread::sleep(Duration::from_millis(50));
    }
    panic!("server did not start within 5s");
}

/// 写一份配置给子进程：管名与数据目录都走 TOML，
/// 路径用单引号字面量串，免得 Windows 反斜杠要转义。
fn write_config(data: &std::path::Path, pipe: &str) -> std::path::PathBuf {
    let id = std::process::id();
    let ns = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let path = data.join(format!("chusql-{}-{}.toml", id, ns));
    let text = format!(
        "[server]\npipe_name = '{}'\n[storage]\ndata_dir = '{}'\n",
        pipe,
        data.display()
    );
    std::fs::write(&path, text).unwrap();
    path
}

#[test]
fn named_database_survives_restart_and_accounts_stay_global() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (server, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    for request in [
        serde_json::json!({"method":"create_database","database":"sales"}),
        serde_json::json!({"method":"create_table","database":"sales","table":"items","columns":[{"name":"id","ty":"int"}]}),
        serde_json::json!({"method":"insert","database":"sales","table":"items","row":{"id":7}}),
    ] {
        let result: serde_json::Value = serde_json::from_str(&send(&mut c, &request.to_string()))?;
        assert_eq!(result["status"], "ok");
    }
    drop(c);
    drop(server);
    let _restarted = start_server_in(&pipe, data.path());
    let mut c = connect(&pipe)?;
    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"sales.items"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":7}]));
    let accounts: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"accounts_list","database":"sales"}"#))?;
    assert_eq!(accounts["status"], "accounts");
    let hidden: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"sales.__chusql_users"}"#))?;
    assert_eq!(hidden["status"], "error");
    Ok(())
}

/// 连到指定端点的存储进程。端点规则与 server 侧共用 endpoint 模块，
/// 所以 Windows 的具名管道与 Unix 的套接字文件都由同一处决定。
fn connect(pipe: &str) -> std::io::Result<LocalSocketStream> {
    chusql_storage::endpoint::connect(pipe)
}

/// 测试自己的库名：服务启动只建 system 库，测试显式建这一个
const FIXTURE_DATABASE: &str = "main";

/// 请求里没写库名就补上测试库：服务不再自带默认库，裸表名必须先选库
fn with_default_database(line: &str) -> String {
    match serde_json::from_str::<serde_json::Value>(line) {
        Ok(serde_json::Value::Object(mut object)) => {
            object.entry("database").or_insert_with(|| serde_json::Value::String(FIXTURE_DATABASE.to_string()));
            serde_json::Value::Object(object).to_string()
        }
        _ => line.to_string(),
    }
}

/// 连通后显式建测试库；重启时库已存在，报错忽略
fn create_fixture_database(pipe: &str) {
    if let Ok(mut stream) = connect(pipe) {
        let _ = send_raw(&mut stream, &format!(r#"{{"method":"create_database","database":"{FIXTURE_DATABASE}"}}"#));
    }
}

/// 发一行请求读一行响应（没写库名就补测试库）
fn send(stream: &mut LocalSocketStream, line: &str) -> String {
    send_raw(stream, &with_default_database(line))
}

/// 原样发一行请求，不补库名——用来验证"没选库"的行为
fn send_raw(stream: &mut LocalSocketStream, line: &str) -> String {
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
fn catalog_is_the_only_table_authority() -> std::io::Result<()> {
    let pipe = unique_pipe_name();
    let (_srv, data) = start_server(&pipe);
    std::fs::write(data.path().join("databases/main/orphan.db"), b"orphan")?;
    let mut c = connect(&pipe)?;
    for method in ["scan", "describe_table"] {
        let result = send(&mut c, &format!(r#"{{"method":"{method}","table":"orphan"}}"#));
        assert!(result.contains("unknown table"), "{result}");
    }
    let result = send(&mut c, r#"{"method":"list_tables"}"#);
    assert!(!result.contains("orphan"), "{result}");
    Ok(())
}

#[test]
fn reserved_tables_reject_every_generic_operation() -> std::io::Result<()> {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    for method in ["scan", "insert", "insert_batch", "delete_keys", "lookup_by_index",
        "replace_all", "describe_table", "create_table", "drop_table", "create_index",
        "drop_index", "drop_column"] {
        let request = serde_json::json!({"method": method, "table": "__chusql_users",
            "column": "id", "key": 1, "keys": [], "row": {"id": 1}, "rows": [], "columns": []});
        let result = send(&mut c, &request.to_string());
        assert!(result.contains("reserved system table"), "{method}: {result}");
    }
    Ok(())
}

#[test]
fn generic_ipc_rejects_path_aliases() -> std::io::Result<()> {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    for table in ["../__chusql_users", "__CHUSQL_USERS", "__chusql_users."] {
        let result = send(&mut c, &serde_json::json!({"method":"scan", "table":table}).to_string());
        assert!(result.contains("error"), "{result}");
    }
    let result = send(&mut c, r#"{"method":"create_index","table":"business","column":"../__chusql_users"}"#);
    assert!(result.contains("invalid column name"), "{result}");
    Ok(())
}

#[test]
fn account_create_reset_drop_and_hidden_from_ordinary_api() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    assert_eq!(serde_json::from_str::<serde_json::Value>(&send(&mut c, r#"{"method":"accounts_list"}"#))?["accounts"], serde_json::json!([]));
    let create = r#"{"method":"account_create","user":"Alice","password_hash":"hash-one"}"#;
    let created: serde_json::Value = serde_json::from_str(&send(&mut c, create))?;
    assert_eq!(created["accounts"].as_array().map(Vec::len), Some(1));
    assert_eq!(created["accounts"][0]["user"], "alice");
    assert_eq!(created["accounts"][0]["revision"], 1);
    assert_eq!(created["accounts"][0].get("administrator"), None);
    assert!(send(&mut c, create).contains("already exists"));
    let reset = r#"{"method":"account_reset","user":"alice","password_hash":"hash-two"}"#;
    let changed: serde_json::Value = serde_json::from_str(&send(&mut c, reset))?;
    assert_eq!(changed["accounts"][0]["revision"], 2);
    assert_eq!(changed["accounts"][0]["password_hash"], "hash-two");
    assert!(send(&mut c, r#"{"method":"account_reset","user":"nobody","password_hash":"hash-x"}"#).contains("unknown account"));
    assert!(send(&mut c, r#"{"method":"account_drop","user":"nobody"}"#).contains("unknown account"));
    let dropped: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"account_drop","user":"alice"}"#))?;
    assert_eq!(dropped["accounts"].as_array().map(Vec::len), Some(0));
    let empty: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"accounts_list"}"#))?;
    assert_eq!(empty["accounts"], serde_json::json!([]));
    for method in ["list_tables", "list_catalog"] {
        let result = send(&mut c, &serde_json::json!({"method":method}).to_string());
        assert!(!result.contains("__chusql_users"), "{result}");
    }
    Ok(())
}

fn restart_at(pipe: &str, data: &std::path::Path) -> std::io::Result<ServerProc> {
    let child = Command::new(env!("CARGO_BIN_EXE_chusql-storage"))
        .arg("--config")
        .arg(write_config(data, pipe))
        .stdout(Stdio::null()).stderr(Stdio::null()).spawn()?;
    let server = ServerProc(child);
    for _ in 0..100 {
        if connect(pipe).is_ok() {
            create_fixture_database(pipe);
            return Ok(server);
        }
        thread::sleep(Duration::from_millis(50));
    }
    Err(std::io::Error::other("server restart timed out"))
}

#[test]
fn account_snapshot_replays_twice_without_duplicating_accounts() -> Result<(), Box<dyn std::error::Error>> {
    use chusql_storage::{protocol::Account, wal::{Wal, WalOp}};
    let pipe = unique_pipe_name();
    let (server, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    let initial = send(&mut c, r#"{"method":"account_create","user":"alice","password_hash":"hash-one"}"#);
    assert!(initial.contains("accounts"), "{initial}");
    let changed: serde_json::Value = serde_json::from_str(&send(&mut c,
        r#"{"method":"account_reset","user":"alice","password_hash":"hash-two"}"#))?;
    let accounts: Vec<Account> = serde_json::from_value(changed["accounts"].clone())?;
    drop(c);
    drop(server);
    for _ in 0..2 {
        let wal = Wal::new(data.path().join("system/wal.log"));
        wal.truncate()?;
        wal.append(&WalOp::Accounts { accounts: accounts.clone() })?;
        drop(wal);
        std::fs::write(data.path().join("system/__chusql_users.idx"), b"torn index")?;
        std::fs::write(data.path().join("system/__chusql_users.db"), b"torn heap")?;
        let restarted = restart_at(&pipe, data.path())?;
        let mut connection = connect(&pipe)?;
        let actual: serde_json::Value = serde_json::from_str(&send(&mut connection, r#"{"method":"accounts_list"}"#))?;
        assert_eq!(actual, changed);
        assert_eq!(std::fs::metadata(data.path().join("system/wal.log"))?.len(), 0);
        drop(connection);
        drop(restarted);
    }
    Ok(())
}

#[test]
fn account_failure_preserves_wal_and_blocks_writes() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (server, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    std::fs::create_dir(data.path().join("system/catalog.json.pending"))?;
    let result = send(&mut c, r#"{"method":"account_create","user":"alice","password_hash":"hash-one"}"#);
    assert!(result.contains("error"), "{result}");
    assert!(std::fs::metadata(data.path().join("system/wal.log"))?.len() > 0);
    for request in [r#"{"method":"accounts_list"}"#,
        r#"{"method":"account_create","user":"bob","password_hash":"hash-two"}"#] {
        assert!(send(&mut c, request).contains("recovery required"), "{request}");
    }
    let created = send(&mut c, r#"{"method":"create_table","table":"business","columns":[{"name":"id","ty":"int"}]}"#);
    assert!(created.contains(r#""status":"ok""#), "{created}");
    drop(c);
    drop(server);
    std::fs::remove_dir(data.path().join("system/catalog.json.pending"))?;
    let _restarted = restart_at(&pipe, data.path())?;
    let mut c = connect(&pipe)?;
    let recovered = send(&mut c, r#"{"method":"accounts_list"}"#);
    assert!(recovered.contains("hash-one"), "{recovered}");
    Ok(())
}

#[test]
fn account_create_rejects_reserved_file_collision() -> std::io::Result<()> {
    let pipe = unique_pipe_name();
    let (_server, data) = start_server(&pipe);
    std::fs::write(data.path().join("system/__chusql_users.db"), b"existing")?;
    let mut c = connect(&pipe)?;
    let result = send(&mut c, r#"{"method":"account_create","user":"alice","password_hash":"hash-one"}"#);
    assert!(result.contains("collision"), "{result}");
    assert_eq!(std::fs::read(data.path().join("system/__chusql_users.db"))?, b"existing");
    Ok(())
}

#[test]
fn oversized_account_hash_is_rejected_before_wal() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (_server, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    let result = send(&mut c, &serde_json::json!({"method":"account_create", "user":"alice",
        "password_hash":"a".repeat(300)}).to_string());
    assert!(result.contains("invalid account credential"), "{result}");
    let wal = data.path().join("system/wal.log");
    assert_eq!(if wal.exists() { std::fs::metadata(&wal)?.len() } else { 0 }, 0);
    assert_eq!(serde_json::from_str::<serde_json::Value>(&send(&mut c, r#"{"method":"accounts_list"}"#))?["accounts"], serde_json::json!([]));
    Ok(())
}

#[test]
fn account_create_rejects_existing_ordinary_catalog_entry() -> std::io::Result<()> {
    let data = tempfile::tempdir()?;
    let mut catalog = chusql_storage::catalog::Catalog::default();
    catalog.create_table("__chusql_users", Vec::new())?;
    std::fs::create_dir(data.path().join("system"))?;
    catalog.save(data.path().join("system/catalog.json"))?;
    let pipe = unique_pipe_name();
    let _server = restart_at(&pipe, data.path())?;
    let mut c = connect(&pipe)?;
    let result = send(&mut c, r#"{"method":"account_create","user":"alice","password_hash":"hash-one"}"#);
    assert!(result.contains("collision"), "{result}");
    Ok(())
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
    assert!(r.contains("duplicate value on the id index"), "got: {}", r);
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
    assert!(r.contains(r#""status":"ok""#), "id 2 should be insertable again: {}", r);
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

    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"sec_t","column":"code","key":5002}"#);
    assert!(r.contains(r#""status":"no_index""#), "got: {}", r);

    let r = send(&mut c, r#"{"method":"create_index","table":"sec_t","column":"code"}"#);
    assert!(r.contains(r#""status":"ok""#), "create_index: {}", r);

    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"sec_t","column":"code","key":5002}"#);
    assert!(r.contains(r#""id":2"#), "lookup by code: {}", r);

    send(&mut c, r#"{"method":"insert","table":"sec_t","row":{"id":4,"code":5004}}"#);
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"sec_t","column":"code","key":5004}"#);
    assert!(r.contains(r#""id":4"#), "lookup new row: {}", r);

    let r = send(&mut c, r#"{"method":"describe_table","table":"sec_t"}"#);
    assert!(r.contains(r#""column":"code""#), "describe: {}", r);

    let r = send(&mut c, r#"{"method":"drop_index","table":"sec_t","column":"code"}"#);
    assert!(r.contains(r#""status":"ok""#), "drop_index: {}", r);
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"sec_t","column":"code","key":5002}"#);
    assert!(r.contains(r#""status":"no_index""#), "after drop: {}", r);
}

/// 删列同时摘掉索引与行内该格
#[test]
fn drop_column_removes_column_index_and_values() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    for (id, code, note) in [(1, 5001, "a"), (2, 5002, "b")] {
        let req = format!(
            r#"{{"method":"insert","table":"drop_t","row":{{"id":{},"code":{},"note":"{}"}}}}"#,
            id, code, note
        );
        send(&mut c, &req);
    }
    let r = send(&mut c, r#"{"method":"create_index","table":"drop_t","column":"code"}"#);
    assert!(r.contains(r#""status":"ok""#), "create_index: {}", r);

    let r = send(&mut c, r#"{"method":"drop_column","table":"drop_t","column":"code"}"#);
    assert!(r.contains(r#""status":"ok""#), "drop_column: {}", r);

    let r = send(&mut c, r#"{"method":"describe_table","table":"drop_t"}"#);
    assert!(!r.contains(r#""name":"code""#), "code column should be gone: {}", r);
    assert!(r.contains(r#""name":"note""#), "note column should still be there: {}", r);
    assert!(!r.contains(r#""column":"code""#), "index on code should be gone: {}", r);

    let r = send(&mut c, r#"{"method":"scan","table":"drop_t"}"#);
    assert!(r.contains(r#""note":"a""#), "row should still be there: {}", r);
    assert!(!r.contains(r#""code""#), "row should no longer carry code: {}", r);

    let r = send(
        &mut c,
        r#"{"method":"lookup_by_index","table":"drop_t","column":"code","key":5001}"#,
    );
    assert!(r.contains(r#""status":"no_index""#), "{}", r);

    let r = send(&mut c, r#"{"method":"describe_table","table":"drop_t"}"#);
    assert!(r.contains(r#""row_count":2"#), "row count should still be 2: {}", r);

    let r = send(&mut c, r#"{"method":"drop_column","table":"drop_t","column":"id"}"#);
    assert!(r.contains(r#""status":"error""#), "the built-in id column must not be dropped: {}", r);
    let r = send(&mut c, r#"{"method":"drop_column","table":"drop_t","column":"nope"}"#);
    assert!(r.contains(r#""status":"error""#), "a missing column should error: {}", r);
}

/// 重复值列允许建索引：点查把重复的行都取回来，删一行不影响另一行；id 仍然唯一
#[test]
fn create_index_accepts_duplicate_values() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    send(&mut c, r#"{"method":"insert","table":"dup_t","row":{"id":1,"age":30}}"#);
    send(&mut c, r#"{"method":"insert","table":"dup_t","row":{"id":2,"age":30}}"#);
    send(&mut c, r#"{"method":"insert","table":"dup_t","row":{"id":3,"age":41}}"#);

    let r = send(&mut c, r#"{"method":"create_index","table":"dup_t","column":"age"}"#);
    assert!(r.contains(r#""status":"ok""#), "duplicate values are allowed: {}", r);

    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"dup_t","column":"age","key":30}"#);
    assert!(r.contains(r#""id":1"#) && r.contains(r#""id":2"#), "both rows come back: {}", r);
    assert!(!r.contains(r#""id":3"#), "only matching rows: {}", r);

    let r = send(&mut c, r#"{"method":"delete_keys","table":"dup_t","keys":[1]}"#);
    assert!(r.contains(r#""status":"ok""#), "delete: {}", r);
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"dup_t","column":"age","key":30}"#);
    assert!(!r.contains(r#""id":1"#) && r.contains(r#""id":2"#), "only the deleted entry goes away: {}", r);

    let r = send(&mut c, r#"{"method":"insert","table":"dup_t","row":{"id":2,"age":99}}"#);
    assert!(r.contains(r#""status":"error""#), "the id index stays unique: {}", r);
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
    assert!(r.contains(r#""name":"age","distinct":2"#), "age has 2 distinct values: {}", r);
    assert!(r.contains(r#""name":"id","distinct":3"#), "id has 3 distinct values: {}", r);

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
        let child = Command::new(env!("CARGO_BIN_EXE_chusql-storage"))
            .arg("--config")
            .arg(write_config(data.path(), &pipe))
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .unwrap();
        let srv = ServerProc(child);
        for _ in 0..100 {
            if connect(&pipe).is_ok() { break; }
            thread::sleep(Duration::from_millis(50));
        }
        create_fixture_database(&pipe);
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
        let child = Command::new(env!("CARGO_BIN_EXE_chusql-storage"))
            .arg("--config")
            .arg(write_config(data.path(), &pipe))
            .stdout(Stdio::null())
            .stderr(Stdio::null())
            .spawn()
            .unwrap();
        let srv = ServerProc(child);
        for _ in 0..100 {
            if connect(&pipe).is_ok() { break; }
            thread::sleep(Duration::from_millis(50));
        }
        create_fixture_database(&pipe);
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

    let child = Command::new(env!("CARGO_BIN_EXE_chusql-storage"))
        .arg("--config")
        .arg(&config_path)
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
    create_fixture_database(&pipe);

    let mut c = connect(&pipe).unwrap();
    assert_eq!(send(&mut c, r#"{"method":"ping"}"#), r#"{"status":"pong"}"#);
    let r = send(&mut c, r#"{"method":"insert","table":"cfg","row":{"id":1}}"#);
    assert!(r.contains(r#""status":"ok""#), "insert: {}", r);

    assert!(
        data_dir.join("databases/main/cfg.db").exists(),
        "data dir from config file was not used"
    );
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

/// 建表带上约束元数据，行里能有 null；replace_schema 整表换列定义与行
#[test]
fn replace_schema_rewrites_columns_and_rows() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    let created = send(
        &mut c,
        r#"{"method":"create_table","table":"alt_t","columns":[{"name":"id","ty":"int","nullable":false,"auto_increment":true,"primary_key":true},{"name":"name","ty":"varchar(8)","default":"x"}]}"#,
    );
    assert!(created.contains(r#""status":"ok""#), "create: {}", created);

    let r = send(&mut c, r#"{"method":"insert","table":"alt_t","row":{"id":1,"name":null}}"#);
    assert!(r.contains(r#""status":"ok""#), "insert null: {}", r);
    let r = send(&mut c, r#"{"method":"insert","table":"alt_t","row":{"id":2,"name":"b"}}"#);
    assert!(r.contains(r#""status":"ok""#), "insert: {}", r);

    let r = send(&mut c, r#"{"method":"describe_table","table":"alt_t"}"#);
    assert!(r.contains(r#""ty":"varchar(8)""#), "type parameters survive: {}", r);
    assert!(r.contains(r#""nullable":false"#), "nullability survives: {}", r);
    assert!(r.contains(r#""auto_increment":true"#), "auto increment survives: {}", r);
    assert!(r.contains(r#""primary_key":true"#), "primary key survives: {}", r);
    assert!(r.contains(r#""default":"x""#), "default survives: {}", r);
    let scanned = send(&mut c, r#"{"method":"scan","table":"alt_t"}"#);
    assert!(scanned.contains(r#""name":null"#), "null value survives a scan: {}", scanned);

    let replaced = send(
        &mut c,
        r#"{"method":"replace_schema","table":"alt_t","columns":[{"name":"id","ty":"int","nullable":false},{"name":"tag","ty":"str","nullable":false,"default":"n"}],"rows":[{"id":1,"tag":"a"},{"id":2,"tag":"b"}]}"#,
    );
    assert!(replaced.contains(r#""status":"ok""#), "replace_schema: {}", replaced);

    let r = send(&mut c, r#"{"method":"describe_table","table":"alt_t"}"#);
    assert!(r.contains(r#""name":"tag""#), "new column: {}", r);
    assert!(!r.contains(r#""name":"name""#), "old column gone: {}", r);
    assert!(r.contains(r#""row_count":2"#), "row count rebuilt: {}", r);

    let r = send(&mut c, r#"{"method":"scan","table":"alt_t"}"#);
    assert!(r.contains(r#""tag":"a""#), "rewritten rows: {}", r);
}

/// 旧类型系统落盘的数据在启动时升级：缺的格补 null，老类型名并到新写法
#[test]
fn legacy_catalog_and_partial_rows_are_migrated() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (server, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"legacy_t","columns":[{"name":"id","ty":"int"},{"name":"name","ty":"str"},{"name":"age","ty":"int"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"legacy_t","row":{"id":1,"name":"a"}}))?;
    drop(c);
    drop(server);

    // 把字典改回老样子：只留 name / ty，类型名写成别名
    let catalog_path = data.path().join("databases/main/catalog.json");
    let mut catalog: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(&catalog_path)?)?;
    let columns = catalog["tables"]["legacy_t"]["columns"].as_array_mut().ok_or("missing columns")?;
    for column in columns.iter_mut() {
        let name = column["name"].clone();
        let ty = if column["ty"] == "int" { "integer" } else { "text" };
        *column = serde_json::json!({"name": name, "ty": ty});
    }
    std::fs::write(&catalog_path, serde_json::to_string_pretty(&catalog)?)?;

    let _restarted = start_server_in(&pipe, data.path());
    let mut c = connect(&pipe)?;

    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"legacy_t"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":1,"name":"a","age":null}]), "缺的格要补 null");

    let described: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"describe_table","table":"legacy_t"}"#))?;
    let types: Vec<&str> = described["columns"].as_array().ok_or("missing columns")?.iter().map(|col| col["ty"].as_str().unwrap_or("")).collect();
    assert_eq!(types, vec!["int", "str", "int"], "老类型名要并到新写法");
    assert!(described["columns"][0]["nullable"].is_boolean(), "新字段要落盘");

    let saved: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(&catalog_path)?)?;
    assert_eq!(saved["tables"]["legacy_t"]["columns"][0]["ty"], "int");
    Ok(())
}

/// 账号表用新类型系统落盘：id 主键、用户名索引、注册与最近登录时间
#[test]
fn account_table_uses_typed_columns_and_stamps_login() -> Result<(), Box<dyn std::error::Error>> {
    let pipe = unique_pipe_name();
    let (_srv, data) = start_server(&pipe);
    let mut c = connect(&pipe)?;

    let created: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"account_create","user":"alice","password_hash":"hash"}"#))?;
    assert_eq!(created["status"], "accounts", "{}", created);
    let account = &created["accounts"][0];
    assert_eq!(account["user"], "alice");
    assert_eq!(account["id"], 1);
    assert_eq!(account["registered_at"].as_str().unwrap_or("").len(), 19, "注册时间要写成 timestamp: {}", account);
    assert!(account["last_login_at"].is_null(), "还没登录过: {}", account);

    let catalog: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(data.path().join("system/catalog.json"))?)?;
    let entry = &catalog["tables"]["__chusql_users"];
    let columns = entry["columns"].as_array().ok_or("missing columns")?;
    let names: Vec<&str> = columns.iter().map(|col| col["name"].as_str().unwrap_or("")).collect();
    assert_eq!(names, vec!["id", "user", "password_hash", "registered_at", "last_login_at", "revision"]);
    let types: Vec<&str> = columns.iter().map(|col| col["ty"].as_str().unwrap_or("")).collect();
    assert_eq!(types, vec!["int", "varchar(64)", "varchar(256)", "timestamp", "timestamp", "int"]);
    assert_eq!(columns[0]["primary_key"], true, "id 是主键");
    assert_eq!(columns[0]["nullable"], false);
    assert_eq!(columns[1]["unique"], true, "用户名唯一");
    let indexes = entry["indexes"].as_array().ok_or("missing indexes")?;
    assert!(indexes.iter().any(|i| i["column"] == "user"), "用户名索引要写进字典: {}", entry);

    let login: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"account_login","user":"alice"}"#))?;
    assert_eq!(login["status"], "accounts", "{}", login);
    let stamp = login["accounts"][0]["last_login_at"].as_str().unwrap_or("");
    assert_eq!(stamp.len(), 19, "登录要盖时间戳: {}", login);
    Ok(())
}

/// 字符串列也能建索引并按键点查
#[test]
fn string_index_lookup_over_the_pipe() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    send(&mut c, r#"{"method":"insert","table":"sidx","row":{"id":1,"name":"alice"}}"#);
    send(&mut c, r#"{"method":"insert","table":"sidx","row":{"id":2,"name":"bob"}}"#);

    let r = send(&mut c, r#"{"method":"create_index","table":"sidx","column":"name"}"#);
    assert!(r.contains(r#""status":"ok""#), "string column index: {}", r);

    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"sidx","column":"name","key":"bob"}"#);
    assert!(r.contains(r#""id":2"#), "string lookup: {}", r);

    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"sidx","column":"name","key":"dave"}"#);
    assert!(r.contains(r#""rows":[]"#), "miss: {}", r);

    let r = send(&mut c, r#"{"method":"insert","table":"sidx","row":{"id":3,"name":"alice"}}"#);
    assert!(r.contains(r#""status":"ok""#), "duplicate string value is allowed on a secondary index: {}", r);
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"sidx","column":"name","key":"alice"}"#);
    assert!(r.contains(r#""id":1"#) && r.contains(r#""id":3"#), "both rows come back: {}", r);
}

/// 范围扫描走索引叶子链，两端可开可闭，没索引回 no_index
#[test]
fn range_scan_over_the_pipe() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    for (id, code) in [(1, 100), (2, 200), (3, 300), (4, 400)] {
        let req = format!(
            r#"{{"method":"insert","table":"rng","row":{{"id":{},"code":{}}}}}"#,
            id, code
        );
        send(&mut c, &req);
    }
    let r = send(&mut c, r#"{"method":"create_index","table":"rng","column":"code"}"#);
    assert!(r.contains(r#""status":"ok""#), "create_index: {}", r);

    let codes = |line: &str| -> Vec<i64> {
        let value: serde_json::Value = serde_json::from_str(line).unwrap();
        let mut out: Vec<i64> = value["rows"]
            .as_array()
            .map(|rows| rows.iter().filter_map(|row| row["code"].as_i64()).collect())
            .unwrap_or_default();
        out.sort_unstable();
        out
    };

    let r = send(&mut c, r#"{"method":"range_by_index","table":"rng","column":"code","lo":200,"hi":400}"#);
    assert_eq!(codes(&r), vec![200, 300, 400], "closed bounds: {}", r);

    let r = send(&mut c, r#"{"method":"range_by_index","table":"rng","column":"code","lo":200,"lo_inclusive":false,"hi":400,"hi_inclusive":false}"#);
    assert_eq!(codes(&r), vec![300], "open bounds: {}", r);

    let r = send(&mut c, r#"{"method":"range_by_index","table":"rng","column":"code","lo":300}"#);
    assert_eq!(codes(&r), vec![300, 400], "lower bound only: {}", r);

    let r = send(&mut c, r#"{"method":"range_by_index","table":"rng","column":"code","hi":200}"#);
    assert_eq!(codes(&r), vec![100, 200], "upper bound only: {}", r);

    let r = send(&mut c, r#"{"method":"range_by_index","table":"rng","column":"nope"}"#);
    assert!(r.contains(r#""status":"no_index""#), "unindexed column: {}", r);
}

/// 字符串列按字节序范围扫描
#[test]
fn string_range_scan_over_the_pipe() {
    let pipe = unique_pipe_name();
    let (_srv, _data) = start_server(&pipe);
    let mut c = connect(&pipe).unwrap();

    for (id, name) in [(1, "alice"), (2, "bob"), (3, "carol"), (4, "dave")] {
        let req = format!(
            r#"{{"method":"insert","table":"srng","row":{{"id":{},"name":"{}"}}}}"#,
            id, name
        );
        send(&mut c, &req);
    }
    let r = send(&mut c, r#"{"method":"create_index","table":"srng","column":"name"}"#);
    assert!(r.contains(r#""status":"ok""#), "create_index: {}", r);

    let names = |line: &str| -> Vec<String> {
        let value: serde_json::Value = serde_json::from_str(line).unwrap();
        let mut out: Vec<String> = value["rows"]
            .as_array()
            .map(|rows| rows.iter().filter_map(|row| row["name"].as_str().map(str::to_string)).collect())
            .unwrap_or_default();
        out.sort();
        out
    };

    let r = send(&mut c, r#"{"method":"range_by_index","table":"srng","column":"name","lo":"b","hi":"carol"}"#);
    assert_eq!(names(&r), vec!["bob".to_string(), "carol".to_string()], "{}", r);

    let r = send(&mut c, r#"{"method":"range_by_index","table":"srng","column":"name","lo":"carol","lo_inclusive":false}"#);
    assert_eq!(names(&r), vec!["dave".to_string()], "{}", r);
}
