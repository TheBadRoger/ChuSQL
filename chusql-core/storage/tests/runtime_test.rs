use std::cell::RefCell;
use std::sync::Arc;

use chusql_core_storage::runtime::Storage;

// 进程内存储集成测试：协议请求、索引、账号与配置。

thread_local! {
    /// 当前线程的存储实例：一个用例一个线程，用例内部按顺序起/停
    static CURRENT: RefCell<Option<Arc<Storage>>> = const { RefCell::new(None) };
}

/// 定点写入检查舍入和整批原子拒绝
#[test]
fn decimal_writes_enforce_precision_before_wal() -> Result<(), Box<dyn std::error::Error>> {
    let (_server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"amounts","columns":[{"name":"id","ty":"int"},{"name":"value","ty":"decimal(4,2)"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"amounts","row":{"id":1,"value":99.994}}))?;
    let snapshot = send(&mut c, r#"{"method":"scan","table":"amounts"}"#);
    let rows: serde_json::Value = serde_json::from_str(&snapshot)?;
    assert_eq!(rows["rows"][0]["value"], serde_json::json!(99.99));
    let checkpoint = std::fs::read(data.path().join("databases/main/wal.checkpoint"))?;
    for value in [99.995, -99.995, 100.0] {
        let response = send(&mut c, &serde_json::json!({"method":"insert_batch","table":"amounts","rows":[{"id":2,"value":1.2},{"id":3,"value":value}]}).to_string());
        assert!(response.contains("exceeds precision"), "{response}");
        let remaining: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"amounts"}"#))?;
        assert_eq!(remaining, rows);
        assert_eq!(std::fs::read(data.path().join("databases/main/wal.checkpoint"))?, checkpoint);
    }
    let invalid = send(&mut c, &serde_json::json!({"method":"create_table","table":"invalid","columns":[{"name":"value","ty":"decimal(2,3)"}]}).to_string());
    assert!(invalid.contains("invalid decimal"), "{invalid}");
    Ok(())
}

/// 检查点失败封闭请求并在重启恢复
#[test]
fn checkpoint_failure_requires_recovery_and_preserves_committed_rows() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"checkpointed","columns":[{"name":"id","ty":"int"}]}))?;
    let pending = data.path().join("databases/main/wal.checkpoint.pending");
    std::fs::create_dir(&pending)?;
    let failed = send(&mut c, r#"{"method":"insert","table":"checkpointed","row":{"id":1}}"#);
    assert!(failed.contains("error"), "{failed}");
    assert!(send(&mut c, r#"{"method":"scan","table":"checkpointed"}"#).contains("recovery required"));
    assert!(std::fs::metadata(data.path().join("databases/main/wal.log"))?.len() > 0);
    drop(c);
    drop(server);
    std::fs::remove_dir(pending)?;
    let _restarted = restart_at(data.path())?;
    let mut c = connect()?;
    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"checkpointed"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":1}]));
    Ok(())
}

/// 应用失败封闭读写并重放完整提交组
#[test]
fn committed_apply_failures_require_recovery() -> Result<(), Box<dyn std::error::Error>> {
    for operation in [
        serde_json::json!({"method":"create_table","table":"created","columns":[{"name":"id","ty":"int"}]}),
        serde_json::json!({"method":"replace_all","table":"items","rows":[{"id":2}]}),
        serde_json::json!({"method":"apply_transaction","ops":[{"op":"delete","table":"items","ids":[1]},{"op":"replace","table":"items","rows":[{"id":2}]}]}),
    ] {
        let (server, data) = start_server();
        let mut c = connect()?;
        request_ok(&mut c, serde_json::json!({"method":"create_table","table":"items","columns":[{"name":"id","ty":"int"}]}))?;
        request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":1}}))?;
        let directory = data.path().join("databases/main");
        let checkpoint = std::fs::read(directory.join("wal.checkpoint"))?;
        let pending = directory.join("catalog.json.pending");
        std::fs::create_dir(&pending)?;
        let failed = send(&mut c, &operation.to_string());
        assert!(failed.contains("error"), "{operation}: {failed}");
        for request in [
            r#"{"method":"scan","table":"items"}"#,
            r#"{"method":"insert","table":"items","row":{"id":3}}"#,
        ] {
            let blocked = send(&mut c, request);
            assert!(blocked.contains("recovery required"), "{blocked}");
        }
        assert_eq!(std::fs::read(directory.join("wal.checkpoint"))?, checkpoint);
        assert!(std::fs::metadata(directory.join("wal.log"))?.len() > 0);
        drop(c);
        drop(server);
        std::fs::remove_dir(pending)?;
        let restarted = restart_at(data.path())?;
        let mut c = connect()?;
        let table = if operation["method"] == "create_table" { "created" } else { "items" };
        let result: serde_json::Value = serde_json::from_str(&send(&mut c, &serde_json::json!({"method":"scan","table":table}).to_string()))?;
        let expected = if table == "created" { serde_json::json!([]) } else { serde_json::json!([{"id":2}]) };
        assert_eq!(result["rows"], expected, "{operation}");
        assert_eq!(std::fs::metadata(directory.join("wal.log"))?.len(), 0);
        drop(c);
        drop(restarted);
    }
    Ok(())
}

/// 首帧撕裂恢复后新日志仍可重放
#[test]
fn torn_first_frame_is_removed_before_new_writes() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"torn","columns":[{"name":"id","ty":"int"}]}))?;
    drop(c);
    drop(server);
    std::fs::write(data.path().join("databases/main/wal.log"), [20, 0, 0, 0, 1])?;
    let restarted = restart_at(data.path())?;
    assert_eq!(std::fs::metadata(data.path().join("databases/main/wal.log"))?.len(), 0);
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"torn","row":{"id":1}}))?;
    drop(c);
    drop(restarted);
    let _again = restart_at(data.path())?;
    let mut c = connect()?;
    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"torn"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":1}]));
    Ok(())
}

/// 校验预装类型幂等与冲突保护
#[test]
fn preinstalled_types_are_idempotent_hidden_and_conflict_safe() -> Result<(), Box<dyn std::error::Error>> {
    let (_server, data) = start_server();
    let mut c = connect()?;
    let premature = send(&mut c, &type_seed_request().to_string());
    assert!(premature.contains("login superuser"), "{premature}");
    send(&mut c, r#"{"method":"bootstrap_system","user":"root","password_hash":"original-hash"}"#);
    let accounts = send(&mut c, r#"{"method":"accounts_list"}"#);
    let request = type_seed_request();
    let seeded: serde_json::Value = serde_json::from_str(&send(&mut c, &request.to_string()))?;
    assert_eq!(seeded["status"], "rows", "{seeded}");
    assert_eq!(seeded["rows"].as_array().map(Vec::len), Some(2));
    assert_eq!(serde_json::from_str::<serde_json::Value>(&send(&mut c, &request.to_string()))?, seeded);
    let mut conflict = request;
    conflict["types"][1]["base_type"] = "int".into();
    let rejected = send(&mut c, &conflict.to_string());
    assert!(rejected.contains("definition conflict: text"), "{rejected}");
    assert_eq!(serde_json::from_str::<serde_json::Value>(&send(&mut c, &type_seed_request().to_string()))?, seeded);
    assert_eq!(send(&mut c, r#"{"method":"accounts_list"}"#), accounts);
    let catalog: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(data.path().join("system/catalog.json"))?)?;
    assert_eq!(catalog["tables"]["__system_types"]["system"], true);
    assert_eq!(catalog["tables"]["__system_types"]["row_count"], 2);
    let denied = send(&mut c, r#"{"method":"scan","database":"system","table":"__system_types"}"#);
    assert!(denied.contains("reserved system table"), "{denied}");
    Ok(())
}

/// 类型预装失败保留日志并在重启后恢复
#[test]
fn preinstalled_types_recover_after_catalog_write_failure() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
    send(&mut c, r#"{"method":"bootstrap_system","user":"root","password_hash":"hash"}"#);
    std::fs::create_dir(data.path().join("system/catalog.json.pending"))?;
    let failed = send(&mut c, &type_seed_request().to_string());
    assert!(failed.contains("error"), "{failed}");
    assert!(std::fs::metadata(data.path().join("system/wal.log"))?.len() > 0);
    assert!(send(&mut c, &type_seed_request().to_string()).contains("recovery required"));
    drop(c);
    drop(server);
    std::fs::remove_dir(data.path().join("system/catalog.json.pending"))?;
    let _restarted = restart_at(data.path())?;
    let mut c = connect()?;
    let restored: serde_json::Value = serde_json::from_str(&send(&mut c, &type_seed_request().to_string()))?;
    assert_eq!(restored["rows"].as_array().map(Vec::len), Some(2), "{restored}");
    Ok(())
}

/// 类型目录不覆盖未登记的现有文件
#[test]
fn preinstalled_types_reject_reserved_file_collision() -> Result<(), Box<dyn std::error::Error>> {
    let (_server, data) = start_server();
    let mut c = connect()?;
    send(&mut c, r#"{"method":"bootstrap_system","user":"root","password_hash":"hash"}"#);
    let path = data.path().join("system/__system_types.db");
    std::fs::write(&path, b"preserved")?;
    let rejected = send(&mut c, &type_seed_request().to_string());
    assert!(rejected.contains("reserved type file collision"), "{rejected}");
    assert_eq!(std::fs::read(path)?, b"preserved");
    Ok(())
}

/// 生成两项预装类型定义
fn type_seed_request() -> serde_json::Value {
    serde_json::json!({"method":"bootstrap_types","types":[
        {"name":"int","base_type":"int","parameterized":false},
        {"name":"text","base_type":"str","parameterized":false}
    ]})
}

/// 引导保留已用身份编号
#[test]
fn bootstrap_respects_previous_identity_ids() -> Result<(), Box<dyn std::error::Error>> {
    let (_server, _data) = start_server();
    let mut c = connect()?;
    send(&mut c, r#"{"method":"account_create","user":"alice","password_hash":"hash"}"#);
    let deleted: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"account_drop","user":"alice"}"#))?;
    assert_eq!(deleted["accounts"], serde_json::json!([]));
    send(&mut c, r#"{"method":"bootstrap_system","user":"root","password_hash":"hash"}"#);
    let seeded: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"accounts_list"}"#))?;
    assert_eq!(seeded["accounts"][0]["id"], 2);
    Ok(())
}

/// 旧账号日志迁移只提升初始管理员
#[test]
fn legacy_account_wal_migrates_without_promoting_other_names() -> Result<(), Box<dyn std::error::Error>> {
    use chusql_core_storage::wal::Wal;
    let (server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","database":"system","table":"__system_roles","columns":[{"name":"name","ty":"str"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert","database":"system","table":"__system_roles","row":{"name":"reader"}}))?;
    drop(c);
    drop(server);
    let path = data.path().join("system/wal.log");
    let lsn = Wal::new(&path).read_checkpoint()?.unwrap_or(0) + 1;
    let legacy = serde_json::json!([
        {"id":7,"user":"owner","password_hash":"owner-hash","revision":3,"registered_at":"2020-01-01 00:00:00","last_login_at":null},
        {"id":9,"user":"alice","password_hash":"alice-hash","revision":2,"registered_at":"2020-01-02 00:00:00","last_login_at":null}
    ]);
    let table = b"__system_users";
    let mut body = lsn.to_le_bytes().to_vec();
    body.push(6);
    body.extend_from_slice(&(table.len() as u16).to_le_bytes());
    body.extend_from_slice(table);
    body.extend_from_slice(&serde_json::to_vec(&legacy)?);
    let mut frame = (body.len() as u32).to_le_bytes().to_vec();
    frame.extend_from_slice(&body);
    std::fs::write(&path, frame)?;
    let _restarted = restart_at(data.path())?;
    let mut c = connect()?;
    let wrong = send(&mut c, r#"{"method":"identity_initialize","administrator":"missing"}"#);
    assert!(wrong.contains("last enabled login superuser"), "{wrong}");
    let migrated: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"identity_initialize","administrator":"owner"}"#))?;
    assert_eq!(migrated["status"], "accounts", "{migrated}");
    assert_eq!(migrated["accounts"][0]["id"], 7);
    assert_eq!(migrated["accounts"][0]["is_superuser"], true);
    assert_eq!(migrated["accounts"][0]["password_hash"], "owner-hash");
    assert_eq!(migrated["accounts"][1]["is_superuser"], false);
    assert_eq!(migrated["accounts"][2]["user"], "reader");
    assert_eq!(migrated["accounts"][2]["id"], 10);
    assert_eq!(serde_json::from_str::<serde_json::Value>(&send(&mut c, r#"{"method":"identity_initialize","administrator":"alice"}"#))?, migrated);
    Ok(())
}

/// 身份属性重启保留且禁止删除最后管理员
#[test]
fn identity_attributes_survive_restart_and_protect_superuser() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
    send(&mut c, r#"{"method":"bootstrap_system","user":"root","password_hash":"hash"}"#);
    let created: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"role_create","user":"Reader"}"#))?;
    assert_eq!(created["status"], "accounts", "{created}");
    assert_eq!(created["accounts"][1]["can_login"], false);
    assert_eq!(created["accounts"][1]["enabled"], true);
    for change in [serde_json::json!({"enabled":false}), serde_json::json!({"can_login":false}), serde_json::json!({"is_superuser":false})] {
        let mut request = change;
        request["method"] = "identity_alter".into();
        request["user"] = "root".into();
        let denied = send(&mut c, &request.to_string());
        assert!(denied.contains("last enabled login superuser"), "{denied}");
    }
    let collision = send(&mut c, r#"{"method":"account_create","user":"READER","password_hash":"hash"}"#);
    assert!(collision.contains("already exists"), "{collision}");
    let changed: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"identity_alter","user":"reader","can_login":true,"is_superuser":true}"#))?;
    assert_eq!(changed["accounts"][1]["revision"], 2);
    drop(c);
    drop(server);
    let _restarted = restart_at(data.path())?;
    let mut c = connect()?;
    let restored: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"accounts_list"}"#))?;
    assert_eq!(restored, changed);
    let denied = send(&mut c, r#"{"method":"scan","database":"system","table":"__system_identities"}"#);
    assert!(denied.contains("reserved system table"), "{denied}");
    Ok(())
}

/// 旧角色迁移与重名拒绝不破坏原数据
#[test]
fn identity_migration_preserves_roles_and_rejects_collisions() -> Result<(), Box<dyn std::error::Error>> {
    let (_server, _data) = start_server();
    let mut c = connect()?;
    send(&mut c, r#"{"method":"bootstrap_system","user":"root","password_hash":"hash"}"#);
    request_ok(&mut c, serde_json::json!({"method":"create_table","database":"system","table":"__system_roles","columns":[{"name":"name","ty":"str"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert","database":"system","table":"__system_roles","row":{"name":"root"}}))?;
    let before = send(&mut c, r#"{"method":"accounts_list"}"#);
    let rejected = send(&mut c, r#"{"method":"identity_initialize","administrator":"root"}"#);
    assert!(rejected.contains("identity name collision: root"), "{rejected}");
    assert_eq!(send(&mut c, r#"{"method":"accounts_list"}"#), before);
    request_ok(&mut c, serde_json::json!({"method":"replace_all","database":"system","table":"__system_roles","rows":[{"name":"reader"}]}))?;
    let migrated: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"identity_initialize","administrator":"root"}"#))?;
    assert_eq!(migrated["accounts"][1]["user"], "reader");
    assert_eq!(migrated["accounts"][1]["can_login"], false);
    assert_eq!(serde_json::from_str::<serde_json::Value>(&send(&mut c, r#"{"method":"identity_initialize","administrator":"reader"}"#))?, migrated);
    Ok(())
}

/// 删除身份清理授权并在恢复后保留编号上界
#[test]
fn identity_drop_cleanup_replays_without_reusing_ids() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
    send(&mut c, r#"{"method":"bootstrap_system","user":"root","password_hash":"hash"}"#);
    send(&mut c, r#"{"method":"role_create","user":"reader"}"#);
    for table in ["__system_grants", "__system_grant_options", "__system_members"] {
        request_ok(&mut c, serde_json::json!({"method":"create_table","database":"system","table":table,"columns":[{"name":"role","ty":"str"},{"name":"member","ty":"str"}]}))?;
        request_ok(&mut c, serde_json::json!({"method":"insert","database":"system","table":table,"row":{"role":"reader","member":"root"}}))?;
    }
    std::fs::create_dir(data.path().join("system/catalog.json.pending"))?;
    let failed = send(&mut c, r#"{"method":"account_drop","user":"reader"}"#);
    assert!(failed.contains("error"), "{failed}");
    drop(c);
    drop(server);
    std::fs::remove_dir(data.path().join("system/catalog.json.pending"))?;
    let _restarted = restart_at(data.path())?;
    let mut c = connect()?;
    for table in ["__system_grants", "__system_grant_options", "__system_members"] {
        let rows: serde_json::Value = serde_json::from_str(&send(&mut c, &serde_json::json!({"method":"scan","database":"system","table":table}).to_string()))?;
        assert_eq!(rows["rows"], serde_json::json!([]));
    }
    let recreated: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"role_create","user":"reader"}"#))?;
    assert_eq!(recreated["accounts"][1]["id"], 3);
    Ok(())
}

/// 命名类型引用与删除依赖在写入时校验
#[test]
fn domain_dependencies_are_checked_before_wal() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, _data) = start_server();
    let mut connection = connect()?;
    let columns = serde_json::json!([{"name":"code","ty":"domain(code,int)"}]);
    let missing = serde_json::json!({"method":"create_table","table":"typed","columns":columns});
    let response: serde_json::Value = serde_json::from_str(&send(&mut connection, &missing.to_string()))?;
    assert_eq!(response["status"], "error");
    request_ok(&mut connection, serde_json::json!({"method":"create_table","table":"__system_domain_code","columns":[{"name":"base","ty":"int"}]}))?;
    let forged = serde_json::json!({"method":"create_table","table":"forged","columns":[{"name":"code","ty":"domain(code,str)"}]});
    let response: serde_json::Value = serde_json::from_str(&send(&mut connection, &forged.to_string()))?;
    assert_eq!(response["status"], "error");
    request_ok(&mut connection, missing)?;
    let response: serde_json::Value = serde_json::from_str(&send(&mut connection, r#"{"method":"drop_table","table":"__system_domain_code"}"#))?;
    assert_eq!(response["status"], "error");
    request_ok(&mut connection, serde_json::json!({"method":"drop_table","table":"typed"}))?;
    request_ok(&mut connection, serde_json::json!({"method":"drop_table","table":"__system_domain_code"}))?;
    let response: serde_json::Value = serde_json::from_str(&send(&mut connection, r#"{"method":"create_table","table":"stale","columns":[{"name":"code","ty":"domain(code,int)"}]}"#))?;
    assert_eq!(response["status"], "error");
    Ok(())
}

/// 嵌套类型依赖与重启持久化
#[test]
fn nested_domains_preserve_dependencies_across_restart() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, data) = start_server();
    let mut connection = connect()?;
    request_ok(&mut connection, serde_json::json!({"method":"create_table","table":"__system_domain_root","columns":[{"name":"base","ty":"int"}]}))?;
    request_ok(&mut connection, serde_json::json!({"method":"create_table","table":"__system_domain_child","columns":[{"name":"base","ty":"domain(root,int)"}]}))?;
    let rejected: serde_json::Value = serde_json::from_str(&send(&mut connection, r#"{"method":"drop_table","table":"__system_domain_root"}"#))?;
    assert_eq!(rejected["status"], "error");
    request_ok(&mut connection, serde_json::json!({"method":"create_table","table":"typed","columns":[{"name":"id","ty":"domain(child,domain(root,int))"}]}))?;
    let forged: serde_json::Value = serde_json::from_str(&send(&mut connection, r#"{"method":"create_table","table":"forged","columns":[{"name":"id","ty":"domain(child,domain(root,str))"}]}"#))?;
    assert_eq!(forged["status"], "error");
    drop(connection);
    CURRENT.with(|c| *c.borrow_mut() = None);
    drop(_srv);
    let _reopened = start_server_in(data.path());
    let mut connection = connect()?;
    let schema: serde_json::Value = serde_json::from_str(&send(&mut connection, r#"{"method":"describe_table","table":"typed"}"#))?;
    assert_eq!(schema["status"], "schema");
    assert_eq!(schema["columns"][0]["ty"], "domain(child,domain(root,int))");
    request_ok(&mut connection, serde_json::json!({"method":"drop_table","table":"typed"}))?;
    request_ok(&mut connection, serde_json::json!({"method":"drop_table","table":"__system_domain_child"}))?;
    request_ok(&mut connection, serde_json::json!({"method":"drop_table","table":"__system_domain_root"}))?;
    Ok(())
}

/// 重置与结构修复保留业务文件及系统数据
#[test]
fn offline_maintenance_preserves_data_and_rejects_live_storage() -> Result<(), Box<dyn std::error::Error>> {
    let data = tempfile::tempdir()?;
    let config = write_config(data.path());
    let business = data.path().join("databases/business");
    std::fs::create_dir_all(&business)?;
    std::fs::write(business.join("sentinel"), b"business data")?;
    let types = type_seed_request()["types"].clone();
    let request = serde_json::json!({"mode":"reset","user":"root","password_hash":"original-hash","types":types});
    Storage::maintenance(config.to_str(), &request.to_string())?;
    let system = data.path().join("system");
    let credentials = std::fs::read(system.join("__system_users.db"))?;
    let mut catalog: serde_json::Value = serde_json::from_slice(&std::fs::read(system.join("catalog.json"))?)?;
    catalog["tables"]["__system_identities"]["columns"] = serde_json::json!([]);
    std::fs::write(system.join("catalog.json"), serde_json::to_vec(&catalog)?)?;
    let repair = serde_json::json!({"mode":"repair","types":types});
    Storage::maintenance(config.to_str(), &repair.to_string())?;
    assert_eq!(std::fs::read(system.join("__system_users.db"))?, credentials);
    assert_eq!(std::fs::read(business.join("sentinel"))?, b"business data");
    let opened = Storage::open(config.to_str())?;
    assert!(Storage::maintenance(config.to_str(), &request.to_string()).unwrap_err().contains("data directory is in use"));
    drop(opened);
    std::fs::remove_file(system.join("__system_users.db"))?;
    let before = std::fs::read(system.join("catalog.json"))?;
    let failure = Storage::maintenance(config.to_str(), &repair.to_string()).unwrap_err();
    assert!(failure.contains("repair would require"));
    assert_eq!(std::fs::read(system.join("catalog.json"))?, before);
    assert!(!system.join("__system_users.db").exists());
    Ok(())
}

/// 从账号 WAL 恢复缺失凭据文件
#[test]
fn offline_recovery_replays_account_wal() -> Result<(), Box<dyn std::error::Error>> {
    let data = tempfile::tempdir()?;
    let config = write_config(data.path());
    let types = type_seed_request()["types"].clone();
    Storage::maintenance(config.to_str(), &serde_json::json!({"mode":"reset","user":"root","password_hash":"original-hash","types":types}).to_string())?;
    let storage = Storage::open(config.to_str())?;
    let accounts: serde_json::Value = serde_json::from_str(&storage.request_line(r#"{"method":"accounts_list"}"#))?;
    let saved = serde_json::from_value(accounts["accounts"].clone())?;
    drop(storage);
    let system = data.path().join("system");
    let wal = chusql_core_storage::wal::Wal::new(system.join("wal.log"));
    wal.append(&chusql_core_storage::wal::WalOp::Accounts { accounts: saved, removed: Vec::new(), migrate_roles: false })?;
    wal.append(&chusql_core_storage::wal::WalOp::Commit)?;
    drop(wal);
    std::fs::remove_file(system.join("__system_users.db"))?;
    Storage::maintenance(config.to_str(), &serde_json::json!({"mode":"recover","types":types}).to_string())?;
    let storage = Storage::open(config.to_str())?;
    let restored: serde_json::Value = serde_json::from_str(&storage.request_line(r#"{"method":"accounts_list"}"#))?;
    assert_eq!(restored["accounts"], accounts["accounts"]);
    drop(storage);
    std::fs::write(system.join("catalog.json"), b"corrupted catalog")?;
    Storage::maintenance(config.to_str(), &serde_json::json!({"mode":"recover","types":types}).to_string())?;
    let storage = Storage::open(config.to_str())?;
    let reconstructed: serde_json::Value = serde_json::from_str(&storage.request_line(r#"{"method":"accounts_list"}"#))?;
    assert_eq!(reconstructed["accounts"], accounts["accounts"]);
    Ok(())
}

/// 一次会话就是当前线程的存储句柄
struct Conn(Arc<Storage>);

/// 扫描能按列投影，空列返回空行
#[test]
fn scan_projects_columns_and_preserves_empty_rows() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, _data) = start_server();
    let mut c = connect()?;
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

/// 分片扫描按页序拼接后与整表扫描逐行一致
#[test]
fn scan_shard_concatenates_to_the_whole_scan() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"wide","columns":[{"name":"id","ty":"int"},{"name":"name","ty":"str"}]}))?;
    let rows: Vec<serde_json::Value> = (1..=1200)
        .map(|i| serde_json::json!({"id": i, "name": format!("row-{i:04}")}))
        .collect();
    request_ok(&mut c, serde_json::json!({"method":"insert_batch","table":"wide","rows":rows}))?;
    let heap_bytes = std::fs::metadata(data.path().join("databases/main/wide.db"))?.len();
    assert!(heap_bytes > 4 * 4096, "分片要有意义得多页，实际只有 {heap_bytes} 字节");
    let whole: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"wide"}"#))?;
    let all = whole["rows"].as_array().expect("rows").clone();
    assert_eq!(all.len(), 1200);
    for shards in [1, 3, 4, 7, 16] {
        let mut merged = Vec::new();
        for shard in 0..shards {
            let request = serde_json::json!({"method":"scan_shard","table":"wide","shard":shard,"shards":shards});
            let part: serde_json::Value = serde_json::from_str(&send(&mut c, &request.to_string()))?;
            assert_eq!(part["status"], "rows", "shards={shards} shard={shard}: {part}");
            merged.extend(part["rows"].as_array().expect("rows").iter().cloned());
        }
        assert_eq!(
            serde_json::Value::Array(merged),
            serde_json::Value::Array(all.clone()),
            "{shards} 个分片拼接后必须与整表同序"
        );
    }
    Ok(())
}

/// 分片扫描支持投影与空列，越界分片和未知名都明确报错
#[test]
fn scan_shard_projects_columns_and_rejects_bad_ranges() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, _data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"items","columns":[{"name":"id","ty":"int"},{"name":"name","ty":"str"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert_batch","table":"items","rows":[{"id":1,"name":"one"},{"id":2,"name":null}]}))?;
    for (columns, expected) in [
        (serde_json::json!(["name"]), serde_json::json!([{"name":"one"},{"name":null}])),
        (serde_json::json!([]), serde_json::json!([{},{}])),
    ] {
        let mut merged = Vec::new();
        for shard in 0..3 {
            let request = serde_json::json!({"method":"scan_shard","table":"items","columns":columns,"shard":shard,"shards":3});
            let part: serde_json::Value = serde_json::from_str(&send(&mut c, &request.to_string()))?;
            assert_eq!(part["status"], "rows", "{part}");
            merged.extend(part["rows"].as_array().expect("rows").iter().cloned());
        }
        assert_eq!(serde_json::Value::Array(merged), expected);
    }
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"blank","columns":[{"name":"id","ty":"int"}]}))?;
    for (line, why) in [
        (r#"{"method":"scan_shard","table":"blank","shard":0,"shards":4}"#, "空表"),
        (r#"{"method":"scan_shard","table":"items","shard":3,"shards":3}"#, "分片号越界"),
        (r#"{"method":"scan_shard","table":"items","shard":0,"shards":0}"#, "分片数为零"),
        (r#"{"method":"scan_shard","table":"missing","shard":0,"shards":2}"#, "未知表"),
        (r#"{"method":"scan_shard","table":"items","columns":["missing"],"shard":0,"shards":2}"#, "未知列"),
    ] {
        let result: serde_json::Value = serde_json::from_str(&send(&mut c, line))?;
        if why == "空表" {
            assert_eq!(result["rows"], serde_json::json!([]), "{line}");
        } else {
            assert_eq!(result["status"], "error", "{why}: {line} -> {result}");
        }
    }
    Ok(())
}

/// 多线程并发分片读同一实例，各分片结果与整表一致
#[test]
fn concurrent_shard_reads_share_one_instance() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, _data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"wide","columns":[{"name":"id","ty":"int"},{"name":"name","ty":"str"}]}))?;
    let rows: Vec<serde_json::Value> = (1..=600)
        .map(|i| serde_json::json!({"id": i, "name": format!("row-{i:04}")}))
        .collect();
    request_ok(&mut c, serde_json::json!({"method":"insert_batch","table":"wide","rows":rows}))?;

    let storage = CURRENT.with(|slot| slot.borrow().as_ref().cloned().expect("storage opened"));
    let shards = 4;
    let parts = std::thread::scope(|scope| {
        let handles: Vec<_> = (0..shards)
            .map(|shard| {
                let storage = Arc::clone(&storage);
                scope.spawn(move || {
                    let request = format!(
                        r#"{{"method":"scan_shard","database":"{FIXTURE_DATABASE}","table":"wide","shard":{shard},"shards":{shards}}}"#
                    );
                    let mut rows = Vec::new();
                    for _ in 0..4 {
                        let reply = storage.request_line(&request);
                        let value: serde_json::Value = serde_json::from_str(&reply).expect("json");
                        assert_eq!(value["status"], "rows", "{reply}");
                        rows = value["rows"].as_array().expect("rows").clone();
                    }
                    rows
                })
            })
            .collect();
        handles
            .into_iter()
            .map(|h| h.join().expect("worker thread"))
            .collect::<Vec<_>>()
    });

    let whole: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"wide"}"#))?;
    let merged: Vec<serde_json::Value> = parts.into_iter().flatten().collect();
    assert_eq!(merged.len(), 600, "并发分片不能丢行");
    assert_eq!(serde_json::Value::Array(merged), whole["rows"]);
    Ok(())
}

/// 分片读与同表写并发时读者只看到一致快照
#[test]
fn concurrent_shard_reads_exclude_writers() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, _data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"wide","columns":[{"name":"id","ty":"int"},{"name":"name","ty":"str"}]}))?;
    let rows: Vec<serde_json::Value> = (1..=400)
        .map(|i| serde_json::json!({"id": i, "name": format!("row-{i:04}")}))
        .collect();
    request_ok(&mut c, serde_json::json!({"method":"insert_batch","table":"wide","rows":rows}))?;

    let storage = CURRENT.with(|slot| slot.borrow().as_ref().cloned().expect("storage opened"));
    let shards = 3;
    let inserted = 20;
    std::thread::scope(|scope| {
        let writer = {
            let storage = Arc::clone(&storage);
            scope.spawn(move || {
                for i in 401..=400 + inserted {
                    let request = format!(
                        r#"{{"method":"insert","database":"{FIXTURE_DATABASE}","table":"wide","row":{{"id":{i},"name":"row-{i:04}"}}}}"#
                    );
                    let reply = storage.request_line(&request);
                    let value: serde_json::Value = serde_json::from_str(&reply).expect("json");
                    assert_eq!(value["status"], "ok", "{reply}");
                }
            })
        };
        let readers: Vec<_> = (0..3)
            .map(|_| {
                let storage = Arc::clone(&storage);
                scope.spawn(move || {
                    let mut seen = Vec::new();
                    for _ in 0..8 {
                        let mut total = 0usize;
                        for shard in 0..shards {
                            let request = format!(
                                r#"{{"method":"scan_shard","database":"{FIXTURE_DATABASE}","table":"wide","shard":{shard},"shards":{shards}}}"#
                            );
                            let reply = storage.request_line(&request);
                            let value: serde_json::Value = serde_json::from_str(&reply).expect("json");
                            assert_eq!(value["status"], "rows", "{reply}");
                            total += value["rows"].as_array().expect("rows").len();
                        }
                        seen.push(total);
                    }
                    seen
                })
            })
            .collect();
        for reader in readers {
            let seen: Vec<usize> = reader.join().expect("reader thread");
            assert!(
                seen.iter().all(|n| (400..=400 + inserted).contains(n)),
                "分片读看到越界行数: {seen:?}"
            );
            assert!(
                seen.windows(2).all(|pair| pair[0] <= pair[1]),
                "并发写时快照行数倒退: {seen:?}"
            );
        }
        writer.join().expect("writer thread");
    });

    let whole: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"wide"}"#))?;
    assert_eq!(whole["rows"].as_array().expect("rows").len(), 400 + inserted);
    Ok(())
}

/// 跨库并发互不串数据，目录查询不被写饿死
#[test]
fn concurrent_requests_across_databases_stay_isolated() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, _data) = start_server();
    let mut c = connect()?;
    let request = |c: &mut Conn, value: serde_json::Value| -> Result<serde_json::Value, serde_json::Error> {
        serde_json::from_str(&send(c, &value.to_string()))
    };
    let databases = ["alpha", "beta"];
    for database in databases {
        assert_eq!(request(&mut c, serde_json::json!({"method":"create_database","database":database}))?["status"], "ok");
        assert_eq!(
            request(&mut c, serde_json::json!({"method":"create_table","database":database,"table":"items","columns":[{"name":"id","ty":"int"}]}))?["status"],
            "ok"
        );
    }

    let storage = CURRENT.with(|slot| slot.borrow().as_ref().cloned().expect("storage opened"));
    let written = 40;
    std::thread::scope(|scope| {
        let writers: Vec<_> = databases
            .iter()
            .map(|database| {
                let storage = Arc::clone(&storage);
                let database = database.to_string();
                scope.spawn(move || {
                    for i in 1..=written {
                        let line = format!(
                            r#"{{"method":"insert","database":"{database}","table":"items","row":{{"id":{i}}}}}"#
                        );
                        let value: serde_json::Value = serde_json::from_str(&storage.request_line(&line)).expect("json");
                        assert_eq!(value["status"], "ok", "{database}: {value}");
                    }
                    database
                })
            })
            .collect();
        let catalog = {
            let storage = Arc::clone(&storage);
            scope.spawn(move || {
                for _ in 0..40 {
                    for line in [r#"{"method":"list_databases"}"#, r#"{"method":"all_catalogs"}"#] {
                        let value: serde_json::Value = serde_json::from_str(&storage.request_line(line)).expect("json");
                        assert_ne!(value["status"], "error", "{line}: {value}");
                    }
                }
            })
        };
        for writer in writers {
            writer.join().expect("writer thread");
        }
        catalog.join().expect("catalog thread");
    });

    for database in databases {
        let rows = request(&mut c, serde_json::json!({"method":"scan","database":database,"table":"items"}))?["rows"].clone();
        assert_eq!(rows.as_array().expect("rows").len(), written, "{database} 行数不对");
    }
    Ok(())
}

/// 删列不改堆字节，重启后不再暴露该列
#[test]
fn drop_column_keeps_heap_bytes_and_hides_values_after_restart() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
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
    let _server = start_server_in(data.path());
    let mut c = connect()?;
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

/// 各库字典与行互相隔离，删库不影响别库
#[test]
fn databases_isolate_catalog_and_rows() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, data) = start_server();
    let mut c = connect()?;
    let request = |c: &mut Conn, value: serde_json::Value| -> Result<serde_json::Value, serde_json::Error> {
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


/// 隐藏列的 WAL 重放两次幂等，且不动堆文件
#[test]
fn hidden_column_wal_replays_twice_without_touching_heap() -> Result<(), Box<dyn std::error::Error>> {
    use chusql_core_storage::wal::{Wal, WalOp};
    let (server, data) = start_server();
    let mut c = connect()?;
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
        let server = start_server_in(data.path());
        let mut c = connect()?;
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

/// 删列失败时保留 WAL，重启后补做并放行
#[test]
fn hidden_column_failure_preserves_wal_until_recovery() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
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
    let _server = start_server_in(data.path());
    let mut c = connect()?;
    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":1}]));
    Ok(())
}

/// 删带索引的列会连索引文件一起删掉
#[test]
fn drop_column_with_a_live_index_removes_the_index_file() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, data) = start_server();
    let mut c = connect()?;
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

/// 整理表会重写页面，清掉隐藏列字节
#[test]
fn compact_reclaims_hidden_column_bytes() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
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
    let _server = start_server_in(data.path());
    let mut c = connect()?;
    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items"}"#))?;
    assert_eq!(rows["rows"].as_array().map(Vec::len), Some(4), "rows survive compaction: {}", rows);
    Ok(())
}

/// 启动时自动整理带隐藏列的表
#[test]
fn startup_compaction_reclaims_dropped_columns() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":1,"secret":"padding"}}))?;
    request_ok(&mut c, serde_json::json!({"method":"drop_column","table":"items","column":"secret"}))?;
    let heap = data.path().join("databases/main/items.db");
    assert!(std::fs::read(&heap)?.windows(6).any(|w| w == &b"secret"[..]));
    drop(c);
    drop(server);

    let _server = start_server_in(data.path());
    let mut c = connect()?;
    assert!(
        !std::fs::read(&heap)?.windows(6).any(|w| w == &b"secret"[..]),
        "startup must compact tables that still carry hidden columns"
    );
    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":1}]));
    Ok(())
}

/// 没有隐藏列时整理表只原地回收页内空间
#[test]
fn compact_defragments_pages_without_dropped_columns() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, data) = start_server();
    let mut c = connect()?;
    let pad = "p".repeat(200);
    for id in 1..=5 {
        request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":id,"note":pad}}))?;
    }
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":6,"note":"doomed-row-marker"}}))?;
    for id in 7..=11 {
        request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":id,"note":pad}}))?;
    }
    request_ok(&mut c, serde_json::json!({"method":"delete_keys","table":"items","keys":[6]}))?;

    let heap = data.path().join("databases/main/items.db");
    assert!(
        std::fs::read(&heap)?.windows(17).any(|w| w == &b"doomed-row-marker"[..]),
        "删行只留墓碑，字节还在页里"
    );
    request_ok(&mut c, serde_json::json!({"method":"compact","table":"items"}))?;
    assert!(
        !std::fs::read(&heap)?.windows(17).any(|w| w == &b"doomed-row-marker"[..]),
        "原地整理要把墓碑字节收掉"
    );
    assert_eq!(std::fs::metadata(data.path().join("databases/main/wal.log"))?.len(), 0);

    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items"}"#))?;
    assert_eq!(rows["rows"].as_array().map(Vec::len), Some(10), "rows survive compaction: {}", rows);
    Ok(())
}

/// 非法库名被拒；system 是唯一保留库
#[test]
fn databases_reject_unsafe_names_and_keep_only_system_reserved() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, data) = start_server();
    let mut c = connect()?;
    for database in ["", "../escape", "a/b", "a.b", "CON", "__system_users"] {
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

/// 账号只存在 system 库，默认工作库不存在
#[test]
fn system_database_owns_accounts_and_nothing_is_default() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, data) = start_server();
    let mut c = connect()?;
    // 启动只建 system，另一个是测试自己建的 main
    let listed: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"list_databases"}"#))?;
    assert_eq!(listed["tables"], serde_json::json!(["main", "system"]));
    let created: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"account_create","user":"alice","password_hash":"hash-1"}"#))?;
    assert_eq!(created["status"], "accounts", "{created}");
    assert!(data.path().join("system/__system_users.db").is_file());
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

/// 裸表名没选库会被拒；限定名不用选库
#[test]
fn unqualified_requests_need_a_selected_database() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, _data) = start_server();
    let mut c = connect()?;
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
fn request_ok(c: &mut Conn, value: serde_json::Value) -> Result<(), Box<dyn std::error::Error>> {
    let response: serde_json::Value = serde_json::from_str(&send(c, &value.to_string()))?;
    assert_eq!(response["status"], "ok", "{value}: {response}");
    Ok(())
}

/// 一次存储实例的持有者
struct ServerProc;

impl Drop for ServerProc {
    /// 实例由本线程 CURRENT 管，这里不用清理
    fn drop(&mut self) {}
}

/// 起存储，返回句柄与数据目录
fn start_server() -> (ServerProc, tempfile::TempDir) {
    let data = tempfile::tempdir().unwrap();
    let server = start_server_in(data.path());
    (server, data)
}

/// 在指定数据目录起存储、引导系统目录并建测试库
fn start_server_in(data: &std::path::Path) -> ServerProc {
    open_storage(&write_config(data));
    bootstrap_system();
    create_fixture_database();
    ServerProc
}

/// 按配置文件打开存储，替换本线程上一个实例
fn open_storage(config: &std::path::Path) {
    CURRENT.with(|slot| *slot.borrow_mut() = None);
    let storage = Arc::new(Storage::open(Some(config.to_str().unwrap())).expect("open storage"));
    CURRENT.with(|slot| *slot.borrow_mut() = Some(storage));
}

/// 生成只写数据目录的临时配置文件
fn write_config(data: &std::path::Path) -> std::path::PathBuf {
    let id = std::process::id();
    let ns = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap()
        .as_nanos();
    let path = data.join(format!("chusql-{}-{}.toml", id, ns));
    let text = format!("[storage]\ndata_dir = '{}'\n", data.display());
    std::fs::write(&path, text).unwrap();
    path
}

/// 命名库重启后仍在，账号仍是全局的
#[test]
fn named_database_survives_restart_and_accounts_stay_global() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
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
    let _restarted = start_server_in(data.path());
    let mut c = connect()?;
    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"sales.items"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":7}]));
    let accounts: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"accounts_list","database":"sales"}"#))?;
    assert_eq!(accounts["status"], "accounts");
    let hidden: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"sales.__system_users"}"#))?;
    assert_eq!(hidden["status"], "error");
    Ok(())
}

/// 连到本线程当前的存储：进程内实现就是本线程当前句柄
fn connect() -> std::io::Result<Conn> {
    CURRENT.with(|slot| slot.borrow().clone())
        .map(Conn)
        .ok_or_else(|| std::io::Error::other("storage not started"))
}

/// 测试用的库名：system 之外的库由测试自建
const FIXTURE_DATABASE: &str = "main";

/// 请求没写库名时补上测试库
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
fn create_fixture_database() {
    if let Ok(mut c) = connect() {
        let _ = send_raw(&mut c, &format!(r#"{{"method":"create_database","database":"{FIXTURE_DATABASE}"}}"#));
    }
}

/// 引导系统目录：建账号表，不塞账号
fn bootstrap_system() {
    let reply = CURRENT.with(|slot| {
        let storage = slot.borrow().as_ref().cloned().expect("storage opened");
        storage.request_line(r#"{"method":"bootstrap_system"}"#)
    });
    assert!(reply.contains(r#""status":"system""#), "{reply}");
}

/// 发一行请求读一行响应（没写库名就补测试库）
fn send(c: &mut Conn, line: &str) -> String {
    send_raw(c, &with_default_database(line))
}

/// 原样发一行请求，不补库名——用来验证"没选库"的行为
fn send_raw(c: &mut Conn, line: &str) -> String {
    c.0.request_line(line).trim().to_string()
}

/// 只有字典登记的表才算存在，孤儿文件不算
#[test]
fn catalog_is_the_only_table_authority() -> std::io::Result<()> {
    let (_srv, data) = start_server();
    std::fs::write(data.path().join("databases/main/orphan.db"), b"orphan")?;
    let mut c = connect()?;
    for method in ["scan", "describe_table"] {
        let result = send(&mut c, &format!(r#"{{"method":"{method}","table":"orphan"}}"#));
        assert!(result.contains("unknown table"), "{result}");
    }
    let result = send(&mut c, r#"{"method":"list_tables"}"#);
    assert!(!result.contains("orphan"), "{result}");
    Ok(())
}

/// 保留系统表拒绝所有通用操作
#[test]
fn reserved_tables_reject_every_generic_operation() -> std::io::Result<()> {
    let (_srv, _data) = start_server();
    let mut c = connect()?;
    for method in ["scan", "insert", "insert_batch", "delete_keys", "lookup_by_index",
        "replace_all", "describe_table", "create_table", "drop_table", "create_index",
        "drop_index", "drop_column"] {
        let request = serde_json::json!({"method": method, "table": "__system_users",
            "column": "id", "key": 1, "keys": [], "row": {"id": 1}, "rows": [], "columns": []});
        let result = send(&mut c, &request.to_string());
        assert!(result.contains("reserved system table"), "{method}: {result}");
    }
    Ok(())
}

/// 通用接口拒绝表名与列名里的路径别名
#[test]
fn generic_ipc_rejects_path_aliases() -> std::io::Result<()> {
    let (_srv, _data) = start_server();
    let mut c = connect()?;
    for table in ["../__system_users", "__SYSTEM_USERS", "__system_users."] {
        let result = send(&mut c, &serde_json::json!({"method":"scan", "table":table}).to_string());
        assert!(result.contains("error"), "{result}");
    }
    let result = send(&mut c, r#"{"method":"create_index","table":"business","column":"../__system_users"}"#);
    assert!(result.contains("invalid column name"), "{result}");
    Ok(())
}

/// 账号增改删，且普通列表接口看不到账号表
#[test]
fn account_create_reset_drop_and_hidden_from_ordinary_api() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, _data) = start_server();
    let mut c = connect()?;
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
        assert!(!result.contains("__system_users"), "{result}");
    }
    Ok(())
}

/// 用同一份数据目录重开一次存储
fn restart_at(data: &std::path::Path) -> std::io::Result<ServerProc> {
    open_storage(&write_config(data));
    bootstrap_system();
    create_fixture_database();
    Ok(ServerProc)
}

/// 账号快照 WAL 重放两次不产生重复账号
#[test]
fn account_snapshot_replays_twice_without_duplicating_accounts() -> Result<(), Box<dyn std::error::Error>> {
    use chusql_core_storage::{protocol::Account, wal::{Wal, WalOp}};
    let (server, data) = start_server();
    let mut c = connect()?;
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
        wal.append(&WalOp::Accounts { accounts: accounts.clone(), removed: Vec::new(), migrate_roles: false })?;
        drop(wal);
        std::fs::write(data.path().join("system/__system_users.idx"), b"torn index")?;
        std::fs::write(data.path().join("system/__system_users.db"), b"torn heap")?;
        let restarted = restart_at(data.path())?;
        let mut connection = connect()?;
        let actual: serde_json::Value = serde_json::from_str(&send(&mut connection, r#"{"method":"accounts_list"}"#))?;
        assert_eq!(actual, changed);
        assert_eq!(std::fs::metadata(data.path().join("system/wal.log"))?.len(), 0);
        drop(connection);
        drop(restarted);
    }
    Ok(())
}

/// 账号操作失败时保留 WAL 并挡住写入
#[test]
fn account_failure_preserves_wal_and_blocks_writes() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
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
    let _restarted = restart_at(data.path())?;
    let mut c = connect()?;
    let recovered = send(&mut c, r#"{"method":"accounts_list"}"#);
    assert!(recovered.contains("hash-one"), "{recovered}");
    let status: serde_json::Value =
        serde_json::from_str(&send(&mut c, r#"{"method":"system_status"}"#))?;
    assert!(
        status["last_lsn"].as_u64().unwrap_or(0) >= 1,
        "重放后检查点要推进: {status}"
    );
    Ok(())
}

/// 账号文件已存在时拒绝建账号
#[test]
fn account_create_rejects_reserved_file_collision() -> std::io::Result<()> {
    let data = tempfile::tempdir()?;
    open_storage(&write_config(data.path()));
    std::fs::write(data.path().join("system/__system_users.db"), b"existing")?;
    let mut c = connect()?;
    let result = send(&mut c, r#"{"method":"account_create","user":"alice","password_hash":"hash-one"}"#);
    assert!(result.contains("collision"), "{result}");
    assert_eq!(std::fs::read(data.path().join("system/__system_users.db"))?, b"existing");
    Ok(())
}

/// 超长密码哈希在写 WAL 之前就被拒
#[test]
fn oversized_account_hash_is_rejected_before_wal() -> Result<(), Box<dyn std::error::Error>> {
    let (_server, data) = start_server();
    let mut c = connect()?;
    let result = send(&mut c, &serde_json::json!({"method":"account_create", "user":"alice",
        "password_hash":"a".repeat(300)}).to_string());
    assert!(result.contains("invalid account credential"), "{result}");
    let wal = data.path().join("system/wal.log");
    assert_eq!(if wal.exists() { std::fs::metadata(&wal)?.len() } else { 0 }, 0);
    assert_eq!(serde_json::from_str::<serde_json::Value>(&send(&mut c, r#"{"method":"accounts_list"}"#))?["accounts"], serde_json::json!([]));
    Ok(())
}

/// 空哈希表示免密，允许建立
#[test]
fn account_create_accepts_empty_hash() -> std::io::Result<()> {
    let (_server, _data) = start_server();
    let mut c = connect()?;
    let created: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"account_create","user":"root","password_hash":""}"#))?;
    assert_eq!(created["accounts"][0]["password_hash"], serde_json::json!(""));
    let listed = send(&mut c, r#"{"method":"accounts_list"}"#);
    assert!(listed.contains("root"), "{listed}");
    Ok(())
}

/// 字典里已有同名普通表时拒绝建账号
#[test]
fn account_create_rejects_existing_ordinary_catalog_entry() -> std::io::Result<()> {
    let data = tempfile::tempdir()?;
    let mut catalog = chusql_core_storage::catalog::Catalog::default();
    catalog.create_table("__system_users", Vec::new())?;
    std::fs::create_dir(data.path().join("system"))?;
    catalog.save(data.path().join("system/catalog.json"))?;
    open_storage(&write_config(data.path()));
    let mut c = connect()?;
    let result = send(&mut c, r#"{"method":"account_create","user":"alice","password_hash":"hash-one"}"#);
    assert!(result.contains("collision"), "{result}");
    Ok(())
}

/// 没引导过就不能动账号
#[test]
fn account_requests_require_bootstrap() -> std::io::Result<()> {
    let data = tempfile::tempdir()?;
    open_storage(&write_config(data.path()));
    let mut c = connect()?;
    let status = send(&mut c, r#"{"method":"system_status"}"#);
    assert!(status.contains(r#""initialized":false"#), "{status}");
    for request in [r#"{"method":"accounts_list"}"#,
        r#"{"method":"account_create","user":"alice","password_hash":"hash-one"}"#] {
        let result = send(&mut c, request);
        assert!(result.contains("csql-bootstrap"), "{request}: {result}");
    }
    Ok(())
}

/// 引导幂等：再跑一次不改已有管理员的口令
#[test]
fn bootstrap_is_idempotent_and_keeps_the_password() -> std::io::Result<()> {
    let data = tempfile::tempdir()?;
    open_storage(&write_config(data.path()));
    let mut c = connect()?;
    let created = send(&mut c, r#"{"method":"bootstrap_system","user":"root","password_hash":"hash-one"}"#);
    assert!(created.contains(r#""initialized":true"#), "{created}");
    let again = send(&mut c, r#"{"method":"bootstrap_system","user":"root","password_hash":"hash-two"}"#);
    assert!(again.contains(r#""initialized":true"#), "{again}");
    let listed = send(&mut c, r#"{"method":"accounts_list"}"#);
    assert!(listed.contains("hash-one"), "{listed}");
    assert!(!listed.contains("hash-two"), "{listed}");
    assert_eq!(std::fs::metadata(data.path().join("system/wal.log"))?.len(), 0);
    Ok(())
}

/// 引导程序不给已经播过种的表塞新账号
#[test]
fn bootstrap_does_not_add_an_account_to_a_seeded_table() -> std::io::Result<()> {
    let data = tempfile::tempdir()?;
    open_storage(&write_config(data.path()));
    let mut c = connect()?;
    send(&mut c, r#"{"method":"bootstrap_system","user":"root","password_hash":"hash-one"}"#);
    let sneaky = send(&mut c, r#"{"method":"bootstrap_system","user":"eve","password_hash":"raw-hash"}"#);
    assert!(sneaky.contains(r#""initialized":true"#), "{sneaky}");
    let listed = send(&mut c, r#"{"method":"accounts_list"}"#);
    assert!(!listed.contains("eve"), "{listed}");
    assert!(listed.contains("hash-one"), "{listed}");
    Ok(())
}

/// 空表还能被重新播种：先只建表，再补管理员
#[test]
fn bootstrap_seeds_an_empty_account_table() -> std::io::Result<()> {
    let data = tempfile::tempdir()?;
    open_storage(&write_config(data.path()));
    let mut c = connect()?;
    let bare = send(&mut c, r#"{"method":"bootstrap_system"}"#);
    assert!(bare.contains(r#""initialized":true"#), "{bare}");
    let empty = send(&mut c, r#"{"method":"accounts_list"}"#);
    assert!(empty.contains(r#""accounts":[]"#), "{empty}");
    let seeded = send(&mut c, r#"{"method":"bootstrap_system","user":"root","password_hash":"hash-again"}"#);
    assert!(seeded.contains(r#""initialized":true"#), "{seeded}");
    let listed = send(&mut c, r#"{"method":"accounts_list"}"#);
    assert!(listed.contains("hash-again"), "{listed}");
    Ok(())
}

/// 列出已建的表
#[test]
fn list_tables_returns_created_tables() {
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

    send(&mut c, r#"{"method":"insert","table":"users","row":{"id":1}}"#);
    send(&mut c, r#"{"method":"insert","table":"orders","row":{"id":1}}"#);

    let r = send(&mut c, r#"{"method":"list_tables"}"#);
    assert!(r.contains("users"), "got {}", r);
    assert!(r.contains("orders"), "got {}", r);
}

/// 整表替换只剩新行
#[test]
fn replace_all_replaces_rows() {
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

    let req = r#"{"method":"insert","table":"dup_id","row":{"id":1,"name":"A"}}"#;
    assert!(send(&mut c, req).contains(r#""status":"ok""#));

    let r = send(&mut c, r#"{"method":"insert","table":"dup_id","row":{"id":1,"name":"B"}}"#);
    assert!(r.contains(r#""status":"error""#), "got: {}", r);
    assert!(r.contains("duplicate value on the id index"), "got: {}", r);
}

/// 整值浮点与整数是同一个索引键
#[test]
fn integral_float_uses_the_same_index_key() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, _data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"float_id","columns":[{"name":"id","ty":"int"},{"name":"name","ty":"str"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"float_id","row":{"id":7,"name":"A"}}))?;

    let found: serde_json::Value = serde_json::from_str(&send(
        &mut c,
        r#"{"method":"lookup_by_index","table":"float_id","key":7.0}"#,
    ))?;
    assert_eq!(
        found["rows"],
        serde_json::json!([{"id":7,"name":"A"}]),
        "lookup by 7.0: {}",
        found
    );

    let dup: serde_json::Value = serde_json::from_str(&send(
        &mut c,
        r#"{"method":"insert","table":"float_id","row":{"id":7.0,"name":"B"}}"#,
    ))?;
    assert_eq!(dup["status"], "error", "7.0 must clash with 7: {}", dup);
    assert!(
        dup["message"].as_str().unwrap_or("").contains("duplicate value on the id index"),
        "got: {}",
        dup
    );
    Ok(())
}

/// 批量插入一次全进
#[test]
fn insert_batch_writes_all_rows() {
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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

/// 事务提交：一批写操作在同一个请求里落地
#[test]
fn apply_transaction_lands_upserts_and_deletes_together() {
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

    for (id, name) in [(1, "Alice"), (2, "Bob")] {
        let req = format!(
            r#"{{"method":"insert","table":"txn_t","row":{{"id":{},"name":"{}"}}}}"#,
            id, name
        );
        send(&mut c, &req);
    }

    let batch = r#"{"method":"apply_transaction","ops":[
        {"op":"delete","table":"txn_t","ids":[1]},
        {"op":"upsert","table":"txn_t","rows":[{"id":3,"name":"Carol"}]}
    ]}"#;
    let r = send(&mut c, batch);
    assert!(r.contains(r#""status":"ok""#), "batch: {}", r);

    let r = send(&mut c, r#"{"method":"scan","table":"txn_t"}"#);
    assert!(!r.contains("Alice"), "deleted row should be gone: {}", r);
    assert!(r.contains("Bob") && r.contains("Carol"), "kept and added rows: {}", r);

    for line in [
        r#"{"method":"apply_transaction","ops":[{"op":"upsert","table":"__system_users","rows":[{"id":1}]}]}"#,
        r#"{"method":"apply_transaction","ops":[{"op":"delete","table":"../evil","ids":[1]}]}"#,
    ] {
        let r = send(&mut c, line);
        assert!(r.contains(r#""status":"error""#), "bad op rejected: {} -> {}", line, r);
    }

    // 没有整数 id 的表用整表替换提交
    let r = send(&mut c, r#"{"method":"insert","table":"txn_no_id","row":{"name":"old"}}"#);
    assert!(r.contains(r#""status":"ok""#), "insert: {}", r);
    let replace = r#"{"method":"apply_transaction","ops":[{"op":"replace","table":"txn_no_id","rows":[{"name":"only"}]}]}"#;
    let r = send(&mut c, replace);
    assert!(r.contains(r#""status":"ok""#), "replace: {}", r);
    let r = send(&mut c, r#"{"method":"scan","table":"txn_no_id"}"#);
    assert!(r.contains("only") && !r.contains("old"), "replaced rows: {}", r);
}

/// 标记已落盘、数据未改的崩溃现场：重启重放并推进检查点
#[test]
fn committed_group_replays_after_restart() -> Result<(), Box<dyn std::error::Error>> {
    use chusql_core_storage::wal::{Wal, WalOp};
    let (server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":1,"secret":"old"}}))?;
    drop(c);
    drop(server);

    let mut added = std::collections::HashMap::new();
    added.insert("id".to_string(), serde_json::json!(2));
    added.insert("secret".to_string(), serde_json::json!("new"));
    let group = vec![
        WalOp::InsertBatch {
            table: "items".into(),
            rows: vec![added.clone()],
        },
        WalOp::DeleteKeys {
            table: "items".into(),
            keys: vec![1],
        },
    ];
    let wal = Wal::new(data.path().join("databases/main/wal.log"));
    let commit_lsn = wal.append_group(&group)?;
    drop(wal);

    let _server = start_server_in(data.path());
    let mut c = connect()?;
    let rows: serde_json::Value =
        serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":2,"secret":"new"}]));
    assert_eq!(std::fs::metadata(data.path().join("databases/main/wal.log"))?.len(), 0);
    let checkpoint = std::fs::read(data.path().join("databases/main/wal.checkpoint"))?;
    assert_eq!(checkpoint.len(), 8, "检查点要落盘");
    assert_eq!(
        u64::from_le_bytes(checkpoint[..8].try_into()?),
        commit_lsn,
        "检查点记到提交 LSN"
    );
    Ok(())
}

/// 没有提交标记的记录组在重启时整组丢掉
#[test]
fn uncommitted_group_is_discarded_on_restart() -> Result<(), Box<dyn std::error::Error>> {
    use chusql_core_storage::wal::{Wal, WalOp};
    let (server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"items","row":{"id":1,"secret":"old"}}))?;
    drop(c);
    drop(server);

    let mut lost = std::collections::HashMap::new();
    lost.insert("id".to_string(), serde_json::json!(2));
    lost.insert("secret".to_string(), serde_json::json!("lost"));
    let wal = Wal::new(data.path().join("databases/main/wal.log"));
    wal.append(&WalOp::InsertBatch {
        table: "items".into(),
        rows: vec![lost.clone()],
    })?;
    wal.append(&WalOp::InsertBatch {
        table: "items".into(),
        rows: vec![lost],
    })?;
    drop(wal);

    let _server = start_server_in(data.path());
    let mut c = connect()?;
    let rows: serde_json::Value =
        serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"items"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":1,"secret":"old"}]));
    assert_eq!(std::fs::metadata(data.path().join("databases/main/wal.log"))?.len(), 0);
    Ok(())
}

/// 直接删表后文件不留，重建同名表是空表
#[test]
fn drop_table_removes_files() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"m4_gone","columns":[{"name":"id","ty":"int"},{"name":"note","ty":"text"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"create_index","table":"m4_gone","column":"note"}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"m4_gone","row":{"id":1,"note":"a"}}))?;
    assert!(data.path().join("databases/main/m4_gone.db").exists(), "建表要落数据文件");

    request_ok(&mut c, serde_json::json!({"method":"drop_table","table":"m4_gone"}))?;
    assert!(!data.path().join("databases/main/m4_gone.db").exists(), "数据文件要删掉");
    assert!(!data.path().join("databases/main/m4_gone.idx").exists(), "id 索引要删掉");
    assert!(!data.path().join("databases/main/m4_gone.note.idx").exists(), "二级索引要删掉");

    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"m4_gone","columns":[{"name":"id","ty":"int"},{"name":"note","ty":"text"}]}))?;
    let rows: serde_json::Value =
        serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"m4_gone"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([]), "旧数据不能复活");
    Ok(())
}

/// 建表操作进了 WAL 后，重启按提交组把表重建出来
#[test]
fn create_table_replays_after_restart() -> Result<(), Box<dyn std::error::Error>> {
    use chusql_core_storage::protocol::SchemaColumn;
    use chusql_core_storage::wal::{Wal, WalOp};
    let (server, data) = start_server();
    drop(server);

    let group = vec![WalOp::CreateTable {
        table: "m4_create".into(),
        columns: vec![
            SchemaColumn {
                name: "id".into(),
                ty: "int".into(),
                ..Default::default()
            },
            SchemaColumn {
                name: "note".into(),
                ty: "text".into(),
                ..Default::default()
            },
        ],
    }];
    let wal = Wal::new(data.path().join("databases/main/wal.log"));
    let commit_lsn = wal.append_group(&group)?;
    drop(wal);

    let _server = start_server_in(data.path());
    let mut c = connect()?;
    let schema: serde_json::Value =
        serde_json::from_str(&send(&mut c, r#"{"method":"describe_table","table":"m4_create"}"#))?;
    assert_eq!(schema["status"], "schema", "{schema}");
    assert_eq!(schema["columns"][1]["name"], "note", "{schema}");
    assert!(data.path().join("databases/main/m4_create.db").exists(), "建表要落数据文件");
    let checkpoint = std::fs::read(data.path().join("databases/main/wal.checkpoint"))?;
    assert_eq!(u64::from_le_bytes(checkpoint[..8].try_into()?), commit_lsn);
    Ok(())
}

/// 删表操作进了 WAL 后，重启把表和数据文件一起清掉
#[test]
fn drop_table_replays_after_restart() -> Result<(), Box<dyn std::error::Error>> {
    use chusql_core_storage::wal::{Wal, WalOp};
    let (server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"m4_drop","columns":[{"name":"id","ty":"int"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"m4_drop","row":{"id":1}}))?;
    drop(c);
    drop(server);

    let wal = Wal::new(data.path().join("databases/main/wal.log"));
    wal.append_group(&[WalOp::DropTable {
        table: "m4_drop".into(),
    }])?;
    drop(wal);

    let _server = start_server_in(data.path());
    let mut c = connect()?;
    let schema: serde_json::Value =
        serde_json::from_str(&send(&mut c, r#"{"method":"describe_table","table":"m4_drop"}"#))?;
    assert_eq!(schema["status"], "error", "{schema}");
    let left: Vec<String> = std::fs::read_dir(data.path().join("databases/main"))?
        .map(|e| e.unwrap().file_name().to_string_lossy().into_owned())
        .collect();
    assert!(!data.path().join("databases/main/m4_drop.db").exists(), "数据文件要删掉，现存 {left:?}");
    assert!(!data.path().join("databases/main/m4_drop.idx").exists(), "id 索引要删掉，现存 {left:?}");
    Ok(())
}

/// 建索引操作进了 WAL 后，重启补建索引文件
#[test]
fn create_index_replays_after_restart() -> Result<(), Box<dyn std::error::Error>> {
    use chusql_core_storage::wal::{Wal, WalOp};
    let (server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"m4_idx","columns":[{"name":"id","ty":"int"},{"name":"note","ty":"text"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"m4_idx","row":{"id":1,"note":"a"}}))?;
    drop(c);
    drop(server);

    let wal = Wal::new(data.path().join("databases/main/wal.log"));
    wal.append_group(&[WalOp::CreateIndex {
        table: "m4_idx".into(),
        column: "note".into(),
    }])?;
    drop(wal);

    let _server = start_server_in(data.path());
    let mut c = connect()?;
    let schema: serde_json::Value =
        serde_json::from_str(&send(&mut c, r#"{"method":"describe_table","table":"m4_idx"}"#))?;
    assert_eq!(schema["status"], "schema", "{schema}");
    let indexed = |schema: &serde_json::Value, column: &str| {
        schema["indexes"]
            .as_array()
            .map(|list| list.iter().any(|i| i["column"] == column))
            .unwrap_or(false)
    };
    assert!(indexed(&schema, "note"), "{schema}");
    assert!(data.path().join("databases/main/m4_idx.note.idx").exists(), "索引文件要建出来");
    let found: serde_json::Value = serde_json::from_str(&send(
        &mut c,
        r#"{"method":"lookup_by_index","table":"m4_idx","column":"note","key":"a"}"#,
    ))?;
    assert_eq!(found["rows"], serde_json::json!([{"id":1,"note":"a"}]), "{found}");
    Ok(())
}

/// 删索引操作进了 WAL 后，重启清掉索引定义和文件
#[test]
fn drop_index_replays_after_restart() -> Result<(), Box<dyn std::error::Error>> {
    use chusql_core_storage::wal::{Wal, WalOp};
    let (server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"m4_didx","columns":[{"name":"id","ty":"int"},{"name":"note","ty":"text"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"create_index","table":"m4_didx","column":"note"}))?;
    drop(c);
    drop(server);

    let wal = Wal::new(data.path().join("databases/main/wal.log"));
    wal.append_group(&[WalOp::DropIndex {
        table: "m4_didx".into(),
        column: "note".into(),
    }])?;
    drop(wal);

    let _server = start_server_in(data.path());
    let mut c = connect()?;
    let schema: serde_json::Value =
        serde_json::from_str(&send(&mut c, r#"{"method":"describe_table","table":"m4_didx"}"#))?;
    assert_eq!(schema["status"], "schema", "{schema}");
    let columns: Vec<&str> = schema["indexes"]
        .as_array()
        .map(|list| list.iter().filter_map(|i| i["column"].as_str()).collect())
        .unwrap_or_default();
    assert_eq!(columns, vec!["id"], "{schema}");
    assert!(!data.path().join("databases/main/m4_didx.note.idx").exists(), "索引文件要删掉");
    Ok(())
}

/// 已生效的建表记录重启时被跳过，不报恢复失败
#[test]
fn already_applied_create_table_is_skipped_on_restart() -> Result<(), Box<dyn std::error::Error>> {
    use chusql_core_storage::protocol::SchemaColumn;
    use chusql_core_storage::wal::{Wal, WalOp};
    let (server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"m4_done","columns":[{"name":"id","ty":"int"}]}))?;
    drop(c);
    drop(server);

    let wal = Wal::new(data.path().join("databases/main/wal.log"));
    wal.append_group(&[WalOp::CreateTable {
        table: "m4_done".into(),
        columns: vec![SchemaColumn {
            name: "id".into(),
            ty: "int".into(),
            ..Default::default()
        }],
    }])?;
    drop(wal);

    let _server = start_server_in(data.path());
    let mut c = connect()?;
    let schema: serde_json::Value =
        serde_json::from_str(&send(&mut c, r#"{"method":"describe_table","table":"m4_done"}"#))?;
    assert_eq!(schema["status"], "schema", "{schema}");
    assert_eq!(std::fs::metadata(data.path().join("databases/main/wal.log"))?.len(), 0);
    Ok(())
}

/// 检查点跟着每组提交走，LSN 跨重启不回头
#[test]
fn checkpoint_advances_and_lsn_survives_restart() -> Result<(), Box<dyn std::error::Error>> {
    let checkpoint = |data: &tempfile::TempDir| -> Result<u64, Box<dyn std::error::Error>> {
        let bytes = std::fs::read(data.path().join("databases/main/wal.checkpoint"))?;
        Ok(u64::from_le_bytes(bytes[..8].try_into()?))
    };

    let (server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","table":"m6_seq","columns":[{"name":"id","ty":"int"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"m6_seq","row":{"id":1}}))?;
    drop(c);
    let first = checkpoint(&data)?;
    assert!(first > 0, "提交后检查点要前进: {first}");
    drop(server);

    // 重启后接着写，新的 LSN 必须落在检查点之后
    let _server = start_server_in(data.path());
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"insert","table":"m6_seq","row":{"id":2}}))?;
    drop(c);
    let second = checkpoint(&data)?;
    assert!(second > first, "重启后 LSN 要接着检查点走: {second} vs {first}");

    // 数据都在，日志本身是空的：每组提交后就按边界截掉了
    let mut c = connect()?;
    let rows: serde_json::Value =
        serde_json::from_str(&send(&mut c, r#"{"method":"scan","table":"m6_seq"}"#))?;
    assert_eq!(rows["rows"].as_array().map(Vec::len), Some(2), "{rows}");
    assert_eq!(
        std::fs::metadata(data.path().join("databases/main/wal.log"))?.len(),
        0
    );
    Ok(())
}

/// 给第二列建索引后能按那一列查
#[test]
fn create_index_on_secondary_column() {
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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

/// 重复值列能建索引，删一行不影响另一行；id 仍唯一
#[test]
fn create_index_accepts_duplicate_values() {
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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
    assert!(
        r.contains(r#""name":"age","distinct":2,"capped":false,"lo":30.0,"hi":41.0"#),
        "age carries a histogram range: {}",
        r
    );

    send(&mut c, r#"{"method":"delete_keys","table":"stat_t","keys":[1]}"#);
    let r = send(&mut c, r#"{"method":"describe_table","table":"stat_t"}"#);
    assert!(r.contains(r#""row_count":2"#), "after delete: {}", r);
    assert!(r.contains(r#""name":"age","distinct":2"#), "remaining ages are still 30 and 41: {}", r);
}

/// 重开服务后索引仍在
#[test]
fn lookup_survives_reopen() {
    let data = tempfile::tempdir().unwrap();

    {
        let _srv = start_server_in(data.path());
        let mut c = connect().unwrap();
        for (id, name) in [(1, "Alice"), (2, "Bob"), (3, "Carol")] {
            let req = format!(
                r#"{{"method":"insert","table":"persist","row":{{"id":{},"name":"{}"}},"key":{}}}"#,
                id, name, id
            );
            send(&mut c, &req);
        }
    }

    let _srv = start_server_in(data.path());

    let mut c = connect().unwrap();
    let r = send(&mut c, r#"{"method":"lookup_by_index","table":"persist","key":2}"#);
    assert!(r.contains("Bob"), "persist lookup: {}", r);
}

/// 配置文件真的生效
#[test]
fn config_file_is_used() {
    let dir = tempfile::tempdir().unwrap();
    let data_dir = dir.path().join("data");
    let config_path = dir.path().join("chusql-core-storage.toml");

    let text = format!(
        "[storage]\ndata_dir = \"{}\"\n[log]\nlevel = \"debug\"\n",
        data_dir.display().to_string().replace('\\', "/")
    );
    std::fs::write(&config_path, text).unwrap();

    open_storage(&config_path);
    bootstrap_system();
    create_fixture_database();

    let mut c = connect().unwrap();
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
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

    let r = send(
        &mut c,
        r#"{"method":"create_table","table":"empty_t","columns":[{"name":"id","ty":"int"}]}"#,
    );
    assert!(r.contains(r#""status":"ok""#), "create: {}", r);

    let r = send(&mut c, r#"{"method":"scan","table":"empty_t"}"#);
    assert!(r.contains(r#""status":"rows""#), "scan: {}", r);
    assert!(r.contains(r#""rows":[]"#), "scan: {}", r);
}

/// 约束元数据落盘；replace_schema 换列与行
#[test]
fn replace_schema_rewrites_columns_and_rows() {
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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

/// 启动时升级旧类型字典，缺的格补 null
#[test]
fn legacy_catalog_and_partial_rows_are_migrated() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
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

    let _restarted = start_server_in(data.path());
    let mut c = connect()?;

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

/// 老系统库的角色表启动时改到内部前缀
#[test]
fn legacy_privilege_tables_are_renamed_on_start() -> Result<(), Box<dyn std::error::Error>> {
    let (server, data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","database":"system","table":"sys_roles","columns":[{"name":"id","ty":"int"},{"name":"name","ty":"str"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"insert","database":"system","table":"sys_roles","row":{"id":1,"name":"admin"}}))?;
    drop(c);
    drop(server);

    let system_dir = data.path().join("system");
    assert!(system_dir.join("sys_roles.db").exists(), "老数据文件要先生成");

    let _restarted = start_server_in(data.path());
    let mut c = connect()?;

    let described: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"describe_table","database":"system","table":"__system_roles"}"#))?;
    assert_eq!(described["status"], "schema", "{described}");
    let rows: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","database":"system","table":"__system_roles"}"#))?;
    assert_eq!(rows["rows"], serde_json::json!([{"id":1,"name":"admin"}]), "行要跟着改名走");
    let old: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"describe_table","database":"system","table":"sys_roles"}"#))?;
    assert_eq!(old["status"], "error", "旧名不该还在：{old}");
    assert!(system_dir.join("__system_roles.db").exists(), "数据文件要改名");
    assert!(!system_dir.join("sys_roles.db").exists(), "老数据文件要改掉");
    Ok(())
}

/// 角色与授权表能被请求通道读到，账号表不能
#[test]
fn privilege_tables_are_reachable_but_account_table_is_not() -> Result<(), Box<dyn std::error::Error>> {
    let (_server, _data) = start_server();
    let mut c = connect()?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","database":"system","table":"__system_roles","columns":[{"name":"id","ty":"int"},{"name":"name","ty":"str"}]}))?;
    request_ok(&mut c, serde_json::json!({"method":"create_table","database":"system","table":"__system_grant_options","columns":[{"name":"id","ty":"int"},{"name":"role","ty":"str"}]}))?;
    let scanned: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","database":"system","table":"__system_roles"}"#))?;
    assert_eq!(scanned["status"], "rows", "{scanned}");
    let options: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","database":"system","table":"__system_grant_options"}"#))?;
    assert_eq!(options["status"], "rows", "{options}");
    let denied: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"scan","database":"system","table":"__system_users"}"#))?;
    assert_eq!(denied["status"], "error", "{denied}");
    assert!(
        denied["message"].as_str().unwrap_or("").contains("reserved system table"),
        "{denied}"
    );
    let listed: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"list_tables","database":"system"}"#))?;
    let tables: Vec<&str> = listed["tables"]
        .as_array()
        .ok_or("missing tables")?
        .iter()
        .map(|one| one.as_str().unwrap_or(""))
        .collect();
    assert!(tables.contains(&"__system_roles"), "授权表要在字典里: {listed}");
    assert!(tables.contains(&"__system_grant_options"), "转授权表要在字典里: {listed}");
    assert!(!tables.contains(&"__system_users"), "账号表不能露面: {listed}");
    Ok(())
}

/// 账号表落盘类型与索引，登录盖时间戳
#[test]
fn account_table_uses_typed_columns_and_stamps_login() -> Result<(), Box<dyn std::error::Error>> {
    let (_srv, data) = start_server();
    let mut c = connect()?;

    let created: serde_json::Value = serde_json::from_str(&send(&mut c, r#"{"method":"account_create","user":"alice","password_hash":"hash"}"#))?;
    assert_eq!(created["status"], "accounts", "{}", created);
    let account = &created["accounts"][0];
    assert_eq!(account["user"], "alice");
    assert_eq!(account["id"], 1);
    assert_eq!(account["registered_at"].as_str().unwrap_or("").len(), 19, "注册时间要写成 timestamp: {}", account);
    assert!(account["last_login_at"].is_null(), "还没登录过: {}", account);

    let catalog: serde_json::Value = serde_json::from_str(&std::fs::read_to_string(data.path().join("system/catalog.json"))?)?;
    let credential_columns = &catalog["tables"]["__system_users"]["columns"];
    assert_eq!(credential_columns.as_array().ok_or("missing credentials")?.iter().map(|c| c["name"].as_str().unwrap_or("")).collect::<Vec<_>>(), vec!["id", "password_hash"]);
    let entry = &catalog["tables"]["__system_identities"];
    let columns = entry["columns"].as_array().ok_or("missing columns")?;
    let names: Vec<&str> = columns.iter().map(|col| col["name"].as_str().unwrap_or("")).collect();
    assert_eq!(names, vec!["id", "user", "can_login", "is_superuser", "system_catalog_manager", "enabled", "identity_version", "registered_at", "last_login_at", "revision"]);
    let types: Vec<&str> = columns.iter().map(|col| col["ty"].as_str().unwrap_or("")).collect();
    assert_eq!(types, vec!["int", "varchar(64)", "bool", "bool", "bool", "bool", "int", "timestamp", "timestamp", "int"]);
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
fn string_index_lookup_in_process() {
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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

/// 范围扫描走索引，两端可开可闭，没索引回 no_index
#[test]
fn range_scan_in_process() {
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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
fn string_range_scan_in_process() {
    let (_srv, _data) = start_server();
    let mut c = connect().unwrap();

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
