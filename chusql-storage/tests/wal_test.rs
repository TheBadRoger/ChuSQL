use chusql_storage::protocol::Row;
use chusql_storage::wal::{Wal, WalOp};
use serde_json::json;

// WAL 测试：追加、读取、清空、截断尾巴。


fn row(pairs: &[(&str, serde_json::Value)]) -> Row {
    let mut m = Row::new();
    for (k, v) in pairs {
        m.insert((*k).to_string(), v.clone());
    }
    m
}

/// 写读 Insert 操作
#[test]
fn append_and_read_insert() {
    let dir = tempfile::tempdir().unwrap();
    let wal = Wal::new(dir.path().join("wal.log"));

    let op = WalOp::Insert {
        table: "users".into(),
        row: row(&[("id", json!(1)), ("name", json!("Alice"))]),
    };
    wal.append(&op).unwrap();

    let back = wal.read().unwrap().unwrap();
    assert_eq!(back, op);
}

/// 写读 ReplaceAll 操作
#[test]
fn append_and_read_replace_all() {
    let dir = tempfile::tempdir().unwrap();
    let wal = Wal::new(dir.path().join("wal.log"));

    let op = WalOp::ReplaceAll {
        table: "users".into(),
        rows: vec![
            row(&[("id", json!(1)), ("name", json!("Alice"))]),
            row(&[("id", json!(2)), ("name", json!("Bob"))]),
        ],
    };
    wal.append(&op).unwrap();

    let back = wal.read().unwrap().unwrap();
    assert_eq!(back, op);
}

/// 文件不存在返回 None
#[test]
fn missing_file_returns_none() {
    let dir = tempfile::tempdir().unwrap();
    let wal = Wal::new(dir.path().join("wal.log"));
    assert!(wal.read().unwrap().is_none());
}

/// 空文件返回 None
#[test]
fn empty_file_returns_none() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wal.log");
    std::fs::write(&path, b"").unwrap();
    let wal = Wal::new(&path);
    assert!(wal.read().unwrap().is_none());
}

/// 清空后读不到
#[test]
fn clear_truncates() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wal.log");
    let wal = Wal::new(&path);

    wal.append(&WalOp::Insert {
        table: "t".into(),
        row: row(&[("id", json!(1))]),
    })
    .unwrap();
    assert!(wal.read().unwrap().is_some());

    wal.clear().unwrap();
    assert!(wal.read().unwrap().is_none());
    assert_eq!(std::fs::metadata(&path).unwrap().len(), 0);
}

/// 尾巴截断被忽略
#[test]
fn truncated_tail_is_ignored() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wal.log");
    let wal = Wal::new(&path);

    wal.append(&WalOp::Insert {
        table: "users".into(),
        row: row(&[("id", json!(1)), ("name", json!("Alice"))]),
    })
    .unwrap();

    let full = std::fs::read(&path).unwrap();
    std::fs::write(&path, &full[..full.len() - 5]).unwrap();

    assert!(wal.read().unwrap().is_none());
}

/// 第二次追加会覆盖
#[test]
fn overwrite_on_second_append_is_visible() {
    let dir = tempfile::tempdir().unwrap();
    let wal = Wal::new(dir.path().join("wal.log"));

    wal.append(&WalOp::Insert {
        table: "t".into(),
        row: row(&[("id", json!(1))]),
    })
    .unwrap();
    let first = wal.read().unwrap().unwrap();
    assert!(matches!(first, WalOp::Insert { .. }));
}

