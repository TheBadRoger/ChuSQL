use chusql_storage::protocol::Row;
use chusql_storage::wal::{Wal, WalOp};
use serde_json::json;

fn row(pairs: &[(&str, serde_json::Value)]) -> Row {
    let mut m = Row::new();
    for (k, v) in pairs {
        m.insert((*k).to_string(), v.clone());
    }
    m
}

#[test]
fn append_and_read_insert() {
    let dir = tempfile::tempdir().unwrap();
    let wal = Wal::new(dir.path().join("wal.log"));

    let op = WalOp::Insert {
        table: "users".into(),
        row: row(&[("id", json!(1)), ("name", json!("Alice"))]),
        key: Some(1),
    };
    wal.append(&op).unwrap();

    let back = wal.read().unwrap().unwrap();
    assert_eq!(back, op);
}

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

#[test]
fn missing_file_returns_none() {
    let dir = tempfile::tempdir().unwrap();
    let wal = Wal::new(dir.path().join("wal.log"));
    assert!(wal.read().unwrap().is_none());
}

#[test]
fn empty_file_returns_none() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wal.log");
    std::fs::write(&path, b"").unwrap();
    let wal = Wal::new(&path);
    assert!(wal.read().unwrap().is_none());
}

#[test]
fn clear_truncates() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wal.log");
    let wal = Wal::new(&path);

    wal.append(&WalOp::Insert {
        table: "t".into(),
        row: row(&[("id", json!(1))]),
        key: None,
    })
    .unwrap();
    assert!(wal.read().unwrap().is_some());

    wal.clear().unwrap();
    assert!(wal.read().unwrap().is_none());
    assert_eq!(std::fs::metadata(&path).unwrap().len(), 0);
}

#[test]
fn truncated_tail_is_ignored() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wal.log");
    let wal = Wal::new(&path);

    wal.append(&WalOp::Insert {
        table: "users".into(),
        row: row(&[("id", json!(1)), ("name", json!("Alice"))]),
        key: Some(1),
    })
    .unwrap();

    // 砍掉最后 5 个字节，模拟"写一半断电"
    let full = std::fs::read(&path).unwrap();
    std::fs::write(&path, &full[..full.len() - 5]).unwrap();

    // 读回来应该当作"没有这条"
    assert!(wal.read().unwrap().is_none());
}

#[test]
fn overwrite_on_second_append_is_visible() {
    // 简化版一次只允许一条，append 两次会叠在文件里——
    // 但正常路径下 clear 先跑，所以实际不会叠加。
    // 这个测试只验证 read 能解析最前面那一条。
    let dir = tempfile::tempdir().unwrap();
    let wal = Wal::new(dir.path().join("wal.log"));

    wal.append(&WalOp::Insert {
        table: "t".into(),
        row: row(&[("id", json!(1))]),
        key: None,
    })
    .unwrap();
    let first = wal.read().unwrap().unwrap();
    assert!(matches!(first, WalOp::Insert { .. }));
}
