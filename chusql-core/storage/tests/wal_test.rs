use chusql_core_storage::protocol::Row;
use chusql_core_storage::wal::{Wal, WalOp};
use serde_json::json;

// WAL 测试：追加、读取、清空、截断尾巴。


/// 用键值对构造一行
fn row(pairs: &[(&str, serde_json::Value)]) -> Row {
    let mut m = Row::new();
    for (k, v) in pairs {
        m.insert((*k).to_string(), v.clone());
    }
    m
}

/// 三种操作写完都能原样读回
#[test]
fn append_and_read_round_trips() {
    let dir = tempfile::tempdir().unwrap();
    let wal = Wal::new(dir.path().join("wal.log"));

    let ops = [
        WalOp::Insert {
            table: "users".into(),
            row: row(&[("id", json!(1)), ("name", json!("Alice"))]),
        },
        WalOp::ReplaceAll {
            table: "users".into(),
            rows: vec![
                row(&[("id", json!(1)), ("name", json!("Alice"))]),
                row(&[("id", json!(2)), ("name", json!("Bob"))]),
            ],
        },
        WalOp::DropColumn {
            table: "users".into(),
            column: "age".into(),
            rows: vec![
                row(&[("id", json!(1)), ("name", json!("Alice"))]),
                row(&[("id", json!(2)), ("name", json!("Bob"))]),
            ],
        },
    ];
    for op in ops {
        wal.clear().unwrap();
        wal.append(&op).unwrap();
        assert_eq!(wal.read().unwrap().unwrap(), op);
    }
}

/// 文件不存在或为空都返回 None
#[test]
fn read_without_content_returns_none() {
    let dir = tempfile::tempdir().unwrap();
    let missing = Wal::new(dir.path().join("missing.log"));
    assert!(missing.read().unwrap().is_none());

    let path = dir.path().join("wal.log");
    std::fs::write(&path, b"").unwrap();
    let empty = Wal::new(&path);
    assert!(empty.read().unwrap().is_none());
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

