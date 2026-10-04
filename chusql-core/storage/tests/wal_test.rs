use chusql_core_storage::protocol::Row;
use chusql_core_storage::wal::{committed_ops, records_after, Wal, WalOp};
use serde_json::json;

// WAL 测试：追加、读取、清空、截断尾巴、LSN 与提交标记。


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

/// 每条记录带递增的 LSN，重开也从文件里的最大号往下走
#[test]
fn every_record_gets_an_increasing_lsn() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wal.log");
    let wal = Wal::new(&path);

    for id in 1..=3 {
        wal.append(&WalOp::Insert {
            table: "t".into(),
            row: row(&[("id", json!(id))]),
        })
        .unwrap();
    }
    let scan = wal.read_all().unwrap();
    let lsns: Vec<u64> = scan.records.iter().map(|(lsn, _)| *lsn).collect();
    assert_eq!(lsns, vec![1, 2, 3]);
    assert_eq!(scan.good_len as u64, std::fs::metadata(&path).unwrap().len());

    let reopened = Wal::new(&path);
    reopened
        .append(&WalOp::Insert {
            table: "t".into(),
            row: row(&[("id", json!(4))]),
        })
        .unwrap();
    assert_eq!(reopened.read_all().unwrap().records.last().unwrap().0, 4);
}

/// 一组操作写完带提交标记，标记的 LSN 也在记录里
#[test]
fn append_group_writes_a_commit_marker() {
    let dir = tempfile::tempdir().unwrap();
    let wal = Wal::new(dir.path().join("wal.log"));

    let group = [
        WalOp::InsertBatch {
            table: "t".into(),
            rows: vec![row(&[("id", json!(2))])],
        },
        WalOp::DeleteKeys {
            table: "t".into(),
            keys: vec![1],
        },
    ];
    let commit_lsn = wal.append_group(&group).unwrap();

    let scan = wal.read_all().unwrap();
    assert_eq!(scan.records.len(), 3);
    assert_eq!(scan.records.last().unwrap().1, WalOp::Commit);
    assert_eq!(scan.records.last().unwrap().0, commit_lsn);
    assert_eq!(wal.read().unwrap().unwrap(), group[0]);
    assert_eq!(
        committed_ops(&scan.records),
        vec![(1, group[0].clone()), (2, group[1].clone())]
    );
}

/// 没有提交标记的记录组算未提交，旧格式的单条仍算已提交
#[test]
fn records_without_a_commit_marker_stay_uncommitted() {
    let dir = tempfile::tempdir().unwrap();
    let wal = Wal::new(dir.path().join("wal.log"));

    for id in 1..=2 {
        wal.append(&WalOp::Insert {
            table: "t".into(),
            row: row(&[("id", json!(id))]),
        })
        .unwrap();
    }
    assert!(committed_ops(&wal.read_all().unwrap().records).is_empty());

    wal.clear().unwrap();
    wal.append(&WalOp::Insert {
        table: "t".into(),
        row: row(&[("id", json!(9))]),
    })
    .unwrap();
    assert_eq!(committed_ops(&wal.read_all().unwrap().records).len(), 1);
}

/// 撕成两半的尾巴能读出完好长度并截掉
#[test]
fn trailing_torn_frame_can_be_truncated() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wal.log");
    let wal = Wal::new(&path);

    for id in 1..=2 {
        wal.append(&WalOp::Insert {
            table: "t".into(),
            row: row(&[("id", json!(id))]),
        })
        .unwrap();
    }
    let full = std::fs::read(&path).unwrap();
    std::fs::write(&path, &full[..full.len() - 3]).unwrap();

    let scan = wal.read_all().unwrap();
    assert_eq!(scan.records.len(), 1);
    assert!(scan.good_len < full.len() - 3);

    wal.truncate_to(scan.good_len).unwrap();
    assert_eq!(std::fs::metadata(&path).unwrap().len(), scan.good_len as u64);
    assert_eq!(wal.read_all().unwrap().records.len(), 1);
}

/// 完整损坏帧返回错误而非截断成功
#[test]
fn complete_corrupt_frame_is_rejected() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wal.log");
    std::fs::write(&path, [1, 0, 0, 0, 255]).unwrap();
    assert!(Wal::new(&path).read_all().is_err());
    assert_eq!(std::fs::read(path).unwrap(), [1, 0, 0, 0, 255]);
}

/// 检查点损坏与替换失败不丢旧值
#[test]
fn checkpoint_corruption_and_write_failure_are_explicit() {
    let dir = tempfile::tempdir().unwrap();
    let wal = Wal::new(dir.path().join("wal.log"));
    for bytes in [vec![], vec![1; 7], vec![1; 9]] {
        std::fs::write(dir.path().join("wal.checkpoint"), bytes).unwrap();
        assert!(wal.read_checkpoint().is_err());
    }
    wal.write_checkpoint(7).unwrap();
    std::fs::create_dir(dir.path().join("wal.checkpoint.pending")).unwrap();
    assert!(wal.write_checkpoint(9).is_err());
    assert_eq!(wal.read_checkpoint().unwrap(), Some(7));
}

/// 检查点写读一致
#[test]
fn checkpoint_round_trips() {
    let dir = tempfile::tempdir().unwrap();
    let wal = Wal::new(dir.path().join("wal.log"));

    assert_eq!(wal.read_checkpoint().unwrap(), None);
    wal.write_checkpoint(7).unwrap();
    assert_eq!(wal.read_checkpoint().unwrap(), Some(7));
}

/// 按 LSN 截断只丢边界以内的帧
#[test]
fn truncate_before_keeps_newer_records() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wal.log");
    let wal = Wal::new(&path);

    let second_op = WalOp::Insert {
        table: "t".into(),
        row: row(&[("id", json!(2))]),
    };
    let first = wal
        .append_group(&[WalOp::Insert {
            table: "t".into(),
            row: row(&[("id", json!(1))]),
        }])
        .unwrap();
    let second = wal.append_group(std::slice::from_ref(&second_op)).unwrap();
    assert!(second > first);

    // 提交号是第一组的最后一条；边界以内的操作与提交标记都该丢掉
    wal.truncate_before(first).unwrap();
    let kept = wal.read_all().unwrap();
    assert!(kept.records.iter().all(|(lsn, _)| *lsn > first));
    let ops: Vec<&WalOp> = kept
        .records
        .iter()
        .map(|(_, op)| op)
        .filter(|op| **op != WalOp::Commit)
        .collect();
    assert_eq!(ops, vec![&second_op], "只剩第二组的操作");

    // 边界划到最后一条之后，日志就空了
    wal.truncate_before(second).unwrap();
    assert_eq!(std::fs::metadata(&path).unwrap().len(), 0);
    assert!(wal.read_all().unwrap().records.is_empty());
}

/// 日志被截断后 LSN 也不回头
#[test]
fn lsn_continues_from_the_checkpoint() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("wal.log");
    let op = WalOp::Insert {
        table: "t".into(),
        row: row(&[("id", json!(1))]),
    };

    let first = {
        let wal = Wal::new(&path);
        let lsn = wal.append_group(std::slice::from_ref(&op)).unwrap();
        wal.write_checkpoint(lsn).unwrap();
        wal.truncate_before(lsn).unwrap();
        lsn
    };

    // 重新打开：号从检查点续
    let wal = Wal::new(&path);
    let next = wal.append_group(std::slice::from_ref(&op)).unwrap();
    assert!(next > first, "重启后 LSN 要接着检查点走: {next} vs {first}");
}

/// 过滤只留下检查点之后的记录
#[test]
fn records_after_keeps_only_the_tail() {
    let records = vec![
        (1u64, WalOp::Insert { table: "t".into(), row: row(&[("id", json!(1))]) }),
        (2, WalOp::Commit),
        (3, WalOp::Insert { table: "t".into(), row: row(&[("id", json!(2))]) }),
        (4, WalOp::Commit),
    ];
    let kept = records_after(&records, 2);
    let lsn: Vec<u64> = kept.iter().map(|(lsn, _)| *lsn).collect();
    assert_eq!(lsn, vec![3, 4]);
    assert_eq!(records_after(&records, 4).len(), 0);
    assert_eq!(records_after(&records, 0).len(), 4);
}

