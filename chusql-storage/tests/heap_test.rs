use chusql_storage::config::DEFAULT_PAGE_SIZE;
use chusql_storage::heap::HeapTable;
use chusql_storage::page::DEFAULT_POOL_SIZE;
use chusql_storage::protocol::Row;
use serde_json::json;

// 堆表测试：插查、跨页、大行、重开、索引重建。

fn row(pairs: &[(&str, serde_json::Value)]) -> Row {
    let mut m = Row::new();
    for (k, v) in pairs {
        m.insert((*k).to_string(), v.clone());
    }
    m
}

/// 插一行再扫描读回
#[test]
fn insert_and_scan_one_row() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");
    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();

    t.insert_row(&row(&[("id", json!(1)), ("name", json!("Alice"))]))
        .unwrap();

    let rows = t.scan().unwrap();
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0]["id"], json!(1));
    assert_eq!(rows[0]["name"], json!("Alice"));
}

/// 500 行跨页顺序不变
#[test]
fn insert_many_rows_cross_pages() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");
    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();

    for i in 0..500 {
        t.insert_row(&row(&[("id", json!(i))])).unwrap();
    }

    let rows = t.scan().unwrap();
    assert_eq!(rows.len(), 500);
    for (i, r) in rows.iter().enumerate() {
        assert_eq!(r["id"], json!(i as i64));
    }
}

/// 2KB 大行放得下
#[test]
fn large_row_still_fits() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");
    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();

    let big = "x".repeat(2000);
    t.insert_row(&row(&[("data", json!(big.clone()))])).unwrap();

    let rows = t.scan().unwrap();
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0]["data"], json!(big));
}

/// 超页的行要报错
#[test]
fn row_too_large_errors() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");
    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();

    let huge = "x".repeat(10_000);
    let err = t.insert_row(&row(&[("data", json!(huge))]));
    assert!(err.is_err());
}

/// 重开文件数据还在
#[test]
fn reopen_and_scan() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");

    {
        let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();
        t.insert_row(&row(&[("id", json!(1))])).unwrap();
        t.insert_row(&row(&[("id", json!(2))])).unwrap();
    }

    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();
    let rows = t.scan().unwrap();
    assert_eq!(rows.len(), 2);
    assert_eq!(rows[0]["id"], json!(1));
    assert_eq!(rows[1]["id"], json!(2));
}

/// 512 字节页可用
#[test]
fn custom_page_size() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("small.db");
    let mut t = HeapTable::open(&path, 512, DEFAULT_POOL_SIZE).unwrap();

    for i in 0..50 {
        t.insert_row(&row(&[("id", json!(i))])).unwrap();
    }
    assert_eq!(t.scan().unwrap().len(), 50);

    let big = "x".repeat(600);
    assert!(t.insert_row(&row(&[("data", json!(big))])).is_err());
}

/// 整表改写后索引重建
#[test]
fn replace_all_rebuilds_index() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = HeapTable::open_indexed(
        dir.path().join("t.db"),
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        chusql_storage::config::DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();

    t.insert_row(&row(&[("id", json!(1)), ("name", json!("Alice"))]))
        .unwrap();
    t.insert_row(&row(&[("id", json!(2)), ("name", json!("Bob"))]))
        .unwrap();
    assert!(t.get_by_key(1).unwrap().is_some());

    t.replace_all(&[
        row(&[("id", json!(7)), ("name", json!("Zoe"))]),
        row(&[("name", json!("NoId"))]),
    ])
    .unwrap();

    assert!(t.get_by_key(1).unwrap().is_none(), "旧键必须作废");
    assert!(t.get_by_key(2).unwrap().is_none());
    let got = t.get_by_key(7).unwrap().expect("新行应该按 id 建好索引");
    assert_eq!(got["name"], json!("Zoe"));
    assert_eq!(t.scan().unwrap().len(), 2);
}

/// 行级删除：行没了、索引条目没了、顺序不乱
#[test]
fn delete_by_keys_removes_rows() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = HeapTable::open_indexed(
        dir.path().join("t.db"),
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        chusql_storage::config::DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();

    for i in 1..=5 {
        t.insert_row(&row(&[("id", json!(i)), ("name", json!(format!("u{}", i)))]))
            .unwrap();
    }

    let deleted = t.delete_by_keys(&[2, 4, 99]).unwrap();
    assert_eq!(deleted.len(), 2, "只有 2、4 真的删掉了");

    let ids: Vec<i64> = t
        .scan()
        .unwrap()
        .iter()
        .map(|r| r["id"].as_i64().unwrap())
        .collect();
    assert_eq!(ids, vec![1, 3, 5], "剩下的行要保持原顺序");

    assert!(t.get_by_key(2).unwrap().is_none(), "索引条目要跟着删");
    assert!(t.get_by_key(4).unwrap().is_none());
    assert!(t.get_by_key(3).unwrap().is_some());

    // 删掉的位置可以再插回来
    t.insert_row(&row(&[("id", json!(2)), ("name", json!("u2b"))]))
        .unwrap();
    assert_eq!(t.get_by_key(2).unwrap().unwrap()["name"], json!("u2b"));
}

/// 批量插入
#[test]
fn insert_rows_batch() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = HeapTable::open_indexed(
        dir.path().join("t.db"),
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        chusql_storage::config::DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();

    let rows: Vec<Row> = (1..=20)
        .map(|i| row(&[("id", json!(i)), ("name", json!(format!("u{}", i)))]))
        .collect();
    t.insert_rows(&rows).unwrap();

    assert_eq!(t.scan().unwrap().len(), 20);
    assert_eq!(t.get_by_key(20).unwrap().unwrap()["name"], json!("u20"));

    // 批里出现重复键要报错（整批之前就查出来，不会插一半）
    let dup = vec![
        row(&[("id", json!(100))]),
        row(&[("id", json!(100))]),
    ];
    assert!(t.insert_rows(&dup).is_err());
}

/// 第二列索引：建索引时要求唯一，建完之后插入/删除都跟着维护
#[test]
fn secondary_index_is_maintained() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = HeapTable::open_indexed(
        dir.path().join("t.db"),
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        chusql_storage::config::DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();

    t.insert_rows(&[
        row(&[("id", json!(1)), ("code", json!(7001))]),
        row(&[("id", json!(2)), ("code", json!(7002))]),
    ])
    .unwrap();

    assert!(!t.has_index("code"));
    t.build_index(
        "code",
        &dir.path().join("t.code.idx"),
        DEFAULT_PAGE_SIZE,
        chusql_storage::config::DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();
    assert!(t.has_index("code"));
    assert_eq!(t.get_by_column_key("code", 7002).unwrap().unwrap()["id"], json!(2));

    // 建索引之后新插入的行也进索引
    t.insert_row(&row(&[("id", json!(3)), ("code", json!(7003))]))
        .unwrap();
    assert_eq!(t.get_by_column_key("code", 7003).unwrap().unwrap()["id"], json!(3));

    // 删掉一行，索引条目跟着走
    t.delete_by_keys(&[2]).unwrap();
    assert!(t.get_by_column_key("code", 7002).unwrap().is_none());

    // code 已经有索引，重复的 code 插不进去（索引列是唯一的）
    assert!(t.insert_row(&row(&[("id", json!(4)), ("code", json!(7001))])).is_err());
    let mut t2 = HeapTable::open(&dir.path().join("d.db"), DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();
    t2.insert_rows(&[
        row(&[("id", json!(1)), ("age", json!(30))]),
        row(&[("id", json!(2)), ("age", json!(30))]),
    ])
    .unwrap();
    let err = t2.build_index(
        "age",
        &dir.path().join("d.age.idx"),
        DEFAULT_PAGE_SIZE,
        chusql_storage::config::DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    );
    assert!(err.is_err(), "重复值不给建索引");
    assert!(!t2.has_index("age"), "失败之后不留半成品");
}

/// 同一个 id 插两次要报错
#[test]
fn duplicate_key_is_rejected() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = HeapTable::open_indexed(
        dir.path().join("t.db"),
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        chusql_storage::config::DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();

    t.insert_row(&row(&[("id", json!(1))])).unwrap();
    let r = t.insert_row(&row(&[("id", json!(1))]));
    assert!(r.is_err());
    assert_eq!(t.scan().unwrap().len(), 1, "失败的那一行不应该进堆表");
}

