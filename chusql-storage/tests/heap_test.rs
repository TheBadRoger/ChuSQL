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

    t.insert(&row(&[("id", json!(1)), ("name", json!("Alice"))]))
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
        t.insert(&row(&[("id", json!(i))])).unwrap();
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
    t.insert(&row(&[("data", json!(big.clone()))])).unwrap();

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
    let err = t.insert(&row(&[("data", json!(huge))]));
    assert!(err.is_err());
}

/// 重开文件数据还在
#[test]
fn reopen_and_scan() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");

    {
        let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();
        t.insert(&row(&[("id", json!(1))])).unwrap();
        t.insert(&row(&[("id", json!(2))])).unwrap();
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
        t.insert(&row(&[("id", json!(i))])).unwrap();
    }
    assert_eq!(t.scan().unwrap().len(), 50);

    let big = "x".repeat(600);
    assert!(t.insert(&row(&[("data", json!(big))])).is_err());
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

    t.insert_keyed(&row(&[("id", json!(1)), ("name", json!("Alice"))]), 1)
        .unwrap();
    t.insert_keyed(&row(&[("id", json!(2)), ("name", json!("Bob"))]), 2)
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
