use chusql_storage::config::DEFAULT_PAGE_SIZE;
use chusql_storage::heap::HeapTable;
use chusql_storage::protocol::Row;
use serde_json::json;

// 堆表测试：插入 + 全表扫描、跨页多行、大行、超大行报错、重开文件后数据还在、自定义页大小。

/// 造一行测试数据（列名 → JSON 值）。
fn row(pairs: &[(&str, serde_json::Value)]) -> Row {
    let mut m = Row::new();
    for (k, v) in pairs {
        m.insert((*k).to_string(), v.clone());
    }
    m
}

/// 插一行再扫描，能原样读回来。
#[test]
fn insert_and_scan_one_row() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");
    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE).unwrap();

    t.insert(&row(&[("id", json!(1)), ("name", json!("Alice"))]))
        .unwrap();

    let rows = t.scan().unwrap();
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0]["id"], json!(1));
    assert_eq!(rows[0]["name"], json!("Alice"));
}

/// 插 500 行（必然跨多页），扫描回来的行数和顺序都要对。
#[test]
fn insert_many_rows_cross_pages() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");
    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE).unwrap();

    for i in 0..500 {
        t.insert(&row(&[("id", json!(i))])).unwrap();
    }

    let rows = t.scan().unwrap();
    assert_eq!(rows.len(), 500);
    // 顺序保持
    for (i, r) in rows.iter().enumerate() {
        assert_eq!(r["id"], json!(i as i64));
    }
}

/// 约 2KB 的大行也要能放进一页。
#[test]
fn large_row_still_fits() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");
    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE).unwrap();

    let big = "x".repeat(2000);
    t.insert(&row(&[("data", json!(big.clone()))])).unwrap();

    let rows = t.scan().unwrap();
    assert_eq!(rows.len(), 1);
    assert_eq!(rows[0]["data"], json!(big));
}

/// 超过一页容量的行要报错，而不是悄悄写坏。
#[test]
fn row_too_large_errors() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");
    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE).unwrap();

    let huge = "x".repeat(10_000);
    let err = t.insert(&row(&[("data", json!(huge))]));
    assert!(err.is_err());
}

/// 关掉再重新打开同一个文件，之前插入的数据还在。
#[test]
fn reopen_and_scan() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");

    {
        let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE).unwrap();
        t.insert(&row(&[("id", json!(1))])).unwrap();
        t.insert(&row(&[("id", json!(2))])).unwrap();
    }

    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE).unwrap();
    let rows = t.scan().unwrap();
    assert_eq!(rows.len(), 2);
    assert_eq!(rows[0]["id"], json!(1));
    assert_eq!(rows[1]["id"], json!(2));
}

/// 页大小可以配置：512 字节的页装得下小行、放不下大行，行数不变。
#[test]
fn custom_page_size() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("small.db");
    let mut t = HeapTable::open(&path, 512).unwrap();

    for i in 0..50 {
        t.insert(&row(&[("id", json!(i))])).unwrap();
    }
    assert_eq!(t.scan().unwrap().len(), 50);

    let big = "x".repeat(600);
    assert!(t.insert(&row(&[("data", json!(big))])).is_err());
}

/// 整表改写之后索引必须跟着重建：旧键查不到，新行按 id 能查到。
#[test]
fn replace_all_rebuilds_index() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = HeapTable::open_indexed(
        dir.path().join("t.db"),
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        chusql_storage::config::DEFAULT_BTREE_ORDER,
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
