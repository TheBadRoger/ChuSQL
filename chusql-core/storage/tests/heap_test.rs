use chusql_core_storage::config::DEFAULT_PAGE_SIZE;
use chusql_core_storage::heap::HeapTable;
use chusql_core_storage::page::DEFAULT_POOL_SIZE;
use chusql_core_storage::protocol::Row;
use serde_json::json;

// 堆表测试：插查、跨页、大行、重开、索引重建。

/// 用键值对构造一行
fn row(pairs: &[(&str, serde_json::Value)]) -> Row {
    let mut m = Row::new();
    for (k, v) in pairs {
        m.insert((*k).to_string(), v.clone());
    }
    m
}

/// 索引读失败时删除报错且保留数据
#[test]
fn delete_propagates_index_read_errors() {
    let dir = tempfile::tempdir().unwrap();
    let data = dir.path().join("t.db");
    let index = dir.path().join("t.idx");
    let mut t = HeapTable::open_indexed(&data, &index, DEFAULT_PAGE_SIZE, 4, 1).unwrap();
    let original = row(&[("id", json!(1))]);
    t.insert_row(&original).unwrap();
    t.flush().unwrap();
    let file = std::fs::OpenOptions::new().write(true).open(&index).unwrap();
    file.set_len(DEFAULT_PAGE_SIZE as u64).unwrap();
    let err = t.delete_by_keys(&[1]).unwrap_err();
    assert_eq!(err.kind(), std::io::ErrorKind::UnexpectedEof);
    assert_eq!(t.scan().unwrap(), vec![original]);
}

/// 跨页投影与分片保持行序及隐藏列
#[test]
fn projected_shards_preserve_order_and_hidden_columns() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = HeapTable::open(dir.path().join("t.db"), 256, 1).unwrap();
    for i in 0..40 {
        t.insert_row(&row(&[("id", json!(i)), ("name", json!("alice")), ("secret", json!(i))])).unwrap();
    }
    t.set_hidden_columns(["secret".to_string()].into_iter().collect());
    let columns = vec!["id".to_string(), "secret".to_string()];
    let expected: Vec<Row> = (0..40).map(|i| row(&[("id", json!(i))])).collect();
    assert_eq!(t.scan_columns(Some(&columns)).unwrap(), expected);
    let pages = t.num_pages().unwrap();
    assert!(pages > 1);
    let mut shards = t.scan_columns_pages(Some(&columns), 0, pages / 2).unwrap();
    shards.extend(t.scan_columns_pages(Some(&columns), pages / 2, pages + 1).unwrap());
    assert_eq!(shards, expected);
    assert_eq!(t.read_at(0, 0).unwrap().unwrap(), row(&[("id", json!(0)), ("name", json!("alice"))]));
}

/// 扫描与点读均返回行解码错误
#[test]
fn malformed_rows_return_decode_errors() {
    use chusql_core_storage::heap::HeapPage;
    use chusql_core_storage::page::{Page, PageFile};
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");
    let mut file = PageFile::open(&path, DEFAULT_PAGE_SIZE).unwrap();
    let mut page = Page::new(0, DEFAULT_PAGE_SIZE);
    HeapPage::insert_tuple(&mut page, b"{\"id\":1} trailing").unwrap();
    file.write_page(&page).unwrap();
    file.flush().unwrap();
    drop(file);
    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE, 1).unwrap();
    assert!(t.scan().is_err());
    assert!(t.scan_columns(Some(&[])).is_err());
    assert!(t.read_at(0, 0).is_err());
}

/// 索引文件被清空（强杀或截断）后打开，会按堆数据补齐
#[test]
fn empty_index_file_is_rebuilt_from_rows() {
    let dir = tempfile::tempdir().unwrap();
    let data = dir.path().join("t.db");
    let index = dir.path().join("t.idx");
    {
        let mut t = HeapTable::open_indexed(&data, &index, DEFAULT_PAGE_SIZE, 4, DEFAULT_POOL_SIZE).unwrap();
        for i in 1..=5 {
            t.insert_row(&row(&[("id", json!(i))])).unwrap();
        }
        t.flush().unwrap();
    }
    std::fs::write(&index, b"").unwrap();
    let mut t = HeapTable::open_indexed(&data, &index, DEFAULT_PAGE_SIZE, 4, DEFAULT_POOL_SIZE).unwrap();
    assert!(t.get_by_column_key("id", 3).unwrap().is_some(), "空索引文件要从堆数据重建");
    assert!(t.get_by_column_key("id", 9).unwrap().is_none());
}

/// 字符串列能真正建索引：按值点查，NULL 不入索引
#[test]
fn string_index_looks_up_by_value() {
    let dir = tempfile::tempdir().unwrap();
    let data = dir.path().join("s.db");
    let index = dir.path().join("s.idx");
    let mut t = HeapTable::open(&data, DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();
    t.insert_row(&row(&[("id", json!(1)), ("name", json!("alice"))])).unwrap();
    t.insert_row(&row(&[("id", json!(2)), ("name", json!("bob"))])).unwrap();
    t.insert_row(&row(&[("id", json!(3)), ("name", json!(null))])).unwrap();
    t.build_index("name", &index, DEFAULT_PAGE_SIZE, 4, DEFAULT_POOL_SIZE).unwrap();

    assert_eq!(t.get_by_string_key("name", "bob").unwrap().unwrap()["id"], json!(2));
    assert!(t.get_by_string_key("name", "dave").unwrap().is_none());
    assert!(t.get_by_string_key("name", "").unwrap().is_none(), "NULL 不入索引");
}

/// 允许重复值建索引：点查把重复的行都取回来
#[test]
fn duplicate_values_are_indexed_and_all_returned() {
    let dir = tempfile::tempdir().unwrap();
    let data = dir.path().join("d.db");
    let index = dir.path().join("d.idx");
    let mut t = HeapTable::open(&data, DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();
    t.insert_row(&row(&[("id", json!(1)), ("name", json!("alice"))])).unwrap();
    t.insert_row(&row(&[("id", json!(2)), ("name", json!("alice"))])).unwrap();
    t.build_index("name", &index, DEFAULT_PAGE_SIZE, 4, DEFAULT_POOL_SIZE).unwrap();

    let mut ids: Vec<i64> = t
        .get_all_by_string_key("name", "alice")
        .unwrap()
        .iter()
        .filter_map(|r| r["id"].as_i64())
        .collect();
    ids.sort_unstable();
    assert_eq!(ids, vec![1, 2]);
    assert!(t.get_all_by_string_key("name", "bob").unwrap().is_empty());
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

/// 整表改写后索引重建
#[test]
fn replace_all_rebuilds_index() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = HeapTable::open_indexed(
        dir.path().join("t.db"),
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        chusql_core_storage::config::DEFAULT_BTREE_ORDER,
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

    assert!(t.get_by_key(1).unwrap().is_none(), "old key must be invalidated");
    assert!(t.get_by_key(2).unwrap().is_none());
    let got = t.get_by_key(7).unwrap().expect("new row should be indexed by id");
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
        chusql_core_storage::config::DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();

    for i in 1..=5 {
        t.insert_row(&row(&[("id", json!(i)), ("name", json!(format!("u{}", i)))]))
            .unwrap();
    }

    let deleted = t.delete_by_keys(&[2, 4, 99]).unwrap();
    assert_eq!(deleted.len(), 2, "only 2 and 4 were actually deleted");

    let ids: Vec<i64> = t
        .scan()
        .unwrap()
        .iter()
        .map(|r| r["id"].as_i64().unwrap())
        .collect();
    assert_eq!(ids, vec![1, 3, 5], "remaining rows must keep their order");

    assert!(t.get_by_key(2).unwrap().is_none(), "index entry should be deleted too");
    assert!(t.get_by_key(4).unwrap().is_none());
    assert!(t.get_by_key(3).unwrap().is_some());

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
        chusql_core_storage::config::DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();

    let rows: Vec<Row> = (1..=20)
        .map(|i| row(&[("id", json!(i)), ("name", json!(format!("u{}", i)))]))
        .collect();
    t.insert_rows(&rows).unwrap();

    assert_eq!(t.scan().unwrap().len(), 20);
    assert_eq!(t.get_by_key(20).unwrap().unwrap()["name"], json!("u20"));

    let dup = vec![
        row(&[("id", json!(100))]),
        row(&[("id", json!(100))]),
    ];
    assert!(t.insert_rows(&dup).is_err());
}

/// 第二列索引：建完插入/删除都跟着维护，值可重复
#[test]
fn secondary_index_is_maintained() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = HeapTable::open_indexed(
        dir.path().join("t.db"),
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        chusql_core_storage::config::DEFAULT_BTREE_ORDER,
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
        chusql_core_storage::config::DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();
    assert!(t.has_index("code"));
    assert_eq!(t.get_by_column_key("code", 7002).unwrap().unwrap()["id"], json!(2));

    t.insert_row(&row(&[("id", json!(3)), ("code", json!(7003))]))
        .unwrap();
    assert_eq!(t.get_by_column_key("code", 7003).unwrap().unwrap()["id"], json!(3));

    t.delete_by_keys(&[2]).unwrap();
    assert!(t.get_by_column_key("code", 7002).unwrap().is_none());

    t.insert_row(&row(&[("id", json!(4)), ("code", json!(7001))])).unwrap();
    let mut dup_ids: Vec<i64> = t
        .get_all_by_column_key("code", 7001)
        .unwrap()
        .iter()
        .filter_map(|r| r["id"].as_i64())
        .collect();
    dup_ids.sort_unstable();
    assert_eq!(dup_ids, vec![1, 4], "duplicate index values keep both rows");

    let mut t2 = HeapTable::open(dir.path().join("d.db"), DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();
    t2.insert_rows(&[
        row(&[("id", json!(1)), ("age", json!(30))]),
        row(&[("id", json!(2)), ("age", json!(30))]),
    ])
    .unwrap();
    t2.build_index(
        "age",
        &dir.path().join("d.age.idx"),
        DEFAULT_PAGE_SIZE,
        chusql_core_storage::config::DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();
    assert_eq!(
        t2.get_all_by_column_key("age", 30).unwrap().len(),
        2,
        "duplicate values are all indexed"
    );
}


/// 整表整理要收回删行留下的页内空洞
#[test]
fn compact_pages_reclaims_dead_space() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");
    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();

    let pad = "x".repeat(200);
    for i in 0..10 {
        t.insert_row(&row(&[("id", json!(i)), ("pad", json!(pad))])).unwrap();
    }
    let pages = t.num_pages().unwrap();

    t.delete_by_keys(&[3]).unwrap();
    assert_eq!(t.num_pages().unwrap(), pages, "删一行不该动页数");
    let reclaimed = t.compact_pages().unwrap();
    assert!(reclaimed > 0, "整理要回收删掉的 {} 字节", 200);
    assert_eq!(t.compact_pages().unwrap(), 0, "整理过就没有空洞了");

    let rows = t.scan().unwrap();
    assert_eq!(rows.len(), 9);
    assert!(!rows.iter().any(|r| r["id"] == json!(3)));
    assert_eq!(rows[0]["pad"], json!(pad));
}

/// 删行的页内空间要能被后续插入复用
#[test]
fn deletes_compact_pages_so_space_is_reused() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");
    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_POOL_SIZE).unwrap();

    let pad = "y".repeat(80);
    for i in 0..30 {
        t.insert_row(&row(&[("id", json!(i)), ("pad", json!(pad))])).unwrap();
    }
    assert_eq!(t.num_pages().unwrap(), 1, "30 行要先塞进一页");

    // 删掉中间的行，只留最靠页尾的那一行
    let keys: Vec<i64> = (10..29).collect();
    t.delete_by_keys(&keys).unwrap();

    for i in 100..119 {
        t.insert_row(&row(&[("id", json!(i)), ("pad", json!(pad))])).unwrap();
    }
    assert_eq!(t.num_pages().unwrap(), 1, "删掉的空间要能被复用");
    assert_eq!(t.scan().unwrap().len(), 30);
}

/// 页内整理之后索引里的位置仍然指向原来的行
#[test]
fn compact_pages_keeps_index_positions_valid() {
    let dir = tempfile::tempdir().unwrap();
    let data = dir.path().join("t.db");
    let index = dir.path().join("t.idx");
    let mut t = HeapTable::open_indexed(&data, &index, DEFAULT_PAGE_SIZE, 4, DEFAULT_POOL_SIZE).unwrap();

    let pad = "z".repeat(80);
    for i in 0..30 {
        t.insert_row(&row(&[("id", json!(i)), ("pad", json!(pad))])).unwrap();
    }
    let keys: Vec<i64> = (5..25).collect();
    t.delete_by_keys(&keys).unwrap();
    t.compact_pages().unwrap();

    for i in [0, 1, 2, 3, 4, 25, 26, 27, 28, 29] {
        let found = t.get_by_column_key("id", i).unwrap();
        assert_eq!(found.as_ref().map(|r| r["pad"].clone()), Some(json!(pad)), "整理后 id={} 仍要能点查", i);
    }
    for i in 5..25 {
        assert!(t.get_by_column_key("id", i).unwrap().is_none(), "删掉的行不该再查出来");
    }
}
