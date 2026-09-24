use chusql_storage::btree::DiskBTree;
use chusql_storage::config::{DEFAULT_BTREE_ORDER, DEFAULT_PAGE_SIZE};

// B+ 树测试：空树查询、插入后查找、重开文件数据仍在、覆盖同一个键、升序遍历、
// 倒序插入 500 个键、页大小 / order 可以配置、配置和文件头对不上时报错。

/// 空树的 get 返回 None，遍历返回空。
#[test]
fn empty_tree_get() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
    )
    .unwrap();
    assert_eq!(t.get(1).unwrap(), None);
    assert_eq!(t.iter_all().unwrap(), vec![]);
}

/// 插 100 个键，每个都能按 key 查回来。
#[test]
fn insert_then_get() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
    )
    .unwrap();
    for i in 0..100 {
        t.insert(i, i as u64 * 10).unwrap();
    }
    for i in 0..100 {
        assert_eq!(t.get(i).unwrap(), Some(i as u64 * 10));
    }
}

/// 关掉再打开同一个索引文件，数据还在。
#[test]
fn reopen_persists() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.idx");
    {
        let mut t = DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER).unwrap();
        for i in 0..50 {
            t.insert(i, i as u64).unwrap();
        }
    }
    let mut t = DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER).unwrap();
    for i in 0..50 {
        assert_eq!(t.get(i).unwrap(), Some(i as u64));
    }
}

/// 同一个键插两次，后写的值生效。
#[test]
fn update_existing() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
    )
    .unwrap();
    t.insert(5, 100).unwrap();
    t.insert(5, 200).unwrap();
    assert_eq!(t.get(5).unwrap(), Some(200));
}

/// 倒序插入后遍历，结果仍按 key 升序。
#[test]
fn iter_all_sorted() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
    )
    .unwrap();
    for i in (0..50).rev() {
        t.insert(i, i as u64).unwrap();
    }
    let all = t.iter_all().unwrap();
    assert_eq!(all.len(), 50);
    for (i, (k, _)) in all.iter().enumerate() {
        assert_eq!(*k, i as i64);
    }
}

/// 倒序插入 500 个键（必然多次分裂），每个键都能查到。
#[test]
fn reverse_order_insert() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
    )
    .unwrap();
    for i in (0..500).rev() {
        t.insert(i, i as u64).unwrap();
    }
    for i in 0..500 {
        assert_eq!(t.get(i).unwrap(), Some(i as u64));
    }
}

/// 页大小和 order 都可以配置：512 字节的页 + 8 个孩子照样能插能查。
#[test]
fn custom_page_size_and_order() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(dir.path().join("t.idx"), 512, 8).unwrap();
    for i in 0..200 {
        t.insert(i, i as u64 + 1).unwrap();
    }
    for i in 0..200 {
        assert_eq!(t.get(i).unwrap(), Some(i as u64 + 1));
    }
}

/// 文件头里记着页大小和 order：换配置再打开要报错，而不是读出乱七八糟的东西。
#[test]
fn config_mismatch_errors() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.idx");
    {
        let mut t = DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER).unwrap();
        t.insert(1, 1).unwrap();
    }
    assert!(DiskBTree::open(&path, DEFAULT_PAGE_SIZE, 8).is_err());
    assert!(DiskBTree::open(&path, 8192, DEFAULT_BTREE_ORDER).is_err());
    assert!(DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER).is_ok());
}

/// 不是本项目的索引文件（比如随便一个空文件）要报错。
#[test]
fn not_an_index_file_errors() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("junk.idx");
    std::fs::write(&path, vec![7u8; DEFAULT_PAGE_SIZE]).unwrap();
    assert!(DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER).is_err());
}

/// order 放不进一页时直接拒绝。
#[test]
fn order_too_large_for_page_errors() {
    let dir = tempfile::tempdir().unwrap();
    assert!(DiskBTree::open(dir.path().join("t.idx"), 512, 300).is_err());
}
