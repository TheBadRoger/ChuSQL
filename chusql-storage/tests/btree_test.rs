use chusql_storage::btree::DiskBTree;
use chusql_storage::config::{DEFAULT_BTREE_ORDER, DEFAULT_PAGE_SIZE};
use chusql_storage::page::DEFAULT_POOL_SIZE;

// B+ 树测试：插查、重开、分裂、页大小与 order 校验。

/// 空树查不到，遍历为空
#[test]
fn empty_tree_get() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();
    assert_eq!(t.get(1).unwrap(), None);
    assert_eq!(t.iter_all().unwrap(), vec![]);
}

/// 插 100 个键都能查回
#[test]
fn insert_then_get() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();
    for i in 0..100 {
        t.insert(i, i as u64 * 10).unwrap();
    }
    for i in 0..100 {
        assert_eq!(t.get(i).unwrap(), Some(i as u64 * 10));
    }
}

/// 重开文件数据还在
#[test]
fn reopen_persists() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.idx");
    {
        let mut t = DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER, DEFAULT_POOL_SIZE).unwrap();
        for i in 0..50 {
            t.insert(i, i as u64).unwrap();
        }
    }
    let mut t = DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER, DEFAULT_POOL_SIZE).unwrap();
    for i in 0..50 {
        assert_eq!(t.get(i).unwrap(), Some(i as u64));
    }
}

/// 同一个键后写的值生效
#[test]
fn update_existing() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();
    t.insert(5, 100).unwrap();
    t.insert(5, 200).unwrap();
    assert_eq!(t.get(5).unwrap(), Some(200));
}

/// 倒序插入后遍历仍升序
#[test]
fn iter_all_sorted() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
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

/// 倒序插 500 个键都能查到
#[test]
fn reverse_order_insert() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();
    for i in (0..500).rev() {
        t.insert(i, i as u64).unwrap();
    }
    for i in 0..500 {
        assert_eq!(t.get(i).unwrap(), Some(i as u64));
    }
}

/// 512 字节页 + 8 个孩子可用
#[test]
fn custom_page_size_and_order() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(dir.path().join("t.idx"), 512, 8, DEFAULT_POOL_SIZE).unwrap();
    for i in 0..200 {
        t.insert(i, i as u64 + 1).unwrap();
    }
    for i in 0..200 {
        assert_eq!(t.get(i).unwrap(), Some(i as u64 + 1));
    }
}

/// 页大小或 order 不符要报错
#[test]
fn config_mismatch_errors() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.idx");
    {
        let mut t = DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER, DEFAULT_POOL_SIZE).unwrap();
        t.insert(1, 1).unwrap();
    }
    assert!(DiskBTree::open(&path, DEFAULT_PAGE_SIZE, 8, DEFAULT_POOL_SIZE).is_err());
    assert!(DiskBTree::open(&path, 8192, DEFAULT_BTREE_ORDER, DEFAULT_POOL_SIZE).is_err());
    assert!(DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER, DEFAULT_POOL_SIZE).is_ok());
}

/// 不是索引文件要报错
#[test]
fn not_an_index_file_errors() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("junk.idx");
    std::fs::write(&path, vec![7u8; DEFAULT_PAGE_SIZE]).unwrap();
    assert!(DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER, DEFAULT_POOL_SIZE).is_err());
}

/// order 放不进一页要报错
#[test]
fn order_too_large_for_page_errors() {
    let dir = tempfile::tempdir().unwrap();
    assert!(DiskBTree::open(dir.path().join("t.idx"), 512, 300, DEFAULT_POOL_SIZE).is_err());
}

/// 删掉键之后查不到，别人不受影响
#[test]
fn delete_removes_only_that_key() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();
    for i in 0..200 {
        t.insert(i, i as u64 + 1).unwrap();
    }

    assert!(t.delete(50).unwrap(), "删一个存在的键应该返回 true");
    assert_eq!(t.get(50).unwrap(), None);
    assert_eq!(t.get(49).unwrap(), Some(50));
    assert_eq!(t.get(51).unwrap(), Some(52));

    // 再删一次：它已经不在了
    assert!(!t.delete(50).unwrap());

    // 遍历里也不能再有它
    let all = t.iter_all().unwrap();
    assert_eq!(all.len(), 199);
    assert!(all.iter().all(|(k, _)| *k != 50));
}

/// 删过之后树还能继续插（惰性删除会留下稀疏节点，但不能影响正确性）
#[test]
fn insert_after_delete_still_works() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = DiskBTree::open(
        dir.path().join("t.idx"),
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
        DEFAULT_POOL_SIZE,
    )
    .unwrap();
    for i in 0..300 {
        t.insert(i, i as u64).unwrap();
    }
    // 把偶数键全删掉
    for i in (0..300).step_by(2) {
        assert!(t.delete(i).unwrap());
    }
    // 再插一批新的
    for i in 1000..1100 {
        t.insert(i, i as u64).unwrap();
    }
    for i in 0..300 {
        let want = if i % 2 == 0 { None } else { Some(i as u64) };
        assert_eq!(t.get(i).unwrap(), want, "key {}", i);
    }
    for i in 1000..1100 {
        assert_eq!(t.get(i).unwrap(), Some(i as u64));
    }
}
