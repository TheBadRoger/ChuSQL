use chusql_storage::btree::{fits_in_page, DiskBTree, MAX_KEY_BYTES};
use chusql_storage::config::{DEFAULT_BTREE_ORDER, DEFAULT_PAGE_SIZE};
use chusql_storage::page::DEFAULT_POOL_SIZE;

// B+ 树测试：字节键、重复键、范围扫描、重开与配置校验。

fn open(dir: &std::path::Path, name: &str) -> DiskBTree {
    DiskBTree::open(dir.join(name), DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER, DEFAULT_POOL_SIZE).unwrap()
}

/// 整数键编码成保序的 8 字节（与堆表一致）
fn key(n: i64) -> [u8; 8] {
    (n as u64 ^ (1u64 << 63)).to_be_bytes()
}

/// 空树查不到，遍历为空
#[test]
fn empty_tree_get() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = open(dir.path(), "t.idx");
    assert_eq!(t.get(&key(1)).unwrap(), None);
    assert_eq!(t.iter_all().unwrap(), vec![]);
}

/// 插 100 个键都能查回
#[test]
fn insert_then_get() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = open(dir.path(), "t.idx");
    for i in 0..100 {
        t.insert(&key(i), i as u64 * 10).unwrap();
    }
    for i in 0..100 {
        assert_eq!(t.get(&key(i)).unwrap(), Some(i as u64 * 10));
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
            t.insert(&key(i), i as u64).unwrap();
        }
    }
    let mut t = DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER, DEFAULT_POOL_SIZE).unwrap();
    for i in 0..50 {
        assert_eq!(t.get(&key(i)).unwrap(), Some(i as u64));
    }
}

/// 同一个键可以挂多个位置，按值升序返回
#[test]
fn duplicate_keys_keep_every_value() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = open(dir.path(), "dup.idx");
    t.insert(b"same", 30).unwrap();
    t.insert(b"same", 10).unwrap();
    t.insert(b"same", 20).unwrap();
    assert_eq!(t.get_all(b"same").unwrap(), vec![10, 20, 30]);
    assert_eq!(t.get(b"same").unwrap(), Some(10));
    assert!(t.delete(b"same", 10).unwrap());
    assert_eq!(t.get_all(b"same").unwrap(), vec![20, 30]);
    assert!(!t.delete(b"same", 10).unwrap());
}

/// 倒序插入后遍历仍升序
#[test]
fn iter_all_sorted() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = open(dir.path(), "t.idx");
    for i in (0..50).rev() {
        t.insert(&key(i), i as u64).unwrap();
    }
    let all = t.iter_all().unwrap();
    assert_eq!(all.len(), 50);
    for (i, (k, _)) in all.iter().enumerate() {
        assert_eq!(k.as_slice(), key(i as i64).as_slice());
    }
}

/// 页大小或 order 不符要报错
#[test]
fn config_mismatch_errors() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.idx");
    {
        let mut t = DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER, DEFAULT_POOL_SIZE).unwrap();
        t.insert(&key(1), 1).unwrap();
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
    assert!(!fits_in_page(64, DEFAULT_BTREE_ORDER));
}

/// 删掉一个键值之后查不到，别人不受影响
#[test]
fn delete_removes_only_that_key() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = open(dir.path(), "t.idx");
    for i in 0..200 {
        t.insert(&key(i), i as u64 + 1).unwrap();
    }

    assert!(t.delete(&key(50), 51).unwrap(), "deleting an existing entry should return true");
    assert_eq!(t.get(&key(50)).unwrap(), None);
    assert_eq!(t.get(&key(49)).unwrap(), Some(50));
    assert_eq!(t.get(&key(51)).unwrap(), Some(52));

    assert!(!t.delete(&key(50), 51).unwrap());

    let all = t.iter_all().unwrap();
    assert_eq!(all.len(), 199);
    assert!(all.iter().all(|(k, _)| k.as_slice() != key(50).as_slice()));
}

/// 删过之后树还能继续插
#[test]
fn insert_after_delete_still_works() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = open(dir.path(), "t.idx");
    for i in 0..300 {
        t.insert(&key(i), i as u64).unwrap();
    }
    for i in (0..300).step_by(2) {
        assert!(t.delete(&key(i), i as u64).unwrap());
    }
    for i in 1000..1100 {
        t.insert(&key(i), i as u64).unwrap();
    }
    for i in 0..300 {
        let want = if i % 2 == 0 { None } else { Some(i as u64) };
        assert_eq!(t.get(&key(i)).unwrap(), want, "key {}", i);
    }
    for i in 1000..1100 {
        assert_eq!(t.get(&key(i)).unwrap(), Some(i as u64));
    }
}

/// 变长键能插能查，最长键也支持
#[test]
fn variable_length_keys_round_trip() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = open(dir.path(), "var.idx");
    t.insert(b"a", 1).unwrap();
    t.insert(b"alice", 2).unwrap();
    t.insert(&[b'z'; MAX_KEY_BYTES], 3).unwrap();
    t.flush().unwrap();

    assert_eq!(t.get(b"a").unwrap(), Some(1));
    assert_eq!(t.get(b"alice").unwrap(), Some(2));
    assert_eq!(t.get(b"bob").unwrap(), None);
    assert_eq!(t.get(&[b'z'; MAX_KEY_BYTES]).unwrap(), Some(3));
    assert_eq!(t.get(&[b'z'; MAX_KEY_BYTES - 1]).unwrap(), None);
}

/// 超出键长上限直接报错，不写坏节点
#[test]
fn oversized_key_is_rejected() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = open(dir.path(), "long.idx");
    let too_long = vec![b'k'; MAX_KEY_BYTES + 1];
    assert!(t.insert(&too_long, 1).is_err());
    assert_eq!(t.get(&too_long).unwrap(), None);
}

/// 插满一页后会分裂，顺序仍然正确
#[test]
fn splits_keep_keys_sorted() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = open(dir.path(), "split.idx");

    let mut expected: Vec<Vec<u8>> = Vec::new();
    for i in 0..600 {
        let k = format!("key-{:04}", (i * 7919) % 600).into_bytes();
        t.insert(&k, i as u64).unwrap();
        expected.push(k);
    }
    expected.sort();
    expected.dedup();

    let all: Vec<Vec<u8>> = t.iter_all().unwrap().into_iter().map(|(k, _)| k).collect();
    assert_eq!(all, expected);
    for k in &expected {
        assert!(t.get(k).unwrap().is_some(), "missing {:?}", String::from_utf8_lossy(k));
    }
}

/// 范围扫描走叶子链，两端都是闭区间
#[test]
fn range_scan_walks_the_leaf_chain() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = open(dir.path(), "range.idx");
    for word in ["apple", "banana", "cherry", "date", "elderberry"] {
        t.insert(word.as_bytes(), word.len() as u64).unwrap();
    }

    let mid: Vec<String> = t
        .scan(Some(b"banana"), Some(b"date"))
        .unwrap()
        .into_iter()
        .map(|(k, _)| String::from_utf8(k).unwrap())
        .collect();
    assert_eq!(mid, vec!["banana", "cherry", "date"]);

    let from: Vec<String> = t
        .scan(Some(b"cherry"), None)
        .unwrap()
        .into_iter()
        .map(|(k, _)| String::from_utf8(k).unwrap())
        .collect();
    assert_eq!(from, vec!["cherry", "date", "elderberry"]);

    let until: Vec<String> = t
        .scan(None, Some(b"banana"))
        .unwrap()
        .into_iter()
        .map(|(k, _)| String::from_utf8(k).unwrap())
        .collect();
    assert_eq!(until, vec!["apple", "banana"]);

    assert!(t.scan(Some(b"z"), None).unwrap().is_empty());
}

/// 清空之后还能接着用
#[test]
fn clear_resets_the_tree() {
    let dir = tempfile::tempdir().unwrap();
    let mut t = open(dir.path(), "clear.idx");
    t.insert(b"a", 1).unwrap();
    t.insert(b"b", 2).unwrap();
    t.clear().unwrap();
    assert!(t.iter_all().unwrap().is_empty());
    t.insert(b"c", 3).unwrap();
    assert_eq!(t.get(b"c").unwrap(), Some(3));
}
