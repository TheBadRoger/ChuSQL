use chusql_storage::config::DEFAULT_PAGE_SIZE;
use chusql_storage::page::{Page, PageFile};

// 页文件测试：读写、追加、覆写、页大小校验与页缓存。

/// 写一页再读回一致
#[test]
fn write_then_read_page() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("test.db");

    let mut pf = PageFile::open(&path, DEFAULT_PAGE_SIZE).unwrap();

    let mut p = Page::new(0, DEFAULT_PAGE_SIZE);
    p.data[0] = 42;
    p.data[DEFAULT_PAGE_SIZE - 1] = 7;
    pf.write_page(&p).unwrap();

    let read = pf.read_page(0).unwrap();
    assert_eq!(read.data[0], 42);
    assert_eq!(read.data[DEFAULT_PAGE_SIZE - 1], 7);
    assert_eq!(read.data.len(), DEFAULT_PAGE_SIZE);
}

/// 空文件读第 0 页报错
#[test]
fn read_out_of_range_returns_error() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("test.db");

    let mut pf = PageFile::open(&path, DEFAULT_PAGE_SIZE).unwrap();
    assert!(pf.read_page(0).is_err());
}

/// 追加让页数加一
#[test]
fn append_grows_file() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("test.db");

    let mut pf = PageFile::open(&path, DEFAULT_PAGE_SIZE).unwrap();
    assert_eq!(pf.num_pages().unwrap(), 0);

    pf.append_page().unwrap();
    assert_eq!(pf.num_pages().unwrap(), 1);

    pf.append_page().unwrap();
    pf.append_page().unwrap();
    assert_eq!(pf.num_pages().unwrap(), 3);
}

/// 两页内容互不影响
#[test]
fn two_pages_independent() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("test.db");

    let mut pf = PageFile::open(&path, DEFAULT_PAGE_SIZE).unwrap();

    let mut a = Page::new(0, DEFAULT_PAGE_SIZE);
    a.data[0] = 1;
    let mut b = Page::new(1, DEFAULT_PAGE_SIZE);
    b.data[0] = 2;
    pf.write_page(&a).unwrap();
    pf.write_page(&b).unwrap();

    assert_eq!(pf.read_page(0).unwrap().data[0], 1);
    assert_eq!(pf.read_page(1).unwrap().data[0], 2);
}

/// 覆写同页读到新内容
#[test]
fn overwrite_page() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("test.db");

    let mut pf = PageFile::open(&path, DEFAULT_PAGE_SIZE).unwrap();

    let mut a = Page::new(0, DEFAULT_PAGE_SIZE);
    a.data[0] = 1;
    pf.write_page(&a).unwrap();

    let mut b = Page::new(0, DEFAULT_PAGE_SIZE);
    b.data[0] = 99;
    pf.write_page(&b).unwrap();

    assert_eq!(pf.read_page(0).unwrap().data[0], 99);
    assert_eq!(pf.num_pages().unwrap(), 1);
}

/// 512 字节页可用
#[test]
fn custom_page_size() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");
    let mut pf = PageFile::open(&path, 512).unwrap();

    pf.write_page(&Page::new(0, 512)).unwrap();
    pf.flush().unwrap();

    assert_eq!(std::fs::metadata(&path).unwrap().len(), 512);
}

/// 长度不是整数倍要报错
#[test]
fn mismatched_file_length_errors() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("odd.db");
    std::fs::write(&path, vec![0u8; 100]).unwrap();

    let mut pf = PageFile::open(&path, DEFAULT_PAGE_SIZE).unwrap();
    assert!(pf.num_pages().is_err());
}

/// 页大小不符拒绝写入
#[test]
fn write_wrong_sized_page_errors() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("test.db");

    let mut pf = PageFile::open(&path, 512).unwrap();
    assert!(pf.write_page(&Page::new(0, 1024)).is_err());
}

/// 第二次读命中缓存
#[test]
fn cache_hit_avoids_second_read() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");

    {
        let mut pf = PageFile::open(&path, 4096).unwrap();
        pf.write_page(&Page::new(0, 4096)).unwrap();
        pf.flush().unwrap();
    }

    let mut pf = PageFile::open(&path, 4096).unwrap();
    let _ = pf.read_page(0).unwrap();
    let _ = pf.read_page(0).unwrap();
    let _ = pf.read_page(0).unwrap();

    assert_eq!(pf.hits(), 2);
    assert_eq!(pf.misses(), 1);
    assert!(pf.hit_rate() > 0.6);
}

/// 退出时脏页写回
#[test]
fn dirty_page_flushed_on_drop() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("t.db");

    {
        let mut pf = PageFile::open(&path, DEFAULT_PAGE_SIZE).unwrap();
        let mut p = Page::new(0, DEFAULT_PAGE_SIZE);
        p.data[0] = 42;
        pf.write_page(&p).unwrap();
    }

    let mut pf2 = PageFile::open(&path, DEFAULT_PAGE_SIZE).unwrap();
    let p = pf2.read_page(0).unwrap();
    assert_eq!(p.data[0], 42);
}
