use chusql_storage::config::DEFAULT_PAGE_SIZE;
use chusql_storage::page::{Page, PageFile};

// 页文件测试：一页的读写、页号越界报错、追加页让文件变长、多页互不干扰、覆写同一页、
// 自定义页大小、页大小和文件对不上时报错。

/// 写入一页再读回来，内容和长度都应该一致。
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

/// 空文件读第 0 页应该报错。
#[test]
fn read_out_of_range_returns_error() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("test.db");

    let mut pf = PageFile::open(&path, DEFAULT_PAGE_SIZE).unwrap();
    assert!(pf.read_page(0).is_err());
}

/// 每 append 一页，num_pages 就加一。
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

/// 页 0 和页 1 的内容互不影响。
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

/// 覆写同一页：读到的是新内容，页数不变。
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

/// 页大小可以配置：512 字节的页照样读写，文件按 512 字节对齐。
#[test]
fn custom_page_size() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("small.db");

    let mut pf = PageFile::open(&path, 512).unwrap();
    assert_eq!(pf.page_size(), 512);

    let mut p = Page::new(0, 512);
    p.data[511] = 9;
    pf.write_page(&p).unwrap();

    assert_eq!(pf.read_page(0).unwrap().data[511], 9);
    assert_eq!(pf.num_pages().unwrap(), 1);
    assert_eq!(std::fs::metadata(&path).unwrap().len(), 512);
}

/// 文件长度不是页大小整数倍（比如页大小被改过）要报错，而不是算出错误的页数。
#[test]
fn mismatched_file_length_errors() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("odd.db");
    std::fs::write(&path, vec![0u8; 100]).unwrap();

    let mut pf = PageFile::open(&path, DEFAULT_PAGE_SIZE).unwrap();
    assert!(pf.num_pages().is_err());
}

/// 页大小对不上的页不许写进去。
#[test]
fn write_wrong_sized_page_errors() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("test.db");

    let mut pf = PageFile::open(&path, 512).unwrap();
    assert!(pf.write_page(&Page::new(0, 1024)).is_err());
}
