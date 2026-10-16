use chusql_core_storage::config::DEFAULT_PAGE_SIZE;
use chusql_core_storage::page::{Page, PageFile};

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

/// 已同步镜像恢复撕裂页且页 LSN 持久保留
#[test]
fn full_page_image_repairs_torn_write_before_read() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("torn.db");
    let metadata = dir.path().join("torn.db.pages");
    let journal = dir.path().join("torn.db.page-wal");
    let mut file = PageFile::with_options(&path, 512, 1).unwrap();
    file.set_lsn(7);
    file.write_page(&Page::new(0, 512)).unwrap();
    file.flush().unwrap();
    let old_metadata = std::fs::read(&metadata).unwrap();
    file.set_lsn(13);
    let mut changed = Page::new(0, 512);
    changed.data.fill(0x5a);
    file.write_page(&changed).unwrap();
    file.append_page().unwrap(); // 淘汰页零前镜像已同步
    let image = std::fs::read(&journal).unwrap();
    assert!(!image.is_empty());
    drop(file);
    std::fs::write(&metadata, old_metadata).unwrap();
    std::fs::write(&journal, &image).unwrap();
    std::fs::write(&path, [0x11; 213]).unwrap();
    let mut reopened = PageFile::open(&path, 512).unwrap();
    assert_eq!(reopened.read_page(0).unwrap().data, changed.data);
    assert_eq!(reopened.page_lsn(0), Some(13));
    drop(reopened);
    assert_eq!(std::fs::metadata(&journal).unwrap().len(), 0);
    let mut reopened = PageFile::open(&path, 512).unwrap();
    assert_eq!(reopened.read_page(0).unwrap().data, changed.data);
    assert_eq!(reopened.page_lsn(0), Some(13));
}

/// 撕裂日志尾不发布，完整损坏帧明确拒绝
#[test]
fn page_image_tail_and_corruption_are_distinguished() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("tail.db");
    let metadata = dir.path().join("tail.db.pages");
    let journal = dir.path().join("tail.db.page-wal");
    let mut file = PageFile::with_options(&path, 512, 1).unwrap();
    file.write_page(&Page::new(0, 512)).unwrap();
    file.flush().unwrap();
    let old_metadata = std::fs::read(&metadata).unwrap();
    let old_data = std::fs::read(&path).unwrap();
    file.set_lsn(20);
    let mut changed = Page::new(0, 512);
    changed.data.fill(9);
    file.write_page(&changed).unwrap();
    file.append_page().unwrap();
    let image = std::fs::read(&journal).unwrap();
    drop(file);
    for end in [1, 39, image.len() - 1] {
        std::fs::write(&metadata, &old_metadata).unwrap();
        std::fs::write(&path, &old_data).unwrap();
        std::fs::write(&journal, &image[..end]).unwrap();
        let mut reopened = PageFile::open(&path, 512).unwrap();
        assert_eq!(reopened.read_page(0).unwrap().data, vec![0; 512]);
    }
    let mut corrupt = image;
    corrupt[40] ^= 1;
    std::fs::write(&metadata, &old_metadata).unwrap();
    std::fs::write(&journal, &corrupt).unwrap();
    assert!(
        PageFile::open(&path, 512)
            .unwrap_err()
            .to_string()
            .contains("checksum")
    );
    assert_eq!(std::fs::read(&journal).unwrap(), corrupt);
}

/// 截断标记阻止旧页重放后复活
#[test]
fn truncate_clears_page_versions_and_survives_reopen() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("reset.db");
    let mut file = PageFile::open(&path, 512).unwrap();
    file.set_lsn(5);
    for _ in 0..3 {
        file.append_page().unwrap();
    }
    file.flush().unwrap();
    file.set_lsn(8);
    file.truncate().unwrap();
    file.append_page().unwrap();
    file.flush().unwrap();
    assert_eq!(file.page_lsn(0), Some(8));
    assert_eq!(file.page_lsn(2), None);
    drop(file);
    let mut file = PageFile::open(&path, 512).unwrap();
    assert_eq!(file.num_pages().unwrap(), 1);
    assert_eq!(file.page_lsn(0), Some(8));
    assert_eq!(file.page_lsn(2), None);
}

/// 镜像写入失败后禁止析构或重试绕过恢复
#[test]
fn page_journal_error_fences_further_flushes() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("fenced.db");
    let journal = dir.path().join("fenced.db.page-wal");
    std::fs::write(&path, [0; 512]).unwrap();
    let mut file = PageFile::open(&path, 512).unwrap();
    let mut page = Page::new(0, 512);
    page.data.fill(7);
    file.write_page(&page).unwrap();
    std::fs::create_dir(&journal).unwrap();
    assert!(file.flush().is_err());
    std::fs::remove_dir(&journal).unwrap();
    assert!(
        file.flush()
            .unwrap_err()
            .to_string()
            .contains("requires recovery")
    );
    drop(file);
    let mut file = PageFile::open(&path, 512).unwrap();
    assert_eq!(file.read_page(0).unwrap().data, vec![0; 512]);
}

/// 小缓存淘汰不会反复记录同一批未改页
#[test]
fn eviction_batches_images_without_pool_sized_write_amplification() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("bounded.db");
    let journal = dir.path().join("bounded.db.page-wal");
    let mut file = PageFile::with_options(&path, 512, 2).unwrap();
    file.set_lsn(9);
    for id in 0..100 {
        let mut page = Page::new(id, 512);
        page.data.fill(id as u8);
        file.write_page(&page).unwrap();
    }
    assert!(std::fs::metadata(&journal).unwrap().len() <= 100 * (512 + 40));
    file.flush().unwrap();
    drop(file);
    let mut file = PageFile::open(&path, 512).unwrap();
    for id in 0..100 {
        assert_eq!(file.read_page(id).unwrap().data, vec![id as u8; 512]);
        assert_eq!(file.page_lsn(id), Some(9));
    }
}
