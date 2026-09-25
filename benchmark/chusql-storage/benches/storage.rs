//! 存储层基准：页 IO、B+ 树、堆表、WAL 与服务端写入路径。
//!
//! 用 `std::time::Instant` 计时，不引第三方基准框架，离线也能跑。
//! 每项跑 `rounds` 轮取最快一轮：存储层的抖动主要来自 fsync 与调度，
//! 取最快一轮最接近"这套代码本身要花多久"。
//!
//! 跑法：`cargo bench`（可选环境变量 `CHUSQL_BENCH_N` 改规模，默认 2000）

use std::time::{Duration, Instant};

use chusql_storage::btree::DiskBTree;
use chusql_storage::config::{DEFAULT_BTREE_ORDER, DEFAULT_PAGE_SIZE};
use chusql_storage::heap::HeapTable;
use chusql_storage::page::{Page, PageFile};
use chusql_storage::protocol::Row;
use chusql_storage::wal::{Wal, WalOp};
use serde_json::json;

// 工具
/// 跑一个操作 `ops` 次，量总耗时并折算每次耗时
fn bench<F: FnMut()>(label: &str, ops: usize, rounds: usize, mut f: F) {
    let mut best = Duration::MAX;
    for _ in 0..rounds {
        let t0 = Instant::now();
        f();
        let d = t0.elapsed();
        if d < best {
            best = d;
        }
    }
    let per = best.as_secs_f64() * 1e6 / ops as f64;
    println!(
        "{:<38} {:>9.2} ms {:>10.3} us/次",
        label,
        best.as_secs_f64() * 1e3,
        per
    );
}

/// 只量一次（fsync 类操作太慢，不适合反复跑）
fn bench_once<F: FnOnce()>(label: &str, ops: usize, f: F) {
    let t0 = Instant::now();
    f();
    let d = t0.elapsed();
    let per = d.as_secs_f64() * 1e6 / ops as f64;
    println!(
        "{:<38} {:>9.2} ms {:>10.3} us/次",
        label,
        d.as_secs_f64() * 1e3,
        per
    );
}

/// 造一行
fn row(id: i64) -> Row {
    let mut m = Row::new();
    m.insert("id".to_string(), json!(id));
    m.insert("name".to_string(), json!(format!("user{}", id)));
    m
}

/// 小随机数发生器，避免引 rand
fn next_rand(state: &mut u64) -> u64 {
    *state = state.wrapping_mul(6364136223846793005).wrapping_add(1442695040888963407);
    *state >> 33
}

// 页 IO
/// 顺序写 / 顺序读 / 随机读
fn bench_page(dir: &std::path::Path, n: usize) {
    let path = dir.join("page.db");
    let mut pf = PageFile::with_options(&path, DEFAULT_PAGE_SIZE, 1024).unwrap();

    bench("page 顺序写（含 flush）", n, 3, || {
        pf.truncate().unwrap();
        for i in 0..n {
            pf.write_page(&Page::new(i as u64, DEFAULT_PAGE_SIZE)).unwrap();
        }
        pf.flush().unwrap();
    });
    drop(pf);

    // 池子故意开小，让顺序读真的去读盘
    let mut pf = PageFile::with_options(&path, DEFAULT_PAGE_SIZE, 16).unwrap();
    bench("page 顺序读（池 16，基本都缺页）", n, 3, || {
        for i in 0..n {
            pf.read_page(i as u64).unwrap();
        }
    });

    let mut pf = PageFile::with_options(&path, DEFAULT_PAGE_SIZE, 16).unwrap();
    let mut state = 12345u64;
    bench("page 随机读（池 16）", n, 3, || {
        for _ in 0..n {
            let id = next_rand(&mut state) % n as u64;
            pf.read_page(id).unwrap();
        }
    });

    // 池子开大、工作集比池子还大：每次缺页都要挑一个牺牲页
    let mut pf = PageFile::with_options(&path, DEFAULT_PAGE_SIZE, 1024).unwrap();
    let mut state = 999u64;
    bench("page 随机读（池 1024，工作集 > 池）", n, 3, || {
        for _ in 0..n {
            let id = next_rand(&mut state) % n as u64;
            pf.read_page(id).unwrap();
        }
    });
    println!(
        "  （池 1024：命中 {}/{})",
        pf.hits(),
        pf.hits() + pf.misses()
    );
}

// B+ 树
/// 插入、点查、全量遍历
fn bench_btree(dir: &std::path::Path, n: usize) {
    let path = dir.join("bt.idx");
    let _ = std::fs::remove_file(&path);
    let mut bt = DiskBTree::open(&path, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER, 1024).unwrap();

    bench("btree 插入（含 flush）", n, 2, || {
        bt.clear().unwrap();
        for i in 0..n {
            bt.insert(i as i64, i as u64).unwrap();
        }
        bt.flush().unwrap();
    });

    bench("btree 点查（全部命中）", n, 3, || {
        for i in 0..n {
            bt.get(i as i64).unwrap().expect("key must exist");
        }
    });

    bench("btree 全量遍历", n, 3, || {
        let v = bt.iter_all().unwrap();
        assert_eq!(v.len(), n);
    });
}

// 堆表
/// 插行、全表扫描、按索引取行
fn bench_heap(dir: &std::path::Path, n: usize) {
    let path = dir.join("heap.db");
    let idx = dir.join("heap.idx");
    let _ = std::fs::remove_file(&path);
    let _ = std::fs::remove_file(&idx);
    let mut t = HeapTable::open(&path, DEFAULT_PAGE_SIZE, 1024).unwrap();

    bench("heap 插入（只在缓冲池，不 flush）", n, 2, || {
        t.replace_all(&[]).unwrap();
        for i in 0..n {
            t.insert_row(&row(i as i64)).unwrap();
        }
    });

    bench("heap 插入 + flush", n, 2, || {
        t.replace_all(&[]).unwrap();
        for i in 0..n {
            t.insert_row(&row(i as i64)).unwrap();
        }
        t.flush().unwrap();
    });

    bench("heap 批量插入 + flush（一次落盘）", n, 2, || {
        t.replace_all(&[]).unwrap();
        let rows: Vec<Row> = (0..n as i64).map(row).collect();
        t.insert_rows(&rows).unwrap();
        t.flush().unwrap();
    });

    bench("heap 全表扫描", n, 3, || {
        let rows = t.scan().unwrap();
        assert_eq!(rows.len(), n);
    });
}

// WAL
/// 追加写与清空
fn bench_wal(dir: &std::path::Path, n: usize) {
    let path = dir.join("wal.log");
    let _ = std::fs::remove_file(&path);
    let wal = Wal::new(&path);
    let op = WalOp::Insert {
        table: "t".to_string(),
        row: row(1),
    };

    bench_once("wal 追加（每条都 fsync）", n, || {
        for _ in 0..n {
            wal.append(&op).unwrap();
        }
    });
    wal.clear().unwrap();

    bench_once("wal 清空（每条都 fsync）", n, || {
        for _ in 0..n {
            wal.clear().unwrap();
        }
    });
    wal.clear().unwrap();

    bench_once("wal 截断（不落盘）", n, || {
        for _ in 0..n {
            wal.truncate().unwrap();
        }
    });

    // 拆分：一次 fsync 本身多贵，一次 open 多贵
    use std::fs::OpenOptions;
    use std::io::Write;
    // 大小和一条真实的 WAL 记录同量级（~200 字节）
    let bytes = vec![0u8; 200];
    bench_once("  └ open + write + fsync", n, || {
        for _ in 0..n {
            let mut f = OpenOptions::new()
                .create(true)
                .append(true)
                .open(&path)
                .unwrap();
            f.write_all(&bytes).unwrap();
            f.sync_data().unwrap();
        }
    });
    let mut open_file = OpenOptions::new()
        .create(true)
        .append(true)
        .open(&path)
        .unwrap();
    bench_once("  └ 复用句柄：write + fsync", n, || {
        for _ in 0..n {
            open_file.write_all(&bytes).unwrap();
            open_file.sync_data().unwrap();
        }
    });
    bench_once("  └ only fsync（不开不写）", n, || {
        for _ in 0..n {
            open_file.sync_data().unwrap();
        }
    });
    bench_once("  └ only open/close", n, || {
        for _ in 0..n {
            let _ = OpenOptions::new()
                .create(true)
                .append(true)
                .open(&path)
                .unwrap();
        }
    });
    wal.clear().unwrap();
}

// 服务端写入路径
/// 复刻 server.rs 里 write_via_wal 的顺序：清 WAL -> 写 WAL -> 改数据 -> flush -> 清 WAL
fn bench_write_path(dir: &std::path::Path, n: usize) {
    let path = dir.join("wp.db");
    let idx = dir.join("wp.idx");
    let wal_path = dir.join("wp.wal");
    let _ = std::fs::remove_file(&path);
    let _ = std::fs::remove_file(&idx);
    let _ = std::fs::remove_file(&wal_path);

    let mut t = HeapTable::open_indexed(
        &path,
        &idx,
        DEFAULT_PAGE_SIZE,
        DEFAULT_BTREE_ORDER,
        1024,
    )
    .unwrap();
    let wal = Wal::new(&wal_path);

    bench_once("写入路径 单条 INSERT（端到端）", n, || {
        for i in 0..n as i64 {
            wal.truncate().unwrap();
            let op = WalOp::Insert {
                table: "t".to_string(),
                row: row(i),
            };
            wal.append(&op).unwrap();
            t.insert_row(&row(i)).unwrap();
            t.flush().unwrap();
            wal.clear().unwrap();
        }
    });

    // 拆开量，好定位时间花在哪一步
    bench_once("  └ wal.truncate()（不落盘）", n, || {
        for _ in 0..n {
            wal.truncate().unwrap();
        }
    });

    let op = WalOp::Insert {
        table: "t".to_string(),
        row: row(1),
    };
    bench_once("  └ wal.append()（含 fsync）", n, || {
        for _ in 0..n {
            wal.append(&op).unwrap();
        }
    });
    wal.clear().unwrap();

    bench_once("  └ wal.clear()（含 fsync）", n, || {
        for _ in 0..n {
            wal.clear().unwrap();
        }
    });

    bench_once("  └ heap.insert_row()（含写索引）", n, || {
        // 主循环已经插过 0..n 了，这里换个号段，免得撞上"索引列唯一"
        for i in n as i64..2 * n as i64 {
            t.insert_row(&row(i)).unwrap();
        }
    });

    bench_once("  └ heap.flush()", n, || {
        for _ in 0..n {
            t.flush().unwrap();
        }
    });
}

fn main() {
    let n: usize = std::env::var("CHUSQL_BENCH_N")
        .ok()
        .and_then(|v| v.parse().ok())
        .unwrap_or(2000);

    println!("chusql-storage 基准：n = {}，页大小 = {} 字节，btree order = {}",
        n, DEFAULT_PAGE_SIZE, DEFAULT_BTREE_ORDER);
    println!("每项取最快一轮；带 fsync 的项只跑一次（跑多了太慢）\n");

    let dir = tempfile::tempdir().unwrap();

    println!("[页 IO]");
    bench_page(dir.path(), n);

    println!("\n[B+ 树]");
    bench_btree(dir.path(), n);

    println!("\n[堆表]");
    bench_heap(dir.path(), n);

    println!("\n[WAL]");
    bench_wal(dir.path(), n.min(200));

    println!("\n[服务端写入路径]");
    bench_write_path(dir.path(), n.min(200));
}
