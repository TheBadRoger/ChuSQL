//! 命名管道服务：按行读写 JSON。Windows 上管道名映射为 \\.\pipe\<name>。
//! 启动时读配置（环境变量 → TOML → 默认值）并把每项来源打进日志。

use std::collections::BTreeSet;
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::sync::Arc;
use std::thread;

use chusql_storage::catalog::Catalog;
use chusql_storage::config::{self, Config, Loaded};
use chusql_storage::heap::HeapTable;
use chusql_storage::log;
use chusql_storage::protocol::{Request, Response};
use chusql_storage::wal::{Wal, WalOp};
use chusql_storage::{log_debug, log_error, log_info, log_warn};
use interprocess::local_socket::{
    prelude::*,
    GenericNamespaced,
    ListenerOptions,
};
use interprocess::TryClone;

// * 路径

/// 表名 -> 数据文件路径。
fn table_path(cfg: &Config, table: &str) -> PathBuf {
    cfg.data_dir.join(format!("{}.db", table))
}

/// 表名 -> 索引文件路径。
fn index_path(cfg: &Config, table: &str) -> PathBuf {
    cfg.data_dir.join(format!("{}.idx", table))
}

/// 数据字典文件路径。
fn catalog_path(cfg: &Config) -> PathBuf {
    cfg.data_dir.join("catalog.json")
}

/// WAL 文件路径。
fn wal_path(cfg: &Config) -> PathBuf {
    cfg.data_dir.join("wal.log")
}

// * 表操作

/// 打开一张表，执行闭包；表不存在就创建。
fn with_table<R>(
    cfg: &Config,
    table: &str,
    f: impl FnOnce(&mut HeapTable) -> std::io::Result<R>,
) -> std::io::Result<R> {
    std::fs::create_dir_all(&cfg.data_dir)?;
    let mut t = HeapTable::open(table_path(cfg, table), cfg.page_size)?;
    f(&mut t)
}

/// 打开带索引的表，执行闭包。
fn with_indexed_table<R>(
    cfg: &Config,
    table: &str,
    f: impl FnOnce(&mut HeapTable) -> std::io::Result<R>,
) -> std::io::Result<R> {
    std::fs::create_dir_all(&cfg.data_dir)?;
    let mut t = HeapTable::open_indexed(
        table_path(cfg, table),
        index_path(cfg, table),
        cfg.page_size,
        cfg.btree_order,
    )?;
    f(&mut t)
}

/// 表文件是否存在且非空。
fn table_exists(cfg: &Config, table: &str) -> bool {
    let p = table_path(cfg, table);
    std::fs::metadata(&p).map(|m| m.len() > 0).unwrap_or(false)
}

/// 打开已存在的表；不存在返回 NotFound。
fn with_existing_table<R>(
    cfg: &Config,
    table: &str,
    f: impl FnOnce(&mut HeapTable) -> std::io::Result<R>,
) -> std::io::Result<R> {
    if !table_exists(cfg, table) {
        return Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("unknown table: {}", table),
        ));
    }
    let mut t = HeapTable::open(table_path(cfg, table), cfg.page_size)?;
    f(&mut t)
}

// * 数据字典操作

/// 读 catalog，按一行数据补列，再写回。
fn catalog_add_row(
    cfg: &Config,
    table: &str,
    row: &serde_json::Map<String, serde_json::Value>,
) -> std::io::Result<()> {
    let mut c = Catalog::load(catalog_path(cfg))?;
    let row: chusql_storage::protocol::Row = row.clone().into_iter().collect();
    c.ensure_columns(table, &row);
    c.save(catalog_path(cfg))
}

/// 读 catalog，按多行数据补列，再写回。
fn catalog_add_rows(
    cfg: &Config,
    table: &str,
    rows: &[serde_json::Map<String, serde_json::Value>],
) -> std::io::Result<()> {
    let mut c = Catalog::load(catalog_path(cfg))?;
    for r in rows {
        let row: chusql_storage::protocol::Row = r.clone().into_iter().collect();
        c.ensure_columns(table, &row);
    }
    c.save(catalog_path(cfg))
}

// * WAL 写路径与恢复

/// 把一条 WAL 操作真正落到磁盘（堆表 + 索引 + catalog）。
fn apply_op(cfg: &Config, op: &WalOp) -> std::io::Result<()> {
    match op {
        WalOp::Insert { table, row, key } => {
            match key {
                None => with_table(cfg, table, |t| t.insert(row))?,
                Some(k) => with_indexed_table(cfg, table, |t| t.insert_keyed(row, *k))?,
            }
            let map: serde_json::Map<_, _> = row.clone().into_iter().collect();
            catalog_add_row(cfg, table, &map)?;
            Ok(())
        }
        WalOp::ReplaceAll { table, rows } => {
            with_indexed_table(cfg, table, |t| t.replace_all(rows))?;
            let maps: Vec<_> = rows.iter().map(|r| r.clone().into_iter().collect()).collect();
            catalog_add_rows(cfg, table, &maps)?;
            Ok(())
        }
    }
}

/// 写前日志 + 写数据 + 清空 WAL。
///
/// 顺序：clear（清掉上次已完成的） → append+fsync → apply → clear。
/// 若中途进程被杀，重启时 WAL 里能读到那条操作并重放。
fn write_via_wal(cfg: &Config, op: &WalOp) -> std::io::Result<()> {
    let wal = Wal::new(wal_path(cfg));

    // 清掉上一次遗留（正常情况下已经是空的）
    wal.clear()?;

    // 先写 WAL 并 fsync
    wal.append(op)?;

    // 再写数据
    let result = apply_op(cfg, op);

    // 无论 apply 成功还是返回 Err，都清空 WAL：
    // 失败的操作不应被重放；若中途被杀，这里跑不到，WAL 会留到重启时重放。
    wal.clear()?;

    result
}

/// 启动时重放 WAL 里的残留操作。
fn recover_wal(cfg: &Config) -> std::io::Result<()> {
    let wal = Wal::new(wal_path(cfg));
    match wal.read()? {
        None => Ok(()),
        Some(op) => {
            log_warn!(core, "replaying incomplete WAL entry");
            apply_op(cfg, &op)?;
            wal.clear()?;
            log_info!(core, "WAL replay complete");
            Ok(())
        }
    }
}

// * 请求分发

/// 把一条请求翻译成一条响应。
fn handle_request(cfg: &Config, req: Request) -> Response {
    match req {
        Request::Ping => {
            log_debug!(pipe, "ping");
            Response::Pong
        }

        Request::Scan { table } => {
            log_debug!(pipe, "scan table={}", table);
            match with_existing_table(cfg, &table, |t| t.scan()) {
                Ok(rows) => Response::Rows { rows },
                Err(e) => {
                    log_warn!(pipe, "scan {}: {}", table, e);
                    Response::Error {
                        message: format!("scan {}: {}", table, e),
                    }
                }
            }
        }

        Request::Insert { table, row, key } => {
            log_debug!(pipe, "insert table={} key={:?}", table, key);
            let tname = table.clone();
            let op = WalOp::Insert { table, row, key };
            match write_via_wal(cfg, &op) {
                Ok(()) => Response::Ok,
                Err(e) => {
                    log_warn!(pipe, "insert {}: {}", tname, e);
                    Response::Error {
                        message: format!("insert {}: {}", tname, e),
                    }
                }
            }
        }

        Request::LookupByIndex { table, key } => {
            log_debug!(pipe, "lookup_by_index table={} key={}", table, key);
            match with_indexed_table(cfg, &table, |t| t.get_by_key(key)) {
                Ok(Some(row)) => Response::Rows { rows: vec![row] },
                Ok(None) => Response::Rows { rows: vec![] },
                Err(e) => {
                    log_warn!(pipe, "lookup_by_index {}: {}", table, e);
                    Response::Error {
                        message: format!("lookup_by_index {}: {}", table, e),
                    }
                }
            }
        }

        Request::ReplaceAll { table, rows } => {
            log_debug!(pipe, "replace_all table={} rows={}", table, rows.len());
            let tname = table.clone();
            let op = WalOp::ReplaceAll { table, rows };
            match write_via_wal(cfg, &op) {
                Ok(()) => Response::Ok,
                Err(e) => {
                    log_warn!(pipe, "replace_all {}: {}", tname, e);
                    Response::Error {
                        message: format!("replace_all {}: {}", tname, e),
                    }
                }
            }
        }

        Request::ListTables => {
            log_debug!(pipe, "list_tables");
            match list_tables(cfg) {
                Ok(tables) => Response::Tables { tables },
                Err(e) => {
                    log_warn!(pipe, "list_tables: {}", e);
                    Response::Error {
                        message: format!("list_tables: {}", e),
                    }
                }
            }
        }

        Request::DescribeTable { table } => {
            log_debug!(pipe, "describe_table table={}", table);
            match describe_table(cfg, &table) {
                Ok(cols) => Response::Schema { columns: cols },
                Err(e) => {
                    log_warn!(pipe, "describe_table {}: {}", table, e);
                    Response::Error {
                        message: format!("describe_table {}: {}", table, e),
                    }
                }
            }
        }
    }
}

/// 列出表名：catalog 记录 + data 目录下的 .db 文件，取并集。
fn list_tables(cfg: &Config) -> std::io::Result<Vec<String>> {
    let mut names = BTreeSet::new();

    let c = Catalog::load(catalog_path(cfg))?;
    for t in c.table_names() {
        names.insert(t);
    }

    let dir = &cfg.data_dir;
    if dir.exists() {
        for entry in std::fs::read_dir(dir)? {
            let entry = entry?;
            let name = entry.file_name().to_string_lossy().to_string();
            if let Some(stem) = name.strip_suffix(".db") {
                names.insert(stem.to_string());
            }
        }
    }

    Ok(names.into_iter().collect())
}

/// 查一张表的 schema。表在 catalog 里没有，但文件存在，返回空列。
fn describe_table(
    cfg: &Config,
    table: &str,
) -> std::io::Result<Vec<chusql_storage::protocol::SchemaColumn>> {
    let c = Catalog::load(catalog_path(cfg))?;
    if let Some(schema) = c.describe(table) {
        return Ok(schema.columns.clone());
    }
    if table_exists(cfg, table) {
        return Ok(Vec::new());
    }
    Err(std::io::Error::new(
        std::io::ErrorKind::NotFound,
        format!("unknown table: {}", table),
    ))
}

// * 连接处理

/// 处理一条连接：逐行读 JSON 请求，逐行回 JSON 响应。
fn handle(cfg: &Config, conn: LocalSocketStream) -> std::io::Result<()> {
    let mut writer = conn.try_clone()?;
    let reader = BufReader::new(conn);

    for line in reader.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }

        let resp = match serde_json::from_str::<Request>(&line) {
            Ok(req) => handle_request(cfg, req),
            Err(e) => {
                log_warn!(pipe, "bad request line: {}", e);
                Response::Error {
                    message: format!("parse error: {}", e),
                }
            }
        };

        writeln!(writer, "{}", serde_json::to_string(&resp).unwrap())?;
        writer.flush()?;
    }
    log_debug!(pipe, "connection closed");
    Ok(())
}

// * 主入口

/// 把"配置从哪来"打进日志。
fn log_config(loaded: &Loaded) {
    match &loaded.config_path {
        Some(p) => log_info!(core, "config file: {}", p.display()),
        None => log_info!(core, "config file: none"),
    }
    for (name, value, origin) in &loaded.origins {
        log_info!(core, "  {:<17} {} [{}]", name, value, origin.describe());
    }
}

fn main() -> std::io::Result<()> {
    let loaded = match config::load() {
        Ok(l) => l,
        Err(e) => {
            log_error!(core, "config error: {}", e);
            return Err(std::io::Error::new(std::io::ErrorKind::InvalidInput, e));
        }
    };
    log::init();
    log::set_level(loaded.config.log_level);

    log_info!(core, "chusql-storage {} starting", env!("CARGO_PKG_VERSION"));
    log_config(&loaded);

    let cfg = Arc::new(loaded.config);

    // 启动时先重放 WAL，再做其他事
    if let Err(e) = recover_wal(&cfg) {
        log_error!(core, "WAL recovery failed: {}", e);
        return Err(e);
    }

    let ns_name = cfg
        .pipe_name
        .as_str()
        .to_ns_name::<GenericNamespaced>()
        .map_err(|e| std::io::Error::other(e.to_string()))?;
    let listener = ListenerOptions::new().name(ns_name).create_sync()?;

    log_info!(
        core,
        "listening on pipe {} (\\\\.\\pipe\\{})",
        cfg.pipe_name,
        cfg.pipe_name
    );

    for conn in listener.incoming() {
        let conn = conn?;
        let cfg = Arc::clone(&cfg);
        thread::spawn(move || {
            log_debug!(pipe, "connection accepted");
            if let Err(e) = handle(&cfg, conn) {
                log_error!(pipe, "connection error: {}", e);
            }
        });
    }
    Ok(())
}
