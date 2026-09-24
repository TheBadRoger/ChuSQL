
use std::collections::{BTreeSet, HashMap};
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread;

use chusql_storage::catalog::Catalog;
use chusql_storage::config::{self, Config, Loaded};
use chusql_storage::heap::HeapTable;
use chusql_storage::log;
use chusql_storage::protocol::{Request, Response, SchemaColumn, TableSchemaWire};
use chusql_storage::wal::{Wal, WalOp};
use chusql_storage::{log_debug, log_error, log_info, log_warn};
use interprocess::local_socket::{
    prelude::*,
    GenericNamespaced,
    ListenerOptions,
};
use interprocess::TryClone;

// 命名管道服务：按行读写 JSON，Windows 上映射成 \\.\pipe\<name>。
// 启动时读配置并把每项来源打进日志；表句柄按名字缓存复用。

// 服务器
/// 服务器状态：配置 + 表句柄缓存 + 写锁
struct Server {
    cfg: Config,
    tables: Mutex<HashMap<String, Arc<Mutex<HeapTable>>>>,
    write_lock: Mutex<()>,
    catalog: Mutex<Catalog>,
}

impl Server {
    /// 新建服务器状态
    fn new(cfg: Config) -> std::io::Result<Self> {
        std::fs::create_dir_all(&cfg.data_dir)?;
        let catalog = Catalog::load(cfg.data_dir.join("catalog.json"))?;
        Ok(Server {
            cfg,
            tables: Mutex::new(HashMap::new()),
            write_lock: Mutex::new(()),
            catalog: Mutex::new(catalog),
        })
    }

// 路径
    /// 表名 -> 数据文件
    fn table_path(&self, table: &str) -> PathBuf {
        self.cfg.data_dir.join(format!("{}.db", table))
    }

    /// 表名 -> 索引文件
    fn index_path(&self, table: &str) -> PathBuf {
        self.cfg.data_dir.join(format!("{}.idx", table))
    }

    /// 数据字典文件
    fn catalog_path(&self) -> PathBuf {
        self.cfg.data_dir.join("catalog.json")
    }

    /// WAL 文件
    fn wal_path(&self) -> PathBuf {
        self.cfg.data_dir.join("wal.log")
    }


// 表句柄
    /// 表文件存在且非空
    fn table_exists(&self, table: &str) -> bool {
        if self.tables.lock().unwrap().contains_key(table) {
            return true;
        }
        if let Ok(c) = Catalog::load(self.catalog_path()) {
            if c.describe(table).is_some() {
                return true;
            }
        }
        std::fs::metadata(self.table_path(table))
            .map(|m| m.len() > 0)
            .unwrap_or(false)
    }

    /// 拿表句柄（缓存复用）
    fn table_handle(&self, table: &str) -> std::io::Result<Arc<Mutex<HeapTable>>> {
        let mut map = self.tables.lock().unwrap();
        if let Some(h) = map.get(table) {
            return Ok(h.clone());
        }
        std::fs::create_dir_all(&self.cfg.data_dir)?;
        let t = HeapTable::open_indexed(
            self.table_path(table),
            self.index_path(table),
            self.cfg.page_size,
            self.cfg.btree_order,
            self.cfg.pool_size,
        )?;
        let h = Arc::new(Mutex::new(t));
        map.insert(table.to_string(), h.clone());
        Ok(h)
    }

    /// 在表上跑闭包
    fn with_table<R>(
        &self,
        table: &str,
        f: impl FnOnce(&mut HeapTable) -> std::io::Result<R>,
    ) -> std::io::Result<R> {
        let h = self.table_handle(table)?;
        let mut guard = h.lock().unwrap();
        f(&mut guard)
    }

    /// 同上，但一定带索引
    fn with_indexed_table<R>(
        &self,
        table: &str,
        f: impl FnOnce(&mut HeapTable) -> std::io::Result<R>,
    ) -> std::io::Result<R> {
        self.with_table(table, f)
    }

    /// 表不存在就报错
    fn with_existing_table<R>(
        &self,
        table: &str,
        f: impl FnOnce(&mut HeapTable) -> std::io::Result<R>,
    ) -> std::io::Result<R> {
        // 已经在缓存里的表不用再查磁盘（省一次 metadata 系统调用）
        let cached = self.tables.lock().unwrap().contains_key(table);
        if !cached && !self.table_exists(table) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("unknown table: {}", table),
            ));
        }
        self.with_table(table, f)
    }

// 写入路径
    /// 把一条 WAL 操作落到磁盘
    fn apply_op(&self, op: &WalOp) -> std::io::Result<()> {
        match op {
            WalOp::Insert { table, row, key } => {
                match key {
                    None => self.with_table(table, |t| t.insert(row))?,
                    Some(k) => self.with_indexed_table(table, |t| t.insert_keyed(row, *k))?,
                }
                let row: chusql_storage::protocol::Row = row.clone().into_iter().collect();
                let mut c = self.catalog.lock().unwrap();
                if c.record_insert(table, &row) {
                    c.save(self.catalog_path())?;
                }
                Ok(())
            }
            WalOp::ReplaceAll { table, rows } => {
                self.with_indexed_table(table, |t| t.replace_all(rows))?;
                let rows: Vec<chusql_storage::protocol::Row> =
                    rows.iter().map(|r| r.clone().into_iter().collect()).collect();
                let mut c = self.catalog.lock().unwrap();
                if c.record_replace_all(table, &rows) {
                    c.save(self.catalog_path())?;
                }
                Ok(())
            }
        }
    }

    /// 先写 WAL 再改数据
    fn write_via_wal(&self, op: &WalOp) -> std::io::Result<()> {
        let _guard = self.write_lock.lock().unwrap();
        std::fs::create_dir_all(&self.cfg.data_dir)?;
        let wal = Wal::new(self.wal_path());

        wal.clear()?;

        wal.append(op)?;

        let result = self.apply_op(op);

        if result.is_ok() {
            if let Err(e) = self.flush_op_tables(op) {
                return Err(e);
            }
        }

        wal.clear()?;
        result
    }

    /// 把涉及的表写回磁盘
    fn flush_op_tables(&self, op: &WalOp) -> std::io::Result<()> {
        match op {
            WalOp::Insert { table, key, .. } => {
                if key.is_some() {
                    self.with_indexed_table(table, |t| t.flush())
                } else {
                    self.with_table(table, |t| t.flush())
                }
            }
            WalOp::ReplaceAll { table, .. } => {
                self.with_indexed_table(table, |t| t.flush())
            }
        }
    }

    /// 启动时重放残留 WAL
    fn recover_wal(&self) -> std::io::Result<()> {
        std::fs::create_dir_all(&self.cfg.data_dir)?;
        let wal = Wal::new(self.wal_path());
        match wal.read()? {
            None => Ok(()),
            Some(op) => {
                log_warn!(core, "replaying incomplete WAL entry");
                self.apply_op(&op)?;
                wal.clear()?;
                log_info!(core, "WAL replay complete");
                Ok(())
            }
        }
    }


// 请求分发
    /// 一条请求翻成一条响应
    fn handle_request(&self, req: Request) -> Response {
        match req {
            Request::Ping => {
                log_debug!(pipe, "ping");
                Response::Pong
            }

            Request::Scan { table } => {
                log_debug!(pipe, "scan table={}", table);

                let result = self.with_existing_table(&table, |t| t.scan());

                match result {
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
                match self.write_via_wal(&op) {
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
                match self.with_indexed_table(&table, |t| t.get_by_key(key)) {
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
                match self.write_via_wal(&op) {
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
                match self.list_tables() {
                    Ok(tables) => Response::Tables { tables },
                    Err(e) => {
                        log_warn!(pipe, "list_tables: {}", e);
                        Response::Error {
                            message: format!("list_tables: {}", e),
                        }
                    }
                }
            }

            Request::ListCatalog => {
                log_debug!(pipe, "list_catalog");
                let c = self.catalog.lock().unwrap();
                let schemas: Vec<TableSchemaWire> = c
                    .all_tables()
                    .into_iter()
                    .map(|(name, ts)| TableSchemaWire {
                        table: name.to_string(),
                        columns: ts.columns.clone(),
                        row_count: ts.row_count,
                    })
                    .collect();
                Response::Catalog { schemas }
            }

            Request::DescribeTable { table } => {
                log_debug!(pipe, "describe_table table={}", table);
                match self.describe_table(&table) {
                    Ok((cols, count)) => Response::Schema {
                        columns: cols,
                        row_count: count,
                    },
                    Err(e) => {
                        log_warn!(pipe, "describe_table {}: {}", table, e);
                        Response::Error {
                            message: format!("describe_table {}: {}", table, e),
                        }
                    }
                }
            }

            Request::CreateTable { table, columns } => {
                log_debug!(pipe, "create_table table={} cols={}", table, columns.len());

                {
                    let c = self.catalog.lock().unwrap();
                    if c.describe(&table).is_some() {
                        return Response::Error {
                            message: format!("create_table {}: table already exists", table),
                        };
                    }
                }

                if let Err(e) = self.with_table(&table, |_t| Ok(())) {
                    return Response::Error {
                        message: format!("create_table {}: {}", table, e),
                    };
                }

                let mut c = self.catalog.lock().unwrap();
                if let Err(e) = c.create_table(&table, columns) {
                    return Response::Error {
                        message: format!("create_table {}: {}", table, e),
                    };
                }
                if let Err(e) = c.save(self.catalog_path()) {
                    return Response::Error {
                        message: format!("create_table {}: catalog save: {}", table, e),
                    };
                }
                Response::Ok
            }

            Request::DropTable { table } => {
                log_debug!(pipe, "drop_table table={}", table);

                {
                    let mut c = self.catalog.lock().unwrap();
                    if let Err(e) = c.drop_table(&table) {
                        return Response::Error {
                            message: format!("drop_table {}: {}", table, e),
                        };
                    }
                    if let Err(e) = c.save(self.catalog_path()) {
                        return Response::Error {
                            message: format!("drop_table {}: catalog save: {}", table, e),
                        };
                    }
                }

                self.tables.lock().unwrap().remove(&table);
                let _ = std::fs::remove_file(self.table_path(&table));
                let _ = std::fs::remove_file(self.index_path(&table));

                Response::Ok
            }
        }
    }

// 信息查询
    /// 表名：catalog 与目录取并集
    fn list_tables(&self) -> std::io::Result<Vec<String>> {
        let mut names = BTreeSet::new();
        {
            let c = self.catalog.lock().unwrap();
            for t in c.table_names() {
                names.insert(t);
            }
        }
        let dir = &self.cfg.data_dir;
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

    /// 查一张表的 schema
    fn describe_table(&self, table: &str) -> std::io::Result<(Vec<SchemaColumn>, u64)> {
        let c = self.catalog.lock().unwrap();
        if let Some(schema) = c.describe(table) {
            return Ok((schema.columns.clone(), schema.row_count));
        }
        drop(c);
        if std::fs::metadata(self.table_path(table)).is_ok() {
            return Ok((Vec::new(), 0));
        }
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("unknown table: {}", table),
        ))
    }
}

impl Drop for Server {
    fn drop(&mut self) {
        if let Ok(c) = self.catalog.lock() {
            let _ = c.save(self.catalog_path());
        }
    }
}


// 连接
/// 处理一条连接
fn handle(server: &Server, conn: LocalSocketStream) -> std::io::Result<()> {
    let mut writer = conn.try_clone()?;
    let reader = BufReader::new(conn);

    for line in reader.lines() {
        let line = line?;
        if line.trim().is_empty() {
            continue;
        }

        let resp = match serde_json::from_str::<Request>(&line) {
            Ok(req) => server.handle_request(req),
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


// 入口
/// 把配置来源打进日志
fn log_config(loaded: &Loaded) {
    match &loaded.config_path {
        Some(p) => log_info!(core, "config file: {}", p.display()),
        None => log_info!(core, "config file: none"),
    }
    for (name, value, origin) in &loaded.origins {
        log_info!(core, "  {:<18} {} [{}]", name, value, origin.describe());
    }
}

/// 进程入口
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

    let server = Arc::new(Server::new(loaded.config)?);

    if let Err(e) = server.recover_wal() {
        log_error!(core, "WAL recovery failed: {}", e);
        return Err(e);
    }

    let ns_name = server
        .cfg
        .pipe_name
        .as_str()
        .to_ns_name::<GenericNamespaced>()
        .map_err(|e| std::io::Error::other(e.to_string()))?;
    let listener = ListenerOptions::new().name(ns_name).create_sync()?;

    log_info!(
        core,
        "listening on pipe {} (\\\\.\\pipe\\{})",
        server.cfg.pipe_name,
        server.cfg.pipe_name
    );

    for conn in listener.incoming() {
        let conn = conn?;
        let server = Arc::clone(&server);
        thread::spawn(move || {
            log_debug!(pipe, "connection accepted");
            if let Err(e) = handle(&server, conn) {
                log_error!(pipe, "connection error: {}", e);
            }
        });
    }
    Ok(())
}
