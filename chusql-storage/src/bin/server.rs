
use std::collections::{BTreeSet, HashMap};
use std::io::{BufRead, BufReader, Write};
use std::path::PathBuf;
use std::sync::{Arc, Mutex};
use std::thread;

use chusql_storage::catalog::Catalog;
use chusql_storage::config::{self, Config, Loaded};
use chusql_storage::heap::HeapTable;
use chusql_storage::log;
use chusql_storage::protocol::{
    ColumnStatWire, IndexWire, Request, Response, SchemaColumn, TableSchemaWire,
};
use chusql_storage::wal::{Wal, WalOp};
use chusql_storage::{log_debug, log_error, log_info, log_warn};
use interprocess::local_socket::{prelude::*, GenericNamespaced, ListenerOptions};
use interprocess::TryClone;

// 命名管道服务：按行读写 JSON，Windows 上映射成 \\.\pipe\<name>。
// 启动时读配置并把每项来源打进日志；表句柄按名字缓存复用。

// 服务器
/// 服务器状态：配置 + 表句柄缓存 + 写锁 + 数据字典
struct Server {
    cfg: Config,
    tables: Mutex<HashMap<String, Arc<Mutex<HeapTable>>>>,
    write_lock: Mutex<()>,
    catalog: Mutex<Catalog>,
    wal: Wal,
}

impl Server {
    /// 新建服务器状态
    fn new(cfg: Config) -> std::io::Result<Self> {
        std::fs::create_dir_all(&cfg.data_dir)?;
        let catalog = Catalog::load(cfg.data_dir.join("catalog.json"))?;
        let wal = Wal::new(cfg.data_dir.join("wal.log"));
        Ok(Server {
            cfg,
            tables: Mutex::new(HashMap::new()),
            write_lock: Mutex::new(()),
            catalog: Mutex::new(catalog),
            wal,
        })
    }

    // 路径
    /// 表名 -> 数据文件
    fn table_path(&self, table: &str) -> PathBuf {
        self.cfg.data_dir.join(format!("{}.db", table))
    }

    /// (表名, 列名) -> 索引文件。
    ///
    /// `id` 是内建索引，沿用老文件名 `表名.idx`（老数据目录不用迁移）；
    /// 其余列是 `表名.列名.idx`。
    fn index_path(&self, table: &str, column: &str) -> PathBuf {
        if column == "id" {
            self.cfg.data_dir.join(format!("{}.idx", table))
        } else {
            self.cfg
                .data_dir
                .join(format!("{}.{}.idx", table, column))
        }
    }

    /// 数据字典文件
    fn catalog_path(&self) -> PathBuf {
        self.cfg.data_dir.join("catalog.json")
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

    /// 一张表该开哪些索引：内建的 `id`，加上数据字典里记的那些
    fn index_columns(&self, table: &str) -> Vec<String> {
        let mut cols = vec!["id".to_string()];
        let c = self.catalog.lock().unwrap();
        if let Some(s) = c.describe(table) {
            for i in &s.indexes {
                if !cols.contains(&i.column) {
                    cols.push(i.column.clone());
                }
            }
        }
        cols
    }

    /// 拿表句柄（缓存复用）
    fn table_handle(&self, table: &str) -> std::io::Result<Arc<Mutex<HeapTable>>> {
        if let Some(h) = self.tables.lock().unwrap().get(table) {
            return Ok(h.clone());
        }
        // 先把索引清单取出来，再进 tables 锁（别套着锁去要 catalog）
        let columns = self.index_columns(table);
        let paths: Vec<(String, PathBuf)> = columns
            .iter()
            .map(|c| (c.clone(), self.index_path(table, c)))
            .collect();

        let mut map = self.tables.lock().unwrap();
        if let Some(h) = map.get(table) {
            return Ok(h.clone());
        }
        std::fs::create_dir_all(&self.cfg.data_dir)?;
        let t = HeapTable::open_with_indexes(
            self.table_path(table),
            &paths,
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
            WalOp::Insert { table, row } => {
                self.with_table(table, |t| t.insert_row(row))?;
                let mut c = self.catalog.lock().unwrap();
                if c.record_insert(table, row) {
                    c.save(self.catalog_path())?;
                }
                Ok(())
            }
            WalOp::InsertBatch { table, rows } => {
                self.with_table(table, |t| t.insert_rows(rows))?;
                let mut c = self.catalog.lock().unwrap();
                let mut changed = false;
                for r in rows {
                    if c.record_insert(table, r) {
                        changed = true;
                    }
                }
                if changed {
                    c.save(self.catalog_path())?;
                }
                Ok(())
            }
            WalOp::DeleteKeys { table, keys } => {
                let deleted = self.with_table(table, |t| t.delete_by_keys(keys))?;
                let mut c = self.catalog.lock().unwrap();
                c.record_delete(table, &deleted);
                Ok(())
            }
            WalOp::ReplaceAll { table, rows } => {
                self.with_table(table, |t| t.replace_all(rows))?;
                // 整表重写 = 统计与行数从头再数一遍
                let mut c = self.catalog.lock().unwrap();
                c.rebuild_stats(table, rows);
                c.save(self.catalog_path())
            }
        }
    }

    /// 先写 WAL 再改数据
    fn write_via_wal(&self, op: &WalOp) -> std::io::Result<()> {
        let _guard = self.write_lock.lock().unwrap();
        std::fs::create_dir_all(&self.cfg.data_dir)?;
        let wal = &self.wal;

        // 只截断不落盘：上一条写操作最后已经把 WAL 刷成空的了，
        // 这里的截断只是保证下面追加的是唯一一条，跟着的 append 会顺手把它落下去。
        wal.truncate()?;

        wal.append(op)?;

        let result = self.apply_op(op);

        if result.is_ok() {
            if let Err(e) = self.flush_op_tables(op) {
                return Err(e);
            }
        }

        // 这一步必须落盘：WAL 空 = 这条操作已经提交
        wal.clear()?;
        result
    }

    /// 把涉及的表写回磁盘
    fn flush_op_tables(&self, op: &WalOp) -> std::io::Result<()> {
        let table = match op {
            WalOp::Insert { table, .. }
            | WalOp::InsertBatch { table, .. }
            | WalOp::DeleteKeys { table, .. }
            | WalOp::ReplaceAll { table, .. } => table,
        };
        self.with_table(table, |t| t.flush())
    }

    /// 启动时重放残留 WAL
    fn recover_wal(&self) -> std::io::Result<()> {
        std::fs::create_dir_all(&self.cfg.data_dir)?;
        let wal = &self.wal;
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

    /// 启动时把列统计重数一遍。
    ///
    /// 统计是"每列见过哪些值"精确数出来的，只在内存里维护（随 catalog 落盘），
    /// 所以重启之后要扫一遍表把它重建出来；否则统计会随重启慢慢失真。
    fn rebuild_stats(&self) -> std::io::Result<()> {
        let names: Vec<String> = self.catalog.lock().unwrap().table_names();
        let mut touched = false;
        for table in names {
            if !self.table_path(&table).exists() {
                continue;
            }
            let rows = self.with_table(&table, |t| t.scan())?;
            let mut c = self.catalog.lock().unwrap();
            c.rebuild_stats(&table, &rows);
            touched = true;
        }
        if touched {
            let c = self.catalog.lock().unwrap();
            c.save(self.catalog_path())?;
        }
        Ok(())
    }

    // 索引维护
    /// 给一列建索引
    fn create_index(&self, table: &str, column: &str) -> std::io::Result<()> {
        if column == "id" {
            return Err(std::io::Error::new(
                std::io::ErrorKind::AlreadyExists,
                "id 是内建索引，不需要再建",
            ));
        }
        {
            let c = self.catalog.lock().unwrap();
            let Some(schema) = c.describe(table) else {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("unknown table: {}", table),
                ));
            };
            if !schema.columns.iter().any(|c| c.name == column) {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("unknown column: {}", column),
                ));
            }
            if schema.indexes.iter().any(|i| i.column == column) {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::AlreadyExists,
                    format!("index on column \"{}\" already exists", column),
                ));
            }
        }

        let path = self.index_path(table, column);
        let (page_size, order, pool) = (self.cfg.page_size, self.cfg.btree_order, self.cfg.pool_size);
        self.with_table(table, |t| {
            t.build_index(column, &path, page_size, order, pool)
        })?;

        let mut c = self.catalog.lock().unwrap();
        c.add_index(table, column)?;
        c.save(self.catalog_path())
    }

    /// 去掉一列的索引（连文件一起删）
    fn drop_index(&self, table: &str, column: &str) -> std::io::Result<()> {
        if column == "id" {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "id 是内建索引，不能删",
            ));
        }
        {
            let c = self.catalog.lock().unwrap();
            if c.describe(table).is_none() {
                return Err(std::io::Error::new(
                    std::io::ErrorKind::NotFound,
                    format!("unknown table: {}", table),
                ));
            }
        }
        self.with_table(table, |t| {
            t.detach_index(column);
            Ok(())
        })?;

        let mut c = self.catalog.lock().unwrap();
        c.remove_index(table, column)?;
        c.save(self.catalog_path())?;
        drop(c);

        let _ = std::fs::remove_file(self.index_path(table, column));
        Ok(())
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

            Request::Insert { table, row } => {
                log_debug!(pipe, "insert table={}", table);
                let tname = table.clone();
                let op = WalOp::Insert { table, row };
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

            Request::InsertBatch { table, rows } => {
                log_debug!(pipe, "insert_batch table={} rows={}", table, rows.len());
                let tname = table.clone();
                let op = WalOp::InsertBatch { table, rows };
                match self.write_via_wal(&op) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(pipe, "insert_batch {}: {}", tname, e);
                        Response::Error {
                            message: format!("insert_batch {}: {}", tname, e),
                        }
                    }
                }
            }

            Request::DeleteKeys { table, keys } => {
                log_debug!(pipe, "delete_keys table={} keys={}", table, keys.len());
                let tname = table.clone();
                let op = WalOp::DeleteKeys { table, keys };
                match self.write_via_wal(&op) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(pipe, "delete_keys {}: {}", tname, e);
                        Response::Error {
                            message: format!("delete_keys {}: {}", tname, e),
                        }
                    }
                }
            }

            Request::LookupByIndex { table, column, key } => {
                log_debug!(pipe, "lookup_by_index table={} column={} key={}", table, column, key);
                let indexed =
                    match self.with_existing_table(&table, |t| Ok(t.has_index(&column))) {
                        Ok(v) => v,
                        Err(e) => {
                            log_warn!(pipe, "lookup_by_index {}: {}", table, e);
                            return Response::Error {
                                message: format!("lookup_by_index {}: {}", table, e),
                            };
                        }
                    };
                if !indexed {
                    // 没有这个索引：告诉查询层"这条我帮不上忙"，让它退回全表扫描。
                    // 回 NoIndex 而不是空结果，是因为"没有索引"和"没查到"必须分开。
                    return Response::NoIndex;
                }
                match self.with_table(&table, |t| t.get_by_column_key(&column, key)) {
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
                        indexes: index_wires(ts),
                        stats: stat_wires(ts),
                    })
                    .collect();
                Response::Catalog { schemas }
            }

            Request::DescribeTable { table } => {
                log_debug!(pipe, "describe_table table={}", table);
                match self.describe_table(&table) {
                    Ok((cols, count, indexes, stats)) => Response::Schema {
                        table,
                        columns: cols,
                        row_count: count,
                        indexes,
                        stats,
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

                let dropped_indexes: Vec<String> = {
                    let mut c = self.catalog.lock().unwrap();
                    let idx = c
                        .describe(&table)
                        .map(|s| s.indexes.iter().map(|i| i.column.clone()).collect())
                        .unwrap_or_default();
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
                    idx
                };

                self.tables.lock().unwrap().remove(&table);
                let _ = std::fs::remove_file(self.table_path(&table));
                let _ = std::fs::remove_file(self.index_path(&table, "id"));
                for col in dropped_indexes {
                    let _ = std::fs::remove_file(self.index_path(&table, &col));
                }

                Response::Ok
            }

            Request::CreateIndex { table, column } => {
                log_debug!(pipe, "create_index table={} column={}", table, column);
                match self.create_index(&table, &column) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(pipe, "create_index {}({}): {}", table, column, e);
                        Response::Error {
                            message: format!("create_index {}({}): {}", table, column, e),
                        }
                    }
                }
            }

            Request::DropIndex { table, column } => {
                log_debug!(pipe, "drop_index table={} column={}", table, column);
                match self.drop_index(&table, &column) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(pipe, "drop_index {}({}): {}", table, column, e);
                        Response::Error {
                            message: format!("drop_index {}({}): {}", table, column, e),
                        }
                    }
                }
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

    /// 查一张表的 schema（列 + 行数 + 索引 + 统计）
    fn describe_table(
        &self,
        table: &str,
    ) -> std::io::Result<(Vec<SchemaColumn>, u64, Vec<IndexWire>, Vec<ColumnStatWire>)> {
        let c = self.catalog.lock().unwrap();
        if let Some(schema) = c.describe(table) {
            return Ok((
                schema.columns.clone(),
                schema.row_count,
                index_wires(schema),
                stat_wires(schema),
            ));
        }
        drop(c);
        if std::fs::metadata(self.table_path(table)).is_ok() {
            return Ok((Vec::new(), 0, Vec::new(), Vec::new()));
        }
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("unknown table: {}", table),
        ))
    }
}

/// 一张表的索引定义 -> 线上格式。
/// `id` 是内建索引（表一开出来就有），数据字典里不记它，但对外要报出来。
fn index_wires(schema: &chusql_storage::catalog::TableSchema) -> Vec<IndexWire> {
    let mut out = vec![IndexWire {
        column: "id".to_string(),
    }];
    for i in &schema.indexes {
        if i.column != "id" {
            out.push(IndexWire {
                column: i.column.clone(),
            });
        }
    }
    out
}

/// 一张表的列统计 -> 线上格式
fn stat_wires(schema: &chusql_storage::catalog::TableSchema) -> Vec<ColumnStatWire> {
    schema
        .stats
        .iter()
        .map(|(name, s)| ColumnStatWire {
            name: name.clone(),
            distinct: s.distinct,
            capped: s.capped,
        })
        .collect()
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

    if let Err(e) = server.rebuild_stats() {
        // 统计重建失败不影响服务：报一声，继续跑（统计会停留在落盘时的值）
        log_warn!(core, "stats rebuild skipped: {}", e);
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
