use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use crate::catalog::Catalog;
use crate::config::{self, Config, Loaded};
use crate::heap::{scalar_int, HeapTable};
use crate::log;
use crate::protocol::{
    blocked_table, reserved_table, Account, ColumnStatWire, IndexWire, Request, Response, Row,
    SchemaColumn, StorageOp, TableSchemaWire, USERS_TABLE,
};
use crate::wal::{committed_ops, records_after, Wal, WalOp};
use crate::{log_debug, log_error, log_info, log_warn};

// 进程内存储运行时：Haskell 侧经 ffi.rs 调进来。
// request_line 的语义等同原来的「一行 JSON 请求 → 一行 JSON 响应」套接字协议。

struct Server {
    cfg: Config,
    tables: Mutex<HashMap<String, Arc<Mutex<HeapTable>>>>,
    write_lock: Mutex<()>,
    gate: RwLock<()>,
    catalog: Mutex<Catalog>,
    wal: Wal,
    recovery_required: AtomicBool,
}

type TableDescription = (Vec<SchemaColumn>, u64, Vec<IndexWire>, Vec<ColumnStatWire>);

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
            gate: RwLock::new(()),
            catalog: Mutex::new(catalog),
            wal,
            recovery_required: AtomicBool::new(false),
        })
    }

    /// 表名 -> 数据文件
    fn table_path(&self, table: &str) -> PathBuf {
        self.cfg.data_dir.join(format!("{}.db", table))
    }

    /// (表名, 列名) 对应哪个索引文件
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

    /// 仅数据字典决定表是否存在
    fn table_exists(&self, table: &str) -> std::io::Result<bool> {
        let catalog = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
        Ok(catalog.describe(table).is_some())
    }

    /// 一张表该开哪些索引
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
        let columns = self.index_columns(table);
        let hidden = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?
            .describe(table).map(|s| s.dropped_columns.iter().cloned().collect()).unwrap_or_default();
        let paths: Vec<(String, PathBuf)> = columns
            .iter()
            .map(|c| (c.clone(), self.index_path(table, c)))
            .collect();

        let mut map = self.tables.lock().unwrap();
        if let Some(h) = map.get(table) {
            return Ok(h.clone());
        }
        std::fs::create_dir_all(&self.cfg.data_dir)?;
        let mut t = HeapTable::open_with_indexes(
            self.table_path(table),
            &paths,
            self.cfg.page_size,
            self.cfg.btree_order,
            self.cfg.pool_size,
        )?;
        t.set_hidden_columns(hidden);
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

    /// 请求带列名时逐个查字典
    fn check_columns(&self, table: &str, columns: Option<&[String]>) -> std::io::Result<()> {
        let Some(cols) = columns else {
            return Ok(());
        };
        let c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
        let schema = c
            .describe(table)
            .ok_or_else(|| std::io::Error::other(format!("unknown table: {table}")))?;
        for col in cols {
            if !schema.columns.iter().any(|c| &c.name == col) {
                return Err(std::io::Error::other(format!("unknown column: {col}")));
            }
        }
        Ok(())
    }

    /// 分片只读扫描：独立句柄读一段连续页，不经表锁
    fn scan_shard(
        &self,
        table: &str,
        columns: Option<&[String]>,
        shard: u32,
        shards: u32,
    ) -> std::io::Result<Vec<Row>> {
        if shards == 0 || shard >= shards {
            return Err(std::io::Error::other("invalid shard range"));
        }
        if !self.table_exists(table)? {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("unknown table: {}", table),
            ));
        }
        let hidden: HashSet<String> = self
            .catalog
            .lock()
            .map_err(|_| std::io::Error::other("catalog lock poisoned"))?
            .describe(table)
            .map(|s| s.dropped_columns.iter().cloned().collect())
            .unwrap_or_default();
        let mut t = HeapTable::open(self.table_path(table), self.cfg.page_size, self.cfg.pool_size)?;
        t.set_hidden_columns(hidden);
        let n = t.num_pages()?;
        let chunk = n.div_ceil(u64::from(shards));
        let from = u64::from(shard).saturating_mul(chunk).min(n);
        let to = u64::from(shard + 1).saturating_mul(chunk).min(n);
        t.scan_columns_pages(columns, from, to)
    }

    /// 表不存在就报错
    fn with_existing_table<R>(
        &self,
        table: &str,
        f: impl FnOnce(&mut HeapTable) -> std::io::Result<R>,
    ) -> std::io::Result<R> {
        if !self.table_exists(table)? {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("unknown table: {}", table),
            ));
        }
        self.with_table(table, f)
    }

    /// 把一条 WAL 操作落到磁盘
    fn apply_op(&self, op: &WalOp) -> std::io::Result<()> {
        match op {
            WalOp::Commit => Ok(()),
            WalOp::HideColumn { table, column } => {
                let mut c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
                c.remove_column(table, column)?;
                c.save(self.catalog_path())?;
                drop(c);
                if let Some(handle) = self.tables.lock().map_err(|_| std::io::Error::other("table lock poisoned"))?.get(table) {
                    handle.lock().map_err(|_| std::io::Error::other("heap lock poisoned"))?.hide_column(column);
                }
                match std::fs::remove_file(self.index_path(table, column)) {
                    Ok(()) => Ok(()),
                    Err(e) if e.kind() == std::io::ErrorKind::NotFound => Ok(()),
                    Err(e) => Err(e),
                }
            }
            WalOp::Compact { table, rows } => {
                self.with_table(table, |t| {
                    t.replace_all(rows)?;
                    t.flush()
                })?;
                let mut c = self.catalog.lock().unwrap();
                c.mark_compacted(table)?;
                c.rebuild_stats(table, rows);
                c.save(self.catalog_path())
            }
            WalOp::Accounts { accounts } => {
                let rows = account_rows(accounts)?;
                self.tables.lock().map_err(|_| std::io::Error::other("table lock poisoned"))?.remove(USERS_TABLE);
                for path in [self.table_path(USERS_TABLE), self.index_path(USERS_TABLE, "id")] {
                    std::fs::File::create(path)?.sync_all()?;
                }
                self.with_table(USERS_TABLE, |t| { t.replace_all(&rows)?; t.flush() })?;
                let mut c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
                declare_system_schema(&mut c, &rows)?;
                c.save(self.catalog_path())
            }
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
                let mut c = self.catalog.lock().unwrap();
                c.rebuild_stats(table, rows);
                c.save(self.catalog_path())
            }
            WalOp::DropColumn { table, column, rows } => {
                self.with_table(table, |t| {
                    t.detach_index(column);
                    t.replace_all(rows)
                })?;
                let mut c = self.catalog.lock().unwrap();
                c.remove_column(table, column)?;
                c.rebuild_stats(table, rows);
                c.save(self.catalog_path())?;
                drop(c);
                let _ = std::fs::remove_file(self.index_path(table, column));
                Ok(())
            }
            WalOp::CreateTable { table, columns } => {
                self.with_table(table, |_t| Ok(()))?;
                let mut c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
                c.create_table(table, columns.clone())?;
                c.save(self.catalog_path())
            }
            WalOp::DropTable { table } => {
                let dropped_indexes: Vec<String> = {
                    let mut c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
                    let idx = c
                        .describe(table)
                        .map(|s| s.indexes.iter().map(|i| i.column.clone()).collect())
                        .unwrap_or_default();
                    if c.describe(table).is_some() {
                        c.drop_table(table)?;
                        c.save(self.catalog_path())?;
                    }
                    idx
                };
                self.tables.lock().map_err(|_| std::io::Error::other("table lock poisoned"))?.remove(table);
                let _ = std::fs::remove_file(self.table_path(table));
                let _ = std::fs::remove_file(self.index_path(table, "id"));
                for col in dropped_indexes {
                    let _ = std::fs::remove_file(self.index_path(table, &col));
                }
                Ok(())
            }
            WalOp::CreateIndex { table, column } => self.apply_create_index(table, column),
            WalOp::DropIndex { table, column } => self.apply_drop_index(table, column),
            WalOp::ReplaceSchema { table, columns, rows } => {
                let old_indexes: Vec<String> = {
                    let c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
                    c.describe(table)
                        .map(|s| s.indexes.iter().map(|i| i.column.clone()).collect())
                        .unwrap_or_default()
                };
                self.with_table(table, |t| {
                    for col in &old_indexes {
                        t.detach_index(col);
                    }
                    t.set_hidden_columns(Default::default());
                    t.replace_all(rows)
                })?;
                let mut c = self.catalog.lock().unwrap();
                c.set_columns(table, columns.clone())?;
                c.rebuild_stats(table, rows);
                c.save(self.catalog_path())?;
                drop(c);
                for col in old_indexes {
                    let _ = std::fs::remove_file(self.index_path(table, &col));
                }
                Ok(())
            }
        }
    }

    /// 先写 WAL 再改数据
    fn write_via_wal(&self, op: &WalOp) -> std::io::Result<()> {
        let _guard = self.write_lock.lock().unwrap();
        if self.recovery_required.load(Ordering::Acquire) {
            return Err(std::io::Error::other("storage recovery required"));
        }
        self.validate_op(op)?;
        self.group_via_wal(std::slice::from_ref(op))
    }

    /// 校验一条操作：命中被删的列就拒绝
    fn validate_op(&self, op: &WalOp) -> std::io::Result<()> {
        let c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
        let validate = |table: &str, rows: &[Row]| -> std::io::Result<()> {
            let Some(schema) = c.describe(table) else { return Ok(()); };
            if let Some(name) = rows.iter().flat_map(|r| r.keys()).find(|k| schema.dropped_columns.contains(*k)) {
                return Err(std::io::Error::other(format!("unknown column: {name}")));
            }
            Ok(())
        };
        match op {
            WalOp::Insert { table, row } => validate(table, std::slice::from_ref(row))?,
            WalOp::InsertBatch { table, rows } | WalOp::ReplaceAll { table, rows } => validate(table, rows)?,
            WalOp::HideColumn { table, column }
                if !c.describe(table).is_some_and(|s| s.columns.iter().any(|col| &col.name == column)) =>
            {
                return Err(std::io::Error::other(format!("unknown column: {column}")));
            }
            _ => {}
        }
        Ok(())
    }

    /// 事务提交：一组写操作加一个提交标记，一次落盘
    ///
    /// 组里每条都过 `validate_op`，跟单条写一致；调用方 `dispatch` 已经持有
    /// `gate.write`，因此整批对外原子可见。崩在提交标记之前，这组记录在恢复
    /// 时整组丢弃；崩在标记之后，逐条按「是否已生效」补写。
    fn apply_transaction(&self, ops: &[StorageOp]) -> std::io::Result<()> {
        let _guard = self.write_lock.lock().unwrap();
        if self.recovery_required.load(Ordering::Acquire) {
            return Err(std::io::Error::other("storage recovery required"));
        }
        let mut wal_ops = Vec::with_capacity(ops.len());
        for op in ops {
            let table = match op {
                StorageOp::Upsert { table, .. }
                | StorageOp::Delete { table, .. }
                | StorageOp::Replace { table, .. } => table,
            };
            if blocked_table(table) {
                return Err(std::io::Error::other("reserved system table"));
            }
            if table.is_empty() || !table.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
                return Err(std::io::Error::other("invalid table name"));
            }
            let wal_op = match op {
                StorageOp::Upsert { table, rows } => WalOp::InsertBatch {
                    table: table.clone(),
                    rows: rows.clone(),
                },
                StorageOp::Delete { table, ids } => WalOp::DeleteKeys {
                    table: table.clone(),
                    keys: ids.clone(),
                },
                StorageOp::Replace { table, rows } => WalOp::ReplaceAll {
                    table: table.clone(),
                    rows: rows.clone(),
                },
            };
            self.validate_op(&wal_op)?;
            wal_ops.push(wal_op);
        }
        self.group_via_wal(&wal_ops)
    }

    /// 一组操作加提交标记落盘，成功后把检查点推到这条提交
    ///
    /// 检查点写在这组数据和字典都刷盘之后，含义是「LSN 到此为止的改动都已落盘」；
    /// 写完再按这个边界丢掉日志前缀，中途崩掉只会留下更长的日志，不会少数据。
    fn group_via_wal(&self, ops: &[WalOp]) -> std::io::Result<()> {
        std::fs::create_dir_all(&self.cfg.data_dir)?;
        let wal = &self.wal;
        let commit_lsn = wal.append_group(ops)?;
        for op in ops {
            let result = self.apply_op(op);
            if result.is_err() && matches!(op, WalOp::HideColumn { .. }) {
                self.recovery_required.store(true, Ordering::Release);
                return result;
            }
            result?;
        }
        for op in ops {
            self.flush_op_tables(op)?;
        }
        self.flush_all_tables()?;
        wal.write_checkpoint(commit_lsn)?;
        wal.truncate_before(commit_lsn)?;
        log_debug!(core, "WAL committed group at LSN {}", commit_lsn);
        Ok(())
    }

    /// 把打开的表都 fsync 一遍
    fn flush_all_tables(&self) -> std::io::Result<()> {
        let handles: Vec<Arc<Mutex<HeapTable>>> = self
            .tables
            .lock()
            .map_err(|_| std::io::Error::other("table lock poisoned"))?
            .values()
            .cloned()
            .collect();
        for handle in handles {
            handle.lock().map_err(|_| std::io::Error::other("heap lock poisoned"))?.flush()?;
        }
        Ok(())
    }

    /// 把涉及的表写回磁盘
    fn flush_op_tables(&self, op: &WalOp) -> std::io::Result<()> {
        let table = match op {
            WalOp::HideColumn { .. } => return Ok(()),
            WalOp::Commit => return Ok(()),
            // 删表后不需要写回
            WalOp::DropTable { .. } => return Ok(()),
            WalOp::Accounts { .. } => USERS_TABLE,
            WalOp::Insert { table, .. }
            | WalOp::InsertBatch { table, .. }
            | WalOp::DeleteKeys { table, .. }
            | WalOp::ReplaceAll { table, .. }
            | WalOp::DropColumn { table, .. }
            | WalOp::Compact { table, .. }
            | WalOp::CreateTable { table, .. }
            | WalOp::CreateIndex { table, .. }
            | WalOp::DropIndex { table, .. }
            | WalOp::ReplaceSchema { table, .. } => table,
        };
        self.with_table(table, |t| t.flush())
    }

    /// 重放残留 WAL：检查点之后的已提交组重放，未提交组丢弃
    fn recover_wal(&self) -> std::io::Result<()> {
        std::fs::create_dir_all(&self.cfg.data_dir)?;
        let wal = &self.wal;
        let scan = wal.read_all()?;
        if scan.records.is_empty() {
            if scan.good_len > 0 {
                wal.truncate_to(scan.good_len)?;
            }
            return Ok(());
        }
        // 检查点以内的记录已经落盘，直接按边界截掉
        let checkpoint = wal.read_checkpoint()?.unwrap_or(0);
        let pending = records_after(&scan.records, checkpoint);
        let records = committed_ops(&pending);
        if records.is_empty() {
            log_warn!(core, "discarding uncommitted WAL records");
            // 边界之后的都是没提交的尾巴，直接丢
            wal.truncate_after(checkpoint)?;
            return Ok(());
        }
        let max_lsn = pending
            .iter()
            .rev()
            .find(|(_, op)| *op == WalOp::Commit)
            .map(|(lsn, _)| *lsn)
            .or_else(|| records.last().map(|(lsn, _)| *lsn))
            .unwrap_or(0);
        log_warn!(core, "replaying {} committed WAL records", records.len());
        for (_, op) in &records {
            self.replay_op(op)?;
            self.flush_op_tables(op)?;
        }
        self.flush_all_tables()?;
        wal.write_checkpoint(max_lsn)?;
        wal.truncate_before(max_lsn)?;
        log_info!(core, "WAL replay complete");
        Ok(())
    }

    /// 重放一条操作：数据已经生效的不再重复写
    fn replay_op(&self, op: &WalOp) -> std::io::Result<()> {
        if self.op_already_applied(op)? {
            log_debug!(core, "WAL record already applied, skipping");
            return Ok(());
        }
        self.apply_op(op)
    }

    /// 这条操作对应的数据是不是已经落盘了
    fn op_already_applied(&self, op: &WalOp) -> std::io::Result<bool> {
        match op {
            WalOp::Insert { table, row } => self.applied_rows(table, std::slice::from_ref(row)),
            WalOp::InsertBatch { table, rows } => self.applied_rows(table, rows),
            WalOp::ReplaceAll { table, rows } => {
                if self.catalog.lock().unwrap().describe(table).is_none() {
                    return Ok(false);
                }
                let stored = self.with_table(table, |t| t.scan())?;
                Ok(stored == *rows)
            }
            WalOp::DeleteKeys { table, keys } => {
                if keys.is_empty() {
                    return Ok(true);
                }
                if self.catalog.lock().unwrap().describe(table).is_none() {
                    return Ok(false);
                }
                let rows = self.with_table(table, |t| t.scan())?;
                let present = rows.iter().any(|row| {
                    row.get("id")
                        .and_then(scalar_int)
                        .is_some_and(|id| keys.contains(&id))
                });
                Ok(!present)
            }
            WalOp::HideColumn { table, column } => {
                let c = self.catalog.lock().unwrap();
                Ok(c.describe(table).is_some_and(|s| !s.columns.iter().any(|col| &col.name == column)))
            }
            WalOp::CreateTable { table, .. } => self.table_exists(table),
            WalOp::DropTable { table } => Ok(!self.table_exists(table)?
                && !self.table_path(table).exists()
                && !self.index_path(table, "id").exists()),
            WalOp::CreateIndex { table, column } => Ok(self.index_exists(table, column)),
            WalOp::DropIndex { table, column } => {
                Ok(!self.index_exists(table, column) && !self.index_path(table, column).exists())
            }
            _ => Ok(false),
        }
    }

    /// 这个列有索引吗
    fn index_exists(&self, table: &str, column: &str) -> bool {
        self.catalog
            .lock()
            .unwrap()
            .describe(table)
            .is_some_and(|s| s.indexes.iter().any(|i| i.column == column))
    }

    /// 这些行是不是都已经在表里
    fn applied_rows(&self, table: &str, rows: &[Row]) -> std::io::Result<bool> {
        if rows.is_empty() {
            return Ok(true);
        }
        if self.catalog.lock().unwrap().describe(table).is_none() {
            return Ok(false);
        }
        let stored = self.with_table(table, |t| t.scan())?;
        Ok(rows.iter().all(|row| stored.iter().any(|one| one == row)))
    }

    /// 启动时重建列统计
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

    /// 老数据迁移：类型名规范、缺列补 null
    fn migrate_data(&self) -> std::io::Result<()> {
        self.migrate_table_names()?;
        let names: Vec<String> = self
            .catalog
            .lock()
            .map_err(|_| std::io::Error::other("catalog lock poisoned"))?
            .table_names();
        for table in names {
            if !self.table_path(&table).exists() {
                continue;
            }
            let columns: Vec<SchemaColumn> = match self
                .catalog
                .lock()
                .map_err(|_| std::io::Error::other("catalog lock poisoned"))?
                .describe(&table)
            {
                Some(schema) => schema.columns.clone(),
                None => continue,
            };
            let rows = self.with_existing_table(&table, |t| t.scan())?;
            if rows.iter().all(|row| columns.iter().all(|c| row.contains_key(&c.name))) {
                continue;
            }
            let filled: Vec<Row> = rows
                .into_iter()
                .map(|mut row| {
                    for column in &columns {
                        row.entry(column.name.clone()).or_insert(serde_json::Value::Null);
                    }
                    row
                })
                .collect();
            log_info!(core, "migrating {}: {} rows", table, filled.len());
            let op = WalOp::ReplaceAll { table, rows: filled };
            self.write_via_wal(&op)?;
        }
        let mut catalog = self
            .catalog
            .lock()
            .map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
        if catalog.normalize_types() {
            catalog.save(self.catalog_path())?;
        }
        Ok(())
    }

    /// 老目录里的角色与授权表改到内部前缀
    fn migrate_table_names(&self) -> std::io::Result<()> {
        for (from, to) in LEGACY_TABLE_RENAMES {
            let indexes = {
                let c = self
                    .catalog
                    .lock()
                    .map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
                match c.describe(from) {
                    Some(schema) if c.describe(to).is_none() => {
                        schema.indexes.iter().map(|i| i.column.clone()).collect::<Vec<String>>()
                    }
                    _ => continue,
                }
            };
            self.rename_table_files(from, to, &indexes);
            {
                let mut c = self
                    .catalog
                    .lock()
                    .map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
                if !c.rename_table(from, to)? {
                    continue;
                }
                c.save(self.catalog_path())?;
            }
            if let Ok(mut tables) = self.tables.lock() {
                tables.remove(from);
            }
            log_info!(core, "renamed system table {} to {}", from, to);
        }
        Ok(())
    }

    /// 把一张表的数据文件与索引文件改名
    fn rename_table_files(&self, from: &str, to: &str, indexes: &[String]) {
        if !self.table_path(from).exists() {
            return;
        }
        let _ = std::fs::rename(self.table_path(from), self.table_path(to));
        for column in std::iter::once("id").chain(indexes.iter().map(String::as_str)) {
            let _ = std::fs::rename(self.index_path(from, column), self.index_path(to, column));
        }
    }

    /// 建索引前的检查：内置列、未知表列、重复索引
    fn check_create_index(&self, table: &str, column: &str) -> std::io::Result<()> {
        if column == "id" {
            return Err(std::io::Error::new(
                std::io::ErrorKind::AlreadyExists,
                "id is the built-in index and cannot be created twice",
            ));
        }
        let c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
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
        Ok(())
    }

    /// 建索引文件并登记到 catalog
    fn apply_create_index(&self, table: &str, column: &str) -> std::io::Result<()> {
        let path = self.index_path(table, column);
        let (page_size, order, pool) = (self.cfg.page_size, self.cfg.btree_order, self.cfg.pool_size);
        self.with_table(table, |t| {
            t.build_index(column, &path, page_size, order, pool)?;
            t.flush()
        })?;

        let mut c = self.catalog.lock().unwrap();
        c.add_index(table, column)?;
        c.save(self.catalog_path())
    }

    /// 去索引前的检查：内置列、未知表、索引不存在
    fn check_drop_index(&self, table: &str, column: &str) -> std::io::Result<()> {
        if column == "id" {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "id is the built-in index and cannot be dropped",
            ));
        }
        let c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
        let Some(schema) = c.describe(table) else {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("unknown table: {}", table),
            ));
        };
        if !schema.indexes.iter().any(|i| i.column == column) {
            return Err(std::io::Error::new(
                std::io::ErrorKind::NotFound,
                format!("no index on column \"{}\"", column),
            ));
        }
        Ok(())
    }

    /// 摘掉索引定义并删文件；重放时容忍 catalog 里已没有它
    fn apply_drop_index(&self, table: &str, column: &str) -> std::io::Result<()> {
        if self.table_exists(table)? {
            self.with_table(table, |t| {
                t.detach_index(column);
                Ok(())
            })?;
            let mut c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
            if c.describe(table).is_some_and(|s| s.indexes.iter().any(|i| i.column == column)) {
                c.remove_index(table, column)?;
                c.save(self.catalog_path())?;
            }
        }
        let _ = std::fs::remove_file(self.index_path(table, column));
        Ok(())
    }

    /// 删除列定义并隐藏旧字段，不改写堆页。
    fn drop_column(&self, table: &str, column: &str) -> std::io::Result<()> {
        if column == "id" {
            return Err(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "the built-in id column cannot be dropped",
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
        }

        let op = WalOp::HideColumn {
            table: table.to_string(),
            column: column.to_string(),
        };
        self.write_via_wal(&op)
    }

    /// 整理一张表：按当前列定义重写堆页，回收隐藏列占的空间
    fn compact_table(&self, table: &str) -> std::io::Result<()> {
        let rows = self.with_existing_table(table, |t| t.scan())?;
        let op = WalOp::Compact {
            table: table.to_string(),
            rows,
        };
        self.write_via_wal(&op)
    }

    /// 启动时整理：把还留着隐藏列的表逐张重写
    fn compact_tables(&self) -> std::io::Result<()> {
        let tables = {
            let c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
            c.dropped_tables()
        };
        for table in tables {
            log_info!(core, "compacting table {}", table);
            self.compact_table(&table)?;
        }
        Ok(())
    }

    /// 账号表是否已登记为系统表
    fn account_table_ready(&self) -> std::io::Result<bool> {
        use std::io::{Error, ErrorKind};
        let registered = {
            let c = self.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?;
            c.describe(USERS_TABLE).map(|schema| schema.system)
        };
        match registered {
            Some(true) => Ok(true),
            Some(false) => Err(Error::new(ErrorKind::AlreadyExists, "reserved account table collision")),
            None => {
                if self.table_path(USERS_TABLE).exists() || self.index_path(USERS_TABLE, "id").exists() {
                    Err(Error::new(ErrorKind::AlreadyExists, "reserved account file collision"))
                } else {
                    Ok(false)
                }
            }
        }
    }

    /// 系统目录里账号表已登记
    fn system_initialized(&self) -> bool {
        self.catalog
            .lock()
            .map(|c| c.describe(USERS_TABLE).is_some_and(|schema| schema.system))
            .unwrap_or(false)
    }

    /// 最近一次落盘的检查点 LSN
    fn last_checkpoint(&self) -> u64 {
        self.wal.read_checkpoint().ok().flatten().unwrap_or(0)
    }

    /// 读账号表；表没登记或数据文件缺失都算错
    fn load_accounts(&self) -> std::io::Result<Vec<Account>> {
        use std::io::Error;
        if !self.table_path(USERS_TABLE).exists() {
            return Err(Error::other("missing account data"));
        }
        let mut accounts: Vec<Account> = self
            .with_existing_table(USERS_TABLE, |t| t.scan())?
            .iter()
            .map(decode_account_row)
            .collect::<std::io::Result<_>>()?;
        for account in accounts.iter_mut() {
            normalize_account(account);
        }
        Ok(accounts)
    }

    /// 引导系统目录：建账号表，空表才补管理员
    fn bootstrap_system(&self, user: Option<&str>, password_hash: Option<&str>) -> std::io::Result<Vec<Account>> {
        use std::io::Error;
        let _guard = self.write_lock.lock().map_err(|_| Error::other("write lock poisoned"))?;
        if self.recovery_required.load(Ordering::Acquire) {
            return Err(Error::other("storage recovery required"));
        }
        let initialized = self.account_table_ready()?;
        let mut accounts = if initialized { self.load_accounts()? } else { Vec::new() };
        let mut changed = !initialized;
        if let Some(user) = user {
            let name = account_name(user)?;
            // 表里已经有行就不再补人：引导程序只在空表上播种，
            // 免得有人拿 storage 透传绕过账号接口往里塞一个已知口令的账号
            if accounts.is_empty() && !accounts.iter().any(|a| a.user == name) {
                let id = accounts.iter().map(|a| a.id).max().unwrap_or(0)
                    .checked_add(1).ok_or_else(|| Error::other("account id exhausted"))?;
                accounts.push(new_account(id, &name, password_hash.unwrap_or_default().to_string())?);
                changed = true;
            }
        }
        if !changed {
            return Ok(accounts);
        }
        for row in account_rows(&accounts)? {
            HeapTable::encode_row(&row, self.cfg.page_size)?;
        }
        let op = WalOp::Accounts { accounts: accounts.clone() };
        self.recovery_required.store(true, Ordering::Release);
        self.wal.truncate()?;
        self.wal.append(&op)?;
        self.apply_op(&op)?;
        self.wal.clear()?;
        self.recovery_required.store(false, Ordering::Release);
        Ok(accounts)
    }

    /// 账号读写共享串行持久化边界
    fn accounts_request(&self, request: Request) -> std::io::Result<Vec<Account>> {
        use std::io::{Error, ErrorKind};
        let _guard = self.write_lock.lock().map_err(|_| Error::other("write lock poisoned"))?;
        if self.recovery_required.load(Ordering::Acquire) {
            return Err(Error::other("storage recovery required"));
        }
        if !self.account_table_ready()? {
            return Err(Error::other("system catalog is not initialized; run csql-bootstrap"));
        }
        let mut accounts = self.load_accounts()?;
        match request {
            Request::AccountsList => return Ok(accounts),
            Request::AccountCreate { user, password_hash } => {
                let name = account_name(&user)?;
                if accounts.iter().any(|a| a.user == name) {
                    return Err(Error::new(ErrorKind::AlreadyExists, "account already exists"));
                }
                let id = accounts.iter().map(|a| a.id).max().unwrap_or(0)
                    .checked_add(1).ok_or_else(|| Error::other("account id exhausted"))?;
                accounts.push(new_account(id, &name, password_hash)?);
            }
            Request::AccountReset { user, password_hash } => {
                let name = account_name(&user)?;
                validate_account_hash(&password_hash)?;
                let account = accounts.iter_mut().find(|a| a.user == name)
                    .ok_or_else(|| Error::new(ErrorKind::NotFound, "unknown account"))?;
                account.revision = account.revision.checked_add(1).ok_or_else(|| Error::other("account revision exhausted"))?;
                account.password_hash = password_hash;
            }
            Request::AccountLogin { user, at } => {
                let name = account_name(&user)?;
                let stamp = at.unwrap_or_else(now_stamp);
                let account = accounts.iter_mut().find(|a| a.user == name)
                    .ok_or_else(|| Error::new(ErrorKind::NotFound, "unknown account"))?;
                account.last_login_at = Some(stamp);
            }
            Request::AccountDrop { user } => {
                let name = account_name(&user)?;
                let before = accounts.len();
                accounts.retain(|a| a.user != name);
                if accounts.len() == before {
                    return Err(Error::new(ErrorKind::NotFound, "unknown account"));
                }
            }
            _ => return Err(Error::other("unexpected account request")),
        }
        for row in account_rows(&accounts)? {
            HeapTable::encode_row(&row, self.cfg.page_size)?;
        }
        let op = WalOp::Accounts { accounts: accounts.clone() };
        self.recovery_required.store(true, Ordering::Release);
        self.wal.truncate()?;
        self.wal.append(&op)?;
        self.apply_op(&op)?;
        self.wal.clear()?;
        self.recovery_required.store(false, Ordering::Release);
        Ok(accounts)
    }

    /// 一条请求翻成一条响应：先过读/写门再分发
    fn handle_request(&self, req: Request) -> Response {
        if concurrent_read(&req) {
            match self.gate.read() {
                Ok(_read) => self.dispatch(req),
                Err(_) => Response::Error {
                    message: "storage read gate poisoned".into(),
                },
            }
        } else {
            match self.gate.write() {
                Ok(_write) => self.dispatch(req),
                Err(_) => Response::Error {
                    message: "storage write gate poisoned".into(),
                },
            }
        }
    }

    /// 持门后分发一条请求
    fn dispatch(&self, req: Request) -> Response {
        if self.recovery_required.load(Ordering::Acquire) && !matches!(req, Request::Ping) {
            return Response::Error { message: "storage recovery required".into() };
        }
        if !req.valid_columns() {
            return Response::Error { message: "invalid column name".into() };
        }
        if let Some(table) = req.table() {
            if blocked_table(table) {
                return Response::Error { message: "reserved system table".into() };
            }
            if table.is_empty() || !table.chars().all(|c| c.is_ascii_alphanumeric() || c == '_') {
                return Response::Error { message: "invalid table name".into() };
            }
        }
        match req {
            request @ (Request::AccountsList | Request::AccountCreate { .. }
                | Request::AccountReset { .. } | Request::AccountLogin { .. }
                | Request::AccountDrop { .. }) => {
                match self.accounts_request(request) {
                    Ok(accounts) => Response::Accounts { accounts },
                    Err(e) => Response::Error { message: e.to_string() },
                }
            }
            Request::BootstrapSystem { user, password_hash } => {
                match self.bootstrap_system(user.as_deref(), password_hash.as_deref()) {
                    Ok(_) => Response::System { initialized: true, last_lsn: self.last_checkpoint() },
                    Err(e) => Response::Error { message: e.to_string() },
                }
            }
            Request::SystemStatus => Response::System { initialized: self.system_initialized(), last_lsn: self.last_checkpoint() },
            Request::Ping => {
                log_debug!(request, "ping");
                Response::Pong
            }

            Request::ApplyTransaction { ops } => {
                log_debug!(request, "apply_transaction ops={}", ops.len());
                match self.apply_transaction(&ops) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(request, "apply_transaction: {}", e);
                        Response::Error {
                            message: format!("apply_transaction: {e}"),
                        }
                    }
                }
            }

            Request::Scan { table, columns } => {
                log_debug!(request, "scan table={}", table);

                let result = (|| {
                    self.check_columns(&table, columns.as_deref())?;
                    self.with_existing_table(&table, |t| t.scan_columns(columns.as_deref()))
                })();

                match result {
                    Ok(rows) => Response::Rows { rows },
                    Err(e) => {
                        log_warn!(request, "scan {}: {}", table, e);
                        Response::Error {
                            message: format!("scan {}: {}", table, e),
                        }
                    }
                }
            }

            Request::ScanShard {
                table,
                columns,
                shard,
                shards,
            } => {
                log_debug!(request, "scan_shard table={} shard={}/{}", table, shard, shards);

                let result = (|| {
                    self.check_columns(&table, columns.as_deref())?;
                    self.scan_shard(&table, columns.as_deref(), shard, shards)
                })();

                match result {
                    Ok(rows) => Response::Rows { rows },
                    Err(e) => {
                        log_warn!(request, "scan {} shard {}/{}: {}", table, shard, shards, e);
                        Response::Error {
                            message: format!("scan {}: {}", table, e),
                        }
                    }
                }
            }

            Request::Insert { table, row } => {
                log_debug!(request, "insert table={}", table);
                let tname = table.clone();
                let op = WalOp::Insert { table, row };
                match self.write_via_wal(&op) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(request, "insert {}: {}", tname, e);
                        Response::Error {
                            message: format!("insert {}: {}", tname, e),
                        }
                    }
                }
            }

            Request::InsertBatch { table, rows } => {
                log_debug!(request, "insert_batch table={} rows={}", table, rows.len());
                let tname = table.clone();
                let op = WalOp::InsertBatch { table, rows };
                match self.write_via_wal(&op) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(request, "insert_batch {}: {}", tname, e);
                        Response::Error {
                            message: format!("insert_batch {}: {}", tname, e),
                        }
                    }
                }
            }

            Request::DeleteKeys { table, keys } => {
                log_debug!(request, "delete_keys table={} keys={}", table, keys.len());
                let tname = table.clone();
                let op = WalOp::DeleteKeys { table, keys };
                match self.write_via_wal(&op) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(request, "delete_keys {}: {}", tname, e);
                        Response::Error {
                            message: format!("delete_keys {}: {}", tname, e),
                        }
                    }
                }
            }

            Request::LookupByIndex { table, column, key } => {
                log_debug!(request, "lookup_by_index table={} column={} key={}", table, column, key);
                let indexed =
                    match self.with_existing_table(&table, |t| Ok(t.has_index(&column))) {
                        Ok(v) => v,
                        Err(e) => {
                            log_warn!(request, "lookup_by_index {}: {}", table, e);
                            return Response::Error {
                                message: format!("lookup_by_index {}: {}", table, e),
                            };
                        }
                    };
                if !indexed {
                    return Response::NoIndex;
                }
                let found = match scalar_int(&key) {
                    Some(k) => self.with_table(&table, |t| t.get_all_by_column_key(&column, k)),
                    None => match key.as_str() {
                        Some(s) => self.with_table(&table, |t| t.get_all_by_string_key(&column, s)),
                        None => {
                            return Response::Error {
                                message: "lookup_by_index: key must be a number or a string".into(),
                            };
                        }
                    },
                };
                match found {
                    Ok(rows) => Response::Rows { rows },
                    Err(e) => {
                        log_warn!(request, "lookup_by_index {}: {}", table, e);
                        Response::Error {
                            message: format!("lookup_by_index {}: {}", table, e),
                        }
                    }
                }
            }

            Request::RangeByIndex { table, column, lo, lo_inclusive, hi, hi_inclusive } => {
                log_debug!(request, "range_by_index table={} column={}", table, column);
                let indexed =
                    match self.with_existing_table(&table, |t| Ok(t.has_index(&column))) {
                        Ok(v) => v,
                        Err(e) => {
                            log_warn!(request, "range_by_index {}: {}", table, e);
                            return Response::Error {
                                message: format!("range_by_index {}: {}", table, e),
                            };
                        }
                    };
                if !indexed {
                    return Response::NoIndex;
                }
                let lo = lo.as_ref().map(|v| (v, lo_inclusive));
                let hi = hi.as_ref().map(|v| (v, hi_inclusive));
                match self.with_table(&table, |t| t.scan_range(&column, lo, hi)) {
                    Ok(Some(rows)) => Response::Rows { rows },
                    Ok(None) => Response::NoIndex,
                    Err(e) => {
                        log_warn!(request, "range_by_index {}: {}", table, e);
                        Response::Error {
                            message: format!("range_by_index {}: {}", table, e),
                        }
                    }
                }
            }

            Request::ReplaceAll { table, rows } => {
                log_debug!(request, "replace_all table={} rows={}", table, rows.len());
                let tname = table.clone();
                let op = WalOp::ReplaceAll { table, rows };
                match self.write_via_wal(&op) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(request, "replace_all {}: {}", tname, e);
                        Response::Error {
                            message: format!("replace_all {}: {}", tname, e),
                        }
                    }
                }
            }

            Request::ListTables => {
                log_debug!(request, "list_tables");
                match self.list_tables() {
                    Ok(tables) => Response::Tables { tables },
                    Err(e) => {
                        log_warn!(request, "list_tables: {}", e);
                        Response::Error {
                            message: format!("list_tables: {}", e),
                        }
                    }
                }
            }

            Request::ListCatalog => {
                log_debug!(request, "list_catalog");
                let c = self.catalog.lock().unwrap();
                let schemas: Vec<TableSchemaWire> = c
                    .all_tables()
                    .into_iter()
                    .filter(|(name, _)| !blocked_table(name))
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
                log_debug!(request, "describe_table table={}", table);
                match self.describe_table(&table) {
                    Ok((cols, count, indexes, stats)) => Response::Schema {
                        table,
                        columns: cols,
                        row_count: count,
                        indexes,
                        stats,
                    },
                    Err(e) => {
                        log_warn!(request, "describe_table {}: {}", table, e);
                        Response::Error {
                            message: format!("describe_table {}: {}", table, e),
                        }
                    }
                }
            }

            Request::CreateTable { table, columns } => {
                log_debug!(request, "create_table table={} cols={}", table, columns.len());
                let exists = match self.table_exists(&table) {
                    Ok(exists) => exists,
                    Err(e) => {
                        return Response::Error {
                            message: format!("create_table {}: {}", table, e),
                        }
                    }
                };
                if exists {
                    return Response::Error {
                        message: format!("create_table {}: table already exists", table),
                    };
                }
                let tname = table.clone();
                let op = WalOp::CreateTable { table, columns };
                match self.write_via_wal(&op) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(request, "create_table {}: {}", tname, e);
                        Response::Error {
                            message: format!("create_table {}: {}", tname, e),
                        }
                    }
                }
            }

            Request::DropTable { table } => {
                log_debug!(request, "drop_table table={}", table);
                let exists = match self.table_exists(&table) {
                    Ok(exists) => exists,
                    Err(e) => {
                        return Response::Error {
                            message: format!("drop_table {}: {}", table, e),
                        }
                    }
                };
                if !exists {
                    return Response::Error {
                        message: format!("drop_table {}: unknown table: {}", table, table),
                    };
                }
                let tname = table.clone();
                let op = WalOp::DropTable { table };
                match self.write_via_wal(&op) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(request, "drop_table {}: {}", tname, e);
                        Response::Error {
                            message: format!("drop_table {}: {}", tname, e),
                        }
                    }
                }
            }

            Request::CreateIndex { table, column } => {
                log_debug!(request, "create_index table={} column={}", table, column);
                if let Err(e) = self.check_create_index(&table, &column) {
                    log_warn!(request, "create_index {}({}): {}", table, column, e);
                    return Response::Error {
                        message: format!("create_index {}({}): {}", table, column, e),
                    };
                }
                let tname = table.clone();
                let op = WalOp::CreateIndex { table, column };
                match self.write_via_wal(&op) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(request, "create_index {}: {}", tname, e);
                        Response::Error {
                            message: format!("create_index {}: {}", tname, e),
                        }
                    }
                }
            }

            Request::DropIndex { table, column } => {
                log_debug!(request, "drop_index table={} column={}", table, column);
                if let Err(e) = self.check_drop_index(&table, &column) {
                    log_warn!(request, "drop_index {}({}): {}", table, column, e);
                    return Response::Error {
                        message: format!("drop_index {}({}): {}", table, column, e),
                    };
                }
                let tname = table.clone();
                let op = WalOp::DropIndex { table, column };
                match self.write_via_wal(&op) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(request, "drop_index {}: {}", tname, e);
                        Response::Error {
                            message: format!("drop_index {}: {}", tname, e),
                        }
                    }
                }
            }

            Request::DropColumn { table, column } => {
                log_debug!(request, "drop_column table={} column={}", table, column);
                match self.drop_column(&table, &column) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(request, "drop_column {}({}): {}", table, column, e);
                        Response::Error {
                            message: format!("drop_column {}({}): {}", table, column, e),
                        }
                    }
                }
            }

            Request::Compact { table } => {
                log_debug!(request, "compact table={}", table);
                match self.compact_table(&table) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(request, "compact {}: {}", table, e);
                        Response::Error {
                            message: format!("compact {}: {}", table, e),
                        }
                    }
                }
            }

            Request::ReplaceSchema { table, columns, rows } => {
                log_debug!(request, "replace_schema table={} cols={}", table, columns.len());
                let tname = table.clone();
                let op = WalOp::ReplaceSchema { table, columns, rows };
                match self.write_via_wal(&op) {
                    Ok(()) => Response::Ok,
                    Err(e) => {
                        log_warn!(request, "replace_schema {}: {}", tname, e);
                        Response::Error {
                            message: format!("replace_schema {}: {}", tname, e),
                        }
                    }
                }
            }
        }
    }

    /// 列出业务表与授权服务用的三张系统表
    fn list_tables(&self) -> std::io::Result<Vec<String>> {
        let c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
        Ok(c.table_names().into_iter().filter(|t| !blocked_table(t)).collect())
    }

    /// 查一张表的 schema
    fn describe_table(
        &self,
        table: &str,
    ) -> std::io::Result<TableDescription> {
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
        Err(std::io::Error::new(
            std::io::ErrorKind::NotFound,
            format!("unknown table: {}", table),
        ))
    }
}

/// 账号列表转成行
fn account_rows(accounts: &[Account]) -> std::io::Result<Vec<Row>> {
    accounts.iter().map(|a| Ok(HashMap::from([
        ("id".into(), serde_json::json!(a.id)),
        ("user".into(), serde_json::Value::String(a.user.clone())),
        ("password_hash".into(), serde_json::Value::String(a.password_hash.clone())),
        ("registered_at".into(), serde_json::Value::String(a.registered_at.clone())),
        ("last_login_at".into(), match &a.last_login_at {
            Some(stamp) => serde_json::Value::String(stamp.clone()),
            None => serde_json::Value::Null,
        }),
        ("revision".into(), serde_json::json!(a.revision)),
    ]))).collect()
}

/// 系统库的账号表结构：引导模块按这份定义建表
fn users_columns() -> Vec<SchemaColumn> {
    vec![
        SchemaColumn { name: "id".into(), ty: "int".into(), nullable: false,
            auto_increment: true, primary_key: true, ..Default::default() },
        SchemaColumn { name: "user".into(), ty: "varchar(64)".into(), nullable: false,
            unique: true, ..Default::default() },
        SchemaColumn { name: "password_hash".into(), ty: "varchar(256)".into(),
            nullable: false, ..Default::default() },
        SchemaColumn { name: "registered_at".into(), ty: "timestamp".into(),
            nullable: false, ..Default::default() },
        SchemaColumn { name: "last_login_at".into(), ty: "timestamp".into(),
            ..Default::default() },
        SchemaColumn { name: "revision".into(), ty: "int".into(), nullable: false,
            ..Default::default() },
    ]
}

/// 把系统表写进字典
fn declare_system_schema(catalog: &mut Catalog, rows: &[Row]) -> std::io::Result<()> {
    catalog.rebuild_stats(USERS_TABLE, rows);
    catalog.set_columns(USERS_TABLE, users_columns())?;
    catalog.rebuild_stats(USERS_TABLE, rows);
    catalog.mark_system(USERS_TABLE)?;
    catalog.add_index(USERS_TABLE, "user")?;
    Ok(())
}

/// 读一行账号（兼容老格式）
fn decode_account_row(row: &Row) -> std::io::Result<Account> {
    if let Some(encoded) = row.get("account").and_then(serde_json::Value::as_str) {
        return serde_json::from_str(encoded).map_err(|_| std::io::Error::other("invalid account record"));
    }
    let id = row.get("id").and_then(serde_json::Value::as_i64)
        .ok_or_else(|| std::io::Error::other("invalid account record"))?;
    let user = row.get("user").and_then(serde_json::Value::as_str)
        .ok_or_else(|| std::io::Error::other("invalid account record"))?;
    let password_hash = row.get("password_hash").and_then(serde_json::Value::as_str)
        .ok_or_else(|| std::io::Error::other("invalid account record"))?;
    Ok(Account {
        id,
        user: user.to_string(),
        password_hash: password_hash.to_string(),
        revision: row.get("revision").and_then(serde_json::Value::as_u64).unwrap_or(1),
        registered_at: row.get("registered_at").and_then(serde_json::Value::as_str).unwrap_or("").to_string(),
        last_login_at: row.get("last_login_at").and_then(serde_json::Value::as_str).map(str::to_string),
    })
}

/// 老记录没有注册时间就补当前时间
fn normalize_account(account: &mut Account) {
    if account.registered_at.is_empty() {
        account.registered_at = now_stamp();
    }
}

/// 当前 UTC 时间，写法与引擎的 timestamp 一致
fn now_stamp() -> String {
    let seconds = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .map(|d| d.as_secs() as i64)
        .unwrap_or(0);
    format_timestamp(seconds)
}

/// Unix 秒转时间戳文本
fn format_timestamp(seconds: i64) -> String {
    let days = seconds.div_euclid(86_400);
    let rest = seconds.rem_euclid(86_400);
    let (year, month, day) = civil_from_days(days);
    format!(
        "{:04}-{:02}-{:02} {:02}:{:02}:{:02}",
        year, month, day, rest / 3600, (rest % 3600) / 60, rest % 60
    )
}

/// 天数 → (年, 月, 日)
fn civil_from_days(days: i64) -> (i64, u32, u32) {
    let z = days + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1460 + doe / 36_524 - doe / 146_096) / 365;
    let year = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let day = (doy - (153 * mp + 2) / 5 + 1) as u32;
    let month = if mp < 10 { mp + 3 } else { mp - 9 } as u32;
    (if month <= 2 { year + 1 } else { year }, month, day)
}

/// 规范化并校验账号名
fn account_name(user: &str) -> std::io::Result<String> {
    if user.is_empty() || user.len() > 64 || !user.bytes().all(|c| c.is_ascii_alphanumeric() || b"_.-".contains(&c)) {
        return Err(std::io::Error::other("invalid account name"));
    }
    Ok(user.to_ascii_lowercase())
}

/// 校验口令哈希长度，空串表示免密
fn validate_account_hash(hash: &str) -> std::io::Result<()> {
    if hash.len() > 256 {
        return Err(std::io::Error::other("invalid account credential"));
    }
    Ok(())
}

/// 造一条新账号记录
fn new_account(id: i64, user: &str, password_hash: String) -> std::io::Result<Account> {
    validate_account_hash(&password_hash)?;
    Ok(Account {
        id,
        user: account_name(user)?,
        password_hash,
        revision: 1,
        registered_at: now_stamp(),
        last_login_at: None,
    })
}

/// 一张表的索引定义转线上格式
fn index_wires(schema: &crate::catalog::TableSchema) -> Vec<IndexWire> {
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
fn stat_wires(schema: &crate::catalog::TableSchema) -> Vec<ColumnStatWire> {
    schema
        .stats
        .iter()
        .map(|(name, s)| ColumnStatWire {
            name: name.clone(),
            distinct: s.distinct,
            capped: s.capped,
            lo: s.lo,
            hi: s.hi,
            hist: s.hist.clone(),
        })
        .collect()
}

impl Drop for Server {
    /// 析构时保存数据字典
    fn drop(&mut self) {
        if let Ok(c) = self.catalog.lock() {
            let _ = c.save(self.catalog_path());
        }
    }
}

/// 系统库名：账号表住在这里，其中的表都是系统表。服务启动时只建它一个，
/// 别的库一律要显式建（没有默认工作库）。
const SYSTEM_DATABASE: &str = "system";

/// 老目录里角色与授权表的旧名对照
const LEGACY_TABLE_RENAMES: [(&str, &str); 3] = [
    ("sys_roles", "__system_roles"),
    ("sys_grants", "__system_grants"),
    ("sys_members", "__system_members"),
];

/// 保留库：不能建、不能删。
fn reserved_database(name: &str) -> bool {
    name == SYSTEM_DATABASE
}

/// 每个库拥有独立的目录、字典、句柄和 WAL。
struct Databases {
    root: Config,
    system: Arc<Server>,
    named: Mutex<HashMap<String, Arc<Server>>>,
}

impl Databases {
    /// 装载系统库与已有的具名库
    fn new(cfg: Config) -> std::io::Result<Self> {
        // 系统库按需创建：目录、字典与系统表缺失时自动补齐。
        // 其余库只在 data/databases/<name>/ 下已存在时才装载，启动不会凭空建库。
        let system = Arc::new(Self::open(&cfg, SYSTEM_DATABASE)?);
        let root = cfg.data_dir.join("databases");
        std::fs::create_dir_all(&root)?;
        let canonical_root = std::fs::canonicalize(&root)?;
        if canonical_root.parent() != Some(std::fs::canonicalize(&cfg.data_dir)?.as_path()) {
            return Err(std::io::Error::other("database root escapes data directory"));
        }
        let mut named = HashMap::new();
        for entry in std::fs::read_dir(root)? {
            let entry = entry?;
            let name = entry.file_name().to_string_lossy().to_string();
            if entry.file_type()?.is_dir() && valid_database(&name) && !reserved_database(&name)
                && name == name.to_ascii_lowercase() && entry.path().join("catalog.json").is_file() {
                if std::fs::canonicalize(entry.path())?.parent() != Some(canonical_root.as_path()) {
                    return Err(std::io::Error::other("database directory escapes database root"));
                }
                let mut config = cfg.clone();
                config.data_dir = entry.path();
                let server = Server::new(config)?;
                server.recover_wal()?;
                server.rebuild_stats()?;
                server.migrate_data()?;
                server.compact_tables()?;
                named.insert(name, Arc::new(server));
            }
        }
        Ok(Self { root: cfg, system, named: Mutex::new(named) })
    }

    /// 按名字打开或新建一个库
    fn open(cfg: &Config, name: &str) -> std::io::Result<Server> {
        let mut config = cfg.clone();
        config.data_dir = cfg.data_dir.join(name);
        let server = Server::new(config)?;
        if !server.catalog_path().is_file() {
            server.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?.save(server.catalog_path())?;
        }
        server.recover_wal()?;
        server.rebuild_stats()?;
        server.migrate_data()?;
        server.compact_tables()?;
        Ok(server)
    }

    /// 库级分发：先处理库管理，再转给选中的库
    fn request(&self, mut value: serde_json::Value) -> std::io::Result<Response> {
        use std::io::Error;
        let method = value.get("method").and_then(|v| v.as_str()).unwrap_or("").to_string();
        let name = match value.get("database") {
            None => String::new(),
            Some(serde_json::Value::String(s)) => s.to_ascii_lowercase(),
            _ => return Err(Error::other("invalid database name")),
        };
        if !name.is_empty() && !valid_database(&name) { return Err(Error::other("invalid database name")); }
        let mut named = self.named.lock().map_err(|_| Error::other("database lock poisoned"))?;
        match method.as_str() {
            "all_catalogs" => {
                let servers: Vec<(String, Arc<Server>)> =
                    std::iter::once((SYSTEM_DATABASE.to_string(), Arc::clone(&self.system)))
                        .chain(named.iter().map(|(name, server)| (name.clone(), Arc::clone(server))))
                        .collect();
                drop(named);
                let mut schemas = Vec::new();
                for (database, server) in servers {
                    match server.handle_request(Request::ListCatalog) {
                        Response::Catalog { schemas: entries } => {
                            for mut entry in entries {
                                entry.table = format!("{database}.{}", entry.table);
                                schemas.push(entry);
                            }
                        }
                        other => return Ok(other),
                    }
                }
                return Ok(Response::Catalog { schemas });
            }
            "list_databases" => {
                let mut databases: Vec<String> = named.keys().cloned().collect();
                databases.sort();
                databases.push(SYSTEM_DATABASE.into());
                return Ok(Response::Tables { tables: databases });
            }
            "create_database" => {
                if !valid_database(&name) { return Err(Error::other("invalid database name")); }
                if reserved_database(&name) || named.contains_key(&name) { return Err(Error::other("database already exists")); }
                let mut cfg = self.root.clone();
                cfg.data_dir = cfg.data_dir.join("databases").join(&name);
                std::fs::create_dir(&cfg.data_dir)?;
                let server = Server::new(cfg)?;
                server.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?.save(server.catalog_path())?;
                named.insert(name, Arc::new(server));
                return Ok(Response::Ok);
            }
            "drop_database" => {
                if name == SYSTEM_DATABASE { return Err(Error::other("cannot drop the system database")); }
                let holder = named.remove(&name).ok_or_else(|| Error::other("unknown database"))?;
                let server = match Arc::try_unwrap(holder) {
                    Ok(server) => server,
                    Err(holder) => {
                        named.insert(name, holder);
                        return Err(Error::other("database is in use"));
                    }
                };
                let root = std::fs::canonicalize(self.root.data_dir.join("databases"))?;
                let target = std::fs::canonicalize(&server.cfg.data_dir)?;
                if target.parent() != Some(root.as_path()) || !target.join("catalog.json").is_file() {
                    return Err(Error::other("invalid database directory"));
                }
                let config = server.cfg.clone();
                drop(server);
                let stamp = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map_err(Error::other)?.as_nanos();
                let tombstone = root.join(format!(".dropped-{name}-{stamp}"));
                if let Err(error) = std::fs::rename(&target, &tombstone) {
                    named.insert(name, Arc::new(Server::new(config)?));
                    return Err(error);
                }
                std::fs::remove_dir_all(&tombstone).map_err(|e| Error::other(format!("database dropped; directory cleanup failed: {e}")))?;
                return Ok(Response::Ok);
            }
            "use_database" => {
                return if reserved_database(&name) || named.contains_key(&name) { Ok(Response::Ok) }
                    else { Err(Error::other("unknown database")) };
            }
            _ => {}
        }
        let mut selected = name;
        let qualified = value.get("table").and_then(|v| v.as_str()).map(str::to_owned);
        if let Some((database, table)) = qualified.as_deref().and_then(|t| t.split_once('.')) {
            if !valid_database(database) { return Err(Error::other("invalid database name")); }
            selected = database.to_ascii_lowercase();
            value["table"] = serde_json::Value::String(table.to_string());
        }
        let request: Request = serde_json::from_value(value).map_err(Error::other)?;
        let global = matches!(request, Request::AccountsList | Request::AccountCreate { .. }
            | Request::AccountReset { .. } | Request::AccountLogin { .. }
            | Request::AccountDrop { .. } | Request::BootstrapSystem { .. }
            | Request::SystemStatus | Request::Ping);
        if !global && selected.is_empty() { return Err(Error::other("no database selected")); }
        let server = if global || selected == SYSTEM_DATABASE { Arc::clone(&self.system) }
            else { Arc::clone(named.get(&selected).ok_or_else(|| Error::other("unknown database"))?) };
        drop(named);
        Ok(server.handle_request(request))
    }
}

/// 只读表页的请求可与其它读并发，其余独占
fn concurrent_read(req: &Request) -> bool {
    matches!(
        req,
        Request::Scan { .. }
            | Request::ScanShard { .. }
            | Request::LookupByIndex { .. }
            | Request::RangeByIndex { .. }
    )
}

/// 校验库名是否合法
fn valid_database(name: &str) -> bool {
    let lower = name.to_ascii_lowercase();
    let device = matches!(lower.as_str(), "con" | "prn" | "aux" | "nul")
        || (lower.len() == 4 && (lower.starts_with("com") || lower.starts_with("lpt"))
            && lower.as_bytes()[3].is_ascii_digit());
    !name.is_empty() && name.len() <= 64 && !device && !reserved_table(name)
        && name.bytes().all(|c| c.is_ascii_alphanumeric() || c == b'_')
        && name.as_bytes().first().is_some_and(|c| c.is_ascii_alphabetic() || *c == b'_')
}

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

/// 进程内存储句柄：一个数据目录一份状态，跨线程共享（内部各自加锁）。
pub struct Storage {
    databases: Databases,
}

impl Storage {
    /// 打开数据目录并初始化存储
    pub fn open(config_path: Option<&str>) -> Result<Storage, String> {
        let loaded = config::load(config_path)?;
        log::init();
        log::set_level(loaded.config.log_level);
        log_info!(core, "chusql-core-storage {} starting", env!("CARGO_PKG_VERSION"));
        log_config(&loaded);
        let databases = Databases::new(loaded.config).map_err(|e| e.to_string())?;
        Ok(Storage { databases })
    }

    /// 一行请求 JSON 换一行响应 JSON
    pub fn request_line(&self, line: &str) -> String {
        let response = match serde_json::from_str::<serde_json::Value>(line) {
            Ok(value) => match self.databases.request(value) {
                Ok(response) => response,
                Err(e) => Response::Error { message: e.to_string() },
            },
            Err(e) => {
                log_warn!(core, "bad request line: {}", e);
                Response::Error {
                    message: format!("parse error: {}", e),
                }
            }
        };
        match serde_json::to_string(&response) {
            Ok(text) => text,
            Err(e) => {
                log_error!(core, "response encode failed: {}", e);
                "{\"status\":\"error\",\"message\":\"response encode failed\"}".to_string()
            }
        }
    }
}

