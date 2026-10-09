use std::collections::{HashMap, HashSet};
use std::path::PathBuf;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::{Arc, Mutex, RwLock};

use crate::catalog::Catalog;
use crate::config::{self, Config, Loaded};
use crate::heap::{scalar_int, HeapTable};
use crate::log;
use crate::protocol::{
    blocked_table, reserved_table, Account, ColumnStatWire, IndexWire, Request, Response, Row, IDENTITIES_TABLE,
    SchemaColumn, StorageOp, TableSchemaWire, USERS_TABLE, TYPES_TABLE,
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
    /// 读取受保护的对象权限快照
    fn object_acl_read(&self) -> std::io::Result<Vec<Row>> {
        if !self.table_exists("__system_object_acl")? { return Ok(Vec::new()); }
        let rows = self.with_existing_table("__system_object_acl", |table| table.scan())?;
        let mut payload = String::new();
        for (index, row) in rows.iter().enumerate() {
            if row.get("id").and_then(|value| value.as_u64()) != Some(index as u64 + 1) {
                return Err(std::io::Error::other("invalid object ACL chunk sequence"));
            }
            payload.push_str(row.get("payload").and_then(|value| value.as_str())
                .ok_or_else(|| std::io::Error::other("invalid object ACL chunk"))?);
        }
        if rows.is_empty() { return Err(std::io::Error::other("empty object ACL snapshot")); }
        Ok(vec![serde_json::from_value(serde_json::json!({"id":1,"payload":payload})).map_err(std::io::Error::other)?])
    }

    /// 比较并原子替换对象权限快照
    fn object_acl_replace(&self, expected: Option<&str>, payload: &str) -> std::io::Result<()> {
        let rows = self.object_acl_read()?;
        let current = rows.first().and_then(|row| row.get("payload")).and_then(|value| value.as_str());
        if current != expected { return Err(std::io::Error::other("object ACL changed concurrently")); }
        let _: serde_json::Value = serde_json::from_str(payload).map_err(std::io::Error::other)?;
        let mut ops = Vec::new();
        if !self.table_exists("__system_object_acl")? {
            ops.push(WalOp::CreateTable { table: "__system_object_acl".into(),
                columns: vec![SchemaColumn { name: "id".into(), ty: "int".into(), ..Default::default() },
                    SchemaColumn { name: "payload".into(), ty: "str".into(), ..Default::default() }], object_id: None });
        }
        let chars: Vec<char> = payload.chars().collect();
        let rows = chars.chunks(256).enumerate().map(|(index, chunk)| {
            serde_json::from_value(serde_json::json!({"id":index + 1,"payload":chunk.iter().collect::<String>()}))
                .map_err(std::io::Error::other)
        }).collect::<std::io::Result<Vec<Row>>>()?;
        ops.push(WalOp::ReplaceAll { table: "__system_object_acl".into(), rows });
        self.group_via_wal(&ops)
    }

    /// 新建服务器状态
    fn new(cfg: Config) -> std::io::Result<Self> {
        std::fs::create_dir_all(&cfg.data_dir)?;
        let catalog = Catalog::load(cfg.data_dir.join("catalog.json"))?;
        catalog.save(cfg.data_dir.join("catalog.json"))?;
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
            WalOp::Accounts { accounts, removed, migrate_roles } => {
                let rows = account_rows(accounts)?;
                let identities = identity_rows(accounts)?;
                for (table, records) in [(USERS_TABLE, &rows), (IDENTITIES_TABLE, &identities)] {
                    self.tables.lock().map_err(|_| std::io::Error::other("table lock poisoned"))?.remove(table);
                    for path in [self.table_path(table), self.index_path(table, "id")] {
                        std::fs::File::create(path)?.sync_all()?;
                    }
                    self.with_table(table, |t| { t.replace_all(records)?; t.flush() })?;
                }
                let legacy_roles = "__system_roles";
                let has_roles = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?.describe(legacy_roles).is_some();
                if has_roles && *migrate_roles {
                    self.with_existing_table(legacy_roles, |t| { t.replace_all(&[])?; t.flush() })?;
                }
                let mut cleaned = Vec::new();
                for table in ["__system_grants", "__system_grant_options", "__system_members"] {
                    let exists = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?.describe(table).is_some();
                    if exists && !removed.is_empty() {
                        let rows = self.with_existing_table(table, |t| t.scan())?.into_iter().filter(|r|
                            !["role", "member"].iter().any(|field| r.get(*field).and_then(serde_json::Value::as_str).is_some_and(|name| removed.iter().any(|old| old == name)))).collect::<Vec<_>>();
                        self.with_existing_table(table, |t| { t.replace_all(&rows)?; t.flush() })?;
                        cleaned.push((table, rows));
                    }
                }
                let mut c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
                declare_system_schema(&mut c, &rows)?;
                c.rebuild_stats(IDENTITIES_TABLE, &identities);
                c.set_columns(IDENTITIES_TABLE, identity_columns())?;
                c.rebuild_stats(IDENTITIES_TABLE, &identities);
                c.mark_system(IDENTITIES_TABLE)?;
                c.add_index(IDENTITIES_TABLE, "user")?;
                c.identity_high_water = c.identity_high_water.max(accounts.iter().map(|a| a.id).max().unwrap_or(0));
                if has_roles && *migrate_roles { c.rebuild_stats(legacy_roles, &[]); }
                for (table, records) in cleaned { c.rebuild_stats(table, &records); }
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
            WalOp::CreateTable { table, columns, object_id } => {
                self.with_table(table, |_t| Ok(()))?;
                let mut c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
                c.create_table_with_id(table, columns.clone(), *object_id)?;
                if table == TYPES_TABLE || table == "__system_object_acl" { c.mark_system(table)?; }
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
        let op = self.normalize_decimal_write(op)?;
        self.validate_op(&op)?;
        self.validate_domains(&op)?;
        self.group_via_wal(std::slice::from_ref(&op))
    }

    /// 写日志前规范化定点数值
    fn normalize_decimal_write(&self, op: &WalOp) -> std::io::Result<WalOp> {
        let mut normalized = op.clone();
        let catalog = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
        let (table, rows): (&str, Vec<&mut Row>) = match &mut normalized {
            WalOp::Insert { table, row } => (table, vec![row]),
            WalOp::InsertBatch { table, rows } | WalOp::ReplaceAll { table, rows } => (table, rows.iter_mut().collect()),
            WalOp::CreateTable { columns, .. } => {
                for column in columns { decimal_parameters(&column.ty)?; }
                return Ok(normalized);
            }
            WalOp::ReplaceSchema { columns, rows, .. } => {
                for column in columns.iter() { decimal_parameters(&column.ty)?; }
                for row in rows { normalize_decimal_row(row, columns)?; }
                return Ok(normalized);
            }
            _ => return Ok(normalized),
        };
        if let Some(schema) = catalog.describe(table) {
            for row in rows {
                normalize_decimal_row(row, &schema.columns)?;
            }
        }
        Ok(normalized)
    }

    /// 校验命名类型引用与删除依赖
    fn validate_domains(&self, op: &WalOp) -> std::io::Result<()> {
        let catalog = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
        match op {
            WalOp::CreateTable { columns, .. } | WalOp::ReplaceSchema { columns, .. } => {
                for column in columns {
                    let mut ty = column.ty.as_str();
                    while let Some(body) = ty.strip_prefix("domain(").and_then(|s| s.strip_suffix(')')) {
                        let (name, base) = body.split_once(',').ok_or_else(|| std::io::Error::other("invalid domain type"))?;
                        let key = format!("__system_domain_{name}");
                        let definition = catalog.describe(&key).ok_or_else(|| std::io::Error::other(format!("unknown domain: {name}")))?;
                        if !definition.columns.iter().any(|c| c.name == "base" && c.ty == base) {
                            return Err(std::io::Error::other(format!("domain base type mismatch: {name}")));
                        }
                        ty = base;
                    }
                }
            }
            WalOp::DropTable { table } => {
                if let Some(name) = table.strip_prefix("__system_domain_") {
                    let prefix = format!("domain({name},");
                    if catalog.all_tables().iter().any(|(_, schema)| schema.columns.iter().any(|c| !schema.dropped_columns.contains(&c.name) && c.ty.contains(&prefix))) {
                        return Err(std::io::Error::other(format!("domain is still used by a column: {name}")));
                    }
                }
            }
            _ => {}
        }
        Ok(())
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
            let wal_op = self.normalize_decimal_write(&wal_op)?;
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
        let mut ops = std::borrow::Cow::Borrowed(ops);
        if ops.iter().any(|op| matches!(op, WalOp::CreateTable { object_id: None, .. })) {
            let mut catalog = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
            for op in ops.to_mut() {
                if let WalOp::CreateTable { object_id, .. } = op
                    && object_id.is_none() {
                    *object_id = Some(catalog.reserve_object_id()?);
                }
            }
        }
        let commit_lsn = wal.append_group(&ops)?;
        self.recovery_required.store(true, Ordering::Release);
        for op in ops.iter() {
            self.apply_op(op)?;
        }
        for op in ops.iter() {
            self.flush_op_tables(op)?;
        }
        self.flush_all_tables()?;
        wal.write_checkpoint(commit_lsn)?;
        wal.truncate_before(commit_lsn)?;
        self.recovery_required.store(false, Ordering::Release);
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
        let checkpoint = wal.read_checkpoint()?.unwrap_or(0);
        let scan = wal.read_all()?;
        if scan.records.is_empty() {
            if wal.path().exists() {
                wal.truncate_to(scan.good_len)?;
            }
            return Ok(());
        }
        // 检查点以内的记录已经落盘，直接按边界截掉
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
            WalOp::CreateTable { table, object_id, .. } => {
                let catalog = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
                match catalog.describe(table) {
                    Some(schema) if object_id.is_some_and(|id| id != schema.object_id) => {
                        Err(std::io::Error::other("WAL catalog object id mismatch"))
                    }
                    Some(_) => Ok(true),
                    None => Ok(false),
                }
            }
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

    /// 整理一张表：原地回收页内碎片，有隐藏列时重写堆页
    fn compact_table(&self, table: &str) -> std::io::Result<()> {
        let rewrite = {
            let c = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
            c.has_dropped_columns(table)
        };
        if !rewrite {
            let reclaimed = self.with_existing_table(table, |t| {
                let n = t.compact_pages()?;
                t.flush()?;
                Ok(n)
            })?;
            log_info!(core, "compact {} reclaimed {} bytes", table, reclaimed);
            return Ok(());
        }

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
            match c.describe(IDENTITIES_TABLE) {
                Some(schema) if !schema.system => return Err(Error::new(ErrorKind::AlreadyExists, "reserved identity table collision")),
                None if self.table_path(IDENTITIES_TABLE).exists() || self.index_path(IDENTITIES_TABLE, "id").exists() =>
                    return Err(Error::new(ErrorKind::AlreadyExists, "reserved identity file collision")),
                _ => {}
            }
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

    /// 读取系统初始化状态与检查点 LSN
    fn system_status(&self) -> std::io::Result<Response> {
        let initialized = self.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?
            .describe(USERS_TABLE).is_some_and(|schema| schema.system);
        let last_lsn = self.wal.read_checkpoint()?.unwrap_or(0);
        Ok(Response::System { initialized, last_lsn })
    }

    /// 读账号表；表没登记或数据文件缺失都算错
    fn load_accounts(&self) -> std::io::Result<Vec<Account>> {
        use std::io::Error;
        if !self.table_path(USERS_TABLE).exists() {
            return Err(Error::other("missing account data"));
        }
        let unified = self.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?.describe(IDENTITIES_TABLE).is_some();
        if unified {
            let credentials = self.with_existing_table(USERS_TABLE, |t| t.scan())?;
            let identities = self.with_existing_table(IDENTITIES_TABLE, |t| t.scan())?;
            return identities.into_iter().map(|mut row| {
                for field in ["can_login", "is_superuser", "enabled"] {
                    if row.get(field).and_then(serde_json::Value::as_bool).is_none() { return Err(Error::other("invalid identity attributes")); }
                }
                let id = row.get("id").and_then(serde_json::Value::as_i64).ok_or_else(|| Error::other("invalid identity record"))?;
                let credential = credentials.iter().find(|r| r.get("id").and_then(serde_json::Value::as_i64) == Some(id))
                    .ok_or_else(|| Error::other("missing identity credential"))?;
                row.insert("password_hash".into(), credential.get("password_hash").cloned().ok_or_else(|| Error::other("invalid identity credential"))?);
                decode_account_row(&row)
            }).collect();
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
                let id = accounts.iter().map(|a| a.id).max().unwrap_or(0).max(self.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?.identity_high_water)
                    .checked_add(1).ok_or_else(|| Error::other("account id exhausted"))?;
                let mut administrator = new_account(id, &name, password_hash.unwrap_or_default().to_string())?;
                administrator.is_superuser = true;
                accounts.push(administrator);
                changed = true;
            }
        }
        if !changed {
            return Ok(accounts);
        }
        for row in account_rows(&accounts)? {
            HeapTable::encode_row(&row, self.cfg.page_size)?;
        }
        let op = WalOp::Accounts { accounts: accounts.clone(), removed: Vec::new(), migrate_roles: false };
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
        let request = match request {
            Request::CatalogManage { command } => {
                let target = match command.as_ref() {
                    Request::AccountCreate { user, .. } | Request::AccountReset { user, .. } | Request::AccountDrop { user }
                        | Request::IdentityAlter { user, is_superuser: None, system_catalog_manager: None, allow_sudo_auth: None, .. } => Some(account_name(user)?),
                    Request::AccountsList => None,
                    _ => return Err(Error::other("superuser required for this catalog operation")),
                };
                if accounts.iter().any(|a| target.as_ref() == Some(&a.user) && (a.is_superuser || a.system_catalog_manager || a.allow_sudo_auth)) {
                    return Err(Error::other("superuser required to manage privileged identities"));
                }
                *command
            }
            other => other,
        };
        let mut names = std::collections::HashSet::new();
        let mut ids = std::collections::HashSet::new();
        for account in &mut accounts {
            let name = account_name(&account.user)?;
            if !names.insert(name.clone()) || account.id < 1 || !ids.insert(account.id) {
                return Err(Error::other("invalid or duplicate identity record"));
            }
            if account.identity_version == 0 { account.user = name; }
        }
        let previous = accounts.clone();
        let migrate_roles = matches!(request, Request::IdentityInitialize { .. });
        if !migrate_roles && !matches!(request, Request::AccountsList) {
            let legacy_exists = self.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?.describe("__system_roles").is_some();
            if legacy_exists && !self.with_existing_table("__system_roles", |t| t.scan())?.is_empty() {
                return Err(Error::other("identity migration required"));
            }
        }
        let had_superuser = accounts.iter().any(|a| a.enabled && a.can_login && a.is_superuser);
        match request {
            Request::AccountsList => return Ok(accounts),
            Request::IdentityInitialize { administrator } => {
                for account in &mut accounts {
                    if account.identity_version == 0 {
                        account.is_superuser = account.user == administrator.to_ascii_lowercase();
                        account.identity_version = 1;
                    }
                }
                let legacy_roles = "__system_roles";
                let exists = self.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?.describe(legacy_roles).is_some();
                if exists {
                    for row in self.with_existing_table(legacy_roles, |t| t.scan())? {
                        let name = account_name(row.get("name").and_then(serde_json::Value::as_str).ok_or_else(|| Error::other("invalid legacy role"))?)?;
                        if accounts.iter().any(|a| a.user == name) { return Err(Error::other(format!("identity name collision: {name}"))); }
                        let id = accounts.iter().map(|a| a.id).max().unwrap_or(0).max(self.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?.identity_high_water)
                            .checked_add(1).ok_or_else(|| Error::other("account id exhausted"))?;
                        let mut role = new_account(id, &name, String::new())?;
                        role.can_login = false;
                        accounts.push(role);
                    }
                }
                let unified = self.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?.describe(IDENTITIES_TABLE).is_some();
                if !accounts.is_empty() { require_superuser(&accounts)?; }
                let sudo_column = self.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?.describe(IDENTITIES_TABLE)
                    .is_some_and(|schema| schema.columns.iter().any(|column| column.name == "allow_sudo_auth"));
                if unified && sudo_column && previous == accounts { return Ok(accounts); }
            }
            Request::AccountCreate { user, password_hash } => {
                let name = account_name(&user)?;
                if name == "public" { return Err(Error::other("public is reserved for default privileges")); }
                if accounts.iter().any(|a| a.user == name) {
                    return Err(Error::new(ErrorKind::AlreadyExists, "account already exists"));
                }
                let id = accounts.iter().map(|a| a.id).max().unwrap_or(0).max(self.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?.identity_high_water)
                    .checked_add(1).ok_or_else(|| Error::other("account id exhausted"))?;
                accounts.push(new_account(id, &name, password_hash)?);
            }
            Request::RoleCreate { user } => {
                let name = account_name(&user)?;
                if name == "public" { return Err(Error::other("public is reserved for default privileges")); }
                if accounts.iter().any(|a| a.user == name) { return Err(Error::new(ErrorKind::AlreadyExists, "identity already exists")); }
                let id = accounts.iter().map(|a| a.id).max().unwrap_or(0).max(self.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?.identity_high_water)
                    .checked_add(1).ok_or_else(|| Error::other("account id exhausted"))?;
                let mut role = new_account(id, &name, String::new())?;
                role.can_login = false;
                accounts.push(role);
            }
            Request::IdentityAlter { user, can_login, is_superuser, enabled, system_catalog_manager, allow_sudo_auth } => {
                let name = account_name(&user)?;
                let account = accounts.iter_mut().find(|a| a.user == name).ok_or_else(|| Error::new(ErrorKind::NotFound, "unknown account"))?;
                account.can_login = can_login.unwrap_or(account.can_login);
                account.is_superuser = is_superuser.unwrap_or(account.is_superuser);
                account.enabled = enabled.unwrap_or(account.enabled);
                account.system_catalog_manager = system_catalog_manager.unwrap_or(account.system_catalog_manager);
                account.allow_sudo_auth = allow_sudo_auth.unwrap_or(account.allow_sudo_auth);
                account.revision = account.revision.checked_add(1).ok_or_else(|| Error::other("account revision exhausted"))?;
                if had_superuser { require_superuser(&accounts)?; }
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
                if !account.enabled || !account.can_login { return Err(Error::other("identity cannot login")); }
                account.last_login_at = Some(stamp);
            }
            Request::AccountDrop { user } => {
                let name = account_name(&user)?;
                let before = accounts.len();
                accounts.retain(|a| a.user != name);
                if accounts.len() == before {
                    return Err(Error::new(ErrorKind::NotFound, "unknown account"));
                }
                if had_superuser { require_superuser(&accounts)?; }
            }
            _ => return Err(Error::other("unexpected account request")),
        }
        for row in account_rows(&accounts)?.into_iter().chain(identity_rows(&accounts)?) {
            HeapTable::encode_row(&row, self.cfg.page_size)?;
        }
        let removed = previous.iter().filter(|old| !accounts.iter().any(|a| a.user == old.user)).map(|a| a.user.clone()).collect();
        let op = WalOp::Accounts { accounts: accounts.clone(), removed, migrate_roles };
        self.recovery_required.store(true, Ordering::Release);
        self.wal.truncate()?;
        self.wal.append(&op)?;
        self.apply_op(&op)?;
        self.wal.clear()?;
        self.recovery_required.store(false, Ordering::Release);
        Ok(accounts)
    }

    /// 初始化并校验内置类型目录
    fn bootstrap_types(&self, types: Vec<Row>) -> std::io::Result<Vec<Row>> {
        use std::io::Error;
        let _guard = self.write_lock.lock().map_err(|_| Error::other("write lock poisoned"))?;
        if !self.account_table_ready()? { return Err(Error::other("system catalog is not initialized; run csql-bootstrap")); }
        require_superuser(&self.load_accounts()?)?;
        let columns = types_columns();
        let existing = self.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?.describe(TYPES_TABLE).cloned();
        if existing.as_ref().is_some_and(|s| !s.system || s.columns != columns) { return Err(Error::other("reserved type catalog collision")); }
        if existing.is_none() && (self.table_path(TYPES_TABLE).exists() || self.index_path(TYPES_TABLE, "id").exists()) {
            return Err(Error::other("reserved type file collision"));
        }
        let mut rows = if existing.is_some() { self.with_existing_table(TYPES_TABLE, |t| t.scan())? } else { Vec::new() };
        let mut names = std::collections::HashSet::new();
        let mut ids = std::collections::HashSet::new();
        for row in &rows {
            let name = type_definition_name(row)?;
            let id = row.get("id").and_then(serde_json::Value::as_i64).ok_or_else(|| Error::other("invalid type id"))?;
            if id < 1 || !ids.insert(id) || !names.insert(name.to_string()) { return Err(Error::other("duplicate type catalog record")); }
        }
        let original = rows.clone();
        let mut requested = std::collections::HashSet::new();
        for mut definition in types {
            let name = type_definition_name(&definition)?.to_string();
            if definition.len() != 3 || !requested.insert(name.clone()) { return Err(Error::other("invalid or duplicate preinstalled type")); }
            if let Some(saved) = rows.iter().find(|row| row.get("name").and_then(serde_json::Value::as_str) == Some(&name)) {
                if saved.get("base_type") != definition.get("base_type") || saved.get("parameterized") != definition.get("parameterized") {
                    return Err(Error::other(format!("preinstalled type definition conflict: {name}")));
                }
            } else {
                let id = rows.iter().filter_map(|r| r.get("id").and_then(serde_json::Value::as_i64)).max().unwrap_or(0)
                    .checked_add(1).ok_or_else(|| Error::other("type id exhausted"))?;
                definition.insert("id".into(), serde_json::json!(id));
                HeapTable::encode_row(&definition, self.cfg.page_size)?;
                rows.push(definition);
            }
        }
        if existing.is_some() && original == rows { return Ok(rows); }
        let mut ops = Vec::new();
        if existing.is_none() { ops.push(WalOp::CreateTable { table: TYPES_TABLE.into(), columns, object_id: None }); }
        ops.push(WalOp::ReplaceAll { table: TYPES_TABLE.into(), rows: rows.clone() });
        self.recovery_required.store(true, Ordering::Release);
        self.group_via_wal(&ops)?;
        self.recovery_required.store(false, Ordering::Release);
        Ok(rows)
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
            Request::BootstrapTypes { types } => match self.bootstrap_types(types) {
                Ok(rows) => Response::Rows { rows },
                Err(e) => Response::Error { message: e.to_string() },
            },
            request @ (Request::AccountsList | Request::CatalogManage { .. } | Request::AccountCreate { .. }
                | Request::IdentityInitialize { .. } | Request::RoleCreate { .. } | Request::IdentityAlter { .. }
                | Request::AccountReset { .. } | Request::AccountLogin { .. }
                | Request::AccountDrop { .. }) => {
                match self.accounts_request(request) {
                    Ok(accounts) => Response::Accounts { accounts },
                    Err(e) => Response::Error { message: e.to_string() },
                }
            }
            Request::BootstrapSystem { user, password_hash } => {
                match self.bootstrap_system(user.as_deref(), password_hash.as_deref()).and_then(|_| self.system_status()) {
                    Ok(response) => response,
                    Err(e) => Response::Error { message: e.to_string() },
                }
            }
            Request::SystemStatus => self.system_status().unwrap_or_else(|e| Response::Error { message: e.to_string() }),
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
                        database_id: c.database_id,
                        object_id: ts.object_id,
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
                let op = WalOp::CreateTable { table, columns, object_id: None };
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

/// 构造内置类型目录列定义
fn types_columns() -> Vec<SchemaColumn> {
    vec![
        SchemaColumn { name: "id".into(), ty: "int".into(), nullable: false, primary_key: true, ..Default::default() },
        SchemaColumn { name: "name".into(), ty: "varchar(64)".into(), nullable: false, unique: true, ..Default::default() },
        SchemaColumn { name: "base_type".into(), ty: "varchar(64)".into(), nullable: false, ..Default::default() },
        SchemaColumn { name: "parameterized".into(), ty: "bool".into(), nullable: false, ..Default::default() },
    ]
}

/// 校验类型定义并读取名称
fn type_definition_name(row: &Row) -> std::io::Result<&str> {
    let name = row.get("name").and_then(serde_json::Value::as_str).ok_or_else(|| std::io::Error::other("invalid type name"))?;
    let base = row.get("base_type").and_then(serde_json::Value::as_str).ok_or_else(|| std::io::Error::other("invalid type definition"))?;
    if name.is_empty() || name.len() > 48 || !name.bytes().all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == b'_')
        || base.is_empty() || base.len() > 64 || row.get("parameterized").and_then(serde_json::Value::as_bool).is_none() {
        return Err(std::io::Error::other("invalid type definition"));
    }
    Ok(name)
}

/// 账号列表转成行
fn account_rows(accounts: &[Account]) -> std::io::Result<Vec<Row>> {
    accounts.iter().map(|a| Ok(HashMap::from([
        ("id".into(), serde_json::json!(a.id)),
        ("password_hash".into(), serde_json::Value::String(a.password_hash.clone())),
    ]))).collect()
}

/// 身份元数据转成行
fn identity_rows(accounts: &[Account]) -> std::io::Result<Vec<Row>> {
    accounts.iter().map(|a| Ok(HashMap::from([
        ("id".into(), serde_json::json!(a.id)),
        ("user".into(), serde_json::Value::String(a.user.clone())),
        ("can_login".into(), serde_json::json!(a.can_login)),
        ("is_superuser".into(), serde_json::json!(a.is_superuser)),
        ("system_catalog_manager".into(), serde_json::json!(a.system_catalog_manager)),
        ("allow_sudo_auth".into(), serde_json::json!(a.allow_sudo_auth)),
        ("enabled".into(), serde_json::json!(a.enabled)),
        ("identity_version".into(), serde_json::json!(a.identity_version)),
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
        SchemaColumn { name: "password_hash".into(), ty: "varchar(256)".into(),
            nullable: false, ..Default::default() },
    ]
}

/// 系统身份目录结构
fn identity_columns() -> Vec<SchemaColumn> {
    vec![
        SchemaColumn { name: "id".into(), ty: "int".into(), nullable: false,
            primary_key: true, ..Default::default() },
        SchemaColumn { name: "user".into(), ty: "varchar(64)".into(), nullable: false,
            unique: true, ..Default::default() },
        SchemaColumn { name: "can_login".into(), ty: "bool".into(), nullable: false, ..Default::default() },
        SchemaColumn { name: "is_superuser".into(), ty: "bool".into(), nullable: false, ..Default::default() },
        SchemaColumn { name: "system_catalog_manager".into(), ty: "bool".into(), nullable: false, ..Default::default() },
        SchemaColumn { name: "allow_sudo_auth".into(), ty: "bool".into(), nullable: false, ..Default::default() },
        SchemaColumn { name: "enabled".into(), ty: "bool".into(), nullable: false, ..Default::default() },
        SchemaColumn { name: "identity_version".into(), ty: "int".into(), nullable: false, ..Default::default() },
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
        can_login: row.get("can_login").and_then(serde_json::Value::as_bool).unwrap_or(true),
        is_superuser: row.get("is_superuser").and_then(serde_json::Value::as_bool).unwrap_or(false),
        system_catalog_manager: row.get("system_catalog_manager").and_then(serde_json::Value::as_bool).unwrap_or(false),
        allow_sudo_auth: match row.get("allow_sudo_auth") {
            None => false,
            Some(value) => value.as_bool().ok_or_else(|| std::io::Error::other("invalid sudo authentication attribute"))?,
        },
        enabled: row.get("enabled").and_then(serde_json::Value::as_bool).unwrap_or(true),
        identity_version: row.get("identity_version").and_then(serde_json::Value::as_u64).unwrap_or(0) as u8,
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
        can_login: true,
        is_superuser: false,
        system_catalog_manager: false,
        allow_sudo_auth: false,
        enabled: true,
        identity_version: 1,
    })
}

/// 保留至少一个启用的登录管理员
fn require_superuser(accounts: &[Account]) -> std::io::Result<()> {
    if accounts.iter().any(|a| a.can_login && a.enabled && a.is_superuser) { Ok(()) }
    else { Err(std::io::Error::other("the last enabled login superuser cannot be removed")) }
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

/// 按列精度规范化一行定点数
fn normalize_decimal_row(row: &mut Row, columns: &[SchemaColumn]) -> std::io::Result<()> {
    for column in columns {
        if let Some((precision, scale)) = decimal_parameters(&column.ty)?
            && let Some(value) = row.get_mut(&column.name).filter(|v| !v.is_null()) {
                let number = value.as_f64().ok_or_else(|| std::io::Error::other("invalid decimal value"))?;
                let factor = 10f64.powi(scale);
                let scaled = number * factor;
                if !scaled.is_finite() { return Err(std::io::Error::other("decimal value is not finite or exceeds precision")); }
                let rounded = scaled.round_ties_even() / factor;
                if rounded.abs() >= 10f64.powi(precision - scale) { return Err(std::io::Error::other("decimal value exceeds precision")); }
                *value = serde_json::json!(rounded);
        }
    }
    Ok(())
}

/// 解析基础或命名定点类型参数
fn decimal_parameters(ty: &str) -> std::io::Result<Option<(i32, i32)>> {
    let mut base = ty.trim();
    while let Some(body) = base.strip_prefix("domain(").and_then(|s| s.strip_suffix(')')) {
        base = body.split_once(',').ok_or_else(|| std::io::Error::other("invalid domain type"))?.1.trim();
    }
    let normalized = crate::catalog::normalize_type(base);
    if normalized == "decimal" { return Ok(Some((10, 0))); }
    if !normalized.starts_with("decimal(") { return Ok(None); }
    let error = || std::io::Error::other("invalid decimal precision or scale");
    let body = normalized.strip_prefix("decimal(").and_then(|s| s.strip_suffix(')')).ok_or_else(error)?;
    let values = body.split(',').map(|s| s.trim().parse::<i32>()).collect::<Result<Vec<_>, _>>().map_err(|_| error())?;
    let (precision, scale) = match values.as_slice() { [p] => (*p, 0), [p, s] => (*p, *s), _ => return Err(error()) };
    if !(1..=308).contains(&precision) || scale < 0 || scale > precision { return Err(error()); }
    Ok(Some((precision, scale)))
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
        {
            let mut catalog = system.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
            let mut ids = HashSet::new();
            if catalog.database_id != 0 { ids.insert(catalog.database_id); }
            for server in named.values() {
                let id = server.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?.database_id;
                if id != 0 && !ids.insert(id) { return Err(std::io::Error::other("duplicate database object id")); }
                catalog.object_high_water = catalog.object_high_water.max(id);
            }
            if catalog.database_id == 0 { catalog.database_id = catalog.reserve_object_id()?; }
            catalog.save(system.catalog_path())?;
        }
        let databases = Self { root: cfg, system, named: Mutex::new(named) };
        {
            let named = databases.named.lock().map_err(|_| std::io::Error::other("database lock poisoned"))?;
            for server in named.values() { databases.assign_database_id(server)?; }
        }
        Ok(databases)
    }

    /// 为数据库持久化不复用的编号
    fn assign_database_id(&self, server: &Server) -> std::io::Result<()> {
        let mut catalog = server.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
        if catalog.database_id == 0 {
            let mut system = self.system.catalog.lock().map_err(|_| std::io::Error::other("catalog lock poisoned"))?;
            let id = system.reserve_object_id()?;
            system.save(self.system.catalog_path())?;
            catalog.database_id = id;
            catalog.save(server.catalog_path())?;
        }
        Ok(())
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
            "object_acl_read" => {
                drop(named);
                let _gate = self.system.gate.read().map_err(|_| Error::other("database gate poisoned"))?;
                if self.system.recovery_required.load(Ordering::Acquire) { return Err(Error::other("recovery required")); }
                return Ok(Response::Rows { rows: self.system.object_acl_read()? });
            }
            "object_acl_replace" => {
                drop(named);
                let _gate = self.system.gate.write().map_err(|_| Error::other("database gate poisoned"))?;
                if self.system.recovery_required.load(Ordering::Acquire) { return Err(Error::other("recovery required")); }
                let expected = match value.get("expected") {
                    None | Some(serde_json::Value::Null) => None,
                    Some(serde_json::Value::String(value)) => Some(value.as_str()),
                    _ => return Err(Error::other("invalid expected ACL payload")),
                };
                let payload = value.get("payload").and_then(|value| value.as_str()).ok_or_else(|| Error::other("missing ACL payload"))?;
                self.system.object_acl_replace(expected, payload)?;
                return Ok(Response::Ok);
            }
            "resolve_object" => {
                let server = if name == SYSTEM_DATABASE { &self.system }
                    else { named.get(&name).ok_or_else(|| Error::other("unknown database"))? };
                let _gate = server.gate.read().map_err(|_| Error::other("database gate poisoned"))?;
                if server.recovery_required.load(Ordering::Acquire) { return Err(Error::other("recovery required")); }
                let catalog = server.catalog.lock().map_err(|_| Error::other("catalog lock poisoned"))?;
                let object_id = match value.get("table") {
                    None => None,
                    Some(serde_json::Value::String(table)) if !blocked_table(table) => {
                        let schema = catalog.describe(table).ok_or_else(|| Error::other("unknown table"))?;
                        if schema.object_id == 0 { return Err(Error::other("missing catalog object id")); }
                        Some(schema.object_id)
                    }
                    _ => return Err(Error::other("invalid object table")),
                };
                return Ok(Response::Object { database_id: catalog.database_id, object_id });
            }
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
                self.assign_database_id(&server)?;
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
        let global = matches!(request, Request::AccountsList | Request::CatalogManage { .. } | Request::AccountCreate { .. }
            | Request::IdentityInitialize { .. } | Request::RoleCreate { .. } | Request::IdentityAlter { .. }
            | Request::AccountReset { .. } | Request::AccountLogin { .. }
            | Request::AccountDrop { .. } | Request::BootstrapSystem { .. }
            | Request::BootstrapTypes { .. }
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

/// 获取数据目录的跨进程独占锁
fn lock_data_directory(cfg: &Config) -> std::io::Result<std::fs::File> {
    std::fs::create_dir_all(&cfg.data_dir)?;
    let file = std::fs::OpenOptions::new().read(true).write(true).create(true).truncate(false)
        .open(cfg.data_dir.join(".chusql.lock"))?;
    file.try_lock().map_err(|e| std::io::Error::other(format!("data directory is in use; stop all database processes: {e}")))?;
    Ok(file)
}

/// 复制系统目录并拒绝链接文件
fn copy_system_directory(source: &std::path::Path, target: &std::path::Path) -> std::io::Result<()> {
    std::fs::create_dir_all(target)?;
    for entry in std::fs::read_dir(source)? {
        let entry = entry?;
        let ty = entry.file_type()?;
        if ty.is_symlink() { return Err(std::io::Error::other("system directory contains a symbolic link")); }
        if ty.is_dir() { copy_system_directory(&entry.path(), &target.join(entry.file_name()))?; }
        else if ty.is_file() { std::fs::copy(entry.path(), target.join(entry.file_name()))?; }
        else { return Err(std::io::Error::other("unsupported system directory entry")); }
    }
    Ok(())
}

/// 构造可重建的系统表结构
fn maintenance_schemas() -> Vec<(&'static str, Vec<SchemaColumn>)> {
    let text = |name: &str, size: usize| SchemaColumn { name: name.into(), ty: format!("varchar({size})"), ..Default::default() };
    vec![(USERS_TABLE, users_columns()), (IDENTITIES_TABLE, identity_columns()), (TYPES_TABLE, types_columns()),
        ("__system_roles", vec![text("name", 64)]),
        ("__system_grants", vec![text("role", 64), text("privilege", 16), text("object", 128)]),
        ("__system_grant_options", vec![text("role", 64), text("privilege", 16), text("object", 128)]),
        ("__system_members", vec![text("role", 64), text("member", 64)])]
}

/// 验证隔离副本并执行所选恢复策略
fn maintenance_candidate(cfg: Config, mode: &str, user: &str, hash: &str, types: Vec<Row>) -> Result<(), String> {
    if mode == "reset" {
        let seed = Server::new(cfg.clone()).map_err(|e| e.to_string())?;
        seed.bootstrap_system(Some(user), Some(hash)).map_err(|e| e.to_string())?;
        seed.bootstrap_types(types.clone()).map_err(|e| e.to_string())?;
    }
    let repair = mode == "repair";
    let catalog_path = cfg.data_dir.join("catalog.json");
    let mut schemas = maintenance_schemas();
    if cfg.data_dir.join("__system_object_acl.db").is_file() {
        schemas.push(("__system_object_acl", vec![SchemaColumn { name: "id".into(), ty: "int".into(), ..Default::default() },
            SchemaColumn { name: "payload".into(), ty: "str".into(), ..Default::default() }]));
    }
    let mut missing_data = Vec::new();
    let mut catalog = match Catalog::load(&catalog_path) {
        Ok(catalog) => catalog,
        Err(_) => {
            for entry in std::fs::read_dir(&cfg.data_dir).map_err(|e| e.to_string())? {
                let entry = entry.map_err(|e| e.to_string())?;
                if entry.path().extension().and_then(|s| s.to_str()) == Some("db") {
                    let name = entry.path().file_stem().map(|s| s.to_string_lossy().into_owned()).unwrap_or_default();
                    if !schemas.iter().any(|(known, _)| *known == name) { return Err(format!("cannot reconstruct unknown system table schema: {name}")); }
                }
            }
            Catalog::default()
        }
    };
    if catalog.all_tables().is_empty() {
        for entry in std::fs::read_dir(&cfg.data_dir).map_err(|e| e.to_string())? {
            let path = entry.map_err(|e| e.to_string())?.path();
            if path.extension().and_then(|s| s.to_str()) == Some("db") {
                let name = path.file_stem().map(|s| s.to_string_lossy().into_owned()).unwrap_or_default();
                if !schemas.iter().any(|(known, _)| *known == name) { return Err(format!("cannot reconstruct unknown system table schema: {name}")); }
            }
        }
    }
    for (name, columns) in &schemas {
        let file = cfg.data_dir.join(format!("{name}.db"));
        if let Some(schema) = catalog.describe(name).filter(|s| !file.exists() && s.row_count > 0) {
            missing_data.push((*name, schema.row_count));
        }
        if repair && !file.exists() && catalog.describe(name).is_none_or(|s| s.row_count != 0) {
            return Err(format!("repair would require initializing or recovering data in {name}"));
        }
        if catalog.describe(name).is_none() { catalog.create_table(name, columns.clone()).map_err(|e| e.to_string())?; }
        else { catalog.set_columns(name, columns.clone()).map_err(|e| e.to_string())?; }
        catalog.mark_system(name).map_err(|e| e.to_string())?;
    }
    catalog.save(&catalog_path).map_err(|e| e.to_string())?;
    let server = Server::new(cfg).map_err(|e| e.to_string())?;
    if repair {
        let scan = server.wal.read_all().map_err(|e| e.to_string())?;
        let checkpoint = server.wal.read_checkpoint().map_err(|e| e.to_string())?.unwrap_or(0);
        if !committed_ops(&records_after(&scan.records, checkpoint)).is_empty() { return Err("repair would require replaying WAL data".into()); }
    } else {
        server.recover_wal().map_err(|e| e.to_string())?;
    }
    for (name, _) in &schemas { server.with_table(name, |t| t.flush()).map_err(|e| e.to_string())?; }
    if !repair {
        if mode == "recover" && server.load_accounts().map_err(|e| e.to_string())?.is_empty() && hash.is_empty() {
            return Err("recover requires --password-stdin to initialize a missing administrator".into());
        }
        server.bootstrap_system(Some(user), Some(hash)).map_err(|e| e.to_string())?;
        server.accounts_request(Request::IdentityInitialize { administrator: user.into() }).map_err(|e| e.to_string())?;
        server.bootstrap_types(types.clone()).map_err(|e| e.to_string())?;
    }
    let accounts = server.load_accounts().map_err(|e| e.to_string())?;
    require_superuser(&accounts).map_err(|e| e.to_string())?;
    for (name, expected) in missing_data {
        let rows = server.with_existing_table(name, |t| t.scan()).map_err(|e| e.to_string())?;
        if (rows.len() as u64) < expected { return Err(format!("WAL cannot reconstruct all missing data in {name}; restore a backup or use reset")); }
    }
    if repair {
        let saved = server.with_existing_table(TYPES_TABLE, |t| t.scan()).map_err(|e| e.to_string())?;
        if types.iter().any(|wanted| !saved.iter().any(|row| wanted.iter().all(|(key, value)| row.get(key) == Some(value)))) {
            return Err("repair would require changing preinstalled type data".into());
        }
    }
    for (name, _) in &schemas { server.with_existing_table(name, |t| t.scan()).map_err(|e| e.to_string())?; }
    {
        let mut catalog = server.catalog.lock().map_err(|_| "catalog lock poisoned")?;
        catalog.identity_high_water = catalog.identity_high_water.max(accounts.iter().map(|a| a.id).max().unwrap_or(0));
        catalog.save(server.catalog_path()).map_err(|e| e.to_string())?;
    }
    server.rebuild_stats().map_err(|e| e.to_string())?;
    server.flush_all_tables().map_err(|e| e.to_string())?;
    Ok(())
}

/// 进程内存储句柄：一个数据目录一份状态，跨线程共享（内部各自加锁）。
pub struct Storage {
    databases: Databases,
    _directory_lock: std::fs::File,
}

impl Storage {
    /// 离线修复或重建系统目录
    pub fn maintenance(config_path: Option<&str>, request: &str) -> Result<String, String> {
        let loaded = config::load(config_path)?;
        let cfg = loaded.config;
        let _lock = lock_data_directory(&cfg).map_err(|e| e.to_string())?;
        let input: serde_json::Value = serde_json::from_str(request).map_err(|e| e.to_string())?;
        let mode = input.get("mode").and_then(|v| v.as_str()).ok_or("missing maintenance mode")?;
        if !["repair", "recover", "reset"].contains(&mode) { return Err("invalid maintenance mode".into()); }
        let user = input.get("user").and_then(|v| v.as_str()).unwrap_or("root");
        let hash = input.get("password_hash").and_then(|v| v.as_str()).unwrap_or("");
        let types: Vec<Row> = serde_json::from_value(input.get("types").cloned().ok_or("missing type manifest")?).map_err(|e| e.to_string())?;
        if types.is_empty() { return Err("type manifest must not be empty".into()); }
        for row in &types { type_definition_name(row).map_err(|e| e.to_string())?; }
        let stamp = std::time::SystemTime::now().duration_since(std::time::UNIX_EPOCH).map_err(|e| e.to_string())?.as_nanos();
        let system = cfg.data_dir.join("system");
        if system.is_symlink() { return Err("system directory cannot be a symbolic link".into()); }
        let work = cfg.data_dir.join(format!("bootstrap-work-{stamp}"));
        let backup = cfg.data_dir.join(format!("system-backup-{stamp}"));
        let staged = work.join("system");
        std::fs::create_dir_all(&staged).map_err(|e| e.to_string())?;
        if mode != "reset" && system.exists() { copy_system_directory(&system, &staged).map_err(|e| e.to_string())?; }
        let mut candidate_cfg = cfg.clone();
        candidate_cfg.data_dir = staged.clone();
        let result = maintenance_candidate(candidate_cfg, mode, user, hash, types);
        if let Err(err) = result { return Err(format!("{err}; original system directory was not changed; candidate: {}. Try csql-bootstrap recover or reset", work.display())); }
        if mode == "repair" && system.exists() {
            for entry in std::fs::read_dir(&system).map_err(|e| e.to_string())? {
                let entry = entry.map_err(|e| e.to_string())?;
                if entry.path().extension().and_then(|s| s.to_str()) == Some("db")
                    && std::fs::read(entry.path()).map_err(|e| e.to_string())? != std::fs::read(staged.join(entry.file_name())).map_err(|e| e.to_string())? {
                    return Err("repair would modify table data; original system directory was not changed. Use recover or reset".into());
                }
            }
        }
        if system.exists() { std::fs::rename(&system, &backup).map_err(|e| e.to_string())?; }
        if let Err(err) = std::fs::rename(&staged, &system) {
            if backup.exists() { std::fs::rename(&backup, &system).map_err(|rollback| format!("publish failed: {err}; rollback failed: {rollback}; backup: {}", backup.display()))?; }
            return Err(format!("cannot publish repaired system directory: {err}"));
        }
        serde_json::to_string(&serde_json::json!({"status":"maintenance", "mode":mode, "backup":backup, "candidate":work,
            "message":"System maintenance completed. Business databases were preserved."})).map_err(|e| e.to_string())
    }

    /// 打开数据目录并初始化存储
    pub fn open(config_path: Option<&str>) -> Result<Storage, String> {
        let loaded = config::load(config_path)?;
        let directory_lock = lock_data_directory(&loaded.config).map_err(|e| e.to_string())?;
        log::init();
        log::set_level(loaded.config.log_level);
        log_info!(core, "chusql-core-storage {} starting", env!("CARGO_PKG_VERSION"));
        log_config(&loaded);
        let databases = Databases::new(loaded.config).map_err(|e| e.to_string())?;
        Ok(Storage { databases, _directory_lock: directory_lock })
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

