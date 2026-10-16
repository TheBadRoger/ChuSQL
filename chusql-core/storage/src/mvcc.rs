use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::fs::File;
use std::io::{self, Write};
use std::path::{Path, PathBuf};
use std::sync::{Condvar, Mutex};
use std::time::{Duration, Instant, SystemTime, UNIX_EPOCH};

use serde::{Deserialize, Serialize};

use crate::heap::{row_key, scalar_int};
use crate::protocol::{Row, StorageOp, TableSchemaWire};

// 持久化行版本、快照引用与事务行锁；等待锁时释放状态互斥量。

const LOCK_TIMEOUT: Duration = Duration::from_secs(5);

type TableRows = Vec<(String, Vec<Row>)>;

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Version {
    begin: u64,
    end: Option<u64>,
    value_begin: u64,
    order: u64,
    row: Row,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct History {
    schema: TableSchemaWire,
    changed: u64,
    live: bool,
    rows: BTreeMap<u64, Vec<Version>>,
}

#[derive(Clone, Debug, Serialize, Deserialize)]
struct Durable {
    format: u32,
    csn: u64,
    next_row: u64,
    tables: BTreeMap<u64, History>,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
enum Key {
    Table(u64),
    Row(u64, u64),
    Unique(u64, String, Vec<u8>),
}

struct Transaction {
    snapshot: u64,
    schemas: Vec<TableSchemaWire>,
    serializable: bool,
    reads: BTreeSet<u64>,
    held: BTreeMap<Key, bool>,
    waiting: HashSet<String>,
    aborted: Option<String>,
    plan: Vec<StorageOp>,
}

struct State {
    durable: Durable,
    transactions: HashMap<String, Transaction>,
    next_transaction: u64,
}

pub(crate) struct Mvcc {
    path: PathBuf,
    nonce: String,
    state: Mutex<State>,
    changed: Condvar,
}

/// 只选出快照可见且按逻辑行序排列的版本
fn visible(history: &History, csn: u64) -> Vec<(u64, &Version)> {
    let mut rows: Vec<_> = history
        .rows
        .iter()
        .filter_map(|(id, chain)| {
            chain
                .iter()
                .rev()
                .find(|v| v.begin <= csn && v.end.is_none_or(|end| csn < end))
                .map(|v| (*id, v))
        })
        .collect();
    rows.sort_by_key(|(id, version)| (version.order, *id));
    rows
}

/// 规范化标量唯一值，保持整值浮点等键
fn unique_key(row: &Row, column: &str) -> Option<Vec<u8>> {
    let value = row.get(column)?;
    if value.is_null() {
        return None;
    }
    if let Some(key) = row_key(row, column) {
        return Some(key);
    }
    match value {
        serde_json::Value::Bool(value) => Some(vec![b'b', u8::from(*value)]),
        serde_json::Value::Number(value) => value
            .as_f64()
            .map(|n| [vec![b'f'], n.to_bits().to_be_bytes().to_vec()].concat()),
        _ => None,
    }
}

struct Change {
    table: u64,
    wanted: Vec<(Option<u64>, Row)>,
    removed: BTreeSet<u64>,
}

/// 将事务最终行集映射到快照中的稳定行编号
fn plans(state: &State, token: &str, ops: &[StorageOp]) -> io::Result<Vec<Change>> {
    let tx = state
        .transactions
        .get(token)
        .ok_or_else(|| io::Error::other("transaction no longer exists"))?;
    let mut edited: BTreeMap<u64, Vec<Row>> = BTreeMap::new();
    for op in ops {
        let name = match op {
            StorageOp::Upsert { table, .. }
            | StorageOp::Delete { table, .. }
            | StorageOp::Replace { table, .. } => table,
        };
        let schema = tx
            .schemas
            .iter()
            .find(|schema| schema.table == *name)
            .ok_or_else(|| io::Error::other("unknown transaction table"))?;
        let history =
            state.durable.tables.get(&schema.object_id).ok_or_else(|| {
                io::Error::other("serialization_failure: transaction table removed")
            })?;
        let rows = edited.entry(schema.object_id).or_insert_with(|| {
            visible(history, tx.snapshot)
                .into_iter()
                .map(|(_, v)| v.row.clone())
                .collect()
        });
        match op {
            StorageOp::Replace { rows: incoming, .. } => *rows = incoming.clone(),
            StorageOp::Upsert { rows: incoming, .. } => rows.extend(incoming.iter().cloned()),
            StorageOp::Delete { ids, .. } => {
                let mut keys: HashSet<_> = ids.iter().copied().collect();
                rows.retain(|row| {
                    !row.get("id")
                        .and_then(scalar_int)
                        .is_some_and(|key| keys.remove(&key))
                });
            }
        }
    }
    edited
        .into_iter()
        .map(|(table, rows)| {
            let old = visible(&state.durable.tables[&table], tx.snapshot);
            let mut used = HashSet::new();
            let mut wanted = Vec::new();
            for row in rows {
                let identity = unique_key(&row, "id");
                let prior = old.iter().find(|(id, v)| {
                    !used.contains(id)
                        && (v.row == row
                            || identity
                                .as_ref()
                                .is_some_and(|key| unique_key(&v.row, "id").as_ref() == Some(key)))
                });
                if let Some((id, _)) = prior {
                    used.insert(*id);
                    wanted.push((Some(*id), row));
                } else {
                    wanted.push((None, row));
                }
            }
            // 无业务键的更新按剩余行序配对，保留内部身份。
            let mut unmatched = old
                .iter()
                .filter(|(id, v)| !used.contains(id) && unique_key(&v.row, "id").is_none());
            for (id, row) in &mut wanted {
                if id.is_none()
                    && unique_key(row, "id").is_none()
                    && let Some((old_id, _)) = unmatched.next()
                {
                    *id = Some(*old_id);
                }
            }
            let retained: HashSet<_> = wanted.iter().filter_map(|(id, _)| *id).collect();
            let removed = old
                .iter()
                .map(|(id, _)| *id)
                .filter(|id| !retained.contains(id))
                .collect();
            Ok(Change {
                table,
                wanted,
                removed,
            })
        })
        .collect()
}

/// 将本事务行改动合并到当前版本而不覆盖他人修改
fn merged_ops(state: &State, token: &str) -> io::Result<Vec<StorageOp>> {
    let tx = state
        .transactions
        .get(token)
        .ok_or_else(|| io::Error::other("transaction no longer exists"))?;
    let mut result = Vec::new();
    for change in plans(state, token, &tx.plan)? {
        let history = &state.durable.tables[&change.table];
        let base: BTreeMap<_, _> = visible(history, tx.snapshot).into_iter().collect();
        let mut current: Vec<_> = visible(history, state.durable.csn)
            .into_iter()
            .map(|(id, v)| (id, v.row.clone()))
            .collect();
        let mut removed = change.removed.clone();
        let mut appended = Vec::new();
        for (id, row) in &change.wanted {
            if id.is_none_or(|id| base.get(&id).is_none_or(|v| v.row != *row)) {
                if let Some(id) = id {
                    removed.insert(*id);
                }
                appended.push(row.clone());
            }
        }
        if removed.is_empty() && appended.is_empty() {
            continue;
        }
        let victims: Vec<_> = current
            .iter()
            .filter(|(id, _)| removed.contains(id))
            .map(|(_, row)| row.clone())
            .collect();
        current.retain(|(id, _)| !removed.contains(id));
        let integers: Option<Vec<_>> = victims
            .iter()
            .map(|row| row.get("id").and_then(scalar_int))
            .collect();
        if let Some(ids) = integers.filter(|ids| {
            ids.iter().collect::<HashSet<_>>().len() == ids.len()
                && current.iter().all(|(_, row)| {
                    row.get("id")
                        .and_then(scalar_int)
                        .is_none_or(|id| !ids.contains(&id))
                })
        }) {
            if !ids.is_empty() {
                result.push(StorageOp::Delete {
                    table: history.schema.table.clone(),
                    ids,
                });
            }
            if !appended.is_empty() {
                result.push(StorageOp::Upsert {
                    table: history.schema.table.clone(),
                    rows: appended,
                });
            }
        } else {
            let mut rows: Vec<_> = current.into_iter().map(|(_, row)| row).collect();
            rows.extend(appended);
            result.push(StorageOp::Replace {
                table: history.schema.table.clone(),
                rows,
            });
        }
    }
    Ok(result)
}

/// 保留最老活跃快照仍可见的行版本
fn prune(state: &mut State) {
    let oldest = state
        .transactions
        .values()
        .filter(|tx| tx.aborted.is_none())
        .map(|tx| tx.snapshot)
        .min()
        .unwrap_or(state.durable.csn);
    for history in state.durable.tables.values_mut() {
        history.rows.retain(|_, chain| {
            chain.retain(|version| version.end.is_none_or(|end| end > oldest));
            !chain.is_empty()
        });
    }
    state
        .durable
        .tables
        .retain(|_, history| history.live || !history.rows.is_empty());
}

/// 查找从等待者回到自身的等待路径
fn cycle(state: &State, current: &str, target: &str, visited: &mut HashSet<String>) -> bool {
    if !visited.insert(current.to_owned()) {
        return false;
    }
    state.transactions.get(current).is_some_and(|tx| {
        tx.waiting
            .iter()
            .any(|owner| owner == target || cycle(state, owner, target, visited))
    })
}

/// 中止事务并释放其全部锁及等待边
fn abort(state: &mut State, token: &str, error: &str) {
    if let Some(tx) = state.transactions.get_mut(token) {
        tx.aborted = Some(error.to_owned());
        tx.held.clear();
        tx.waiting.clear();
    }
    for tx in state.transactions.values_mut() {
        tx.waiting.remove(token);
    }
}

/// 复查对象、行版本以及串行化读取依赖
fn validate(state: &State, token: &str, writes: bool) -> io::Result<()> {
    let tx = state
        .transactions
        .get(token)
        .ok_or_else(|| io::Error::other("transaction no longer exists"))?;
    if let Some(error) = &tx.aborted {
        return Err(io::Error::other(error.clone()));
    }
    for (key, _) in tx.held.iter().filter(|(_, exclusive)| **exclusive) {
        let table = match key {
            Key::Table(id) | Key::Row(id, _) | Key::Unique(id, _, _) => id,
        };
        let expected = tx
            .schemas
            .iter()
            .find(|schema| schema.object_id == *table)
            .ok_or_else(|| io::Error::other("transaction schema missing"))?;
        let current =
            state.durable.tables.get(table).ok_or_else(|| {
                io::Error::other("serialization_failure: transaction object changed")
            })?;
        if !current.live
            || current.schema.schema_version != expected.schema_version
            || current.schema.columns != expected.columns
        {
            return Err(io::Error::other(
                "serialization_failure: transaction schema changed",
            ));
        }
        match key {
            Key::Row(_, id) => {
                let chain = current
                    .rows
                    .get(id)
                    .ok_or_else(|| io::Error::other("serialization_failure: row was removed"))?;
                let before = chain
                    .iter()
                    .rev()
                    .find(|v| v.begin <= tx.snapshot && v.end.is_none_or(|end| tx.snapshot < end));
                let latest = chain.last().filter(|v| v.end.is_none());
                if before.map(|v| v.value_begin) != latest.map(|v| v.value_begin) {
                    return Err(io::Error::other(
                        "serialization_failure: row version changed",
                    ));
                }
            }
            Key::Table(_) if current.changed > tx.snapshot => {
                return Err(io::Error::other("serialization_failure: table changed"));
            }
            _ => (),
        }
    }
    if writes && tx.serializable {
        for id in &tx.reads {
            if state
                .durable
                .tables
                .get(id)
                .is_none_or(|table| !table.live || table.changed > tx.snapshot)
            {
                return Err(io::Error::other(
                    "serialization_failure: serializable read changed",
                ));
            }
        }
    }
    Ok(())
}

impl Mvcc {
    /// 打开版本目录，未提交事务不会跨进程存活
    pub(crate) fn open(directory: &Path) -> io::Result<Self> {
        let path = directory.join("mvcc.json");
        let durable = match std::fs::read(&path) {
            Ok(bytes) => serde_json::from_slice::<Durable>(&bytes).map_err(io::Error::other)?,
            Err(error) if error.kind() == io::ErrorKind::NotFound => Durable {
                format: 1,
                csn: 0,
                next_row: 0,
                tables: BTreeMap::new(),
            },
            Err(error) => return Err(error),
        };
        if durable.format != 1
            || durable.tables.values().any(|table| {
                table.rows.iter().any(|(id, chain)| {
                    *id > durable.next_row
                        || chain.windows(2).any(|pair| pair[0].begin >= pair[1].begin)
                        || chain.iter().any(|v| {
                            v.begin > durable.csn
                                || v.end.is_some_and(|end| end <= v.begin || end > durable.csn)
                        })
                })
            })
        {
            return Err(io::Error::other("invalid MVCC version catalog"));
        }
        let nonce = format!(
            "{:x}-{}",
            SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map_err(io::Error::other)?
                .as_nanos(),
            std::process::id()
        );
        Ok(Self {
            path,
            nonce,
            state: Mutex::new(State {
                durable,
                transactions: HashMap::new(),
                next_transaction: 0,
            }),
            changed: Condvar::new(),
        })
    }

    /// 数据同步后原子发布版本目录并同步目录项
    fn persist(&self, durable: &Durable) -> io::Result<()> {
        let temporary = self.path.with_extension("pending");
        let mut file = File::create(&temporary)?;
        serde_json::to_writer(&mut file, durable).map_err(io::Error::other)?;
        file.flush()?;
        file.sync_all()?;
        drop(file);
        std::fs::rename(&temporary, &self.path)?;
        #[cfg(unix)]
        File::open(
            self.path
                .parent()
                .ok_or_else(|| io::Error::other("MVCC parent missing"))?,
        )?
        .sync_all()?;
        Ok(())
    }

    /// 检查当前版本是否已覆盖提交点
    pub(crate) fn csn(&self) -> io::Result<u64> {
        Ok(self
            .state
            .lock()
            .map_err(|_| io::Error::other("MVCC lock poisoned"))?
            .durable
            .csn)
    }

    /// 将提交后的表行转换成稳定编号与版本链
    pub(crate) fn publish(
        &self,
        csn: u64,
        tables: Vec<(TableSchemaWire, Vec<Row>)>,
    ) -> io::Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| io::Error::other("MVCC lock poisoned"))?;
        if csn < state.durable.csn {
            return Err(io::Error::other("MVCC commit sequence moved backwards"));
        }
        let mut next = state.durable.clone();
        let current_ids: HashSet<_> = tables.iter().map(|(schema, _)| schema.object_id).collect();
        for (schema, rows) in tables {
            let history = next
                .tables
                .entry(schema.object_id)
                .or_insert_with(|| History {
                    schema: schema.clone(),
                    changed: csn,
                    live: true,
                    rows: BTreeMap::new(),
                });
            let old: Vec<_> = visible(history, next.csn)
                .into_iter()
                .map(|(id, v)| (id, v.clone()))
                .collect();
            let mut used = HashSet::new();
            let mut changed = history.schema.columns != schema.columns
                || history.schema.schema_version != schema.schema_version;
            for (position, row) in rows.into_iter().enumerate() {
                let identity = unique_key(&row, "id");
                let prior = old.iter().find(|(id, v)| {
                    !used.contains(id)
                        && (v.row == row
                            || identity
                                .as_ref()
                                .is_some_and(|key| unique_key(&v.row, "id").as_ref() == Some(key)))
                });
                let order = position as u64;
                if let Some((id, previous)) = prior {
                    used.insert(*id);
                    if previous.row == row && previous.order == order {
                        continue;
                    }
                    if previous.begin == csn {
                        return Err(io::Error::other(
                            "MVCC replay does not match published version",
                        ));
                    }
                    let chain = history
                        .rows
                        .get_mut(id)
                        .ok_or_else(|| io::Error::other("MVCC row missing"))?;
                    chain
                        .last_mut()
                        .ok_or_else(|| io::Error::other("MVCC chain empty"))?
                        .end = Some(csn);
                    let value_begin = if previous.row == row {
                        previous.value_begin
                    } else {
                        changed = true;
                        csn
                    };
                    chain.push(Version {
                        begin: csn,
                        end: None,
                        value_begin,
                        order,
                        row,
                    });
                } else {
                    next.next_row = next
                        .next_row
                        .checked_add(1)
                        .ok_or_else(|| io::Error::other("MVCC row id exhausted"))?;
                    history.rows.insert(
                        next.next_row,
                        vec![Version {
                            begin: csn,
                            end: None,
                            value_begin: csn,
                            order,
                            row,
                        }],
                    );
                    changed = true;
                }
            }
            for (id, previous) in &old {
                if !used.contains(id) {
                    if previous.begin == csn {
                        return Err(io::Error::other("MVCC replay removed a published row"));
                    }
                    history
                        .rows
                        .get_mut(id)
                        .and_then(|chain| chain.last_mut())
                        .ok_or_else(|| io::Error::other("MVCC row missing"))?
                        .end = Some(csn);
                    changed = true;
                }
            }
            history.schema = schema;
            history.live = true;
            if changed {
                history.changed = csn;
            }
        }
        for (id, history) in &mut next.tables {
            if !current_ids.contains(id) {
                history.live = false;
                for chain in history.rows.values_mut() {
                    if let Some(last) = chain.last_mut().filter(|v| v.end.is_none()) {
                        last.end = Some(csn);
                    }
                }
                history.changed = csn;
            }
        }
        next.csn = csn;
        let oldest = state
            .transactions
            .values()
            .filter(|tx| tx.aborted.is_none())
            .map(|tx| tx.snapshot)
            .min()
            .unwrap_or(csn);
        for history in next.tables.values_mut() {
            history.rows.retain(|_, chain| {
                chain.retain(|v| v.end.is_none_or(|end| end > oldest));
                !chain.is_empty()
            });
        }
        next.tables
            .retain(|id, history| current_ids.contains(id) || !history.rows.is_empty());
        self.persist(&next)?;
        state.durable = next;
        self.changed.notify_all();
        Ok(())
    }

    /// 建立持有版本与结构引用的事务快照
    pub(crate) fn begin(
        &self,
        schemas: Vec<TableSchemaWire>,
        serializable: bool,
    ) -> io::Result<(String, TableRows, u64)> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| io::Error::other("MVCC lock poisoned"))?;
        state.next_transaction = state
            .next_transaction
            .checked_add(1)
            .ok_or_else(|| io::Error::other("transaction id exhausted"))?;
        let token = format!("{}-{}", self.nonce, state.next_transaction);
        let snapshot = state.durable.csn;
        let mut rows = Vec::new();
        for schema in &schemas {
            let history = state
                .durable
                .tables
                .get(&schema.object_id)
                .ok_or_else(|| io::Error::other("MVCC schema is not synchronized"))?;
            rows.push((
                schema.table.clone(),
                visible(history, snapshot)
                    .into_iter()
                    .map(|(_, v)| v.row.clone())
                    .collect(),
            ));
        }
        state.transactions.insert(
            token.clone(),
            Transaction {
                snapshot,
                schemas,
                serializable,
                reads: BTreeSet::new(),
                held: BTreeMap::new(),
                waiting: HashSet::new(),
                aborted: None,
                plan: Vec::new(),
            },
        );
        Ok((token, rows, snapshot))
    }

    /// 获取增量行锁并检测跨请求等待环
    pub(crate) fn lock(&self, token: &str, ops: &[StorageOp], reads: &[String]) -> io::Result<()> {
        let result = self.lock_inner(token, ops, reads);
        if let Err(error) = &result {
            let message = error.to_string();
            if message.starts_with("serialization_failure:") {
                let mut state = self
                    .state
                    .lock()
                    .map_err(|_| io::Error::other("MVCC lock poisoned"))?;
                abort(&mut state, token, &message);
                self.changed.notify_all();
            }
        }
        result
    }

    /// 锁等待期间持续复查已有写入版本
    fn lock_inner(&self, token: &str, ops: &[StorageOp], reads: &[String]) -> io::Result<()> {
        let deadline = Instant::now() + LOCK_TIMEOUT;
        let mut state = self
            .state
            .lock()
            .map_err(|_| io::Error::other("MVCC lock poisoned"))?;
        validate(&state, token, false)?;
        let tx = &state.transactions[token];
        let mut wanted = BTreeMap::new();
        let mut touched = BTreeSet::new();
        for name in reads {
            touched.insert(
                tx.schemas
                    .iter()
                    .find(|schema| schema.table == *name)
                    .ok_or_else(|| io::Error::other("unknown transaction read table"))?
                    .object_id,
            );
        }
        for change in plans(&state, token, ops)? {
            let table = change.table;
            let schema = tx
                .schemas
                .iter()
                .find(|schema| schema.object_id == table)
                .ok_or_else(|| io::Error::other("transaction schema missing"))?;
            wanted.insert(Key::Table(table), false);
            let history = &state.durable.tables[&table];
            let old: BTreeMap<_, _> = visible(history, tx.snapshot).into_iter().collect();
            let mut rows = Vec::new();
            for id in &change.removed {
                wanted.insert(Key::Row(table, *id), true);
                rows.push(&old[id].row);
            }
            for (id, row) in &change.wanted {
                if id.is_some_and(|id| old[&id].row == *row) {
                    continue;
                }
                if let Some(id) = id {
                    wanted.insert(Key::Row(table, *id), true);
                    rows.push(&old[id].row);
                }
                rows.push(row);
            }
            for row in rows {
                let mut columns: BTreeSet<_> = schema
                    .columns
                    .iter()
                    .filter(|column| column.unique || column.primary_key)
                    .map(|column| column.name.as_str())
                    .collect();
                columns.insert("id");
                for column in columns {
                    if let Some(key) = unique_key(row, column) {
                        wanted.insert(Key::Unique(table, column.to_owned(), key), true);
                    }
                }
            }
        }
        state
            .transactions
            .get_mut(token)
            .ok_or_else(|| io::Error::other("transaction missing"))?
            .reads = touched;
        for (key, exclusive) in wanted {
            loop {
                validate(&state, token, false)?;
                let blockers: HashSet<_> = state
                    .transactions
                    .iter()
                    .filter(|(other, tx)| {
                        other.as_str() != token
                            && tx.aborted.is_none()
                            && tx.held.get(&key).is_some_and(|held| *held || exclusive)
                    })
                    .map(|(other, _)| other.clone())
                    .collect();
                if blockers.is_empty() {
                    let tx = state
                        .transactions
                        .get_mut(token)
                        .ok_or_else(|| io::Error::other("transaction missing"))?;
                    tx.waiting.clear();
                    tx.held
                        .entry(key.clone())
                        .and_modify(|held| *held |= exclusive)
                        .or_insert(exclusive);
                    break;
                }
                state
                    .transactions
                    .get_mut(token)
                    .ok_or_else(|| io::Error::other("transaction missing"))?
                    .waiting = blockers;
                if cycle(&state, token, token, &mut HashSet::new()) {
                    abort(&mut state, token, "deadlock_detected: transaction aborted");
                    self.changed.notify_all();
                    return Err(io::Error::other("deadlock_detected: transaction aborted"));
                }
                let remaining = deadline.saturating_duration_since(Instant::now());
                if remaining.is_zero() {
                    abort(&mut state, token, "lock_timeout: transaction aborted");
                    self.changed.notify_all();
                    return Err(io::Error::other("lock_timeout: transaction aborted"));
                }
                let (next, _) = self
                    .changed
                    .wait_timeout(state, remaining)
                    .map_err(|_| io::Error::other("MVCC lock poisoned"))?;
                state = next;
            }
        }
        validate(&state, token, false)?;
        state
            .transactions
            .get_mut(token)
            .ok_or_else(|| io::Error::other("transaction missing"))?
            .plan = ops.to_vec();
        Ok(())
    }

    /// 返回提交校验使用的最新完整快照与合并操作
    pub(crate) fn preview(
        &self,
        token: &str,
        schemas: Vec<TableSchemaWire>,
    ) -> io::Result<(TableRows, Vec<StorageOp>)> {
        let state = self
            .state
            .lock()
            .map_err(|_| io::Error::other("MVCC lock poisoned"))?;
        let tx = state
            .transactions
            .get(token)
            .ok_or_else(|| io::Error::other("transaction no longer exists"))?;
        validate(&state, token, !tx.plan.is_empty())?;
        let ops = merged_ops(&state, token)?;
        let mut tables: BTreeMap<_, _> = schemas
            .iter()
            .map(|schema| {
                let rows = state
                    .durable
                    .tables
                    .get(&schema.object_id)
                    .map(|history| {
                        visible(history, state.durable.csn)
                            .into_iter()
                            .map(|(_, v)| v.row.clone())
                            .collect()
                    })
                    .unwrap_or_default();
                (schema.table.clone(), rows)
            })
            .collect();
        for op in &ops {
            match op {
                StorageOp::Replace { table, rows } => {
                    tables.insert(table.clone(), rows.clone());
                }
                StorageOp::Upsert { table, rows } => tables
                    .get_mut(table)
                    .ok_or_else(|| io::Error::other("transaction table missing"))?
                    .extend(rows.iter().cloned()),
                StorageOp::Delete { table, ids } => {
                    let mut keys: HashSet<_> = ids.iter().copied().collect();
                    tables
                        .get_mut(table)
                        .ok_or_else(|| io::Error::other("transaction table missing"))?
                        .retain(|row| {
                            !row.get("id")
                                .and_then(scalar_int)
                                .is_some_and(|id| keys.remove(&id))
                        });
                }
            }
        }
        Ok((tables.into_iter().collect(), ops))
    }

    /// 提交前复查版本、锁覆盖和串行化读取
    pub(crate) fn validate_commit(&self, token: &str, ops: &[StorageOp]) -> io::Result<()> {
        let state = self
            .state
            .lock()
            .map_err(|_| io::Error::other("MVCC lock poisoned"))?;
        validate(&state, token, !ops.is_empty())?;
        let tx = &state.transactions[token];
        for op in ops {
            let table = match op {
                StorageOp::Upsert { table, .. }
                | StorageOp::Delete { table, .. }
                | StorageOp::Replace { table, .. } => table,
            };
            let schema = tx
                .schemas
                .iter()
                .find(|schema| schema.table == *table)
                .ok_or_else(|| io::Error::other("unknown transaction table"))?;
            if !tx.held.contains_key(&Key::Table(schema.object_id)) {
                return Err(io::Error::other("transaction write has no locks"));
            }
        }
        Ok(())
    }

    /// 返回仍然存活的最老快照提交点
    pub(crate) fn oldest_snapshot(&self) -> io::Result<Option<u64>> {
        let state = self
            .state
            .lock()
            .map_err(|_| io::Error::other("MVCC lock poisoned"))?;
        Ok(state
            .transactions
            .values()
            .filter(|tx| tx.aborted.is_none())
            .map(|tx| tx.snapshot)
            .min())
    }

    /// 结束事务并释放全部版本引用和行锁
    pub(crate) fn finish(&self, token: &str) -> io::Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| io::Error::other("MVCC lock poisoned"))?;
        state.transactions.remove(token);
        prune(&mut state);
        for tx in state.transactions.values_mut() {
            tx.waiting.remove(token);
        }
        self.changed.notify_all();
        Ok(())
    }

    /// 存储进入恢复封闭时唤醒并中止所有等待者
    pub(crate) fn fail_all(&self) -> io::Result<()> {
        let mut state = self
            .state
            .lock()
            .map_err(|_| io::Error::other("MVCC lock poisoned"))?;
        let tokens: Vec<_> = state.transactions.keys().cloned().collect();
        for token in tokens {
            abort(&mut state, &token, "storage recovery required");
        }
        self.changed.notify_all();
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    use std::sync::Arc;

    // 验证版本可见性、并发行锁及持久化边界。

    /// 构造有主键和唯一用户名的表结构
    fn schema() -> TableSchemaWire {
        serde_json::from_value(json!({"table":"items","object_id":1,"schema_version":"1",
            "columns":[{"name":"id","ty":"Int","primary_key":true},
                       {"name":"name","ty":"String","unique":true}]}))
        .unwrap()
    }

    /// 构造两条固定身份的测试行
    fn rows(first: &str, second: &str) -> Vec<Row> {
        [json!({"id":1,"name":first}), json!({"id":2,"name":second})]
            .into_iter()
            .map(|row| serde_json::from_value(row).unwrap())
            .collect()
    }

    /// 建立最终行集计划
    fn replace(rows: Vec<Row>) -> Vec<StorageOp> {
        vec![StorageOp::Replace {
            table: "items".into(),
            rows,
        }]
    }

    /// 发布后旧快照保留且不同行修改可合并
    #[test]
    fn snapshots_merge_disjoint_rows_and_survive_restart() -> io::Result<()> {
        let directory = tempfile::tempdir()?;
        let mvcc = Mvcc::open(directory.path())?;
        mvcc.publish(1, vec![(schema(), rows("a", "b"))])?;
        let (first, snapshot, _) = mvcc.begin(vec![schema()], false)?;
        let (second, _, _) = mvcc.begin(vec![schema()], false)?;
        mvcc.lock(&first, &replace(rows("aa", "b")), &[])?;
        mvcc.lock(&second, &replace(rows("a", "bb")), &[])?;
        mvcc.publish(2, vec![(schema(), rows("aa", "b"))])?;
        mvcc.finish(&first)?;
        let (merged, ops) = mvcc.preview(&second, vec![schema()])?;
        assert_eq!(snapshot[0].1, rows("a", "b"));
        assert!(merged[0].1.contains(&rows("aa", "bb")[0]));
        assert!(merged[0].1.contains(&rows("aa", "bb")[1]));
        mvcc.validate_commit(&second, &ops)?;
        mvcc.publish(3, vec![(schema(), merged[0].1.clone())])?;
        mvcc.finish(&second)?;
        drop(mvcc);
        let reopened = Mvcc::open(directory.path())?;
        let (_, latest, csn) = reopened.begin(vec![schema()], false)?;
        assert_eq!(csn, 3);
        assert_eq!(latest, merged);
        Ok(())
    }

    /// 写锁等待释放后拒绝旧版本更新
    #[test]
    fn waiter_rechecks_version_after_owner_commits() -> io::Result<()> {
        let directory = tempfile::tempdir()?;
        let mvcc = Arc::new(Mvcc::open(directory.path())?);
        mvcc.publish(1, vec![(schema(), rows("a", "b"))])?;
        let (first, _, _) = mvcc.begin(vec![schema()], false)?;
        let (second, _, _) = mvcc.begin(vec![schema()], false)?;
        mvcc.lock(&first, &replace(rows("aa", "b")), &[])?;
        let worker = Arc::clone(&mvcc);
        let waiting = second.clone();
        let thread =
            std::thread::spawn(move || worker.lock(&waiting, &replace(rows("ab", "b")), &[]));
        let deadline = Instant::now() + Duration::from_secs(2);
        loop {
            if !mvcc.state.lock().unwrap().transactions[&second]
                .waiting
                .is_empty()
            {
                break;
            }
            assert!(Instant::now() < deadline, "waiter did not enter lock wait");
            std::thread::yield_now();
        }
        mvcc.publish(2, vec![(schema(), rows("aa", "b"))])?;
        mvcc.finish(&first)?;
        assert!(
            thread
                .join()
                .unwrap()
                .unwrap_err()
                .to_string()
                .contains("serialization_failure")
        );
        mvcc.finish(&second)?;
        Ok(())
    }

    /// 等待环中止一个事务并唤醒另一个
    #[test]
    fn deadlock_aborts_victim_and_releases_locks() -> io::Result<()> {
        let directory = tempfile::tempdir()?;
        let mvcc = Arc::new(Mvcc::open(directory.path())?);
        mvcc.publish(1, vec![(schema(), rows("a", "b"))])?;
        let (first, _, _) = mvcc.begin(vec![schema()], false)?;
        let (second, _, _) = mvcc.begin(vec![schema()], false)?;
        mvcc.lock(&first, &replace(rows("aa", "b")), &[])?;
        mvcc.lock(&second, &replace(rows("a", "bb")), &[])?;
        let worker = Arc::clone(&mvcc);
        let waiting = first.clone();
        let thread =
            std::thread::spawn(move || worker.lock(&waiting, &replace(rows("aa", "bb")), &[]));
        let deadline = Instant::now() + Duration::from_secs(2);
        loop {
            if !mvcc.state.lock().unwrap().transactions[&first]
                .waiting
                .is_empty()
            {
                break;
            }
            assert!(Instant::now() < deadline, "waiter did not enter lock wait");
            std::thread::yield_now();
        }
        assert!(
            mvcc.lock(&second, &replace(rows("aa", "bb")), &[])
                .unwrap_err()
                .to_string()
                .contains("deadlock_detected")
        );
        thread.join().unwrap()?;
        assert!(mvcc.preview(&second, vec![schema()]).is_err());
        mvcc.finish(&first)?;
        mvcc.finish(&second)?;
        Ok(())
    }

    /// 取消等待者会唤醒请求并释放其旧锁
    #[test]
    fn rollback_cancels_waiting_request() -> io::Result<()> {
        let directory = tempfile::tempdir()?;
        let mvcc = Arc::new(Mvcc::open(directory.path())?);
        mvcc.publish(1, vec![(schema(), rows("a", "b"))])?;
        let (owner, _, _) = mvcc.begin(vec![schema()], false)?;
        let (waiter, _, _) = mvcc.begin(vec![schema()], false)?;
        mvcc.lock(&owner, &replace(rows("aa", "b")), &[])?;
        let worker = Arc::clone(&mvcc);
        let waiting = waiter.clone();
        let thread =
            std::thread::spawn(move || worker.lock(&waiting, &replace(rows("ab", "b")), &[]));
        let deadline = Instant::now() + Duration::from_secs(2);
        loop {
            if !mvcc.state.lock().unwrap().transactions[&waiter]
                .waiting
                .is_empty()
            {
                break;
            }
            assert!(Instant::now() < deadline);
            std::thread::yield_now();
        }
        mvcc.finish(&waiter)?;
        assert!(
            thread
                .join()
                .unwrap()
                .unwrap_err()
                .to_string()
                .contains("no longer exists")
        );
        mvcc.finish(&owner)?;
        assert_eq!(mvcc.oldest_snapshot()?, None);
        Ok(())
    }

    /// 超时中止并释放已有行锁
    #[test]
    fn lock_timeout_aborts_transaction() -> io::Result<()> {
        let directory = tempfile::tempdir()?;
        let mvcc = Mvcc::open(directory.path())?;
        mvcc.publish(1, vec![(schema(), rows("a", "b"))])?;
        let (owner, _, _) = mvcc.begin(vec![schema()], false)?;
        let (waiter, _, _) = mvcc.begin(vec![schema()], false)?;
        mvcc.lock(&owner, &replace(rows("aa", "b")), &[])?;
        let error = mvcc
            .lock(&waiter, &replace(rows("ab", "b")), &[])
            .unwrap_err();
        assert!(error.to_string().starts_with("lock_timeout:"));
        assert!(
            mvcc.state.lock().unwrap().transactions[&waiter]
                .held
                .is_empty()
        );
        mvcc.finish(&owner)?;
        mvcc.finish(&waiter)?;
        Ok(())
    }

    /// 活跃旧快照阻止回收，结束后释放过期版本
    #[test]
    fn old_snapshot_pins_versions_until_finish() -> io::Result<()> {
        let directory = tempfile::tempdir()?;
        let mvcc = Mvcc::open(directory.path())?;
        mvcc.publish(1, vec![(schema(), rows("a", "b"))])?;
        let (token, _, _) = mvcc.begin(vec![schema()], false)?;
        mvcc.publish(2, vec![(schema(), rows("aa", "b"))])?;
        {
            let state = mvcc.state.lock().unwrap();
            assert_eq!(
                visible(&state.durable.tables[&1], 1)[0].1.row,
                rows("a", "b")[0]
            );
            assert!(
                state.durable.tables[&1]
                    .rows
                    .values()
                    .any(|chain| chain.len() == 2)
            );
        }
        mvcc.finish(&token)?;
        let state = mvcc.state.lock().unwrap();
        assert!(
            state.durable.tables[&1]
                .rows
                .values()
                .all(|chain| chain.len() == 1)
        );
        Ok(())
    }

    /// 串行化事务拒绝读依赖中的幻行
    #[test]
    fn serializable_rejects_changed_read_dependency() -> io::Result<()> {
        let directory = tempfile::tempdir()?;
        let mvcc = Mvcc::open(directory.path())?;
        mvcc.publish(1, vec![(schema(), rows("a", "b"))])?;
        let (token, _, _) = mvcc.begin(vec![schema()], true)?;
        mvcc.lock(&token, &replace(rows("aa", "b")), &["items".into()])?;
        let mut inserted = rows("a", "b");
        inserted.push(serde_json::from_value(json!({"id":3,"name":"c"})).unwrap());
        mvcc.publish(2, vec![(schema(), inserted)])?;
        assert!(
            mvcc.preview(&token, vec![schema()])
                .unwrap_err()
                .to_string()
                .contains("serializable read changed")
        );
        Ok(())
    }
}
