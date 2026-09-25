use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, HashSet};
use std::hash::{Hash, Hasher};
use std::io;
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::protocol::{ColumnType, Row, SchemaColumn};

// 数据字典：记每张表有哪些列、什么类型、多少行、有哪些索引，以及列级统计。

const STATS_DISTINCT_CAP: u64 = 4096;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct IndexSchema {
    pub column: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ColumnStat {
    pub distinct: u64,
    #[serde(default)]
    pub capped: bool,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TableSchema {
    pub columns: Vec<SchemaColumn>,
    #[serde(default)]
    pub row_count: u64,
    #[serde(default)]
    pub indexes: Vec<IndexSchema>,
    #[serde(default)]
    pub stats: BTreeMap<String, ColumnStat>,
}

#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Catalog {
    tables: BTreeMap<String, TableSchema>,
    #[serde(skip)]
    seen: BTreeMap<String, HashSet<u64>>,
}

impl Catalog {
    /// 从文件读；不存在给空字典
    pub fn load<P: AsRef<Path>>(path: P) -> io::Result<Self> {
        if !path.as_ref().exists() {
            return Ok(Catalog::default());
        }
        let bytes = std::fs::read(path)?;
        if bytes.is_empty() {
            return Ok(Catalog::default());
        }
        serde_json::from_slice(&bytes).map_err(io::Error::other)
    }

    /// 返回所有表的 (表名, schema)。
    pub fn all_tables(&self) -> Vec<(&str, &TableSchema)> {
        self.tables.iter().map(|(k, v)| (k.as_str(), v)).collect()
    }

    /// 写回文件
    pub fn save<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let bytes = serde_json::to_vec_pretty(self).map_err(io::Error::other)?;
        std::fs::write(path, bytes)
    }

    /// 一张表的 schema
    pub fn describe(&self, table: &str) -> Option<&TableSchema> {
        self.tables.get(table)
    }

    /// 记录过的所有表名
    pub fn table_names(&self) -> Vec<String> {
        self.tables.keys().cloned().collect()
    }

    /// 按一行数据补列
    pub fn ensure_columns(&mut self, table: &str, row: &Row) {
        let entry = self.tables.entry(table.to_string()).or_default();
        add_missing_columns(entry, row);
    }

    /// 建表；已存在报错
    pub fn create_table(&mut self, table: &str, columns: Vec<SchemaColumn>) -> io::Result<()> {
        if self.tables.contains_key(table) {
            return Err(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!("table already exists: {}", table),
            ));
        }
        self.tables.insert(
            table.to_string(),
            TableSchema {
                columns,
                row_count: 0,
                indexes: Vec::new(),
                stats: BTreeMap::new(),
            },
        );
        Ok(())
    }

    /// 记一次插入，返回列有没有变化
    pub fn record_insert(&mut self, table: &str, row: &Row) -> bool {
        let changed = {
            let entry = self.tables.entry(table.to_string()).or_default();
            let changed = add_missing_columns(entry, row);
            entry.row_count += 1;
            changed
        };
        self.count_values(table, row);
        changed
    }

    /// 记一次删除：行数减少 + 更新列统计
    pub fn record_delete(&mut self, table: &str, rows: &[Row]) {
        let entry = self.tables.entry(table.to_string()).or_default();
        entry.row_count = entry.row_count.saturating_sub(rows.len() as u64);
        let seen = self.seen.entry(table.to_string()).or_default();
        for r in rows {
            for (name, v) in r {
                let stat = entry.stats.entry(name.clone()).or_default();
                if stat.capped {
                    continue;
                }
                if seen.remove(&fold(name, value_hash(v))) {
                    stat.distinct = stat.distinct.saturating_sub(1);
                }
            }
        }
    }

    /// 重数一遍统计与行数
    pub fn rebuild_stats(&mut self, table: &str, rows: &[Row]) -> bool {
        self.seen.remove(table);
        {
            let entry = self.tables.entry(table.to_string()).or_default();
            entry.stats.clear();
            entry.row_count = 0;
        }
        rows.iter()
            .fold(false, |changed, r| self.record_insert(table, r) || changed)
    }

    /// 这一列第一次见到这个值就加一
    fn count_values(&mut self, table: &str, row: &Row) {
        let entry = self.tables.entry(table.to_string()).or_default();
        let seen = self.seen.entry(table.to_string()).or_default();
        for (name, v) in row {
            let stat = entry.stats.entry(name.clone()).or_default();
            if stat.capped {
                continue;
            }
            if seen.insert(fold(name, value_hash(v))) {
                stat.distinct += 1;
                if stat.distinct >= STATS_DISTINCT_CAP {
                    stat.capped = true;
                    seen.clear();
                }
            }
        }
    }

    /// 删表
    pub fn drop_table(&mut self, table: &str) -> io::Result<()> {        if self.tables.remove(table).is_none() {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("unknown table: {}", table),
            ));
        }
        self.seen.remove(table);
        Ok(())
    }

    /// 删一列；之前没有也返回成功（重放幂等）
    pub fn remove_column(&mut self, table: &str, column: &str) -> io::Result<bool> {
        let entry = self
            .tables
            .get_mut(table)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, format!("unknown table: {}", table)))?;
        let had_column = {
            let before = entry.columns.len();
            entry.columns.retain(|c| c.name != column);
            entry.columns.len() != before
        };
        entry.stats.remove(column);
        entry.indexes.retain(|i| i.column != column);
        self.seen.remove(table);
        Ok(had_column)
    }

    /// 记下一条索引定义
    pub fn add_index(&mut self, table: &str, column: &str) -> io::Result<()> {
        let entry = self
            .tables
            .get_mut(table)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, format!("unknown table: {}", table)))?;
        if entry.indexes.iter().any(|i| i.column == column) {
            return Err(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!("index on column \"{}\" already exists", column),
            ));
        }
        entry.indexes.push(IndexSchema {
            column: column.to_string(),
        });
        Ok(())
    }

    /// 去掉一条索引定义
    pub fn remove_index(&mut self, table: &str, column: &str) -> io::Result<()> {
        let entry = self
            .tables
            .get_mut(table)
            .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, format!("unknown table: {}", table)))?;
        match entry.indexes.iter().position(|i| i.column == column) {
            None => Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("no index on column \"{}\"", column),
            )),
            Some(i) => {
                entry.indexes.remove(i);
                Ok(())
            }
        }
    }
}

/// 把行里"字典还不知道的列"补进来；返回有没有变化
fn add_missing_columns(entry: &mut TableSchema, row: &Row) -> bool {
    let mut changed = false;
    for (k, v) in row {
        if entry.columns.iter().any(|c| &c.name == k) {
            continue;
        }
        if let Some(ty) = infer_type(v) {
            entry.columns.push(SchemaColumn {
                name: k.clone(),
                ty,
            });
            changed = true;
        }
    }
    changed
}

/// 由 JSON 值猜列类型
fn infer_type(v: &serde_json::Value) -> Option<ColumnType> {
    match v {
        serde_json::Value::Number(_) => Some(ColumnType::Int),
        serde_json::Value::String(_) => Some(ColumnType::Str),
        serde_json::Value::Bool(_) => Some(ColumnType::Bool),
        _ => None,
    }
}

/// 一个值的哈希，统计不同值个数用
fn value_hash(v: &serde_json::Value) -> u64 {
    let mut h = DefaultHasher::new();
    v.to_string().hash(&mut h);
    h.finish()
}

/// 列名与值哈希合成一个哈希
fn fold(column: &str, value_hash: u64) -> u64 {
    let mut h = DefaultHasher::new();
    column.hash(&mut h);
    value_hash.hash(&mut h);
    h.finish()
}
