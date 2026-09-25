
use std::collections::hash_map::DefaultHasher;
use std::collections::{BTreeMap, HashSet};
use std::hash::{Hash, Hasher};
use std::io;
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::protocol::{ColumnType, Row, SchemaColumn};

// 数据字典：记每张表有哪些列、什么类型、多少行、有哪些索引，以及列级统计。
//
// 统计只在内存里精确维护（每列见过哪些值的哈希，有上限），随 catalog.json 一起落盘；
// 服务启动时会扫一遍表把统计重建出来，所以重启之后数字仍然是对的。

/// 一列的不同值个数最多精确统计到这里；超过就只报"下界"
const STATS_DISTINCT_CAP: u64 = 4096;

// 表结构
#[derive(Debug, Clone, Serialize, Deserialize)]
/// 一条索引定义：只记"盯的是哪一列"（索引按列命名，一列最多一个）
pub struct IndexSchema {
    pub column: String,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
/// 一列的统计
pub struct ColumnStat {
    /// 不同值个数（`capped` 为真时是下界）
    pub distinct: u64,
    /// 不同值太多，已经不再精确统计
    #[serde(default)]
    pub capped: bool,
}

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
/// 一张表的 schema + 统计
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
/// 数据字典：表名到 schema
pub struct Catalog {
    tables: BTreeMap<String, TableSchema>,
    /// 每列"见过的值"的哈希集合（只在内存里，不落盘；有上限）
    #[serde(skip)]
    seen: BTreeMap<String, HashSet<u64>>,
}

impl Catalog {
    // 读取与保存
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

    // 查询
    /// 一张表的 schema
    pub fn describe(&self, table: &str) -> Option<&TableSchema> {
        self.tables.get(table)
    }

    /// 记录过的所有表名
    pub fn table_names(&self) -> Vec<String> {
        self.tables.keys().cloned().collect()
    }

    // 更新
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

    /// 记一次插入：补列 + 行数 +1 + 更新列统计；返回"列有没有变化"（决定要不要立刻写盘）
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
                // 这个值彻底没了才减：集合里删得掉说明没有别的行还用它
                if seen.remove(&fold(name, value_hash(v))) {
                    stat.distinct = stat.distinct.saturating_sub(1);
                }
            }
        }
    }

    /// 重数一遍统计与行数（启动时重建、整表改写完都走它）；返回"列有没有变化"
    pub fn rebuild_stats(&mut self, table: &str, rows: &[Row]) -> bool {
        self.seen.remove(table);
        {
            let entry = self.tables.entry(table.to_string()).or_default();
            entry.stats.clear();
            entry.row_count = 0;
        }
        // 逐行"插入"地数：行数、列、统计一次到位
        rows.iter()
            .fold(false, |changed, r| self.record_insert(table, r) || changed)
    }

    /// 列统计：这一列第一次见到这个值就 distinct +1（到上限之后只报下界）
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
                    seen.clear(); // 到上限就不再精确统计，把内存还回去
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

    // 索引
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

// 推断
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

/// 一个值的哈希（按它的 JSON 文本算；统计"不同值个数"用）
fn value_hash(v: &serde_json::Value) -> u64 {
    let mut h = DefaultHasher::new();
    v.to_string().hash(&mut h);
    h.finish()
}

/// 把"列名"和"值哈希"合成一个哈希：避免两列的同值互相干扰
fn fold(column: &str, value_hash: u64) -> u64 {
    let mut h = DefaultHasher::new();
    column.hash(&mut h);
    value_hash.hash(&mut h);
    h.finish()
}
