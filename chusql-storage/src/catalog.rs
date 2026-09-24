//! 数据字典：记录每张表有哪些列、什么类型。
//! 存在 data/catalog.json。insert 时自动补列，describe 时读回。

use std::collections::BTreeMap;
use std::io;
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::protocol::{ColumnType, Row, SchemaColumn};

/// 一张表的 schema。
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct TableSchema {
    pub columns: Vec<SchemaColumn>,
}

/// 整个数据字典。
#[derive(Debug, Default, Serialize, Deserialize)]
pub struct Catalog {
    tables: BTreeMap<String, TableSchema>,
}

impl Catalog {
    /// 从文件读；文件不存在返回空字典。
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

    /// 写回文件。
    pub fn save<P: AsRef<Path>>(&self, path: P) -> io::Result<()> {
        let bytes = serde_json::to_vec_pretty(self).map_err(io::Error::other)?;
        std::fs::write(path, bytes)
    }

    /// 一张表的 schema；没有就返回 None。
    pub fn describe(&self, table: &str) -> Option<&TableSchema> {
        self.tables.get(table)
    }

    /// catalog 里记录的所有表名。
    pub fn table_names(&self) -> Vec<String> {
        self.tables.keys().cloned().collect()
    }

    /// 按一行数据补列；已有的列不动。
    pub fn ensure_columns(&mut self, table: &str, row: &Row) {
        let entry = self.tables.entry(table.to_string()).or_default();
        for (k, v) in row {
            if entry.columns.iter().any(|c| &c.name == k) {
                continue;
            }
            if let Some(ty) = infer_type(v) {
                entry.columns.push(SchemaColumn {
                    name: k.clone(),
                    ty,
                });
            }
        }
    }

    /// 按多行数据补列；replace_all 用。
    pub fn ensure_columns_from_rows(&mut self, table: &str, rows: &[Row]) {
        for r in rows {
            self.ensure_columns(table, r);
        }
    }
}

/// 从 JSON 值推断列类型；不支持的类型返回 None。
fn infer_type(v: &serde_json::Value) -> Option<ColumnType> {
    match v {
        serde_json::Value::Number(_) => Some(ColumnType::Int),
        serde_json::Value::String(_) => Some(ColumnType::Str),
        serde_json::Value::Bool(_) => Some(ColumnType::Bool),
        _ => None,
    }
}
