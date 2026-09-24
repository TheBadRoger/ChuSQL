
use std::collections::BTreeMap;
use std::io;
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::protocol::{ColumnType, Row, SchemaColumn};

// 数据字典：记每张表有哪些列、什么类型、多少行。

#[derive(Debug, Clone, Default, Serialize, Deserialize)]
/// 一张表的 schema
pub struct TableSchema {
    pub columns: Vec<SchemaColumn>,
    #[serde(default)]
    pub row_count: u64,
}

#[derive(Debug, Default, Serialize, Deserialize)]
/// 数据字典：表名到 schema
pub struct Catalog {
    tables: BTreeMap<String, TableSchema>,
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
            },
        );
        Ok(())
    }

    /// 按多行数据补列
    pub fn ensure_columns_from_rows(&mut self, table: &str, rows: &[Row]) {
        for r in rows {
            self.ensure_columns(table, r);
        }
    }
    /// 记一次插入：补列 + 行数 +1
    pub fn record_insert(&mut self, table: &str, row: &Row) -> bool {
        let entry = self.tables.entry(table.to_string()).or_default();
        let mut changed = false;
        for (k, v) in row {
            if entry.columns.iter().any(|c| &c.name == k) {
                continue;
            }
            if let Some(ty) = infer_type(v) {
                entry.columns.push(SchemaColumn { name: k.clone(), ty });
                changed = true;
            }
        }
        entry.row_count += 1;
        changed
    }


    /// 记一次整表替换：列照补，行数重置
    pub fn record_replace_all(&mut self, table: &str, rows: &[Row]) -> bool {
        let entry = self.tables.entry(table.to_string()).or_default();
        let mut changed = false;
        for r in rows {
            for (k, v) in r {
                if entry.columns.iter().any(|c| &c.name == k) {
                    continue;
                }
                if let Some(ty) = infer_type(v) {
                    entry.columns.push(SchemaColumn { name: k.clone(), ty });
                    changed = true;
                }
            }
        }
        entry.row_count = rows.len() as u64;
        changed
    }

    /// 删表
    pub fn drop_table(&mut self, table: &str) -> io::Result<()> {
        if self.tables.remove(table).is_none() {
            return Err(io::Error::new(
                io::ErrorKind::NotFound,
                format!("unknown table: {}", table),
            ));
        }
        Ok(())
    }
}

// 推断
/// 由 JSON 值猜列类型
fn infer_type(v: &serde_json::Value) -> Option<ColumnType> {
    match v {
        serde_json::Value::Number(_) => Some(ColumnType::Int),
        serde_json::Value::String(_) => Some(ColumnType::Str),
        serde_json::Value::Bool(_) => Some(ColumnType::Bool),
        _ => None,
    }
}
