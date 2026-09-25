use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// 协议：Haskell 与 Rust 之间的请求 / 响应类型。

pub type Row = HashMap<String, serde_json::Value>;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum ColumnType {
    Int,
    Str,
    Bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SchemaColumn {
    pub name: String,
    pub ty: ColumnType,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ColumnStatWire {
    pub name: String,
    pub distinct: u64,
    #[serde(default)]
    pub capped: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct IndexWire {
    pub column: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "method", rename_all = "snake_case")]
pub enum Request {
    Ping,
    Scan {
        table: String,
    },
    Insert {
        table: String,
        row: Row,
    },
    InsertBatch {
        table: String,
        rows: Vec<Row>,
    },
    DeleteKeys {
        table: String,
        keys: Vec<i64>,
    },
    LookupByIndex {
        table: String,
        #[serde(default = "default_id_column")]
        column: String,
        key: i64,
    },
    ReplaceAll {
        table: String,
        rows: Vec<Row>,
    },
    ListTables,
    DescribeTable {
        table: String,
    },
    CreateTable {
        table: String,
        columns: Vec<SchemaColumn>,
    },
    DropTable {
        table: String,
    },
    ListCatalog,
    CreateIndex {
        table: String,
        column: String,
    },
    DropIndex {
        table: String,
        column: String,
    },
    DropColumn {
        table: String,
        column: String,
    },
}

/// 没写 column 时按 `id` 算
fn default_id_column() -> String {
    "id".to_string()
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "status", rename_all = "snake_case")]
pub enum Response {
    Pong,
    Rows { rows: Vec<Row> },
    Tables { tables: Vec<String> },
    Schema {
        table: String,
        columns: Vec<SchemaColumn>,
        #[serde(default)]
        row_count: u64,
        #[serde(default)]
        indexes: Vec<IndexWire>,
        #[serde(default)]
        stats: Vec<ColumnStatWire>,
    },
    NoIndex,
    Ok,
    Error { message: String },
    Catalog { schemas: Vec<TableSchemaWire> },
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TableSchemaWire {
    pub table: String,
    pub columns: Vec<SchemaColumn>,
    #[serde(default)]
    pub row_count: u64,
    #[serde(default)]
    pub indexes: Vec<IndexWire>,
    #[serde(default)]
    pub stats: Vec<ColumnStatWire>,
}
