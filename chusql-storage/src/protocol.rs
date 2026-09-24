use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// 协议：Haskell 与 Rust 之间的请求 / 响应类型。

/// 一行：列名到 JSON 值
pub type Row = HashMap<String, serde_json::Value>;

#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
/// 列类型（对应 Haskell 的 TInt / TStr / TBool）
pub enum ColumnType {
    Int,
    Str,
    Bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
/// 一列的 schema
pub struct SchemaColumn {
    pub name: String,
    pub ty: ColumnType,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "method", rename_all = "snake_case")]
/// 请求：method 字段决定类型
pub enum Request {
    Ping,
    Scan { table: String },
    Insert {
        table: String,
        row: Row,
        #[serde(default)]
        key: Option<i64>,
    },
    LookupByIndex { table: String, key: i64 },
    ReplaceAll { table: String, rows: Vec<Row> },
    ListTables,
    DescribeTable { table: String },
    CreateTable {
        table: String,
        columns: Vec<SchemaColumn>,
    },
    DropTable { table: String },
    ListCatalog
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "status", rename_all = "lowercase")]
/// 响应：status 字段决定类型
pub enum Response {
    Pong,
    Rows { rows: Vec<Row> },
    Tables { tables: Vec<String> },
    Schema {
        columns: Vec<SchemaColumn>,
        #[serde(default)]
        row_count: u64,
    },
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
}
