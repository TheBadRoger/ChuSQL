use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// 协议定义：Haskell ↔ Rust 之间那条"行分隔 JSON"消息的类型（请求 / 响应）。

/// 一行：列名到 JSON 值的映射。
pub type Row = HashMap<String, serde_json::Value>;

/// 列类型；跟 Haskell 侧 TInt / TStr / TBool 对应。
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "lowercase")]
pub enum ColumnType {
    Int,
    Str,
    Bool,
}

/// 一列的 schema。
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SchemaColumn {
    pub name: String,
    pub ty: ColumnType,
}

/// 请求；method 字段做 tag。
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "method", rename_all = "snake_case")]
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
}

/// 响应；status 字段做 tag。
#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "status", rename_all = "lowercase")]
pub enum Response {
    Pong,
    Rows { rows: Vec<Row> },
    Tables { tables: Vec<String> },
    Schema { columns: Vec<SchemaColumn> },
    Ok,
    Error { message: String },
}
