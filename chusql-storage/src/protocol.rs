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

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
/// 一列的统计（随数据字典一起走）
pub struct ColumnStatWire {
    pub name: String,
    /// 不同值个数；`capped` 为真时它只是下界
    pub distinct: u64,
    #[serde(default)]
    pub capped: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
/// 一条索引定义
pub struct IndexWire {
    pub column: String,
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "method", rename_all = "snake_case")]
/// 请求：method 字段决定类型
pub enum Request {
    Ping,
    Scan {
        table: String,
    },
    /// 插一行；要不要写索引由表上的索引决定（调用方不用管）
    Insert {
        table: String,
        row: Row,
    },
    /// 一批行一起插：一次 WAL、一次 fsync
    InsertBatch {
        table: String,
        rows: Vec<Row>,
    },
    /// 按 id 批量删行
    DeleteKeys {
        table: String,
        keys: Vec<i64>,
    },
    /// 按某一列的索引取一行（列名默认 "id"，兼容老客户端）
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
    /// 给某一列建索引（要求这一列整数取值唯一）
    CreateIndex {
        table: String,
        column: String,
    },
    /// 去掉某一列的索引
    DropIndex {
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
/// 响应：status 字段决定类型
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
    /// 这一列没有索引 —— 调用方应该退回全表扫描
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
