use serde::{Deserialize, Serialize};
use std::collections::HashMap;

// 协议：Haskell 与 Rust 之间的请求 / 响应类型。

pub type Row = HashMap<String, serde_json::Value>;

pub const USERS_TABLE: &str = "__chusql_users";

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Account {
    pub id: i64,
    pub user: String,
    pub password_hash: String,
    pub revision: u64,
    #[serde(default)]
    pub registered_at: String,
    #[serde(default)]
    pub last_login_at: Option<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SchemaColumn {
    pub name: String,
    pub ty: String,
    #[serde(default = "default_true")]
    pub nullable: bool,
    #[serde(default)]
    pub default: Option<serde_json::Value>,
    #[serde(default)]
    pub auto_increment: bool,
    #[serde(default)]
    pub primary_key: bool,
    #[serde(default)]
    pub unique: bool,
    #[serde(default)]
    pub check: Option<String>,
}

impl Default for SchemaColumn {
    fn default() -> Self {
        SchemaColumn {
            name: String::new(),
            ty: String::new(),
            nullable: true,
            default: None,
            auto_increment: false,
            primary_key: false,
            unique: false,
            check: None,
        }
    }
}

fn default_true() -> bool {
    true
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
    AccountsList,
    AccountCreate { user: String, password_hash: String },
    AccountReset { user: String, password_hash: String },
    AccountLogin {
        user: String,
        #[serde(default)]
        at: Option<String>,
    },
    AccountDrop { user: String },
    Ping,
    Scan {
        table: String,
        #[serde(default)]
        columns: Option<Vec<String>>,
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
        key: serde_json::Value,
    },
    RangeByIndex {
        table: String,
        column: String,
        #[serde(default)]
        lo: Option<serde_json::Value>,
        #[serde(default = "default_true")]
        lo_inclusive: bool,
        #[serde(default)]
        hi: Option<serde_json::Value>,
        #[serde(default = "default_true")]
        hi_inclusive: bool,
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
    Compact {
        table: String,
    },
    ReplaceSchema {
        table: String,
        columns: Vec<SchemaColumn>,
        rows: Vec<Row>,
    },
}

impl Request {
    /// 拒绝可能影响文件路径的列名
    pub fn valid_columns(&self) -> bool {
        let valid = |name: &str| !name.is_empty()
            && name.chars().all(|c| c.is_ascii_alphanumeric() || c == '_');
        match self {
            Self::CreateIndex { column, .. } | Self::DropIndex { column, .. }
            | Self::DropColumn { column, .. } | Self::LookupByIndex { column, .. }
            | Self::RangeByIndex { column, .. } => valid(column),
            Self::CreateTable { columns, .. } => columns.iter().all(|c| valid(&c.name)),
            Self::ReplaceSchema { columns, .. } => columns.iter().all(|c| valid(&c.name)),
            _ => true,
        }
    }
    /// 普通表操作的统一访问边界
    pub fn table(&self) -> Option<&str> {
        match self {
            Self::Scan { table, .. } | Self::Insert { table, .. }
            | Self::InsertBatch { table, .. } | Self::DeleteKeys { table, .. }
            | Self::LookupByIndex { table, .. } | Self::ReplaceAll { table, .. }
            | Self::RangeByIndex { table, .. }
            | Self::DescribeTable { table } | Self::CreateTable { table, .. }
            | Self::DropTable { table } | Self::CreateIndex { table, .. }
            | Self::DropIndex { table, .. } | Self::DropColumn { table, .. }
            | Self::Compact { table }
            | Self::ReplaceSchema { table, .. } => Some(table),
            Self::Ping | Self::ListTables | Self::ListCatalog | Self::AccountsList
            | Self::AccountCreate { .. } | Self::AccountReset { .. } | Self::AccountLogin { .. }
            | Self::AccountDrop { .. } => None,
        }
    }
}

pub fn reserved_table(table: &str) -> bool {
    table.to_ascii_lowercase().starts_with("__chusql_")
}

/// 没写 column 时按 `id` 算
fn default_id_column() -> String {
    "id".to_string()
}

#[derive(Debug, Serialize, Deserialize)]
#[serde(tag = "status", rename_all = "snake_case")]
pub enum Response {
    Accounts { accounts: Vec<Account> },
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
