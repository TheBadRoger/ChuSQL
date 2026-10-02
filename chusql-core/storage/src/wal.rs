use std::fs::{File, OpenOptions};
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use serde::{Deserialize, Serialize};

use crate::protocol::{Account, Row, SchemaColumn, USERS_TABLE};

// 预写日志：先记操作并 fsync，再改数据；重启重放残留。

const OP_INSERT: u8 = 1;
const OP_REPLACE_ALL: u8 = 2;
const OP_INSERT_BATCH: u8 = 3;
const OP_DELETE_KEYS: u8 = 4;
const OP_DROP_COLUMN: u8 = 5;
const OP_ACCOUNTS: u8 = 6;
const OP_REPLACE_SCHEMA: u8 = 7;
const OP_HIDE_COLUMN: u8 = 8;
const OP_COMPACT: u8 = 9;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum WalOp {
    HideColumn { table: String, column: String },
    Compact { table: String, rows: Vec<Row> },
    Accounts { accounts: Vec<Account> },
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
    ReplaceAll {
        table: String,
        rows: Vec<Row>,
    },
    DropColumn {
        table: String,
        column: String,
        rows: Vec<Row>,
    },
    ReplaceSchema {
        table: String,
        columns: Vec<SchemaColumn>,
        rows: Vec<Row>,
    },
}

#[derive(Debug, Serialize, Deserialize)]
struct DropColumnPayload {
    column: String,
    rows: Vec<Row>,
}

#[derive(Debug, Serialize, Deserialize)]
struct ReplaceSchemaPayload {
    columns: Vec<SchemaColumn>,
    rows: Vec<Row>,
}

pub struct Wal {
    path: PathBuf,
    handle: Mutex<Option<File>>,
}

impl Wal {
    /// 绑定 WAL 文件路径
    pub fn new<P: AsRef<Path>>(path: P) -> Self {
        Wal {
            path: path.as_ref().to_path_buf(),
            handle: Mutex::new(None),
        }
    }

    /// 日志文件路径
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// 拿常驻句柄（没有就开一个）
    fn with_handle<R>(&self, f: impl FnOnce(&mut File) -> io::Result<R>) -> io::Result<R> {
        let mut slot = self.handle.lock().unwrap();
        if slot.is_none() {
            *slot = Some(
                OpenOptions::new()
                    .read(true)
                    .write(true)
                    .create(true)
                    .truncate(false)
                    .open(&self.path)?,
            );
        }
        f(slot.as_mut().expect("just opened"))
    }

    /// 截断到空并落盘
    pub fn clear(&self) -> io::Result<()> {
        self.with_handle(|f| {
            truncate(f)?;
            f.sync_all()
        })
    }

    /// 只截断，落盘交给紧随的 append
    pub fn truncate(&self) -> io::Result<()> {
        self.with_handle(truncate)
    }

    /// 追加一条并 fsync
    pub fn append(&self, op: &WalOp) -> io::Result<()> {
        let bytes = encode(op)?;
        self.with_handle(|f| {
            f.write_all(&bytes)?;
            f.sync_data()
        })
    }

    /// 读当前 WAL；空则 None
    pub fn read(&self) -> io::Result<Option<WalOp>> {
        if !self.path.exists() {
            return Ok(None);
        }
        let mut f = File::open(&self.path)?;
        let mut buf = Vec::new();
        f.read_to_end(&mut buf)?;
        if buf.is_empty() {
            return Ok(None);
        }
        match decode(&buf) {
            Ok(opt) => Ok(opt),
            Err(e) => Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!("corrupt WAL: {}", e),
            )),
        }
    }
}

/// 清空文件并把读写位置归零
fn truncate(f: &mut File) -> io::Result<()> {
    f.set_len(0)?;
    f.seek(SeekFrom::Start(0))?;
    Ok(())
}

/// 把操作编成字节
fn encode(op: &WalOp) -> io::Result<Vec<u8>> {
    let (op_type, table, payload) = match op {
        WalOp::HideColumn { table, column } => (OP_HIDE_COLUMN, table.clone(),
            serde_json::to_vec(column).map_err(io::Error::other)?),
        WalOp::Compact { table, rows } => {
            let p = serde_json::to_vec(rows).map_err(io::Error::other)?;
            (OP_COMPACT, table.clone(), p)
        }
        WalOp::Accounts { accounts } => (OP_ACCOUNTS, USERS_TABLE.to_string(),
            serde_json::to_vec(accounts).map_err(io::Error::other)?),
        WalOp::Insert { table, row } => {
            let p = serde_json::to_vec(row).map_err(io::Error::other)?;
            (OP_INSERT, table.clone(), p)
        }
        WalOp::InsertBatch { table, rows } => {
            let p = serde_json::to_vec(rows).map_err(io::Error::other)?;
            (OP_INSERT_BATCH, table.clone(), p)
        }
        WalOp::DeleteKeys { table, keys } => {
            let p = serde_json::to_vec(keys).map_err(io::Error::other)?;
            (OP_DELETE_KEYS, table.clone(), p)
        }
        WalOp::ReplaceAll { table, rows } => {
            let p = serde_json::to_vec(rows).map_err(io::Error::other)?;
            (OP_REPLACE_ALL, table.clone(), p)
        }
        WalOp::DropColumn { table, column, rows } => {
            let p = serde_json::to_vec(&DropColumnPayload {
                column: column.clone(),
                rows: rows.clone(),
            })
            .map_err(io::Error::other)?;
            (OP_DROP_COLUMN, table.clone(), p)
        }
        WalOp::ReplaceSchema { table, columns, rows } => {
            let p = serde_json::to_vec(&ReplaceSchemaPayload {
                columns: columns.clone(),
                rows: rows.clone(),
            })
            .map_err(io::Error::other)?;
            (OP_REPLACE_SCHEMA, table.clone(), p)
        }
    };

    let table_bytes = table.as_bytes();

    let lsn: u64 = 1;
    let mut body = Vec::with_capacity(8 + 1 + 2 + table_bytes.len() + payload.len());
    body.extend_from_slice(&lsn.to_le_bytes());
    body.push(op_type);
    body.extend_from_slice(&(table_bytes.len() as u16).to_le_bytes());
    body.extend_from_slice(table_bytes);
    body.extend_from_slice(&payload);

    let mut out = Vec::with_capacity(4 + body.len());
    out.extend_from_slice(&(body.len() as u32).to_le_bytes());
    out.extend_from_slice(&body);
    Ok(out)
}

/// 从字节解回操作
fn decode(buf: &[u8]) -> Result<Option<WalOp>, String> {
    if buf.len() < 4 {
        return Ok(None);
    }
    let body_len = u32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]) as usize;
    if buf.len() < 4 + body_len {
        return Ok(None);
    }
    let body = &buf[4..4 + body_len];
    if body.len() < 8 + 1 + 2 {
        return Err("body too short".into());
    }
    let _lsn = u64::from_le_bytes([
        body[0], body[1], body[2], body[3], body[4], body[5], body[6], body[7],
    ]);
    let op_type = body[8];
    let table_len = u16::from_le_bytes([body[9], body[10]]) as usize;
    if body.len() < 11 + table_len {
        return Err("table name truncated".into());
    }
    let table = String::from_utf8(body[11..11 + table_len].to_vec())
        .map_err(|e| format!("bad utf8 table name: {}", e))?;
    let payload = &body[11 + table_len..];

    let op = match op_type {
        OP_HIDE_COLUMN => {
            let column = serde_json::from_slice(payload)
                .map_err(|e| format!("bad hide_column payload: {e}"))?;
            WalOp::HideColumn { table, column }
        }
        OP_COMPACT => {
            let rows: Vec<Row> = serde_json::from_slice(payload)
                .map_err(|e| format!("bad compact payload: {e}"))?;
            WalOp::Compact { table, rows }
        }
        OP_ACCOUNTS => {
            if table != USERS_TABLE { return Err("invalid account WAL table".into()); }
            let accounts = serde_json::from_slice(payload)
                .map_err(|e| format!("bad account payload: {e}"))?;
            WalOp::Accounts { accounts }
        }
        OP_INSERT => {
            let row: Row =
                serde_json::from_slice(payload).map_err(|e| format!("bad insert payload: {}", e))?;
            WalOp::Insert { table, row }
        }
        OP_INSERT_BATCH => {
            let rows: Vec<Row> = serde_json::from_slice(payload)
                .map_err(|e| format!("bad insert_batch payload: {}", e))?;
            WalOp::InsertBatch { table, rows }
        }
        OP_DELETE_KEYS => {
            let keys: Vec<i64> = serde_json::from_slice(payload)
                .map_err(|e| format!("bad delete_keys payload: {}", e))?;
            WalOp::DeleteKeys { table, keys }
        }
        OP_REPLACE_ALL => {
            let rows: Vec<Row> = serde_json::from_slice(payload)
                .map_err(|e| format!("bad replace_all payload: {}", e))?;
            WalOp::ReplaceAll { table, rows }
        }
        OP_DROP_COLUMN => {
            let p: DropColumnPayload = serde_json::from_slice(payload)
                .map_err(|e| format!("bad drop_column payload: {}", e))?;
            WalOp::DropColumn {
                table,
                column: p.column,
                rows: p.rows,
            }
        }
        OP_REPLACE_SCHEMA => {
            let p: ReplaceSchemaPayload = serde_json::from_slice(payload)
                .map_err(|e| format!("bad replace_schema payload: {}", e))?;
            WalOp::ReplaceSchema {
                table,
                columns: p.columns,
                rows: p.rows,
            }
        }
        other => return Err(format!("unknown op type: {}", other)),
    };
    Ok(Some(op))
}
