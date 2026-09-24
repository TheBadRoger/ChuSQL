
use std::fs::{File, OpenOptions};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::protocol::Row;

// 预写日志：先记操作并 fsync，再改数据；重启重放残留。

const OP_INSERT: u8 = 1;
const OP_REPLACE_ALL: u8 = 2;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
/// 一条待写入的操作
pub enum WalOp {
    Insert {
        table: String,
        row: Row,
        key: Option<i64>,
    },
    ReplaceAll {
        table: String,
        rows: Vec<Row>,
    },
}

/// WAL 文件
pub struct Wal {
    path: PathBuf,
}

impl Wal {
    // 读写
    /// 绑定 WAL 文件路径
    pub fn new<P: AsRef<Path>>(path: P) -> Self {
        Wal {
            path: path.as_ref().to_path_buf(),
        }
    }

    /// 日志文件路径
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// 清空 WAL
    pub fn clear(&self) -> io::Result<()> {
        if self.path.exists() {
            let f = OpenOptions::new().write(true).open(&self.path)?;
            f.set_len(0)?;
            f.sync_all()?;
        }
        Ok(())
    }

    /// 追加一条并 fsync
    pub fn append(&self, op: &WalOp) -> io::Result<()> {
        let bytes = encode(op)?;
        let mut f = OpenOptions::new()
            .create(true)
            .append(true)
            .open(&self.path)?;
        f.write_all(&bytes)?;
        f.sync_all()?;
        Ok(())
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

// 编解码
/// 把操作编成字节
fn encode(op: &WalOp) -> io::Result<Vec<u8>> {
    let (op_type, table, payload) = match op {
        WalOp::Insert { table, row, key } => {
            let p = serde_json::to_vec(&(row, key)).map_err(io::Error::other)?;
            (OP_INSERT, table.clone(), p)
        }
        WalOp::ReplaceAll { table, rows } => {
            let p = serde_json::to_vec(rows).map_err(io::Error::other)?;
            (OP_REPLACE_ALL, table.clone(), p)
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
        OP_INSERT => {
            let (row, key): (Row, Option<i64>) = serde_json::from_slice(payload)
                .map_err(|e| format!("bad insert payload: {}", e))?;
            WalOp::Insert { table, row, key }
        }
        OP_REPLACE_ALL => {
            let rows: Vec<Row> = serde_json::from_slice(payload)
                .map_err(|e| format!("bad replace_all payload: {}", e))?;
            WalOp::ReplaceAll { table, rows }
        }
        other => return Err(format!("unknown op type: {}", other)),
    };
    Ok(Some(op))
}
