//! 简化版 WAL：写前日志 + 崩溃恢复。
//!
//! 条目格式：
//!   [u32 body_len][body]
//!   body = [u64 LSN][u8 操作类型][u16 表名长度][表名字节][JSON 载荷]
//!
//! 简化版约束：
//!   - 最多只有一条未完成的操作。写成功就清空 WAL。
//!   - 不做 checkpoint / redo / undo / 事务。
//!
//! 崩溃恢复：
//!   - 启动时若 WAL 非空，重放那一条，然后清空。

use std::fs::{File, OpenOptions};
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

use crate::protocol::Row;

/// 操作类型标记。
const OP_INSERT: u8 = 1;
const OP_REPLACE_ALL: u8 = 2;

/// 一条 WAL 操作。
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
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

/// WAL 文件句柄。
pub struct Wal {
    path: PathBuf,
}

impl Wal {
    /// 绑定到 WAL 文件路径；首次 append 时自动创建文件。
    pub fn new<P: AsRef<Path>>(path: P) -> Self {
        Wal {
            path: path.as_ref().to_path_buf(),
        }
    }

    /// 日志文件路径。
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// 清空 WAL；文件不存在则什么都不做。
    pub fn clear(&self) -> io::Result<()> {
        if self.path.exists() {
            let f = OpenOptions::new().write(true).open(&self.path)?;
            f.set_len(0)?;
            f.sync_all()?;
        }
        Ok(())
    }

    /// 追加一条操作并 fsync。
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

    /// 读取当前 WAL；空文件 / 不存在返回 None。
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

/// 把一条操作编成字节。
fn encode(op: &WalOp) -> io::Result<Vec<u8>> {
    // 先编出 (类型, 表名, 载荷)
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

    // body = [u64 LSN][u8 类型][u16 表名长度][表名][载荷]
    // 简化版 LSN 固定为 1；完整版会改成真正递增。
    let lsn: u64 = 1;
    let mut body = Vec::with_capacity(8 + 1 + 2 + table_bytes.len() + payload.len());
    body.extend_from_slice(&lsn.to_le_bytes());
    body.push(op_type);
    body.extend_from_slice(&(table_bytes.len() as u16).to_le_bytes());
    body.extend_from_slice(table_bytes);
    body.extend_from_slice(&payload);

    // 整条 = [u32 body_len][body]
    let mut out = Vec::with_capacity(4 + body.len());
    out.extend_from_slice(&(body.len() as u32).to_le_bytes());
    out.extend_from_slice(&body);
    Ok(out)
}

/// 解析一条 WAL 条目。
///
/// - `Ok(Some(op))`：成功解析出操作
/// - `Ok(None)`：尾部被截断（写入一半就断电），按"没有这条"处理
/// - `Err(_)`：字节流本身损坏，抛错
fn decode(buf: &[u8]) -> Result<Option<WalOp>, String> {
    if buf.len() < 4 {
        return Ok(None);
    }
    let body_len = u32::from_le_bytes([buf[0], buf[1], buf[2], buf[3]]) as usize;
    if buf.len() < 4 + body_len {
        // 尾部被截断——忽略
        return Ok(None);
    }
    let body = &buf[4..4 + body_len];
    if body.len() < 8 + 1 + 2 {
        return Err("body too short".into());
    }
    // LSN 简化版忽略
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
