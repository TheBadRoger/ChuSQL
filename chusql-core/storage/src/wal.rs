use std::fs::{File, OpenOptions};
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};
use std::sync::Mutex;

use serde::{Deserialize, Serialize};

use crate::protocol::{Account, Row, SchemaColumn, USERS_TABLE};

/// 一帧的位置：LSN、操作、起止字节偏移
type Frame = (u64, WalOp, usize, usize);

// 预写日志：先记操作并 fsync，再改数据；重启按 LSN 重放已提交的组。

const OP_INSERT: u8 = 1;
const OP_REPLACE_ALL: u8 = 2;
const OP_INSERT_BATCH: u8 = 3;
const OP_DELETE_KEYS: u8 = 4;
const OP_DROP_COLUMN: u8 = 5;
const OP_ACCOUNTS: u8 = 6;
const OP_REPLACE_SCHEMA: u8 = 7;
const OP_HIDE_COLUMN: u8 = 8;
const OP_COMPACT: u8 = 9;
const OP_COMMIT: u8 = 10;
const OP_CREATE_TABLE: u8 = 11;
const OP_DROP_TABLE: u8 = 12;
const OP_CREATE_INDEX: u8 = 13;
const OP_DROP_INDEX: u8 = 14;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum WalOp {
    HideColumn { table: String, column: String },
    Compact { table: String, rows: Vec<Row> },
    Accounts { accounts: Vec<Account>, removed: Vec<String>, migrate_roles: bool },
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
    CreateTable {
        table: String,
        columns: Vec<SchemaColumn>,
    },
    DropTable {
        table: String,
    },
    CreateIndex {
        table: String,
        column: String,
    },
    DropIndex {
        table: String,
        column: String,
    },
    /// 一组写操作的提交标记；没有标记的组算未提交
    Commit,
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

#[derive(Debug, Serialize, Deserialize)]
struct CreateTablePayload {
    columns: Vec<SchemaColumn>,
}

pub struct Wal {
    path: PathBuf,
    handle: Mutex<Option<File>>,
    next_lsn: Mutex<u64>,
}

/// 一次扫描的结果：记录与完好字节长度
pub struct WalScan {
    pub records: Vec<(u64, WalOp)>,
    pub good_len: usize,
}

impl Wal {
    /// 绑定 WAL 文件路径
    pub fn new<P: AsRef<Path>>(path: P) -> Self {
        Wal {
            path: path.as_ref().to_path_buf(),
            handle: Mutex::new(None),
            next_lsn: Mutex::new(0),
        }
    }

    /// 日志文件路径
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// 检查点文件路径
    pub fn checkpoint_path(&self) -> PathBuf {
        self.path.with_extension("checkpoint")
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

    /// 把一段记录写到文件尾
    fn write_at_end(&self, bytes: &[u8]) -> io::Result<()> {
        self.with_handle(|f| {
            f.seek(SeekFrom::End(0))?;
            f.write_all(bytes)?;
            f.sync_data()
        })
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

    /// 截到指定长度（去掉撕裂的尾巴）
    pub fn truncate_to(&self, len: usize) -> io::Result<()> {
        self.with_handle(|f| {
            f.set_len(len as u64)?;
            f.seek(SeekFrom::Start(0))?;
            f.sync_data()
        })
    }

    /// 丢掉 LSN 不大于边界的帧，其余记录原样留下
    ///
    /// 边界以内的记录代表「已经落盘」，所以整段前缀可以丢弃；做法是把尾巴
    /// 写进同目录的临时文件再改名，崩在中间也不会留下半新半旧的文件。
    pub fn truncate_before(&self, lsn: u64) -> io::Result<()> {
        let (frames, good_len) = self.parse()?;
        let keep = frames
            .iter()
            .find(|(frame_lsn, _, _, _)| *frame_lsn > lsn)
            .map(|(_, _, start, _)| *start);
        match keep {
            None => self.clear(),
            Some(0) => Ok(()),
            Some(start) => self.rewrite_tail(start, good_len),
        }
    }

    /// 丢掉 LSN 大于边界的帧（未提交的尾巴），前缀原样留下
    pub fn truncate_after(&self, lsn: u64) -> io::Result<()> {
        let (frames, good_len) = self.parse()?;
        let keep = frames
            .iter()
            .find(|(frame_lsn, _, _, _)| *frame_lsn > lsn)
            .map(|(_, _, start, _)| *start)
            .unwrap_or(good_len);
        self.truncate_to(keep.min(good_len))
    }

    /// 把 [start, end) 一段写到文件头
    fn rewrite_tail(&self, start: usize, end: usize) -> io::Result<()> {
        // 常驻句柄认的是旧文件，改名后必须重新打开
        *self.handle.lock().unwrap() = None;
        let bytes = std::fs::read(&self.path)?;
        let tail = bytes.get(start..end).unwrap_or(&[]);
        let mut temp = self.path.clone().into_os_string();
        temp.push(".tmp");
        let temp = PathBuf::from(temp);
        {
            let mut f = File::create(&temp)?;
            f.write_all(tail)?;
            f.sync_all()?;
        }
        std::fs::rename(&temp, &self.path)
    }

    /// 追加一条并 fsync
    pub fn append(&self, op: &WalOp) -> io::Result<()> {
        let bytes = self.encode_next(op)?;
        self.write_at_end(&bytes)
    }

    /// 追加一组操作与提交标记，返回提交 LSN
    pub fn append_group(&self, ops: &[WalOp]) -> io::Result<u64> {
        let mut frames = Vec::new();
        for op in ops {
            frames.extend_from_slice(&self.encode_next(op)?);
        }
        let commit_lsn = self.take_lsn()?;
        frames.extend_from_slice(&encode(&WalOp::Commit, commit_lsn)?);
        self.write_at_end(&frames)?;
        Ok(commit_lsn)
    }

    /// 读当前 WAL 的第一条操作；空则 None
    pub fn read(&self) -> io::Result<Option<WalOp>> {
        let scan = self.read_all()?;
        Ok(scan.records.into_iter().find_map(|(_, op)| {
            if op == WalOp::Commit {
                None
            } else {
                Some(op)
            }
        }))
    }

    /// 读全部记录，撕裂或损坏的尾巴按长度截断
    pub fn read_all(&self) -> io::Result<WalScan> {
        let (frames, good_len) = self.parse()?;
        Ok(WalScan {
            records: frames
                .into_iter()
                .map(|(lsn, op, _, _)| (lsn, op))
                .collect(),
            good_len,
        })
    }

    /// 解析整个文件：每帧的 LSN、操作与字节范围，外加完好长度
    fn parse(&self) -> io::Result<(Vec<Frame>, usize)> {
        if !self.path.exists() {
            return Ok((Vec::new(), 0));
        }
        let mut f = File::open(&self.path)?;
        let mut buf = Vec::new();
        f.read_to_end(&mut buf)?;
        let mut frames = Vec::new();
        let mut offset = 0usize;
        while offset + 4 <= buf.len() {
            let body_len =
                u32::from_le_bytes([buf[offset], buf[offset + 1], buf[offset + 2], buf[offset + 3]])
                    as usize;
            let end = offset + 4 + body_len;
            if end > buf.len() {
                break;
            }
            match decode_body(&buf[offset + 4..end]) {
                Ok((lsn, op)) => {
                    frames.push((lsn, op, offset, end));
                    offset = end;
                }
                Err(error) => return Err(io::Error::new(io::ErrorKind::InvalidData, error)),
            }
        }
        Ok((frames, offset))
    }

    /// 记录检查点：到此为止的 LSN 都已落盘
    pub fn write_checkpoint(&self, lsn: u64) -> io::Result<()> {
        let checkpoint = self.checkpoint_path();
        let pending = checkpoint.with_extension("checkpoint.pending");
        let mut f = File::create(&pending)?;
        f.write_all(&lsn.to_le_bytes())?;
        f.sync_all()?;
        drop(f);
        std::fs::rename(pending, checkpoint)
    }

    /// 读检查点 LSN
    pub fn read_checkpoint(&self) -> io::Result<Option<u64>> {
        let path = self.checkpoint_path();
        if !path.exists() {
            return Ok(None);
        }
        let bytes = std::fs::read(&path)?;
        if bytes.len() != 8 {
            return Err(io::Error::new(io::ErrorKind::InvalidData, "invalid WAL checkpoint length"));
        }
        let mut raw = [0u8; 8];
        raw.copy_from_slice(&bytes[..8]);
        Ok(Some(u64::from_le_bytes(raw)))
    }

    /// 取下一条 LSN
    ///
    /// 号从检查点与文件尾里较大的那个续，因此日志被按边界截断之后 LSN 也不回头。
    fn take_lsn(&self) -> io::Result<u64> {
        let mut slot = self.next_lsn.lock().unwrap();
        if *slot == 0 {
            let mut base = self.read_checkpoint()?.unwrap_or(0);
            let (frames, _) = self.parse()?;
            if let Some((lsn, _, _, _)) = frames.last() {
                base = base.max(*lsn);
            }
            *slot = base.checked_add(1).ok_or_else(|| io::Error::other("WAL LSN exhausted"))?;
        }
        let lsn = *slot;
        *slot = slot.checked_add(1).ok_or_else(|| io::Error::other("WAL LSN exhausted"))?;
        Ok(lsn)
    }

    /// 编一条带新 LSN 的记录
    fn encode_next(&self, op: &WalOp) -> io::Result<Vec<u8>> {
        let lsn = self.take_lsn()?;
        encode(op, lsn)
    }
}

/// 已提交的记录：提交标记前的整组；无标记的单条按旧格式算已提交
pub fn committed_ops(records: &[(u64, WalOp)]) -> Vec<(u64, WalOp)> {
    let mut out = Vec::new();
    let mut pending: Vec<(u64, WalOp)> = Vec::new();
    for (lsn, op) in records {
        if *op == WalOp::Commit {
            out.append(&mut pending);
        } else {
            pending.push((*lsn, op.clone()));
        }
    }
    if pending.len() == 1 {
        out.append(&mut pending);
    }
    out
}

/// 只留下检查点之后的记录：边界以内的已经落盘，不必重放
pub fn records_after(records: &[(u64, WalOp)], checkpoint: u64) -> Vec<(u64, WalOp)> {
    records
        .iter()
        .filter(|(lsn, _)| *lsn > checkpoint)
        .cloned()
        .collect()
}

/// 清空文件并把读写位置归零
fn truncate(f: &mut File) -> io::Result<()> {
    f.set_len(0)?;
    f.seek(SeekFrom::Start(0))?;
    Ok(())
}

/// 把操作编成字节
fn encode(op: &WalOp, lsn: u64) -> io::Result<Vec<u8>> {
    let (op_type, table, payload) = match op {
        WalOp::HideColumn { table, column } => (OP_HIDE_COLUMN, table.clone(),
            serde_json::to_vec(column).map_err(io::Error::other)?),
        WalOp::Compact { table, rows } => {
            let p = serde_json::to_vec(rows).map_err(io::Error::other)?;
            (OP_COMPACT, table.clone(), p)
        }
        WalOp::Accounts { accounts, removed, migrate_roles } => (OP_ACCOUNTS, USERS_TABLE.to_string(),
            serde_json::to_vec(&serde_json::json!({"accounts":accounts,"removed":removed,"migrate_roles":migrate_roles})).map_err(io::Error::other)?),
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
        WalOp::CreateTable { table, columns } => (
            OP_CREATE_TABLE,
            table.clone(),
            serde_json::to_vec(&CreateTablePayload { columns: columns.clone() })
                .map_err(io::Error::other)?,
        ),
        WalOp::DropTable { table } => (OP_DROP_TABLE, table.clone(), Vec::new()),
        WalOp::CreateIndex { table, column } => (
            OP_CREATE_INDEX,
            table.clone(),
            serde_json::to_vec(column).map_err(io::Error::other)?,
        ),
        WalOp::DropIndex { table, column } => (
            OP_DROP_INDEX,
            table.clone(),
            serde_json::to_vec(column).map_err(io::Error::other)?,
        ),
        WalOp::Commit => (OP_COMMIT, String::new(), Vec::new()),
    };

    let table_bytes = table.as_bytes();

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

/// 从一个帧体解出 LSN 与操作
fn decode_body(body: &[u8]) -> Result<(u64, WalOp), String> {
    if body.len() < 8 + 1 + 2 {
        return Err("body too short".into());
    }
    let lsn = u64::from_le_bytes([
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
        OP_COMMIT => WalOp::Commit,
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
            let value: serde_json::Value = serde_json::from_slice(payload).map_err(|e| format!("bad account payload: {e}"))?;
            let (accounts, removed, migrate_roles) = if value.is_array() {
                (serde_json::from_value(value).map_err(|e| format!("bad account payload: {e}"))?, Vec::new(), false)
            } else {
                (serde_json::from_value(value.get("accounts").cloned().ok_or("missing account payload")?).map_err(|e| format!("bad account payload: {e}"))?,
                    serde_json::from_value(value.get("removed").cloned().ok_or("missing identity cleanup")?).map_err(|e| format!("bad account cleanup: {e}"))?,
                    value.get("migrate_roles").and_then(serde_json::Value::as_bool).ok_or("missing role migration flag")?)
            };
            WalOp::Accounts { accounts, removed, migrate_roles }
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
        OP_CREATE_TABLE => {
            let p: CreateTablePayload = serde_json::from_slice(payload)
                .map_err(|e| format!("bad create_table payload: {e}"))?;
            WalOp::CreateTable {
                table,
                columns: p.columns,
            }
        }
        OP_DROP_TABLE => WalOp::DropTable { table },
        OP_CREATE_INDEX => {
            let column: String = serde_json::from_slice(payload)
                .map_err(|e| format!("bad create_index payload: {e}"))?;
            WalOp::CreateIndex { table, column }
        }
        OP_DROP_INDEX => {
            let column: String = serde_json::from_slice(payload)
                .map_err(|e| format!("bad drop_index payload: {e}"))?;
            WalOp::DropIndex { table, column }
        }
        other => return Err(format!("unknown op type: {}", other)),
    };
    Ok((lsn, op))
}
