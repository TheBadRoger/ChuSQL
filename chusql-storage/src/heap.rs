use std::collections::HashSet;
use std::io;
use std::path::Path;

use crate::btree::DiskBTree;
use crate::page::PageId;
use crate::page::{Page, PageFile};
use crate::protocol::Row;

// 堆表：行按插入顺序进页，支持全表扫描、按索引取行、行级删除。
//
// 索引由**表自己维护**：有哪些索引记在数据字典里，插入时按索引的列从行里取值写进去，
// 调用方不需要知道有哪些索引，也不需要自己指定 key。

// 页内布局
/// 页头：slot_count 占 2 字节
const HEADER_SIZE: usize = 2;
/// 一个槽位占 8 字节
const SLOT_SIZE: usize = 8;
/// 槽位目录起点
const SLOT_DIR_START: usize = HEADER_SIZE;

/// 读一个小端 u16
fn read_u16(buf: &[u8], off: usize) -> u16 {
    u16::from_le_bytes([buf[off], buf[off + 1]])
}

/// 写一个小端 u16
fn write_u16(buf: &mut [u8], off: usize, v: u16) {
    buf[off..off + 2].copy_from_slice(&v.to_le_bytes());
}

/// 读一个小端 u32
fn read_u32(buf: &[u8], off: usize) -> u32 {
    u32::from_le_bytes([buf[off], buf[off + 1], buf[off + 2], buf[off + 3]])
}

/// 写一个小端 u32
fn write_u32(buf: &mut [u8], off: usize, v: u32) {
    buf[off..off + 4].copy_from_slice(&v.to_le_bytes());
}

// 索引位置
/// (页号, 槽位号) 打包成 u64
fn pack_position(page_id: PageId, slot: u16) -> u64 {
    (page_id << 16) | (slot as u64)
}

/// 从 u64 解回 (页号, 槽位号)
fn unpack_position(v: u64) -> (PageId, u16) {
    (v >> 16, (v & 0xFFFF) as u16)
}

/// 取行里某一列的整数值（不是整数就没有）
fn row_int(row: &Row, column: &str) -> Option<i64> {
    match row.get(column) {
        Some(serde_json::Value::Number(n)) => n.as_i64(),
        _ => None,
    }
}

// 页内操作
/// 只管一页里的字节
pub struct HeapPage;

impl HeapPage {
    /// 这一页有几个槽位
    pub fn slot_count(page: &Page) -> u16 {
        read_u16(&page.data, 0)
    }

    /// 改写槽位数量
    fn set_slot_count(page: &mut Page, v: u16) {
        write_u16(&mut page.data, 0, v);
    }

    /// 空闲空间下界
    fn free_start(page: &Page) -> usize {
        SLOT_DIR_START + (Self::slot_count(page) as usize) * SLOT_SIZE
    }

    /// 空闲空间上界
    fn free_end(page: &Page) -> usize {
        let n = Self::slot_count(page);
        let mut min_off = page.data.len() as u32;
        for i in 0..n as usize {
            let slot_off = SLOT_DIR_START + i * SLOT_SIZE;
            let off = read_u32(&page.data, slot_off);
            let len = read_u32(&page.data, slot_off + 4);
            if len > 0 && off < min_off {
                min_off = off;
            }
        }
        min_off as usize
    }

    /// 试插一行，放不下返回 None
    pub fn insert_tuple(page: &mut Page, tuple: &[u8]) -> Option<u16> {
        let n = Self::slot_count(page);
        let free_start = Self::free_start(page);
        let free_end = Self::free_end(page);

        if free_start + SLOT_SIZE + tuple.len() > free_end {
            return None;
        }

        let tuple_off = free_end - tuple.len();
        page.data[tuple_off..tuple_off + tuple.len()].copy_from_slice(tuple);

        let slot_off = SLOT_DIR_START + (n as usize) * SLOT_SIZE;
        write_u32(&mut page.data, slot_off, tuple_off as u32);
        write_u32(&mut page.data, slot_off + 4, tuple.len() as u32);
        Self::set_slot_count(page, n + 1);

        Some(n)
    }

    /// 删掉一个槽位：只把长度置 0，行字节原地留着（读的时候按"长度 0 = 没有这一行"处理）
    pub fn delete_tuple(page: &mut Page, slot: u16) -> bool {
        let n = Self::slot_count(page);
        if slot >= n {
            return false;
        }
        let slot_off = SLOT_DIR_START + (slot as usize) * SLOT_SIZE;
        if read_u32(&page.data, slot_off + 4) == 0 {
            return false;
        }
        write_u32(&mut page.data, slot_off + 4, 0);
        true
    }

    /// 读槽位里的行字节
    pub fn get_tuple(page: &Page, slot: u16) -> Option<Vec<u8>> {
        let n = Self::slot_count(page);
        if slot >= n {
            return None;
        }
        let slot_off = SLOT_DIR_START + (slot as usize) * SLOT_SIZE;
        let off = read_u32(&page.data, slot_off) as usize;
        let len = read_u32(&page.data, slot_off + 4) as usize;
        if len == 0 {
            return None;
        }
        Some(page.data[off..off + len].to_vec())
    }

    /// 所有槽位号
    pub fn iter_slots(page: &Page) -> Vec<u16> {
        (0..Self::slot_count(page)).collect()
    }
}

// 索引
/// 表上的一个索引：盯着一列，键是这一列的整数值
struct NamedIndex {
    column: String,
    tree: DiskBTree,
}

// 堆表
/// 一个数据文件 = 若干页；外加若干索引文件
pub struct HeapTable {
    file: PageFile,
    indexes: Vec<NamedIndex>,
}

impl HeapTable {
    /// 打开表（不带索引）
    pub fn open<P: AsRef<Path>>(path: P, page_size: usize, pool_size: usize) -> io::Result<Self> {
        Ok(HeapTable {
            file: PageFile::with_options(path, page_size, pool_size)?,
            indexes: Vec::new(),
        })
    }

    /// 打开表 + `id` 列索引（`id` 沿用老文件名 `表名.idx`）
    pub fn open_indexed<P: AsRef<Path>, Q: AsRef<Path>>(
        path: P,
        index_path: Q,
        page_size: usize,
        btree_order: usize,
        pool_size: usize,
    ) -> io::Result<Self> {
        let mut t = HeapTable {
            file: PageFile::with_options(path, page_size, pool_size)?,
            indexes: Vec::new(),
        };
        t.attach_index("id", index_path, page_size, btree_order, pool_size)?;
        Ok(t)
    }

    /// 打开表 + 任意组索引：(列名, 索引文件路径)
    pub fn open_with_indexes<P: AsRef<Path>>(
        path: P,
        indexes: &[(String, std::path::PathBuf)],
        page_size: usize,
        btree_order: usize,
        pool_size: usize,
    ) -> io::Result<Self> {
        let mut t = HeapTable {
            file: PageFile::with_options(path, page_size, pool_size)?,
            indexes: Vec::new(),
        };
        for (col, p) in indexes {
            t.attach_index(col, p, page_size, btree_order, pool_size)?;
        }
        Ok(t)
    }

    /// 挂上一棵索引树（只打开/创建文件，不填数据）
    fn attach_index<P: AsRef<Path>>(
        &mut self,
        column: &str,
        path: P,
        page_size: usize,
        btree_order: usize,
        pool_size: usize,
    ) -> io::Result<()> {
        let tree = DiskBTree::open(path, page_size, btree_order, pool_size)?;
        self.indexes.push(NamedIndex {
            column: column.to_string(),
            tree,
        });
        Ok(())
    }

    /// 这一列有索引吗
    pub fn has_index(&self, column: &str) -> bool {
        self.indexes.iter().any(|i| i.column == column)
    }

    /// 建索引：先扫一遍查重，再建树填数据。
    ///
    /// **要求这一列的整数取值唯一**，不唯一就报错、一个索引文件都不留。
    /// 这样"走索引取一行"和"全表扫一遍再筛"必然得到同一个答案；
    /// 非唯一索引要等 B+ 树支持重复键（记在项目计划的 P2-② 里）。
    pub fn build_index(
        &mut self,
        column: &str,
        path: &Path,
        page_size: usize,
        btree_order: usize,
        pool_size: usize,
    ) -> io::Result<()> {
        if self.has_index(column) {
            return Err(io::Error::new(
                io::ErrorKind::AlreadyExists,
                format!("index on column \"{}\" already exists", column),
            ));
        }

        // 第 1 步：扫全表，查重并攒下 (键, 位置)
        let mut seen: HashSet<i64> = HashSet::new();
        let mut entries: Vec<(i64, u64)> = Vec::new();
        for (page_id, slot, r) in self.scan_with_positions()? {
            let Some(k) = row_int(&r, column) else {
                continue; // 这一列不是整数的行不进索引
            };
            if !seen.insert(k) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!(
                        "column \"{}\" has duplicate value {} — 暂不支持非唯一索引",
                        column, k
                    ),
                ));
            }
            entries.push((k, pack_position(page_id, slot)));
        }

        // 第 2 步：建树
        self.attach_index(column, path, page_size, btree_order, pool_size)?;
        let i = self.indexes.len() - 1;
        self.indexes[i].tree.clear()?; // 清掉可能残留的旧文件内容
        for (k, pos) in entries {
            self.indexes[i].tree.insert(k, pos)?;
        }
        Ok(())
    }

    /// 摘掉一个索引（不删文件）
    pub fn detach_index(&mut self, column: &str) -> bool {
        match self.indexes.iter().position(|i| i.column == column) {
            None => false,
            Some(i) => {
                self.indexes.remove(i);
                true
            }
        }
    }

    /// 插入一行：进堆表，并按每个索引的列顺手写索引
    pub fn insert_row(&mut self, row: &Row) -> io::Result<()> {
        self.insert_row_returning_position(row).map(|_| ())
    }

    /// 插入一行并返回它的位置
    pub fn insert_row_returning_position(&mut self, row: &Row) -> io::Result<(PageId, u16)> {
        // 先把键算好并查重：**查重必须在落堆之前**，
        // 否则某个索引写不进去时行已经进堆了，表和索引就不一致了。
        let mut pending: Vec<(usize, i64)> = Vec::new();
        for (i, idx) in self.indexes.iter_mut().enumerate() {
            let Some(k) = row_int(row, &idx.column) else {
                continue; // 这一列不是整数（或没这一列）：这行不进这个索引
            };
            if idx.tree.get(k)?.is_some() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("duplicate key {} on indexed column \"{}\"", k, idx.column),
                ));
            }
            pending.push((i, k));
        }

        let (page_id, slot) = self.insert_returning_position(row)?;
        let pos = pack_position(page_id, slot);
        for (i, k) in pending {
            self.indexes[i].tree.insert(k, pos)?;
        }
        Ok((page_id, slot))
    }

    /// 批量插入（只写缓冲池，落盘由调用方 `flush` 一次完成）
    pub fn insert_rows(&mut self, rows: &[Row]) -> io::Result<()> {
        for r in rows {
            self.insert_row(r)?;
        }
        Ok(())
    }

    /// 插入一行，只进堆表、不碰索引
    fn insert_returning_position(&mut self, row: &Row) -> io::Result<(PageId, u16)> {
        let tuple = serde_json::to_vec(row).map_err(io::Error::other)?;

        if HEADER_SIZE + SLOT_SIZE + tuple.len() > self.file.page_size() {
            return Err(io::Error::other(format!(
                "tuple too large: {} bytes",
                tuple.len()
            )));
        }

        let n = self.file.num_pages()?;
        if n > 0 {
            let last_id = n - 1;
            let mut slot = None;
            self.file.update_page(last_id, |p| {
                slot = HeapPage::insert_tuple(p, &tuple);
            })?;
            if let Some(s) = slot {
                return Ok((last_id, s));
            }
        }

        let mut p = self.file.append_page()?;
        let page_id = p.id;
        let slot = HeapPage::insert_tuple(&mut p, &tuple).expect("pre-checked fits");
        self.file.write_page(&p)?;
        Ok((page_id, slot))
    }

    /// 按 `id` 取一行
    pub fn get_by_key(&mut self, key: i64) -> io::Result<Option<Row>> {
        self.get_by_column_key("id", key)
    }

    /// 按某一列的索引取一行
    pub fn get_by_column_key(&mut self, column: &str, key: i64) -> io::Result<Option<Row>> {
        let Some(idx) = self.indexes.iter_mut().find(|i| i.column == column) else {
            return Ok(None);
        };
        let Some(pos) = idx.tree.get(key)? else {
            return Ok(None);
        };
        let (page_id, slot) = unpack_position(pos);
        self.read_at(page_id, slot)
    }

    /// 按位置读一行
    pub fn read_at(&mut self, page_id: PageId, slot: u16) -> io::Result<Option<Row>> {
        let bytes = self.file.with_page(page_id, |p| HeapPage::get_tuple(p, slot))?;
        match bytes {
            None => Ok(None),
            Some(bytes) => {
                let row: Row = serde_json::from_slice(&bytes).map_err(io::Error::other)?;
                Ok(Some(row))
            }
        }
    }

    /// 按位置删一行：槽位长度置 0，从所有索引里摘掉，并把这一行交出来
    pub fn delete_at(&mut self, page_id: PageId, slot: u16) -> io::Result<Option<Row>> {
        // 先读出来：删完就不知道这行各列是什么，索引也就无从摘起
        let row = self.read_at(page_id, slot)?;
        let existed = self.file.update_page(page_id, |p| HeapPage::delete_tuple(p, slot))?;
        if !existed {
            return Ok(None);
        }
        if let Some(r) = &row {
            for idx in &mut self.indexes {
                if let Some(k) = row_int(r, &idx.column) {
                    idx.tree.delete(k)?;
                }
            }
        }
        Ok(row)
    }

    /// 按 `id` 批量删行，返回被删掉的那些行（数据字典要用它更新统计）。
    ///
    /// 定位靠 `id` 索引；没有这个索引就退化成一趟全表扫描（仍然只删目标行）。
    pub fn delete_by_keys(&mut self, keys: &[i64]) -> io::Result<Vec<Row>> {
        let mut positions: Vec<(PageId, u16)> = Vec::new();

        if self.has_index("id") {
            for &k in keys {
                let pos = self
                    .indexes
                    .iter_mut()
                    .find(|i| i.column == "id")
                    .and_then(|i| i.tree.get(k).ok().flatten());
                if let Some(p) = pos {
                    positions.push(unpack_position(p));
                }
            }
        } else {
            let mut wanted: Vec<i64> = keys.to_vec();
            wanted.sort_unstable();
            for (page_id, slot, r) in self.scan_with_positions()? {
                if let Some(k) = row_int(&r, "id") {
                    if wanted.binary_search(&k).is_ok() {
                        positions.push((page_id, slot));
                    }
                }
            }
        }

        let mut deleted: Vec<Row> = Vec::new();
        for (page_id, slot) in positions {
            if let Some(r) = self.delete_at(page_id, slot)? {
                deleted.push(r);
            }
        }
        Ok(deleted)
    }

    /// 全表扫描
    pub fn scan(&mut self) -> io::Result<Vec<Row>> {
        Ok(self
            .scan_with_positions()?
            .into_iter()
            .map(|(_, _, r)| r)
            .collect())
    }

    /// 全表扫描，同时给出每行的位置（建索引用）
    pub fn scan_with_positions(&mut self) -> io::Result<Vec<(PageId, u16, Row)>> {
        let n = self.file.num_pages()?;
        let mut out = Vec::new();
        for i in 0..n {
            let tuples = self.file.with_page(i, |p| {
                let mut v = Vec::new();
                for slot in HeapPage::iter_slots(p) {
                    if let Some(bytes) = HeapPage::get_tuple(p, slot) {
                        v.push((slot, bytes));
                    }
                }
                v
            })?;
            for (slot, bytes) in tuples {
                let row: Row = serde_json::from_slice(&bytes).map_err(io::Error::other)?;
                out.push((i, slot, row));
            }
        }
        Ok(out)
    }

    /// 整表改写并重建所有索引
    pub fn replace_all(&mut self, rows: &[Row]) -> io::Result<()> {
        self.file.truncate()?;
        for idx in &mut self.indexes {
            idx.tree.clear()?;
        }
        for r in rows {
            let (page_id, slot) = self.insert_returning_position(r)?;
            let pos = pack_position(page_id, slot);
            for idx in &mut self.indexes {
                if let Some(k) = row_int(r, &idx.column) {
                    idx.tree.insert(k, pos)?;
                }
            }
        }
        Ok(())
    }

    /// 把脏页写回（索引也一起）
    pub fn flush(&mut self) -> io::Result<()> {
        for idx in &mut self.indexes {
            idx.tree.flush()?;
        }
        self.file.flush()
    }

    // 缓冲池统计
    /// 当前缓存页数
    pub fn cached_pages(&self) -> usize { self.file.cached_pages() }
    /// 命中次数
    pub fn hits(&self) -> u64 { self.file.hits() }
    /// 未命中次数
    pub fn misses(&self) -> u64 { self.file.misses() }
    /// 命中率
    pub fn hit_rate(&self) -> f64 { self.file.hit_rate() }
}
