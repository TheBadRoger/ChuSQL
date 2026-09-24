use std::io;
use std::path::Path;

use crate::btree::DiskBTree;
use crate::page::PageId;
use crate::page::{Page, PageFile};
use crate::protocol::Row;

// 堆表：行按插入顺序进页，支持全表扫描与按索引取行。

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

// 堆表
/// 一个文件 = 若干页
pub struct HeapTable {
    file: PageFile,
    index: Option<DiskBTree>,
}

impl HeapTable {
    /// 打开表（不带索引）
    pub fn open<P: AsRef<Path>>(
        path: P,
        page_size: usize,
        pool_size: usize,
    ) -> io::Result<Self> {
         Ok(HeapTable {
            file: PageFile::with_options(path, page_size, pool_size)?,
            index: None,
        })
    }

    /// 打开表 + 索引
    pub fn open_indexed<P: AsRef<Path>, Q: AsRef<Path>>(
        path: P,
        index_path: Q,
        page_size: usize,
        btree_order: usize,
        pool_size: usize,
    ) -> io::Result<Self> {
        Ok(HeapTable {
            file: PageFile::with_options(path, page_size, pool_size)?,
            index: Some(DiskBTree::open(index_path, page_size, btree_order, pool_size)?),
        })
    }

    /// 插入一行，不更新索引
    pub fn insert(&mut self, row: &Row) -> io::Result<()> {
        self.insert_returning_position(row).map(|_| ())
    }

    /// 插入并返回行位置
    pub fn insert_returning_position(&mut self, row: &Row) -> io::Result<(PageId, u16)> {
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

    /// 插入一行并写索引
    pub fn insert_keyed(&mut self, row: &Row, key: i64) -> io::Result<()> {
        let (page_id, slot) = self.insert_returning_position(row)?;
        if let Some(idx) = &mut self.index {
            idx.insert(key, pack_position(page_id, slot))?;
        }
        Ok(())
    }

    /// 按索引键取一行
    pub fn get_by_key(&mut self, key: i64) -> io::Result<Option<Row>> {
        let pos = match &mut self.index {
            None => return Ok(None),
            Some(idx) => match idx.get(key)? {
                None => return Ok(None),
                Some(p) => p,
            },
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

    /// 全表扫描
    pub fn scan(&mut self) -> io::Result<Vec<Row>> {
        let n = self.file.num_pages()?;
        let mut rows = Vec::new();
        for i in 0..n {
            let tuples = self.file.with_page(i, |p| {
                let mut out = Vec::new();
                for slot in HeapPage::iter_slots(p) {
                    if let Some(bytes) = HeapPage::get_tuple(p, slot) {
                        out.push(bytes);
                    }
                }
                out
            })?;
            for bytes in tuples {
                let row: Row = serde_json::from_slice(&bytes).map_err(io::Error::other)?;
                rows.push(row);
            }
        }
        Ok(rows)
    }

    /// 整表改写并重建索引
    pub fn replace_all(&mut self, rows: &[Row]) -> io::Result<()> {
        self.file.truncate()?;
        if let Some(idx) = &mut self.index {
            idx.clear()?;
        }
        for r in rows {
            let (page_id, slot) = self.insert_returning_position(r)?;
            if let (Some(idx), Some(key)) = (&mut self.index, row_key(r)) {
                idx.insert(key, pack_position(page_id, slot))?;
            }
        }
        Ok(())
    }

    /// 把脏页写回
    pub fn flush(&mut self) -> io::Result<()> {
        if let Some(idx) = &mut self.index {
            idx.flush()?;
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

/// 取行里的 id 作索引键
fn row_key(row: &Row) -> Option<i64> {
    match row.get("id") {
        Some(serde_json::Value::Number(n)) => n.as_i64(),
        _ => None,
    }
}
