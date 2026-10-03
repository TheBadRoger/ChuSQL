use std::collections::HashSet;
use std::io;
use std::path::Path;
use serde::de::{IgnoredAny, MapAccess, Visitor};
use serde::Deserializer;

use crate::btree::DiskBTree;
use crate::page::PageId;
use crate::page::{Page, PageFile};
use crate::protocol::Row;

// 堆表：行按插入顺序进页，支持全表扫描、按索引取行、行级删除。

const HEADER_SIZE: usize = 2;
const SLOT_SIZE: usize = 8;
const SLOT_DIR_START: usize = HEADER_SIZE;

/// 未选字段只校验并跳过，不分配值。
fn decode_row(bytes: &[u8], columns: Option<&[String]>, hidden: &HashSet<String>) -> io::Result<Row> {
    struct RowVisitor<'a>(Option<&'a [String]>, &'a HashSet<String>);
    impl<'de> Visitor<'de> for RowVisitor<'_> {
        type Value = Row;
        /// 说明期望的输入形态
        fn expecting(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
            f.write_str("a row object")
        }
        /// 逐字段解出一行
        fn visit_map<M: MapAccess<'de>>(self, mut map: M) -> Result<Row, M::Error> {
            let mut row = Row::new();
            while let Some(key) = map.next_key::<String>()? {
                if !self.1.contains(&key) && self.0.is_none_or(|cols| cols.contains(&key)) {
                    row.insert(key, map.next_value()?);
                } else {
                    map.next_value::<IgnoredAny>()?;
                }
            }
            Ok(row)
        }
    }
    let mut decoder = serde_json::Deserializer::from_slice(bytes);
    let row = decoder.deserialize_map(RowVisitor(columns, hidden)).map_err(io::Error::other)?;
    decoder.end().map_err(io::Error::other)?;
    Ok(row)
}

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

/// (页号, 槽位号) 打包成 u64
fn pack_position(page_id: PageId, slot: u16) -> u64 {
    (page_id << 16) | (slot as u64)
}

/// 从 u64 解回 (页号, 槽位号)
fn unpack_position(v: u64) -> (PageId, u16) {
    (v >> 16, (v & 0xFFFF) as u16)
}

/// 数值里取整数，整值浮点折成整数
pub(crate) fn scalar_int(v: &serde_json::Value) -> Option<i64> {
    match v {
        serde_json::Value::Number(n) => n.as_i64().or_else(|| {
            let f = n.as_f64()?;
            if f.fract() != 0.0 || f.abs() > 9_007_199_254_740_992.0 {
                return None;
            }
            Some(f as i64)
        }),
        _ => None,
    }
}

/// 取行里某一列的整数值（不是整数就没有）
fn row_int(row: &Row, column: &str) -> Option<i64> {
    row.get(column).and_then(scalar_int)
}

/// i64 键 → 8 字节大端（保序，负数也在前）
fn key_of(value: i64) -> [u8; 8] {
    (value as u64 ^ (1u64 << 63)).to_be_bytes()
}

/// JSON 值 → 索引键字节（与写索引时同一套编码）
fn value_key(v: &serde_json::Value) -> Option<Vec<u8>> {
    match v {
        serde_json::Value::Number(_) => scalar_int(v).map(|k| key_of(k).to_vec()),
        serde_json::Value::String(s) => Some(s.as_bytes().to_vec()),
        serde_json::Value::Bool(b) => Some(vec![u8::from(*b)]),
        _ => None,
    }
}

/// 取行里某一列的索引键；NULL 与缺列不入索引
fn row_key(row: &Row, column: &str) -> Option<Vec<u8>> {
    row.get(column).and_then(value_key)
}

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

    /// 删掉一个槽位，只把长度置 0
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

    /// 页内死空间字节数：行区里除活行以外的部分
    pub fn dead_bytes(page: &Page) -> usize {
        let n = Self::slot_count(page);
        let mut live = 0;
        for i in 0..n as usize {
            let slot_off = SLOT_DIR_START + i * SLOT_SIZE;
            live += read_u32(&page.data, slot_off + 4) as usize;
        }
        (page.data.len() - Self::free_end(page)).saturating_sub(live)
    }

    /// 页内整理：活行挪到页尾，槽位号不动，返回回收字节数
    pub fn compact_page(page: &mut Page) -> usize {
        let reclaimed = Self::dead_bytes(page);
        if reclaimed == 0 {
            return 0;
        }

        let n = Self::slot_count(page);
        let mut live: Vec<(usize, Vec<u8>)> = Vec::new();
        for i in 0..n as usize {
            let slot_off = SLOT_DIR_START + i * SLOT_SIZE;
            let off = read_u32(&page.data, slot_off) as usize;
            let len = read_u32(&page.data, slot_off + 4) as usize;
            if len > 0 {
                live.push((i, page.data[off..off + len].to_vec()));
            } else {
                write_u32(&mut page.data, slot_off, 0);
            }
        }

        let mut end = page.data.len();
        for (i, bytes) in live {
            end -= bytes.len();
            page.data[end..end + bytes.len()].copy_from_slice(&bytes);
            write_u32(&mut page.data, SLOT_DIR_START + i * SLOT_SIZE, end as u32);
        }
        reclaimed
    }

    /// 死空间超过四分之一就整理这一页
    pub fn compact_if_fragmented(page: &mut Page) -> usize {
        if Self::dead_bytes(page) * 4 >= page.data.len() {
            Self::compact_page(page)
        } else {
            0
        }
    }
}

struct NamedIndex {
    column: String,
    tree: DiskBTree,
}

pub struct HeapTable {
    hidden_columns: HashSet<String>,
    file: PageFile,
    indexes: Vec<NamedIndex>,
}

impl HeapTable {
    /// 打开表（不带索引）
    pub fn open<P: AsRef<Path>>(path: P, page_size: usize, pool_size: usize) -> io::Result<Self> {
        Ok(HeapTable {
            hidden_columns: HashSet::new(),
            file: PageFile::with_options(path, page_size, pool_size)?,
            indexes: Vec::new(),
        })
    }

    /// 打开表并挂上 id 列索引
    pub fn open_indexed<P: AsRef<Path>, Q: AsRef<Path>>(
        path: P,
        index_path: Q,
        page_size: usize,
        btree_order: usize,
        pool_size: usize,
    ) -> io::Result<Self> {
        Self::open_with_indexes(
            path,
            &[("id".to_string(), index_path.as_ref().to_path_buf())],
            page_size,
            btree_order,
            pool_size,
        )
    }

    /// 打开表并挂上多组索引
    pub fn open_with_indexes<P: AsRef<Path>>(
        path: P,
        indexes: &[(String, std::path::PathBuf)],
        page_size: usize,
        btree_order: usize,
        pool_size: usize,
    ) -> io::Result<Self> {
        let mut t = HeapTable {
            hidden_columns: HashSet::new(),
            file: PageFile::with_options(path, page_size, pool_size)?,
            indexes: Vec::new(),
        };
        let mut filled = false;
        for (col, p) in indexes {
            match t.attach_index(col, p, page_size, btree_order, pool_size) {
                Ok(()) => {}
                Err(err) if err.kind() == io::ErrorKind::InvalidData => {
                    // 格式不兼容（例如上一版落的索引文件）：删掉重开
                    let _ = std::fs::remove_file(p);
                    t.attach_index(col, p, page_size, btree_order, pool_size)?;
                }
                Err(err) => return Err(err),
            }
            // 空索引（新文件、被截断、强杀留下）都按堆数据补齐
            if t.index_empty(col)? {
                t.fill_index(col)?;
                filled = true;
            }
        }
        if filled {
            t.flush()?;
        }
        Ok(t)
    }

    /// 挂上一棵索引树，不填数据
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

    /// 建索引：id 列必须唯一，其余列可重复
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

        let mut seen: HashSet<Vec<u8>> = HashSet::new();
        let mut entries: Vec<(Vec<u8>, u64)> = Vec::new();
        for (page_id, slot, r) in self.scan_with_positions()? {
            let Some(k) = row_key(&r, column) else {
                continue;
            };
            // 行身份索引仍然唯一；其余列允许重复值
            if column == "id" && !seen.insert(k.clone()) {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    format!("column \"{}\" has duplicate values", column),
                ));
            }
            entries.push((k, pack_position(page_id, slot)));
        }

        self.attach_index(column, path, page_size, btree_order, pool_size)?;
        let i = self.indexes.len() - 1;
        self.indexes[i].tree.clear()?;
        for (k, pos) in entries {
            self.indexes[i].tree.insert(&k, pos)?;
        }
        Ok(())
    }

    /// 这一列的索引是空的吗
    fn index_empty(&mut self, column: &str) -> io::Result<bool> {
        match self.indexes.iter_mut().find(|i| i.column == column) {
            Some(idx) => idx.tree.is_empty(),
            None => Ok(false),
        }
    }

    /// 按堆里的行把某一列索引补齐
    fn fill_index(&mut self, column: &str) -> io::Result<()> {
        let rows = self.scan_with_positions()?;
        let Some(i) = self.indexes.iter().position(|idx| idx.column == column) else {
            return Ok(());
        };
        for (page_id, slot, row) in rows {
            if let Some(k) = row_key(&row, column) {
                self.indexes[i]
                    .tree
                    .insert(&k, pack_position(page_id, slot))?;
            }
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

    /// 设置隐藏列集合
    pub fn set_hidden_columns(&mut self, columns: HashSet<String>) {
        self.hidden_columns = columns;
    }

    /// 隐藏一列并摘掉它的索引
    pub fn hide_column(&mut self, column: &str) {
        self.detach_index(column);
        self.hidden_columns.insert(column.to_string());
    }

    /// 插入一行，并顺手写各索引
    pub fn insert_row(&mut self, row: &Row) -> io::Result<()> {
        self.insert_row_returning_position(row).map(|_| ())
    }

    /// 插入一行并返回它的位置
    pub fn insert_row_returning_position(&mut self, row: &Row) -> io::Result<(PageId, u16)> {
        let mut pending: Vec<(usize, Vec<u8>)> = Vec::new();
        for (i, idx) in self.indexes.iter_mut().enumerate() {
            let Some(k) = row_key(row, &idx.column) else {
                continue;
            };
            if idx.column == "id" && idx.tree.get(&k)?.is_some() {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "duplicate value on the id index".to_string(),
                ));
            }
            pending.push((i, k));
        }

        let (page_id, slot) = self.insert_returning_position(row)?;
        let pos = pack_position(page_id, slot);
        for (i, k) in pending {
            self.indexes[i].tree.insert(&k, pos)?;
        }
        Ok((page_id, slot))
    }

    /// 批量插入，落盘由调用方负责
    pub fn insert_rows(&mut self, rows: &[Row]) -> io::Result<()> {
        for r in rows {
            self.insert_row(r)?;
        }
        Ok(())
    }

    /// 编码并验证一行能放进页
    pub fn encode_row(row: &Row, page_size: usize) -> io::Result<Vec<u8>> {
        let tuple = serde_json::to_vec(row).map_err(io::Error::other)?;

        if HEADER_SIZE + SLOT_SIZE + tuple.len() > page_size {
            return Err(io::Error::other(format!(
                "tuple too large: {} bytes",
                tuple.len()
            )));
        }
        Ok(tuple)
    }

    /// 插入一行，只进堆表、不碰索引
    fn insert_returning_position(&mut self, row: &Row) -> io::Result<(PageId, u16)> {
        let tuple = Self::encode_row(row, self.file.page_size())?;
        let n = self.file.num_pages()?;
        if n > 0 {
            let last_id = n - 1;
            let mut slot = None;
            self.file.update_page(last_id, |p| {
                HeapPage::compact_page(p);
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

    /// 按某一列的整数索引取一行
    pub fn get_by_column_key(&mut self, column: &str, key: i64) -> io::Result<Option<Row>> {
        self.get_by_bytes(column, &key_of(key))
    }

    /// 按某一列的字符串索引取一行
    pub fn get_by_string_key(&mut self, column: &str, key: &str) -> io::Result<Option<Row>> {
        self.get_by_bytes(column, key.as_bytes())
    }

    /// 按某一列的整数索引取全部行
    pub fn get_all_by_column_key(&mut self, column: &str, key: i64) -> io::Result<Vec<Row>> {
        self.get_all_by_bytes(column, &key_of(key))
    }

    /// 按某一列的字符串索引取全部行
    pub fn get_all_by_string_key(&mut self, column: &str, key: &str) -> io::Result<Vec<Row>> {
        self.get_all_by_bytes(column, key.as_bytes())
    }

    /// 按索引键的字节取第一行
    fn get_by_bytes(&mut self, column: &str, key: &[u8]) -> io::Result<Option<Row>> {
        Ok(self.get_all_by_bytes(column, key)?.into_iter().next())
    }

    /// 按索引键的字节取全部行（重复值都给出来）
    fn get_all_by_bytes(&mut self, column: &str, key: &[u8]) -> io::Result<Vec<Row>> {
        let Some(idx) = self.indexes.iter_mut().find(|i| i.column == column) else {
            return Ok(Vec::new());
        };
        let positions = idx.tree.get_all(key)?;
        let mut rows = Vec::new();
        for pos in positions {
            let (page_id, slot) = unpack_position(pos);
            if let Some(row) = self.read_at(page_id, slot)? {
                rows.push(row);
            }
        }
        Ok(rows)
    }

    /// 按索引做范围扫描；没索引返回 None
    pub fn scan_range(
        &mut self,
        column: &str,
        lo: Option<(&serde_json::Value, bool)>,
        hi: Option<(&serde_json::Value, bool)>,
    ) -> io::Result<Option<Vec<Row>>> {
        let Some(i) = self.indexes.iter().position(|idx| idx.column == column) else {
            return Ok(None);
        };
        let lo_bound = lo.and_then(|(v, inclusive)| value_key(v).map(|k| (k, inclusive)));
        let hi_bound = hi.and_then(|(v, inclusive)| value_key(v).map(|k| (k, inclusive)));
        // 树按闭区间取，排他端在取回来之后再筛
        let pairs = self.indexes[i].tree.scan(
            lo_bound.as_ref().map(|(k, _)| k.as_slice()),
            hi_bound.as_ref().map(|(k, _)| k.as_slice()),
        )?;
        let mut rows = Vec::new();
        for (key, pos) in pairs {
            if let Some((bound, inclusive)) = &lo_bound {
                let outside = if *inclusive { &key < bound } else { &key <= bound };
                if outside {
                    continue;
                }
            }
            if let Some((bound, inclusive)) = &hi_bound {
                let outside = if *inclusive { &key > bound } else { &key >= bound };
                if outside {
                    continue;
                }
            }
            let (page_id, slot) = unpack_position(pos);
            if let Some(row) = self.read_at(page_id, slot)? {
                rows.push(row);
            }
        }
        Ok(Some(rows))
    }

    /// 按位置读一行
    pub fn read_at(&mut self, page_id: PageId, slot: u16) -> io::Result<Option<Row>> {
        let bytes = self.file.with_page(page_id, |p| HeapPage::get_tuple(p, slot))?;
        match bytes {
            None => Ok(None),
            Some(bytes) => {
                let row = decode_row(&bytes, None, &self.hidden_columns)?;
                Ok(Some(row))
            }
        }
    }

    /// 按位置删一行并返回它
    pub fn delete_at(&mut self, page_id: PageId, slot: u16) -> io::Result<Option<Row>> {
        let row = self.read_at(page_id, slot)?;
        let existed = self.file.update_page(page_id, |p| {
            let ok = HeapPage::delete_tuple(p, slot);
            if ok {
                HeapPage::compact_if_fragmented(p);
            }
            ok
        })?;
        if !existed {
            return Ok(None);
        }
        if let Some(r) = &row {
            let pos = pack_position(page_id, slot);
            for idx in &mut self.indexes {
                if let Some(k) = row_key(r, &idx.column) {
                    idx.tree.delete(&k, pos)?;
                }
            }
        }
        Ok(row)
    }

    /// 按 id 批量删行，返回删掉的行
    pub fn delete_by_keys(&mut self, keys: &[i64]) -> io::Result<Vec<Row>> {
        let mut positions: Vec<(PageId, u16)> = Vec::new();

        if self.has_index("id") {
            for &k in keys {
                let pos = self
                    .indexes
                    .iter_mut()
                    .find(|i| i.column == "id")
                    .and_then(|i| i.tree.get(&key_of(k)).ok().flatten());
                if let Some(p) = pos {
                    positions.push(unpack_position(p));
                }
            }
        } else {
            let mut wanted: Vec<i64> = keys.to_vec();
            wanted.sort_unstable();
            for (page_id, slot, r) in self.scan_with_positions()? {
                if row_int(&r, "id").is_some_and(|k| wanted.binary_search(&k).is_ok()) {
                    positions.push((page_id, slot));
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
        self.scan_columns(None)
    }

    /// 全表扫描，只解码指定列
    pub fn scan_columns(&mut self, columns: Option<&[String]>) -> io::Result<Vec<Row>> {
        Ok(self
            .scan_projected_with_positions(columns)?
            .into_iter()
            .map(|(_, _, r)| r)
            .collect())
    }

    /// 全表扫描，同时给出每行的位置（建索引用）
    pub fn scan_with_positions(&mut self) -> io::Result<Vec<(PageId, u16, Row)>> {
        self.scan_projected_with_positions(None)
    }

    /// 分片扫描，只取 [from, to) 页区间内的行
    pub fn scan_columns_pages(
        &mut self,
        columns: Option<&[String]>,
        from: PageId,
        to: PageId,
    ) -> io::Result<Vec<Row>> {
        Ok(self
            .scan_pages_with_positions(columns, from, to)?
            .into_iter()
            .map(|(_, _, r)| r)
            .collect())
    }

    /// 当前页数
    pub fn num_pages(&mut self) -> io::Result<u64> {
        self.file.num_pages()
    }

    /// 全表扫描，带位置与列投影
    fn scan_projected_with_positions(&mut self, columns: Option<&[String]>) -> io::Result<Vec<(PageId, u16, Row)>> {
        let n = self.file.num_pages()?;
        self.scan_pages_with_positions(columns, 0, n)
    }

    /// 扫描 [from, to) 页区间，带位置与列投影
    fn scan_pages_with_positions(
        &mut self,
        columns: Option<&[String]>,
        from: PageId,
        to: PageId,
    ) -> io::Result<Vec<(PageId, u16, Row)>> {
        let n = self.file.num_pages()?;
        let mut out = Vec::new();
        for i in from..to.min(n) {
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
                let row = decode_row(&bytes, columns, &self.hidden_columns)?;
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
                if let Some(k) = row_key(r, &idx.column) {
                    idx.tree.insert(&k, pos)?;
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

    /// 整理整张表的页内碎片，返回回收字节数
    pub fn compact_pages(&mut self) -> io::Result<usize> {
        let n = self.file.num_pages()?;
        let mut reclaimed = 0;
        for i in 0..n {
            if self.file.with_page(i, HeapPage::dead_bytes)? == 0 {
                continue;
            }
            reclaimed += self.file.update_page(i, HeapPage::compact_page)?;
        }
        Ok(reclaimed)
    }

    /// 当前缓存页数
    pub fn cached_pages(&self) -> usize { self.file.cached_pages() }
    /// 命中次数
    pub fn hits(&self) -> u64 { self.file.hits() }
    /// 未命中次数
    pub fn misses(&self) -> u64 { self.file.misses() }
    /// 命中率
    pub fn hit_rate(&self) -> f64 { self.file.hit_rate() }
}
