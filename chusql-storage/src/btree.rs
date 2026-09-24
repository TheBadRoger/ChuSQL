use std::io;
use std::path::Path;

use crate::page::{Page, PageFile, PageId};

// 磁盘版 B+ 树：每个节点占一页，page 0 存文件头（魔数 + 页大小 + order + 根页号）。
// 节点页布局（order = 一个节点最多几个孩子，来自配置）：
//   [0]              节点类型：0=叶子，1=内部
//   [1..3]           键数量 u16
//   [3..11]          叶子的 next 页号 u64（内部保留）
//   [12..12+K*8]     键：K = order-1 个 i64
//   [后面]           叶子：K 个 u64 值；内部：order 个 u64 孩子页号

/// 文件头放在这一页。
const META_PAGE: PageId = 0;
/// 文件头魔数。
const META_MAGIC: &[u8; 4] = b"CBTR";
/// 文件头里各字段的偏移。
const OFF_META_PAGE_SIZE: usize = 4;
const OFF_META_ORDER: usize = 8;
const OFF_META_ROOT: usize = 10;

/// 节点类型标记。
const NODE_LEAF: u8 = 0;
const NODE_INTERNAL: u8 = 1;
/// 布局偏移。
const OFF_TYPE: usize = 0;
const OFF_COUNT: usize = 1;
const OFF_NEXT: usize = 3;
const OFF_KEYS: usize = 12;

/// 一个 order 的节点占多少字节（内部节点最坏情况：order-1 个键 + order 个孩子）。
fn node_bytes(order: usize) -> usize {
    OFF_KEYS + max_keys(order) * 8 + order * 8
}

/// 一个节点最多几个键。
fn max_keys(order: usize) -> usize {
    order - 1
}

/// 键后面那片区域的起点。
fn off_payload(order: usize) -> usize {
    OFF_KEYS + max_keys(order) * 8
}

/// 这个 order 放得进 size 字节的页吗。
pub fn fits_in_page(page_size: usize, order: usize) -> bool {
    order >= 2 && node_bytes(order) <= page_size
}

/// 给定页大小，order 最大能取多少（解 12 + (order-1)*8 + order*8 <= page_size）。
pub fn max_order(page_size: usize) -> usize {
    page_size.saturating_sub(4) / 16
}

/// 读一个 u8。
fn read_u8(p: &Page, off: usize) -> u8 {
    p.data[off]
}

/// 写一个 u8。
fn write_u8(p: &mut Page, off: usize, v: u8) {
    p.data[off] = v;
}

/// 读一个小端 u16。
fn read_u16(p: &Page, off: usize) -> u16 {
    u16::from_le_bytes([p.data[off], p.data[off + 1]])
}

/// 写一个小端 u16。
fn write_u16(p: &mut Page, off: usize, v: u16) {
    p.data[off..off + 2].copy_from_slice(&v.to_le_bytes());
}

/// 读一个小端 u32。
fn read_u32(p: &Page, off: usize) -> u32 {
    u32::from_le_bytes([p.data[off], p.data[off + 1], p.data[off + 2], p.data[off + 3]])
}

/// 写一个小端 u32。
fn write_u32(p: &mut Page, off: usize, v: u32) {
    p.data[off..off + 4].copy_from_slice(&v.to_le_bytes());
}

/// 读一个小端 u64。
fn read_u64(p: &Page, off: usize) -> u64 {
    let mut b = [0u8; 8];
    b.copy_from_slice(&p.data[off..off + 8]);
    u64::from_le_bytes(b)
}

/// 写一个小端 u64。
fn write_u64(p: &mut Page, off: usize, v: u64) {
    p.data[off..off + 8].copy_from_slice(&v.to_le_bytes());
}

/// 写文件头。
fn write_meta(page: &mut Page, page_size: usize, order: usize, root: PageId) {
    page.zero();
    page.data[0..4].copy_from_slice(META_MAGIC);
    write_u32(page, OFF_META_PAGE_SIZE, page_size as u32);
    write_u16(page, OFF_META_ORDER, order as u16);
    write_u64(page, OFF_META_ROOT, root);
}

/// 检查文件头和当前配置是否一致（页大小 / order 改了就会对不上）。
fn check_meta(page: &Page, page_size: usize, order: usize) -> io::Result<()> {
    if page.data[0..4] != *META_MAGIC {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "not a chusql index file (bad magic)",
        ));
    }
    let stored_page = read_u32(page, OFF_META_PAGE_SIZE) as usize;
    let stored_order = read_u16(page, OFF_META_ORDER) as usize;
    if stored_page != page_size || stored_order != order {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            format!(
                "index file was built with page.size={} btree.order={}, config says page.size={} btree.order={}",
                stored_page, stored_order, page_size, order
            ),
        ));
    }
    Ok(())
}

/// 一个节点在内存里的样子。
enum NodeData {
    Leaf {
        keys: Vec<i64>,
        values: Vec<u64>,
        next: PageId,
    },
    Internal {
        keys: Vec<i64>,
        children: Vec<PageId>,
    },
}

/// 从一页里读出节点。
fn read_node(p: &Page, order: usize) -> NodeData {
    let ty = read_u8(p, OFF_TYPE);
    let n = read_u16(p, OFF_COUNT) as usize;
    let payload = off_payload(order);
    if ty == NODE_LEAF {
        let next = read_u64(p, OFF_NEXT);
        let mut keys = Vec::with_capacity(n);
        let mut values = Vec::with_capacity(n);
        for i in 0..n {
            keys.push(read_u64(p, OFF_KEYS + i * 8) as i64);
            values.push(read_u64(p, payload + i * 8));
        }
        NodeData::Leaf { keys, values, next }
    } else {
        let mut keys = Vec::with_capacity(n);
        let mut children = Vec::with_capacity(n + 1);
        for i in 0..n {
            keys.push(read_u64(p, OFF_KEYS + i * 8) as i64);
        }
        for i in 0..(n + 1) {
            children.push(read_u64(p, payload + i * 8));
        }
        NodeData::Internal { keys, children }
    }
}

/// 把节点写回一页（先清零，避免残留）。
fn write_node(p: &mut Page, node: &NodeData, order: usize) {
    p.zero();
    let payload = off_payload(order);
    match node {
        NodeData::Leaf { keys, values, next } => {
            write_u8(p, OFF_TYPE, NODE_LEAF);
            write_u16(p, OFF_COUNT, keys.len() as u16);
            write_u64(p, OFF_NEXT, *next);
            for (i, k) in keys.iter().enumerate() {
                write_u64(p, OFF_KEYS + i * 8, *k as u64);
            }
            for (i, v) in values.iter().enumerate() {
                write_u64(p, payload + i * 8, *v);
            }
        }
        NodeData::Internal { keys, children } => {
            write_u8(p, OFF_TYPE, NODE_INTERNAL);
            write_u16(p, OFF_COUNT, keys.len() as u16);
            for (i, k) in keys.iter().enumerate() {
                write_u64(p, OFF_KEYS + i * 8, *k as u64);
            }
            for (i, c) in children.iter().enumerate() {
                write_u64(p, payload + i * 8, *c);
            }
        }
    }
}

/// 二分找 key 应该下探到的孩子下标。
fn child_index(keys: &[i64], key: i64) -> usize {
    let mut lo = 0;
    let mut hi = keys.len();
    while lo < hi {
        let mid = (lo + hi) / 2;
        if keys[mid] <= key {
            lo = mid + 1;
        } else {
            hi = mid;
        }
    }
    lo
}

/// 磁盘版 B+ 树。
pub struct DiskBTree {
    file: PageFile,
    order: usize,
}

impl DiskBTree {
    /// 打开或新建索引文件；页大小 / order 和文件头里记的不一致就报错。
    pub fn open<P: AsRef<Path>>(path: P, page_size: usize, order: usize) -> io::Result<Self> {
        if !fits_in_page(page_size, order) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "btree order {} does not fit in a {}-byte page (max {})",
                    order,
                    page_size,
                    max_order(page_size)
                ),
            ));
        }
        let mut file = PageFile::open(path, page_size)?;
        if file.num_pages()? == 0 {
            let mut meta = Page::new(META_PAGE, page_size);
            write_meta(&mut meta, page_size, order, 0);
            file.write_page(&meta)?;
        } else {
            let meta = file.read_page(META_PAGE)?;
            check_meta(&meta, page_size, order)?;
        }
        Ok(DiskBTree { file, order })
    }

    /// 读根页号；0 表示空树。
    fn root(&mut self) -> io::Result<Option<PageId>> {
        let meta = self.file.read_page(META_PAGE)?;
        let r = read_u64(&meta, OFF_META_ROOT);
        Ok(if r == 0 { None } else { Some(r) })
    }

    /// 写根页号。
    fn set_root(&mut self, r: Option<PageId>) -> io::Result<()> {
        let mut meta = self.file.read_page(META_PAGE)?;
        write_u64(&mut meta, OFF_META_ROOT, r.unwrap_or(0));
        self.file.write_page(&meta)
    }

    /// 分配一个新页。
    fn alloc(&mut self) -> io::Result<PageId> {
        Ok(self.file.append_page()?.id)
    }

    /// 清空整棵树：截断文件后重写文件头（根页号归零）。
    pub fn clear(&mut self) -> io::Result<()> {
        let page_size = self.file.page_size();
        self.file.truncate()?;
        let mut meta = Page::new(META_PAGE, page_size);
        write_meta(&mut meta, page_size, self.order, 0);
        self.file.write_page(&meta)?;
        Ok(())
    }

    /// 空页，用来拼一个新节点。
    fn new_page(&self, id: PageId) -> Page {
        Page::new(id, self.file.page_size())
    }

    /// 按 key 找 value。
    pub fn get(&mut self, key: i64) -> io::Result<Option<u64>> {
        match self.root()? {
            None => Ok(None),
            Some(r) => self.get_rec(r, key),
        }
    }

    /// 递归查找。
    fn get_rec(&mut self, page_id: PageId, key: i64) -> io::Result<Option<u64>> {
        let p = self.file.read_page(page_id)?;
        match read_node(&p, self.order) {
            NodeData::Leaf { keys, values, .. } => {
                Ok(keys.binary_search(&key).ok().map(|i| values[i]))
            }
            NodeData::Internal { keys, children } => {
                let i = child_index(&keys, key);
                self.get_rec(children[i], key)
            }
        }
    }

    /// 插入 / 覆盖一个键值对。
    pub fn insert(&mut self, key: i64, value: u64) -> io::Result<()> {
        match self.root()? {
            None => {
                let id = self.alloc()?;
                let node = NodeData::Leaf {
                    keys: vec![key],
                    values: vec![value],
                    next: 0,
                };
                let mut p = self.new_page(id);
                write_node(&mut p, &node, self.order);
                self.file.write_page(&p)?;
                self.set_root(Some(id))
            }
            Some(r) => {
                let (_, split) = self.insert_rec(r, key, value)?;
                if let Some((sep, right_id)) = split {
                    // 根分裂：长高一层。
                    let root_id = self.alloc()?;
                    let node = NodeData::Internal {
                        keys: vec![sep],
                        children: vec![r, right_id],
                    };
                    let mut p = self.new_page(root_id);
                    write_node(&mut p, &node, self.order);
                    self.file.write_page(&p)?;
                    self.set_root(Some(root_id))
                } else {
                    Ok(())
                }
            }
        }
    }

    /// 递归插入；返回可能上提的 (分隔键, 新右页号)。
    fn insert_rec(
        &mut self,
        page_id: PageId,
        key: i64,
        value: u64,
    ) -> io::Result<(PageId, Option<(i64, PageId)>)> {
        let p = self.file.read_page(page_id)?;
        let limit = max_keys(self.order);
        match read_node(&p, self.order) {
            NodeData::Leaf { mut keys, mut values, next } => {
                match keys.binary_search(&key) {
                    Ok(i) => values[i] = value,
                    Err(i) => {
                        keys.insert(i, key);
                        values.insert(i, value);
                    }
                }
                if keys.len() <= limit {
                    let mut p2 = self.new_page(page_id);
                    write_node(&mut p2, &NodeData::Leaf { keys, values, next }, self.order);
                    self.file.write_page(&p2)?;
                    Ok((page_id, None))
                } else {
                    // 叶子分裂：右叶最小键复制上提。
                    let mid = keys.len() / 2;
                    let rkeys = keys.split_off(mid);
                    let rvalues = values.split_off(mid);
                    let sep = rkeys[0];

                    let right_id = self.alloc()?;
                    let mut rp = self.new_page(right_id);
                    write_node(
                        &mut rp,
                        &NodeData::Leaf { keys: rkeys, values: rvalues, next },
                        self.order,
                    );
                    self.file.write_page(&rp)?;

                    let mut lp = self.new_page(page_id);
                    write_node(
                        &mut lp,
                        &NodeData::Leaf { keys, values, next: right_id },
                        self.order,
                    );
                    self.file.write_page(&lp)?;

                    Ok((page_id, Some((sep, right_id))))
                }
            }

            NodeData::Internal { mut keys, mut children } => {
                let i = child_index(&keys, key);
                let (new_child, split) = self.insert_rec(children[i], key, value)?;
                children[i] = new_child;
                match split {
                    None => {
                        let mut p2 = self.new_page(page_id);
                        write_node(&mut p2, &NodeData::Internal { keys, children }, self.order);
                        self.file.write_page(&p2)?;
                        Ok((page_id, None))
                    }
                    Some((sep, right_id)) => {
                        keys.insert(i, sep);
                        children.insert(i + 1, right_id);
                        if keys.len() <= limit {
                            let mut p2 = self.new_page(page_id);
                            write_node(
                                &mut p2,
                                &NodeData::Internal { keys, children },
                                self.order,
                            );
                            self.file.write_page(&p2)?;
                            Ok((page_id, None))
                        } else {
                            // 内部节点分裂：中间键上提，不再保留原件。
                            let mid = keys.len() / 2;
                            let up = keys[mid];
                            let rkeys = keys.split_off(mid + 1);
                            keys.truncate(mid);
                            let rchildren = children.split_off(mid + 1);

                            let right_id = self.alloc()?;
                            let mut rp = self.new_page(right_id);
                            write_node(
                                &mut rp,
                                &NodeData::Internal { keys: rkeys, children: rchildren },
                                self.order,
                            );
                            self.file.write_page(&rp)?;

                            let mut lp = self.new_page(page_id);
                            write_node(
                                &mut lp,
                                &NodeData::Internal { keys, children },
                                self.order,
                            );
                            self.file.write_page(&lp)?;

                            Ok((page_id, Some((up, right_id))))
                        }
                    }
                }
            }
        }
    }

    /// 按 key 升序返回所有键值对。
    pub fn iter_all(&mut self) -> io::Result<Vec<(i64, u64)>> {
        let mut out = Vec::new();
        if let Some(r) = self.root()? {
            self.walk(r, &mut out)?;
        }
        Ok(out)
    }

    /// 中序遍历一棵子树。
    fn walk(&mut self, page_id: PageId, out: &mut Vec<(i64, u64)>) -> io::Result<()> {
        let p = self.file.read_page(page_id)?;
        match read_node(&p, self.order) {
            NodeData::Leaf { keys, values, .. } => {
                for i in 0..keys.len() {
                    out.push((keys[i], values[i]));
                }
            }
            NodeData::Internal { children, .. } => {
                for c in children {
                    self.walk(c, out)?;
                }
            }
        }
        Ok(())
    }
}
