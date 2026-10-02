use std::io;
use std::path::Path;

use crate::page::{Page, PageFile, PageId};

// 磁盘 B+ 树：键是变长字节串，允许重复键，叶子链支持范围扫描。

const META_PAGE: PageId = 0;
const META_MAGIC: &[u8; 4] = b"CBK2";
const OFF_PAGE_SIZE: usize = 4;
const OFF_MAX_KEY: usize = 8;
const OFF_ROOT: usize = 10;
const OFF_ORDER: usize = 18;

const NODE_LEAF: u8 = 0;
const NODE_INTERNAL: u8 = 1;
const OFF_KIND: usize = 0;
const OFF_COUNT: usize = 1;
const OFF_NEXT: usize = 3;
const HEADER: usize = 11;
const SLOT: usize = 2;
const CELL_FIXED: usize = 2 + 8;

/// 键长上限：节点里的格子必须放得进一页
pub const MAX_KEY_BYTES: usize = 64;

/// 一个节点最多几个键
fn max_keys(order: usize) -> usize {
    order - 1
}

/// 一个格子最多占多少字节
fn max_cell_bytes() -> usize {
    CELL_FIXED + MAX_KEY_BYTES
}

/// 这个 order 放得进一页吗
pub fn fits_in_page(page_size: usize, order: usize) -> bool {
    order >= 2 && HEADER + max_keys(order) * (SLOT + max_cell_bytes()) <= page_size
}

/// 一页最多几个孩子
pub fn max_order(page_size: usize) -> usize {
    page_size.saturating_sub(HEADER) / (SLOT + max_cell_bytes()) + 1
}

/// 读一个 u8
fn read_u8(p: &Page, off: usize) -> u8 {
    p.data[off]
}

/// 写一个 u8
fn write_u8(p: &mut Page, off: usize, v: u8) {
    p.data[off] = v;
}

/// 读一个小端 u16
fn read_u16(p: &Page, off: usize) -> u16 {
    u16::from_le_bytes([p.data[off], p.data[off + 1]])
}

/// 写一个小端 u16
fn write_u16(p: &mut Page, off: usize, v: u16) {
    p.data[off..off + 2].copy_from_slice(&v.to_le_bytes());
}

/// 读一个小端 u32
fn read_u32(p: &Page, off: usize) -> u32 {
    u32::from_le_bytes([p.data[off], p.data[off + 1], p.data[off + 2], p.data[off + 3]])
}

/// 写一个小端 u32
fn write_u32(p: &mut Page, off: usize, v: u32) {
    p.data[off..off + 4].copy_from_slice(&v.to_le_bytes());
}

/// 读一个小端 u64
fn read_u64(p: &Page, off: usize) -> u64 {
    let mut b = [0u8; 8];
    b.copy_from_slice(&p.data[off..off + 8]);
    u64::from_le_bytes(b)
}

/// 写一个小端 u64
fn write_u64(p: &mut Page, off: usize, v: u64) {
    p.data[off..off + 8].copy_from_slice(&v.to_le_bytes());
}

/// 写文件头
fn write_meta(page: &mut Page, page_size: usize, order: usize, root: PageId) {
    page.zero();
    page.data[0..4].copy_from_slice(META_MAGIC);
    write_u32(page, OFF_PAGE_SIZE, page_size as u32);
    write_u16(page, OFF_MAX_KEY, MAX_KEY_BYTES as u16);
    write_u64(page, OFF_ROOT, root);
    write_u16(page, OFF_ORDER, order as u16);
}

/// 校验文件头与配置
fn check_meta(page: &Page, page_size: usize, order: usize) -> io::Result<()> {
    if page.data[0..4] != *META_MAGIC {
        return Err(io::Error::new(
            io::ErrorKind::InvalidData,
            "not a chusql index file (bad magic)",
        ));
    }
    let stored_page = read_u32(page, OFF_PAGE_SIZE) as usize;
    let stored_order = read_u16(page, OFF_ORDER) as usize;
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

/// 一个节点：叶子存行位置，内部节点存孩子页号
struct Node {
    kind: u8,
    next: PageId,
    keys: Vec<Vec<u8>>,
    values: Vec<u64>,
}

/// 一个格子占多少字节
fn cell_bytes(key: &[u8]) -> usize {
    CELL_FIXED + key.len()
}

/// 节点占多少字节
fn node_bytes(node: &Node) -> usize {
    HEADER + node.keys.len() * SLOT + node.keys.iter().map(|k| cell_bytes(k)).sum::<usize>()
}

/// 把节点写回一页（格子从页尾往前摆，槽位从头部往后长）
fn put_node(p: &mut Page, node: &Node) -> io::Result<()> {
    if node.keys.len() > u16::MAX as usize || node_bytes(node) > p.data.len() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "index node does not fit in a page",
        ));
    }
    p.zero();
    write_u8(p, OFF_KIND, node.kind);
    write_u16(p, OFF_COUNT, node.keys.len() as u16);
    // 内部节点比键多一个孩子，最后一个放在 next 槽位里
    let trailer = if node.kind == NODE_INTERNAL {
        node.values.last().copied().unwrap_or(0)
    } else {
        node.next
    };
    write_u64(p, OFF_NEXT, trailer);
    let mut cursor = p.data.len();
    for (i, key) in node.keys.iter().enumerate() {
        let size = cell_bytes(key);
        cursor -= size;
        write_u16(p, cursor, key.len() as u16);
        p.data[cursor + 2..cursor + 2 + key.len()].copy_from_slice(key);
        write_u64(p, cursor + 2 + key.len(), node.values[i]);
        write_u16(p, HEADER + i * SLOT, cursor as u16);
    }
    Ok(())
}

/// 从一页读出节点
fn get_node(p: &Page) -> Node {
    let kind = read_u8(p, OFF_KIND);
    let count = read_u16(p, OFF_COUNT) as usize;
    let trailer = read_u64(p, OFF_NEXT);
    let mut keys = Vec::with_capacity(count);
    let mut values = Vec::with_capacity(count + 1);
    for i in 0..count {
        let off = read_u16(p, HEADER + i * SLOT) as usize;
        let klen = read_u16(p, off) as usize;
        keys.push(p.data[off + 2..off + 2 + klen].to_vec());
        values.push(read_u64(p, off + 2 + klen));
    }
    if kind == NODE_INTERNAL {
        values.push(trailer);
        Node { kind, next: 0, keys, values }
    } else {
        Node { kind, next: trailer, keys, values }
    }
}

/// 叶子里的插入位置：同键按 value 排，保证顺序稳定
fn leaf_slot(keys: &[Vec<u8>], values: &[u64], key: &[u8], value: u64) -> usize {
    let mut i = keys.partition_point(|k| k.as_slice() < key);
    while i < keys.len() && keys[i].as_slice() == key && values[i] < value {
        i += 1;
    }
    i
}

/// 内部节点里选哪个孩子
fn child_slot(keys: &[Vec<u8>], key: &[u8]) -> usize {
    keys.partition_point(|k| k.as_slice() <= key)
}

/// 切开一个节点：返回上提的键与右半节点
fn split_node(node: &mut Node) -> (Vec<u8>, Node) {
    let mid = node.keys.len() / 2;
    if node.kind == NODE_LEAF {
        let sep = node.keys[mid].clone();
        let right = Node {
            kind: NODE_LEAF,
            next: node.next,
            keys: node.keys.split_off(mid),
            values: node.values.split_off(mid),
        };
        (sep, right)
    } else {
        let sep = node.keys[mid].clone();
        let right_keys = node.keys.split_off(mid + 1);
        let right_values = node.values.split_off(mid + 1);
        node.keys.pop();
        (
            sep,
            Node { kind: NODE_INTERNAL, next: 0, keys: right_keys, values: right_values },
        )
    }
}

/// 磁盘 B+ 树
pub struct DiskBTree {
    file: PageFile,
    order: usize,
}

impl DiskBTree {
    /// 打开或新建索引文件
    pub fn open<P: AsRef<Path>>(
        path: P,
        page_size: usize,
        order: usize,
        pool_size: usize,
    ) -> io::Result<Self> {
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
        let mut file = PageFile::with_options(path, page_size, pool_size)?;
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

    /// 键太长直接报错，避免写坏节点
    fn check_key(&self, key: &[u8]) -> io::Result<()> {
        if key.len() > MAX_KEY_BYTES {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "index key is {} bytes, at most {} are supported",
                    key.len(),
                    MAX_KEY_BYTES
                ),
            ));
        }
        Ok(())
    }

    /// 读根页页号；空树是 None
    fn root(&mut self) -> io::Result<Option<PageId>> {
        let r = self.file.with_page(META_PAGE, |p| read_u64(p, OFF_ROOT))?;
        Ok(if r == 0 { None } else { Some(r) })
    }

    /// 写根页页号
    fn set_root(&mut self, r: Option<PageId>) -> io::Result<()> {
        self.file
            .update_page(META_PAGE, |p| write_u64(p, OFF_ROOT, r.unwrap_or(0)))
    }

    /// 追加一个新节点页
    fn alloc(&mut self, node: &Node) -> io::Result<PageId> {
        let page = self.file.append_page()?;
        let id = page.id;
        self.file.update_page(id, |p| put_node(p, node))??;
        Ok(id)
    }

    /// 按页号读节点
    fn load(&mut self, id: PageId) -> io::Result<Node> {
        self.file.with_page(id, get_node)
    }

    /// 按页号写节点
    fn store(&mut self, id: PageId, node: &Node) -> io::Result<()> {
        self.file.update_page(id, |p| put_node(p, node))?
    }

    /// 清空整棵树
    pub fn clear(&mut self) -> io::Result<()> {
        let page_size = self.file.page_size();
        self.file.truncate()?;
        let mut meta = Page::new(META_PAGE, page_size);
        write_meta(&mut meta, page_size, self.order, 0);
        self.file.write_page(&meta)?;
        Ok(())
    }

    /// 树是空的吗（只看最左叶子，不用整树遍历）
    pub fn is_empty(&mut self) -> io::Result<bool> {
        let Some(mut id) = self.root()? else {
            return Ok(true);
        };
        loop {
            let node = self.load(id)?;
            if node.kind == NODE_LEAF {
                return Ok(node.keys.is_empty());
            }
            id = node.values[0];
        }
    }

    /// 按 key 取第一个位置
    pub fn get(&mut self, key: &[u8]) -> io::Result<Option<u64>> {
        Ok(self.get_all(key)?.into_iter().next())
    }

    /// 按 key 取全部位置（重复键按值升序）
    pub fn get_all(&mut self, key: &[u8]) -> io::Result<Vec<u64>> {
        let Some(mut id) = self.root()? else {
            return Ok(Vec::new());
        };
        loop {
            let node = self.load(id)?;
            if node.kind == NODE_LEAF {
                let start = node.keys.partition_point(|k| k.as_slice() < key);
                let mut out = Vec::new();
                for i in start..node.keys.len() {
                    if node.keys[i].as_slice() != key {
                        break;
                    }
                    out.push(node.values[i]);
                }
                return Ok(out);
            }
            id = node.values[child_slot(&node.keys, key)];
        }
    }

    /// 插入一个键值对（键可重复）
    pub fn insert(&mut self, key: &[u8], value: u64) -> io::Result<()> {
        self.check_key(key)?;
        match self.root()? {
            None => {
                let node = Node {
                    kind: NODE_LEAF,
                    next: 0,
                    keys: vec![key.to_vec()],
                    values: vec![value],
                };
                let id = self.alloc(&node)?;
                self.set_root(Some(id))
            }
            Some(root) => match self.insert_rec(root, key, value)? {
                None => Ok(()),
                Some((sep, right_id)) => {
                    let node = Node {
                        kind: NODE_INTERNAL,
                        next: 0,
                        keys: vec![sep],
                        values: vec![root, right_id],
                    };
                    let id = self.alloc(&node)?;
                    self.set_root(Some(id))
                }
            },
        }
    }

    /// 递归插入，溢出就把右半节点与上提键交给上层
    fn insert_rec(
        &mut self,
        id: PageId,
        key: &[u8],
        value: u64,
    ) -> io::Result<Option<(Vec<u8>, PageId)>> {
        let mut node = self.load(id)?;
        if node.kind == NODE_LEAF {
            let at = leaf_slot(&node.keys, &node.values, key, value);
            node.keys.insert(at, key.to_vec());
            node.values.insert(at, value);
        } else {
            let child = node.values[child_slot(&node.keys, key)];
            if let Some((sep, right_id)) = self.insert_rec(child, key, value)? {
                let at = node.keys.partition_point(|k| k.as_slice() <= sep.as_slice());
                node.keys.insert(at, sep);
                node.values.insert(at + 1, right_id);
            }
        }
        if node.keys.len() <= max_keys(self.order) {
            self.store(id, &node)?;
            return Ok(None);
        }
        let (sep, mut right) = split_node(&mut node);
        let right_id = self.alloc(&right)?;
        if node.kind == NODE_LEAF {
            right.next = node.next;
            node.next = right_id;
        }
        self.store(id, &node)?;
        Ok(Some((sep, right_id)))
    }

    /// 删掉一个 (key, value)
    pub fn delete(&mut self, key: &[u8], value: u64) -> io::Result<bool> {
        let Some(root) = self.root()? else {
            return Ok(false);
        };
        self.delete_rec(root, key, value)
    }

    /// 递归删一个 (key, value)
    fn delete_rec(&mut self, id: PageId, key: &[u8], value: u64) -> io::Result<bool> {
        let mut node = self.load(id)?;
        if node.kind == NODE_LEAF {
            let at = leaf_slot(&node.keys, &node.values, key, value);
            if at < node.keys.len() && node.keys[at].as_slice() == key && node.values[at] == value {
                node.keys.remove(at);
                node.values.remove(at);
                self.store(id, &node)?;
                return Ok(true);
            }
            return Ok(false);
        }
        let child = node.values[child_slot(&node.keys, key)];
        self.delete_rec(child, key, value)
    }

    /// 叶子链上按顺序取全部 (键, 值)
    pub fn iter_all(&mut self) -> io::Result<Vec<(Vec<u8>, u64)>> {
        self.scan(None, None)
    }

    /// 范围扫描：lo / hi 都是闭区间，None 表示不限
    pub fn scan(
        &mut self,
        lo: Option<&[u8]>,
        hi: Option<&[u8]>,
    ) -> io::Result<Vec<(Vec<u8>, u64)>> {
        let Some(mut id) = self.root()? else {
            return Ok(Vec::new());
        };
        loop {
            let node = self.load(id)?;
            if node.kind == NODE_LEAF {
                id = match lo {
                    None => id,
                    Some(start) => {
                        let at = node.keys.partition_point(|k| k.as_slice() < start);
                        if at < node.keys.len() { id } else { node.next }
                    }
                };
                break;
            }
            id = match lo {
                None => node.values[0],
                Some(start) => node.values[child_slot(&node.keys, start)],
            };
        }
        let mut out = Vec::new();
        while id != 0 {
            let node = self.load(id)?;
            for i in 0..node.keys.len() {
                if lo.is_some_and(|start| node.keys[i].as_slice() < start) {
                    continue;
                }
                if hi.is_some_and(|end| node.keys[i].as_slice() > end) {
                    return Ok(out);
                }
                out.push((node.keys[i].clone(), node.values[i]));
            }
            id = node.next;
        }
        Ok(out)
    }

    /// 脏页写回
    pub fn flush(&mut self) -> io::Result<()> {
        self.file.flush()
    }

    /// 当前缓存页数
    pub fn cached_pages(&self) -> usize {
        self.file.cached_pages()
    }
}
