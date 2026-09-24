use std::collections::{HashMap, HashSet, VecDeque};
use std::fs::{File, OpenOptions};
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::path::Path;

// 页管理：定长页的读写，外加一层页缓冲池。

// 页
/// 页号：从 0 开始
pub type PageId = u64;

/// 缓冲池默认容量（页数）
pub const DEFAULT_POOL_SIZE: usize = 64;

#[derive(Debug, Clone)]
/// 一页：定长字节 + 页号
pub struct Page {
    pub id: PageId,
    pub data: Vec<u8>,
}

impl Page {
    /// 新建一页，全 0
    pub fn new(id: PageId, size: usize) -> Self {
        Page {
            id,
            data: vec![0u8; size],
        }
    }

    /// 清零
    pub fn zero(&mut self) {
        self.data.fill(0);
    }
}

#[derive(Debug)]
/// 页文件：定长页读写 + 页缓存
pub struct PageFile {
    file: File,
    page_size: usize,
    capacity: usize,
    cache: HashMap<PageId, Page>,
    order: VecDeque<PageId>,
    dirty: HashSet<PageId>,
    hits: u64,
    misses: u64,
}

impl PageFile {
    /// 打开页文件（默认池容量）
    pub fn open<P: AsRef<Path>>(path: P, page_size: usize) -> io::Result<Self> {
        Self::with_options(path, page_size, DEFAULT_POOL_SIZE)
    }

    /// 打开页文件并指定池容量
    pub fn with_options<P: AsRef<Path>>(
        path: P,
        page_size: usize,
        capacity: usize,
    ) -> io::Result<Self> {
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(path)?;
        Ok(PageFile {
            file,
            page_size,
            capacity,
            cache: HashMap::new(),
            order: VecDeque::new(),
            dirty: HashSet::new(),
            hits: 0,
            misses: 0,
        })
    }

    /// 页大小
    pub fn page_size(&self) -> usize {
        self.page_size
    }

// 缓冲池
    /// 池容量（页数）
    pub fn capacity(&self) -> usize {
        self.capacity
    }

    /// 当前缓存了多少页
    pub fn cached_pages(&self) -> usize {
        self.cache.len()
    }

    /// 命中次数
    pub fn hits(&self) -> u64 {
        self.hits
    }

    /// 未命中次数
    pub fn misses(&self) -> u64 {
        self.misses
    }

    /// 命中率
    pub fn hit_rate(&self) -> f64 {
        let total = self.hits + self.misses;
        if total == 0 {
            0.0
        } else {
            self.hits as f64 / total as f64
        }
    }


    /// 标记为最近使用
    fn touch(&mut self, id: PageId) {
        if let Some(pos) = self.order.iter().position(|&x| x == id) {
            self.order.remove(pos);
        }
        self.order.push_back(id);
    }

    /// 淘汰一个最冷的页
    fn evict_one(&mut self) -> io::Result<bool> {
        while let Some(victim) = self.order.pop_front() {
            if let Some(p) = self.cache.get(&victim) {
                if self.dirty.contains(&victim) {
                    let p = p.clone();
                    self.write_raw(&p)?;
                    self.dirty.remove(&victim);
                }
                self.cache.remove(&victim);
                return Ok(true);
            }
        }
        Ok(false)
    }

    /// 直接写文件
    fn write_raw(&mut self, page: &Page) -> io::Result<()> {
        if page.data.len() != self.page_size {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "page has {} bytes, this file uses {}",
                    page.data.len(),
                    self.page_size
                ),
            ));
        }
        self.file
            .seek(SeekFrom::Start(page.id * self.page_size as u64))?;
        self.file.write_all(&page.data)
    }

    /// 直接从文件读
    fn read_raw(&mut self, id: PageId) -> io::Result<Page> {
        let mut data = vec![0u8; self.page_size];
        self.file.seek(SeekFrom::Start(id * self.page_size as u64))?;
        self.file.read_exact(&mut data)?;
        Ok(Page { id, data })
    }


    /// 读一页（走缓存）
    pub fn read_page(&mut self, id: PageId) -> io::Result<Page> {
        if let Some(p) = self.cache.get(&id) {
            let p = p.clone();
            self.hits += 1;
            self.touch(id);
            return Ok(p);
        }
        self.misses += 1;
        let p = self.read_raw(id)?;
        if self.cache.len() >= self.capacity {
            self.evict_one()?;
        }
        self.cache.insert(id, p.clone());
        self.order.push_back(id);
        Ok(p)
    }

    /// 写一页（走缓存）
    pub fn write_page(&mut self, page: &Page) -> io::Result<()> {
        if page.data.len() != self.page_size {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "page has {} bytes, this file uses {}",
                    page.data.len(),
                    self.page_size
                ),
            ));
        }
        if self.cache.len() >= self.capacity && !self.cache.contains_key(&page.id) {
            self.evict_one()?;
        }
        self.cache.insert(page.id, page.clone());
        self.dirty.insert(page.id);
        self.touch(page.id);
        Ok(())
    }

    /// 末尾追加一页
    pub fn append_page(&mut self) -> io::Result<Page> {
        let id = self.num_pages()?;
        let page = Page::new(id, self.page_size);
        self.write_page(&page)?;
        Ok(page)
    }

    /// 文件里有多少页
    pub fn num_pages(&mut self) -> io::Result<PageId> {
        let len = self.file.metadata()?.len();
        let size = self.page_size as u64;
        if len % size != 0 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                format!(
                    "file length {} is not a multiple of page size {} (did page.size change?)",
                    len, size
                ),
            ));
        }
        let on_disk = len / size;
        let cached_max = self
            .cache
            .keys()
            .copied()
            .max()
            .map(|m| m + 1)
            .unwrap_or(0);
        Ok(on_disk.max(cached_max))
    }

    /// 把脏页写回磁盘
    pub fn flush(&mut self) -> io::Result<()> {
        let dirty: Vec<PageId> = self.dirty.iter().copied().collect();
        for id in dirty {
            if let Some(p) = self.cache.get(&id) {
                let p = p.clone();
                self.write_raw(&p)?;
            }
        }
        self.file.sync_all()?;
        self.dirty.clear();
        Ok(())
    }

    /// flush 之后 fsync
    pub fn sync(&mut self) -> io::Result<()> {
        self.flush()
    }

    /// 清空文件
    pub fn truncate(&mut self) -> io::Result<()> {
        self.cache.clear();
        self.order.clear();
        self.dirty.clear();
        self.file.set_len(0)?;
        self.file.seek(SeekFrom::Start(0))?;
        Ok(())
    }
}

    /// 退出前把脏页写回
impl Drop for PageFile {
    fn drop(&mut self) {
        let _ = self.flush();
    }
}
