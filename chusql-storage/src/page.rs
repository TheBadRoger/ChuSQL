use std::fs::{File, OpenOptions};
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::path::Path;

// 页管理：数据库文件按固定大小的页组织，页大小由配置给（默认 4096，见 config.rs）。
// 只负责"按页号读 / 写"，不理解页里的内容 —— 内容怎么解释由上层（堆表 / B+ 树 / WAL）决定。
// 页号从 0 开始，第 n 页在文件中的偏移是 n * page_size。

/// 页号：从 0 开始。u64 足够大，不用担心中途用完。
pub type PageId = u64;

/// 一页：固定 page_size 字节，外加自己的页号。
#[derive(Debug, Clone)]
pub struct Page {
    pub id: PageId,
    pub data: Vec<u8>,
}

impl Page {
    /// 新建一页，内容全 0。
    pub fn new(id: PageId, size: usize) -> Self {
        Page {
            id,
            data: vec![0u8; size],
        }
    }

    /// 清零。复用时先清空，避免残留旧数据。
    pub fn zero(&mut self) {
        self.data.fill(0);
    }
}

/// 页文件：负责把 Page 落到磁盘上（不做缓存，每次读写都直接 syscall；缓冲池是后面的阶段）。
#[derive(Debug)]
pub struct PageFile {
    file: File,
    page_size: usize,
}

impl PageFile {
    /// 打开（或创建）一个页文件；页大小由配置传进来。
    pub fn open<P: AsRef<Path>>(path: P, page_size: usize) -> io::Result<Self> {
        let file = OpenOptions::new()
            .read(true)
            .write(true)
            .create(true)
            .truncate(false)
            .open(path)?;
        Ok(PageFile { file, page_size })
    }

    /// 这个文件的页大小。
    pub fn page_size(&self) -> usize {
        self.page_size
    }

    /// 读取指定页号的页；页号超出文件末尾时返回 UnexpectedEof。
    pub fn read_page(&mut self, id: PageId) -> io::Result<Page> {
        let mut data = vec![0u8; self.page_size];
        self.file.seek(SeekFrom::Start(id * self.page_size as u64))?;
        self.file.read_exact(&mut data)?;
        Ok(Page { id, data })
    }

    /// 写入一页。若页号超出当前文件末尾，文件会自动扩展。
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
        self.file
            .seek(SeekFrom::Start(page.id * self.page_size as u64))?;
        self.file.write_all(&page.data)?;
        self.file.flush()?;
        Ok(())
    }

    /// 在文件末尾追加一页，返回新页的页号。
    pub fn append_page(&mut self) -> io::Result<Page> {
        let id = self.num_pages()?;
        let page = Page::new(id, self.page_size);
        self.write_page(&page)?;
        Ok(page)
    }

    /// 当前文件里有多少页；长度不是页大小整数倍，说明页大小和文件对不上。
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
        Ok(len / size)
    }

    /// 强制把文件缓冲刷到磁盘。写入关键路径时用。
    pub fn sync(&mut self) -> io::Result<()> {
        self.file.sync_all()
    }

    /// 清空文件，游标归零。
    pub fn truncate(&mut self) -> io::Result<()> {
        self.file.set_len(0)?;
        self.file.seek(SeekFrom::Start(0))?;
        Ok(())
    }
}
