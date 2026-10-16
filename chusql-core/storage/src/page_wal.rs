use std::collections::BTreeMap;
use std::fs::{File, OpenOptions};
use std::io::{self, Read, Seek, SeekFrom, Write};
use std::path::{Path, PathBuf};

use serde::{Deserialize, Serialize};

// 全页预写镜像修复撕裂写，页 LSN 在数据同步后原子发布。

const HEADER: usize = 40;
const RESET: u64 = u64::MAX;

#[derive(Debug, Serialize, Deserialize)]
struct State {
    version: u32,
    page_size: usize,
    sequence: u64,
    pages: BTreeMap<u64, u64>,
}

#[derive(Debug)]
pub(crate) struct PageWal {
    path: PathBuf,
    state: State,
    lsn: u64,
    pending: bool,
    failed: bool,
    file: Option<File>,
}

/// 为页文件生成独立的元数据路径
fn sidecar(path: &Path, suffix: &str) -> PathBuf {
    let mut name = path.as_os_str().to_os_string();
    name.push(suffix);
    PathBuf::from(name)
}

/// 计算帧校验和以拒绝损坏的完整镜像
fn checksum(bytes: &[u8]) -> u64 {
    bytes.iter().fold(0xcbf29ce484222325, |hash, byte| {
        (hash ^ u64::from(*byte)).wrapping_mul(0x100000001b3)
    })
}

/// 同步目录中的创建、替换与删除
fn sync_parent(path: &Path) -> io::Result<()> {
    #[cfg(unix)]
    File::open(
        path.parent()
            .filter(|p| !p.as_os_str().is_empty())
            .unwrap_or(Path::new(".")),
    )?
    .sync_all()?;
    Ok(())
}

impl PageWal {
    /// 加载页版本并先恢复未完成的全页写入
    pub(crate) fn open(path: &Path, page_size: usize, data: &mut File) -> io::Result<Self> {
        let metadata = sidecar(path, ".pages");
        let state = match std::fs::read(&metadata) {
            Ok(bytes) => serde_json::from_slice::<State>(&bytes).map_err(io::Error::other)?,
            Err(error) if error.kind() == io::ErrorKind::NotFound => State {
                version: 1,
                page_size,
                sequence: 0,
                pages: BTreeMap::new(),
            },
            Err(error) => return Err(error),
        };
        if state.version != 1 || state.page_size != page_size {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "unsupported page LSN metadata",
            ));
        }
        let mut journal = Self {
            path: path.to_path_buf(),
            state,
            lsn: 0,
            pending: false,
            failed: false,
            file: None,
        };
        journal.recover(data)?;
        Ok(journal)
    }

    /// 设置后续脏页所属的逻辑 WAL 提交点
    pub(crate) fn set_lsn(&mut self, lsn: u64) {
        self.lsn = lsn;
    }

    /// 读取已写镜像记录的页 LSN
    pub(crate) fn page_lsn(&self, page: u64) -> Option<u64> {
        self.state.pages.get(&page).copied()
    }

    /// 延迟创建日志并同步目录项
    fn handle(&mut self) -> io::Result<&mut File> {
        if self.file.is_none() {
            let path = sidecar(&self.path, ".page-wal");
            let file = OpenOptions::new()
                .read(true)
                .write(true)
                .create(true)
                .truncate(false)
                .open(&path)?;
            sync_parent(&path)?;
            self.file = Some(file);
        }
        Ok(self.file.as_mut().expect("opened above"))
    }

    /// 持久化一批镜像后才允许修改数据页
    pub(crate) fn prepare(&mut self, pages: &[(u64, &[u8])]) -> io::Result<()> {
        if self.failed {
            return Err(io::Error::other("page journal requires recovery"));
        }
        if pages.iter().any(|(page, bytes)| {
            (*page == RESET && !bytes.is_empty())
                || (*page != RESET
                    && (bytes.len() != self.state.page_size
                        || page.checked_mul(self.state.page_size as u64).is_none()))
        }) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "invalid page image size or offset",
            ));
        }
        if pages.is_empty() {
            return Ok(());
        }
        self.failed = true;
        for (page, bytes) in pages {
            let sequence = self
                .state
                .sequence
                .checked_add(1)
                .ok_or_else(|| io::Error::other("page write sequence exhausted"))?;
            let lsn = self
                .lsn
                .max(self.state.pages.get(page).copied().unwrap_or(0));
            let mut frame = Vec::with_capacity(HEADER + bytes.len());
            frame.extend_from_slice(b"CPW1");
            frame.extend_from_slice(&sequence.to_le_bytes());
            frame.extend_from_slice(&lsn.to_le_bytes());
            frame.extend_from_slice(&page.to_le_bytes());
            frame.extend_from_slice(&(bytes.len() as u32).to_le_bytes());
            let mut checked = frame.clone();
            checked.extend_from_slice(bytes);
            frame.extend_from_slice(&checksum(&checked).to_le_bytes());
            frame.extend_from_slice(bytes);
            let file = self.handle()?;
            file.seek(SeekFrom::End(0))?;
            file.write_all(&frame)?;
            self.state.sequence = sequence;
            if *page == RESET {
                self.state.pages.clear();
            } else {
                self.state.pages.insert(*page, lsn);
            }
            self.pending = true;
        }
        self.handle()?.sync_all()?;
        self.failed = false;
        Ok(())
    }

    /// 先记录截断以免重放旧页复活已删除数据
    pub(crate) fn reset(&mut self) -> io::Result<()> {
        self.prepare(&[(RESET, &[])])
    }

    /// 数据同步后发布页版本并清理镜像日志
    pub(crate) fn checkpoint(&mut self) -> io::Result<()> {
        if self.failed {
            return Err(io::Error::other("page journal requires recovery"));
        }
        if !self.pending {
            return Ok(());
        }
        self.failed = true;
        let temporary = sidecar(&self.path, ".pages.pending");
        let metadata = sidecar(&self.path, ".pages");
        let mut file = File::create(&temporary)?;
        serde_json::to_writer(&mut file, &self.state).map_err(io::Error::other)?;
        file.sync_all()?;
        drop(file);
        std::fs::rename(&temporary, &metadata)?;
        sync_parent(&metadata)?;
        self.handle()?.set_len(0)?;
        self.handle()?.sync_all()?;
        self.pending = false;
        self.failed = false;
        Ok(())
    }

    /// 验证日志帧后重放比页检查点更新的镜像
    fn recover(&mut self, data: &mut File) -> io::Result<()> {
        let path = sidecar(&self.path, ".page-wal");
        let mut file = match File::open(path) {
            Ok(file) => file,
            Err(error) if error.kind() == io::ErrorKind::NotFound => return Ok(()),
            Err(error) => return Err(error),
        };
        if file.metadata()?.len() == 0 {
            return Ok(());
        }
        let checkpoint = self.state.sequence;
        let mut previous = 0;
        loop {
            let mut header = [0; HEADER];
            match file.read_exact(&mut header) {
                Ok(()) => (),
                Err(error) if error.kind() == io::ErrorKind::UnexpectedEof => break,
                Err(error) => return Err(error),
            }
            let sequence = u64::from_le_bytes(header[4..12].try_into().expect("fixed header"));
            let lsn = u64::from_le_bytes(header[12..20].try_into().expect("fixed header"));
            let page = u64::from_le_bytes(header[20..28].try_into().expect("fixed header"));
            let len = u32::from_le_bytes(header[28..32].try_into().expect("fixed header")) as usize;
            if &header[..4] != b"CPW1"
                || sequence <= previous
                || (page == RESET && len != 0)
                || (page != RESET && len != self.state.page_size)
            {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "invalid page image header",
                ));
            }
            let mut bytes = vec![0; len];
            match file.read_exact(&mut bytes) {
                Ok(()) => (),
                Err(error) if error.kind() == io::ErrorKind::UnexpectedEof => break,
                Err(error) => return Err(error),
            }
            let mut checked = header[..32].to_vec();
            checked.extend_from_slice(&bytes);
            if checksum(&checked)
                != u64::from_le_bytes(header[32..40].try_into().expect("fixed header"))
            {
                return Err(io::Error::new(
                    io::ErrorKind::InvalidData,
                    "page image checksum mismatch",
                ));
            }
            previous = sequence;
            if sequence <= checkpoint {
                continue;
            }
            if page == RESET {
                data.set_len(0)?;
                self.state.pages.clear();
            } else {
                let offset = page
                    .checked_mul(self.state.page_size as u64)
                    .ok_or_else(|| io::Error::other("page offset overflow"))?;
                data.seek(SeekFrom::Start(offset))?;
                data.write_all(&bytes)?;
                self.state.pages.insert(page, lsn);
            }
            self.state.sequence = sequence;
        }
        data.sync_all()?;
        self.pending = true;
        self.checkpoint()
    }
}

/// 删除页文件及其版本与镜像文件
pub(crate) fn remove(path: &Path) -> io::Result<()> {
    for suffix in [".page-wal", ".pages", ".pages.pending", ""] {
        match std::fs::remove_file(sidecar(path, suffix)) {
            Ok(()) => (),
            Err(error) if error.kind() == io::ErrorKind::NotFound => (),
            Err(error) => return Err(error),
        }
    }
    sync_parent(path)
}

/// 改名时保留对应页版本与恢复材料
pub(crate) fn rename(from: &Path, to: &Path) -> io::Result<()> {
    for suffix in [".page-wal", ".pages", ".pages.pending", ""] {
        match std::fs::rename(sidecar(from, suffix), sidecar(to, suffix)) {
            Ok(()) => (),
            Err(error) if error.kind() == io::ErrorKind::NotFound => (),
            Err(error) => return Err(error),
        }
    }
    sync_parent(to)
}
