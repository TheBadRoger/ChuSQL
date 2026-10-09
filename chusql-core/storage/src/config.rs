use std::path::PathBuf;

use serde::Deserialize;

use crate::log::Level;

// 全局配置：只从 TOML 文件读，缺项回退内置默认值；
// 存储相关分区由本模块解析校验，其它层的分区一概不碰。

/// 配置文件名
pub const CONFIG_FILE_NAME: &str = "settings.toml";
/// 配置目录名（随系统惯例放在用户配置目录下）
pub const APP_DIR_NAME: &str = "ChuSQL";
/// Unix 下的数据目录名（XDG 惯例小写，跟安装脚本的默认安装目录一致）
pub const UNIX_APP_DIR_NAME: &str = "chusql";
/// 数据子目录名
pub const DATA_DIR_NAME: &str = "data";

pub const DEFAULT_POOL_SIZE: usize = 1024;
pub const DEFAULT_PAGE_SIZE: usize = 4096;
pub const DEFAULT_BTREE_ORDER: usize = 4;
pub const DEFAULT_LOG_LEVEL: &str = "info";
/// 默认日志输出目录（相对启动目录）
pub const DEFAULT_LOG_FILES: &str = "./logs";

const MIN_PAGE_SIZE: usize = 512;
const MAX_PAGE_SIZE: usize = 65536;
const MIN_BTREE_ORDER: usize = 3;

/// 默认配置文件路径（按平台惯例）
pub fn default_config_path() -> PathBuf {
    #[cfg(windows)]
    {
        let appdata = std::env::var_os("APPDATA").filter(|v| !v.is_empty());
        if let Some(appdata) = appdata {
            return PathBuf::from(appdata).join(APP_DIR_NAME).join(CONFIG_FILE_NAME);
        }
    }
    #[cfg(not(windows))]
    {
        let xdg = std::env::var_os("XDG_CONFIG_HOME").filter(|v| !v.is_empty());
        if let Some(xdg) = xdg {
            return PathBuf::from(xdg).join(APP_DIR_NAME).join(CONFIG_FILE_NAME);
        }
        let home = std::env::var_os("HOME").filter(|v| !v.is_empty());
        if let Some(home) = home {
            return PathBuf::from(home)
                .join(".config")
                .join(APP_DIR_NAME)
                .join(CONFIG_FILE_NAME);
        }
    }
    PathBuf::from(APP_DIR_NAME).join(CONFIG_FILE_NAME)
}

/// Windows 默认数据目录
#[cfg(windows)]
pub fn default_data_dir() -> PathBuf {
    let local = std::env::var_os("LOCALAPPDATA").filter(|v| !v.is_empty());
    if let Some(local) = local {
        return PathBuf::from(local).join(APP_DIR_NAME).join(DATA_DIR_NAME);
    }
    let roaming = std::env::var_os("APPDATA").filter(|v| !v.is_empty());
    if let Some(roaming) = roaming {
        return PathBuf::from(roaming).join(APP_DIR_NAME).join(DATA_DIR_NAME);
    }
    PathBuf::from(APP_DIR_NAME).join(DATA_DIR_NAME)
}

/// Unix 默认数据目录（XDG 惯例）
#[cfg(not(windows))]
pub fn default_data_dir() -> PathBuf {
    let xdg = std::env::var_os("XDG_DATA_HOME").filter(|v| !v.is_empty());
    if let Some(xdg) = xdg {
        return PathBuf::from(xdg)
            .join(UNIX_APP_DIR_NAME)
            .join(DATA_DIR_NAME);
    }
    let home = std::env::var_os("HOME").filter(|v| !v.is_empty());
    if let Some(home) = home {
        return PathBuf::from(home)
            .join(".local")
            .join("share")
            .join(UNIX_APP_DIR_NAME)
            .join(DATA_DIR_NAME);
    }
    PathBuf::from(UNIX_APP_DIR_NAME).join(DATA_DIR_NAME)
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub page_size: usize,
    pub btree_order: usize,
    pub pool_size: usize,
    pub data_dir: PathBuf,
    pub log_files: PathBuf,
    pub log_level: Level,
}

impl Default for Config {
    /// 内置默认配置
    fn default() -> Self {
        Config {
            page_size: DEFAULT_PAGE_SIZE,
            btree_order: DEFAULT_BTREE_ORDER,
            pool_size: DEFAULT_POOL_SIZE,
            data_dir: default_data_dir(),
            log_files: PathBuf::from(DEFAULT_LOG_FILES),
            log_level: Level::Info,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Origin {
    Default,
    File(PathBuf),
}

impl Origin {
    /// 来源的文案
    pub fn describe(&self) -> String {
        match self {
            Origin::Default => "default".to_string(),
            Origin::File(p) => format!("file {}", p.display()),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Loaded {
    pub config: Config,
    pub config_path: Option<PathBuf>,
    pub origins: Vec<(&'static str, String, Origin)>,
}

#[derive(Debug, Default, Deserialize)]
struct FileConfig {
    page: Option<FilePage>,
    btree: Option<FileBtree>,
    buffer: Option<FileBuffer>,
    storage: Option<FileStorage>,
    log: Option<FileLog>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
struct FileBuffer {
    pool_size: Option<usize>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
struct FilePage {
    size: Option<usize>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
struct FileBtree {
    order: Option<usize>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
struct FileStorage {
    data_dir: Option<String>,
    log_files: Option<String>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
struct FileLog {
    level: Option<String>,
}

/// 加载配置：显式路径优先，其次默认位置
pub fn load(explicit: Option<&str>) -> Result<Loaded, String> {
    let explicit = explicit.map(str::trim).filter(|p| !p.is_empty());
    let path = match explicit {
        Some(p) => {
            let p = PathBuf::from(p);
            if !p.is_file() {
                return Err(format!("config file not found: {}", p.display()));
            }
            Some(p)
        }
        None => {
            let p = default_config_path();
            if p.is_file() { Some(p) } else { None }
        }
    };
    let text = match &path {
        Some(p) => Some(
            std::fs::read_to_string(p)
                .map_err(|e| format!("cannot read config file {}: {}", p.display(), e))?,
        ),
        None => None,
    };
    resolve(text.as_deref(), path)
}

/// 解析文本：文件 -> 默认
pub fn resolve(text: Option<&str>, path: Option<PathBuf>) -> Result<Loaded, String> {
    let file: FileConfig = match text {
        None => FileConfig::default(),
        Some(t) => toml::from_str(t).map_err(|e| match &path {
            Some(p) => format!("bad config file {}: {}", p.display(), e),
            None => format!("bad config: {}", e),
        })?,
    };

    let file_origin = Origin::File(
        path.clone().unwrap_or_else(default_config_path),
    );

    let (page_size, page_origin) = pick(
        file.page.and_then(|p| p.size),
        DEFAULT_PAGE_SIZE,
        &file_origin,
    );
    let (btree_order, order_origin) = pick(
        file.btree.and_then(|b| b.order),
        DEFAULT_BTREE_ORDER,
        &file_origin,
    );
    let (pool_size, pool_origin) = pick(
        file.buffer.and_then(|b| b.pool_size),
        DEFAULT_POOL_SIZE,
        &file_origin,
    );
    let storage = file.storage.unwrap_or_default();
    let (data_dir, dir_origin) = pick(
        storage.data_dir,
        default_data_dir().to_string_lossy().into_owned(),
        &file_origin,
    );
    let (log_files, log_files_origin) = pick(
        storage.log_files,
        DEFAULT_LOG_FILES.to_string(),
        &file_origin,
    );
    let (log_level, log_origin) = pick(
        file.log.and_then(|l| l.level),
        DEFAULT_LOG_LEVEL.to_string(),
        &file_origin,
    );

    let data_dir = data_dir.trim().to_string();
    let log_files = log_files.trim().to_string();
    let log_level = log_level.trim();

    let log_level = Level::parse(log_level)
        .ok_or_else(|| format!("log.level is not one of off/error/warn/info/debug: {}", log_level))?;

    if data_dir.is_empty() {
        return Err("storage.data_dir must not be empty".to_string());
    }
    if log_files.is_empty() {
        return Err("storage.log_files must not be empty".to_string());
    }
    validate_layout(page_size, btree_order)?;
    if pool_size == 0 {
        return Err("buffer.pool_size must be at least 1".to_string());
    }
    let origins = vec![
        ("page.size", page_size.to_string(), page_origin),
        ("btree.order", btree_order.to_string(), order_origin),
        ("buffer.pool_size", pool_size.to_string(), pool_origin),
        ("storage.data_dir", data_dir.clone(), dir_origin),
        ("storage.log_files", log_files.clone(), log_files_origin),
        ("log.level", log_level.name().to_string(), log_origin),
    ];

    Ok(Loaded {
        config: Config {
            page_size,
            btree_order,
            pool_size,
            data_dir: PathBuf::from(data_dir),
            log_files: PathBuf::from(log_files),
            log_level,
        },
        config_path: if text.is_some() { path } else { None },
        origins,
    })
}

/// 取一项：文件 -> 默认
fn pick<T>(file_value: Option<T>, default: T, file_origin: &Origin) -> (T, Origin) {
    if let Some(value) = file_value {
        return (value, file_origin.clone());
    }
    (default, Origin::Default)
}

/// 检查选项是否自洽
fn validate_layout(page_size: usize, btree_order: usize) -> Result<(), String> {
    if !(MIN_PAGE_SIZE..=MAX_PAGE_SIZE).contains(&page_size) || !page_size.is_power_of_two() {
        return Err(format!(
            "page.size must be a power of two between {} and {}, got {}",
            MIN_PAGE_SIZE, MAX_PAGE_SIZE, page_size
        ));
    }
    if btree_order < MIN_BTREE_ORDER {
        return Err(format!(
            "btree.order must be at least {}, got {}",
            MIN_BTREE_ORDER, btree_order
        ));
    }
    if !crate::btree::fits_in_page(page_size, btree_order) {
        return Err(format!(
            "btree.order {} does not fit in a {}-byte page (max {})",
            btree_order,
            page_size,
            crate::btree::max_order(page_size)
        ));
    }
    Ok(())
}
