use std::path::PathBuf;

use serde::Deserialize;

use crate::log::Level;

// 配置：环境变量优先于 TOML，最后回退内置默认值。

// 默认值
/// 默认配置文件路径
pub const DEFAULT_CONFIG_PATH: &str = "chusql-storage.toml";

/// 默认池容量（页数）
pub const DEFAULT_POOL_SIZE: usize = 1024;
/// 默认页大小
pub const DEFAULT_PAGE_SIZE: usize = 4096;
/// 默认 B+ 树 order
pub const DEFAULT_BTREE_ORDER: usize = 4;
/// 默认数据目录
pub const DEFAULT_DATA_DIR: &str = "data";
/// 默认管道名
pub const DEFAULT_PIPE_NAME: &str = "chusql-storage";
/// 默认日志等级
pub const DEFAULT_LOG_LEVEL: &str = "info";

/// 页大小允许范围
const MIN_PAGE_SIZE: usize = 512;
const MAX_PAGE_SIZE: usize = 65536;
/// order 最小值
const MIN_BTREE_ORDER: usize = 3;

#[derive(Debug, Clone, PartialEq, Eq)]
// 配置
/// 生效的配置
pub struct Config {
    pub page_size: usize,
    pub btree_order: usize,
    pub pool_size: usize,
    pub data_dir: PathBuf,
    pub pipe_name: String,
    pub log_level: Level,
}

/// 全默认值
impl Default for Config {
    fn default() -> Self {
        Config {
            page_size: DEFAULT_PAGE_SIZE,
            btree_order: DEFAULT_BTREE_ORDER,
            pool_size: DEFAULT_POOL_SIZE,
            data_dir: PathBuf::from(DEFAULT_DATA_DIR),
            pipe_name: DEFAULT_PIPE_NAME.to_string(),
            log_level: Level::Info,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
/// 一个选项的来源
pub enum Origin {
    Default,
    File(PathBuf),
    Env(&'static str),
}

impl Origin {
    /// 来源的文案
    pub fn describe(&self) -> String {
        match self {
            Origin::Default => "default".to_string(),
            Origin::File(p) => format!("file {}", p.display()),
            Origin::Env(k) => format!("env {}", k),
        }
    }
}

#[derive(Debug, Clone)]
/// 配置 + 每项来源
pub struct Loaded {
    pub config: Config,
    pub config_path: Option<PathBuf>,
    pub origins: Vec<(&'static str, String, Origin)>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
// 文件形状
/// TOML 的形状（字段都可选）
struct FileConfig {
    page: Option<FilePage>,
    btree: Option<FileBtree>,
    buffer: Option<FileBuffer>,
    storage: Option<FileStorage>,
    server: Option<FileServer>,
    log: Option<FileLog>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
/// [buffer] 段
struct FileBuffer {
    pool_size: Option<usize>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
/// [page] 段
struct FilePage {
    size: Option<usize>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
/// [btree] 段
struct FileBtree {
    order: Option<usize>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
/// [storage] 段
struct FileStorage {
    data_dir: Option<String>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
/// [server] 段
struct FileServer {
    pipe_name: Option<String>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
/// [log] 段
struct FileLog {
    level: Option<String>,
}

// 加载
/// 按真实环境加载
pub fn load() -> Result<Loaded, String> {
    let path = match std::env::var("CHUSQL_CONFIG") {
        Ok(p) if !p.trim().is_empty() => Some(PathBuf::from(p)),
        _ => {
            let p = PathBuf::from(DEFAULT_CONFIG_PATH);
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
    resolve(text.as_deref(), path, &|k| std::env::var(k).ok())
}

/// 解析文本并套环境变量
pub fn resolve(
    text: Option<&str>,
    path: Option<PathBuf>,
    env: &dyn Fn(&str) -> Option<String>,
) -> Result<Loaded, String> {
    let file: FileConfig = match text {
        None => FileConfig::default(),
        Some(t) => toml::from_str(t).map_err(|e| match &path {
            Some(p) => format!("bad config file {}: {}", p.display(), e),
            None => format!("bad config: {}", e),
        })?,
    };

    let file_origin = Origin::File(
        path.clone()
            .unwrap_or_else(|| PathBuf::from(DEFAULT_CONFIG_PATH)),
    );

    let (page_size, page_origin) = pick(
        "CHUSQL_PAGE_SIZE",
        file.page.and_then(|p| p.size),
        DEFAULT_PAGE_SIZE,
        &file_origin,
        env,
        |s| {
            s.trim()
                .parse::<usize>()
                .map_err(|_| format!("not a number: {}", s))
        },
    )?;
    let (btree_order, order_origin) = pick(
        "CHUSQL_BTREE_ORDER",
        file.btree.and_then(|b| b.order),
        DEFAULT_BTREE_ORDER,
        &file_origin,
        env,
        |s| {
            s.trim()
                .parse::<usize>()
                .map_err(|_| format!("not a number: {}", s))
        },
    )?;
    let (pool_size, pool_origin) = pick(
        "CHUSQL_POOL_SIZE",
        file.buffer.and_then(|b| b.pool_size),
        DEFAULT_POOL_SIZE,
        &file_origin,
        env,
        |s| {
            s.trim()
                .parse::<usize>()
                .map_err(|_| format!("not a number: {}", s))
        },
    )?;
    let (data_dir, dir_origin) = pick(
        "CHUSQL_DATA_DIR",
        file.storage.and_then(|s| s.data_dir),
        DEFAULT_DATA_DIR.to_string(),
        &file_origin,
        env,
        |s| Ok(s.trim().to_string()),
    )?;
    let (pipe_name, pipe_origin) = pick(
        "CHUSQL_PIPE",
        file.server.and_then(|s| s.pipe_name),
        DEFAULT_PIPE_NAME.to_string(),
        &file_origin,
        env,
        |s| Ok(s.trim().to_string()),
    )?;
    let (log_level, log_origin) = pick(
        "CHUSQL_LOG",
        file.log.and_then(|l| l.level),
        DEFAULT_LOG_LEVEL.to_string(),
        &file_origin,
        env,
        |s| Ok(s.trim().to_string()),
    )?;
    let log_level = Level::parse(&log_level)
        .ok_or_else(|| format!("log.level is not one of off/error/warn/info/debug: {}", log_level))?;

    if data_dir.is_empty() {
        return Err("storage.data_dir must not be empty".to_string());
    }
    if pipe_name.is_empty() {
        return Err("server.pipe_name must not be empty".to_string());
    }
    validate_layout(page_size, btree_order)?;
    if pool_size == 0 {
        return Err("buffer.pool_size must be at least 1".to_string());
    }
    let origins = vec![
        ("page.size", page_size.to_string(), page_origin),
        ("btree.order", btree_order.to_string(), order_origin),
        ("buffer.pool_size", pool_size.to_string(), pool_origin),    // 新增
        ("storage.data_dir", data_dir.clone(), dir_origin),
        ("server.pipe_name", pipe_name.clone(), pipe_origin),
        ("log.level", log_level.name().to_string(), log_origin),
    ];

    Ok(Loaded {
        config: Config {
            page_size,
            btree_order,
            pool_size,
            data_dir: PathBuf::from(data_dir),
            pipe_name,
            log_level,
        },
        config_path: if text.is_some() { path } else { None },
        origins,
    })
}

/// 取一项：env -> 文件 -> 默认
fn pick<T: Clone>(
    env_key: &'static str,
    file_value: Option<T>,
    default: T,
    file_origin: &Origin,
    env: &dyn Fn(&str) -> Option<String>,
    parse: impl Fn(&str) -> Result<T, String>,
) -> Result<(T, Origin), String> {
    if let Some(raw) = env(env_key) {
        let value = parse(&raw).map_err(|e| format!("{}: {}", env_key, e))?;
        return Ok((value, Origin::Env(env_key)));
    }
    if let Some(value) = file_value {
        return Ok((value, file_origin.clone()));
    }
    Ok((default, Origin::Default))
}

// 校验
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
