use std::path::PathBuf;

use serde::Deserialize;

use crate::log::Level;

// 配置：优先环境变量，其次 TOML 文件，最后内置默认值（下面写的就是默认值）。
// 每个选项的来源都记在 Loaded::origins 里，server 启动时打进日志——读了哪个文件、
// 哪些选项走了默认回退，一眼能看出来。

/// 默认配置文件名（相对当前工作目录）；可用 CHUSQL_CONFIG 指到别处。
pub const DEFAULT_CONFIG_PATH: &str = "chusql-storage.toml";

/// 默认页大小（字节）。
pub const DEFAULT_PAGE_SIZE: usize = 4096;
/// 默认 B+ 树节点最大孩子数。
pub const DEFAULT_BTREE_ORDER: usize = 4;
/// 默认数据目录。
pub const DEFAULT_DATA_DIR: &str = "data";
/// 默认管道名。
pub const DEFAULT_PIPE_NAME: &str = "chusql-storage";
/// 默认日志等级。
pub const DEFAULT_LOG_LEVEL: &str = "info";

/// 页大小允许的范围。
const MIN_PAGE_SIZE: usize = 512;
const MAX_PAGE_SIZE: usize = 65536;
/// B+ 树节点至少要放得下 2 个键，分裂规则才成立。
const MIN_BTREE_ORDER: usize = 3;

/// 生效的配置。
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Config {
    pub page_size: usize,
    pub btree_order: usize,
    pub data_dir: PathBuf,
    pub pipe_name: String,
    pub log_level: Level,
}

impl Default for Config {
    fn default() -> Self {
        Config {
            page_size: DEFAULT_PAGE_SIZE,
            btree_order: DEFAULT_BTREE_ORDER,
            data_dir: PathBuf::from(DEFAULT_DATA_DIR),
            pipe_name: DEFAULT_PIPE_NAME.to_string(),
            log_level: Level::Info,
        }
    }
}

/// 一个选项的来源。
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Origin {
    Default,
    File(PathBuf),
    Env(&'static str),
}

impl Origin {
    /// 日志里那一列怎么写。
    pub fn describe(&self) -> String {
        match self {
            Origin::Default => "default".to_string(),
            Origin::File(p) => format!("file {}", p.display()),
            Origin::Env(k) => format!("env {}", k),
        }
    }
}

/// 生效配置 + 每个选项的来源。
#[derive(Debug, Clone)]
pub struct Loaded {
    pub config: Config,
    pub config_path: Option<PathBuf>,
    pub origins: Vec<(&'static str, String, Origin)>,
}

/// TOML 文件里的形状：字段全部可选，没写的就当"文件没提这一项"。
#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
struct FileConfig {
    page: Option<FilePage>,
    btree: Option<FileBtree>,
    storage: Option<FileStorage>,
    server: Option<FileServer>,
    log: Option<FileLog>,
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
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
struct FileServer {
    pipe_name: Option<String>,
}

#[derive(Debug, Default, Deserialize)]
#[serde(deny_unknown_fields)]
struct FileLog {
    level: Option<String>,
}

/// 从真实环境加载：CHUSQL_CONFIG（或当前目录的默认文件）+ 环境变量覆盖。
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

/// 解析 TOML 文本 + 套环境变量覆盖；测试直接调这个，不碰真实环境。
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

    let origins = vec![
        ("page.size", page_size.to_string(), page_origin),
        ("btree.order", btree_order.to_string(), order_origin),
        ("storage.data_dir", data_dir.clone(), dir_origin),
        ("server.pipe_name", pipe_name.clone(), pipe_origin),
        ("log.level", log_level.name().to_string(), log_origin),
    ];

    Ok(Loaded {
        config: Config {
            page_size,
            btree_order,
            data_dir: PathBuf::from(data_dir),
            pipe_name,
            log_level,
        },
        config_path: if text.is_some() { path } else { None },
        origins,
    })
}

/// 取一个选项：环境变量 → 配置文件 → 默认值，并记下它从哪来。
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

/// 检查选项本身和选项之间是否说得通。
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
