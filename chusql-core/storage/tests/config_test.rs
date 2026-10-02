use std::path::PathBuf;
use std::sync::Mutex;

use chusql_core_storage::config::{self, Config, DEFAULT_BTREE_ORDER, DEFAULT_PAGE_SIZE, Origin};
use chusql_core_storage::log::Level;

// 配置测试：默认值、文件覆盖、环境变量彻底无效、非法值报错。

/// set_var 改的是进程环境，用例之间要串行，免得互相污染
static ENV_LOCK: Mutex<()> = Mutex::new(());

/// 拿锁（前面的用例 panic 过也照样拿）
fn env_guard() -> std::sync::MutexGuard<'static, ()> {
    ENV_LOCK.lock().unwrap_or_else(|e| e.into_inner())
}

/// 无文件时全默认
#[test]
fn defaults_without_file() {
    let _guard = env_guard();
    let loaded = config::resolve(None, None).unwrap();
    assert_eq!(loaded.config, Config::default());
    assert!(loaded.config_path.is_none());
    assert!(loaded.origins.iter().all(|(_, _, o)| *o == Origin::Default));
    assert_eq!(loaded.config.page_size, DEFAULT_PAGE_SIZE);
    assert_eq!(loaded.config.btree_order, DEFAULT_BTREE_ORDER);
    assert_eq!(loaded.config.data_dir, config::default_data_dir());
    assert_eq!(loaded.config.log_level, Level::Info);
}

/// 只覆盖文件里写了的项
#[test]
fn file_overrides_only_written_keys() {
    let _guard = env_guard();
    let path = PathBuf::from("t.toml");
    let loaded = config::resolve(Some("[page]\nsize = 8192\n"), Some(path.clone()))
        .unwrap();

    assert_eq!(loaded.config.page_size, 8192);
    assert_eq!(loaded.config.btree_order, DEFAULT_BTREE_ORDER);
    assert_eq!(loaded.config.data_dir, config::default_data_dir());
    assert_eq!(loaded.config_path, Some(path.clone()));

    let page = loaded.origins.iter().find(|(n, _, _)| *n == "page.size").unwrap();
    assert_eq!(page.2, Origin::File(path));
    let order = loaded.origins.iter().find(|(n, _, _)| *n == "btree.order").unwrap();
    assert_eq!(order.2, Origin::Default);
}

/// 五项全写都生效
#[test]
fn file_all_keys() {
    let _guard = env_guard();
    let text = "[page]\nsize = 8192\n[btree]\norder = 32\n[buffer]\npool_size = 128\n[storage]\ndata_dir = \"mydata\"\n[log]\nlevel = \"debug\"\n";
    let path = PathBuf::from("t.toml");
    let loaded = config::resolve(Some(text), Some(path.clone())).unwrap();

    assert_eq!(loaded.config.page_size, 8192);
    assert_eq!(loaded.config.btree_order, 32);
    assert_eq!(loaded.config.pool_size, 128);
    assert_eq!(loaded.config.data_dir, PathBuf::from("mydata"));
    assert_eq!(loaded.config.log_level, Level::Debug);
    assert!(
        loaded
            .origins
            .iter()
            .all(|(_, _, o)| *o == Origin::File(path.clone()))
    );
}

/// 设了 CHUSQL_* 环境变量也不生效
#[test]
fn env_vars_are_ignored_entirely() {
    let _guard = env_guard();
    unsafe {
        std::env::set_var("CHUSQL_DATA_DIR", "from-env");
        std::env::set_var("CHUSQL_PAGE_SIZE", "8192");
    }

    let text = "[storage]\ndata_dir = \"from-file\"\n";
    let loaded = config::resolve(Some(text), Some(PathBuf::from("t.toml"))).unwrap();
    assert_eq!(loaded.config.data_dir, PathBuf::from("from-file"));
    assert_eq!(loaded.config.page_size, DEFAULT_PAGE_SIZE);
    let dir = loaded
        .origins
        .iter()
        .find(|(n, _, _)| *n == "storage.data_dir")
        .unwrap();
    assert_eq!(dir.2, Origin::File(PathBuf::from("t.toml")));

    let builtin = config::resolve(None, None).unwrap();
    assert_eq!(builtin.config.data_dir, config::default_data_dir());
    assert_eq!(builtin.config.page_size, DEFAULT_PAGE_SIZE);

    unsafe {
        std::env::remove_var("CHUSQL_DATA_DIR");
        std::env::remove_var("CHUSQL_PAGE_SIZE");
    }
}

/// 固定配置路径在系统配置目录的 ChuSQL 下
#[test]
fn default_config_path_follows_system_convention() {
    let _guard = env_guard();
    let path = config::default_config_path();
    assert_eq!(path.file_name().unwrap(), "chusql.toml");
    assert_eq!(path.parent().unwrap().file_name().unwrap(), "ChuSQL");
    assert!(path.ends_with(format!("ChuSQL{}chusql.toml", std::path::MAIN_SEPARATOR)), "got {}", path.display());

    // 本机 APPDATA 存在时必须是 %APPDATA%\ChuSQL\chusql.toml
    #[cfg(windows)]
    if let Some(appdata) = std::env::var_os("APPDATA") {
        assert_eq!(
            path,
            PathBuf::from(appdata).join("ChuSQL").join("chusql.toml")
        );
    }
}

/// 默认数据目录落地按系统惯例
#[test]
fn default_data_dir_follows_system_convention() {
    let _guard = env_guard();
    let dir = config::default_data_dir();
    let expected_parent = if cfg!(windows) { "ChuSQL" } else { "chusql" };
    assert_eq!(dir.file_name().unwrap(), "data");
    assert_eq!(dir.parent().unwrap().file_name().unwrap(), expected_parent);
    // 老的内置默认是仓库里的相对路径 ../localdata，现在不能再指回仓库
    assert_ne!(dir, PathBuf::from("../localdata"));

    #[cfg(windows)]
    if let Some(local) = std::env::var_os("LOCALAPPDATA").filter(|v| !v.is_empty()) {
        assert_eq!(dir, PathBuf::from(local).join("ChuSQL").join("data"));
    } else if let Some(roaming) = std::env::var_os("APPDATA").filter(|v| !v.is_empty()) {
        assert_eq!(dir, PathBuf::from(roaming).join("ChuSQL").join("data"));
    }

    #[cfg(not(windows))]
    if let Some(xdg) = std::env::var_os("XDG_DATA_HOME").filter(|v| !v.is_empty()) {
        assert_eq!(dir, PathBuf::from(xdg).join("chusql").join("data"));
    } else if let Some(home) = std::env::var_os("HOME").filter(|v| !v.is_empty()) {
        assert_eq!(
            dir,
            PathBuf::from(home)
                .join(".local")
                .join("share")
                .join("chusql")
                .join("data")
        );
    }
}

/// 显式给了一个不存在的路径：必须报错，不能静默回退默认
#[test]
fn explicit_missing_path_is_an_error() {
    let _guard = env_guard();
    let missing = std::env::temp_dir().join("chusql-no-such-config-9f21.toml");
    assert!(!missing.is_file());

    let err = config::load(Some(missing.to_str().unwrap())).unwrap_err();
    assert!(
        err.contains("config file not found") || err.contains("cannot read config file"),
        "got {}",
        err
    );
    assert!(err.contains("chusql-no-such-config-9f21.toml"), "got {}", err);
}

/// 不给 --config 且无配置文件时用内置默认
#[test]
fn load_without_explicit_path_uses_builtin_defaults() {
    let _guard = env_guard();
    let default = config::default_config_path();
    let loaded = config::load(None).unwrap();
    if default.is_file() {
        assert_eq!(loaded.config_path, Some(default));
    } else {
        assert_eq!(loaded.config, Config::default());
        assert_eq!(loaded.config.data_dir, config::default_data_dir());
        assert!(loaded.origins.iter().all(|(_, _, o)| *o == Origin::Default));
        assert!(loaded.config_path.is_none());
    }
}

/// [server] 分区归别的层管；本层拼写错误要报错
#[test]
fn server_section_belongs_to_another_layer() {
    let _guard = env_guard();
    let db_server = "[server]\nhost = \"0.0.0.0\"\nport = 7778\nmax_rows = 10\n";
    let loaded = config::resolve(Some(db_server), None).unwrap();
    assert_eq!(loaded.config.data_dir, config::default_data_dir());

    let typo = "[storage]\ndata_dirs = \"oops\"\n";
    assert!(config::resolve(Some(typo), None).is_err());
}

/// 日志等级非法要报错
#[test]
fn rejects_bad_log_level() {
    let _guard = env_guard();
    let err = config::resolve(Some("[log]\nlevel = \"loud\"\n"), None).unwrap_err();
    assert!(err.contains("log.level"), "got {}", err);
}

/// 页大小或 order 非法都要报错
#[test]
fn rejects_bad_numeric_limits() {
    let _guard = env_guard();
    for text in ["[page]\nsize = 3000\n", "[page]\nsize = 128\n"] {
        let err = config::resolve(Some(text), None).unwrap_err();
        assert!(err.contains("page.size"), "{}: {}", text, err);
    }

    let err = config::resolve(Some("[btree]\norder = 2\n"), None).unwrap_err();
    assert!(err.contains("btree.order"), "got {}", err);

    let err = config::resolve(Some("[btree]\norder = 300\n"), None).unwrap_err();
    assert!(err.contains("does not fit"), "got {}", err);
}

/// 未知配置键要报错
#[test]
fn rejects_unknown_key() {
    let _guard = env_guard();
    let err = config::resolve(Some("[page]\nsizes = 4096\n"), None).unwrap_err();
    assert!(err.contains("bad config"), "got {}", err);
}

/// 坏的页面尺寸只能来自文件
#[test]
fn rejects_bad_page_size_from_file() {
    let _guard = env_guard();
    unsafe { std::env::set_var("CHUSQL_PAGE_SIZE", "big") };

    let err = config::resolve(Some("[page]\nsize = 3000\n"), None).unwrap_err();
    assert!(err.contains("page.size"), "got {}", err);

    unsafe { std::env::remove_var("CHUSQL_PAGE_SIZE") };
}
