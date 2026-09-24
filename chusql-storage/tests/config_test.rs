use std::path::PathBuf;

use chusql_storage::config::{
    self, Config, DEFAULT_BTREE_ORDER, DEFAULT_DATA_DIR, DEFAULT_PAGE_SIZE, DEFAULT_PIPE_NAME,
    Origin,
};
use chusql_storage::log::Level;

// 配置测试：默认值、文件覆盖、环境变量优先、非法值报错。

/// 无文件时全默认
#[test]
fn defaults_without_file() {
    let loaded = config::resolve(None, None, &|_| None).unwrap();
    assert_eq!(loaded.config, Config::default());
    assert!(loaded.config_path.is_none());
    assert!(loaded.origins.iter().all(|(_, _, o)| *o == Origin::Default));
    assert_eq!(loaded.config.page_size, DEFAULT_PAGE_SIZE);
    assert_eq!(loaded.config.btree_order, DEFAULT_BTREE_ORDER);
    assert_eq!(loaded.config.data_dir, PathBuf::from(DEFAULT_DATA_DIR));
    assert_eq!(loaded.config.pipe_name, DEFAULT_PIPE_NAME);
    assert_eq!(loaded.config.log_level, Level::Info);
}

/// 只覆盖文件里写了的项
#[test]
fn file_overrides_only_written_keys() {
    let path = PathBuf::from("t.toml");
    let loaded = config::resolve(Some("[page]\nsize = 8192\n"), Some(path.clone()), &|_| None)
        .unwrap();

    assert_eq!(loaded.config.page_size, 8192);
    assert_eq!(loaded.config.btree_order, DEFAULT_BTREE_ORDER);
    assert_eq!(loaded.config.data_dir, PathBuf::from(DEFAULT_DATA_DIR));
    assert_eq!(loaded.config_path, Some(path.clone()));

    let page = loaded.origins.iter().find(|(n, _, _)| *n == "page.size").unwrap();
    assert_eq!(page.2, Origin::File(path));
    let order = loaded.origins.iter().find(|(n, _, _)| *n == "btree.order").unwrap();
    assert_eq!(order.2, Origin::Default);
}

/// 六项全写都生效
#[test]
fn file_all_keys() {
    let text = "[page]\nsize = 8192\n[btree]\norder = 32\n[buffer]\npool_size = 128\n[storage]\ndata_dir = \"mydata\"\n[server]\npipe_name = \"mypipe\"\n[log]\nlevel = \"debug\"\n";
    let path = PathBuf::from("t.toml");
    let loaded = config::resolve(Some(text), Some(path.clone()), &|_| None).unwrap();

    assert_eq!(loaded.config.page_size, 8192);
    assert_eq!(loaded.config.btree_order, 32);
    assert_eq!(loaded.config.pool_size, 128);
    assert_eq!(loaded.config.data_dir, PathBuf::from("mydata"));
    assert_eq!(loaded.config.pipe_name, "mypipe");
    assert_eq!(loaded.config.log_level, Level::Debug);
    assert!(
        loaded
            .origins
            .iter()
            .all(|(_, _, o)| *o == Origin::File(path.clone()))
    );
}

/// 环境变量优先于文件
#[test]
fn env_beats_file() {
    let text = "[storage]\ndata_dir = \"from-file\"\n";
    let env = |k: &str| (k == "CHUSQL_DATA_DIR").then(|| "from-env".to_string());
    let loaded = config::resolve(Some(text), Some(PathBuf::from("t.toml")), &env).unwrap();

    assert_eq!(loaded.config.data_dir, PathBuf::from("from-env"));
    let dir = loaded
        .origins
        .iter()
        .find(|(n, _, _)| *n == "storage.data_dir")
        .unwrap();
    assert_eq!(dir.2, Origin::Env("CHUSQL_DATA_DIR"));
}

/// 日志等级非法要报错
#[test]
fn rejects_bad_log_level() {
    let err = config::resolve(Some("[log]\nlevel = \"loud\"\n"), None, &|_| None).unwrap_err();
    assert!(err.contains("log.level"), "got {}", err);
}

/// 页大小非法要报错
#[test]
fn rejects_bad_page_size() {
    let err = config::resolve(Some("[page]\nsize = 3000\n"), None, &|_| None).unwrap_err();
    assert!(err.contains("page.size"), "got {}", err);

    let err = config::resolve(Some("[page]\nsize = 128\n"), None, &|_| None).unwrap_err();
    assert!(err.contains("page.size"), "got {}", err);
}

/// order 非法要报错
#[test]
fn rejects_bad_btree_order() {
    let err = config::resolve(Some("[btree]\norder = 2\n"), None, &|_| None).unwrap_err();
    assert!(err.contains("btree.order"), "got {}", err);

    let err = config::resolve(Some("[btree]\norder = 300\n"), None, &|_| None).unwrap_err();
    assert!(err.contains("does not fit"), "got {}", err);
}

/// 未知配置键要报错
#[test]
fn rejects_unknown_key() {
    let err = config::resolve(Some("[page]\nsizes = 4096\n"), None, &|_| None).unwrap_err();
    assert!(err.contains("bad config"), "got {}", err);
}

/// 环境变量非法要报错
#[test]
fn rejects_bad_env_value() {
    let env = |k: &str| (k == "CHUSQL_PAGE_SIZE").then(|| "big".to_string());
    let err = config::resolve(None, None, &env).unwrap_err();
    assert!(err.contains("CHUSQL_PAGE_SIZE"), "got {}", err);
}

/// 来源文案正确
#[test]
fn origin_description() {
    assert_eq!(Origin::Default.describe(), "default");
    assert_eq!(Origin::Env("CHUSQL_PIPE").describe(), "env CHUSQL_PIPE");
    assert_eq!(
        Origin::File(PathBuf::from("a.toml")).describe(),
        format!("file {}", PathBuf::from("a.toml").display())
    );
}
