use chusql_storage::catalog::Catalog;
use chusql_storage::protocol::{ColumnType, Row};
use serde_json::json;

// 数据字典测试：补列、去重、未知表。



fn row(pairs: &[(&str, serde_json::Value)]) -> Row {
    let mut m = Row::new();
    for (k, v) in pairs {
        m.insert((*k).to_string(), v.clone());
    }
    m
}

/// 空文件得到空字典
#[test]
fn empty_file_gives_empty_catalog() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("catalog.json");
    let c = Catalog::load(&path).unwrap();
    assert_eq!(c.table_names().len(), 0);
}

/// 补列之后能查到 schema
#[test]
fn ensure_then_describe() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("catalog.json");

    let mut c = Catalog::load(&path).unwrap();
    c.ensure_columns("users", &row(&[("id", json!(1)), ("name", json!("Alice"))]));
    c.save(&path).unwrap();

    let c2 = Catalog::load(&path).unwrap();
    let s = c2.describe("users").unwrap();
    assert_eq!(s.columns.len(), 2);
    let names: Vec<&str> = s.columns.iter().map(|c| c.name.as_str()).collect();
    assert!(names.contains(&"id"));
    assert!(names.contains(&"name"));
    let id_col = s.columns.iter().find(|c| c.name == "id").unwrap();
    assert_eq!(id_col.ty, ColumnType::Int);
    let name_col = s.columns.iter().find(|c| c.name == "name").unwrap();
    assert_eq!(name_col.ty, ColumnType::Str);
}

/// 再插入不会重复列
#[test]
fn second_insert_does_not_duplicate_columns() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("catalog.json");

    let mut c = Catalog::load(&path).unwrap();
    c.ensure_columns("users", &row(&[("id", json!(1))]));
    c.ensure_columns("users", &row(&[("id", json!(2))]));
    assert_eq!(c.describe("users").unwrap().columns.len(), 1);
}

/// 未知表返回 None
#[test]
fn describe_unknown_returns_none() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("catalog.json");
    let c = Catalog::load(&path).unwrap();
    assert!(c.describe("nope").is_none());
}
