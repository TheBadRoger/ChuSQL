use chusql_core_storage::catalog::Catalog;
use chusql_core_storage::protocol::Row;
use serde_json::json;

// 数据字典测试：补列、去重、未知表。



/// 用键值对构造一行
fn row(pairs: &[(&str, serde_json::Value)]) -> Row {
    let mut m = Row::new();
    for (k, v) in pairs {
        m.insert((*k).to_string(), v.clone());
    }
    m
}

/// 空文件得到空字典，未知表返回 None
#[test]
fn empty_file_gives_empty_catalog_and_unknown_table_returns_none() {    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("catalog.json");
    let c = Catalog::load(&path).unwrap();
    assert_eq!(c.table_names().len(), 0);
    assert!(c.describe("nope").is_none());
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
    assert_eq!(id_col.ty, "int");
    let name_col = s.columns.iter().find(|c| c.name == "name").unwrap();
    assert_eq!(name_col.ty, "str");
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

/// 删列同时清掉统计与索引
#[test]
fn remove_column_drops_definition_stats_and_index() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("catalog.json");

    let mut c = Catalog::load(&path).unwrap();
    c.ensure_columns("t", &row(&[("id", json!(1)), ("code", json!(5))]));
    c.add_index("t", "code").unwrap();
    c.rebuild_stats("t", &[row(&[("id", json!(1)), ("code", json!(5))])]);

    let removed = c.remove_column("t", "code").unwrap();
    assert!(removed, "first remove should hit the column");
    let s = c.describe("t").unwrap();
    assert_eq!(s.columns.len(), 1);
    assert_eq!(s.columns[0].name, "id");
    assert!(s.indexes.is_empty(), "index on code should be gone");
    assert!(!s.stats.contains_key("code"), "stats for code should be gone");

    let again = c.remove_column("t", "code").unwrap();
    assert!(!again);

    assert!(c.remove_column("nope", "code").is_err());
}

/// 直方图随插入累计、随删除递减，值跑出区间就拓宽
#[test]
fn stats_track_a_histogram_for_numeric_columns() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("catalog.json");

    let mut c = Catalog::load(&path).unwrap();
    let rows: Vec<Row> = (0..100)
        .map(|n| row(&[("id", json!(n)), ("name", json!(format!("u{}", n)))]))
        .collect();
    c.rebuild_stats("t", &rows);

    let s = c.describe("t").unwrap();
    let id = s.stats.get("id").unwrap();
    assert_eq!(id.distinct, 100);
    assert_eq!(id.lo, Some(0.0));
    assert_eq!(id.hi, Some(99.0));
    assert_eq!(id.hist.len(), 16);
    assert_eq!(id.hist.iter().sum::<u64>(), 100);
    let name = s.stats.get("name").unwrap();
    assert!(name.hist.is_empty(), "字符串列不进直方图");
    assert_eq!(name.lo, None);

    c.record_insert("t", &row(&[("id", json!(1000)), ("name", json!("x"))]));
    let s = c.describe("t").unwrap();
    let id = s.stats.get("id").unwrap();
    assert_eq!(id.lo, Some(0.0));
    assert_eq!(id.hi, Some(1000.0));
    assert_eq!(id.hist.iter().sum::<u64>(), 101);

    c.record_delete("t", &[row(&[("id", json!(1000)), ("name", json!("x"))])]);
    let s = c.describe("t").unwrap();
    assert_eq!(s.stats.get("id").unwrap().hist.iter().sum::<u64>(), 100);

    c.record_delete("t", &rows);
    let s = c.describe("t").unwrap();
    let id = s.stats.get("id").unwrap();
    assert_eq!(id.lo, None, "删空之后直方图清掉");
    assert_eq!(id.hist.iter().sum::<u64>(), 0);
}

/// 重复值删除保留仍存在的不同值计数
#[test]
fn duplicate_deletion_keeps_distinct_until_last_occurrence() {
    let mut catalog = Catalog::default();
    let duplicate = row(&[("id", json!(1)), ("v", json!(7))]);
    catalog.rebuild_stats("t", &[duplicate.clone(), duplicate.clone()]);
    catalog.record_delete("t", std::slice::from_ref(&duplicate));
    assert_eq!(catalog.describe("t").unwrap().stats["v"].distinct, 1);
    catalog.record_delete("t", &[duplicate]);
    assert_eq!(catalog.describe("t").unwrap().stats["v"].distinct, 0);
}

/// 去重封顶不影响直方图和其他列
#[test]
fn capped_distinct_keeps_histogram_and_other_columns_accurate() {
    let mut catalog = Catalog::default();
    let rows: Vec<Row> = (0..5000).map(|n| row(&[("id", json!(n)), ("v", json!(n % 2))])).collect();
    catalog.rebuild_stats("t", &rows);
    let schema = catalog.describe("t").unwrap();
    assert!(schema.stats["id"].capped);
    assert_eq!(schema.stats["id"].hist.iter().sum::<u64>(), 5000);
    assert_eq!(schema.stats["v"].distinct, 2);
    catalog.record_delete("t", &rows[..1000]);
    let schema = catalog.describe("t").unwrap();
    assert_eq!(schema.stats["id"].hist.iter().sum::<u64>(), 4000);
    assert_eq!(schema.stats["v"].distinct, 2);
    catalog.rebuild_stats("t", &rows[1000..]);
    assert_eq!(catalog.describe("t").unwrap().stats["id"].distinct, 4000);
}

/// 老类型名并到新写法，参数保留，未知类型原样
#[test]
fn normalize_type_maps_legacy_names() {
    use chusql_core_storage::catalog::normalize_type;
    assert_eq!(normalize_type("integer"), "int");
    assert_eq!(normalize_type("TEXT"), "str");
    assert_eq!(normalize_type("BOOLEAN"), "bool");
    assert_eq!(normalize_type("varchar(20)"), "varchar(20)");
    assert_eq!(normalize_type("numeric(8,2)"), "decimal(8,2)");
    assert_eq!(normalize_type("real"), "float");
    assert_eq!(normalize_type("weird"), "weird");
}

/// 老格式字典（列里没有新字段）能读进来，规范化后写回新写法
#[test]
fn legacy_catalog_loads_and_normalizes() {
    let dir = tempfile::tempdir().unwrap();
    let path = dir.path().join("catalog.json");
    std::fs::write(
        &path,
        r#"{"tables":{"legacy":{"columns":[{"name":"id","ty":"integer"},{"name":"name","ty":"text"}]}}}"#,
    )
    .unwrap();

    let mut c = Catalog::load(&path).unwrap();
    let s = c.describe("legacy").unwrap();
    assert_eq!(s.columns.len(), 2);
    assert!(s.columns[0].nullable, "老列默认可空");
    assert!(c.normalize_types());
    let types: Vec<&str> = c.describe("legacy").unwrap().columns.iter().map(|col| col.ty.as_str()).collect();
    assert_eq!(types, vec!["int", "str"]);
    assert!(!c.normalize_types(), "第二次没有可改的");
}
