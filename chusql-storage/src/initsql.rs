//! `scripts/init.sql` 的最小读取：只认 CREATE TABLE 与 CREATE INDEX。
//! 系统库的 schema 由这份脚本声明，存储进程按它建系统表。

use crate::protocol::SchemaColumn;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InitColumn {
    pub name: String,
    pub ty: String,
    pub nullable: bool,
    pub auto_increment: bool,
    pub primary_key: bool,
    pub unique: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InitTable {
    pub name: String,
    pub columns: Vec<InitColumn>,
    pub indexes: Vec<String>,
}

impl InitColumn {
    /// 转成数据字典里的列
    pub fn to_schema(&self) -> SchemaColumn {
        SchemaColumn {
            name: self.name.clone(),
            ty: self.ty.clone(),
            nullable: self.nullable,
            default: None,
            auto_increment: self.auto_increment,
            primary_key: self.primary_key,
            unique: self.unique,
            check: None,
        }
    }
}

/// 解析整份脚本；不认识的语句直接报错，避免悄悄漏建表
pub fn parse(text: &str) -> Result<Vec<InitTable>, String> {
    let mut tables: Vec<InitTable> = Vec::new();
    for statement in statements(text) {
        let words = statement.split_whitespace().collect::<Vec<_>>();
        let head = |index: usize| words.get(index).map(|word| word.to_ascii_uppercase());
        match (head(0).as_deref(), head(1).as_deref()) {
            (Some("CREATE"), Some("TABLE")) => {
                let name = clean_name(words.get(2).ok_or("CREATE TABLE needs a name")?);
                let body = statement.split_once('(').ok_or("CREATE TABLE needs a column list")?.1;
                let inner = body.rsplit_once(')').map(|(inner, _)| inner).unwrap_or(body);
                tables.push(InitTable { name, columns: columns(inner)?, indexes: Vec::new() });
            }
            (Some("CREATE"), Some("INDEX")) => {
                let table = clean_name(words.get(2).ok_or("CREATE INDEX needs a table")?);
                let column = statement
                    .split_once('(')
                    .and_then(|(_, rest)| rest.split_once(')'))
                    .map(|(column, _)| clean_name(column))
                    .ok_or("CREATE INDEX needs a column")?;
                let entry = tables
                    .iter_mut()
                    .find(|entry| entry.name.eq_ignore_ascii_case(&table))
                    .ok_or_else(|| format!("CREATE INDEX on unknown table: {table}"))?;
                entry.indexes.push(column);
            }
            _ => return Err(format!("unsupported statement: {}", statement.trim())),
        }
    }
    Ok(tables)
}

/// 一行一列：`name type [NOT NULL] [AUTO_INCREMENT] [PRIMARY KEY] [UNIQUE]`
fn columns(body: &str) -> Result<Vec<InitColumn>, String> {
    let mut out = Vec::new();
    for part in split_top_level(body) {
        let words = part.split_whitespace().collect::<Vec<_>>();
        if words.is_empty() {
            continue;
        }
        let keyword = words[0].to_ascii_uppercase();
        if matches!(keyword.as_str(), "PRIMARY" | "UNIQUE" | "KEY" | "CONSTRAINT" | "INDEX" | "CHECK" | "FOREIGN") {
            return Err(format!("table-level constraint is not supported: {}", part.trim()));
        }
        let name = clean_name(words[0]);
        let ty = words
            .get(1)
            .ok_or_else(|| format!("column {name} needs a type"))?
            .to_string();
        let upper = part.to_ascii_uppercase();
        out.push(InitColumn {
            name,
            ty,
            nullable: !upper.contains("NOT NULL"),
            auto_increment: upper.contains("AUTO_INCREMENT"),
            primary_key: upper.contains("PRIMARY KEY"),
            unique: upper.contains("UNIQUE"),
        });
    }
    Ok(out)
}

/// 按顶层的逗号切开（类型里的括号不算）
fn split_top_level(body: &str) -> Vec<String> {
    let mut parts = Vec::new();
    let mut current = String::new();
    let mut depth = 0;
    for ch in body.chars() {
        match ch {
            '(' => {
                depth += 1;
                current.push(ch);
            }
            ')' => {
                depth -= 1;
                current.push(ch);
            }
            ',' if depth == 0 => {
                parts.push(std::mem::take(&mut current));
            }
            _ => current.push(ch),
        }
    }
    if !current.trim().is_empty() {
        parts.push(current);
    }
    parts
}

/// 去掉反引号与空白
fn clean_name(raw: &str) -> String {
    raw.trim().trim_matches('`').trim_matches('"').trim().to_string()
}

/// 去掉注释后按分号切开
fn statements(text: &str) -> Vec<String> {
    let mut cleaned = String::new();
    let mut chars = text.chars().peekable();
    while let Some(ch) = chars.next() {
        if ch == '-' && chars.peek() == Some(&'-') {
            for next in chars.by_ref() {
                if next == '\n' {
                    cleaned.push('\n');
                    break;
                }
            }
        } else if ch == '/' && chars.peek() == Some(&'*') {
            chars.next();
            while let Some(next) = chars.next() {
                if next == '*' && chars.peek() == Some(&'/') {
                    chars.next();
                    break;
                }
            }
        } else {
            cleaned.push(ch);
        }
    }
    cleaned
        .split(';')
        .map(|statement| statement.trim().to_string())
        .filter(|statement| !statement.is_empty())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    /// 仓库里的系统库脚本必须始终能解析
    const SYSTEM_SCHEMA: &str = include_str!("../../scripts/init.sql");

    #[test]
    fn parses_the_repository_init_script() {
        let tables = parse(SYSTEM_SCHEMA).expect("scripts/init.sql must parse");
        let users = tables
            .iter()
            .find(|table| table.name == "__chusql_users")
            .expect("init.sql must declare the account table");
        let names: Vec<&str> = users.columns.iter().map(|column| column.name.as_str()).collect();
        assert_eq!(names, ["id", "user", "password_hash", "registered_at", "last_login_at", "revision"]);
        assert_eq!(users.columns[0].ty, "int");
        assert!(users.columns[0].primary_key && users.columns[0].auto_increment && !users.columns[0].nullable);
        assert_eq!(users.columns[1].ty, "varchar(64)");
        assert!(users.columns[1].unique);
        assert_eq!(users.columns[4].ty, "timestamp");
        assert!(users.columns[4].nullable);
        assert_eq!(users.indexes, ["user"]);
    }

    #[test]
    fn rejects_unknown_statements_and_table_level_constraints() {
        assert!(parse("DROP TABLE users;").is_err());
        assert!(parse("CREATE TABLE t (id int, PRIMARY KEY (id));").is_err());
        assert!(parse("CREATE INDEX missing (user);").is_err());
    }

    #[test]
    fn ignores_comments_and_blank_statements() {
        let tables = parse("-- note\nCREATE TABLE t (a int NOT NULL);\n\n/* done */\n").unwrap();
        assert_eq!(tables.len(), 1);
        assert_eq!(tables[0].columns[0].name, "a");
        assert!(!tables[0].columns[0].nullable);
    }
}
