# SQL 语法

[← 返回 README](../README.md)

本页提供常用写法和主要限制。示例中的数据库与表须事先存在，展示创建步骤的示例除外。

## 查询、名称与事务

选择数据库后可直接写表名，也可使用 `库名.表名`。列重名时加表限定符；表声明别名后使用别名引用。

### 查询

支持 WHERE、INNER / LEFT JOIN、聚合、GROUP BY、HAVING、ORDER BY、LIMIT、DISTINCT，以及标量、IN、EXISTS 和 ANY / ALL 子查询。

```sql
SELECT user_id, SUM(amount) AS total
FROM orders
GROUP BY user_id
HAVING COUNT(*) > 1
ORDER BY total DESC
LIMIT 10;
```

ORDER BY 可用输出别名、从 1 开始的列序号或表达式。NULL 升序在前、降序在后；同键保持原行序。DISTINCT 按完整输出去重，排序项须来自输出。

派生表须带别名。WITH 定义非递归的公共表表达式（CTE），按定义顺序引用：

```sql
WITH active AS (SELECT id, name FROM users WHERE age > 18)
SELECT name FROM active ORDER BY name;
```

比较遇到 NULL 时结果为 NULL，WHERE 仅保留 TRUE。空子查询的 ANY 为 FALSE、ALL 为 TRUE；使用 NOT IN 时注意右侧 NULL 会影响筛选结果。

### 写入

```sql
CREATE TABLE notes (id INT PRIMARY KEY, title TEXT);
INSERT INTO notes (id, title) VALUES (1, 'Hello');
UPDATE notes SET title = 'Updated' WHERE id = 1;
DELETE FROM notes WHERE id = 1;
```

写入前检查类型、默认值和约束，检查失败保留原数据。表结构变更在显式事务之外执行。

### 事务与保存点

```sql
BEGIN;
SAVEPOINT before_edit;
UPDATE notes SET title = 'Draft' WHERE id = 1;
ROLLBACK TO before_edit;
COMMIT;
```

BEGIN 读取开始时的快照及自身修改，COMMIT 提交，ROLLBACK 回滚。需要先检查再写入时，可用 `BEGIN ISOLATION LEVEL SERIALIZABLE`；该模式按表复查读取依赖，可能产生保守冲突。

同一连接同时运行一个事务。DDL 和 USE 在事务结束后执行；保存点回滚保留已取得的锁。

冲突、死锁和等待超时分别返回 `serialization_failure`、`deadlock_detected`、`lock_timeout`，锁等待上限为 5 秒。收到这些错误后执行 ROLLBACK，再重做完整事务。提交响应断线时，先核对业务结果再决定是否重试。

## 命名枚举 ENUM

管理员在当前库声明枚举，列值按标签集合校验：

```sql
CREATE TYPE blog_role AS ENUM ('MEMBER', 'AUTHOR', 'MASTER');
CREATE TABLE members (id INT PRIMARY KEY, role blog_role);
INSERT INTO members (id, role) VALUES (1, 'MEMBER');
SELECT * FROM members WHERE role = 'MEMBER';
```

标签区分大小写，比较和排序按声明顺序。ENUM 支持索引、唯一约束和分组，不同枚举或普通文本之间不隐式转换。

解除引用后用 `DROP TYPE blog_role` 删除。当前支持命名枚举，修改标签集合需迁移列数据。类型限制见[类型说明](types.md#命名枚举-enum)。

## 自定义类型 DOMAIN

DOMAIN 为基础类型命名，并附加约束：

```sql
CREATE DOMAIN quantity AS INT DEFAULT 1 NOT NULL
    CONSTRAINT positive CHECK (VALUE > 0);
CREATE TABLE products (id INT, stock quantity);
ALTER DOMAIN quantity SET DEFAULT 2;
ALTER DOMAIN quantity ADD CONSTRAINT bounded CHECK (VALUE <= 100);
ALTER DOMAIN quantity DROP CONSTRAINT bounded;
SHOW DOMAINS;
```

CHECK 使用 VALUE 表示当前值。DOMAIN 可继承其他 DOMAIN，列默认值优先于领域默认值；显式 NULL 按空值检查。

管理员在当前库创建、修改或删除 DOMAIN。修改前校验已有数据，解除引用后用 DROP DOMAIN 删除；这些操作在显式事务之外执行。完整约束规则见[类型说明](types.md#domain-约束与变更)。

## 账号与权限

管理员创建身份、设置属性并分配角色：

```sql
CREATE USER alice IDENTIFIED BY 'password';
ALTER USER alice IDENTIFIED BY 'new-password';
CREATE ROLE analyst;
GRANT analyst TO alice;
ALTER USER alice ENABLED LOGIN;
SHOW ROLES;
SHOW GRANTS;
```

用户默认可登录，角色默认 NOLOGIN，两者默认启用且使用 NOSUPERUSER。身份属性见[用户与角色](users-and-roles.md)。

### 对象授权

数据库权限为 CONNECT、CREATE，表权限为 SELECT、INSERT、UPDATE、DELETE：

```sql
GRANT CONNECT ON DATABASE sales TO analyst;
GRANT SELECT ON sales.orders TO analyst;
GRANT SELECT ON sales.* TO analyst;
GRANT SELECT ON sales.orders TO alice WITH GRANT OPTION;
REVOKE SELECT ON sales.orders FROM alice;
```

`ALL` 按对象范围展开，`库名.*` 覆盖现有和未来的表。PUBLIC 默认提供业务库 CONNECT，可用 REVOKE 撤销。

WITH GRANT OPTION 允许转授权，撤销时清理失去来源的下游授权，独立来源保留。表创建者拥有表所有权，最高权限由登录身份自身属性决定。

删除使用 DROP USER / DROP ROLE，系统保护最后一个启用且可登录的管理员。
