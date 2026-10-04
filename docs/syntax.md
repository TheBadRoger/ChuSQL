# SQL 语法

客户端选项与元命令见 [commands.md](commands.md)，类型与身份字段分别见 [types.md](types.md) 和 [users-and-roles.md](users-and-roles.md)。

## 查询、名称与事务

### 表名与列名

- 表级 DDL 可使用当前库限定名，例如选定 `sales` 后执行 `CREATE TABLE sales.orders (id INT)`；必须先选库，跨库 DDL 会被拒绝。

- 选定数据库后语句里直接写表名即可，例如 `SELECT name FROM orders WHERE orders.id = 1`。
- 表名可以带库前缀（`sales.orders`），列名可以带表限定符（`orders.id`）或库限定符（`sales.orders.id`），这几种写法指向同一张表和同一个列。
- 只有候选唯一时才自动推导前缀；同名的表或列出现在多个候选上会以 `ambiguous table` / `ambiguous column` 报错，需要写全。
- `FROM orders AS o` 之后只能用 `o.id`，不能再写 `orders.id`。

### SELECT 与派生表

- `SELECT` 支持 `WHERE`、`ORDER BY`、`LIMIT`、`JOIN` / `LEFT JOIN`、`GROUP BY` 与聚合，以及标量 / `IN` / `EXISTS` 子查询和量词比较（`op ANY` / `op ALL`）。
- 来源可以是派生表：`SELECT d.name FROM (SELECT name FROM users) d`。派生表必须带别名，内层不能引用外层列；输出列取裸列名（内层写 `u.id`，外层用 `d.id` 引用）。
- 投影列可以起别名：`SELECT count(*) AS total FROM orders ORDER BY total`，别名就是结果集的列名。`ORDER BY` 优先使用唯一的投影别名，再查来源列；重复的排序别名报歧义错误，来源列可以用限定名消除同名冲突。排序暂不接受任意表达式或列序号。
- 鉴权覆盖整棵来源树：查派生表时内层表也要有权限，只有外层引用的表有权限不够。

### 比较运算符

- 支持 `=`、`<>`（也可写 `!=`）、`>`、`>=`、`<`、`<=`；`<>` 与 `!=` 是同一个运算符。
- `>`、`>=`、`<`、`<=` 在列上有索引时会改写成索引范围扫描；`>=` / `<=` 是闭区间，端点值本身也在结果里。
- 任一操作数为 `NULL` 时比较结果是 `NULL`，`WHERE` 只保留结果为真的行。

### ANY / ALL

- `值 op ANY (SELECT 单列 …)` 表示子查询里至少有一行让比较成立；`值 op ALL (…)` 表示每一行都成立。
- `ANY` 的空子查询为假、`ALL` 的空子查询为真；子查询里出现 `NULL` 时按 `AND` / `OR` 的三值规则，条件可能为 `NULL`（`WHERE` 里等同不通过）。
- 量词与运算符组合示例：`<> ALL` 与 `NOT IN` 同义，`<> ANY` 表示至少存在一个不相等的值。

### WITH / CTE

- `WITH 名字 AS (SELECT …), 另一个 AS (SELECT …) SELECT … FROM 名字` 先定义再引用，名字可以带列清单（`WITH t(who) AS (SELECT name FROM users) …`）。
- `WITH` 在解析期就展开成派生表，所以引用处只能当普通表用：没有别名时用 CTE 名当限定名（`WITH t AS (…) SELECT t.name FROM t`）。
- 名字不分大小写；只能引用前面已经定义的 CTE，向前引用会当成普通表名去查（有同名表就用表，没有就报 `unknown table`）。
- 不支持递归 CTE，主语句与每个 CTE 都只能是 `SELECT`；同一个 CTE 被引用两次会各执行一次（不做物化）。
- 鉴权同样覆盖 CTE 里的表：`WITH d AS (SELECT name FROM orders) …` 要求对 `orders` 有权限。

### 事务与保存点

- `BEGIN`（或 `START TRANSACTION`）开始显式事务，`COMMIT` 提交，`ROLLBACK` 回滚。
- `SAVEPOINT 名字` 在事务里记一个回滚点，`ROLLBACK TO 名字`（可写 `ROLLBACK TO SAVEPOINT 名字`）退回该点但保留它，`RELEASE 名字` 丢掉该点与它之后的回滚点。
- 事务内只允许数据语句；DDL 与 `USE` 会被拒绝，`SHOW DATABASES` 这类不碰表数据的语句照常可用。
- 事务里的读都看开事务那一刻的快照：别的连接在事务期间提交的改动不会出现在事务里，本会话未提交的改动别的连接也看不见。
- 同一连接同时只能有一个事务，没有嵌套事务；回滚点只在事务内有效，事务结束就没了。开事务前要先选好数据库。
- 提交时按行 `id` 写回本事务改动过的行，事务没碰过的行不受影响；没有整数 `id` 的表按整表替换提交。
- 提交前会比对基线：本事务改过的行如果已被别的连接改掉或删掉，提交报 `serialization_failure` 并保留事务，由调用方 `ROLLBACK` 或重试。
- 提交是持久的：整批改动与一条提交标记一次落盘，提交后进程被强杀也会在重启时重放；提交前崩溃则整批丢弃，不会只落一半。

## 自定义类型 DOMAIN

由具有 `SUPERUSER` 属性的管理员选定工作库后执行：

```sql
USE sales;
CREATE DOMAIN short_label AS VARCHAR(32);
CREATE DOMAIN amount AS DECIMAL(10, 2);
CREATE TABLE products (id INT, label short_label, price amount);
INSERT INTO products (id, label, price) VALUES (1, 'book', 19.95);
SHOW DOMAINS;
```

`sales` 需事先存在。`short_label` 复用 `VARCHAR(32)` 的长度检查；`amount` 复用 `DECIMAL(10, 2)` 的数值处理。列保留 DOMAIN 名称，但赋值、比较、运算仍使用基础类型的语义。

已有表也可在 `ALTER TABLE ... ADD COLUMN` 或 `ALTER TABLE ... ALTER COLUMN ... TYPE` 中使用 DOMAIN。`SHOW DOMAINS` 返回当前库的 `domain`（名称）和 `base_type`（基础类型表示）。

名称不分大小写，长度为 1–48 个字符，使用 ASCII 字母、数字、下划线，并遵守 SQL 标识符的首字符规则。类型按工作库隔离：另一个库不会自动获得 `sales` 中的定义。类型名不得与已有类型冲突。

删除前必须解除所有列引用，例如：

```sql
DROP TABLE products;
DROP DOMAIN short_label;
DROP DOMAIN amount;
```

DOMAIN 可以基于内置基础类型或当前库已有的 DOMAIN 创建，例如：

```sql
CREATE DOMAIN short_text AS VARCHAR(32);
CREATE DOMAIN product_label AS short_text;
CREATE TABLE labels (label product_label);
```

`product_label` 保留对 `short_text` 的引用，最终仍按 `VARCHAR(32)` 检查。`SHOW DOMAINS` 中它的基础表示为 `domain(short_text,varchar(32))`。同样可以多层复用；被表列或其他 DOMAIN 引用的类型不能删除，需先删除引用者，再从外层到内层删除类型。不存在的基础 DOMAIN、自引用及循环定义会被拒绝。

当前不支持 DOMAIN 自身的 `CHECK`、`NOT NULL`、`DEFAULT`；这些约束仍可定义在表列上。创建和删除 DOMAIN 不能在显式事务内执行。

## 账号与权限

默认管理员账号为 `root`。普通账号创建后默认不拥有数据访问权限，需要管理员通过角色进行授权。

常用账号命令：

```sql
CREATE USER alice IDENTIFIED BY 'password';
ALTER USER alice IDENTIFIED BY 'new-password';
DROP USER alice;
```

用户和角色共用名称空间。`CREATE USER` 默认可以登录，`CREATE ROLE` 默认不能登录；两者默认启用，且不具有最高权限。管理员可以修改身份属性：

```sql
ALTER ROLE analyst LOGIN;
ALTER ROLE analyst IDENTIFIED BY 'analyst-password-1';
ALTER USER alice NOLOGIN;
ALTER USER alice DISABLED;
ALTER USER alice ENABLED LOGIN;
ALTER ROLE analyst SUPERUSER;
ALTER ROLE analyst NOSUPERUSER;
SHOW ROLES;
```

`SHOW ROLES` 展示全部身份的编号、名称、登录能力、最高权限标志、启用状态及创建时间，不返回口令。`ALTER USER` 与 `ALTER ROLE` 的属性和口令修改语法通用；同一属性不能重复指定。身份管理仅允许最高权限身份执行。

`SUPERUSER` 不依赖启动参数里的管理员名称（`--user`），也不会通过角色成员关系继承。禁用登录身份会拒绝登录；禁用权限角色会停止贡献继承权限。属性修改会使受影响的既有连接失效，下一次请求返回 `unauthorized` 后关闭。最后一个启用且可以登录的最高权限身份不能删除、禁用、取消登录或取消最高权限。

服务启动时迁移旧账号及角色，保留账号编号、口令、时间和原有授权关系。旧用户名与角色名重名时明确拒绝迁移，不合并身份。删除身份会同时清理其授权和两向成员关系；重建同名身份会分配新编号。

常用角色和授权命令：

```sql
CREATE ROLE analyst;
CREATE ROLE reviewer;
GRANT SELECT ON orders TO analyst;
GRANT SELECT ON orders TO reviewer WITH GRANT OPTION;
GRANT analyst TO reviewer;
GRANT reviewer TO alice;
REVOKE SELECT ON orders FROM analyst;
REVOKE analyst FROM reviewer;
REVOKE reviewer FROM alice;
DROP ROLE analyst;
```

当前表级权限包括：`SELECT`、`INSERT`、`UPDATE`、`DELETE` 和 `ALL`。

角色可以互相继承：`GRANT analyst TO reviewer` 让 `reviewer` 成为 `analyst` 的成员并取得其授权；继承会沿成员关系传递，成环的授权被拒绝（报 `conflict`），自继承同样拒绝。用户也可以直接接受表级授权或作为其他角色的成员。

管理员拥有完整权限；数据库、表、索引等结构管理操作仅允许管理员执行。

`GRANT ... WITH GRANT OPTION` 让拿到的授权可以再转授：普通身份只有手里那条授权带 grant option 时才能执行 `GRANT`，报错是 `grant option required: <权限> ON <表>`；`REVOKE` 仍然只允许管理员，收回授权会连它的 grant option 一起收掉，且不会级联收回别人转授出去的授权。转授权随角色继承一起传递。

系统目录访问还受目录管理属性控制，具体能力见 [用户与角色](users-and-roles.md)。


目录管理属性示例（仅 SUPERUSER 可设置）：

```sql
ALTER USER alice SYSTEM_CATALOG_MANAGER;
ALTER USER alice NOSYSTEM_CATALOG_MANAGER;
```
