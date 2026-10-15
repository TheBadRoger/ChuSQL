# SQL 语法

[← 返回 README](../README.md)

**本页导航：** [查询、名称与事务](#查询名称与事务) · [自定义类型 DOMAIN](#自定义类型-domain) · [账号与权限](#账号与权限)

客户端选项与元命令见 [commands.md](commands.md)，类型与身份字段分别见 [types.md](types.md) 和 [users-and-roles.md](users-and-roles.md)。

## 查询、名称与事务

### 表名与列名

- 表级 DDL 可使用当前库限定名，例如选定 `sales` 后执行 `CREATE TABLE sales.orders (id INT)`；必须先选库，跨库 DDL 会被拒绝。
- 选定数据库后语句里直接写表名即可，例如 `SELECT name FROM orders WHERE orders.id = 1`。
- 表名可以带库前缀（`sales.orders`），列名可以带表限定符（`orders.id`）或库限定符（`sales.orders.id`），这几种写法指向同一张表和同一个列。
- 只有候选唯一时才自动推导前缀；同名的表或列出现在多个候选上会以 `ambiguous table` / `ambiguous column` 报错，需要写全。
- `FROM orders AS o` 之后只能用 `o.id`，不能再写 `orders.id`。

### SELECT 与派生表

- `SELECT` 支持 `WHERE`、`GROUP BY`、`HAVING`、`ORDER BY`、`LIMIT`、`JOIN` / `LEFT JOIN` 与聚合，以及标量 / `IN` / `EXISTS` 子查询和量词比较（`op ANY` / `op ALL`）。
- 来源可以是派生表：`SELECT d.name FROM (SELECT name FROM users) d`。派生表必须带别名，内层不能引用外层列；输出列取裸列名（内层写 `u.id`，外层用 `d.id` 引用）。
- 投影列可以起别名：`SELECT count(*) AS total FROM orders ORDER BY total`，别名就是结果集的列名。`ORDER BY` 的单独名称优先使用唯一的投影别名，再查来源列；重复的排序别名报歧义错误，来源列可以用限定名消除同名冲突。排序接受从 1 开始的输出列序号（例如 `ORDER BY 2 DESC`），在 `*` 展开后解析，零或越界序号报错。
- 排序也支持表达式、函数、聚合与子查询，例如 `ORDER BY age + 1 DESC`。复合表达式中的列名只查来源，不能引用输出别名；排序表达式在 LIMIT 前求值，其他投影在 LIMIT 后求值。NULL 升序在前、降序在后，同键保留原行序。
- `HAVING` 在聚合之后、DISTINCT / ORDER BY / LIMIT 之前过滤，只保留布尔真；支持未投影的聚合、子查询及外层相关列，不能引用投影别名。没有 GROUP BY 时 HAVING 形成一个整体组，空输入也有这个组；当前层未聚合的来源列必须出现在 GROUP BY 中。
- 鉴权覆盖整棵查询树：派生表、HAVING 与排序表达式中的子查询也要求内层表的 SELECT 权限。

```sql
SELECT user_id, SUM(amount) AS total FROM orders
GROUP BY user_id HAVING COUNT(*) > 1
ORDER BY SUM(amount) + user_id DESC LIMIT 10;
```

### DISTINCT 与 ALL

`SELECT DISTINCT` 按完整输出值组合去重，保留首次出现的行，然后执行排序和 LIMIT。NULL 在去重时视为相同值；等值整数与整值浮点按既有数值语义合并。默认 SELECT 或显式 `SELECT ALL` 保留重复行。

```sql
SELECT DISTINCT user_id FROM orders ORDER BY 1 LIMIT 10;
SELECT DISTINCT user_id + 1 AS next_id FROM orders ORDER BY next_id;
```

DISTINCT 的排序项必须出现在输出中；排序可使用输出别名、选出的来源列、输出列序号或与投影完全相同的表达式。缺少比较能力的 List / Maybe / Tuple 类型仍拒绝 DISTINCT。去重支持连接、聚合、子查询、CTE 和当前连接的事务快照。

### 比较运算符

- 支持 `=`、`<>`（也可写 `!=`）、`>`、`>=`、`<`、`<=`；`<>` 与 `!=` 是同一个运算符。
- `>`、`>=`、`<`、`<=` 在列上有索引时会改写成索引范围扫描；`>=` / `<=` 是闭区间，端点值本身也在结果里。
- 任一操作数为 `NULL` 时比较结果是 `NULL`，`WHERE` 只保留结果为真的行。

### ANY / ALL

- `值 op ANY (SELECT 单列 …)` 表示子查询里至少有一行让比较成立；`值 op ALL (…)` 表示每一行都成立。
- `ANY` 的空子查询为假、`ALL` 的空子查询为真；子查询里出现 `NULL` 时按 `AND` / `OR` 的三值规则，条件可能为 `NULL`（`WHERE` 里等同不通过）。
- 量词与运算符组合示例：`<> ALL` 与 `NOT IN` 同义，`<> ANY` 表示至少存在一个不相等的值。

### IN / EXISTS 的执行优化

独立 `IN` / `NOT IN` 过滤，以及单表等值相关 `EXISTS` / `NOT EXISTS` 过滤可使用哈希半连接／反连接：右侧只扫描一次，左侧行序和重复行保留，右侧重复值不会放大输出。复杂条件仍使用查询内物化及相关子查询路径；SQL 写法不变。

`NOT EXISTS` 保留没有匹配的行，包括空值键；`NOT IN` 的右侧出现 NULL 时，未匹配的非空键得到 NULL，过滤不通过。右侧为空时，IN 为假、NOT IN 为真，包括左值为 NULL 的情况；右侧非空时，NULL 左值按三值逻辑得到 NULL。表达式和半连接使用相同规则。

### WITH / CTE

- `WITH 名字 AS (SELECT …), 另一个 AS (SELECT …) SELECT … FROM 名字` 先定义再引用，名字可以带列清单（`WITH t(who) AS (SELECT name FROM users) …`）。
- `WITH` 在解析期就展开成派生表，所以引用处只能当普通表用：没有别名时用 CTE 名当限定名（`WITH t AS (…) SELECT t.name FROM t`）。
- 名字不分大小写；只能引用前面已经定义的 CTE，向前引用会当成普通表名去查（有同名表就用表，没有就报 `unknown table`）。
- 不支持递归 CTE，主语句与每个 CTE 都只能是 `SELECT`。重复引用时，相同执行子树在一次查询中物化复用；不同投影或优化后的子树仍分别执行，尚无 `MATERIALIZED` / `NOT MATERIALIZED` 语法。
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

`SHOW GRANTS` 允许登录身份查看自己的有效对象权限，输出 `identity`、`privilege`、`object` 和 `grant_option`。结果包含直接授权、启用角色继承、PUBLIC 默认 CONNECT 和所有权隐含权限；表范围授权按现存表展开，超级用户展示全部业务对象权限。系统对象不列出，不接受 `FOR` 查询他人身份。该命令不要求选中工作库；事务中也读取当前权限目录。

`SHOW ROLES` 展示全部身份的编号、名称、登录能力、最高权限标志、启用状态及创建时间，不返回口令。`ALTER USER` 与 `ALTER ROLE` 的属性和口令修改语法通用；同一属性不能重复指定。身份管理仅允许最高权限身份执行。

`SUPERUSER` 不依赖启动参数里的管理员名称（`--user`），也不会通过角色成员关系继承。禁用登录身份会拒绝登录；禁用权限角色会停止贡献继承权限。属性修改会使受影响的既有连接失效，下一次请求返回 `unauthorized` 后关闭。最后一个启用且可以登录的最高权限身份不能删除、禁用、取消登录或取消最高权限。

删除身份同时清理授权和双向成员关系；重建同名身份分配新编号。升级迁移的兼容边界见 [用户与角色](users-and-roles.md#升级兼容)。

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

表级权限包括 `SELECT`、`INSERT`、`UPDATE`、`DELETE`；数据库权限包括 `CONNECT`、`CREATE`。`ALL` 按对象类型展开。未限定的表名及 `*` 使用当前数据库，`库名.表名` 和 `库名.*` 可指定其他数据库；系统对象不能通过业务 GRANT 授权。

```sql
REVOKE CONNECT ON DATABASE sales FROM PUBLIC;
GRANT CONNECT, CREATE ON DATABASE sales TO analyst;
GRANT SELECT ON TABLE sales.orders TO reviewer WITH GRANT OPTION;
GRANT SELECT ON sales.* TO analyst;
REVOKE SELECT ON sales.orders FROM reviewer;
```

角色可以互相继承：`GRANT analyst TO reviewer` 让 `reviewer` 成为 `analyst` 的成员并取得其授权；继承会沿成员关系传递，成环的授权被拒绝（报 `conflict`），自继承同样拒绝。用户也可以直接接受表级授权或作为其他角色的成员。

最高权限身份拥有完整权限。数据库默认向 PUBLIC 开放 CONNECT，不默认授予 CREATE 或数据权限。具有数据库 CREATE 的身份可建表，并成为新表所有者；表所有者可管理表、字段及索引，并授予该表权限。数据库所有者可删除自己的数据库，创建数据库与管理 DOMAIN 仍要求最高权限。

`GRANT ... WITH GRANT OPTION` 允许再转授；所有者也可授予自身对象的权限。普通授予者可撤销自己的授予，所有者及最高权限身份可撤销对象上的其他授予。撤销时收回相应 grant option，并级联清理失去合法来源的下游授权；仍有独立来源的授权保留。转授权能力随启用的角色继承，循环转授不能独立维持权限。REVOKE 不移除所有者的隐含权限。

`REVOKE GRANT OPTION FOR SELECT ON users FROM alice` 只移除指定授权来源的转授权能力，保留 alice 的 SELECT 权限；失去唯一支持来源的下游授权会级联撤销，独立来源继续有效。最高权限身份或对象所有者可撤销全部匹配来源，普通授予者只能撤销自己的来源；所有权自身的权限不受 REVOKE 影响。

系统目录访问还受目录管理属性控制，具体能力见 [用户与角色](users-and-roles.md)。

目录管理属性示例（仅 SUPERUSER 可设置）：

```sql
ALTER USER alice SYSTEM_CATALOG_MANAGER;
ALTER USER alice NOSYSTEM_CATALOG_MANAGER;
ALTER USER alice ALLOW_SUDO_AUTH;
ALTER USER alice NOALLOW_SUDO_AUTH;
```
