# SQL 语法

[← 返回 README](../README.md)

本页供编写 SQL 时查阅，覆盖查询、事务、自定义类型和权限语法。示例里的数据库与表须事先存在，展示了创建步骤的示例除外。

客户端选项与元命令见[命令与选项](commands.md)，类型与身份字段分别见[类型说明](types.md)和[用户与角色](users-and-roles.md)。

本页内容：

- [查询、名称与事务](#查询名称与事务)
- [命名枚举 ENUM](#命名枚举-enum)
- [自定义类型 DOMAIN](#自定义类型-domain)
- [账号与权限](#账号与权限)

## 查询、名称与事务

### 表名与列名

名称按以下规则解析：

- 表级 DDL 可用当前库限定名，例如选定 `sales` 后执行 `CREATE TABLE sales.orders (id INT)`。先选库，跨库 DDL 会被拒绝。
- 选定数据库后直接写表名，例如 `SELECT name FROM orders WHERE orders.id = 1`。
- 表名可带库前缀（`sales.orders`），列名可带表限定符（`orders.id`）或库限定符（`sales.orders.id`），这几种写法指向同一张表和同一个列。
- 只有候选唯一时才自动推导前缀；多个候选同名时报 `ambiguous table` / `ambiguous column`，需要写全。
- 使用 `FROM orders AS o` 之后只能用 `o.id`，`orders.id` 不再有效。

### SELECT 与派生表

SELECT 支持以下来源和子句：

- 支持 `WHERE`、`GROUP BY`、`HAVING`、`ORDER BY`、`LIMIT`、`JOIN` / `LEFT JOIN` 与聚合，以及标量、`IN`、`EXISTS` 子查询和量词比较（`op ANY` / `op ALL`）。
- 来源可以是派生表：`SELECT d.name FROM (SELECT name FROM users) d`。派生表必须带别名，内层只引用内层可见列；输出列取裸列名，内层写 `u.id` 时外层用 `d.id` 引用。

#### 输出别名与排序

投影是查询最终返回的列或表达式。

- 输出别名：`SELECT count(*) AS total FROM orders ORDER BY total` 中别名就是结果集的列名。
- 名称排序：`ORDER BY` 的单独名称优先使用唯一的输出别名，再查来源列。重复的排序别名报歧义错误，来源列可用限定名消除同名冲突。
- 序号排序：接受从 1 开始的输出列序号，例如 `ORDER BY 2 DESC`。序号在 `*` 展开后解析，零或越界序号报错。
- 排序也接受表达式、函数、聚合与子查询，例如 `ORDER BY age + 1 DESC`。复合表达式中的列名只查来源，不引用输出别名；排序表达式在 LIMIT 前求值，其他投影在 LIMIT 后求值。NULL 升序在前、降序在后，同键保留原行序。

#### 分组后的过滤

- `HAVING` 在聚合之后、DISTINCT / ORDER BY / LIMIT 之前过滤，只保留布尔真；支持未投影的聚合、子查询及外层相关列，不引用投影别名。没有 GROUP BY 时 HAVING 形成一个整体组，空输入也有这个组；当前层未聚合的来源列必须出现在 GROUP BY 中。
- 鉴权覆盖整棵查询树：派生表、HAVING 与排序表达式中的子查询也要求内层表的 SELECT 权限。

以下示例按用户汇总订单，筛选订单数大于 1 的组，排序后返回前 10 组：

```sql
SELECT user_id, SUM(amount) AS total FROM orders
GROUP BY user_id HAVING COUNT(*) > 1
ORDER BY SUM(amount) + user_id DESC LIMIT 10;
```

### DISTINCT 与 ALL

`SELECT DISTINCT` 按完整输出值组合去重，保留首次出现的行，然后执行排序和 LIMIT。NULL 在去重时视为相同值，等值整数与整值浮点按数值语义合并。默认 SELECT 或显式 `SELECT ALL` 保留重复行。

内建 List、Maybe 和 Tuple 按完整类型的结构值比较、排序、分组和去重，也可用于 JOIN、IN、ANY / ALL 与 MIN / MAX。类型之间不隐式转换，SQL NULL 与 Maybe 的 Nothing 分开处理。规则见 [复合值的比较、排序与分组](types.md#比较排序与分组)。

```sql
SELECT DISTINCT user_id FROM orders ORDER BY 1 LIMIT 10;
SELECT DISTINCT user_id + 1 AS next_id FROM orders ORDER BY next_id;
```

DISTINCT 的排序项须出现在输出中，可用输出别名、选出的来源列、输出列序号或与投影完全相同的表达式。缺少比较能力的 List / Maybe / Tuple 类型拒绝 DISTINCT。去重支持连接、聚合、子查询、CTE 和当前连接的事务快照。

### 比较运算符

- 支持 `=`、`<>`（也可写 `!=`）、`>`、`>=`、`<`、`<=`；`<>` 与 `!=` 是同一个运算符。
- `>`、`>=`、`<`、`<=` 在列有索引时改写成索引范围扫描；`>=` / `<=` 是闭区间，端点值也在结果里。
- 任一操作数为 `NULL` 时比较结果是 `NULL`，`WHERE` 只保留结果为真的行。

### ANY / ALL

ANY / ALL 把值与单列子查询的结果逐项比较：

- `值 op ANY (SELECT 单列 …)` 表示子查询至少有一行让比较成立；`值 op ALL (…)` 表示每一行都成立。
- `ANY` 的空子查询为假，`ALL` 的空子查询为真；子查询出现 `NULL` 时按 `AND` / `OR` 的三值规则，条件可能为 `NULL`，在 `WHERE` 里等同不通过。
- `<> ALL` 与 `NOT IN` 同义，`<> ANY` 表示至少存在一个不相等的值。

### IN / EXISTS 的执行优化

半连接和反连接按右侧是否存在匹配行筛选左行，只返回左侧列。独立 `IN` / `NOT IN` 过滤，以及单表等值相关 `EXISTS` / `NOT EXISTS` 过滤可以用这种优化。

优化时右侧只扫描一次，左侧行序和重复行保留，右侧重复值不会放大输出。复杂条件仍走查询内物化和相关子查询路径，SQL 写法不变。

`NOT EXISTS` 保留没有匹配的行，包括空值键；`NOT IN` 的右侧出现 NULL 时，未匹配的非空键得到 NULL，过滤不通过。

右侧为空时 IN 为假、NOT IN 为真，左值为 NULL 时也一样；右侧非空时，NULL 左值按三值逻辑得到 NULL。表达式和半连接使用相同规则。

### WITH / CTE

公共表表达式（CTE）用 WITH 为子查询命名，供主查询引用。

- `WITH 名字 AS (SELECT …), 另一个 AS (SELECT …) SELECT … FROM 名字` 先定义再引用，名字可以带列清单：`WITH t(who) AS (SELECT name FROM users) …`。
- `WITH` 在解析期展开成派生表，引用处当普通表使用；没有别名时用 CTE 名当限定名：`WITH t AS (…) SELECT t.name FROM t`。
- 名字不分大小写；只能引用前面已定义的 CTE，向前引用会当成普通表名，有同名表就用表，没有就报 `unknown table`。
- CTE 的主语句和定义体都使用 `SELECT`，递归 CTE 待实现。重复引用时，相同执行子树在一次查询中物化复用，不同投影或优化后的子树分别执行；尚无 `MATERIALIZED` / `NOT MATERIALIZED` 语法。
- 鉴权同样覆盖 CTE 里的表：`WITH d AS (SELECT name FROM orders) …` 要求对 `orders` 有权限。

### 事务与保存点

开始事务前先选定数据库。

- `BEGIN`（或 `START TRANSACTION`）开始显式事务，`COMMIT` 提交，`ROLLBACK` 回滚。
- `BEGIN [TRANSACTION] ISOLATION LEVEL SERIALIZABLE` 或 `START TRANSACTION ISOLATION LEVEL SERIALIZABLE` 开始带读依赖复查的事务，用于先检查再写入的业务。
- `SAVEPOINT 名字` 记录回滚点，`ROLLBACK TO 名字`（也可写 `ROLLBACK TO SAVEPOINT 名字`）退回该点并保留它，`RELEASE 名字` 丢掉该点及其后的回滚点。

#### 事务可见性与限制

- 事务内只允许数据语句；DDL 与 `USE` 会被拒绝，`SHOW DATABASES` 这类不碰表数据的语句照常可用。
- 事务里的读都看开事务那一刻的快照：其他连接在事务期间提交的改动不可见，本会话未提交的改动其他连接也看不见。
- 同一连接同时只有一个事务，没有嵌套事务；回滚点只在事务内有效，事务结束即失效。开事务前先选好数据库。

#### 提交与冲突

- 存储层以稳定的内部行编号跟踪版本，把本事务的修改与其他连接已提交的修改合并；内部编号不要求表中声明整数 `id` 列。业务 `id` 可用于匹配修改前后的行，没有业务键时按快照行和剩余行顺序匹配。
- 提交前比对基线：本事务改过的行已被其他连接改掉或删掉时，提交报 `serialization_failure`。调用方先执行 `ROLLBACK`，再从 `BEGIN` 重做读取、检查和写入，只重发 `COMMIT` 不能解决。
- 合并修改后、发布前复查最终行集的 CHECK、非空和唯一约束；冲突返回 `serialization_failure`，其他表的修改也不会发布。
- SERIALIZABLE 写事务还会复查所有已执行数据语句涉及表的行集，包括子查询；读过的表发生变化即拒绝提交，因此也防止幻行。该模式按表复查，可能拒绝不同行上的合法并发修改；只读事务可以完成原快照读取。回退保存点也会撤销其后的读依赖。
- 修改冲突行或相同唯一键时等待持锁事务结束；死锁返回 `deadlock_detected`，总等待超过 5 秒返回 `lock_timeout`。这两种错误会中止存储事务，客户端仍须执行 `ROLLBACK` 清理会话后重新开始。回滚保存点不释放已取得的锁。
- 成功提交前写入整批 WAL 和提交标记，重启恢复只重放已提交事务，提交前崩溃的修改不会发布。断线后客户端可能无法确认提交结果，应先按稳定业务键查询，再决定是否重试。

## 命名枚举 ENUM

管理员在当前工作库声明可复用的枚举类型，再把名称用于列声明：

```sql
CREATE TYPE blog_role AS ENUM ('MEMBER', 'AUTHOR', 'MASTER', 'BANNED');
CREATE TABLE members (
    id INT PRIMARY KEY,
    role blog_role NOT NULL DEFAULT 'MEMBER'
);
INSERT INTO members (id) VALUES (1);
UPDATE members SET role = 'AUTHOR' WHERE id = 1;
SELECT id, role FROM members WHERE role = 'AUTHOR';
```

标签是单引号字符串，集合不能为空，标签不能重复，每个标签最多 63 个 UTF-8 字节且不含 NUL。大小写和空白精确匹配，中文及转义单引号均保留。未声明的值会使 INSERT、UPDATE、列默认值或类型转换失败。NULL 由列的可空约束决定，不声明为枚举标签。

枚举比较、排序及 MIN / MAX 使用声明顺序。上面的定义中 `MEMBER < AUTHOR < MASTER < BANNED`，与字典顺序无关。客户端收到字符串标签；内部保存枚举身份和标签集合，不把值当作普通文本。

不同命名枚举之间、枚举列与普通文本列之间不能隐式赋值或比较；字符串字面量按目标枚举校验。

支持 UNIQUE、PRIMARY KEY、索引、GROUP BY 和 DISTINCT，行大小仍受存储限制。命名类型与 DOMAIN 共用当前库的名称空间和依赖保护，用 `SHOW DOMAINS` 查看名称与枚举定义。

已有文本列可用 `ALTER TABLE members ALTER COLUMN role TYPE blog_role` 转换，发布前校验所有现存值。删除前先解除列和其他类型的引用：

```sql
DROP TABLE members;
DROP TYPE blog_role;
```

类型名称按 DOMAIN 的命名规则处理，定义随数据库持久化。当前支持命名 ENUM；列内 `ENUM (...)`、`ALTER TYPE ... ADD VALUE`、重命名标签和通用显式 CAST 留待以后。枚举之间不能直接转换，调整标签时先把列转为 TEXT，再转为另一个枚举，或迁移到新列。

行为参照 [PostgreSQL ENUM 的声明、排序与类型安全规则](https://www.postgresql.org/docs/current/datatype-enum.html)；存储布局、系统目录和未列出的扩展语法不承诺兼容。引导程序保留原生 BOOL / BOOLEAN，不预装 BoolText、YesNo 或 OnOff 这类项目自定枚举。

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

`sales` 需事先存在。`short_label` 复用 `VARCHAR(32)` 的长度检查，`amount` 复用 `DECIMAL(10, 2)` 的数值处理。列保留 DOMAIN 名称，赋值、比较、运算仍用基础类型的语义。

已有表也可在 `ALTER TABLE ... ADD COLUMN` 或 `ALTER TABLE ... ALTER COLUMN ... TYPE` 中使用 DOMAIN。`SHOW DOMAINS` 返回当前库的 `domain`（名称）和 `base_type`（基础类型表示）。

名称不分大小写，长度 1–48 个字符，使用 ASCII 字母、数字、下划线，并遵守 SQL 标识符的首字符规则。类型按工作库隔离，同名定义不跨库共享，类型名不能与已有类型冲突。

### 删除与嵌套定义

删除前解除所有列引用：

```sql
DROP TABLE products;
DROP DOMAIN short_label;
DROP DOMAIN amount;
```

DOMAIN 可以基于内置基础类型或当前库已有的 DOMAIN 创建：

```sql
CREATE DOMAIN short_text AS VARCHAR(32);
CREATE DOMAIN product_label AS short_text;
CREATE TABLE labels (label product_label);
```

`product_label` 保留对 `short_text` 的引用，最终仍按 `VARCHAR(32)` 检查。`SHOW DOMAINS` 中它的基础表示为 `domain(short_text,varchar(32))`。

可以多层复用；被表列或其他 DOMAIN 引用的类型不能删除，先删除引用者，再从外层到内层删除类型。不存在的基础 DOMAIN、自引用和循环定义会被拒绝。

### 默认值与检查约束

DOMAIN 可声明默认值、非空和具名或匿名检查条件，也可单独修改：

```sql
CREATE DOMAIN quantity AS INT DEFAULT 1 NOT NULL
    CONSTRAINT positive CHECK (VALUE > 0) CHECK (VALUE < 1000);
ALTER DOMAIN quantity SET DEFAULT 2;
ALTER DOMAIN quantity DROP DEFAULT;
ALTER DOMAIN quantity SET NOT NULL;
ALTER DOMAIN quantity DROP NOT NULL;
ALTER DOMAIN quantity ADD CONSTRAINT bounded CHECK (VALUE <= 100);
ALTER DOMAIN quantity DROP CONSTRAINT bounded;
```

`VALUE` 指当前值，不分大小写；CHECK 不接受表列、子查询或聚合，结果为 NULL 时通过。多层领域继承全部检查和非空限制；默认值按列、最近领域、基础领域的顺序选择，只在省略列时使用。

修改前校验受影响的已有行和默认值，失败不发布；并发写入冲突返回 `serialization_failure`，可重试整条语句。领域 DEFAULT 的修改不重写旧行。

创建、修改和删除 DOMAIN 都要求管理员选定工作库，不能在显式事务中执行。CASCADE、领域改名和修改基础类型留待以后；详细语义见 [类型说明](types.md#domain-约束与变更)。

## 账号与权限

默认管理员为 `root`，普通账号通过直接授权或角色继承取得数据权限。

### 创建与修改身份

```sql
CREATE USER alice IDENTIFIED BY 'password';
ALTER USER alice IDENTIFIED BY 'new-password';
DROP USER alice;
```

用户和角色共用名称空间。`CREATE USER` 默认可登录，`CREATE ROLE` 默认使用 NOLOGIN，两者默认 ENABLED、NOSUPERUSER。管理员可以修改身份属性：

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

### 查询身份与权限

`SHOW GRANTS` 允许登录身份查看自己的有效对象权限，输出 `identity`、`privilege`、`object` 和 `grant_option`。结果包含直接授权、启用角色继承、PUBLIC 默认 CONNECT 和所有权隐含权限；表范围授权按现存表展开，超级用户展示全部业务对象权限。

结果只包含业务对象；`FOR` 查询他人身份留待以后。该命令不要求选中工作库，事务中也读取当前权限目录。

`SHOW ROLES` 展示全部身份的编号、名称、登录能力、最高权限标志、启用状态及创建时间，口令保持隐藏。`ALTER USER` 与 `ALTER ROLE` 语法相同，每条语句为每项属性指定一次。身份管理只允许最高权限身份执行。

### 最高权限与身份保护

`SUPERUSER` 不依赖启动参数里的管理员名称（`--user`），也不通过角色成员关系继承。登录身份被禁用后拒绝登录，权限角色被禁用后停止贡献继承权限。属性修改会让受影响的既有连接失效，下一次请求返回 `unauthorized` 后关闭。最后一个启用且可登录的最高权限身份不能删除、禁用、取消登录或取消最高权限。

删除身份同时清理授权和双向成员关系；重建同名身份分配新编号。迁移边界见 [用户与角色](users-and-roles.md#升级兼容)。

### 角色成员与对象授权

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

表级权限包括 `SELECT`、`INSERT`、`UPDATE`、`DELETE`，数据库权限包括 `CONNECT`、`CREATE`。`ALL` 按对象类型展开。未限定的表名及 `*` 使用当前数据库，`库名.表名` 和 `库名.*` 可指定其他数据库；业务 GRANT 不接受系统对象。

```sql
REVOKE CONNECT ON DATABASE sales FROM PUBLIC;
GRANT CONNECT, CREATE ON DATABASE sales TO analyst;
GRANT SELECT ON TABLE sales.orders TO reviewer WITH GRANT OPTION;
GRANT SELECT ON sales.* TO analyst;
REVOKE SELECT ON sales.orders FROM reviewer;
```

角色可以互相继承：`GRANT analyst TO reviewer` 让 `reviewer` 成为 `analyst` 的成员并取得其授权。继承沿成员关系传递，成环的授权被拒绝（报 `conflict`），自继承同样拒绝。用户可以直接接受表级授权，也可以作为其他角色的成员。

最高权限身份拥有完整权限。数据库默认向 PUBLIC 开放 CONNECT，默认不授予 CREATE 或数据权限。具有数据库 CREATE 的身份可以建表并成为新表所有者；表所有者管理表、字段及索引，并可授予该表权限。数据库所有者可以删除自己的数据库，创建数据库和管理 DOMAIN 仍要求最高权限。

### 转授权与撤销

转授权指把自己持有的权限再授予其他身份。`GRANT ... WITH GRANT OPTION` 允许再转授，所有者也可以授予自身对象的权限。普通授予者撤销自己的授予，所有者及最高权限身份可撤销对象上的其他授予。

撤销时收回相应的 grant option，并级联清理失去合法来源的下游授权；仍有独立来源的授权保留。转授权能力随启用的角色继承，循环转授不能独立维持权限。REVOKE 之后所有者保留隐含权限。

`REVOKE GRANT OPTION FOR SELECT ON users FROM alice` 只移除指定授权来源的转授权能力，alice 的 SELECT 权限保留；失去唯一支持来源的下游授权级联撤销，独立来源继续有效。最高权限身份或对象所有者可撤销全部匹配来源，普通授予者只能撤销自己的来源。

### 系统目录管理属性

系统目录访问还受目录管理属性控制，具体能力见 [用户与角色](users-and-roles.md)。该属性只能由 SUPERUSER 设置：

```sql
ALTER USER alice SYSTEM_CATALOG_MANAGER;
ALTER USER alice NOSYSTEM_CATALOG_MANAGER;
ALTER USER alice ALLOW_SUDO_AUTH;
ALTER USER alice NOALLOW_SUDO_AUTH;
```
