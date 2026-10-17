# 命令与选项

[← 返回 README](../README.md)

本页面向命令行和 Web 使用者，说明启动参数、交互命令及编辑事务的行为。CLI 指命令行客户端。

类型见 [类型与函数](types.md)，身份与授权见 [用户与角色](users-and-roles.md)。

查询、事务、类型、用户与授权语法见 [SQL 语法](syntax.md)。

本页内容：

- [组件命令](#组件命令)
- [csql](#csql)
- [chusql-web](#chusql-web)
- [chusql-server](#chusql-server)
- [范式分析](#范式分析)

## 组件命令

安装器先运行 `csql-bootstrap` 初始化系统身份和类型目录，再启动服务；初始化失败即停止。重复引导与离线维护见 [系统引导与恢复](bootstrap.md)。

| 命令 | 作用 |
| --- | --- |
| `chusql-server` | 启动数据库服务 |
| `chusql-web` | 启动 Web 管理端 |
| `csql` | 启动命令行客户端 |
| `csql-web` | 按配置启动 Web 管理端 |
| `csql-bootstrap` | 初始化系统身份与内置类型目录 |

各组件默认使用同一份 `settings.toml`。需要临时使用其他配置时，可通过 `--config FILE` 指定。

## csql

基本用法：

```sh
csql [options]
```

常用选项：

| 选项 | 说明 |
| --- | --- |
| `-u, --user USER` | 指定登录账号 |
| `-d, --database DB` | 登录后选择数据库 |
| `-f, --format FORMAT` | 输出格式：`table`、`json` 或 `csv` |
| `-e, --execute SQL` | 执行一条 SQL 后退出 |
| `--normalize TABLE` | 只读分析表的范式；与 `--execute` 互斥，需 `--declaration` |
| `--declaration FILE` | UTF-8 JSON 业务依赖声明；范式输出使用 `--format text` 或 `json` |
| `--history FILE` | 指定历史记录文件 |
| `--sudo` | 使用本机 root / 提升管理员身份认证；需配置映射并启用身份属性，见 [用户与角色](users-and-roles.md#本机-sudo-认证) |
| `--config FILE` | 指定配置文件 |
| `-h, --help` | 显示帮助 |

### 历史记录与口令输入

CLI 按完整 SQL 或元命令记录历史。输入包含 `IDENTIFIED` 或 `PASSWORD`（忽略大小写）时整体排除，包括多行输入、普通字符串和标识符。

已有历史文件由用户按需清理。含这些文本的 SQL 解析错误会隐藏输入原文。

示例假定 `sales.orders` 已存在：

```sh
csql -d sales -e "SELECT * FROM orders LIMIT 5"
csql -d sales -f json -e "SELECT count(*) FROM orders"
```

在脚本里非交互使用（配合 `-e`）：

- 口令从标准输入读取，兼容 LF、CRLF 和无行尾；保留口令中的空格。
- 管道输入时口令提示和错误信息写在标准错误，标准输出保留查询结果，失败时退出码为 `1`。

### 交互模式

SQL 语句以分号结束，可跨多行输入。

常用元命令：

| 命令 | 作用 |
| --- | --- |
| `\q` | 退出 |
| `\?` | 查看帮助 |
| `\l` | 列出数据库 |
| `\dt` | 列出当前数据库中的表 |
| `\d <表>` | 查看表结构 |
| `\c <库>` | 切换数据库 |
| `\f <格式>` | 修改输出格式 |
| `\dr` | 查看角色与授权信息（管理员） |
| `\normalize TABLE --declaration FILE --format text\|json` | 只读分析表的范式 |

## chusql-web

基本用法：

```sh
chusql-web [options]
```

常用选项：

| 选项 | 说明 |
| --- | --- |
| `--port N` | 监听端口 |
| `--listen-host H` | 监听地址 |
| `--user NAME` | 管理员账号名 |
| `--config FILE` | 指定配置文件 |
| `-h, --help` | 显示帮助 |

静态资源目录固定为 `static`。其余请求大小、会话、登录限制、分页等参数通常通过 `settings.toml` 配置即可。

### 表格编辑与事务

首次打开业务数据表时建立编辑快照。同一页面内、同一数据库的跨表修改统一暂存。

编辑操作的效果如下：

| 操作 | 效果 |
| --- | --- |
| 提交全部 | 在服务端事务中执行，全部成功才持久化 |
| 逐项撤销 | 只移除本地编辑项，下次提交从原快照重放剩余编辑 |
| 撤销全部 | 执行 ROLLBACK，重新加载表和统计 |

### 编辑失败与提交重试

途中发生约束错误时，整批语句退回编辑保存点，完整编辑内容仍可修正。写写冲突不会覆盖其他会话，必须撤销并重新加载后重新编辑。提交响应断线时冻结编辑集；再次点击提交使用同一标识确认或重试同一批修改，已成功的提交不会重复执行。

### 页面关闭与快照释放

编辑事务绑定登录会话、页面和数据库；待提交修改存在时，先提交或撤销才能换库或运行 SQL。

关闭最后一个表标签且没有待提交修改时释放快照。关闭页面会请求回滚，退出登录会关闭连接并丢弃快照；若页面断线、回滚请求未送达，后台每分钟按配置的会话空闲与最长寿命清理连接。重新登录后建立新事务。

### SQL 控制台事务

SQL 控制台使用另一条服务端连接；SQL 的 BEGIN / COMMIT / ROLLBACK 绑定发起它的页面与查询标签。事务尚未结束时，其他控制台和普通 REST 写操作不能占用该连接；关闭所属查询标签或页面时请求回滚。系统身份通过独立管理入口修改，账号表只读，使用 SQL 用户与角色命令管理。

## chusql-server

基本用法：

```sh
chusql-server [options]
```

常用选项：

| 选项 | 说明 |
| --- | --- |
| `--port N` | 数据库服务监听端口 |
| `--listen-host H` | 数据库服务监听地址 |
| `--user NAME` | 管理员账号名 |
| `--max-message N` | 单条请求大小上限 |
| `--max-rows N` | 单次查询返回行数上限 |
| `--config FILE` | 指定配置文件 |
| `-h, --help` | 显示帮助 |

`chusql-server` 是数据目录的唯一直接访问者。Web、CLI 及其他客户端均通过数据库服务访问数据。

## 范式分析

范式分析检查表结构是否符合声明的业务依赖，用于发现结构上的冗余。schema 指列名、类型和约束等表结构信息。

分析依据 schema 和业务依赖声明，面向业务表，要求数据库 `CONNECT` 与目标表 `SELECT` 权限。数据迁移另行执行。

声明文件使用 UTF-8 JSON，字段名须与表结构一致。`declared_keys` 声明键列，`dependencies` 声明函数依赖，即哪些列能唯一决定其他列；`lhs` 是决定列，`rhs` 是被决定列。

`dependencies_complete` 表示声明是否覆盖全部业务依赖，`atomic_attributes` 表示字段是否具有业务上的原子性。示例如下：

```json
{
  "declared_keys": [["student", "course"]],
  "dependencies": [{"lhs": ["student"], "rhs": ["name"]}],
  "dependencies_complete": true,
  "atomic_attributes": true
}
```

将声明保存为 `dependencies.json` 后，执行以下命令。示例假定 `school` 数据库及 `enrollment` 表已存在：

```sh
csql -d school --normalize enrollment --declaration dependencies.json --format json
```

| 入口 | 用法 |
| --- | --- |
| CLI 批处理 | `--normalize` 与 `--execute` 互斥，需提供声明文件 |
| CLI 交互 | `\normalize enrollment --declaration dependencies.json --format text` |
| Web | 在资源管理器展开表，进入结构区的“范式分析”，填写声明 JSON |

> [!NOTE]
> 结论以用户声明为条件。完整性默认 `false`、原子性默认 `null`，信息不足时仅给出证据支持的范式结论；SQL 约束和样本数据不会自动证明业务依赖。

报告包含候选键、1NF / 2NF / 3NF / BCNF 结论、违反证据及一次 BCNF 分解建议。子关系需继续分析；依赖保持和现有数据是否符合声明需分别验证。Web 使用发起页面或控制台的事务视图；对象或 schema 变化须重新分析。

分析的资源限制和退出码如下：

| 项目 | 限制或行为 |
| --- | --- |
| 表列数 | 最多 12 列 |
| 依赖声明、键声明 | 各最多 256 项 |
| 运行时间 | 预算 5 秒 |
| 超限、权限或输入错误 | 明确返回失败，完整报告以成功响应为准 |
| CLI 成功获得报告 | 退出码为 `0`，包括违反范式或信息不足的结论 |
| CLI 请求失败 | 退出码非零 |
