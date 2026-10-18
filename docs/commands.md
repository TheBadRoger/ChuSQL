# 命令与选项

[← 返回 README](../README.md)

`csql` 是命令行客户端，`csql-web` 启动 Web 管理端，`chusql-server` 提供数据库服务，`csql-bootstrap` 负责初始化和离线维护。各组件用 `--config FILE` 指定配置文件，`--help` 查看完整参数。

## csql

```sh
csql [options]
```

| 选项 | 说明 |
| --- | --- |
| `-u, --user USER` | 指定登录账号 |
| `-d, --database DB` | 登录后选择数据库 |
| `-f, --format FORMAT` | 输出格式：`table`、`json` 或 `csv` |
| `-e, --execute SQL` | 执行一条 SQL 后退出 |
| `--normalize TABLE` | 只读分析表的范式；与 `--execute` 互斥，需 `--declaration` |
| `--declaration FILE` | UTF-8 JSON 业务依赖声明；范式输出使用 `--format text` 或 `json` |
| `--history FILE` | 指定历史记录文件 |
| `--sudo` | 使用本机 root 或提升管理员身份认证；需配置映射并启用身份属性，见 [用户与角色](users-and-roles.md#本机-sudo-认证) |
| `--config FILE` | 指定配置文件 |
| `-h, --help` | 显示帮助 |

例如，在 sales 库执行查询：

```sh
csql -d sales -e "SELECT * FROM orders LIMIT 5"
```

交互登录会提示输入口令，脚本模式从标准输入读取口令。CLI 历史按完整语句记录，含 IDENTIFIED 或 PASSWORD 的输入整体排除；已有历史由用户清理。

### 交互模式

SQL 语句以分号结束，可跨多行输入。

| 元命令 | 作用 |
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

```sh
chusql-web [options]
```

| 选项 | 说明 |
| --- | --- |
| `--port N` | 监听端口 |
| `--listen-host H` | 监听地址 |
| `--user NAME` | 管理员账号名 |
| `--config FILE` | 指定配置文件 |
| `-h, --help` | 显示帮助 |

静态资源目录固定为 `static`。请求大小、会话、登录限制和分页等参数在 `settings.toml` 中配置。

### 表格编辑与事务

同一页面和数据库的修改统一暂存，“提交全部”原子保存，“撤销全部”回滚并重新加载。约束错误可修正后重试，写冲突须撤销并重新加载。响应断线时再次提交会确认或重试原批次。

换库或运行 SQL 前先处理待提交修改。表格编辑和 SQL 控制台使用独立事务；关闭页面或退出登录时请求回滚，断线会话按超时清理。

## chusql-server

```sh
chusql-server [options]
```

| 选项 | 说明 |
| --- | --- |
| `--port N` | 数据库服务监听端口 |
| `--listen-host H` | 数据库服务监听地址 |
| `--user NAME` | 管理员账号名 |
| `--max-message N` | 单条请求大小上限 |
| `--max-rows N` | 单次查询返回行数上限 |
| `--config FILE` | 指定配置文件 |
| `-h, --help` | 显示帮助 |

`chusql-server` 是数据目录的唯一直接访问者，Web、CLI 及其他客户端均通过数据库服务访问数据。

## 范式分析

范式分析依据表结构和业务依赖声明给出结构建议，面向有 CONNECT、SELECT 权限的业务表。它是只读分析，迁移由使用者执行。

下面的示例假定 `school.enrollment` 已存在：

```json
{
  "declared_keys": [["student", "course"]],
  "dependencies": [{"lhs": ["student"], "rhs": ["name"]}],
  "dependencies_complete": true,
  "atomic_attributes": true
}
```

将声明保存为 `dependencies.json`，在 `school` 数据库中分析 `enrollment` 表：

```sh
csql -d school --normalize enrollment --declaration dependencies.json --format json
```

Web 表结构页也提供“范式分析”。结果包含候选键、范式判断和一次分解建议，结论取决于声明的完整性与原子性。分析最多 12 列，依赖和键声明各 256 项，运行预算 5 秒。
