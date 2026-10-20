# 命令与选项

[← 返回 README](../README.md)

`csql` 是命令行客户端，`chusql-server` 提供数据库服务，`csql-bootstrap` 负责初始化和离线维护。各组件用 `--config FILE` 指定配置文件，`--help` 查看完整参数。

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
| `--history FILE` | 指定历史记录文件 |
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

`chusql-server` 是数据目录的唯一直接访问者，CLI 及其他客户端均通过数据库服务访问数据。
