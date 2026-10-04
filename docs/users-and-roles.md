# 用户、角色与 root 属性

ChuSQL 的用户和角色共用一套身份目录及名称空间。用户是允许登录的角色；`CREATE USER` 与 `CREATE ROLE` 的主要区别是默认登录能力。同名用户和角色不能同时存在。

## root 的含义

安装阶段默认创建名为 `root` 的管理员身份，并赋予以下属性：

| 属性 | 初始值 | 含义 |
| --- | --- | --- |
| `can_login` | `true` | 可以使用账号及口令登录 |
| `is_superuser` | `true` | 具有系统管理及数据访问的最高权限 |
| `system_catalog_manager` | `false` | 独立目录管理属性；SUPERUSER 本身已具有有效目录管理能力 |
| `enabled` | `true` | 身份处于启用状态 |

管理员名称可通过安装选项修改。`root` 不是凭名字获得特殊权限：授权判断依据身份属性，其他身份也可被授予 `SUPERUSER`。修改启动参数中的管理员名称不会提升已有普通身份。

口令由安装脚本通过标准输入交给引导程序，经哈希后保存，不写入配置文件。重复引导保留已有口令和属性；已有引导身份被降权或禁用时明确拒绝继续，不自动恢复最高权限。

系统保护最后一个同时满足 `enabled=true`、`can_login=true`、`is_superuser=true` 的身份：不能删除、禁用、取消登录能力或取消最高权限。若要替换 root，应先创建另一个可登录且启用的管理员。

## 默认属性与修改方法

| 创建方式 | 登录能力 | 最高权限 | 启用状态 |
| --- | --- | --- | --- |
| 安装引导管理员（默认 root） | LOGIN | SUPERUSER | ENABLED |
| `CREATE USER` | LOGIN | NOSUPERUSER | ENABLED |
| `CREATE ROLE` | NOLOGIN | NOSUPERUSER | ENABLED |

身份创建与修改语法见 [SQL 语法](syntax.md#账号与权限)。

`ALTER USER` 与 `ALTER ROLE` 的属性、口令修改语法通用。可使用 `LOGIN / NOLOGIN`、`SUPERUSER / NOSUPERUSER`、`ENABLED / DISABLED`、`SYSTEM_CATALOG_MANAGER / NOSYSTEM_CATALOG_MANAGER`；同一条语句不能重复指定同一属性。

SUPERUSER 可设置目录管理属性。该属性默认为 false，不经成员关系继承；只有自身启用且允许登录的身份能使用它。目录管理者可以进入 system 库、查看身份目录、创建普通身份、修改普通身份属性与口令及删除普通用户身份。它不能管理 SUPERUSER 或其他目录管理者，不能授予这两个特权属性，不能执行 `DROP ROLE`，也不会因此获得创建业务数据库、直接修改系统表或读取业务数据的权限。受限身份变更在存储写锁内再次检查目标权限。

## 全部身份及凭据字段

身份元数据保存于隐藏的 `system.__system_identities`；凭据单独保存于 `system.__system_users`。以下字段解释实际持久化内容，不表示都能通过 SQL 修改或查询。

| 身份字段 | 含义 | 管理方式 |
| --- | --- | --- |
| `id` | 身份的稳定编号；删除后不复用，重建同名身份获得新编号 | 系统分配 |
| `user` | 用户或角色名称，共用名称空间，不区分大小写 | 创建时指定；当前不支持改名 |
| `can_login` | 是否允许直接登录；仅允许登录仍不足以保证登录成功，还需启用及正确口令 | `LOGIN / NOLOGIN` |
| `is_superuser` | 是否具有最高权限；不通过角色成员关系继承 | `SUPERUSER / NOSUPERUSER` |
| `system_catalog_manager` | 是否具有受限的系统目录管理能力；不继承、不替代数据访问授权 | `SYSTEM_CATALOG_MANAGER / NOSYSTEM_CATALOG_MANAGER`，仅 SUPERUSER 可设置 |
| `enabled` | 是否启用身份；禁用后不能登录，也不再贡献角色继承权限 | `ENABLED / DISABLED` |
| `identity_version` | 身份目录格式与旧数据迁移标记；旧记录可为 0，当前统一身份格式为 1 | 系统维护；不是会话版本 |
| `registered_at` | 身份创建时间；旧记录缺失时由迁移补齐 | 系统记录 |
| `last_login_at` | 最近一次成功登录时间；尚未登录时为空 | 成功登录时更新 |
| `revision` | 会话校验版本；修改口令或身份属性后递增，使旧登录凭据失效 | 系统维护；不是登录次数 |

| 凭据字段 | 含义 |
| --- | --- |
| `id` | 对应身份目录中的编号 |
| `password_hash` | 口令哈希，不是明文口令；通过口令修改命令更新 |

内部账号协议可将身份字段和 `password_hash` 合并为账号对象，但哈希不属于公开的角色展示内容。`SHOW ROLES` 展示全部身份的编号、名称、登录能力、最高权限、启用状态、目录管理属性和创建时间，不返回口令哈希，也不展示全部内部字段。系统表由身份管理接口维护，不应直接修改。

## 属性、授权与会话的关系

普通用户需要直接授权或通过角色继承获得数据权限。示例见 [SQL 语法](syntax.md#账号与权限)。

`NOLOGIN` 只禁止该身份直接登录，角色仍可贡献继承权限；`DISABLED` 会使该角色停止贡献继承权限。`SUPERUSER` 始终由登录身份自身的属性决定，加入一个最高权限角色不会继承最高权限。

修改身份属性、口令或相关权限会使受影响的既有会话失效；TCP 连接在下一次请求时收到 `unauthorized` 并关闭。删除身份会清理它的授权及双向成员关系；重新创建同名身份不会恢复原有授权。

当前尚未提供 PostgreSQL 的全部角色选项，例如独立的 `CREATEDB`、`CREATEROLE`、连接数限制或口令到期时间。应以本文列出的属性及当前支持的授权命令为准。
