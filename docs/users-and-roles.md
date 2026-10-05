# 用户、角色与 root 属性

ChuSQL 的用户和角色共用一套身份目录及名称空间。用户是允许登录的角色；`CREATE USER` 与 `CREATE ROLE` 的主要区别是默认登录能力。同名用户和角色不能同时存在。

## root 的含义

安装阶段默认创建名为 `root` 的管理员身份，并赋予以下属性：

| 属性 | 初始值 | 含义 |
| --- | --- | --- |
| `can_login` | `true` | 可以使用账号及口令登录 |
| `is_superuser` | `true` | 具有系统管理及数据访问的最高权限 |
| `allow_sudo_auth` | `false` | 允许经本机提升身份验证后免数据库口令登录；不赋予权限 |
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

`ALTER USER` 与 `ALTER ROLE` 的属性、口令修改语法通用。可使用 `LOGIN / NOLOGIN`、`SUPERUSER / NOSUPERUSER`、`ENABLED / DISABLED`、`SYSTEM_CATALOG_MANAGER / NOSYSTEM_CATALOG_MANAGER`、`ALLOW_SUDO_AUTH / NOALLOW_SUDO_AUTH`；同一条语句不能重复指定同一属性。

SUPERUSER 可设置目录管理属性。该属性默认为 false，不经成员关系继承；只有自身启用且允许登录的身份能使用它。目录管理者可以进入 system 库、查看身份目录、创建普通身份、修改普通身份属性与口令及删除普通用户身份。它不能管理 SUPERUSER、其他目录管理者或启用 sudo 认证的身份，不能授予这三个特权属性，不能执行 `DROP ROLE`，也不会因此获得创建业务数据库、直接修改系统表或读取业务数据的权限。受限身份变更在存储写锁内再次检查目标权限。

## 全部身份及凭据字段

身份元数据保存于隐藏的 `system.__system_identities`；凭据单独保存于 `system.__system_users`。以下字段解释实际持久化内容，不表示都能通过 SQL 修改或查询。

| 身份字段 | 含义 | 管理方式 |
| --- | --- | --- |
| `id` | 身份的稳定编号；删除后不复用，重建同名身份获得新编号 | 系统分配 |
| `user` | 用户或角色名称，共用名称空间，不区分大小写 | 创建时指定；当前不支持改名 |
| `can_login` | 是否允许直接登录；仅允许登录仍不足以保证登录成功，还需启用及正确口令 | `LOGIN / NOLOGIN` |
| `is_superuser` | 是否具有最高权限；不通过角色成员关系继承 | `SUPERUSER / NOSUPERUSER` |
| `system_catalog_manager` | 是否具有受限的系统目录管理能力；不继承、不替代数据访问授权 | `SYSTEM_CATALOG_MANAGER / NOSYSTEM_CATALOG_MANAGER`，仅 SUPERUSER 可设置 |
| `allow_sudo_auth` | 是否接受受信任本机提升身份认证；默认 false，不继承，不改变业务权限 | `ALLOW_SUDO_AUTH / NOALLOW_SUDO_AUTH`，仅 SUPERUSER 可设置 |
| `enabled` | 是否启用身份；禁用后不能登录，也不再贡献角色继承权限 | `ENABLED / DISABLED` |
| `identity_version` | 身份目录格式与旧数据迁移标记；旧记录可为 0，当前统一身份格式为 1 | 系统维护；不是会话版本 |
| `registered_at` | 身份创建时间；旧记录缺失时由迁移补齐 | 系统记录 |
| `last_login_at` | 最近一次成功登录时间；尚未登录时为空 | 成功登录时更新 |
| `revision` | 会话校验版本；修改口令或身份属性后递增，使旧登录凭据失效 | 系统维护；不是登录次数 |

| 凭据字段 | 含义 |
| --- | --- |
| `id` | 对应身份目录中的编号 |
| `password_hash` | 口令哈希，不是明文口令；通过口令修改命令更新 |

内部账号协议可将身份字段和 `password_hash` 合并为账号对象，但哈希不属于公开的角色展示内容。`SHOW ROLES` 展示全部身份的编号、名称、登录能力、最高权限、启用状态、目录管理属性、sudo 认证开关和创建时间，不返回口令哈希，也不展示全部内部字段。系统表由身份管理接口维护，不应直接修改。

## 本机 sudo 认证

此入口适用于 Linux、macOS 和 Windows，默认关闭。Unix 有效 UID 0（通常通过 sudo 获得）映射到 `[server] sudo_auth_user`；Windows 提升令牌中的 Administrators 成员映射到同一配置身份。不会使用客户端上报的操作系统用户名、`SUDO_USER`、数据库用户名或角色属性作为操作系统身份凭据。

先用数据库 SUPERUSER 配置目标身份，例如：

```sql
ALTER USER local_operator ALLOW_SUDO_AUTH;
```

目标身份仍需 `LOGIN` 与 `ENABLED`；按需要另行授予业务权限。即使它是普通身份，sudo 登录也只获得该身份已有的权限。`SUPERUSER` 和 `SYSTEM_CATALOG_MANAGER` 不会隐含允许 sudo 登录。

在服务端与 CLI 使用的配置中显式设置映射：

```toml
[server]
sudo_auth_user = "local_operator"
```

停止原服务后，以 Unix root 或 Windows 提升后的管理员重启 `chusql-server`，使用受管理员保护的配置文件、数据目录和程序路径。以相同提升身份运行 CLI；Unix 示例：

```sh
sudo csql --config /absolute/path/settings.toml --sudo -e "SELECT 1"
```

Windows 在提升的终端中运行 `csql --config C:\absolute\path\settings.toml --sudo -e "SELECT 1"`。`--sudo` 不读取数据库口令，省略 `--user` 时使用配置映射。显式指定其他账号也不能绕过服务端映射。普通 TCP/CLI 口令登录与 Web 登录保持原有口令认证；远程连接无法请求此入口，CLI 也校验实际连接对端为回环地址。

服务启动时产生独立随机凭据。Unix 放在 `/var/run/chusql-sudo-<实际端口>/credential`，目录 root 所有且为 0700，文件为 0600；Windows 放在系统目录的 `config\ChuSQL-Sudo-<实际端口>\credential`，使用不继承的 Administrators / SYSTEM 专属 DACL。读取入口检查当前进程提升身份；Unix 还验证文件所有权、权限和类型，拒绝链接，Windows 拒绝重解析路径。凭据不会传输到网络，CLI 使用 HMAC-SHA256 回应绑定连接、账号和服务实例的一次性随机挑战；挑战 60 秒后过期，每次认证尝试都会消费挑战，失败按账号限流。

服务端在 stderr 记录启用映射和认证结果，不记录凭据或证明。成功登录更新身份目录的 `last_login_at`。`ALTER USER local_operator NOALLOW_SUDO_AUTH`、`DISABLED`、`NOLOGIN`、修改口令或删除身份都会使既有连接在下一次请求时失效。映射配置清空并重启服务后，本机入口关闭。

正常停止服务时清理凭据。异常退出可能留下端口专属目录；再次启用会明确报错，不读取或覆盖旧凭据。确认对应服务已停止后，由提升身份清理该目录再启动；不要把凭据复制到普通账号可访问的目录。

## 对象授权

业务授权绑定数据库编号与表编号，身份与所有者绑定身份编号。删除后重建同名数据库、表或身份会获得新编号，旧授权不会随名称恢复。对象存在性以 catalog 为准；不同数据库的同名表互不共享授权。

| 对象范围 | 权限 | 默认与所有者能力 |
| --- | --- | --- |
| DATABASE 库名 | CONNECT、CREATE | PUBLIC 默认 CONNECT；CREATE 允许建表；数据库所有者可管理该数据库 |
| TABLE 库名.表名 | SELECT、INSERT、UPDATE、DELETE | 默认无数据权限；表所有者拥有四项数据权限、结构管理及转授权能力 |
| 库名.* | 四项表级数据权限 | 覆盖该数据库内现有及未来的表；不包含数据库 CREATE |

`ALL` 按对象范围展开。未限定表名与 `*` 绑定当前数据库。数据库所有权不自动赋予其他身份所建表的数据权限；启用的所有者角色可以通过成员关系贡献所有权，最高权限属性仍不继承。创建数据库及管理 DOMAIN 仍要求 SUPERUSER；所有权不能通过 REVOKE 移除，当前没有所有权转移命令。

```sql
REVOKE CONNECT ON DATABASE sales FROM PUBLIC;
GRANT CONNECT, CREATE ON DATABASE sales TO analyst;
GRANT SELECT ON TABLE sales.orders TO reader WITH GRANT OPTION;
GRANT SELECT ON sales.* TO analyst;
REVOKE SELECT ON sales.orders FROM reader;
```

PUBLIC 是所有身份共享的虚拟角色，不能新建同名用户或角色。已有同名旧身份仍按真实身份编号解析，删除该身份后才使用虚拟 PUBLIC。撤销 PUBLIC CONNECT 会关闭数据库的默认连接权限，其他显式 CONNECT 或数据库所有权仍有效；重新授予会恢复默认连接权限。

每次授予记录授予者。普通身份须持有有效 grant option 或对象所有权，可撤销自己的授予；所有者和 SUPERUSER 可撤销对象上的其他来源。撤销、删除身份或成员变更会清理失去合法来源的转授权链；其他独立来源保留，循环转授不能维持失去根来源的权限。禁用身份暂停权限贡献，保留所有权及管理员直接授权；失去启用授予者的下游转授权会被清理。

旧名称授权在首次启动时一次性绑定现存对象和身份编号。旧记录不含授予者，迁移为管理来源；旧全局 `*` 仅转换成当时现存表的明确授权，不扩展到未来对象。已经不存在的对象和身份记录被清理。迁移后不再读取旧名称授权作为鉴权依据。

权限快照位于受保护系统目录，经 WAL 原子保存；普通存储请求不能访问该目录。SQL、TCP 和 Web 使用同一服务端检查；TCP 原始存储入口仅允许 SUPERUSER。查询与授权变更通过共享读写锁排序，建表前持久化创建者声明供重启恢复所有权。事务提交重新检查权限和稳定对象编号，权限撤销或对象替换后不能提交旧事务。

## 属性、授权与会话的关系

普通用户需要直接授权或通过角色继承获得数据权限。示例见 [SQL 语法](syntax.md#账号与权限)。

`NOLOGIN` 只禁止该身份直接登录，角色仍可贡献继承权限；`DISABLED` 会使该角色停止贡献继承权限。`SUPERUSER` 始终由登录身份自身的属性决定，加入一个最高权限角色不会继承最高权限。

修改身份属性、口令或相关权限会使受影响的既有会话失效；TCP 连接在下一次请求时收到 `unauthorized` 并关闭。删除身份会清理它的授权及双向成员关系；重新创建同名身份不会恢复原有授权。

当前尚未提供 PostgreSQL 的全部角色选项，例如独立的 `CREATEDB`、`CREATEROLE`、连接数限制或口令到期时间。应以本文列出的属性及当前支持的授权命令为准。
