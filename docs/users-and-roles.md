# 用户与角色

[← 返回 README](../README.md)

本页帮助管理员理解身份属性、配置本机 sudo 登录并授予对象权限。SQL 命令写法见 [SQL 语法](syntax.md#账号与权限)。

用户与角色共用身份目录，名称须唯一。用户是可以登录的角色；`CREATE USER` 与 `CREATE ROLE` 的默认登录能力不同。

本页内容：

- [root 的含义](#root-的含义)
- [创建与修改身份](#创建与修改身份)
- [身份与凭据字段](#身份与凭据字段)
- [本机 sudo 认证](#本机-sudo-认证)
- [对象授权](#对象授权)
- [属性、授权与会话的关系](#属性授权与会话的关系)
- [升级兼容](#升级兼容)

## root 的含义

安装阶段默认创建名为 `root` 的管理员身份：

| 属性 | 初始值 | 含义 |
| --- | --- | --- |
| `can_login` | `true` | 可以用账号和口令登录 |
| `is_superuser` | `true` | 具有系统管理与数据访问的最高权限 |
| `allow_sudo_auth` | `false` | 允许经本机提升身份验证后免口令登录，业务权限另行授予 |
| `system_catalog_manager` | `false` | 受限的系统目录管理属性；最高权限本身已经包含有效的目录管理能力 |
| `enabled` | `true` | 身份处于启用状态 |

管理员名称可在安装时指定。之后的登录能力与权限通过 SQL 属性管理，判断依据是身份属性，其他身份同样可以被授予最高权限。

安装脚本通过标准输入把口令交给引导程序，由引导程序哈希保存。重复引导保留已有口令和属性；引导身份须保持启用和最高权限，否则报错。

系统保护最后一个同时满足 `enabled`、`can_login`、`is_superuser` 的身份，删除、禁用、取消登录能力或取消最高权限都会被拒绝。替换 root 前先创建一个可登录且启用的管理员。

## 创建与修改身份

| 创建方式 | 登录能力 | 最高权限 | 启用状态 |
| --- | --- | --- | --- |
| 安装引导管理员（默认 root） | LOGIN | SUPERUSER | ENABLED |
| `CREATE USER` | LOGIN | NOSUPERUSER | ENABLED |
| `CREATE ROLE` | NOLOGIN | NOSUPERUSER | ENABLED |

`ALTER USER` 与 `ALTER ROLE` 语法相同，每条语句为每项属性指定一次：

| 属性 | 开启 / 关闭 |
| --- | --- |
| 登录能力 | `LOGIN / NOLOGIN` |
| 最高权限 | `SUPERUSER / NOSUPERUSER` |
| 启用状态 | `ENABLED / DISABLED` |
| 目录管理 | `SYSTEM_CATALOG_MANAGER / NOSYSTEM_CATALOG_MANAGER` |
| 本机 sudo 认证 | `ALLOW_SUDO_AUTH / NOALLOW_SUDO_AUTH` |

目录管理属性默认关闭，不经角色继承，只有自身启用且允许登录的身份能使用。目录管理者可以进入 system 库、查看身份目录、创建普通身份、修改普通身份的属性与口令、删除普通用户身份。它的权限止于普通身份：不能管理最高权限身份、其他目录管理者或启用了 sudo 认证的身份，也不能授予这三类特权；不能执行 `DROP ROLE`；不能创建业务数据库、直接修改系统表或读取业务数据。受限身份变更会在存储写锁内再次检查目标权限。

## 身份与凭据字段

本节供维护系统目录和协议时查阅。身份保存在隐藏的 `system.__system_identities`，凭据保存在 `system.__system_users`，字段表示实际持久化的内容。

| 身份字段 | 含义 |
| --- | --- |
| `id` | 身份的稳定编号，删除后不复用；重建同名身份获得新编号 |
| `user` | 用户或角色名称，共用名称空间，不区分大小写；当前不支持改名 |
| `can_login` | 允许直接登录；登录成功还需要身份启用和正确口令 |
| `is_superuser` | 具有最高权限，由身份自身属性决定 |
| `system_catalog_manager` | 具有受限的目录管理能力，由身份自身属性决定 |
| `allow_sudo_auth` | 接受受信任的本机 sudo 认证，默认关闭 |
| `enabled` | 身份启用；禁用后不能登录，也不再贡献角色继承权限 |
| `identity_version` | 身份目录格式与迁移标记；旧记录可为 0，当前格式为 1 |
| `registered_at` | 创建时间，旧记录缺失时由迁移补齐 |
| `last_login_at` | 最近一次成功登录时间，尚未登录时为空 |
| `revision` | 会话校验版本，修改口令或属性后递增，使旧登录凭据失效 |

凭据字段为 `id`（对应身份编号）和 `password_hash`（口令哈希，通过改口令命令更新）。`SHOW ROLES` 展示编号、名称、登录能力、最高权限、启用状态、目录管理、sudo 开关和创建时间；口令哈希等内部字段保持隐藏。

## 本机 sudo 认证

本机 sudo 认证让受信任的系统管理员免数据库口令登录指定身份，默认关闭。Unix 有效 UID 0（通常经 sudo 获得）映射到 `[server] sudo_auth_user`；Windows 提升令牌中的 Administrators 成员映射到同一身份。客户端上报的操作系统用户名、`SUDO_USER`、数据库用户名和角色属性都不参与操作系统身份判断。

先创建目标身份，再用最高权限身份开启认证：

```sql
ALTER USER local_operator ALLOW_SUDO_AUTH;
```

目标身份须为 `LOGIN` 和 `ENABLED`，其余权限按需授予；普通身份的 sudo 登录只拥有该身份已有的权限。`SUPERUSER` 与 `SYSTEM_CATALOG_MANAGER` 不会隐含允许 sudo 登录。

在服务端与 CLI 使用的配置中写入映射：

```toml
[server]
sudo_auth_user = "local_operator"
```

以 Unix root 或 Windows 提升后的管理员重启 `chusql-server`，再以相同身份运行 CLI：

```sh
sudo csql --config /absolute/path/settings.toml --sudo -e "SELECT 1"
```

Windows 在提升的终端中运行 `csql --config C:\absolute\path\settings.toml --sudo -e "SELECT 1"`。`--sudo` 不读取数据库口令，省略 `--user` 时使用配置中的映射；显式指定其他账号也绕不过服务端映射。普通口令登录与 Web 登录保持原方式，远程连接无法请求该入口，CLI 会校验连接对端是回环地址。

服务启动时生成独立随机凭据。Unix 放在 `/var/run/chusql-sudo-<实际端口>/credential`，目录 root 所有、权限 0700，文件权限 0600；Windows 放在系统目录的 `config\ChuSQL-Sudo-<实际端口>\credential`，使用只允许 Administrators 与 SYSTEM 的 DACL。读取入口检查当前进程的提升身份，Unix 还校验所有权、权限和类型并拒绝链接，Windows 拒绝重解析路径。

凭据不经过网络传输。CLI 用 HMAC-SHA256 回应一次性挑战，挑战绑定连接、账号和服务实例，60 秒后过期；每次尝试都会消费挑战，失败按账号限流。服务端在 stderr 记录启用与认证结果，不记录凭据。

关闭 `ALLOW_SUDO_AUTH`、`DISABLED`、`NOLOGIN`、修改口令或删除身份都会让既有连接在下一次请求时失效；清空映射并重启服务后入口关闭。正常停止服务会清理凭据。异常退出可能留下端口专属目录，再次启用会报错，确认对应服务已停止后由提升身份清掉该目录再启动；凭据只放在管理员可访问的位置。

## 对象授权

业务授权绑定数据库编号与表编号，身份与所有者绑定身份编号。删除后重建同名数据库、表或身份会得到新编号，旧授权不会随名称恢复；对象是否存在以 catalog 为准，不同数据库的同名表互不共享授权。

| 对象范围 | 权限 | 默认与所有者能力 |
| --- | --- | --- |
| DATABASE 库名 | CONNECT、CREATE | PUBLIC 默认有 CONNECT；CREATE 允许建表；所有者可管理该库 |
| TABLE 库名.表名 | SELECT、INSERT、UPDATE、DELETE | 默认无数据权限；所有者拥有四项数据权限、结构管理和转授权能力 |
| 库名.* | 四项表级数据权限 | 覆盖该库现有和未来的表，不含数据库 CREATE |

`ALL` 按对象范围展开，未限定表名与 `*` 绑定当前数据库。其他身份所建表的数据权限需要另行授予；启用的所有者角色通过成员关系贡献所有权，最高权限属性不继承。创建数据库和管理 DOMAIN 仍需最高权限；REVOKE 后所有权保留，所有权转移命令待实现。

```sql
REVOKE CONNECT ON DATABASE sales FROM PUBLIC;
GRANT CONNECT, CREATE ON DATABASE sales TO analyst;
GRANT SELECT ON TABLE sales.orders TO reader WITH GRANT OPTION;
GRANT SELECT ON sales.* TO analyst;
REVOKE SELECT ON sales.orders FROM reader;
```

PUBLIC 是所有身份共享的虚拟角色，新建同名用户或角色会被拒绝；已有同名旧身份按真实编号解析，删除该身份后才使用虚拟 PUBLIC。撤销 PUBLIC CONNECT 会关闭数据库的默认连接权限，显式 CONNECT 或数据库所有权仍然有效，重新授予即恢复默认连接。

转授权（grant option）让身份把持有的权限授予其他身份，每次授予记录授予者。普通身份须持有有效的 grant option 或对象所有权才能转授；所有者和最高权限身份可以撤销对象上的其他来源。撤销、删除身份或成员变化会清理失去合法来源的转授权链，其他独立来源保留，循环转授不能维持失去根来源的权限。

权限快照保存在受保护的系统目录并经 WAL 原子写入，普通存储请求无法访问。SQL、TCP 和 Web 使用同一服务端检查，TCP 原始存储入口只允许最高权限身份；查询与授权变更通过共享读写锁排序，建表前持久化创建者声明，供重启后恢复所有权。事务提交时重新检查权限和稳定对象编号，权限撤销或对象替换后旧事务不能提交。

## 属性、授权与会话的关系

普通用户需要直接授权或通过角色继承获得数据权限，示例见 [SQL 语法](syntax.md#账号与权限)。

`NOLOGIN` 只禁止身份直接登录，角色仍可贡献继承权限；`DISABLED` 会让角色停止贡献。最高权限始终由登录身份自身的属性决定，加入最高权限角色不会继承它。

修改身份属性、口令或相关权限会让受影响的既有会话失效，TCP 连接在下一次请求时收到 `unauthorized` 并关闭。删除身份会清理它的授权和双向成员关系，重建同名身份不会恢复原授权。

当前提供本文列出的属性与授权命令；`CREATEDB`、`CREATEROLE`、连接数限制和口令到期时间留待以后实现。

## 升级兼容

旧名称授权在首次启动时一次性绑定现存的对象和身份编号。旧记录没有授予者，迁移为管理来源；旧全局 `*` 只转换为当时现存表的明确授权，不扩展到未来对象。迁移后不再读取旧名称授权作为鉴权依据。

旧账号与角色迁移保留编号、口令、时间和授权关系；同名冲突会拒绝迁移，身份不合并。迁移后的身份目录和 DOMAIN 编码不能被旧二进制安全读取，已升级的数据目录请勿用旧版本打开。
