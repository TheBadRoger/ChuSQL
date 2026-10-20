# 用户与角色

[← 返回 README](../README.md)

用户和角色共享名称空间。用户默认可登录，角色默认用于汇集权限；SQL 示例见[账号与权限](syntax.md#账号与权限)。

## root 的含义

安装默认创建管理员 `root`，权限来自 SUPERUSER 属性，名称可在安装时指定。替换管理员前，先创建另一个启用且可登录的最高权限身份；系统会保护最后一个管理员。

## 创建与修改身份

`ALTER USER` 与 `ALTER ROLE` 使用相同的属性语法：

| 属性 | 用途 |
| --- | --- |
| `LOGIN / NOLOGIN` | 允许或关闭直接登录 |
| `SUPERUSER / NOSUPERUSER` | 开启或关闭最高权限 |
| `ENABLED / DISABLED` | 启用或禁用身份 |
| `SYSTEM_CATALOG_MANAGER / NOSYSTEM_CATALOG_MANAGER` | 管理普通身份的系统目录权限 |

新用户和角色默认启用、使用 NOSUPERUSER；角色默认 NOLOGIN。目录管理者只管理普通身份，业务数据仍需另行授权。

## 对象授权

| 范围 | 权限 |
| --- | --- |
| 数据库 | CONNECT、CREATE |
| 表 | SELECT、INSERT、UPDATE、DELETE |
| `库名.*` | 库内现有及未来表的数据权限 |

PUBLIC 默认提供业务库 CONNECT。表创建者拥有表所有权；数据库所有权不自动授予他人表的数据权限。

角色成员继承业务权限，SUPERUSER 等特权属性不继承。`WITH GRANT OPTION` 允许转授权；撤销时会清理失去来源的下游授权，独立授权来源保留。


## 属性、授权与会话的关系

NOLOGIN 关闭直接登录，DISABLED 同时停止角色贡献继承权限。身份属性、口令或相关授权变化后，既有连接在下一次请求时返回 `unauthorized` 并关闭。

## 查看身份与权限

身份目录保存编号、属性和时间，凭据目录保存口令哈希。日常管理使用 SQL 身份命令；`SHOW ROLES` 查看身份属性，`SHOW GRANTS` 查看自己的有效权限。

## 升级兼容

授权绑定稳定编号，重建同名身份或对象不会恢复旧授权。升级前备份完整数据目录；版本回退使用旧程序和升级前备份。
