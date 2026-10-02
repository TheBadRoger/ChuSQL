-- ChuSQL 系统库 schema。
--
-- 系统库（system）不存在就自动建，里面的表都是系统表；这份脚本是它们唯一的定义，
-- 存储进程启动时按它声明表结构（见 chusql-core/storage/src/initsql.rs）。
-- 这里只描述"应该长什么样"，不包含任何历史数据搬迁。

CREATE TABLE __chusql_users (
    id int NOT NULL AUTO_INCREMENT PRIMARY KEY,
    user varchar(64) NOT NULL UNIQUE,
    password_hash varchar(256) NOT NULL,
    registered_at timestamp NOT NULL,
    last_login_at timestamp,
    revision int NOT NULL
);

CREATE INDEX __chusql_users (user);
