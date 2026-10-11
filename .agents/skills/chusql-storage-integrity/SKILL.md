---
name: chusql-storage-integrity
description: 修改 ChuSQL 存储页、索引、序列化、catalog、系统元数据、初始化、持久化事务或恢复时使用；维护实际落盘顺序与恢复约束。普通 SQL 表达式或界面任务不加载。
---

# ChuSQL 存储完整性

## Scope

负责持久化状态及其与服务端事务的关系。类型语义使用 chusql-type-system，接口生命周期使用 chusql-boundary-contracts；不照搬 PostgreSQL 内核设计。路径相对仓库根目录，只读受影响模块。

## Architecture

- `chusql-core/storage/src/runtime.rs`：Storage 持有数据目录文件锁、管理数据库；Server 管每库 catalog、WAL、表缓存和读写门。
- 数据根目录中 system/ 保存系统库，databases/<name>/ 保存业务库。每库有 catalog.json、<table>.db、内建 <table>.idx、其他 <table>.<column>.idx、wal.log 与 wal.checkpoint；诊断日志由 log_files 单独配置。
- `chusql-core/storage/src/page.rs` 管定长页与缓存刷盘；`heap.rs` 管页内槽位、JSON 行、隐藏列和 compact；`btree.rs` 管变长字节键、重复键与叶子链范围扫描。
- `chusql-core/storage/src/catalog.rs` 管 schema、索引、统计、隐藏列、对象编号 / 高水位；`protocol.rs` 定义线上 schema 和系统表常量。
- `chusql-core/storage/src/wal.rs` 管 LSN、提交组、日志扫描与检查点；runtime.rs 的 group_via_wal、recover_wal、replay_op 连接记录与应用。
- `chusql-server/src/ChuSQL/Server/Session.hs` 管会话快照、保存点、差分与冲突；`Backend.hs` 向存储提交操作。`chusql-bootstrap/` 调用 Storage::maintenance 与 maintenance_candidate 做离线维护。

## Invariants

- 表存在性以 catalog 为准，文件存在不表示已登记；不能靠打开文件创建逻辑表。语义与优化只读取 schema。
- Catalog::save 写 pending 文件、同步后替换；单文件替换不等于 catalog、页与索引已整体原子提交。
- 普通组先 append_group 落盘，再设置 recovery_required，然后应用、刷表、推进检查点、截断日志，全部成功才解除恢复状态。
- 应用 / 刷盘 / 检查点 / 截断失败必须保持恢复状态并报错。普通业务请求被阻断；Ping / SystemStatus 例外由各自分派层处理，新增入口不得绕过状态。
- 恢复仅重放检查点之后已提交组，保持 replay_op 幂等；不应用未提交组。现有 WAL 是 redo，检查点是库级 LSN，不是 undo 或页级恢复。
- 账号引导 / 变更存在独立 WAL 路径，不能假定所有写入都走普通组函数。
- 库 / 表对象和身份编号依赖持久化高水位，删除后不复用；迁移不能降低计数器，不能从现存对象推断已删除编号上限。
- 系统身份 / 凭据、角色、授权、类型与 DOMAIN 元数据保持对应关系。引导保留已有口令和降权状态，拒绝定义 / 保留文件冲突；类型目录不是可动态扩展基础类型的机制。
- 数据目录锁覆盖存储和维护生命周期；维护前停止占用进程。每库读共享 / 写独占、表句柄加锁，不假定同库跨表并行写。
- 会话事务是整库内存快照，无嵌套事务，事务内禁止 DDL / USE。提交跳过未修改表，整数 id 按键差分，否则整表比较 / 替换；对象身份和权限需复查，冲突返回 serialization_failure 并保留事务。
- 页内 compact 保留槽位编号与索引位置；隐藏列 compact 可能整表重写。布局、键编码、行序和隐藏列改动须检查真实消费者。
- 页大小、B+Tree magic / 布局、槽位小端字段、JSON 行、WAL 标签 / 帧和 catalog 字段是格式契约。改变前明确加载、迁移或拒绝策略，不以重新初始化掩盖不兼容。

## Engineering Decisions

- 优化可减少分配或重复读取，但保留验证、刷盘顺序、错误传播与可观察行序。结构变更同时核对 WAL、应用、幂等恢复与 catalog 保存。
- 数据库目录建删不走普通表 WAL，删库使用墓碑目录；不能承诺表提交组相同的崩溃原子性。
- 离线维护在候选目录操作并保留备份；repair 不改既有表数据，recover 依赖可用 WAL，reset 重建系统目录且保留业务库。失败保留原目录 / 恢复材料。
- 当前 Catalog::load 对不存在或空文件返回空目录，JSON 损坏报错；不能宣称所有损坏都会拒绝。rename_table_files 和部分析构 / 清理路径仍忽略 I/O 错误，不作为成功持久化保证。
- 尚无行版本 MVCC、页级恢复、跨页填洞、并行写或通用结构迁移事务，不据此放松校验。

## Validation

- 按影响选择 `chusql-core/storage/tests/` 的 page / heap / btree / catalog / wal / runtime 测试；说明修正核对源码即可。
- 格式 / 元数据 / WAL 改动验证重开、提交前后中断、重复恢复、缺失 / 截断 / 损坏输入与失败后状态；断言须检测实际不一致。
- 会话差分 / 冲突变更还验证 `chusql-server/test/Spec.hs`；并发压力测试不是正确性证明。
- 规定入口为 `gate-check/run-step.ps1 -Kind cargo`，动态库构建为 -Kind build；受影响 Haskell 模块按脚本命令串行构建，全量门禁遵守 AGENTS.md。

## References

恢复操作按需读 `docs/bootstrap.md`，配置见 `docs/config.md`，限制见 `.agents/context/known-issues.txt`；涉及拟议能力时仍以源码为准。
