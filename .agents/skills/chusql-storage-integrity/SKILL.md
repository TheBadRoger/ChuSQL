---
name: chusql-storage-integrity
description: 修改 ChuSQL 存储页、索引、序列化、catalog、系统元数据、初始化、持久化事务或恢复时使用；维护实际落盘顺序与恢复约束。普通 SQL 表达式或界面任务不加载。
---

# ChuSQL 存储完整性

## 适用范围

负责持久化状态及其与服务端事务的关系。类型语义使用 chusql-type-system，接口生命周期使用 chusql-boundary-contracts；不照搬 PostgreSQL 内核设计。路径相对仓库根目录，只读受影响模块。

## 模块与调用路径

- `chusql-core/storage/src/runtime.rs`：Storage 持有数据目录文件锁、管理数据库；Server 管每库 catalog、WAL、表缓存和读写门。
- 数据根目录中 system/ 保存系统库，databases/<name>/ 保存业务库。

  每库有 catalog.json、mvcc.json、<table>.db、内建 <table>.idx、其他 <table>.<column>.idx、wal.log 与 wal.checkpoint；非空 file_generation 使用 <table>.g<代次> 前缀；页文件另有 .pages、.page-wal 和可能残留的 .pages.pending，备份复制整个目录；诊断日志由 log_files 单独配置。
- `chusql-core/storage/src/page.rs` 管定长页与缓存刷盘；`heap.rs` 管页内槽位、JSON 行、隐藏列和 compact；`btree.rs` 管变长字节键、重复键与叶子链范围扫描。
- `chusql-core/storage/src/catalog.rs` 管 schema、索引、统计、隐藏列、对象编号 / 高水位；`protocol.rs` 定义线上 schema 和系统表常量。
- `chusql-core/storage/src/mvcc.rs` 管持久化行版本、稳定行编号、快照引用、行 / 唯一键锁、等待图与回收。
- `chusql-core/storage/src/wal.rs` 管 LSN、提交组、日志扫描与检查点；runtime.rs 的 group_via_wal、recover_wal、replay_op 连接记录与应用。
- `chusql-server/src/ChuSQL/Server/Session.hs` 管会话快照、保存点、差分与冲突；`Backend.hs` 向存储提交操作。

  `chusql-bootstrap/` 调用 Storage::maintenance 与 maintenance_candidate 做离线维护。

## 必须保持的不变量

### 目录与提交顺序

- 表存在性以 catalog 为准，文件存在不表示已登记；不能靠打开文件创建逻辑表。语义与优化只读取 schema。
- Catalog::save 写 pending 文件、同步后替换；单文件替换不等于 catalog、页与索引已整体原子提交。
- 普通组先 append_group 落盘，再设置 recovery_required，然后应用、刷表、原子发布行版本目录、推进检查点、截断日志，全部成功才解除恢复状态。
- 应用 / 刷盘 / 检查点 / 截断失败必须保持恢复状态并报错。普通业务请求被阻断；Ping / SystemStatus 例外由各自分派层处理，新增入口不得绕过状态。
- 恢复仅重放检查点之后已提交组，保持 replay_op 幂等；不应用未提交组。现有 WAL 是 redo，检查点是库级 LSN，不是 undo 或页级恢复。
- 账号引导 / 变更已走普通带提交标记的组；旧账号 WAL 仍须兼容恢复，不删除既有解码和迁移规则。
- 全页镜像先同步，再覆盖页文件；数据同步后发布页 LSN 元数据，最后清理镜像。镜像用独立写序号区分同一提交内的重复页写，错误后当前页句柄不得继续刷新。

### 身份与生命周期

- 库 / 表对象和身份编号依赖持久化高水位，删除后不复用；迁移不能降低计数器，不能从现存对象推断已删除编号上限。
- 系统身份 / 凭据、角色、授权、类型与 DOMAIN 元数据保持对应关系。引导保留已有口令和降权状态，拒绝定义 / 保留文件冲突；类型目录不是可动态扩展基础类型的机制。
- 数据目录锁覆盖存储和维护生命周期；维护前停止占用进程。同库行修改可并行准备，行锁等待不占提交门；最终 WAL 发布、页刷盘和版本发布仍串行，表句柄加锁。

### 事务与格式

- SQL 使用持久化行版本快照，工作行集仍整库物化，无嵌套事务，DDL / USE 须在事务结束后执行。提交按内部行编号合并，整数 id 可差分物理写入，否则整表替换；对象身份、权限和最终约束需复查。行锁等待图中止环的新等待者，5 秒总等待超时；保存点回滚不释放已有锁。错误后调用方回滚，不能只重发 COMMIT。
- 页内 compact 保留槽位编号与索引位置；隐藏列 compact 可能整表重写。布局、键编码、行序和隐藏列改动须检查真实消费者。
- 页大小、B+Tree magic / 布局、槽位小端字段、JSON 行、WAL 标签 / 帧和 catalog 字段是格式契约。改变前明确加载、迁移或拒绝策略，不以重新初始化掩盖不兼容。

## 实现取舍与边界

- 优化须保留验证、刷盘顺序、错误传播与可观察行序。结构变更同时核对 WAL、应用、幂等恢复与 catalog 保存。
- 数据库目录建删不走普通表 WAL，删库使用墓碑目录；不能承诺表提交组相同的崩溃原子性。
- 离线维护在候选目录操作并保留备份；repair 不改既有表数据，recover 依赖可用 WAL，reset 重建系统目录且保留业务库。失败保留原目录 / 恢复材料。
- 当前 Catalog::load 对不存在或空文件返回空目录，JSON 损坏报错；不能宣称所有损坏都会拒绝。文件发布路径改名与删除须同时维护页侧文件并传播错误；部分析构 / 清理路径仍可能忽略 I/O 错误，不作为成功持久化保证。
- 在线 compact 复制影子堆 / 索引，最终库写门内追平，经 Migrate WAL 切换 file_generation；影子文件先同步，catalog 再发布。快照结束后回收退役文件，启动先恢复再清理孤立代次。对象编号、schema 版本和逻辑行序不变；同名重建不能复用仍被快照保留的旧文件。
- 行版本以提交 LSN 为可见序号，内容变化和行序变化分开；版本超前于可恢复 WAL 时拒绝打开。旧版本保留到最老活跃快照，不复用行编号。尚无通用结构迁移事务，不据此放松校验。

## 验证入口

- 按影响选择 `chusql-core/storage/tests/` 的 page / heap / btree / catalog / wal / runtime 测试；说明修正核对源码即可。
- 格式 / 元数据 / WAL 改动验证重开、提交前后中断、重复恢复、缺失 / 截断 / 损坏输入与失败后状态；断言须检测实际不一致。
- 会话差分 / 冲突变更还验证 `chusql-server/test/Spec.hs`；并发压力测试不是正确性证明。
- 规定入口为 `gate-check/run-step.ps1 -Kind cargo`，动态库构建为 -Kind build；受影响 Haskell 模块按脚本命令串行构建，全量门禁遵守 AGENTS.md。

## 相关资料

恢复操作按需读 `docs/bootstrap.md`，配置见 `docs/config.md`，限制见 `.agents/context/known-issues.txt`；涉及拟议能力时仍以源码为准。
