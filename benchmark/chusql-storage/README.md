# chusql-storage 基准测试（预留）

这个目录预留给 **Rust 存储层** 的基准测试代码，目前 `chusql-storage/` 还没有开工，
所以这里先占位。

## 计划放什么

`chusql-storage` 做的是磁盘那一层的事情，性能指标和查询层完全不同 ——
查询层关心"少算几行"，存储层关心"少读几次盘"。所以基准要分开测：

| 待测模块 | 关心的指标 |
| -------- | ---------- |
| 页管理 | 单页读写延迟、顺序读 vs 随机读吞吐 |
| B+ 树 | 插入/查找/范围扫描的耗时随数据量增长的趋势 |
| 缓冲池 | 命中率、缺页次数、替换策略（LRU 等）的对比 |
| WAL | 追加写吞吐、日志落盘延迟、恢复耗时 |
| 崩溃恢复 | 从日志重放的时间、恢复后数据一致性 |

## 用什么

Rust 侧的基准测试一般用 [criterion](https://github.com/bheisler/criterion.rs)：

```
benchmark/chusql-storage/
├── Cargo.toml
└── benches/
    ├── page.rs
    ├── btree.rs
    ├── buffer_pool.rs
    └── wal.rs
```

跑法：`cargo bench`

另外还要和 Haskell 层对齐口径：查询层的基准是"整条查询的耗时"，
存储层的基准是"单次 IO / 单次查找的耗时"，两边不要混在一起比。
