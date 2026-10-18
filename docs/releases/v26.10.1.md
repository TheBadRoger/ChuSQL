# ChuSQL v26.10.1

ChuSQL 是一个以 SQL 为主要接口的关系数据库，由 Haskell 查询引擎和 Rust 存储层组成。服务端管理数据与用户会话，用户可通过命令行客户端或浏览器中的 Web 界面访问。

这是 ChuSQL 的第一个正式发布版，提供建表、数据增删改查、连接、聚合、子查询和 CTE，以及索引、查询优化、事务、保存点、WAL 崩溃恢复和用户／角色授权。

本版还提供运行时类型与函数基础：支持 String、List、Maybe、Tuple 等类型，以及 length、not 函数；复合值可通过 SQL 构造、查询和持久化，CLI 与 Web 均可显示。

发行包支持 Windows x86_64 和 Linux x86_64，包含数据库服务端、初始化工具、CLI、Web 界面和安装／卸载脚本。下载下方压缩包，使用包内 install.ps1（Windows）或 install.sh（Linux）安装，可选择 CLI 或 Web 组件，无需自行安装 Haskell 或 Rust 工具链。每个包附 SHA256 校验文件。

安装与操作说明：[安装指南](https://github.com/TheBadRoger/ChuSQL/blob/v26.10.1/docs/install.md)、[SQL 语法](https://github.com/TheBadRoger/ChuSQL/blob/v26.10.1/docs/syntax.md)、[运行时类型](https://github.com/TheBadRoger/ChuSQL/blob/v26.10.1/docs/runtime-types.md)。

首版适合学习、开发和小规模内部应用试点。事务采用整库内存快照，同库写入串行；DECIMAL 使用浮点表示；尚无运行时类型的完整比较／索引能力。关键业务上线前应完成真实负载测试及备份恢复演练。升级已有数据前请备份，并避免用旧二进制打开新增类型的数据。

本版已通过所有者全量门禁；发布流程要求 Linux 测试与两个平台的发行包安装验证全部成功后上传正式下载包。
