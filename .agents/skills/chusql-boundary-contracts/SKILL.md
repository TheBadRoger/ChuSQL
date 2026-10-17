---
name: chusql-boundary-contracts
description: 修改 ChuSQL Rust/Haskell FFI、客户端与 Server 协议、跨组件序列化契约或公开接口时使用；核对调用链、线程与所有权。单模块内部算法且接口未变时不加载。
---

# ChuSQL 边界契约

## 适用范围

负责组件间表示、调用与生命周期。类型内容使用 chusql-type-system，落盘与恢复使用 chusql-storage-integrity，通用 unsafe 检查使用 rust-c-ffi-safety。路径相对仓库根目录，按变更读相关消费者。

## 模块与调用路径

- CLI / Web 后端通过 `chusql-interface/src/ChuSQL/Interface/Link.hs` 连接 Server TCP；`Protocol.hs` 定义网络消息，`Session.hs` 提供客户端会话操作。
- `chusql-server/src/ChuSQL/Server/TCP.hs` 分派消息，`Session.hs` 管登录 / 鉴权，`Backend.hs` 调引擎。

  浏览器通过 `chusql-web/src/ChuSQL/Web/API.hs` 的 HTTP API 访问 Web 后端，不直接访问 Rust。
- `chusql-core/engine/src/ChuSQL/Core/Engine/Storage/IPC.hs` 将 MonadStorage 操作编码为 JSON，经 localStorageLink 调 FFI；IPC 名称不表示当前另起存储进程或 Socket 服务。
- FFI 两端是 `chusql-core/engine/src/ChuSQL/Core/Engine/Storage/FFI.hs` 与 `chusql-core/storage/src/ffi.rs`，方向 Haskell → Rust，没有回调 ABI。
- 存储 JSON 定义于 `chusql-core/model/src/ChuSQL/Core/Protocol.hs` 与 `chusql-core/storage/src/protocol.rs`；它与 Interface TCP 属于不同协议。

## ABI 与所有权

下表符号均带 chusql_storage_ 前缀；Rust usize 对 Haskell CSize、i32 对 CInt，句柄使用 Ptr ()。不直接跨语言传 Rust struct、enum、Vec 或 Haskell 对象布局。

| 符号 | C ABI 形状 | 契约 |
| --- | --- | --- |
| version | () → const char* | 静态 NUL 字符串，不释放 |
| open | const char* → Storage* | NULL 路径用默认；失败 NULL，成功由 close 归还 |
| close | Storage* → void | NULL 安全；非空句柄只关闭一次 |
| request | const Storage*, const u8*, usize, u8**, usize* → i32 | 输入借用；输出由 Rust 分配；0 / -1 |
| maintenance | const char*, const u8*, usize, u8**, usize* → i32 | 离线操作；响应所有权同 request |
| free | u8*, usize → void | 归还原始指针 / 长度对，不跨分配器、不重复释放 |
| last_error | usize* → const u8* | 借用线程局部错误，立即复制、不释放 |

## 必须保持的不变量

### 表示与指针

- 路径为 NUL 结尾 UTF-8；请求 / 响应为显式长度 UTF-8 JSON，响应不保证 NUL 结尾，不能用 C 字符串函数读取。
- Haskell request 使用 Ptr CChar、CSize、Ptr (Ptr CChar)、Ptr CSize 并返回 CInt，maintenance 以 CString 替代句柄。改签名同步两端与实际动态库。
- 借用指针整个调用中有效，输出可写且不与输入或其他输出重叠。Rust 拒绝 NULL / 超过 isize::MAX 的请求长度，但不能验证非空指针的分配范围 / 来源合法性（provenance）；catch_unwind 不是非法指针保护。

### 所有权与错误

- 成功输出是 Box<[u8]> 转出的所有权；Haskell callStorage 复制后 finally 调 free，失败清空输出。保留取消屏蔽，避免复制 / 释放窗口泄漏。
- open / request / maintenance 捕获 Rust panic，经 NULL / -1 和 last_error 返回；业务错误也可能是 ABI 成功响应中的 JSON error，两层失败都要处理。
- close / free 捕获析构 panic 但没有状态返回，当前忽略捕获结果；不能依靠析构保证提交或宣称它报告落盘失败。

### 线程与句柄

- onStorageThread 在支持 bound threads 的 RTS 中绑定调用和错误读取到同一 OS 线程；last_error 缓冲只到本线程下次设置错误前有效。
- Haskell open / close / request / maintenance 是 safe ccall，version / free / last_error 是 unsafe ccall；长 I/O 不改为 unsafe。请求期间不得关闭句柄。
- IPC 的 withStorageLink 计数在途请求，closeConnection 等空闲再关闭；不绕过它更换 / 释放链路。Env 保存会话工作库，不把全局默认库当多连接状态。

### 消息与消费者

- Interface TCP 是一行 UTF-8 JSON，Hello 校验 protocolVersion=1；同一 Client 的 MVar 串行化请求 / 响应。Server 每连接建 Session 并限制行长度，客户端锁不是全局数据库锁。
- SchemaColumn 字段、method / status 标签、缺省字段、错误码及 QueryResult columns / rows 的位置关系是消费者契约，不用自动派生构造器名替代稳定标签。
- 复合值标签为 __chusql_runtime_v1，由 Core.Runtime 编解码；Rust 行仍是通用 JSON。封装变化检查 CLI / Web / 存储重载，Web 数值受 JavaScript 安全整数限制。
- ENUM 的 __chusql_enum_v1 是存储 / FFI 值编码，保存类型名、标签集合和值；公开查询结果通过 queryResultJson 输出标签字符串。不能直接复用存储编码给客户端，CLI / Web 编辑须复用类型校验及转义。
- 原始存储透传仍经过 Server 会话鉴权；新入口核对 TCP 分派、会话策略与 Web 消费者，不绕过对象权限或业务错误。

## 实现取舍与边界

优先扩展共享协议与真实调用者，不增加无用途适配层；确认无人依赖的内部接口可直接调整。新增类型化值沿 JSON 载荷传递，只有实际需要才扩展 ABI。
Rust panic、Haskell IO 失败与业务错误按边界转成明确失败；当前没有自动 ABI 兼容协商或后台回调生命周期管理。

## 验证入口

- ABI / ownership 变更验证 `chusql-core/storage/tests/ffi_test.rs` 的成功、错误、长度、编码和释放路径；必要时用可用 Miri，工具安装按实际验证需要决定。
- 存储协议变化测试相关 Rust 模块并重建实际 cdylib，再串行验证受影响 Haskell 模块；真实 FFI 可用性须通过实际动态库验证。
- TCP 改动验证 Hello、错误响应和真实消费者，复用 `chusql-server/test/Spec.hs`、`chusql-web/test/Spec.hs`、`chusql-cli/test/Spec.hs` 中相关行为。
- 入口见 `gate-check/run-step.ps1`，测试范围按风险决定，不自动运行所有者全量门禁。

## 相关资料

Web 值展示 / SQL 生成消费者见 `chusql-web/static/core.js`；总体路径见 `docs/architecture.md`。核对协议时读两端编码 / 解码和调用者，不依赖绝对行号。
