// 本地端点：Windows 上是具名管道 `\\.\pipe\<name>`，Unix/macOS 上是文件系统套接字
// `<socket_dir>/<name>.sock`。存储进程、Haskell 前端（见 chusql-engine 的 Storage.IPC）
// 和集成测试都用同一条规则，保证对同一个 `[server] pipe_name` 算出同一个端点。
//
// 用文件系统套接字而不是 Linux 抽象命名空间：抽象命名空间没有文件、跨平台不存在，
// 而带路径的套接字两个方向都能连，Haskell 那边也能用常规 socket 连上。

use interprocess::local_socket::{prelude::*, ListenerOptions};
#[cfg(unix)]
use interprocess::local_socket::GenericFilePath;
#[cfg(windows)]
use interprocess::local_socket::GenericNamespaced;
#[cfg(unix)]
use std::path::{Path, PathBuf};

/// 端点的可读形式：Windows 是 `\\.\pipe\<name>`，Unix 是套接字文件路径。
pub fn display(pipe_name: &str) -> String {
    #[cfg(windows)]
    {
        format!("\\\\.\\pipe\\{}", pipe_name)
    }
    #[cfg(unix)]
    {
        socket_path(pipe_name).display().to_string()
    }
}

/// Unix 端点路径：`$XDG_RUNTIME_DIR` → `$TMPDIR` → `/tmp`，文件名固定 `<pipe_name>.sock`。
#[cfg(unix)]
pub fn socket_path(pipe_name: &str) -> PathBuf {
    let dir = std::env::var_os("XDG_RUNTIME_DIR")
        .filter(|v| !v.is_empty())
        .or_else(|| std::env::var_os("TMPDIR").filter(|v| !v.is_empty()))
        .map(PathBuf::from)
        .unwrap_or_else(|| PathBuf::from("/tmp"));
    dir.join(format!("{}.sock", pipe_name))
}

/// 连到正在监听的存储进程。
pub fn connect(pipe_name: &str) -> std::io::Result<LocalSocketStream> {
    #[cfg(windows)]
    {
        let name = pipe_name
            .to_ns_name::<GenericNamespaced>()
            .map_err(|e| std::io::Error::other(e.to_string()))?;
        LocalSocketStream::connect(name)
    }
    #[cfg(unix)]
    {
        let path = socket_path(pipe_name);
        let name = path
            .to_fs_name::<GenericFilePath>()
            .map_err(|e| std::io::Error::other(e.to_string()))?;
        LocalSocketStream::connect(name)
    }
}

/// 监听端点：Windows 直接建具名管道；Unix 先探活，再清掉上次崩溃留下的残留套接字文件。
pub fn listen(pipe_name: &str) -> std::io::Result<LocalSocketListener> {
    #[cfg(windows)]
    {
        let name = pipe_name
            .to_ns_name::<GenericNamespaced>()
            .map_err(|e| std::io::Error::other(e.to_string()))?;
        ListenerOptions::new().name(name).create_sync()
    }
    #[cfg(unix)]
    {
        let path = socket_path(pipe_name);
        prepare_socket_path(&path)?;
        let name = path
            .to_fs_name::<GenericFilePath>()
            .map_err(|e| std::io::Error::other(e.to_string()))?;
        ListenerOptions::new().name(name).create_sync()
    }
}

/// Unix：先探活的监听者，再清掉残留的套接字文件。
#[cfg(unix)]
fn prepare_socket_path(path: &Path) -> std::io::Result<()> {
    match std::os::unix::net::UnixStream::connect(path) {
        Ok(_) => Err(std::io::Error::new(
            std::io::ErrorKind::AddrInUse,
            format!(
                "another storage server already listens on {}",
                path.display()
            ),
        )),
        Err(_) => {
            if path.exists() {
                std::fs::remove_file(path)?;
            }
            if let Some(parent) = path.parent() {
                std::fs::create_dir_all(parent)?;
            }
            Ok(())
        }
    }
}
