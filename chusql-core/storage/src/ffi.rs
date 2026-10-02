use std::cell::RefCell;
use std::ffi::{c_char, CStr};
use std::panic::{catch_unwind, AssertUnwindSafe};
use std::ptr;
use std::slice;

use crate::runtime::Storage;

// C ABI：Haskell 侧唯一存储入口，请求与响应都是 UTF-8 JSON。
// 不 panic 穿过边界，失败走线程局部 last_error。

const OK: i32 = 0;
const ERR: i32 = -1;

thread_local! {
    static LAST_ERROR: RefCell<Vec<u8>> = const { RefCell::new(Vec::new()) };
}

/// 记下本线程最近的失败信息
fn set_error(message: impl AsRef<str>) {
    let text = message.as_ref().as_bytes().to_vec();
    LAST_ERROR.with(|slot| *slot.borrow_mut() = text);
}

/// 版本号字符串（静态常量，调用方不要释放）。
#[unsafe(no_mangle)]
pub extern "C" fn chusql_storage_version() -> *const c_char {
    const VERSION: &str = concat!(env!("CARGO_PKG_VERSION"), "\0");
    VERSION.as_ptr() as *const c_char
}

/// 打开数据目录；失败返回 NULL
///
/// # Safety
/// `config_path` 要么是 NULL，要么指向以 NUL 结尾的有效字符串。
#[unsafe(no_mangle)]
pub unsafe extern "C" fn chusql_storage_open(config_path: *const c_char) -> *mut Storage {
    let opened = catch_unwind(AssertUnwindSafe(|| {
        let path = if config_path.is_null() {
            None
        } else {
            let text = unsafe { CStr::from_ptr(config_path) }
                .to_str()
                .map_err(|_| "config path is not valid UTF-8".to_string())?;
            if text.is_empty() { None } else { Some(text.to_string()) }
        };
        Storage::open(path.as_deref()).map(Box::new)
    }));
    match opened {
        Ok(Ok(storage)) => Box::into_raw(storage),
        Ok(Err(e)) => {
            set_error(e);
            ptr::null_mut()
        }
        Err(_) => {
            set_error("panic in chusql_storage_open");
            ptr::null_mut()
        }
    }
}

/// 关闭句柄（NULL 安全）
///
/// # Safety
/// `handle` 必须是 chusql_storage_open 返回且未被关闭过的句柄。
#[unsafe(no_mangle)]
pub unsafe extern "C" fn chusql_storage_close(handle: *mut Storage) {
    if handle.is_null() {
        return;
    }
    let _ = catch_unwind(AssertUnwindSafe(|| unsafe { drop(Box::from_raw(handle)) }));
}

/// 处理一条请求，响应缓冲须归还
///
/// # Safety
/// `handle` 必须是有效句柄；`request` 指向 `request_len` 个字节；`out`/`out_len` 可写。
#[unsafe(no_mangle)]
pub unsafe extern "C" fn chusql_storage_request(
    handle: *const Storage,
    request: *const u8,
    request_len: usize,
    out: *mut *mut u8,
    out_len: *mut usize,
) -> i32 {
    if !out.is_null() {
        unsafe { *out = ptr::null_mut() };
    }
    if !out_len.is_null() {
        unsafe { *out_len = 0 };
    }
    if handle.is_null() || request.is_null() || out.is_null() || out_len.is_null() {
        set_error("invalid argument");
        return ERR;
    }

    let storage = unsafe { &*handle };
    let bytes = unsafe { slice::from_raw_parts(request, request_len) };
    let text = match std::str::from_utf8(bytes) {
        Ok(text) => text,
        Err(e) => {
            set_error(format!("request is not valid UTF-8: {}", e));
            return ERR;
        }
    };

    let response = match catch_unwind(AssertUnwindSafe(|| storage.request_line(text))) {
        Ok(response) => response,
        Err(_) => {
            set_error("panic in chusql_storage_request");
            return ERR;
        }
    };

    let mut buffer = response.into_bytes().into_boxed_slice();
    let ptr = buffer.as_mut_ptr();
    let len = buffer.len();
    std::mem::forget(buffer);
    unsafe {
        *out = ptr;
        *out_len = len;
    }
    OK
}

/// 归还响应缓冲
///
/// # Safety
/// `data`/`len` 必须是 chusql_storage_request 原样返回且未被释放过的一对值。
#[unsafe(no_mangle)]
pub unsafe extern "C" fn chusql_storage_free(data: *mut u8, len: usize) {
    if data.is_null() {
        return;
    }
    let _ = catch_unwind(AssertUnwindSafe(|| unsafe {
        let slice = ptr::slice_from_raw_parts_mut(data, len);
        let _owned: Box<[u8]> = Box::from_raw(slice);
    }));
}

/// 取本线程最近的失败信息
///
/// # Safety
/// `out_len` 可写，或为 NULL。
#[unsafe(no_mangle)]
pub unsafe extern "C" fn chusql_storage_last_error(out_len: *mut usize) -> *const u8 {
    LAST_ERROR.with(|slot| {
        let bytes = slot.borrow();
        if !out_len.is_null() {
            unsafe { *out_len = bytes.len() };
        }
        if bytes.is_empty() { ptr::null() } else { bytes.as_ptr() }
    })
}
