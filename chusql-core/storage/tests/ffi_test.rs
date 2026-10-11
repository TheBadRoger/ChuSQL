use std::ffi::CString;
use std::ptr;
use std::slice;

use chusql_core_storage::ffi::*;
use chusql_core_storage::runtime::Storage;

// 验证 FFI 错误输出、长度边界与响应所有权。

/// 读取当前线程的 FFI 错误
fn last_error() -> String {
    let mut len = 0;
    // SAFETY: 长度可写，返回缓冲在复制前没有被修改。
    unsafe {
        let data = chusql_storage_last_error(&mut len);
        assert!(!data.is_null());
        String::from_utf8(slice::from_raw_parts(data, len).to_vec()).unwrap()
    }
}

/// 维护失败清空输出并拒绝超长请求
#[test]
fn maintenance_rejects_invalid_arguments_and_resets_outputs() {
    let input = b"{}";
    let mut out = input.as_ptr() as *mut u8;
    let mut len = 99;
    // SAFETY: 输出均可写；无效输入应在任何读取之前被拒绝。
    unsafe {
        assert_eq!(chusql_storage_maintenance(ptr::null(), ptr::null(), 0, &mut out, &mut len), -1);
        assert!(out.is_null());
        assert_eq!(len, 0);
        len = 99;
        assert_eq!(chusql_storage_maintenance(ptr::null(), input.as_ptr(), input.len(), ptr::null_mut(), &mut len), -1);
        assert_eq!(len, 0);
        out = input.as_ptr() as *mut u8;
        assert_eq!(chusql_storage_maintenance(ptr::null(), input.as_ptr(), input.len(), &mut out, ptr::null_mut()), -1);
        assert!(out.is_null());
        assert_eq!(chusql_storage_maintenance(ptr::null(), input.as_ptr(), usize::MAX, &mut out, &mut len), -1);
    }
    assert!(out.is_null());
    assert_eq!(len, 0);
    assert_eq!(last_error(), "invalid maintenance argument");
}

/// 请求验证长度和编码并归还响应
#[test]
fn request_validates_input_and_returns_owned_response() -> Result<(), Box<dyn std::error::Error>> {
    let temp = tempfile::tempdir()?;
    let config = temp.path().join("settings.toml");
    std::fs::write(&config, format!("[storage]\ndata_dir = '{}'\nlog_files = '{}'\n[log]\nlevel = 'off'\n", temp.path().join("data").display(), temp.path().join("logs").display()))?;
    let storage = Storage::open(config.to_str())?;
    let input = br#"{"method":"ping"}"#;
    let mut out = ptr::null_mut();
    let mut len = 0;
    // SAFETY: 句柄和实际请求有效；超长长度必须在构造切片前被拒绝。
    unsafe {
        assert_eq!(chusql_storage_request(&storage, input.as_ptr(), usize::MAX, &mut out, &mut len), -1);
        assert!(out.is_null());
        assert_eq!(len, 0);
        assert_eq!(last_error(), "invalid argument");
        assert_eq!(chusql_storage_request(&storage, [0xff].as_ptr(), 1, &mut out, &mut len), -1);
        assert!(last_error().starts_with("request is not valid UTF-8:"));
        assert_eq!(chusql_storage_request(&storage, input.as_ptr(), input.len(), &mut out, &mut len), 0);
        assert!(!out.is_null());
        let response = slice::from_raw_parts(out, len).to_vec();
        chusql_storage_free(out, len);
        let response: serde_json::Value = serde_json::from_slice(&response)?;
        assert_eq!(response["status"], "pong");
    }
    drop(storage);
    let config = CString::new(config.to_str().unwrap())?;
    // SAFETY: 路径与输出有效；失败不产生需要释放的缓冲。
    unsafe {
        assert_eq!(chusql_storage_maintenance(config.as_ptr(), [0xff].as_ptr(), 1, &mut out, &mut len), -1);
    }
    assert!(out.is_null());
    assert_eq!(len, 0);
    assert!(!last_error().is_empty());
    let maintenance = br#"{"mode":"reset","user":"root","password_hash":"hash","types":[{"name":"int","base_type":"int","parameterized":false}]}"#;
    // SAFETY: 维护仅使用本测试临时目录；成功响应复制后由配套函数释放。
    unsafe {
        assert_eq!(chusql_storage_maintenance(config.as_ptr(), maintenance.as_ptr(), maintenance.len(), &mut out, &mut len), 0, "{}", last_error());
        assert!(!out.is_null());
        let response = slice::from_raw_parts(out, len).to_vec();
        chusql_storage_free(out, len);
        let response: serde_json::Value = serde_json::from_slice(&response)?;
        assert_eq!(response["status"], "maintenance");
        assert_eq!(response["mode"], "reset");
    }
    Ok(())
}
