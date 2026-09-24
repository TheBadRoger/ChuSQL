use std::fmt;
use std::io::{IsTerminal, Write};
use std::sync::atomic::{AtomicU8, Ordering};
use std::sync::OnceLock;

// 极简日志：等级用原子变量，关掉的等级只花一次原子读。

// 颜色
/// 转义序列收尾
const RESET: &str = "\u{1b}[0m";
/// 加粗红（ERROR）
const BOLD_RED: &str = "\u{1b}[1m\u{1b}[31m";
/// 加粗橙（WARN）
const BOLD_ORANGE: &str = "\u{1b}[1m\u{1b}[38;5;208m";
/// 只加粗（DEBUG）
const BOLD: &str = "\u{1b}[1m";

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
// 等级与来源
/// 日志等级
pub enum Level {
    Off,
    Error,
    Warn,
    Info,
    Debug,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
/// 来源：服务自身或管道请求
pub enum Channel {
    Core,
    Pipe,
}

impl Channel {
    /// 来源标签
    fn tag(self) -> &'static str {
        match self {
            Channel::Core => "core",
            Channel::Pipe => "pipe",
        }
    }
}

impl Level {
    /// 解析等级名
    pub fn parse(s: &str) -> Option<Level> {
        match s.trim().to_ascii_lowercase().as_str() {
            "off" => Some(Level::Off),
            "error" => Some(Level::Error),
            "warn" | "warning" => Some(Level::Warn),
            "info" => Some(Level::Info),
            "debug" => Some(Level::Debug),
            _ => None,
        }
    }

    /// 等级名
    pub fn name(self) -> &'static str {
        match self {
            Level::Off => "off",
            Level::Error => "error",
            Level::Warn => "warn",
            Level::Info => "info",
            Level::Debug => "debug",
        }
    }

    /// 行里的等级标签
    fn tag(self) -> &'static str {
        match self {
            Level::Error => "ERROR",
            Level::Warn => "WARN ",
            Level::Info => "INFO ",
            _ => "DEBUG",
        }
    }

    /// 等级的颜色
    fn color(self) -> &'static str {
        match self {
            Level::Error => BOLD_RED,
            Level::Warn => BOLD_ORANGE,
            Level::Debug => BOLD,
            _ => "",
        }
    }
}

static LEVEL: AtomicU8 = AtomicU8::new(Level::Info as u8);

static COLOR: OnceLock<bool> = OnceLock::new();

// 写入
/// 设置全局等级
pub fn set_level(level: Level) {
    LEVEL.store(level as u8, Ordering::Relaxed);
}

#[inline]
/// 这个等级要不要输出
pub fn enabled(level: Level) -> bool {
    level as u8 <= LEVEL.load(Ordering::Relaxed)
}

/// 启动时调一次
pub fn init() {
    enable_vt();
}

/// 写出一行
pub fn write(channel: Channel, level: Level, args: fmt::Arguments) {
    let msg = args.to_string();
    let line = render(level, channel, color_enabled(), &now(), &msg);
    let stderr = std::io::stderr();
    let mut out = stderr.lock();
    let _ = writeln!(out, "{}", line);
}

/// 是否着色（终端或强制）
fn color_enabled() -> bool {
    *COLOR.get_or_init(|| match std::env::var("CHUSQL_LOG_COLOR").as_deref().map(str::trim) {
        Ok("always") | Ok("yes") | Ok("1") => true,
        Ok("never") | Ok("no") | Ok("0") => false,
        _ => std::io::stderr().is_terminal(),
    })
}

/// 拼一行（纯函数，可测）
fn render(level: Level, channel: Channel, color: bool, stamp: &Stamp, msg: &str) -> String {
    let tag = level.tag();
    let paint = level.color();
    let level_text = if color && !paint.is_empty() {
        format!("{}{}{}", paint, tag, RESET)
    } else {
        tag.to_string()
    };
    format!("[{} {}] [{}] {}", level_text, stamp, channel.tag(), msg)
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
// 时间
/// 本地日期时间
pub struct Stamp {
    pub year: u16,
    pub month: u16,
    pub day: u16,
    pub hour: u16,
    pub minute: u16,
    pub second: u16,
    pub milli: u16,
}

/// 格式：年-月-日 时:分:秒.毫秒
impl fmt::Display for Stamp {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "{:04}-{:02}-{:02} {:02}:{:02}:{:02}.{:03}",
            self.year, self.month, self.day, self.hour, self.minute, self.second, self.milli
        )
    }
}

/// 当前本地时间（Windows）
#[cfg(windows)]
fn now() -> Stamp {
    #[repr(C)]
    struct SystemTime {
        year: u16,
        month: u16,
        day_of_week: u16,
        day: u16,
        hour: u16,
        minute: u16,
        second: u16,
        milliseconds: u16,
    }

    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn GetLocalTime(out: *mut SystemTime);
    }

    let mut t = SystemTime {
        year: 0,
        month: 0,
        day_of_week: 0,
        day: 0,
        hour: 0,
        minute: 0,
        second: 0,
        milliseconds: 0,
    };
    unsafe { GetLocalTime(&mut t) };
    Stamp {
        year: t.year,
        month: t.month,
        day: t.day,
        hour: t.hour,
        minute: t.minute,
        second: t.second,
        milli: t.milliseconds,
    }
}

/// 当前时间（其他平台用 UTC）
#[cfg(not(windows))]
fn now() -> Stamp {
    let d = std::time::SystemTime::now()
        .duration_since(std::time::UNIX_EPOCH)
        .unwrap_or_default();
    let secs = d.as_secs();
    let (year, month, day) = civil_from_days((secs / 86_400) as i64);
    Stamp {
        year: year as u16,
        month: month as u16,
        day: day as u16,
        hour: ((secs % 86_400) / 3600) as u16,
        minute: ((secs % 3_600) / 60) as u16,
        second: (secs % 60) as u16,
        milli: d.subsec_millis() as u16,
    }
}

/// 天数换算成日期
pub fn civil_from_days(days: i64) -> (i64, u16, u16) {
    let z = days + 719_468;
    let era = if z >= 0 { z } else { z - 146_096 } / 146_097;
    let doe = z - era * 146_097;
    let yoe = (doe - doe / 1_460 + doe / 36_524 - doe / 146_096) / 365;
    let y = yoe + era * 400;
    let doy = doe - (365 * yoe + yoe / 4 - yoe / 100);
    let mp = (5 * doy + 2) / 153;
    let day = (doy - (153 * mp + 2) / 5 + 1) as u16;
    let month = if mp < 10 { mp + 3 } else { mp - 9 } as u16;
    (y + i64::from(month <= 2), month, day)
}

// 终端
/// 打开 Windows 的 VT 处理
#[cfg(windows)]
fn enable_vt() {
    #[link(name = "kernel32")]
    unsafe extern "system" {
        fn GetStdHandle(which: u32) -> *mut core::ffi::c_void;
        fn GetConsoleMode(handle: *mut core::ffi::c_void, mode: *mut u32) -> i32;
        fn SetConsoleMode(handle: *mut core::ffi::c_void, mode: u32) -> i32;
    }

    const STD_ERROR_HANDLE: u32 = 0xFFFF_FFF4;
    const ENABLE_VIRTUAL_TERMINAL_PROCESSING: u32 = 0x0004;

    unsafe {
        let handle = GetStdHandle(STD_ERROR_HANDLE);
        let mut mode = 0u32;
        if GetConsoleMode(handle, &mut mode) != 0 {
            let _ = SetConsoleMode(handle, mode | ENABLE_VIRTUAL_TERMINAL_PROCESSING);
        }
    }
}

/// 其他平台什么都不用做
#[cfg(not(windows))]
fn enable_vt() {}

// 宏
/// debug 日志
#[macro_export]
macro_rules! log_debug {
    (core, $($arg:tt)*) => {
        $crate::log_line!($crate::log::Channel::Core, $crate::log::Level::Debug, $($arg)*)
    };
    (pipe, $($arg:tt)*) => {
        $crate::log_line!($crate::log::Channel::Pipe, $crate::log::Level::Debug, $($arg)*)
    };
}

/// info 日志
#[macro_export]
macro_rules! log_info {
    (core, $($arg:tt)*) => {
        $crate::log_line!($crate::log::Channel::Core, $crate::log::Level::Info, $($arg)*)
    };
    (pipe, $($arg:tt)*) => {
        $crate::log_line!($crate::log::Channel::Pipe, $crate::log::Level::Info, $($arg)*)
    };
}

/// warn 日志
#[macro_export]
macro_rules! log_warn {
    (core, $($arg:tt)*) => {
        $crate::log_line!($crate::log::Channel::Core, $crate::log::Level::Warn, $($arg)*)
    };
    (pipe, $($arg:tt)*) => {
        $crate::log_line!($crate::log::Channel::Pipe, $crate::log::Level::Warn, $($arg)*)
    };
}

/// error 日志
#[macro_export]
macro_rules! log_error {
    (core, $($arg:tt)*) => {
        $crate::log_line!($crate::log::Channel::Core, $crate::log::Level::Error, $($arg)*)
    };
    (pipe, $($arg:tt)*) => {
        $crate::log_line!($crate::log::Channel::Pipe, $crate::log::Level::Error, $($arg)*)
    };
}

/// 四个宏的公共部分
#[doc(hidden)]
#[macro_export]
macro_rules! log_line {
    ($chan:expr, $lvl:expr, $($arg:tt)*) => {
        if $crate::log::enabled($lvl) {
            $crate::log::write($chan, $lvl, format_args!($($arg)*))
        }
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn civil_from_days_known_dates() {
        assert_eq!(civil_from_days(0), (1970, 1, 1));
        assert_eq!(civil_from_days(10_957), (2000, 1, 1));
        assert_eq!(civil_from_days(11_016), (2000, 2, 29));
        assert_eq!(civil_from_days(19_723), (2024, 1, 1));
        assert_eq!(civil_from_days(19_782), (2024, 2, 29));
    }

    #[test]
    fn render_has_time_channel_and_colors() {
        let stamp = Stamp {
            year: 2026,
            month: 9,
            day: 24,
            hour: 13,
            minute: 45,
            second: 12,
            milli: 130,
        };

        assert_eq!(
            render(Level::Error, Channel::Pipe, true, &stamp, "boom"),
            "[\u{1b}[1m\u{1b}[31mERROR\u{1b}[0m 2026-09-24 13:45:12.130] [pipe] boom"
        );
        assert_eq!(
            render(Level::Warn, Channel::Core, true, &stamp, "careful"),
            "[\u{1b}[1m\u{1b}[38;5;208mWARN \u{1b}[0m 2026-09-24 13:45:12.130] [core] careful"
        );
        assert_eq!(
            render(Level::Debug, Channel::Pipe, true, &stamp, "detail"),
            "[\u{1b}[1mDEBUG\u{1b}[0m 2026-09-24 13:45:12.130] [pipe] detail"
        );
        assert_eq!(
            render(Level::Info, Channel::Core, true, &stamp, "hello"),
            "[INFO  2026-09-24 13:45:12.130] [core] hello"
        );
    }

    #[test]
    fn render_without_color_is_plain() {
        let stamp = Stamp {
            year: 2026,
            month: 9,
            day: 24,
            hour: 1,
            minute: 2,
            second: 3,
            milli: 4,
        };
        assert_eq!(
            render(Level::Error, Channel::Core, false, &stamp, "boom"),
            "[ERROR 2026-09-24 01:02:03.004] [core] boom"
        );
    }
}
