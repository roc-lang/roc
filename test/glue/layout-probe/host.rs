#![no_std]
#![allow(improper_ctypes)]
#![allow(improper_ctypes_definitions)]

#[path = "roc_platform_abi.rs"]
mod abi;

use core::ffi::c_void;
use core::fmt::{self, Write};

unsafe extern "C" {
    fn malloc(size: usize) -> *mut c_void;
    fn free(ptr: *mut c_void);
    fn write(fd: i32, buf: *const u8, count: usize) -> isize;
    fn exit(status: i32) -> !;
}

struct ReportWriter<'a> {
    buf: &'a mut [u8],
    len: usize,
}

impl<'a> ReportWriter<'a> {
    fn new(buf: &'a mut [u8]) -> Self {
        Self { buf, len: 0 }
    }
}

impl Write for ReportWriter<'_> {
    fn write_str(&mut self, value: &str) -> fmt::Result {
        let remaining = self.buf.len().saturating_sub(self.len);
        let write_len = remaining.min(value.len());
        self.buf[self.len..self.len + write_len].copy_from_slice(&value.as_bytes()[..write_len]);
        self.len += write_len;
        Ok(())
    }
}

fn write_stderr(bytes: &[u8]) {
    unsafe {
        let _ = write(2, bytes.as_ptr(), bytes.len());
        let _ = write(2, b"\n".as_ptr(), 1);
    }
}

fn write_stderr_fmt(args: fmt::Arguments<'_>) {
    let mut buf = [0; 512];
    let len = {
        let mut writer = ReportWriter::new(&mut buf);
        let _ = writer.write_fmt(args);
        writer.len
    };
    write_stderr(&buf[..len]);
}

fn exit_failure() -> ! {
    unsafe { exit(1) }
}

#[panic_handler]
fn panic(_: &core::panic::PanicInfo<'_>) -> ! {
    exit_failure()
}

#[no_mangle]
pub extern "C" fn rust_eh_personality() {}

static mut ALLOC_COUNT: usize = 0;
static mut DEALLOC_COUNT: usize = 0;
static mut FAILURE_COUNT: usize = 0;
static mut REPORT: [u8; 512] = [0; 512];
static mut REPORT_LEN: usize = 0;

fn align_forward(value: usize, alignment: usize) -> usize {
    (value + alignment - 1) & !(alignment - 1)
}

fn fail(message: &str) {
    unsafe {
        if FAILURE_COUNT == 0 {
            let prefix = b"FAIL layout-probe RustGlue: ";
            let report = &mut *core::ptr::addr_of_mut!(REPORT);
            let mut written = 0;
            for byte in prefix {
                report[written] = *byte;
                written += 1;
            }
            for byte in message.as_bytes() {
                if written >= report.len() {
                    break;
                }
                report[written] = *byte;
                written += 1;
            }
            REPORT_LEN = written;
        }
        FAILURE_COUNT += 1;
    }
}

#[no_mangle]
pub extern "C" fn roc_alloc(length: usize, alignment: usize) -> *mut c_void {
    let total = length + alignment - 1 + core::mem::size_of::<usize>();
    let raw = unsafe { malloc(total.max(1)) as *mut u8 };
    if raw.is_null() {
        fail("malloc failed");
        return core::ptr::null_mut();
    }
    let aligned = align_forward(unsafe { raw.add(core::mem::size_of::<usize>()) } as usize, alignment);
    unsafe {
        *((aligned - core::mem::size_of::<usize>()) as *mut usize) = raw as usize;
        ALLOC_COUNT += 1;
    }
    aligned as *mut c_void
}

#[no_mangle]
pub extern "C" fn roc_dealloc(ptr: *mut c_void, _alignment: usize) {
    if ptr.is_null() {
        return;
    }
    unsafe {
        let raw = *((ptr as usize - core::mem::size_of::<usize>()) as *const usize) as *mut c_void;
        free(raw);
        DEALLOC_COUNT += 1;
    }
}

#[no_mangle]
pub extern "C" fn roc_realloc(ptr: *mut c_void, new_length: usize, alignment: usize) -> *mut c_void {
    if !ptr.is_null() {
        roc_dealloc(ptr, alignment);
    }
    roc_alloc(new_length, alignment)
}

#[no_mangle]
pub extern "C" fn roc_dbg(bytes: *const u8, len: usize) {
    let slice = unsafe { core::slice::from_raw_parts(bytes, len) };
    write_stderr(slice);
}

#[no_mangle]
pub extern "C" fn roc_expect_failed(_bytes: *const u8, _len: usize) {
    fail("roc_expect_failed");
}

#[no_mangle]
pub extern "C" fn roc_crashed(_bytes: *const u8, _len: usize) {
    fail("roc_crashed");
    exit_failure();
}

#[no_mangle]
pub extern "C" fn roc_probe_roundtrip(arg0: abi::ProbeLayoutProbe) -> abi::ProbeLayoutProbe {
    arg0
}

#[no_mangle]
pub extern "C" fn main(_argc: i32, _argv: *const *const u8) -> i32 {
    unsafe { abi::roc_main(); }
    let failure_count = unsafe { FAILURE_COUNT };
    if failure_count != 0 {
        let report_len = unsafe { REPORT_LEN };
        let message = if report_len == 0 {
            b"FAIL layout-probe RustGlue: unknown failure".as_slice()
        } else {
            unsafe { &(&*core::ptr::addr_of!(REPORT))[..report_len] }
        };
        write_stderr(message);
        return 1;
    }
    let alloc_count = unsafe { ALLOC_COUNT };
    let dealloc_count = unsafe { DEALLOC_COUNT };
    write_stderr_fmt(format_args!(
        "PASS glue-runtime layout-probe RustGlue native alloc={} dealloc={}",
        alloc_count, dealloc_count
    ));
    0
}
