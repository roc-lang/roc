#![allow(improper_ctypes)]
#![allow(improper_ctypes_definitions)]

#[path = "roc_platform_abi.rs"]
mod abi;

use core::ffi::c_void;

const WASM_PAGE_SIZE: usize = 65_536;

static mut REPORT: [u8; 512] = [0; 512];
static mut REPORT_LEN: usize = 0;
static mut FAILURE_COUNT: usize = 0;
static mut ALLOC_COUNT: usize = 0;
static mut DEALLOC_COUNT: usize = 0;
static mut HEAP_CURSOR: usize = 0;

fn align_forward(value: usize, alignment: usize) -> usize {
    (value + alignment - 1) & !(alignment - 1)
}

fn set_report(prefix: &[u8], message: &[u8]) {
    unsafe {
        if FAILURE_COUNT != 0 {
            return;
        }
        let report = &mut *core::ptr::addr_of_mut!(REPORT);
        let mut written = 0;
        for byte in prefix {
            report[written] = *byte;
            written += 1;
        }
        for byte in message {
            if written >= report.len() {
                break;
            }
            report[written] = *byte;
            written += 1;
        }
        REPORT_LEN = written;
    }
}

fn fail(message: &str) {
    set_report(b"FAIL duplicate-tags RustGlue wasm32: ", message.as_bytes());
    unsafe {
        FAILURE_COUNT += 1;
    }
}

fn finish_pass() {
    let message = b"PASS glue-runtime duplicate-tags RustGlue wasm32";
    unsafe {
        (&mut *core::ptr::addr_of_mut!(REPORT))[..message.len()].copy_from_slice(message);
        REPORT_LEN = message.len();
    }
}

#[no_mangle]
pub extern "C" fn roc_alloc(length: usize, alignment: usize) -> *mut c_void {
    unsafe {
        if HEAP_CURSOR == 0 {
            HEAP_CURSOR = core::arch::wasm32::memory_size(0) * WASM_PAGE_SIZE;
        }
        let ptr = align_forward(HEAP_CURSOR, alignment);
        let Some(end) = ptr.checked_add(length) else {
            fail("allocation overflow");
            return core::ptr::null_mut();
        };
        let required_pages = (end + WASM_PAGE_SIZE - 1) / WASM_PAGE_SIZE;
        let current_pages = core::arch::wasm32::memory_size(0);
        if required_pages > current_pages && core::arch::wasm32::memory_grow(0, required_pages - current_pages) == usize::MAX {
            fail("memory grow failed");
            return core::ptr::null_mut();
        }
        HEAP_CURSOR = end;
        ALLOC_COUNT += 1;
        ptr as *mut c_void
    }
}

#[no_mangle]
pub extern "C" fn roc_dealloc(_ptr: *mut c_void, _alignment: usize) {
    unsafe {
        DEALLOC_COUNT += 1;
    }
}

#[no_mangle]
pub extern "C" fn roc_realloc(_ptr: *mut c_void, new_length: usize, alignment: usize) -> *mut c_void {
    roc_alloc(new_length, alignment)
}

#[no_mangle]
pub extern "C" fn roc_dbg(_bytes: *const u8, _len: usize) {}

#[no_mangle]
pub extern "C" fn roc_expect_failed(_bytes: *const u8, _len: usize) {
    fail("roc_expect_failed");
}

#[no_mangle]
pub extern "C" fn roc_crashed(_bytes: *const u8, _len: usize) {
    fail("roc_crashed");
}

macro_rules! ok_result {
    ($result:ty, $tag:ty) => {{
        let mut result: $result = unsafe { core::mem::zeroed() };
        result.tag = <$tag>::Ok;
        result
    }};
}

macro_rules! hosted_ok {
    ($name:ident, $result:ty, $tag:ty) => {
        #[no_mangle]
        pub extern "C" fn $name(arg0: abi::RocStr) -> $result {
            let _ = arg0;
            ok_result!($result, $tag)
        }
    };
}

hosted_ok!(roc_a_unit, abi::AUnitResult, abi::AUnitResultTag);
hosted_ok!(roc_a_str, abi::AStrResult, abi::AStrResultTag);
hosted_ok!(roc_a_bytes, abi::ABytesResult, abi::ABytesResultTag);
hosted_ok!(roc_a_record, abi::ARecordResult, abi::ARecordResultTag);
hosted_ok!(roc_a_nested, abi::ANestedResult, abi::ANestedResultTag);

hosted_ok!(roc_b_unit, abi::BUnitResult, abi::BUnitResultTag);
hosted_ok!(roc_b_str, abi::BStrResult, abi::BStrResultTag);
hosted_ok!(roc_b_bytes, abi::BBytesResult, abi::BBytesResultTag);
hosted_ok!(roc_b_record, abi::BRecordResult, abi::BRecordResultTag);
hosted_ok!(roc_b_nested, abi::BNestedResult, abi::BNestedResultTag);

hosted_ok!(roc_c_unit, abi::CUnitResult, abi::CUnitResultTag);
hosted_ok!(roc_c_str, abi::CStrResult, abi::CStrResultTag);
hosted_ok!(roc_c_bytes, abi::CBytesResult, abi::CBytesResultTag);
hosted_ok!(roc_c_record, abi::CRecordResult, abi::CRecordResultTag);
hosted_ok!(roc_c_nested, abi::CNestedResult, abi::CNestedResultTag);

hosted_ok!(roc_d_unit, abi::DUnitResult, abi::DUnitResultTag);
hosted_ok!(roc_d_str, abi::DStrResult, abi::DStrResultTag);
hosted_ok!(roc_d_bytes, abi::DBytesResult, abi::DBytesResultTag);
hosted_ok!(roc_d_record, abi::DRecordResult, abi::DRecordResultTag);
hosted_ok!(roc_d_nested, abi::DNestedResult, abi::DNestedResultTag);

hosted_ok!(roc_fallible_unit, abi::HostFallibleUnitResult, abi::HostFallibleUnitResultTag);
hosted_ok!(roc_fallible_str, abi::HostFallibleStrResult, abi::HostFallibleStrResultTag);
hosted_ok!(roc_fallible_bytes, abi::HostFallibleBytesResult, abi::HostFallibleBytesResultTag);
hosted_ok!(roc_fallible_record, abi::HostFallibleRecordResult, abi::HostFallibleRecordResultTag);
hosted_ok!(roc_fallible_nested, abi::HostFallibleNestedResult, abi::HostFallibleNestedResultTag);

#[no_mangle]
pub extern "C" fn wasm_main() -> *const u8 {
    unsafe {
        REPORT_LEN = 0;
        FAILURE_COUNT = 0;
    }
    unsafe { abi::roc_main(); }
    unsafe {
        if FAILURE_COUNT == 0 {
            finish_pass();
        } else if REPORT_LEN == 0 {
            set_report(b"FAIL duplicate-tags RustGlue wasm32: ", b"unknown failure");
        }
        core::ptr::addr_of!(REPORT) as *const u8
    }
}

#[no_mangle]
pub extern "C" fn wasm_result_len() -> usize {
    unsafe { REPORT_LEN }
}

#[no_mangle]
pub extern "C" fn wasm_alloc_count() -> usize {
    unsafe { ALLOC_COUNT }
}

#[no_mangle]
pub extern "C" fn wasm_dealloc_count() -> usize {
    unsafe { DEALLOC_COUNT }
}
