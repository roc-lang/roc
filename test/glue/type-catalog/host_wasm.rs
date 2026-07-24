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
    set_report(b"FAIL type-catalog RustGlue wasm32: ", message.as_bytes());
    unsafe {
        FAILURE_COUNT += 1;
    }
}

fn finish_pass() {
    let message = b"PASS glue-runtime type-catalog RustGlue wasm32";
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

#[no_mangle]
pub extern "C" fn roc_catalog_roundtrip(arg0: abi::EmptyOrPairOrPayloadOrRecursive) -> abi::EmptyOrPairOrPayloadOrRecursive {
    arg0
}

#[no_mangle]
pub extern "C" fn roc_catalog_single_no_payload() -> *mut c_void {
    core::ptr::null_mut()
}

#[no_mangle]
pub extern "C" fn roc_catalog_single_payload_roundtrip(arg0: abi::CatalogPayload) -> abi::CatalogPayload {
    arg0
}

fn expect_str(value: &abi::RocStr, expected: &[u8], label: &str) {
    if value.as_slice() != expected {
        fail(label);
    }
}

fn run_contract() {
    let point = unsafe { abi::roc_point() };
    if point.x != -17 || point.y != 42 {
        fail("point mismatch");
    }
    let structural = unsafe { abi::roc_structural() };
    if structural.count != 19 {
        fail("structural count mismatch");
    }
    expect_str(&structural.name, b"catalog", "structural name mismatch");
    if structural.nested.byte != 7 || !structural.nested.flag {
        fail("structural nested mismatch");
    }
    let result_a = unsafe { abi::roc_result_a() };
    if result_a.tag != abi::AResultTag::Ok {
        fail("A.Result tag mismatch");
    }
    expect_str(&result_a.payload_ok(), b"alpha", "A.Result payload mismatch");
    let result_b = unsafe { abi::roc_result_b() };
    if result_b.tag != abi::BResultTag::Err {
        fail("B.Result tag mismatch");
    }
    let err = result_b.payload_err();
    if err.code != 5 {
        fail("B.Result code mismatch");
    }
    expect_str(&err.message, b"bravo", "B.Result message mismatch");
    let dec = abi::RocDec { num: 1_250_000_000_000_000_000i128 };
    if unsafe { abi::roc_dec(dec) }.num != dec.num {
        fail("Dec identity mismatch");
    }
    if unsafe { abi::roc_i128(-123456789) } != -123456789 {
        fail("I128 identity mismatch");
    }
    if unsafe { abi::roc_u128(123456789) } != 123456789 {
        fail("U128 identity mismatch");
    }
}

#[no_mangle]
pub extern "C" fn wasm_main() -> *const u8 {
    unsafe {
        REPORT_LEN = 0;
        FAILURE_COUNT = 0;
    }
    run_contract();
    unsafe {
        if FAILURE_COUNT == 0 {
            finish_pass();
        } else if REPORT_LEN == 0 {
            set_report(b"FAIL type-catalog RustGlue wasm32: ", b"unknown failure");
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
