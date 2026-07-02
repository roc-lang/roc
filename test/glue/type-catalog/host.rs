#![allow(improper_ctypes)]
#![allow(improper_ctypes_definitions)]

#[path = "roc_platform_abi.rs"]
mod abi;

use core::ffi::c_void;

unsafe extern "C" {
    fn malloc(size: usize) -> *mut c_void;
    fn free(ptr: *mut c_void);
}

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
            let prefix = b"FAIL type-catalog RustGlue: ";
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
    eprintln!("{}", String::from_utf8_lossy(slice));
}

#[no_mangle]
pub extern "C" fn roc_expect_failed(_bytes: *const u8, _len: usize) {
    fail("roc_expect_failed");
}

#[no_mangle]
pub extern "C" fn roc_crashed(_bytes: *const u8, _len: usize) {
    fail("roc_crashed");
    std::process::exit(1);
}

#[no_mangle]
pub extern "C" fn roc_catalog_roundtrip(arg0: abi::EmptyOrPairOrPayloadOrRecursiveType0) -> abi::EmptyOrPairOrPayloadOrRecursiveType0 {
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
pub extern "C" fn main(_argc: i32, _argv: *const *const u8) -> i32 {
    run_contract();
    unsafe {
        if FAILURE_COUNT != 0 {
            let alloc_count = ALLOC_COUNT;
            let dealloc_count = DEALLOC_COUNT;
            let message = if REPORT_LEN == 0 {
                b"FAIL type-catalog RustGlue: unknown failure".as_slice()
            } else {
                &(&*core::ptr::addr_of!(REPORT))[..REPORT_LEN]
            };
            eprintln!("{}", String::from_utf8_lossy(message));
            eprintln!("alloc={} dealloc={}", alloc_count, dealloc_count);
            return 1;
        }
        let alloc_count = ALLOC_COUNT;
        let dealloc_count = DEALLOC_COUNT;
        eprintln!("PASS glue-runtime type-catalog RustGlue native alloc={} dealloc={}", alloc_count, dealloc_count);
    }
    0
}
