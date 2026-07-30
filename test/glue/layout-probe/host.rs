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
    fn exit(status: i32) -> !;
}

#[cfg(not(target_os = "windows"))]
unsafe extern "C" {
    fn write(fd: i32, buf: *const u8, count: usize) -> isize;
}

#[cfg(target_os = "windows")]
unsafe extern "C" {
    #[link_name = "_write"]
    fn windows_write(fd: i32, buf: *const u8, count: u32) -> i32;
}

fn raw_write(fd: i32, bytes: &[u8]) {
    unsafe {
        #[cfg(not(target_os = "windows"))]
        {
            let _ = write(fd, bytes.as_ptr(), bytes.len());
        }
        #[cfg(target_os = "windows")]
        {
            let _ = windows_write(fd, bytes.as_ptr(), bytes.len() as u32);
        }
    }
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
    raw_write(2, bytes);
    raw_write(2, b"\n");
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
pub extern "C" fn roc_probe_roundtrip_u8x16(arg0: abi::RocU8x16) -> abi::RocU8x16 { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_i8x16(arg0: abi::RocI8x16) -> abi::RocI8x16 { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_u16x8(arg0: abi::RocU16x8) -> abi::RocU16x8 { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_i16x8(arg0: abi::RocI16x8) -> abi::RocI16x8 { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_u32x4(arg0: abi::RocU32x4) -> abi::RocU32x4 { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_i32x4(arg0: abi::RocI32x4) -> abi::RocI32x4 { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_u64x2(arg0: abi::RocU64x2) -> abi::RocU64x2 { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_i64x2(arg0: abi::RocI64x2) -> abi::RocI64x2 { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_vector_record(arg0: abi::ProbeVectorRecord) -> abi::ProbeVectorRecord { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_vector_quad(arg0: abi::ProbeVectorQuad) -> abi::ProbeVectorQuad { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_vector_hva(arg0: abi::ProbeVectorHva) -> abi::ProbeVectorHva { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_vector_wrapper(arg0: abi::ProbeVectorWrapper) -> abi::ProbeVectorWrapper { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_vector_tag(arg0: abi::ProbeVectorTag) -> abi::ProbeVectorTag { arg0 }
#[no_mangle]
pub extern "C" fn roc_probe_roundtrip_vector_tuple(
    arg0: abi::ProbeRoundtripVectorTupleArgs,
) -> abi::AnonStructFbe9eaebfd8c38fd {
    abi::AnonStructFbe9eaebfd8c38fd { _1: arg0._1, _2: arg0._2, _0: arg0._0 }
}

#[no_mangle]
pub extern "C" fn roc_probe_exhaust_registers(
    _arg0: i64, _arg1: i64, _arg2: i64, _arg3: i64, _arg4: i64, _arg5: i64,
    _arg6: f64, _arg7: f64, _arg8: f64, _arg9: f64, _arg10: f64, _arg11: f64, _arg12: f64, _arg13: f64,
    arg14: abi::RocU8x16,
) -> abi::RocU8x16 {
    arg14
}

#[no_mangle]
pub extern "C" fn roc_probe_spill_vector_hva(
    _arg0: abi::RocU8x16, _arg1: abi::RocU8x16, _arg2: abi::RocU8x16, _arg3: abi::RocU8x16,
    _arg4: abi::RocU8x16, _arg5: abi::RocU8x16, _arg6: abi::RocU8x16,
    arg7: abi::ProbeNestedVectorHva,
) -> abi::ProbeNestedVectorHva { arg7 }

#[no_mangle]
pub extern "C" fn roc_probe_spill_float_hfa(
    _arg0: f64, _arg1: f64, _arg2: f64, _arg3: f64, _arg4: f64, _arg5: f64, _arg6: f64,
    arg7: abi::ProbeNestedFloatHfa,
) -> abi::ProbeNestedFloatHfa { arg7 }

#[no_mangle]
pub extern "C" fn roc_probe_spill_integer_pair(
    _arg0: i64, _arg1: i64, _arg2: i64, _arg3: i64, _arg4: i64, _arg5: i64, _arg6: i64,
    arg7: abi::ProbeIntegerPair,
) -> abi::ProbeIntegerPair { arg7 }

#[no_mangle]
pub extern "C" fn roc_probe_align_i128(_arg0: i64, arg1: i128) -> i128 { arg1 }

#[no_mangle]
pub extern "C" fn roc_probe_spill_i128(
    _arg0: i64, _arg1: i64, _arg2: i64, _arg3: i64, _arg4: i64, arg5: i128,
) -> i128 { arg5 }

#[no_mangle]
pub extern "C" fn roc_probe_spill_dec(
    _arg0: i64, _arg1: i64, _arg2: i64, _arg3: i64, _arg4: i64, arg5: abi::RocDec,
) -> abi::RocDec { arg5 }

#[no_mangle]
pub extern "C" fn roc_probe_compact_stack(
    _arg0: i64, _arg1: i64, _arg2: i64, _arg3: i64,
    _arg4: i64, _arg5: i64, _arg6: i64, _arg7: i64,
    tiny: u8, short: u16, word: u32,
) -> u64 { u64::from(tiny) + u64::from(short) + u64::from(word) }

fn vector_from_bits<T: Copy>(bits: u128) -> T {
    assert!(core::mem::size_of::<T>() == core::mem::size_of::<u128>());
    unsafe { core::mem::transmute_copy(&bits) }
}

fn vector_bits<T: Copy>(value: T) -> u128 {
    assert!(core::mem::size_of::<T>() == core::mem::size_of::<u128>());
    unsafe { core::mem::transmute_copy(&value) }
}

fn check_provided_abi() {
    let bits = 0x0ffeedfccbbaa9988776655443322110_u128;
    let u8x16 = vector_from_bits::<abi::RocU8x16>(bits);
    let i8x16 = vector_from_bits::<abi::RocI8x16>(bits);
    let u16x8 = vector_from_bits::<abi::RocU16x8>(bits);
    let i16x8 = vector_from_bits::<abi::RocI16x8>(bits);
    let u32x4 = vector_from_bits::<abi::RocU32x4>(bits);
    let i32x4 = vector_from_bits::<abi::RocI32x4>(bits);
    let u64x2 = vector_from_bits::<abi::RocU64x2>(bits);
    let i64x2 = vector_from_bits::<abi::RocI64x2>(bits);

    unsafe {
        if vector_bits(abi::roc_provide_u8x16(u8x16)) != bits { fail("provided U8x16 mismatch"); }
        if vector_bits(abi::roc_provide_i8x16(i8x16)) != bits { fail("provided I8x16 mismatch"); }
        if vector_bits(abi::roc_provide_u16x8(u16x8)) != bits { fail("provided U16x8 mismatch"); }
        if vector_bits(abi::roc_provide_i16x8(i16x8)) != bits { fail("provided I16x8 mismatch"); }
        if vector_bits(abi::roc_provide_u32x4(u32x4)) != bits { fail("provided U32x4 mismatch"); }
        if vector_bits(abi::roc_provide_i32x4(i32x4)) != bits { fail("provided I32x4 mismatch"); }
        if vector_bits(abi::roc_provide_u64x2(u64x2)) != bits { fail("provided U64x2 mismatch"); }
        if vector_bits(abi::roc_provide_i64x2(i64x2)) != bits { fail("provided I64x2 mismatch"); }

        let wrapper = abi::ProbeVectorWrapper { only: u8x16 };
        let wrapper_back = abi::roc_provide_vector_wrapper(wrapper);
        if vector_bits(wrapper_back.only) != bits { fail("provided vector wrapper mismatch"); }

        let record = abi::ProbeVectorRecord {
            bytes: u8x16,
            words: i32x4,
            before: 0x1020304050607080,
            after: 0xa0b0c0d0,
        };
        let record_back = abi::roc_provide_vector_record(record);
        if vector_bits(record_back.bytes) != bits
            || vector_bits(record_back.words) != bits
            || record_back.before != record.before
            || record_back.after != record.after
        {
            fail("provided vector record mismatch");
        }

        let quad = abi::ProbeVectorQuad { a: u8x16, b: i16x8, c: u32x4, d: i64x2 };
        let quad_back = abi::roc_provide_vector_quad(quad);
        if vector_bits(quad_back.a) != bits
            || vector_bits(quad_back.b) != bits
            || vector_bits(quad_back.c) != bits
            || vector_bits(quad_back.d) != bits
        {
            fail("provided vector quad mismatch");
        }

        let hva = abi::ProbeVectorHva { a: u8x16, b: u8x16, c: u8x16, d: u8x16 };
        let hva_back = abi::roc_provide_vector_hva(hva);
        if vector_bits(hva_back.a) != bits
            || vector_bits(hva_back.b) != bits
            || vector_bits(hva_back.c) != bits
            || vector_bits(hva_back.d) != bits
        {
            fail("provided vector HVA mismatch");
        }

        let tuple = abi::AnonStructFbe9eaebfd8c38fd {
            _1: u8x16,
            _2: i16x8,
            _0: 0x1020304050607080,
        };
        let tuple_back = abi::roc_provide_vector_tuple(tuple);
        if vector_bits(tuple_back._1) != bits
            || vector_bits(tuple_back._2) != bits
            || tuple_back._0 != tuple._0
        {
            fail("provided vector tuple mismatch");
        }

        let tag = abi::roc_make_vector_tag();
        let tag_back = abi::roc_provide_vector_tag(tag);
        if tag_back.tag != abi::ProbeVectorTagTag::Pair {
            fail("provided vector tag discriminant mismatch");
        } else {
            let pair = tag_back.payload_pair();
            if pair._0 != 0x1020304050607080
                || vector_bits(pair._1) != 0x00112233445566778899aabbccddeeff
            {
                fail("provided vector tag payload mismatch");
            }
        }

        let exhausted = abi::roc_provide_exhaust_registers(
            1, 2, 3, 4, 5, 6, 0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, 7.5, u8x16,
        );
        if vector_bits(exhausted) != bits { fail("provided exhausted-register vector mismatch"); }

        let nested_hva = abi::ProbeNestedVectorHva { wrapped: u8x16, raw: u8x16 };
        let nested_hva_back = abi::roc_provide_spill_vector_hva(u8x16, u8x16, u8x16, u8x16, u8x16, u8x16, u8x16, nested_hva);
        if vector_bits(nested_hva_back.wrapped) != bits || vector_bits(nested_hva_back.raw) != bits {
            fail("provided spilled HVA mismatch");
        }

        let nested_hfa = abi::ProbeNestedFloatHfa { wrapped: 12.5, raw: -7.25 };
        let nested_hfa_back = abi::roc_provide_spill_float_hfa(0.5, 1.5, 2.5, 3.5, 4.5, 5.5, 6.5, nested_hfa);
        if nested_hfa_back.wrapped != nested_hfa.wrapped || nested_hfa_back.raw != nested_hfa.raw {
            fail("provided spilled HFA mismatch");
        }

        let pair = abi::ProbeIntegerPair { first: -0x102030405060708, second: 0x8877665544332211 };
        let pair_back = abi::roc_provide_spill_integer_pair(1, 2, 3, 4, 5, 6, 7, pair);
        if pair_back.first != pair.first || pair_back.second != pair.second {
            fail("provided spilled integer pair mismatch");
        }

        let wide_i128 = 0x00112233445566778899aabbccddeeff_i128;
        if abi::roc_provide_align_i128(9, wide_i128) != wide_i128 { fail("provided aligned i128 mismatch"); }
        if abi::roc_provide_spill_i128(1, 2, 3, 4, 5, wide_i128) != wide_i128 { fail("provided spilled i128 mismatch"); }
        let wide_dec = abi::RocDec { num: 0x0123456789abcdeffedcba9876543210_i128 };
        if abi::roc_provide_spill_dec(1, 2, 3, 4, 5, wide_dec).num != wide_dec.num { fail("provided spilled Dec mismatch"); }
        let compact = abi::roc_provide_compact_stack(1, 2, 3, 4, 5, 6, 7, 8, 0x12, 0x3456, 0x789abcde);
        if compact != 0x12_u64 + 0x3456_u64 + 0x789abcde_u64 { fail("provided compact stack mismatch"); }
    }
}

#[no_mangle]
pub extern "C" fn main(_argc: i32, _argv: *const *const u8) -> i32 {
    unsafe { abi::roc_main(); }
    check_provided_abi();
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
