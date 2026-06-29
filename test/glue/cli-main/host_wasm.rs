#![allow(improper_ctypes)]
#![allow(improper_ctypes_definitions)]

#[path = "roc_platform_abi.rs"]
mod abi;

use core::ffi::c_void;
use core::fmt::{self, Write};
use core::ptr;

const MAX_ALLOCATIONS: usize = 512;
const CANARY_SIZE: usize = 16;
const CANARY_BYTE: u8 = 0xA5;
const POISON_BYTE: u8 = 0xCC;
const WASM_PAGE_SIZE: usize = 65_536;

#[derive(Clone, Copy)]
struct Allocation {
    user: *mut u8,
    length: usize,
    alignment: usize,
    live: bool,
}

const EMPTY_ALLOCATION: Allocation = Allocation {
    user: ptr::null_mut(),
    length: 0,
    alignment: 0,
    live: false,
};

struct ContractEnv {
    allocations: [Allocation; MAX_ALLOCATIONS],
    heap_cursor: usize,
    alloc_count: usize,
    dealloc_count: usize,
    live_alloc_count: usize,
    allocator_error_count: usize,
    failure_count: usize,
    log_count: usize,
    report: [u8; 1024],
    report_len: usize,
}

impl ContractEnv {
    const fn new() -> Self {
        Self {
            allocations: [EMPTY_ALLOCATION; MAX_ALLOCATIONS],
            heap_cursor: 0,
            alloc_count: 0,
            dealloc_count: 0,
            live_alloc_count: 0,
            allocator_error_count: 0,
            failure_count: 0,
            log_count: 0,
            report: [0; 1024],
            report_len: 0,
        }
    }

    fn reset(&mut self) {
        *self = Self::new();
    }

    fn fail(&mut self, args: fmt::Arguments<'_>) {
        if self.failure_count == 0 {
            let mut writer = ReportWriter::new(&mut self.report);
            let _ = writer.write_str("FAIL cli-main RustGlue wasm32: ");
            let _ = writer.write_fmt(args);
            self.report_len = writer.len;
        }
        self.failure_count += 1;
    }

    fn allocator_fail(&mut self, args: fmt::Arguments<'_>) {
        self.allocator_error_count += 1;
        if self.failure_count == 0 {
            let mut writer = ReportWriter::new(&mut self.report);
            let _ = writer.write_str("FAIL cli-main RustGlue wasm32 allocator: ");
            let _ = writer.write_fmt(args);
            self.report_len = writer.len;
        }
        self.failure_count += 1;
    }

    fn finish_pass(&mut self) {
        let mut writer = ReportWriter::new(&mut self.report);
        let _ = writer.write_str("PASS glue-runtime cli-main RustGlue wasm32");
        self.report_len = writer.len;
    }

    fn check_canaries(&mut self, allocation: &Allocation) -> bool {
        for offset in 0..CANARY_SIZE {
            let prefix = unsafe { *allocation.user.sub(CANARY_SIZE).add(offset) };
            let suffix = unsafe { *allocation.user.add(allocation.length + offset) };
            if prefix != CANARY_BYTE {
                self.allocator_fail(format_args!("prefix canary changed"));
                return false;
            }
            if suffix != CANARY_BYTE {
                self.allocator_fail(format_args!("suffix canary changed"));
                return false;
            }
        }
        true
    }

    fn ensure_wasm_memory(&mut self, end: usize) -> bool {
        let required_pages = (end + WASM_PAGE_SIZE - 1) / WASM_PAGE_SIZE;
        let current_pages = core::arch::wasm32::memory_size(0);
        if required_pages <= current_pages {
            return true;
        }
        let previous_pages = core::arch::wasm32::memory_grow(0, required_pages - current_pages);
        previous_pages != usize::MAX
    }

    fn bump_alloc(&mut self, total: usize, alignment: usize) -> *mut u8 {
        if self.heap_cursor == 0 {
            self.heap_cursor = core::arch::wasm32::memory_size(0) * WASM_PAGE_SIZE;
        }
        let raw = align_forward(self.heap_cursor, alignment);
        let Some(end) = raw.checked_add(total) else {
            self.allocator_fail(format_args!("bump allocation overflow"));
            return ptr::null_mut();
        };
        if !self.ensure_wasm_memory(end) {
            self.allocator_fail(format_args!("wasm memory grow failed"));
            return ptr::null_mut();
        }
        self.heap_cursor = end;
        raw as *mut u8
    }

    fn alloc(&mut self, length: usize, alignment: usize) -> *mut c_void {
        if alignment == 0 || (alignment & (alignment - 1)) != 0 {
            self.allocator_fail(format_args!("invalid alignment {alignment}"));
            return ptr::null_mut();
        }
        if length > usize::MAX - CANARY_SIZE - CANARY_SIZE - alignment {
            self.allocator_fail(format_args!("allocation size overflow length={length} alignment={alignment}"));
            return ptr::null_mut();
        }

        let total = CANARY_SIZE + alignment - 1 + length + CANARY_SIZE;
        let raw = self.bump_alloc(total.max(1), alignment);
        if raw.is_null() {
            return ptr::null_mut();
        }

        let user_addr = align_forward(unsafe { raw.add(CANARY_SIZE) } as usize, alignment);
        let user = user_addr as *mut u8;
        if user_addr % alignment != 0 {
            self.allocator_fail(format_args!("returned pointer is not aligned to {alignment}"));
            return ptr::null_mut();
        }

        let Some(slot) = self.allocations.iter_mut().find(|allocation| !allocation.live) else {
            self.allocator_fail(format_args!("allocation table exhausted"));
            return ptr::null_mut();
        };

        unsafe {
            ptr::write_bytes(user.sub(CANARY_SIZE), CANARY_BYTE, CANARY_SIZE);
            ptr::write_bytes(user, POISON_BYTE, length);
            ptr::write_bytes(user.add(length), CANARY_BYTE, CANARY_SIZE);
        }

        *slot = Allocation {
            user,
            length,
            alignment,
            live: true,
        };
        self.alloc_count += 1;
        self.live_alloc_count += 1;
        user as *mut c_void
    }

    fn dealloc(&mut self, ptr: *mut c_void, alignment: usize) {
        if ptr.is_null() {
            return;
        }
        let Some(index) = self
            .allocations
            .iter()
            .position(|allocation| allocation.live && allocation.user == ptr as *mut u8)
        else {
            self.allocator_fail(format_args!("unknown or double free for {ptr:p}"));
            return;
        };
        let allocation = self.allocations[index];
        if allocation.alignment != alignment {
            self.allocator_fail(format_args!(
                "dealloc alignment mismatch allocated={} freed={alignment}",
                allocation.alignment
            ));
        }
        let _ = self.check_canaries(&allocation);
        unsafe {
            ptr::write_bytes(allocation.user, 0xDD, allocation.length);
        }
        self.allocations[index].live = false;
        self.dealloc_count += 1;
        self.live_alloc_count -= 1;
    }

    fn realloc(&mut self, ptr: *mut c_void, new_length: usize, alignment: usize) -> *mut c_void {
        if ptr.is_null() {
            return self.alloc(new_length, alignment);
        }
        let Some(index) = self
            .allocations
            .iter()
            .position(|allocation| allocation.live && allocation.user == ptr as *mut u8)
        else {
            self.allocator_fail(format_args!("realloc unknown pointer {ptr:p}"));
            return ptr::null_mut();
        };
        let old = self.allocations[index];
        if old.alignment != alignment {
            self.allocator_fail(format_args!(
                "realloc alignment mismatch allocated={} requested={alignment}",
                old.alignment
            ));
            return ptr::null_mut();
        }
        if !self.check_canaries(&old) {
            return ptr::null_mut();
        }

        let copy_length = old.length.min(new_length);
        let new_ptr = self.alloc(new_length, alignment);
        if new_ptr.is_null() {
            return ptr::null_mut();
        }
        unsafe {
            ptr::copy_nonoverlapping(old.user, new_ptr as *mut u8, copy_length);
            let old_bytes = core::slice::from_raw_parts(old.user, copy_length);
            let new_bytes = core::slice::from_raw_parts(new_ptr as *const u8, copy_length);
            if old_bytes != new_bytes {
                self.allocator_fail(format_args!("realloc did not preserve old bytes"));
            }
        }
        self.dealloc(ptr, alignment);
        new_ptr
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

static mut ENV: ContractEnv = ContractEnv::new();
static mut HOST_PTR: *const abi::RocHost = ptr::null();

fn env_mut() -> &'static mut ContractEnv {
    unsafe { &mut *ptr::addr_of_mut!(ENV) }
}

fn current_host() -> &'static abi::RocHost {
    let host_ptr = unsafe { HOST_PTR };
    if host_ptr.is_null() {
        env_mut().fail(format_args!("RocHost was not initialized"));
        unsafe { core::hint::unreachable_unchecked() }
    }
    unsafe { &*host_ptr }
}

fn align_forward(value: usize, alignment: usize) -> usize {
    (value + alignment - 1) & !(alignment - 1)
}

extern "C" fn host_alloc(host: *mut abi::RocHost, length: usize, alignment: usize) -> *mut c_void {
    let env = unsafe { &mut *((*host).env as *mut ContractEnv) };
    env.alloc(length, alignment)
}

extern "C" fn host_dealloc(host: *mut abi::RocHost, ptr: *mut c_void, alignment: usize) {
    let env = unsafe { &mut *((*host).env as *mut ContractEnv) };
    env.dealloc(ptr, alignment);
}

extern "C" fn host_realloc(host: *mut abi::RocHost, ptr: *mut c_void, new_length: usize, alignment: usize) -> *mut c_void {
    let env = unsafe { &mut *((*host).env as *mut ContractEnv) };
    env.realloc(ptr, new_length, alignment)
}

extern "C" fn host_dbg(_host: *mut abi::RocHost, _bytes: *const u8, _len: usize) {}

extern "C" fn host_expect_failed(host: *mut abi::RocHost, _bytes: *const u8, _len: usize) {
    let env = unsafe { &mut *((*host).env as *mut ContractEnv) };
    env.fail(format_args!("roc_expect_failed"));
}

extern "C" fn host_crashed(host: *mut abi::RocHost, _bytes: *const u8, _len: usize) {
    let env = unsafe { &mut *((*host).env as *mut ContractEnv) };
    env.fail(format_args!("roc_crashed"));
}

#[no_mangle]
pub extern "C" fn roc_alloc(length: usize, alignment: usize) -> *mut c_void {
    env_mut().alloc(length, alignment)
}

#[no_mangle]
pub extern "C" fn roc_dealloc(ptr: *mut c_void, alignment: usize) {
    env_mut().dealloc(ptr, alignment);
}

#[no_mangle]
pub extern "C" fn roc_realloc(ptr: *mut c_void, new_length: usize, alignment: usize) -> *mut c_void {
    env_mut().realloc(ptr, new_length, alignment)
}

#[no_mangle]
pub extern "C" fn roc_dbg(_bytes: *const u8, _len: usize) {}

#[no_mangle]
pub extern "C" fn roc_expect_failed(_bytes: *const u8, _len: usize) {
    env_mut().fail(format_args!("roc_expect_failed"));
}

#[no_mangle]
pub extern "C" fn roc_crashed(_bytes: *const u8, _len: usize) {
    env_mut().fail(format_args!("roc_crashed"));
}

#[no_mangle]
pub extern "C" fn roc_cli_read() -> abi::RocStr {
    abi::RocStr::from_slice(b"contract-input", current_host())
}

#[no_mangle]
pub extern "C" fn roc_cli_log(arg0: abi::RocStr) {
    let expected = b"roc saw contract-input argc=2 first=alpha";
    if arg0.as_slice() != expected {
        env_mut().fail(format_args!("unexpected log payload"));
    }
    env_mut().log_count += 1;
    unsafe {
        arg0.decref(current_host());
    }
}

#[no_mangle]
pub extern "C" fn roc_cli_many(
    arg0: u8,
    arg1: u16,
    arg2: u32,
    arg3: u64,
    arg4: u128,
    arg5: i8,
    arg6: i16,
    arg7: i32,
    arg8: i64,
    arg9: i128,
    arg10: f32,
    arg11: f64,
    arg12: f64,
    arg13: bool,
    arg14: abi::RocStr,
) -> abi::CliHostManyResult {
    let _ = (arg0, arg1, arg2, arg3, arg4, arg5, arg6, arg7, arg8, arg9, arg10, arg11, arg12, arg13);
    unsafe {
        arg14.decref(current_host());
    }
    env_mut().fail(format_args!("roc_cli_many was called"));
    unsafe { core::mem::zeroed() }
}

#[no_mangle]
pub extern "C" fn roc_cli_shape(arg0: abi::CircleOrEmptyOrRect, arg1: abi::AnonStruct22) -> abi::CliHostNamedRecord {
    let _ = (arg0, arg1);
    env_mut().fail(format_args!("roc_cli_shape was called"));
    unsafe { core::mem::zeroed() }
}

#[no_mangle]
pub extern "C" fn roc_cli_wide(arg0: f64, arg1: i128, arg2: u128) -> abi::CliHostWideRetRecord {
    let _ = (arg0, arg1, arg2);
    env_mut().fail(format_args!("roc_cli_wide was called"));
    unsafe { core::mem::zeroed() }
}

fn validate_refcounted_list_header(list: abi::RocList<abi::RocStr>) {
    if list.elements.is_null() {
        env_mut().fail(format_args!("argument list has null elements"));
        return;
    }
    let count = unsafe { *((list.elements as *const usize).sub(2)) };
    if count != list.length {
        env_mut().fail(format_args!("refcounted list element count header expected {} got {count}", list.length));
    }
}

fn make_args(host: &abi::RocHost) -> abi::RocList<abi::RocStr> {
    let list = unsafe { abi::RocList::<abi::RocStr>::allocate(2, host) };
    unsafe {
        ptr::write(list.elements.add(0), abi::RocStr::from_slice(b"alpha", host));
        ptr::write(list.elements.add(1), abi::RocStr::from_slice(b"beta", host));
    }
    validate_refcounted_list_header(list);
    list
}

fn run_contract() {
    let host = abi::RocHost {
        env: env_mut() as *mut ContractEnv as *mut c_void,
        roc_alloc: host_alloc,
        roc_dealloc: host_dealloc,
        roc_realloc: host_realloc,
        roc_dbg: host_dbg,
        roc_expect_failed: host_expect_failed,
        roc_crashed: host_crashed,
    };
    unsafe {
        HOST_PTR = &host as *const abi::RocHost;
    }

    let args = make_args(&host);
    let result = unsafe { abi::roc_main(args) };
    if result.tag != abi::MainForHostResultTag::Ok {
        env_mut().fail(format_args!("roc_main returned Err tag={:?}", result.tag));
    }

    if env_mut().log_count != 1 {
        let count = env_mut().log_count;
        env_mut().fail(format_args!("expected one log call, saw {count}"));
    }
    if env_mut().allocator_error_count != 0 {
        let count = env_mut().allocator_error_count;
        env_mut().fail(format_args!("allocator recorded {count} errors"));
    }
    if env_mut().live_alloc_count != 0 {
        let count = env_mut().live_alloc_count;
        env_mut().fail(format_args!("live allocations after scenario: {count}"));
    }

    if env_mut().failure_count == 0 {
        env_mut().finish_pass();
    } else if env_mut().report_len == 0 {
        env_mut().fail(format_args!("unknown failure"));
    }
}

#[no_mangle]
pub extern "C" fn wasm_main() -> *const u8 {
    env_mut().reset();
    run_contract();
    env_mut().report.as_ptr()
}

#[no_mangle]
pub extern "C" fn wasm_result_len() -> usize {
    env_mut().report_len
}

#[no_mangle]
pub extern "C" fn wasm_alloc_count() -> usize {
    env_mut().alloc_count
}

#[no_mangle]
pub extern "C" fn wasm_dealloc_count() -> usize {
    env_mut().dealloc_count
}
