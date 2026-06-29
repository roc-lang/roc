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

unsafe extern "C" {
    fn malloc(size: usize) -> *mut c_void;
    fn free(ptr: *mut c_void);
}

#[derive(Clone, Copy)]
struct Allocation {
    raw: *mut u8,
    user: *mut u8,
    length: usize,
    alignment: usize,
    live: bool,
}

const EMPTY_ALLOCATION: Allocation = Allocation {
    raw: ptr::null_mut(),
    user: ptr::null_mut(),
    length: 0,
    alignment: 0,
    live: false,
};

struct ContractEnv {
    allocations: [Allocation; MAX_ALLOCATIONS],
    alloc_count: usize,
    dealloc_count: usize,
    live_alloc_count: usize,
    allocator_error_count: usize,
    failure_count: usize,
    report: [u8; 1024],
    report_len: usize,
}

impl ContractEnv {
    const fn new() -> Self {
        Self {
            allocations: [EMPTY_ALLOCATION; MAX_ALLOCATIONS],
            alloc_count: 0,
            dealloc_count: 0,
            live_alloc_count: 0,
            allocator_error_count: 0,
            failure_count: 0,
            report: [0; 1024],
            report_len: 0,
        }
    }

    fn fail(&mut self, args: fmt::Arguments<'_>) {
        if self.failure_count == 0 {
            let mut writer = ReportWriter::new(&mut self.report);
            let _ = writer.write_str("FAIL app-model: ");
            let _ = writer.write_fmt(args);
            self.report_len = writer.len;
        }
        self.failure_count += 1;
    }

    fn allocator_fail(&mut self, args: fmt::Arguments<'_>) {
        self.allocator_error_count += 1;
        if self.failure_count == 0 {
            let mut writer = ReportWriter::new(&mut self.report);
            let _ = writer.write_str("FAIL app-model: allocator: ");
            let _ = writer.write_fmt(args);
            self.report_len = writer.len;
        }
        self.failure_count += 1;
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
        let raw = unsafe { malloc(total) as *mut u8 };
        if raw.is_null() {
            self.allocator_fail(format_args!("malloc failed length={length} alignment={alignment}"));
            return ptr::null_mut();
        }

        let user_addr = align_forward(unsafe { raw.add(CANARY_SIZE) } as usize, alignment);
        let user = user_addr as *mut u8;
        if user_addr % alignment != 0 {
            unsafe { free(raw as *mut c_void) };
            self.allocator_fail(format_args!("returned pointer is not aligned to {alignment}"));
            return ptr::null_mut();
        }

        let Some(slot) = self.allocations.iter_mut().find(|allocation| !allocation.live) else {
            unsafe { free(raw as *mut c_void) };
            self.allocator_fail(format_args!("allocation table exhausted"));
            return ptr::null_mut();
        };

        unsafe {
            ptr::write_bytes(user.sub(CANARY_SIZE), CANARY_BYTE, CANARY_SIZE);
            ptr::write_bytes(user, POISON_BYTE, length);
            ptr::write_bytes(user.add(length), CANARY_BYTE, CANARY_SIZE);
        }

        *slot = Allocation {
            raw,
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
            free(allocation.raw as *mut c_void);
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

fn env_mut() -> &'static mut ContractEnv {
    unsafe { &mut *ptr::addr_of_mut!(ENV) }
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

extern "C" fn host_dbg(_host: *mut abi::RocHost, bytes: *const u8, len: usize) {
    let slice = unsafe { core::slice::from_raw_parts(bytes, len) };
    eprintln!("{}", String::from_utf8_lossy(slice));
}

extern "C" fn host_expect_failed(host: *mut abi::RocHost, bytes: *const u8, len: usize) {
    let slice = unsafe { core::slice::from_raw_parts(bytes, len) };
    eprintln!("{}", String::from_utf8_lossy(slice));
    let env = unsafe { &mut *((*host).env as *mut ContractEnv) };
    env.fail(format_args!("roc_expect_failed"));
}

extern "C" fn host_crashed(host: *mut abi::RocHost, bytes: *const u8, len: usize) {
    let slice = unsafe { core::slice::from_raw_parts(bytes, len) };
    eprintln!("{}", String::from_utf8_lossy(slice));
    let env = unsafe { &mut *((*host).env as *mut ContractEnv) };
    env.fail(format_args!("roc_crashed"));
    std::process::exit(1);
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
pub extern "C" fn roc_dbg(bytes: *const u8, len: usize) {
    let slice = unsafe { core::slice::from_raw_parts(bytes, len) };
    eprintln!("{}", String::from_utf8_lossy(slice));
}

#[no_mangle]
pub extern "C" fn roc_expect_failed(bytes: *const u8, len: usize) {
    let slice = unsafe { core::slice::from_raw_parts(bytes, len) };
    eprintln!("{}", String::from_utf8_lossy(slice));
    env_mut().fail(format_args!("roc_expect_failed"));
}

#[no_mangle]
pub extern "C" fn roc_crashed(bytes: *const u8, len: usize) {
    let slice = unsafe { core::slice::from_raw_parts(bytes, len) };
    eprintln!("{}", String::from_utf8_lossy(slice));
    env_mut().fail(format_args!("roc_crashed"));
    std::process::exit(1);
}

fn reset_msg() -> abi::Msg {
    let mut msg: abi::Msg = unsafe { core::mem::zeroed() };
    msg.tag = abi::MsgTag::Reset;
    msg
}

fn run_app_model_contract(host: &abi::RocHost) {
    let initial = unsafe { abi::roc_init() };
    let updated = unsafe { abi::roc_update(initial, reset_msg()) };
    let view = unsafe { abi::roc_render(updated) };

    if view.title.as_slice() != b"ready" {
        env_mut().fail(format_args!("render title mismatch"));
    }
    if view.lifecycle.tag != abi::FailedOrReadyOrWaitingTag::Ready {
        env_mut().fail(format_args!("render lifecycle expected Ready got {:?}", view.lifecycle.tag));
    }
    if view.messages.length != 0 {
        env_mut().fail(format_args!("render messages expected empty got {}", view.messages.length));
    }
    unsafe {
        view.decref(host);
    }
}

#[no_mangle]
pub extern "C" fn main(_argc: i32, _argv: *const *const u8) -> i32 {
    let host = abi::RocHost {
        env: env_mut() as *mut ContractEnv as *mut c_void,
        roc_alloc: host_alloc,
        roc_dealloc: host_dealloc,
        roc_realloc: host_realloc,
        roc_dbg: host_dbg,
        roc_expect_failed: host_expect_failed,
        roc_crashed: host_crashed,
    };
    run_app_model_contract(&host);
    if env_mut().allocator_error_count != 0 {
        let count = env_mut().allocator_error_count;
        env_mut().fail(format_args!("allocator recorded {count} errors"));
    }
    if env_mut().live_alloc_count != 0 {
        let count = env_mut().live_alloc_count;
        env_mut().fail(format_args!("live allocations after scenario: {count}"));
    }

    if env_mut().failure_count != 0 {
        let env = env_mut();
        let message = if env.report_len == 0 {
            "FAIL app-model: unknown failure".as_bytes()
        } else {
            &env.report[..env.report_len]
        };
        eprintln!("{}", String::from_utf8_lossy(message));
        eprintln!(
            "alloc_count={} dealloc_count={} live={} allocator_errors={}",
            env.alloc_count, env.dealloc_count, env.live_alloc_count, env.allocator_error_count
        );
        return 1;
    }

    let env = env_mut();
    eprintln!(
        "PASS glue-runtime app-model RustGlue native alloc={} dealloc={}",
        env.alloc_count, env.dealloc_count
    );
    0
}
