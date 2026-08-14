#![no_std]
#![allow(improper_ctypes)]
#![allow(improper_ctypes_definitions)]

#[path = "roc_platform_abi.rs"]
mod abi;

use core::ffi::c_void;
use core::panic::PanicInfo;
use core::ptr;

#[panic_handler]
fn panic(_info: &PanicInfo<'_>) -> ! {
    core::arch::wasm32::unreachable()
}

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
            report: [0; 1024],
            report_len: 0,
        }
    }

    fn reset(&mut self) {
        *self = Self::new();
    }

    fn set_report(&mut self, prefix: &[u8], message: &[u8]) {
        let prefix_len = prefix.len().min(self.report.len());
        self.report[..prefix_len].copy_from_slice(&prefix[..prefix_len]);
        let message_len = message.len().min(self.report.len() - prefix_len);
        self.report[prefix_len..prefix_len + message_len].copy_from_slice(&message[..message_len]);
        self.report_len = prefix_len + message_len;
    }

    fn fail(&mut self, message: &str) {
        if self.failure_count == 0 {
            self.set_report(b"FAIL app-model RustGlue wasm32: ", message.as_bytes());
        }
        self.failure_count += 1;
    }

    fn allocator_fail(&mut self, message: &str) {
        self.allocator_error_count += 1;
        if self.failure_count == 0 {
            self.set_report(b"FAIL app-model RustGlue wasm32 allocator: ", message.as_bytes());
        }
        self.failure_count += 1;
    }

    fn finish_pass(&mut self) {
        self.set_report(b"", b"PASS glue-runtime app-model RustGlue wasm32");
    }

    fn check_canaries(&mut self, allocation: &Allocation) -> bool {
        for offset in 0..CANARY_SIZE {
            let prefix = unsafe { *allocation.user.sub(CANARY_SIZE).add(offset) };
            let suffix = unsafe { *allocation.user.add(allocation.length + offset) };
            if prefix != CANARY_BYTE {
                self.allocator_fail("prefix canary changed");
                return false;
            }
            if suffix != CANARY_BYTE {
                self.allocator_fail("suffix canary changed");
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
            self.allocator_fail("bump allocation overflow");
            return ptr::null_mut();
        };
        if !self.ensure_wasm_memory(end) {
            self.allocator_fail("wasm memory grow failed");
            return ptr::null_mut();
        }
        self.heap_cursor = end;
        raw as *mut u8
    }

    fn alloc(&mut self, length: usize, alignment: usize) -> *mut c_void {
        if alignment == 0 || (alignment & (alignment - 1)) != 0 {
            self.allocator_fail("invalid alignment");
            return ptr::null_mut();
        }
        if length > usize::MAX - CANARY_SIZE - CANARY_SIZE - alignment {
            self.allocator_fail("allocation size overflow");
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
            self.allocator_fail("returned pointer is not aligned");
            return ptr::null_mut();
        }

        let Some(slot) = self.allocations.iter_mut().find(|allocation| !allocation.live) else {
            self.allocator_fail("allocation table exhausted");
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
            self.allocator_fail("unknown or double free");
            return;
        };
        let allocation = self.allocations[index];
        if allocation.alignment != alignment {
            self.allocator_fail("dealloc alignment mismatch");
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
            self.allocator_fail("realloc unknown pointer");
            return ptr::null_mut();
        };
        let old = self.allocations[index];
        if old.alignment != alignment {
            self.allocator_fail("realloc alignment mismatch");
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
                self.allocator_fail("realloc did not preserve old bytes");
            }
        }
        self.dealloc(ptr, alignment);
        new_ptr
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

extern "C" fn host_dbg(_host: *mut abi::RocHost, _bytes: *const u8, _len: usize) {}

extern "C" fn host_expect_failed(host: *mut abi::RocHost, _bytes: *const u8, _len: usize) {
    let env = unsafe { &mut *((*host).env as *mut ContractEnv) };
    env.fail("roc_expect_failed");
}

extern "C" fn host_crashed(host: *mut abi::RocHost, _bytes: *const u8, _len: usize) {
    let env = unsafe { &mut *((*host).env as *mut ContractEnv) };
    env.fail("roc_crashed");
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
    env_mut().fail("roc_expect_failed");
}

#[no_mangle]
pub extern "C" fn roc_crashed(_bytes: *const u8, _len: usize) {
    env_mut().fail("roc_crashed");
}

fn reset_msg() -> abi::Msg {
    let mut msg: abi::Msg = unsafe { core::mem::zeroed() };
    msg.tag = abi::MsgTag::Reset;
    msg
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

    let initial = unsafe { abi::roc_init() };
    let updated = unsafe { abi::roc_update(initial, reset_msg()) };
    let view = unsafe { abi::roc_render(updated) };
    if view.title.as_slice() != b"ready" {
        env_mut().fail("render title mismatch");
    }
    if view.lifecycle.tag != abi::FailedOrReadyOrWaitingTag::Ready {
        env_mut().fail("render lifecycle mismatch");
    }
    if view.messages.length != 0 {
        env_mut().fail("render messages expected empty");
    }
    unsafe {
        view.decref(&host);
    }

    if env_mut().allocator_error_count != 0 {
        env_mut().fail("allocator recorded errors");
    }
    if env_mut().live_alloc_count != 0 {
        env_mut().fail("live allocations after scenario");
    }

    if env_mut().failure_count == 0 {
        env_mut().finish_pass();
    } else if env_mut().report_len == 0 {
        env_mut().fail("unknown failure");
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
