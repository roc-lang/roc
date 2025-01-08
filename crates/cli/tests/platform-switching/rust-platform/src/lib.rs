#![allow(non_snake_case)]
#![no_main]

use core::ffi::c_void;
use roc_std::RocStr;
use std::io::Write;

extern "C" {
    #[link_name = "roc__main_for_host_1_exposed_generic"]
    fn roc_main(_: &mut RocStr);
}

/// # Safety
///
/// TODO
#[no_mangle]
pub unsafe extern "C" fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    libc::malloc(size)
}

/// # Safety
///
/// TODO
#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    libc::realloc(c_ptr, new_size)
}

/// # Safety
///
/// TODO
#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    libc::free(c_ptr);
}

/// # Safety
///
/// TODO
#[no_mangle]
pub unsafe extern "C" fn roc_panic(msg: *mut RocStr, tag_id: u32) {
    match tag_id {
        0 => {
            eprintln!("Roc standard library hit a panic: {}", &*msg);
        }
        1 => {
            eprintln!("Application hit a panic: {}", &*msg);
        }
        _ => unreachable!(),
    }
    std::process::exit(1);
}

/// # Safety
///
/// TODO
#[no_mangle]
pub unsafe extern "C" fn roc_dbg(loc: *mut RocStr, msg: *mut RocStr, src: *mut RocStr) {
    eprintln!("[{}] {} = {}", &*loc, &*src, &*msg);
}

/// # Safety
///
/// TODO
#[no_mangle]
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    libc::memset(dst, c, n)
}

/// # Safety
///
/// TODO
#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_getppid() -> libc::pid_t {
    libc::getppid()
}

/// # Safety
///
/// TODO
#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_mmap(
    addr: *mut libc::c_void,
    len: libc::size_t,
    prot: libc::c_int,
    flags: libc::c_int,
    fd: libc::c_int,
    offset: libc::off_t,
) -> *mut libc::c_void {
    libc::mmap(addr, len, prot, flags, fd, offset)
}

/// # Safety
///
/// TODO
#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_shm_open(
    name: *const libc::c_char,
    oflag: libc::c_int,
    mode: libc::mode_t,
) -> libc::c_int {
    libc::shm_open(name, oflag, mode as libc::c_uint)
}

/// # Safety
///
/// TODO
#[no_mangle]
pub extern "C" fn main(_argc: isize, _argv: *const *const u8) -> isize {
    let mut roc_str = RocStr::default();
    unsafe { roc_main(&mut roc_str) };

    if let Err(e) = std::io::stdout().write_all(roc_str.as_bytes()) {
        panic!("Writing to stdout failed! {:?}", e);
    }

    // roc_str will not print without flushing if it does not contain a newline and you're using --linker=legacy
    if let Err(e) = std::io::stdout().flush() {
        panic!("Failed to flush stdout: {:?}", e);
    }

    0
}
