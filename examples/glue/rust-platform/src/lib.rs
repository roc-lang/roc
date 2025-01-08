#![allow(non_snake_case)]

use core::ffi::c_void;
use roc_app::Op;
use roc_std::RocStr;
use std::ffi::CStr;
use std::io::Write;
use std::os::raw::c_char;

use roc_app::main_for_host as roc_main;

#[no_mangle]
pub unsafe extern "C" fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    return libc::malloc(size);
}

#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    return libc::realloc(c_ptr, new_size);
}

#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    return libc::free(c_ptr);
}

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

#[no_mangle]
pub unsafe extern "C" fn roc_dbg(loc: *mut RocStr, msg: *mut RocStr, src: *mut RocStr) {
    eprintln!("[{}] {} = {}", &*loc, &*src, &*msg);
}

#[no_mangle]
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    libc::memset(dst, c, n)
}

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_getppid() -> libc::pid_t {
    libc::getppid()
}

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

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_shm_open(
    name: *const libc::c_char,
    oflag: libc::c_int,
    mode: libc::mode_t,
) -> libc::c_int {
    libc::shm_open(name, oflag, mode as libc::c_uint)
}

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    use roc_app::discriminant_Op::*;

    println!("Let's do things!");

    let mut op: Op = roc_main();

    loop {
        match op.discriminant() {
            StdoutWrite => {
                let stdout_write = op.get_StdoutWrite();
                let output: RocStr = stdout_write.f0;
                op = unsafe { stdout_write.f1.force_thunk() };

                if let Err(e) = std::io::stdout().write_all(output.as_bytes()) {
                    panic!("Writing to stdout failed! {:?}", e);
                }
            }
            StderrWrite => {
                let stderr_write = op.get_StderrWrite();
                let output: RocStr = stderr_write.f0;
                op = unsafe { stderr_write.f1.force_thunk() };

                if let Err(e) = std::io::stderr().write_all(output.as_bytes()) {
                    panic!("Writing to stdout failed! {:?}", e);
                }
            }
            Done => {
                break;
            }
        }
    }

    println!("Done!");

    // Exit code
    0
}
