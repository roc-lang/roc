#![allow(non_snake_case)]

mod test_glue;

use core::ffi::c_void;
use roc_std::RocStr;
use std::ffi::CStr;
use std::io::Write;
use std::os::raw::c_char;
use test_glue::Op;

use test_glue::mainForHost as roc_main;

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
pub unsafe extern "C" fn roc_panic(c_ptr: *mut c_void, tag_id: u32) {
    match tag_id {
        0 => {
            let slice = CStr::from_ptr(c_ptr as *const c_char);
            let string = slice.to_str().unwrap();
            eprintln!("Roc hit a panic: {}", string);
            std::process::exit(1);
        }
        _ => todo!(),
    }
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
    use test_glue::discriminant_Op::*;

    println!("Let's do things!");

    let mut op: Op = roc_main();

    loop {
        match dbg!(op.discriminant()) {
            StdoutWrite => {
                let stdout_write = op.get_StdoutWrite();
                let output: RocStr = stdout_write.f0;
                op = unsafe { stdout_write.f1.force_thunk(()) };

                if let Err(e) = std::io::stdout().write_all(output.as_bytes()) {
                    panic!("Writing to stdout failed! {:?}", e);
                }
            }
            StderrWrite => {
                let stderr_write = op.get_StderrWrite();
                let output: RocStr = stderr_write.f0;
                op = unsafe { stderr_write.f1.force_thunk(()) };

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
