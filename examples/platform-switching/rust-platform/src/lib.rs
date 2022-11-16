#![allow(non_snake_case)]

mod glue;

use core::ffi::c_void;
use glue::Op;
use roc_std::RocStr;
use std::ffi::CStr;
use std::io::Write;
use std::mem::MaybeUninit;
use std::os::raw::c_char;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed_generic"]
    fn roc_main(_: *mut Op);
}

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
pub unsafe extern "C" fn roc_memcpy(dst: *mut c_void, src: *mut c_void, n: usize) -> *mut c_void {
    libc::memcpy(dst, src, n)
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

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_send_signal(pid: libc::pid_t, sig: libc::c_int) -> libc::c_int {
    libc::kill(pid, sig)
}

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    use glue::discriminant_Op::*;

    let op: Op = unsafe {
        let mut mem = MaybeUninit::uninit();

        roc_main(mem.as_mut_ptr());

        mem.assume_init()
    };

    match op.discriminant() {
        StdoutWrite => {
            let output: RocStr = unsafe { op.get_StdoutWrite_0() };
            // let _next = unsafe { op.get_StdoutWrite_1() };

            if let Err(e) = std::io::stdout().write_all(output.as_bytes()) {
                panic!("Writing to stdout failed! {:?}", e);
            }
        }
        StderrWrite => {
            let output: RocStr = unsafe { op.get_StderrWrite_0() };
            // let _next = unsafe { op.get_StderrWrite_1() };

            if let Err(e) = std::io::stderr().write_all(output.as_bytes()) {
                panic!("Writing to stderr failed! {:?}", e);
            }
        }
        Done => {}
    }

    // Exit code
    0
}
