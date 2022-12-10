#![allow(non_snake_case)]

use core::alloc::Layout;
use core::ffi::c_void;
use core::mem::MaybeUninit;
use libc;
use roc_std::{RocList, RocStr};
use std::env;
use std::ffi::CStr;
use std::fs::File;
use std::io::{BufRead, BufReader, Read, Write};
use std::os::raw::c_char;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed_generic"]
    fn roc_main(output: *mut u8, args: &RocStr);

    #[link_name = "roc__mainForHost_size"]
    fn roc_main_size() -> i64;

    #[link_name = "roc__mainForHost_1__Fx_caller"]
    fn call_Fx(flags: *const u8, closure_data: *const u8, output: *mut u8);

    #[allow(dead_code)]
    #[link_name = "roc__mainForHost_1__Fx_size"]
    fn size_Fx() -> i64;

    #[link_name = "roc__mainForHost_1__Fx_result_size"]
    fn size_Fx_result() -> i64;
}

#[no_mangle]
pub unsafe extern "C" fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    libc::malloc(size)
}

#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    libc::realloc(c_ptr, new_size)
}

#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    libc::free(c_ptr)
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

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    let arg = env::args()
        .nth(1)
        .expect("Please pass a .false file as a command-line argument to the false interpreter!");
    let arg = RocStr::from(arg.as_str());

    let size = unsafe { roc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();

    unsafe {
        // TODO allocate on the stack if it's under a certain size
        let buffer = std::alloc::alloc(layout);

        roc_main(buffer, &arg);

        // arg has been passed to roc now, and it assumes ownership.
        // so we must not touch its refcount now
        std::mem::forget(arg);

        let result = call_the_closure(buffer);

        std::alloc::dealloc(buffer, layout);

        result
    };

    // Exit code
    0
}

unsafe fn call_the_closure(closure_data_ptr: *const u8) -> i64 {
    let size = size_Fx_result() as usize;
    let layout = Layout::array::<u8>(size).unwrap();
    let buffer = std::alloc::alloc(layout) as *mut u8;

    call_Fx(
        // This flags pointer will never get dereferenced
        MaybeUninit::uninit().as_ptr(),
        closure_data_ptr as *const u8,
        buffer as *mut u8,
    );

    std::alloc::dealloc(buffer, layout);

    0
}

#[no_mangle]
pub extern "C" fn roc_fx_getLine() -> RocStr {
    let stdin = std::io::stdin();
    let line1 = stdin.lock().lines().next().unwrap().unwrap();

    RocStr::from(line1.as_str())
}

#[no_mangle]
pub extern "C" fn roc_fx_getChar() -> u8 {
    let mut buffer = [0];

    if let Err(ioerr) = std::io::stdin().lock().read_exact(&mut buffer[..]) {
        if ioerr.kind() == std::io::ErrorKind::UnexpectedEof {
            u8::MAX
        } else {
            panic!("Got an unexpected error while reading char from stdin");
        }
    } else {
        buffer[0]
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_putLine(line: &RocStr) {
    let string = line.as_str();
    println!("{}", string);
    std::io::stdout().lock().flush();
}

#[no_mangle]
pub extern "C" fn roc_fx_putRaw(line: &RocStr) {
    let string = line.as_str();
    print!("{}", string);
    std::io::stdout().lock().flush();
}

#[no_mangle]
pub extern "C" fn roc_fx_getFileLine(br_ptr: *mut BufReader<File>) -> RocStr {
    let br = unsafe { &mut *br_ptr };
    let mut line1 = String::default();

    br.read_line(&mut line1)
        .expect("Failed to read line from file");

    RocStr::from(line1.as_str())
}

#[no_mangle]
pub extern "C" fn roc_fx_getFileBytes(br_ptr: *mut BufReader<File>) -> RocList<u8> {
    let br = unsafe { &mut *br_ptr };
    let mut buffer = [0; 0x10 /* This is intentionally small to ensure correct implementation */];

    let count = br
        .read(&mut buffer[..])
        .expect("Failed to read bytes from file");

    RocList::from_slice(&buffer[..count])
}

#[no_mangle]
pub extern "C" fn roc_fx_closeFile(br_ptr: *mut BufReader<File>) {
    unsafe {
        Box::from_raw(br_ptr);
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_openFile(name: &RocStr) -> *mut BufReader<File> {
    let string = name.as_str();
    match File::open(string) {
        Ok(f) => {
            let br = BufReader::new(f);

            Box::into_raw(Box::new(br))
        }
        Err(_) => {
            panic!("unable to open file {:?}", name)
        }
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_withFileOpen(name: &RocStr, buffer: *const u8) {
    // TODO: figure out accepting a closure in an fx and passing data to it.
    // let f = File::open(name.as_str()).expect("Unable to open file");
    // let mut br = BufReader::new(f);

    // unsafe {
    //     let closure_data_ptr = buffer.offset(8);
    //     call_the_closure(closure_data_ptr);
    // }
}
