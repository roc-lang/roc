#![allow(non_snake_case)]

use core::ffi::c_void;
use core::mem::MaybeUninit;
use libc;
use roc_std::{RocList, RocResult, RocStr};
use std::collections::HashMap;
use std::env;
use std::fs::File;
use std::io::{BufRead, BufReader, Read, Write};
use std::sync::atomic::{AtomicU64, Ordering};
use std::sync::{Mutex, OnceLock};

static FILE_ID: AtomicU64 = AtomicU64::new(0);

fn file_handles() -> &'static Mutex<HashMap<u64, BufReader<File>>> {
    static FILE_HANDLES: OnceLock<Mutex<HashMap<u64, BufReader<File>>>> = OnceLock::new();

    FILE_HANDLES.get_or_init(|| Mutex::new(HashMap::default()))
}

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed_generic"]
    fn roc_main(output: *mut u8, args: &RocStr);

    #[link_name = "roc__mainForHost_1_exposed_size"]
    fn roc_main_size() -> i64;

    #[link_name = "roc__mainForHost_0_caller"]
    fn call_Fx(flags: *const u8, closure_data: *const u8, output: *mut u8);

    #[link_name = "roc__mainForHost_0_size"]
    fn size_Fx() -> i64;

    #[link_name = "roc__mainForHost_0_result_size"]
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
    let arg = env::args()
        .nth(1)
        .expect("Please pass a .false file as a command-line argument to the false interpreter!");
    let arg = RocStr::from(arg.as_str());

    let size = unsafe { roc_main_size() } as usize;

    unsafe {
        let buffer = roc_alloc(size, 1) as *mut u8;

        roc_main(buffer, &arg);

        // arg has been passed to roc now, and it assumes ownership.
        // so we must not touch its refcount now
        std::mem::forget(arg);

        let result = call_the_closure(buffer);

        roc_dealloc(buffer as _, 1);

        result
    };

    // Exit code
    0
}

unsafe fn call_the_closure(closure_data_ptr: *const u8) -> i64 {
    let size = size_Fx_result() as usize;
    let buffer = roc_alloc(size, 1) as *mut u8;

    call_Fx(
        // This flags pointer will never get dereferenced
        MaybeUninit::uninit().as_ptr(),
        closure_data_ptr as *const u8,
        buffer as *mut u8,
    );

    roc_dealloc(buffer as _, 1);
    0
}

#[no_mangle]
pub extern "C" fn roc_fx_getLine() -> RocResult<RocStr, ()> {
    let stdin = std::io::stdin();
    let line1 = stdin.lock().lines().next().unwrap().unwrap();

    RocResult::ok(RocStr::from(line1.as_str()))
}

#[no_mangle]
pub extern "C" fn roc_fx_getChar() -> RocResult<u8, ()> {
    let mut buffer = [0];

    if let Err(ioerr) = std::io::stdin().lock().read_exact(&mut buffer[..]) {
        if ioerr.kind() == std::io::ErrorKind::UnexpectedEof {
            RocResult::ok(u8::MAX)
        } else {
            panic!("Got an unexpected error while reading char from stdin");
        }
    } else {
        RocResult::ok(buffer[0])
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_putLine(line: &RocStr) -> RocResult<(), ()> {
    let string = line.as_str();
    println!("{}", string);
    let _ = std::io::stdout().lock().flush();

    RocResult::ok(())
}

#[no_mangle]
pub extern "C" fn roc_fx_putRaw(line: &RocStr) -> RocResult<(), ()> {
    let string = line.as_str();
    print!("{}", string);
    let _ = std::io::stdout().lock().flush();

    RocResult::ok(())
}

#[no_mangle]
pub extern "C" fn roc_fx_getFileLine(br_id: u64) -> RocResult<RocStr, ()> {
    let mut br_map = file_handles().lock().unwrap();
    let br = br_map.get_mut(&br_id).unwrap();
    let mut line1 = String::default();

    br.read_line(&mut line1)
        .expect("Failed to read line from file");

    RocResult::ok(RocStr::from(line1.as_str()))
}

#[no_mangle]
pub extern "C" fn roc_fx_getFileBytes(br_id: u64) -> RocResult<RocList<u8>, ()> {
    let mut br_map = file_handles().lock().unwrap();
    let br = br_map.get_mut(&br_id).unwrap();
    let mut buffer = [0; 0x10 /* This is intentionally small to ensure correct implementation */];

    let count = br
        .read(&mut buffer[..])
        .expect("Failed to read bytes from file");

    RocResult::ok(RocList::from_slice(&buffer[..count]))
}

#[no_mangle]
pub extern "C" fn roc_fx_closeFile(br_id: u64) -> RocResult<(), ()> {
    file_handles().lock().unwrap().remove(&br_id);

    RocResult::ok(())
}

#[no_mangle]
pub extern "C" fn roc_fx_openFile(name: &RocStr) -> RocResult<u64, ()> {
    let string = name.as_str();
    match File::open(string) {
        Ok(f) => {
            let br = BufReader::new(f);
            let br_id = FILE_ID.fetch_add(1, Ordering::SeqCst);

            file_handles().lock().unwrap().insert(br_id, br);

            RocResult::ok(br_id)
        }
        Err(_) => {
            panic!(
                "false interpreter platform crashed, unable to open file {:?}",
                name
            )
        }
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_withFileOpen(_name: &RocStr, _buffer: *const u8) -> RocResult<(), ()> {
    // TODO: figure out accepting a closure in an fx and passing data to it.
    // let f = File::open(name.as_str()).expect("Unable to open file");
    // let mut br = BufReader::new(f);

    // unsafe {
    //     let closure_data_ptr = buffer.offset(8);
    //     call_the_closure(closure_data_ptr);
    // }

    RocResult::ok(())
}
