#![allow(non_snake_case)]

use core::ffi::c_void;
use libc;
use roc_std::{RocList, RocStr};
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
    #[link_name = "roc__main_for_host_1_exposed_generic"]
    fn roc_main(void: *const c_void, args: *mut RocStr);
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
    let mut arg = RocStr::from(arg.as_str());

    unsafe { roc_main(std::ptr::null(), &mut arg) };
    std::mem::forget(arg);

    // This really shouldn't need to be freed, but valgrid is picky about possibly lost.
    *file_handles().lock().unwrap() = HashMap::default();

    // Exit code
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
    let _ = std::io::stdout().lock().flush();
}

#[no_mangle]
pub extern "C" fn roc_fx_putRaw(line: &RocStr) {
    let string = line.as_str();
    print!("{}", string);
    let _ = std::io::stdout().lock().flush();
}

#[no_mangle]
pub extern "C" fn roc_fx_getFileLine(br_id: u64) -> RocStr {
    let mut br_map = file_handles().lock().unwrap();
    let br = br_map.get_mut(&br_id).unwrap();
    let mut line1 = String::default();

    br.read_line(&mut line1)
        .expect("Failed to read line from file");

    RocStr::from(line1.as_str())
}

#[no_mangle]
pub extern "C" fn roc_fx_getFileBytes(br_id: u64) -> RocList<u8> {
    let mut br_map = file_handles().lock().unwrap();
    let br = br_map.get_mut(&br_id).unwrap();
    let mut buffer = [0; 0x10 /* This is intentionally small to ensure correct implementation */];

    let count = br
        .read(&mut buffer[..])
        .expect("Failed to read bytes from file");

    RocList::from_slice(&buffer[..count])
}

#[no_mangle]
pub extern "C" fn roc_fx_closeFile(br_id: u64) {
    file_handles().lock().unwrap().remove(&br_id);
}

#[no_mangle]
pub extern "C" fn roc_fx_openFile(name: &RocStr) -> u64 {
    let string = name.as_str();
    match File::open(string) {
        Ok(f) => {
            let br = BufReader::new(f);
            let br_id = FILE_ID.fetch_add(1, Ordering::SeqCst);

            file_handles().lock().unwrap().insert(br_id, br);

            br_id
        }
        Err(_) => {
            panic!(
                "false interpreter platform crashed, unable to open file {:?}",
                name
            )
        }
    }
}
