#![allow(non_snake_case)]

use core::alloc::Layout;
use core::ffi::c_void;
use core::mem::{ManuallyDrop, MaybeUninit};
use libc;
use roc_std::{RocList, RocStr};
use std::env;
use std::ffi::CStr;
use std::fs::File;
use std::io::{BufRead, BufReader, Read, Write};
use std::os::raw::c_char;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed"]
    fn roc_main(args: RocStr, output: *mut u8) -> ();

    #[link_name = "roc__mainForHost_size"]
    fn roc_main_size() -> i64;

    #[link_name = "roc__mainForHost_1_Fx_caller"]
    fn call_Fx(flags: *const u8, closure_data: *const u8, output: *mut u8) -> ();

    #[allow(dead_code)]
    #[link_name = "roc__mainForHost_1_Fx_size"]
    fn size_Fx() -> i64;

    #[link_name = "roc__mainForHost_1_Fx_result_size"]
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

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    let arg = env::args().skip(1).next().unwrap();
    let arg = RocStr::from_slice(arg.as_bytes());

    let size = unsafe { roc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();

    unsafe {
        // TODO allocate on the stack if it's under a certain size
        let buffer = std::alloc::alloc(layout);

        roc_main(arg, buffer);

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
    use std::io::{self, BufRead};

    let stdin = io::stdin();
    let line1 = stdin.lock().lines().next().unwrap().unwrap();

    RocStr::from_slice(line1.as_bytes())
}

#[no_mangle]
pub extern "C" fn roc_fx_getChar() -> u8 {
    use std::io::{self, BufRead};
    let mut buffer = [0];

    if let Err(ioerr) = io::stdin().lock().read_exact(&mut buffer[..]) {
        if ioerr.kind() == io::ErrorKind::UnexpectedEof {
            u8::MAX
        } else {
            panic!("Got an unexpected error while reading char from stdin");
        }
    } else {
        buffer[0]
    }
}

#[no_mangle]
pub extern "C" fn roc_fx_putLine(line: ManuallyDrop<RocStr>) {
    let bytes = line.as_slice();
    let string = unsafe { std::str::from_utf8_unchecked(bytes) };
    println!("{}", string);
    std::io::stdout().lock().flush();
}

#[no_mangle]
pub extern "C" fn roc_fx_putRaw(line: ManuallyDrop<RocStr>) {
    let bytes = line.as_slice();
    let string = unsafe { std::str::from_utf8_unchecked(bytes) };
    print!("{}", string);
    std::io::stdout().lock().flush();
}

#[no_mangle]
pub extern "C" fn roc_fx_getFileLine(br_ptr: *mut BufReader<File>) -> RocStr {
    let br = unsafe { &mut *br_ptr };
    let mut line1 = String::default();

    br.read_line(&mut line1)
        .expect("Failed to read line from file");

    RocStr::from_slice(line1.as_bytes())
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
pub extern "C" fn roc_fx_openFile(name: ManuallyDrop<RocStr>) -> *mut BufReader<File> {
    let f = File::open(name.as_str()).expect("Unable to open file");
    let br = BufReader::new(f);

    Box::into_raw(Box::new(br))
}

#[no_mangle]
pub extern "C" fn roc_fx_withFileOpen(name: ManuallyDrop<RocStr>, buffer: *const u8) {
    // TODO: figure out accepting a closure in an fx and passing data to it.
    // let f = File::open(name.as_str()).expect("Unable to open file");
    // let mut br = BufReader::new(f);

    // unsafe {
    //     let closure_data_ptr = buffer.offset(8);
    //     call_the_closure(closure_data_ptr);
    // }
}
