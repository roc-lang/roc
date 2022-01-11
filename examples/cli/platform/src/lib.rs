#![allow(non_snake_case)]

use core::alloc::Layout;
use core::ffi::c_void;
use core::mem::{ManuallyDrop, MaybeUninit};
use libc;
use roc_std::{RocList, RocResult, RocStr};
use std::ffi::CStr;
use std::fs::File;
use std::io::{self, BufReader, Read};
use std::os::raw::c_char;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed"]
    fn roc_main(output: *mut u8) -> ();

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
    let size = unsafe { roc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();

    unsafe {
        // TODO allocate on the stack if it's under a certain size
        let buffer = std::alloc::alloc(layout);

        roc_main(buffer);

        let result = call_the_closure(buffer);

        std::alloc::dealloc(buffer, layout);

        result
    };

    // Exit code
    0
}

unsafe extern "C" fn call_the_closure(closure_data_ptr: *const u8) -> i64 {
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
    use std::io::BufRead;

    let stdin = io::stdin();
    let line1 = stdin.lock().lines().next().unwrap().unwrap();

    RocStr::from_slice(line1.as_bytes())
}

#[no_mangle]
pub extern "C" fn roc_fx_putLine(line: ManuallyDrop<RocStr>) {
    let bytes = line.as_slice();
    let string = unsafe { std::str::from_utf8_unchecked(bytes) };
    println!("{}", string);
}

#[repr(C)]
pub struct ReadErr {
    path: RocStr,
    // errno: i32, // needed once OpenErr is in the mix
    tag: ReadErrTag,
}

#[repr(u64)] // TODO this will be a u8 after Ayaz's PR merges
pub enum ReadErrTag {
    ////// THESE MUST BE ALPHABETIZED!!! //////
    FileBusy = 0,
    FileWasDir,
    IllegalByteSequence,
    InvalidSeek,
}

#[no_mangle]
pub extern "C" fn roc_fx_readAllBytes(
    path: ManuallyDrop<RocStr>,
) -> RocResult<RocList<u8>, ReadErr> {
    println!("in roc_fx_readAllBytes({})", path.as_str());
    let result = read_bytes(path.as_str());

    match result {
        Ok(list) => {
            println!("Read this list of bytes: {:?}", list);

            RocResult::ok(list)
        }
        Err(err) => {
            println!("Error reading file: {:?}", err);

            // TODO give a more helpful error
            let tag = ReadErrTag::FileBusy;
            let path = "TODO roc read result error".into();

            RocResult::err(ReadErr {
                path,
                // errno: i32, // needed once OpenErr is in the mix
                tag,
            })
        }
    }
}

fn read_bytes(path: &str) -> io::Result<RocList<u8>> {
    let file = File::open(path)?;
    let mut reader = BufReader::new(file);

    // TODO use a RocList as the buffer directly
    let mut buf = Vec::new();

    reader.read_to_end(&mut buf)?;

    Ok(buf.as_slice().into())
}
