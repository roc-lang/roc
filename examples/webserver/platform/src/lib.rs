#![allow(non_snake_case)]

use core::alloc::Layout;
use core::ffi::c_void;
use core::mem::MaybeUninit;
use libc;
use roc_std::{RocList, RocStr};
use std::ffi::CStr;
use std::os::raw::c_char;

extern "C" {
    #[link_name = "roc__routeHandlers_1_exposed"]
    fn roc_main() -> RocList<u8>;

    #[link_name = "roc__routeHandlers_size"]
    fn roc_main_size() -> i64;

    // #[link_name = "roc__mainForHost_1_Fx_caller"]
    // fn call_Fx(flags: *const u8, closure_data: *const u8, output: *mut u8) -> ();

    // #[allow(dead_code)]
    // #[link_name = "roc__mainForHost_1_Fx_size"]
    // fn size_Fx() -> i64;

    // #[link_name = "roc__mainForHost_1_Fx_result_size"]
    // fn size_Fx_result() -> i64;
}

#[no_mangle]
pub unsafe fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    libc::malloc(size)
}

#[no_mangle]
pub unsafe fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    libc::realloc(c_ptr, new_size)
}

#[no_mangle]
pub unsafe fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    libc::free(c_ptr)
}

#[no_mangle]
pub unsafe fn roc_panic(c_ptr: *mut c_void, tag_id: u32) {
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
pub fn rust_main() -> isize {
    let size = unsafe { roc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();

    unsafe {
        // TODO allocate on the stack if it's under a certain size

        let handlers = roc_main();
        dbg!(handlers);

        // let result = call_the_closure(output);
        // let output = buffer as *mut u8;

        // let buffer = std::alloc::alloc(layout);
        // std::alloc::dealloc(buffer, layout);
    };

    // Exit code
    0
}

// unsafe fn call_the_closure(closure_data_ptr: *const u8) -> i64 {
//     let size = size_Fx_result() as usize;
//     let layout = Layout::array::<u8>(size).unwrap();
//     let buffer = std::alloc::alloc(layout) as *mut u8;
//
//     call_Fx(
//         // This flags pointer will never get dereferenced
//         MaybeUninit::uninit().as_ptr(),
//         closure_data_ptr as *const u8,
//         buffer as *mut u8,
//     );
//
//     std::alloc::dealloc(buffer, layout);
//     0
// }

#[no_mangle]
pub fn roc_fx_getLine() -> RocStr {
    use std::io::{self, BufRead};

    let stdin = io::stdin();
    let line1 = stdin.lock().lines().next().unwrap().unwrap();

    RocStr::from_slice(line1.as_bytes())
}

#[no_mangle]
pub fn roc_fx_putLine(line: RocStr) -> () {
    let bytes = line.as_slice();
    let string = unsafe { std::str::from_utf8_unchecked(bytes) };
    println!("{}", string);

    // don't mess with the refcount!
    core::mem::forget(line);

    ()
}

#[no_mangle]
pub fn roc_fx_errLine(line: RocStr) -> () {
    let bytes = line.as_slice();
    let string = unsafe { std::str::from_utf8_unchecked(bytes) };
    eprintln!("{}", string);

    // don't mess with the refcount!
    core::mem::forget(line);

    ()
}

#[no_mangle]
pub fn roc_fx_httpGetUtf8(url: RocStr) -> (RocStr, u16) {
    eprintln!(
        "TODO implement Http.get in the host for URL {}",
        url.as_str()
    );

    std::process::exit(1);
    // match ureq::get(unsafe { url.as_str() }).call() {
    //     Ok(resp) => match resp.into_string() {
    //         Ok(contents) => RocResult::Ok(RocStr::from_slice(contents.as_bytes())), // TODO make roc::Result!
    //         // TODO turn this error into an enum!
    //         Err(err) => RocResult::Err(RocStr::from_slice(format!("{:?}", err).as_bytes())),
    //     },
    //     // TODO turn this error into an enum!
    //     Err(err) => RocResult::Err(RocStr::from_slice(format!("{:?}", err).as_bytes())),
    // }
}

#[no_mangle]
pub fn roc_fx_writeAllUtf8(path: RocStr, _contents: RocStr) -> (RocStr, u16) {
    eprintln!(
        "TODO implement File.writeUtf8 in the host for path {}",
        path.as_str()
    );

    std::process::exit(1);
    // match ureq::get(unsafe { url.as_str() }).call() {
    //     Ok(resp) => match resp.into_string() {
    //         Ok(contents) => RocResult::Ok(RocStr::from_slice(contents.as_bytes())), // TODO make roc::Result!
    //         // TODO turn this error into an enum!
    //         Err(err) => RocResult::Err(RocStr::from_slice(format!("{:?}", err).as_bytes())),
    //     },
    //     // TODO turn this error into an enum!
    //     Err(err) => RocResult::Err(RocStr::from_slice(format!("{:?}", err).as_bytes())),
    // }
}

#[no_mangle]
pub fn roc_fx_readAllUtf8(path: RocStr) -> (RocStr, u16) {
    eprintln!(
        "TODO implement File.readUtf8 in the host for path {}",
        path.as_str()
    );

    std::process::exit(1);
    // match ureq::get(unsafe { url.as_str() }).call() {
    //     Ok(resp) => match resp.into_string() {
    //         Ok(contents) => RocResult::Ok(RocStr::from_slice(contents.as_bytes())), // TODO make roc::Result!
    //         // TODO turn this error into an enum!
    //         Err(err) => RocResult::Err(RocStr::from_slice(format!("{:?}", err).as_bytes())),
    //     },
    //     // TODO turn this error into an enum!
    //     Err(err) => RocResult::Err(RocStr::from_slice(format!("{:?}", err).as_bytes())),
    // }
}
