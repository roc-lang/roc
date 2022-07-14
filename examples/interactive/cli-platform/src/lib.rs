#![allow(non_snake_case)]

mod glue;

use core::alloc::Layout;
use core::ffi::c_void;
use core::mem::MaybeUninit;
use glue::Metadata;
use libc;
use roc_std::{RocList, RocStr};
use std::ffi::CStr;
use std::os::raw::c_char;
use ureq::Error;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed_generic"]
    fn roc_main(output: *mut u8);

    #[link_name = "roc__mainForHost_size"]
    fn roc_main_size() -> i64;

    #[link_name = "roc__mainForHost_1_Fx_caller"]
    fn call_Fx(flags: *const u8, closure_data: *const u8, output: *mut u8);

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

    RocStr::from(line1.as_str())
}

#[no_mangle]
pub extern "C" fn roc_fx_putLine(line: &RocStr) {
    let string = line.as_str();
    println!("{}", string);
}

#[no_mangle]
pub extern "C" fn roc_fx_send_request(roc_request: &glue::Request) -> glue::Response {
    // let (mimetype, body_bytes_slice): (String, Vec<u8>) = match roc_request.body.discriminant() {
    //     glue::discriminant_Body::EmptyBody => ("".into(), vec![]),
    //     glue::discriminant_Body::Body => {
    //         let (mimetype_union, body_roclist) = unsafe { roc_request.body.as_Body() };
    //         let mimetype_string: String = unsafe { mimetype_union.as_MimeType() }.as_str().into();
    //         let body_bytes: &[u8] = body_roclist.as_slice();
    //         (mimetype_string, Vec::from(body_bytes))
    //     }
    // };

    let url = roc_request.url.as_str();
    match ureq::get(url).call() {
        Ok(response) => {
            let statusCode = response.status();

            let mut buffer: Vec<u8> = vec![];
            let mut reader = response.into_reader();
            reader.read(&mut buffer).expect("can't read response");
            let body = RocList::from_slice(&buffer);

            let metadata = Metadata {
                headers: RocList::empty(),
                statusText: RocStr::empty(),
                url: RocStr::empty(),
                statusCode,
            };

            glue::Response::GoodStatus(metadata, body)
        }
        Err(Error::Status(statusCode, response)) => {
            let mut buffer: Vec<u8> = vec![];
            let mut reader = response.into_reader();
            reader.read(&mut buffer).expect("can't read response");
            let body = RocList::from_slice(&buffer);

            let metadata = Metadata {
                headers: RocList::empty(),
                statusText: RocStr::empty(),
                url: RocStr::empty(),
                statusCode,
            };

            glue::Response::BadStatus(metadata, body)
        }
        Err(transortError) => {
            use ureq::ErrorKind::*;
            match transortError.kind() {
                InvalidUrl | UnknownScheme => glue::Response::BadUrl(RocStr::from(url)),
                _ => glue::Response::NetworkError,
            }
        }
    }
}

/*
pub enum Error {
    Status(u16, Response),
    Transport(Transport),
}
match ureq::get("http://mypage.example.com/").call() {
    Ok(response) => { /* it worked */},
    Err(Error::Status(code, response)) => {
        /* the server returned an unexpected status
           code (such as 400, 500 etc) */
    }
    Err(_) => { /* some kind of io/transport error */ }
}

enum ErrorKind {
    InvalidUrl,
    UnknownScheme,
    Dns,
    InsecureRequestHttpsOnly,
    ConnectionFailed,
    TooManyRedirects,
    BadStatus,
    BadHeader,
    Io,
    InvalidProxyUrl,
    ProxyConnect,
    ProxyUnauthorized,
    HTTP,
}
*/
