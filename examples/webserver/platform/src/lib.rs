#![allow(non_snake_case)]

use core::alloc::Layout;
use core::ffi::c_void;
use core::mem::MaybeUninit;
use libc;
use roc_std::{RocList, RocStr};
use std::ffi::CStr;
use std::os::raw::c_char;

#[derive(Debug)]
#[repr(C)]
struct RawRocList {
    elements: *mut u8,
    length: usize,
}

#[derive(Debug)]
#[repr(C)]
struct Response {
    flag: i64,
    body: RocStr,
    bytes: [i16; 1],
    something: i16,
}
// { i64, { { [3 x i64] }, i64 } }

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed_generic"]
    fn roc_main(url: RocStr, output: *mut u8) -> ();

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
    server_main();
    return 0;

    let size = unsafe { roc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();

    unsafe {
        // TODO allocate on the stack if it's under a certain size
        let url = RocStr::from_slice(b"/users/3");
        let buffer = std::alloc::alloc(layout);

        roc_main(url, buffer);

        let _ = call_the_closure(buffer);

        std::alloc::dealloc(buffer, layout);
    };

    // Exit code
    0
}

fn roc_handle_request(url: &str) -> (u16, String) {
    let size = unsafe { roc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();

    unsafe {
        // TODO allocate on the stack if it's under a certain size
        let url = RocStr::from_slice(url.as_bytes());
        let closure_data_ptr = std::alloc::alloc(layout);

        roc_main(url, closure_data_ptr);

        let size = size_Fx_result() as usize;
        let layout = Layout::array::<u8>(size).unwrap();
        let mut buffer: MaybeUninit<Response> = MaybeUninit::uninit();

        call_Fx(
            // This flags pointer will never get dereferenced
            MaybeUninit::uninit().as_ptr(),
            closure_data_ptr as *const u8,
            buffer.as_ptr() as *mut u8,
        );

        let response = unsafe { buffer.assume_init() };

        dbg!(&response);
        dbg!(&response.body.as_str());

        std::alloc::dealloc(closure_data_ptr, layout);

        return (200, response.body.as_str().to_string());
    };
}

unsafe fn call_the_closure(closure_data_ptr: *const u8) -> i64 {
    let size = size_Fx_result() as usize;
    let layout = Layout::array::<u8>(size).unwrap();
    let mut buffer: MaybeUninit<Response> = MaybeUninit::uninit();

    call_Fx(
        // This flags pointer will never get dereferenced
        MaybeUninit::uninit().as_ptr(),
        closure_data_ptr as *const u8,
        buffer.as_ptr() as *mut u8,
    );

    let response = unsafe { buffer.assume_init() };

    dbg!(&response);
    dbg!(&response.body.as_str());

    0
}

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

#[repr(C)]
struct Pair<T, U>(T, U);

#[no_mangle]
extern "C" fn roc_fx_readAllUtf8(path: RocStr) -> Pair<RocStr, i32> {
    // TODO use libc to do the operation with minimal overhead and get errno
    let answer = match std::fs::read(path.as_str()) {
        Ok(bytes) => {
            match RocStr::from_utf8(&bytes) {
                Ok(roc_str) => Pair(roc_str, 0),
                Err(_) => {
                    // errno must always be a positive value,
                    // so this negative number indicates a utf-8 problem
                    Pair(RocStr::default(), -1)
                }
            }
        }
        Err(err) => {
            use std::io::ErrorKind::*;

            let errno = match err.kind() {
                PermissionDenied => 1,
                NotFound => 2,
                // TODO others
                _ => i32::MIN, //Unknown
            };

            Pair(RocStr::default(), errno)
        }
    };

    std::mem::forget(path); // The app may still reference this

    answer
}

// Updated example from http://rosettacode.org/wiki/Hello_world/Web_server#Rust
// to work with Rust 1.0 beta

use std::io::{Read, Write};
use std::net::{TcpListener, TcpStream};
use std::thread;

fn handle_read(mut stream: &TcpStream) -> Option<String> {
    let mut buf = [0u8; 4096];
    match stream.read(&mut buf) {
        Ok(_) => {
            let req_str = String::from_utf8_lossy(&buf);
            let line = req_str.lines().nth(0).unwrap();
            let url = line.split_whitespace().nth(1).unwrap();
            Some(url.to_string())
        }
        Err(_) => None,
    }
}

fn handle_write(url: &str, mut stream: TcpStream) {
    let (status, message) = roc_handle_request(url);

    // let response = b"HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n<html><body>Hello world</body></html>\r\n";
    let response = format!("HTTP/1.1 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n<html><body>{}</body></html>\r\n", message);
    match stream.write(response.as_str().as_bytes()) {
        Ok(_) => println!("Response sent"),
        Err(e) => println!("Failed sending response: {}", e),
    }
}

fn handle_client(stream: TcpStream) {
    match handle_read(&stream) {
        Some(url) => handle_write(&url, stream),
        None => println!("Could not read stream"),
    }
}

fn server_main() {
    // very simple HTTP server, taken from https://gist.github.com/mjohnsullivan/e5182707caf0a9dbdf2d
    let listener = TcpListener::bind("127.0.0.1:8080").unwrap();
    println!("Listening for connections on port {}", 8080);

    for stream in listener.incoming() {
        match stream {
            Ok(stream) => {
                thread::spawn(|| handle_client(stream));
            }
            Err(e) => {
                println!("Unable to connect: {}", e);
            }
        }
    }
}
