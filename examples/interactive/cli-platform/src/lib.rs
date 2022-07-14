#![allow(non_snake_case)]

mod glue;

use core::alloc::Layout;
use core::ffi::c_void;
use core::mem::{ManuallyDrop, MaybeUninit};
use libc;
use reqwest::{Client, Method};
use roc_std::{RocList, RocResult, RocStr};
use std::ffi::CStr;
use std::os::raw::c_char;

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

// We just provide just one Client to the Roc program for simplicity, so it must be static in the host.
// `thread_local!` avoids contention between threads, should that ever arise
// `lazy_static` & `Mutex` might have easier-to-understand semantics for resources like cookies
use std::cell::RefCell;
std::thread_local! {
    static HTTP_CLIENT: RefCell<Option<Client>> = RefCell::new(None);
}

#[no_mangle]
pub extern "C" fn roc_fx_send_request(roc_request: &glue::Request, /* should I borrow or not? */) {
    HTTP_CLIENT.with(|refcell| {
        let client = match refcell.take() {
            Some(c) => c,
            None => {
                // Lazily create the client, the first time the Roc program decides to send a request
                let builder = Client::builder();
                let c = builder.build().expect("Failed to create HTTP client");
                refcell.replace(Some(c.clone())); // cheap to clone, has internal refcount
                c
            }
        };

        let (mimetype, body_bytes_slice): (String, Vec<u8>) = match roc_request.body.discriminant() {
            glue::discriminant_Body::EmptyBody => ("".into(), vec![]),
            glue::discriminant_Body::Body => {
                let (mimetype_union, body_roclist) = unsafe { roc_request.body.as_Body() };
                let mimetype_string: String = unsafe { mimetype_union.as_MimeType() }.as_str().into();
                let body_bytes: &[u8] = body_roclist.as_slice();
                (mimetype_string, Vec::from(body_bytes))
            }
        };

        let url = match reqwest::Url::parse(roc_request.url.as_str()) {
            Ok(u) => u,
            Err(_) => todo!("return a Future that immediately resolves to BadUrl"),
        };

        let request = client
            .request(Method::GET /* TODO */, url)
            .body(Vec::from(body_bytes_slice))
            .build();


        // let rust_body_bytes: Vec<u8> = vec![]; // reqwest something something
        // let roc_body_bytes = RocList::from_slice(&rust_body_bytes);

        // let roc_response: glue::Response = todo!();

        // call_the_closure
    });
}
