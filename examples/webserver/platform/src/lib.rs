use futures::{Future, FutureExt};
use hyper::{Body, Request, Response, Server, StatusCode};
use std::convert::Infallible;
use std::net::SocketAddr;
use std::panic::AssertUnwindSafe;
use tokio::task::spawn_blocking;
use roc_app;

const DEFAULT_PORT: u16 = 8000;

fn call_roc(req_bytes: &[u8]) -> (StatusCode, Vec<u8>) {
    // TODO setjmp (both for signal handlers and for roc_panic, bc calling Rust panics from FFI code is UB)
    // TODO install signal handlers
    // TODO convert roc_bytes to RocList<u8>, call roc_mainForHost, and convert from its RocList<u8> response
    let req_str: &str = std::str::from_utf8(req_bytes).unwrap(); // TODO don't unwrap

    (StatusCode::OK, roc_app::mainForHost(req_str.into()).as_str().into())
}

async fn handle(req: Request<Body>) -> Response<Body> {
    match hyper::body::to_bytes(req.into_body()).await {
        Ok(req_body) => {
            spawn_blocking(move || {
                let (status_code, resp_bytes) = call_roc(&req_body);

                Response::builder()
                    .status(status_code) // TODO get status code from Roc too
                    .body(Body::from(resp_bytes))
                    .unwrap() // TODO don't unwrap() here
            })
            .then(|resp| async {
                resp.unwrap() // TODO don't unwrap here
            })
            .await
        }
        Err(_) => todo!(), // TODO
    }
}

/// Translate Rust panics in the given Future into 500 errors
async fn handle_panics(
    fut: impl Future<Output = Response<Body>>,
) -> Result<Response<Body>, Infallible> {
    match AssertUnwindSafe(fut).catch_unwind().await {
        Ok(response) => Ok(response),
        Err(_panic) => {
            let error = Response::builder()
                .status(StatusCode::INTERNAL_SERVER_ERROR)
                .body("Panic detected!".into())
                .unwrap(); // TODO don't unwrap here

            Ok(error)
        }
    }
}

const LOCALHOST: [u8; 4] = [127, 0, 0, 1];

async fn run_server(port: u16) -> i32 {
    let addr = SocketAddr::from((LOCALHOST, port));
    let server = Server::bind(&addr).serve(hyper::service::make_service_fn(|_conn| async {
        Ok::<_, Infallible>(hyper::service::service_fn(|req| handle_panics(handle(req))))
    }));

    println!("Listening on <http://localhost:{port}>");

    match server.await {
        Ok(_) => 0,
        Err(err) => {
            eprintln!("Error initializing Rust `hyper` server: {}", err); // TODO improve this

            1
        }
    }
}

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    match tokio::runtime::Builder::new_multi_thread().enable_all().build() {
        Ok(runtime) => runtime.block_on(async { run_server(DEFAULT_PORT).await }),
        Err(err) => {
            eprintln!("Error initializing tokio multithreaded runtime: {}", err); // TODO improve this

            1
        }
    }
}

// Externs required by roc_std and by the Roc app

use core::ffi::c_void;
use std::ffi::CStr;
use std::os::raw::c_char;

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
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    libc::memset(dst, c, n)
}
