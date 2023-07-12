use futures::{Future, FutureExt};
use hyper::{Body, Request, Response, Server, StatusCode};
use roc_app;
use roc_std::RocStr;
use std::cell::RefCell;
use std::convert::Infallible;
use std::net::SocketAddr;
use std::os::raw::{c_int, c_long, c_void};
use std::panic::AssertUnwindSafe;
use tokio::task::spawn_blocking;
use libc::{sigaction, siginfo_t, sigemptyset, SIGBUS, SIGFPE, SIGILL, SIGSEGV, sighandler_t, sigset_t, SA_SIGINFO, SIG_DFL};

const DEFAULT_PORT: u16 = 8000;

// If we have a roc_panic or a segfault, these will be used to record where to jump back to
// (a point at which we can return a different response).
thread_local! {
    // 64 is the biggest jmp_buf in setjmp.h
    static SETJMP_ENV: RefCell<[c_long; 64]> = RefCell::new([0 as c_long; 64]);
    static ROC_CRASH_MSG: RefCell<RocStr> = RefCell::new(RocStr::empty());
    static SIGNAL_CAUGHT: RefCell<c_int> = RefCell::new(0);
}

extern "C" {
    #[link_name = "setjmp"]
    pub fn setjmp(env: *mut c_void) -> c_int;

    #[link_name = "longjmp"]
    pub fn longjmp(env: *mut c_void, val: c_int);
}

unsafe extern "C" fn signal_handler(sig: c_int, _: *mut siginfo_t, _: *mut libc::c_void) {
    SIGNAL_CAUGHT.with(|val| {
        *val.borrow_mut() = sig;
    });

    SETJMP_ENV.with(|env| {
        longjmp(env.borrow_mut().as_mut_ptr().cast(), 1);
    });
}

fn setup_signal(sig: c_int) {
    let sa = libc::sigaction {
        sa_sigaction: signal_handler as sighandler_t,
        sa_mask: sigset_t::default(),
        sa_flags: SA_SIGINFO,
    };

    let mut old_sa = libc::sigaction {
        sa_sigaction: SIG_DFL,
        sa_mask: sigset_t::default(),
        sa_flags: 0,
    };

    unsafe {
        sigemptyset(&mut old_sa.sa_mask as *mut sigset_t);
        sigaction(sig, &sa, &mut old_sa);
    }
}

fn call_roc(req_bytes: &[u8]) -> Response<Body> {
    let mut setjmp_result = 0;

    SETJMP_ENV.with(|env| {
        setjmp_result = unsafe { setjmp(env.borrow_mut().as_mut_ptr().cast()) };
    });

    if setjmp_result == 0 {
        setup_signal(SIGSEGV);
        setup_signal(SIGILL);
        setup_signal(SIGFPE);
        setup_signal(SIGBUS);

        let req_str: &str = std::str::from_utf8(req_bytes).unwrap(); // TODO don't unwrap
        let resp: String = roc_app::mainForHost(req_str.into()).as_str().into();

        Response::builder()
            .status(StatusCode::OK) // TODO get status code from Roc too
            .body(Body::from(resp))
            .unwrap() // TODO don't unwrap() here
    } else {
        let mut crash_msg: String = String::new();
        let mut sig: c_int = 0;

        SIGNAL_CAUGHT.with(|val| {
            sig = *val.borrow();
        });

        if sig == 0 {
            ROC_CRASH_MSG.with(|env| {
                crash_msg = env.borrow().as_str().into();
            });
        } else {
            crash_msg = "Roc crashed with signal {sig}".into(); // TODO print the name of the signal
        }

        Response::builder()
            .status(StatusCode::INTERNAL_SERVER_ERROR)
            .body(Body::from(crash_msg))
            .unwrap() // TODO don't unwrap() here
    }
}

async fn handle_req(req: Request<Body>) -> Response<Body> {
    match hyper::body::to_bytes(req.into_body()).await {
        Ok(req_body) => {
            spawn_blocking(move || call_roc(&req_body))
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
        Ok::<_, Infallible>(hyper::service::service_fn(|req| handle_panics(handle_req(req))))
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
    match tokio::runtime::Builder::new_multi_thread()
        .enable_all()
        .build()
    {
        Ok(runtime) => runtime.block_on(async { run_server(DEFAULT_PORT).await }),
        Err(err) => {
            eprintln!("Error initializing tokio multithreaded runtime: {}", err); // TODO improve this

            1
        }
    }
}

// Externs required by roc_std and by the Roc app

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
pub unsafe extern "C" fn roc_panic(msg: RocStr) {
    // Set the last caught signal to 0, so we don't mistake this for a signal.
    SIGNAL_CAUGHT.with(|val| {
        *val.borrow_mut() = 0;
    });

    ROC_CRASH_MSG.with(|val| {
        *val.borrow_mut() = msg;
    });

    SETJMP_ENV.with(|env| {
        longjmp(env.borrow_mut().as_mut_ptr().cast(), 1);
    });
}

#[no_mangle]
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    libc::memset(dst, c, n)
}
