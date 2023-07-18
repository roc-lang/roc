use crate::signal_handling::{
    install_signal_handlers, setjmp, ROC_CRASH_MSG, SETJMP_ENV, SIGNAL_CAUGHT,
};
use futures::{Future, FutureExt};
use hyper::{Body, Request, Response, Server, StatusCode};
use roc_app;
use roc_std;
use std::convert::Infallible;
use std::net::SocketAddr;
use std::os::raw::c_int;
use std::panic::AssertUnwindSafe;
use tokio::task::spawn_blocking;

const DEFAULT_PORT: u16 = 8000;

pub fn start() -> i32 {
    match tokio::runtime::Builder::new_current_thread()
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

fn call_roc(headers: Vec<(String, String)>) -> Response<Body> {
    let mut setjmp_result = 0;

    SETJMP_ENV.with(|env| {
        setjmp_result = unsafe { setjmp(env.borrow_mut().as_mut_ptr().cast()) };
    });

    if setjmp_result == 0 {
        install_signal_handlers();
        eprintln!("\nCalling roc function to handle request...");

        let req_header_str: roc_std::RocStr = headers
            .iter()
            .map(|(name, val)| format!("{name}: {val}, "))
            .collect::<String>()
            .as_str()
            .into();

        let resp: String = roc_app::main(&req_header_str).as_str().to_string();

        // TODO: do something like this once roc_app::main returns a RocStr
        // let resp: String = roc_app::main().as_str().into();

        let _ = req_header_str; // Extend lifetime of req_header_str just in case

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
    let headers: Vec<(String, String)> = req
        .headers()
        .iter()
        .map(|(name, val)| {
            (
                name.as_str().to_string(),
                val.to_str()
                    .unwrap_or("<non-ASCII header value>") // TODO handle this better
                    .to_string(),
            )
        })
        .collect();

    spawn_blocking(move || call_roc(headers))
        .then(|resp| async {
            resp.unwrap() // TODO don't unwrap here
        })
        .await
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
        Ok::<_, Infallible>(hyper::service::service_fn(|req| {
            handle_panics(handle_req(req))
        }))
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
