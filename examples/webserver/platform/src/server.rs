use crate::signal_handling::install_signal_handlers;
use futures::{Future, FutureExt};
use hyper::{Body, Request, Response, Server, StatusCode};
use roc_app;
use std::convert::Infallible;
use std::net::SocketAddr;
use std::panic::AssertUnwindSafe;
use tokio::task::spawn_blocking;

const DEFAULT_PORT: u16 = 8000;

pub fn start() -> i32 {
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

fn call_roc(url: &str) -> Response<Body> {
    install_signal_handlers();

    let resp: String = roc_app::main(url.into()).as_str().to_string();

    Response::builder()
        .status(StatusCode::OK) // TODO get status code from Roc too
        .body(Body::from(resp))
        .unwrap() // TODO don't unwrap() here
}

async fn handle_req(req: Request<Body>) -> Response<Body> {
    let url: String = req.uri().to_string();

    spawn_blocking(move || call_roc(&url))
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

const LOCALHOST: [u8; 4] = [0, 0, 0, 0];

async fn run_server(port: u16) -> i32 {
    let addr = SocketAddr::from((LOCALHOST, port));
    let server = Server::bind(&addr).serve(hyper::service::make_service_fn(|_conn| async {
        Ok::<_, Infallible>(hyper::service::service_fn(|req| {
            handle_panics(handle_req(req))
        }))
    }));

    println!("Listening on 0.0.0.0 port {port}");

    match server.await {
        Ok(_) => 0,
        Err(err) => {
            eprintln!("Error initializing Rust `hyper` server: {}", err); // TODO improve this

            1
        }
    }
}
