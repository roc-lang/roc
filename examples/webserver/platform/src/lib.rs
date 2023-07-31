use roc_fn::roc_fn;
use roc_std::{RocResult, RocStr};
use std::cell::RefCell;
use std::os::raw::c_void;

mod http_client;
mod request_handling;
mod server;
mod signal_handling;

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    server::start()
}

// Externs required by roc_std and by the Roc app

#[no_mangle]
pub unsafe extern "C" fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    let c_ptr = libc::malloc(size);
    // eprintln!("+0x{:x} <- alloc({size})", c_ptr as usize);

    return c_ptr;
}

#[no_mangle]
pub unsafe extern "C" fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    let answer_ptr = libc::realloc(c_ptr, new_size);
    // eprintln!(
    //     "~Ox{:x} <- realloc(0x{:x}, {new_size})",
    //     answer_ptr as usize, c_ptr as usize,
    // );
    return answer_ptr;
}

#[no_mangle]
pub unsafe extern "C" fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    // eprintln!("Here is the call stack that led to the crash:\n");

    // let mut entries = Vec::new();

    // #[derive(Default)]
    // struct Entry {
    //     pub fn_name: String,
    //     pub filename: Option<String>,
    //     pub line: Option<u32>,
    //     pub col: Option<u32>,
    // }

    // backtrace::trace(|frame| {
    //     backtrace::resolve_frame(frame, |symbol| {
    //         if let Some(fn_name) = symbol.name() {
    //             let fn_name = fn_name.to_string();

    //             let mut entry: Entry = Default::default();

    //             entry.fn_name = fn_name;

    //             if let Some(path) = symbol.filename() {
    //                 entry.filename = Some(path.to_string_lossy().into_owned());
    //             };

    //             entry.line = symbol.lineno();
    //             entry.col = symbol.colno();

    //             entries.push(entry);
    //         } else {
    //             entries.push(Entry {
    //                 fn_name: "???".to_string(),
    //                 ..Default::default()
    //             });
    //         }
    //     });

    //     true // keep going to the next frame
    // });

    // for entry in entries {
    //     eprintln!("\t{}", entry.fn_name);

    //     if let Some(filename) = entry.filename {
    //         eprintln!("\t\t{filename}");
    //     }
    // }

    // eprintln!("-0x{:x} <- dealloc", c_ptr as usize);
    // libc::free(c_ptr);
    // eprintln!("Done calling libc::free()");
}

thread_local! {
    static ROC_CRASH_MSG: RefCell<RocStr> = RefCell::new(RocStr::empty());
}

#[no_mangle]
pub unsafe extern "C" fn roc_panic(msg: RocStr) {
    panic!("The Roc app crashed with: {}", msg.as_str());
}

#[no_mangle]
pub unsafe extern "C" fn roc_memset(dst: *mut c_void, c: i32, n: usize) -> *mut c_void {
    libc::memset(dst, c, n)
}

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_mmap(
    addr: *mut libc::c_void,
    len: libc::size_t,
    prot: libc::c_int,
    flags: libc::c_int,
    fd: libc::c_int,
    offset: libc::off_t,
) -> *mut libc::c_void {
    libc::mmap(addr, len, prot, flags, fd, offset)
}

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_shm_open(
    name: *const libc::c_char,
    oflag: libc::c_int,
    mode: libc::mode_t,
) -> libc::c_int {
    libc::shm_open(name, oflag, mode as libc::c_uint)
}

#[cfg(unix)]
#[no_mangle]
pub unsafe extern "C" fn roc_getppid() -> libc::pid_t {
    libc::getppid()
}

// #[no_mangle]
// pub unsafe extern "C" fn roc_panic(msg: &RocStr, tag_id: u32) {
//     match tag_id {
//         0 => {
//             eprintln!("Roc crashed with:\n\n\t{}\n", msg.as_str());

//             print_backtrace();
//             std::process::exit(1);
//         }
//         1 => {
//             eprintln!("The program crashed with:\n\n\t{}\n", msg.as_str());

//             print_backtrace();
//             std::process::exit(1);
//         }
//         _ => todo!(),
//     }
// }

// fn print_backtrace() {
//     eprintln!("Here is the call stack that led to the crash:\n");

//     let mut entries = Vec::new();

//     #[derive(Default)]
//     struct Entry {
//         pub fn_name: String,
//         pub filename: Option<String>,
//         pub line: Option<u32>,
//         pub col: Option<u32>,
//     }

//     backtrace::trace(|frame| {
//         backtrace::resolve_frame(frame, |symbol| {
//             if let Some(fn_name) = symbol.name() {
//                 let fn_name = fn_name.to_string();

//                 if should_show_in_backtrace(&fn_name) {
//                     let mut entry: Entry = Default::default();

//                     entry.fn_name = format_fn_name(&fn_name);

//                     if let Some(path) = symbol.filename() {
//                         entry.filename = Some(path.to_string_lossy().into_owned());
//                     };

//                     entry.line = symbol.lineno();
//                     entry.col = symbol.colno();

//                     entries.push(entry);
//                 }
//             } else {
//                 entries.push(Entry {
//                     fn_name: "???".to_string(),
//                     ..Default::default()
//                 });
//             }
//         });

//         true // keep going to the next frame
//     });

//     for entry in entries {
//         eprintln!("\t{}", entry.fn_name);

//         if let Some(filename) = entry.filename {
//             eprintln!("\t\t{filename}");
//         }
//     }

//     eprintln!("\nOptimizations can make this list inaccurate! If it looks wrong, try running without `--optimize` and with `--linker=legacy`\n");
// }

// fn should_show_in_backtrace(fn_name: &str) -> bool {
//     let is_from_rust = fn_name.contains("::");
//     let is_host_fn = fn_name.starts_with("roc_panic")
//         || fn_name.starts_with("_Effect_effect")
//         || fn_name.starts_with("_roc__")
//         || fn_name.starts_with("rust_main")
//         || fn_name == "_main";

//     !is_from_rust && !is_host_fn
// }

// fn format_fn_name(fn_name: &str) -> String {
//     // e.g. convert "_Num_sub_a0c29024d3ec6e3a16e414af99885fbb44fa6182331a70ab4ca0886f93bad5"
//     // to ["Num", "sub", "a0c29024d3ec6e3a16e414af99885fbb44fa6182331a70ab4ca0886f93bad5"]
//     let mut pieces_iter = fn_name.split("_");

//     if let (_, Some(module_name), Some(name)) =
//         (pieces_iter.next(), pieces_iter.next(), pieces_iter.next())
//     {
//         display_roc_fn(module_name, name)
//     } else {
//         "???".to_string()
//     }
// }

// fn display_roc_fn(module_name: &str, fn_name: &str) -> String {
//     let module_name = if module_name == "#UserApp" {
//         "app"
//     } else {
//         module_name
//     };

//     let fn_name = if fn_name.parse::<u64>().is_ok() {
//         "(anonymous function)"
//     } else {
//         fn_name
//     };

//     format!("\u{001B}[36m{module_name}\u{001B}[39m.{fn_name}")
// }

#[roc_fn(name = "sendRequest")]
fn send_req(roc_request: &roc_app::Request) -> roc_app::Response {
    http_client::send_req(roc_request)
}

#[roc_fn(name = "envVar")]
fn env_var(roc_str: &RocStr) -> RocResult<RocStr, ()> {
    match std::env::var_os(roc_str.as_str()) {
        Some(os_str) => RocResult::ok(RocStr::from(os_str.to_string_lossy().to_string().as_str())),
        None => RocResult::err(()),
    }
}
