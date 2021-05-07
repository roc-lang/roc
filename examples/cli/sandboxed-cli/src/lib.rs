#![allow(non_snake_case)]

use roc_std::{alloca, RocCallResult, RocResult, RocStr};
use std::alloc::Layout;

extern "C" {
    #[link_name = "roc__mainForHost_1_exposed"]
    fn roc_main(output: *mut u8) -> ();

    #[link_name = "roc__mainForHost_1_size"]
    fn roc_main_size() -> i64;

    #[link_name = "roc__mainForHost_1_Fx_caller"]
    fn call_Fx(
        flags: &'static u8,
        function_pointer: *const u8,
        closure_data: *const u8,
        output: *mut u8,
    ) -> ();

    #[allow(dead_code)]
    #[link_name = "roc__mainForHost_1_Fx_size"]
    fn size_Fx() -> i64;

    #[link_name = "roc__mainForHost_1_Fx_result_size"]
    fn size_Fx_result() -> i64;
}

#[no_mangle]
pub fn roc_fx_putChar(foo: i64) -> () {
    let character = foo as u8 as char;
    print!("{}", character);

    ()
}

#[no_mangle]
pub fn roc_fx_putLine(line: RocStr) -> () {
    let bytes = line.as_slice();
    let string = unsafe { std::str::from_utf8_unchecked(bytes) };
    println!("{}", string);

    ()
}

#[no_mangle]
pub fn roc_fx_httpGetUtf8(url: RocStr) -> RocResult<RocStr, RocStr> {
    use std::io::{self};

    println!(
        "\n\x1B[33mThis application wants to read the file \x1B[36m{} \x1B[33m\nDo you want to allow this?\n\n\x1B[34m(Y)\x1B[33mes   \x1B[34m(N)\x1B[33mo   \x1B[34m(A)\x1B[33mlways yes   Yes for this \x1B[34m(D)\x1B[33mirectory only\x1B[39m",
        "/Users/rtfeldman/Documents"
    );

    let mut buffer = String::new();
    let stdin = io::stdin();

    stdin.read_line(&mut buffer).unwrap();

    println!(
        "\n\x1B[33mThis application wants to make a HTTP request to \x1B[36m{} \x1B[33m\nDo you want to allow this?\n\n\x1B[34m(Y)\x1B[33mes   \x1B[34m(N)\x1B[33mo   \x1B[34m(A)\x1B[33mlways yes   Yes for this \x1B[34m(D)\x1B[33momain only\x1B[39m",
        unsafe { url.as_str() }
    );

    let mut buffer = String::new();
    let stdin = io::stdin();

    stdin.read_line(&mut buffer).unwrap();

    match ureq::get(unsafe { url.as_str() }).call() {
        Ok(resp) => match resp.into_string() {
            Ok(contents) => RocResult::Ok(RocStr::from_slice(contents.as_bytes())), // TODO make roc::Result!
            // TODO turn this error into an enum!
            Err(err) => RocResult::Err(RocStr::from_slice(format!("{:?}", err).as_bytes())),
        },
        // TODO turn this error into an enum!
        Err(err) => RocResult::Err(RocStr::from_slice(format!("{:?}", err).as_bytes())),
    }
}

#[no_mangle]
pub fn roc_fx_getLine() -> RocStr {
    use std::io::{self, BufRead};

    let stdin = io::stdin();
    let line1 = stdin.lock().lines().next().unwrap().unwrap();

    RocStr::from_slice(line1.as_bytes())
}

unsafe fn call_the_closure(function_pointer: *const u8, closure_data_ptr: *const u8) -> i64 {
    let size = size_Fx_result() as usize;

    alloca::with_stack_bytes(size, |buffer| {
        let buffer: *mut std::ffi::c_void = buffer;
        let buffer: *mut u8 = buffer as *mut u8;

        call_Fx(
            &0,
            function_pointer,
            closure_data_ptr as *const u8,
            buffer as *mut u8,
        );

        let output = &*(buffer as *mut RocCallResult<()>);

        match output.into() {
            Ok(_) => 0,
            Err(e) => panic!("failed with {}", e),
        }
    })
}

#[no_mangle]
pub fn rust_main() -> isize {
    let size = unsafe { roc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();

    unsafe {
        let buffer = std::alloc::alloc(layout);

        roc_main(buffer);

        let output = &*(buffer as *mut RocCallResult<()>);

        match output.into() {
            Ok(()) => {
                let function_pointer = {
                    // this is a pointer to the location where the function pointer is stored
                    // we pass just the function pointer
                    let temp = buffer.offset(8) as *const i64;

                    (*temp) as *const u8
                };

                let closure_data_ptr = buffer.offset(16);

                let result =
                    call_the_closure(function_pointer as *const u8, closure_data_ptr as *const u8);

                std::alloc::dealloc(buffer, layout);

                result
            }
            Err(msg) => {
                std::alloc::dealloc(buffer, layout);

                panic!("Roc failed with message: {}", msg);
            }
        }
    };

    // Exit code
    0
}
