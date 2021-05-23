#![allow(non_snake_case)]

use roc_std::alloca;
use roc_std::RocCallResult;
use roc_std::RocStr;
use std::alloc::Layout;
use std::ffi::c_void;
use std::time::SystemTime;

extern "C" {
    #[link_name = "roc__rocMain_1_exposed"]
    fn roc_main(output: *mut u8) -> ();

    #[link_name = "roc__rocMain_1_size"]
    fn roc_main_size() -> i64;

    #[link_name = "roc__rocMain_1_Fx_caller"]
    fn call_Fx(function_pointer: *const u8, closure_data: *const u8, output: *mut u8) -> ();

    #[link_name = "roc__rocMain_1_Fx_size"]
    fn size_Fx() -> i64;

    fn malloc(size: usize) -> *mut c_void;
    fn realloc(c_ptr: *mut c_void, size: usize) -> *mut c_void;
    fn free(c_ptr: *mut c_void);
}

#[no_mangle]
pub unsafe fn roc_alloc(_alignment: usize, size: usize) -> *mut c_void {
    return malloc(size);
}

#[no_mangle]
pub unsafe fn roc_realloc(
    _alignment: usize,
    c_ptr: *mut c_void,
    _old_size: usize,
    new_size: usize,
) -> *mut c_void {
    return realloc(c_ptr, new_size);
}

#[no_mangle]
pub unsafe fn roc_dealloc(_alignment: usize, c_ptr: *mut c_void) {
    return free(c_ptr);
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
pub fn roc_fx_getLine() -> RocStr {
    use std::io::{self, BufRead};

    let stdin = io::stdin();
    let line1 = stdin.lock().lines().next().unwrap().unwrap();

    RocStr::from_slice_with_capacity(line1.as_bytes(), line1.len())
}

unsafe fn call_the_closure(function_pointer: *const u8, closure_data_ptr: *const u8) -> i64 {
    let size = size_Fx() as usize;

    alloca::with_stack_bytes(size, |buffer| {
        let buffer: *mut std::ffi::c_void = buffer;
        let buffer: *mut u8 = buffer as *mut u8;

        call_Fx(
            function_pointer,
            closure_data_ptr as *const u8,
            buffer as *mut u8,
        );

        let output = &*(buffer as *mut RocCallResult<i64>);

        match output.into() {
            Ok(_) => 0,
            Err(e) => panic!("failed with {}", e),
        }
    })
}

#[no_mangle]
pub fn rust_main() -> isize {
    println!("Running Roc closure");
    let start_time = SystemTime::now();

    let size = unsafe { roc_main_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();
    let answer = unsafe {
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
    let end_time = SystemTime::now();
    let duration = end_time.duration_since(start_time).unwrap();

    println!(
        "Roc execution took {:.4} ms",
        duration.as_secs_f64() * 1000.0,
    );

    // Exit code
    0
}
