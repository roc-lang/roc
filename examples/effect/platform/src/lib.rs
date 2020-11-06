use roc_std::alloca;
use roc_std::RocCallResult;
use std::alloc::Layout;
use std::time::SystemTime;

extern "C" {
    #[link_name = "main_1_exposed"]
    fn roc_main(output: *mut u8) -> ();
}

#[no_mangle]
pub fn roc_fx_put_char(foo: i64) -> () {
    let character = foo as u8 as char;
    println!("HELLO WORLD");
    println!("{}", character);

    ()
}

//unsafe fn call_the_closure(function_pointer: *const u8, closure_data_ptr: *const u8) -> i64 {
//    let size = 32; //size_MyClosure() as usize;
//
//    alloca::with_stack_bytes(size, |buffer| {
//        let buffer: *mut std::ffi::c_void = buffer;
//        let buffer: *mut u8 = buffer as *mut u8;
//
//        call_MyClosure(
//            function_pointer,
//            closure_data_ptr as *const u8,
//            buffer as *mut u8,
//        );
//
//        let output = &*(buffer as *mut RocCallResult<i64>);
//
//        match output.into() {
//            Ok(v) => v,
//            Err(e) => panic!("failed with {}", e),
//        }
//    })
//}

#[no_mangle]
pub fn rust_main() -> isize {
    println!("Running Roc closure");
    let start_time = SystemTime::now();

    let size = 32; // unsafe { closure_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();
    let answer = unsafe {
        let buffer = std::alloc::alloc(layout);

        roc_main(buffer);

        let output = &*(buffer as *mut RocCallResult<(fn(i64) -> (), i64)>);

        match output.into() {
            Ok((function, closure_val)) => function(closure_val),
            Err(msg) => {
                std::alloc::dealloc(buffer, layout);

                panic!("Roc failed with message: {}", msg);
            }
        }
    };
    let end_time = SystemTime::now();
    let duration = end_time.duration_since(start_time).unwrap();

    println!(
        "Roc closure took {:.4} ms to compute this answer: {:?}",
        duration.as_secs_f64() * 1000.0,
        // truncate the answer, so stdout is not swamped
        answer
    );

    // Exit code
    0
}
