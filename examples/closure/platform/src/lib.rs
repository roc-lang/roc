use roc_std::alloca;
use std::alloc::Layout;
use std::ffi::CString;
use std::os::raw::c_char;
use std::time::SystemTime;
use RocCallResult::*;

extern "C" {
    #[link_name = "makeClosure_1_exposed"]
    fn make_closure(output: *mut u8) -> ();

    #[link_name = "makeClosure_1_size"]
    fn closure_size() -> i64;

    #[link_name = "makeClosure_1_MyClosure_caller"]
    fn call_MyClosure(function_pointer: *const u8, closure_data: *const u8, output: *mut u8) -> ();

    #[link_name = "makeClosure_1_MyClosure_size"]
    fn size_MyClosure() -> i64;
}

unsafe fn call_the_closure(function_pointer: *const u8, closure_data_ptr: *const u8) -> i64 {
    let size = size_MyClosure() as usize;

    alloca::with_stack_bytes(size, |buffer| {
        let buffer: *mut std::ffi::c_void = buffer;
        let buffer: *mut u8 = buffer as *mut u8;

        call_MyClosure(
            function_pointer,
            closure_data_ptr as *const u8,
            buffer as *mut u8,
        );

        let output = &*(buffer as *mut RocCallResult<i64>);

        match output.into() {
            Ok(v) => v,
            Err(e) => panic!("failed with {}", e),
        }
    })
}

#[no_mangle]
pub fn rust_main() -> isize {
    println!("Running Roc closure");
    let start_time = SystemTime::now();

    let size = unsafe { closure_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();
    let answer = unsafe {
        let buffer = std::alloc::alloc(layout);

        make_closure(buffer);

        let output = &*(buffer as *mut RocCallResult<((), ())>);

        match output.into() {
            Ok((_, _)) => {
                let function_pointer = {
                    // this is a pointer to the location where the function pointer is stored
                    // we pass just the function pointer
                    let temp = buffer.offset(8) as *const i64;

                    (*temp) as *const u8
                };

                let closure_data_ptr = buffer.offset(16);

                call_the_closure(function_pointer as *const u8, closure_data_ptr as *const u8)
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
        "Roc closure took {:.4} ms to compute this answer: {:?}",
        duration.as_secs_f64() * 1000.0,
        // truncate the answer, so stdout is not swamped
        answer
    );

    // Exit code
    0
}

#[repr(u64)]
pub enum RocCallResult<T> {
    Success(T),
    Failure(*mut c_char),
}

impl<T: Sized> Into<Result<T, String>> for RocCallResult<T> {
    fn into(self) -> Result<T, String> {
        match self {
            Success(value) => Ok(value),
            Failure(failure) => Err({
                let raw = unsafe { CString::from_raw(failure) };

                let result = format!("{:?}", raw);

                // make sure rust does not try to free the Roc string
                std::mem::forget(raw);

                result
            }),
        }
    }
}

impl<T: Sized + Copy> Into<Result<T, String>> for &RocCallResult<T> {
    fn into(self) -> Result<T, String> {
        match self {
            Success(value) => Ok(*value),
            Failure(failure) => Err({
                let raw = unsafe { CString::from_raw(*failure) };

                let result = format!("{:?}", raw);

                // make sure rust does not try to free the Roc string
                std::mem::forget(raw);

                result
            }),
        }
    }
}
