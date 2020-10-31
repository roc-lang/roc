use std::alloc::Layout;
use std::ffi::CString;
use std::mem::MaybeUninit;
use std::os::raw::c_char;
use std::time::SystemTime;
use RocCallResult::*;

extern "C" {
    #[link_name = "closure_1_exposed"]
    fn closure(output: *mut u8) -> ();

    #[link_name = "closure_1_size"]
    fn closure_size() -> i64;
}

#[no_mangle]
pub fn rust_main() -> isize {
    println!("Running Roc closure");
    let start_time = SystemTime::now();

    let size = unsafe { closure_size() } as usize;
    let layout = Layout::array::<u8>(size).unwrap();
    let roc_closure = unsafe {
        let buffer = std::alloc::alloc(layout);

        closure(buffer);

        let output = &*(buffer as *mut RocCallResult<(fn(i64) -> i64, i64)>);

        match output.into() {
            Ok((function_pointer, closure_data)) => {
                std::alloc::dealloc(buffer, layout);

                move || function_pointer(closure_data)
            }
            Err(msg) => {
                std::alloc::dealloc(buffer, layout);

                panic!("Roc failed with message: {}", msg);
            }
        }
    };
    let answer = roc_closure();
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
