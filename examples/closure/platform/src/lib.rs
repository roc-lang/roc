use std::mem::MaybeUninit;
use std::time::SystemTime;

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
    let (function_pointer, closure_data) = unsafe {
        let mut output: MaybeUninit<(fn(i64) -> i64, i64)> = MaybeUninit::uninit();

        closure(output.as_mut_ptr() as _);

        output.assume_init()
    };
    let answer = function_pointer(closure_data);
    let end_time = SystemTime::now();
    let duration = end_time.duration_since(start_time).unwrap();

    println!(
        "Roc closure took {:.4} ms to compute this answer: {:?}",
        duration.as_secs_f64() * 1000.0,
        // truncate the answer, so stdout is not swamped
        answer
    );

    println!("closure size {:?}", unsafe { closure_size() });

    // Exit code
    0
}
