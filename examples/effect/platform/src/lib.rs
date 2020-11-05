use roc_std::alloca;
use roc_std::RocCallResult;
use std::alloc::Layout;
use std::time::SystemTime;

extern "C" {
    #[link_name = "main_1_exposed"]
    fn roc_main(output: *mut u8) -> ();
}

#[no_mangle]
pub fn roc_fx_put_char(unit: ()) -> () {
    let character = 68u8 as char;
    println!("{}", character);

    ()
}

#[no_mangle]
pub fn rust_main() -> isize {
    println!("Running Roc closure");
    let start_time = SystemTime::now();

    let answer = unsafe {
        let mut temp = (1i64, 1i64);
        let buffer: *mut (i64, i64) = &mut temp;
        let buffer: *mut u8 = buffer as *mut u8;

        roc_main(buffer);

        let output = &*(buffer as *mut RocCallResult<()>);

        match output.into() {
            Ok(()) => 1,
            Err(msg) => {
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
