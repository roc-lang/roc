use platform::run_main;

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    run_main();

    // Exit code
    0
}
