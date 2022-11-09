
mod ui;
mod roc;
mod glue;

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {

    ui::run_event_loop("RocOut!");

    // Exit code
    0
}
