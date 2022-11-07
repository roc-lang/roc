
mod ui;
mod roc;

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    let bounds = roc::Bounds {
        width: 1900.0,
        height: 1000.0,
    };

    ui::run_event_loop("RocOut!", bounds);

    // Exit code
    0
}
