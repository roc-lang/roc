
mod ui;
mod roc;
mod glue;

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    let bounds = glue::Bounds {
        width: 1900.0,
        height: 1000.0,
    };

    ui::run_event_loop("RocOut!", bounds);

    // Exit code
    0
}
