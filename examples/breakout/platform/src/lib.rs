mod graphics;
mod gui;
mod roc;

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    let bounds = roc::Bounds {
        width: 1900.0,
        height: 1000.0,
    };

    gui::run_event_loop("RocOut!", bounds).expect("Error running event loop");

    // Exit code
    0
}
