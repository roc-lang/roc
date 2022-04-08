mod graphics;
mod gui;
mod roc;

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    let state = roc::State {
        width: 1900.0,
        height: 1000.0,
    };

    gui::run_event_loop("RocOut!", state).expect("Error running event loop");

    // Exit code
    0
}
