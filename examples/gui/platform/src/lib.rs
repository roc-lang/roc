mod graphics;
mod gui;
mod rects_and_texts;
mod roc;

use crate::roc::RocElem;

extern "C" {
    #[link_name = "roc__renderForHost_1_exposed"]
    fn roc_render() -> RocElem;
}

#[no_mangle]
pub extern "C" fn rust_main() -> i32 {
    let root_elem = unsafe { roc_render() };

    gui::render("test title".into(), root_elem);

    // Exit code
    0
}
