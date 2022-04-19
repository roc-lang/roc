mod roc;

use app_sys::roc_main;

use wasm_bindgen::prelude::{
    wasm_bindgen,
};

#[wasm_bindgen(start)]
pub fn run_main() {
    let roc_str = unsafe { roc_main() };

    let window = web_sys::window().expect("no global `window` exists");
    let document = window
        .document()
        .expect("should have a HTML document on window");
    let body = document.body().expect("HTML document should have a body");

    let val = document
        .create_element("p")
        .expect("Failed to create <p> element.");
    val.set_text_content(Some(roc_str.as_str()));

    body.append_child(&val).ok();
}
