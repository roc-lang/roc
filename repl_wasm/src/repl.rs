mod generated_app_bytes;

use bumpalo::Bump;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(catch)]
    pub async fn js_create_and_run_app(
        app_bytes: &[u8],
        app_memory_size_ptr: *mut usize,
    ) -> Result<(), JsValue>;
    pub fn js_copy_app_memory(buffer_alloc_addr: *mut u8) -> usize;
}

#[wasm_bindgen]
pub async fn webrepl_run(input_text: String) -> Result<String, String> {
    let arena = &Bump::new();

    // Compile the app
    let (app_bytes, identifiers) = compile(arena, input_text)?;

    // Execute the app (asynchronously in JS)
    let mut app_final_memory_size: usize = 0;
    let size_mut_ptr = (&mut app_final_memory_size) as *mut usize;
    js_create_and_run_app(app_bytes, size_mut_ptr)
        .await
        .map_err(|js| format!("{:?}", js))?;

    // Get the address of the result in the app's memory, and a copy of its memory buffer
    let app_memory_copy: &mut [u8] = arena.alloc_slice_fill_default(app_final_memory_size);
    let app_result_addr = js_copy_app_memory(app_memory_copy.as_mut_ptr());

    // Create a String representation of the result value
    let output_text = stringify(app_memory_copy, app_result_addr, identifiers);

    Ok(output_text)
}
