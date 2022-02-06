use bumpalo::Bump;
use std::mem::size_of;
use wasm_bindgen::prelude::wasm_bindgen;
use wasm_bindgen::JsValue;

use roc_load::file::MonomorphizedModule;
use roc_parse::ast::Expr;
use roc_repl_eval::{gen::compile_to_mono, ReplApp, ReplAppMemory};
use roc_target::TargetInfo;

#[wasm_bindgen]
extern "C" {
    #[wasm_bindgen(catch)]
    pub async fn js_create_app(wasm_module_bytes: &[u8]) -> Result<(), JsValue>;

    pub fn js_run_app() -> usize;

    pub fn js_get_result_and_memory(buffer_alloc_addr: *mut u8) -> usize;
}

pub struct WasmReplApp<'a> {
    arena: &'a Bump,
    _module: &'a [u8],
}

/// A copy of the app's memory, made after running the main function
/// The Wasm app ran in a separate address space from the compiler and the eval code.
/// This means we can't simply dereference its pointers as if they were local, because
/// an unrelated value may exist at the same-numbered address in our own address space!
/// Instead we have dereferencing methods that index into the copied bytes.
pub struct WasmMemory<'a> {
    copied_bytes: &'a [u8],
}

macro_rules! deref_number {
    ($name: ident, $t: ty) => {
        fn $name(&self, address: usize) -> $t {
            const N: usize = size_of::<$t>();
            let mut array = [0; N];
            array.copy_from_slice(&self.copied_bytes[address..][..N]);
            <$t>::from_le_bytes(array)
        }
    };
}

impl<'a> ReplAppMemory for WasmMemory<'a> {
    fn deref_bool(&self, address: usize) -> bool {
        self.copied_bytes[address] != 0
    }

    deref_number!(deref_u8, u8);
    deref_number!(deref_u16, u16);
    deref_number!(deref_u32, u32);
    deref_number!(deref_u64, u64);
    deref_number!(deref_u128, u128);
    deref_number!(deref_usize, usize);

    deref_number!(deref_i8, i8);
    deref_number!(deref_i16, i16);
    deref_number!(deref_i32, i32);
    deref_number!(deref_i64, i64);
    deref_number!(deref_i128, i128);
    deref_number!(deref_isize, isize);

    deref_number!(deref_f32, f32);
    deref_number!(deref_f64, f64);

    fn deref_str(&self, addr: usize) -> &str {
        let elems_addr = self.deref_usize(addr);
        let len = self.deref_usize(addr + size_of::<usize>());
        let bytes = &self.copied_bytes[elems_addr..][..len];
        std::str::from_utf8(bytes).unwrap()
    }
}

impl<'a> ReplApp<'a> for WasmReplApp<'a> {
    type Memory = WasmMemory<'a>;

    /// Run user code that returns a type with a `Builtin` layout
    /// Size of the return value is statically determined from its Rust type
    /// The `transform` callback takes the app's memory and the returned value
    /// _main_fn_name is always the same and we don't use it here
    fn call_function<Return, F>(&self, _main_fn_name: &str, transform: F) -> Expr<'a>
    where
        F: Fn(&'a Self::Memory, Return) -> Expr<'a>,
        Self::Memory: 'a,
    {
        let app_final_memory_size: usize = js_run_app();

        // Allocate a buffer to copy the app memory into
        // Aligning it to 64 bits will preserve the original alignment of all Wasm numbers
        let copy_buffer_aligned: &mut [u64] = self
            .arena
            .alloc_slice_fill_default((app_final_memory_size / size_of::<u64>()) + 1);
        let copied_bytes: &mut [u8] = unsafe { std::mem::transmute(copy_buffer_aligned) };

        let app_result_addr = js_get_result_and_memory(copied_bytes.as_mut_ptr());

        let result: Return = unsafe {
            let ptr: *const Return = std::mem::transmute(&copied_bytes[app_result_addr]);
            ptr.read()
        };
        let mem = self.arena.alloc(WasmMemory { copied_bytes });

        transform(mem, result)
    }

    /// Run user code that returns a struct or union, whose size is provided as an argument
    /// The `transform` callback takes the app's memory and the address of the returned value
    /// _main_fn_name and _ret_bytes are only used for the CLI REPL. For Wasm they are compiled-in
    /// to the test_wrapper function of the app itself
    fn call_function_dynamic_size<T, F>(
        &self,
        _main_fn_name: &str,
        _ret_bytes: usize,
        transform: F,
    ) -> T
    where
        F: Fn(&'a Self::Memory, usize) -> T,
        Self::Memory: 'a,
    {
        let app_final_memory_size: usize = js_run_app();

        // Allocate a buffer to copy the app memory into
        // Aligning it to 64 bits will preserve the original alignment of all Wasm numbers
        let copy_buffer_aligned: &mut [u64] = self
            .arena
            .alloc_slice_fill_default((app_final_memory_size / size_of::<u64>()) + 1);
        let copied_bytes: &mut [u8] = unsafe { std::mem::transmute(copy_buffer_aligned) };

        let app_result_addr = js_get_result_and_memory(copied_bytes.as_mut_ptr());
        let mem = self.arena.alloc(WasmMemory { copied_bytes });

        transform(mem, app_result_addr)
    }
}

#[wasm_bindgen]
pub async fn repl_wasm_entrypoint_from_js(src: String) -> Result<String, String> {
    let arena = &Bump::new();

    // Compile the app
    let mono = match compile_to_mono(arena, &src, TargetInfo::default_wasm32()) {
        Ok(m) => m,
        Err(messages) => return Err(messages.join("\n\n")),
    };

    let MonomorphizedModule {
        module_id,
        procedures,
        mut interns,
        exposed_to_host,
        ..
    } = mono;

    /*
        TODO
        - reuse code from test_gen/src/wasm.rs
        - use return type to create test_wrapper

    */

    let app_module_bytes: &[u8] = &[];

    js_create_app(app_module_bytes)
        .await
        .map_err(|js| format!("{:?}", js))?;

    let app_final_memory_size: usize = js_run_app();

    // Copy the app's memory and get the result address
    let app_memory_copy: &mut [u8] = arena.alloc_slice_fill_default(app_final_memory_size);
    let app_result_addr = js_get_result_and_memory(app_memory_copy.as_mut_ptr());

    /*
        TODO
        - gen_and_eval_wasm

    */

    // Create a String representation of the result value
    let output_text = format!("{}", app_result_addr);

    Ok(output_text)
}
