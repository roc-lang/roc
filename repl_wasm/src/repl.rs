use bumpalo::{collections::vec::Vec, Bump};
use std::mem::size_of;

use roc_collections::all::MutSet;
use roc_gen_wasm::wasm32_result;
use roc_load::file::MonomorphizedModule;
use roc_parse::ast::Expr;
use roc_repl_eval::{
    eval::jit_to_ast,
    gen::{compile_to_mono, format_answer, ReplOutput},
    ReplApp, ReplAppMemory,
};
use roc_target::TargetInfo;
use roc_types::pretty_print::{content_to_string, name_all_type_vars};

use crate::{js_create_app, js_get_result_and_memory, js_run_app};

const WRAPPER_NAME: &str = "wrapper";

pub struct WasmReplApp<'a> {
    arena: &'a Bump,
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

impl<'a> WasmReplApp<'a> {
    /// Allocate a buffer to copy the app memory into
    /// Buffer is aligned to 64 bits to preserve the original alignment of all Wasm numbers
    fn allocate_buffer(&self, size: usize) -> &'a mut [u8] {
        let size64 = (size / size_of::<u64>()) + 1;
        let buffer64: &mut [u64] = self.arena.alloc_slice_fill_default(size64);

        // Note: Need `from_raw_parts_mut` as well as `transmute` to ensure slice has correct length!
        let buffer: &mut [u8] = unsafe {
            let ptr8: *mut u8 = std::mem::transmute(buffer64.as_mut_ptr());
            std::slice::from_raw_parts_mut(ptr8, size)
        };

        buffer
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

        let copied_bytes: &mut [u8] = self.allocate_buffer(app_final_memory_size);

        let app_result_addr = js_get_result_and_memory(copied_bytes.as_mut_ptr());

        let result_bytes = &copied_bytes[app_result_addr..];
        let result: Return = unsafe {
            let ptr: *const Return = std::mem::transmute(result_bytes.as_ptr());
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

        let copied_bytes: &mut [u8] = self.allocate_buffer(app_final_memory_size);

        let app_result_addr = js_get_result_and_memory(copied_bytes.as_mut_ptr());
        let mem = self.arena.alloc(WasmMemory { copied_bytes });

        transform(mem, app_result_addr)
    }
}

pub async fn entrypoint_from_js(src: String) -> Result<String, String> {
    let arena = &Bump::new();
    let pre_linked_binary: &'static [u8] = include_bytes!("../data/pre_linked_binary.o");

    // Compile the app
    let target_info = TargetInfo::default_wasm32();
    let mono = match compile_to_mono(arena, &src, target_info) {
        Ok(m) => m,
        Err(messages) => return Err(messages.join("\n\n")),
    };

    let MonomorphizedModule {
        module_id,
        procedures,
        mut interns,
        mut subs,
        exposed_to_host,
        ..
    } = mono;

    debug_assert_eq!(exposed_to_host.values.len(), 1);
    let (main_fn_symbol, main_fn_var) = exposed_to_host.values.iter().next().unwrap();
    let main_fn_symbol = *main_fn_symbol;
    let main_fn_var = *main_fn_var;

    // pretty-print the expr type string for later.
    name_all_type_vars(main_fn_var, &mut subs);
    let content = subs.get_content_without_compacting(main_fn_var);
    let expr_type_str = content_to_string(content, &subs, module_id, &interns);

    let (_, main_fn_layout) = match procedures.keys().find(|(s, _)| *s == main_fn_symbol) {
        Some(layout) => *layout,
        None => return Ok(format!("<function> : {}", expr_type_str)),
    };

    let app_module_bytes = {
        let env = roc_gen_wasm::Env {
            arena,
            module_id,
            exposed_to_host: exposed_to_host
                .values
                .keys()
                .copied()
                .collect::<MutSet<_>>(),
        };

        let (mut module, called_preload_fns, main_fn_index) = {
            roc_gen_wasm::build_module_without_wrapper(
                &env,
                &mut interns, // NOTE: must drop this mutable ref before jit_to_ast
                pre_linked_binary,
                procedures,
            )
        };

        wasm32_result::insert_wrapper_for_layout(
            arena,
            &mut module,
            WRAPPER_NAME,
            main_fn_index,
            &main_fn_layout.result,
        );

        module.remove_dead_preloads(env.arena, called_preload_fns);

        let mut buffer = Vec::with_capacity_in(module.size(), arena);
        module.serialize(&mut buffer);

        buffer
    };

    // Send the compiled binary out to JS and create an executable instance from it
    js_create_app(&app_module_bytes)
        .await
        .map_err(|js| format!("{:?}", js))?;

    let app = WasmReplApp { arena };

    // Run the app and transform the result value to an AST `Expr`
    // Restore type constructor names, and other user-facing info that was erased during compilation.
    let res_answer = jit_to_ast(
        arena,
        &app,
        "", // main_fn_name is ignored (only passed to WasmReplApp methods)
        main_fn_layout,
        content,
        &interns,
        module_id,
        &subs,
        target_info,
    );

    // Transform the Expr to a string
    // `Result::Err` becomes a JS exception that will be caught and displayed
    match format_answer(arena, res_answer, expr_type_str) {
        ReplOutput::NoProblems { expr, expr_type } => Ok(format!("{} : {}", expr, expr_type)),
        ReplOutput::Problems(lines) => Err(format!("\n{}\n", lines.join("\n\n"))),
    }
}
