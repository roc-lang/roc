use bumpalo::{collections::vec::Vec, Bump};
use roc_reporting::report::{DEFAULT_PALETTE_HTML, HTML_STYLE_CODES};
use std::{cell::RefCell, mem::size_of};

use roc_collections::all::MutSet;
use roc_gen_wasm::wasm32_result;
use roc_load::MonomorphizedModule;
use roc_parse::ast::Expr;
use roc_repl_eval::{
    eval::jit_to_ast,
    gen::{format_answer, ReplOutput},
    ReplApp, ReplAppMemory,
};
use roc_repl_ui::{
    format_output,
    repl_state::{ReplAction, ReplState},
    TIPS,
};
use roc_target::Target;
use roc_types::pretty_print::{name_and_print_var, DebugPrint};

use crate::{js_create_app, js_get_result_and_memory, js_run_app};

const WRAPPER_NAME: &str = "wrapper";

// On the web, we keep the REPL state in a global variable, because `main` is not in our Rust code!
// We return back to JS after every line of input. `main` is in the browser engine, running the JS event loop.
std::thread_local! {
    static REPL_STATE: RefCell<ReplState> = RefCell::new(ReplState::new());
}

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
        // We can't use RocStr, we need our own small/big string logic.
        // The first field is *not* a pointer. We can calculate a pointer for it, but only for big strings.
        // If changing this code, remember it also runs in wasm32, not just the app.
        let last_byte = self.copied_bytes[addr + 4 + 4 + 3] as i8;
        let is_small = last_byte < 0;

        let str_bytes = if is_small {
            let len = (last_byte & 0x7f) as usize;
            &self.copied_bytes[addr..][..len]
        } else {
            let chars_index = self.deref_usize(addr);
            let seamless_slice_mask = u32::MAX as usize >> 1;
            let len = self.deref_usize(addr + 4) & seamless_slice_mask;
            &self.copied_bytes[chars_index..][..len]
        };

        unsafe { std::str::from_utf8_unchecked(str_bytes) }
    }

    fn deref_pointer_with_tag_id(&self, addr: usize) -> (u16, u64) {
        let addr_with_id = self.deref_usize(addr);
        let tag_id_mask = 0b11;

        let tag_id = addr_with_id & tag_id_mask;
        let data_addr = addr_with_id & !tag_id_mask;
        (tag_id as _, data_addr as _)
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
    fn call_function<Return, F>(&mut self, _main_fn_name: &str, mut transform: F) -> Expr<'a>
    where
        F: FnMut(&'a Self::Memory, Return) -> Expr<'a>,
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
        &mut self,
        _main_fn_name: &str,
        _ret_bytes: usize,
        mut transform: F,
    ) -> Option<T>
    where
        F: FnMut(&'a Self::Memory, usize) -> T,
        Self::Memory: 'a,
    {
        let app_final_memory_size: usize = js_run_app();

        let copied_bytes: &mut [u8] = self.allocate_buffer(app_final_memory_size);

        let app_result_addr = js_get_result_and_memory(copied_bytes.as_mut_ptr());
        let mem = self.arena.alloc(WasmMemory { copied_bytes });

        Some(transform(mem, app_result_addr))
    }
}

const PRE_LINKED_BINARY: &[u8] =
    include_bytes!(concat!(env!("OUT_DIR"), "/pre_linked_binary.wasm")) as &[_];

pub async fn entrypoint_from_js(src: String) -> String {
    // If our Rust code panics, redirect the error message to JS console.error
    // Also, our JS code overrides console.error to display the error message text (including stack trace) in the REPL output.
    #[cfg(feature = "console_error_panic_hook")]
    console_error_panic_hook::set_once();

    // TODO: make this a global and reset it?
    let arena = &Bump::new();

    // Compile the app
    let target = Target::Wasm32;

    // Advance the REPL state machine
    let action = REPL_STATE.with(|repl_state_cell| {
        let mut repl_state = repl_state_cell.borrow_mut();
        repl_state.step(arena, &src, target, DEFAULT_PALETTE_HTML)
    });

    // Perform the action the state machine asked for, and return the appropriate output string
    match action {
        ReplAction::Help => TIPS.to_string(),
        ReplAction::Exit => {
            "To exit the web version of the REPL, just close the browser tab!".to_string()
        }
        ReplAction::FileProblem { .. } => {
            "The web version of the REPL cannot import files... for now!".to_string()
        }
        ReplAction::Nothing => String::new(),
        ReplAction::Eval { opt_mono, problems } => {
            let opt_output = match opt_mono {
                Some(mono) => eval_wasm(arena, target, mono).await,
                None => None,
            };

            format_output(HTML_STYLE_CODES, opt_output, problems)
        }
    }
}

async fn eval_wasm<'a>(
    arena: &'a Bump,
    target: Target,
    mono: MonomorphizedModule<'a>,
) -> Option<ReplOutput> {
    let MonomorphizedModule {
        module_id,
        procedures,
        mut interns,
        mut subs,
        exposed_to_host,
        mut layout_interner,
        ..
    } = mono;

    debug_assert_eq!(exposed_to_host.top_level_values.len(), 1);
    let (main_fn_symbol, main_fn_var) = exposed_to_host.top_level_values.iter().next().unwrap();
    let main_fn_symbol = *main_fn_symbol;
    let main_fn_var = *main_fn_var;

    // pretty-print the expr type string for later.
    let expr_type = name_and_print_var(
        main_fn_var,
        &mut subs,
        module_id,
        &interns,
        DebugPrint::NOTHING,
    );

    let (_, main_fn_layout) = *procedures.keys().find(|(s, _)| *s == main_fn_symbol)?;

    let app_module_bytes = {
        let env = roc_gen_wasm::Env {
            arena,
            module_id,
            stack_bytes: roc_gen_wasm::Env::DEFAULT_STACK_BYTES,
            exposed_to_host: exposed_to_host
                .top_level_values
                .keys()
                .copied()
                .collect::<MutSet<_>>(),
        };

        let (mut module, mut called_fns, main_fn_index) = {
            let host_module = roc_gen_wasm::parse_host(env.arena, PRE_LINKED_BINARY).unwrap();
            roc_gen_wasm::build_app_module(
                &env,
                &mut layout_interner,
                &mut interns, // NOTE: must drop this mutable ref before jit_to_ast
                host_module,
                procedures,
            )
        };

        wasm32_result::insert_wrapper_for_layout(
            arena,
            &layout_interner,
            &mut module,
            WRAPPER_NAME,
            main_fn_index,
            main_fn_layout.result,
        );
        called_fns.push(true);

        module.eliminate_dead_code(env.arena, called_fns);

        let mut buffer = Vec::with_capacity_in(module.size(), arena);
        module.serialize(&mut buffer);

        buffer
    };

    // Send the compiled binary out to JS, which will asynchronously create an executable WebAssembly instance
    match js_create_app(&app_module_bytes).await {
        Ok(()) => {}
        Err(js_exception) => {
            return Some(ReplOutput {
                expr: format!("<span class='color-red'>{js_exception:?}</span>"),
                expr_type: String::new(),
            })
        }
    }

    let mut app = WasmReplApp { arena };

    // Run the app and transform the result value to an AST `Expr`
    // Restore type constructor names, and other user-facing info that was erased during compilation.
    let res_answer = jit_to_ast(
        arena,
        &mut app,
        "", // main_fn_name is ignored (only passed to WasmReplApp methods)
        main_fn_layout,
        main_fn_var,
        &subs,
        &interns,
        layout_interner.into_global().fork(),
        target,
    );

    // Transform the Expr to a string
    let expr = format_answer(arena, res_answer).to_string();

    Some(ReplOutput { expr, expr_type })
}
