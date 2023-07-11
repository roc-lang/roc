use bumpalo::Bump;
use inkwell::context::Context;
use libloading::Library;
use roc_build::link::llvm_module_to_dylib;
use roc_collections::all::MutSet;
use roc_error_macros::internal_error;
use roc_gen_llvm::llvm::build::LlvmBackendMode;
use roc_gen_llvm::llvm::externs::add_default_roc_externs;
use roc_gen_llvm::{run_jit_function, run_jit_function_dynamic_type};
use roc_load::{EntryPoint, MonomorphizedModule};
use roc_mono::ir::OptLevel;
use roc_mono::layout::STLayoutInterner;
use roc_parse::ast::Expr;
use roc_repl_eval::eval::jit_to_ast;
use roc_repl_eval::gen::{compile_to_mono, format_answer, Problems, ReplOutput};
use roc_repl_eval::{ReplApp, ReplAppMemory};
use roc_reporting::report::DEFAULT_PALETTE;
use roc_std::RocStr;
use roc_target::TargetInfo;
use roc_types::pretty_print::{name_and_print_var, DebugPrint};
use roc_types::subs::Subs;
use target_lexicon::Triple;

pub fn gen_and_eval_llvm<'a, I: Iterator<Item = &'a str>>(
    defs: I,
    src: &str,
    target: Triple,
    opt_level: OptLevel,
) -> (Option<ReplOutput>, Problems) {
    let arena = Bump::new();
    let target_info = TargetInfo::from(&target);

    let mut loaded;
    let problems;

    match compile_to_mono(&arena, defs, src, target_info, DEFAULT_PALETTE) {
        (Some(mono), probs) => {
            loaded = mono;
            problems = probs;
        }
        (None, probs) => {
            return (None, probs);
        }
    };

    debug_assert_eq!(loaded.exposed_to_host.top_level_values.len(), 1);
    let (main_fn_symbol, main_fn_var) = loaded
        .exposed_to_host
        .top_level_values
        .iter()
        .next()
        .unwrap();
    let main_fn_symbol = *main_fn_symbol;
    let main_fn_var = *main_fn_var;

    // pretty-print the expr type string for later.
    let expr_type_str = name_and_print_var(
        main_fn_var,
        &mut loaded.subs,
        loaded.module_id,
        &loaded.interns,
        DebugPrint::NOTHING,
    );

    let (_, main_fn_layout) = match loaded.procedures.keys().find(|(s, _)| *s == main_fn_symbol) {
        Some(layout) => *layout,
        None => {
            let empty_vec: Vec<String> = Vec::new(); // rustc can't infer the type of this Vec.
            debug_assert_ne!(problems.errors, empty_vec, "Got no errors but also no valid layout for the generated main function in the repl!");

            return (None, problems);
        }
    };

    let interns = loaded.interns.clone();

    let (lib, main_fn_name, subs, layout_interner) =
        mono_module_to_dylib(&arena, target, loaded, opt_level).expect("we produce a valid Dylib");

    let mut app = CliApp { lib };

    let expr = jit_to_ast(
        &arena,
        &mut app,
        main_fn_name,
        main_fn_layout,
        main_fn_var,
        &subs,
        &interns,
        layout_interner.into_global().fork(),
        target_info,
    );
    let expr_str = format_answer(&arena, expr).to_string();

    (
        Some(ReplOutput {
            expr: expr_str,
            expr_type: expr_type_str,
        }),
        problems,
    )
}

struct CliApp {
    lib: Library,
}

struct CliMemory;

impl<'a> ReplApp<'a> for CliApp {
    type Memory = CliMemory;

    /// Run user code that returns a type with a `Builtin` layout
    /// Size of the return value is statically determined from its Rust type
    fn call_function<Return, F>(&mut self, main_fn_name: &str, mut transform: F) -> Expr<'a>
    where
        F: FnMut(&'a Self::Memory, Return) -> Expr<'a>,
        Self::Memory: 'a,
    {
        run_jit_function!(self.lib, main_fn_name, Return, |v| transform(&CliMemory, v))
    }

    /// Run user code that returns a struct or union, whose size is provided as an argument
    fn call_function_dynamic_size<T, F>(
        &mut self,
        main_fn_name: &str,
        ret_bytes: usize,
        mut transform: F,
    ) -> T
    where
        F: FnMut(&'a Self::Memory, usize) -> T,
        Self::Memory: 'a,
    {
        run_jit_function_dynamic_type!(self.lib, main_fn_name, ret_bytes, |v| transform(
            &CliMemory, v
        ))
    }
}

macro_rules! deref_number {
    ($name: ident, $t: ty) => {
        fn $name(&self, addr: usize) -> $t {
            let ptr = addr as *const _;
            unsafe { *ptr }
        }
    };
}

impl ReplAppMemory for CliMemory {
    deref_number!(deref_bool, bool);

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
        let reference: &RocStr = unsafe { std::mem::transmute(addr) };
        reference.as_str()
    }

    fn deref_pointer_with_tag_id(&self, addr: usize) -> (u16, u64) {
        let addr_with_id = self.deref_usize(addr);
        let tag_id_mask = 0b111;

        let tag_id = addr_with_id & tag_id_mask;
        let data_addr = addr_with_id & !tag_id_mask;
        (tag_id as _, data_addr as _)
    }
}

fn mono_module_to_dylib<'a>(
    arena: &'a Bump,
    target: Triple,
    loaded: MonomorphizedModule<'a>,
    opt_level: OptLevel,
) -> Result<(libloading::Library, &'a str, Subs, STLayoutInterner<'a>), libloading::Error> {
    let target_info = TargetInfo::from(&target);

    let MonomorphizedModule {
        procedures,
        entry_point,
        interns,
        subs,
        layout_interner,
        ..
    } = loaded;

    let context = Context::create();
    let builder = context.create_builder();
    let module = arena.alloc(roc_gen_llvm::llvm::build::module_from_builtins(
        &target, &context, "",
    ));

    let module = arena.alloc(module);
    let (module_pass, function_pass) =
        roc_gen_llvm::llvm::build::construct_optimization_passes(module, opt_level);

    let (dibuilder, compile_unit) = roc_gen_llvm::llvm::build::Env::new_debug_info(module);

    // Compile and add all the Procs before adding main
    let env = roc_gen_llvm::llvm::build::Env {
        arena,
        builder: &builder,
        dibuilder: &dibuilder,
        compile_unit: &compile_unit,
        context: &context,
        interns,
        module,
        target_info,
        mode: LlvmBackendMode::GenTest, // so roc_panic is generated
        // important! we don't want any procedures to get the C calling convention
        exposed_to_host: MutSet::default(),
    };

    // Add roc_alloc, roc_realloc, and roc_dealloc, since the repl has no
    // platform to provide them.
    add_default_roc_externs(&env);

    let entry_point = match entry_point {
        EntryPoint::Executable {
            exposed_to_host,
            platform_path: _,
        } => {
            // TODO support multiple of these!
            debug_assert_eq!(exposed_to_host.len(), 1);
            let (symbol, layout) = exposed_to_host[0];

            roc_mono::ir::SingleEntryPoint { symbol, layout }
        }
        EntryPoint::Test => {
            unreachable!()
        }
    };

    let (main_fn_name, main_fn) = roc_gen_llvm::llvm::build::build_procedures_return_main(
        &env,
        &layout_interner,
        opt_level,
        procedures,
        entry_point,
    );

    env.dibuilder.finalize();

    // we don't use the debug info, and it causes weird errors.
    module.strip_debug_info();

    // Uncomment this to see the module's un-optimized LLVM instruction output:
    // env.module.print_to_stderr();

    if main_fn.verify(true) {
        function_pass.run_on(&main_fn);
    } else {
        internal_error!("Main function {main_fn_name} failed LLVM verification in build. Uncomment things nearby to see more details.", );
    }

    module_pass.run_on(env.module);

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    // Verify the module
    if let Err(errors) = env.module.verify() {
        internal_error!("Errors defining module:\n{}", errors.to_string());
    }

    llvm_module_to_dylib(env.module, &target, opt_level)
        .map(|lib| (lib, main_fn_name, subs, layout_interner))
}
