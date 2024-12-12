use bumpalo::Bump;
use inkwell::context::Context;
use libloading::Library;
use roc_build::link::llvm_module_to_dylib;
use roc_collections::all::MutSet;

use roc_gen_llvm::llvm::build::LlvmBackendMode;
use roc_gen_llvm::llvm::externs::add_default_roc_externs;
use roc_gen_llvm::{run_jit_function, run_jit_function_dynamic_type};
use roc_load::{EntryPoint, MonomorphizedModule};
use roc_mono::ir::OptLevel;
use roc_mono::layout::STLayoutInterner;
use roc_parse::ast::Expr;
use roc_repl_eval::eval::jit_to_ast;
use roc_repl_eval::gen::{format_answer, ReplOutput};
use roc_repl_eval::{ReplApp, ReplAppMemory};
use roc_std::RocStr;
use roc_target::Target;
use roc_types::pretty_print::{name_and_print_var, DebugPrint};
use roc_types::subs::Subs;

pub fn eval_llvm(
    mut loaded: MonomorphizedModule<'_>,
    target: Target,
    opt_level: OptLevel,
) -> Option<ReplOutput> {
    let arena = Bump::new();

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

    let (_, main_fn_layout) = *loaded
        .procedures
        .keys()
        .find(|(s, _)| *s == main_fn_symbol)?;

    let interns = loaded.interns.clone();

    #[cfg(not(all(
        any(target_os = "linux", target_os = "macos"),
        any(target_arch = "x86_64", target_arch = "aarch64")
    )))]
    let (lib, main_fn_name, subs, layout_interner) =
        mono_module_to_dylib_llvm(&arena, target, loaded, opt_level)
            .expect("we produce a valid Dylib");

    #[cfg(all(
        any(target_os = "linux", target_os = "macos"),
        any(target_arch = "x86_64", target_arch = "aarch64")
    ))]
    let (lib, main_fn_name, subs, layout_interner) =
        mono_module_to_dylib_asm(&arena, target, loaded, opt_level)
            .expect("We failed to produce a valid Dylib.\nTIP: if you're on macos, try this:\n\t<https://github.com/roc-lang/roc/issues/5797#issuecomment-1786105269>");

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
        target,
    );

    let expr_str = format_answer(&arena, expr).to_string();

    Some(ReplOutput {
        expr: expr_str,
        expr_type: expr_type_str,
    })
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
    ) -> Option<T>
    where
        F: FnMut(&'a Self::Memory, usize) -> T,
        Self::Memory: 'a,
    {
        let mut t = |v| transform(&CliMemory, v);
        run_jit_function_dynamic_type!(self.lib, main_fn_name, ret_bytes, t)
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

#[cfg_attr(
    all(
        any(target_os = "linux", target_os = "macos"),
        any(target_arch = "x86_64", target_arch = "aarch64")
    ),
    allow(unused)
)]
fn mono_module_to_dylib_llvm<'a>(
    arena: &'a Bump,
    target: Target,
    loaded: MonomorphizedModule<'a>,
    opt_level: OptLevel,
) -> Result<(libloading::Library, &'a str, Subs, STLayoutInterner<'a>), libloading::Error> {
    let MonomorphizedModule {
        procedures,
        host_exposed_lambda_sets,
        entry_point,
        interns,
        subs,
        layout_interner,
        ..
    } = loaded;

    let context = Context::create();
    let builder = context.create_builder();
    let module = arena.alloc(roc_gen_llvm::llvm::build::module_from_builtins(
        target, &context, "",
    ));

    let module = arena.alloc(module);

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
        target,
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
            let (name, symbol, layout) = exposed_to_host[0];

            roc_mono::ir::SingleEntryPoint {
                name,
                symbol,
                layout,
            }
        }
        EntryPoint::Test => {
            unreachable!()
        }
    };

    let (main_fn_name, _main_fn) = roc_gen_llvm::llvm::build::build_procedures_return_main(
        &env,
        &layout_interner,
        opt_level,
        procedures,
        host_exposed_lambda_sets,
        entry_point,
    );

    let emit_debug_info = true;
    let ll_file_path = std::env::temp_dir().join("repl.ll");
    roc_build::llvm_passes::optimize_llvm_ir(
        &env,
        target,
        opt_level,
        emit_debug_info,
        &ll_file_path,
    );

    llvm_module_to_dylib(env.module, target, opt_level)
        .map(|lib| (lib, main_fn_name, subs, layout_interner))
}

#[cfg_attr(
    not(all(
        any(target_os = "linux", target_os = "macos"),
        any(target_arch = "x86_64", target_arch = "aarch64")
    )),
    allow(unused)
)]
fn mono_module_to_dylib_asm<'a>(
    arena: &'a Bump,
    target: Target,
    loaded: MonomorphizedModule<'a>,
    _opt_level: OptLevel,
) -> Result<(libloading::Library, &'a str, Subs, STLayoutInterner<'a>), libloading::Error> {
    // let dir = std::env::temp_dir().join("roc_repl");
    let dir = tempfile::tempdir().unwrap();

    let app_o_file = dir.path().join("app.o");

    let MonomorphizedModule {
        module_id,
        procedures,
        host_exposed_lambda_sets: _,
        exposed_to_host,
        mut interns,
        subs,
        mut layout_interner,
        ..
    } = loaded;

    let lazy_literals = true;
    let env = roc_gen_dev::Env {
        arena,
        module_id,
        exposed_to_host: exposed_to_host.top_level_values.keys().copied().collect(),
        lazy_literals,
        mode: roc_gen_dev::AssemblyBackendMode::Repl,
    };

    let module_object =
        roc_gen_dev::build_module(&env, &mut interns, &mut layout_interner, target, procedures);

    let module_out = module_object
        .write()
        .expect("failed to build output object");
    std::fs::write(&app_o_file, module_out).expect("failed to write object to file");

    // TODO make this an environment variable
    if false {
        let file_path = std::env::temp_dir().join("app.o");
        println!("gen-test object file written to {}", file_path.display());
        std::fs::copy(&app_o_file, file_path).unwrap();
    }

    let builtins_host_tempfile =
        roc_bitcode::host_tempfile().expect("failed to write host builtins object to tempfile");

    let (mut child, dylib_path) = roc_build::link::link(
        target,
        app_o_file.clone(),
        // Long term we probably want a smarter way to link in zig builtins.
        // With the current method all methods are kept and it adds about 100k to all outputs.
        &[
            app_o_file.to_str().unwrap(),
            builtins_host_tempfile.path().to_str().unwrap(),
        ],
        roc_build::link::LinkType::Dylib,
    )
    .expect("failed to link dynamic library");

    child.wait().unwrap();

    // Load the dylib
    let path = dylib_path.as_path().to_str().unwrap();

    let lib = unsafe { Library::new(path) }?;

    Ok((lib, "test_main", subs, layout_interner))
}
