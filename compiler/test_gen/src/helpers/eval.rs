use libloading::Library;
use roc_build::link::module_to_dylib;
use roc_build::program::FunctionIterator;
use roc_can::builtins::builtin_defs_map;
use roc_can::def::Def;
use roc_collections::all::{MutMap, MutSet};
use roc_gen_llvm::llvm::externs::add_default_roc_externs;
use roc_module::symbol::Symbol;
use roc_mono::ir::OptLevel;
use roc_types::subs::VarStore;

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer = String::from("app \"test\" provides [ main ] to \"./platform\"\n\nmain =\n");

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}
pub fn test_builtin_defs(symbol: Symbol, var_store: &mut VarStore) -> Option<Def> {
    builtin_defs_map(symbol, var_store)
}

// this is not actually dead code, but only used by cfg_test modules
// so "normally" it is dead, only at testing time is it used
#[allow(dead_code)]
#[inline(never)]
pub fn helper<'a>(
    arena: &'a bumpalo::Bump,
    src: &str,
    stdlib: &'a roc_builtins::std::StdLib,
    leak: bool,
    ignore_problems: bool,
    context: &'a inkwell::context::Context,
) -> (&'static str, String, Library) {
    use std::path::{Path, PathBuf};

    let filename = PathBuf::from("Test.roc");
    let src_dir = Path::new("fake/test/path");

    let module_src;
    let temp;
    if src.starts_with("app") {
        // this is already a module
        module_src = src;
    } else {
        // this is an expression, promote it to a module
        temp = promote_expr_to_module(src);
        module_src = &temp;
    }

    let target = target_lexicon::Triple::host();
    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;

    let exposed_types = MutMap::default();
    let loaded = roc_load::file::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        stdlib,
        src_dir,
        exposed_types,
        ptr_bytes,
        test_builtin_defs,
    );

    let mut loaded = match loaded {
        Ok(x) => x,
        Err(roc_load::file::LoadingProblem::FormattedReport(report)) => {
            println!("{}", report);
            panic!();
        }
        Err(e) => panic!("{:?}", e),
    };

    use roc_load::file::MonomorphizedModule;
    let MonomorphizedModule {
        procedures,
        entry_point,
        interns,
        ..
    } = loaded;

    let mut lines = Vec::new();
    // errors whose reporting we delay (so we can see that code gen generates runtime errors)
    let mut delayed_errors = Vec::new();

    for (home, (module_path, src)) in loaded.sources {
        use roc_reporting::report::{
            can_problem, mono_problem, type_problem, RocDocAllocator, DEFAULT_PALETTE,
        };

        let can_problems = loaded.can_problems.remove(&home).unwrap_or_default();
        let type_problems = loaded.type_problems.remove(&home).unwrap_or_default();
        let mono_problems = loaded.mono_problems.remove(&home).unwrap_or_default();

        let error_count = can_problems.len() + type_problems.len() + mono_problems.len();

        if error_count == 0 {
            continue;
        }

        let src_lines: Vec<&str> = src.split('\n').collect();
        let palette = DEFAULT_PALETTE;

        // Report parsing and canonicalization problems
        let alloc = RocDocAllocator::new(&src_lines, home, &interns);

        use roc_problem::can::Problem::*;
        for problem in can_problems.into_iter() {
            // Ignore "unused" problems
            match problem {
                UnusedDef(_, _)
                | UnusedArgument(_, _, _)
                | UnusedImport(_, _)
                | RuntimeError(_)
                | UnsupportedPattern(_, _)
                | ExposedButNotDefined(_) => {
                    let report = can_problem(&alloc, module_path.clone(), problem);
                    let mut buf = String::new();

                    report.render_color_terminal(&mut buf, &alloc, &palette);

                    delayed_errors.push(buf.clone());
                    lines.push(buf);
                }
                _ => {
                    let report = can_problem(&alloc, module_path.clone(), problem);
                    let mut buf = String::new();

                    report.render_color_terminal(&mut buf, &alloc, &palette);

                    lines.push(buf);
                }
            }
        }

        for problem in type_problems {
            let report = type_problem(&alloc, module_path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            lines.push(buf);
        }

        for problem in mono_problems {
            let report = mono_problem(&alloc, module_path.clone(), problem);
            let mut buf = String::new();

            report.render_color_terminal(&mut buf, &alloc, &palette);

            delayed_errors.push(buf.clone());
            lines.push(buf);
        }
    }

    if !lines.is_empty() {
        println!("{}", lines.join("\n"));

        // only crash at this point if there were no delayed_errors
        if delayed_errors.is_empty() && !ignore_problems {
            assert_eq!(0, 1, "Mistakes were made");
        }
    }

    let builder = context.create_builder();
    let module = roc_gen_llvm::llvm::build::module_from_builtins(context, "app");

    // Add roc_alloc, roc_realloc, and roc_dealloc, since the repl has no
    // platform to provide them.
    add_default_roc_externs(context, &module, &builder, ptr_bytes);

    // strip Zig debug stuff
    module.strip_debug_info();

    let opt_level = if cfg!(debug_assertions) {
        OptLevel::Normal
    } else {
        OptLevel::Optimize
    };

    let module = arena.alloc(module);
    let (module_pass, function_pass) =
        roc_gen_llvm::llvm::build::construct_optimization_passes(module, opt_level);

    let (dibuilder, compile_unit) = roc_gen_llvm::llvm::build::Env::new_debug_info(module);

    // mark our zig-defined builtins as internal
    use inkwell::attributes::{Attribute, AttributeLoc};
    use inkwell::module::Linkage;

    let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
    debug_assert!(kind_id > 0);
    let attr = context.create_enum_attribute(kind_id, 1);

    for function in FunctionIterator::from_module(module) {
        let name = function.get_name().to_str().unwrap();
        if name.starts_with("roc_builtins") {
            function.set_linkage(Linkage::Internal);
        }

        if name.starts_with("roc_builtins.dict") {
            function.add_attribute(AttributeLoc::Function, attr);
        }
    }

    // Compile and add all the Procs before adding main
    let env = roc_gen_llvm::llvm::build::Env {
        arena,
        builder: &builder,
        dibuilder: &dibuilder,
        compile_unit: &compile_unit,
        context,
        interns,
        module,
        ptr_bytes,
        leak,
        // important! we don't want any procedures to get the C calling convention
        exposed_to_host: MutSet::default(),
    };

    let (main_fn_name, main_fn) = roc_gen_llvm::llvm::build::build_procedures_return_main(
        &env,
        opt_level,
        procedures,
        entry_point,
    );

    env.dibuilder.finalize();

    // Uncomment this to see the module's un-optimized LLVM instruction output:
    // env.module.print_to_stderr();

    if main_fn.verify(true) {
        function_pass.run_on(&main_fn);
    } else {
        panic!("Main function {} failed LLVM verification in NON-OPTIMIZED build. Uncomment things nearby to see more details.", main_fn_name);
    }

    module_pass.run_on(env.module);

    // Verify the module
    if let Err(errors) = env.module.verify() {
        panic!("Errors defining module:\n\n{}", errors.to_string());
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    let lib = module_to_dylib(env.module, &target, opt_level)
        .expect("Error loading compiled dylib for test");

    (main_fn_name, delayed_errors.join("\n"), lib)
}

#[macro_export]
macro_rules! assert_llvm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $leak:expr, $ignore_problems:expr) => {
        use bumpalo::Bump;
        use inkwell::context::Context;
        use roc_gen_llvm::run_jit_function;

        let arena = Bump::new();
        let context = Context::create();

        // NOTE the stdlib must be in the arena; just taking a reference will segfault
        let stdlib = arena.alloc(roc_builtins::std::standard_stdlib());

        let (main_fn_name, errors, lib) =
            $crate::helpers::eval::helper(&arena, $src, stdlib, $leak, $ignore_problems, &context);

        let transform = |success| {
            let expected = $expected;
            #[allow(clippy::redundant_closure_call)]
            let given = $transform(success);
            assert_eq!(&given, &expected);
        };
        run_jit_function!(lib, main_fn_name, $ty, transform, errors)
    };

    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        assert_llvm_evals_to!($src, $expected, $ty, $transform, true, false);
    };
}

#[macro_export]
macro_rules! assert_evals_to {
    ($src:expr, $expected:expr, $ty:ty) => {{
        assert_evals_to!($src, $expected, $ty, (|val| val));
    }};
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        // Same as above, except with an additional transformation argument.
        {
            assert_evals_to!($src, $expected, $ty, $transform, true);
        }
    };
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $leak:expr) => {
        // Run un-optimized tests, and then optimized tests, in separate scopes.
        // These each rebuild everything from scratch, starting with
        // parsing the source, so that there's no chance their passing
        // or failing depends on leftover state from the previous one.
        {
            assert_llvm_evals_to!($src, $expected, $ty, $transform, $leak, false);
        }
        {
            // NOTE at the moment, the optimized tests do the same thing
            // assert_opt_evals_to!($src, $expected, $ty, $transform, $leak);
        }
    };
}

#[macro_export]
macro_rules! assert_non_opt_evals_to {
    ($src:expr, $expected:expr, $ty:ty) => {{
        assert_llvm_evals_to!($src, $expected, $ty, (|val| val));
    }};
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        // Same as above, except with an additional transformation argument.
        {
            assert_llvm_evals_to!($src, $expected, $ty, $transform, true, false);
        }
    };
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $leak:expr) => {{
        assert_llvm_evals_to!($src, $expected, $ty, $transform, $leak);
    }};
}
