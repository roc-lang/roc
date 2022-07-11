use std::path::PathBuf;

use inkwell::module::Module;
use libloading::Library;
use roc_build::link::llvm_module_to_dylib;
use roc_build::program::FunctionIterator;
use roc_collections::all::MutSet;
use roc_gen_llvm::llvm::build::LlvmBackendMode;
use roc_gen_llvm::llvm::externs::add_default_roc_externs;
use roc_load::Threading;
use roc_mono::ir::OptLevel;
use roc_region::all::LineInfo;
use roc_reporting::report::RenderTarget;
use target_lexicon::Triple;

#[cfg(feature = "gen-llvm-wasm")]
use crate::helpers::from_wasm32_memory::FromWasm32Memory;

#[cfg(feature = "gen-llvm-wasm")]
use roc_gen_wasm::wasm32_result::Wasm32Result;

#[cfg(feature = "gen-llvm-wasm")]
const TEST_WRAPPER_NAME: &str = "$Test.wasm_test_wrapper";

#[allow(dead_code)]
pub const OPT_LEVEL: OptLevel = if cfg!(debug_assertions) {
    OptLevel::Normal
} else {
    OptLevel::Optimize
};

fn promote_expr_to_module(src: &str) -> String {
    let mut buffer = String::from("app \"test\" provides [main] to \"./platform\"\n\nmain =\n");

    for line in src.lines() {
        // indent the body!
        buffer.push_str("    ");
        buffer.push_str(line);
        buffer.push('\n');
    }

    buffer
}

#[allow(clippy::too_many_arguments)]
fn create_llvm_module<'a>(
    arena: &'a bumpalo::Bump,
    src: &str,
    config: HelperConfig,
    context: &'a inkwell::context::Context,
    target: &Triple,
) -> (&'static str, String, &'a Module<'a>) {
    let target_info = roc_target::TargetInfo::from(target);

    let filename = PathBuf::from("Test.roc");
    let src_dir = PathBuf::from("fake/test/path");

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

    let loaded = roc_load::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        src_dir,
        Default::default(),
        target_info,
        RenderTarget::ColorTerminal,
        Threading::Single,
    );

    let mut loaded = match loaded {
        Ok(x) => x,
        Err(roc_load::LoadingProblem::FormattedReport(report)) => {
            println!("{}", report);
            panic!();
        }
        Err(e) => panic!("{:?}", e),
    };

    use roc_load::MonomorphizedModule;
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
        use roc_reporting::report::{can_problem, type_problem, RocDocAllocator, DEFAULT_PALETTE};

        let can_problems = loaded.can_problems.remove(&home).unwrap_or_default();
        let type_problems = loaded.type_problems.remove(&home).unwrap_or_default();

        let error_count = can_problems.len() + type_problems.len();

        if error_count == 0 {
            continue;
        }

        let line_info = LineInfo::new(&src);
        let src_lines: Vec<&str> = src.split('\n').collect();
        let palette = DEFAULT_PALETTE;

        // Report parsing and canonicalization problems
        let alloc = RocDocAllocator::new(&src_lines, home, &interns);

        use roc_problem::can::Problem::*;
        for problem in can_problems.into_iter() {
            match problem {
                // Ignore "unused" problems
                UnusedDef(_, _)
                | UnusedArgument(_, _, _)
                | UnusedImport(_, _)
                | RuntimeError(_)
                | UnsupportedPattern(_, _)
                | ExposedButNotDefined(_) => {
                    let report = can_problem(&alloc, &line_info, module_path.clone(), problem);
                    let mut buf = String::new();

                    report.render_color_terminal(&mut buf, &alloc, &palette);

                    delayed_errors.push(buf.clone());
                    lines.push(buf);
                }
                // We should be able to compile even when abilities are used as types
                AbilityUsedAsType(..) => {}
                _ => {
                    let report = can_problem(&alloc, &line_info, module_path.clone(), problem);
                    let mut buf = String::new();

                    report.render_color_terminal(&mut buf, &alloc, &palette);

                    lines.push(buf);
                }
            }
        }

        for problem in type_problems {
            if let Some(report) = type_problem(&alloc, &line_info, module_path.clone(), problem) {
                let mut buf = String::new();

                report.render_color_terminal(&mut buf, &alloc, &palette);

                lines.push(buf);
            }
        }
    }

    if !lines.is_empty() {
        println!("{}", lines.join("\n"));

        // only crash at this point if there were no delayed_errors
        if delayed_errors.is_empty() && !config.ignore_problems {
            assert_eq!(0, 1, "Mistakes were made");
        }
    }

    let builder = context.create_builder();
    let module = roc_gen_llvm::llvm::build::module_from_builtins(target, context, "app");

    let module = arena.alloc(module);
    let (module_pass, function_pass) =
        roc_gen_llvm::llvm::build::construct_optimization_passes(module, config.opt_level);

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
            if name.starts_with("roc_builtins.expect") {
                function.set_linkage(Linkage::External);
            } else {
                function.set_linkage(Linkage::Internal);
            }
        }

        if name.starts_with("roc_builtins.dict") {
            function.add_attribute(AttributeLoc::Function, attr);
        }

        if name.starts_with("roc_builtins.list") {
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
        target_info,
        mode: config.mode,
        // important! we don't want any procedures to get the C calling convention
        exposed_to_host: MutSet::default(),
    };

    // strip Zig debug stuff
    module.strip_debug_info();

    // Add roc_alloc, roc_realloc, and roc_dealloc, since the repl has no
    // platform to provide them.
    add_default_roc_externs(&env);

    let (main_fn_name, main_fn) = match config.mode {
        LlvmBackendMode::Binary => unreachable!(),
        LlvmBackendMode::WasmGenTest => roc_gen_llvm::llvm::build::build_wasm_test_wrapper(
            &env,
            config.opt_level,
            procedures,
            entry_point,
        ),
        LlvmBackendMode::GenTest => roc_gen_llvm::llvm::build::build_procedures_return_main(
            &env,
            config.opt_level,
            procedures,
            entry_point,
        ),
    };

    env.dibuilder.finalize();

    // strip all debug info: we don't use it at the moment and causes weird validation issues
    module.strip_debug_info();

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

    (main_fn_name, delayed_errors.join("\n"), env.module)
}

#[derive(Debug, Clone, Copy)]
pub struct HelperConfig {
    pub mode: LlvmBackendMode,
    pub ignore_problems: bool,
    pub add_debug_info: bool,
    pub opt_level: OptLevel,
}

#[allow(dead_code)]
#[inline(never)]
pub fn helper<'a>(
    arena: &'a bumpalo::Bump,
    config: HelperConfig,
    src: &str,
    context: &'a inkwell::context::Context,
) -> (&'static str, String, Library) {
    let target = target_lexicon::Triple::host();

    let (main_fn_name, delayed_errors, module) =
        create_llvm_module(arena, src, config, context, &target);

    let res_lib = if config.add_debug_info {
        let module = annotate_with_debug_info(module, context);
        llvm_module_to_dylib(&module, &target, config.opt_level)
    } else {
        llvm_module_to_dylib(module, &target, config.opt_level)
    };

    let lib = res_lib.expect("Error loading compiled dylib for test");

    (main_fn_name, delayed_errors, lib)
}

fn annotate_with_debug_info<'ctx>(
    module: &Module<'ctx>,
    context: &'ctx inkwell::context::Context,
) -> Module<'ctx> {
    use std::process::Command;

    let app_ll_file = "/tmp/roc-debugir.ll";
    let app_dbg_ll_file = "/tmp/roc-debugir.dbg.ll";
    let app_bc_file = "/tmp/roc-debugir.bc";

    // write the ll code to a file, so we can modify it
    module.print_to_file(&app_ll_file).unwrap();

    // run the debugir https://github.com/vaivaswatha/debugir tool
    match Command::new("debugir")
        .args(&["-instnamer", app_ll_file])
        .output()
    {
        Ok(_) => {}
        Err(error) => {
            use std::io::ErrorKind;
            match error.kind() {
                ErrorKind::NotFound => panic!(
                    r"I could not find the `debugir` tool on the PATH, install it from https://github.com/vaivaswatha/debugir"
                ),
                _ => panic!("{:?}", error),
            }
        }
    }

    Command::new("llvm-as")
        .args(&[app_dbg_ll_file, "-o", app_bc_file])
        .output()
        .unwrap();

    inkwell::module::Module::parse_bitcode_from_path(&app_bc_file, context).unwrap()
}

#[allow(dead_code)]
fn wasm32_target_tripple() -> Triple {
    use target_lexicon::{Architecture, BinaryFormat};

    let mut triple = Triple::unknown();

    triple.architecture = Architecture::Wasm32;
    triple.binary_format = BinaryFormat::Wasm;

    triple
}

#[allow(dead_code)]
fn compile_to_wasm_bytes<'a>(
    arena: &'a bumpalo::Bump,
    config: HelperConfig,
    src: &str,
    context: &'a inkwell::context::Context,
) -> Vec<u8> {
    let target = wasm32_target_tripple();

    let (_main_fn_name, _delayed_errors, llvm_module) =
        create_llvm_module(arena, src, config, context, &target);

    let temp_dir = tempfile::tempdir().unwrap();
    let wasm_file = llvm_module_to_wasm_file(&temp_dir, llvm_module);
    std::fs::read(wasm_file).unwrap()
}

#[allow(dead_code)]
fn llvm_module_to_wasm_file(
    temp_dir: &tempfile::TempDir,
    llvm_module: &inkwell::module::Module,
) -> PathBuf {
    use inkwell::targets::{InitializationConfig, Target, TargetTriple};

    let dir_path = temp_dir.path();

    let test_a_path = dir_path.join("test.a");
    let test_wasm_path = dir_path.join("libmain.wasm");

    Target::initialize_webassembly(&InitializationConfig::default());

    let triple = TargetTriple::create("wasm32-unknown-unknown-wasm");

    llvm_module.set_triple(&triple);
    llvm_module.set_source_file_name("Test.roc");

    let target_machine = Target::from_name("wasm32")
        .unwrap()
        .create_target_machine(
            &triple,
            "",
            "", // TODO: this probably should be TargetMachine::get_host_cpu_features() to enable all features.
            inkwell::OptimizationLevel::None,
            inkwell::targets::RelocMode::Default,
            inkwell::targets::CodeModel::Default,
        )
        .unwrap();

    let file_type = inkwell::targets::FileType::Object;

    target_machine
        .write_to_file(llvm_module, file_type, &test_a_path)
        .unwrap();

    let mut wasm_test_platform = std::env::current_dir().unwrap();
    wasm_test_platform.push("build/wasm_test_platform.wasm");

    use std::process::Command;

    Command::new(&crate::helpers::zig_executable())
        .current_dir(dir_path)
        .args(&[
            "wasm-ld",
            wasm_test_platform.to_str().unwrap(),
            test_a_path.to_str().unwrap(),
            "-o",
            test_wasm_path.to_str().unwrap(),
            "--export-dynamic",
            "--allow-undefined",
            "--no-entry",
        ])
        .status()
        .unwrap();

    test_wasm_path
}

#[allow(dead_code)]
fn fake_wasm_main_function(_: u32, _: u32) -> u32 {
    panic!("wasm entered the main function; this should never happen!")
}

#[cfg(feature = "gen-llvm-wasm")]
pub fn assert_wasm_evals_to_help<T>(src: &str, ignore_problems: bool) -> Result<T, String>
where
    T: FromWasm32Memory + Wasm32Result,
{
    let arena = bumpalo::Bump::new();
    let context = inkwell::context::Context::create();

    let config = HelperConfig {
        mode: LlvmBackendMode::WasmGenTest,
        add_debug_info: false,
        ignore_problems,
        opt_level: OPT_LEVEL,
    };

    let wasm_bytes = compile_to_wasm_bytes(&arena, config, src, &context);

    crate::helpers::wasm::run_wasm_test_bytes::<T>(TEST_WRAPPER_NAME, wasm_bytes)
}

#[allow(unused_macros)]
macro_rules! assert_wasm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $ignore_problems:expr) => {
        match $crate::helpers::llvm::assert_wasm_evals_to_help::<$ty>($src, $ignore_problems) {
            Err(msg) => panic!("Wasm test failed: {}", msg),
            Ok(actual) => {
                assert_eq!($transform(actual), $expected, "Wasm test failed")
            }
        }
    };

    ($src:expr, $expected:expr, $ty:ty) => {
        $crate::helpers::llvm::assert_wasm_evals_to!(
            $src,
            $expected,
            $ty,
            $crate::helpers::llvm::identity,
            false
        );
    };

    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        $crate::helpers::llvm::assert_wasm_evals_to!($src, $expected, $ty, $transform, false);
    };
}

#[allow(unused_macros)]
macro_rules! assert_llvm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $ignore_problems:expr) => {
        use bumpalo::Bump;
        use inkwell::context::Context;
        use roc_gen_llvm::llvm::build::LlvmBackendMode;
        use roc_gen_llvm::run_jit_function;

        let arena = Bump::new();
        let context = Context::create();

        let config = $crate::helpers::llvm::HelperConfig {
            mode: LlvmBackendMode::GenTest,
            add_debug_info: false,
            ignore_problems: $ignore_problems,
            opt_level: $crate::helpers::llvm::OPT_LEVEL,
        };

        let (main_fn_name, errors, lib) =
            $crate::helpers::llvm::helper(&arena, config, $src, &context);

        let transform = |success| {
            let expected = $expected;
            #[allow(clippy::redundant_closure_call)]
            let given = $transform(success);
            assert_eq!(&given, &expected, "LLVM test failed");
        };
        run_jit_function!(lib, main_fn_name, $ty, transform, errors)
    };

    ($src:expr, $expected:expr, $ty:ty) => {
        $crate::helpers::llvm::assert_llvm_evals_to!(
            $src,
            $expected,
            $ty,
            $crate::helpers::llvm::identity,
            false
        );
    };

    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        $crate::helpers::llvm::assert_llvm_evals_to!($src, $expected, $ty, $transform, false);
    };
}

#[allow(unused_macros)]
macro_rules! assert_evals_to {
    ($src:expr, $expected:expr, $ty:ty) => {{
        assert_evals_to!($src, $expected, $ty, $crate::helpers::llvm::identity, false);
    }};
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {{
        // same as above, except with an additional transformation argument.
        assert_evals_to!($src, $expected, $ty, $transform, false);
    }};
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $ignore_problems: expr) => {{
        // same as above, except with ignore_problems.
        #[cfg(feature = "gen-llvm-wasm")]
        $crate::helpers::llvm::assert_wasm_evals_to!(
            $src,
            $expected,
            $ty,
            $transform,
            $ignore_problems
        );

        $crate::helpers::llvm::assert_llvm_evals_to!(
            $src,
            $expected,
            $ty,
            $transform,
            $ignore_problems
        );
    }};
}

macro_rules! expect_runtime_error_panic {
    ($src:expr) => {{
        #[cfg(feature = "gen-llvm-wasm")]
        $crate::helpers::llvm::assert_wasm_evals_to!(
            $src,
            false, // fake value/type for eval
            bool,
            $crate::helpers::llvm::identity,
            true // ignore problems
        );

        $crate::helpers::llvm::assert_llvm_evals_to!(
            $src,
            false, // fake value/type for eval
            bool,
            $crate::helpers::llvm::identity,
            true // ignore problems
        );
    }};
}

#[allow(dead_code)]
pub fn identity<T>(value: T) -> T {
    value
}

#[allow(unused_macros)]
macro_rules! assert_non_opt_evals_to {
    ($src:expr, $expected:expr, $ty:ty) => {{
        $crate::helpers::llvm::assert_llvm_evals_to!(
            $src,
            $expected,
            $ty,
            $crate::helpers::llvm::identity
        );
    }};
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        // Same as above, except with an additional transformation argument.
        {
            $crate::helpers::llvm::assert_llvm_evals_to!($src, $expected, $ty, $transform);
        }
    };
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {{
        $crate::helpers::llvm::assert_llvm_evals_to!($src, $expected, $ty, $transform);
    }};
}

#[allow(unused_imports)]
pub(crate) use assert_evals_to;
#[allow(unused_imports)]
pub(crate) use assert_llvm_evals_to;
#[allow(unused_imports)]
pub(crate) use assert_non_opt_evals_to;
#[allow(unused_imports)]
pub(crate) use assert_wasm_evals_to;
#[allow(unused_imports)]
pub(crate) use expect_runtime_error_panic;
