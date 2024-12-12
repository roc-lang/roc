use std::mem::MaybeUninit;
use std::path::PathBuf;
use std::sync::OnceLock;

use inkwell::module::Module;
use libloading::Library;
use roc_build::link::llvm_module_to_dylib;
use roc_collections::all::MutSet;
use roc_command_utils::zig;
use roc_gen_llvm::llvm::externs::add_default_roc_externs;
use roc_gen_llvm::{llvm::build::LlvmBackendMode, run_roc::RocCallResult};
use roc_load::{
    EntryPoint, ExecutionMode, FunctionKind, LoadConfig, LoadMonomorphizedError, Threading,
};
use roc_mono::ir::{CrashTag, OptLevel, SingleEntryPoint};
use roc_packaging::cache::RocCacheDir;
use roc_region::all::LineInfo;
use roc_reporting::report::{RenderTarget, DEFAULT_PALETTE};
use roc_target::Target;

#[cfg(feature = "gen-llvm-wasm")]
use crate::helpers::from_wasm32_memory::FromWasm32Memory;

#[cfg(feature = "gen-llvm-wasm")]
use roc_gen_wasm::wasm32_result::Wasm32Result;

#[cfg(feature = "gen-llvm-wasm")]
const TEST_WRAPPER_NAME: &str = "test_wrapper";

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

fn create_llvm_module<'a>(
    arena: &'a bumpalo::Bump,
    src: &str,
    config: HelperConfig,
    context: &'a inkwell::context::Context,
    target: Target,
    function_kind: FunctionKind,
) -> (&'static str, String, &'a Module<'a>) {
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

    let load_config = LoadConfig {
        target,
        function_kind,
        render: RenderTarget::ColorTerminal,
        palette: DEFAULT_PALETTE,
        threading: Threading::Single,
        exec_mode: ExecutionMode::Executable,
    };
    let loaded = roc_load::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        src_dir,
        None,
        RocCacheDir::Disallowed,
        load_config,
    );

    let mut loaded = match loaded {
        Ok(x) => x,
        Err(LoadMonomorphizedError::LoadingProblem(roc_load::LoadingProblem::FormattedReport(
            report,
            _,
        ))) => {
            println!("{report}");
            panic!();
        }
        Err(e) => panic!("{e:?}"),
    };

    use roc_load::MonomorphizedModule;
    let MonomorphizedModule {
        procedures,
        host_exposed_lambda_sets,
        interns,
        layout_interner,
        ..
    } = loaded;

    let mut lines = Vec::new();
    // errors whose reporting we delay (so we can see that code gen generates runtime errors)
    let mut delayed_errors = Vec::new();

    for (home, (module_path, src)) in loaded.sources {
        use roc_reporting::report::{can_problem, type_problem, RocDocAllocator};

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
                | UnusedArgument(_, _, _, _)
                | UnusedModuleImport(_, _)
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

    let (dibuilder, compile_unit) = roc_gen_llvm::llvm::build::Env::new_debug_info(module);

    // mark our zig-defined builtins as internal
    use inkwell::attributes::{Attribute, AttributeLoc};
    use inkwell::module::Linkage;

    let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
    debug_assert!(kind_id > 0);
    let attr = context.create_enum_attribute(kind_id, 0);

    for function in module.get_functions() {
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
        target,
        mode: config.mode,
        // important! we don't want any procedures to get the C calling convention
        exposed_to_host: MutSet::default(),
    };

    // Add roc_alloc, roc_realloc, and roc_dealloc, since the repl has no
    // platform to provide them.
    add_default_roc_externs(&env);

    let entry_point = match loaded.entry_point {
        EntryPoint::Executable {
            exposed_to_host,
            platform_path: _,
        } => {
            // TODO support multiple of these!
            debug_assert_eq!(exposed_to_host.len(), 1);
            let (name, symbol, layout) = exposed_to_host[0];

            SingleEntryPoint {
                name,
                symbol,
                layout,
            }
        }
        EntryPoint::Test => {
            unreachable!()
        }
    };
    let (main_fn_name, _main_fn) = match config.mode {
        LlvmBackendMode::Binary => unreachable!(),
        LlvmBackendMode::BinaryWithExpect => unreachable!(),
        LlvmBackendMode::BinaryGlue => unreachable!(),
        LlvmBackendMode::CliTest => unreachable!(),
        LlvmBackendMode::WasmGenTest => roc_gen_llvm::llvm::build::build_wasm_test_wrapper(
            &env,
            &layout_interner,
            config.opt_level,
            procedures,
            entry_point,
        ),
        LlvmBackendMode::GenTest => roc_gen_llvm::llvm::build::build_procedures_return_main(
            &env,
            &layout_interner,
            config.opt_level,
            procedures,
            host_exposed_lambda_sets,
            entry_point,
        ),
    };

    env.dibuilder.finalize();

    let ll_file_path = std::env::temp_dir().join("test.ll");
    let opt_level = OptLevel::Development;
    let emit_debug_info = true;
    roc_build::llvm_passes::optimize_llvm_ir(
        &env,
        target,
        opt_level,
        emit_debug_info,
        &ll_file_path,
    );

    if let Ok(path) = std::env::var("ROC_DEBUG_LLVM") {
        env.module.print_to_file(path).unwrap();
    }

    let delayed_errors = if config.ignore_problems {
        String::new()
    } else {
        delayed_errors.join("\n")
    };
    (main_fn_name, delayed_errors, env.module)
}

#[derive(Debug, Clone, Copy)]
pub struct HelperConfig {
    pub mode: LlvmBackendMode,
    pub ignore_problems: bool,
    pub emit_debug_info: bool,
    pub opt_level: OptLevel,
}

#[allow(dead_code)]
#[inline(never)]
pub fn helper<'a>(
    arena: &'a bumpalo::Bump,
    config: HelperConfig,
    src: &str,
    context: &'a inkwell::context::Context,
    function_kind: FunctionKind,
) -> (&'static str, String, Library) {
    let target = target_lexicon::Triple::host().into();

    let (main_fn_name, delayed_errors, module) =
        create_llvm_module(arena, src, config, context, target, function_kind);

    // for debugging:
    //module.print_to_file(std::path::Path::new("/home/username/roc/llvm_ir.ll")).unwrap();

    if !config.emit_debug_info {
        module.strip_debug_info();
    }
    let res_lib = llvm_module_to_dylib(module, target, config.opt_level);

    let lib = res_lib.expect("Error loading compiled dylib for test");

    (main_fn_name, delayed_errors, lib)
}

#[allow(dead_code)]
fn write_final_wasm() -> bool {
    #[allow(unused_imports)]
    use roc_debug_flags::{dbg_do, ROC_WRITE_FINAL_WASM};

    dbg_do!(ROC_WRITE_FINAL_WASM, {
        return true;
    });

    false
}

#[allow(dead_code)]
fn compile_to_wasm_bytes<'a>(
    arena: &'a bumpalo::Bump,
    config: HelperConfig,
    src: &str,
    context: &'a inkwell::context::Context,
    function_kind: FunctionKind,
) -> Vec<u8> {
    // globally cache the temporary directory
    static TEMP_DIR: OnceLock<tempfile::TempDir> = OnceLock::new();
    let temp_dir = TEMP_DIR.get_or_init(|| tempfile::tempdir().unwrap());

    let target = Target::Wasm32;

    let (_main_fn_name, _delayed_errors, llvm_module) =
        create_llvm_module(arena, src, config, context, target, function_kind);

    let content_hash = crate::helpers::src_hash(src);
    let wasm_file = llvm_module_to_wasm_file(temp_dir, content_hash, llvm_module);
    let compiled_bytes = std::fs::read(wasm_file).unwrap();

    if write_final_wasm() {
        crate::helpers::save_wasm_file(&compiled_bytes, content_hash)
    };

    compiled_bytes
}

#[allow(dead_code)]
fn llvm_module_to_wasm_file(
    temp_dir: &tempfile::TempDir,
    content_hash: u64,
    llvm_module: &inkwell::module::Module,
) -> PathBuf {
    use inkwell::targets::{InitializationConfig, Target, TargetTriple};

    let dir_path = temp_dir.path();

    let test_a_path = dir_path.join(format!("test_{content_hash}.a"));
    let test_wasm_path = dir_path.join(format!("libmain_{content_hash}.wasm"));

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

    let output = zig()
        .current_dir(dir_path)
        .args([
            "wasm-ld",
            concat!(env!("OUT_DIR"), "/wasm_test_platform.wasm"),
            test_a_path.to_str().unwrap(),
            "-o",
            test_wasm_path.to_str().unwrap(),
            "--export-dynamic",
            "--allow-undefined",
            "--no-entry",
        ])
        .output()
        .unwrap();

    if !output.stderr.is_empty() {
        let msg = String::from_utf8_lossy(&output.stderr);

        if msg.contains("wasm-ld: error: unknown file type") {
            panic!("{msg}\nThis can happen if multiple tests have the same input string");
        } else {
            panic!("{}", msg);
        }
    }

    assert!(output.status.success(), "{output:#?}");
    assert!(output.stdout.is_empty(), "{output:#?}");

    test_wasm_path
}

#[allow(dead_code)]
fn fake_wasm_main_function(_: u32, _: u32) -> u32 {
    panic!("wasm entered the main function; this should never happen!")
}

#[cfg(feature = "gen-llvm-wasm")]
pub fn assert_wasm_evals_to_help<T>(
    src: &str,
    ignore_problems: bool,
    function_kind: FunctionKind,
) -> Result<T, String>
where
    T: FromWasm32Memory + Wasm32Result,
{
    let arena = bumpalo::Bump::new();
    let context = inkwell::context::Context::create();

    let config = HelperConfig {
        mode: LlvmBackendMode::WasmGenTest,
        emit_debug_info: false,
        ignore_problems,
        opt_level: OPT_LEVEL,
    };

    let wasm_bytes = compile_to_wasm_bytes(&arena, config, src, &context, function_kind);

    crate::helpers::wasm::run_wasm_test_bytes::<T>(TEST_WRAPPER_NAME, wasm_bytes)
}

#[cfg(feature = "gen-llvm-wasm")]
macro_rules! assert_wasm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $ignore_problems:expr) => {
        match $crate::helpers::llvm::assert_wasm_evals_to_help::<$ty>(
            $src,
            $ignore_problems,
            roc_load::FunctionKind::LambdaSet,
        ) {
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

#[allow(dead_code)]
pub fn try_run_lib_function<T>(
    main_fn_name: &str,
    lib: &libloading::Library,
) -> Result<T, (String, CrashTag)> {
    unsafe {
        let main: libloading::Symbol<unsafe extern "C" fn(*mut RocCallResult<T>)> = lib
            .get(main_fn_name.as_bytes())
            .ok()
            .ok_or(format!("Unable to JIT compile `{main_fn_name}`"))
            .expect("errored");

        let mut main_result = MaybeUninit::uninit();
        main(main_result.as_mut_ptr());

        main_result.assume_init().into()
    }
}

#[allow(dead_code)]
// only used in tests
#[allow(dead_code)]
pub(crate) fn llvm_evals_to<T, U, F>(
    src: &str,
    expected: U,
    transform: F,
    ignore_problems: bool,
    function_kind: FunctionKind,
) where
    U: PartialEq + std::fmt::Debug,
    F: FnOnce(T) -> U,
{
    use bumpalo::Bump;
    use inkwell::context::Context;

    let arena = Bump::new();
    let context = Context::create();

    let config = crate::helpers::llvm::HelperConfig {
        mode: LlvmBackendMode::GenTest,
        emit_debug_info: false,
        ignore_problems,
        opt_level: crate::helpers::llvm::OPT_LEVEL,
    };

    let (main_fn_name, errors, lib) =
        crate::helpers::llvm::helper(&arena, config, src, &context, function_kind);

    let result = crate::helpers::llvm::try_run_lib_function::<T>(main_fn_name, &lib);

    match result {
        Ok(raw) => {
            // only if there are no exceptions thrown, check for errors
            assert!(errors.is_empty(), "Encountered errors:\n{errors}");

            #[allow(clippy::redundant_closure_call)]
            let given = transform(raw);
            assert_eq!(&given, &expected, "LLVM test failed");

            // on Windows, there are issues with the drop instances of some roc_std
            #[cfg(windows)]
            std::mem::forget(given);
        }
        Err((msg, tag)) => match tag {
            CrashTag::Roc => panic!(r#"Roc failed with message: "{msg}""#),
            CrashTag::User => panic!(r#"User crash with message: "{msg}""#),
        },
    }
}

#[allow(unused_macros)]
macro_rules! assert_llvm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $ignore_problems:expr) => {
        crate::helpers::llvm::llvm_evals_to::<$ty, _, _>(
            $src,
            $expected,
            $transform,
            $ignore_problems,
            roc_load::FunctionKind::LambdaSet,
        );
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

// windows testing code
//   let mut target = target_lexicon::Triple::host();
//
//   target.operating_system = target_lexicon::OperatingSystem::Windows;
//
//   let (_main_fn_name, _delayed_errors, _module) =
//       $crate::helpers::llvm::create_llvm_module(&arena, $src, config, &context, &target);
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

        #[cfg(not(feature = "gen-llvm-wasm"))]
        $crate::helpers::llvm::assert_llvm_evals_to!(
            $src,
            $expected,
            $ty,
            $transform,
            $ignore_problems
        );
    }};
}

#[allow(unused_macros)]
macro_rules! assert_evals_to_erased {
    ($src:expr, $expected:expr, $ty:ty) => {{
        crate::helpers::llvm::llvm_evals_to::<$ty, _, _>(
            $src,
            $expected,
            $crate::helpers::llvm::identity,
            false,
            roc_load::FunctionKind::Erased,
        );
    }};
}

#[allow(dead_code)]
pub fn identity<T>(value: T) -> T {
    value
}

#[allow(unused_imports)]
pub(crate) use assert_evals_to;
#[allow(unused_imports)]
pub(crate) use assert_evals_to_erased;
#[allow(unused_imports)]
pub(crate) use assert_llvm_evals_to;
#[cfg(feature = "gen-llvm-wasm")]
pub(crate) use assert_wasm_evals_to;
