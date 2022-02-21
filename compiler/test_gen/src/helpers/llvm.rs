use crate::helpers::from_wasmer_memory::FromWasmerMemory;
use inkwell::module::Module;
use libloading::Library;
use roc_build::link::module_to_dylib;
use roc_build::program::FunctionIterator;
use roc_collections::all::{MutMap, MutSet};
use roc_gen_llvm::llvm::externs::add_default_roc_externs;
use roc_mono::ir::OptLevel;
use roc_region::all::LineInfo;
use target_lexicon::Triple;

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

#[allow(clippy::too_many_arguments)]
fn create_llvm_module<'a>(
    arena: &'a bumpalo::Bump,
    src: &str,
    stdlib: &'a roc_builtins::std::StdLib,
    is_gen_test: bool,
    ignore_problems: bool,
    context: &'a inkwell::context::Context,
    target: &Triple,
    opt_level: OptLevel,
) -> (&'static str, String, &'a Module<'a>) {
    use std::path::{Path, PathBuf};

    let target_info = roc_target::TargetInfo::from(target);

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

    let exposed_types = MutMap::default();
    let loaded = roc_load::file::load_and_monomorphize_from_str(
        arena,
        filename,
        module_src,
        stdlib,
        src_dir,
        exposed_types,
        target_info,
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

        let line_info = LineInfo::new(&src);
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
                    let report = can_problem(&alloc, &line_info, module_path.clone(), problem);
                    let mut buf = String::new();

                    report.render_color_terminal(&mut buf, &alloc, &palette);

                    delayed_errors.push(buf.clone());
                    lines.push(buf);
                }
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

        for problem in mono_problems {
            let report = mono_problem(&alloc, &line_info, module_path.clone(), problem);
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
    let module = roc_gen_llvm::llvm::build::module_from_builtins(target, context, "app");

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
        is_gen_test,
        // important! we don't want any procedures to get the C calling convention
        exposed_to_host: MutSet::default(),
    };

    // strip Zig debug stuff
    module.strip_debug_info();

    // Add roc_alloc, roc_realloc, and roc_dealloc, since the repl has no
    // platform to provide them.
    add_default_roc_externs(&env);

    let (main_fn_name, main_fn) = roc_gen_llvm::llvm::build::build_procedures_return_main(
        &env,
        opt_level,
        procedures,
        entry_point,
    );

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

#[allow(dead_code)]
#[inline(never)]
pub fn helper<'a>(
    arena: &'a bumpalo::Bump,
    src: &str,
    stdlib: &'a roc_builtins::std::StdLib,
    is_gen_test: bool,
    ignore_problems: bool,
    context: &'a inkwell::context::Context,
) -> (&'static str, String, Library) {
    let target = target_lexicon::Triple::host();

    let opt_level = if cfg!(debug_assertions) {
        OptLevel::Normal
    } else {
        OptLevel::Optimize
    };

    let (main_fn_name, delayed_errors, module) = create_llvm_module(
        arena,
        src,
        stdlib,
        is_gen_test,
        ignore_problems,
        context,
        &target,
        opt_level,
    );

    let lib =
        module_to_dylib(module, &target, opt_level).expect("Error loading compiled dylib for test");

    (main_fn_name, delayed_errors, lib)
}

fn wasm32_target_tripple() -> Triple {
    use target_lexicon::{Architecture, BinaryFormat};

    let mut triple = Triple::unknown();

    triple.architecture = Architecture::Wasm32;
    triple.binary_format = BinaryFormat::Wasm;

    triple
}

#[allow(dead_code)]
pub fn helper_wasm<'a>(
    arena: &'a bumpalo::Bump,
    src: &str,
    stdlib: &'a roc_builtins::std::StdLib,
    _is_gen_test: bool,
    ignore_problems: bool,
    context: &'a inkwell::context::Context,
) -> wasmer::Instance {
    let target = wasm32_target_tripple();

    let opt_level = if cfg!(debug_assertions) {
        OptLevel::Normal
    } else {
        OptLevel::Optimize
    };

    let is_gen_test = false;
    let (_main_fn_name, _delayed_errors, llvm_module) = create_llvm_module(
        arena,
        src,
        stdlib,
        is_gen_test,
        ignore_problems,
        context,
        &target,
        opt_level,
    );

    use inkwell::targets::{InitializationConfig, Target, TargetTriple};

    let dir = tempfile::tempdir().unwrap();
    let dir_path = dir.path();
    // let zig_global_cache_path = std::path::PathBuf::from("/home/folkertdev/roc/wasm/mess");

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

    use std::process::Command;

    Command::new(&crate::helpers::zig_executable())
        .current_dir(dir_path)
        .args(&[
            "wasm-ld",
            "/home/folkertdev/roc/wasm/libmain.a",
            "/home/folkertdev/roc/wasm/libc.a",
            test_a_path.to_str().unwrap(),
            "-o",
            test_wasm_path.to_str().unwrap(),
            "--export-dynamic",
            "--allow-undefined",
            "--no-entry",
        ])
        .status()
        .unwrap();

    // now, do wasmer stuff

    use wasmer::{Function, Instance, Module, Store};

    let store = Store::default();
    let module = Module::from_file(&store, &test_wasm_path).unwrap();

    // First, we create the `WasiEnv`
    use wasmer_wasi::WasiState;
    let mut wasi_env = WasiState::new("hello")
        // .args(&["world"])
        // .env("KEY", "Value")
        .finalize()
        .unwrap();

    // Then, we get the import object related to our WASI
    // and attach it to the Wasm instance.
    let mut import_object = wasi_env
        .import_object(&module)
        .unwrap_or_else(|_| wasmer::imports!());

    {
        let mut exts = wasmer::Exports::new();

        let main_function = Function::new_native(&store, fake_wasm_main_function);
        let ext = wasmer::Extern::Function(main_function);
        exts.insert("main", ext);

        let main_function = Function::new_native(&store, wasm_roc_panic);
        let ext = wasmer::Extern::Function(main_function);
        exts.insert("roc_panic", ext);

        import_object.register("env", exts);
    }

    Instance::new(&module, &import_object).unwrap()
}

#[allow(dead_code)]
fn wasm_roc_panic(address: u32, tag_id: u32) {
    match tag_id {
        0 => {
            let mut string = "";

            MEMORY.with(|f| {
                let memory = f.borrow().unwrap();

                let ptr: wasmer::WasmPtr<u8, wasmer::Array> = wasmer::WasmPtr::new(address);
                let width = 100;
                let c_ptr = (ptr.deref(memory, 0, width)).unwrap();

                use std::ffi::CStr;
                use std::os::raw::c_char;
                let slice = unsafe { CStr::from_ptr(c_ptr as *const _ as *const c_char) };
                string = slice.to_str().unwrap();
            });

            panic!("Roc failed with message: {:?}", string)
        }
        _ => todo!(),
    }
}

use std::cell::RefCell;

thread_local! {
    pub static MEMORY: RefCell<Option<&'static wasmer::Memory>> = RefCell::new(None);
}

#[allow(dead_code)]
fn fake_wasm_main_function(_: u32, _: u32) -> u32 {
    panic!("wasm entered the main function; this should never happen!")
}

#[allow(dead_code)]
pub fn assert_wasm_evals_to_help<T>(src: &str, ignore_problems: bool) -> Result<T, String>
where
    T: FromWasmerMemory,
{
    let arena = bumpalo::Bump::new();
    let context = inkwell::context::Context::create();

    // NOTE the stdlib must be in the arena; just taking a reference will segfault
    let stdlib = arena.alloc(roc_builtins::std::standard_stdlib());

    let is_gen_test = true;
    let instance = crate::helpers::llvm::helper_wasm(
        &arena,
        src,
        stdlib,
        is_gen_test,
        ignore_problems,
        &context,
    );

    let memory = instance.exports.get_memory("memory").unwrap();

    crate::helpers::llvm::MEMORY.with(|f| {
        *f.borrow_mut() = Some(unsafe { std::mem::transmute(memory) });
    });

    let test_wrapper = instance.exports.get_function("test_wrapper").unwrap();

    match test_wrapper.call(&[]) {
        Err(e) => Err(format!("call to `test_wrapper`: {:?}", e)),
        Ok(result) => {
            let address = result[0].unwrap_i32();

            let output = <T as crate::helpers::llvm::FromWasmerMemory>::decode(
                memory,
                // skip the RocCallResult tag id
                address as u32 + 8,
            );

            Ok(output)
        }
    }
}

#[allow(unused_macros)]
macro_rules! assert_wasm_evals_to {
    ($src:expr, $expected:expr, $ty:ty, $transform:expr, $ignore_problems:expr) => {
        match $crate::helpers::llvm::assert_wasm_evals_to_help::<$ty>($src, $ignore_problems) {
            Err(msg) => panic!("Wasm test failed: {:?}", msg),
            Ok(actual) => {
                #[allow(clippy::bool_assert_comparison)]
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
        use roc_gen_llvm::run_jit_function;

        let arena = Bump::new();
        let context = Context::create();

        // NOTE the stdlib must be in the arena; just taking a reference will segfault
        let stdlib = arena.alloc(roc_builtins::std::standard_stdlib());

        let is_gen_test = true;
        let (main_fn_name, errors, lib) = $crate::helpers::llvm::helper(
            &arena,
            $src,
            stdlib,
            is_gen_test,
            $ignore_problems,
            &context,
        );

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
        assert_evals_to!($src, $expected, $ty, $crate::helpers::llvm::identity);
    }};
    ($src:expr, $expected:expr, $ty:ty, $transform:expr) => {
        // same as above, except with an additional transformation argument.
        {
            #[cfg(feature = "wasm-cli-run")]
            $crate::helpers::llvm::assert_wasm_evals_to!(
                $src, $expected, $ty, $transform, false, false
            );

            $crate::helpers::llvm::assert_llvm_evals_to!($src, $expected, $ty, $transform, false);
        }
    };
}

#[allow(unused_macros)]
macro_rules! assert_expect_failed {
    ($src:expr, $expected:expr, $ty:ty) => {
        use bumpalo::Bump;
        use inkwell::context::Context;
        use roc_gen_llvm::run_jit_function;

        let arena = Bump::new();
        let context = Context::create();

        // NOTE the stdlib must be in the arena; just taking a reference will segfault
        let stdlib = arena.alloc(roc_builtins::std::standard_stdlib());

        let is_gen_test = true;
        let (main_fn_name, errors, lib) =
            $crate::helpers::llvm::helper(&arena, $src, stdlib, is_gen_test, false, &context);

        let transform = |success| {
            let expected = $expected;
            assert_eq!(&success, &expected, "LLVM test failed");
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

macro_rules! expect_runtime_error_panic {
    ($src:expr) => {{
        #[cfg(feature = "wasm-cli-run")]
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
pub(crate) use assert_expect_failed;
#[allow(unused_imports)]
pub(crate) use assert_llvm_evals_to;
#[allow(unused_imports)]
pub(crate) use assert_non_opt_evals_to;
#[allow(unused_imports)]
pub(crate) use assert_wasm_evals_to;
#[allow(unused_imports)]
pub(crate) use expect_runtime_error_panic;
