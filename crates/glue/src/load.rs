use crate::rust_glue;
use crate::types::{Env, Types};
use bumpalo::Bump;
use inkwell::context::Context;
use roc_collections::MutSet;
use roc_gen_llvm::llvm::build::LlvmBackendMode;
use roc_gen_llvm::llvm::externs::add_default_roc_externs;
use roc_gen_llvm::run_roc::RocCallResult;
use roc_gen_llvm::run_roc_dylib;
use roc_load::{EntryPoint, MonomorphizedModule};
use roc_load::{ExecutionMode, LoadConfig, LoadedModule, LoadingProblem, Threading};
use roc_mono::ir::OptLevel;
use roc_reporting::report::RenderTarget;
use roc_target::{Architecture, OperatingSystem, TargetInfo};
use std::fs::File;
use std::io::{self, ErrorKind, Write};
use std::path::{Path, PathBuf};
use std::process;
use strum::IntoEnumIterator;
use target_lexicon::Triple;

pub fn generate(
    input_path: &Path,
    output_path: &Path,
    opt_script: Option<&Path>,
) -> io::Result<i32> {
    if let Some(script) = opt_script {
        return generate_script(input_path, output_path, script);
    }

    match load_types(input_path.to_path_buf(), Threading::AllAvailable) {
        Ok(types_and_targets) => {
            let mut file = File::create(output_path).unwrap_or_else(|err| {
                eprintln!(
                    "Unable to create output file {} - {:?}",
                    output_path.display(),
                    err
                );

                process::exit(1);
            });

            let mut buf = std::str::from_utf8(rust_glue::HEADER).unwrap().to_string();
            let body = rust_glue::emit(&types_and_targets);

            buf.push_str(&body);

            file.write_all(buf.as_bytes()).unwrap_or_else(|err| {
                eprintln!(
                    "Unable to write bindings to output file {} - {:?}",
                    output_path.display(),
                    err
                );

                process::exit(1);
            });

            println!(
                "🎉 Generated type declarations in:\n\n\t{}",
                output_path.display()
            );

            Ok(0)
        }
        Err(err) => match err.kind() {
            ErrorKind::NotFound => {
                eprintln!("Platform module file not found: {}", input_path.display());
                process::exit(1);
            }
            error => {
                eprintln!(
                    "Error loading platform module file {} - {:?}",
                    input_path.display(),
                    error
                );
                process::exit(1);
            }
        },
    }
}

use core::ffi::c_void;

/// # Safety
/// The Roc application needs this.
#[no_mangle]
pub unsafe fn roc_alloc(size: usize, _alignment: u32) -> *mut c_void {
    libc::malloc(size)
}

/// # Safety
/// The Roc application needs this.
#[no_mangle]
pub unsafe fn roc_realloc(
    c_ptr: *mut c_void,
    new_size: usize,
    _old_size: usize,
    _alignment: u32,
) -> *mut c_void {
    libc::realloc(c_ptr, new_size)
}

/// # Safety
/// The Roc application needs this.
#[no_mangle]
pub unsafe fn roc_dealloc(c_ptr: *mut c_void, _alignment: u32) {
    libc::free(c_ptr)
}

fn generate_script(input_path: &Path, output_path: &Path, script: &Path) -> io::Result<i32> {
    let arena = bumpalo::Bump::new();

    let load_config = LoadConfig {
        target_info: TargetInfo::default_x86_64(),
        render: RenderTarget::ColorTerminal,
        threading: Threading::AllAvailable,
        exec_mode: ExecutionMode::Executable,
    };

    let loaded =
        roc_load::load_and_monomorphize(&arena, script.to_owned(), Default::default(), load_config)
            .unwrap();

    let (lib, main_fn_name) = mono_module_to_dylib(
        &arena,
        Triple::host(),
        loaded,
        OptLevel::Normal,
        LlvmBackendMode::CliTest,
    )
    .unwrap();

    use roc_std::RocStr;
    let main_function = run_roc_dylib!(arena.alloc(lib), main_fn_name, &RocStr, RocStr);

    let mut main_result = RocCallResult::default();
    let input = RocStr::from("foo bar baz");

    unsafe { main_function(&input, &mut main_result) };

    let result: Result<_, _> = main_result.into();

    dbg!(result);

    Ok(0)
}

fn mono_module_to_dylib<'a>(
    arena: &'a Bump,
    target: Triple,
    loaded: MonomorphizedModule<'a>,
    opt_level: OptLevel,
    mode: LlvmBackendMode,
) -> Result<(libloading::Library, String), libloading::Error> {
    let target_info = TargetInfo::from(&target);

    let MonomorphizedModule {
        toplevel_expects,
        procedures,
        entry_point,
        interns,
        ..
    } = loaded;

    let context = Context::create();
    let builder = context.create_builder();
    let module = arena.alloc(roc_gen_llvm::llvm::build::module_from_builtins(
        &target, &context, "",
    ));

    let module = arena.alloc(module);
    let (module_pass, _function_pass) =
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
        mode,
        // important! we don't want any procedures to get the C calling convention
        exposed_to_host: MutSet::default(),
    };

    // Add roc_alloc, roc_realloc, and roc_dealloc, since the repl has no
    // platform to provide them.
    add_default_roc_externs(&env);

    let opt_entry_point = match entry_point {
        EntryPoint::Executable { symbol, layout, .. } => {
            Some(roc_mono::ir::EntryPoint { symbol, layout })
        }
        EntryPoint::Test => None,
    };

    let (main_name, _) = roc_gen_llvm::llvm::build::build_procedures_return_main(
        &env,
        opt_level,
        procedures,
        opt_entry_point.unwrap(),
    );

    env.dibuilder.finalize();

    // we don't use the debug info, and it causes weird errors.
    module.strip_debug_info();

    // Uncomment this to see the module's un-optimized LLVM instruction output:
    // env.module.print_to_stderr();

    module_pass.run_on(env.module);

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    // Verify the module
    if let Err(errors) = env.module.verify() {
        let path = std::env::temp_dir().join("test.ll");
        env.module.print_to_file(&path).unwrap();
        panic!(
            "Errors defining module:\n{}\n\nUncomment things nearby to see more details. IR written to `{:?}`",
            errors.to_string(), path,
        );
    }

    roc_build::link::llvm_module_to_dylib(env.module, &target, opt_level)
        .map(|lib| (lib, main_name.to_owned()))
}

pub fn load_types(
    full_file_path: PathBuf,
    threading: Threading,
) -> Result<Vec<(Types, TargetInfo)>, io::Error> {
    let target_info = (&Triple::host()).into();

    let arena = &Bump::new();
    let subs_by_module = Default::default();
    let LoadedModule {
        module_id: home,
        mut can_problems,
        mut type_problems,
        mut declarations_by_id,
        mut solved,
        mut interns,
        ..
    } = roc_load::load_and_typecheck(
        arena,
        full_file_path,
        subs_by_module,
        LoadConfig {
            target_info,
            render: RenderTarget::Generic,
            threading,
            exec_mode: ExecutionMode::Check,
        },
    )
    .unwrap_or_else(|problem| match problem {
        LoadingProblem::FormattedReport(report) => {
            eprintln!("{}", report);

            process::exit(1);
        }
        problem => {
            todo!("{:?}", problem);
        }
    });

    let decls = declarations_by_id.remove(&home).unwrap();
    let subs = solved.inner_mut();

    let can_problems = can_problems.remove(&home).unwrap_or_default();
    let type_problems = type_problems.remove(&home).unwrap_or_default();

    if !can_problems.is_empty() || !type_problems.is_empty() {
        todo!(
            "Gracefully report compilation problems during glue generation: {:?}, {:?}",
            can_problems,
            type_problems
        );
    }

    let variables = (0..decls.len()).filter_map(|index| {
        use roc_can::expr::DeclarationTag::*;

        match decls.declarations[index] {
            Value | Function(_) | Recursive(_) | TailRecursive(_) => Some(decls.variables[index]),
            Destructure(_) => {
                // figure out if we need to export non-identifier defs - when would that
                // happen?
                None
            }
            MutualRecursion { .. } => {
                // handled by future iterations
                None
            }
            Expectation | ExpectationFx => {
                // not publicly visible
                None
            }
        }
    });

    let types_and_targets = Architecture::iter()
        .map(|arch| {
            let target_info = TargetInfo {
                architecture: arch,
                operating_system: OperatingSystem::Unix,
            };
            let mut env = Env::new(arena, subs, &mut interns, target_info);

            (env.vars_to_types(variables.clone()), target_info)
        })
        .collect();

    Ok(types_and_targets)
}
