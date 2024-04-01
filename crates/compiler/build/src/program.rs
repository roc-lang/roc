use crate::link::{
    legacy_host_file, link, preprocess_host_wasm32, rebuild_host, LinkType, LinkingStrategy,
};
use bumpalo::Bump;
use inkwell::memory_buffer::MemoryBuffer;
use roc_error_macros::internal_error;
use roc_gen_dev::AssemblyBackendMode;
use roc_gen_llvm::llvm::build::{module_from_builtins, LlvmBackendMode};
use roc_gen_llvm::llvm::externs::add_default_roc_externs;
use roc_load::{
    EntryPoint, ExecutionMode, ExpectMetadata, FunctionKind, LoadConfig, LoadMonomorphizedError,
    LoadedModule, LoadingProblem, MonomorphizedModule, Threading,
};
use roc_mono::ir::{OptLevel, SingleEntryPoint};
use roc_packaging::cache::RocCacheDir;
use roc_reporting::{
    cli::{report_problems, Problems},
    report::{RenderTarget, DEFAULT_PALETTE},
};
use roc_target::{Architecture, Target};
use std::ffi::OsStr;
use std::ops::Deref;
use std::{
    path::{Path, PathBuf},
    thread::JoinHandle,
    time::{Duration, Instant},
};

#[cfg(feature = "target-wasm32")]
use roc_collections::all::MutSet;

pub const DEFAULT_ROC_FILENAME: &str = "main.roc";

#[derive(Debug, Clone, Copy, Default)]
pub struct CodeGenTiming {
    pub generate_final_ir: Duration,
    pub code_gen_object: Duration,
    pub total: Duration,
}

pub fn report_problems_monomorphized(loaded: &mut MonomorphizedModule) -> Problems {
    report_problems(
        &loaded.sources,
        &loaded.interns,
        &mut loaded.can_problems,
        &mut loaded.type_problems,
    )
}

pub fn report_problems_typechecked(loaded: &mut LoadedModule) -> Problems {
    report_problems(
        &loaded.sources,
        &loaded.interns,
        &mut loaded.can_problems,
        &mut loaded.type_problems,
    )
}

pub enum CodeObject {
    MemoryBuffer(MemoryBuffer),
    Vector(Vec<u8>),
}

impl Deref for CodeObject {
    type Target = [u8];

    fn deref(&self) -> &Self::Target {
        match self {
            CodeObject::MemoryBuffer(memory_buffer) => memory_buffer.as_slice(),
            CodeObject::Vector(vector) => vector.as_slice(),
        }
    }
}

#[derive(Debug, Clone, Copy)]
pub enum CodeGenBackend {
    Assembly(AssemblyBackendMode),
    Llvm(LlvmBackendMode),
    Wasm,
}

#[derive(Debug, Clone, Copy)]
pub struct CodeGenOptions {
    pub backend: CodeGenBackend,
    pub opt_level: OptLevel,
    pub emit_debug_info: bool,
    pub emit_llvm_ir: bool,
    pub fuzz: bool,
}

type GenFromMono<'a> = (CodeObject, CodeGenTiming, ExpectMetadata<'a>);

#[allow(clippy::too_many_arguments)]
pub fn gen_from_mono_module<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    roc_file_path: &Path,
    target: Target,
    code_gen_options: CodeGenOptions,
    preprocessed_host_path: &Path,
    wasm_dev_stack_bytes: Option<u32>,
) -> GenFromMono<'a> {
    let path = roc_file_path;
    let debug = code_gen_options.emit_debug_info;
    let emit_llvm_ir = code_gen_options.emit_llvm_ir;
    let fuzz = code_gen_options.fuzz;
    let opt = code_gen_options.opt_level;

    match code_gen_options.backend {
        CodeGenBackend::Wasm => gen_from_mono_module_dev(
            arena,
            loaded,
            target,
            preprocessed_host_path,
            wasm_dev_stack_bytes,
            AssemblyBackendMode::Binary, // dummy value, unused in practice
        ),
        CodeGenBackend::Assembly(backend_mode) => gen_from_mono_module_dev(
            arena,
            loaded,
            target,
            preprocessed_host_path,
            wasm_dev_stack_bytes,
            backend_mode,
        ),
        CodeGenBackend::Llvm(backend_mode) => gen_from_mono_module_llvm(
            arena,
            loaded,
            path,
            target,
            opt,
            backend_mode,
            debug,
            emit_llvm_ir,
            fuzz,
        ),
    }
}

// TODO how should imported modules factor into this? What if those use builtins too?
// TODO this should probably use more helper functions
// TODO make this polymorphic in the llvm functions so it can be reused for another backend.
#[allow(clippy::too_many_arguments)]
fn gen_from_mono_module_llvm<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    roc_file_path: &Path,
    target: Target,
    opt_level: OptLevel,
    backend_mode: LlvmBackendMode,
    emit_debug_info: bool,
    emit_llvm_ir: bool,
    fuzz: bool,
) -> GenFromMono<'a> {
    use crate::target::{self, convert_opt_level};
    use inkwell::attributes::{Attribute, AttributeLoc};
    use inkwell::context::Context;
    use inkwell::module::Linkage;
    use inkwell::targets::{FileType, RelocMode};

    let all_code_gen_start = Instant::now();

    // Generate the binary
    let context = Context::create();
    let module = arena.alloc(module_from_builtins(target, &context, "app"));

    let app_ll_file = {
        let mut roc_file_path_buf = PathBuf::from(roc_file_path);
        roc_file_path_buf.set_extension("ll");

        roc_file_path_buf
    };

    let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
    debug_assert!(kind_id > 0);
    let enum_attr = context.create_enum_attribute(kind_id, 0);

    for function in module.get_functions() {
        let name = function.get_name().to_str().unwrap();

        // mark our zig-defined builtins as internal
        if name.starts_with("roc_builtins") {
            function.set_linkage(Linkage::Internal);
        }

        if name.starts_with("roc_builtins.dict")
            || name.starts_with("roc_builtins.list")
            || name.starts_with("roc_builtins.dec")
            || name.starts_with("list.RocList")
            || name.starts_with("dict.RocDict")
            || name.contains("incref")
            || name.contains("decref")
        {
            function.add_attribute(AttributeLoc::Function, enum_attr);
        }
    }

    let builder = context.create_builder();
    let (dibuilder, compile_unit) = roc_gen_llvm::llvm::build::Env::new_debug_info(module);
    let (mpm, _fpm) = roc_gen_llvm::llvm::build::construct_optimization_passes(module, opt_level);

    // Compile and add all the Procs before adding main
    let env = roc_gen_llvm::llvm::build::Env {
        arena,
        builder: &builder,
        dibuilder: &dibuilder,
        compile_unit: &compile_unit,
        context: &context,
        interns: loaded.interns,
        module,
        target,
        mode: backend_mode,

        exposed_to_host: loaded
            .exposed_to_host
            .top_level_values
            .keys()
            .copied()
            .collect(),
    };

    // does not add any externs for this mode (we have a host) but cleans up some functions around
    // expects that would confuse the surgical linker
    add_default_roc_externs(&env);

    let entry_point = match loaded.entry_point {
        EntryPoint::Executable {
            exposed_to_host,
            platform_path: _,
        } => {
            // TODO support multiple of these!
            debug_assert_eq!(exposed_to_host.len(), 1);
            let (symbol, layout) = exposed_to_host[0];

            roc_mono::ir::EntryPoint::Single(SingleEntryPoint { symbol, layout })
        }
        EntryPoint::Test => roc_mono::ir::EntryPoint::Expects { symbols: &[] },
    };

    roc_gen_llvm::llvm::build::build_procedures(
        &env,
        &loaded.layout_interner,
        opt_level,
        loaded.procedures,
        loaded.host_exposed_lambda_sets,
        entry_point,
        Some(&app_ll_file),
        &loaded.glue_layouts,
    );

    // We are now finished building the LLVM IR.
    let generate_final_ir = all_code_gen_start.elapsed();
    let code_gen_object_start = Instant::now();

    env.dibuilder.finalize();

    if !emit_debug_info {
        module.strip_debug_info();
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    mpm.run_on(module);

    // Verify the module
    if let Err(errors) = env.module.verify() {
        // write the ll code to a file, so we can modify it
        env.module.print_to_file(&app_ll_file).unwrap();

        internal_error!(
            "😱 LLVM errors when defining module; I wrote the full LLVM IR to {:?}\n\n {}",
            app_ll_file,
            errors.to_string(),
        );
    }

    // Uncomment this to see the module's optimized LLVM instruction output:
    // env.module.print_to_stderr();

    let gen_sanitizers = cfg!(feature = "sanitizers") && std::env::var("ROC_SANITIZERS").is_ok();
    let memory_buffer = if fuzz || gen_sanitizers {
        let dir = tempfile::tempdir().unwrap();
        let dir = dir.into_path();

        let temp_app_ll_file = dir.join("app.ll");
        let temp_app_processed_file = dir.join("app_processed.ll"); // app.ll with llvm passes applied
        let temp_app_processed_file_str = temp_app_processed_file.to_str().unwrap().to_owned();
        let temp_app_o_file = dir.join("app.o");

        // write the ll code to a file, so we can modify it
        module.print_to_file(&temp_app_ll_file).unwrap();

        // Apply coverage passes.
        // Note, this is specifically tailored for `cargo afl` and afl++.
        // It most likely will not work with other fuzzer setups without modification.
        let mut passes = vec![];
        let mut extra_args = vec![];
        let mut unrecognized = vec![];
        if fuzz {
            passes.push("sancov-module");
            extra_args.extend_from_slice(&[
                "-sanitizer-coverage-level=4",
                "-sanitizer-coverage-inline-8bit-counters",
                "-sanitizer-coverage-pc-table",
                "-sanitizer-coverage-trace-compares",
            ]);
        }
        if gen_sanitizers {
            for sanitizer in std::env::var("ROC_SANITIZERS")
                .unwrap()
                .split(',')
                .map(|x| x.trim())
            {
                match sanitizer {
                    "address" => passes.push("asan-module"),
                    "memory" => passes.push("msan-module"),
                    "thread" => passes.push("tsan-module"),
                    x => unrecognized.push(x.to_owned()),
                }
            }
        }
        if !unrecognized.is_empty() {
            let out = unrecognized
                .iter()
                .map(|x| format!("{x:?}"))
                .collect::<Vec<String>>()
                .join(", ");
            eprintln!("Unrecognized sanitizer: {out}\nSupported options are \"address\", \"memory\", \"thread\", \"cargo-fuzz\", and \"afl.rs\".");
            eprintln!("Note: \"cargo-fuzz\" and \"afl.rs\" both enable sanitizer coverage for fuzzing. They just use different parameters to match the respective libraries.")
        }

        use std::process::Command;

        // apply passes to app.ll
        let mut opt_command = Command::new("opt");

        opt_command
            .args([
                temp_app_ll_file.to_str().unwrap(),
                "-o",
                &temp_app_processed_file_str,
            ])
            .args(extra_args);
        if !passes.is_empty() {
            opt_command.arg(format!("-passes={}", passes.join(",")));
        }

        let opt_output = opt_command.output().unwrap();

        assert!(opt_output.stderr.is_empty(), "{opt_output:#?}");

        if emit_llvm_ir {
            eprintln!("Emitting LLVM IR to {}", &app_ll_file.display());

            std::fs::copy(temp_app_processed_file, app_ll_file).unwrap();
        }

        // write the .o file. Note that this builds the .o for the local machine,
        // and ignores the `target_machine` entirely.
        //
        // different systems name this executable differently, so we shotgun for
        // the most common ones and then give up.
        let bc_to_object_output = Command::new("llc")
            .args([
                "-relocation-model=pic",
                "-filetype=obj",
                &temp_app_processed_file_str,
                "-o",
                temp_app_o_file.to_str().unwrap(),
            ])
            .output()
            .unwrap();

        assert!(
            bc_to_object_output.status.success(),
            "{bc_to_object_output:#?}"
        );

        MemoryBuffer::create_from_file(&temp_app_o_file).expect("memory buffer creation works")
    } else {
        if emit_llvm_ir {
            eprintln!("Emitting LLVM IR to {}", &app_ll_file.display());
            module.print_to_file(&app_ll_file).unwrap();
        }

        // Emit the .o file
        match target.architecture() {
            Architecture::X86_64 | Architecture::X86_32 | Architecture::Aarch64 => {
                let reloc = RelocMode::PIC;
                let target_machine =
                    target::target_machine(target, convert_opt_level(opt_level), reloc).unwrap();

                target_machine
                    .write_to_memory_buffer(env.module, FileType::Object)
                    .expect("Writing .o file failed")
            }
            Architecture::Wasm32 => {
                // Useful for debugging
                // module.print_to_file(app_ll_file);
                module.write_bitcode_to_memory()
            }
            _ => internal_error!(
                "TODO gracefully handle unsupported architecture: {:?}",
                target.architecture()
            ),
        }
    };

    let code_gen_object = code_gen_object_start.elapsed();
    let total = all_code_gen_start.elapsed();

    (
        CodeObject::MemoryBuffer(memory_buffer),
        CodeGenTiming {
            generate_final_ir,
            code_gen_object,
            total,
        },
        ExpectMetadata {
            interns: env.interns,
            layout_interner: loaded.layout_interner,
            expectations: loaded.expectations,
        },
    )
}

#[cfg(feature = "target-wasm32")]
fn gen_from_mono_module_dev<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    target: Target,
    preprocessed_host_path: &Path,
    wasm_dev_stack_bytes: Option<u32>,
    backend_mode: AssemblyBackendMode,
) -> GenFromMono<'a> {
    match target.architecture() {
        Architecture::Wasm32 => gen_from_mono_module_dev_wasm32(
            arena,
            loaded,
            preprocessed_host_path,
            wasm_dev_stack_bytes,
        ),
        Architecture::X86_64 | Architecture::Aarch64 => {
            gen_from_mono_module_dev_assembly(arena, loaded, target, backend_mode)
        }
        _ => todo!(),
    }
}

#[cfg(not(feature = "target-wasm32"))]
pub fn gen_from_mono_module_dev<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    target: Target,
    _host_input_path: &Path,
    _wasm_dev_stack_bytes: Option<u32>,
    backend_mode: AssemblyBackendMode,
) -> GenFromMono<'a> {
    match target.architecture() {
        Architecture::X86_64 | Architecture::Aarch64 => {
            gen_from_mono_module_dev_assembly(arena, loaded, target, backend_mode)
        }
        _ => todo!(),
    }
}

#[cfg(feature = "target-wasm32")]
fn gen_from_mono_module_dev_wasm32<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    preprocessed_host_path: &Path,
    wasm_dev_stack_bytes: Option<u32>,
) -> GenFromMono<'a> {
    let all_code_gen_start = Instant::now();
    let MonomorphizedModule {
        module_id,
        procedures,
        mut interns,
        mut layout_interner,
        ..
    } = loaded;

    let exposed_to_host = loaded
        .exposed_to_host
        .top_level_values
        .keys()
        .copied()
        .collect::<MutSet<_>>();

    let env = roc_gen_wasm::Env {
        arena,
        module_id,
        exposed_to_host,
        stack_bytes: wasm_dev_stack_bytes.unwrap_or(roc_gen_wasm::Env::DEFAULT_STACK_BYTES),
    };

    let host_bytes = std::fs::read(preprocessed_host_path).unwrap_or_else(|_| {
        internal_error!(
            "Failed to read host object file {}! Try omitting --prebuilt-platform",
            preprocessed_host_path.display()
        )
    });

    let host_module = roc_gen_wasm::parse_host(arena, &host_bytes).unwrap_or_else(|e| {
        internal_error!(
            "I ran into a problem with the host object file, {} at offset 0x{:x}:\n{}",
            preprocessed_host_path.display(),
            e.offset,
            e.message
        )
    });

    let final_binary_bytes = roc_gen_wasm::build_app_binary(
        &env,
        &mut layout_interner,
        &mut interns,
        host_module,
        procedures,
    );

    let generate_final_ir = all_code_gen_start.elapsed();
    let code_gen_object_start = Instant::now();
    let code_gen_object = code_gen_object_start.elapsed();
    let total = all_code_gen_start.elapsed();

    (
        CodeObject::Vector(final_binary_bytes),
        CodeGenTiming {
            generate_final_ir,
            code_gen_object,
            total,
        },
        ExpectMetadata {
            interns,
            layout_interner,
            expectations: loaded.expectations,
        },
    )
}

fn gen_from_mono_module_dev_assembly<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    target: Target,
    backend_mode: AssemblyBackendMode,
) -> GenFromMono<'a> {
    let all_code_gen_start = Instant::now();

    let lazy_literals = true;

    let MonomorphizedModule {
        module_id,
        procedures,
        mut interns,
        exposed_to_host,
        mut layout_interner,
        ..
    } = loaded;

    let env = roc_gen_dev::Env {
        arena,
        module_id,
        exposed_to_host: exposed_to_host.top_level_values.keys().copied().collect(),
        lazy_literals,
        mode: backend_mode,
    };

    let module_object =
        roc_gen_dev::build_module(&env, &mut interns, &mut layout_interner, target, procedures);

    let generate_final_ir = all_code_gen_start.elapsed();
    let code_gen_object_start = Instant::now();

    let module_out = module_object
        .write()
        .expect("failed to build output object");

    let code_gen_object = code_gen_object_start.elapsed();
    let total = all_code_gen_start.elapsed();

    (
        CodeObject::Vector(module_out),
        CodeGenTiming {
            generate_final_ir,
            code_gen_object,
            total,
        },
        ExpectMetadata {
            interns,
            layout_interner,
            expectations: loaded.expectations,
        },
    )
}

fn report_timing(buf: &mut String, label: &str, duration: Duration) {
    use std::fmt::Write;

    writeln!(
        buf,
        "        {:9.3} ms   {}",
        duration.as_secs_f64() * 1000.0,
        label,
    )
    .unwrap()
}

pub struct BuiltFile<'a> {
    pub binary_path: PathBuf,
    pub problems: Problems,
    pub total_time: Duration,
    pub expect_metadata: ExpectMetadata<'a>,
}

pub enum BuildOrdering {
    /// Run up through typechecking first; continue building iff that is successful.
    BuildIfChecks,
    /// Always build the Roc binary, even if there are type errors.
    AlwaysBuild,
}

#[derive(Debug)]
#[allow(clippy::large_enum_variant)]
pub enum BuildFileError<'a> {
    LoadingProblem(LoadingProblem<'a>),
    ErrorModule {
        module: LoadedModule,
        total_time: Duration,
    },
}

impl<'a> BuildFileError<'a> {
    fn from_mono_error(error: LoadMonomorphizedError<'a>, compilation_start: Instant) -> Self {
        match error {
            LoadMonomorphizedError::LoadingProblem(problem) => {
                BuildFileError::LoadingProblem(problem)
            }
            LoadMonomorphizedError::ErrorModule(module) => BuildFileError::ErrorModule {
                module,
                total_time: compilation_start.elapsed(),
            },
        }
    }
}

pub fn handle_error_module(
    mut module: roc_load::LoadedModule,
    total_time: std::time::Duration,
    filename: &OsStr,
    print_run_anyway_hint: bool,
) -> std::io::Result<i32> {
    debug_assert!(module.total_problems() > 0);

    let problems = report_problems_typechecked(&mut module);

    problems.print_error_warning_count(total_time);

    if print_run_anyway_hint {
        // If you're running "main.roc" then you can just do `roc run`
        // to re-run the program.
        print!(".\n\nYou can run the program anyway with \x1B[32mroc run");

        if filename != DEFAULT_ROC_FILENAME {
            print!(" {}", &filename.to_string_lossy());
        }

        println!("\x1B[39m");
    }

    Ok(problems.exit_code())
}

pub fn handle_loading_problem(problem: LoadingProblem) -> std::io::Result<i32> {
    match problem {
        LoadingProblem::FormattedReport(report) => {
            print!("{report}");
            Ok(1)
        }
        _ => {
            // TODO: tighten up the types here, we should always end up with a
            // formatted report from load.
            println!("Failed with error: {problem:?}");
            Ok(1)
        }
    }
}

pub fn standard_load_config(
    target: Target,
    order: BuildOrdering,
    threading: Threading,
) -> LoadConfig {
    let exec_mode = match order {
        BuildOrdering::BuildIfChecks => ExecutionMode::ExecutableIfCheck,
        BuildOrdering::AlwaysBuild => ExecutionMode::Executable,
    };

    // UNSTABLE(lambda-erasure)
    let function_kind = if cfg!(debug_assertions) {
        if std::env::var("EXPERIMENTAL_ROC_ERASE").is_ok() {
            FunctionKind::Erased
        } else {
            FunctionKind::LambdaSet
        }
    } else {
        FunctionKind::LambdaSet
    };

    LoadConfig {
        target,
        function_kind,
        render: RenderTarget::ColorTerminal,
        palette: DEFAULT_PALETTE,
        threading,
        exec_mode,
    }
}

#[allow(clippy::too_many_arguments)]
pub fn build_file<'a>(
    arena: &'a Bump,
    target: Target,
    app_module_path: PathBuf,
    code_gen_options: CodeGenOptions,
    emit_timings: bool,
    link_type: LinkType,
    linking_strategy: LinkingStrategy,
    prebuilt_requested: bool,
    wasm_dev_stack_bytes: Option<u32>,
    roc_cache_dir: RocCacheDir<'_>,
    load_config: LoadConfig,
    out_path: Option<&Path>,
) -> Result<BuiltFile<'a>, BuildFileError<'a>> {
    let compilation_start = Instant::now();

    // Step 1: compile the app and generate the .o file
    let loaded =
        roc_load::load_and_monomorphize(arena, app_module_path.clone(), roc_cache_dir, load_config)
            .map_err(|e| BuildFileError::from_mono_error(e, compilation_start))?;

    build_loaded_file(
        arena,
        target,
        app_module_path,
        code_gen_options,
        emit_timings,
        link_type,
        linking_strategy,
        prebuilt_requested,
        wasm_dev_stack_bytes,
        loaded,
        compilation_start,
        out_path,
    )
}

#[allow(clippy::too_many_arguments)]
fn build_loaded_file<'a>(
    arena: &'a Bump,
    target: Target,
    app_module_path: PathBuf,
    code_gen_options: CodeGenOptions,
    emit_timings: bool,
    link_type: LinkType,
    mut linking_strategy: LinkingStrategy,
    prebuilt_requested: bool,
    wasm_dev_stack_bytes: Option<u32>,
    loaded: roc_load::MonomorphizedModule<'a>,
    compilation_start: Instant,
    out_path: Option<&Path>,
) -> Result<BuiltFile<'a>, BuildFileError<'a>> {
    let platform_main_roc = match &loaded.entry_point {
        EntryPoint::Executable { platform_path, .. } => platform_path.to_path_buf(),
        _ => unreachable!(),
    };

    // For example, if we're loading the platform from a URL, it's automatically prebuilt
    // even if the --prebuilt-platform CLI flag wasn't set.
    let is_platform_prebuilt = prebuilt_requested || loaded.uses_prebuilt_platform;

    if is_platform_prebuilt && linking_strategy == LinkingStrategy::Surgical {
        // Fallback to legacy linking if the preprocessed host file does not exist, but a legacy host does exist.
        let preprocessed_host_path =
            platform_main_roc.with_file_name(roc_linker::preprocessed_host_filename(target));
        let legacy_host_path = legacy_host_file(target, &platform_main_roc);
        if !preprocessed_host_path.exists() && legacy_host_path.exists() {
            linking_strategy = LinkingStrategy::Legacy;
        }
    }

    // the preprocessed host is stored beside the platform's main.roc
    let preprocessed_host_path = if linking_strategy == LinkingStrategy::Legacy {
        if target == Target::Wasm32 {
            // when compiling a wasm application, we implicitly assume here that the host is in zig
            // and has a file called "host.zig"
            platform_main_roc.with_file_name("host.zig")
        } else {
            legacy_host_file(target, &platform_main_roc)
        }
    } else {
        platform_main_roc.with_file_name(roc_linker::preprocessed_host_filename(target))
    };

    let output_exe_path = match out_path {
        Some(path) => {
            // true iff the path ends with a directory separator,
            // e.g. '/' on UNIX, '/' or '\\' on Windows
            let ends_with_sep = {
                #[cfg(unix)]
                {
                    use std::os::unix::ffi::OsStrExt;

                    path.as_os_str().as_bytes().ends_with(&[b'/'])
                }

                #[cfg(windows)]
                {
                    use std::os::windows::ffi::OsStrExt;

                    let last = path.as_os_str().encode_wide().last();

                    last == Some(0x002f)// UTF-16 slash
                        || last == Some(0x005c) // UTF-16 backslash
                }
            };

            // If you specified a path that ends in in a directory separator, then
            // use that directory, but use the app module's filename for the filename.
            if ends_with_sep {
                let filename = app_module_path.file_name().unwrap_or_default();

                with_output_extension(&path.join(filename), target, linking_strategy, link_type)
            } else {
                path.to_path_buf()
            }
        }
        None => with_output_extension(&app_module_path, target, linking_strategy, link_type),
    };

    // We don't need to spawn a rebuild thread when using a prebuilt host.
    let rebuild_thread = if matches!(link_type, LinkType::Dylib | LinkType::None) {
        None
    } else if is_platform_prebuilt {
        if !preprocessed_host_path.exists() {
            invalid_prebuilt_platform(prebuilt_requested, preprocessed_host_path);

            std::process::exit(1);
        }

        if linking_strategy == LinkingStrategy::Surgical {
            // Copy preprocessed host to executable location.
            // The surgical linker will modify that copy in-place.
            std::fs::copy(&preprocessed_host_path, output_exe_path.as_path()).unwrap();
        }

        None
    } else {
        // TODO this should probably be moved before load_and_monomorphize.
        // To do this we will need to preprocess files just for their exported symbols.
        // Also, we should no longer need to do this once we have platforms on
        // a package repository, as we can then get prebuilt platforms from there.

        let dll_stub_symbols = roc_linker::ExposedSymbols::from_exposed_to_host(
            &loaded.interns,
            &loaded.exposed_to_host,
        );

        let join_handle = spawn_rebuild_thread(
            code_gen_options.opt_level,
            linking_strategy,
            platform_main_roc.clone(),
            preprocessed_host_path.clone(),
            output_exe_path.clone(),
            target,
            dll_stub_symbols,
        );

        Some(join_handle)
    };

    let buf = &mut String::with_capacity(1024);

    let mut it = loaded.timings.iter().peekable();
    while let Some((module_id, module_timing)) = it.next() {
        let module_name = loaded.interns.module_name(*module_id);

        buf.push_str("    ");

        if module_name.is_empty() {
            // the App module
            buf.push_str("Application Module");
        } else {
            buf.push_str(module_name);
        }

        buf.push('\n');

        use std::fmt::Write;
        write!(buf, "{module_timing}").unwrap();

        if it.peek().is_some() {
            buf.push('\n');
        }
    }

    // This only needs to be mutable for report_problems. This can't be done
    // inside a nested scope without causing a borrow error!
    let mut loaded = loaded;
    let problems = report_problems_monomorphized(&mut loaded);
    let loaded = loaded;

    enum HostRebuildTiming {
        BeforeApp(u128),
        ConcurrentWithApp(JoinHandle<u128>),
    }

    let opt_rebuild_timing = if let Some(rebuild_thread) = rebuild_thread {
        if linking_strategy == LinkingStrategy::Additive {
            let rebuild_duration = rebuild_thread
                .join()
                .expect("Failed to (re)build platform.");

            if emit_timings && !is_platform_prebuilt {
                println!("Finished rebuilding the platform in {rebuild_duration} ms\n");
            }

            Some(HostRebuildTiming::BeforeApp(rebuild_duration))
        } else {
            Some(HostRebuildTiming::ConcurrentWithApp(rebuild_thread))
        }
    } else {
        None
    };

    let (roc_app_bytes, code_gen_timing, expect_metadata) = gen_from_mono_module(
        arena,
        loaded,
        &app_module_path,
        target,
        code_gen_options,
        &preprocessed_host_path,
        wasm_dev_stack_bytes,
    );

    buf.push('\n');
    buf.push_str("    ");
    buf.push_str("Code Generation");
    buf.push('\n');

    report_timing(
        buf,
        "Generate final IR from Mono IR",
        code_gen_timing.generate_final_ir,
    );
    report_timing(buf, "Generate object", code_gen_timing.code_gen_object);
    buf.push('\n');
    report_timing(buf, "Total", code_gen_timing.total);

    let compilation_end = compilation_start.elapsed();
    let size = roc_app_bytes.len();

    if emit_timings {
        println!(
            "\n\nCompilation finished!\n\nHere's how long each module took to compile:\n\n{buf}"
        );

        println!(
            "Finished compilation and code gen in {} ms\n\nProduced a app.o file of size {:?}\n",
            compilation_end.as_millis(),
            size,
        );
    }

    if let Some(HostRebuildTiming::ConcurrentWithApp(thread)) = opt_rebuild_timing {
        let rebuild_duration = thread.join().expect("Failed to (re)build platform.");

        if emit_timings && !is_platform_prebuilt {
            println!("Finished rebuilding the platform in {rebuild_duration} ms\n");
        }
    }

    // Step 2: link the prebuilt platform and compiled app
    let link_start = Instant::now();

    match (linking_strategy, link_type) {
        (LinkingStrategy::Surgical, _) => {
            roc_linker::link_preprocessed_host(
                target,
                &platform_main_roc,
                &roc_app_bytes,
                &output_exe_path,
            );
        }
        (LinkingStrategy::Additive, _) | (LinkingStrategy::Legacy, LinkType::None) => {
            // Just copy the object file to the output folder.
            std::fs::write(&output_exe_path, &*roc_app_bytes).unwrap();
        }
        (LinkingStrategy::Legacy, _) => {
            let extension = if target == Target::Wasm32 {
                // Legacy linker is only by used llvm wasm backend, not dev.
                // llvm wasm backend directly emits a bitcode file when targeting wasi, not a `.o` or `.wasm` file.
                // If we set the extension wrong, zig will print a ton of warnings when linking.
                "bc"
            } else {
                target.object_file_ext()
            };
            let app_o_file = tempfile::Builder::new()
                .prefix("roc_app")
                .suffix(&format!(".{extension}"))
                .tempfile()
                .map_err(|err| todo!("TODO Gracefully handle tempfile creation error {:?}", err))?;
            let app_o_file = app_o_file.path();

            std::fs::write(app_o_file, &*roc_app_bytes).unwrap();

            let builtins_host_tempfile = roc_bitcode::host_tempfile()
                .expect("failed to write host builtins object to tempfile");

            let mut inputs = vec![app_o_file.to_str().unwrap()];

            if !matches!(link_type, LinkType::Dylib | LinkType::None) {
                // the host has been compiled into a .o or .obj file
                inputs.push(preprocessed_host_path.as_path().to_str().unwrap());
            }

            if matches!(code_gen_options.backend, CodeGenBackend::Assembly(_)) {
                inputs.push(builtins_host_tempfile.path().to_str().unwrap());
            }

            let (mut child, _) = link(target, output_exe_path.clone(), &inputs, link_type)
                .map_err(|_| todo!("gracefully handle `ld` failing to spawn."))?;

            let exit_status = child
                .wait()
                .map_err(|_| todo!("gracefully handle error after `ld` spawned"))?;

            // Extend the lifetime of the tempfile so it doesn't get dropped
            // (and thus deleted) before the child process is done using it!
            let _ = builtins_host_tempfile;

            if !exit_status.success() {
                todo!(
                    "gracefully handle `ld` (or `zig` in the case of wasm with --optimize) returning exit code {:?}",
                    exit_status.code()
                );
            }
        }
    }

    let linking_time = link_start.elapsed();

    if emit_timings {
        println!("Finished linking in {} ms\n", linking_time.as_millis());
    }

    let total_time = compilation_start.elapsed();

    Ok(BuiltFile {
        binary_path: output_exe_path,
        problems,
        total_time,
        expect_metadata,
    })
}

fn invalid_prebuilt_platform(prebuilt_requested: bool, preprocessed_host_path: PathBuf) {
    let prefix = if prebuilt_requested {
        "Because I was run with --prebuilt-platform, "
    } else {
        ""
    };

    let preprocessed_host_path_str = preprocessed_host_path.to_string_lossy();
    let extra_err_msg = if preprocessed_host_path_str.ends_with(".rh") {
        "\n\n\tNote: If the platform does have an .rh1 file but no .rh file, it's because it's been built with an older version of roc. Contact the author to release a new build of the platform using a roc release newer than March 21 2023.\n"
    } else {
        ""
    };

    eprintln!(
        indoc::indoc!(
            r#"
            {}I was expecting this file to exist:

                {}

            However, it was not there!{}

            If you have the platform's source code locally, you may be able to generate it by re-running this command omitting --prebuilt-platform
            "#
        ),
        prefix,
        preprocessed_host_path.to_string_lossy(),
        extra_err_msg
    );
}

#[allow(clippy::too_many_arguments)]
fn spawn_rebuild_thread(
    opt_level: OptLevel,
    linking_strategy: LinkingStrategy,
    platform_main_roc: PathBuf,
    preprocessed_host_path: PathBuf,
    output_exe_path: PathBuf,
    target: Target,
    dll_stub_symbols: Vec<String>,
) -> std::thread::JoinHandle<u128> {
    std::thread::spawn(move || {
        // Printing to stderr because we want stdout to contain only the output of the roc program.
        // We are aware of the trade-offs.
        // `cargo run` follows the same approach
        eprintln!("🔨 Rebuilding platform...");

        let rebuild_host_start = Instant::now();

        match linking_strategy {
            LinkingStrategy::Additive => {
                let host_dest = rebuild_host(opt_level, target, platform_main_roc.as_path(), None);

                preprocess_host_wasm32(host_dest.as_path(), &preprocessed_host_path);
            }
            LinkingStrategy::Surgical => {
                build_and_preprocess_host_lowlevel(
                    opt_level,
                    target,
                    platform_main_roc.as_path(),
                    preprocessed_host_path.as_path(),
                    &dll_stub_symbols,
                );

                // Copy preprocessed host to executable location.
                // The surgical linker will modify that copy in-place.
                std::fs::copy(&preprocessed_host_path, output_exe_path.as_path()).unwrap();
            }
            LinkingStrategy::Legacy => {
                rebuild_host(opt_level, target, platform_main_roc.as_path(), None);
            }
        }

        rebuild_host_start.elapsed().as_millis()
    })
}

pub fn build_and_preprocess_host(
    opt_level: OptLevel,
    target: Target,
    platform_main_roc: &Path,
    preprocessed_host_path: &Path,
    exposed_symbols: roc_linker::ExposedSymbols,
) {
    let stub_dll_symbols = exposed_symbols.stub_dll_symbols();

    build_and_preprocess_host_lowlevel(
        opt_level,
        target,
        platform_main_roc,
        preprocessed_host_path,
        &stub_dll_symbols,
    )
}

fn build_and_preprocess_host_lowlevel(
    opt_level: OptLevel,
    target: Target,
    platform_main_roc: &Path,
    preprocessed_host_path: &Path,
    stub_dll_symbols: &[String],
) {
    let stub_lib =
        roc_linker::generate_stub_lib_from_loaded(target, platform_main_roc, stub_dll_symbols);

    debug_assert!(stub_lib.exists());

    rebuild_host(opt_level, target, platform_main_roc, Some(&stub_lib));

    roc_linker::preprocess_host(
        target,
        platform_main_roc,
        preprocessed_host_path,
        &stub_lib,
        stub_dll_symbols,
    )
}

#[allow(clippy::too_many_arguments)]
pub fn check_file<'a>(
    arena: &'a Bump,
    roc_file_path: PathBuf,
    emit_timings: bool,
    roc_cache_dir: RocCacheDir<'_>,
    threading: Threading,
) -> Result<(Problems, Duration), LoadingProblem<'a>> {
    let compilation_start = Instant::now();

    // only used for generating errors. We don't do code generation, so hardcoding should be fine
    // we need monomorphization for when exhaustiveness checking
    let target = Target::LinuxX64;

    // Step 1: compile the app and generate the .o file

    let load_config = LoadConfig {
        target,
        // TODO: we may not want this for just checking.
        function_kind: FunctionKind::LambdaSet,
        // TODO: expose this from CLI?
        render: RenderTarget::ColorTerminal,
        palette: DEFAULT_PALETTE,
        threading,
        exec_mode: ExecutionMode::Check,
    };
    let mut loaded =
        roc_load::load_and_typecheck(arena, roc_file_path, roc_cache_dir, load_config)?;

    let buf = &mut String::with_capacity(1024);

    let mut it = loaded.timings.iter().peekable();
    while let Some((module_id, module_timing)) = it.next() {
        let module_name = loaded.interns.module_name(*module_id);

        buf.push_str("    ");

        if module_name.is_empty() {
            // the App module
            buf.push_str("Application Module");
        } else {
            buf.push_str(module_name);
        }

        buf.push('\n');

        report_timing(buf, "Read .roc file from disk", module_timing.read_roc_file);
        report_timing(buf, "Parse header", module_timing.parse_header);
        report_timing(buf, "Parse body", module_timing.parse_body);
        report_timing(buf, "Canonicalize", module_timing.canonicalize);
        report_timing(buf, "Constrain", module_timing.constrain);
        report_timing(buf, "Solve", module_timing.solve);
        report_timing(buf, "Other", module_timing.other());
        buf.push('\n');
        report_timing(buf, "Total", module_timing.total());

        if it.peek().is_some() {
            buf.push('\n');
        }
    }

    let compilation_end = compilation_start.elapsed();

    if emit_timings {
        println!(
            "\n\nCompilation finished!\n\nHere's how long each module took to compile:\n\n{buf}"
        );

        println!("Finished checking in {} ms\n", compilation_end.as_millis(),);
    }

    Ok((report_problems_typechecked(&mut loaded), compilation_end))
}

pub fn build_str_test<'a>(
    arena: &'a Bump,
    app_module_path: &Path,
    app_module_source: &'a str,
    assume_prebuild: bool,
) -> Result<BuiltFile<'a>, BuildFileError<'a>> {
    let target = target_lexicon::Triple::host().into();

    let code_gen_options = CodeGenOptions {
        backend: CodeGenBackend::Llvm(LlvmBackendMode::Binary),
        opt_level: OptLevel::Normal,
        emit_debug_info: false,
        emit_llvm_ir: false,
        fuzz: false,
    };

    let emit_timings = false;
    let link_type = LinkType::Executable;
    let linking_strategy = LinkingStrategy::Surgical;
    let wasm_dev_stack_bytes = None;

    let roc_cache_dir = roc_packaging::cache::RocCacheDir::Disallowed;
    let build_ordering = BuildOrdering::AlwaysBuild;
    let threading = Threading::AtMost(2);

    let load_config = standard_load_config(target, build_ordering, threading);

    let compilation_start = std::time::Instant::now();

    // Step 1: compile the app and generate the .o file
    let loaded = roc_load::load_and_monomorphize_from_str(
        arena,
        PathBuf::from("valgrind_test.roc"),
        app_module_source,
        app_module_path.to_path_buf(),
        roc_cache_dir,
        load_config,
    )
    .map_err(|e| BuildFileError::from_mono_error(e, compilation_start))?;

    build_loaded_file(
        arena,
        target,
        app_module_path.to_path_buf(),
        code_gen_options,
        emit_timings,
        link_type,
        linking_strategy,
        assume_prebuild,
        wasm_dev_stack_bytes,
        loaded,
        compilation_start,
        None,
    )
}

fn with_output_extension(
    path: &Path,
    target: Target,
    linking_strategy: LinkingStrategy,
    link_type: LinkType,
) -> PathBuf {
    match (linking_strategy, link_type) {
        (LinkingStrategy::Additive, _) | (LinkingStrategy::Legacy, LinkType::None) => {
            // Additive linking and no linking both output the object file type.
            path.with_extension(target.object_file_ext())
        }
        _ => path.with_extension(target.executable_file_ext().unwrap_or_default()),
    }
}
