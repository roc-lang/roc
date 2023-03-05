use crate::link::{
    legacy_host_filename, link, preprocess_host_wasm32, rebuild_host, LinkType, LinkingStrategy,
};
use bumpalo::Bump;
use inkwell::memory_buffer::MemoryBuffer;
use roc_builtins::bitcode;
use roc_error_macros::internal_error;
use roc_gen_llvm::llvm::build::{module_from_builtins, LlvmBackendMode};
use roc_gen_llvm::llvm::externs::add_default_roc_externs;
use roc_load::{
    EntryPoint, ExecutionMode, ExpectMetadata, LoadConfig, LoadMonomorphizedError, LoadedModule,
    LoadingProblem, MonomorphizedModule, Threading,
};
use roc_mono::ir::{OptLevel, SingleEntryPoint};
use roc_packaging::cache::RocCacheDir;
use roc_reporting::{
    cli::{report_problems, Problems},
    report::{RenderTarget, DEFAULT_PALETTE},
};
use roc_target::TargetInfo;
use std::ops::Deref;
use std::{
    path::{Path, PathBuf},
    thread::JoinHandle,
    time::{Duration, Instant},
};
use target_lexicon::Triple;

#[cfg(feature = "target-wasm32")]
use roc_collections::all::MutSet;

#[derive(Debug, Clone, Copy, Default)]
pub struct CodeGenTiming {
    pub code_gen: Duration,
}

pub fn report_problems_monomorphized(loaded: &mut MonomorphizedModule) -> Problems {
    report_problems(
        loaded.total_problems(),
        &loaded.sources,
        &loaded.interns,
        &mut loaded.can_problems,
        &mut loaded.type_problems,
    )
}

pub fn report_problems_typechecked(loaded: &mut LoadedModule) -> Problems {
    report_problems(
        loaded.total_problems(),
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
    Assembly,
    Llvm,
    Wasm,
}

#[derive(Debug, Clone, Copy)]
pub struct CodeGenOptions {
    pub backend: CodeGenBackend,
    pub opt_level: OptLevel,
    pub emit_debug_info: bool,
}

type GenFromMono<'a> = (CodeObject, CodeGenTiming, ExpectMetadata<'a>);

#[allow(clippy::too_many_arguments)]
pub fn gen_from_mono_module<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    roc_file_path: &Path,
    target: &target_lexicon::Triple,
    code_gen_options: CodeGenOptions,
    preprocessed_host_path: &Path,
    wasm_dev_stack_bytes: Option<u32>,
) -> GenFromMono<'a> {
    match code_gen_options.backend {
        CodeGenBackend::Assembly => gen_from_mono_module_dev(
            arena,
            loaded,
            target,
            preprocessed_host_path,
            wasm_dev_stack_bytes,
        ),
        CodeGenBackend::Llvm => {
            gen_from_mono_module_llvm(arena, loaded, roc_file_path, target, code_gen_options)
        }
        CodeGenBackend::Wasm => {
            // emit wasm via the llvm backend
            gen_from_mono_module_llvm(arena, loaded, roc_file_path, target, code_gen_options)
        }
    }
}

// TODO how should imported modules factor into this? What if those use builtins too?
// TODO this should probably use more helper functions
// TODO make this polymorphic in the llvm functions so it can be reused for another backend.
fn gen_from_mono_module_llvm<'a>(
    arena: &'a bumpalo::Bump,
    mut loaded: MonomorphizedModule<'a>,
    roc_file_path: &Path,
    target: &target_lexicon::Triple,
    code_gen_options: CodeGenOptions,
) -> GenFromMono<'a> {
    use crate::target::{self, convert_opt_level};
    use inkwell::attributes::{Attribute, AttributeLoc};
    use inkwell::context::Context;
    use inkwell::module::Linkage;
    use inkwell::targets::{FileType, RelocMode};

    let code_gen_start = Instant::now();

    // Generate the binary
    let target_info = roc_target::TargetInfo::from(target);
    let context = Context::create();
    let module = arena.alloc(module_from_builtins(target, &context, "app"));

    // strip Zig debug stuff
    // module.strip_debug_info();

    // mark our zig-defined builtins as internal
    let app_ll_file = {
        let mut temp = PathBuf::from(roc_file_path);
        temp.set_extension("ll");

        temp
    };

    let kind_id = Attribute::get_named_enum_kind_id("alwaysinline");
    debug_assert!(kind_id > 0);
    let enum_attr = context.create_enum_attribute(kind_id, 1);

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

    let CodeGenOptions {
        backend: _,
        opt_level,
        emit_debug_info,
    } = code_gen_options;

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
        target_info,
        mode: match opt_level {
            OptLevel::Development => LlvmBackendMode::BinaryDev,
            OptLevel::Normal | OptLevel::Size | OptLevel::Optimize => LlvmBackendMode::Binary,
        },

        exposed_to_host: loaded.exposed_to_host.values.keys().copied().collect(),
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
        &mut loaded.layout_interner,
        opt_level,
        loaded.procedures,
        entry_point,
        Some(&app_ll_file),
    );

    env.dibuilder.finalize();

    // we don't use the debug info, and it causes weird errors.
    module.strip_debug_info();

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

    // annotate the LLVM IR output with debug info
    // so errors are reported with the line number of the LLVM source
    let memory_buffer = if cfg!(feature = "sanitizers") && std::env::var("ROC_SANITIZERS").is_ok() {
        let dir = tempfile::tempdir().unwrap();
        let dir = dir.into_path();

        let app_ll_file = dir.join("app.ll");
        let app_bc_file = dir.join("app.bc");
        let app_o_file = dir.join("app.o");

        // write the ll code to a file, so we can modify it
        module.print_to_file(&app_ll_file).unwrap();

        // Apply coverage passes.
        // Note, this is specifically tailored for `cargo afl` and afl++.
        // It most likely will not work with other fuzzer setups without modification.
        let mut passes = vec![];
        let mut extra_args = vec![];
        let mut unrecognized = vec![];
        for sanitizer in std::env::var("ROC_SANITIZERS")
            .unwrap()
            .split(',')
            .map(|x| x.trim())
        {
            match sanitizer {
                "address" => passes.push("asan-module"),
                "memory" => passes.push("msan-module"),
                "thread" => passes.push("tsan-module"),
                "cargo-fuzz" => {
                    passes.push("sancov-module");
                    extra_args.extend_from_slice(&[
                        "-sanitizer-coverage-level=3",
                        "-sanitizer-coverage-prune-blocks=0",
                        "-sanitizer-coverage-inline-8bit-counters",
                        "-sanitizer-coverage-pc-table",
                    ]);
                }
                "afl.rs" => {
                    passes.push("sancov-module");
                    extra_args.extend_from_slice(&[
                        "-sanitizer-coverage-level=3",
                        "-sanitizer-coverage-prune-blocks=0",
                        "-sanitizer-coverage-trace-pc-guard",
                    ]);
                }
                x => unrecognized.push(x.to_owned()),
            }
        }
        if !unrecognized.is_empty() {
            let out = unrecognized
                .iter()
                .map(|x| format!("{:?}", x))
                .collect::<Vec<String>>()
                .join(", ");
            eprintln!("Unrecognized sanitizer: {}\nSupported options are \"address\", \"memory\", \"thread\", \"cargo-fuzz\", and \"afl.rs\".", out);
            eprintln!("Note: \"cargo-fuzz\" and \"afl.rs\" both enable sanitizer coverage for fuzzing. They just use different parameters to match the respective libraries.")
        }

        use std::process::Command;
        let mut opt = Command::new("opt");
        opt.args([
            app_ll_file.to_str().unwrap(),
            "-o",
            app_bc_file.to_str().unwrap(),
        ])
        .args(extra_args);
        if !passes.is_empty() {
            opt.arg(format!("-passes={}", passes.join(",")));
        }
        let opt = opt.output().unwrap();

        assert!(opt.stderr.is_empty(), "{:#?}", opt);

        // write the .o file. Note that this builds the .o for the local machine,
        // and ignores the `target_machine` entirely.
        //
        // different systems name this executable differently, so we shotgun for
        // the most common ones and then give up.
        let bc_to_object = Command::new("llc")
            .args([
                "-relocation-model=pic",
                "-filetype=obj",
                app_bc_file.to_str().unwrap(),
                "-o",
                app_o_file.to_str().unwrap(),
            ])
            .output()
            .unwrap();

        assert!(bc_to_object.status.success(), "{:#?}", bc_to_object);

        MemoryBuffer::create_from_file(&app_o_file).expect("memory buffer creation works")
    } else if emit_debug_info {
        module.strip_debug_info();

        let mut app_ll_dbg_file = PathBuf::from(roc_file_path);
        app_ll_dbg_file.set_extension("dbg.ll");

        let mut app_o_file = PathBuf::from(roc_file_path);
        app_o_file.set_extension("o");

        use std::process::Command;

        // write the ll code to a file, so we can modify it
        module.print_to_file(&app_ll_file).unwrap();

        // run the debugir https://github.com/vaivaswatha/debugir tool
        match Command::new("debugir")
            .args(["-instnamer", app_ll_file.to_str().unwrap()])
            .output()
        {
            Ok(_) => {}
            Err(error) => {
                use std::io::ErrorKind;
                match error.kind() {
                    ErrorKind::NotFound => internal_error!(
                        r"I could not find the `debugir` tool on the PATH, install it from https://github.com/vaivaswatha/debugir"
                    ),
                    _ => internal_error!("{:?}", error),
                }
            }
        }

        use target_lexicon::Architecture;
        match target.architecture {
            Architecture::X86_64
            | Architecture::X86_32(_)
            | Architecture::Aarch64(_)
            | Architecture::Wasm32 => {
                // write the .o file. Note that this builds the .o for the local machine,
                // and ignores the `target_machine` entirely.
                //
                // different systems name this executable differently, so we shotgun for
                // the most common ones and then give up.
                let ll_to_object = Command::new("llc")
                    .args([
                        "-relocation-model=pic",
                        "-filetype=obj",
                        app_ll_dbg_file.to_str().unwrap(),
                        "-o",
                        app_o_file.to_str().unwrap(),
                    ])
                    .output()
                    .unwrap();

                assert!(ll_to_object.stderr.is_empty(), "{:#?}", ll_to_object);
            }
            _ => unreachable!(),
        }

        MemoryBuffer::create_from_file(&app_o_file).expect("memory buffer creation works")
    } else {
        // Emit the .o file
        use target_lexicon::Architecture;
        match target.architecture {
            Architecture::X86_64 | Architecture::X86_32(_) | Architecture::Aarch64(_) => {
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
                target.architecture
            ),
        }
    };

    let code_gen = code_gen_start.elapsed();

    (
        CodeObject::MemoryBuffer(memory_buffer),
        CodeGenTiming { code_gen },
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
    target: &target_lexicon::Triple,
    preprocessed_host_path: &Path,
    wasm_dev_stack_bytes: Option<u32>,
) -> GenFromMono<'a> {
    use target_lexicon::Architecture;

    match target.architecture {
        Architecture::Wasm32 => gen_from_mono_module_dev_wasm32(
            arena,
            loaded,
            preprocessed_host_path,
            wasm_dev_stack_bytes,
        ),
        Architecture::X86_64 | Architecture::Aarch64(_) => {
            gen_from_mono_module_dev_assembly(arena, loaded, target)
        }
        _ => todo!(),
    }
}

#[cfg(not(feature = "target-wasm32"))]
pub fn gen_from_mono_module_dev<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    target: &target_lexicon::Triple,
    _host_input_path: &Path,
    _wasm_dev_stack_bytes: Option<u32>,
) -> GenFromMono<'a> {
    use target_lexicon::Architecture;

    match target.architecture {
        Architecture::X86_64 | Architecture::Aarch64(_) => {
            gen_from_mono_module_dev_assembly(arena, loaded, target)
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
    let code_gen_start = Instant::now();
    let MonomorphizedModule {
        module_id,
        procedures,
        mut interns,
        mut layout_interner,
        ..
    } = loaded;

    let exposed_to_host = loaded
        .exposed_to_host
        .values
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
            "Failed to read host object file {}! Try setting --prebuilt-platform=false",
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

    let code_gen = code_gen_start.elapsed();

    (
        CodeObject::Vector(final_binary_bytes),
        CodeGenTiming { code_gen },
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
    target: &target_lexicon::Triple,
) -> GenFromMono<'a> {
    let code_gen_start = Instant::now();

    let lazy_literals = true;
    let generate_allocators = false; // provided by the platform

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
        exposed_to_host: exposed_to_host.values.keys().copied().collect(),
        lazy_literals,
        generate_allocators,
    };

    let module_object =
        roc_gen_dev::build_module(&env, &mut interns, &mut layout_interner, target, procedures);

    let code_gen = code_gen_start.elapsed();

    let module_out = module_object
        .write()
        .expect("failed to build output object");

    (
        CodeObject::Vector(module_out),
        CodeGenTiming { code_gen },
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

pub fn standard_load_config(
    target: &Triple,
    order: BuildOrdering,
    threading: Threading,
) -> LoadConfig {
    let target_info = TargetInfo::from(target);

    let exec_mode = match order {
        BuildOrdering::BuildIfChecks => ExecutionMode::ExecutableIfCheck,
        BuildOrdering::AlwaysBuild => ExecutionMode::Executable,
    };

    LoadConfig {
        target_info,
        render: RenderTarget::ColorTerminal,
        palette: DEFAULT_PALETTE,
        threading,
        exec_mode,
    }
}

#[allow(clippy::too_many_arguments)]
pub fn build_file<'a>(
    arena: &'a Bump,
    target: &Triple,
    app_module_path: PathBuf,
    code_gen_options: CodeGenOptions,
    emit_timings: bool,
    link_type: LinkType,
    linking_strategy: LinkingStrategy,
    prebuilt_requested: bool,
    wasm_dev_stack_bytes: Option<u32>,
    roc_cache_dir: RocCacheDir<'_>,
    load_config: LoadConfig,
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
    )
}

#[allow(clippy::too_many_arguments)]
fn build_loaded_file<'a>(
    arena: &'a Bump,
    target: &Triple,
    app_module_path: PathBuf,
    code_gen_options: CodeGenOptions,
    emit_timings: bool,
    link_type: LinkType,
    linking_strategy: LinkingStrategy,
    prebuilt_requested: bool,
    wasm_dev_stack_bytes: Option<u32>,
    loaded: roc_load::MonomorphizedModule<'a>,
    compilation_start: Instant,
) -> Result<BuiltFile<'a>, BuildFileError<'a>> {
    let operating_system = roc_target::OperatingSystem::from(target.operating_system);

    let platform_main_roc = match &loaded.entry_point {
        EntryPoint::Executable { platform_path, .. } => platform_path.to_path_buf(),
        _ => unreachable!(),
    };

    // the preprocessed host is stored beside the platform's main.roc
    let preprocessed_host_path = if linking_strategy == LinkingStrategy::Legacy {
        if let roc_target::OperatingSystem::Wasi = operating_system {
            // when compiling a wasm application, we implicitly assume here that the host is in zig
            // and has a file called "host.zig"
            platform_main_roc.with_file_name("host.zig")
        } else {
            platform_main_roc.with_file_name(legacy_host_filename(target).unwrap())
        }
    } else {
        platform_main_roc.with_file_name(roc_linker::preprocessed_host_filename(target).unwrap())
    };

    // For example, if we're loading the platform from a URL, it's automatically prebuilt
    // even if the --prebuilt-platform=true CLI flag wasn't set.
    let is_platform_prebuilt = prebuilt_requested || loaded.uses_prebuilt_platform;

    let cwd = app_module_path.parent().unwrap();
    let mut output_exe_path = cwd.join(&*loaded.output_path);

    if let Some(extension) = operating_system.executable_file_ext() {
        output_exe_path.set_extension(extension);
    }

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

        let exposed_values = loaded
            .exposed_to_host
            .values
            .keys()
            .map(|x| x.as_str(&loaded.interns).to_string())
            .collect();

        let exposed_closure_types = loaded
            .exposed_to_host
            .closure_types
            .iter()
            .map(|x| {
                format!(
                    "{}_{}",
                    x.module_string(&loaded.interns),
                    x.as_str(&loaded.interns)
                )
            })
            .collect();

        let join_handle = spawn_rebuild_thread(
            code_gen_options.opt_level,
            linking_strategy,
            platform_main_roc.clone(),
            preprocessed_host_path.clone(),
            output_exe_path.clone(),
            target,
            exposed_values,
            exposed_closure_types,
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
        write!(buf, "{}", module_timing).unwrap();

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
                println!(
                    "Finished rebuilding the platform in {} ms\n",
                    rebuild_duration
                );
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
        "Generate Assembly from Mono IR",
        code_gen_timing.code_gen,
    );

    let compilation_end = compilation_start.elapsed();
    let size = roc_app_bytes.len();

    if emit_timings {
        println!(
            "\n\nCompilation finished!\n\nHere's how long each module took to compile:\n\n{}",
            buf
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
            println!(
                "Finished rebuilding the platform in {} ms\n",
                rebuild_duration
            );
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
            output_exe_path.set_extension(operating_system.object_file_ext());
            std::fs::write(&output_exe_path, &*roc_app_bytes).unwrap();
        }
        (LinkingStrategy::Legacy, _) => {
            let app_o_file = tempfile::Builder::new()
                .prefix("roc_app")
                .suffix(&format!(".{}", operating_system.object_file_ext()))
                .tempfile()
                .map_err(|err| todo!("TODO Gracefully handle tempfile creation error {:?}", err))?;
            let app_o_file = app_o_file.path();

            std::fs::write(app_o_file, &*roc_app_bytes).unwrap();

            let builtins_host_tempfile =
                bitcode::host_tempfile().expect("failed to write host builtins object to tempfile");

            let mut inputs = vec![app_o_file.to_str().unwrap()];

            if !matches!(link_type, LinkType::Dylib | LinkType::None) {
                // the host has been compiled into a .o or .obj file
                inputs.push(preprocessed_host_path.as_path().to_str().unwrap());
            }

            if matches!(code_gen_options.backend, CodeGenBackend::Assembly) {
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
    let prefix = match prebuilt_requested {
        true => "Because I was run with --prebuilt-platform=true, ",
        false => "",
    };

    eprintln!(
        indoc::indoc!(
            r#"
            {}I was expecting this file to exist:

                {}

            However, it was not there!

            If you have the platform's source code locally, you may be able to generate it by re-running this command with --prebuilt-platform=false
            "#
        ),
        prefix,
        preprocessed_host_path.to_string_lossy(),
    );
}

#[allow(clippy::too_many_arguments)]
fn spawn_rebuild_thread(
    opt_level: OptLevel,
    linking_strategy: LinkingStrategy,
    platform_main_roc: PathBuf,
    preprocessed_host_path: PathBuf,
    output_exe_path: PathBuf,
    target: &Triple,
    exported_symbols: Vec<String>,
    exported_closure_types: Vec<String>,
) -> std::thread::JoinHandle<u128> {
    let thread_local_target = target.clone();
    std::thread::spawn(move || {
        // Printing to stderr because we want stdout to contain only the output of the roc program.
        // We are aware of the trade-offs.
        // `cargo run` follows the same approach
        eprintln!("🔨 Rebuilding platform...");

        let rebuild_host_start = Instant::now();

        match linking_strategy {
            LinkingStrategy::Additive => {
                let host_dest = rebuild_host(
                    opt_level,
                    &thread_local_target,
                    platform_main_roc.as_path(),
                    None,
                );

                preprocess_host_wasm32(host_dest.as_path(), &preprocessed_host_path);
            }
            LinkingStrategy::Surgical => {
                build_and_preprocess_host(
                    opt_level,
                    &thread_local_target,
                    platform_main_roc.as_path(),
                    preprocessed_host_path.as_path(),
                    exported_symbols,
                    exported_closure_types,
                );

                // Copy preprocessed host to executable location.
                // The surgical linker will modify that copy in-place.
                std::fs::copy(&preprocessed_host_path, output_exe_path.as_path()).unwrap();
            }
            LinkingStrategy::Legacy => {
                rebuild_host(
                    opt_level,
                    &thread_local_target,
                    platform_main_roc.as_path(),
                    None,
                );
            }
        }

        rebuild_host_start.elapsed().as_millis()
    })
}

pub fn build_and_preprocess_host(
    opt_level: OptLevel,
    target: &Triple,
    platform_main_roc: &Path,
    preprocessed_host_path: &Path,
    exposed_to_host: Vec<String>,
    exported_closure_types: Vec<String>,
) {
    let (stub_lib, stub_dll_symbols) = roc_linker::generate_stub_lib_from_loaded(
        target,
        platform_main_roc,
        exposed_to_host,
        exported_closure_types,
    );
    rebuild_host(opt_level, target, platform_main_roc, Some(&stub_lib));

    roc_linker::preprocess_host(
        target,
        platform_main_roc,
        preprocessed_host_path,
        &stub_lib,
        &stub_dll_symbols,
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
    let target_info = TargetInfo::default_x86_64();

    // Step 1: compile the app and generate the .o file

    let load_config = LoadConfig {
        target_info,
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
            "\n\nCompilation finished!\n\nHere's how long each module took to compile:\n\n{}",
            buf
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
    let triple = target_lexicon::Triple::host();

    let code_gen_options = CodeGenOptions {
        backend: CodeGenBackend::Llvm,
        opt_level: OptLevel::Normal,
        emit_debug_info: false,
    };

    let emit_timings = false;
    let link_type = LinkType::Executable;
    let linking_strategy = LinkingStrategy::Surgical;
    let wasm_dev_stack_bytes = None;

    let roc_cache_dir = roc_packaging::cache::RocCacheDir::Disallowed;
    let build_ordering = BuildOrdering::AlwaysBuild;
    let threading = Threading::AtMost(2);

    let load_config = standard_load_config(&triple, build_ordering, threading);

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
        &triple,
        app_module_path.to_path_buf(),
        code_gen_options,
        emit_timings,
        link_type,
        linking_strategy,
        assume_prebuild,
        wasm_dev_stack_bytes,
        loaded,
        compilation_start,
    )
}
