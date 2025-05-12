use crate::link::{link, preprocess_host_wasm32, rebuild_host, LinkType, LinkingStrategy};
use bumpalo::collections::CollectIn;
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
use std::ops::Deref;
use std::{
    path::{Path, PathBuf},
    time::{Duration, Instant},
};

#[cfg(feature = "target-wasm32")]
use roc_collections::all::MutSet;
use roc_target::SurgicalHostArtifacts;

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
    built_host_opt: &BuiltHostOpt,
    wasm_dev_stack_bytes: Option<u32>,
) -> GenFromMono<'a> {
    let path = roc_file_path;
    let debug = code_gen_options.emit_debug_info;
    let emit_llvm_ir = code_gen_options.emit_llvm_ir;
    let fuzz = code_gen_options.fuzz;
    let opt = code_gen_options.opt_level;

    match code_gen_options.backend {
        CodeGenBackend::Wasm => {
            assert_ne!(
                *built_host_opt,
                BuiltHostOpt::None,
                "Wasm backend needs a built host."
            );

            gen_from_mono_module_dev(
                arena,
                loaded,
                target,
                built_host_opt,
                wasm_dev_stack_bytes,
                AssemblyBackendMode::Binary, // dummy value, unused in practice
            )
        }
        CodeGenBackend::Assembly(backend_mode) => gen_from_mono_module_dev(
            arena,
            loaded,
            target,
            built_host_opt,
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
            let entry_points: bumpalo::collections::Vec<_> = exposed_to_host
                .iter()
                .map(|(fn_name, symbol, layout)| SingleEntryPoint {
                    name: fn_name,
                    symbol: *symbol,
                    layout: *layout,
                })
                .collect_in(arena);

            roc_mono::ir::EntryPoint::Program(entry_points.into_bump_slice())
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

    crate::llvm_passes::optimize_llvm_ir(&env, target, opt_level, emit_debug_info, &app_ll_file);

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

fn gen_from_mono_module_dev<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    target: Target,
    built_host_opt: &BuiltHostOpt,
    wasm_dev_stack_bytes: Option<u32>,
    #[allow(unused_variables)] backend_mode: AssemblyBackendMode,
) -> GenFromMono<'a> {
    match (built_host_opt, target.architecture()) {
        (BuiltHostOpt::Additive(host_path), Architecture::Wasm32) => {
            #[cfg(feature = "target-wasm32")]
            {
                gen_from_mono_module_dev_wasm32(arena, loaded, host_path, wasm_dev_stack_bytes)
            }

            #[cfg(not(feature = "target-wasm32"))]
            {
                internal_error!("Compiler was not built with feature 'target-wasm32'.");
            }
        }
        (BuiltHostOpt::None, Architecture::Wasm32) => {
            internal_error!("Cannot compile wasm32 without a host on the dev compiler backend.")
        }
        (BuiltHostOpt::Legacy(host_path), Architecture::Wasm32) => internal_error!(
            "Unsupported host files found for use with wasm32 dev compiler backend:\n    {}",
            host_path.display()
        ),
        (
            BuiltHostOpt::Surgical(SurgicalHostArtifacts {
                preprocessed_host, ..
            }),
            Architecture::Wasm32,
        ) => internal_error!(
            "Unsupported host files found for use with wasm32 dev compiler backend:\n    {}",
            preprocessed_host.display()
        ),
        (_, Architecture::X86_64 | Architecture::Aarch64) => {
            #[cfg(not(feature = "target-wasm32"))]
            {
                gen_from_mono_module_dev_assembly(arena, loaded, target, backend_mode)
            }

            #[cfg(feature = "target-wasm32")]
            {
                internal_error!("Compiler was built with feature 'target-wasm32'.")
            }
        }
        (_, Architecture::Aarch32) => {
            internal_error!("Dev compiler backend does not support 32 bit ARM architectures")
        }
        (_, Architecture::X86_32) => {
            internal_error!("Dev compiler backend does not support 32 bit x86 architectures")
        }
    }
}

#[cfg(feature = "target-wasm32")]
fn gen_from_mono_module_dev_wasm32<'a>(
    arena: &'a bumpalo::Bump,
    loaded: MonomorphizedModule<'a>,
    built_host_path: &Path,
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

    let host_bytes = std::fs::read(built_host_path).unwrap_or_else(|_| {
        internal_error!(
            "Failed to read host object file {}!",
            built_host_path.display()
        )
    });

    let host_module = roc_gen_wasm::parse_host(arena, &host_bytes).unwrap_or_else(|e| {
        internal_error!(
            "I ran into a problem with the host object file, {} at offset 0x{:x}:\n{}",
            built_host_path.display(),
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

#[allow(dead_code)]
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
) -> std::io::Result<i32> {
    debug_assert!(module.total_problems() > 0);

    let problems = report_problems_typechecked(&mut module);

    problems.print_error_warning_count(total_time);

    Ok(problems.exit_code())
}

pub fn handle_loading_problem(problem: LoadingProblem) -> std::io::Result<i32> {
    match problem {
        LoadingProblem::FormattedReport(report, _) => {
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

    LoadConfig {
        target,
        function_kind: FunctionKind::from_env(),
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
    build_host: bool,
    suppress_build_host_warning: bool,
    wasm_dev_stack_bytes: Option<u32>,
    roc_cache_dir: RocCacheDir<'_>,
    load_config: LoadConfig,
    out_path: Option<&Path>,
    verbose: bool,
) -> Result<BuiltFile<'a>, BuildFileError<'a>> {
    let compilation_start = Instant::now();

    let loaded = roc_load::load_and_monomorphize(
        arena,
        app_module_path.clone(),
        None,
        roc_cache_dir,
        load_config,
    )
    .map_err(|e| BuildFileError::from_mono_error(e, compilation_start))?;

    build_loaded_file(
        arena,
        target,
        app_module_path,
        code_gen_options,
        emit_timings,
        link_type,
        linking_strategy,
        build_host,
        suppress_build_host_warning,
        wasm_dev_stack_bytes,
        loaded,
        compilation_start,
        out_path,
        verbose,
    )
}

#[derive(Debug, PartialEq, Eq)]
/// Opt because of possible None value
// Advice: do not try to wrap this in an Option, that would require cloning in build_loaded_file.
pub enum BuiltHostOpt {
    Additive(PathBuf),
    Legacy(PathBuf),
    // SurgicalHostArtifacts contains metadata, preprocessed_host
    Surgical(SurgicalHostArtifacts),
    None,
}

fn build_and_preprocess_host(
    code_gen_options: CodeGenOptions,
    dll_stub_symbols: Vec<String>,
    emit_timings: bool,
    linking_strategy: LinkingStrategy,
    platform_main_roc: &Path,
    preprocessed_host_path: &Path,
    target: Target,
) -> BuiltHostOpt {
    let rebuild_thread = match linking_strategy {
        LinkingStrategy::Additive => spawn_wasm32_host_build_thread(
            code_gen_options.opt_level,
            target,
            platform_main_roc.to_owned(),
            preprocessed_host_path.to_owned(),
        ),
        LinkingStrategy::Surgical => {
            let preprocessed_path =
                platform_main_roc.with_file_name(target.prebuilt_surgical_host());
            let metadata_path = platform_main_roc.with_file_name(target.metadata_file_name());

            spawn_surgical_host_build_thread(
                code_gen_options.opt_level,
                target,
                platform_main_roc.to_owned(),
                dll_stub_symbols,
                preprocessed_path,
                preprocessed_host_path.to_owned(),
                metadata_path,
            )
        }
        LinkingStrategy::Legacy => spawn_legacy_host_build_thread(
            code_gen_options.opt_level,
            target,
            platform_main_roc.to_owned(),
        ),
    };
    let (rebuild_duration, path) = rebuild_thread.join().expect("Failed to build host.");
    if emit_timings {
        println!(
            "Finished rebuilding the platform host in {} ms\n",
            rebuild_duration
        );
    }
    path
}

#[allow(clippy::too_many_arguments)]
fn build_loaded_file<'a>(
    arena: &'a Bump,
    target: Target,
    app_module_path: PathBuf,
    code_gen_options: CodeGenOptions,
    emit_timings: bool,
    link_type: LinkType,
    linking_strategy: LinkingStrategy,
    build_host_requested: bool,
    suppress_build_host_warning: bool,
    wasm_dev_stack_bytes: Option<u32>,
    loaded: roc_load::MonomorphizedModule<'a>,
    compilation_start: Instant,
    out_path: Option<&Path>,
    verbose: bool,
) -> Result<BuiltFile<'a>, BuildFileError<'a>> {
    // get the platform path from the app header
    let platform_main_roc_path = match &loaded.entry_point {
        EntryPoint::Executable { platform_path, .. } => platform_path.to_path_buf(),
        _ => unreachable!(),
    };

    let output_exe_path = get_exe_path(
        out_path,
        app_module_path.as_path(),
        target,
        linking_strategy,
        link_type,
    );

    let dll_stub_symbols =
        roc_linker::ExposedSymbols::from_exposed_to_host(&loaded.interns, &loaded.exposed_to_host);

    let built_host_opt =
        // Not sure if this is correct for all calls with LinkType::Dylib...
        if link_type == LinkType::None || link_type == LinkType::Dylib || target == Target::Wasm32 {
            BuiltHostOpt::None
        } else {
            let prebuilt_host = determine_built_host_path(&platform_main_roc_path, target, build_host_requested, link_type, linking_strategy, suppress_build_host_warning);

            match prebuilt_host {
                BuiltHostOpt::None => {
                    build_and_preprocess_host(
                        code_gen_options,
                        dll_stub_symbols,
                        emit_timings,
                        linking_strategy,
                        &platform_main_roc_path,
                        &output_exe_path,
                        target,
                    )
                }
                BuiltHostOpt::Surgical(ref surgical_artifacts) => {
                    // Copy preprocessed host to executable location.
                    // The surgical linker will modify that copy in-place.
                    std::fs::copy(&surgical_artifacts.preprocessed_host, output_exe_path.as_path()).unwrap();
                    prebuilt_host
                }
                other => other
            }
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

    let (roc_app_bytes, code_gen_timing, expect_metadata) = gen_from_mono_module(
        arena,
        loaded,
        &app_module_path,
        target,
        code_gen_options,
        &built_host_opt,
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

    // link the prebuilt platform and compiled app
    let link_start = Instant::now();

    match (linking_strategy, link_type) {
        (LinkingStrategy::Surgical, _) => {
            let metadata_file = platform_main_roc_path.with_file_name(target.metadata_file_name());

            roc_linker::link_preprocessed_host(
                target,
                &roc_app_bytes,
                &output_exe_path,
                metadata_file,
                verbose,
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

            let mut inputs = vec![app_o_file.to_str().unwrap()];

            let mut host_path = String::new();

            match built_host_opt {
                BuiltHostOpt::Legacy(p) => {
                    host_path.push_str(&p.to_string_lossy());
                    inputs.push(&host_path);
                }
                BuiltHostOpt::None => {
                    // In case of link_type == LinkType::Dylib or target == Target::Wasm32
                    // When compiling a Dylib there is no host, such as when generating glue using `roc glue`.
                    if target == Target::Wasm32 {
                        let wasm_host_zig: PathBuf =
                            platform_main_roc_path.with_file_name("host.zig");

                        assert!(
                            wasm_host_zig.exists(),
                            "No host.zig file found at {} when building wasm32 target.",
                            wasm_host_zig.display()
                        );

                        host_path.push_str(&wasm_host_zig.to_string_lossy());
                        inputs.push(&host_path);
                    }
                }
                other => {
                    panic!("Unexpected variant of built_host_opt in combination with `LinkingStrategy::Legacy`: {:?}", other);
                }
            }

            let builtins_host_tempfile = roc_bitcode::host_tempfile()
                .expect("failed to write host builtins object to tempfile");

            if matches!(code_gen_options.backend, CodeGenBackend::Assembly(_)) {
                inputs.push(builtins_host_tempfile.path().to_str().unwrap());
            }

            let (mut child, _) = link(target, output_exe_path.clone(), &inputs, link_type)
                .map_err(|_| todo!("linker failed to spawn."))?;

            let exit_status = child
                .wait()
                .map_err(|_| todo!("linker error after spawning"))?;

            // Extend the lifetime of the tempfile so it doesn't get dropped
            // (and thus deleted) before the child process is done using it!
            let _ = builtins_host_tempfile;

            if !exit_status.success() {
                todo!("linker failed with exit code {:?}", exit_status.code());
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

fn determine_built_host_path(
    platform_main_roc_path: &Path,
    target: Target,
    build_host_requested: bool,
    link_type: LinkType,
    linking_strategy: LinkingStrategy,
    suppress_build_host_warning: bool,
) -> BuiltHostOpt {
    if build_host_requested {
        if !suppress_build_host_warning {
            // TODO
            //report_rebuilding_existing_host(&preprocessed_host.to_string_lossy());
            unimplemented!()
        }

        match link_type {
            LinkType::Executable => BuiltHostOpt::None,
            LinkType::Dylib => {
                eprintln!("You asked me to build the host, but I don't know how to rebuild a host for a dynamic library.");
                std::process::exit(1);
            }
            LinkType::None => {
                eprintln!("You asked me to build the host, but I don't know how to rebuild a host for an unlinked object.");
                std::process::exit(1);
            }
        }
    } else {
        match linking_strategy {
            LinkingStrategy::Legacy => {
                let legacy_host_path_res = target.find_legacy_host(platform_main_roc_path);

                match legacy_host_path_res {
                    Ok(legacy_host_path) => BuiltHostOpt::Legacy(legacy_host_path),
                    Err(err_msg) => {
                        eprintln!("Legacy linking failed: {}", err_msg);
                        #[cfg(target_os = "linux")]
                        eprintln!(
                            "\n    TIP: Maybe try surgical linking with the flag --linker=surgical"
                        );
                        std::process::exit(1);
                    }
                }
            }
            LinkingStrategy::Surgical => {
                let surgical_artifacts = target.find_surgical_host(platform_main_roc_path);

                match surgical_artifacts {
                    Ok(surgical_artifacts) => BuiltHostOpt::Surgical(surgical_artifacts),
                    Err(paths_str) => {
                        // TODO improve error message
                        eprintln!(
                            "LinkingStrategy was set to Surgical (default), but \
                            I tried to find the surgical host at any of these paths {} but it does not exist.",
                            paths_str
                        );
                        std::process::exit(1);
                    }
                }
            }
            LinkingStrategy::Additive => {
                unimplemented!()
            }
        }
    }
}

/// Get outut path for the executable.
///
/// If you specified a path that ends in in a directory separator, then
/// use that directory, but use the app module's filename for the filename.
fn get_exe_path(
    out_path: Option<&Path>,
    app_module_path: &Path,
    target: Target,
    linking_strategy: LinkingStrategy,
    link_type: LinkType,
) -> PathBuf {
    match out_path {
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

            if ends_with_sep {
                let filename = app_module_path.file_name().unwrap_or_default();

                with_output_extension(&path.join(filename), target, linking_strategy, link_type)
            } else {
                path.to_path_buf()
            }
        }
        None => with_output_extension(app_module_path, target, linking_strategy, link_type),
    }
}

#[allow(dead_code)]
fn report_rebuilding_existing_host(host_path: &str) {
    eprintln!(
        indoc::indoc!(
            r#"
            WARNING: I found an existing compiled host at:

                {}

            However, the --build-host flag was set! I will rebuild the host and overwrite the existing file.

            Remove the --build-host flag to use the existing host and silence this warning.
            Rebuilding hosts using the roc compiler is deprecated and will be removed in a future version.
            "#
        ),
        host_path,
    );
}

#[allow(dead_code)]
fn report_rebuilding_missing_host(host_path: &str) {
    eprintln!(
        indoc::indoc!(
            r#"
            WARNING: I was expecting a prebuilt host to exist at:

                {}

            However, it was not there! I will rebuild the host and write it to that location.

            Rebuilding hosts using the roc compiler is deprecated and will be removed in a future version.
            "#
        ),
        host_path,
    );
}

#[allow(dead_code)]
fn report_missing_prebuilt_host(msg: &str) {
    eprintln!(
        indoc::indoc!(
            r#"
            I was expecting a prebuilt host to exist:

                {}

            However, it was not there!

            If you have the platform's source code locally, you may be able to generate it by using a build script.
            "#
        ),
        msg
    );
}

#[allow(dead_code)]
fn report_refusing_to_rebuild_host(host_path: &str) {
    eprintln!(
        indoc::indoc!(
            r#"
            I found a prebuilt host for this platform, but you requested to rebuild it:

                {}

            Remove the `--build-host` flag to use the prebuilt host.
            The `--build-host` flag is deprecated and will be removed in a future release.
            "#
        ),
        host_path,
    );
}

fn spawn_wasm32_host_build_thread(
    opt_level: OptLevel,
    target: Target,
    platform_main_roc: PathBuf,
    output_path: PathBuf,
) -> std::thread::JoinHandle<(u128, BuiltHostOpt)> {
    std::thread::spawn(move || {
        // Printing to stderr because we want stdout to contain only the output of the roc program.
        // We are aware of the trade-offs.
        // `cargo run` follows the same approach
        eprintln!("🔨 Building host ...");

        let start = Instant::now();

        let host_dest = rebuild_host(opt_level, target, platform_main_roc.as_path(), None);

        preprocess_host_wasm32(host_dest.as_path(), &output_path);

        (
            start.elapsed().as_millis(),
            BuiltHostOpt::Additive(output_path),
        )
    })
}

/// Note this will copy the preprocessed host to the executable location
/// where the surgical linker will modify that copy in-place.
fn spawn_surgical_host_build_thread(
    opt_level: OptLevel,
    target: Target,
    platform_main_roc: PathBuf,
    dll_stub_symbols: Vec<String>,
    preprocessed_path: PathBuf,
    output_exe_path: PathBuf,
    metadata_path: PathBuf,
) -> std::thread::JoinHandle<(u128, BuiltHostOpt)> {
    std::thread::spawn(move || {
        // Printing to stderr because we want stdout to contain only the output of the roc program.
        // We are aware of the trade-offs.
        // `cargo run` follows the same approach
        eprintln!("🔨 Building host ...");

        let start = Instant::now();

        let stub_lib = roc_linker::generate_stub_lib_from_loaded(
            target,
            platform_main_roc.as_path(),
            dll_stub_symbols.as_slice(),
        );

        debug_assert!(stub_lib.exists());

        let host_exe = rebuild_host(
            opt_level,
            target,
            platform_main_roc.as_path(),
            Some(&stub_lib),
        );

        roc_linker::preprocess_host(
            target,
            host_exe.as_path(),
            metadata_path.as_path(),
            preprocessed_path.as_path(),
            &stub_lib,
            false,
            false,
        );

        // Copy preprocessed host to executable location.
        // The surgical linker will modify that copy in-place.
        std::fs::copy(&preprocessed_path, &output_exe_path).unwrap();

        (
            start.elapsed().as_millis(),
            BuiltHostOpt::Surgical(SurgicalHostArtifacts {
                metadata: metadata_path,
                preprocessed_host: preprocessed_path,
            }),
        )
    })
}

// Note the output host will be
fn spawn_legacy_host_build_thread(
    opt_level: OptLevel,
    target: Target,
    platform_main_roc: PathBuf,
) -> std::thread::JoinHandle<(u128, BuiltHostOpt)> {
    std::thread::spawn(move || {
        // Printing to stderr because we want stdout to contain only the output of the roc program.
        // We are aware of the trade-offs.
        // `cargo run` follows the same approach
        eprintln!("🔨 Building host ...");

        let start = Instant::now();

        let host_dest = rebuild_host(opt_level, target, platform_main_roc.as_path(), None);

        (start.elapsed().as_millis(), BuiltHostOpt::Legacy(host_dest))
    })
}

#[allow(clippy::too_many_arguments)]
pub fn check_file<'a>(
    arena: &'a Bump,
    roc_file_path: PathBuf,
    opt_main_path: Option<PathBuf>,
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
        function_kind: FunctionKind::from_env(),
        // TODO: expose this from CLI?
        render: RenderTarget::ColorTerminal,
        palette: DEFAULT_PALETTE,
        threading,
        exec_mode: ExecutionMode::Check,
    };
    let mut loaded = roc_load::load_and_typecheck(
        arena,
        roc_file_path,
        opt_main_path,
        roc_cache_dir,
        load_config,
    )?;

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
    build_host_requested: bool,
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
        None,
        roc_cache_dir,
        load_config,
    )
    .map_err(|e| BuildFileError::from_mono_error(e, compilation_start))?;

    // we are in a test, so we don't need to provide a warning about rebuilding the host
    let suppress_build_host_warning = true;

    build_loaded_file(
        arena,
        target,
        app_module_path.to_path_buf(),
        code_gen_options,
        emit_timings,
        link_type,
        linking_strategy,
        build_host_requested,
        suppress_build_host_warning,
        wasm_dev_stack_bytes,
        loaded,
        compilation_start,
        None,
        false,
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
