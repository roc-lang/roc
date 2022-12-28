use bumpalo::Bump;
use roc_build::{
    link::{
        legacy_host_filename, link, preprocess_host_wasm32, preprocessed_host_filename,
        rebuild_host, LinkType, LinkingStrategy,
    },
    program::{self, CodeGenOptions},
};
use roc_builtins::bitcode;
use roc_load::{
    EntryPoint, ExecutionMode, ExpectMetadata, LoadConfig, LoadMonomorphizedError, LoadedModule,
    LoadingProblem, Threading,
};
use roc_mono::ir::OptLevel;
use roc_packaging::cache::RocCacheDir;
use roc_reporting::{
    cli::Problems,
    report::{RenderTarget, DEFAULT_PALETTE},
};
use roc_target::TargetInfo;
use std::time::{Duration, Instant};
use std::{path::PathBuf, thread::JoinHandle};
use target_lexicon::Triple;

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

    let host_input_path = match &loaded.entry_point {
        EntryPoint::Executable { platform_path, .. } => {
            use roc_target::OperatingSystem::*;

            let host_filename = match operating_system {
                Wasi => "host.zig".to_string(),
                Unix => legacy_host_filename(target).unwrap(),
                Windows => legacy_host_filename(target).unwrap(),
            };

            platform_path.with_file_name(host_filename)
        }
        _ => unreachable!(),
    };

    let preprocessed_host_path = if linking_strategy == LinkingStrategy::Legacy {
        let filename = legacy_host_filename(target).unwrap();
        host_input_path.with_file_name(filename)
    } else {
        let filename = preprocessed_host_filename(target).unwrap();
        host_input_path.with_file_name(filename)
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
            host_input_path.clone(),
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
    let problems = program::report_problems_monomorphized(&mut loaded);
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

    let (roc_app_bytes, code_gen_timing, expect_metadata) = program::gen_from_mono_module(
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
                &host_input_path,
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
                inputs.push(host_input_path.as_path().to_str().unwrap());
            }

            if matches!(code_gen_options.backend, program::CodeGenBackend::Assembly) {
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
    host_input_path: PathBuf,
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
                    host_input_path.as_path(),
                    None,
                );

                preprocess_host_wasm32(host_dest.as_path(), &preprocessed_host_path);
            }
            LinkingStrategy::Surgical => {
                roc_linker::build_and_preprocess_host(
                    opt_level,
                    &thread_local_target,
                    host_input_path.as_path(),
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
                    host_input_path.as_path(),
                    None,
                );
            }
        }

        rebuild_host_start.elapsed().as_millis()
    })
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

    Ok((
        program::report_problems_typechecked(&mut loaded),
        compilation_end,
    ))
}
