use bumpalo::Bump;
use roc_build::{
    link::{link, preprocess_host_wasm32, rebuild_host, LinkType, LinkingStrategy},
    program::{self, Problems},
};
use roc_builtins::bitcode;
use roc_collections::VecMap;
use roc_load::{
    EntryPoint, ExecutionMode, Expectations, LoadConfig, LoadMonomorphizedError, LoadedModule,
    LoadingProblem, Threading,
};
use roc_module::symbol::{Interns, ModuleId};
use roc_mono::ir::OptLevel;
use roc_reporting::report::RenderTarget;
use roc_target::TargetInfo;
use std::time::{Duration, Instant};
use std::{path::PathBuf, thread::JoinHandle};
use target_lexicon::Triple;
use tempfile::Builder;

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

pub struct BuiltFile {
    pub binary_path: PathBuf,
    pub problems: Problems,
    pub total_time: Duration,
    pub expectations: VecMap<ModuleId, Expectations>,
    pub interns: Interns,
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

#[allow(clippy::too_many_arguments)]
pub fn build_file<'a>(
    arena: &'a Bump,
    target: &Triple,
    app_module_path: PathBuf,
    opt_level: OptLevel,
    emit_debug_info: bool,
    emit_timings: bool,
    link_type: LinkType,
    linking_strategy: LinkingStrategy,
    prebuilt: bool,
    threading: Threading,
    wasm_dev_stack_bytes: Option<u32>,
    order: BuildOrdering,
) -> Result<BuiltFile, BuildFileError<'a>> {
    let compilation_start = Instant::now();
    let target_info = TargetInfo::from(target);

    // Step 1: compile the app and generate the .o file
    let subs_by_module = Default::default();

    let exec_mode = match order {
        BuildOrdering::BuildIfChecks => ExecutionMode::ExecutableIfCheck,
        BuildOrdering::AlwaysBuild => ExecutionMode::Executable,
    };

    let load_config = LoadConfig {
        target_info,
        // TODO: expose this from CLI?
        render: RenderTarget::ColorTerminal,
        threading,
        exec_mode,
    };
    let load_result = roc_load::load_and_monomorphize(
        arena,
        app_module_path.clone(),
        subs_by_module,
        load_config,
    );
    let loaded = match load_result {
        Ok(loaded) => loaded,
        Err(LoadMonomorphizedError::LoadingProblem(problem)) => {
            return Err(BuildFileError::LoadingProblem(problem))
        }
        Err(LoadMonomorphizedError::ErrorModule(module)) => {
            return Err(BuildFileError::ErrorModule {
                module,
                total_time: compilation_start.elapsed(),
            })
        }
    };

    use target_lexicon::Architecture;
    let emit_wasm = matches!(target.architecture, Architecture::Wasm32);

    // TODO wasm host extension should be something else ideally
    // .bc does not seem to work because
    //
    // > Non-Emscripten WebAssembly hasn't implemented __builtin_return_address
    //
    // and zig does not currently emit `.a` webassembly static libraries
    let (host_extension, app_extension, extension) = {
        use roc_target::OperatingSystem::*;

        match roc_target::OperatingSystem::from(target.operating_system) {
            Wasi => {
                if matches!(opt_level, OptLevel::Development) {
                    ("wasm", "wasm", Some("wasm"))
                } else {
                    ("zig", "bc", Some("wasm"))
                }
            }
            Unix => ("o", "o", None),
            Windows => ("obj", "obj", Some("exe")),
        }
    };

    let cwd = app_module_path.parent().unwrap();
    let mut binary_path = cwd.join(&*loaded.output_path);

    if let Some(extension) = extension {
        binary_path.set_extension(extension);
    }

    let host_input_path = if let EntryPoint::Executable { platform_path, .. } = &loaded.entry_point
    {
        cwd.join(platform_path)
            .with_file_name("host")
            .with_extension(host_extension)
    } else {
        unreachable!();
    };

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

    let preprocessed_host_path = if emit_wasm {
        host_input_path.with_file_name("preprocessedhost.o")
    } else {
        host_input_path.with_file_name("preprocessedhost")
    };

    let rebuild_thread = spawn_rebuild_thread(
        opt_level,
        linking_strategy,
        prebuilt,
        host_input_path.clone(),
        preprocessed_host_path.clone(),
        binary_path.clone(),
        target,
        exposed_values,
        exposed_closure_types,
    );

    let app_o_file = Builder::new()
        .prefix("roc_app")
        .suffix(&format!(".{}", app_extension))
        .tempfile()
        .map_err(|err| todo!("TODO Gracefully handle tempfile creation error {:?}", err))?;
    let app_o_file = app_o_file.path();
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
        report_timing(
            buf,
            "Find Specializations",
            module_timing.find_specializations,
        );
        let multiple_make_specializations_passes = module_timing.make_specializations.len() > 1;
        for (i, pass_time) in module_timing.make_specializations.iter().enumerate() {
            let suffix = if multiple_make_specializations_passes {
                format!(" (Pass {})", i)
            } else {
                String::new()
            };
            report_timing(buf, &format!("Make Specializations{}", suffix), *pass_time);
        }
        report_timing(buf, "Other", module_timing.other());
        buf.push('\n');
        report_timing(buf, "Total", module_timing.total());

        if it.peek().is_some() {
            buf.push('\n');
        }
    }

    // This only needs to be mutable for report_problems. This can't be done
    // inside a nested scope without causing a borrow error!
    let mut loaded = loaded;
    let problems = program::report_problems_monomorphized(&mut loaded);
    let expectations = std::mem::take(&mut loaded.expectations);
    let loaded = loaded;

    let interns = loaded.interns.clone();

    enum HostRebuildTiming {
        BeforeApp(u128),
        ConcurrentWithApp(JoinHandle<u128>),
    }

    let rebuild_timing = if linking_strategy == LinkingStrategy::Additive {
        let rebuild_duration = rebuild_thread.join().unwrap();
        if emit_timings && !prebuilt {
            println!(
                "Finished rebuilding the platform in {} ms\n",
                rebuild_duration
            );
        }
        HostRebuildTiming::BeforeApp(rebuild_duration)
    } else {
        HostRebuildTiming::ConcurrentWithApp(rebuild_thread)
    };

    let code_gen_timing = program::gen_from_mono_module(
        arena,
        loaded,
        &app_module_path,
        target,
        app_o_file,
        opt_level,
        emit_debug_info,
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
    report_timing(buf, "Emit .o file", code_gen_timing.emit_o_file);

    let compilation_end = compilation_start.elapsed();

    let size = std::fs::metadata(&app_o_file)
        .unwrap_or_else(|err| {
            panic!(
                "Could not open {:?} - which was supposed to have been generated. Error: {:?}",
                app_o_file, err
            );
        })
        .len();

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

    if let HostRebuildTiming::ConcurrentWithApp(thread) = rebuild_timing {
        let rebuild_duration = thread.join().unwrap();
        if emit_timings && !prebuilt {
            println!(
                "Finished rebuilding the platform in {} ms\n",
                rebuild_duration
            );
        }
    }

    // Step 2: link the prebuilt platform and compiled app
    let link_start = Instant::now();
    let problems = match (linking_strategy, link_type) {
        (LinkingStrategy::Surgical, _) => {
            roc_linker::link_preprocessed_host(target, &host_input_path, app_o_file, &binary_path);
            problems
        }
        (LinkingStrategy::Additive, _) | (LinkingStrategy::Legacy, LinkType::None) => {
            // Just copy the object file to the output folder.
            binary_path.set_extension(app_extension);
            std::fs::copy(app_o_file, &binary_path).unwrap();
            problems
        }
        (LinkingStrategy::Legacy, _) => {
            let mut inputs = vec![
                host_input_path.as_path().to_str().unwrap(),
                app_o_file.to_str().unwrap(),
            ];

            let str_host_obj_path = bitcode::get_builtins_host_obj_path();

            if matches!(opt_level, OptLevel::Development) {
                inputs.push(&str_host_obj_path);
            }

            let (mut child, _) =  // TODO use lld
            link(target, binary_path.clone(), &inputs, link_type)
                .map_err(|_| todo!("gracefully handle `ld` failing to spawn."))?;

            let exit_status = child
                .wait()
                .map_err(|_| todo!("gracefully handle error after `ld` spawned"))?;

            if exit_status.success() {
                problems
            } else {
                let mut problems = problems;

                // Add an error for `ld` failing
                problems.errors += 1;

                problems
            }
        }
    };

    let linking_time = link_start.elapsed();

    if emit_timings {
        println!("Finished linking in {} ms\n", linking_time.as_millis());
    }

    let total_time = compilation_start.elapsed();

    Ok(BuiltFile {
        binary_path,
        problems,
        total_time,
        interns,
        expectations,
    })
}

#[allow(clippy::too_many_arguments)]
fn spawn_rebuild_thread(
    opt_level: OptLevel,
    linking_strategy: LinkingStrategy,
    prebuilt: bool,
    host_input_path: PathBuf,
    preprocessed_host_path: PathBuf,
    binary_path: PathBuf,
    target: &Triple,
    exported_symbols: Vec<String>,
    exported_closure_types: Vec<String>,
) -> std::thread::JoinHandle<u128> {
    let thread_local_target = target.clone();
    std::thread::spawn(move || {
        if !prebuilt {
            // Printing to stderr because we want stdout to contain only the output of the roc program.
            // We are aware of the trade-offs.
            // `cargo run` follows the same approach
            eprintln!("🔨 Rebuilding platform...");
        }

        let rebuild_host_start = Instant::now();

        if !prebuilt {
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
        }
        if linking_strategy == LinkingStrategy::Surgical {
            // Copy preprocessed host to executable location.
            std::fs::copy(preprocessed_host_path, binary_path.as_path()).unwrap();
        }
        let rebuild_host_end = rebuild_host_start.elapsed();

        rebuild_host_end.as_millis()
    })
}

#[allow(clippy::too_many_arguments)]
pub fn check_file(
    arena: &Bump,
    roc_file_path: PathBuf,
    emit_timings: bool,
    threading: Threading,
) -> Result<(program::Problems, Duration), LoadingProblem> {
    let compilation_start = Instant::now();

    // only used for generating errors. We don't do code generation, so hardcoding should be fine
    // we need monomorphization for when exhaustiveness checking
    let target_info = TargetInfo::default_x86_64();

    // Step 1: compile the app and generate the .o file
    let subs_by_module = Default::default();

    let load_config = LoadConfig {
        target_info,
        // TODO: expose this from CLI?
        render: RenderTarget::ColorTerminal,
        threading,
        exec_mode: ExecutionMode::Check,
    };
    let mut loaded =
        roc_load::load_and_typecheck(arena, roc_file_path, subs_by_module, load_config)?;

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
