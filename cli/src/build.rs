use bumpalo::Bump;
use roc_build::{
    link::{link, rebuild_host, LinkType},
    program,
};
use roc_builtins::bitcode;
use roc_can::builtins::builtin_defs_map;
use roc_collections::all::MutMap;
use roc_load::file::LoadingProblem;
use roc_mono::ir::OptLevel;
use std::path::PathBuf;
use std::time::{Duration, SystemTime};
use target_lexicon::Triple;
use tempfile::Builder;

fn report_timing(buf: &mut String, label: &str, duration: Duration) {
    buf.push_str(&format!(
        "        {:9.3} ms   {}\n",
        duration.as_secs_f64() * 1000.0,
        label,
    ));
}

pub enum BuildOutcome {
    NoProblems,
    OnlyWarnings,
    Errors,
}

impl BuildOutcome {
    pub fn status_code(&self) -> i32 {
        match self {
            Self::NoProblems => 0,
            Self::OnlyWarnings => 1,
            Self::Errors => 2,
        }
    }
}

pub struct BuiltFile {
    pub binary_path: PathBuf,
    pub outcome: BuildOutcome,
    pub total_time: Duration,
}

#[allow(clippy::too_many_arguments)]
pub fn build_file<'a>(
    arena: &'a Bump,
    target: &Triple,
    src_dir: PathBuf,
    roc_file_path: PathBuf,
    opt_level: OptLevel,
    emit_debug_info: bool,
    emit_timings: bool,
    link_type: LinkType,
    surgically_link: bool,
    precompiled: bool,
    target_valgrind: bool,
) -> Result<BuiltFile, LoadingProblem<'a>> {
    let compilation_start = SystemTime::now();
    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;

    // Step 1: compile the app and generate the .o file
    let subs_by_module = MutMap::default();

    // Release builds use uniqueness optimizations
    let stdlib = arena.alloc(roc_builtins::std::standard_stdlib());

    let loaded = roc_load::file::load_and_monomorphize(
        arena,
        roc_file_path.clone(),
        stdlib,
        src_dir.as_path(),
        subs_by_module,
        ptr_bytes,
        builtin_defs_map,
    )?;

    use target_lexicon::Architecture;
    let emit_wasm = matches!(target.architecture, Architecture::Wasm32);

    // TODO wasm host extension should be something else ideally
    // .bc does not seem to work because
    //
    // > Non-Emscripten WebAssembly hasn't implemented __builtin_return_address
    //
    // and zig does not currently emit `.a` webassembly static libraries
    let host_extension = if emit_wasm { "zig" } else { "o" };
    let app_extension = if emit_wasm { "bc" } else { "o" };

    let cwd = roc_file_path.parent().unwrap();
    let mut binary_path = cwd.join(&*loaded.output_path); // TODO should join ".exe" on Windows

    if emit_wasm {
        binary_path.set_extension("wasm");
    }

    let mut host_input_path = PathBuf::from(cwd);
    let path_to_platform = loaded.platform_path.clone();
    host_input_path.push(&*path_to_platform);
    host_input_path.push("host");
    host_input_path.set_extension(host_extension);

    // TODO this should probably be moved before load_and_monomorphize.
    // To do this we will need to preprocess files just for their exported symbols.
    // Also, we should no longer need to do this once we have platforms on
    // a package repository, as we can then get precompiled hosts from there.
    let rebuild_thread = spawn_rebuild_thread(
        opt_level,
        surgically_link,
        precompiled,
        host_input_path.clone(),
        binary_path.clone(),
        target,
        loaded
            .exposed_to_host
            .keys()
            .map(|x| x.as_str(&loaded.interns).to_string())
            .collect(),
        target_valgrind,
    );

    // TODO try to move as much of this linking as possible to the precompiled
    // host, to minimize the amount of host-application linking required.
    let app_o_file = Builder::new()
        .prefix("roc_app")
        .suffix(&format!(".{}", app_extension))
        .tempfile()
        .map_err(|err| {
            todo!("TODO Gracefully handle tempfile creation error {:?}", err);
        })?;
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
        report_timing(
            buf,
            "Make Specializations",
            module_timing.make_specializations,
        );
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
    program::report_problems_monomorphized(&mut loaded);
    let loaded = loaded;

    let code_gen_timing = program::gen_from_mono_module(
        arena,
        loaded,
        &roc_file_path,
        target,
        app_o_file,
        opt_level,
        emit_debug_info,
    );

    buf.push('\n');
    buf.push_str("    ");
    buf.push_str("Code Generation");
    buf.push('\n');

    report_timing(buf, "Generate LLVM IR", code_gen_timing.code_gen);
    report_timing(buf, "Emit .o file", code_gen_timing.emit_o_file);

    let compilation_end = compilation_start.elapsed().unwrap();

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

    let rebuild_duration = rebuild_thread.join().unwrap();
    if emit_timings && !precompiled {
        println!(
            "Finished rebuilding and preprocessing the host in {} ms\n",
            rebuild_duration
        );
    }

    // Step 2: link the precompiled host and compiled app
    let link_start = SystemTime::now();
    let outcome = if surgically_link {
        roc_linker::link_preprocessed_host(target, &host_input_path, app_o_file, &binary_path)
            .map_err(|_| {
                todo!("gracefully handle failing to surgically link");
            })?;
        BuildOutcome::NoProblems
    } else {
        let mut inputs = vec![
            host_input_path.as_path().to_str().unwrap(),
            app_o_file.to_str().unwrap(),
        ];
        if matches!(opt_level, OptLevel::Development) {
            inputs.push(bitcode::BUILTINS_HOST_OBJ_PATH);
        }

        let (mut child, _) =  // TODO use lld
            link(
                target,
                binary_path.clone(),
                &inputs,
                link_type
            )
            .map_err(|_| {
                todo!("gracefully handle `ld` failing to spawn.");
            })?;

        let exit_status = child.wait().map_err(|_| {
            todo!("gracefully handle error after `ld` spawned");
        })?;

        // TODO change this to report whether there were errors or warnings!
        if exit_status.success() {
            BuildOutcome::NoProblems
        } else {
            BuildOutcome::Errors
        }
    };
    let linking_time = link_start.elapsed().unwrap();

    if emit_timings {
        println!("Finished linking in {} ms\n", linking_time.as_millis());
    }

    let total_time = compilation_start.elapsed().unwrap();

    Ok(BuiltFile {
        binary_path,
        outcome,
        total_time,
    })
}

#[allow(clippy::too_many_arguments)]
fn spawn_rebuild_thread(
    opt_level: OptLevel,
    surgically_link: bool,
    precompiled: bool,
    host_input_path: PathBuf,
    binary_path: PathBuf,
    target: &Triple,
    exported_symbols: Vec<String>,
    target_valgrind: bool,
) -> std::thread::JoinHandle<u128> {
    let thread_local_target = target.clone();
    std::thread::spawn(move || {
        let rebuild_host_start = SystemTime::now();
        if !precompiled {
            if surgically_link {
                roc_linker::build_and_preprocess_host(
                    opt_level,
                    &thread_local_target,
                    host_input_path.as_path(),
                    exported_symbols,
                    target_valgrind,
                )
                .unwrap();
            } else {
                rebuild_host(
                    opt_level,
                    &thread_local_target,
                    host_input_path.as_path(),
                    None,
                    target_valgrind,
                );
            }
        }
        if surgically_link {
            // Copy preprocessed host to executable location.
            let prehost = host_input_path.with_file_name("preprocessedhost");
            std::fs::copy(prehost, binary_path.as_path()).unwrap();
        }
        let rebuild_host_end = rebuild_host_start.elapsed().unwrap();
        rebuild_host_end.as_millis()
    })
}

#[allow(clippy::too_many_arguments)]
pub fn check_file(
    arena: &Bump,
    src_dir: PathBuf,
    roc_file_path: PathBuf,
    emit_timings: bool,
) -> Result<usize, LoadingProblem> {
    let compilation_start = SystemTime::now();

    // only used for generating errors. We don't do code generation, so hardcoding should be fine
    // we need monomorphization for when exhaustiveness checking
    let ptr_bytes = 8;

    // Step 1: compile the app and generate the .o file
    let subs_by_module = MutMap::default();

    // Release builds use uniqueness optimizations
    let stdlib = arena.alloc(roc_builtins::std::standard_stdlib());

    let mut loaded = roc_load::file::load_and_typecheck(
        arena,
        roc_file_path,
        stdlib,
        src_dir.as_path(),
        subs_by_module,
        ptr_bytes,
        builtin_defs_map,
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
        report_timing(
            buf,
            "Find Specializations",
            module_timing.find_specializations,
        );
        report_timing(
            buf,
            "Make Specializations",
            module_timing.make_specializations,
        );
        report_timing(buf, "Other", module_timing.other());
        buf.push('\n');
        report_timing(buf, "Total", module_timing.total());

        if it.peek().is_some() {
            buf.push('\n');
        }
    }

    let compilation_end = compilation_start.elapsed().unwrap();

    if emit_timings {
        println!(
            "\n\nCompilation finished!\n\nHere's how long each module took to compile:\n\n{}",
            buf
        );

        println!("Finished checking in {} ms\n", compilation_end.as_millis(),);
    }

    Ok(program::report_problems_typechecked(&mut loaded))
}
