use bumpalo::Bump;
use roc_build::{
    link::{link, rebuild_host, LinkType},
    program,
};
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

pub struct BuiltFile {
    pub binary_path: PathBuf,
    pub outcome: BuildOutcome,
    pub total_time: Duration,
}

#[cfg(feature = "llvm")]
pub fn build_file<'a>(
    arena: &'a Bump,
    target: &Triple,
    src_dir: PathBuf,
    roc_file_path: PathBuf,
    opt_level: OptLevel,
    emit_debug_info: bool,
    link_type: LinkType,
) -> Result<BuiltFile, LoadingProblem<'a>> {
    let compilation_start = SystemTime::now();
    let ptr_bytes = target.pointer_width().unwrap().bytes() as u32;

    // Step 1: compile the app and generate the .o file
    let subs_by_module = MutMap::default();

    // Release builds use uniqueness optimizations
    let stdlib = match opt_level {
        OptLevel::Normal => arena.alloc(roc_builtins::std::standard_stdlib()),
        OptLevel::Optimize => arena.alloc(roc_builtins::std::standard_stdlib()),
    };

    let loaded = roc_load::file::load_and_monomorphize(
        &arena,
        roc_file_path.clone(),
        stdlib,
        src_dir.as_path(),
        subs_by_module,
        ptr_bytes,
        builtin_defs_map,
    )?;

    let path_to_platform = loaded.platform_path.clone();
    let app_o_file = Builder::new()
        .prefix("roc_app")
        .suffix(".o")
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

    let cwd = roc_file_path.parent().unwrap();
    let binary_path = cwd.join(&*loaded.output_path); // TODO should join ".exe" on Windows
    let code_gen_timing = program::gen_from_mono_module(
        &arena,
        loaded,
        &roc_file_path,
        Triple::host(),
        &app_o_file,
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

    if emit_debug_info {
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

    // Step 2: link the precompiled host and compiled app
    let mut host_input_path = PathBuf::from(cwd);

    host_input_path.push(&*path_to_platform);
    host_input_path.push("host.o");

    // TODO we should no longer need to do this once we have platforms on
    // a package repository, as we can then get precompiled hosts from there.
    let rebuild_host_start = SystemTime::now();
    rebuild_host(host_input_path.as_path());
    let rebuild_host_end = rebuild_host_start.elapsed().unwrap();

    if emit_debug_info {
        println!(
            "Finished rebuilding the host in {} ms\n",
            rebuild_host_end.as_millis()
        );
    }

    // TODO try to move as much of this linking as possible to the precompiled
    // host, to minimize the amount of host-application linking required.
    let link_start = SystemTime::now();
    let (mut child, binary_path) =  // TODO use lld
        link(
            target,
            binary_path,
            &[host_input_path.as_path().to_str().unwrap(), app_o_file.to_str().unwrap()],
            link_type
        )
        .map_err(|_| {
            todo!("gracefully handle `rustc` failing to spawn.");
        })?;

    let cmd_result = child.wait().map_err(|_| {
        todo!("gracefully handle error after `rustc` spawned");
    });

    let linking_time = link_start.elapsed().unwrap();

    if emit_debug_info {
        println!("Finished linking in {} ms\n", linking_time.as_millis());
    }

    let total_time = compilation_start.elapsed().unwrap();

    // If the cmd errored out, return the Err.
    cmd_result?;

    // TODO change this to report whether there were errors or warnings!
    let outcome = BuildOutcome::NoProblems;

    Ok(BuiltFile {
        binary_path,
        outcome,
        total_time,
    })
}
