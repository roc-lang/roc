use bumpalo::Bump;
use roc_build::{link::link, program};
use roc_collections::all::MutMap;
use roc_gen::llvm::build::OptLevel;
use roc_load::file::LoadingProblem;
use std::fs;
use std::path::PathBuf;
use std::time::{Duration, SystemTime};
use target_lexicon::Triple;

fn report_timing(buf: &mut String, label: &str, duration: Duration) {
    buf.push_str(&format!(
        "        {:.3} ms   {}\n",
        duration.as_secs_f64() * 1000.0,
        label,
    ));
}

pub fn build_file(
    target: &Triple,
    src_dir: PathBuf,
    filename: PathBuf,
    opt_level: OptLevel,
) -> Result<PathBuf, LoadingProblem> {
    let compilation_start = SystemTime::now();
    let arena = Bump::new();

    // Step 1: compile the app and generate the .o file
    let subs_by_module = MutMap::default();

    // Release builds use uniqueness optimizations
    let stdlib = match opt_level {
        OptLevel::Normal => roc_builtins::std::standard_stdlib(),
        OptLevel::Optimize => roc_builtins::unique::uniq_stdlib(),
    };
    let monomorphize = roc_load::file::Phases::Monomorphize;
    let loaded = roc_load::file::load(
        filename.clone(),
        &stdlib,
        src_dir.as_path(),
        subs_by_module,
        monomorphize,
    )?;
    let dest_filename = filename.with_file_name("roc_app.o");
    let buf = &mut String::with_capacity(1024);

    for (module_id, module_timing) in loaded.timings.iter() {
        let module_name = loaded.interns.module_name(*module_id);

        buf.push_str("    ");
        buf.push_str(module_name);
        buf.push_str("\n");

        report_timing(buf, "Read .roc file from disk", module_timing.read_roc_file);
        report_timing(buf, "Parse header", module_timing.parse_header);
        report_timing(buf, "Parse body", module_timing.parse_body);
        report_timing(buf, "Canonicalize", module_timing.canonicalize);
        report_timing(buf, "Constrain", module_timing.constrain);
        report_timing(buf, "Solve", module_timing.solve);
        report_timing(buf, "Other", module_timing.other());
        buf.push('\n');
        report_timing(buf, "Total", module_timing.total());
    }

    println!(
        "\n\nCompilation finished! Here's how long each module took to compile:\n\n{}",
        buf
    );

    program::gen(
        &arena,
        loaded,
        filename,
        Triple::host(),
        &dest_filename,
        opt_level,
    );

    let compilation_end = compilation_start.elapsed().unwrap();

    println!(
        "Finished compilation and code gen in {} ms\n",
        compilation_end.as_millis()
    );

    let cwd = dest_filename.parent().unwrap();

    // Step 2: link the precompiled host and compiled app
    let host_input_path = cwd.join("platform").join("host.o");
    let binary_path = cwd.join("app"); // TODO should be app.exe on Windows

    // TODO try to move as much of this linking as possible to the precompiled
    // host, to minimize the amount of host-application linking required.
    let cmd_result =  // TODO use lld
        link(
            target,
            binary_path.as_path(),
            host_input_path.as_path(),
            dest_filename.as_path(),
        )
        .map_err(|_| {
            todo!("gracefully handle `rustc` failing to spawn.");
        })?
        .wait()
        .map_err(|_| {
            todo!("gracefully handle error after `rustc` spawned");
        });

    // Clean up the leftover .o file from the Roc, if possible.
    // (If cleaning it up fails, that's fine. No need to take action.)
    // TODO compile the dest_filename to a tmpdir, as an extra precaution.
    let _ = fs::remove_file(dest_filename);

    // If the cmd errored out, return the Err.
    cmd_result?;

    Ok(binary_path)
}
