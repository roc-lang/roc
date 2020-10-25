use bumpalo::Bump;
use roc_build::{
    link::{link, rebuild_host, LinkType},
    program,
};
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
    roc_file_path: PathBuf,
    opt_level: OptLevel,
    link_type: LinkType,
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
    let loaded = roc_load::file::load_and_monomorphize(
        &arena,
        roc_file_path.clone(),
        stdlib,
        src_dir.as_path(),
        subs_by_module,
    )?;
    let app_o_file = roc_file_path.with_file_name("roc_app.o");
    let buf = &mut String::with_capacity(1024);

    for (module_id, module_timing) in loaded.timings.iter() {
        let module_name = loaded.interns.module_name(*module_id);

        buf.push_str("    ");
        buf.push_str(module_name);
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
    }

    println!(
        "\n\nCompilation finished! Here's how long each module took to compile:\n\n{}",
        buf
    );

    program::gen_from_mono_module(
        &arena,
        loaded,
        roc_file_path,
        Triple::host(),
        &app_o_file,
        opt_level,
    );

    println!("\nSuccess! 🎉\n\n\t➡ {}\n", app_o_file.display());

    let compilation_end = compilation_start.elapsed().unwrap();

    println!(
        "Finished compilation and code gen in {} ms\n",
        compilation_end.as_millis()
    );

    let cwd = app_o_file.parent().unwrap();

    // Step 2: link the precompiled host and compiled app
    let host_input_path = cwd.join("platform").join("host.o");
    let binary_path = cwd.join("app"); // TODO should be app.exe on Windows

    // TODO we should no longer need to do this once we have platforms on
    // a package repository, as we can then get precompiled hosts from there.
    rebuild_host(host_input_path.as_path());

    // TODO try to move as much of this linking as possible to the precompiled
    // host, to minimize the amount of host-application linking required.
    let (mut child, binary_path) =  // TODO use lld
        link(
            target,
            binary_path,
            &[host_input_path.as_path().to_str().unwrap(), app_o_file.as_path().to_str().unwrap()],
            link_type
        )
        .map_err(|_| {
            todo!("gracefully handle `rustc` failing to spawn.");
        })?;

    let cmd_result = child.wait().map_err(|_| {
        todo!("gracefully handle error after `rustc` spawned");
    });

    // Clean up the leftover .o file from the Roc, if possible.
    // (If cleaning it up fails, that's fine. No need to take action.)
    // TODO compile the app_o_file to a tmpdir, as an extra precaution.
    let _ = fs::remove_file(app_o_file);

    // If the cmd errored out, return the Err.
    cmd_result?;

    Ok(binary_path)
}
