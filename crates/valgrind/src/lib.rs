#![cfg(test)]

use std::{
    ffi::OsStr,
    path::{Path, PathBuf},
};

use roc_build::{
    link::{LinkType, LinkingStrategy},
    program::{CodeGenBackend, CodeGenOptions},
};
use roc_cli::build::{BuildOrdering, BuiltFile};
use roc_load::Threading;
use roc_mono::ir::OptLevel;

fn list_files(dir: &std::path::Path) -> std::vec::Vec<PathBuf> {
    std::fs::read_dir(dir)
        .unwrap()
        .map(|f| PathBuf::from(f.unwrap().file_name()))
        .collect::<std::vec::Vec<_>>()
}

fn run_example(app_module_path: impl AsRef<Path>) {
    let app_module_path = app_module_path.as_ref();

    let arena = bumpalo::Bump::new();
    let triple = target_lexicon::Triple::host();

    let code_gen_options = CodeGenOptions {
        backend: CodeGenBackend::Llvm,
        opt_level: OptLevel::Normal,
        emit_debug_info: false,
    };

    let emit_timings = false;
    let link_type = LinkType::Executable;
    let linking_strategy = LinkingStrategy::Surgical;
    let prebuilt_requested = true;
    let wasm_dev_stack_bytes = None;

    let roc_cache_dir = roc_packaging::cache::RocCacheDir::Disallowed;
    let build_ordering = BuildOrdering::AlwaysBuild;

    let res_binary_path = roc_cli::build::build_file(
        &arena,
        &triple,
        PathBuf::from(app_module_path),
        code_gen_options,
        emit_timings,
        link_type,
        linking_strategy,
        prebuilt_requested,
        Threading::AtMost(2),
        wasm_dev_stack_bytes,
        roc_cache_dir,
        build_ordering,
    );

    match res_binary_path {
        Ok(BuiltFile {
            binary_path,
            problems,
            total_time: _,
            expect_metadata: _,
        }) => {
            if problems.exit_code() != 0 {
                panic!("there are problems")
            }
            // If possible, report the generated executable name relative to the current dir.
            let generated_filename = binary_path
                .strip_prefix(std::env::current_dir().unwrap())
                .unwrap_or(&binary_path)
                .to_str()
                .unwrap();

            let (out, _raw_xml) =
                cli_utils::helpers::run_with_valgrind([], &[generated_filename.to_string()]);

            if !out.status.success() {
                panic!(
                    "Running the application `{:?}` failed!\n\n{}\n\n{}",
                    out.cmd_str, out.stdout, out.stderr
                );
            }
        }
        Err(e) => panic!("{:?}", e),
    }
}

#[test]
fn it_works() {
    for dir_entry in std::fs::read_dir("tests").unwrap() {
        let path = dir_entry.unwrap().path();

        if path.extension() != Some(OsStr::new("roc")) {
            continue;
        }

        run_example(path);
    }
}
