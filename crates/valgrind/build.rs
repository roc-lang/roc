use std::path::PathBuf;

use roc_build::{
    link::{LinkType, LinkingStrategy},
    program::{CodeGenBackend, CodeGenOptions},
};
use roc_cli::build::BuildOrdering;
use roc_load::Threading;
use roc_mono::ir::OptLevel;

fn main() {
    // goal: build the platform, so tests can use `precompiled-platform=true`
    // idea: just compile one of the tests; that'll re-build the platform for us
    let app_module_path = "tests/str_concat_1.roc";

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
    let prebuilt_requested = false;
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

    assert!(res_binary_path.is_ok());
}
