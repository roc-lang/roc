use bumpalo::Bump;
use roc_gen_llvm::llvm::build::LlvmBackendMode;
use roc_mono::ir::OptLevel;
use roc_test_utils_dir::workspace_root;
use std::mem::ManuallyDrop;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=./host.zig");
    println!("cargo:rerun-if-changed=./libhost.a");
    println!("cargo:rerun-if-changed=./host.lib");

    println!(
        "cargo::rustc-env=ROC_ZIG_GLUE_PATH={}",
        zig_glue_path().as_path().display()
    );

    let target = roc_target::Target::default();
    let link_type = roc_build::link::LinkType::Executable;
    let arena = ManuallyDrop::new(Bump::new());
    let opt_level = OptLevel::Optimize;
    let code_gen_backend = roc_build::program::CodeGenBackend::Llvm(LlvmBackendMode::Binary);
    let emit_llvm_ir = false;
    let emit_debug_info = false;
    let emit_timings = false;
    let threading = roc_load::Threading::Single;
    let linking_strategy = roc_build::link::LinkingStrategy::Legacy; // LinkingStrategy::Legacy
    let build_host = true;
    let suppress_build_host_warning = true;

    let fuzz = false;
    let wasm_dev_stack_bytes = None;
    let build_ordering = roc_build::program::BuildOrdering::BuildIfChecks;

    let code_gen_options = roc_build::program::CodeGenOptions {
        backend: code_gen_backend,
        opt_level,
        emit_debug_info,
        emit_llvm_ir,
        fuzz,
    };

    let load_config = roc_build::program::standard_load_config(target, build_ordering, threading);
    let cache_dir = roc_packaging::cache::roc_cache_packages_dir();
    let roc_cache_dir = roc_packaging::cache::RocCacheDir::Persistent(cache_dir.as_path());
    let out_path = None;

    let res_binary_path = roc_build::program::build_file(
        &arena,
        target,
        stub_path(),
        code_gen_options,
        emit_timings,
        link_type,
        linking_strategy,
        build_host,
        suppress_build_host_warning,
        wasm_dev_stack_bytes,
        roc_cache_dir,
        load_config,
        out_path,
    )
    .unwrap();

    // let output = std::process::Command::new(roc_cli_path())
    //     .arg("build")
    //     .arg("--build-host")
    //     .arg("--suppress-build-host-warning")
    //     .arg(format!("{}", stub_path().as_path().display()))
    //     .output()
    //     .unwrap();

    // dbg!(&output);

    // assert!(output.status.success());
    // assert!(output.stderr.is_empty());
    // assert!(output.stdout.is_empty());

    // roc_command_utils::zig()
    //     .arg("build-lib")
    //     .args([
    //         "--mod",
    //         &format!("glue::{}", zig_glue_path().as_path().display()),
    //         "--deps",
    //         "glue",
    //         "-lc", // include libc
    //         &format!("{}", host_path().as_path().display()),
    //     ])
    //     .status()
    //     .expect("Failed to build the zig project");
}

fn roc_cli_path() -> std::path::PathBuf {
    let out_dir = PathBuf::from(
        std::env::var("OUT_DIR").expect("Could not find the OUT_DIR environment variable"),
    );

    let cli_path = out_dir.join("..").join("..").join("..").join("roc");

    if cli_path.exists() {
        cli_path
    } else {
        panic!("Could not find the roc binary at the path: {:?}", cli_path);
    }
}

fn stub_path() -> std::path::PathBuf {
    roc_test_utils_dir::workspace_root()
        .join("crates")
        .join("test-platform-simple-zig")
        .join("app.roc")
}

fn host_path() -> std::path::PathBuf {
    roc_test_utils_dir::workspace_root()
        .join("crates")
        .join("test-platform-simple-zig")
        .join("host.zig")
}

fn zig_glue_path() -> std::path::PathBuf {
    roc_test_utils_dir::workspace_root()
        .join("crates")
        .join("compiler")
        .join("builtins")
        .join("bitcode")
        .join("src")
        .join("glue.zig")
}
