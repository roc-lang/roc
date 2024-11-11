fn main() {
    println!("cargo::rerun-if-changed=./host.zig");
    println!("cargo::rerun-if-changed=./libhost.a");
    println!("cargo::rerun-if-changed=./host.lib");

    roc_command_utils::zig()
        .arg("build-lib")
        .args([
            "--mod",
            &format!("glue::{}", zig_glue_path().as_path().display()),
            "--deps",
            "glue",
            "-lc", // include libc
            &format!("{}", host_path().as_path().display()),
        ])
        .status()
        .expect("Failed to build the zig project");
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
