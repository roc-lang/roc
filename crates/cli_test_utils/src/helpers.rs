extern crate bumpalo;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;
extern crate tempfile;

use roc_command_utils::{cargo, root_dir};
use std::env;
use std::path::PathBuf;

pub fn path_to_roc_binary() -> PathBuf {
    path_to_binary(if cfg!(windows) { "roc.exe" } else { "roc" })
}

pub fn path_to_binary(binary_name: &str) -> PathBuf {
    // Adapted from https://github.com/volta-cli/volta/blob/cefdf7436a15af3ce3a38b8fe53bb0cfdb37d3dd/tests/acceptance/support/sandbox.rs#L680
    // by the Volta Contributors - license information can be found in
    // the LEGAL_DETAILS file in the root directory of this distribution.
    //
    // Thank you, Volta contributors!
    let mut path = env::var_os("CARGO_BIN_PATH")
            .map(PathBuf::from)
            .or_else(|| {
                env::current_exe().ok().map(|mut path| {
                    path.pop();
                    if path.ends_with("deps") {
                        path.pop();
                    }
                    path
                })
            })
            .unwrap_or_else(|| panic!("CARGO_BIN_PATH wasn't set, and couldn't be inferred from context. Can't run CLI tests."));

    path.push(binary_name);

    path
}

// If we don't already have a /target/release/roc, build it!
pub fn build_roc_bin_cached() -> PathBuf {
    let roc_binary_path = path_to_roc_binary();

    if !roc_binary_path.exists() {
        build_roc_bin(&[]);
    }

    roc_binary_path
}

pub fn build_roc_bin(extra_args: &[&str]) -> PathBuf {
    let roc_binary_path = path_to_roc_binary();

    // Remove the /target/release/roc part
    let root_project_dir = roc_binary_path
        .parent()
        .unwrap()
        .parent()
        .unwrap()
        .parent()
        .unwrap();

    // cargo build --bin roc
    // (with --release iff the test is being built with --release)
    let mut args = if cfg!(debug_assertions) {
        vec!["build", "--bin", "roc"]
    } else {
        vec!["build", "--release", "--bin", "roc"]
    };

    args.extend(extra_args);

    let mut cargo_cmd = cargo();

    cargo_cmd.current_dir(root_project_dir).args(&args);

    let cargo_cmd_str = format!("{cargo_cmd:?}");

    let cargo_output = cargo_cmd.output().unwrap();

    if !cargo_output.status.success() {
        panic!(
            "The following cargo command failed:\n\n  {}\n\n  stdout was:\n\n    {}\n\n  stderr was:\n\n    {}\n",
            cargo_cmd_str,
            String::from_utf8(cargo_output.stdout).unwrap(),
            String::from_utf8(cargo_output.stderr).unwrap()
        );
    }

    roc_binary_path
}

#[allow(dead_code)]
pub fn dir_from_root(dir_name: &str) -> PathBuf {
    let mut path = root_dir();

    path.extend(dir_name.split('/')); // Make slashes cross-target

    path
}

pub fn file_from_root(dir_name: &str, file_name: &str) -> PathBuf {
    let mut path = dir_from_root(dir_name);

    path.push(file_name);

    path
}
