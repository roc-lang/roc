extern crate bumpalo;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;
extern crate tempfile;

use regex::Regex;
use indoc::indoc;
use roc_command_utils::{cargo, pretty_command_string, root_dir};
use roc_reporting::report::ANSI_STYLE_CODES;
use std::env;
use std::ffi::{OsStr, OsString};
use std::io::Write;
use std::ops::Deref;
use std::path::PathBuf;
use std::process::{Command, ExitStatus, Stdio};
use std::sync::Mutex;
use tempfile::NamedTempFile;
use crate::command::{run_command, CmdOut};
use crate::run_roc::run_roc_exe;

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

pub fn roc_file_to_exe(roc_file: &str) -> String {
    let base_name = roc_file.trim_end_matches(".roc");
    #[cfg(windows)]
    return format!("{}.exe", base_name);
    #[cfg(not(windows))]
    return base_name.to_string();
}

/// Runs the roc executable and checks if it produces expected_output. If with_valgrind is true, the executable is also tested with valgrind.
pub fn check_output_maybe_valgrind(roc_exe_path: &PathBuf, expected_output: &str, with_valgrind: bool) {
    // valgrind first, in case of a segfault it will produce a more useful error message   
    if with_valgrind {
        check_exe_with_valgrind(&roc_exe_path);
    }
    
    let roc_exe_out = run_roc_exe(&roc_exe_path);
    roc_exe_out.assert_clean_stdout(expected_output);
}

pub fn check_exe_with_valgrind(roc_exe: &PathBuf) {
    let mut valgrind_cmd = Command::new("valgrind");
    valgrind_cmd.arg("--leak-check=full");
    valgrind_cmd.arg("--error-exitcode=1");
    valgrind_cmd.arg(&roc_exe);
    
    let valgrind_out = run_command(valgrind_cmd, &[]);
    
    if !valgrind_out.status.success() {
        panic!(
            indoc::indoc! {"
            >>>Valgrind found issues with {}:

                stdout:

                {}

                stderr:

                {}

            Valgrind command:

                {}
            "},
            roc_exe.display(),
            valgrind_out.stdout,
            valgrind_out.stderr,
            valgrind_out.cmd_str.to_str().unwrap()
        );
    }
}

