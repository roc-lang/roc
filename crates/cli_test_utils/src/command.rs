use std::io::Write;
use std::{
    ffi::OsString,
    process::{Command, ExitStatus, Stdio},
};

use regex::Regex;
use roc_command_utils::pretty_command_string;
use roc_reporting::report::ANSI_STYLE_CODES;

pub fn run_command(mut cmd: Command, stdin_opt: Option<&str>) -> CmdOut {
    let cmd_str = pretty_command_string(&cmd);

    let command = cmd
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped());

    let mut roc_cmd_child = command.spawn().unwrap_or_else(|err| {
        panic!("Failed to execute command:\n\n  {cmd_str:?}\n\nwith error:\n\n  {err}",)
    });

    let stdin = roc_cmd_child.stdin.as_mut().expect("Failed to open stdin");

    if let Some(stdin_str) = stdin_opt {
        stdin.write_all(stdin_str.as_bytes()).unwrap_or_else(|err| {
            panic!(
                "Failed to write to stdin for command\n\n  {cmd_str:?}\n\nwith error:\n\n  {err}"
            )
        });
    }
    let roc_cmd_output = roc_cmd_child.wait_with_output().unwrap_or_else(|err| {
        panic!("Failed to get output for command\n\n  {cmd_str:?}\n\nwith error:\n\n  {err}")
    });

    CmdOut {
        stdout: String::from_utf8(roc_cmd_output.stdout).unwrap(),
        stderr: String::from_utf8(roc_cmd_output.stderr).unwrap(),
        status: roc_cmd_output.status,
        cmd_str,
    }
}

#[derive(Debug)]
pub struct CmdOut {
    pub stdout: String,
    pub stderr: String,
    pub status: ExitStatus,
    pub cmd_str: OsString, // command with all its arguments, for easy debugging
}

impl std::fmt::Display for CmdOut {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Command: {}\n\nExit Code: {}\n\nStdout:\n{}\n\nStderr:\n{}",
            self.cmd_str.to_str().unwrap(),
            self.status,
            self.stdout,
            self.stderr
        )
    }
}

impl CmdOut {
    /// Assert that the command succeeded and that there are no unexpected errors in the stderr.
    pub fn assert_clean_success(&self) {
        self.assert_success_with_no_unexpected_errors();
    }

    pub fn assert_zero_exit(&self) {
        assert!(self.status.success(), "Command failed\n\n{self}");
    }

    pub fn assert_nonzero_exit(&self) {
        assert!(!self.status.success());
    }

    /// Assert that the command succeeded and that there are no unexpected errors in the stderr.
    /// This DOES NOT normalise the output, use assert_stdout_ends_with for that.
    pub fn assert_success_with_no_unexpected_errors(&self) {
        assert!(self.status.success(), "Command failed\n\n{self}");
        assert_no_unexpected_error(&self.stderr);
    }

    pub fn assert_clean_stdout(&self, expected_out: &str) {
        self.assert_clean_success();

        let normalised_output = normalize_for_tests(&self.stdout);

        assert_eq!(normalised_output, expected_out);
    }

    /// Assert that the stdout ends with the expected string
    /// This normalises the output for comparison in tests such as replacing timings
    /// with a placeholder, or stripping ANSI colors
    pub fn assert_stdout_and_stderr_ends_with(&self, expected: &str) {
        let normalised_stdout_stderr = self.normalize_stdout_and_stderr();

        assert!(
            normalised_stdout_stderr.ends_with(expected),
            "\n{}EXPECTED stdout and stderr after normalizing:\n----------------\n{}{}\n{}ACTUAL stdout and stderr after normalizing:\n----------------\n{}{}{}\n----------------\n{}",
            ANSI_STYLE_CODES.cyan,
            ANSI_STYLE_CODES.reset,
            expected,
            ANSI_STYLE_CODES.cyan,
            ANSI_STYLE_CODES.reset,
            normalised_stdout_stderr,
            ANSI_STYLE_CODES.cyan,
            ANSI_STYLE_CODES.reset,
        );
    }

    pub fn normalize_stdout_and_stderr(&self) -> String {
        format!(
            "{}{}",
            normalize_for_tests(&self.stdout),
            normalize_for_tests(&self.stderr)
        )
    }
}

pub fn assert_no_unexpected_error(stderr: &str) {
    let mut stderr_part_clean = String::from(stderr);

    let expected_errors = [
        "🔨 Building host ...\n",
        "ld: warning: -undefined dynamic_lookup may not work with chained fixups",
        "warning: ignoring debug info with an invalid version (0) in app\r\n",
    ];
    for expected_error in expected_errors {
        stderr_part_clean = stderr_part_clean.replacen(expected_error, "", 1);
    }

    let clean_stderr = stderr_part_clean.trim();

    assert!(
        clean_stderr.is_empty(),
        "Unexpected error:\n{}",
        clean_stderr
    );
}

use lazy_static::lazy_static;

/// Replaces things that are annoying to compare in tests with placeholders.
fn normalize_for_tests(original_output: &str) -> String {
    // normalise from windows line endings to unix line endings
    let mut part_normalized = original_output.replace("\r\n", "\n");

    // remove ANSI color codes
    part_normalized = roc_reporting::report::strip_colors(&part_normalized);

    // replace timings with a placeholder
    let replacement = " in <ignored for test> ms.";
    part_normalized = TIMING_REGEX
        .replace_all(&part_normalized, replacement)
        .into_owned();

    // replace file paths with a placeholder
    let filepath_replacement = "[<ignored for tests>:$2]";
    part_normalized = FILEPATH_REGEX
        .replace_all(&part_normalized, filepath_replacement)
        .into_owned();

    // replace error summary timings
    let error_summary_replacement = "$1 error$2 and $3 warning$4 found in <ignored for test> ms";
    ERROR_SUMMARY_REGEX
        .replace_all(&part_normalized, error_summary_replacement)
        .into_owned()
}

// Used by ^normalize_for_tests^
lazy_static! {
    static ref TIMING_REGEX: Regex = Regex::new(r" in (\d+) ms\.").expect("Invalid regex pattern");
    static ref FILEPATH_REGEX: Regex =
        Regex::new(r"\[([^:]+):(\d+)\]").expect("Invalid filepath regex pattern");
    static ref ERROR_SUMMARY_REGEX: Regex =
        Regex::new(r"(\d+) error(s)? and (\d+) warning(s)? found in \d+ ms")
            .expect("Invalid error summary regex pattern");
}
