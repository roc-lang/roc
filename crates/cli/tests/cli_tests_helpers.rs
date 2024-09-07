use regex::Regex;
use std::ffi::{OsString};
use std::process::{ExitStatus};
use tempfile::NamedTempFile;

/*
    let ignore_stderr: [&str; 4] = [
        "🔨 Building host ...\n",
        "ld: warning: -undefined dynamic_lookup may not work with chained fixups",
        "warning: ignoring debug info with an invalid version (0) in app\r\n",
        "runtime: .*ms\n",
    ];
*/

#[derive(Debug)]
pub struct CmdOut {
    pub cmd_str: OsString, // command with all its arguments, for easy debugging
    pub stdout: String,
    pub stderr: String,
    pub status: ExitStatus,
}

impl std::fmt::Display for CmdOut {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "Command: {}\n\nExit Code: {}\nStdout:\n{}\n\nStderr:\n{}",
            self.cmd_str.to_str().unwrap(),
            self.status,
            self.stdout,
            self.stderr
        )
    }
}


fn clean_test_output(dirty_output: &str) -> String {
    let part_cleaned_output = dirty_output.replace("\r\n", "\n");
    let part_cleaned_output = roc_reporting::report::strip_colors(&part_cleaned_output);

    let part_cleaned_output = TIMING_REGEX.replace_all(&part_cleaned_output, " in <ignored for test> ms.");
    let part_cleaned_output = FILEPATH_REGEX.replace_all(&part_cleaned_output, "[<ignored for tests>:$2]");

    ERROR_SUMMARY_REGEX.replace_all(&part_cleaned_output, "$1 error and $2 warning found in <ignored for test> ms")
        .into_owned()
}

// used in ^clean_output_for_tests^
// lazy_static to avoid building these regex for every test
lazy_static::lazy_static! {
    static ref TIMING_REGEX: Regex = Regex::new(r" in (\d+) ms\.").unwrap();
    static ref FILEPATH_REGEX: Regex = Regex::new(r"\[([^:]+):(\d+)\]").unwrap();
    static ref ERROR_SUMMARY_REGEX: Regex = Regex::new(r"(\d+) error(?:s)? and (\d+) warning(?:s)? found in \d+ ms").unwrap();
}