extern crate bumpalo;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;
extern crate tempfile;

use regex::Regex;
use roc_command_utils::{cargo, pretty_command_string, root_dir};
use roc_reporting::report::ANSI_STYLE_CODES;
use serde::Deserialize;
use serde_xml_rs::from_str;
use std::env;
use std::ffi::{OsStr, OsString};
use std::io::Write;
use std::ops::Deref;
use std::path::PathBuf;
use std::process::{Command, ExitStatus, Stdio};
use std::sync::Mutex;
use tempfile::NamedTempFile;

lazy_static::lazy_static! {
    pub static ref COMMON_STDERR: [ExpectedString; 4] = [
        "🔨 Building host ...\n".into(),
        "ld: warning: -undefined dynamic_lookup may not work with chained fixups".into(),
        "warning: ignoring debug info with an invalid version (0) in app\r\n".into(),
        ExpectedString::new_fuzzy(r"runtime: .*ms\n"),
    ];
}

// Since glue is always compiling the same plugin, it can not be run in parallel.
// That would lead to a race condition in writing the output shared library.
// Thus, all calls to glue in a test are made sequential.
// TODO: In the future, look into compiling the shared library once and then caching it.
static GLUE_LOCK: Mutex<()> = Mutex::new(());

#[derive(Debug)]
pub struct Out {
    pub cmd_str: OsString, // command with all its arguments, for easy debugging
    pub stdout: String,
    pub stderr: String,
    pub status: ExitStatus,
    pub run: ExecCLI,
    pub valgrind_xml: Option<NamedTempFile>,
}

impl std::fmt::Display for Out {
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

impl Out {
    /// Assert that the command succeeded and that there are no unexpected errors in the stderr.
    pub fn assert_clean_success(&self) {
        self.assert_success_with_no_unexpected_errors(COMMON_STDERR.as_slice());
    }

    pub fn assert_nonzero_exit(&self) {
        assert!(!self.status.success());
    }

    /// Assert that the command succeeded and that there are no unexpected errors in the stderr.
    /// This DOES NOT normalise the output, use assert_stdout_ends_with for that.
    pub fn assert_success_with_no_unexpected_errors(&self, expected_errors: &[ExpectedString]) {
        assert!(self.status.success(), "Command failed\n\n{self}");
        assert!(
            !has_unexpected_error(&self.stderr, expected_errors),
            "Unexpected error:\n{}",
            self.stderr
        );
    }

    /// Normalise the output for comparison in tests by replacing with a placeholder
    fn normalize_for_tests(input: &str) -> String {
        // normalise from windows line endings to unix line endings
        let without_clrf = input.replace("\r\n", "\n");

        // remove ANSI color codes
        let without_ansi = roc_reporting::report::strip_colors(without_clrf.as_str());

        // replace timings with a placeholder
        let regex = Regex::new(r" in (\d+) ms\.").expect("Invalid regex pattern");
        let replacement = " in <ignored for test> ms.";
        let without_timings = regex.replace_all(&without_ansi, replacement);

        // replace file paths with a placeholder
        let filepath_regex =
            Regex::new(r"\[([^:]+):(\d+)\]").expect("Invalid filepath regex pattern");
        let filepath_replacement = "[<ignored for tests>:$2]";
        let without_filepaths = filepath_regex.replace_all(&without_timings, filepath_replacement);

        // replace error summary timings
        let error_summary_regex =
            Regex::new(r"(\d+) error(?:s)? and (\d+) warning(?:s)? found in \d+ ms")
                .expect("Invalid error summary regex pattern");
        let error_summary_replacement = "$1 error and $2 warning found in <ignored for test> ms";
        error_summary_regex
            .replace_all(&without_filepaths, error_summary_replacement)
            .to_string()
    }

    /// Assert that the stdout ends with the expected string
    /// This normalises the output for comparison in tests such as replacing timings
    /// with a placeholder, or stripping ANSI colors
    pub fn assert_stdout_and_stderr_ends_with(&self, expected: &str) {
        let normalised_output = format!(
            "{}{}",
            Out::normalize_for_tests(&self.stdout),
            Out::normalize_for_tests(&self.stderr)
        );

        assert!(
            normalised_output.ends_with(expected),
            "\n{}EXPECTED stdout and stderr after normalizing:\n----------------\n{}{}\n{}ACTUAL stdout and stderr after normalizing:\n----------------\n{}{}{}\n----------------\n{}",
            ANSI_STYLE_CODES.cyan,
            ANSI_STYLE_CODES.reset,
            expected,
            ANSI_STYLE_CODES.cyan,
            ANSI_STYLE_CODES.reset,
            normalised_output,
            ANSI_STYLE_CODES.cyan,
            ANSI_STYLE_CODES.reset,
        );
    }
}

/// A builder for running a command.
///
/// Unlike `std::process::Command`, this builder is clonable and provides convenience methods for
/// constructing Commands for the configured run and instrumenting the configured command with valgrind.
#[derive(Debug, Clone)]
pub struct ExecCLI {
    mode: Mode,
    args: Vec<OsString>,
    env: Vec<(String, String)>,
    stdin_vals: Vec<&'static str>,
    cwd: Option<OsString>,
    run_with_valgrind: bool,
}

#[derive(Debug, Clone)]
pub enum Mode {
    /// `roc build app.roc` followed by `./app` 
    BuildAndRun,
    /// dev/test/check/...
    SingleCmd
}

// We're using the builder pattern to enable optional arguments
impl ExecCLI {
    pub fn new<S>(exe: S) -> Self
    where
        S: AsRef<OsStr>,
    {
        let exe: OsString = exe.as_ref().into();
        Self {
            mode: Mode::SingleCmd,
            args: vec![exe],
            stdin_vals: vec![],
            env: vec![],
            cwd: None,
            run_with_valgrind: false,
        }
    }

    pub fn new_roc() -> Self {
        Self::new(path_to_roc_binary())
    }

    pub fn command(&self) -> Command {
        let mut cmd = Command::new(&self.args[0]);
        for arg in self.args[1..].iter() {
            cmd.arg(arg);
        }
        for (k, v) in self.env.iter() {
            cmd.env(k, v);
        }
        cmd
    }

    pub fn valgrind_command(&self) -> (Command, NamedTempFile) {
        let mut cmd = Command::new("valgrind");
        let named_tempfile =
            NamedTempFile::new().expect("Unable to create tempfile for valgrind results");

        cmd.arg("--tool=memcheck");
        cmd.arg("--xml=yes");
        cmd.arg(format!("--xml-file={}", named_tempfile.path().display()));

        // If you are having valgrind issues on MacOS, you may need to suppress some
        // of the errors. Read more here: https://github.com/roc-lang/roc/issues/746
        if let Some(suppressions_file_os_str) = env::var_os("VALGRIND_SUPPRESSIONS") {
            match suppressions_file_os_str.to_str() {
                None => {
                    panic!("Could not determine suppression file location from OsStr");
                }
                Some(suppressions_file) => {
                    let mut buf = String::new();

                    buf.push_str("--suppressions=");
                    buf.push_str(suppressions_file);

                    cmd.arg(buf);
                }
            }
        }

        for arg in self.args.iter() {
            cmd.arg(arg);
        }

        (cmd, named_tempfile)
    }

    pub fn set_mode(mut self, mode: Mode) -> Self {
        self.mode = mode;
        self
    }

    pub fn arg<S>(mut self, arg: S) -> Self
    where
        S: Into<OsString>,
    {
        self.args.push(arg.into());
        self
    }

    pub fn add_args<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<OsStr>,
    {
        for arg in args {
            self = self.arg(&arg);
        }
        self
    }

    pub fn add_arg_if<S>(mut self, arg: S, enabled: bool) -> Self
    where
        S: Into<OsString>,
    {
        if enabled {
            self.args.push(arg.into());
        }
        self
    }

    pub fn get_args(&self) -> impl Iterator<Item = &OsStr> {
        self.args.iter().map(Deref::deref)
    }

    pub fn get_env(&self) -> impl Iterator<Item = (&str, &str)> {
        self.env.iter().map(|(k, v)| (k.as_str(), v.as_str()))
    }

    pub fn with_stdin_vals<I>(mut self, stdin_vals: I) -> Self
    where
        I: IntoIterator<Item = &'static str>,
    {
        self.stdin_vals.extend(stdin_vals);
        self
    }

    pub fn with_valgrind(mut self, use_valgrind: bool) -> Self {
        self.run_with_valgrind = use_valgrind;
        self
    }

    pub fn cwd<S>(mut self, arg: S) -> Self
    where
        S: Into<OsString>,
    {
        self.cwd = Some(arg.into());
        self
    }

    pub fn with_env<'a, I>(&mut self, env: I) -> &mut Self
    where
        I: IntoIterator<Item = (&'a str, &'a str)>,
    {
        for (k, v) in env {
            self.env.push((k.into(), v.into()));
        }
        self
    }

    fn run_with_command(self, mut cmd: Command) -> Out {
        let cmd_str = pretty_command_string(&cmd);

        let command = cmd
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped());

        if let Some(cwd) = &self.cwd {
            command.current_dir(cwd);
        }

        let mut roc_cmd_child = command.spawn().unwrap_or_else(|err| {
            panic!("Failed to execute command\n\n  {cmd_str:?}\n\nwith error:\n\n  {err}",)
        });

        let stdin = roc_cmd_child.stdin.as_mut().expect("Failed to open stdin");

        for stdin_str in self.stdin_vals.iter() {
            stdin
                .write_all(stdin_str.as_bytes())
                .unwrap_or_else(|err| {
                    panic!("Failed to write to stdin for command\n\n  {cmd_str:?}\n\nwith error:\n\n  {err}")
                });
        }
        let roc_cmd_output = roc_cmd_child.wait_with_output().unwrap_or_else(|err| {
            panic!("Failed to get output for command\n\n  {cmd_str:?}\n\nwith error:\n\n  {err}")
        });

        Out {
            cmd_str,
            stdout: String::from_utf8(roc_cmd_output.stdout).unwrap(),
            stderr: String::from_utf8(roc_cmd_output.stderr).unwrap(),
            status: roc_cmd_output.status,
            valgrind_xml: None,
            run: self,
        }
    }

    pub fn run(self) -> Out {
        if self.run_with_valgrind {
            self.run_with_valgrind()
        } else {
            let command = self.command();
            self.run_with_command(command)
        }
    }

    pub fn run_glue(self) -> Out {
        let _guard = GLUE_LOCK.lock().unwrap();
        self.run()
    }

    pub fn run_with_valgrind(self) -> Out {
        let (valgrind_cmd, valgrind_xml) = self.valgrind_command();
        let mut out = self.run_with_command(valgrind_cmd);
        out.valgrind_xml = Some(valgrind_xml);
        out
    }
}

pub fn has_unexpected_error(stderr: &str, expected_errors: &[ExpectedString]) -> bool {
    let mut stderr_stripped = String::from(stderr);
    for expected_error in expected_errors {
        stderr_stripped = expected_error.replacen(&stderr_stripped, "", 1);
    }
    !stderr_stripped.trim().is_empty()
}

pub enum ExpectedString {
    Exact(String),
    Fuzzy(regex::Regex),
}

impl ExpectedString {
    pub fn new(s: &str) -> ExpectedString {
        ExpectedString::Exact(s.to_string())
    }

    pub fn new_fuzzy(s: &str) -> ExpectedString {
        let r = regex::Regex::new(s).unwrap();
        ExpectedString::Fuzzy(r)
    }

    pub fn found_in(&self, haystack: &str) -> bool {
        match self {
            ExpectedString::Exact(sub) => haystack.contains(sub),
            ExpectedString::Fuzzy(regex) => regex.is_match(haystack),
        }
    }

    pub fn replacen(&self, haystack: &str, replacement: &str, n: usize) -> String {
        match self {
            ExpectedString::Exact(sub) => haystack.replacen(sub, replacement, n),
            ExpectedString::Fuzzy(regex) => regex.replacen(haystack, n, replacement).to_string(),
        }
    }
}

impl From<&str> for ExpectedString {
    fn from(s: &str) -> Self {
        ExpectedString::new(s)
    }
}

impl From<regex::Regex> for ExpectedString {
    fn from(r: regex::Regex) -> Self {
        Self::Fuzzy(r)
    }
}

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

#[derive(Debug, Deserialize)]
struct ValgrindOutput {
    #[serde(rename = "$value")]
    pub fields: Vec<ValgrindField>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
#[allow(dead_code)] // Some fields are unused but this allows for easy deserialization of the xml.
enum ValgrindField {
    ProtocolVersion(isize),
    ProtocolTool(String),
    Preamble(ValgrindDummyStruct),
    Pid(isize),
    PPid(isize),
    Tool(String),
    Args(ValgrindDummyStruct),
    Error(ValgrindError),
    Status(ValgrindDummyStruct),
    Stack(ValgrindDummyStruct),
    #[serde(rename = "fatal_signal")]
    FatalSignal(ValgrindDummyStruct),
    ErrorCounts(ValgrindDummyStruct),
    SuppCounts(ValgrindDummyStruct),
}

#[derive(Debug, Deserialize)]
struct ValgrindDummyStruct {}

#[derive(Debug, Deserialize, Clone)]
pub struct ValgrindError {
    pub kind: String,
    #[serde(default)]
    pub what: Option<String>,
    #[serde(default)]
    pub xwhat: Option<ValgrindErrorXWhat>,
}

#[derive(Debug, Deserialize, Clone)]
pub struct ValgrindErrorXWhat {
    pub text: String,
    #[serde(default)]
    pub leakedbytes: Option<isize>,
    #[serde(default)]
    pub leakedblocks: Option<isize>,
}

#[allow(dead_code)]
pub fn extract_valgrind_errors(xml: &str) -> Result<Vec<ValgrindError>, serde_xml_rs::Error> {
    let parsed_xml: ValgrindOutput = from_str(xml)?;
    let answer = parsed_xml
        .fields
        .iter()
        .filter_map(|field| match field {
            ValgrindField::Error(err) => Some(err.clone()),
            _ => None,
        })
        .collect();

    Ok(answer)
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
