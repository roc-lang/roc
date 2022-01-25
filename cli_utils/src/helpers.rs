extern crate bumpalo;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;
extern crate tempfile;

use roc_cli::repl::{INSTRUCTIONS, WELCOME_MESSAGE};
use serde::Deserialize;
use serde_xml_rs::from_str;
use std::env;
use std::io::Read;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, ExitStatus, Stdio};
use tempfile::NamedTempFile;

#[derive(Debug)]
pub struct Out {
    pub stdout: String,
    pub stderr: String,
    pub status: ExitStatus,
}

pub fn path_to_roc_binary() -> PathBuf {
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

    path.push("roc");

    path
}

#[allow(dead_code)]
pub fn run_roc(args: &[&str]) -> Out {
    let mut cmd = Command::new(path_to_roc_binary());

    for arg in args {
        cmd.arg(arg);
    }

    let output = cmd
        .output()
        .expect("failed to execute compiled `roc` binary in CLI test");

    Out {
        stdout: String::from_utf8(output.stdout).unwrap(),
        stderr: String::from_utf8(output.stderr).unwrap(),
        status: output.status,
    }
}

#[allow(dead_code)]
pub fn run_cmd(cmd_name: &str, stdin_vals: &[&str], args: &[&str]) -> Out {
    let mut cmd = Command::new(cmd_name);

    for arg in args {
        cmd.arg(arg);
    }

    let mut child = cmd
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .unwrap_or_else(|_| panic!("failed to execute cmd `{}` in CLI test", cmd_name));

    {
        let stdin = child.stdin.as_mut().expect("Failed to open stdin");

        for stdin_str in stdin_vals {
            stdin
                .write_all(stdin_str.as_bytes())
                .expect("Failed to write to stdin");
        }
    }

    let output = child
        .wait_with_output()
        .unwrap_or_else(|_| panic!("failed to execute cmd `{}` in CLI test", cmd_name));

    Out {
        stdout: String::from_utf8(output.stdout).unwrap(),
        stderr: String::from_utf8(output.stderr).unwrap(),
        status: output.status,
    }
}

#[allow(dead_code)]
pub fn run_with_valgrind(stdin_vals: &[&str], args: &[&str]) -> (Out, String) {
    //TODO: figure out if there is a better way to get the valgrind executable.
    let mut cmd = Command::new("valgrind");
    let named_tempfile =
        NamedTempFile::new().expect("Unable to create tempfile for valgrind results");
    let filepath = named_tempfile.path().to_str().unwrap();

    cmd.arg("--tool=memcheck");
    cmd.arg("--xml=yes");

    // If you are having valgrind issues on MacOS, you may need to suppress some
    // of the errors. Read more here: https://github.com/rtfeldman/roc/issues/746
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

    cmd.arg(format!("--xml-file={}", filepath));

    for arg in args {
        cmd.arg(arg);
    }

    cmd.stdin(Stdio::piped());
    cmd.stdout(Stdio::piped());
    cmd.stderr(Stdio::piped());

    let mut child = cmd
        .spawn()
        .expect("failed to execute compiled `valgrind` binary in CLI test");

    {
        let stdin = child.stdin.as_mut().expect("Failed to open stdin");

        for stdin_str in stdin_vals {
            stdin
                .write_all(stdin_str.as_bytes())
                .expect("Failed to write to stdin");
        }
    }

    let output = child
        .wait_with_output()
        .expect("failed to execute compiled `valgrind` binary in CLI test");

    let mut file = named_tempfile.into_file();
    let mut raw_xml = String::new();

    file.read_to_string(&mut raw_xml).unwrap();

    (
        Out {
            stdout: String::from_utf8(output.stdout).unwrap(),
            stderr: String::from_utf8(output.stderr).unwrap(),
            status: output.status,
        },
        raw_xml,
    )
}

#[derive(Debug, Deserialize)]
struct ValgrindOutput {
    #[serde(rename = "$value")]
    pub fields: Vec<ValgrindField>,
}

#[derive(Deserialize, Debug)]
#[serde(rename_all = "lowercase")]
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
pub fn root_dir() -> PathBuf {
    let mut path = env::current_exe().ok().unwrap();

    // Get rid of the filename in target/debug/deps/cli_run-99c65e4e9a1fbd06
    path.pop();

    // If we're in deps/ get rid of deps/ in target/debug/deps/
    if path.ends_with("deps") {
        path.pop();
    }

    // Get rid of target/debug/ so we're back at the project root
    path.pop();
    path.pop();

    path
}

#[allow(dead_code)]
pub fn examples_dir(dir_name: &str) -> PathBuf {
    let mut path = root_dir();

    // Descend into examples/{dir_name}
    path.push("examples");
    path.push(dir_name);

    path
}

#[allow(dead_code)]
pub fn example_file(dir_name: &str, file_name: &str) -> PathBuf {
    let mut path = examples_dir(dir_name);

    path.push(file_name);

    path
}

#[allow(dead_code)]
pub fn fixtures_dir(dir_name: &str) -> PathBuf {
    let mut path = root_dir();

    // Descend into cli/tests/fixtures/{dir_name}
    path.push("cli");
    path.push("tests");
    path.push("fixtures");
    path.push(dir_name);

    path
}

#[allow(dead_code)]
pub fn fixture_file(dir_name: &str, file_name: &str) -> PathBuf {
    let mut path = fixtures_dir(dir_name);

    path.push(file_name);

    path
}

#[allow(dead_code)]
pub fn known_bad_file(file_name: &str) -> PathBuf {
    let mut path = root_dir();

    // Descend into cli/tests/known_bad/{file_name}
    path.push("cli");
    path.push("tests");
    path.push("known_bad");
    path.push(file_name);

    path
}

#[allow(dead_code)]
pub fn repl_eval(input: &str) -> Out {
    let mut cmd = Command::new(path_to_roc_binary());

    cmd.arg("repl");

    let mut child = cmd
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .spawn()
        .expect("failed to execute compiled `roc` binary in CLI test");

    {
        let stdin = child.stdin.as_mut().expect("Failed to open stdin");

        // Send the input expression
        stdin
            .write_all(input.as_bytes())
            .expect("Failed to write input to stdin");

        // Evaluate the expression
        stdin
            .write_all(b"\n")
            .expect("Failed to write newline to stdin");

        // Gracefully exit the repl
        stdin
            .write_all(b":exit\n")
            .expect("Failed to write :exit to stdin");
    }

    let output = child
        .wait_with_output()
        .expect("Error waiting for REPL child process to exit.");

    // Remove the initial instructions from the output.

    let expected_instructions = format!("{}{}", WELCOME_MESSAGE, INSTRUCTIONS);
    let stdout = String::from_utf8(output.stdout).unwrap();

    assert!(
        stdout.starts_with(&expected_instructions),
        "Unexpected repl output: {}",
        stdout
    );

    let (_, answer) = stdout.split_at(expected_instructions.len());
    let answer = if answer.is_empty() {
        // The repl crashed before completing the evaluation.
        // This is most likely due to a segfault.
        if output.status.to_string() == "signal: 11" {
            panic!(
                "repl segfaulted during the test. Stderr was {:?}",
                String::from_utf8(output.stderr).unwrap()
            );
        } else {
            panic!("repl exited unexpectedly before finishing evaluation. Exit status was {:?} and stderr was {:?}", output.status, String::from_utf8(output.stderr).unwrap());
        }
    } else {
        let expected_after_answer = "\n".to_string();

        assert!(
            answer.ends_with(&expected_after_answer),
            "Unexpected repl output after answer: {}",
            answer
        );

        // Use [1..] to trim the leading '\n'
        // and (len - 1) to trim the trailing '\n'
        let (answer, _) = answer[1..].split_at(answer.len() - expected_after_answer.len() - 1);

        // Remove ANSI escape codes from the answer - for example:
        //
        //     Before: "42 \u{1b}[35m:\u{1b}[0m Num *"
        //     After:  "42 : Num *"
        strip_ansi_escapes::strip(answer).unwrap()
    };

    Out {
        stdout: String::from_utf8(answer).unwrap(),
        stderr: String::from_utf8(output.stderr).unwrap(),
        status: output.status,
    }
}
