extern crate bumpalo;
extern crate inlinable_string;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;
// extern crate roc_cli; // TODO FIXME why doesn't this resolve?

use std::env;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, ExitStatus, Stdio};

pub struct Out {
    pub stdout: String,
    pub stderr: String,
    pub status: ExitStatus,
}

// TODO get these from roc_cli::repl instead, after figuring out why
// `extern crate roc_cli;` doesn't work.
const WELCOME_MESSAGE: &str = "\n  The rockin’ \u{001b}[36mroc repl\u{001b}[0m\n\u{001b}[35m────────────────────────\u{001b}[0m\n\n";
const INSTRUCTIONS: &str =
    "Enter an expression, or :help for a list of commands, or :exit to exit.\n";
const PROMPT: &str = "\n\u{001b}[36m»\u{001b}[0m ";

// We use this to strip the welcome message and instructions from stdout,
// so we end with only the answer that got printed after the user's input.
const START_OF_ANSWER: usize =
    WELCOME_MESSAGE.len() + INSTRUCTIONS.len() + PROMPT.len() + "\n".len();

pub fn path_to_roc_binary() -> PathBuf {
    // Adapted from https://github.com/volta-cli/volta/blob/cefdf7436a15af3ce3a38b8fe53bb0cfdb37d3dd/tests/acceptance/support/sandbox.rs#L680 - BSD-2-Clause licensed
    let mut path = env::var_os("CARGO_BIN_PATH")
            .map(PathBuf::from)
            .or_else(|| {
                env::current_exe().ok().map(|mut path| {
                    path.pop();
                    if path.ends_with("deps") { path.pop();
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
pub fn example_dir(dir_name: &str) -> PathBuf {
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

    // Descend into examples/{dir_name}
    path.push("examples");
    path.push(dir_name);

    path
}

#[allow(dead_code)]
pub fn example_file(dir_name: &str, file_name: &str) -> PathBuf {
    let mut path = example_dir(dir_name);

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
            .write_all("\n".as_bytes())
            .expect("Failed to write newline to stdin");

        // Gracefully exit the repl
        stdin
            .write_all(":exit\n".as_bytes())
            .expect("Failed to write :exit to stdin");
    }

    let output = child
        .wait_with_output()
        .expect("Error waiting for REPL child process to exit.");

    // Remove the initial prompt from the output.

    let stdout = String::from_utf8(output.stdout).unwrap();
    let (instructions, answer) = stdout.split_at(START_OF_ANSWER);

    // TODO get these from roc_cli::repl instead, after figuring out why
    // `extern crate roc_cli;` doesn't work.
    assert_eq!(
        instructions,
        &format!("{}{}{}\n", WELCOME_MESSAGE, INSTRUCTIONS, PROMPT)
    );

    let expected_after_answer = format!("\n{}", PROMPT);
    let (answer, after_answer) = answer.split_at(answer.len() - expected_after_answer.len());

    assert_eq!(after_answer, expected_after_answer);

    // Remove ANSI escape codes from the answer - for example:
    //
    //     Before: "42 \u{1b}[35m:\u{1b}[0m Num *"
    //     After:  "42 : Num *"
    let answer = strip_ansi_escapes::strip(answer).unwrap();

    Out {
        stdout: String::from_utf8(answer).unwrap(),
        stderr: String::from_utf8(output.stderr).unwrap(),
        status: output.status,
    }
}
