use roc_repl_cli::WELCOME_MESSAGE;
use roc_repl_ui::SHORT_INSTRUCTIONS;
use roc_test_utils::assert_multiline_str_eq;
use std::env;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, ExitStatus, Stdio};

const ERROR_MESSAGE_START: char = '─';

#[derive(Debug)]
pub struct Out {
    pub stdout: String,
    pub stderr: String,
    pub status: ExitStatus,
}

fn path_to_roc_binary() -> PathBuf {
    // Adapted from https://github.com/volta-cli/volta/blob/cefdf7436a15af3ce3a38b8fe53bb0cfdb37d3dd/tests/acceptance/support/sandbox.rs#L680
    // by the Volta Contributors - license information can be found in
    // the legal_details file in the root directory of this distribution.
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

    let expected_instructions = format!("{WELCOME_MESSAGE}{SHORT_INSTRUCTIONS}");
    let stdout = String::from_utf8(output.stdout).unwrap();

    assert!(
        stdout.starts_with(&expected_instructions),
        "Unexpected repl output: {stdout}"
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
            "Unexpected repl output after answer: {answer}"
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

pub fn expect_success(input: &str, expected: &str) {
    let out = repl_eval(input.trim());

    assert_multiline_str_eq!("", out.stderr.as_str());

    let mut iter = out.stdout.lines().rev();
    let line = iter.next().unwrap();

    if line.is_empty() {
        assert_multiline_str_eq!(expected, iter.next().unwrap().trim_end());
    } else {
        assert_multiline_str_eq!(expected, line);
    }

    assert!(out.status.success());
}

pub fn expect_failure(input: &str, expected: &str) {
    let out = repl_eval(input);

    // there may be some other stuff printed (e.g. unification errors)
    // so skip till the header of the first error
    match out.stdout.find(ERROR_MESSAGE_START) {
        Some(index) => {
            assert_multiline_str_eq!("", out.stderr.as_str());
            assert_multiline_str_eq!(expected, &out.stdout[index..]);
            assert!(out.status.success());
        }
        None => {
            assert_multiline_str_eq!("", out.stderr.as_str());
            assert!(out.status.success());
            panic!(
                "I expected a failure, but there is no error message in stdout:\n\n{}",
                &out.stdout
            );
        }
    }
}
