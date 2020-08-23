#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;
extern crate inlinable_string;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;

mod helpers;

#[cfg(test)]
mod repl_run {
    use crate::helpers::{example_file, path_to_roc_binary, Out};
    use std::io::{Read, Write};
    use std::process::{Command, Stdio};

    #[test]
    fn eval_repl() {
        roc_repl(&["0"], &[|stdout, stderr| assert_eq!(stdout, "0 : Num *")]);
    }

    pub fn roc_repl<F: Fn(String, String)>(inputs: &[&str], checks: &[F]) {
        let mut cmd = Command::new(path_to_roc_binary());

        cmd.arg("repl");

        let mut child = cmd
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .spawn()
            .expect("failed to execute compiled `roc` binary in CLI test");

        for (input, check) in inputs.iter().zip(checks) {
            {
                let stdin = child.stdin.as_mut().expect("Failed to open stdin");

                stdin
                    .write_all(input.as_bytes())
                    .expect("Failed to write to stdin");

                stdin
                    .write_all("\n".as_bytes())
                    .expect("Failed to write newline to stdin");
            }

            {
                let mut stdout = Vec::with_capacity(1024);

                child
                    .stdout
                    .as_mut()
                    .expect("Failed to open stdout")
                    .read(&mut stdout)
                    .expect("Error reading from stdout");
                // let mut stderr = Vec::with_capacity(1024);

                // child
                //     .stderr
                //     .as_mut()
                //     .expect("Failed to open stderr")
                //     .read(&mut stderr)
                //     .expect("Error reading from stderr");

                check(
                    String::from_utf8(stdout).unwrap(),
                    "TODO stderr".to_string(),
                    // String::from_utf8(stderr).unwrap(),
                );
            }
        }

        // Gracefully exit the repl
        {
            let stdin = child.stdin.as_mut().expect("Failed to open stdin");

            stdin
                .write_all(":exit\n".as_bytes())
                .expect("Failed to write :exit to stdin");
        }

        let output = child
            .wait_with_output()
            .expect("Error waiting for REPL child process to exit.");
    }
}
