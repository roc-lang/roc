#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;
extern crate inlinable_string;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;

#[cfg(test)]
mod cli_run {
    use std::env;
    use std::path::PathBuf;
    use std::process::{Command, ExitStatus};

    // HELPERS

    pub struct Out {
        pub stdout: String,
        pub stderr: String,
        pub status: ExitStatus,
    }

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

    pub fn example_file(dir_name: &str, file_name: &str) -> PathBuf {
        let mut path = example_dir(dir_name);

        path.push(file_name);

        path
    }

    // TESTS

    #[test]
    fn run_hello_world() {
        let out = run_roc(&[
            "run",
            example_file("hello-world", "Hello.roc").to_str().unwrap(),
        ]);

        assert_eq!(&out.stderr, "");
        assert!(&out.stdout.ends_with("Hello, World!\n"));
        assert!(out.status.success());
    }

    #[test]
    fn run_quicksort() {
        let out = run_roc(&[
            "run",
            example_file("quicksort", "Quicksort.roc").to_str().unwrap(),
            "--optimize",
        ]);

        assert_eq!(&out.stderr, "");
        assert!(&out
            .stdout
            .ends_with("[5,6,10,12,20,21,22,23,24,32,33,42,45,54]\n"));
        assert!(out.status.success());
    }
}
