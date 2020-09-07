#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;
extern crate inlinable_string;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;

mod helpers;

#[cfg(test)]
mod cli_run {
    use crate::helpers::{example_file, run_roc};

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
            .ends_with("[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"));
        assert!(out.status.success());
    }
}
