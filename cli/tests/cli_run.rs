// #[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;
extern crate inlinable_string;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;

mod helpers;

#[cfg(test)]
mod cli_run {
    use crate::helpers::{example_file, run_roc, run_with_valgrind};

    #[test]
    fn run_hello_world() {
        let out = run_roc(&[
            "build",
            example_file("hello-world", "Hello.roc").to_str().unwrap(),
        ]);
        if !out.stderr.is_empty() {
            panic!(out.stderr);
        }
        assert!(out.status.success());

        let valgrind_out =
            run_with_valgrind(&[example_file("hello-world", "app").to_str().unwrap()]);
        assert!(&valgrind_out.stdout.ends_with("Hello, World!!!!!!!!!!!!!\n"));
        assert!(valgrind_out.status.success());
        // TODO: Actually process the valgrind output for leaks.
    }

    #[test]
    fn run_quicksort() {
        let out = run_roc(&[
            "build",
            example_file("quicksort", "Quicksort.roc").to_str().unwrap(),
        ]);
        if !out.stderr.is_empty() {
            panic!(out.stderr);
        }
        assert!(out.status.success());

        let valgrind_out = run_with_valgrind(&[example_file("quicksort", "app").to_str().unwrap()]);
        assert!(&valgrind_out
            .stdout
            .ends_with("[0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0]\n"));
        assert!(valgrind_out.status.success());
        // TODO: Actually process the valgrind output for leaks.
    }
}
