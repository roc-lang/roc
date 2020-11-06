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
    use crate::helpers::{
        example_file, extract_valgrind_errors, fixture_file, run_cmd, run_roc, run_with_valgrind,
    };
    use serial_test::serial;
    use std::path::Path;

    fn check_output(file: &Path, flags: &[&str], expected_ending: &str, use_valgrind: bool) {
        let compile_out = run_roc(&[&["build", file.to_str().unwrap()], flags].concat());
        if !compile_out.stderr.is_empty() {
            panic!(compile_out.stderr);
        }
        assert!(compile_out.status.success());

        let out = if use_valgrind {
            let (valgrind_out, raw_xml) =
                run_with_valgrind(&[file.with_file_name("app").to_str().unwrap()]);
            let memory_errors = extract_valgrind_errors(&raw_xml);
            if !memory_errors.is_empty() {
                panic!("{:?}", memory_errors);
            }
            valgrind_out
        } else {
            run_cmd(file.with_file_name("app").to_str().unwrap(), &[])
        };
        if !&out.stdout.ends_with(expected_ending) {
            panic!(
                "expected output to end with {:?} but instead got {:#?}",
                expected_ending, out
            );
        }
        assert!(out.status.success());
    }

    #[test]
    #[serial(hello_world)]
    fn run_hello_world() {
        check_output(
            &example_file("hello-world", "Hello.roc"),
            &[],
            "Hello, World!!!!!!!!!!!!!\n",
            true,
        );
    }

    #[test]
    #[serial(hello_world)]
    fn run_hello_world_optimized() {
        check_output(
            &example_file("hello-world", "Hello.roc"),
            &[],
            "Hello, World!!!!!!!!!!!!!\n",
            true,
        );
    }

    #[test]
    #[serial(quicksort)]
    fn run_quicksort_not_optimized() {
        check_output(
            &example_file("quicksort", "Quicksort.roc"),
            &[],
            "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            false,
        );
    }

    #[test]
    #[serial(quicksort)]
    fn run_quicksort_optimized() {
        check_output(
            &example_file("quicksort", "Quicksort.roc"),
            &["--optimize"],
            "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            false,
        );
    }

    #[test]
    #[serial(quicksort)]
    // TODO: Stop ignoring this test once we are correctly freeing the RocList even when in dev build.
    #[ignore]
    fn run_quicksort_valgrind() {
        check_output(
            &example_file("quicksort", "Quicksort.roc"),
            &[],
            "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            true,
        );
    }

    #[test]
    #[serial(quicksort)]
    // TODO: Stop ignoring this test once valgrind supports AVX512.
    #[ignore]
    fn run_quicksort_optimized_valgrind() {
        check_output(
            &example_file("quicksort", "Quicksort.roc"),
            &["--optimize"],
            "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            true,
        );
    }

    #[test]
    #[serial(multi_module)]
    fn run_multi_module() {
        check_output(
            &example_file("multi-module", "Quicksort.roc"),
            &[],
            "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            false,
        );
    }

    #[test]
    #[serial(multi_module)]
    fn run_multi_module_optimized() {
        check_output(
            &example_file("multi-module", "Quicksort.roc"),
            &["--optimize"],
            "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            false,
        );
    }

    #[test]
    #[serial(multi_module)]
    // TODO: Stop ignoring this test once we are correctly freeing the RocList even when in dev build.
    #[ignore]
    fn run_multi_module_valgrind() {
        check_output(
            &example_file("multi-module", "Quicksort.roc"),
            &[],
            "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            true,
        );
    }

    #[test]
    #[serial(multi_module)]
    // TODO: Stop ignoring this test once valgrind supports AVX512.
    #[ignore]
    fn run_multi_module_optimized_valgrind() {
        check_output(
            &example_file("multi-module", "Quicksort.roc"),
            &["--optimize"],
            "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            true,
        );
    }

    #[test]
    #[serial(effect)]
    fn run_effect_unoptimized() {
        check_output(
            &example_file("effect", "Main.roc"),
            &[],
            "I am Dep2.str2\n",
            true,
        );
    }

    #[test]
    #[serial(multi_dep_str)]
    fn run_multi_dep_str_unoptimized() {
        check_output(
            &fixture_file("multi-dep-str", "Main.roc"),
            &[],
            "I am Dep2.str2\n",
            true,
        );
    }

    #[test]
    #[serial(multi_dep_str)]
    fn run_multi_dep_str_optimized() {
        check_output(
            &fixture_file("multi-dep-str", "Main.roc"),
            &[],
            "I am Dep2.str2\n",
            true,
        );
    }

    #[test]
    #[serial(multi_dep_thunk)]
    fn run_multi_dep_thunk_unoptimized() {
        check_output(
            &fixture_file("multi-dep-thunk", "Main.roc"),
            &[],
            "I am Dep2.value2\n",
            true,
        );
    }

    #[test]
    #[serial(multi_dep_str)]
    fn run_multi_dep_thunk_optimized() {
        check_output(
            &fixture_file("multi-dep-thunk", "Main.roc"),
            &[],
            "I am Dep2.value2\n",
            true,
        );
    }
}
