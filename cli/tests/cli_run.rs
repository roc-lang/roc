#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;
extern crate inlinable_string;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;

#[cfg(test)]
mod cli_run {
    use cli_utils::helpers::{
        example_file, extract_valgrind_errors, fixture_file, root_dir, run_cmd, run_roc,
        run_with_valgrind, ValgrindError, ValgrindErrorXWhat,
    };
    use serial_test::serial;
    use std::collections::HashMap;
    use std::fs::{self, File};
    use std::io::Read;
    use std::path::Path;

    #[cfg(not(target_os = "macos"))]
    const ALLOW_VALGRIND: bool = true;

    // Disallow valgrind on macOS by default, because it reports a ton
    // of false positives. For local development on macOS, feel free to
    // change this to true!
    #[cfg(target_os = "macos")]
    const ALLOW_VALGRIND: bool = false;

    #[derive(Debug, PartialEq, Eq)]
    struct Example<'a> {
        filename: &'a str,
        executable_filename: &'a str,
        stdin: &'a [&'a str],
        expected_ending: &'a str,
        use_valgrind: bool,
    }

    fn check_output_with_stdin(
        file: &Path,
        stdin: &[&str],
        executable_filename: &str,
        flags: &[&str],
        expected_ending: &str,
        use_valgrind: bool,
    ) {
        let compile_out = run_roc(&[&["build", file.to_str().unwrap()], flags].concat());
        if !compile_out.stderr.is_empty() {
            panic!("{}", compile_out.stderr);
        }

        assert!(compile_out.status.success(), "bad status {:?}", compile_out);

        let out = if use_valgrind && ALLOW_VALGRIND {
            let (valgrind_out, raw_xml) = run_with_valgrind(
                stdin,
                &[file.with_file_name(executable_filename).to_str().unwrap()],
            );

            if valgrind_out.status.success() {
                let memory_errors = extract_valgrind_errors(&raw_xml).unwrap_or_else(|err| {
                    panic!("failed to parse the `valgrind` xml output. Error was:\n\n{:?}\n\nvalgrind xml was: \"{}\"\n\nvalgrind stdout was: \"{}\"\n\nvalgrind stderr was: \"{}\"", err, raw_xml, valgrind_out.stdout, valgrind_out.stderr);
                });

                if !memory_errors.is_empty() {
                    for error in memory_errors {
                        let ValgrindError {
                            kind,
                            what: _,
                            xwhat,
                        } = error;
                        println!("Valgrind Error: {}\n", kind);

                        if let Some(ValgrindErrorXWhat {
                            text,
                            leakedbytes: _,
                            leakedblocks: _,
                        }) = xwhat
                        {
                            println!("    {}", text);
                        }
                    }
                    panic!("Valgrind reported memory errors");
                }
            } else {
                let exit_code = match valgrind_out.status.code() {
                    Some(code) => format!("exit code {}", code),
                    None => "no exit code".to_string(),
                };

                panic!("`valgrind` exited with {}. valgrind stdout was: \"{}\"\n\nvalgrind stderr was: \"{}\"", exit_code, valgrind_out.stdout, valgrind_out.stderr);
            }

            valgrind_out
        } else {
            run_cmd(
                file.with_file_name(executable_filename).to_str().unwrap(),
                stdin,
                &[],
            )
        };
        if !&out.stdout.ends_with(expected_ending) {
            panic!(
                "expected output to end with {:?} but instead got {:#?}",
                expected_ending, out
            );
        }
        assert!(out.status.success());
    }

    /// This macro does two things.
    ///
    /// First, it generates and runs a separate test for each of the given
    /// Example expressions. Each of these should test a particular .roc file
    /// in the examples/ directory.
    ///
    /// Second, it generates an extra test which recursively traverses the
    /// examples/ directory and verifies that each of the .roc files in there
    /// has had a corresponding test generated in the previous step. This test
    /// will fail if we ever add a new .roc file to examples/ and forget to
    /// add a test for it here!
    macro_rules! examples {
        ($($test_name:ident:$name:expr => $example:expr,)+) => {
            $(
                #[test]
                fn $test_name() {
                    let dir_name = $name;
                    let example = $example;
                    let file_name = example_file(dir_name, example.filename);

                    // Check with and without optimizations
                    check_output_with_stdin(
                        &file_name,
                        example.stdin,
                        example.executable_filename,
                        &[],
                        example.expected_ending,
                        example.use_valgrind,
                    );

                    check_output_with_stdin(
                        &file_name,
                        example.stdin,
                        example.executable_filename,
                        &["--optimize"],
                        example.expected_ending,
                        example.use_valgrind,
                    );
                }
            )*

            #[test]
            fn all_examples_have_tests() {
                let mut all_examples: HashMap<&str, Example<'_>> = HashMap::default();

                $(
                    all_examples.insert($name, $example);
                )*

                check_for_tests("../examples", &mut all_examples);
            }
        }
    }

    // examples! macro format:
    //
    // "name-of-subdirectory-inside-examples-dir" => [
    //     test_name_1: Example {
    //         ...
    //     },
    //     test_name_2: Example {
    //         ...
    //     },
    // ]
    examples! {
        hello_world:"hello-world" => Example {
            filename: "Hello.roc",
            executable_filename: "hello-world",
            stdin: &[],
            expected_ending:"Hello, World!\n",
            use_valgrind: true,
        },
        hello_zig:"hello-zig" => Example {
            filename: "Hello.roc",
            executable_filename: "hello-world",
            stdin: &[],
            expected_ending:"Hello, World!\n",
            use_valgrind: true,
        },
        hello_rust:"hello-rust" => Example {
            filename: "Hello.roc",
            executable_filename: "hello-world",
            stdin: &[],
            expected_ending:"Hello, World!\n",
            use_valgrind: true,
        },
        quicksort:"quicksort" => Example {
            filename: "Quicksort.roc",
            executable_filename: "quicksort",
            stdin: &[],
            expected_ending: "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            use_valgrind: true,
        },
        // shared_quicksort:"shared-quicksort" => Example {
        //     filename: "Quicksort.roc",
        //     executable_filename: "quicksort",
        //     stdin: &[],
        //     expected_ending: "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
        //     use_valgrind: true,
        // },
        effect:"effect" => Example {
            filename: "Main.roc",
            executable_filename: "effect-example",
            stdin: &["hi there!"],
            expected_ending: "hi there!\n",
            use_valgrind: true,
        },
        // tea:"tea" => Example {
        //     filename: "Main.roc",
        //     executable_filename: "tea-example",
        //     stdin: &[],
        //     expected_ending: "",
        //     use_valgrind: true,
        // },
        // cli:"cli" => Example {
        //     filename: "Echo.roc",
        //     executable_filename: "echo",
        //     stdin: &["Giovanni\n", "Giorgio\n"],
        //     expected_ending: "Giovanni Giorgio!\n",
        //     use_valgrind: true,
        // },
        // custom_malloc:"custom-malloc" => Example {
        //     filename: "Main.roc",
        //     executable_filename: "custom-malloc-example",
        //     stdin: &[],
        //     expected_ending: "ms!\nThe list was small!\n",
        //     use_valgrind: true,
        // },
        // task:"task" => Example {
        //     filename: "Main.roc",
        //     executable_filename: "task-example",
        //     stdin: &[],
        //     expected_ending: "successfully wrote to file\n",
        //     use_valgrind: true,
        // },
    }

    macro_rules! benchmarks {
        ($($test_name:ident => $benchmark:expr,)+) => {
            $(
                #[test]
                #[serial(benchmark)]
                fn $test_name() {
                    let benchmark = $benchmark;
                    let file_name = root_dir().join("benchmarks").join(benchmark.filename);

                    // Check with and without optimizations
                    check_output_with_stdin(
                        &file_name,
                        benchmark.stdin,
                        benchmark.executable_filename,
                        &[],
                        benchmark.expected_ending,
                        benchmark.use_valgrind,
                    );

                    check_output_with_stdin(
                        &file_name,
                        benchmark.stdin,
                        benchmark.executable_filename,
                        &["--optimize"],
                        benchmark.expected_ending,
                        benchmark.use_valgrind,
                    );
                }
            )*

            #[test]
            fn all_benchmarks_have_tests() {
                let mut all_benchmarks: HashMap<&str, Example<'_>> = HashMap::default();

                $(
                    let benchmark = $benchmark;

                    all_benchmarks.insert(benchmark.filename, benchmark);
                )*

                check_for_benchmarks("../benchmarks", &mut all_benchmarks);
            }
        }
    }

    benchmarks! {
        nqueens => Example {
            filename: "NQueens.roc",
            executable_filename: "nqueens",
            stdin: &["6"],
            expected_ending: "4\n",
            use_valgrind: true,
        },
        cfold => Example {
            filename: "CFold.roc",
            executable_filename: "cfold",
            stdin: &["3"],
            expected_ending: "11 & 11\n",
            use_valgrind: true,
        },
        deriv => Example {
            filename: "Deriv.roc",
            executable_filename: "deriv",
            stdin: &["2"],
            expected_ending: "1 count: 6\n2 count: 22\n",
            use_valgrind: true,
        },
        // rbtree_ck => Example {
        //     filename: "RBTreeCk.roc",
        //     executable_filename: "rbtree-ck",
        //     stdin: &[],
        //     expected_ending: "Node Black 0 {} Empty Empty\n",
        //     use_valgrind: true,
        // },
        rbtree_insert => Example {
            filename: "RBTreeInsert.roc",
            executable_filename: "rbtree-insert",
            stdin: &[],
            expected_ending: "Node Black 0 {} Empty Empty\n",
            use_valgrind: true,
        },
        rbtree_del => Example {
            filename: "RBTreeDel.roc",
            executable_filename: "rbtree-del",
            stdin: &["420"],
            expected_ending: "30\n",
            use_valgrind: true,
        },
        astar => Example {
            filename: "TestAStar.roc",
            executable_filename: "test-astar",
            stdin: &[],
            expected_ending: "True\n",
            use_valgrind: false,
        },
        base64 => Example {
            filename: "TestBase64.roc",
            executable_filename: "test-base64",
            stdin: &[],
            expected_ending: "encoded: SGVsbG8gV29ybGQ=\ndecoded: Hello World\n",
            use_valgrind: true,
        },
        closure => Example {
            filename: "Closure.roc",
            executable_filename: "closure",
            stdin: &[],
            expected_ending: "",
            use_valgrind: true,
        },
        // quicksort_app => Example {
        //     filename: "QuicksortApp.roc",
        //     executable_filename: "quicksortapp",
        //     stdin: &[],
        //     expected_ending: "todo put the correct quicksort answer here",
        //     use_valgrind: true,
        // },
    }

    fn check_for_tests(examples_dir: &str, all_examples: &mut HashMap<&str, Example<'_>>) {
        let entries = fs::read_dir(examples_dir).unwrap_or_else(|err| {
            panic!(
                "Error trying to read {} as an examples directory: {}",
                examples_dir, err
            );
        });

        for entry in entries {
            let entry = entry.unwrap();

            if entry.file_type().unwrap().is_dir() {
                let example_dir_name = entry.file_name().into_string().unwrap();

                all_examples.remove(example_dir_name.as_str()).unwrap_or_else(|| {
                    panic!("The example directory {}/{} does not have any corresponding tests in cli_run. Please add one, so if it ever stops working, we'll know about it right away!", examples_dir, example_dir_name);
                });
            }
        }

        assert_eq!(all_examples, &mut HashMap::default());
    }

    fn check_for_benchmarks(benchmarks_dir: &str, all_benchmarks: &mut HashMap<&str, Example<'_>>) {
        use std::ffi::OsStr;

        let entries = fs::read_dir(benchmarks_dir).unwrap_or_else(|err| {
            panic!(
                "Error trying to read {} as a benchmark directory: {}",
                benchmarks_dir, err
            );
        });

        for entry in entries {
            let entry = entry.unwrap();
            let path = entry.path();

            if let Some("roc") = path.extension().and_then(OsStr::to_str) {
                let benchmark_file_name = entry.file_name().into_string().unwrap();

                // Verify that this is an app module by reading the first 3
                // bytes of the file.
                let buf: &mut [u8] = &mut [0, 0, 0];
                let mut file = File::open(path).unwrap();

                file.read_exact(buf).unwrap();

                // Only app modules in this directory are considered benchmarks.
                if "app".as_bytes() == buf {
                    all_benchmarks.remove(benchmark_file_name.as_str()).unwrap_or_else(|| {
                    panic!("The benchmark {}/{} does not have any corresponding tests in cli_run. Please add one, so if it ever stops working, we'll know about it right away!", benchmarks_dir, benchmark_file_name);
                });
                }
            }
        }

        assert_eq!(all_benchmarks, &mut HashMap::default());
    }

    #[test]
    #[serial(multi_dep_str)]
    fn run_multi_dep_str_unoptimized() {
        check_output_with_stdin(
            &fixture_file("multi-dep-str", "Main.roc"),
            &[],
            "multi-dep-str",
            &[],
            "I am Dep2.str2\n",
            true,
        );
    }

    #[test]
    #[serial(multi_dep_str)]
    fn run_multi_dep_str_optimized() {
        check_output_with_stdin(
            &fixture_file("multi-dep-str", "Main.roc"),
            &[],
            "multi-dep-str",
            &["--optimize"],
            "I am Dep2.str2\n",
            true,
        );
    }

    #[test]
    #[serial(multi_dep_thunk)]
    fn run_multi_dep_thunk_unoptimized() {
        check_output_with_stdin(
            &fixture_file("multi-dep-thunk", "Main.roc"),
            &[],
            "multi-dep-thunk",
            &[],
            "I am Dep2.value2\n",
            true,
        );
    }

    #[test]
    #[serial(multi_dep_thunk)]
    fn run_multi_dep_thunk_optimized() {
        check_output_with_stdin(
            &fixture_file("multi-dep-thunk", "Main.roc"),
            &[],
            "multi-dep-thunk",
            &["--optimize"],
            "I am Dep2.value2\n",
            true,
        );
    }
}
