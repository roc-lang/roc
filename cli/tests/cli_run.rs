// #[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;
extern crate inlinable_string;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;

#[macro_use]
extern crate maplit;

#[cfg(test)]
mod cli_run {
    use cli_utils::helpers::{
        example_file, extract_valgrind_errors, run_cmd, run_roc, run_with_valgrind, ValgrindError,
        ValgrindErrorXWhat,
    };
    use std::fs;
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

    #[test]
    fn check_all_examples() {
        let mut all_examples = hashmap! {
            "hello-world" => vec![
                Example {
                    filename: "Hello.roc",
                    executable_filename: "hello-world",
                    stdin: &[],
                    expected_ending:"Hello, World!\n",
                    use_valgrind: true,
                }
            ],
            "hello-zig" => vec![
                Example {
                    filename: "Hello.roc",
                    executable_filename: "hello-world",
                    stdin: &[],
                    expected_ending:"Hello, World!\n",
                    use_valgrind: true,
                }
            ],
            "quicksort" => vec![
                Example {
                    filename: "Quicksort.roc",
                    executable_filename: "quicksort",
                    stdin: &[],
                    expected_ending: "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
                    use_valgrind: true,
                }
            ],
            "shared-quicksort" => vec![
                Example {
                    filename: "Quicksort.roc",
                    executable_filename: "quicksort",
                    stdin: &[],
                    expected_ending: "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
                    use_valgrind: true,
                }
            ],
            "effect" => vec![
                Example {
                    filename: "Main.roc",
                    executable_filename: "effect-example",
                    stdin: &["hi there!"],
                    expected_ending: "hi there!\n",
                    use_valgrind: true,
                }
            ],
            "multi-dep-str" => vec![
                Example {
                    filename: "Main.roc",
                    executable_filename: "multi-dep-str",
                    stdin: &[],
                    expected_ending: "I am Dep2.str2\n",
                    use_valgrind: true,
                }
            ],
            "multi-dep-thunk" => vec![
                Example {
                    filename: "Main.roc",
                    executable_filename: "multi-dep-thunk",
                    stdin: &[],
                    expected_ending: "I am Dep2.value2\n",
                    use_valgrind: true,
                }
            ],
            "tea" => vec![
                Example {
                    filename: "Main.roc",
                    executable_filename: "tea-example",
                    stdin: &[],
                    expected_ending: "",
                    use_valgrind: true,
                }
            ],
            // "cli" => vec![
            //     Example {
            //         filename: "Echo.roc",
            //         executable_filename: "echo",
            //         stdin: &["Giovanni\n", "Giorgio\n"],
            //         expected_ending: "Giovanni Giorgio!\n",
            //         use_valgrind: true,
            //     }
            // ],
            // "custom-malloc" => vec![
            //     Example {
            //         filename: "Main.roc",
            //         executable_filename: "custom-malloc-example",
            //         stdin: &[],
            //         expected_ending: "ms!\nThe list was small!\n",
            //         use_valgrind: true,
            //     }
            // ],
            // "task" => vec![
            //     Example {
            //         filename: "Main.roc",
            //         executable_filename: "task-example",
            //         stdin: &[],
            //         expected_ending: "successfully wrote to file\n",
            //         use_valgrind: true,
            //     }
            // ],
            "benchmarks" => vec![
                Example {
                    filename: "NQueens.roc",
                    executable_filename: "nqueens",
                    stdin: &["6"],
                    expected_ending: "4\n",
                    use_valgrind: true,
                },
                Example {
                    filename: "CFold.roc",
                    executable_filename: "cfold",
                    stdin: &["3"],
                    expected_ending: "11 & 11\n",
                    use_valgrind: true,
                },
                Example {
                    filename: "Deriv.roc",
                    executable_filename: "deriv",
                    stdin: &["2"],
                    expected_ending: "1 count: 6\n2 count: 22\n",
                    use_valgrind: true,
                },
                Example {
                    filename: "RBTreeInsert.roc",
                    executable_filename: "rbtree-insert",
                    stdin: &[],
                    expected_ending: "Node Black 0 {} Empty Empty\n",
                    use_valgrind: true,
                },
                Example {
                    filename: "RBTreeDel.roc",
                    executable_filename: "rbtree-del",
                    stdin: &["420"],
                    expected_ending: "30\n",
                    use_valgrind: true,
                },
                Example {
                    filename: "TestAStar.roc",
                    executable_filename: "test-astar",
                    stdin: &[],
                    expected_ending: "True\n",
                    use_valgrind: false,
                },
                Example {
                    filename: "TestBase64.roc",
                    executable_filename: "test-base64",
                    stdin: &[],
                    expected_ending: "encoded: SGVsbG8gV29ybGQ=\ndecoded: Hello World\n",
                    use_valgrind: true,
                },
                Example {
                    filename: "Closure.roc",
                    executable_filename: "closure",
                    stdin: &[],
                    expected_ending: "",
                    use_valgrind: true,
                }
            ],
        };

        for entry in fs::read_dir("../examples").unwrap() {
            let entry = entry.unwrap();

            if entry.file_type().unwrap().is_dir() {
                let dir_name = entry.file_name().into_string().unwrap();
                let examples = all_examples.remove(dir_name.as_str()).unwrap_or(Vec::new());

                if examples.is_empty() {
                    panic!("The directory examples/{} does not have any corresponding tests in cli_run. Please add one, so if it ever stops compiling, we'll know about it right away!", dir_name);
                }

                for example in examples {
                    println!(
                        "Running {:?}...",
                        &example_file(dir_name.as_str(), example.filename),
                    );
                    // Check with and without optimizations
                    check_output_with_stdin(
                        &example_file(dir_name.as_str(), example.filename),
                        example.stdin,
                        example.executable_filename,
                        &[],
                        example.expected_ending,
                        example.use_valgrind,
                    );

                    check_output_with_stdin(
                        &example_file(dir_name.as_str(), example.filename),
                        example.stdin,
                        example.executable_filename,
                        &["--optimize"],
                        example.expected_ending,
                        example.use_valgrind,
                    );
                }
            }
        }

        assert_eq!(all_examples, std::collections::HashMap::default());
    }
}
