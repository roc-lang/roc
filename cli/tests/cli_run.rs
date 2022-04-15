#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;
extern crate indoc;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;

#[cfg(test)]
mod cli_run {
    use cli_utils::helpers::{
        example_file, examples_dir, extract_valgrind_errors, fixture_file, fixtures_dir,
        known_bad_file, run_cmd, run_roc, run_with_valgrind, Out, ValgrindError,
        ValgrindErrorXWhat,
    };
    use indoc::indoc;
    use roc_test_utils::assert_multiline_str_eq;
    use serial_test::serial;
    use std::path::{Path, PathBuf};

    #[cfg(not(debug_assertions))]
    use roc_collections::all::MutMap;

    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    const TEST_LEGACY_LINKER: bool = true;

    // Surgical linker currently only supports linux x86_64,
    // so we're always testing the legacy linker on other targets.
    #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
    const TEST_LEGACY_LINKER: bool = false;

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
        input_file: Option<&'a str>,
        expected_ending: &'a str,
        use_valgrind: bool,
    }

    fn strip_colors(str: &str) -> String {
        use roc_reporting::report::ANSI_STYLE_CODES;
        str.replace(ANSI_STYLE_CODES.red, "")
            .replace(ANSI_STYLE_CODES.green, "")
            .replace(ANSI_STYLE_CODES.yellow, "")
            .replace(ANSI_STYLE_CODES.blue, "")
            .replace(ANSI_STYLE_CODES.magenta, "")
            .replace(ANSI_STYLE_CODES.cyan, "")
            .replace(ANSI_STYLE_CODES.white, "")
            .replace(ANSI_STYLE_CODES.bold, "")
            .replace(ANSI_STYLE_CODES.underline, "")
            .replace(ANSI_STYLE_CODES.reset, "")
    }

    fn check_compile_error(file: &Path, flags: &[&str], expected: &str) {
        let compile_out = run_roc(&[&["check", file.to_str().unwrap()], flags].concat());
        let err = compile_out.stdout.trim();
        let err = strip_colors(err);
        assert_multiline_str_eq!(err, expected.into());
    }

    fn check_format_check_as_expected(file: &Path, expects_success_exit_code: bool) {
        let flags = &["--check"];
        let out = run_roc(&[&["format", file.to_str().unwrap()], &flags[..]].concat());
        if expects_success_exit_code {
            assert!(out.status.success());
        } else {
            assert!(!out.status.success());
        }
    }

    fn build_example(file: &Path, flags: &[&str]) -> Out {
        let compile_out = run_roc(&[&["build", file.to_str().unwrap()], flags].concat());
        if !compile_out.stderr.is_empty() {
            panic!("roc build had stderr: {}", compile_out.stderr);
        }

        assert!(compile_out.status.success(), "bad status {:?}", compile_out);

        compile_out
    }

    fn check_output_with_stdin(
        file: &Path,
        stdin: &[&str],
        executable_filename: &str,
        flags: &[&str],
        input_file: Option<PathBuf>,
        expected_ending: &str,
        use_valgrind: bool,
    ) {
        let mut all_flags = vec![];
        all_flags.extend_from_slice(flags);

        if use_valgrind {
            all_flags.extend_from_slice(&["--valgrind"]);
        }

        build_example(file, &all_flags[..]);

        let out = if use_valgrind && ALLOW_VALGRIND {
            let (valgrind_out, raw_xml) = if let Some(input_file) = input_file {
                run_with_valgrind(
                    stdin,
                    &[
                        file.with_file_name(executable_filename).to_str().unwrap(),
                        input_file.to_str().unwrap(),
                    ],
                )
            } else {
                run_with_valgrind(
                    stdin,
                    &[file.with_file_name(executable_filename).to_str().unwrap()],
                )
            };

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
        } else if let Some(input_file) = input_file {
            run_cmd(
                file.with_file_name(executable_filename).to_str().unwrap(),
                stdin,
                &[input_file.to_str().unwrap()],
            )
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
                expected_ending, out.stdout
            );
        }
        assert!(out.status.success());
    }

    #[cfg(feature = "wasm32-cli-run")]
    fn check_wasm_output_with_stdin(
        file: &Path,
        stdin: &[&str],
        executable_filename: &str,
        flags: &[&str],
        input_file: Option<PathBuf>,
        expected_ending: &str,
    ) {
        assert_eq!(input_file, None, "Wasm does not support input files");
        let mut flags = flags.to_vec();
        flags.push("--target=wasm32");

        let compile_out = run_roc(&[&["build", file.to_str().unwrap()], flags.as_slice()].concat());
        if !compile_out.stderr.is_empty() {
            panic!("{}", compile_out.stderr);
        }

        assert!(compile_out.status.success(), "bad status {:?}", compile_out);

        let path = file.with_file_name(executable_filename);
        let stdout = crate::run_with_wasmer(&path, stdin);

        if !stdout.ends_with(expected_ending) {
            panic!(
                "expected output to end with {:?} but instead got {:#?}",
                expected_ending, stdout
            );
        }
    }
    /// This macro does two things.
    ///
    /// First, it generates and runs a separate test for each of the given
    /// Example expressions. Each of these should test a particular .roc file
    /// in the examples/ directory.
    ///
    /// Second, it generates an extra test which (non-recursively) traverses the
    /// examples/ directory and verifies that each of the .roc files in there
    /// has had a corresponding test generated in the previous step. This test
    /// will fail if we ever add a new .roc file to examples/ and forget to
    /// add a test for it here!
    macro_rules! examples {
        ($($test_name:ident:$name:expr => $example:expr,)+) => {
            $(
                #[test]
                #[allow(non_snake_case)]
                fn $test_name() {
                    let dir_name = $name;
                    let example = $example;
                    let file_name = example_file(dir_name, example.filename);

                    match example.executable_filename {
                        "helloWeb" => {
                            // this is a web webassembly example, but we don't test with JS at the moment
                            eprintln!("WARNING: skipping testing example {} because the test is broken right now!", example.filename);
                            return;
                        }
                        "form" => {
                            // test is skipped until we upgrate to zig 0.9 / llvm 13
                            eprintln!("WARNING: skipping testing example {} because the test is broken right now!", example.filename);
                            return;
                        }
                        "helloSwift" => {
                            if cfg!(not(target_os = "macos")) {
                                eprintln!("WARNING: skipping testing example {} because it only works on MacOS.", example.filename);
                                return;
                            }
                        }
                        "hello-gui" => {
                            // Since this one requires opening a window, we do `roc build` on it but don't run it.
                            build_example(&file_name, &["--optimize"]);

                            return;
                        }
                        _ => {}
                    }

                    // Check with and without optimizations
                    check_output_with_stdin(
                        &file_name,
                        example.stdin,
                        example.executable_filename,
                        &[],
                        example.input_file.and_then(|file| Some(example_file(dir_name, file))),
                        example.expected_ending,
                        example.use_valgrind,
                    );

                    // This is mostly because the false interpreter is still very slow -
                    // 25s for the cli tests is just not acceptable during development!
                    #[cfg(not(debug_assertions))]
                    check_output_with_stdin(
                        &file_name,
                        example.stdin,
                        example.executable_filename,
                        &["--optimize"],
                        example.input_file.and_then(|file| Some(example_file(dir_name, file))),
                        example.expected_ending,
                        example.use_valgrind,
                    );

                    // Also check with the legacy linker.

                    if TEST_LEGACY_LINKER {
                        check_output_with_stdin(
                            &file_name,
                            example.stdin,
                            example.executable_filename,
                            &["--linker", "legacy"],
                            example.input_file.and_then(|file| Some(example_file(dir_name, file))),
                            example.expected_ending,
                            example.use_valgrind,
                        );
                    }
                }
            )*

            #[test]
            #[cfg(not(debug_assertions))]
            fn all_examples_have_tests() {
                let mut all_examples: MutMap<&str, Example<'_>> = MutMap::default();

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
        helloWorld:"hello-world" => Example {
            filename: "helloWorld.roc",
            executable_filename: "helloWorld",
            stdin: &[],
            input_file: None,
            expected_ending:"Hello, World!\n",
            use_valgrind: true,
        },
        helloC:"hello-world/c-platform" => Example {
            filename: "helloC.roc",
            executable_filename: "helloC",
            stdin: &[],
            input_file: None,
            expected_ending:"Hello, World!\n",
            use_valgrind: true,
        },
        helloZig:"hello-world/zig-platform" => Example {
            filename: "helloZig.roc",
            executable_filename: "helloZig",
            stdin: &[],
            input_file: None,
            expected_ending:"Hello, World!\n",
            use_valgrind: true,
        },
        helloRust:"hello-world/rust-platform" => Example {
            filename: "helloRust.roc",
            executable_filename: "helloRust",
            stdin: &[],
            input_file: None,
            expected_ending:"Hello, World!\n",
            use_valgrind: true,
        },
        helloSwift:"hello-world/swift-platform" => Example {
            filename: "helloSwift.roc",
            executable_filename: "helloSwift",
            stdin: &[],
            input_file: None,
            expected_ending:"Hello, World!\n",
            use_valgrind: true,
        },
        helloWeb:"hello-world/web-platform" => Example {
            filename: "helloWeb.roc",
            executable_filename: "helloWeb",
            stdin: &[],
            input_file: None,
            expected_ending:"Hello, World!\n",
            use_valgrind: true,
        },
        fib:"algorithms" => Example {
            filename: "fibonacci.roc",
            executable_filename: "fibonacci",
            stdin: &[],
            input_file: None,
            expected_ending:"55\n",
            use_valgrind: true,
        },
        gui:"gui" => Example {
            filename: "Hello.roc",
            executable_filename: "hello-gui",
            stdin: &[],
            input_file: None,
            expected_ending: "",
            use_valgrind: false,
        },
        quicksort:"algorithms" => Example {
            filename: "quicksort.roc",
            executable_filename: "quicksort",
            stdin: &[],
            input_file: None,
            expected_ending: "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            use_valgrind: true,
        },
        // shared_quicksort:"shared-quicksort" => Example {
        //     filename: "Quicksort.roc",
        //     executable_filename: "quicksort",
        //     stdin: &[],
        //     input_file: None,
        //     expected_ending: "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
        //     use_valgrind: true,
        // },
        effects:"interactive" => Example {
            filename: "effects.roc",
            executable_filename: "effects",
            stdin: &["hi there!"],
            input_file: None,
            expected_ending: "hi there!\nIt is known\n",
            use_valgrind: true,
        },
        // tea:"tea" => Example {
        //     filename: "Main.roc",
        //     executable_filename: "tea-example",
        //     stdin: &[],
        //     input_file: None,
        //     expected_ending: "",
        //     use_valgrind: true,
        // },
        cli:"interactive" => Example {
            filename: "form.roc",
            executable_filename: "form",
            stdin: &["Giovanni\n", "Giorgio\n"],
            input_file: None,
            expected_ending: "Hi, Giovanni Giorgio! 👋\n",
            use_valgrind: false,
        },
        tui:"interactive" => Example {
            filename: "tui.roc",
            executable_filename: "tui",
            stdin: &["foo\n"], // NOTE: adding more lines leads to memory leaks
            input_file: None,
            expected_ending: "Hello Worldfoo!\n",
            use_valgrind: true,
        },
        // custom_malloc:"custom-malloc" => Example {
        //     filename: "Main.roc",
        //     executable_filename: "custom-malloc-example",
        //     stdin: &[],
        //     input_file: None,
        //     expected_ending: "ms!\nThe list was small!\n",
        //     use_valgrind: true,
        // },
        // task:"task" => Example {
        //     filename: "Main.roc",
        //     executable_filename: "task-example",
        //     stdin: &[],
        //     input_file: None,
        //     expected_ending: "successfully wrote to file\n",
        //     use_valgrind: true,
        // },
        false_interpreter:"false-interpreter" => {
            Example {
                filename: "False.roc",
                executable_filename: "false",
                stdin: &[],
                input_file: Some("examples/hello.false"),
                expected_ending:"Hello, World!\n",
                use_valgrind: false,
            }
        },
    }

    macro_rules! benchmarks {
        ($($test_name:ident => $benchmark:expr,)+) => {

            $(
                #[test]
                #[cfg_attr(not(debug_assertions), serial(benchmark))]
                #[cfg(all(not(feature = "wasm32-cli-run"), not(feature = "i386-cli-run")))]
                fn $test_name() {
                    let benchmark = $benchmark;
                    let file_name = examples_dir("benchmarks").join(benchmark.filename);

                    // TODO fix QuicksortApp and then remove this!
                    match benchmark.filename {
                        "QuicksortApp.roc" => {
                            eprintln!("WARNING: skipping testing benchmark {} because the test is broken right now!", benchmark.filename);
                            return;
                        }
                        _ => {}
                    }

                    // Check with and without optimizations
                    check_output_with_stdin(
                        &file_name,
                        benchmark.stdin,
                        benchmark.executable_filename,
                        &[],
                        benchmark.input_file.and_then(|file| Some(examples_dir("benchmarks").join(file))),
                        benchmark.expected_ending,
                        benchmark.use_valgrind,
                    );

                    check_output_with_stdin(
                        &file_name,
                        benchmark.stdin,
                        benchmark.executable_filename,
                        &["--optimize"],
                        benchmark.input_file.and_then(|file| Some(examples_dir("benchmarks").join(file))),
                        benchmark.expected_ending,
                        benchmark.use_valgrind,
                    );
                }

            )*

            #[cfg(feature = "wasm32-cli-run")]
            mod wasm32 {
                use super::*;
            $(
                #[test]
                #[cfg_attr(not(debug_assertions), serial(benchmark))]
                fn $test_name() {
                    let benchmark = $benchmark;
                    let file_name = examples_dir("benchmarks").join(benchmark.filename);

                    // TODO fix QuicksortApp and then remove this!
                    match benchmark.filename {
                        "QuicksortApp.roc"  => {
                            eprintln!("WARNING: skipping testing benchmark {} because the test is broken right now!", benchmark.filename);
                            return;
                        }
                        _ => {}
                    }

                    // Check with and without optimizations
                    check_wasm_output_with_stdin(
                        &file_name,
                        benchmark.stdin,
                        benchmark.executable_filename,
                        &[],
                        benchmark.input_file.and_then(|file| Some(examples_dir("benchmarks").join(file))),
                        benchmark.expected_ending,
                    );

                    check_wasm_output_with_stdin(
                        &file_name,
                        benchmark.stdin,
                        benchmark.executable_filename,
                        &["--optimize"],
                        benchmark.input_file.and_then(|file| Some(examples_dir("benchmarks").join(file))),
                        benchmark.expected_ending,
                    );
                }
            )*
            }

            #[cfg(feature = "i386-cli-run")]
            mod i386 {
                use super::*;
            $(
                #[test]
                #[cfg_attr(not(debug_assertions), serial(benchmark))]
                fn $test_name() {
                    let benchmark = $benchmark;
                    let file_name = examples_dir("benchmarks").join(benchmark.filename);

                    // TODO fix QuicksortApp and then remove this!
                    match benchmark.filename {
                        "QuicksortApp.roc" => {
                            eprintln!("WARNING: skipping testing benchmark {} because the test is broken right now!", benchmark.filename);
                            return;
                        }
                        _ => {}
                    }

                    // Check with and without optimizations
                    check_output_with_stdin(
                        &file_name,
                        benchmark.stdin,
                        benchmark.executable_filename,
                        &["--target=x86_32"],
                        benchmark.input_file.and_then(|file| Some(examples_dir("benchmarks").join(file))),
                        benchmark.expected_ending,
                        benchmark.use_valgrind,
                    );

                    check_output_with_stdin(
                        &file_name,
                        benchmark.stdin,
                        benchmark.executable_filename,
                        &["--target=x86_32", "--optimize"],
                        benchmark.input_file.and_then(|file| Some(examples_dir("benchmarks").join(file))),
                        benchmark.expected_ending,
                        benchmark.use_valgrind,
                    );
                }
            )*
            }

            #[test]
            #[cfg(not(debug_assertions))]
            fn all_benchmarks_have_tests() {
                let mut all_benchmarks: MutMap<&str, Example<'_>> = MutMap::default();

                $(
                    let benchmark = $benchmark;

                    all_benchmarks.insert(benchmark.filename, benchmark);
                )*

                check_for_benchmarks("../examples/benchmarks", &mut all_benchmarks);
            }
        }
    }

    benchmarks! {
            nqueens => Example {
                filename: "NQueens.roc",
                executable_filename: "nqueens",
                stdin: &["6"],
                input_file: None,
                expected_ending: "4\n",
                use_valgrind: true,
            },
            cfold => Example {
                filename: "CFold.roc",
                executable_filename: "cfold",
                stdin: &["3"],
                input_file: None,
                expected_ending: "11 & 11\n",
                use_valgrind: true,
            },
            deriv => Example {
                filename: "Deriv.roc",
                executable_filename: "deriv",
                stdin: &["2"],
                input_file: None,
                expected_ending: "1 count: 6\n2 count: 22\n",
                use_valgrind: true,
            },
            rbtree_ck => Example {
                filename: "RBTreeCk.roc",
                executable_filename: "rbtree-ck",
                stdin: &["100"],
                input_file: None,
                expected_ending: "10\n",
                use_valgrind: true,
            },
            rbtree_insert => Example {
                filename: "RBTreeInsert.roc",
                executable_filename: "rbtree-insert",
                stdin: &[],
                input_file: None,
                expected_ending: "Node Black 0 {} Empty Empty\n",
                use_valgrind: true,
            },
    //        rbtree_del => Example {
    //            filename: "RBTreeDel.roc",
    //            executable_filename: "rbtree-del",
    //            stdin: &["420"],
    //            input_file: None,
    //            expected_ending: "30\n",
    //            use_valgrind: true,
    //        },
            astar => Example {
                filename: "TestAStar.roc",
                executable_filename: "test-astar",
                stdin: &[],
                input_file: None,
                expected_ending: "True\n",
                use_valgrind: false,
            },
            base64 => Example {
                filename: "TestBase64.roc",
                executable_filename: "test-base64",
                stdin: &[],
                input_file: None,
                expected_ending: "encoded: SGVsbG8gV29ybGQ=\ndecoded: Hello World\n",
                use_valgrind: true,
            },
            closure => Example {
                filename: "Closure.roc",
                executable_filename: "closure",
                stdin: &[],
                input_file: None,
                expected_ending: "",
                use_valgrind: true,
            },
            issue2279 => Example {
                filename: "Issue2279.roc",
                executable_filename: "issue2279",
                stdin: &[],
                input_file: None,
                expected_ending: "Hello, world!\n",
                use_valgrind: true,
            },
            quicksort_app => Example {
                filename: "QuicksortApp.roc",
                executable_filename: "quicksortapp",
                stdin: &[],
                input_file: None,
                expected_ending: "todo put the correct quicksort answer here",
                use_valgrind: true,
            },
        }

    #[cfg(not(debug_assertions))]
    fn check_for_tests(examples_dir: &str, all_examples: &mut MutMap<&str, Example<'_>>) {
        let entries = std::fs::read_dir(examples_dir).unwrap_or_else(|err| {
            panic!(
                "Error trying to read {} as an examples directory: {}",
                examples_dir, err
            );
        });

        for entry in entries {
            let entry = entry.unwrap();

            if entry.file_type().unwrap().is_dir() {
                let example_dir_name = entry.file_name().into_string().unwrap();

                // TODO: Improve this with a more-dynamic approach. (Read all subdirectories?)
                // Some hello-world examples live in nested directories
                if example_dir_name == "hello-world" {
                    for sub_dir in [
                        "c-platform",
                        "rust-platform",
                        "swift-platform",
                        "web-platform",
                        "zig-platform",
                    ] {
                        all_examples.remove(format!("{}/{}", example_dir_name, sub_dir).as_str()).unwrap_or_else(|| {
                            panic!("The example directory {}/{}/{} does not have any corresponding tests in cli_run. Please add one, so if it ever stops working, we'll know about it right away!", examples_dir, example_dir_name, sub_dir);
                        });
                    }
                }

                // We test benchmarks separately
                if example_dir_name != "benchmarks" {
                    all_examples.remove(example_dir_name.as_str()).unwrap_or_else(|| {
                    panic!("The example directory {}/{} does not have any corresponding tests in cli_run. Please add one, so if it ever stops working, we'll know about it right away!", examples_dir, example_dir_name);
                });
                }
            }
        }

        assert_eq!(all_examples, &mut MutMap::default());
    }

    #[cfg(not(debug_assertions))]
    fn check_for_benchmarks(benchmarks_dir: &str, all_benchmarks: &mut MutMap<&str, Example<'_>>) {
        use std::ffi::OsStr;
        use std::fs::File;
        use std::io::Read;

        let entries = std::fs::read_dir(benchmarks_dir).unwrap_or_else(|err| {
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
                if "app".as_bytes() == buf && !benchmark_file_name.contains("RBTreeDel") {
                    all_benchmarks.remove(benchmark_file_name.as_str()).unwrap_or_else(|| {
                        panic!("The benchmark {}/{} does not have any corresponding tests in cli_run. Please add one, so if it ever stops working, we'll know about it right away!", benchmarks_dir, benchmark_file_name);
                    });
                }
            }
        }

        assert_eq!(all_benchmarks, &mut MutMap::default());
    }

    #[test]
    #[serial(multi_dep_str)]
    fn run_multi_dep_str_unoptimized() {
        check_output_with_stdin(
            &fixture_file("multi-dep-str", "Main.roc"),
            &[],
            "multi-dep-str",
            &[],
            None,
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
            None,
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
            None,
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
            None,
            "I am Dep2.value2\n",
            true,
        );
    }

    #[test]
    fn known_type_error() {
        check_compile_error(
            &known_bad_file("TypeError.roc"),
            &[],
            indoc!(
                r#"
                ── UNRECOGNIZED NAME ───────────────────────────────────────────────────────────

                I cannot find a `d` value

                10│      _ <- await (line d)
                                          ^

                Did you mean one of these?

                    U8
                    Ok
                    I8
                    F64

                ────────────────────────────────────────────────────────────────────────────────"#
            ),
        );
    }

    #[test]
    fn exposed_not_defined() {
        check_compile_error(
            &known_bad_file("ExposedNotDefined.roc"),
            &[],
            indoc!(
                r#"
                ── MISSING DEFINITION ──────────────────────────────────────────────────────────

                bar is listed as exposed, but it isn't defined in this module.

                You can fix this by adding a definition for bar, or by removing it
                from exposes.

                ────────────────────────────────────────────────────────────────────────────────"#
            ),
        );
    }

    #[test]
    fn unused_import() {
        check_compile_error(
            &known_bad_file("UnusedImport.roc"),
            &[],
            indoc!(
                r#"
                ── UNUSED IMPORT ───────────────────────────────────────────────────────────────

                Nothing from Symbol is used in this module.

                3│      imports [ Symbol.{ Ident } ]
                                  ^^^^^^^^^^^^^^^^

                Since Symbol isn't used, you don't need to import it.

                ────────────────────────────────────────────────────────────────────────────────"#
            ),
        );
    }

    #[test]
    fn unknown_generates_with() {
        check_compile_error(
            &known_bad_file("UnknownGeneratesWith.roc"),
            &[],
            indoc!(
                r#"
                ── UNKNOWN GENERATES FUNCTION ──────────────────────────────────────────────────

                I don't know how to generate the foobar function.

                4│      generates Effect with [ after, map, always, foobar ]
                                                                    ^^^^^^

                Only specific functions like `after` and `map` can be generated.Learn
                more about hosted modules at TODO.

                ────────────────────────────────────────────────────────────────────────────────"#
            ),
        );
    }

    #[test]
    fn format_check_good() {
        check_format_check_as_expected(&fixture_file("format", "Formatted.roc"), true);
    }

    #[test]
    fn format_check_reformatting_needed() {
        check_format_check_as_expected(&fixture_file("format", "NotFormatted.roc"), false);
    }

    #[test]
    fn format_check_folders() {
        // This fails, because "NotFormatted.roc" is present in this folder
        check_format_check_as_expected(&fixtures_dir("format"), false);

        // This doesn't fail, since only "Formatted.roc" and non-roc files are present in this folder
        check_format_check_as_expected(&fixtures_dir("format/formatted_directory"), true);
    }
}

#[allow(dead_code)]
fn run_with_wasmer(wasm_path: &std::path::Path, stdin: &[&str]) -> String {
    use std::io::Write;
    use wasmer::{Instance, Module, Store};

    //    std::process::Command::new("cp")
    //        .args(&[
    //            wasm_path.to_str().unwrap(),
    //            "/home/folkertdev/roc/wasm/nqueens.wasm",
    //        ])
    //        .output()
    //        .unwrap();

    let store = Store::default();
    let module = Module::from_file(&store, &wasm_path).unwrap();

    let mut fake_stdin = wasmer_wasi::Pipe::new();
    let fake_stdout = wasmer_wasi::Pipe::new();
    let fake_stderr = wasmer_wasi::Pipe::new();

    for line in stdin {
        write!(fake_stdin, "{}", line).unwrap();
    }

    // First, we create the `WasiEnv`
    use wasmer_wasi::WasiState;
    let mut wasi_env = WasiState::new("hello")
        .stdin(Box::new(fake_stdin))
        .stdout(Box::new(fake_stdout))
        .stderr(Box::new(fake_stderr))
        .finalize()
        .unwrap();

    // Then, we get the import object related to our WASI
    // and attach it to the Wasm instance.
    let import_object = wasi_env
        .import_object(&module)
        .unwrap_or_else(|_| wasmer::imports!());

    let instance = Instance::new(&module, &import_object).unwrap();

    let start = instance.exports.get_function("_start").unwrap();

    match start.call(&[]) {
        Ok(_) => read_wasi_stdout(wasi_env),
        Err(e) => {
            use wasmer_wasi::WasiError;
            match e.downcast::<WasiError>() {
                Ok(WasiError::Exit(0)) => {
                    // we run the `_start` function, so exit(0) is expected
                    read_wasi_stdout(wasi_env)
                }
                other => format!("Something went wrong running a wasm test: {:?}", other),
            }
        }
    }
}

#[allow(dead_code)]
fn read_wasi_stdout(wasi_env: wasmer_wasi::WasiEnv) -> String {
    let mut state = wasi_env.state.lock().unwrap();

    match state.fs.stdout_mut() {
        Ok(Some(stdout)) => {
            let mut buf = String::new();
            stdout.read_to_string(&mut buf).unwrap();

            buf
        }
        _ => todo!(),
    }
}
