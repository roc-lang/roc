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
        known_bad_file, run_cmd, run_roc, run_with_valgrind, strip_colors, Out, ValgrindError,
        ValgrindErrorXWhat,
    };
    use const_format::concatcp;
    use indoc::indoc;
    use roc_cli::{CMD_BUILD, CMD_CHECK, CMD_FORMAT, CMD_RUN};
    use roc_test_utils::assert_multiline_str_eq;
    use serial_test::serial;
    use std::iter;
    use std::path::Path;
    use strum::IntoEnumIterator;
    use strum_macros::EnumIter;

    const OPTIMIZE_FLAG: &str = concatcp!("--", roc_cli::FLAG_OPTIMIZE);
    const LINKER_FLAG: &str = concatcp!("--", roc_cli::FLAG_LINKER);
    const CHECK_FLAG: &str = concatcp!("--", roc_cli::FLAG_CHECK);
    const PREBUILT_PLATFORM: &str = concatcp!("--", roc_cli::FLAG_PREBUILT, "=true");
    #[allow(dead_code)]
    const TARGET_FLAG: &str = concatcp!("--", roc_cli::FLAG_TARGET);

    use std::sync::Once;
    static BENCHMARKS_BUILD_PLATFORM: Once = Once::new();

    #[derive(Debug, EnumIter)]
    enum CliMode {
        RocBuild,
        RocRun,
        Roc,
    }

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
        input_paths: &'a [&'a str],
        expected_ending: &'a str,
        use_valgrind: bool,
    }

    fn check_compile_error(file: &Path, flags: &[&str], expected: &str) {
        let compile_out = run_roc([CMD_CHECK, file.to_str().unwrap()].iter().chain(flags), &[]);
        let err = compile_out.stdout.trim();
        let err = strip_colors(err);

        // e.g. "1 error and 0 warnings found in 123 ms."
        let (before_first_digit, _) = err.split_at(err.rfind("found in ").unwrap());
        let err = format!("{}found in <ignored for test> ms.", before_first_digit);

        assert_multiline_str_eq!(err.as_str(), expected);
    }

    fn check_format_check_as_expected(file: &Path, expects_success_exit_code: bool) {
        let out = run_roc([CMD_FORMAT, file.to_str().unwrap(), CHECK_FLAG], &[]);

        assert_eq!(out.status.success(), expects_success_exit_code);
    }

    fn run_roc_on<'a, I: IntoIterator<Item = &'a str>>(
        file: &'a Path,
        args: I,
        stdin: &[&str],
        app_args: &[String],
    ) -> Out {
        let compile_out = run_roc(
            // converting these all to String avoids lifetime issues
            args.into_iter()
                .map(|arg| arg.to_string())
                .chain([file.to_str().unwrap().to_string(), "--".to_string()])
                .chain(app_args.iter().cloned()),
            stdin,
        );

        let ignorable = "🔨 Rebuilding platform...\n";
        let stderr = compile_out.stderr.replacen(ignorable, "", 1);
        let is_reporting_runtime = stderr.starts_with("runtime: ") && stderr.ends_with("ms\n");
        if !(stderr.is_empty() || is_reporting_runtime) {
            panic!("`roc` command had unexpected stderr: {}", stderr);
        }

        assert!(compile_out.status.success(), "bad status {:?}", compile_out);

        compile_out
    }

    fn check_output_with_stdin(
        file: &Path,
        stdin: &[&str],
        executable_filename: &str,
        flags: &[&str],
        app_args: &[String],
        expected_ending: &str,
        use_valgrind: bool,
    ) {
        // valgrind does not yet support avx512 instructions, see #1963.
        // we can't enable this only when testing with valgrind because of host re-use between tests
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        if is_x86_feature_detected!("avx512f") {
            std::env::set_var("NO_AVX512", "1");
        }

        for cli_mode in CliMode::iter() {
            let flags = {
                let mut vec = flags.to_vec();

                vec.push("--max-threads=1");

                vec.into_iter()
            };

            let out = match cli_mode {
                CliMode::RocBuild => {
                    run_roc_on(file, iter::once(CMD_BUILD).chain(flags.clone()), &[], &[]);

                    if use_valgrind && ALLOW_VALGRIND {
                        let mut valgrind_args = vec![file
                            .with_file_name(executable_filename)
                            .to_str()
                            .unwrap()
                            .to_string()];
                        valgrind_args.extend(app_args.iter().cloned());
                        let (valgrind_out, raw_xml) =
                            run_with_valgrind(stdin.iter().copied(), &valgrind_args);
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
                            stdin.iter().copied(),
                            app_args,
                        )
                    }
                }
                CliMode::Roc => run_roc_on(file, flags.clone(), stdin, app_args),
                CliMode::RocRun => run_roc_on(
                    file,
                    iter::once(CMD_RUN).chain(flags.clone()),
                    stdin,
                    app_args,
                ),
            };

            if !&out.stdout.ends_with(expected_ending) {
                panic!(
                    "expected output to end with {:?} but instead got {:#?} - stderr was: {:#?}",
                    expected_ending, out.stdout, out.stderr
                );
            }

            assert!(out.status.success());
        }
    }

    #[cfg(feature = "wasm32-cli-run")]
    fn check_wasm_output_with_stdin(
        file: &Path,
        stdin: &[&str],
        executable_filename: &str,
        flags: &[&str],
        input_paths: &[&str],
        expected_ending: &str,
    ) {
        assert!(input_paths.is_empty(), "Wasm does not support input files");
        let mut flags = flags.to_vec();
        flags.push(concatcp!(TARGET_FLAG, "=wasm32"));

        let compile_out = run_roc(
            [CMD_BUILD, file.to_str().unwrap()]
                .iter()
                .chain(flags.as_slice()),
        );
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

                    let mut app_args: Vec<String> = vec![];
                    for file in example.input_paths {
                        app_args.push(example_file(dir_name, file).to_str().unwrap().to_string());
                    }

                    match example.executable_filename {
                        "form" | "hello-gui" | "breakout" | "ruby" => {
                            // Since these require things the build system often doesn't have
                            // (e.g. GUIs open a window, Ruby needs ruby installed, WASM needs a browser)
                            // we do `roc build` on them but don't run them.
                            run_roc_on(&file_name, [CMD_BUILD, OPTIMIZE_FLAG], &[], &[]);
                            return;
                        }
                        "rocLovesSwift" => {
                            if cfg!(not(target_os = "macos")) {
                                eprintln!("WARNING: skipping testing example {} because it only works on MacOS.", example.filename);
                                return;
                            }
                        }
                        "rocLovesWebAssembly" => {
                            // this is a web assembly example, but we don't test with JS at the moment
                            eprintln!("WARNING: skipping testing example {} because it only works in a browser!", example.filename);
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
                        &app_args,
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
                        &[OPTIMIZE_FLAG],
                        &app_args,
                        example.expected_ending,
                        example.use_valgrind,
                    );

                    // Also check with the legacy linker.

                    if TEST_LEGACY_LINKER {
                        check_output_with_stdin(
                            &file_name,
                            example.stdin,
                            example.executable_filename,
                            &[LINKER_FLAG, "legacy"],
                            &app_args,
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

                check_for_tests("../../examples", &mut all_examples);
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
            filename: "main.roc",
            executable_filename: "helloWorld",
            stdin: &[],
            input_paths: &[],
            expected_ending:"Hello, World!\n",
            use_valgrind: true,
        },
        platformSwitching:"platform-switching" => Example {
            filename: "main.roc",
            executable_filename: "rocLovesPlatforms",
            stdin: &[],
            input_paths: &[],
            expected_ending:"Which platform am I running on now?\n",
            use_valgrind: true,
        },
        // We exclude the C platforming switching example
        // because the main platform switching example runs the c platform.
        // If we don't a race condition leads to test flakiness.
        // platformSwitchingC:"platform-switching" => Example {
        //     filename: "rocLovesC.roc",
        //     executable_filename: "rocLovesC",
        //     stdin: &[],
        //     input_paths: &[],
        //     expected_ending:"Roc <3 C!\n",
        //     use_valgrind: true,
        // },
        platformSwitchingRust:"platform-switching" => Example {
            filename: "rocLovesRust.roc",
            executable_filename: "rocLovesRust",
            stdin: &[],
            input_paths: &[],
            expected_ending:"Roc <3 Rust!\n",
            use_valgrind: true,
        },
        platformSwitchingSwift:"platform-switching" => Example {
            filename: "rocLovesSwift.roc",
            executable_filename: "rocLovesSwift",
            stdin: &[],
            input_paths: &[],
            expected_ending:"Roc <3 Swift!\n",
            use_valgrind: true,
        },
        platformSwitchingWebAssembly:"platform-switching" => Example {
            filename: "rocLovesWebAssembly.roc",
            executable_filename: "rocLovesWebAssembly",
            stdin: &[],
            input_paths: &[],
            expected_ending:"Roc <3 Web Assembly!\n",
            use_valgrind: true,
        },
        platformSwitchingZig:"platform-switching" => Example {
            filename: "rocLovesZig.roc",
            executable_filename: "rocLovesZig",
            stdin: &[],
            input_paths: &[],
            expected_ending:"Roc <3 Zig!\n",
            use_valgrind: true,
        },
        ruby:"ruby-interop" => Example {
            filename: "main.roc",
            executable_filename: "libhello",
            stdin: &[],
            input_paths: &[],
            expected_ending:"",
            use_valgrind: true,
        },
        fib:"algorithms" => Example {
            filename: "fibonacci.roc",
            executable_filename: "fibonacci",
            stdin: &[],
            input_paths: &[],
            expected_ending:"55\n",
            use_valgrind: true,
        },
        gui:"gui" => Example {
            filename: "Hello.roc",
            executable_filename: "hello-gui",
            stdin: &[],
            input_paths: &[],
            expected_ending: "",
            use_valgrind: false,
        },
        breakout:"breakout" => Example {
            filename: "breakout.roc",
            executable_filename: "breakout",
            stdin: &[],
            input_paths: &[],
            expected_ending: "",
            use_valgrind: false,
        },
        quicksort:"algorithms" => Example {
            filename: "quicksort.roc",
            executable_filename: "quicksort",
            stdin: &[],
            input_paths: &[],
            expected_ending: "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            use_valgrind: true,
        },
        // shared_quicksort:"shared-quicksort" => Example {
        //     filename: "Quicksort.roc",
        //     executable_filename: "quicksort",
        //     stdin: &[],
        //     input_paths: &[],
        //     expected_ending: "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
        //     use_valgrind: true,
        // },
        effects:"interactive" => Example {
            filename: "effects.roc",
            executable_filename: "effects",
            stdin: &["hi there!"],
            input_paths: &[],
            expected_ending: "hi there!\nIt is known\n",
            use_valgrind: true,
        },
        // tea:"tea" => Example {
        //     filename: "Main.roc",
        //     executable_filename: "tea-example",
        //     stdin: &[],
        //     input_paths: &[],
        //     expected_ending: "",
        //     use_valgrind: true,
        // },
        cli:"interactive" => Example {
            filename: "form.roc",
            executable_filename: "form",
            stdin: &["Giovanni\n", "Giorgio\n"],
            input_paths: &[],
            expected_ending: "Hi, Giovanni Giorgio! 👋\n",
            use_valgrind: false,
        },
        tui:"interactive" => Example {
            filename: "tui.roc",
            executable_filename: "tui",
            stdin: &["foo\n"], // NOTE: adding more lines leads to memory leaks
            input_paths: &[],
            expected_ending: "Hello Worldfoo!\n",
            use_valgrind: true,
        },
        // custom_malloc:"custom-malloc" => Example {
        //     filename: "Main.roc",
        //     executable_filename: "custom-malloc-example",
        //     stdin: &[],
        //     input_paths: &[],
        //     expected_ending: "ms!\nThe list was small!\n",
        //     use_valgrind: true,
        // },
        // task:"task" => Example {
        //     filename: "Main.roc",
        //     executable_filename: "task-example",
        //     stdin: &[],
        //     input_paths: &[],
        //     expected_ending: "successfully wrote to file\n",
        //     use_valgrind: true,
        // },
        false_interpreter:"false-interpreter" => {
            Example {
                filename: "False.roc",
                executable_filename: "false",
                stdin: &[],
                input_paths: &["examples/hello.false"],
                expected_ending:"Hello, World!\n",
                use_valgrind: false,
            }
        },
        static_site_gen: "static-site-gen" => {
            Example {
                filename: "static-site.roc",
                executable_filename: "static-site",
                stdin: &[],
                input_paths: &["input", "output"],
                expected_ending: "hello.txt -> hello.html\n",
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

                    let mut ran_without_optimizations = false;

                    let mut app_args: Vec<String> = vec![];
                    for file in benchmark.input_paths {
                        app_args.push(examples_dir("benchmarks").join(file).to_str().unwrap().to_string());
                    }

                    BENCHMARKS_BUILD_PLATFORM.call_once( || {
                        // Check with and without optimizations
                        check_output_with_stdin(
                            &file_name,
                            benchmark.stdin,
                            benchmark.executable_filename,
                            &[],
                            &app_args,
                            benchmark.expected_ending,
                            benchmark.use_valgrind,
                        );

                        ran_without_optimizations = true;
                    });

                    // now we can pass the `PREBUILT_PLATFORM` flag, because the
                    // `call_once` will have built the platform

                    if !ran_without_optimizations {
                        // Check with and without optimizations
                        check_output_with_stdin(
                            &file_name,
                            benchmark.stdin,
                            benchmark.executable_filename,
                            &[PREBUILT_PLATFORM],
                            &app_args,
                            benchmark.expected_ending,
                            benchmark.use_valgrind,
                        );
                    }

                    check_output_with_stdin(
                        &file_name,
                        benchmark.stdin,
                        benchmark.executable_filename,
                        &[PREBUILT_PLATFORM, OPTIMIZE_FLAG],
                        &app_args,
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
                        benchmark.input_paths.iter().map(|file| examples_dir("benchmarks").join(file)),
                        benchmark.expected_ending,
                    );

                    check_wasm_output_with_stdin(
                        &file_name,
                        benchmark.stdin,
                        benchmark.executable_filename,
                        &[OPTIMIZE_FLAG],
                        benchmark.input_paths.iter().map(|file| examples_dir("benchmarks").join(file)),
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
                        [concatcp!(TARGET_FLAG, "=x86_32")],
                        benchmark.input_paths.iter().map(|file| Some(examples_dir("benchmarks").join(file))),
                        benchmark.expected_ending,
                        benchmark.use_valgrind,
                    );

                    check_output_with_stdin(
                        &file_name,
                        benchmark.stdin,
                        benchmark.executable_filename,
                        [concatcp!(TARGET_FLAG, "=x86_32"), OPTIMIZE_FLAG],
                        benchmark.input_paths.iter().map(|file| Some(examples_dir("benchmarks").join(file))),
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

                check_for_benchmarks("../../examples/benchmarks", &mut all_benchmarks);
            }
        }
    }

    benchmarks! {
            nqueens => Example {
                filename: "NQueens.roc",
                executable_filename: "nqueens",
                stdin: &["6"],
                input_paths: &[],
                expected_ending: "4\n",
                use_valgrind: true,
            },
            cfold => Example {
                filename: "CFold.roc",
                executable_filename: "cfold",
                stdin: &["3"],
                input_paths: &[],
                expected_ending: "11 & 11\n",
                use_valgrind: true,
            },
            deriv => Example {
                filename: "Deriv.roc",
                executable_filename: "deriv",
                stdin: &["2"],
                input_paths: &[],
                expected_ending: "1 count: 6\n2 count: 22\n",
                use_valgrind: true,
            },
            rbtree_ck => Example {
                filename: "RBTreeCk.roc",
                executable_filename: "rbtree-ck",
                stdin: &["100"],
                input_paths: &[],
                expected_ending: "10\n",
                use_valgrind: true,
            },
            rbtree_insert => Example {
                filename: "RBTreeInsert.roc",
                executable_filename: "rbtree-insert",
                stdin: &[],
                input_paths: &[],
                expected_ending: "Node Black 0 {} Empty Empty\n",
                use_valgrind: true,
            },
    //        rbtree_del => Example {
    //            filename: "RBTreeDel.roc",
    //            executable_filename: "rbtree-del",
    //            stdin: &["420"],
    //            input_paths: &[],
    //            expected_ending: "30\n",
    //            use_valgrind: true,
    //        },
            astar => Example {
                filename: "TestAStar.roc",
                executable_filename: "test-astar",
                stdin: &[],
                input_paths: &[],
                expected_ending: "True\n",
                use_valgrind: false,
            },
            base64 => Example {
                filename: "TestBase64.roc",
                executable_filename: "test-base64",
                stdin: &[],
                input_paths: &[],
                expected_ending: "encoded: SGVsbG8gV29ybGQ=\ndecoded: Hello World\n",
                use_valgrind: true,
            },
            closure => Example {
                filename: "Closure.roc",
                executable_filename: "closure",
                stdin: &[],
                input_paths: &[],
                expected_ending: "",
                use_valgrind: false,
            },
            issue2279 => Example {
                filename: "Issue2279.roc",
                executable_filename: "issue2279",
                stdin: &[],
                input_paths: &[],
                expected_ending: "Hello, world!\n",
                use_valgrind: true,
            },
            quicksort_app => Example {
                filename: "QuicksortApp.roc",
                executable_filename: "quicksortapp",
                stdin: &[],
                input_paths: &[],
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
            &[OPTIMIZE_FLAG],
            &[],
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
            &[OPTIMIZE_FLAG],
            &[],
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
                ── UNRECOGNIZED NAME ─────────────────────────── tests/known_bad/TypeError.roc ─

                Nothing is named `d` in this scope.

                10│      _ <- await (line d)
                                          ^

                Did you mean one of these?

                    U8
                    Ok
                    I8
                    F64

                ────────────────────────────────────────────────────────────────────────────────

                1 error and 0 warnings found in <ignored for test> ms."#
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
                ── MISSING DEFINITION ────────────────── tests/known_bad/ExposedNotDefined.roc ─

                bar is listed as exposed, but it isn't defined in this module.

                You can fix this by adding a definition for bar, or by removing it
                from exposes.

                ────────────────────────────────────────────────────────────────────────────────

                1 error and 0 warnings found in <ignored for test> ms."#
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
                ── UNUSED IMPORT ──────────────────────────── tests/known_bad/UnusedImport.roc ─

                Nothing from Symbol is used in this module.

                3│      imports [Symbol.{ Ident }]
                                 ^^^^^^^^^^^^^^^^

                Since Symbol isn't used, you don't need to import it.

                ────────────────────────────────────────────────────────────────────────────────

                0 errors and 1 warning found in <ignored for test> ms."#
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
                ── UNKNOWN GENERATES FUNCTION ─────── tests/known_bad/UnknownGeneratesWith.roc ─

                I don't know how to generate the foobar function.

                4│      generates Effect with [after, map, always, foobar]
                                                                   ^^^^^^

                Only specific functions like `after` and `map` can be generated.Learn
                more about hosted modules at TODO.

                ────────────────────────────────────────────────────────────────────────────────

                1 error and 0 warnings found in <ignored for test> ms."#
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
