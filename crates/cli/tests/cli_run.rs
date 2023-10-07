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
        extract_valgrind_errors, file_path_from_root, fixture_file, fixtures_dir, has_error,
        known_bad_file, run_cmd, run_roc, run_with_valgrind, strip_colors, Out, ValgrindError,
        ValgrindErrorXWhat,
    };
    use const_format::concatcp;
    use indoc::indoc;
    use roc_cli::{CMD_BUILD, CMD_CHECK, CMD_DEV, CMD_FORMAT, CMD_RUN, CMD_TEST};
    use roc_test_utils::assert_multiline_str_eq;
    use serial_test::serial;
    use std::iter;
    use std::path::Path;

    #[cfg(all(unix, not(target_os = "macos")))]
    const ALLOW_VALGRIND: bool = true;

    // Disallow valgrind on macOS by default, because it reports a ton
    // of false positives. For local development on macOS, feel free to
    // change this to true!
    #[cfg(target_os = "macos")]
    const ALLOW_VALGRIND: bool = false;

    #[cfg(windows)]
    const ALLOW_VALGRIND: bool = false;

    // use valgrind (if supported on the current platform)
    #[derive(Debug, Clone, Copy)]
    enum UseValgrind {
        Yes,
        No,
    }

    #[derive(Debug, Clone, Copy)]
    enum TestCliCommands {
        Many,
        Run,
        Test,
        Dev,
    }

    const OPTIMIZE_FLAG: &str = concatcp!("--", roc_cli::FLAG_OPTIMIZE);
    const LINKER_FLAG: &str = concatcp!("--", roc_cli::FLAG_LINKER);
    const CHECK_FLAG: &str = concatcp!("--", roc_cli::FLAG_CHECK);
    const PREBUILT_PLATFORM: &str = concatcp!("--", roc_cli::FLAG_PREBUILT);
    #[allow(dead_code)]
    const TARGET_FLAG: &str = concatcp!("--", roc_cli::FLAG_TARGET);

    #[derive(Debug)]
    enum CliMode {
        Roc,      // buildAndRunIfNoErrors
        RocBuild, // buildOnly
        RocRun,   // buildAndRun
        RocTest,
        RocDev,
    }

    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    const TEST_LEGACY_LINKER: bool = true;

    // Surgical linker currently only supports linux x86_64,
    // so we're always testing the legacy linker on other targets.
    #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
    const TEST_LEGACY_LINKER: bool = false;

    #[derive(Debug, PartialEq, Eq)]
    enum Arg<'a> {
        ExamplePath(&'a str),
        PlainText(&'a str),
    }

    fn check_compile_error(file: &Path, flags: &[&str], expected: &str) {
        let compile_out = run_roc(
            [CMD_CHECK, file.to_str().unwrap()].iter().chain(flags),
            &[],
            &[],
        );
        let err = compile_out.stdout.trim();
        let err = strip_colors(err);

        // e.g. "1 error and 0 warnings found in 123 ms."
        let (before_first_digit, _) = err.split_at(err.rfind("found in ").unwrap());
        let err = format!("{before_first_digit}found in <ignored for test> ms.");

        // make paths consistent
        let err = err.replace('\\', "/");

        // consistency with typewriters, very important
        let err = err.replace('\r', "");

        assert_multiline_str_eq!(err.as_str(), expected);
    }

    fn check_format_check_as_expected(file: &Path, expects_success_exit_code: bool) {
        let out = run_roc([CMD_FORMAT, file.to_str().unwrap(), CHECK_FLAG], &[], &[]);

        assert_eq!(out.status.success(), expects_success_exit_code);
    }

    fn run_roc_on_failure_is_panic<'a, I: IntoIterator<Item = &'a str>>(
        file: &'a Path,
        args: I,
        stdin: &[&str],
        roc_app_args: &[String],
        env: &[(&str, &str)],
    ) -> Out {
        let compile_out = run_roc_on(file, args, stdin, roc_app_args, env);

        assert!(
            compile_out.status.success(),
            "\n___________\nRoc command failed with status {:?}:\n\n  {} {}\n___________\n",
            compile_out.status,
            compile_out.stdout,
            compile_out.stderr,
        );

        compile_out
    }

    fn run_roc_on<'a, I: IntoIterator<Item = &'a str>>(
        file: &'a Path,
        args: I,
        stdin: &[&str],
        roc_app_args: &[String],
        env: &[(&str, &str)],
    ) -> Out {
        let compile_out = run_roc(
            // converting these all to String avoids lifetime issues
            args.into_iter()
                .map(|arg| arg.to_string())
                .chain([file.to_str().unwrap().to_string(), "--".to_string()])
                .chain(roc_app_args.iter().cloned()),
            stdin,
            env,
        );

        if has_error(&compile_out.stderr) {
            panic!("\n___________\nThe roc command:\n\n  {:?}\n\nhad unexpected stderr:\n\n  {}\n___________\n", compile_out.cmd_str, compile_out.stderr);
        }

        compile_out
    }

    #[allow(clippy::too_many_arguments)]
    fn check_output_with_stdin(
        file: &Path,
        stdin: &[&str],
        executable_filename: &str,
        flags: &[&str],
        roc_app_args: &[String],
        extra_env: &[(&str, &str)],
        expected_ending: &str,
        use_valgrind: UseValgrind,
        test_cli_commands: TestCliCommands,
    ) {
        // valgrind does not yet support avx512 instructions, see #1963.
        // we can't enable this only when testing with valgrind because of host re-use between tests
        #[cfg(any(target_arch = "x86", target_arch = "x86_64"))]
        if is_x86_feature_detected!("avx512f") {
            std::env::set_var("NO_AVX512", "1");
        }

        // TODO: expects don't currently work on windows
        let cli_commands = if cfg!(windows) {
            match test_cli_commands {
                TestCliCommands::Many => vec![CliMode::RocBuild, CliMode::RocRun],
                TestCliCommands::Run => vec![CliMode::RocRun],
                TestCliCommands::Test => vec![],
                TestCliCommands::Dev => vec![],
            }
        } else {
            match test_cli_commands {
                TestCliCommands::Many => vec![CliMode::RocBuild, CliMode::RocRun, CliMode::Roc],
                TestCliCommands::Run => vec![CliMode::Roc],
                TestCliCommands::Test => vec![CliMode::RocTest],
                TestCliCommands::Dev => vec![CliMode::RocDev],
            }
        };

        for cli_mode in cli_commands.iter() {
            let flags = {
                let mut vec = flags.to_vec();

                // max-threads segfaults on windows right now
                if !cfg!(windows) {
                    vec.push("--max-threads=1");
                }

                vec.into_iter()
            };

            let out = match cli_mode {
                CliMode::RocBuild => {
                    run_roc_on_failure_is_panic(
                        file,
                        iter::once(CMD_BUILD).chain(flags.clone()),
                        &[],
                        &[],
                        &[],
                    );

                    if matches!(use_valgrind, UseValgrind::Yes) && ALLOW_VALGRIND {
                        let mut valgrind_args = vec![file
                            .with_file_name(executable_filename)
                            .to_str()
                            .unwrap()
                            .to_string()];
                        valgrind_args.extend(roc_app_args.iter().cloned());
                        let (valgrind_out, raw_xml) =
                            run_with_valgrind(stdin.iter().copied(), &valgrind_args);
                        if valgrind_out.status.success() {
                            let memory_errors = extract_valgrind_errors(&raw_xml).unwrap_or_else(|err| {
                                panic!("failed to parse the `valgrind` xml output:\n\n  Error was:\n\n    {:?}\n\n  valgrind xml was:\n\n    \"{}\"\n\n  valgrind stdout was:\n\n    \"{}\"\n\n  valgrind stderr was:\n\n    \"{}\"", err, raw_xml, valgrind_out.stdout, valgrind_out.stderr);
                            });

                            if !memory_errors.is_empty() {
                                for error in memory_errors {
                                    let ValgrindError {
                                        kind,
                                        what: _,
                                        xwhat,
                                    } = error;
                                    println!("Valgrind Error: {kind}\n");

                                    if let Some(ValgrindErrorXWhat {
                                        text,
                                        leakedbytes: _,
                                        leakedblocks: _,
                                    }) = xwhat
                                    {
                                        println!("    {text}");
                                    }
                                }
                                panic!("Valgrind reported memory errors");
                            }
                        } else {
                            let exit_code = match valgrind_out.status.code() {
                                Some(code) => format!("exit code {code}"),
                                None => "no exit code".to_string(),
                            };

                            panic!("`valgrind` exited with {}. valgrind stdout was: \"{}\"\n\nvalgrind stderr was: \"{}\"", exit_code, valgrind_out.stdout, valgrind_out.stderr);
                        }

                        valgrind_out
                    } else {
                        run_cmd(
                            file.with_file_name(executable_filename).to_str().unwrap(),
                            stdin.iter().copied(),
                            roc_app_args,
                            extra_env.iter().copied(),
                        )
                    }
                }
                CliMode::Roc => {
                    run_roc_on_failure_is_panic(file, flags.clone(), stdin, roc_app_args, extra_env)
                }
                CliMode::RocRun => run_roc_on_failure_is_panic(
                    file,
                    iter::once(CMD_RUN).chain(flags.clone()),
                    stdin,
                    roc_app_args,
                    extra_env,
                ),
                CliMode::RocTest => {
                    // here failure is what we expect

                    run_roc_on(
                        file,
                        iter::once(CMD_TEST).chain(flags.clone()),
                        stdin,
                        roc_app_args,
                        extra_env,
                    )
                }
                CliMode::RocDev => {
                    // here failure is what we expect

                    run_roc_on(
                        file,
                        iter::once(CMD_DEV).chain(flags.clone()),
                        stdin,
                        roc_app_args,
                        extra_env,
                    )
                }
            };

            let mut actual = strip_colors(&out.stdout);

            // e.g. "1 failed and 0 passed in 123 ms."
            if let Some(split) = actual.rfind("passed in ") {
                let (before_first_digit, _) = actual.split_at(split);
                actual = format!("{before_first_digit}passed in <ignored for test> ms.");
            }

            let self_path = file.display().to_string();
            actual = actual.replace(&self_path, "<ignored for tests>");

            if !actual.ends_with(expected_ending) {
                panic!(
                    "> expected output to end with:\n{}\n> but instead got:\n{}\n> stderr was:\n{}",
                    expected_ending, actual, out.stderr
                );
            }

            if !out.status.success() && !matches!(cli_mode, CliMode::RocTest) {
                // We don't need stdout, Cargo prints it for us.
                panic!(
                    "Example program exited with status {:?}\nstderr was:\n{:#?}",
                    out.status, out.stderr
                );
            }
        }
    }

    // when you want to run `roc test` to execute `expect`s, perhaps on a library rather than an application.
    fn test_roc_expect(dir_name: &str, roc_filename: &str) {
        let path = file_path_from_root(dir_name, roc_filename);
        let out = run_roc([CMD_TEST, path.to_str().unwrap()], &[], &[]);
        assert!(out.status.success());
    }

    // when you don't need args, stdin or extra_env
    fn test_roc_app_slim(
        dir_name: &str,
        roc_filename: &str,
        executable_filename: &str,
        expected_ending: &str,
        use_valgrind: UseValgrind,
    ) {
        test_roc_app(
            dir_name,
            roc_filename,
            executable_filename,
            &[],
            &[],
            &[],
            expected_ending,
            use_valgrind,
            TestCliCommands::Run,
        )
    }

    #[allow(clippy::too_many_arguments)]
    fn test_roc_app(
        dir_name: &str,
        roc_filename: &str,
        executable_filename: &str,
        stdin: &[&str],
        args: &[Arg],
        extra_env: &[(&str, &str)],
        expected_ending: &str,
        use_valgrind: UseValgrind,
        test_cli_commands: TestCliCommands,
    ) {
        let file_name = file_path_from_root(dir_name, roc_filename);
        let mut roc_app_args: Vec<String> = Vec::new();

        for arg in args {
            match arg {
                Arg::ExamplePath(file) => {
                    roc_app_args.push(
                        file_path_from_root(dir_name, file)
                            .to_str()
                            .unwrap()
                            .to_string(),
                    );
                }
                Arg::PlainText(arg) => {
                    roc_app_args.push(arg.to_string());
                }
            }
        }

        // workaround for surgical linker issue, see PR #3990
        let mut custom_flags: Vec<&str> = Vec::new();

        match executable_filename {
            "form" | "hello-gui" | "breakout" | "libhello" | "inspect-gui" => {
                // Since these require things the build system often doesn't have
                // (e.g. GUIs open a window, Ruby needs ruby installed, WASM needs a browser)
                // we do `roc build` on them but don't run them.
                run_roc_on(&file_name, [CMD_BUILD, OPTIMIZE_FLAG], &[], &[], &[]);
                return;
            }
            "swiftui" | "rocLovesSwift" => {
                if cfg!(not(target_os = "macos")) {
                    eprintln!(
                        "WARNING: skipping testing example {roc_filename} because it only works on MacOS."
                    );
                    return;
                } else {
                    run_roc_on(&file_name, [CMD_BUILD, OPTIMIZE_FLAG], &[], &[], &[]);
                    return;
                }
            }
            "rocLovesWebAssembly" => {
                // this is a web assembly example, but we don't test with JS at the moment
                eprintln!(
                    "WARNING: skipping testing example {roc_filename} because it only works in a browser!"
                );
                return;
            }
            "args" => {
                custom_flags = vec![LINKER_FLAG, "legacy"];
            }
            _ => {}
        }

        // Check with and without optimizations
        check_output_with_stdin(
            &file_name,
            stdin,
            executable_filename,
            &custom_flags,
            &roc_app_args,
            extra_env,
            expected_ending,
            use_valgrind,
            test_cli_commands,
        );

        custom_flags.push(OPTIMIZE_FLAG);
        // This is mostly because the false interpreter is still very slow -
        // 25s for the cli tests is just not acceptable during development!
        #[cfg(not(debug_assertions))]
        check_output_with_stdin(
            &file_name,
            stdin,
            executable_filename,
            &custom_flags,
            &roc_app_args,
            extra_env,
            expected_ending,
            use_valgrind,
            test_cli_commands,
        );

        // Also check with the legacy linker.

        if TEST_LEGACY_LINKER {
            check_output_with_stdin(
                &file_name,
                stdin,
                executable_filename,
                &[LINKER_FLAG, "legacy"],
                &roc_app_args,
                extra_env,
                expected_ending,
                use_valgrind,
                test_cli_commands,
            );
        }
    }

    #[test]
    #[serial(zig_platform_parser_package_basic_cli_url)]
    #[cfg_attr(windows, ignore)]
    fn hello_world() {
        test_roc_app_slim(
            "examples",
            "helloWorld.roc",
            "helloWorld",
            "Hello, World!\n",
            UseValgrind::Yes,
        )
    }

    #[test]
    #[serial(cli_platform)]
    #[cfg_attr(windows, ignore)]
    fn hello_world_no_url() {
        test_roc_app_slim(
            "examples",
            "helloWorldNoURL.roc",
            "helloWorld",
            "Hello, World!\n",
            UseValgrind::Yes,
        )
    }

    #[cfg(windows)]
    const LINE_ENDING: &str = "\r\n";
    #[cfg(not(windows))]
    const LINE_ENDING: &str = "\n";

    #[test]
    #[cfg_attr(windows, ignore)]
    // uses C platform
    fn platform_switching_main() {
        test_roc_app_slim(
            "examples/platform-switching",
            "main.roc",
            "rocLovesPlatforms",
            &("Which platform am I running on now?".to_string() + LINE_ENDING),
            UseValgrind::Yes,
        )
    }

    // We exclude the C platforming switching example
    // because the main platform switching example runs the c platform.
    // If we don't, a race condition leads to test flakiness.

    #[test]
    #[cfg_attr(windows, ignore)]
    fn platform_switching_rust() {
        test_roc_app_slim(
            "examples/platform-switching",
            "rocLovesRust.roc",
            "rocLovesRust",
            "Roc <3 Rust!\n",
            UseValgrind::Yes,
        )
    }

    // zig_platform_parser_package_basic_cli_url use to be split up but then things could get stuck
    #[test]
    #[serial(zig_platform_parser_package_basic_cli_url)]
    #[cfg_attr(windows, ignore)]
    fn platform_switching_zig() {
        test_roc_app_slim(
            "examples/platform-switching",
            "rocLovesZig.roc",
            "rocLovesZig",
            "Roc <3 Zig!\n",
            UseValgrind::Yes,
        )
    }

    #[test]
    fn platform_switching_wasm() {
        test_roc_app_slim(
            "examples/platform-switching",
            "rocLovesWebAssembly.roc",
            "rocLovesWebAssembly",
            "Roc <3 Web Assembly!\n",
            UseValgrind::Yes,
        )
    }

    #[test]
    fn platform_switching_swift() {
        test_roc_app_slim(
            "examples/platform-switching",
            "rocLovesSwift.roc",
            "rocLovesSwift",
            "Roc <3 Swift!\n",
            UseValgrind::Yes,
        )
    }

    #[test]
    fn expects_dev_and_test() {
        // these are in the same test function so we don't have to worry about race conditions
        // on the building of the platform

        test_roc_app(
            "crates/cli_testing_examples/expects",
            "expects.roc",
            "expects-test",
            &[],
            &[],
            &[],
            indoc!(
                r#"
                This expectation failed:

                18│      expect x != x
                                ^^^^^^

                When it failed, these variables had these values:

                x : Num *
                x = 42

                [<ignored for tests> 19:9] 42
                [<ignored for tests> 20:9] "Fjoer en ferdjer frieten oan dyn geve lea"
                [<ignored for tests> 13:9] "abc"
                [<ignored for tests> 13:9] 10
                [<ignored for tests> 13:9] A (B C)
                Program finished!
                "#
            ),
            UseValgrind::Yes,
            TestCliCommands::Dev,
        );

        test_roc_app(
            "crates/cli_testing_examples/expects",
            "expects.roc",
            "expects-test",
            &[],
            &[],
            &[],
            indoc!(
                r#"
                This expectation failed:

                 6│>  expect
                 7│>      a = 1
                 8│>      b = 2
                 9│>
                10│>      a == b

                When it failed, these variables had these values:

                a : Num *
                a = 1

                b : Num *
                b = 2



                1 failed and 0 passed in <ignored for test> ms."#
            ),
            UseValgrind::Yes,
            TestCliCommands::Test,
        );
    }

    #[test]
    #[cfg_attr(
        windows,
        ignore = "this platform is broken, and `roc run --lib` is missing on windows"
    )]
    fn ruby_interop() {
        test_roc_app_slim(
            "examples/ruby-interop",
            "main.roc",
            "libhello",
            "",
            UseValgrind::Yes,
        )
    }

    #[test]
    #[cfg_attr(
        windows,
        ignore = "Flaky failure: Roc command failed with status ExitStatus(ExitStatus(3221225477))"
    )]
    fn fibonacci() {
        test_roc_app_slim(
            "crates/cli_testing_examples/algorithms",
            "fibonacci.roc",
            "fibonacci",
            "",
            UseValgrind::Yes,
        )
    }

    #[test]
    fn hello_gui() {
        test_roc_app_slim(
            "examples/gui",
            "helloBROKEN.roc",
            "hello-gui",
            "",
            UseValgrind::No,
        )
    }

    #[cfg_attr(windows, ignore)] // flaky error; issue #5024
    #[serial(breakout)]
    #[test]
    fn breakout() {
        test_roc_app_slim(
            "examples/gui/breakout",
            "breakoutBROKEN.roc",
            "breakout",
            "",
            UseValgrind::No,
        )
    }

    #[test]
    #[serial(breakout)]
    fn breakout_hello_gui() {
        test_roc_app_slim(
            "examples/gui/breakout",
            "hello-guiBROKEN.roc",
            "hello-gui",
            "",
            UseValgrind::No,
        )
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn quicksort() {
        test_roc_app_slim(
            "crates/cli_testing_examples/algorithms",
            "quicksort.roc",
            "quicksort",
            "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            UseValgrind::Yes,
        )
    }

    #[test]
    #[cfg_attr(windows, ignore = "missing __udivdi3 and some other symbols")]
    #[serial(cli_platform)]
    fn cli_args() {
        test_roc_app(
            "examples/cli",
            "args.roc",
            "args",
            &[],
            &[
                Arg::PlainText("log"),
                Arg::PlainText("-b"),
                Arg::PlainText("3"),
                Arg::PlainText("--num"),
                Arg::PlainText("81"),
            ],
            &[],
            "4.000000000000001\n",
            UseValgrind::No,
            TestCliCommands::Run,
        )
    }

    // TODO: remove in favor of cli_args once mono bugs are resolved in investigation
    #[test]
    #[cfg_attr(windows, ignore = "missing __udivdi3 and some other symbols")]
    #[serial(cli_platform)]
    fn cli_args_check() {
        let path = file_path_from_root("examples/cli", "args.roc");
        let out = run_roc([CMD_CHECK, path.to_str().unwrap()], &[], &[]);
        assert!(out.status.success());
    }

    // TODO: write a new test once mono bugs are resolved in investigation
    #[test]
    #[cfg(not(debug_assertions))] // https://github.com/roc-lang/roc/issues/4806
    fn check_virtual_dom_server() {
        let path = file_path_from_root("examples/virtual-dom-wip", "example-server.roc");
        let out = run_roc([CMD_CHECK, path.to_str().unwrap()], &[], &[]);
        assert!(out.status.success());
    }

    // TODO: write a new test once mono bugs are resolved in investigation
    #[test]
    #[cfg(not(debug_assertions))] // https://github.com/roc-lang/roc/issues/4806
    fn check_virtual_dom_client() {
        let path = file_path_from_root("examples/virtual-dom-wip", "example-client.roc");
        let out = run_roc([CMD_CHECK, path.to_str().unwrap()], &[], &[]);
        assert!(out.status.success());
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn interactive_effects() {
        test_roc_app(
            "examples/cli",
            "effects.roc",
            "effects",
            &["hi there!"],
            &[],
            &[],
            "hi there!\nIt is known\n",
            UseValgrind::Yes,
            TestCliCommands::Run,
        )
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    // tea = The Elm Architecture
    fn terminal_ui_tea() {
        test_roc_app(
            "examples/cli",
            "tui.roc",
            "tui",
            &["foo\n"], // NOTE: adding more lines leads to memory leaks
            &[],
            &[],
            "Hello Worldfoo!\n",
            UseValgrind::Yes,
            TestCliCommands::Run,
        )
    }

    #[test]
    #[cfg_attr(any(target_os = "windows", target_os = "linux"), ignore = "Segfault")]
    fn false_interpreter() {
        test_roc_app(
            "examples/cli/false-interpreter",
            "False.roc",
            "false",
            &[],
            &[Arg::ExamplePath("examples/sqrt.false")],
            &[],
            "1414",
            UseValgrind::Yes,
            TestCliCommands::Many,
        )
    }

    #[test]
    fn swift_ui() {
        test_roc_app_slim(
            "examples/swiftui",
            "main.roc",
            "swiftui",
            "",
            UseValgrind::No,
        )
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn static_site_gen() {
        test_roc_app(
            "examples/static-site-gen",
            "static-site.roc",
            "static-site",
            &[],
            &[Arg::ExamplePath("input"), Arg::ExamplePath("output")],
            &[],
            "Processed 4 files with 3 successes and 0 errors\n",
            UseValgrind::No,
            TestCliCommands::Run,
        )
    }

    #[test]
    #[serial(cli_platform)]
    #[cfg_attr(windows, ignore)]
    fn with_env_vars() {
        test_roc_app(
            "examples/cli",
            "env.roc",
            "env",
            &[],
            &[],
            &[
                ("EDITOR", "roc-editor"),
                ("SHLVL", "3"),
                ("LETTERS", "a,c,e,j"),
            ],
            "Your favorite editor is roc-editor!\n\
            Your current shell level is 3!\n\
            Your favorite letters are: a c e j\n",
            UseValgrind::No,
            TestCliCommands::Run,
        )
    }

    #[test]
    #[serial(cli_platform)]
    #[cfg_attr(windows, ignore)]
    fn ingested_file() {
        test_roc_app(
            "examples/cli",
            "ingested-file.roc",
            "ingested-file",
            &[],
            &[],
            &[],
            indoc!(
                r#"
                This roc file can print it's own source code. The source is:

                app "ingested-file"
                    packages { pf: "cli-platform/main.roc" }
                    imports [
                        pf.Stdout,
                        "ingested-file.roc" as ownCode : Str,
                    ]
                    provides [main] to pf

                main =
                    Stdout.line "\nThis roc file can print it's own source code. The source is:\n\n\(ownCode)"

                "#
            ),
            UseValgrind::No,
            TestCliCommands::Run,
        )
    }

    #[test]
    #[serial(cli_platform)]
    #[cfg_attr(windows, ignore)]
    fn ingested_file_bytes() {
        test_roc_app(
            "examples/cli",
            "ingested-file-bytes.roc",
            "ingested-file-bytes",
            &[],
            &[],
            &[],
            "22424\n",
            UseValgrind::No,
            TestCliCommands::Run,
        )
    }

    #[test]
    #[serial(zig_platform_parser_package_basic_cli_url)]
    #[cfg_attr(windows, ignore)]
    fn parse_movies_csv() {
        test_roc_app_slim(
            "examples/parser/examples",
            "parse-movies-csv.roc",
            "example",
            "Parse success!\n",
            UseValgrind::No,
        )
    }

    #[test]
    #[serial(zig_platform_parser_package_basic_cli_url)]
    #[cfg_attr(windows, ignore)]
    fn parse_letter_counts() {
        test_roc_app_slim(
            "examples/parser/examples",
            "letter-counts.roc",
            "example",
            "I counted 7 letter A's!\n",
            UseValgrind::No,
        )
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn parse_http() {
        test_roc_expect("examples/parser/package", "ParserHttp.roc")
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn inspect_logging() {
        test_roc_app_slim(
            "examples",
            "inspect-logging.roc",
            "inspect-logging",
            r#"{people: [{firstName: "John", lastName: "Smith", age: 27, hasBeard: true, favoriteColor: Blue}, {firstName: "Debby", lastName: "Johnson", age: 47, hasBeard: false, favoriteColor: Green}, {firstName: "Jane", lastName: "Doe", age: 33, hasBeard: false, favoriteColor: (RGB (255, 255, 0))}], friends: [{2}, {2}, {0, 1}]}
"#,
            UseValgrind::Yes,
        )
    }

    #[test]
    fn inspect_gui() {
        test_roc_app_slim(
            "examples",
            "inspect-gui.roc",
            "inspect-gui",
            "",
            UseValgrind::No,
        )
    }

    // TODO not sure if this cfg should still be here: #[cfg(not(debug_assertions))]
    // this is for testing the benchmarks, to perform proper benchmarks see crates/cli/benches/README.md
    mod test_benchmarks {
        use super::{TestCliCommands, UseValgrind};
        use cli_utils::helpers::cli_testing_dir;

        use super::{check_output_with_stdin, OPTIMIZE_FLAG, PREBUILT_PLATFORM};

        use std::{path::Path, sync::Once};

        static BENCHMARKS_BUILD_PLATFORM: Once = Once::new();

        fn test_benchmark(
            roc_filename: &str,
            executable_filename: &str,
            stdin: &[&str],
            expected_ending: &str,
            use_valgrind: UseValgrind,
        ) {
            let file_name = cli_testing_dir("benchmarks").join(roc_filename);

            // TODO fix QuicksortApp and then remove this!
            match roc_filename {
                "QuicksortApp.roc" => {
                    eprintln!(
                    "WARNING: skipping testing benchmark {roc_filename} because the test is broken right now!"
                );
                    return;
                }
                "TestAStar.roc" => {
                    if cfg!(feature = "wasm32-cli-run") {
                        eprintln!(
                        "WARNING: skipping testing benchmark {roc_filename} because it currently does not work on wasm32 due to dictionaries."
                    );
                        return;
                    }
                }
                _ => {}
            }

            #[cfg(all(not(feature = "wasm32-cli-run"), not(feature = "i386-cli-run")))]
            check_output_regular(
                &file_name,
                stdin,
                executable_filename,
                expected_ending,
                use_valgrind,
            );

            #[cfg(feature = "wasm32-cli-run")]
            check_output_wasm(&file_name, stdin, executable_filename, expected_ending);

            #[cfg(feature = "i386-cli-run")]
            check_output_i386(
                &file_name,
                stdin,
                executable_filename,
                expected_ending,
                use_valgrind,
            );
        }

        #[cfg(all(not(feature = "wasm32-cli-run"), not(feature = "i386-cli-run")))]
        fn check_output_regular(
            file_name: &Path,
            stdin: &[&str],
            executable_filename: &str,
            expected_ending: &str,
            use_valgrind: UseValgrind,
        ) {
            let mut ran_without_optimizations = false;

            BENCHMARKS_BUILD_PLATFORM.call_once(|| {
                // Check with and without optimizations
                check_output_with_stdin(
                    file_name,
                    stdin,
                    executable_filename,
                    &[],
                    &[],
                    &[],
                    expected_ending,
                    use_valgrind,
                    TestCliCommands::Run,
                );

                ran_without_optimizations = true;
            });

            // now we can pass the `PREBUILT_PLATFORM` flag, because the
            // `call_once` will have built the platform

            if !ran_without_optimizations {
                // Check with and without optimizations
                check_output_with_stdin(
                    file_name,
                    stdin,
                    executable_filename,
                    &[PREBUILT_PLATFORM],
                    &[],
                    &[],
                    expected_ending,
                    use_valgrind,
                    TestCliCommands::Run,
                );
            }

            check_output_with_stdin(
                file_name,
                stdin,
                executable_filename,
                &[PREBUILT_PLATFORM, OPTIMIZE_FLAG],
                &[],
                &[],
                expected_ending,
                use_valgrind,
                TestCliCommands::Run,
            );
        }

        #[cfg(feature = "wasm32-cli-run")]
        fn check_output_wasm(
            file_name: &Path,
            stdin: &[&str],
            executable_filename: &str,
            expected_ending: &str,
        ) {
            // Check with and without optimizations
            check_wasm_output_with_stdin(
                file_name,
                stdin,
                executable_filename,
                &[],
                expected_ending,
            );

            check_wasm_output_with_stdin(
                file_name,
                stdin,
                executable_filename,
                &[OPTIMIZE_FLAG],
                expected_ending,
            );
        }

        #[cfg(feature = "wasm32-cli-run")]
        fn check_wasm_output_with_stdin(
            file: &Path,
            stdin: &[&str],
            executable_filename: &str,
            flags: &[&str],
            expected_ending: &str,
        ) {
            use super::{concatcp, run_roc, CMD_BUILD, TARGET_FLAG};

            let mut flags = flags.to_vec();
            flags.push(concatcp!(TARGET_FLAG, "=wasm32"));

            let compile_out = run_roc(
                [CMD_BUILD, file.to_str().unwrap()]
                    .iter()
                    .chain(flags.as_slice()),
                &[],
                &[],
            );

            assert!(
                compile_out.status.success(),
                "bad status stderr:\n{}\nstdout:\n{}",
                compile_out.stderr,
                compile_out.stdout
            );

            let mut path = file.with_file_name(executable_filename);
            path.set_extension("wasm");

            let stdout = crate::run_wasm(&path, stdin);

            if !stdout.ends_with(expected_ending) {
                panic!(
                    "expected output to end with {:?} but instead got {:#?}",
                    expected_ending, stdout
                );
            }
        }

        #[cfg(feature = "i386-cli-run")]
        fn check_output_i386(
            file_name: &Path,
            stdin: &[&str],
            executable_filename: &str,
            expected_ending: &str,
            use_valgrind: UseValgrind,
        ) {
            use super::{concatcp, CMD_BUILD, TARGET_FLAG};

            check_output_with_stdin(
                &file_name,
                stdin,
                executable_filename,
                &[concatcp!(TARGET_FLAG, "=x86_32")],
                &[],
                &[],
                expected_ending,
                use_valgrind,
                TestCliCommands::Run,
            );

            check_output_with_stdin(
                &file_name,
                stdin,
                executable_filename,
                &[concatcp!(TARGET_FLAG, "=x86_32"), OPTIMIZE_FLAG],
                &[],
                &[],
                expected_ending,
                use_valgrind,
                TestCliCommands::Run,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn nqueens() {
            test_benchmark("NQueens.roc", "nqueens", &["6"], "4\n", UseValgrind::Yes)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn cfold() {
            test_benchmark("CFold.roc", "cfold", &["3"], "11 & 11\n", UseValgrind::Yes)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn deriv() {
            test_benchmark(
                "Deriv.roc",
                "deriv",
                &["2"],
                "1 count: 6\n2 count: 22\n",
                UseValgrind::Yes,
            )
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn rbtree_ck() {
            test_benchmark(
                "RBTreeCk.roc",
                "rbtree-ck",
                &["100"],
                "10\n",
                UseValgrind::Yes,
            )
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn rbtree_insert() {
            test_benchmark(
                "RBTreeInsert.roc",
                "rbtree-insert",
                &[],
                "Node Black 0 {} Empty Empty\n",
                UseValgrind::Yes,
            )
        }

        /*
        // rbtree_del does not work
        #[test]
        fn rbtree_del() {
            test_benchmark(
                "RBTreeDel.roc",
                "rbtree-del",
                &["420"],
                &[],
                "30\n",
                true
            )
        }*/

        #[test]
        #[cfg_attr(windows, ignore)]
        fn astar() {
            test_benchmark(
                "TestAStar.roc",
                "test-astar",
                &[],
                "True\n",
                UseValgrind::No,
            )
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn base64() {
            test_benchmark(
                "TestBase64.roc",
                "test-base64",
                &[],
                "encoded: SGVsbG8gV29ybGQ=\ndecoded: Hello World\n",
                UseValgrind::Yes,
            )
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn closure() {
            test_benchmark("Closure.roc", "closure", &[], "", UseValgrind::No)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn issue2279() {
            test_benchmark(
                "Issue2279.roc",
                "issue2279",
                &[],
                "Hello, world!\n",
                UseValgrind::Yes,
            )
        }

        #[test]
        fn quicksort_app() {
            test_benchmark(
                "QuicksortApp.roc",
                "quicksortapp",
                &[],
                "todo put the correct quicksort answer here",
                UseValgrind::Yes,
            )
        }
    }

    #[test]
    #[serial(multi_dep_str)]
    #[cfg_attr(windows, ignore)]
    fn run_multi_dep_str_unoptimized() {
        check_output_with_stdin(
            &fixture_file("multi-dep-str", "Main.roc"),
            &[],
            "multi-dep-str",
            &[],
            &[],
            &[],
            "I am Dep2.str2\n",
            UseValgrind::Yes,
            TestCliCommands::Run,
        );
    }

    #[test]
    #[serial(multi_dep_str)]
    #[cfg_attr(windows, ignore)]
    fn run_multi_dep_str_optimized() {
        check_output_with_stdin(
            &fixture_file("multi-dep-str", "Main.roc"),
            &[],
            "multi-dep-str",
            &[OPTIMIZE_FLAG],
            &[],
            &[],
            "I am Dep2.str2\n",
            UseValgrind::Yes,
            TestCliCommands::Run,
        );
    }

    #[test]
    #[serial(multi_dep_thunk)]
    #[cfg_attr(windows, ignore)]
    fn run_multi_dep_thunk_unoptimized() {
        check_output_with_stdin(
            &fixture_file("multi-dep-thunk", "Main.roc"),
            &[],
            "multi-dep-thunk",
            &[],
            &[],
            &[],
            "I am Dep2.value2\n",
            UseValgrind::Yes,
            TestCliCommands::Run,
        );
    }

    #[test]
    #[serial(multi_dep_thunk)]
    #[cfg_attr(
        windows,
        ignore = "Flaky failure: Roc command failed with status ExitStatus(ExitStatus(3221225477))"
    )]
    fn run_multi_dep_thunk_optimized() {
        check_output_with_stdin(
            &fixture_file("multi-dep-thunk", "Main.roc"),
            &[],
            "multi-dep-thunk",
            &[OPTIMIZE_FLAG],
            &[],
            &[],
            "I am Dep2.value2\n",
            UseValgrind::Yes,
            TestCliCommands::Run,
        );
    }

    #[test]
    #[serial(multi_dep_thunk)]
    #[cfg_attr(windows, ignore)]
    fn run_packages_unoptimized() {
        check_output_with_stdin(
            &fixture_file("packages", "app.roc"),
            &[],
            "packages-test",
            &[],
            &[],
            &[],
            "Hello, World! This text came from a package! This text came from a CSV package!\n",
            UseValgrind::Yes,
            TestCliCommands::Run,
        );
    }

    #[test]
    #[serial(multi_dep_thunk)]
    #[cfg_attr(windows, ignore)]
    fn run_packages_optimized() {
        check_output_with_stdin(
            &fixture_file("packages", "app.roc"),
            &[],
            "packages-test",
            &[OPTIMIZE_FLAG],
            &[],
            &[],
            "Hello, World! This text came from a package! This text came from a CSV package!\n",
            UseValgrind::Yes,
            TestCliCommands::Run,
        );
    }

    #[test]
    fn known_type_error() {
        check_compile_error(
            &known_bad_file("TypeError.roc"),
            &[],
            indoc!(
                r#"
                ── TYPE MISMATCH ─────────────────────────────── tests/known_bad/TypeError.roc ─

                Something is off with the body of the main definition:

                6│  main : Str -> Task {} []
                7│  main = /_ ->
                8│      "this is a string, not a Task {} [] function like the platform expects."
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                The body is a string of type:

                    Str

                But the type annotation on main says it should be:

                    Effect.Effect (Result {} [])

                Tip: Type comparisons between an opaque type are only ever equal if
                both types are the same opaque type. Did you mean to create an opaque
                type by wrapping it? If I have an opaque type Age := U32 I can create
                an instance of this opaque type by doing @Age 23.

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

#[cfg(feature = "wasm32-cli-run")]
fn run_wasm(wasm_path: &std::path::Path, stdin: &[&str]) -> String {
    use bumpalo::Bump;
    use roc_wasm_interp::{DefaultImportDispatcher, Instance, Value, WasiFile};

    let wasm_bytes = std::fs::read(wasm_path).unwrap();
    let arena = Bump::new();

    let mut instance = {
        let mut fake_stdin = vec![];
        let fake_stdout = vec![];
        let fake_stderr = vec![];
        for s in stdin {
            fake_stdin.extend_from_slice(s.as_bytes())
        }

        let mut dispatcher = DefaultImportDispatcher::default();
        dispatcher.wasi.files = vec![
            WasiFile::ReadOnly(fake_stdin),
            WasiFile::WriteOnly(fake_stdout),
            WasiFile::WriteOnly(fake_stderr),
        ];

        Instance::from_bytes(&arena, &wasm_bytes, dispatcher, false).unwrap()
    };

    let result = instance.call_export("_start", []);

    match result {
        Ok(Some(Value::I32(0))) => match &instance.import_dispatcher.wasi.files[1] {
            WasiFile::WriteOnly(fake_stdout) => String::from_utf8(fake_stdout.clone())
                .unwrap_or_else(|_| "Wasm test printed invalid UTF-8".into()),
            _ => unreachable!(),
        },
        Ok(Some(Value::I32(exit_code))) => {
            format!("WASI app exit code {}", exit_code)
        }
        Ok(Some(val)) => {
            format!("WASI _start returned an unexpected number type {:?}", val)
        }
        Ok(None) => "WASI _start returned no value".into(),
        Err(e) => {
            format!("WASI error {}", e)
        }
    }
}
