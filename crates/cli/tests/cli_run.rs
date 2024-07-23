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
        known_bad_file, run_cmd, run_roc, run_with_valgrind, Out, ValgrindError,
        ValgrindErrorXWhat,
    };
    use const_format::concatcp;
    use indoc::indoc;
    use regex::Regex;
    use roc_cli::{CMD_BUILD, CMD_CHECK, CMD_DEV, CMD_FORMAT, CMD_RUN, CMD_TEST};
    use roc_reporting::report::strip_colors;
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
    #[allow(dead_code)]
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
        // allow because we may need PlainText in the future
        #[allow(dead_code)]
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

            let cmd_output = match cli_mode {
                CliMode::RocBuild => {
                    run_roc_on_failure_is_panic(
                        file,
                        iter::once(CMD_BUILD).chain(flags.clone()),
                        &[],
                        &[],
                        &[],
                    );

                    let file_ext = if cfg!(windows) { "exe " } else { "" };

                    if matches!(use_valgrind, UseValgrind::Yes) && ALLOW_VALGRIND {
                        let mut valgrind_args =
                            vec![file.with_extension(file_ext).to_str().unwrap().to_string()];
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
                            file.with_extension(file_ext).to_str().unwrap(),
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

            let self_path = file.display().to_string();

            let actual_cmd_stdout = ignore_test_timings(&strip_colors(&cmd_output.stdout))
                .replace(&self_path, "<ignored for tests>");

            if !actual_cmd_stdout.ends_with(expected_ending) {
                panic!(
                    "> expected output to end with:\n{}\n> but instead got:\n{}\n> stderr was:\n{}",
                    expected_ending, actual_cmd_stdout, cmd_output.stderr
                );
            }

            if !cmd_output.status.success() && !matches!(cli_mode, CliMode::RocTest) {
                // We don't need stdout, Cargo prints it for us.
                panic!(
                    "Example program exited with status {:?}\nstderr was:\n{:#?}",
                    cmd_output.status, cmd_output.stderr
                );
            }
        }
    }

    fn ignore_test_timings(cmd_output: &str) -> String {
        let regex = Regex::new(r" in (\d+) ms\.").expect("Invalid regex pattern");
        let replacement = " in <ignored for test> ms.";
        regex.replace_all(cmd_output, replacement).to_string()
    }

    // when you want to run `roc test` to execute `expect`s, perhaps on a library rather than an application.
    fn test_roc_expect(dir_name: &str, roc_filename: &str, flags: &[&str], expected_ending: &str) {
        let path = file_path_from_root(dir_name, roc_filename);
        check_output_with_stdin(
            &path,
            &[],
            flags,
            &[],
            &[],
            expected_ending,
            UseValgrind::Yes,
            TestCliCommands::Test,
        );
    }

    // when you don't need args, stdin or extra_env
    fn test_roc_app_slim(
        dir_name: &str,
        roc_filename: &str,
        expected_ending: &str,
        use_valgrind: UseValgrind,
    ) {
        test_roc_app(
            dir_name,
            roc_filename,
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

        if dir_name.to_ascii_lowercase().contains("webassembly")
            || roc_filename.to_ascii_lowercase().contains("webassembly")
        {
            // this is a web assembly example, but we don't test with JS at the moment
            eprintln!(
                    "WARNING: skipping testing example {roc_filename} because it only works in a browser!"
                );
            return;
        }

        // Only run Swift examples on macOS
        if dir_name.to_ascii_lowercase().contains("swift")
            || roc_filename.to_ascii_lowercase().contains("swift")
        {
            if cfg!(target_os = "macos") {
                run_roc_on(&file_name, [CMD_BUILD, OPTIMIZE_FLAG], &[], &[], &[]);
                return;
            } else {
                eprintln!(
                        "WARNING: skipping testing example {roc_filename} because it only works on MacOS."
                    );
                return;
            }
        }

        if dir_name.starts_with("examples/gui")
            || roc_filename.ends_with("gui.roc")
            || dir_name.ends_with("-interop")
        {
            // Since these require things the build system often doesn't have
            // (e.g. GUIs open a window, interop needs a language installed)
            // we do `roc build` on them but don't run them.
            run_roc_on(&file_name, [CMD_BUILD, OPTIMIZE_FLAG], &[], &[], &[]);
            return;
        } else if roc_filename == "args.roc" {
            custom_flags = vec![LINKER_FLAG, "legacy"];
        }

        // Check with and without optimizations
        check_output_with_stdin(
            &file_name,
            stdin,
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
            "Roc <3 Zig!\n",
            UseValgrind::Yes,
        )
    }

    #[test]
    fn platform_switching_wasm() {
        test_roc_app_slim(
            "examples/platform-switching",
            "rocLovesWebAssembly.roc",
            "Roc <3 Web Assembly!\n",
            UseValgrind::Yes,
        )
    }

    #[test]
    fn expects_dev_and_test() {
        // these are in the same test function so we don't have to worry about race conditions
        // on the building of the platform

        test_roc_app(
            "crates/cli/tests/expects",
            "expects.roc",
            &[],
            &[],
            &[],
            indoc!(
                r#"
                ── EXPECT FAILED in tests/expects/expects.roc ──────────────────────────────────

                This expectation failed:

                28│      expect words == []
                                ^^^^^^^^^^^

                When it failed, these variables had these values:

                words : List Str
                words = ["this", "will", "for", "sure", "be", "a", "large", "string", "so", "when", "we", "split", "it", "it", "will", "use", "seamless", "slices", "which", "affect", "printing"]

                [<ignored for tests>:31] x = 42
                [<ignored for tests>:33] "Fjoer en ferdjer frieten oan dyn geve lea" = "Fjoer en ferdjer frieten oan dyn geve lea"
                [<ignored for tests>:35] "this is line 24" = "this is line 24"
                [<ignored for tests>:21] x = "abc"
                [<ignored for tests>:21] x = 10
                [<ignored for tests>:21] x = (A (B C))
                Program finished!
                "#
            ),
            UseValgrind::Yes,
            TestCliCommands::Dev,
        );

        test_roc_app(
            "crates/cli/tests/expects",
            "expects.roc",
            &[],
            &[],
            &[],
            indoc!(
                r#"
                ── EXPECT FAILED in tests/expects/expects.roc ──────────────────────────────────

                This expectation failed:

                9│      expect a == 2
                               ^^^^^^

                When it failed, these variables had these values:

                a : Num *
                a = 1

                ── EXPECT FAILED in tests/expects/expects.roc ──────────────────────────────────

                This expectation failed:

                10│      expect a == 3
                                ^^^^^^

                When it failed, these variables had these values:

                a : Num *
                a = 1

                ── EXPECT FAILED in tests/expects/expects.roc ──────────────────────────────────

                This expectation failed:

                14│>  expect
                15│>      a = makeA
                16│>      b = 2i64
                17│>
                18│>      a == b

                When it failed, these variables had these values:

                a : Int Signed64
                a = 1

                b : I64
                b = 2


                1 failed and 0 passed in <ignored for test> ms.
                "#
            ),
            UseValgrind::Yes,
            TestCliCommands::Test,
        );
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn test_module_imports_pkg_w_flag() {
        test_roc_expect(
            "crates/cli/tests/module_imports_pkg",
            "Module.roc",
            &["--main", "tests/module_imports_pkg/app.roc"],
            indoc!(
                r#"
                0 failed and 1 passed in <ignored for test> ms.
                "#
            ),
        )
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn test_module_imports_pkg_no_flag() {
        test_roc_expect(
            "crates/cli/tests/module_imports_pkg",
            "Module.roc",
            &[],
            indoc!(
                r#"
                ── UNRECOGNIZED PACKAGE in tests/module_imports_pkg/Module.roc ─────────────────

                This module is trying to import from `pkg`:

                3│  import pkg.Foo
                           ^^^^^^^

                A lowercase name indicates a package shorthand, but I don't know which
                packages are available.

                When checking a module directly, I look for a `main.roc` app or
                package to resolve shorthands from.

                You can create it, or specify an existing one with the --main flag."#
            ),
        )
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn test_module_imports_unknown_pkg() {
        test_roc_expect(
            "crates/cli/tests/module_imports_pkg",
            "ImportsUnknownPkg.roc",
            &["--main", "tests/module_imports_pkg/app.roc"],
            indoc!(
                r#"
                ── UNRECOGNIZED PACKAGE in tests/module_imports_pkg/ImportsUnknownPkg.roc ──────

                This module is trying to import from `cli`:

                3│  import cli.Foo
                           ^^^^^^^

                A lowercase name indicates a package shorthand, but I don't recognize
                this one. Did you mean one of these?

                    pkg

                Note: I'm using the following module to resolve package shorthands:

                    tests/module_imports_pkg/app.roc

                You can specify a different one with the --main flag."#
            ),
        )
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn transitive_expects() {
        test_roc_expect(
            "crates/cli/tests/expects_transitive",
            "main.roc",
            &[],
            indoc!(
                r#"
                0 failed and 3 passed in <ignored for test> ms.
                "#
            ),
        );
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn transitive_expects_verbose() {
        test_roc_expect(
            "crates/cli/tests/expects_transitive",
            "main.roc",
            &["--verbose"],
            indoc!(
                r#"
                Compiled in <ignored for test> ms.

                Direct.roc:
                    0 failed and 2 passed in <ignored for test> ms.

                Transitive.roc:
                    0 failed and 1 passed in <ignored for test> ms.
                "#
            ),
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
            "libhello.roc",
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
            "crates/cli/tests/algorithms",
            "fibonacci.roc",
            "",
            UseValgrind::Yes,
        )
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn quicksort() {
        test_roc_app_slim(
            "crates/cli/tests/algorithms",
            "quicksort.roc",
            "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
            UseValgrind::Yes,
        )
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
    #[serial(cli_platform)]
    fn cli_countdown_check() {
        let path = file_path_from_root("crates/cli/tests/cli", "countdown.roc");
        let out = run_roc([CMD_CHECK, path.to_str().unwrap()], &[], &[]);
        assert!(out.status.success());
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    #[serial(cli_platform)]
    fn cli_echo_check() {
        let path = file_path_from_root("crates/cli/tests/cli", "echo.roc");
        let out = run_roc([CMD_CHECK, path.to_str().unwrap()], &[], &[]);
        assert!(out.status.success());
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    #[serial(cli_platform)]
    fn cli_file_check() {
        let path = file_path_from_root("crates/cli/tests/cli", "fileBROKEN.roc");
        let out = run_roc([CMD_CHECK, path.to_str().unwrap()], &[], &[]);
        assert!(out.status.success());
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    #[serial(cli_platform)]
    fn cli_form_check() {
        let path = file_path_from_root("crates/cli/tests/cli", "form.roc");
        let out = run_roc([CMD_CHECK, path.to_str().unwrap()], &[], &[]);
        assert!(out.status.success());
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    #[serial(cli_platform)]
    fn cli_http_get_check() {
        let path = file_path_from_root("crates/cli/tests/cli", "http-get.roc");
        let out = run_roc([CMD_CHECK, path.to_str().unwrap()], &[], &[]);
        assert!(out.status.success());
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn interactive_effects() {
        test_roc_app(
            "examples/cli",
            "effects.roc",
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
            &[],
            &[Arg::ExamplePath("examples/sqrt.false")],
            &[],
            "1414",
            UseValgrind::Yes,
            TestCliCommands::Many,
        )
    }

    #[test]
    #[serial(cli_platform)]
    #[cfg_attr(windows, ignore)]
    fn with_env_vars() {
        test_roc_app(
            "crates/cli/tests/cli",
            "env.roc",
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
            "crates/cli/tests/cli",
            "ingested-file.roc",
            &[],
            &[],
            &[],
            format!(
                "\nThis roc file can print its own source code. The source is:\n\n{}\n",
                include_str!("cli/ingested-file.roc")
            )
            .as_str(),
            UseValgrind::No,
            TestCliCommands::Run,
        )
    }

    #[test]
    #[serial(cli_platform)]
    #[cfg_attr(windows, ignore)]
    fn combine_tasks_with_record_builder() {
        test_roc_app(
            "crates/cli/tests/cli",
            "combine-tasks.roc",
            &[],
            &[],
            &[],
            "For multiple tasks: {a: 123, b: \"abc\", c: [123], d: [\"abc\"], e: {\"a\": \"b\"}}\n",
            UseValgrind::No,
            TestCliCommands::Run,
        )
    }

    #[test]
    #[serial(cli_platform)]
    #[cfg_attr(windows, ignore)]
    fn parse_args_with_record_builder() {
        test_roc_app(
            "crates/cli/tests/cli",
            "parse-args.roc",
            &[],
            &[],
            &[],
            "Success: {count: 5, doubled: 14, file: \"file.txt\"}\n",
            UseValgrind::No,
            TestCliCommands::Run,
        )
    }

    #[test]
    #[serial(cli_platform)]
    #[cfg_attr(windows, ignore)]
    fn ingested_file_bytes() {
        test_roc_app(
            "crates/cli/tests/cli",
            "ingested-file-bytes.roc",
            &[],
            &[],
            &[],
            "6239\n",
            UseValgrind::No,
            TestCliCommands::Run,
        )
    }
    #[test]
    #[serial(cli_platform)]
    #[cfg_attr(windows, ignore)]
    fn ingested_file_bytes_no_ann() {
        test_roc_app(
            "crates/cli/tests/cli",
            "ingested-file-bytes-no-ann.roc",
            &[],
            &[],
            &[],
            "6239\n",
            UseValgrind::No,
            TestCliCommands::Run,
        )
    }

    #[test]
    #[serial(zig_platform_parser_package_basic_cli_url)]
    #[cfg_attr(windows, ignore)]
    fn parse_movies_csv() {
        test_roc_app_slim(
            "crates/cli/tests/cli",
            "parser-movies-csv.roc",
            "2 movies were found:\n\nThe movie 'Airplane!' was released in 1980 and stars Robert Hays and Julie Hagerty\nThe movie 'Caddyshack' was released in 1980 and stars Chevy Chase, Rodney Dangerfield, Ted Knight, Michael O'Keefe and Bill Murray\n\nParse success!\n\n",
            UseValgrind::No,
        )
    }

    #[test]
    #[serial(zig_platform_parser_package_basic_cli_url)]
    #[cfg_attr(windows, ignore)]
    fn parse_letter_counts() {
        test_roc_app_slim(
            "crates/cli/tests/cli",
            "parser-letter-counts.roc",
            "I counted 7 letter A's!\n",
            UseValgrind::No,
        )
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn inspect_logging() {
        test_roc_app_slim(
            "examples",
            "inspect-logging.roc",
            r#"(@Community {friends: [{2}, {2}, {0, 1}], people: [(@Person {age: 27, favoriteColor: Blue, firstName: "John", hasBeard: Bool.true, lastName: "Smith"}), (@Person {age: 47, favoriteColor: Green, firstName: "Debby", hasBeard: Bool.false, lastName: "Johnson"}), (@Person {age: 33, favoriteColor: (RGB (255, 255, 0)), firstName: "Jane", hasBeard: Bool.false, lastName: "Doe"})]})
"#,
            UseValgrind::Yes,
        )
    }

    // TODO not sure if this cfg should still be here: #[cfg(not(debug_assertions))]
    // this is for testing the benchmarks, to perform proper benchmarks see crates/cli/benches/README.md
    mod test_benchmarks {
        #[allow(unused_imports)]
        use super::{TestCliCommands, UseValgrind};
        use cli_utils::helpers::cli_testing_dir;

        #[allow(unused_imports)]
        use super::{check_output_with_stdin, OPTIMIZE_FLAG, PREBUILT_PLATFORM};

        #[allow(unused_imports)]
        use std::{path::Path, sync::Once};

        fn test_benchmark(
            roc_filename: &str,
            stdin: &[&str],
            expected_ending: &str,
            _use_valgrind: UseValgrind,
        ) {
            let file_name = cli_testing_dir("benchmarks").join(roc_filename);

            // TODO fix QuicksortApp and then remove this!
            match roc_filename {
                "quicksortApp.roc" => {
                    eprintln!(
                    "WARNING: skipping testing benchmark {roc_filename} because the test is broken right now!"
                );
                    return;
                }
                "testAStar.roc" => {
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
            check_output_regular(&file_name, stdin, expected_ending, _use_valgrind);

            #[cfg(feature = "wasm32-cli-run")]
            check_output_wasm(&file_name, stdin, expected_ending);

            #[cfg(feature = "i386-cli-run")]
            check_output_i386(&file_name, stdin, expected_ending, _use_valgrind);
        }

        #[cfg(all(not(feature = "wasm32-cli-run"), not(feature = "i386-cli-run")))]
        static BENCHMARKS_BUILD_PLATFORM: Once = Once::new();

        #[cfg(all(not(feature = "wasm32-cli-run"), not(feature = "i386-cli-run")))]
        fn check_output_regular(
            file_name: &Path,
            stdin: &[&str],
            expected_ending: &str,
            use_valgrind: UseValgrind,
        ) {
            let mut ran_without_optimizations = false;

            BENCHMARKS_BUILD_PLATFORM.call_once(|| {
                // Check with and without optimizations
                check_output_with_stdin(
                    file_name,
                    stdin,
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
                &[PREBUILT_PLATFORM, OPTIMIZE_FLAG],
                &[],
                &[],
                expected_ending,
                use_valgrind,
                TestCliCommands::Run,
            );
        }

        #[cfg(feature = "wasm32-cli-run")]
        fn check_output_wasm(file_name: &Path, stdin: &[&str], expected_ending: &str) {
            // Check with and without optimizations
            check_wasm_output_with_stdin(file_name, stdin, &[], expected_ending);

            check_wasm_output_with_stdin(file_name, stdin, &[OPTIMIZE_FLAG], expected_ending);
        }

        #[cfg(feature = "wasm32-cli-run")]
        fn check_wasm_output_with_stdin(
            file: &Path,
            stdin: &[&str],
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

            let stdout = crate::run_wasm(&file.with_extension("wasm"), stdin);

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
            expected_ending: &str,
            use_valgrind: UseValgrind,
        ) {
            use super::{concatcp, CMD_BUILD, TARGET_FLAG};

            check_output_with_stdin(
                &file_name,
                stdin,
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
            test_benchmark("nQueens.roc", &["6"], "4\n", UseValgrind::Yes)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn cfold() {
            test_benchmark("cFold.roc", &["3"], "11 & 11\n", UseValgrind::Yes)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn deriv() {
            test_benchmark(
                "deriv.roc",
                &["2"],
                "1 count: 6\n2 count: 22\n",
                UseValgrind::Yes,
            )
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn rbtree_ck() {
            test_benchmark("rBTreeCk.roc", &["100"], "10\n", UseValgrind::Yes)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn rbtree_insert() {
            test_benchmark(
                "rBTreeInsert.roc",
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
                "rBTreeDel.roc",
                &["420"],
                "30\n",
                UseValgrind::Yes,
            )
        }
        */

        #[test]
        #[cfg_attr(windows, ignore)]
        fn astar() {
            test_benchmark("testAStar.roc", &[], "True\n", UseValgrind::No)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn base64() {
            test_benchmark(
                "testBase64.roc",
                &[],
                "encoded: SGVsbG8gV29ybGQ=\ndecoded: Hello World\n",
                UseValgrind::Yes,
            )
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn closure() {
            test_benchmark("closure.roc", &[], "", UseValgrind::No)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn issue2279() {
            test_benchmark("issue2279.roc", &[], "Hello, world!\n", UseValgrind::Yes)
        }

        #[test]
        fn quicksort_app() {
            test_benchmark(
                "quicksortApp.roc",
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
            &[OPTIMIZE_FLAG],
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
    fn run_transitive_deps_app() {
        check_output_with_stdin(
            &fixture_file("transitive-deps", "direct-one.roc"),
            &[],
            &[],
            &[],
            &[],
            "[One imports Two: From two]\n",
            UseValgrind::Yes,
            TestCliCommands::Run,
        );
    }

    #[test]
    #[serial(multi_dep_thunk)]
    #[cfg_attr(windows, ignore)]
    fn run_transitive_and_direct_dep_app() {
        check_output_with_stdin(
            &fixture_file("transitive-deps", "direct-one-and-two.roc"),
            &[],
            &[],
            &[],
            &[],
            "[One imports Two: From two] | From two\n",
            UseValgrind::Yes,
            TestCliCommands::Run,
        );
    }

    #[test]
    #[serial(multi_dep_thunk)]
    #[cfg_attr(windows, ignore)]
    fn run_double_transitive_dep_app() {
        check_output_with_stdin(
            &fixture_file("transitive-deps", "direct-zero.roc"),
            &[],
            &[],
            &[],
            &[],
            "[Zero imports One: [One imports Two: From two]]\n",
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
                ── TYPE MISMATCH in tests/known_bad/TypeError.roc ──────────────────────────────

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
    fn known_type_error_with_long_path() {
        check_compile_error(
            &known_bad_file("UnusedImportButWithALongFileNameForTesting.roc"),
            &[],
            indoc!(
                r#"
                ── UNUSED IMPORT in ...nown_bad/UnusedImportButWithALongFileNameForTesting.roc ─

                Symbol is imported but not used.

                3│      imports [Symbol.{ Ident }]
                                 ^^^^^^^^^^^^^^^^

                Since Symbol isn't used, you don't need to import it.

                ────────────────────────────────────────────────────────────────────────────────

                0 errors and 1 warning found in <ignored for test> ms."#
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
                ── MISSING DEFINITION in tests/known_bad/ExposedNotDefined.roc ─────────────────

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
                ── UNUSED IMPORT in tests/known_bad/UnusedImport.roc ───────────────────────────

                Symbol is imported but not used.

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
                ── UNKNOWN GENERATES FUNCTION in tests/known_bad/UnknownGeneratesWith.roc ──────

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
