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
        dir_path_from_root, extract_valgrind_errors, file_path_from_root, known_bad_file,
        ExpectedString, Out, Run, ValgrindError, ValgrindErrorXWhat, COMMON_STDERR,
    };
    use const_format::concatcp;
    use indoc::indoc;
    use regex::Regex;
    use roc_cli::{CMD_BUILD, CMD_CHECK, CMD_DEV, CMD_FORMAT, CMD_RUN, CMD_TEST};
    use roc_parse::keyword::CRASH;
    use roc_reporting::report::strip_colors;
    use roc_test_utils::assert_multiline_str_eq;
    use serial_test::serial;
    use std::io::Read;
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

    impl UseValgrind {
        fn and_is_supported(&self) -> bool {
            matches!(self, UseValgrind::Yes) && ALLOW_VALGRIND
        }
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
    const TARGET_FLAG: &str = concatcp!("--", roc_cli::FLAG_TARGET);

    #[derive(Debug, Clone, Copy)]
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

    /// Run `roc check` on a file and check that the output matches the expected string.
    fn check_compile_error(file: &Path, flags: &[&str], expected: &str) {
        let runner = Run::new_roc()
            .arg(CMD_CHECK)
            .arg(file)
            .add_args(flags.clone());

        let out = runner.run();

        let err = out.stdout.trim();
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
        let runner = Run::new_roc()
            .arg(CMD_FORMAT)
            .arg(CHECK_FLAG)
            .arg(file.to_str().unwrap());

        let out = runner.run();

        assert_eq!(out.status.success(), expects_success_exit_code);
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
        todo!()
    }

    #[allow(clippy::too_many_arguments)]
    fn get_output_with_stdin(
        file: &Path,
        stdin: Vec<&'static str>,
        flags: &[&str],
        roc_app_args: &[String],
        extra_env: &[(&str, &str)],
        use_valgrind: UseValgrind,
        test_cli_commands: TestCliCommands,
    ) -> Vec<(CliMode, Out)> {
        let mut output = Vec::new();
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

        for cli_mode in cli_commands.into_iter() {
            let flags = {
                let mut vec = flags.to_vec();

                vec.push("--build-host");

                // max-threads segfaults on windows right now
                if !cfg!(windows) {
                    vec.push("--max-threads=1");
                }

                vec.into_iter()
            };

            let cmd_output = match cli_mode {
                CliMode::RocBuild => {
                    let runner = Run::new_roc()
                        .arg(file.to_str().unwrap())
                        .arg(CMD_BUILD)
                        .add_args(flags.clone());

                    let out = runner.clone().run();

                    out.assert_clean_success();

                    output.push((cli_mode, out));

                    let file_ext = if cfg!(windows) { "exe " } else { "" };

                    if matches!(use_valgrind, UseValgrind::Yes) && ALLOW_VALGRIND {
                        let out = runner.run_with_valgrind();

                        let mut raw_xml = String::new();

                        out.valgrind_xml
                            .as_ref()
                            .unwrap()
                            .read_to_string(&mut raw_xml)
                            .unwrap();

                        if out.status.success() {
                            let memory_errors = extract_valgrind_errors(&raw_xml).unwrap_or_else(|err| {
                                panic!("failed to parse the `valgrind` xml output:\n\n  Error was:\n\n    {:?}\n\n  valgrind xml was:\n\n    \"{}\"\n\n  valgrind stdout was:\n\n    \"{}\"\n\n  valgrind stderr was:\n\n    \"{}\"", err, raw_xml, out.stdout, out.stderr);
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
                            let exit_code = match out.status.code() {
                                Some(code) => format!("exit code {code}"),
                                None => "no exit code".to_string(),
                            };

                            panic!("`valgrind` exited with {}. valgrind stdout was: \"{}\"\n\nvalgrind stderr was: \"{}\"", exit_code, out.stdout, out.stderr);
                        }

                        output.push((cli_mode, out));
                    } else {
                        let mut runner = Run::new_roc()
                            .arg(file.with_extension(file_ext).to_str().unwrap())
                            .add_args(roc_app_args)
                            .with_stdin_vals(stdin.clone());

                        for env in extra_env {
                            // this is funky, fix me
                            runner.with_env([*env]);
                        }

                        let out = runner.run();

                        out.assert_clean_success();

                        output.push((cli_mode, out));
                    }
                }
                CliMode::Roc => {
                    let mut runner = Run::new_roc()
                        .arg(file)
                        .add_args(flags.clone())
                        .add_args(roc_app_args)
                        .with_stdin_vals(stdin.clone());

                    for env in extra_env {
                        // this is funky, fix me
                        runner.with_env([*env]);
                    }

                    let out = runner.run();

                    out.assert_clean_success();

                    output.push((cli_mode, out));
                }
                CliMode::RocRun => {
                    let mut runner = Run::new_roc()
                        .arg(CMD_RUN)
                        .add_args(flags.clone())
                        .add_args(roc_app_args)
                        .arg(file)
                        .with_stdin_vals(stdin.clone());

                    for env in extra_env {
                        // this is funky, fix me
                        runner.with_env([*env]);
                    }

                    let out = runner.run();

                    out.assert_clean_success();

                    output.push((cli_mode, out));
                }
                CliMode::RocTest => {
                    // here failure is what we expect

                    let mut runner = Run::new_roc()
                        .arg(CMD_TEST)
                        .add_args(flags.clone())
                        .add_args(roc_app_args)
                        .arg(file)
                        .with_stdin_vals(stdin.clone());

                    for env in extra_env {
                        // this is funky, fix me
                        runner.with_env([*env]);
                    }

                    let out = runner.run();

                    out.assert_clean_success();

                    output.push((cli_mode, out));
                }
                CliMode::RocDev => {
                    // here failure is what we expect

                    let mut runner = Run::new_roc()
                        .arg(file)
                        .add_args(iter::once(CMD_DEV).chain(flags.clone()))
                        .add_args(roc_app_args)
                        .with_stdin_vals(stdin.clone());

                    for env in extra_env {
                        // this is funky, fix me
                        runner.with_env([*env]);
                    }

                    let out = runner.run();

                    out.assert_clean_success();

                    output.push((cli_mode, out));
                }
            };
        }
        output
    }

    /// Run `roc test` to execute `expect`s, perhaps on a library rather than an application
    /// will use valgrind if it's supported
    fn test_roc_expect(dir_name: &str, roc_filename: &str, flags: &[&str], expected_ending: &str) {
        let runner = Run::new_roc()
            .arg(CMD_TEST)
            .arg(file_path_from_root(dir_name, roc_filename).as_path())
            .add_args(flags);

        let use_valgrind = UseValgrind::Yes;

        if use_valgrind.and_is_supported() {
            let out = runner.run_with_valgrind();
            out.assert_clean_success();
            out.assert_stdout_ends_with(expected_ending);
        } else {
            let out = runner.run();
            out.assert_clean_success();
            out.assert_stdout_ends_with(expected_ending);
        }
    }

    /// For when you don't need args, stdin or extra_env pizzazz.
    fn test_roc_app_slim(
        dir_name: &str,
        roc_filename: &str,
        expected_ending: &str,
        use_valgrind: UseValgrind,
    ) {
        let runner = Run::new_roc()
            .arg(CMD_RUN)
            .arg(file_path_from_root(dir_name, roc_filename).as_path());

        if use_valgrind.and_is_supported() {
            let out = runner.run_with_valgrind();
            out.assert_clean_success();
            out.assert_stdout_ends_with(expected_ending);
        } else {
            let out = runner.run();
            out.assert_clean_success();
            out.assert_stdout_ends_with(expected_ending);
        }
    }

    #[allow(clippy::too_many_arguments)]
    fn test_roc_app(
        dir_name: &str,
        roc_filename: &str,
        stdin: Vec<&'static str>,
        args: &[&str],
        extra_env: Vec<(&str, &str)>,
        expected_ending: &str,
        use_valgrind: UseValgrind,
        test_cli_commands: TestCliCommands,
    ) {
        if dir_name.to_ascii_lowercase().contains("webassembly")
            || roc_filename.to_ascii_lowercase().contains("webassembly")
        {
            // this is a web assembly example, but we don't test with JS at the moment
            eprintln!(
                    "WARNING: skipping testing example {roc_filename} because it only works in a browser!"
                );
            return;
        }

        let runner = Run::new_roc()
            .arg(CMD_RUN)
            .arg(OPTIMIZE_FLAG) // because the false interpreter is still very slow without it
            .with_stdin_vals(stdin)
            .add_args(args)
            // TODO we should pipe this in here... just need to fix lifetimes 
            // .with_env(extra_env)
            .arg(file_path_from_root(dir_name, roc_filename).as_path());

        if use_valgrind.and_is_supported() {
            let out = runner.run_with_valgrind();
            out.assert_clean_success();
            out.assert_stdout_ends_with(expected_ending);
        } else {
            let out = runner.run();
            out.assert_clean_success();
            out.assert_stdout_ends_with(expected_ending);
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

    #[ignore = "TODO move this to roc-lang/examples repository"]
    #[test]
    fn expects_dev_and_test() {
        // these are in the same test function so we don't have to worry about race conditions
        // on the building of the platform

        test_roc_app(
            "crates/cli/tests/expects",
            "expects.roc",
            vec![],
            &[],
            vec![],
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
            vec![],
            &[],
            vec![],
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
    fn platform_requires_pkg() {
        test_roc_app_slim(
            "crates/cli/tests/platform_requires_pkg",
            "app.roc",
            "from app from package",
            UseValgrind::No,
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

    #[ignore = "TODO move this to roc-lang/examples repository"]
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
        Run::new_roc()
            .add_args([
                CMD_CHECK,
                file_path_from_root("examples/virtual-dom-wip", "example-server.roc")
                    .to_str()
                    .unwrap(),
            ])
            .run()
            .assert_clean_success();
    }

    // TODO: write a new test once mono bugs are resolved in investigation
    #[test]
    #[cfg(not(debug_assertions))] // https://github.com/roc-lang/roc/issues/4806
    fn check_virtual_dom_client() {
        Run::new_roc()
            .add_args([
                CMD_CHECK,
                file_path_from_root("examples/virtual-dom-wip", "example-client.roc")
                    .to_str()
                    .unwrap(),
            ])
            .run()
            .assert_clean_success();
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    #[serial(cli_platform)]
    fn cli_countdown_check() {
        Run::new_roc()
            .add_args([
                CMD_CHECK,
                file_path_from_root("crates/cli/tests/cli", "countdown.roc")
                    .to_str()
                    .unwrap(),
            ])
            .run()
            .assert_clean_success();
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    #[serial(cli_platform)]
    fn cli_echo_check() {
        Run::new_roc()
            .add_args([
                CMD_CHECK,
                file_path_from_root("crates/cli/tests/cli", "echo.roc")
                    .to_str()
                    .unwrap(),
            ])
            .run()
            .assert_clean_success();
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    #[serial(cli_platform)]
    fn cli_file_check() {
        Run::new_roc()
            .add_args([
                CMD_CHECK,
                file_path_from_root("crates/cli/tests/cli", "fileBROKEN.roc")
                    .as_os_str()
                    .to_str()
                    .unwrap(),
            ])
            .run()
            .assert_clean_success();
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    #[serial(cli_platform)]
    fn cli_form_check() {
        Run::new_roc()
            .add_args([
                CMD_CHECK,
                file_path_from_root("crates/cli/tests/cli", "form.roc")
                    .as_os_str()
                    .to_str()
                    .unwrap(),
            ])
            .run()
            .assert_clean_success();
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    #[serial(cli_platform)]
    fn cli_http_get_check() {
        Run::new_roc()
            .add_args([
                CMD_CHECK,
                file_path_from_root("crates/cli/tests/cli", "http-get.roc")
                    .as_os_str()
                    .to_str()
                    .unwrap(),
            ])
            .run()
            .assert_clean_success();
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn interactive_effects() {
        test_roc_app(
            "examples/cli",
            "effects.roc",
            vec!["hi there!"],
            &[],
            vec![],
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
            vec!["foo\n"], // NOTE: adding more lines leads to memory leaks
            &[],
            vec![],
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
            vec![],
            &["examples/sqrt.false"],
            vec![],
            "1414",
            UseValgrind::Yes,
            TestCliCommands::Many,
        )
    }

    #[ignore = "TODO move this to roc-lang/examples repository"]
    #[test]
    #[serial(cli_platform)]
    #[cfg_attr(windows, ignore)]
    fn with_env_vars() {
        test_roc_app(
            "crates/cli/tests/cli",
            "env.roc",
            vec![],
            &[],
            vec![
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
            vec![],
            &[],
            vec![],
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
            vec![],
            &[],
            vec![],
            "For multiple tasks: {a: 123, b: \"abc\", c: [123]}\n",
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
            vec![],
            &[],
            vec![],
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
            vec![],
            &[],
            vec![],
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
            vec![],
            &[],
            vec![],
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
        use crate::cli_run::test_roc_app;

        #[allow(unused_imports)]
        use super::{TestCliCommands, UseValgrind};
        use cli_utils::helpers::{file_path_from_root, Run};
        use roc_cli::CMD_BUILD;

        #[allow(unused_imports)]
        use super::{check_output_with_stdin, OPTIMIZE_FLAG};

        #[allow(unused_imports)]
        use std::{path::Path, sync::Once};

        fn test_benchmark(
            roc_filename: &str,
            stdin: Vec<&'static str>,
            expected_ending: &str,
            use_valgrind: UseValgrind,
        ) {
            let dir_name = "crates/cli/tests/benchmarks";

            // Build the platform host once, and use it for all benchmark tests
            BENCHMARKS_BUILD_PLATFORM.call_once(|| {
                Run::new_roc()
                    .arg(CMD_BUILD)
                    .arg(file_path_from_root(dir_name, roc_filename).as_path())
                    .run();
            });

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
            {
                test_roc_app(
                    dir_name,
                    roc_filename,
                    stdin,
                    &[],
                    vec![],
                    expected_ending,
                    use_valgrind,
                    TestCliCommands::Run,
                )
            }

            // TODO RESTORE
            // #[cfg(feature = "wasm32-cli-run")]
            // check_output_wasm(&file_name, stdin, expected_ending);

            // TODO RESTORE
            // #[cfg(feature = "i386-cli-run")]
            // check_output_i386(&file_name, stdin, expected_ending, _use_valgrind);
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

            // now we can pass the `PREBUILT_PLATFORM` flag, because the
            // `call_once` will have built the platform

            if !ran_without_optimizations {
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
            }

            check_output_with_stdin(
                file_name,
                stdin,
                &[OPTIMIZE_FLAG],
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
            test_benchmark("nQueens.roc", vec!["6"], "4\n", UseValgrind::Yes)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn cfold() {
            test_benchmark("cFold.roc", vec!["3"], "11 & 11\n", UseValgrind::Yes)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn deriv() {
            test_benchmark(
                "deriv.roc",
                vec!["2"],
                "1 count: 6\n2 count: 22\n",
                UseValgrind::Yes,
            )
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn rbtree_ck() {
            test_benchmark("rBTreeCk.roc", vec!["100"], "10\n", UseValgrind::Yes)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn rbtree_insert() {
            test_benchmark(
                "rBTreeInsert.roc",
                vec![],
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
            test_benchmark("testAStar.roc", vec![], "True\n", UseValgrind::No)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn base64() {
            test_benchmark(
                "testBase64.roc",
                vec![],
                "encoded: SGVsbG8gV29ybGQ=\ndecoded: Hello World\n",
                UseValgrind::Yes,
            )
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn closure() {
            test_benchmark("closure.roc", vec![], "", UseValgrind::No)
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn issue2279() {
            test_benchmark("issue2279.roc", vec![], "Hello, world!\n", UseValgrind::Yes)
        }

        #[test]
        fn quicksort_app() {
            test_benchmark(
                "quicksortApp.roc",
                vec![],
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
            file_path_from_root("crates/cli/tests/fixtures/multi-dep-str", "Main.roc").as_path(),
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
            file_path_from_root("crates/cli/tests/fixtures/multi-dep-str", "Main.roc").as_path(),
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
            file_path_from_root("crates/cli/tests/fixtures/multi-dep-thunk", "Main.roc").as_path(),
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
            file_path_from_root("crates/cli/tests/fixtures/multi-dep-thunk", "Main.roc").as_path(),
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
            file_path_from_root("crates/cli/tests/fixtures/packages", "app.roc").as_path(),
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
            file_path_from_root("crates/cli/tests/fixtures/packages", "app.roc").as_path(),
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
            file_path_from_root(
                "crates/cli/tests/fixtures/transitive-deps",
                "direct-one.roc",
            )
            .as_path(),
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
            file_path_from_root(
                "crates/cli/tests/fixtures/transitive-deps",
                "direct-one-and-two.roc",
            )
            .as_path(),
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
        get_output_with_stdin(
            file_path_from_root(
                "crates/cli/tests/fixtures/transitive-deps",
                "direct-zero.roc",
            )
            .as_path(),
            vec![],
            &[],
            &[],
            &[],
            UseValgrind::Yes,
            TestCliCommands::Run,
        )
        .iter()
        .for_each(|(cli_mode, out)| {
            out.assert_clean_success();
            match cli_mode {
                CliMode::Roc | CliMode::RocRun | CliMode::RocDev => {
                    out.assert_stdout_ends_with(
                        "[Zero imports One: [One imports Two: From two]]\n",
                    );
                }
                _ => {}
            }
        });
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

                Tip: Add type annotations to functions or values to help you figure
                this out.

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
        check_format_check_as_expected(
            file_path_from_root("crates/cli/tests/fixtures/format", "Formatted.roc").as_path(),
            true,
        );
    }

    #[test]
    fn format_check_reformatting_needed() {
        check_format_check_as_expected(
            file_path_from_root("crates/cli/tests/fixtures/format", "NotFormatted.roc").as_path(),
            false,
        );
    }

    #[test]
    fn format_check_folders() {
        // This fails, because "NotFormatted.roc" is present in this folder
        check_format_check_as_expected(
            dir_path_from_root("crates/cli/tests/fixtures/format").as_path(),
            false,
        );

        // This doesn't fail, since only "Formatted.roc" and non-roc files are present in this folder
        check_format_check_as_expected(
            dir_path_from_root("crates/cli/tests/fixtures/format/formatted_directory").as_path(),
            true,
        );
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
