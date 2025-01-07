extern crate pretty_assertions;

extern crate bumpalo;
extern crate indoc;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;

#[cfg(test)]
mod cli_tests {
    use cli_test_utils::exec_cli::ExecCli;
    use cli_test_utils::helpers::{dir_from_root, file_from_root};
    use const_format::concatcp;
    use roc_cli::{CMD_BUILD, CMD_CHECK, CMD_FORMAT, CMD_TEST};

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
        #[allow(dead_code)] // We still want to be able to easily switch to No if needed
        No,
    }

    const OPTIMIZE_FLAG: &str = concatcp!("--", roc_cli::FLAG_OPTIMIZE);
    const LEGACY_LINKER_FLAG: &str = concatcp!("--", roc_cli::FLAG_LINKER, "=", "legacy");
    const BUILD_HOST_FLAG: &str = concatcp!("--", roc_cli::FLAG_BUILD_HOST);
    const SUPPRESS_BUILD_HOST_WARNING_FLAG: &str =
        concatcp!("--", roc_cli::FLAG_SUPPRESS_BUILD_HOST_WARNING);
    const CHECK_FLAG: &str = concatcp!("--", roc_cli::FLAG_CHECK);
    #[allow(dead_code)]
    const TARGET_FLAG: &str = concatcp!("--", roc_cli::FLAG_TARGET);

    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    const TEST_LEGACY_LINKER: bool = true;

    // Surgical linker currently only supports linux x86_64,
    // so we're always testing the legacy linker on other targets.
    #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
    const TEST_LEGACY_LINKER: bool = false;

    #[test]
    #[ignore = "Needs investigation, see also github.com/roc-lang/roc/pull/7231"]
    fn platform_switching_rust() {
        // pre-build the platform
        std::process::Command::new("bash")
            .arg(file_from_root(
                "examples/platform-switching/rust-platform",
                "build.sh",
            ))
            .status()
            .unwrap();

        let cli_build = ExecCli::new(
            roc_cli::CMD_DEV,
            file_from_root("examples/platform-switching", "rocLovesRust.roc"),
        );

        let expected_output = "Roc <3 Rust!\n";

        let output = cli_build.run();

        output.assert_clean_stdout(expected_output);
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn platform_switching_zig() {
        copy_zig_glue::initialize_zig_test_platforms();

        let cli_build = ExecCli::new(
            CMD_BUILD,
            file_from_root("examples/platform-switching", "rocLovesZig.roc"),
        )
        .arg(BUILD_HOST_FLAG)
        .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);

        let expected_output = "Roc <3 Zig!\n";

        cli_build.full_check_build_and_run(
            expected_output,
            TEST_LEGACY_LINKER,
            ALLOW_VALGRIND,
            None,
            None,
        );
    }

    #[test]
    fn platform_switching_wasm() {
        copy_zig_glue::initialize_zig_test_platforms();

        // this is a web assembly example, but we don't test with JS at the moment
        // so let's just check it for now
        let cli_check = ExecCli::new(
            CMD_CHECK,
            file_from_root("examples/platform-switching", "rocLovesWebAssembly.roc"),
        );

        let cli_check_out = cli_check.run();
        cli_check_out.assert_clean_success();
    }

    #[test]
    #[cfg_attr(
        windows,
        ignore = "Flaky failure: Roc command failed with status ExitStatus(ExitStatus(3221225477))"
    )]
    fn fibonacci() {
        copy_zig_glue::initialize_zig_test_platforms();

        let cli_build = ExecCli::new(
            CMD_BUILD,
            file_from_root("crates/cli/tests/test-projects/algorithms", "fibonacci.roc"),
        )
        .arg(BUILD_HOST_FLAG)
        .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);

        let expected_output = "55\n";

        cli_build.full_check_build_and_run(
            expected_output,
            TEST_LEGACY_LINKER,
            ALLOW_VALGRIND,
            None,
            None,
        );
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn quicksort() {
        copy_zig_glue::initialize_zig_test_platforms();

        let cli_build = ExecCli::new(
            CMD_BUILD,
            file_from_root("crates/cli/tests/test-projects/algorithms", "quicksort.roc"),
        )
        .arg(BUILD_HOST_FLAG)
        .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);

        let expected_output = "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n";

        cli_build.full_check_build_and_run(
            expected_output,
            TEST_LEGACY_LINKER,
            ALLOW_VALGRIND,
            None,
            None,
        );
    }

    // TODO: write a new test once mono bugs are resolved in investigation
    // Encountering this TODO years later, I presume the new test should test the execution, not just roc check.
    #[test]
    #[cfg(not(debug_assertions))] // https://github.com/roc-lang/roc/issues/4806 - later observation: this issue is closed but the tests still hangs in debug mode
    fn check_virtual_dom_server() {
        let cli_check = ExecCli::new(
            CMD_CHECK,
            file_from_root("examples/virtual-dom-wip", "example-server.roc"),
        );

        let cli_check_out = cli_check.run();
        cli_check_out.assert_clean_success();
    }

    // TODO: write a new test once mono bugs are resolved in investigation
    // Encountering this TODO years later, I presume the new test should test the execution, not just roc check.
    #[test]
    #[cfg(not(debug_assertions))] // https://github.com/roc-lang/roc/issues/4806 - later observation: this issue is closed but the tests still hangs in debug mode
    fn check_virtual_dom_client() {
        let cli_check = ExecCli::new(
            CMD_CHECK,
            file_from_root("examples/virtual-dom-wip", "example-client.roc"),
        );

        let cli_check_out = cli_check.run();
        cli_check_out.assert_clean_success();
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    // tea = The Elm Architecture
    fn terminal_ui_tea() {
        copy_zig_glue::initialize_zig_test_platforms();

        let cli_build = ExecCli::new(
            CMD_BUILD,
            file_from_root("crates/cli/tests/test-projects/tui", "main.roc"),
        )
        .arg(BUILD_HOST_FLAG)
        .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);

        let expected_output = "Hello World!\nHello Worldfoo!\n";

        cli_build.full_check_build_and_run(
            expected_output,
            TEST_LEGACY_LINKER,
            ALLOW_VALGRIND,
            Some("foo\n"),
            None,
        );
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn false_interpreter() {
        let cli_build = ExecCli::new(
            CMD_BUILD,
            file_from_root(
                "crates/cli/tests/test-projects/false-interpreter",
                "main.roc",
            ),
        )
        .arg(BUILD_HOST_FLAG)
        .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);

        let sqrt_false_path_buf = file_from_root(
            "crates/cli/tests/test-projects/false-interpreter/examples",
            "sqrt.false",
        );

        let app_args = [sqrt_false_path_buf.as_path().to_str().unwrap()];

        cli_build.full_check_build_and_run(
            "1414",
            TEST_LEGACY_LINKER,
            ALLOW_VALGRIND,
            None,
            Some(&app_args),
        );
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn transitive_expects() {
        let cli_test = ExecCli::new(
            CMD_TEST,
            file_from_root(
                "crates/cli/tests/test-projects/expects_transitive",
                "main.roc",
            ),
        );

        let cli_test_out = cli_test.run();
        cli_test_out.assert_clean_success();
        cli_test_out.assert_stdout_and_stderr_ends_with(
            "0 failed and 3 passed in <ignored for test> ms.\n",
        );
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn transitive_expects_verbose() {
        let cli_test = ExecCli::new(
            CMD_TEST,
            file_from_root(
                "crates/cli/tests/test-projects/expects_transitive",
                "main.roc",
            ),
        )
        .arg("--verbose");

        let cli_test_out = cli_test.run();
        cli_test_out.assert_clean_success();
        insta::assert_snapshot!(cli_test_out.normalize_stdout_and_stderr());
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn multiple_exposed() {
        copy_zig_glue::initialize_zig_test_platforms();

        let cli_build = ExecCli::new(
            CMD_BUILD,
            file_from_root(
                "crates/cli/tests/test-projects/multiple_exposed",
                "main.roc",
            ),
        )
        .arg(BUILD_HOST_FLAG)
        .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);

        let expected_output = "55\n3628800\n";

        cli_build.full_check_build_and_run(
            expected_output,
            TEST_LEGACY_LINKER,
            ALLOW_VALGRIND,
            None,
            None,
        );
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn test_module_imports_pkg_w_flag() {
        let cli_test = ExecCli::new(
            CMD_TEST,
            file_from_root(
                "crates/cli/tests/test-projects/module_imports_pkg",
                "Module.roc",
            ),
        )
        .add_args(["--main", "tests/test-projects/module_imports_pkg/app.roc"]);

        let cli_test_out = cli_test.run();
        cli_test_out.assert_clean_success();
        cli_test_out.assert_stdout_and_stderr_ends_with(
            "0 failed and 1 passed in <ignored for test> ms.\n",
        );
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn test_module_imports_pkg_no_flag() {
        let cli_test = ExecCli::new(
            CMD_TEST,
            file_from_root(
                "crates/cli/tests/test-projects/module_imports_pkg",
                "Module.roc",
            ),
        );

        let cli_test_out = cli_test.run();
        insta::assert_snapshot!(cli_test_out.normalize_stdout_and_stderr());
    }

    mod no_platform {

        use super::*;

        #[test]
        #[cfg_attr(windows, ignore)]
        fn roc_check_markdown_docs() {
            let cli_build = ExecCli::new(
                CMD_CHECK,
                file_from_root("crates/cli/tests/markdown", "form.md"),
            );

            let expected_out =
                "0 errors and 0 warnings found in <ignored for test> ms.\n\n0 errors and 0 warnings found in <ignored for test> ms.\n\n";

            cli_build.run().assert_clean_stdout(expected_out);
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn import_in_expect() {
            let cli_build = ExecCli::new(
                CMD_TEST,
                file_from_root(
                    "crates/cli/tests/test-projects/module_params",
                    "ImportInExpect.roc",
                ),
            );

            let expected_out = "0 failed and 3 passed in <ignored for test> ms.\n";

            cli_build.run().assert_clean_stdout(expected_out);
        }
    }

    mod test_platform_basic_cli {

        use super::*;
        use roc_cli::CMD_DEV;

        #[test]
        #[cfg_attr(
            windows,
            ignore = "basic-cli platform doesn't have support for Windows"
        )]
        #[cfg_attr(
            unix,
            ignore = "broken when running in nix CI, TODO replace with a zig test platform"
        )]
        fn combine_tasks_with_record_builder() {
            let cli_build = ExecCli::new(
                CMD_BUILD,
                file_from_root(
                    "crates/cli/tests/test-projects/effectful",
                    "combine-tasks.roc",
                ),
            );

            let expected_output = "For multiple tasks: {a: 123, b: \"abc\", c: [123]}\n";

            cli_build.check_build_and_run(expected_output, ALLOW_VALGRIND, None, None);
        }

        #[test]
        #[cfg_attr(
            windows,
            ignore = "basic-cli platform doesn't have support for Windows"
        )]
        #[cfg_attr(
            unix,
            ignore = "broken when running in nix CI, TODO replace with a zig test platform"
        )]
        fn module_params_different_types() {
            let cli_build = ExecCli::new(
                CMD_BUILD,
                file_from_root(
                    "crates/cli/tests/test-projects/module_params",
                    "different_types.roc",
                ),
            );

            let expected_output = "Write something:\n42\n";

            cli_build.check_build_and_run(expected_output, ALLOW_VALGRIND, Some("42\n"), None);
        }

        #[test]
        #[cfg_attr(
            windows,
            ignore = "basic-cli platform doesn't have support for Windows"
        )]
        #[cfg_attr(
            unix,
            ignore = "broken when running in nix CI, TODO replace with a zig test platform"
        )]
        fn module_params_issue_7116() {
            let cli_build = ExecCli::new(
                CMD_DEV,
                file_from_root(
                    "crates/cli/tests/test-projects/module_params",
                    "issue_7116.roc",
                ),
            );

            cli_build.run().assert_zero_exit();
        }

        #[test]
        #[cfg_attr(
            windows,
            ignore = "basic-cli platform doesn't have support for Windows"
        )]
        #[cfg_attr(
            unix,
            ignore = "broken when running in nix CI, TODO replace with a zig test platform"
        )]
        fn module_params_pass_task() {
            let cli_build = ExecCli::new(
                CMD_DEV,
                file_from_root(
                    "crates/cli/tests/test-projects/module_params",
                    "pass_task.roc",
                ),
            );

            let expected_output = "Hi, Agus!\n";

            cli_build.check_build_and_run(expected_output, ALLOW_VALGRIND, None, None);
        }
    }

    mod test_platform_simple_zig {
        use super::*;
        use roc_cli::{CMD_BUILD, CMD_DEV, CMD_TEST};

        static BUILD_PLATFORM_HOST: std::sync::Once = std::sync::Once::new();

        /// Build the platform host once for all tests in this module
        fn build_platform_host() {
            BUILD_PLATFORM_HOST.call_once(|| {
                copy_zig_glue::initialize_zig_test_platforms();

                let cli_build = ExecCli::new(
                    CMD_BUILD,
                    file_from_root(
                        "crates/cli/tests/test-projects/test-platform-simple-zig",
                        "app.roc",
                    ),
                )
                .arg(BUILD_HOST_FLAG)
                .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG)
                .arg(OPTIMIZE_FLAG);

                let cli_build_out = cli_build.run();
                cli_build_out.assert_clean_success();

                if TEST_LEGACY_LINKER {
                    let cli_build_legacy = cli_build.arg(LEGACY_LINKER_FLAG);

                    let cli_build_legacy_out = cli_build_legacy.run();
                    cli_build_legacy_out.assert_clean_success();
                }
            });
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_multi_dep_str() {
            build_platform_host();

            let cli_build_unoptimized = ExecCli::new(
                CMD_BUILD,
                file_from_root(
                    "crates/cli/tests/test-projects/fixtures/multi-dep-str",
                    "main.roc",
                ),
            );

            let expected_output = "I am Dep2.str2\n";

            cli_build_unoptimized.clone().full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                ALLOW_VALGRIND,
                None,
                None,
            );

            let cli_build_optimized = cli_build_unoptimized.arg(OPTIMIZE_FLAG);

            cli_build_optimized.full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                ALLOW_VALGRIND,
                None,
                None,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_multi_dep_thunk() {
            build_platform_host();

            let cli_build_unoptimized = ExecCli::new(
                CMD_BUILD,
                file_from_root(
                    "crates/cli/tests/test-projects/fixtures/multi-dep-thunk",
                    "main.roc",
                ),
            );

            let expected_output = "I am Dep2.value2\n";

            cli_build_unoptimized.clone().full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                ALLOW_VALGRIND,
                None,
                None,
            );

            let cli_build_optimized = cli_build_unoptimized.arg(OPTIMIZE_FLAG);

            cli_build_optimized.full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                ALLOW_VALGRIND,
                None,
                None,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_packages() {
            build_platform_host();

            let cli_build_unoptimized = ExecCli::new(
                CMD_BUILD,
                file_from_root(
                    "crates/cli/tests/test-projects/fixtures/packages",
                    "main.roc",
                ),
            );

            let expected_output =
                "Hello, World! This text came from a package! This text came from a CSV package!\n";

            cli_build_unoptimized.clone().full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                ALLOW_VALGRIND,
                None,
                None,
            );

            let cli_build_optimized = cli_build_unoptimized.arg(OPTIMIZE_FLAG);

            cli_build_optimized.full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                ALLOW_VALGRIND,
                None,
                None,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_transitive_deps_app() {
            build_platform_host();

            let cli_build = ExecCli::new(
                CMD_BUILD,
                file_from_root(
                    "crates/cli/tests/test-projects/fixtures/transitive-deps",
                    "direct-one.roc",
                ),
            );

            let expected_output = "[One imports Two: From two]\n";

            cli_build.full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                ALLOW_VALGRIND,
                None,
                None,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_transitive_and_direct_dep_app() {
            build_platform_host();

            let cli_build = ExecCli::new(
                CMD_BUILD,
                file_from_root(
                    "crates/cli/tests/test-projects/fixtures/transitive-deps",
                    "direct-one-and-two.roc",
                ),
            );

            let expected_output = "[One imports Two: From two] | From two\n";

            cli_build.full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                ALLOW_VALGRIND,
                None,
                None,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_double_transitive_dep_app() {
            build_platform_host();

            let cli_build = ExecCli::new(
                CMD_BUILD,
                file_from_root(
                    "crates/cli/tests/test-projects/fixtures/transitive-deps",
                    "direct-zero.roc",
                ),
            );

            let expected_output = "[Zero imports One: [One imports Two: From two]]\n";

            cli_build.full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                ALLOW_VALGRIND,
                None,
                None,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn module_params() {
            build_platform_host();

            let cli_dev = ExecCli::new(
                CMD_DEV,
                file_from_root("crates/cli/tests/test-projects/module_params", "app.roc"),
            );

            let cli_dev_out = cli_dev.run();
            cli_dev_out.assert_clean_success();

            insta::assert_snapshot!(cli_dev_out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn module_params_arity_mismatch() {
            build_platform_host();

            let cli_dev = ExecCli::new(
                CMD_DEV,
                file_from_root(
                    "crates/cli/tests/test-projects/module_params",
                    "arity_mismatch.roc",
                ),
            );

            let cli_dev_out = cli_dev.run();
            cli_dev_out.assert_nonzero_exit();

            insta::assert_snapshot!(cli_dev_out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn module_params_bad_ann() {
            build_platform_host();

            let cli_dev = ExecCli::new(
                CMD_DEV,
                file_from_root(
                    "crates/cli/tests/test-projects/module_params",
                    "bad_ann.roc",
                ),
            );

            let cli_dev_out = cli_dev.run();
            cli_dev_out.assert_nonzero_exit();

            insta::assert_snapshot!(cli_dev_out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn module_params_multiline_pattern() {
            build_platform_host();

            let cli_dev = ExecCli::new(
                CMD_DEV,
                file_from_root(
                    "crates/cli/tests/test-projects/module_params",
                    "multiline_params.roc",
                ),
            );

            let cli_dev_out = cli_dev.run();
            cli_dev_out.assert_clean_success();
            cli_dev_out.assert_stdout_and_stderr_ends_with("hi\n");
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn module_params_unexpected_fn() {
            build_platform_host();

            let cli_dev = ExecCli::new(
                CMD_DEV,
                file_from_root(
                    "crates/cli/tests/test-projects/module_params",
                    "unexpected_fn.roc",
                ),
            );

            let cli_dev_out = cli_dev.run();
            cli_dev_out.assert_nonzero_exit();

            insta::assert_snapshot!(cli_dev_out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(
            windows,
            ignore = "running `expect`s does not currently work on windows"
        )]
        #[cfg_attr(
            unix,
            ignore = "https://roc.zulipchat.com/#narrow/channel/304641-ideas/topic/roc.20dev.20expect.20failure/near/477682063"
        )]
        fn expects_dev_failure() {
            build_platform_host();

            let cli_dev = ExecCli::new(
                CMD_DEV,
                file_from_root("crates/cli/tests/test-projects/expects", "expects.roc"),
            );

            let cli_dev_out = cli_dev.run();
            // TODO enable or delete this based on https://roc.zulipchat.com/#narrow/channel/304641-ideas/topic/roc.20dev.20expect.20failure/near/477682063
            //cli_dev_out.assert_nonzero_exit();

            insta::assert_snapshot!(cli_dev_out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore = "running tests does not work on windows right now")]
        fn expects_test_failure() {
            build_platform_host();

            let cli_test = ExecCli::new(
                CMD_TEST,
                file_from_root("crates/cli/tests/test-projects/expects", "expects.roc"),
            );

            let cli_test_out = cli_test.run();
            cli_test_out.assert_nonzero_exit();

            insta::assert_snapshot!(cli_test_out.normalize_stdout_and_stderr());
        }
    }

    mod test_platform_effects_zig {
        use super::*;
        use cli_test_utils::helpers::file_from_root;

        static BUILD_PLATFORM_HOST: std::sync::Once = std::sync::Once::new();

        /// Build the platform host once for all tests in this module
        fn build_platform_host() {
            BUILD_PLATFORM_HOST.call_once(|| {
                copy_zig_glue::initialize_zig_test_platforms();

                let cli_build = ExecCli::new(
                    CMD_BUILD,
                    file_from_root(
                        "crates/cli/tests/test-projects/test-platform-effects-zig/",
                        "app-stub.roc",
                    ),
                )
                .arg(BUILD_HOST_FLAG)
                .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG)
                .arg(OPTIMIZE_FLAG);

                let cli_build_out = cli_build.run();
                cli_build_out.assert_clean_success();

                if TEST_LEGACY_LINKER {
                    let cli_build_legacy = cli_build.arg(LEGACY_LINKER_FLAG);

                    let cli_build_legacy_out = cli_build_legacy.run();
                    cli_build_legacy_out.assert_clean_success();
                }
            });
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn interactive_effects() {
            build_platform_host();

            let cli_build = ExecCli::new(
                CMD_BUILD,
                file_from_root("crates/cli/tests/test-projects/effectful", "print-line.roc"),
            );

            let expected_output =
                "Welcome!\nWhat's your name?\nYou entered: hi there!\nIt is known\n";

            cli_build.full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                ALLOW_VALGRIND,
                Some("hi there!"),
                None,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn inspect_logging() {
            build_platform_host();

            let cli_build = ExecCli::new(
                CMD_BUILD,
                file_from_root(
                    "crates/cli/tests/test-projects/effectful",
                    "inspect-logging.roc",
                ),
            );

            let expected_output = "(@Community {friends: [{2}, {2}, {0, 1}], people: [(@Person {age: 27, favoriteColor: Blue, firstName: \"John\", hasBeard: Bool.true, lastName: \"Smith\"}), (@Person {age: 47, favoriteColor: Green, firstName: \"Debby\", hasBeard: Bool.false, lastName: \"Johnson\"}), (@Person {age: 33, favoriteColor: (RGB (255, 255, 0)), firstName: \"Jane\", hasBeard: Bool.false, lastName: \"Doe\"})]})\n";

            cli_build.full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                ALLOW_VALGRIND,
                None,
                None,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn effectful_form() {
            build_platform_host();

            let cli_build = ExecCli::new(
                roc_cli::CMD_BUILD,
                file_from_root("crates/cli/tests/test-projects/effectful", "form.roc"),
            );

            let expected_output = "What's your first name?\nWhat's your last name?\n\nHi, Agus Zubiaga!\n\nHow old are you?\n\nNice! You can vote!\n\nBye! 👋\n";

            cli_build.full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                ALLOW_VALGRIND,
                Some("Agus\nZubiaga\n27\n"),
                None,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn effectful_hello() {
            build_platform_host();

            let cli_dev = ExecCli::new(
                roc_cli::CMD_DEV,
                file_from_root("crates/cli/tests/test-projects/effectful/", "hello.roc"),
            );

            let expected_out = "I'm an effect 👻\n";

            cli_dev.run().assert_clean_stdout(expected_out);
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn effectful_loops() {
            build_platform_host();

            let cli_dev = ExecCli::new(
                roc_cli::CMD_DEV,
                file_from_root("crates/cli/tests/test-projects/effectful/", "loops.roc"),
            );

            let expected_out = "Lu\nMarce\nJoaquin\nChloé\nMati\nPedro\n";

            cli_dev.run().assert_clean_stdout(expected_out);
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn effectful_untyped_passed_fx() {
            build_platform_host();

            let cli_dev = ExecCli::new(
                roc_cli::CMD_DEV,
                file_from_root(
                    "crates/cli/tests/test-projects/effectful/",
                    "untyped_passed_fx.roc",
                ),
            );

            let expected_out = "Before hello\nHello, World!\nAfter hello\n";

            cli_dev.run().assert_clean_stdout(expected_out);
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn effectful_ignore_result() {
            build_platform_host();

            let cli_dev = ExecCli::new(
                roc_cli::CMD_DEV,
                file_from_root(
                    "crates/cli/tests/test-projects/effectful/",
                    "ignore_result.roc",
                ),
            );

            let expected_out = "I asked for input and I ignored it. Deal with it! 😎\n";

            cli_dev.run().assert_clean_stdout(expected_out);
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn effectful_suffixed_record_field() {
            build_platform_host();

            let cli_build = ExecCli::new(
                roc_cli::CMD_BUILD,
                file_from_root(
                    "crates/cli/tests/test-projects/effectful",
                    "suffixed_record_field.roc",
                ),
            );

            let expected_output = "notEffectful: hardcoded\neffectful: from stdin\n";

            cli_build.check_build_and_run(
                expected_output,
                ALLOW_VALGRIND,
                Some("from stdin"),
                None,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn effectful_on_err() {
            build_platform_host();

            let cli_build = ExecCli::new(
                roc_cli::CMD_BUILD,
                file_from_root("crates/cli/tests/test-projects/effectful", "on_err.roc"),
            );

            let expected_output = "Enter your password:\nLOG: Failed login attempt\n";

            cli_build.check_build_and_run(expected_output, ALLOW_VALGRIND, Some("42"), None);
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn effectful_for_each_try() {
            build_platform_host();

            let cli_build = ExecCli::new(
                roc_cli::CMD_BUILD,
                file_from_root(
                    "crates/cli/tests/test-projects/effectful",
                    "for_each_try.roc",
                ),
            );

            let expected_output = "✅ 0\n✅ 2\n✅ 4\n✅ 6\n✅ 8\n9 is not even! ABORT!\n";

            cli_build.check_build_and_run(expected_output, ALLOW_VALGRIND, None, None);
        }
    }

    // this is for testing the benchmarks (on small inputs), to perform proper benchmarks see crates/cli/benches/README.md
    mod test_benchmarks {
        use super::{
            UseValgrind, ALLOW_VALGRIND, BUILD_HOST_FLAG, OPTIMIZE_FLAG,
            SUPPRESS_BUILD_HOST_WARNING_FLAG, TEST_LEGACY_LINKER,
        };
        use cli_test_utils::{exec_cli::ExecCli, helpers::file_from_root};
        use indoc::indoc;
        use roc_cli::CMD_BUILD;

        // #[allow(unused_imports)]
        use std::sync::Once;

        static BUILD_PLATFORM_HOST: Once = Once::new();

        /// Build the platform host once for all tests in this module
        fn build_platform_host() {
            BUILD_PLATFORM_HOST.call_once(|| {
                copy_zig_glue::initialize_zig_test_platforms();

                let cli_build = ExecCli::new(
                    CMD_BUILD,
                    file_from_root("crates/cli/tests/benchmarks/platform", "app.roc"),
                )
                .arg(BUILD_HOST_FLAG)
                .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG)
                .arg(OPTIMIZE_FLAG);

                let cli_build_out = cli_build.run();
                cli_build_out.assert_clean_success();

                if TEST_LEGACY_LINKER {
                    let cli_build_legacy = cli_build.arg(super::LEGACY_LINKER_FLAG);

                    let cli_build_legacy_out = cli_build_legacy.run();
                    cli_build_legacy_out.assert_clean_success();
                }
            });
        }

        fn test_benchmark(
            roc_filename: &'static str,
            expected_output: &'static str,
            stdin: Option<&'static str>,
            use_valgrind: UseValgrind,
        ) {
            let dir_name = "crates/cli/tests/benchmarks";
            let roc_file_path = file_from_root(dir_name, roc_filename);

            build_platform_host();

            #[cfg(all(not(feature = "wasm32-cli-run"), not(feature = "i386-cli-run")))]
            {
                let cli_build = ExecCli::new(CMD_BUILD, roc_file_path);

                let with_valgrind = matches!(use_valgrind, UseValgrind::Yes) && ALLOW_VALGRIND;
                cli_build.full_check_build_and_run(
                    expected_output,
                    TEST_LEGACY_LINKER,
                    with_valgrind,
                    stdin,
                    None,
                );
            }

            #[cfg(feature = "wasm32-cli-run")]
            run_wasm_check_output(roc_file_path.as_path(), expected_output, stdin);

            #[cfg(feature = "i386-cli-run")]
            check_output_i386(roc_file_path.as_path(), expected_output, stdin);
        }

        #[cfg(feature = "wasm32-cli-run")]
        fn run_wasm_check_output(
            roc_file_path: &std::path::Path,
            expected_output: &'static str,
            stdin_opt: Option<&'static str>,
        ) {
            // Check with and without optimizations
            run_wasm_check_output_with_flags(
                roc_file_path,
                expected_output,
                stdin_opt.clone(),
                &[],
            );

            run_wasm_check_output_with_flags(
                roc_file_path,
                expected_output,
                stdin_opt,
                &[OPTIMIZE_FLAG],
            );
        }

        #[cfg(feature = "wasm32-cli-run")]
        fn run_wasm_check_output_with_flags(
            roc_file_path: &std::path::Path,
            expected_output: &'static str,
            stdin_opt: Option<&'static str>,
            flags: &[&str],
        ) {
            use super::{concatcp, TARGET_FLAG};

            let mut flags = flags.to_vec();
            flags.push(concatcp!(TARGET_FLAG, "=wasm32"));

            let cli_build = ExecCli::new(CMD_BUILD, roc_file_path.to_path_buf()).add_args(flags);

            let cli_build_out = cli_build.run();
            cli_build_out.assert_clean_success();

            // wasm can't take stdin, so we pass it as an arg
            let wasm_args = if let Some(stdin) = stdin_opt {
                vec![stdin]
            } else {
                vec![]
            };

            let wasm_run_out = crate::run_wasm_for_cli_test(
                &roc_file_path.with_extension("wasm"),
                wasm_args.clone(),
            );

            assert_eq!(wasm_run_out, expected_output);

            if TEST_LEGACY_LINKER {
                let cli_build_legacy = cli_build.arg(super::LEGACY_LINKER_FLAG);

                let cli_build_legacy_out = cli_build_legacy.run();
                cli_build_legacy_out.assert_clean_success();

                let wasm_run_out_legacy =
                    crate::run_wasm_for_cli_test(&roc_file_path.with_extension("wasm"), wasm_args);

                assert_eq!(wasm_run_out_legacy, expected_output);
            }
        }

        #[cfg(feature = "i386-cli-run")]
        fn check_output_i386(
            roc_file_path: &std::path::Path,
            expected_output: &'static str,
            stdin_opt: Option<&'static str>,
        ) {
            use super::{concatcp, TARGET_FLAG};

            let i386_target_arg = concatcp!(TARGET_FLAG, "=x86_32");

            let cli_build =
                ExecCli::new(CMD_BUILD, roc_file_path.to_path_buf()).arg(i386_target_arg);

            cli_build.clone().full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                false,
                stdin_opt,
                None,
            );

            // also test optimized build
            let cli_build_optimized = cli_build.arg(OPTIMIZE_FLAG);

            cli_build_optimized.full_check_build_and_run(
                expected_output,
                TEST_LEGACY_LINKER,
                false,
                stdin_opt,
                None,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn nqueens() {
            let expected_output = indoc! {"
                Please enter an integer
                4
            "};
            test_benchmark("nQueens.roc", expected_output, Some("6"), UseValgrind::Yes);
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn cfold() {
            let expected_output = indoc! {"
                Please enter an integer
                11 & 11
            "};
            test_benchmark("cFold.roc", expected_output, Some("3"), UseValgrind::Yes);
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn deriv() {
            let expected_output = indoc! {"
                Please enter an integer
                1 count: 6
                2 count: 22
            "};
            test_benchmark("deriv.roc", expected_output, Some("2"), UseValgrind::Yes);
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn rbtree_ck() {
            let expected_output = indoc! {"
                Please enter an integer
                10
            "};
            test_benchmark(
                "rBTreeCk.roc",
                expected_output,
                Some("100"),
                UseValgrind::Yes,
            );
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn rbtree_insert() {
            let expected_output = "Node Black 0 {} Empty Empty\n";
            test_benchmark("rBTreeInsert.roc", expected_output, None, UseValgrind::Yes);
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn astar() {
            if cfg!(feature = "wasm32-cli-run") {
                eprintln!("WARNING: skipping testing benchmark testAStar.roc because it currently does not work on wasm32 due to dictionaries.");
            } else {
                let expected_output = "True\n";
                test_benchmark("testAStar.roc", expected_output, None, UseValgrind::Yes);
            }
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn base64() {
            let expected_output = indoc! {"
                encoded: SGVsbG8gV29ybGQ=
                decoded: Hello World
            "};
            test_benchmark("testBase64.roc", expected_output, None, UseValgrind::Yes);
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn closure() {
            let expected_output = "";
            test_benchmark("closure.roc", expected_output, None, UseValgrind::Yes);
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn issue2279() {
            let expected_output = "Hello, world!\n";
            test_benchmark("issue2279.roc", expected_output, None, UseValgrind::Yes);
        }

        #[test]
        #[cfg_attr(windows, ignore = "Command failed Exit Code: exit code: 0xc0000005")]
        fn quicksort_app() {
            let expected_output = "Please enter an integer\n[0, 1, 1, 2, 2, 2, 3, 3, 3, 3, 3, 4, 4, 4, 4, 5, 6, 6, 8, 9]\n";
            test_benchmark(
                "quicksortApp.roc",
                expected_output,
                Some("0"),
                UseValgrind::Yes,
            );
        }
    }

    #[test]
    #[ignore = "flaky currently due to 7022"]
    fn known_type_error() {
        let cli_check = ExecCli::new(
            CMD_CHECK,
            file_from_root("crates/cli/tests/test-projects/known_bad", "TypeError.roc"),
        );

        let cli_check_out = cli_check.run();
        cli_check_out.assert_nonzero_exit();

        insta::assert_snapshot!(cli_check_out.normalize_stdout_and_stderr());
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    fn test_module_imports_unknown_pkg() {
        let cli_test = ExecCli::new(
            CMD_TEST,
            file_from_root(
                "crates/cli/tests/test-projects/module_imports_pkg",
                "ImportsUnknownPkg.roc",
            ),
        )
        .add_args(["--main", "tests/test-projects/module_imports_pkg/app.roc"]);

        let cli_test_out = cli_test.run();
        cli_test_out.assert_nonzero_exit();

        insta::assert_snapshot!(cli_test_out.normalize_stdout_and_stderr());
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    /// this tests that a platform can correctly import a package
    fn platform_requires_pkg() {
        copy_zig_glue::initialize_zig_test_platforms();

        let cli_build = ExecCli::new(
            CMD_BUILD,
            file_from_root(
                "crates/cli/tests/test-projects/platform_requires_pkg",
                "app.roc",
            ),
        )
        .arg(BUILD_HOST_FLAG)
        .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);

        let expected_output = "from app from package";

        cli_build.full_check_build_and_run(
            expected_output,
            TEST_LEGACY_LINKER,
            ALLOW_VALGRIND,
            None,
            None,
        );
    }

    #[test]
    fn known_type_error_with_long_path() {
        let cli_check = ExecCli::new(
            CMD_CHECK,
            file_from_root(
                "crates/cli/tests/test-projects/known_bad",
                "UnusedImportButWithALongFileNameForTesting.roc",
            ),
        );

        let cli_check_out = cli_check.run();
        cli_check_out.assert_nonzero_exit();

        insta::assert_snapshot!(cli_check_out.normalize_stdout_and_stderr());
    }

    #[test]
    fn exposed_not_defined() {
        let cli_check = ExecCli::new(
            CMD_CHECK,
            file_from_root(
                "crates/cli/tests/test-projects/known_bad",
                "ExposedNotDefined.roc",
            ),
        );

        let cli_check_out = cli_check.run();
        cli_check_out.assert_nonzero_exit();

        insta::assert_snapshot!(cli_check_out.normalize_stdout_and_stderr());
    }

    #[test]
    fn unused_import() {
        let cli_check = ExecCli::new(
            CMD_CHECK,
            file_from_root(
                "crates/cli/tests/test-projects/known_bad",
                "UnusedImport.roc",
            ),
        );

        let cli_check_out = cli_check.run();
        cli_check_out.assert_nonzero_exit();

        insta::assert_snapshot!(cli_check_out.normalize_stdout_and_stderr());
    }

    #[test]
    fn format_check_good() {
        ExecCli::new(
            CMD_FORMAT,
            file_from_root(
                "crates/cli/tests/test-projects/fixtures/format",
                "formatted.roc",
            ),
        )
        .arg(CHECK_FLAG)
        .run()
        .assert_clean_success();
    }

    #[test]
    fn format_check_reformatting_needed() {
        ExecCli::new(
            CMD_FORMAT,
            file_from_root(
                "crates/cli/tests/test-projects/fixtures/format",
                "not-formatted.roc",
            ),
        )
        .arg(CHECK_FLAG)
        .run()
        .assert_nonzero_exit();
    }

    #[test]
    fn format_check_folders() {
        // This fails, because "not-formatted.roc" is present in this folder
        ExecCli::new(
            CMD_FORMAT,
            dir_from_root("crates/cli/tests/test-projects/fixtures/format"),
        )
        .arg(CHECK_FLAG)
        .run()
        .assert_nonzero_exit();

        // This doesn't fail, since only "formatted.roc" and non-roc files are present in this folder
        ExecCli::new(
            CMD_FORMAT,
            dir_from_root("crates/cli/tests/test-projects/fixtures/format/formatted_directory"),
        )
        .run()
        .assert_clean_success();
    }

    #[test]
    fn module_params_effectful_param() {
        let cli_check = ExecCli::new(
            CMD_CHECK,
            file_from_root(
                "crates/cli/tests/test-projects/module_params",
                "effect_module.roc",
            ),
        );

        let cli_check_out = cli_check.run();
        cli_check_out.assert_clean_success();
    }
}

#[cfg(feature = "wasm32-cli-run")]
fn run_wasm_for_cli_test(wasm_path: &std::path::Path, stdin: Vec<&str>) -> String {
    use bumpalo::Bump;
    use roc_wasm_interp::{DefaultImportDispatcher, Instance, Value, WasiFile};

    let wasm_bytes = std::fs::read(wasm_path).unwrap();
    let arena = Bump::new();

    let mut instance = {
        let mut fake_stdin = vec![];
        let fake_stdout = vec![];
        let fake_stderr = vec![];

        let stdin_is_empty = stdin.is_empty();
        for s in stdin {
            fake_stdin.extend_from_slice(s.as_bytes())
        }

        if !stdin_is_empty {
            // add newline to finish input
            fake_stdin.extend_from_slice("\n".as_bytes());
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
