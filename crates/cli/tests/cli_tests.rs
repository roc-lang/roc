extern crate pretty_assertions;

extern crate bumpalo;
extern crate indoc;
extern crate roc_collections;
extern crate roc_load;
extern crate roc_module;

#[cfg(test)]
mod cli_tests {
    use cli_test_utils::helpers::{dir_from_root, file_from_root};
    use cli_test_utils::exec_cli::ExecCli;
    use const_format::concatcp;
    use roc_cli::{CMD_BUILD, CMD_CHECK, CMD_FORMAT, CMD_RUN, CMD_TEST};

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

    const OPTIMIZE_FLAG: &str = concatcp!("--", roc_cli::FLAG_OPTIMIZE);
    const LINKER_FLAG: &str = concatcp!("--", roc_cli::FLAG_LINKER, "=", "legacy");
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
    #[cfg_attr(windows, ignore)]
    fn platform_switching_rust() {
        let cli_build = ExecCli::new(
                                CMD_BUILD,
                                file_from_root("examples/platform-switching", "rocLovesRust.roc")
            )
            .arg(BUILD_HOST_FLAG)
            .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);
        
        let expected_output = "Roc <3 Rust!\n";
        
        cli_build.full_check_build_and_run(expected_output, TEST_LEGACY_LINKER, ALLOW_VALGRIND, None, None);
    }
    
    #[test]
    #[cfg_attr(windows, ignore)]
    fn platform_switching_zig() {
        let cli_build = ExecCli::new(
                                CMD_BUILD,
                                file_from_root("examples/platform-switching", "rocLovesZig.roc")
            )
            .arg(BUILD_HOST_FLAG)
            .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);
        
        let expected_output = "Roc <3 Zig!\n";
        
        cli_build.full_check_build_and_run(expected_output, TEST_LEGACY_LINKER, ALLOW_VALGRIND, None, None);
    }
    
    #[test]
    fn platform_switching_wasm() {
        // this is a web assembly example, but we don't test with JS at the moment
        // so let's just check it for now
        let cli_check = ExecCli::new(
                                CMD_CHECK,
                                file_from_root("examples/platform-switching", "rocLovesWebAssembly.roc")
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
        let cli_build = ExecCli::new(
                                CMD_BUILD,
                                file_from_root("crates/cli/tests/test-projects/algorithms", "fibonacci.roc")
            )
            .arg(BUILD_HOST_FLAG)
            .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);
        
        let expected_output = "55\n";
        
        cli_build.full_check_build_and_run(expected_output, TEST_LEGACY_LINKER, ALLOW_VALGRIND, None, None);
    }
    
    #[test]
    #[cfg_attr(windows, ignore)]
    fn quicksort() {
        let cli_build = ExecCli::new(
                                CMD_BUILD,
                                file_from_root("crates/cli/tests/test-projects/algorithms", "quicksort.roc")
            )
            .arg(BUILD_HOST_FLAG)
            .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);
        
        let expected_output = "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n";
        
        cli_build.full_check_build_and_run(expected_output, TEST_LEGACY_LINKER, ALLOW_VALGRIND, None, None);
    }
    
    // TODO: write a new test once mono bugs are resolved in investigation
    // Encountering this TODO years later, I presume the new test should test the execution, not just roc check.
    #[test]
    #[cfg(not(debug_assertions))] // https://github.com/roc-lang/roc/issues/4806 - later observation: this issue is closed but the tests still hangs in debug mode
    fn check_virtual_dom_server() {        
        let cli_check = ExecCli::new(
                                CMD_CHECK,
                                file_from_root("examples/virtual-dom-wip", "example-server.roc")
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
                                file_from_root("examples/virtual-dom-wip", "example-client.roc")
            );

        let cli_check_out = cli_check.run();
        cli_check_out.assert_clean_success();
    }
    
    #[test]
    #[cfg_attr(windows, ignore)]
    // tea = The Elm Architecture
    fn terminal_ui_tea() {
        let cli_build = ExecCli::new(
                                CMD_BUILD,
                                file_from_root("crates/cli/tests/test-projects/tui", "main.roc")
            )
            .arg(BUILD_HOST_FLAG)
            .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);
        
        let expected_output = "Hello World!\nHello Worldfoo!\n";
        
        cli_build.full_check_build_and_run(expected_output, TEST_LEGACY_LINKER, ALLOW_VALGRIND, Some("foo\n"), None);
    }
    
    
    // TODO check this out, there's more that's going wrong than a segfault
    #[test]
    /*#[cfg_attr(
        any(target_os = "windows", target_os = "linux", target_os = "macos"),
        ignore = "Segfault, likely broken because of alias analysis: https://github.com/roc-lang/roc/issues/6544"
    )]*/
    fn false_interpreter() {
        let cli_build = ExecCli::new(
                                CMD_BUILD,
                                file_from_root("crates/cli/tests/test-projects/false-interpreter", "main.roc")
            )
            .arg(BUILD_HOST_FLAG)
            .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);
        
        let sqrt_false_path_buf = file_from_root("crates/cli/tests/test-projects/false-interpreter/examples", "sqrt.false");
        
        let app_args = ["--",
                        sqrt_false_path_buf
                            .as_path()
                            .to_str()
                            .unwrap()];
        
        cli_build.full_check_build_and_run("1414", TEST_LEGACY_LINKER, ALLOW_VALGRIND, None, Some(&app_args));
    }
    
    #[test]
    #[cfg_attr(windows, ignore)]
    fn transitive_expects() {
        let cli_test = ExecCli::new(
                                CMD_TEST,
                                file_from_root("crates/cli/tests/test-projects/expects_transitive", "main.roc")
            );

        let cli_test_out = cli_test.run();
        cli_test_out.assert_clean_success();
        cli_test_out.assert_stdout_and_stderr_ends_with("0 failed and 3 passed in <ignored for test> ms.\n");
    }
    
    #[test]
    #[cfg_attr(windows, ignore)]
    fn transitive_expects_verbose() {
        let cli_test = ExecCli::new(
                                CMD_TEST,
                                file_from_root("crates/cli/tests/test-projects/expects_transitive", "main.roc")
            ).arg("--verbose");

        let cli_test_out = cli_test.run();
        cli_test_out.assert_clean_success();
        insta::assert_snapshot!(cli_test_out.normalize_stdout_and_stderr());
    }
    
    #[test]
    #[cfg_attr(windows, ignore)]
    fn multiple_exposed() {
        let cli_build = ExecCli::new(
                                CMD_BUILD,
                                file_from_root("crates/cli/tests/test-projects/multiple_exposed", "main.roc")
            )
            .arg(BUILD_HOST_FLAG)
            .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG);
        
        let expected_output = "55\n3628800\n";
        
        cli_build.full_check_build_and_run(expected_output, TEST_LEGACY_LINKER, ALLOW_VALGRIND, None, None);
    }
    
    #[test]
    #[cfg_attr(windows, ignore)]
    fn test_module_imports_pkg_w_flag() {
        let cli_test = ExecCli::new(
                                CMD_TEST,
                                file_from_root("crates/cli/tests/test-projects/module_imports_pkg", "Module.roc")
            ).add_args(["--main", "tests/test-projects/module_imports_pkg/app.roc"]);

        let cli_test_out = cli_test.run();
        cli_test_out.assert_clean_success();
        cli_test_out.assert_stdout_and_stderr_ends_with("0 failed and 1 passed in <ignored for test> ms.\n");
    }
    
    #[test]
    #[cfg_attr(windows, ignore)]
    fn test_module_imports_pkg_no_flag() {
        let cli_test = ExecCli::new(
                                CMD_TEST,
                                file_from_root("crates/cli/tests/test-projects/module_imports_pkg", "Module.roc")
            );

        let cli_test_out = cli_test.run();
        insta::assert_snapshot!(cli_test_out.normalize_stdout_and_stderr());
    }
    /*
    #[test]
    #[cfg_attr(windows, ignore)]
    fn test_module_imports_unknown_pkg() {
        let runner = ExecCli::new_roc()
            .arg(CMD_TEST)
            .with_valgrind(ALLOW_VALGRIND)
            .add_args(["--main", "tests/module_imports_pkg/app.roc"])
            .arg(
                file_from_root(
                    "crates/cli/tests/module_imports_pkg",
                    "ImportsUnknownPkg.roc",
                )
                .as_path(),
            );

        let out = runner.run();
        insta::assert_snapshot!(out.normalize_stdout_and_stderr());
    }

    #[test]
    #[cfg_attr(windows, ignore)]
    /// this tests that a platform can correctly import a package
    fn platform_requires_pkg() {
        let runner = ExecCli::new_roc()
            .arg(CMD_RUN)
            .arg(BUILD_HOST_FLAG)
            .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG)
            .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
            .arg(file_from_root("crates/cli/tests/platform_requires_pkg", "app.roc").as_path());

        let out = runner.run();
        out.assert_clean_success();
        insta::assert_snapshot!(out.normalize_stdout_and_stderr());
    }

    mod test_platform_effects_zig {
        use super::*;
        use cli_test_utils::helpers::{file_from_root, ExecCli};
        use roc_cli::{CMD_BUILD, CMD_RUN};

        static BUILD_PLATFORM_HOST: std::sync::Once = std::sync::Once::new();

        /// Build the platform host once for all tests in this module
        fn build_platform_host() {
            BUILD_PLATFORM_HOST.call_once(|| {
                let out = ExecCli::new_roc()
                    .arg(CMD_BUILD)
                    .arg(BUILD_HOST_FLAG)
                    .arg(OPTIMIZE_FLAG)
                    .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG)
                    .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                    .arg(
                        file_from_root("crates/cli/tests/effects/platform/", "app-stub.roc")
                            .as_path(),
                    )
                    .run();
                out.assert_clean_success();
            });
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn interactive_effects() {
            build_platform_host();

            let runner = ExecCli::new_roc()
                .arg(CMD_RUN)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(file_from_root("crates/cli/tests/effects", "print-line.roc").as_path())
                .with_stdin_vals(vec!["hi there!"]);

            let out = runner.run();
            out.assert_clean_success();
            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn combine_tasks_with_record_builder() {
            build_platform_host();

            let runner = ExecCli::new_roc()
                .arg(CMD_RUN)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .arg(file_from_root("crates/cli/tests/effects", "combine-tasks.roc").as_path());

            let out = runner.run();
            out.assert_clean_success();
            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn inspect_logging() {
            build_platform_host();

            let runner = ExecCli::new_roc()
                .arg(CMD_RUN)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(file_from_root("crates/cli/tests/effects", "inspect-logging.roc").as_path());

            let out = runner.run();
            out.assert_clean_success();
            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn module_params_pass_task() {
            build_platform_host();

            let runner = ExecCli::new_roc()
                .arg(CMD_RUN)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(file_from_root("crates/cli/tests/module_params", "pass_task.roc").as_path());

            let out = runner.run();
            out.assert_clean_success();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }
    }

    mod test_platform_simple_zig {
        use super::*;
        use cli_test_utils::helpers::{file_from_root, ExecCli};
        use roc_cli::{CMD_BUILD, CMD_DEV, CMD_RUN, CMD_TEST};

        static BUILD_PLATFORM_HOST: std::sync::Once = std::sync::Once::new();

        /// Build the platform host once for all tests in this module
        fn build_platform_host() {
            BUILD_PLATFORM_HOST.call_once(|| {
                let out = ExecCli::new_roc()
                    .arg(CMD_BUILD)
                    .arg(BUILD_HOST_FLAG)
                    .arg(OPTIMIZE_FLAG)
                    .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG)
                    .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                    .arg(
                        file_from_root("crates/cli/tests/test-platform-simple-zig", "app.roc")
                            .as_path(),
                    )
                    .run();
                out.assert_clean_success();
            });
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_multi_dep_str_unoptimized() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(roc_cli::CMD_RUN)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(
                    file_from_root("crates/cli/tests/fixtures/multi-dep-str", "Main.roc").as_path(),
                );

            let out = runner.run();
            out.assert_clean_success();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_multi_dep_str_optimized() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(CMD_RUN)
                .arg(OPTIMIZE_FLAG)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(
                    file_from_root("crates/cli/tests/fixtures/multi-dep-str", "Main.roc").as_path(),
                );

            let out = runner.run();
            out.assert_clean_success();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_multi_dep_thunk_unoptimized() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(CMD_RUN)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(
                    file_from_root("crates/cli/tests/fixtures/multi-dep-thunk", "Main.roc")
                        .as_path(),
                );

            let out = runner.run();
            out.assert_clean_success();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(
            windows,
            ignore = "Flaky failure: Roc command failed with status ExitStatus(ExitStatus(3221225477))"
        )]
        fn run_multi_dep_thunk_optimized() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(CMD_RUN)
                .arg(OPTIMIZE_FLAG)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(
                    file_from_root("crates/cli/tests/fixtures/multi-dep-thunk", "Main.roc")
                        .as_path(),
                );

            let out = runner.run();
            out.assert_clean_success();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_packages_unoptimized() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(CMD_RUN)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(file_from_root("crates/cli/tests/fixtures/packages", "app.roc").as_path());

            let out = runner.run();
            out.assert_clean_success();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_packages_optimized() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(CMD_RUN)
                .arg(OPTIMIZE_FLAG)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(file_from_root("crates/cli/tests/fixtures/packages", "app.roc").as_path());

            let out = runner.run();
            out.assert_clean_success();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_transitive_deps_app() {
            build_platform_host();

            let file_path = file_from_root(
                "crates/cli/tests/fixtures/transitive-deps",
                "direct-one.roc",
            );

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(CMD_RUN)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(file_path.as_path());

            let out = runner.run();
            out.assert_clean_success();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_transitive_and_direct_dep_app() {
            build_platform_host();

            let file_path = file_from_root(
                "crates/cli/tests/fixtures/transitive-deps",
                "direct-one-and-two.roc",
            );

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(CMD_RUN)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(file_path.as_path());

            let out = runner.run();
            out.assert_clean_success();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn run_double_transitive_dep_app() {
            build_platform_host();

            let file_path = file_from_root(
                "crates/cli/tests/fixtures/transitive-deps",
                "direct-zero.roc",
            );

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(CMD_RUN)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(file_path.as_path());

            let out = runner.run();
            out.assert_clean_success();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        fn expects_dev() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(CMD_DEV)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(file_from_root("crates/cli/tests/expects", "expects.roc").as_path());

            let out = runner.run();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        fn expects_test() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(CMD_TEST)
                .with_valgrind(ALLOW_VALGRIND)
                .arg(file_from_root("crates/cli/tests/expects", "expects.roc").as_path());

            let out = runner.run();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn module_params() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(CMD_RUN)
                .arg(file_from_root("crates/cli/tests/module_params", "app.roc").as_path());

            let out = runner.run();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn module_params_arity_mismatch() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc().arg(CMD_DEV).arg(
                file_from_root("crates/cli/tests/module_params", "arity_mismatch.roc").as_path(),
            );

            let out = runner.run();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn module_params_bad_ann() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc()
                .arg(CMD_DEV)
                .arg(file_from_root("crates/cli/tests/module_params", "bad_ann.roc").as_path());

            let out = runner.run();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn module_params_multiline_pattern() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc().arg(CMD_DEV).arg(
                file_from_root("crates/cli/tests/module_params", "multiline_params.roc").as_path(),
            );

            let out = runner.run();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn module_params_unexpected_fn() {
            build_platform_host();

            let runner = cli_test_utils::helpers::ExecCli::new_roc().arg(CMD_DEV).arg(
                file_from_root("crates/cli/tests/module_params", "unexpected_fn.roc").as_path(),
            );

            let out = runner.run();

            insta::assert_snapshot!(out.normalize_stdout_and_stderr());
        }
    }

    // TODO not sure if this cfg should still be here: #[cfg(not(debug_assertions))]
    // this is for testing the benchmarks, to perform proper benchmarks see crates/cli/benches/README.md
    mod test_benchmarks {
        use super::{
            UseValgrind, ALLOW_VALGRIND, BUILD_HOST_FLAG, OPTIMIZE_FLAG,
            SUPPRESS_BUILD_HOST_WARNING_FLAG,
        };
        use cli_test_utils::helpers::{file_from_root, ExecCli};
        use roc_cli::CMD_BUILD;

        // #[allow(unused_imports)]
        use std::sync::Once;

        static BUILD_PLATFORM_HOST: Once = Once::new();

        /// Build the platform host once for all tests in this module
        fn build_platform_host() {
            BUILD_PLATFORM_HOST.call_once(|| {
                let out = ExecCli::new_roc()
                    .arg(CMD_BUILD)
                    .arg(BUILD_HOST_FLAG)
                    .arg(OPTIMIZE_FLAG)
                    .arg(SUPPRESS_BUILD_HOST_WARNING_FLAG)
                    .add_arg_if(super::LINKER_FLAG, super::TEST_LEGACY_LINKER)
                    .arg(
                        file_from_root("crates/cli/tests/benchmarks/platform", "app.roc").as_path(),
                    )
                    .run();
                out.assert_clean_success();
            });
        }

        fn test_benchmark(
            roc_filename: &str,
            stdin: Vec<&'static str>,
            use_valgrind: UseValgrind,
        ) -> String {
            let dir_name = "crates/cli/tests/benchmarks";
            let file_path = file_from_root(dir_name, roc_filename);

            build_platform_host();

            #[cfg(all(not(feature = "wasm32-cli-run"), not(feature = "i386-cli-run")))]
            {
                let runner = cli_test_utils::helpers::ExecCli::new_roc()
                    .arg(roc_cli::CMD_RUN)
                    .add_arg_if(super::LINKER_FLAG, super::TEST_LEGACY_LINKER)
                    .arg(file_path.as_path())
                    .with_valgrind(matches!(use_valgrind, UseValgrind::Yes) && ALLOW_VALGRIND)
                    .with_stdin_vals(stdin);

                let out = runner.run();
                out.assert_clean_success();

                out.normalize_stdout_and_stderr()
            }

            #[cfg(feature = "wasm32-cli-run")]
            check_output_wasm(file_path.as_path(), stdin);

            #[cfg(feature = "i386-cli-run")]
            check_output_i386(file_path.as_path(), stdin);
        }

        #[cfg(feature = "wasm32-cli-run")]
        fn check_output_wasm(file_name: &std::path::Path, stdin: Vec<&str>) {
            // Check with and without optimizations
            check_wasm_output_with_stdin(file_name, stdin.clone(), &[]);

            check_wasm_output_with_stdin(file_name, stdin, &[OPTIMIZE_FLAG]);
        }

        #[cfg(feature = "wasm32-cli-run")]
        fn check_wasm_output_with_stdin(file: &std::path::Path, stdin: Vec<&str>, flags: &[&str]) {
            use super::{concatcp, TARGET_FLAG};

            let mut flags = flags.to_vec();
            flags.push(concatcp!(TARGET_FLAG, "=wasm32"));

            let out = ExecCli::new_roc()
                .arg(CMD_BUILD)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .arg(file)
                .add_args(flags)
                .run();

            out.assert_clean_success();

            let stdout = crate::run_wasm(&file.with_extension("wasm"), stdin);

            insta::assert_snapshot!(stdout);
        }

        #[cfg(feature = "i386-cli-run")]
        fn check_output_i386(file_path: &std::path::Path, stdin: Vec<&'static str>) {
            use super::{concatcp, TARGET_FLAG};

            let i386_target_arg = concatcp!(TARGET_FLAG, "=x86_32");

            let runner = ExecCli::new_roc()
                .arg(CMD_RUN)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .arg(i386_target_arg)
                .arg(file_path)
                .with_stdin_vals(stdin.clone());

            let out = runner.run();
            out.assert_clean_success();
            insta::assert_snapshot!(out.normalize_stdout_and_stderr());

            let run_optimized = ExecCli::new_roc()
                .arg(CMD_RUN)
                .add_arg_if(LINKER_FLAG, TEST_LEGACY_LINKER)
                .arg(i386_target_arg)
                .arg(OPTIMIZE_FLAG)
                .arg(file_path)
                .with_stdin_vals(stdin.clone());

            let out_optimized = run_optimized.run();
            out_optimized.assert_clean_success();
            insta::assert_snapshot!(out_optimized.normalize_stdout_and_stderr());
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn nqueens() {
            insta::assert_snapshot!(test_benchmark("nQueens.roc", vec!["6"], UseValgrind::Yes));
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn cfold() {
            insta::assert_snapshot!(test_benchmark("cFold.roc", vec!["3"], UseValgrind::Yes))
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn deriv() {
            insta::assert_snapshot!(test_benchmark("deriv.roc", vec!["2"], UseValgrind::Yes))
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn rbtree_ck() {
            insta::assert_snapshot!(test_benchmark(
                "rBTreeCk.roc",
                vec!["100"],
                UseValgrind::Yes
            ))
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn rbtree_insert() {
            insta::assert_snapshot!(test_benchmark("rBTreeInsert.roc", vec![], UseValgrind::Yes))
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
            if cfg!(feature = "wasm32-cli-run") {
                eprintln!("WARNING: skipping testing benchmark testAStar.roc because it currently does not work on wasm32 due to dictionaries.");
            } else {
                insta::assert_snapshot!(test_benchmark("testAStar.roc", vec![], UseValgrind::No))
            }
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn base64() {
            insta::assert_snapshot!(test_benchmark("testBase64.roc", vec![], UseValgrind::Yes))
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn closure() {
            insta::assert_snapshot!(test_benchmark("closure.roc", vec![], UseValgrind::No))
        }

        #[test]
        #[cfg_attr(windows, ignore)]
        fn issue2279() {
            insta::assert_snapshot!(test_benchmark("issue2279.roc", vec![], UseValgrind::Yes))
        }

        #[test]
        fn quicksort_app() {
            eprintln!("WARNING: skipping testing benchmark quicksortApp.roc because the test is broken right now!");
            // test_benchmark(
            //     "quicksortApp.roc",
            //     vec![],
            //     "todo put the correct quicksort answer here",
            //     UseValgrind::Yes,
            // )
        }
    }

    #[test]
    fn known_type_error() {
        let out = ExecCli::new_roc()
            .arg(CMD_CHECK)
            .arg(file_from_root(
                "crates/cli/tests/known_bad",
                "TypeError.roc",
            ))
            .run();

        insta::assert_snapshot!(out.normalize_stdout_and_stderr());
    }

    #[test]
    fn known_type_error_with_long_path() {
        let out = ExecCli::new_roc()
            .arg(CMD_CHECK)
            .arg(file_from_root(
                "crates/cli/tests/known_bad",
                "UnusedImportButWithALongFileNameForTesting.roc",
            ))
            .run();
        insta::assert_snapshot!(out.normalize_stdout_and_stderr());
    }

    #[test]
    fn exposed_not_defined() {
        let out = ExecCli::new_roc()
            .arg(CMD_CHECK)
            .arg(file_from_root(
                "crates/cli/tests/known_bad",
                "ExposedNotDefined.roc",
            ))
            .run();
        insta::assert_snapshot!(out.normalize_stdout_and_stderr());
    }

    #[test]
    fn unused_import() {
        let out = ExecCli::new_roc()
            .arg(CMD_CHECK)
            .arg(file_from_root(
                "crates/cli/tests/known_bad",
                "UnusedImport.roc",
            ))
            .run();
        insta::assert_snapshot!(out.normalize_stdout_and_stderr());
    }

    #[test]
    fn format_check_good() {
        ExecCli::new_roc()
            .arg(CMD_FORMAT)
            .arg(CHECK_FLAG)
            .arg(file_from_root("crates/cli/tests/fixtures/format", "Formatted.roc").as_path())
            .run()
            .assert_clean_success();
    }

    #[test]
    fn format_check_reformatting_needed() {
        ExecCli::new_roc()
            .arg(CMD_FORMAT)
            .arg(CHECK_FLAG)
            .arg(file_from_root("crates/cli/tests/fixtures/format", "NotFormatted.roc").as_path())
            .run()
            .assert_nonzero_exit();
    }

    #[test]
    fn format_check_folders() {
        // This fails, because "NotFormatted.roc" is present in this folder
        ExecCli::new_roc()
            .arg(CMD_FORMAT)
            .arg(CHECK_FLAG)
            .arg(dir_from_root("crates/cli/tests/fixtures/format").as_path())
            .run()
            .assert_nonzero_exit();

        // This doesn't fail, since only "Formatted.roc" and non-roc files are present in this folder
        ExecCli::new_roc()
            .arg(CMD_FORMAT)
            .arg(CHECK_FLAG)
            .arg(dir_from_root("crates/cli/tests/fixtures/format/formatted_directory").as_path())
            .run()
            .assert_clean_success();
    }
}

#[cfg(feature = "wasm32-cli-run")]
fn run_wasm(wasm_path: &std::path::Path, stdin: Vec<&str>) -> String {
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
    }*/
}
