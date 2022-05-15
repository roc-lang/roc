#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate indoc;

#[cfg(not(debug_assertions))]
extern crate roc_collections;

mod helpers;

#[cfg(test)]
mod bindgen_cli_run {
    use crate::helpers::fixtures_dir;
    use cli_utils::helpers::{
        example_file, examples_dir, extract_valgrind_errors, fixture_file, known_bad_file,
        path_to_binary, run_bindgen, run_cmd, run_roc, run_with_stdin, run_with_valgrind,
        strip_colors, Out, ValgrindError, ValgrindErrorXWhat,
    };
    use roc_test_utils::assert_multiline_str_eq;
    use serial_test::serial;
    use std::iter;
    use std::path::{Path, PathBuf};

    #[cfg(not(debug_assertions))]
    use roc_collections::all::MutMap;

    #[derive(Debug, PartialEq, Eq)]
    struct Example<'a> {
        filename: &'a str,
        executable_filename: &'a str,
        stdin: &'a [&'a str],
        input_file: Option<&'a str>,
        expected_ending: &'a str,
        use_valgrind: bool,
    }

    // fn check_output_with_stdin(
    //     file: &Path,
    //     stdin: &[&str],
    //     executable_filename: &str,
    //     input_file: Option<PathBuf>,
    //     expected_ending: &str,
    // ) {
    //     let args = vec!["TODO_arg"];
    //     let out = run_bindgen_on(file, args.into_iter(), stdin, input_file.clone());

    //     if !&out.stdout.ends_with(expected_ending) {
    //         panic!(
    //             "expected output to end with {:?} but instead got {:#?} - stderr was: {:#?}",
    //             expected_ending, out.stdout, out.stderr
    //         );
    //     }

    //     assert!(out.status.success());
    // }

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

                    // Check with and without optimizations
                    check_output_with_stdin(
                        &file_name,
                        example.stdin,
                        example.executable_filename,
                        example.input_file.and_then(|file| Some(example_file(dir_name, file))),
                        example.expected_ending,
                    );
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
    // examples! {
    //     helloWorld:"hello-world" => Example {
    //         filename: "helloWorld.roc",
    //         executable_filename: "helloWorld",
    //         stdin: &[],
    //         input_file: None,
    //         expected_ending:"Hello, World!\n",
    //         use_valgrind: true,
    //     },
    //     helloC:"hello-world/c-platform" => Example {
    //         filename: "helloC.roc",
    //         executable_filename: "helloC",
    //         stdin: &[],
    //         input_file: None,
    //         expected_ending:"Hello, World!\n",
    //         use_valgrind: true,
    //     },
    //     helloZig:"hello-world/zig-platform" => Example {
    //         filename: "helloZig.roc",
    //         executable_filename: "helloZig",
    //         stdin: &[],
    //         input_file: None,
    //         expected_ending:"Hello, World!\n",
    //         use_valgrind: true,
    //     },
    //     helloRust:"hello-world/rust-platform" => Example {
    //         filename: "helloRust.roc",
    //         executable_filename: "helloRust",
    //         stdin: &[],
    //         input_file: None,
    //         expected_ending:"Hello, World!\n",
    //         use_valgrind: true,
    //     },
    //     helloSwift:"hello-world/swift-platform" => Example {
    //         filename: "helloSwift.roc",
    //         executable_filename: "helloSwift",
    //         stdin: &[],
    //         input_file: None,
    //         expected_ending:"Hello, World!\n",
    //         use_valgrind: true,
    //     },
    //     helloWeb:"hello-world/web-platform" => Example {
    //         filename: "helloWeb.roc",
    //         executable_filename: "helloWeb",
    //         stdin: &[],
    //         input_file: None,
    //         expected_ending:"Hello, World!\n",
    //         use_valgrind: true,
    //     },
    //     fib:"algorithms" => Example {
    //         filename: "fibonacci.roc",
    //         executable_filename: "fibonacci",
    //         stdin: &[],
    //         input_file: None,
    //         expected_ending:"55\n",
    //         use_valgrind: true,
    //     },
    //     gui:"gui" => Example {
    //         filename: "Hello.roc",
    //         executable_filename: "hello-gui",
    //         stdin: &[],
    //         input_file: None,
    //         expected_ending: "",
    //         use_valgrind: false,
    //     },
    //     breakout:"breakout" => Example {
    //         filename: "breakout.roc",
    //         executable_filename: "breakout",
    //         stdin: &[],
    //         input_file: None,
    //         expected_ending: "",
    //         use_valgrind: false,
    //     },
    //     quicksort:"algorithms" => Example {
    //         filename: "quicksort.roc",
    //         executable_filename: "quicksort",
    //         stdin: &[],
    //         input_file: None,
    //         expected_ending: "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
    //         use_valgrind: true,
    //     },
    //     // shared_quicksort:"shared-quicksort" => Example {
    //     //     filename: "Quicksort.roc",
    //     //     executable_filename: "quicksort",
    //     //     stdin: &[],
    //     //     input_file: None,
    //     //     expected_ending: "[0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 2]\n",
    //     //     use_valgrind: true,
    //     // },
    //     effects:"interactive" => Example {
    //         filename: "effects.roc",
    //         executable_filename: "effects",
    //         stdin: &["hi there!"],
    //         input_file: None,
    //         expected_ending: "hi there!\nIt is known\n",
    //         use_valgrind: true,
    //     },
    //     // tea:"tea" => Example {
    //     //     filename: "Main.roc",
    //     //     executable_filename: "tea-example",
    //     //     stdin: &[],
    //     //     input_file: None,
    //     //     expected_ending: "",
    //     //     use_valgrind: true,
    //     // },
    //     cli:"interactive" => Example {
    //         filename: "form.roc",
    //         executable_filename: "form",
    //         stdin: &["Giovanni\n", "Giorgio\n"],
    //         input_file: None,
    //         expected_ending: "Hi, Giovanni Giorgio! 👋\n",
    //         use_valgrind: false,
    //     },
    //     tui:"interactive" => Example {
    //         filename: "tui.roc",
    //         executable_filename: "tui",
    //         stdin: &["foo\n"], // NOTE: adding more lines leads to memory leaks
    //         input_file: None,
    //         expected_ending: "Hello Worldfoo!\n",
    //         use_valgrind: true,
    //     },
    //     // custom_malloc:"custom-malloc" => Example {
    //     //     filename: "Main.roc",
    //     //     executable_filename: "custom-malloc-example",
    //     //     stdin: &[],
    //     //     input_file: None,
    //     //     expected_ending: "ms!\nThe list was small!\n",
    //     //     use_valgrind: true,
    //     // },
    //     // task:"task" => Example {
    //     //     filename: "Main.roc",
    //     //     executable_filename: "task-example",
    //     //     stdin: &[],
    //     //     input_file: None,
    //     //     expected_ending: "successfully wrote to file\n",
    //     //     use_valgrind: true,
    //     // },
    //     false_interpreter:"false-interpreter" => {
    //         Example {
    //             filename: "False.roc",
    //             executable_filename: "false",
    //             stdin: &[],
    //             input_file: Some("examples/hello.false"),
    //             expected_ending:"Hello, World!\n",
    //             use_valgrind: false,
    //         }
    //     },
    // }

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

    fn generate_bindings_for<'a, I: IntoIterator<Item = &'a str>>(
        platform_dir: &'a Path,
        args: I,
    ) -> Out {
        // Generate bindings.rs for this platform
        let package_config = platform_dir.join("Package-Config.roc");
        let bindings_file = platform_dir.join("src").join("bindings.rs");
        let bindgen_out = run_bindgen(
            // converting these all to String avoids lifetime issues
            args.into_iter().map(|arg| arg.to_string()).chain([
                package_config.to_str().unwrap().to_string(),
                bindings_file.to_str().unwrap().to_string(),
            ]),
        );

        // If there is any stderr, it should be reporting the runtime and that's it!
        if !(bindgen_out.stderr.is_empty()
            || bindgen_out.stderr.starts_with("runtime: ") && bindgen_out.stderr.ends_with("ms\n"))
        {
            panic!(
                "`roc-bindgen` command had unexpected stderr: {}",
                bindgen_out.stderr
            );
        }

        assert!(bindgen_out.status.success(), "bad status {:?}", bindgen_out);

        bindgen_out
    }

    fn run_app<'a, I: IntoIterator<Item = &'a str>>(
        app_file: &'a Path,
        args: I,
        stdin: &[&str],
    ) -> Out {
        // Generate bindings.rs for this platform
        let compile_out = run_roc(
            // converting these all to String avoids lifetime issues
            args.into_iter()
                .map(|arg| arg.to_string())
                .chain([app_file.to_str().unwrap().to_string()]),
            stdin,
        );

        // If there is any stderr, it should be reporting the runtime and that's it!
        if !(compile_out.stderr.is_empty()
            || compile_out.stderr.starts_with("runtime: ") && compile_out.stderr.ends_with("ms\n"))
        {
            panic!(
                "`roc` command had unexpected stderr: {}",
                compile_out.stderr
            );
        }

        assert!(compile_out.status.success(), "bad status {:?}", compile_out);

        compile_out
    }

    #[test]
    #[serial(basic_record)]
    fn basic_record() {
        let dir = fixtures_dir("basic-record");

        generate_bindings_for(&dir.join("platform"), std::iter::empty());
        let out = run_app(&dir.join("app.roc"), std::iter::empty(), &[]);

        assert_eq!(out.stderr, "");
        assert_eq!(out.status.code(), Some(0));
        assert!(
            out.stdout
                .ends_with("Record was: MyRcd { b: 42, a: 1995 }\n"),
            "Unexpected {:?}",
            out.stdout
        );
    }
}
