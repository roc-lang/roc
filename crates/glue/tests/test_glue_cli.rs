#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate indoc;

extern crate dircpy;
extern crate roc_collections;

mod helpers;

#[cfg(test)]
mod glue_cli_tests {
    use cli_test_utils::{command::CmdOut, exec_cli::ExecCli};

    use crate::helpers::fixtures_dir;
    use std::path::{Path, PathBuf};

    #[cfg(all(target_os = "linux", target_arch = "x86_64"))]
    const TEST_LEGACY_LINKER: bool = true;

    // Surgical linker currently only supports linux x86_64,
    // so we're always testing the legacy linker on other targets.
    #[cfg(not(all(target_os = "linux", target_arch = "x86_64")))]
    const TEST_LEGACY_LINKER: bool = false;

    /// This macro does two things.
    ///
    /// First, it generates and runs a separate test for each of the given
    /// expected stdout endings. Each of these should test a particular .roc file
    /// in the fixtures/ directory. The fixtures themselves run assertions too, but
    /// the stdout check verifies that we're actually running the code we think we are;
    /// without it, it would be possible that the fixtures are just exiting without running
    /// any assertions, and we would have no way to find out!
    ///
    /// Second, this generates an extra test which (non-recursively) traverses the
    /// fixtures/ directory and verifies that each of the .roc files in there
    /// has had a corresponding test generated in the previous step. This test
    /// will fail if we ever add a new .roc file to fixtures/ and forget to
    /// add a test for it here!
    macro_rules! fixtures {
        ($($test_name:ident:$fixture_dir:expr => $ends_with:expr,)+) => {
            $(
                #[test]
                #[allow(non_snake_case)]
                fn $test_name() {
                    let dir = fixtures_dir($fixture_dir);

                    generate_glue_for(&dir, std::iter::empty());

                    fn validate<'a, I: IntoIterator<Item = &'a str> + std::fmt::Debug>(dir: PathBuf, args: I) {
                        let out = run_app(&dir.join("app.roc"), args);

                        assert!(out.status.success());
                        let ignorable = "🔨 Building host ...\n";
                        let stderr = out.stderr.replacen(ignorable, "", 1);
                        assert_eq!(stderr, "");
                        assert!(
                            out.stdout.ends_with($ends_with),
                            "Unexpected stdout ending\n\n  expected:\n\n    {}\n\n  but stdout was:\n\n    {}",
                            $ends_with,
                            out.stdout
                        );
                    }


                    let test_name_str = stringify!($test_name);

                    // TODO after #5924 is fixed; remove this
                    let skip_on_linux_surgical_linker = ["rust_closures", "rust_option", "rust_nullable_wrapped", "rust_nullable_unwrapped", "rust_nonnullable_unwrapped", "rust_enumeration", "rust_nested_record", "rust_advanced_recursive_union"];

                    // Validate linux with the default linker.
                    if !(cfg!(target_os = "linux") && (skip_on_linux_surgical_linker.contains(&test_name_str))) {
                        validate(dir.clone(), ["--build-host", "--suppress-build-host-warning"]);
                    }

                    if TEST_LEGACY_LINKER {
                        validate(dir, ["--build-host", "--suppress-build-host-warning", "--linker=legacy"]);
                    }
                }
            )*

            #[test]
            #[ignore]
            fn all_fixtures_have_tests() {
                use roc_collections::VecSet;

                let mut all_fixtures: VecSet<String> = VecSet::default();

                $(
                    all_fixtures.insert($fixture_dir.to_string());
                )*

                check_for_tests(&mut all_fixtures);
            }
        }
    }

    fixtures! {
        rust_basic_record:"rust/basic-record" => "Record was: MyRcd { b: 42, a: 1995 }\n",
        rust_nested_record:"rust/nested-record" => "Record was: Outer { y: \"foo\", z: [1, 2], x: Inner { b: 24.0, a: 5 } }\n",
        rust_enumeration:"rust/enumeration" => "tag_union was: MyEnum::Foo, Bar is: MyEnum::Bar, Baz is: MyEnum::Baz\n",
        rust_single_tag_union:"rust/single-tag-union" => indoc!(r#"
            tag_union was: SingleTagUnion::OneTag
        "#),
        rust_union_with_padding:"rust/union-with-padding" => indoc!(r#"
            tag_union was: NonRecursive::Foo("This is a test")
            `Foo "small str"` is: NonRecursive::Foo("small str")
            `Foo "A long enough string to not be small"` is: NonRecursive::Foo("A long enough string to not be small")
            `Bar 123` is: NonRecursive::Bar(123)
            `Baz` is: NonRecursive::Baz(())
            `Blah 456` is: NonRecursive::Blah(456)
        "#),
        rust_union_without_padding:"rust/union-without-padding" => indoc!(r#"
            tag_union was: NonRecursive::Foo("This is a test")
            `Foo "small str"` is: NonRecursive::Foo("small str")
            `Bar 123` is: NonRecursive::Bar(123)
            `Baz` is: NonRecursive::Baz(())
            `Blah 456` is: NonRecursive::Blah(456)
        "#),
        rust_nullable_wrapped:"rust/nullable-wrapped" => indoc!(r#"
            tag_union was: StrFingerTree::More("foo", StrFingerTree::More("bar", StrFingerTree::Empty))
            `More "small str" (Single "other str")` is: StrFingerTree::More("small str", StrFingerTree::Single("other str"))
            `More "small str" Empty` is: StrFingerTree::More("small str", StrFingerTree::Empty)
            `Single "small str"` is: StrFingerTree::Single("small str")
            `Empty` is: StrFingerTree::Empty
        "#),
        rust_nullable_unwrapped:"rust/nullable-unwrapped" => indoc!(r#"
            tag_union was: StrConsList::Cons("World!", StrConsList::Cons("Hello ", StrConsList::Nil))
            `Cons "small str" Nil` is: StrConsList::Cons("small str", StrConsList::Nil)
            `Nil` is: StrConsList::Nil
        "#),
        rust_nonnullable_unwrapped:"rust/nonnullable-unwrapped" => indoc!(r#"
            tag_union was: StrRoseTree::Tree("root", [StrRoseTree::Tree("leaf1", []), StrRoseTree::Tree("leaf2", [])])
            Tree "foo" [] is: StrRoseTree::Tree("foo", [])
        "#),
        rust_basic_recursive_union:"rust/basic-recursive-union" => indoc!(r#"
            tag_union was: Expr::Concat(Expr::String("Hello, "), Expr::String("World!"))
            `Concat (String "Hello, ") (String "World!")` is: Expr::Concat(Expr::String("Hello, "), Expr::String("World!"))
            `String "this is a test"` is: Expr::String("this is a test")
        "#),
        rust_advanced_recursive_union:"rust/advanced-recursive-union" => indoc!(r#"
            rbt was: Rbt { default: Job::Job(R1 { command: Command::Command(R2 { tool: Tool::SystemTool(R4 { name: "test", num: 42 }) }), input_files: ["foo"] }) }
        "#),
        rust_list_recursive_union:"rust/list-recursive-union" => indoc!(r#"
            rbt was: Rbt { default: Job::Job(R1 { command: Command::Command(R2 { args: [], tool: Tool::SystemTool(R3 { name: "test" }) }), input_files: ["foo"], job: [] }) }
        "#),
        rust_multiple_modules:"rust/multiple-modules" => indoc!(r#"
            combined was: Combined { s1: DepStr1::S("hello"), s2: DepStr2::R("world") }
        "#),
        // issue https://github.com/roc-lang/roc/issues/6121
        // TODO: re-enable this test. Currently it is flaking on macos x86-64 with a bad exit code.
        // nested_record:"nested-record" => "Record was: Outer { y: \"foo\", z: [1, 2], x: Inner { b: 24.0, a: 5 } }\n",
        // enumeration:"enumeration" => "tag_union was: MyEnum::Foo, Bar is: MyEnum::Bar, Baz is: MyEnum::Baz\n",
        //        arguments:"arguments" => indoc!(r#"
        //            Answer was: 84
        //        "#),
        rust_closures:"rust/closures" => indoc!(r#"
            Answer was: 672
        "#),
        rust_rocresult:"rust/rocresult" => indoc!(r#"
            Answer was: RocOk(ManuallyDrop { value: "Hello World!" })
            Answer was: RocErr(ManuallyDrop { value: 42 })
        "#),
        rust_option:"rust/option" => indoc!(r#"
            Answer was: "Hello World!"
            Answer was: discriminant_U1::None
        "#),
        c_hello_world:"c/hello-world" => indoc!(r#"
            main_for_host = 42
        "#),
    }

    fn check_for_tests(all_fixtures: &mut roc_collections::VecSet<String>) {
        use roc_collections::VecSet;

        let fixtures = fixtures_dir("");
        let entries = std::fs::read_dir(fixtures.as_path()).unwrap_or_else(|err| {
            panic!(
                "Error trying to read {} as a fixtures directory: {}",
                fixtures.to_string_lossy(),
                err
            );
        });

        for entry in entries {
            let entry = entry.unwrap();

            if entry.file_type().unwrap().is_dir() {
                let fixture_dir_name = entry.file_name().into_string().unwrap();

                if !all_fixtures.remove(&fixture_dir_name) {
                    panic!(
                        "The glue fixture directory {} does not have any corresponding tests in test_glue_cli. Please add one, so if it ever stops working, we'll know about it right away!",
                        entry.path().to_string_lossy()
                    );
                }
            }
        }

        assert_eq!(all_fixtures, &mut VecSet::default());
    }

    fn generate_glue_for<'a, I: IntoIterator<Item = &'a str>>(
        platform_dir: &'a Path,
        args: I,
    ) -> CmdOut {
        let platform_module_path = platform_dir.join("platform.roc");
        let glue_dir = platform_dir.join("test_glue");
        let tests_dir = platform_dir
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .parent()
            .unwrap();

        let fixture_templates_dir = tests_dir.join("fixture-templates");

        let fixtures_subfolder_name = platform_dir.parent().unwrap().file_name().unwrap();

        let fixture_template_dir = fixture_templates_dir.join(fixtures_subfolder_name);

        if fixture_template_dir.exists() {
            // Copy the template from the templates directory into the fixture dir if it exists
            dircpy::CopyBuilder::new(fixture_template_dir, platform_dir)
                .overwrite(true) // overwrite any files that were already present
                .run()
                .unwrap();
        }

        // Delete the glue files to make sure we're actually regenerating it!
        if glue_dir.exists() {
            std::fs::remove_dir_all(&glue_dir)
                .expect("Unable to remove test_glue dir in order to regenerate it in the test");
            // std::fs::create_dir(&glue_dir)
        }

        let glue_spec_filename = match fixtures_subfolder_name.to_str().unwrap() {
            "rust" => "RustGlue.roc",
            "zig" => "ZigGlue.roc",
            "c" => "CGlue.roc",
            unknown_subfolder => panic!("I don't know which glue file to use for tests in the `{}` subfolder! Please add one here!", unknown_subfolder),
        };

        println!("here");

        let rust_glue_spec = tests_dir
            .parent()
            .unwrap()
            .join("src")
            .join(glue_spec_filename);

        // Generate a fresh test_glue for this platform
        let all_args : Vec<_> =
            // converting these all to String avoids lifetime issues
            args.into_iter().map(|arg| arg.to_string()).chain([
                glue_dir.to_str().unwrap().to_string(),
                platform_module_path.to_str().unwrap().to_string(),
            ]).collect();

        let glue_cmd = ExecCli::new("glue", rust_glue_spec).add_args(all_args);
        let glue_cmd_out = glue_cmd.run();

        glue_cmd_out.assert_clean_success();

        glue_cmd_out
    }

    fn run_app<'a, 'b, I: IntoIterator<Item = &'a str> + std::fmt::Debug>(
        app_file_path: &'b Path,
        args: I,
    ) -> CmdOut {
        let dev_cmd = ExecCli::new(
            "dev", // can't import CMD_DEV from roc_cli, that would create a cycle
            app_file_path.to_path_buf(),
        )
        .add_args(args);

        let dev_cmd_out = dev_cmd.run();

        dev_cmd_out.assert_clean_success();

        dev_cmd_out
    }
}
