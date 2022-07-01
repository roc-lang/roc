#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate indoc;

extern crate dircpy;
extern crate roc_collections;

mod helpers;

#[cfg(test)]
mod bindgen_cli_run {
    use crate::helpers::{fixtures_dir, root_dir};
    use cli_utils::helpers::{run_bindgen, run_roc, Out};
    use std::fs;
    use std::path::Path;
    use std::process::Command;

    // All of these tests rely on `target/` for the `cli` crate being up-to-date,
    // so do a `cargo build` on it first!
    #[ctor::ctor]
    fn init() {
        let args = if cfg!(debug_assertions) {
            vec!["build"]
        } else {
            vec!["build", "--release"]
        };

        println!(
            "Running `cargo {}` on the `cli` crate before running the tests. This may take a bit!",
            args.join(" ")
        );

        let output = Command::new("cargo")
            .args(args)
            .current_dir(root_dir().join("crates").join("cli"))
            .output()
            .unwrap_or_else(|err| {
                panic!(
                    "Failed to `cargo build` roc CLI for bindgen CLI tests - error was: {:?}",
                    err
                )
            });

        assert!(output.status.success());
    }

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

                    generate_bindings_for(&dir, std::iter::empty());
                    let out = run_app(&dir.join("app.roc"), std::iter::empty());

                    assert!(out.status.success());
                    assert_eq!(out.stderr, "");
                    assert!(
                        out.stdout.ends_with($ends_with),
                        "Unexpected stdout ending - expected {:?} but stdout was: {:?}",
                        $ends_with,
                        out.stdout
                    );
                }
            )*

            #[test]
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
        basic_record:"basic-record" => "Record was: MyRcd { b: 42, a: 1995 }\n",
        nested_record:"nested-record" => "Record was: Outer { y: \"foo\", z: [1, 2], x: Inner { b: 24.0, a: 5 } }\n",
        enumeration:"enumeration" => "tag_union was: MyEnum::Foo, Bar is: MyEnum::Bar, Baz is: MyEnum::Baz\n",
        union_with_padding:"union-with-padding" => indoc!(r#"
            tag_union was: NonRecursive::Foo("This is a test")
            `Foo "small str"` is: NonRecursive::Foo("small str")
            `Foo "A long enough string to not be small"` is: NonRecursive::Foo("A long enough string to not be small")
            `Bar 123` is: NonRecursive::Bar(123)
            `Baz` is: NonRecursive::Baz
            `Blah 456` is: NonRecursive::Blah(456)
        "#),
        union_without_padding:"union-without-padding" => indoc!(r#"
            tag_union was: NonRecursive::Foo("This is a test")
            `Foo "small str"` is: NonRecursive::Foo("small str")
            `Bar 123` is: NonRecursive::Bar(123)
            `Baz` is: NonRecursive::Baz
            `Blah 456` is: NonRecursive::Blah(456)
        "#),
        nullable_unwrapped:"nullable-unwrapped" => indoc!(r#"
            tag_union was: StrConsList::Cons("World!", StrConsList::Cons("Hello ", StrConsList::Nil))
            `Cons "small str" Nil` is: StrConsList::Cons("small str", StrConsList::Nil)
            `Nil` is: StrConsList::Nil
        "#),
        basic_recursive_union:"basic-recursive-union" => indoc!(r#"
            tag_union was: Expr::Concat(Expr::String("Hello, "), Expr::String("World!"))
            `Concat (String "Hello, ") (String "World!")` is: Expr::Concat(Expr::String("Hello, "), Expr::String("World!"))
            `String "this is a test"` is: Expr::String("this is a test")
        "#),
        advanced_recursive_union:"advanced-recursive-union" => indoc!(r#"
            rbt was: Rbt { default: Job::Job(R1 { command: Command::Command(R2 { tool: Tool::SystemTool(R4 { name: "test", num: 42 }) }), inputFiles: ["foo"] }) }
        "#),
        list_recursive_union:"list-recursive-union" => indoc!(r#"
            rbt was: Rbt { default: Job::Job(R1 { command: Command::Command(R2 { args: [], tool: Tool::SystemTool(R3 { name: "test" }) }), inputFiles: ["foo"], job: [] }) }
        "#),
    }

    fn check_for_tests(all_fixtures: &mut roc_collections::VecSet<String>) {
        use roc_collections::VecSet;

        // todo!("Remove a bunch of duplication - don't have a ton of files in there.");

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
                        "The bindgen fixture directory {} does not have any corresponding tests in cli_run. Please add one, so if it ever stops working, we'll know about it right away!",
                        entry.path().to_string_lossy()
                    );
                }
            }
        }

        assert_eq!(all_fixtures, &mut VecSet::default());
    }

    fn generate_bindings_for<'a, I: IntoIterator<Item = &'a str>>(
        platform_dir: &'a Path,
        args: I,
    ) -> Out {
        let platform_module_path = platform_dir.join("platform.roc");
        let bindings_file = platform_dir.join("src").join("bindings.rs");
        let fixture_templates_dir = platform_dir
            .parent()
            .unwrap()
            .parent()
            .unwrap()
            .join("fixture-templates");

        // Copy the rust template from the templates directory into the fixture dir.
        dircpy::CopyBuilder::new(fixture_templates_dir.join("rust"), platform_dir)
            .overwrite(true) // overwrite any files that were already present
            .run()
            .unwrap();

        // Delete the bindings file to make sure we're actually regenerating it!
        if bindings_file.exists() {
            fs::remove_file(&bindings_file)
                .expect("Unable to remove bindings.rs in order to regenerate it in the test");
        }

        // Generate a fresh bindings.rs for this platform
        let bindgen_out = run_bindgen(
            // converting these all to String avoids lifetime issues
            args.into_iter().map(|arg| arg.to_string()).chain([
                platform_module_path.to_str().unwrap().to_string(),
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

    fn run_app<'a, I: IntoIterator<Item = &'a str>>(app_file: &'a Path, args: I) -> Out {
        // Generate bindings.rs for this platform
        let compile_out = run_roc(
            // converting these all to String avoids lifetime issues
            args.into_iter()
                .map(|arg| arg.to_string())
                .chain([app_file.to_str().unwrap().to_string()]),
            &[],
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
}
