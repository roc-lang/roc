#![cfg(test)]

use indoc::indoc;

fn valgrind_test(source: &str) {
    #[cfg(target_os = "linux")]
    {
        valgrind_test_linux(source)
    }

    #[cfg(not(target_os = "linux"))]
    {
        let _ = source;
    }
}

#[cfg(target_os = "linux")]
fn valgrind_test_linux(source: &str) {
    use roc_cli::build::BuiltFile;

    let pf = std::env::current_dir()
        .unwrap()
        .join("zig-platform/main.roc");

    assert!(pf.exists(), "{:?}", &pf);

    let mut app_module_source = format!(
        indoc::indoc!(
            r#"
                app "test"
                    packages {{ pf: "{}" }}
                    imports []
                    provides [main] to pf

                main =
            "#
        ),
        pf.to_str().unwrap()
    );

    for line in source.lines() {
        app_module_source.push_str("    ");
        app_module_source.push_str(line);
        app_module_source.push('\n');
    }

    let temp_dir = tempfile::tempdir().unwrap();
    let app_module_path = temp_dir.path().join("app.roc");

    let arena = bumpalo::Bump::new();
    let assume_prebuilt = true;
    let res_binary_path = roc_cli::build::build_str_test(
        &arena,
        &app_module_path,
        &app_module_source,
        assume_prebuilt,
    );

    match res_binary_path {
        Ok(BuiltFile {
            binary_path,
            problems,
            total_time: _,
            expect_metadata: _,
        }) => {
            if problems.exit_code() != 0 {
                panic!("there are problems")
            }

            run_with_valgrind(&binary_path);
        }
        Err(roc_cli::build::BuildFileError::LoadingProblem(
            roc_load::LoadingProblem::FormattedReport(report),
        )) => {
            eprintln!("{}", report);
            panic!("");
        }
        Err(e) => panic!("{:?}", e),
    }

    drop(temp_dir)
}

#[allow(unused)]
fn run_with_valgrind(binary_path: &std::path::Path) {
    use cli_utils::helpers::{extract_valgrind_errors, ValgrindError, ValgrindErrorXWhat};

    // If possible, report the generated executable name relative to the current dir.
    let generated_filename = binary_path
        .strip_prefix(std::env::current_dir().unwrap())
        .unwrap_or(binary_path)
        .to_str()
        .unwrap();

    let (valgrind_out, raw_xml) =
        cli_utils::helpers::run_with_valgrind([], &[generated_filename.to_string()]);

    if valgrind_out.status.success() {
        let memory_errors = extract_valgrind_errors(&raw_xml).unwrap_or_else(|err| {
            panic!(
                indoc!(
                    r#"
                    failed to parse the `valgrind` xml output:

                        Error was:

                            {:?}

                        valgrind xml was:

                            {}

                        valgrind stdout was:

                            {}

                        valgrind stderr was:

                            {}
                    "#
                ),
                err, raw_xml, valgrind_out.stdout, valgrind_out.stderr
            );
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
            panic!("Valgrind found memory errors");
        }
    } else {
        let exit_code = match valgrind_out.status.code() {
            Some(code) => format!("exit code {}", code),
            None => "no exit code".to_string(),
        };

        panic!(
            "`valgrind` exited with {}. valgrind stdout was: \"{}\"\n\nvalgrind stderr was: \"{}\"",
            exit_code, valgrind_out.stdout, valgrind_out.stderr
        );
    }
}

#[test]
fn list_concat_consumes_first_argument() {
    valgrind_test("List.concat (List.withCapacity 1024) [1,2,3] |> List.len |> Num.toStr");
}

#[test]
fn str_capacity_concat() {
    valgrind_test(r#"Str.withCapacity 42 |> Str.concat "foobar""#);
}

#[test]
fn append_scalar() {
    valgrind_test(indoc!(
        r#"
        Str.appendScalar "abcd" 'A'
            |> Result.withDefault ""
        "#
    ));
}

#[test]
fn split_not_present() {
    valgrind_test(indoc!(
        r#"
        Str.split (Str.concat "a string that is stored on the heap" "!") "\n"
            |> Str.joinWith ""
        "#
    ));
}

#[test]
fn str_concat_first_argument_not_unique() {
    valgrind_test(indoc!(
        r#"
        (
            str1 = Str.reserve "" 48
            str2 = "a"

            out = Str.concat str1 str2
            if Bool.false then
                out
            else
                str1
            )
        "#
    ));
}
