#[macro_use]
extern crate pretty_assertions;
extern crate bumpalo;
extern crate indoc;
extern crate roc_reporting;

mod helpers;

#[cfg(test)]
mod test_reporting {
    use crate::helpers::{can_expr, infer_expr, test_home, CanExprOut, ParseErrOut};
    use bumpalo::Bump;
    use indoc::indoc;
    use roc_can::abilities::AbilitiesStore;
    use roc_can::expr::PendingDerives;
    use roc_load::{self, LoadedModule, LoadingProblem, Threading};
    use roc_module::symbol::{Interns, ModuleId};
    use roc_region::all::LineInfo;
    use roc_reporting::report::{
        can_problem, parse_problem, type_problem, RenderTarget, Report, Severity, ANSI_STYLE_CODES,
        DEFAULT_PALETTE,
    };
    use roc_reporting::report::{RocDocAllocator, RocDocBuilder};
    use roc_solve::solve;
    use roc_test_utils::assert_multiline_str_eq;
    use roc_types::subs::Subs;
    use std::path::PathBuf;

    fn filename_from_string(str: &str) -> PathBuf {
        let mut filename = PathBuf::new();
        filename.push(str);

        filename
    }

    fn to_simple_report(doc: RocDocBuilder) -> Report {
        Report {
            title: "".to_string(),
            doc,
            filename: filename_from_string(r"/code/proj/Main.roc"),
            severity: Severity::RuntimeError,
        }
    }

    fn promote_expr_to_module(src: &str) -> String {
        let mut buffer = String::from("app \"test\" provides [main] to \"./platform\"\n\nmain =\n");

        for line in src.lines() {
            // indent the body!
            buffer.push_str("    ");
            buffer.push_str(line);
            buffer.push('\n');
        }

        buffer
    }

    fn run_load_and_infer<'a>(
        subdir: &str,
        arena: &'a Bump,
        src: &'a str,
    ) -> (String, Result<LoadedModule, LoadingProblem<'a>>) {
        use std::fs::File;
        use std::io::Write;

        let module_src = if src.starts_with("app") {
            // this is already a module
            src.to_string()
        } else {
            // this is an expression, promote it to a module
            promote_expr_to_module(src)
        };

        let exposed_types = Default::default();
        let loaded = {
            // Use a deterministic temporary directory.
            // We can't have all tests use "tmp" because tests run in parallel,
            // so append the test name to the tmp path.
            let tmp = format!("tmp/{}", subdir);
            let dir = roc_test_utils::TmpDir::new(&tmp);

            let filename = PathBuf::from("Test.roc");
            let file_path = dir.path().join(filename);
            let full_file_path = file_path.clone();
            let mut file = File::create(file_path).unwrap();
            writeln!(file, "{}", module_src).unwrap();
            let result = roc_load::load_and_typecheck(
                arena,
                full_file_path,
                dir.path(),
                exposed_types,
                roc_target::TargetInfo::default_x86_64(),
                RenderTarget::Generic,
                Threading::Single,
            );
            drop(file);

            result
        };

        (module_src, loaded)
    }

    #[allow(clippy::type_complexity)]
    fn infer_expr_help_new<'a>(
        subdir: &str,
        arena: &'a Bump,
        expr_src: &'a str,
    ) -> Result<
        (
            String,
            Vec<solve::TypeError>,
            Vec<roc_problem::can::Problem>,
            ModuleId,
            Interns,
        ),
        LoadingProblem<'a>,
    > {
        let (module_src, result) = run_load_and_infer(subdir, arena, expr_src);
        let LoadedModule {
            module_id: home,
            mut can_problems,
            mut type_problems,
            interns,
            ..
        } = result?;

        let can_problems = can_problems.remove(&home).unwrap_or_default();
        let type_problems = type_problems.remove(&home).unwrap_or_default();

        Ok((module_src, type_problems, can_problems, home, interns))
    }

    fn list_reports_new<F>(subdir: &str, arena: &Bump, src: &str, finalize_render: F) -> String
    where
        F: FnOnce(RocDocBuilder<'_>, &mut String),
    {
        use ven_pretty::DocAllocator;

        let filename = filename_from_string(r"/code/proj/Main.roc");

        let mut buf = String::new();

        match infer_expr_help_new(subdir, arena, src) {
            Err(LoadingProblem::FormattedReport(fail)) => fail,
            Ok((module_src, type_problems, can_problems, home, interns)) => {
                let lines = LineInfo::new(&module_src);
                let src_lines: Vec<&str> = module_src.split('\n').collect();
                let mut reports = Vec::new();

                let alloc = RocDocAllocator::new(&src_lines, home, &interns);

                for problem in can_problems {
                    let report = can_problem(&alloc, &lines, filename.clone(), problem.clone());
                    reports.push(report);
                }

                for problem in type_problems {
                    if let Some(report) =
                        type_problem(&alloc, &lines, filename.clone(), problem.clone())
                    {
                        reports.push(report);
                    }
                }

                let has_reports = !reports.is_empty();

                let doc = alloc
                    .stack(reports.into_iter().map(|v| v.pretty(&alloc)))
                    .append(if has_reports {
                        alloc.line()
                    } else {
                        alloc.nil()
                    });

                finalize_render(doc, &mut buf);
                buf
            }
            Err(other) => {
                panic!("failed to load: {:?}", other);
            }
        }
    }

    fn infer_expr_help<'a>(
        arena: &'a Bump,
        expr_src: &'a str,
    ) -> Result<
        (
            Vec<solve::TypeError>,
            Vec<roc_problem::can::Problem>,
            ModuleId,
            Interns,
        ),
        ParseErrOut<'a>,
    > {
        let CanExprOut {
            loc_expr: _,
            output,
            var_store,
            var,
            constraints,
            constraint,
            home,
            interns,
            problems: can_problems,
            ..
        } = can_expr(arena, expr_src)?;
        let mut subs = Subs::new_from_varstore(var_store);

        for named in output.introduced_variables.named {
            subs.rigid_var(named.variable, named.name);
        }

        for var in output.introduced_variables.wildcards {
            subs.rigid_var(var.value, "*".into());
        }

        let mut solve_aliases = roc_solve::solve::Aliases::default();

        for (name, alias) in output.aliases {
            solve_aliases.insert(name, alias);
        }

        let mut unify_problems = Vec::new();
        let mut abilities_store = AbilitiesStore::default();
        let (_content, _subs) = infer_expr(
            subs,
            &mut unify_problems,
            &constraints,
            &constraint,
            // Use `new_report_problem_as` in order to get proper derives.
            // TODO: remove the non-new reporting test infra.
            PendingDerives::default(),
            &mut solve_aliases,
            &mut abilities_store,
            var,
        );

        Ok((unify_problems, can_problems, home, interns))
    }

    fn list_reports<F>(arena: &Bump, src: &str, buf: &mut String, callback: F)
    where
        F: FnOnce(RocDocBuilder<'_>, &mut String),
    {
        use ven_pretty::DocAllocator;

        let src_lines: Vec<&str> = src.split('\n').collect();
        let lines = LineInfo::new(src);

        let filename = filename_from_string(r"/code/proj/Main.roc");

        match infer_expr_help(arena, src) {
            Err(parse_err) => {
                let ParseErrOut {
                    fail,
                    home,
                    interns,
                } = parse_err;

                let alloc = RocDocAllocator::new(&src_lines, home, &interns);

                let problem = fail.into_file_error(filename.clone());
                let doc = parse_problem(&alloc, &lines, filename, 0, problem);

                callback(doc.pretty(&alloc).append(alloc.line()), buf)
            }
            Ok((type_problems, can_problems, home, interns)) => {
                let mut reports = Vec::new();

                let alloc = RocDocAllocator::new(&src_lines, home, &interns);

                for problem in can_problems {
                    let report = can_problem(&alloc, &lines, filename.clone(), problem.clone());
                    reports.push(report);
                }

                for problem in type_problems {
                    if let Some(report) =
                        type_problem(&alloc, &lines, filename.clone(), problem.clone())
                    {
                        reports.push(report);
                    }
                }

                let has_reports = !reports.is_empty();

                let doc = alloc
                    .stack(reports.into_iter().map(|v| v.pretty(&alloc)))
                    .append(if has_reports {
                        alloc.line()
                    } else {
                        alloc.nil()
                    });

                callback(doc, buf)
            }
        }
    }

    fn list_header_reports<F>(arena: &Bump, src: &str, buf: &mut String, callback: F)
    where
        F: FnOnce(RocDocBuilder<'_>, &mut String),
    {
        use ven_pretty::DocAllocator;

        use roc_parse::state::State;

        let state = State::new(src.as_bytes());

        let filename = filename_from_string(r"/code/proj/Main.roc");
        let src_lines: Vec<&str> = src.split('\n').collect();
        let lines = LineInfo::new(src);

        match roc_parse::module::parse_header(arena, state) {
            Err(fail) => {
                let interns = Interns::default();
                let home = crate::helpers::test_home();

                let alloc = RocDocAllocator::new(&src_lines, home, &interns);

                use roc_parse::parser::SyntaxError;
                let problem = fail
                    .map_problem(SyntaxError::Header)
                    .into_file_error(filename.clone());
                let doc = parse_problem(&alloc, &lines, filename, 0, problem);

                callback(doc.pretty(&alloc).append(alloc.line()), buf)
            }
            Ok(_) => todo!(),
        }
    }

    fn report_problem_as(src: &str, expected_rendering: &str) {
        let mut buf: String = String::new();
        let arena = Bump::new();

        let callback = |doc: RocDocBuilder<'_>, buf: &mut String| {
            doc.1
                .render_raw(70, &mut roc_reporting::report::CiWrite::new(buf))
                .expect("list_reports")
        };

        list_reports(&arena, src, &mut buf, callback);

        // convenient to copy-paste the generated message
        if buf != expected_rendering {
            for line in buf.split('\n') {
                println!("                {}", line);
            }
        }

        assert_multiline_str_eq!(expected_rendering, buf.as_str());
    }

    fn report_header_problem_as(src: &str, expected_rendering: &str) {
        let mut buf: String = String::new();
        let arena = Bump::new();

        let callback = |doc: RocDocBuilder<'_>, buf: &mut String| {
            doc.1
                .render_raw(70, &mut roc_reporting::report::CiWrite::new(buf))
                .expect("list_reports")
        };

        list_header_reports(&arena, src, &mut buf, callback);

        // convenient to copy-paste the generated message
        if buf != expected_rendering {
            for line in buf.split('\n') {
                println!("                {}", line);
            }
        }

        assert_eq!(buf, expected_rendering);
    }

    fn color_report_problem_as(src: &str, expected_rendering: &str) {
        let mut buf: String = String::new();
        let arena = Bump::new();

        let callback = |doc: RocDocBuilder<'_>, buf: &mut String| {
            doc.1
                .render_raw(
                    70,
                    &mut roc_reporting::report::ColorWrite::new(
                        &roc_reporting::report::DEFAULT_PALETTE,
                        buf,
                    ),
                )
                .expect("list_reports")
        };

        list_reports(&arena, src, &mut buf, callback);

        let readable = human_readable(&buf);

        assert_eq!(readable, expected_rendering);
    }

    /// Do not call this directly! Use the test_report macro below!
    fn __new_report_problem_as(subdir: &str, src: &str, expected_rendering: &str) {
        let arena = Bump::new();

        let finalize_render = |doc: RocDocBuilder<'_>, buf: &mut String| {
            doc.1
                .render_raw(70, &mut roc_reporting::report::CiWrite::new(buf))
                .expect("list_reports")
        };

        let buf = list_reports_new(subdir, &arena, src, finalize_render);

        // convenient to copy-paste the generated message
        if buf != expected_rendering {
            for line in buf.split('\n') {
                println!("                {}", line);
            }
        }

        assert_multiline_str_eq!(expected_rendering, buf.as_str());
    }

    macro_rules! test_report {
        ($test_name:ident, $program:expr, $output:expr) => {
            #[test]
            fn $test_name() {
                __new_report_problem_as(std::stringify!($test_name), $program, $output)
            }
        };
    }

    fn human_readable(str: &str) -> String {
        str.replace(ANSI_STYLE_CODES.red, "<red>")
            .replace(ANSI_STYLE_CODES.white, "<white>")
            .replace(ANSI_STYLE_CODES.blue, "<blue>")
            .replace(ANSI_STYLE_CODES.yellow, "<yellow>")
            .replace(ANSI_STYLE_CODES.green, "<green>")
            .replace(ANSI_STYLE_CODES.cyan, "<cyan>")
            .replace(ANSI_STYLE_CODES.magenta, "<magenta>")
            .replace(ANSI_STYLE_CODES.reset, "<reset>")
            .replace(ANSI_STYLE_CODES.bold, "<bold>")
            .replace(ANSI_STYLE_CODES.underline, "<underline>")
    }

    #[test]
    fn value_not_exposed() {
        report_problem_as(
            indoc!(
                r#"
                List.isempty 1 2
            "#
            ),
            indoc!(
                r#"
                ── NOT EXPOSED ─────────────────────────────────────────── /code/proj/Main.roc ─

                The List module does not expose `isempty`:

                1│  List.isempty 1 2
                    ^^^^^^^^^^^^

                Did you mean one of these?

                    List.isEmpty
                    List.set
                    List.get
                    List.keepIf
                "#
            ),
        )
    }

    #[test]
    fn report_unused_def() {
        report_problem_as(
            indoc!(
                r#"
                x = 1
                y = 2

                x
            "#
            ),
            indoc!(
                r#"
                ── UNUSED DEFINITION ───────────────────────────────────── /code/proj/Main.roc ─

                `y` is not used anywhere in your code.

                2│  y = 2
                    ^

                If you didn't intend on using `y` then remove it so future readers of
                your code don't wonder why it is there.
                "#
            ),
        )
    }

    #[test]
    fn report_shadowing() {
        report_problem_as(
            indoc!(
                r#"
               i = 1

               s = \i ->
                   i + 1

               s i
           "#
            ),
            indoc!(
                r#"
                ── DUPLICATE NAME ──────────────────────────────────────── /code/proj/Main.roc ─

                The `i` name is first defined here:

                1│  i = 1
                    ^

                But then it's defined a second time here:

                3│  s = \i ->
                         ^

                Since these variables have the same name, it's easy to use the wrong
                one on accident. Give one of them a new name.
                "#
            ),
        )
    }

    #[test]
    fn report_shadowing_in_annotation() {
        report_problem_as(
            indoc!(
                r#"
                Booly : [Yes, No]

                Booly : [Yes, No, Maybe]

                x : List Booly
                x = []

                x
           "#
            ),
            indoc!(
                r#"
                ── DUPLICATE NAME ──────────────────────────────────────── /code/proj/Main.roc ─

                The `Booly` name is first defined here:

                1│  Booly : [Yes, No]
                    ^^^^^^^^^^^^^^^^^

                But then it's defined a second time here:

                3│  Booly : [Yes, No, Maybe]
                    ^^^^^^^^^^^^^^^^^^^^^^^^

                Since these aliases have the same name, it's easy to use the wrong one
                on accident. Give one of them a new name.
                "#
            ),
        )
    }

    // #[test]
    // fn report_multi_line_shadowing_in_annotation() {
    //     report_problem_as(
    //         indoc!(
    //             r#"
    //             Booly :
    //                 [
    //                     Yes,
    //                     No
    //                ]
    //
    //             Booly :
    //                 [
    //                     Yes,
    //                     No,
    //                     Maybe
    //                ]
    //
    //             x =
    //                 No
    //
    //             x
    //        "#
    //         ),
    //         indoc!(
    //             r#"
    //             Booly is first defined here:
    //
    //             1│> Booly :
    //             2│>    [
    //             3│>        Yes,
    //             4│>        No
    //             5│>   ]
    //
    //             But then it's defined a second time here:
    //
    //             7 │> Booly :
    //             8 │>    [
    //             9 │>        Yes,
    //             10│>        No,
    //             11│>        Maybe
    //             12│>   ]
    //
    //             Since these variables have the same name, it's easy to use the wrong one on accident. Give one of them a new name."#
    //         ),
    //     )
    // }

    // #[test]
    // fn report_unsupported_top_level_def() {
    //     report_problem_as(
    //         indoc!(
    //             r#"
    //             x = 1
    //
    //             5 = 2 + 1
    //
    //             x
    //         "#
    //         ),
    //         indoc!(r#"     "#),
    //     )
    // }

    #[test]
    fn report_precedence_problem_single_line() {
        report_problem_as(
            indoc!(
                r#"x = 1
                y =
                    if selectedId != thisId == adminsId then
                        4

                    else
                        5

                { x, y }
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                Using != and == together requires parentheses, to clarify how they
                should be grouped.

                3│      if selectedId != thisId == adminsId then
                           ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
                "#
            ),
        )
    }

    #[test]
    fn unrecognized_name() {
        report_problem_as(
            indoc!(
                r#"
                foo = { x: 1 == 1, y: 0x4 }

                baz = 3

                main : Str
                main =
                    when foo.y is
                        4 -> bar baz "yay"
                        _ -> "nay"

                main
                "#
            ),
            indoc!(
                r#"
                ── UNRECOGNIZED NAME ───────────────────────────────────── /code/proj/Main.roc ─

                Nothing is named `bar` in this scope.

                8│          4 -> bar baz "yay"
                                 ^^^

                Did you mean one of these?

                    baz
                    Str
                    Err
                    main
                "#
            ),
        )
    }

    #[test]
    fn lowercase_primitive_tag_bool() {
        report_problem_as(
            indoc!(
                r#"
                if true then 1 else 2
                "#
            ),
            indoc!(
                r#"
                ── UNRECOGNIZED NAME ───────────────────────────────────── /code/proj/Main.roc ─

                Nothing is named `true` in this scope.

                1│  if true then 1 else 2
                       ^^^^

                Did you mean one of these?

                    True
                    Str
                    Err
                    List
                "#
            ),
        )
    }

    #[test]
    fn report_precedence_problem_multiline() {
        report_problem_as(
            indoc!(
                r#"
                if
                    1
                        == 2
                        == 3
                then
                    2

                else
                    3
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                Using more than one == like this requires parentheses, to clarify how
                things should be grouped.

                2│>      1
                3│>          == 2
                4│>          == 3
                "#
            ),
        )
    }

    #[test]
    fn unused_arg_and_unused_def() {
        report_problem_as(
            indoc!(
                r#"
                 y = 9

                 box = \class, htmlChildren ->
                     div [class] []

                 div = \_, _ -> 4

                 box "wizard" []
             "#
            ),
            indoc!(
                r#"
                 ── UNUSED ARGUMENT ─────────────────────────────────────── /code/proj/Main.roc ─

                 `box` doesn't use `htmlChildren`.

                 3│  box = \class, htmlChildren ->
                                   ^^^^^^^^^^^^

                 If you don't need `htmlChildren`, then you can just remove it. However,
                 if you really do need `htmlChildren` as an argument of `box`, prefix it
                 with an underscore, like this: "_`htmlChildren`". Adding an underscore
                 at the start of a variable name is a way of saying that the variable
                 is not used.

                 ── UNUSED DEFINITION ───────────────────────────────────── /code/proj/Main.roc ─

                 `y` is not used anywhere in your code.

                 1│  y = 9
                     ^

                 If you didn't intend on using `y` then remove it so future readers of
                 your code don't wonder why it is there.
                "#
            ),
        );
    }

    #[test]
    fn report_value_color() {
        let src: &str = indoc!(
            r#"
                activityIndicatorLarge = div

                view activityIndicatorLarge
            "#
        );

        let arena = Bump::new();
        let (_type_problems, _can_problems, home, interns) =
            infer_expr_help(&arena, src).expect("parse error");

        let mut buf = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();

        let alloc = RocDocAllocator::new(&src_lines, home, &interns);

        let symbol = interns.symbol(test_home(), "activityIndicatorLarge".into());

        to_simple_report(alloc.symbol_unqualified(symbol)).render_color_terminal(
            &mut buf,
            &alloc,
            &DEFAULT_PALETTE,
        );

        assert_eq!(human_readable(&buf), "<blue>activityIndicatorLarge<reset>");
    }

    #[test]
    fn report_module_color() {
        let src: &str = indoc!(
            r#"
                x = 1
                y = 2

                x
            "#
        );

        let arena = Bump::new();
        let (_type_problems, _can_problems, home, mut interns) =
            infer_expr_help(&arena, src).expect("parse error");

        let mut buf = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();
        let module_id = interns.module_id(&"Util.Int".into());

        let alloc = RocDocAllocator::new(&src_lines, home, &interns);
        to_simple_report(alloc.module(module_id)).render_color_terminal(
            &mut buf,
            &alloc,
            &DEFAULT_PALETTE,
        );

        assert_eq!(human_readable(&buf), "<green>Util.Int<reset>");
    }

    #[test]
    fn report_region_in_color() {
        color_report_problem_as(
            indoc!(
                r#"
                    isDisabled = \user -> user.isAdmin

                    theAdmin
                        |> isDisabled
                "#
            ),
            indoc!(
                r#"
                <cyan>── UNRECOGNIZED NAME ───────────────────────────────────── /code/proj/Main.roc ─<reset>

                Nothing is named `theAdmin` in this scope.

                <cyan>3<reset><cyan>│<reset>  <white>theAdmin<reset>
                    <red>^^^^^^^^<reset>

                Did you mean one of these?

                    Set
                    List
                    True
                    Box
                "#
            ),
        );
    }

    // #[test]
    // fn shadowing_type_alias() {
    //     report_problem_as(
    //         indoc!(
    //             r#"
    //                 foo : I64 as I64
    //                 foo = 42

    //                 foo
    //                 "#
    //         ),
    //         indoc!(
    //             r#"
    //                 You cannot mix (!=) and (==) without parentheses

    //                 3│     if selectedId != thisId == adminsId then
    //                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    //                 "#
    //         ),
    //     )
    // }

    // #[test]
    // fn invalid_as_type_alias() {
    //     report_problem_as(
    //         indoc!(
    //             r#"
    //                 foo : I64 as a
    //                 foo = 42

    //                 foo
    //                 "#
    //         ),
    //         indoc!(
    //             r#"
    //                 You cannot mix (!=) and (==) without parentheses

    //                 3│     if selectedId != thisId == adminsId then
    //                            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    //                 "#
    //         ),
    //     )
    // }

    #[test]
    fn if_condition_not_bool() {
        report_problem_as(
            indoc!(
                r#"
                if "foo" then 2 else 3
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This `if` condition needs to be a Bool:

                1│  if "foo" then 2 else 3
                       ^^^^^

                Right now it’s a string of type:

                    Str

                But I need every `if` condition to evaluate to a Bool—either `True` or
                `False`.
                "#
            ),
        )
    }

    #[test]
    fn when_if_guard() {
        report_problem_as(
            indoc!(
                r#"
                when 1 is
                    2 if 1 -> 0x0
                    _ -> 0x1
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This `if` guard condition needs to be a Bool:

                1│   when 1 is
                2│>      2 if 1 -> 0x0
                3│       _ -> 0x1

                Right now it’s a number of type:

                    Num a

                But I need every `if` guard condition to evaluate to a Bool—either
                `True` or `False`.
                "#
            ),
        )
    }

    #[test]
    fn if_2_branch_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                if True then 2 else "foo"
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This `if` has an `else` branch with a different type from its `then` branch:

                1│  if True then 2 else "foo"
                                        ^^^^^

                The `else` branch is a string of type:

                    Str

                but the `then` branch has the type:

                    Num a

                All branches in an `if` must have the same type!
                "#
            ),
        )
    }

    #[test]
    fn if_3_branch_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                 if True then 2 else if False then 2 else "foo"
                 "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 3rd branch of this `if` does not match all the previous branches:

                1│  if True then 2 else if False then 2 else "foo"
                                                             ^^^^^

                The 3rd branch is a string of type:

                    Str

                But all the previous branches have type:

                    Num a

                All branches in an `if` must have the same type!
                "#
            ),
        )
    }

    #[test]
    fn when_branch_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                when 1 is
                    2 -> "foo"
                    3 -> {}
                    _ -> ""
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd branch of this `when` does not match all the previous branches:

                1│   when 1 is
                2│       2 -> "foo"
                3│>      3 -> {}
                4│       _ -> ""

                The 2nd branch is a record of type:

                    {}

                But all the previous branches have type:

                    Str

                All branches of a `when` must have the same type!
                "#
            ),
        )
    }

    #[test]
    fn elem_in_list() {
        report_problem_as(
            indoc!(
                r#"
                [1, 3, "foo"]
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This list contains elements with different types:

                1│  [1, 3, "foo"]
                           ^^^^^

                Its 3rd element is a string of type:

                    Str

                However, the preceding elements in the list all have the type:

                    Num a

                Every element in a list must have the same type!
                "#
            ),
        )
    }

    #[test]
    fn record_update_value() {
        report_problem_as(
            indoc!(
                r#"
                x : { foo : {} }
                x = { foo: {} }

                { x & foo: "bar" }
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                I cannot update the `.foo` field like this:

                4│  { x & foo: "bar" }
                               ^^^^^

                You are trying to update `.foo` to be a string of type:

                    Str

                But it should be:

                    {}

                Record update syntax does not allow you to change the type of fields.
                You can achieve that with record literal syntax.
                "#
            ),
        )
    }

    #[test]
    fn circular_type() {
        report_problem_as(
            indoc!(
                r#"
                f = \g -> g g

                f
                "#
            ),
            indoc!(
                r#"
                ── CIRCULAR TYPE ───────────────────────────────────────── /code/proj/Main.roc ─

                I'm inferring a weird self-referential type for `g`:

                1│  f = \g -> g g
                         ^

                Here is my best effort at writing down the type. You will see ∞ for
                parts of the type that repeat something already printed out
                infinitely.

                    ∞ -> a
                "#
            ),
        )
    }

    #[test]
    fn polymorphic_recursion() {
        report_problem_as(
            indoc!(
                r#"
                f = \x -> f [x]

                f
                "#
            ),
            indoc!(
                r#"
                ── CIRCULAR TYPE ───────────────────────────────────────── /code/proj/Main.roc ─

                I'm inferring a weird self-referential type for `f`:

                1│  f = \x -> f [x]
                    ^

                Here is my best effort at writing down the type. You will see ∞ for
                parts of the type that repeat something already printed out
                infinitely.

                    List ∞ -> a
                "#
            ),
        )
    }

    #[test]
    fn polymorphic_mutual_recursion() {
        report_problem_as(
            indoc!(
                r#"
                f = \x -> g x
                g = \x -> f [x]

                f
                "#
            ),
            indoc!(
                r#"
                ── CIRCULAR TYPE ───────────────────────────────────────── /code/proj/Main.roc ─

                I'm inferring a weird self-referential type for `f`:

                1│  f = \x -> g x
                    ^

                Here is my best effort at writing down the type. You will see ∞ for
                parts of the type that repeat something already printed out
                infinitely.

                    List ∞ -> a

                ── CIRCULAR TYPE ───────────────────────────────────────── /code/proj/Main.roc ─

                I'm inferring a weird self-referential type for `g`:

                2│  g = \x -> f [x]
                    ^

                Here is my best effort at writing down the type. You will see ∞ for
                parts of the type that repeat something already printed out
                infinitely.

                    List ∞ -> a
                "#
            ),
        )
    }

    #[test]
    fn polymorphic_mutual_recursion_annotated() {
        report_problem_as(
            indoc!(
                r#"
                f : a -> List a
                f = \x -> g x
                g = \x -> f [x]

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This expression is used in an unexpected way:

                2│  f = \x -> g x
                              ^^^

                This `g` call produces:

                    List List a

                But you are trying to use it as:

                    List a

                Tip: The type annotation uses the type variable `a` to say that this
                definition can produce any type of value. But in the body I see that
                it will only produce a `List` value of a single specific type. Maybe
                change the type annotation to be more specific? Maybe change the code
                to be more general?
                "#
            ),
        )
    }

    #[test]
    fn polymorphic_mutual_recursion_dually_annotated_lie() {
        report_problem_as(
            indoc!(
                r#"
                f : a -> List a
                f = \x -> g x
                g : b -> List b
                g = \x -> f [x]

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This expression is used in an unexpected way:

                4│  g = \x -> f [x]
                              ^^^^^

                This `f` call produces:

                    List List b

                But you are trying to use it as:

                    List b

                Tip: The type annotation uses the type variable `b` to say that this
                definition can produce any type of value. But in the body I see that
                it will only produce a `List` value of a single specific type. Maybe
                change the type annotation to be more specific? Maybe change the code
                to be more general?
                "#
            ),
        )
    }

    #[test]
    fn record_field_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                bar = { bar : 0x3 }

                f : { foo : Num.Int * } -> [Yes, No]
                f = \_ -> Yes

                f bar
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 1st argument to `f` is not what I expect:

                6│  f bar
                      ^^^

                This `bar` value is a:

                    { bar : Int a }

                But `f` needs the 1st argument to be:

                    { foo : Int * }

                Tip: Seems like a record field typo. Maybe `bar` should be `foo`?

                Tip: Can more type annotations be added? Type annotations always help
                me give more specific messages, and I think they could help a lot in
                this case
                "#
            ),
        )
    }

    #[test]
    fn tag_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                f : [Red, Green] -> [Yes, No]
                f = \_ -> Yes

                f Blue
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 1st argument to `f` is not what I expect:

                4│  f Blue
                      ^^^^

                This `Blue` tag has the type:

                    [Blue]a

                But `f` needs the 1st argument to be:

                    [Green, Red]

                Tip: Seems like a tag typo. Maybe `Blue` should be `Red`?

                Tip: Can more type annotations be added? Type annotations always help
                me give more specific messages, and I think they could help a lot in
                this case
                "#
            ),
        )
    }

    #[test]
    fn tag_with_arguments_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                f : [Red (Num.Int *), Green Str] -> Str
                f = \_ -> "yes"

                f (Blue 3.14)
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 1st argument to `f` is not what I expect:

                4│  f (Blue 3.14)
                       ^^^^^^^^^

                This `Blue` tag application has the type:

                    [Blue (Frac a)]b

                But `f` needs the 1st argument to be:

                    [Green Str, Red (Int *)]

                Tip: Seems like a tag typo. Maybe `Blue` should be `Red`?

                Tip: Can more type annotations be added? Type annotations always help
                me give more specific messages, and I think they could help a lot in
                this case
                "#
            ),
        )
    }

    #[test]
    fn from_annotation_if() {
        report_problem_as(
            indoc!(
                r#"
                x : Num.Int *
                x = if True then 3.14 else 4

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the `then` branch of this `if` expression:

                1│  x : Num.Int *
                2│  x = if True then 3.14 else 4
                                     ^^^^

                The 1st branch is a frac of type:

                    Frac a

                But the type annotation on `x` says it should be:

                    Int *

                Tip: You can convert between Int and Frac using functions like
                `Num.toFrac` and `Num.round`.
                "#
            ),
        )
    }

    #[test]
    fn from_annotation_when() {
        report_problem_as(
            indoc!(
                r#"
                x : Num.Int *
                x =
                    when True is
                        _ -> 3.14

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `x` definition:

                1│   x : Num.Int *
                2│   x =
                3│>      when True is
                4│>          _ -> 3.14

                This `when` expression produces:

                    Frac a

                But the type annotation on `x` says it should be:

                    Int *

                Tip: You can convert between Int and Frac using functions like
                `Num.toFrac` and `Num.round`.
                "#
            ),
        )
    }

    #[test]
    fn from_annotation_function() {
        report_problem_as(
            indoc!(
                r#"
                x : Num.Int * -> Num.Int *
                x = \_ -> 3.14

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `x` definition:

                1│  x : Num.Int * -> Num.Int *
                2│  x = \_ -> 3.14
                              ^^^^

                The body is a frac of type:

                    Frac a

                But the type annotation on `x` says it should be:

                    Int *

                Tip: You can convert between Int and Frac using functions like
                `Num.toFrac` and `Num.round`.
                "#
            ),
        )
    }

    #[test]
    fn fncall_value() {
        report_problem_as(
            indoc!(
                r#"
                x : Num.I64
                x = 42

                x 3
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY ARGS ───────────────────────────────────────── /code/proj/Main.roc ─

                The `x` value is not a function, but it was given 1 argument:

                4│  x 3
                    ^

                Are there any missing commas? Or missing parentheses?
                "#
            ),
        )
    }

    #[test]
    fn fncall_overapplied() {
        report_problem_as(
            indoc!(
                r#"
                f : Num.I64 -> Num.I64
                f = \_ -> 42

                f 1 2
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY ARGS ───────────────────────────────────────── /code/proj/Main.roc ─

                The `f` function expects 1 argument, but it got 2 instead:

                4│  f 1 2
                    ^

                Are there any missing commas? Or missing parentheses?
                "#
            ),
        )
    }

    #[test]
    fn fncall_underapplied() {
        report_problem_as(
            indoc!(
                r#"
                f : Num.I64, Num.I64 -> Num.I64
                f = \_, _ -> 42

                f 1
                "#
            ),
            indoc!(
                r#"
                ── TOO FEW ARGS ────────────────────────────────────────── /code/proj/Main.roc ─

                The `f` function expects 2 arguments, but it got only 1:

                4│  f 1
                    ^

                Roc does not allow functions to be partially applied. Use a closure to
                make partial application explicit.
                "#
            ),
        )
    }

    #[test]
    fn pattern_when_condition() {
        report_problem_as(
            indoc!(
                r#"
                when 1 is
                    {} -> 42
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The branches of this `when` expression don't match the condition:

                1│>  when 1 is
                2│       {} -> 42

                The `when` condition is a number of type:

                    Num a

                But the branch patterns have type:

                    {}a

                The branches must be cases of the `when` condition's type!
                "#
            ),
        )
    }

    #[test]
    fn pattern_when_pattern() {
        report_problem_as(
            indoc!(
                r#"
                when 1 is
                    2 -> 3
                    {} -> 42
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd pattern in this `when` does not match the previous ones:

                3│      {} -> 42
                        ^^

                The 2nd pattern is trying to match record values of type:

                    {}a

                But all the previous branches match:

                    Num a
                "#
            ),
        )
    }

    #[test]
    fn pattern_guard_mismatch_alias() {
        report_problem_as(
            indoc!(
                r#"
                 when { foo: 1 } is
                     { foo: True } -> 42
                 "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The branches of this `when` expression don't match the condition:

                1│>  when { foo: 1 } is
                2│       { foo: True } -> 42

                The `when` condition is a record of type:

                    { foo : Num a }

                But the branch patterns have type:

                    { foo : [True] }

                The branches must be cases of the `when` condition's type!
                "#
            ),
        )
    }

    #[test]
    fn pattern_guard_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                 when { foo: "" } is
                     { foo: True } -> 42
                 "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The branches of this `when` expression don't match the condition:

                1│>  when { foo: "" } is
                2│       { foo: True } -> 42

                The `when` condition is a record of type:

                    { foo : Str }

                But the branch patterns have type:

                    { foo : [True] }

                The branches must be cases of the `when` condition's type!
                "#
            ),
        )
    }

    #[test]
    fn pattern_guard_does_not_bind_label() {
        // needs some improvement, but the principle works
        report_problem_as(
            indoc!(
                r#"
                 when { foo: 1 } is
                     { foo: _ } -> foo
                 "#
            ),
            indoc!(
                r#"
                ── UNRECOGNIZED NAME ───────────────────────────────────── /code/proj/Main.roc ─

                Nothing is named `foo` in this scope.

                2│      { foo: _ } -> foo
                                      ^^^

                Did you mean one of these?

                    Box
                    Set
                    Str
                    Ok
                "#
            ),
        )
    }

    #[test]
    fn pattern_guard_can_be_shadowed_above() {
        report_problem_as(
            indoc!(
                r#"
                foo = 3

                when { foo: 1 } is
                    { foo: 2 } -> foo
                    _ -> foo
                 "#
            ),
            // should give no error
            "",
        )
    }

    #[test]
    fn pattern_guard_can_be_shadowed_below() {
        report_problem_as(
            indoc!(
                r#"
                when { foo: 1 } is
                    { foo: 2 } ->
                        foo = 3

                        foo
                    _ -> 3
                 "#
            ),
            // should give no error
            "",
        )
    }

    #[test]
    fn pattern_or_pattern_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                when { foo: 1 } is
                    {} | 1 -> 3
                "#
            ),
            // Just putting this here. We should probably handle or-patterns better
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd pattern in this branch does not match the previous ones:

                2│      {} | 1 -> 3
                             ^

                The 2nd pattern is trying to match numbers:

                    Num a

                But all the previous branches match:

                    {}a
                "#
            ),
        )
    }

    #[test]
    fn pattern_let_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                (Foo x) = 42

                x
                "#
            ),
            // Maybe this should specifically say the pattern doesn't work?
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This expression is used in an unexpected way:

                1│  (Foo x) = 42
                              ^^

                It is a number of type:

                    Num a

                But you are trying to use it as:

                    [Foo a]
                "#
            ),
        )
    }

    #[test]
    fn from_annotation_complex_pattern() {
        report_problem_as(
            indoc!(
                r#"
                { x } : { x : Num.Int * }
                { x } = { x: 4.0 }

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of this definition:

                1│  { x } : { x : Num.Int * }
                2│  { x } = { x: 4.0 }
                            ^^^^^^^^^^

                The body is a record of type:

                    { x : Frac a }

                But the type annotation says it should be:

                    { x : Int * }

                Tip: You can convert between Int and Frac using functions like
                `Num.toFrac` and `Num.round`.
                "#
            ),
        )
    }

    #[test]
    fn malformed_int_pattern() {
        report_problem_as(
            indoc!(
                r#"
                when 1 is
                    100A -> 3
                    _ -> 4
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This integer pattern is malformed:

                2│      100A -> 3
                        ^^^^

                Tip: Learn more about number literals at TODO
                "#
            ),
        )
    }

    #[test]
    fn malformed_float_pattern() {
        report_problem_as(
            indoc!(
                r#"
                when 1 is
                    2.X -> 3
                    _ -> 4
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This float pattern is malformed:

                2│      2.X -> 3
                        ^^^

                Tip: Learn more about number literals at TODO
                "#
            ),
        )
    }

    #[test]
    fn malformed_hex_pattern() {
        report_problem_as(
            indoc!(
                r#"
                when 1 is
                    0xZ -> 3
                    _ -> 4
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This hex integer pattern is malformed:

                2│      0xZ -> 3
                        ^^^

                Tip: Learn more about number literals at TODO
                "#
            ),
        )
    }

    #[test]
    fn malformed_oct_pattern() {
        report_problem_as(
            indoc!(
                r#"
                when 1 is
                    0o9 -> 3
                    _ -> 4
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This octal integer pattern is malformed:

                2│      0o9 -> 3
                        ^^^

                Tip: Learn more about number literals at TODO
                "#
            ),
        )
    }

    #[test]
    fn malformed_bin_pattern() {
        report_problem_as(
            indoc!(
                r#"
                when 1 is
                    0b4 -> 3
                    _ -> 4
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This binary integer pattern is malformed:

                2│      0b4 -> 3
                        ^^^

                Tip: Learn more about number literals at TODO
                "#
            ),
        )
    }

    #[test]
    fn missing_fields() {
        report_problem_as(
            indoc!(
                r#"
                x : { a : Num.Int *, b : Num.Frac *, c : Str }
                x = { b: 4.0 }

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `x` definition:

                1│  x : { a : Num.Int *, b : Num.Frac *, c : Str }
                2│  x = { b: 4.0 }
                        ^^^^^^^^^^

                The body is a record of type:

                    { b : Frac a }

                But the type annotation on `x` says it should be:

                    { a : Int *, b : Frac *, c : Str }

                Tip: Looks like the c and a fields are missing.
                "#
            ),
        )
    }

    #[test]
    fn bad_double_rigid() {
        // this previously reported the message below, not sure which is better
        //
        //                Something is off with the body of the `f` definition:
        //
        //                1│ f : a, b -> a
        //                2│ f = \x, y -> if True then x else y
        //                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
        //
        //                The body is an anonymous function of type:
        //
        //                    a, a -> a
        //
        //                But the type annotation on `f` says it should be:
        //
        //                    a, b -> a
        report_problem_as(
            indoc!(
                r#"
                f : a, b -> a
                f = \x, y -> if True then x else y

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the `else` branch of this `if` expression:

                1│  f : a, b -> a
                2│  f = \x, y -> if True then x else y
                                                     ^

                This `y` value is a:

                    b

                But the type annotation on `f` says it should be:

                    a

                Tip: Your type annotation uses `b` and `a` as separate type variables.
                Your code seems to be saying they are the same though. Maybe they
                should be the same in your type annotation? Maybe your code uses them
                in a weird way?
                "#
            ),
        )
    }

    #[test]
    fn bad_rigid_function() {
        report_problem_as(
            indoc!(
                r#"
                f : Str -> msg
                f = \_ -> Foo

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `f` definition:

                1│  f : Str -> msg
                2│  f = \_ -> Foo
                              ^^^

                This `Foo` tag has the type:

                    [Foo]a

                But the type annotation on `f` says it should be:

                    msg

                Tip: The type annotation uses the type variable `msg` to say that this
                definition can produce any type of value. But in the body I see that
                it will only produce a tag value of a single specific type. Maybe
                change the type annotation to be more specific? Maybe change the code
                to be more general?
                "#
            ),
        )
    }

    #[test]
    fn bad_rigid_value() {
        report_problem_as(
            indoc!(
                r#"
                f : msg
                f = 0x3

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `f` definition:

                1│  f : msg
                2│  f = 0x3
                        ^^^

                The body is an integer of type:

                    Int a

                But the type annotation on `f` says it should be:

                    msg

                Tip: The type annotation uses the type variable `msg` to say that this
                definition can produce any type of value. But in the body I see that
                it will only produce a `Int` value of a single specific type. Maybe
                change the type annotation to be more specific? Maybe change the code
                to be more general?
                "#
            ),
        )
    }

    #[test]
    fn typo_lowercase_ok() {
        // TODO improve tag suggestions
        report_problem_as(
            indoc!(
                r#"
                f : Str -> [Ok Num.I64, InvalidFoo]
                f = \_ -> ok 4

                f
                "#
            ),
            indoc!(
                r#"
                ── UNRECOGNIZED NAME ───────────────────────────────────── /code/proj/Main.roc ─

                Nothing is named `ok` in this scope.

                2│  f = \_ -> ok 4
                              ^^

                Did you mean one of these?

                    Ok
                    f
                    Box
                    Set
               "#
            ),
        )
    }

    #[test]
    fn typo_uppercase_ok() {
        // these error messages seem pretty helpful
        report_problem_as(
            indoc!(
                r#"
                f : Str -> Num.I64
                f = \_ ->
                    ok = 3

                    Ok

                f
                "#
            ),
            indoc!(
                r#"
                ── UNUSED DEFINITION ───────────────────────────────────── /code/proj/Main.roc ─

                `ok` is not used anywhere in your code.

                3│      ok = 3
                        ^^

                If you didn't intend on using `ok` then remove it so future readers of
                your code don't wonder why it is there.

                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `f` definition:

                1│  f : Str -> Num.I64
                2│  f = \_ ->
                3│      ok = 3
                4│
                5│      Ok
                        ^^

                This `Ok` tag has the type:

                    [Ok]a

                But the type annotation on `f` says it should be:

                    I64
                "#
            ),
        )
    }

    #[test]
    fn circular_definition_self() {
        // invalid recursion
        report_problem_as(
            indoc!(
                r#"
                f = f

                f
                "#
            ),
            indoc!(
                r#"
                ── CIRCULAR DEFINITION ─────────────────────────────────── /code/proj/Main.roc ─

                The `f` value is defined directly in terms of itself, causing an
                infinite loop.
                "#
            ),
        )
    }

    #[test]
    fn circular_definition() {
        // invalid mutual recursion
        report_problem_as(
            indoc!(
                r#"
                foo = bar

                bar = foo

                foo
                "#
            ),
            indoc!(
                r#"
                ── CIRCULAR DEFINITION ─────────────────────────────────── /code/proj/Main.roc ─

                The `foo` definition is causing a very tricky infinite loop:

                1│  foo = bar
                    ^^^

                The `foo` value depends on itself through the following chain of
                definitions:

                    ┌─────┐
                    │     foo
                    │     ↓
                    │     bar
                    └─────┘
                "#
            ),
        )
    }

    #[test]
    fn update_empty_record() {
        report_problem_as(
            indoc!(
                r#"
                x = {}

                { x & foo: 3 }
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This `x` record doesn’t have a `foo` field:

                3│  { x & foo: 3 }
                          ^^^^^^

                In fact, `x` is a record with no fields at all!
                "#
            ),
        )
    }

    #[test]
    fn update_record() {
        report_problem_as(
            indoc!(
                r#"
                x = { fo: 3, bar: 4 }

                { x & foo: 3 }
                "#
            ),
            // TODO also suggest fields with the correct type
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This `x` record doesn’t have a `foo` field:

                3│  { x & foo: 3 }
                          ^^^^^^

                There may be a typo. These `x` fields are the most similar:

                    {
                        fo : Num b,
                        bar : Num a,
                    }

                Maybe `foo:` should be `fo:` instead?
                "#
            ),
        )
    }

    #[test]
    fn update_record_ext() {
        report_problem_as(
            indoc!(
                r#"
                f : { fo: Num.I64 }ext -> Num.I64
                f = \r ->
                    r2 = { r & foo: r.fo }

                    r2.fo

                f
                "#
            ),
            // TODO also suggest fields with the correct type
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This `r` record doesn’t have a `foo` field:

                3│      r2 = { r & foo: r.fo }
                                   ^^^^^^^^^

                There may be a typo. These `r` fields are the most similar:

                    {
                        fo : I64,
                    }ext

                Maybe `foo:` should be `fo:` instead?
                "#
            ),
        )
    }

    #[test]
    fn update_record_snippet() {
        report_problem_as(
            indoc!(
                r#"
                x = { fo: 3, bar: 4, baz: 3, spam: 42, foobar: 3 }

                { x & foo: 3 }
                "#
            ),
            // TODO also suggest fields with the correct type
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This `x` record doesn’t have a `foo` field:

                3│  { x & foo: 3 }
                          ^^^^^^

                There may be a typo. These `x` fields are the most similar:

                    {
                        fo : Num c,
                        foobar : Num d,
                        bar : Num a,
                        baz : Num b,
                        …
                    }

                Maybe `foo:` should be `fo:` instead?
                "#
            ),
        )
    }

    #[test]
    fn plus_on_str() {
        report_problem_as(
            indoc!(
                r#"
                0x4 + "foo"
                "#
            ),
            // TODO also suggest fields with the correct type
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd argument to `add` is not what I expect:

                1│  0x4 + "foo"
                          ^^^^^

                This argument is a string of type:

                    Str

                But `add` needs the 2nd argument to be:

                    Num (Integer a)
                "#
            ),
        )
    }

    #[test]
    fn int_frac() {
        report_problem_as(
            indoc!(
                r#"
                0x4 + 3.14
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd argument to `add` is not what I expect:

                1│  0x4 + 3.14
                          ^^^^

                This argument is a frac of type:

                    Frac a

                But `add` needs the 2nd argument to be:

                    Num (Integer a)

                Tip: You can convert between Int and Frac using functions like
                `Num.toFrac` and `Num.round`.
                "#
            ),
        )
    }

    #[test]
    fn boolean_tag() {
        report_problem_as(
            indoc!(
                r#"
                42 + True
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd argument to `add` is not what I expect:

                1│  42 + True
                         ^^^^

                This `True` boolean has the type:

                    [True]a

                But `add` needs the 2nd argument to be:

                    Num a
                "#
            ),
        )
    }

    #[test]
    fn tag_missing() {
        report_problem_as(
            indoc!(
                r#"
                f : [A] -> [A, B]
                f = \a -> a

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `f` definition:

                1│  f : [A] -> [A, B]
                2│  f = \a -> a
                              ^

                This `a` value is a:

                    [A]

                But the type annotation on `f` says it should be:

                    [A, B]

                Tip: Looks like a closed tag union does not have the `B` tag.

                Tip: Closed tag unions can't grow, because that might change the size
                in memory. Can you use an open tag union?
                "#
            ),
        )
    }

    #[test]
    fn tags_missing() {
        report_problem_as(
            indoc!(
                r#"
                f : [A] -> [A, B, C]
                f = \a -> a

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `f` definition:

                1│  f : [A] -> [A, B, C]
                2│  f = \a -> a
                              ^

                This `a` value is a:

                    [A]

                But the type annotation on `f` says it should be:

                    [A, B, C]

                Tip: Looks like a closed tag union does not have the `B` and `C` tags.

                Tip: Closed tag unions can't grow, because that might change the size
                in memory. Can you use an open tag union?
                "#
            ),
        )
    }

    #[test]
    fn patterns_fn_not_exhaustive() {
        report_problem_as(
            indoc!(
                r#"
                Either : [Left {}, Right Str]

                x : Either
                x = Left {}

                f : Either -> {}
                f = \Left v -> v

                f x
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────── /code/proj/Main.roc ─

                This pattern does not cover all the possibilities:

                7│  f = \Left v -> v
                         ^^^^^^

                Other possibilities include:

                    Right _

                I would have to crash if I saw one of those! So rather than pattern
                matching in function arguments, put a `when` in the function body to
                account for all possibilities.
                "#
            ),
        )
    }

    #[test]
    fn patterns_let_not_exhaustive() {
        report_problem_as(
            indoc!(
                r#"
                x : [Left {}, Right Str]
                x = Left {}


                (Left y) = x

                y
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This expression is used in an unexpected way:

                5│  (Left y) = x
                               ^

                This `x` value is a:

                    [Left {}, Right Str]

                But you are trying to use it as:

                    [Left a]

                Tip: Looks like a closed tag union does not have the `Right` tag.

                Tip: Closed tag unions can't grow, because that might change the size
                in memory. Can you use an open tag union?
                "#
            ),
        )
    }

    #[test]
    fn patterns_when_not_exhaustive() {
        report_problem_as(
            indoc!(
                r#"
                when 0x1 is
                    2 -> 0x3
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────── /code/proj/Main.roc ─

                This `when` does not cover all the possibilities:

                1│>  when 0x1 is
                2│>      2 -> 0x3

                Other possibilities include:

                    _

                I would have to crash if I saw one of those! Add branches for them!
                "#
            ),
        )
    }

    #[test]
    fn patterns_bool_not_exhaustive() {
        report_problem_as(
            indoc!(
                r#"
                x : [Red, Green]
                x = Green

                when x is
                    Red -> 3
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────── /code/proj/Main.roc ─

                This `when` does not cover all the possibilities:

                4│>  when x is
                5│>      Red -> 3

                Other possibilities include:

                    Green

                I would have to crash if I saw one of those! Add branches for them!
                "#
            ),
        )
    }

    #[test]
    fn patterns_enum_not_exhaustive() {
        report_problem_as(
            indoc!(
                r#"
                x : [Red, Green, Blue]
                x = Red

                when x is
                    Red -> 0
                    Green -> 1
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────── /code/proj/Main.roc ─

                This `when` does not cover all the possibilities:

                4│>  when x is
                5│>      Red -> 0
                6│>      Green -> 1

                Other possibilities include:

                    Blue

                I would have to crash if I saw one of those! Add branches for them!
                "#
            ),
        )
    }

    #[test]
    fn patterns_remote_data_not_exhaustive() {
        report_problem_as(
            indoc!(
                r#"
                RemoteData e a :  [NotAsked, Loading, Failure e, Success a]

                x : RemoteData Num.I64 Str

                when x is
                    NotAsked -> 3
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────── /code/proj/Main.roc ─

                This `when` does not cover all the possibilities:

                5│>  when x is
                6│>      NotAsked -> 3

                Other possibilities include:

                    Failure _
                    Loading
                    Success _

                I would have to crash if I saw one of those! Add branches for them!
                "#
            ),
        )
    }

    #[test]
    fn patterns_record_not_exhaustive() {
        report_problem_as(
            indoc!(
                r#"
                x = { a: 3 }

                when x is
                    { a: 4 } -> 4
                "#
            ),
            // Tip: Looks like a record field guard is not exhaustive. Learn more about record pattern matches at TODO.
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────── /code/proj/Main.roc ─

                This `when` does not cover all the possibilities:

                3│>  when x is
                4│>      { a: 4 } -> 4

                Other possibilities include:

                    { a }

                I would have to crash if I saw one of those! Add branches for them!
                "#
            ),
        )
    }

    #[test]
    fn patterns_record_guard_not_exhaustive() {
        report_problem_as(
            indoc!(
                r#"
                y : [Nothing, Just Num.I64]
                y = Just 4
                x = { a: y, b: 42}

                when x is
                    { a: Nothing } -> 4
                    { a: Just 3 } -> 4
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────── /code/proj/Main.roc ─

                This `when` does not cover all the possibilities:

                5│>  when x is
                6│>      { a: Nothing } -> 4
                7│>      { a: Just 3 } -> 4

                Other possibilities include:

                    { a: Just _ }

                I would have to crash if I saw one of those! Add branches for them!
                "#
            ),
        )
    }

    #[test]
    fn patterns_nested_tag_not_exhaustive() {
        report_problem_as(
            indoc!(
                r#"
                when Record Nothing 1 is
                    Record (Nothing) b -> b
                    Record (Just 3) b -> b
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────── /code/proj/Main.roc ─

                This `when` does not cover all the possibilities:

                1│>  when Record Nothing 1 is
                2│>      Record (Nothing) b -> b
                3│>      Record (Just 3) b -> b

                Other possibilities include:

                    Record (Just _) _

                I would have to crash if I saw one of those! Add branches for them!
                "#
            ),
        )
    }

    #[test]
    fn patterns_int_redundant() {
        report_problem_as(
            indoc!(
                r#"
                when 0x1 is
                    2 -> 3
                    2 -> 4
                    _ -> 5
                "#
            ),
            indoc!(
                r#"
                ── REDUNDANT PATTERN ───────────────────────────────────── /code/proj/Main.roc ─

                The 2nd pattern is redundant:

                1│   when 0x1 is
                2│       2 -> 3
                3│>      2 -> 4
                4│       _ -> 5

                Any value of this shape will be handled by a previous pattern, so this
                one should be removed.
                "#
            ),
        )
    }

    #[test]
    fn unify_alias_other() {
        report_problem_as(
            indoc!(
                r#"
                Foo a : { x : Num.Int a }

                f : Foo a -> Num.Int a
                f = \r -> r.x

                f { y: 3.14 }
                "#
            ),
            // de-aliases the alias to give a better error message
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 1st argument to `f` is not what I expect:

                6│  f { y: 3.14 }
                      ^^^^^^^^^^^

                This argument is a record of type:

                    { y : Frac a }

                But `f` needs the 1st argument to be:

                    { x : Int a }

                Tip: Seems like a record field typo. Maybe `y` should be `x`?

                Tip: Can more type annotations be added? Type annotations always help
                me give more specific messages, and I think they could help a lot in
                this case
                "#
            ),
        )
    }

    #[test]
    #[ignore]
    fn cyclic_alias() {
        report_problem_as(
            indoc!(
                r#"
                Foo : { x : Bar }
                Bar : { y : Foo }

                f : Foo

                f
                "#
            ),
            // should not report Bar as unused!
            indoc!(
                r#"
                ── CYCLIC ALIAS ────────────────────────────────────────── /code/proj/Main.roc ─

                The `Foo` alias is recursive in an invalid way:

                1│  Foo : { x : Bar }
                          ^^^^^^^^^^^

                The `Foo` alias depends on itself through the following chain of
                definitions:

                    ┌─────┐
                    │     Foo
                    │     ↓
                    │     Bar
                    └─────┘

                Recursion in aliases is only allowed if recursion happens behind a
                tag.

                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                `Bar` is not used anywhere in your code.

                2│  Bar : { y : Foo }
                    ^^^^^^^^^^^^^^^^^

                If you didn't intend on using `Bar` then remove it so future readers of
                your code don't wonder why it is there.
                "#
            ),
        )
    }

    #[test]
    fn self_recursive_alias() {
        report_problem_as(
            indoc!(
                r#"
                Foo : { x : Foo }

                f : Foo
                f = 3

                f
                "#
            ),
            // should not report Bar as unused!
            indoc!(
                r#"
                ── CYCLIC ALIAS ────────────────────────────────────────── /code/proj/Main.roc ─

                The `Foo` alias is self-recursive in an invalid way:

                1│  Foo : { x : Foo }
                    ^^^

                Recursion in aliases is only allowed if recursion happens behind a
                tagged union, at least one variant of which is not recursive.
                "#
            ),
        )
    }

    #[test]
    fn record_duplicate_field_same_type() {
        report_problem_as(
            indoc!(
                r#"
                { x: 4, y: 3, x: 4 }
                "#
            ),
            indoc!(
                r#"
                ── DUPLICATE FIELD NAME ────────────────────────────────── /code/proj/Main.roc ─

                This record defines the `.x` field twice!

                1│  { x: 4, y: 3, x: 4 }
                      ^^^^        ^^^^

                In the rest of the program, I will only use the latter definition:

                1│  { x: 4, y: 3, x: 4 }
                                  ^^^^

                For clarity, remove the previous `.x` definitions from this record.
                "#
            ),
        )
    }

    #[test]
    fn record_duplicate_field_different_types() {
        report_problem_as(
            indoc!(
                r#"
                { x: 4, y: 3, x: "foo" }
                "#
            ),
            indoc!(
                r#"
                ── DUPLICATE FIELD NAME ────────────────────────────────── /code/proj/Main.roc ─

                This record defines the `.x` field twice!

                1│  { x: 4, y: 3, x: "foo" }
                      ^^^^        ^^^^^^^^

                In the rest of the program, I will only use the latter definition:

                1│  { x: 4, y: 3, x: "foo" }
                                  ^^^^^^^^

                For clarity, remove the previous `.x` definitions from this record.
                "#
            ),
        )
    }

    #[test]
    fn record_duplicate_field_multiline() {
        report_problem_as(
            indoc!(
                r#"
                {
                    x: 4,
                    y: 3,
                    x: "foo"
                }
                "#
            ),
            indoc!(
                r#"
                ── DUPLICATE FIELD NAME ────────────────────────────────── /code/proj/Main.roc ─

                This record defines the `.x` field twice!

                1│   {
                2│>      x: 4,
                3│       y: 3,
                4│>      x: "foo"
                5│   }

                In the rest of the program, I will only use the latter definition:

                1│   {
                2│       x: 4,
                3│       y: 3,
                4│>      x: "foo"
                5│   }

                For clarity, remove the previous `.x` definitions from this record.
                "#
            ),
        )
    }

    #[test]
    fn record_update_duplicate_field_multiline() {
        report_problem_as(
            indoc!(
                r#"
                \r ->
                    { r &
                        x: 4,
                        y: 3,
                        x: "foo"
                    }
                "#
            ),
            indoc!(
                r#"
                ── DUPLICATE FIELD NAME ────────────────────────────────── /code/proj/Main.roc ─

                This record defines the `.x` field twice!

                2│       { r &
                3│>          x: 4,
                4│           y: 3,
                5│>          x: "foo"
                6│       }

                In the rest of the program, I will only use the latter definition:

                2│       { r &
                3│           x: 4,
                4│           y: 3,
                5│>          x: "foo"
                6│       }

                For clarity, remove the previous `.x` definitions from this record.
                "#
            ),
        )
    }

    #[test]
    fn record_type_duplicate_field() {
        report_problem_as(
            indoc!(
                r#"
                a : { foo : Num.I64, bar : {}, foo : Str }
                a = { bar: {}, foo: "foo" }

                a
                "#
            ),
            indoc!(
                r#"
                ── DUPLICATE FIELD NAME ────────────────────────────────── /code/proj/Main.roc ─

                This record type defines the `.foo` field twice!

                1│  a : { foo : Num.I64, bar : {}, foo : Str }
                          ^^^^^^^^^^^^^            ^^^^^^^^^

                In the rest of the program, I will only use the latter definition:

                1│  a : { foo : Num.I64, bar : {}, foo : Str }
                                                   ^^^^^^^^^

                For clarity, remove the previous `.foo` definitions from this record
                type.
                "#
            ),
        )
    }

    #[test]
    fn tag_union_duplicate_tag() {
        report_problem_as(
            indoc!(
                r#"
                a : [Foo Num.I64, Bar {}, Foo Str]
                a = Foo "foo"

                a
                "#
            ),
            indoc!(
                r#"
                ── DUPLICATE TAG NAME ──────────────────────────────────── /code/proj/Main.roc ─

                This tag union type defines the `Foo` tag twice!

                1│  a : [Foo Num.I64, Bar {}, Foo Str]
                         ^^^^^^^^^^^          ^^^^^^^

                In the rest of the program, I will only use the latter definition:

                1│  a : [Foo Num.I64, Bar {}, Foo Str]
                                              ^^^^^^^

                For clarity, remove the previous `Foo` definitions from this tag union
                type.
                "#
            ),
        )
    }

    #[test]
    fn annotation_definition_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                bar : Num.I64
                foo = \x -> x

                # NOTE: neither bar or foo are defined at this point
                4
                "#
            ),
            indoc!(
                r#"
                ── NAMING PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This annotation does not match the definition immediately following
                it:

                1│>  bar : Num.I64
                2│>  foo = \x -> x

                Is it a typo? If not, put either a newline or comment between them.
                "#
            ),
        )
    }

    #[test]
    fn annotation_newline_body_is_fine() {
        report_problem_as(
            indoc!(
                r#"
                bar : Num.I64

                foo = \x -> x

                foo bar
                "#
            ),
            indoc!(""),
        )
    }

    #[test]
    fn invalid_alias_rigid_var_pattern() {
        report_problem_as(
            indoc!(
                r#"
                MyAlias 1 : Num.I64

                4
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This pattern in the definition of `MyAlias` is not what I expect:

                1│  MyAlias 1 : Num.I64
                            ^

                Only type variables like `a` or `value` can occur in this position.

                ── UNUSED DEFINITION ───────────────────────────────────── /code/proj/Main.roc ─

                `MyAlias` is not used anywhere in your code.

                1│  MyAlias 1 : Num.I64
                    ^^^^^^^^^^^^^^^^^^^

                If you didn't intend on using `MyAlias` then remove it so future readers
                of your code don't wonder why it is there.
                "#
            ),
        )
    }

    #[test]
    fn invalid_opaque_rigid_var_pattern() {
        report_problem_as(
            indoc!(
                r#"
                Age 1 := Num.I64

                a : Age
                a
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This pattern in the definition of `Age` is not what I expect:

                1│  Age 1 := Num.I64
                        ^

                Only type variables like `a` or `value` can occur in this position.
                "#
            ),
        )
    }

    #[test]
    fn invalid_num() {
        report_problem_as(
            indoc!(
                r#"
                a : Num.Num Num.I64 Num.F64
                a = 3

                a
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY TYPE ARGUMENTS ─────────────────────────────── /code/proj/Main.roc ─

                The `Num` alias expects 1 type argument, but it got 2 instead:

                1│  a : Num.Num Num.I64 Num.F64
                        ^^^^^^^^^^^^^^^^^^^^^^^

                Are there missing parentheses?
                "#
            ),
        )
    }

    #[test]
    fn invalid_num_fn() {
        report_problem_as(
            indoc!(
                r#"
                f : Str -> Num.Num Num.I64 Num.F64
                f = \_ -> 3

                f
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY TYPE ARGUMENTS ─────────────────────────────── /code/proj/Main.roc ─

                The `Num` alias expects 1 type argument, but it got 2 instead:

                1│  f : Str -> Num.Num Num.I64 Num.F64
                               ^^^^^^^^^^^^^^^^^^^^^^^

                Are there missing parentheses?
                "#
            ),
        )
    }

    #[test]
    fn too_few_type_arguments() {
        report_problem_as(
            indoc!(
                r#"
                Pair a b : [Pair a b]

                x : Pair Num.I64
                x = Pair 2 3

                x
                "#
            ),
            indoc!(
                r#"
                ── TOO FEW TYPE ARGUMENTS ──────────────────────────────── /code/proj/Main.roc ─

                The `Pair` alias expects 2 type arguments, but it got 1 instead:

                3│  x : Pair Num.I64
                        ^^^^^^^^^^^^

                Are there missing parentheses?
                "#
            ),
        )
    }

    #[test]
    fn too_many_type_arguments() {
        report_problem_as(
            indoc!(
                r#"
                Pair a b : [Pair a b]

                x : Pair Num.I64 Num.I64 Num.I64
                x = 3

                x
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY TYPE ARGUMENTS ─────────────────────────────── /code/proj/Main.roc ─

                The `Pair` alias expects 2 type arguments, but it got 3 instead:

                3│  x : Pair Num.I64 Num.I64 Num.I64
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Are there missing parentheses?
                "#
            ),
        )
    }

    #[test]
    fn phantom_type_variable() {
        report_problem_as(
            indoc!(
                r#"
                Foo a : [Foo]

                f : Foo Num.I64

                f
                "#
            ),
            indoc!(
                r#"
                ── UNUSED TYPE ALIAS PARAMETER ─────────────────────────── /code/proj/Main.roc ─

                The `a` type parameter is not used in the `Foo` alias definition:

                1│  Foo a : [Foo]
                        ^

                Roc does not allow unused type alias parameters!

                Tip: If you want an unused type parameter (a so-called "phantom
                type"), read the guide section on phantom values.
                "#
            ),
        )
    }

    #[test]
    fn elm_function_syntax() {
        report_problem_as(
            indoc!(
                r#"
                f x y = x
                "#
            ),
            indoc!(
                r#"
                ── ARGUMENTS BEFORE EQUALS ─────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a definition, but I got stuck here:

                1│  f x y = x
                      ^^^

                Looks like you are trying to define a function. In roc, functions are
                always written as a lambda, like increment = \n -> n + 1.
                "#
            ),
        )
    }

    #[test]
    fn two_different_cons() {
        report_problem_as(
            indoc!(
                r#"
                ConsList a : [Cons a (ConsList a), Nil]

                x : ConsList {}
                x = Cons {} (Cons "foo" Nil)

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `x` definition:

                3│  x : ConsList {}
                4│  x = Cons {} (Cons "foo" Nil)
                        ^^^^^^^^^^^^^^^^^^^^^^^^

                This `Cons` tag application has the type:

                    [Cons {} [Cons Str [Cons {} a, Nil] as a, Nil], Nil]

                But the type annotation on `x` says it should be:

                    [Cons {} a, Nil] as a
                "#
            ),
        )
    }

    #[test]
    fn mutually_recursive_types_with_type_error() {
        report_problem_as(
            indoc!(
                r#"
                AList a b : [ACons a (BList a b), ANil]
                BList a b : [BCons a (AList a b), BNil]

                x : AList Num.I64 Num.I64
                x = ACons 0 (BCons 1 (ACons "foo" BNil ))

                y : BList a a
                y = BNil

                { x, y }
                "#
            ),
            // TODO render tag unions across multiple lines
            // TODO do not show recursion var if the recursion var does not render on the surface of a type
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `x` definition:

                4│  x : AList Num.I64 Num.I64
                5│  x = ACons 0 (BCons 1 (ACons "foo" BNil ))
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                This `ACons` tag application has the type:

                    [ACons (Num (Integer Signed64)) [BCons (Num (Integer Signed64)) [ACons Str [BCons I64 [ACons I64 (BList I64 I64),
                    ANil] as ∞, BNil], ANil], BNil], ANil]

                But the type annotation on `x` says it should be:

                    [ACons I64 (BList I64 I64), ANil] as a
                "#
            ),
        )
    }

    #[test]
    fn integer_out_of_range() {
        report_problem_as(
            indoc!(
                r#"
                x = 170_141_183_460_469_231_731_687_303_715_884_105_728_000

                y = -170_141_183_460_469_231_731_687_303_715_884_105_728_000

                h = 0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF
                l = -0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF

                minlit = -170_141_183_460_469_231_731_687_303_715_884_105_728
                maxlit =  340_282_366_920_938_463_463_374_607_431_768_211_455

                x + y + h + l + minlit + maxlit
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This integer literal is too big:

                1│  x = 170_141_183_460_469_231_731_687_303_715_884_105_728_000
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                The largest number representable in Roc is the maximum U128 value,
                340_282_366_920_938_463_463_374_607_431_768_211_455.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This integer literal is too small:

                3│  y = -170_141_183_460_469_231_731_687_303_715_884_105_728_000
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                The smallest number representable in Roc is the minimum I128 value,
                -170_141_183_460_469_231_731_687_303_715_884_105_728.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This integer literal is too big:

                5│  h = 0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                The largest number representable in Roc is the maximum U128 value,
                340_282_366_920_938_463_463_374_607_431_768_211_455.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This integer literal is too small:

                6│  l = -0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                The smallest number representable in Roc is the minimum I128 value,
                -170_141_183_460_469_231_731_687_303_715_884_105_728.

                Tip: Learn more about number literals at TODO
                "#
            ),
        )
    }

    #[test]
    fn float_out_of_range() {
        // have to deal with some whitespace issues because of the format! macro
        report_problem_as(
            indoc!(
                r#"
                overflow = 11.7976931348623157e308
                underflow = -11.7976931348623157e308

                overflow + underflow
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This float literal is too big:

                1│  overflow = 11.7976931348623157e308
                               ^^^^^^^^^^^^^^^^^^^^^^^

                Roc uses signed 64-bit floating points, allowing values between
                -1.7976931348623157e308 and 1.7976931348623157e308

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This float literal is too small:

                2│  underflow = -11.7976931348623157e308
                                ^^^^^^^^^^^^^^^^^^^^^^^^

                Roc uses signed 64-bit floating points, allowing values between
                -1.7976931348623157e308 and 1.7976931348623157e308

                Tip: Learn more about number literals at TODO
                "#
            ),
        )
    }

    #[test]
    fn integer_malformed() {
        // the generated messages here are incorrect. Waiting for a rust nightly feature to land,
        // see https://github.com/rust-lang/rust/issues/22639
        // this test is here to spot regressions in error reporting
        report_problem_as(
            indoc!(
                r#"
                dec = 100A

                hex = 0xZZZ

                oct = 0o9

                bin = 0b2

                dec + hex + oct + bin
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This integer literal contains an invalid digit:

                1│  dec = 100A
                          ^^^^

                Integer literals can only contain the digits
                0-9, or have an integer suffix.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This hex integer literal contains an invalid digit:

                3│  hex = 0xZZZ
                          ^^^^^

                Hexadecimal (base-16) integer literals can only contain the digits
                0-9, a-f and A-F, or have an integer suffix.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This octal integer literal contains an invalid digit:

                5│  oct = 0o9
                          ^^^

                Octal (base-8) integer literals can only contain the digits
                0-7, or have an integer suffix.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This binary integer literal contains an invalid digit:

                7│  bin = 0b2
                          ^^^

                Binary (base-2) integer literals can only contain the digits
                0 and 1, or have an integer suffix.

                Tip: Learn more about number literals at TODO
                "#
            ),
        )
    }

    #[test]
    fn integer_empty() {
        report_problem_as(
            indoc!(
                r#"
                dec = 20

                hex = 0x

                oct = 0o

                bin = 0b

                dec + hex + oct + bin
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This hex integer literal contains no digits:

                3│  hex = 0x
                          ^^

                Hexadecimal (base-16) integer literals must contain at least one of
                the digits 0-9, a-f and A-F, or have an integer suffix.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This octal integer literal contains no digits:

                5│  oct = 0o
                          ^^

                Octal (base-8) integer literals must contain at least one of the
                digits 0-7, or have an integer suffix.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This binary integer literal contains no digits:

                7│  bin = 0b
                          ^^

                Binary (base-2) integer literals must contain at least one of the
                digits 0 and 1, or have an integer suffix.

                Tip: Learn more about number literals at TODO
                "#
            ),
        )
    }

    #[test]
    fn float_malformed() {
        report_problem_as(
            indoc!(
                r#"
                x = 3.0A

                x
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This float literal contains an invalid digit:

                1│  x = 3.0A
                        ^^^^

                Floating point literals can only contain the digits 0-9, or use
                scientific notation 10e4, or have a float suffix.

                Tip: Learn more about number literals at TODO
                "#
            ),
        )
    }

    #[test]
    fn invalid_record_update() {
        report_problem_as(
            indoc!(
                r#"
                foo = { bar: 3 }
                updateNestedRecord = { foo.bar & x: 4 }

                example = { age: 42 }

                # these should work
                y = { Test.example & age: 3 }
                x = { example & age: 4 }

                { updateNestedRecord, foo, x, y }
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This expression cannot be updated:

                2│  updateNestedRecord = { foo.bar & x: 4 }
                                           ^^^^^^^

                Only variables can be updated with record update syntax.
                "#
            ),
        )
    }

    #[test]
    fn module_not_imported() {
        report_problem_as(
            indoc!(
                r#"
                Foo.test
                "#
            ),
            indoc!(
                r#"
                ── MODULE NOT IMPORTED ─────────────────────────────────── /code/proj/Main.roc ─

                The `Foo` module is not imported:

                1│  Foo.test
                    ^^^^^^^^

                Is there an import missing? Perhaps there is a typo. Did you mean one
                of these?

                    Box
                    Bool
                    Num
                    Set
                "#
            ),
        )
    }

    #[test]
    fn optional_record_default_type_error() {
        report_problem_as(
            indoc!(
                r#"
                \{ x, y ? True } -> x + y
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd argument to `add` is not what I expect:

                1│  \{ x, y ? True } -> x + y
                                            ^

                This `y` value is a:

                    [True]a

                But `add` needs the 2nd argument to be:

                    Num a
                "#
            ),
        )
    }

    #[test]
    fn optional_record_default_with_signature() {
        report_problem_as(
            indoc!(
                r#"
                f : { x : Num.I64, y ? Num.I64 } -> Num.I64
                f = \{ x, y ? "foo" } -> (\g, _ -> g) x y

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 1st argument to `f` is weird:

                2│  f = \{ x, y ? "foo" } -> (\g, _ -> g) x y
                         ^^^^^^^^^^^^^^^^

                The argument is a pattern that matches record values of type:

                    { x : I64, y ? Str }

                But the annotation on `f` says the 1st argument should be:

                    { x : I64, y ? I64 }
                "#
            ),
        )
    }

    #[test]
    fn optional_record_invalid_let_binding() {
        report_problem_as(
            indoc!(
                r#"
                \rec ->
                    { x, y } : { x : Num.I64, y ? Str }
                    { x, y } = rec

                    { x, y }
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of this definition:

                2│>      { x, y } : { x : Num.I64, y ? Str }
                3│>      { x, y } = rec

                The body is a value of type:

                    { x : I64, y : Str }

                But the type annotation says it should be:

                    { x : I64, y ? Str }

                Tip: To extract the `.y` field it must be non-optional, but the type
                says this field is optional. Learn more about optional fields at TODO.
                "#
            ),
        )
    }

    #[test]
    fn optional_record_invalid_function() {
        report_problem_as(
            indoc!(
                r#"
                f : { x : Num.I64, y ? Num.I64 } -> Num.I64
                f = \{ x, y } -> x + y

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 1st argument to `f` is weird:

                2│  f = \{ x, y } -> x + y
                         ^^^^^^^^

                The argument is a pattern that matches record values of type:

                    { x : I64, y : I64 }

                But the annotation on `f` says the 1st argument should be:

                    { x : I64, y ? I64 }

                Tip: To extract the `.y` field it must be non-optional, but the type
                says this field is optional. Learn more about optional fields at TODO.
                "#
            ),
        )
    }

    #[test]
    fn optional_record_invalid_when() {
        report_problem_as(
            indoc!(
                r#"
                f : { x : Num.I64, y ? Num.I64 } -> Num.I64
                f = \r ->
                        when r is
                            { x, y } -> x + y

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The branches of this `when` expression don't match the condition:

                3│>          when r is
                4│               { x, y } -> x + y

                This `r` value is a:

                    { x : I64, y ? I64 }

                But the branch patterns have type:

                    { x : I64, y : I64 }

                The branches must be cases of the `when` condition's type!

                Tip: To extract the `.y` field it must be non-optional, but the type
                says this field is optional. Learn more about optional fields at TODO.
                "#
            ),
        )
    }

    #[test]
    fn optional_record_invalid_access() {
        report_problem_as(
            indoc!(
                r#"
                f : { x : Num.I64, y ? Num.I64 } -> Num.I64
                f = \r -> r.y

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This expression is used in an unexpected way:

                2│  f = \r -> r.y
                              ^^^

                This `r` value is a:

                    { x : I64, y ? I64 }

                But you are trying to use it as:

                    { x : I64, y : I64 }

                Tip: To extract the `.y` field it must be non-optional, but the type
                says this field is optional. Learn more about optional fields at TODO.
                "#
            ),
        )
    }

    #[test]
    fn optional_record_invalid_accessor() {
        report_problem_as(
            indoc!(
                r#"
                    f : { x : Num.I64, y ? Num.I64 } -> Num.I64
                    f = \r -> .y r

                    f
                    "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 1st argument to this function is not what I expect:

                2│  f = \r -> .y r
                                 ^

                This `r` value is a:

                    { x : I64, y ? I64 }

                But this function needs the 1st argument to be:

                    { x : I64, y : I64 }

                Tip: To extract the `.y` field it must be non-optional, but the type
                says this field is optional. Learn more about optional fields at TODO.
                "#
            ),
        )
    }

    #[test]
    fn guard_mismatch_with_annotation() {
        report_problem_as(
            indoc!(
                r#"
                f : { x : Num.I64, y : Num.I64 } -> Num.I64
                f = \r ->
                        when r is
                            { x, y : "foo" } -> x + 0
                            _ -> 0

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The branches of this `when` expression don't match the condition:

                3│>          when r is
                4│               { x, y : "foo" } -> x + 0
                5│               _ -> 0

                This `r` value is a:

                    { x : I64, y : I64 }

                But the branch patterns have type:

                    { x : I64, y : Str }

                The branches must be cases of the `when` condition's type!
                "#
            ),
        )
    }

    #[test]
    fn optional_field_mismatch_with_annotation() {
        report_problem_as(
            indoc!(
                r#"
                f : { x : Num.I64, y ? Num.I64 } -> Num.I64
                f = \r ->
                        when r is
                            { x, y ? "foo" } -> (\g, _ -> g) x y
                            _ -> 0

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The branches of this `when` expression don't match the condition:

                3│>          when r is
                4│               { x, y ? "foo" } -> (\g, _ -> g) x y
                5│               _ -> 0

                This `r` value is a:

                    { x : I64, y ? I64 }

                But the branch patterns have type:

                    { x : I64, y ? Str }

                The branches must be cases of the `when` condition's type!
                "#
            ),
        )
    }

    #[test]
    fn incorrect_optional_field() {
        report_problem_as(
            indoc!(
                r#"
                { x: 5, y ? 42 }
                "#
            ),
            indoc!(
                r#"
                ── BAD OPTIONAL VALUE ──────────────────────────────────── /code/proj/Main.roc ─

                This record uses an optional value for the `.y` field in an incorrect
                context!

                1│  { x: 5, y ? 42 }
                            ^^^^^^

                You can only use optional values in record destructuring, like:

                    { answer ? 42, otherField } = myRecord
                "#
            ),
        )
    }
    #[test]
    fn first_wildcard_is_required() {
        report_problem_as(
            indoc!(
                r#"
                when Foo 1 2 3 is
                    Foo _ 1 _ -> 1
                    _ -> 2
                "#
            ),
            "",
        )
    }

    #[test]
    fn second_wildcard_is_redundant() {
        report_problem_as(
            indoc!(
                r#"
                when Foo 1 2 3 is
                    Foo _ 1 _ -> 1
                    _ -> 2
                    _ -> 3
                "#
            ),
            indoc!(
                r#"
            ── REDUNDANT PATTERN ───────────────────────────────────── /code/proj/Main.roc ─

            The 3rd pattern is redundant:

            1│  when Foo 1 2 3 is
            2│      Foo _ 1 _ -> 1
            3│      _ -> 2
            4│      _ -> 3
                    ^

            Any value of this shape will be handled by a previous pattern, so this
            one should be removed.
            "#
            ),
        )
    }

    #[test]
    fn alias_using_alias() {
        report_problem_as(
            indoc!(
                r#"
                # The color of a node. Leaves are considered Black.
                NodeColor : [Red, Black]

                RBTree k v : [Node NodeColor k v (RBTree k v) (RBTree k v), Empty]

                # Create an empty dictionary.
                empty : RBTree k v
                empty =
                    Empty

                empty
                "#
            ),
            "",
        )
    }

    #[test]
    fn unused_argument() {
        report_problem_as(
            indoc!(
                r#"
                f = \foo -> 1

                f
                "#
            ),
            indoc!(
                r#"
            ── UNUSED ARGUMENT ─────────────────────────────────────── /code/proj/Main.roc ─

            `f` doesn't use `foo`.

            1│  f = \foo -> 1
                     ^^^

            If you don't need `foo`, then you can just remove it. However, if you
            really do need `foo` as an argument of `f`, prefix it with an underscore,
            like this: "_`foo`". Adding an underscore at the start of a variable
            name is a way of saying that the variable is not used.
            "#
            ),
        )
    }

    #[test]
    fn qualified_tag() {
        report_problem_as(
            indoc!(
                r#"
                Foo.Bar
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I am trying to parse a qualified name here:

                1│  Foo.Bar
                           ^

                This looks like a qualified tag name to me, but tags cannot be
                qualified! Maybe you wanted a qualified name, something like
                Json.Decode.string?
            "#
            ),
        )
    }

    #[test]
    fn module_ident_ends_with_dot() {
        report_problem_as(
            indoc!(
                r#"
                Foo.Bar.
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I am trying to parse a qualified name here:

                1│  Foo.Bar.
                            ^

                I was expecting to see an identifier next, like height. A complete
                qualified name looks something like Json.Decode.string.
            "#
            ),
        )
    }

    #[test]
    fn record_access_ends_with_dot() {
        report_problem_as(
            indoc!(
                r#"
                foo.bar.
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I trying to parse a record field access here:

                1│  foo.bar.
                            ^

                So I expect to see a lowercase letter next, like .name or .height.
            "#
            ),
        )
    }

    #[test]
    fn type_annotation_double_colon() {
        report_problem_as(
            indoc!(
                r#"
                f :: I64
                f = 42

                f
                "#
            ),
            indoc!(
                r#"
                ── UNKNOWN OPERATOR ────────────────────────────────────── /code/proj/Main.roc ─

                This looks like an operator, but it's not one I recognize!

                1│  f :: I64
                      ^^

                I have no specific suggestion for this operator, see TODO for the full
                list of operators in Roc.
            "#
            ),
        )
    }

    #[test]
    fn double_equals_in_def() {
        // NOTE: VERY BAD ERROR MESSAGE
        //
        // looks like `x y` are considered argument to the add, even though they are
        // on a lower indentation level
        report_problem_as(
            indoc!(
                r#"
                x = 3
                y =
                    x == 5
                    Num.add 1 2

                { x,  y }
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY ARGS ───────────────────────────────────────── /code/proj/Main.roc ─

                This value is not a function, but it was given 3 arguments:

                3│      x == 5
                             ^

                Are there any missing commas? Or missing parentheses?
            "#
            ),
        )
    }

    #[test]
    fn tag_union_open() {
        report_problem_as(
            indoc!(
                r#"
                f : [
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED TAG UNION TYPE ───────────────────────────── /code/proj/Main.roc ─

                I just started parsing a tag union type, but I got stuck here:

                1│  f : [
                         ^

                Tag unions look like [Many I64, None], so I was expecting to see a tag
                name next.
            "#
            ),
        )
    }

    #[test]
    fn tag_union_end() {
        report_problem_as(
            indoc!(
                r#"
                f : [Yes,
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED TAG UNION TYPE ───────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a tag union type, but I got stuck here:

                1│  f : [Yes,
                             ^

                I was expecting to see a closing square bracket before this, so try
                adding a ] and see if that helps?
            "#
            ),
        )
    }

    #[test]
    fn tag_union_lowercase_tag_name() {
        report_problem_as(
            indoc!(
                r#"
                f : [lowercase]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD TAG NAME ──────────────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a tag union type, but I got stuck here:

                1│  f : [lowercase]
                         ^

                I was expecting to see a tag name.

                Hint: Tag names start with an uppercase letter, like Err or Green.
            "#
            ),
        )
    }

    #[test]
    fn tag_union_second_lowercase_tag_name() {
        report_problem_as(
            indoc!(
                r#"
                f : [Good, bad]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD TAG NAME ──────────────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a tag union type, but I got stuck here:

                1│  f : [Good, bad]
                               ^

                I was expecting to see a tag name.

                Hint: Tag names start with an uppercase letter, like Err or Green.
            "#
            ),
        )
    }

    #[test]
    fn record_type_open() {
        report_problem_as(
            indoc!(
                r#"
                f : {
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED RECORD TYPE ──────────────────────────────── /code/proj/Main.roc ─

                I just started parsing a record type, but I got stuck here:

                1│  f : {
                         ^

                Record types look like { name : String, age : Int }, so I was
                expecting to see a field name next.
            "#
            ),
        )
    }

    #[test]
    fn record_type_open_indent() {
        report_problem_as(
            indoc!(
                r#"
                f : {
                foo : I64,
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED RECORD TYPE ──────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a record type, but I got stuck here:

                1│  f : {
                         ^

                I was expecting to see a closing curly brace before this, so try
                adding a } and see if that helps?

                Note: I may be confused by indentation
            "#
            ),
        )
    }

    #[test]
    fn record_type_end() {
        report_problem_as(
            indoc!(
                r#"
                f : { a: Int,
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED RECORD TYPE ──────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a record type, but I got stuck here:

                1│  f : { a: Int,
                                 ^

                I was expecting to see a closing curly brace before this, so try
                adding a } and see if that helps?
            "#
            ),
        )
    }

    #[test]
    fn record_type_keyword_field_name() {
        report_problem_as(
            indoc!(
                r#"
                f : { if : I64 }
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED RECORD TYPE ──────────────────────────────── /code/proj/Main.roc ─

                I just started parsing a record type, but I got stuck on this field
                name:

                1│  f : { if : I64 }
                          ^^

                Looks like you are trying to use `if` as a field name, but that is a
                reserved word. Try using a different name!
            "#
            ),
        )
    }

    #[test]
    fn record_type_missing_comma() {
        // a case where the message cannot be as good as elm's
        report_problem_as(
            indoc!(
                r#"
                f : { foo  bar }
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED RECORD TYPE ──────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a record type, but I got stuck here:

                1│  f : { foo  bar }
                               ^

                I was expecting to see a colon, question mark, comma or closing curly
                brace.
            "#
            ),
        )
    }

    #[test]
    fn record_type_tab() {
        // a case where the message cannot be as good as elm's
        report_problem_as(
            "f : { foo \t }",
            indoc!(
                r#"
                ── TAB CHARACTER ───────────────────────────────────────── /code/proj/Main.roc ─

                I encountered a tab character

                1│  f : { foo 	 }
                              ^

                Tab characters are not allowed.
            "#
            ),
        )
    }

    #[test]
    fn comment_with_tab() {
        report_problem_as(
            "# comment with a \t\n4",
            indoc!(
                "
                ── TAB CHARACTER ───────────────────────────────────────── /code/proj/Main.roc ─

                I encountered a tab character

                1│  # comment with a \t
                                     ^

                Tab characters are not allowed.
            "
            ),
        )
    }

    #[test]
    fn type_in_parens_start() {
        // TODO bad error message
        report_problem_as(
            indoc!(
                r#"
                f : (
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED TYPE ─────────────────────────────────────── /code/proj/Main.roc ─

                I just started parsing a type, but I got stuck here:

                1│  f : (
                         ^

                I am expecting a type next, like Bool or List a.
            "#
            ),
        )
    }

    #[test]
    fn type_in_parens_end() {
        report_problem_as(
            indoc!(
                r#"
                f : ( I64
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED PARENTHESES ──────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a type in parentheses, but I got stuck
                here:

                1│  f : ( I64
                             ^

                I was expecting to see a parenthesis before this, so try adding a )
                and see if that helps?

                Note: I may be confused by indentation
            "#
            ),
        )
    }

    #[test]
    fn type_apply_double_dot() {
        report_problem_as(
            indoc!(
                r#"
                f : Foo..Bar

                f
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I am confused by this type name:

                1│  f : Foo..Bar
                        ^^^^^^^^

                Type names start with an uppercase letter, and can optionally be
                qualified by a module name, like Bool or Http.Request.Request.
            "#
            ),
        )

        //                ── DOUBLE DOT ──────────────────────────────────────────────────────────────────
        //
        //                I encountered two dots in a row:
        //
        //                1│  f : Foo..Bar
        //                            ^
        //
        //                Try removing one of them.
    }

    #[test]
    fn type_apply_trailing_dot() {
        report_problem_as(
            indoc!(
                r#"
                f : Foo.Bar.

                f
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I am confused by this type name:

                1│  f : Foo.Bar.
                        ^^^^^^^^

                Type names start with an uppercase letter, and can optionally be
                qualified by a module name, like Bool or Http.Request.Request.
            "#
            ),
        )

        //                ── TRAILING DOT ────────────────────────────────────────────────────────────────
        //
        //                I encountered a dot with nothing after it:
        //
        //                1│  f : Foo.Bar.
        //                                ^
        //
        //                Dots are used to refer to a type in a qualified way, like
        //                Num.I64 or List.List a. Try adding a type name next.
    }

    #[test]
    fn type_apply_stray_dot() {
        report_problem_as(
            indoc!(
                r#"
                f : .
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED TYPE ─────────────────────────────────────── /code/proj/Main.roc ─

                I just started parsing a type, but I got stuck here:

                1│  f : .
                        ^

                I am expecting a type next, like Bool or List a.
            "#
            ),
        )
    }

    #[test]
    fn type_apply_start_with_number() {
        report_problem_as(
            indoc!(
                r#"
                f : Foo.1

                f
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I am confused by this type name:

                1│  f : Foo.1
                        ^^^^^

                Type names start with an uppercase letter, and can optionally be
                qualified by a module name, like Bool or Http.Request.Request.
            "#
            ),
        )

        //                ── WEIRD QUALIFIED NAME ────────────────────────────────────────────────────────
        //
        //                I encountered a number at the start of a qualified name segment:
        //
        //                1│  f : Foo.1
        //                            ^
        //
        //                All parts of a qualified type name must start with an uppercase
        //                letter, like Num.I64 or List.List a.
    }

    #[test]
    fn type_apply_start_with_lowercase() {
        report_problem_as(
            indoc!(
                r#"
                f : Foo.foo

                f
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I am confused by this type name:

                1│  f : Foo.foo
                        ^^^^^^^

                Type names start with an uppercase letter, and can optionally be
                qualified by a module name, like Bool or Http.Request.Request.
            "#
            ),
        )
    }

    #[test]
    fn def_missing_final_expression() {
        report_problem_as(
            indoc!(
                r#"
                f : Foo.foo
                "#
            ),
            indoc!(
                r#"
                ── MISSING FINAL EXPRESSION ────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a definition's final expression, but I
                got stuck here:

                1│  f : Foo.foo
                               ^

                This definition is missing a final expression. A nested definition
                must be followed by either another definition, or an expression

                    x = 4
                    y = 2

                    x + y
            "#
            ),
        )
    }

    #[test]
    fn type_inline_alias() {
        report_problem_as(
            indoc!(
                r#"
                f : I64 as
                f = 0

                f
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED INLINE ALIAS ─────────────────────────────── /code/proj/Main.roc ─

                I just started parsing an inline type alias, but I got stuck here:

                1│  f : I64 as
                              ^

                Note: I may be confused by indentation
            "#
            ),
        )
    }

    #[test]
    fn type_double_comma() {
        report_problem_as(
            indoc!(
                r#"
                f : I64,,I64 -> I64
                f = 0

                f
                "#
            ),
            indoc!(
                r#"
                ── DOUBLE COMMA ────────────────────────────────────────── /code/proj/Main.roc ─

                I just started parsing a function argument type, but I encountered two
                commas in a row:

                1│  f : I64,,I64 -> I64
                            ^

                Try removing one of them.
            "#
            ),
        )
    }

    #[test]
    fn type_argument_no_arrow() {
        report_problem_as(
            indoc!(
                r#"
                f : I64, I64
                f = 0

                f
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED TYPE ─────────────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a type, but I got stuck here:

                1│  f : I64, I64
                                ^

                Note: I may be confused by indentation
            "#
            ),
        )
    }

    #[test]
    fn type_argument_arrow_then_nothing() {
        // TODO could do better by pointing out we're parsing a function type
        report_problem_as(
            indoc!(
                r#"
                f : I64, I64 ->
                f = 0

                f
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED TYPE ─────────────────────────────────────── /code/proj/Main.roc ─

                I just started parsing a type, but I got stuck here:

                1│  f : I64, I64 ->
                                   ^

                Note: I may be confused by indentation
            "#
            ),
        )
    }

    #[test]
    fn dict_type_formatting() {
        // TODO could do better by pointing out we're parsing a function type
        report_problem_as(
            indoc!(
                r#"
                myDict : Dict Num.I64 Str
                myDict = Dict.insert Dict.empty "foo" 42

                myDict
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `myDict` definition:

                1│  myDict : Dict Num.I64 Str
                2│  myDict = Dict.insert Dict.empty "foo" 42
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                This `insert` call produces:

                    Dict Str (Num a)

                But the type annotation on `myDict` says it should be:

                    Dict I64 Str
            "#
            ),
        )
    }

    #[test]
    fn alias_type_diff() {
        report_problem_as(
            indoc!(
                r#"
                HSet a : Set a

                foo : Str -> HSet {}

                myDict : HSet Str
                myDict = foo "bar"

                myDict
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `myDict` definition:

                5│  myDict : HSet Str
                6│  myDict = foo "bar"
                             ^^^^^^^^^

                This `foo` call produces:

                    HSet {}

                But the type annotation on `myDict` says it should be:

                    HSet Str
            "#
            ),
        )
    }

    #[test]
    fn if_guard_without_condition() {
        // this should get better with time
        report_problem_as(
            indoc!(
                r#"
                when Just 4 is
                    Just if ->
                        4

                    _ ->
                        2
                "#
            ),
            indoc!(
                r#"
                ── IF GUARD NO CONDITION ───────────────────────────────── /code/proj/Main.roc ─

                I just started parsing an if guard, but there is no guard condition:

                1│  when Just 4 is
                2│      Just if ->
                                ^

                Try adding an expression before the arrow!
            "#
            ),
        )
    }

    #[test]
    fn empty_or_pattern() {
        report_problem_as(
            indoc!(
                r#"
                when Just 4 is
                    Just 4 | ->
                        4

                    _ ->
                        2
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED PATTERN ──────────────────────────────────── /code/proj/Main.roc ─

                I just started parsing a pattern, but I got stuck here:

                2│      Just 4 | ->
                                 ^

                Note: I may be confused by indentation
                "#
            ),
        )
    }

    #[test]
    fn pattern_binds_keyword() {
        // TODO check if "what_is_next" is a keyword
        report_problem_as(
            indoc!(
                r#"
                when Just 4 is
                    Just when ->
                        4

                    _ ->
                        2
                "#
            ),
            indoc!(
                r#"
                ── MISSING EXPRESSION ──────────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a definition, but I got stuck here:

                1│  when Just 4 is
                2│      Just when ->
                             ^

                I was expecting to see an expression like 42 or "hello".
            "#
            ),
        )
    }

    #[test]
    fn when_missing_arrow() {
        // this should get better with time
        report_problem_as(
            indoc!(
                r#"
                when 5 is
                    1 -> 2
                    _
                "#
            ),
            indoc!(
                r#"
                ── MISSING ARROW ───────────────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a `when` expression, but got stuck here:

                2│      1 -> 2
                3│      _
                         ^

                I was expecting to see an arrow next.

                Note: Sometimes I get confused by indentation, so try to make your `when`
                look something like this:

                    when List.first plants is
                      Ok n ->
                        n

                      Err _ ->
                        200

                Notice the indentation. All patterns are aligned, and each branch is
                indented a bit more than the corresponding pattern. That is important!
            "#
            ),
        )
    }

    #[test]
    fn lambda_double_comma() {
        report_problem_as(
            indoc!(
                r#"
                \a,,b -> 1
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED ARGUMENT LIST ────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a function argument list, but I got stuck
                at this comma:

                1│  \a,,b -> 1
                       ^

                I was expecting an argument pattern before this, so try adding an
                argument before the comma and see if that helps?
            "#
            ),
        )
    }

    #[test]
    fn lambda_leading_comma() {
        report_problem_as(
            indoc!(
                r#"
                \,b -> 1
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED ARGUMENT LIST ────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a function argument list, but I got stuck
                at this comma:

                1│  \,b -> 1
                     ^

                I was expecting an argument pattern before this, so try adding an
                argument before the comma and see if that helps?
            "#
            ),
        )
    }

    #[test]
    fn when_outdented_branch() {
        // this should get better with time
        report_problem_as(
            indoc!(
                r#"
                when 4 is
                    5 -> 2
                 2 -> 2
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I got stuck here:

                1│  when 4 is
                2│      5 -> 2
                              ^

                Whatever I am running into is confusing me a lot! Normally I can give
                fairly specific hints, but something is really tripping me up this
                time.
                "#
            ),
            // TODO this formerly gave
            //
            //                ── UNFINISHED WHEN ─────────────────────────────────────────────────────────────
            //
            //                I was partway through parsing a `when` expression, but I got stuck here:
            //
            //                3│    _ -> 2
            //                        ^
            //
            //                I suspect this is a pattern that is not indented enough? (by 2 spaces)
            //
            // but that requires parsing the next pattern blindly, irrespective of indentation. Can
            // we find an efficient solution that doesn't require parsing an extra pattern for
            // every `when`, i.e. we want a good error message for the test case above, but for
            // a valid `when`, we don't want to do extra work, e.g. here
            //
            //  x
            //      when x is
            //          n -> n
            //
            //  4
            //
            // We don't want to parse the `4` and say it's an outdented pattern!
        )
    }

    #[test]
    fn when_over_indented_underscore() {
        report_problem_as(
            indoc!(
                r#"
                when 4 is
                    5 -> 2
                     _ -> 2
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I got stuck here:

                1│  when 4 is
                2│      5 -> 2
                              ^

                Whatever I am running into is confusing me a lot! Normally I can give
                fairly specific hints, but something is really tripping me up this
                time.
            "#
            ),
        )
    }

    #[test]
    fn when_over_indented_int() {
        report_problem_as(
            indoc!(
                r#"
                when 4 is
                    5 -> Num.neg
                     2 -> 2
                "#
            ),
            indoc!(
                r#"
                ── UNEXPECTED ARROW ────────────────────────────────────── /code/proj/Main.roc ─

                I am parsing a `when` expression right now, but this arrow is confusing
                me:

                3│       2 -> 2
                           ^^

                It makes sense to see arrows around here, so I suspect it is something
                earlier.Maybe this pattern is indented a bit farther from the previous
                patterns?

                Note: Here is an example of a valid `when` expression for reference.

                    when List.first plants is
                      Ok n ->
                        n

                      Err _ ->
                        200

                Notice the indentation. All patterns are aligned, and each branch is
                indented a bit more than the corresponding pattern. That is important!
            "#
            ),
        )
    }

    #[test]
    fn if_outdented_then() {
        // TODO I think we can do better here
        report_problem_as(
            indoc!(
                r#"
                x =
                    if 5 == 5
                then 2 else 3

                x
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED IF ───────────────────────────────────────── /code/proj/Main.roc ─

                I was partway through parsing an `if` expression, but I got stuck here:

                2│      if 5 == 5
                                 ^

                I was expecting to see the `then` keyword next.
            "#
            ),
        )
    }

    #[test]
    fn if_missing_else() {
        // this should get better with time
        report_problem_as(
            indoc!(
                r#"
                if 5 == 5 then 2
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED IF ───────────────────────────────────────── /code/proj/Main.roc ─

                I was partway through parsing an `if` expression, but I got stuck here:

                1│  if 5 == 5 then 2
                                    ^

                I was expecting to see the `else` keyword next.
            "#
            ),
        )
    }

    #[test]
    fn list_double_comma() {
        report_problem_as(
            indoc!(
                r#"
                [1, 2, , 3]
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED LIST ─────────────────────────────────────── /code/proj/Main.roc ─

                I am partway through started parsing a list, but I got stuck here:

                1│  [1, 2, , 3]
                           ^

                I was expecting to see a list entry before this comma, so try adding a
                list entry and see if that helps?
            "#
            ),
        )
    }

    #[test]
    fn list_without_end() {
        report_problem_as(
            indoc!(
                r#"
                [1, 2,
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED LIST ─────────────────────────────────────── /code/proj/Main.roc ─

                I am partway through started parsing a list, but I got stuck here:

                1│  [1, 2,
                          ^

                I was expecting to see a closing square bracket before this, so try
                adding a ] and see if that helps?

                Note: When I get stuck like this, it usually means that there is a
                missing parenthesis or bracket somewhere earlier. It could also be a
                stray keyword or operator.
            "#
            ),
        )
    }

    #[test]
    fn number_double_dot() {
        report_problem_as(
            indoc!(
                r#"
                1.1.1
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This float literal contains an invalid digit:

                1│  1.1.1
                    ^^^^^

                Floating point literals can only contain the digits 0-9, or use
                scientific notation 10e4, or have a float suffix.

                Tip: Learn more about number literals at TODO
            "#
            ),
        )
    }

    #[test]
    fn unicode_not_hex() {
        report_problem_as(
            r#""abc\u(zzzz)def""#,
            indoc!(
                r#"
                ── WEIRD CODE POINT ────────────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a unicode code point, but I got stuck
                here:

                1│  "abc\u(zzzz)def"
                           ^

                I was expecting a hexadecimal number, like \u(1100) or \u(00FF).

                Learn more about working with unicode in roc at TODO
            "#
            ),
        )
    }

    #[test]
    fn interpolate_not_identifier() {
        report_problem_as(
            r#""abc\(32)def""#,
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This string interpolation is invalid:

                1│  "abc\(32)def"
                          ^^

                I was expecting an identifier, like \u(message) or
                \u(LoremIpsum.text).

                Learn more about string interpolation at TODO
            "#
            ),
        )
    }

    #[test]
    fn unicode_too_large() {
        report_problem_as(
            r#""abc\u(110000)def""#,
            indoc!(
                r#"
                ── INVALID UNICODE ─────────────────────────────────────── /code/proj/Main.roc ─

                This unicode code point is invalid:

                1│  "abc\u(110000)def"
                           ^^^^^^

                Learn more about working with unicode in roc at TODO
            "#
            ),
        )
    }

    #[test]
    fn weird_escape() {
        report_problem_as(
            r#""abc\qdef""#,
            indoc!(
                r#"
                ── WEIRD ESCAPE ────────────────────────────────────────── /code/proj/Main.roc ─

                I was partway through parsing a  string literal, but I got stuck here:

                1│  "abc\qdef"
                        ^^

                This is not an escape sequence I recognize. After a backslash, I am
                looking for one of these:

                    - A newline: \n
                    - A caret return: \r
                    - A tab: \t
                    - An escaped quote: \"
                    - An escaped backslash: \\
                    - A unicode code point: \u(00FF)
                    - An interpolated string: \(myVariable)
            "#
            ),
        )
    }

    #[test]
    fn single_no_end() {
        report_problem_as(
            r#""there is no end"#,
            indoc!(
                r#"
                ── ENDLESS STRING ──────────────────────────────────────── /code/proj/Main.roc ─

                I cannot find the end of this string:

                1│  "there is no end
                     ^

                You could change it to something like "to be or not to be" or even
                just "".
            "#
            ),
        )
    }

    #[test]
    fn multi_no_end() {
        report_problem_as(
            r#""""there is no end"#,
            indoc!(
                r#"
                ── ENDLESS STRING ──────────────────────────────────────── /code/proj/Main.roc ─

                I cannot find the end of this block string:

                1│  """there is no end
                       ^

                You could change it to something like """to be or not to be""" or even
                just """""".
            "#
            ),
        )
    }

    #[test]
    // https://github.com/rtfeldman/roc/issues/1714
    fn interpolate_concat_is_transparent_1714() {
        report_problem_as(
            indoc!(
                r#"
                greeting = "Privet"

                if True then 1 else "\(greeting), World!"
                "#,
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This `if` has an `else` branch with a different type from its `then` branch:

                3│  if True then 1 else "\(greeting), World!"
                                        ^^^^^^^^^^^^^^^^^^^^^

                The `else` branch is a string of type:

                    Str

                but the `then` branch has the type:

                    Num a

                All branches in an `if` must have the same type!
                "#
            ),
        )
    }

    macro_rules! comparison_binop_transparency_tests {
        ($($op:expr, $name:ident),* $(,)?) => {
            $(
            #[test]
            fn $name() {
                report_problem_as(
                    &format!(r#"if True then "abc" else 1 {} 2"#, $op),
                    &format!(
r#"── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

This `if` has an `else` branch with a different type from its `then` branch:

1│  if True then "abc" else 1 {} 2
                            ^^{}^^

This comparison produces:

    Bool

but the `then` branch has the type:

    Str

All branches in an `if` must have the same type!
"#,
                        $op, "^".repeat($op.len())
                    ),
                )
            }
            )*
        }
    }

    comparison_binop_transparency_tests! {
        "<", lt_binop_is_transparent,
        ">", gt_binop_is_transparent,
        "==", eq_binop_is_transparent,
        "!=", neq_binop_is_transparent,
        "<=", leq_binop_is_transparent,
        ">=", geq_binop_is_transparent,
    }

    #[test]
    fn keyword_record_field_access() {
        report_problem_as(
            indoc!(
                r#"
                foo = {}

                foo.if
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This `foo` record doesn’t have a `if` field:

                3│  foo.if
                    ^^^^^^

                In fact, `foo` is a record with no fields at all!
            "#
            ),
        )
    }

    #[test]
    fn keyword_qualified_import() {
        report_problem_as(
            indoc!(
                r#"
                Num.if
                "#
            ),
            indoc!(
                r#"
                ── NOT EXPOSED ─────────────────────────────────────────── /code/proj/Main.roc ─

                The Num module does not expose `if`:

                1│  Num.if
                    ^^^^^^

                Did you mean one of these?

                    Num.sin
                    Num.div
                    Num.abs
                    Num.neg
            "#
            ),
        )
    }

    #[test]
    fn stray_dot_expr() {
        report_problem_as(
            indoc!(
                r#"
                Num.add . 23
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I trying to parse a record field access here:

                1│  Num.add . 23
                             ^

                So I expect to see a lowercase letter next, like .name or .height.
            "#
            ),
        )
    }

    #[test]
    fn opaque_ref_field_access() {
        report_problem_as(
            indoc!(
                r#"
                @UUID.bar
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I am very confused by this field access:

                1│  @UUID.bar
                         ^^^^

                It looks like a record field access on an opaque reference.
            "#
            ),
        )
    }

    #[test]
    fn weird_accessor() {
        report_problem_as(
            indoc!(
                r#"
                .foo.bar
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I am very confused by this field access

                1│  .foo.bar
                    ^^^^^^^^

                It looks like a field access on an accessor. I parse.client.name as
                (.client).name. Maybe use an anonymous function like
                (\r -> r.client.name) instead?
            "#
            ),
        )
    }

    #[test]
    fn part_starts_with_number() {
        report_problem_as(
            indoc!(
                r#"
                foo.100
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I trying to parse a record field access here:

                1│  foo.100
                        ^

                So I expect to see a lowercase letter next, like .name or .height.
            "#
            ),
        )
    }

    #[test]
    fn closure_underscore_ident() {
        report_problem_as(
            indoc!(
                r#"
                \the_answer -> 100
                "#
            ),
            indoc!(
                r#"
                ── NAMING PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I am trying to parse an identifier here:

                1│  \the_answer -> 100
                        ^

                Underscores are not allowed in identifiers. Use camelCase instead!
            "#
            ),
        )
    }

    #[test]
    #[ignore]
    fn double_binop() {
        report_problem_as(
            indoc!(
                r#"
                key >= 97 && <= 122
                "#
            ),
            indoc!(
                r#"
                "#
            ),
        )
    }

    #[test]
    #[ignore]
    fn case_of() {
        report_problem_as(
            indoc!(
                r#"
                case 1 of
                    1 -> True
                    _ -> False
                "#
            ),
            indoc!(
                r#"
                "#
            ),
        )
    }

    #[test]
    fn argument_without_space() {
        report_problem_as(
            indoc!(
                r#"
                ["foo", bar("")]
                "#
            ),
            indoc!(
                r#"
                ── UNRECOGNIZED NAME ───────────────────────────────────── /code/proj/Main.roc ─

                Nothing is named `bar` in this scope.

                1│  ["foo", bar("")]
                            ^^^

                Did you mean one of these?

                    Str
                    Err
                    Box
                    Set
                "#
            ),
        )
    }

    #[test]
    fn invalid_operator() {
        report_problem_as(
            indoc!(
                r#"
                main =
                    5 ** 3
                "#
            ),
            indoc!(
                r#"
                ── UNKNOWN OPERATOR ────────────────────────────────────── /code/proj/Main.roc ─

                This looks like an operator, but it's not one I recognize!

                1│  main =
                2│      5 ** 3
                          ^^

                I have no specific suggestion for this operator, see TODO for the full
                list of operators in Roc.
            "#
            ),
        )
    }

    #[test]
    fn double_plus() {
        report_problem_as(
            indoc!(
                r#"
                main =
                    [] ++ []
                "#
            ),
            indoc!(
                r#"
                ── UNKNOWN OPERATOR ────────────────────────────────────── /code/proj/Main.roc ─

                This looks like an operator, but it's not one I recognize!

                1│  main =
                2│      [] ++ []
                           ^^

                To concatenate two lists or strings, try using List.concat or
                Str.concat instead.
            "#
            ),
        )
    }

    #[test]
    fn inline_hastype() {
        report_problem_as(
            indoc!(
                r#"
                main =
                    (\x -> x) : I64

                    3
                "#
            ),
            indoc!(
                r#"
                ── UNKNOWN OPERATOR ────────────────────────────────────── /code/proj/Main.roc ─

                This looks like an operator, but it's not one I recognize!

                1│  main =
                2│      (\x -> x) : I64
                                  ^

                The has-type operator : can only occur in a definition's type
                signature, like

                    increment : I64 -> I64
                    increment = \x -> x + 1
            "#
            ),
        )
    }

    #[test]
    fn wild_case_arrow() {
        // this is still bad, but changing the order and progress of other parsers should improve it
        // down the line
        report_problem_as(
            indoc!(
                r#"
                main = 5 -> 3
                "#
            ),
            indoc!(
                r#"
                ── UNKNOWN OPERATOR ────────────────────────────────────── /code/proj/Main.roc ─

                This looks like an operator, but it's not one I recognize!

                1│  main = 5 -> 3
                             ^^

                The arrow -> is only used to define cases in a `when`.

                    when color is
                        Red -> "stop!"
                        Green -> "go!"
            "#
            ),
        )
    }

    #[test]
    fn provides_to_identifier() {
        report_header_problem_as(
            indoc!(
                r#"
                app "test-base64"
                    packages { pf: "platform" }
                    imports [pf.Task, Base64]
                    provides [main, @Foo] to pf
                "#
            ),
            indoc!(
                r#"
                ── WEIRD PROVIDES ──────────────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a provides list, but I got stuck here:

                3│      imports [pf.Task, Base64]
                4│      provides [main, @Foo] to pf
                                        ^

                I was expecting a type name, value name or function name next, like

                    provides [Animal, default, tame]
            "#
            ),
        )
    }

    #[test]
    fn platform_requires_rigids() {
        report_header_problem_as(
            indoc!(
                r#"
                platform "folkertdev/foo"
                    requires { main : Effect {} }
                    exposes []
                    packages {}
                    imports [Task]
                    provides [mainForHost]
                    effects fx.Effect
                         {
                             putChar : I64 -> Effect {},
                             putLine : Str -> Effect {},
                             getLine : Effect Str
                         }
                "#
            ),
            indoc!(
                r#"
                ── BAD REQUIRES ────────────────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a header, but I got stuck here:

                1│  platform "folkertdev/foo"
                2│      requires { main : Effect {} }
                                   ^

                I am expecting a list of type names like `{}` or `{ Model }` next. A full
                `requires` definition looks like

                    requires { Model, Msg } {main : Effect {}}
            "#
            ),
        )
    }

    #[test]
    fn missing_imports() {
        report_header_problem_as(
            indoc!(
                r#"
                interface Foobar
                    exposes [main, Foo]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD IMPORTS ───────────────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a header, but I got stuck here:

                2│      exposes [main, Foo]
                                           ^

                I am expecting the `imports` keyword next, like

                    imports [Animal, default, tame]
                "#
            ),
        )
    }

    #[test]
    fn exposes_identifier() {
        report_header_problem_as(
            indoc!(
                r#"
                interface Foobar
                    exposes [main, @Foo]
                    imports [pf.Task, Base64]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD EXPOSES ───────────────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing an `exposes` list, but I got stuck here:

                1│  interface Foobar
                2│      exposes [main, @Foo]
                                       ^

                I was expecting a type name, value name or function name next, like

                    exposes [Animal, default, tame]
            "#
            ),
        )
    }

    #[test]
    fn invalid_module_name() {
        report_header_problem_as(
            indoc!(
                r#"
                interface foobar
                    exposes [main, @Foo]
                    imports [pf.Task, Base64]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD MODULE NAME ───────────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a header, but got stuck here:

                1│  interface foobar
                              ^

                I am expecting a module name next, like BigNum or Main. Module names
                must start with an uppercase letter.
            "#
            ),
        )
    }

    #[test]
    fn invalid_app_name() {
        report_header_problem_as(
            indoc!(
                r#"
                app foobar
                    exposes [main, @Foo]
                    imports [pf.Task, Base64]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD APP NAME ──────────────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a header, but got stuck here:

                1│  app foobar
                        ^

                I am expecting an application name next, like app "main" or
                app "editor". App names are surrounded by quotation marks.
            "#
            ),
        )
    }

    #[test]
    fn apply_unary_negative() {
        report_problem_as(
            indoc!(
                r#"
                foo = 3

                -foo 1 2
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY ARGS ───────────────────────────────────────── /code/proj/Main.roc ─

                This value is not a function, but it was given 2 arguments:

                3│  -foo 1 2
                    ^^^^

                Are there any missing commas? Or missing parentheses?
            "#
            ),
        )
    }

    #[test]
    fn apply_unary_not() {
        report_problem_as(
            indoc!(
                r#"
                foo = True

                !foo 1 2
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY ARGS ───────────────────────────────────────── /code/proj/Main.roc ─

                This value is not a function, but it was given 2 arguments:

                3│  !foo 1 2
                    ^^^^

                Are there any missing commas? Or missing parentheses?
            "#
            ),
        )
    }

    #[test]
    fn applied_tag_function() {
        report_problem_as(
            indoc!(
                r#"
                x : List [Foo Str]
                x = List.map [1, 2] Foo

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `x` definition:

                1│  x : List [Foo Str]
                2│  x = List.map [1, 2] Foo
                        ^^^^^^^^^^^^^^^^^^^

                This `map` call produces:

                    List [Foo Num a]

                But the type annotation on `x` says it should be:

                    List [Foo Str]
                "#
            ),
        )
    }

    #[test]
    fn pattern_in_parens_open() {
        report_problem_as(
            indoc!(
                r#"
                \( a
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED PARENTHESES ──────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a pattern in parentheses, but I got stuck
                here:

                1│  \( a
                        ^

                I was expecting to see a closing parenthesis before this, so try
                adding a ) and see if that helps?
            "#
            ),
        )
    }

    #[test]
    fn pattern_in_parens_end_comma() {
        report_problem_as(
            indoc!(
                r#"
                \( a,
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED PARENTHESES ──────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a pattern in parentheses, but I got stuck
                here:

                1│  \( a,
                        ^

                I was expecting to see a closing parenthesis before this, so try
                adding a ) and see if that helps?
            "#
            ),
        )
    }

    #[test]
    fn pattern_in_parens_end() {
        report_problem_as(
            indoc!(
                r#"
                \( a
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED PARENTHESES ──────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a pattern in parentheses, but I got stuck
                here:

                1│  \( a
                        ^

                I was expecting to see a closing parenthesis before this, so try
                adding a ) and see if that helps?
            "#
            ),
        )
    }

    #[test]
    fn pattern_in_parens_indent_end() {
        report_problem_as(
            indoc!(
                r#"
                x = \( a
                )
                "#
            ),
            indoc!(
                r#"
                ── NEED MORE INDENTATION ───────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a pattern in parentheses, but I got stuck
                here:

                1│  x = \( a
                2│  )
                    ^

                I need this parenthesis to be indented more. Try adding more spaces
                before it!
            "#
            ),
        )
    }

    #[test]
    fn pattern_in_parens_indent_open() {
        report_problem_as(
            indoc!(
                r#"
                \(
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED PATTERN ──────────────────────────────────── /code/proj/Main.roc ─

                I just started parsing a pattern, but I got stuck here:

                1│  \(
                      ^

                Note: I may be confused by indentation
            "#
            ),
        )
    }

    #[test]
    fn outdented_in_parens() {
        report_problem_as(
            indoc!(
                r#"
                Box : (
                    Str
                )

                4
                "#
            ),
            indoc!(
                r#"
                ── NEED MORE INDENTATION ───────────────────────────────── /code/proj/Main.roc ─

                I am partway through parsing a type in parentheses, but I got stuck
                here:

                1│  Box : (
                2│      Str
                3│  )
                    ^

                I need this parenthesis to be indented more. Try adding more spaces
                before it!
            "#
            ),
        )
    }

    #[test]
    fn backpassing_type_error() {
        report_problem_as(
            indoc!(
                r#"
                x <- List.map ["a", "b"]

                x + 1
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd argument to `map` is not what I expect:

                1│>  x <- List.map ["a", "b"]
                2│>
                3│>  x + 1

                This argument is an anonymous function of type:

                    Num a -> Num a

                But `map` needs the 2nd argument to be:

                    Str -> Num a
            "#
            ),
        )
    }

    #[test]
    fn underscore_let() {
        report_problem_as(
            indoc!(
                r#"
                _ = 3

                4
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                Underscore patterns are not allowed in definitions

                1│  _ = 3
                    ^
            "#
            ),
        )
    }

    #[test]
    fn expect_expr_type_error() {
        report_problem_as(
            indoc!(
                r#"
                expect "foobar"

                4
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This `expect` condition needs to be a Bool:

                1│  expect "foobar"
                           ^^^^^^^^

                Right now it’s a string of type:

                    Str

                But I need every `expect` condition to evaluate to a Bool—either `True`
                or `False`.
            "#
            ),
        )
    }

    #[test]
    fn num_too_general_wildcard() {
        report_problem_as(
            indoc!(
                r#"
                mult : Num.Num *, Num.F64 -> Num.F64
                mult = \a, b -> a * b

                mult 0 0
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd argument to `mul` is not what I expect:

                2│  mult = \a, b -> a * b
                                        ^

                This `b` value is a:

                    F64

                But `mul` needs the 2nd argument to be:

                    Num *

                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `mult` definition:

                1│  mult : Num.Num *, Num.F64 -> Num.F64
                2│  mult = \a, b -> a * b
                                    ^^^^^

                This `mul` call produces:

                    Num *

                But the type annotation on `mult` says it should be:

                    F64
            "#
            ),
        )
    }

    #[test]
    fn num_too_general_named() {
        report_problem_as(
            indoc!(
                r#"
                mult : Num.Num a, Num.F64 -> Num.F64
                mult = \a, b -> a * b

                mult 0 0
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd argument to `mul` is not what I expect:

                2│  mult = \a, b -> a * b
                                        ^

                This `b` value is a:

                    F64

                But `mul` needs the 2nd argument to be:

                    Num a

                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `mult` definition:

                1│  mult : Num.Num a, Num.F64 -> Num.F64
                2│  mult = \a, b -> a * b
                                    ^^^^^

                This `mul` call produces:

                    Num a

                But the type annotation on `mult` says it should be:

                    F64
            "#
            ),
        )
    }

    #[test]
    fn inference_var_not_enough_in_alias() {
        report_problem_as(
            indoc!(
                r#"
                Result a b : [Ok a, Err b]

                canIGo : _ -> Result _
                canIGo = \color ->
                    when color is
                        "green" -> Ok "go!"
                        "yellow" -> Err (SlowIt "whoa, let's slow down!")
                        "red" -> Err (StopIt "absolutely not")
                        _ -> Err (UnknownColor "this is a weird stoplight")
                canIGo
                "#
            ),
            indoc!(
                r#"
                ── TOO FEW TYPE ARGUMENTS ──────────────────────────────── /code/proj/Main.roc ─

                The `Result` alias expects 2 type arguments, but it got 1 instead:

                3│  canIGo : _ -> Result _
                                  ^^^^^^^^

                Are there missing parentheses?
                "#
            ),
        )
    }

    #[test]
    fn inference_var_too_many_in_alias() {
        report_problem_as(
            indoc!(
                r#"
                Result a b : [Ok a, Err b]

                canIGo : _ -> Result _ _ _
                canIGo = \color ->
                    when color is
                        "green" -> Ok "go!"
                        "yellow" -> Err (SlowIt "whoa, let's slow down!")
                        "red" -> Err (StopIt "absolutely not")
                        _ -> Err (UnknownColor "this is a weird stoplight")
                canIGo
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY TYPE ARGUMENTS ─────────────────────────────── /code/proj/Main.roc ─

                The `Result` alias expects 2 type arguments, but it got 3 instead:

                3│  canIGo : _ -> Result _ _ _
                                  ^^^^^^^^^^^^

                Are there missing parentheses?
                "#
            ),
        )
    }

    #[test]
    fn inference_var_conflict_in_rigid_links() {
        report_problem_as(
            indoc!(
                r#"
                f : a -> (_ -> b)
                f = \x -> \y -> if x == y then x else y
                f
                "#
            ),
            // TODO: We should tell the user that we inferred `_` as `a`
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `f` definition:

                1│  f : a -> (_ -> b)
                2│  f = \x -> \y -> if x == y then x else y
                              ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                The body is an anonymous function of type:

                    a -> a

                But the type annotation on `f` says it should be:

                    a -> b

                Tip: Your type annotation uses `a` and `b` as separate type variables.
                Your code seems to be saying they are the same though. Maybe they
                should be the same in your type annotation? Maybe your code uses them
                in a weird way?
                "#
            ),
        )
    }

    #[test]
    fn error_wildcards_are_related() {
        report_problem_as(
            indoc!(
                r#"
                f : * -> *
                f = \x -> x

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `f` definition:

                1│  f : * -> *
                2│  f = \x -> x
                              ^

                The type annotation on `f` says this `x` value should have the type:

                    *

                However, the type of this `x` value is connected to another type in a
                way that isn't reflected in this annotation.

                Tip: Any connection between types must use a named type variable, not
                a `*`! Maybe the annotation  on `f` should have a named type variable in
                place of the `*`?
                "#
            ),
        )
    }

    #[test]
    fn error_nested_wildcards_are_related() {
        report_problem_as(
            indoc!(
                r#"
                f : a, b, * -> {x: a, y: b, z: *}
                f = \x, y, z -> {x, y, z}

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `f` definition:

                1│  f : a, b, * -> {x: a, y: b, z: *}
                2│  f = \x, y, z -> {x, y, z}
                                    ^^^^^^^^^

                The type annotation on `f` says the body is a record should have the
                type:

                    { x : a, y : b, z : * }

                However, the type of the body is a record is connected to another type
                in a way that isn't reflected in this annotation.

                Tip: Any connection between types must use a named type variable, not
                a `*`! Maybe the annotation  on `f` should have a named type variable in
                place of the `*`?
                "#
            ),
        )
    }

    #[test]
    fn error_wildcards_are_related_in_nested_defs() {
        report_problem_as(
            indoc!(
                r#"
                f : a, b, * -> *
                f = \_, _, x2 ->
                    inner : * -> *
                    inner = \y -> y
                    inner x2

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `inner` definition:

                3│      inner : * -> *
                4│      inner = \y -> y
                                      ^

                The type annotation on `inner` says this `y` value should have the type:

                    *

                However, the type of this `y` value is connected to another type in a
                way that isn't reflected in this annotation.

                Tip: Any connection between types must use a named type variable, not
                a `*`! Maybe the annotation  on `inner` should have a named type variable
                in place of the `*`?
                "#
            ),
        )
    }

    #[test]
    fn error_inline_alias_not_an_alias() {
        report_problem_as(
            indoc!(
                r#"
                f : List elem -> [Nil, Cons elem a] as a
                "#
            ),
            indoc!(
                r#"
                ── NOT AN INLINE ALIAS ─────────────────────────────────── /code/proj/Main.roc ─

                The inline type after this `as` is not a type alias:

                1│  f : List elem -> [Nil, Cons elem a] as a
                                                           ^

                Inline alias types must start with an uppercase identifier and be
                followed by zero or more type arguments, like Point or List a.
                "#
            ),
        )
    }

    #[test]
    fn error_inline_alias_qualified() {
        report_problem_as(
            indoc!(
                r#"
                f : List elem -> [Nil, Cons elem a] as Module.LinkedList a
                "#
            ),
            indoc!(
                r#"
                ── QUALIFIED ALIAS NAME ────────────────────────────────── /code/proj/Main.roc ─

                This type alias has a qualified name:

                1│  f : List elem -> [Nil, Cons elem a] as Module.LinkedList a
                                                           ^

                An alias introduces a new name to the current scope, so it must be
                unqualified.
                "#
            ),
        )
    }

    #[test]
    fn error_inline_alias_argument_uppercase() {
        report_problem_as(
            indoc!(
                r#"
                f : List elem -> [Nil, Cons elem a] as LinkedList U
                "#
            ),
            indoc!(
                r#"
                ── TYPE ARGUMENT NOT LOWERCASE ─────────────────────────── /code/proj/Main.roc ─

                This alias type argument is not lowercase:

                1│  f : List elem -> [Nil, Cons elem a] as LinkedList U
                                                                      ^

                All type arguments must be lowercase.
                "#
            ),
        )
    }

    #[test]
    fn mismatched_single_tag_arg() {
        report_problem_as(
            indoc!(
                r#"
                isEmpty =
                    \email ->
                        Email str = email
                        Str.isEmpty str

                isEmpty (Name "boo")
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 1st argument to `isEmpty` is not what I expect:

                6│  isEmpty (Name "boo")
                             ^^^^^^^^^^

                This `Name` tag application has the type:

                    [Name Str]a

                But `isEmpty` needs the 1st argument to be:

                    [Email Str]

                Tip: Seems like a tag typo. Maybe `Name` should be `Email`?

                Tip: Can more type annotations be added? Type annotations always help
                me give more specific messages, and I think they could help a lot in
                this case
                "#
            ),
        )
    }

    #[test]
    fn issue_2326() {
        report_problem_as(
            indoc!(
                r#"
                C a b : a -> D a b
                D a b : { a, b }

                f : C a Num.Nat -> D a Num.Nat
                f = \c -> c 6
                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 1st argument to `c` is not what I expect:

                5│  f = \c -> c 6
                                ^

                This argument is a number of type:

                    Num a

                But `c` needs the 1st argument to be:

                    a

                Tip: The type annotation uses the type variable `a` to say that this
                definition can produce any type of value. But in the body I see that
                it will only produce a `Num` value of a single specific type. Maybe
                change the type annotation to be more specific? Maybe change the code
                to be more general?
                "#
            ),
        )
    }

    #[test]
    fn issue_2380_annotations_only() {
        report_problem_as(
            indoc!(
                r#"
                F : F
                a : F
                a
                "#
            ),
            indoc!(
                r#"
                ── CYCLIC ALIAS ────────────────────────────────────────── /code/proj/Main.roc ─

                The `F` alias is self-recursive in an invalid way:

                1│  F : F
                    ^

                Recursion in aliases is only allowed if recursion happens behind a
                tagged union, at least one variant of which is not recursive.
                "#
            ),
        )
    }

    #[test]
    fn issue_2380_typed_body() {
        report_problem_as(
            indoc!(
                r#"
                F : F
                a : F
                a = 1
                a
                "#
            ),
            indoc!(
                r#"
                ── CYCLIC ALIAS ────────────────────────────────────────── /code/proj/Main.roc ─

                The `F` alias is self-recursive in an invalid way:

                1│  F : F
                    ^

                Recursion in aliases is only allowed if recursion happens behind a
                tagged union, at least one variant of which is not recursive.
                "#
            ),
        )
    }

    #[test]
    fn issue_2380_alias_with_vars() {
        report_problem_as(
            indoc!(
                r#"
                F a b : F a b
                a : F Str Str
                a
                "#
            ),
            indoc!(
                r#"
                ── CYCLIC ALIAS ────────────────────────────────────────── /code/proj/Main.roc ─

                The `F` alias is self-recursive in an invalid way:

                1│  F a b : F a b
                    ^

                Recursion in aliases is only allowed if recursion happens behind a
                tagged union, at least one variant of which is not recursive.
                "#
            ),
        )
    }

    #[test]
    fn issue_2167_record_field_optional_and_required_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                Job : [Job { inputs : List Str }]
                job : { inputs ? List Str } -> Job
                job = \{ inputs } ->
                    Job { inputs }

                job { inputs: ["build", "test"] }
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 1st argument to `job` is weird:

                3│  job = \{ inputs } ->
                           ^^^^^^^^^^

                The argument is a pattern that matches record values of type:

                    { inputs : List Str }

                But the annotation on `job` says the 1st argument should be:

                    { inputs ? List Str }

                Tip: To extract the `.inputs` field it must be non-optional, but the
                type says this field is optional. Learn more about optional fields at
                TODO.
                "#
            ),
        )
    }

    #[test]
    fn unify_recursive_with_nonrecursive() {
        report_problem_as(
            indoc!(
                r#"
                Job : [Job { inputs : List Job }]

                job : { inputs : List Str } -> Job
                job = \{ inputs } ->
                    Job { inputs }

                job { inputs: ["build", "test"] }
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `job` definition:

                3│  job : { inputs : List Str } -> Job
                4│  job = \{ inputs } ->
                5│      Job { inputs }
                        ^^^^^^^^^^^^^^

                This `Job` tag application has the type:

                    [Job { inputs : List Str }]

                But the type annotation on `job` says it should be:

                    [Job { inputs : List a }] as a
                "#
            ),
        )
    }

    #[test]
    fn nested_datatype() {
        report_problem_as(
            indoc!(
                r#"
                Nested a : [Chain a (Nested (List a)), Term]

                s : Nested Str

                s
                "#
            ),
            indoc!(
                r#"
                ── NESTED DATATYPE ─────────────────────────────────────── /code/proj/Main.roc ─

                `Nested` is a nested datatype. Here is one recursive usage of it:

                1│  Nested a : [Chain a (Nested (List a)), Term]
                                         ^^^^^^^^^^^^^^^

                But recursive usages of `Nested` must match its definition:

                1│  Nested a : [Chain a (Nested (List a)), Term]
                    ^^^^^^^^

                Nested datatypes are not supported in Roc.

                Hint: Consider rewriting the definition of `Nested` to use the recursive type with the same arguments.
                "#
            ),
        )
    }

    #[test]
    fn nested_datatype_inline() {
        report_problem_as(
            indoc!(
                r#"
                f : {} -> [Chain a (Nested (List a)), Term] as Nested a

                f
                "#
            ),
            indoc!(
                r#"
                ── NESTED DATATYPE ─────────────────────────────────────── /code/proj/Main.roc ─

                `Nested` is a nested datatype. Here is one recursive usage of it:

                1│  f : {} -> [Chain a (Nested (List a)), Term] as Nested a
                                        ^^^^^^^^^^^^^^^

                But recursive usages of `Nested` must match its definition:

                1│  f : {} -> [Chain a (Nested (List a)), Term] as Nested a
                                                                   ^^^^^^^^

                Nested datatypes are not supported in Roc.

                Hint: Consider rewriting the definition of `Nested` to use the recursive type with the same arguments.
                "#
            ),
        )
    }

    macro_rules! mismatched_suffix_tests {
        ($($number:expr, $suffix:expr, $name:ident)*) => {$(
            #[test]
            fn $name() {
                let number = $number.to_string();
                let mut typ = $suffix.to_string();
                typ.get_mut(0..1).unwrap().make_ascii_uppercase();
                let bad_type = if $suffix == "u8" { "I8" } else { "U8" };
                let carets = "^".repeat(number.len() + $suffix.len());
                let kind = match $suffix {
                    "dec"|"f32"|"f64" => "a frac",
                    _ => "an integer",
                };

                report_problem_as(
                    &format!(indoc!(
                        r#"
                        use : Num.{} -> Num.U8
                        use {}{}
                        "#
                    ), bad_type, number, $suffix),
                    &format!(indoc!(
                        r#"
                        ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                        The 1st argument to `use` is not what I expect:

                        2│  use {}{}
                                {}

                        This argument is {} of type:

                            {}

                        But `use` needs the 1st argument to be:

                            {}
                        "#
                    ), number, $suffix, carets, kind, typ, bad_type),
                )
            }
        )*}
    }

    mismatched_suffix_tests! {
        1, "u8",   mismatched_suffix_u8
        1, "u16",  mismatched_suffix_u16
        1, "u32",  mismatched_suffix_u32
        1, "u64",  mismatched_suffix_u64
        1, "u128", mismatched_suffix_u128
        1, "i8",   mismatched_suffix_i8
        1, "i16",  mismatched_suffix_i16
        1, "i32",  mismatched_suffix_i32
        1, "i64",  mismatched_suffix_i64
        1, "i128", mismatched_suffix_i128
        1, "nat",  mismatched_suffix_nat
        1, "dec",  mismatched_suffix_dec
        1, "f32",  mismatched_suffix_f32
        1, "f64",  mismatched_suffix_f64
    }

    macro_rules! mismatched_suffix_tests_in_pattern {
        ($($number:expr, $suffix:expr, $name:ident)*) => {$(
            #[test]
            fn $name() {
                let number = $number.to_string();
                let mut typ = $suffix.to_string();
                typ.get_mut(0..1).unwrap().make_ascii_uppercase();
                let bad_suffix = if $suffix == "u8" { "i8" } else { "u8" };
                let bad_type = if $suffix == "u8" { "I8" } else { "U8" };

                report_problem_as(
                    &format!(indoc!(
                        r#"
                        when {}{} is
                            {}{} -> 1
                            _ -> 1
                        "#
                    ), number, bad_suffix, number, $suffix),
                    &format!(indoc!(
                        r#"
                        ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                        The branches of this `when` expression don't match the condition:

                        1│>  when {}{} is
                        2│       {}{} -> 1
                        3│       _ -> 1

                        The `when` condition is an integer of type:

                            {}

                        But the branch patterns have type:

                            {}

                        The branches must be cases of the `when` condition's type!
                        "#
                    ), number, bad_suffix, number, $suffix, bad_type, typ),
                )
            }
        )*}
    }

    mismatched_suffix_tests_in_pattern! {
        1, "u8",   mismatched_suffix_u8_pattern
        1, "u16",  mismatched_suffix_u16_pattern
        1, "u32",  mismatched_suffix_u32_pattern
        1, "u64",  mismatched_suffix_u64_pattern
        1, "u128", mismatched_suffix_u128_pattern
        1, "i8",   mismatched_suffix_i8_pattern
        1, "i16",  mismatched_suffix_i16_pattern
        1, "i32",  mismatched_suffix_i32_pattern
        1, "i64",  mismatched_suffix_i64_pattern
        1, "i128", mismatched_suffix_i128_pattern
        1, "nat",  mismatched_suffix_nat_pattern
        1, "dec",  mismatched_suffix_dec_pattern
        1, "f32",  mismatched_suffix_f32_pattern
        1, "f64",  mismatched_suffix_f64_pattern
    }

    #[test]
    fn bad_numeric_literal_suffix() {
        report_problem_as(
            indoc!(
                r#"
                1u256
                "#
            ),
            // TODO: link to number suffixes
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This integer literal contains an invalid digit:

                1│  1u256
                    ^^^^^

                Integer literals can only contain the digits
                0-9, or have an integer suffix.

                Tip: Learn more about number literals at TODO
                "#
            ),
        )
    }

    #[test]
    fn numer_literal_multi_suffix() {
        report_problem_as(
            indoc!(
                r#"
                1u8u8
                "#
            ),
            // TODO: link to number suffixes
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                This integer literal contains an invalid digit:

                1│  1u8u8
                    ^^^^^

                Integer literals can only contain the digits
                0-9, or have an integer suffix.

                Tip: Learn more about number literals at TODO
                "#
            ),
        )
    }

    #[test]
    fn int_literal_has_float_suffix() {
        report_problem_as(
            indoc!(
                r#"
                0b1f32
                "#
            ),
            indoc!(
                r#"
                ── CONFLICTING NUMBER SUFFIX ───────────────────────────── /code/proj/Main.roc ─

                This number literal is an integer, but it has a float suffix:

                1│  0b1f32
                    ^^^^^^
                "#
            ),
        )
    }

    #[test]
    fn float_literal_has_int_suffix() {
        report_problem_as(
            indoc!(
                r#"
                1.0u8
                "#
            ),
            indoc!(
                r#"
                ── CONFLICTING NUMBER SUFFIX ───────────────────────────── /code/proj/Main.roc ─

                This number literal is a float, but it has an integer suffix:

                1│  1.0u8
                    ^^^^^
                "#
            ),
        )
    }

    #[test]
    fn u8_overflow() {
        report_problem_as(
            "256u8",
            indoc!(
                r#"
                ── NUMBER OVERFLOWS SUFFIX ─────────────────────────────── /code/proj/Main.roc ─

                This integer literal overflows the type indicated by its suffix:

                1│  256u8
                    ^^^^^

                Tip: The suffix indicates this integer is a U8, whose maximum value is
                255.
                "#
            ),
        )
    }

    #[test]
    fn negative_u8() {
        report_problem_as(
            "-1u8",
            indoc!(
                r#"
                ── NUMBER UNDERFLOWS SUFFIX ────────────────────────────── /code/proj/Main.roc ─

                This integer literal underflows the type indicated by its suffix:

                1│  -1u8
                    ^^^^

                Tip: The suffix indicates this integer is a U8, whose minimum value is
                0.
                "#
            ),
        )
    }

    #[test]
    fn u16_overflow() {
        report_problem_as(
            "65536u16",
            indoc!(
                r#"
                ── NUMBER OVERFLOWS SUFFIX ─────────────────────────────── /code/proj/Main.roc ─

                This integer literal overflows the type indicated by its suffix:

                1│  65536u16
                    ^^^^^^^^

                Tip: The suffix indicates this integer is a U16, whose maximum value
                is 65535.
                "#
            ),
        )
    }

    #[test]
    fn negative_u16() {
        report_problem_as(
            "-1u16",
            indoc!(
                r#"
                ── NUMBER UNDERFLOWS SUFFIX ────────────────────────────── /code/proj/Main.roc ─

                This integer literal underflows the type indicated by its suffix:

                1│  -1u16
                    ^^^^^

                Tip: The suffix indicates this integer is a U16, whose minimum value
                is 0.
                "#
            ),
        )
    }

    #[test]
    fn u32_overflow() {
        report_problem_as(
            "4_294_967_296u32",
            indoc!(
                r#"
                ── NUMBER OVERFLOWS SUFFIX ─────────────────────────────── /code/proj/Main.roc ─

                This integer literal overflows the type indicated by its suffix:

                1│  4_294_967_296u32
                    ^^^^^^^^^^^^^^^^

                Tip: The suffix indicates this integer is a U32, whose maximum value
                is 4_294_967_295.
                "#
            ),
        )
    }

    #[test]
    fn negative_u32() {
        report_problem_as(
            "-1u32",
            indoc!(
                r#"
                ── NUMBER UNDERFLOWS SUFFIX ────────────────────────────── /code/proj/Main.roc ─

                This integer literal underflows the type indicated by its suffix:

                1│  -1u32
                    ^^^^^

                Tip: The suffix indicates this integer is a U32, whose minimum value
                is 0.
                "#
            ),
        )
    }

    #[test]
    fn u64_overflow() {
        report_problem_as(
            "18_446_744_073_709_551_616u64",
            indoc!(
                r#"
                ── NUMBER OVERFLOWS SUFFIX ─────────────────────────────── /code/proj/Main.roc ─

                This integer literal overflows the type indicated by its suffix:

                1│  18_446_744_073_709_551_616u64
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Tip: The suffix indicates this integer is a U64, whose maximum value
                is 18_446_744_073_709_551_615.
                "#
            ),
        )
    }

    #[test]
    fn negative_u64() {
        report_problem_as(
            "-1u64",
            indoc!(
                r#"
                ── NUMBER UNDERFLOWS SUFFIX ────────────────────────────── /code/proj/Main.roc ─

                This integer literal underflows the type indicated by its suffix:

                1│  -1u64
                    ^^^^^

                Tip: The suffix indicates this integer is a U64, whose minimum value
                is 0.
                "#
            ),
        )
    }

    #[test]
    fn negative_u128() {
        report_problem_as(
            "-1u128",
            indoc!(
                r#"
                ── NUMBER UNDERFLOWS SUFFIX ────────────────────────────── /code/proj/Main.roc ─

                This integer literal underflows the type indicated by its suffix:

                1│  -1u128
                    ^^^^^^

                Tip: The suffix indicates this integer is a U128, whose minimum value
                is 0.
                "#
            ),
        )
    }

    #[test]
    fn i8_overflow() {
        report_problem_as(
            "128i8",
            indoc!(
                r#"
                ── NUMBER OVERFLOWS SUFFIX ─────────────────────────────── /code/proj/Main.roc ─

                This integer literal overflows the type indicated by its suffix:

                1│  128i8
                    ^^^^^

                Tip: The suffix indicates this integer is a I8, whose maximum value is
                127.
                "#
            ),
        )
    }

    #[test]
    fn i8_underflow() {
        report_problem_as(
            "-129i8",
            indoc!(
                r#"
                ── NUMBER UNDERFLOWS SUFFIX ────────────────────────────── /code/proj/Main.roc ─

                This integer literal underflows the type indicated by its suffix:

                1│  -129i8
                    ^^^^^^

                Tip: The suffix indicates this integer is a I8, whose minimum value is
                -128.
                "#
            ),
        )
    }

    #[test]
    fn i16_overflow() {
        report_problem_as(
            "32768i16",
            indoc!(
                r#"
                ── NUMBER OVERFLOWS SUFFIX ─────────────────────────────── /code/proj/Main.roc ─

                This integer literal overflows the type indicated by its suffix:

                1│  32768i16
                    ^^^^^^^^

                Tip: The suffix indicates this integer is a I16, whose maximum value
                is 32767.
                "#
            ),
        )
    }

    #[test]
    fn i16_underflow() {
        report_problem_as(
            "-32769i16",
            indoc!(
                r#"
                ── NUMBER UNDERFLOWS SUFFIX ────────────────────────────── /code/proj/Main.roc ─

                This integer literal underflows the type indicated by its suffix:

                1│  -32769i16
                    ^^^^^^^^^

                Tip: The suffix indicates this integer is a I16, whose minimum value
                is -32768.
                "#
            ),
        )
    }

    #[test]
    fn i32_overflow() {
        report_problem_as(
            "2_147_483_648i32",
            indoc!(
                r#"
                ── NUMBER OVERFLOWS SUFFIX ─────────────────────────────── /code/proj/Main.roc ─

                This integer literal overflows the type indicated by its suffix:

                1│  2_147_483_648i32
                    ^^^^^^^^^^^^^^^^

                Tip: The suffix indicates this integer is a I32, whose maximum value
                is 2_147_483_647.
                "#
            ),
        )
    }

    #[test]
    fn i32_underflow() {
        report_problem_as(
            "-2_147_483_649i32",
            indoc!(
                r#"
                ── NUMBER UNDERFLOWS SUFFIX ────────────────────────────── /code/proj/Main.roc ─

                This integer literal underflows the type indicated by its suffix:

                1│  -2_147_483_649i32
                    ^^^^^^^^^^^^^^^^^

                Tip: The suffix indicates this integer is a I32, whose minimum value
                is -2_147_483_648.
                "#
            ),
        )
    }

    #[test]
    fn i64_overflow() {
        report_problem_as(
            "9_223_372_036_854_775_808i64",
            indoc!(
                r#"
                ── NUMBER OVERFLOWS SUFFIX ─────────────────────────────── /code/proj/Main.roc ─

                This integer literal overflows the type indicated by its suffix:

                1│  9_223_372_036_854_775_808i64
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Tip: The suffix indicates this integer is a I64, whose maximum value
                is 9_223_372_036_854_775_807.
                "#
            ),
        )
    }

    #[test]
    fn i64_underflow() {
        report_problem_as(
            "-9_223_372_036_854_775_809i64",
            indoc!(
                r#"
                ── NUMBER UNDERFLOWS SUFFIX ────────────────────────────── /code/proj/Main.roc ─

                This integer literal underflows the type indicated by its suffix:

                1│  -9_223_372_036_854_775_809i64
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Tip: The suffix indicates this integer is a I64, whose minimum value
                is -9_223_372_036_854_775_808.
                "#
            ),
        )
    }

    #[test]
    fn i128_overflow() {
        report_problem_as(
            "170_141_183_460_469_231_731_687_303_715_884_105_728i128",
            indoc!(
                r#"
                ── NUMBER OVERFLOWS SUFFIX ─────────────────────────────── /code/proj/Main.roc ─

                This integer literal overflows the type indicated by its suffix:

                1│  170_141_183_460_469_231_731_687_303_715_884_105_728i128
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Tip: The suffix indicates this integer is a I128, whose maximum value
                is 170_141_183_460_469_231_731_687_303_715_884_105_727.
                "#
            ),
        )
    }

    #[test]
    fn list_get_negative_number() {
        report_problem_as(
            indoc!(
                r#"
                 List.get [1,2,3] -1
                 "#
            ),
            // TODO: this error message could be improved, e.g. something like "This argument can
            // be used as ... because of its literal value"
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd argument to `get` is not what I expect:

                1│  List.get [1,2,3] -1
                                     ^^

                This argument is a number of type:

                    I8, I16, I32, I64, I128, F32, F64, or Dec

                But `get` needs the 2nd argument to be:

                    Nat
                "#
            ),
        )
    }

    #[test]
    fn list_get_negative_number_indirect() {
        report_problem_as(
            indoc!(
                r#"
                 a = -9_223_372_036_854
                 List.get [1,2,3] a
                 "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd argument to `get` is not what I expect:

                2│  List.get [1,2,3] a
                                     ^

                This `a` value is a:

                    I64, I128, F32, F64, or Dec

                But `get` needs the 2nd argument to be:

                    Nat
                "#
            ),
        )
    }

    #[test]
    fn list_get_negative_number_double_indirect() {
        report_problem_as(
            indoc!(
                r#"
                 a = -9_223_372_036_854
                 b = a
                 List.get [1,2,3] b
                 "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd argument to `get` is not what I expect:

                3│  List.get [1,2,3] b
                                     ^

                This `b` value is a:

                    I64, I128, F32, F64, or Dec

                But `get` needs the 2nd argument to be:

                    Nat
                "#
            ),
        )
    }

    #[test]
    fn compare_unsigned_to_signed() {
        report_problem_as(
            indoc!(
                r#"
                when -1 is
                   1u8 -> 1
                   _ -> 1
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The branches of this `when` expression don't match the condition:

                1│>  when -1 is
                2│      1u8 -> 1
                3│      _ -> 1

                The `when` condition is a number of type:

                    I8, I16, I32, I64, I128, F32, F64, or Dec

                But the branch patterns have type:

                    U8

                The branches must be cases of the `when` condition's type!
                "#
            ),
        )
    }

    #[test]
    fn recursive_type_alias_is_newtype() {
        report_problem_as(
            indoc!(
                r#"
                R a : [Only (R a)]

                v : R Str
                v
                "#
            ),
            indoc!(
                r#"
                ── CYCLIC ALIAS ────────────────────────────────────────── /code/proj/Main.roc ─

                The `R` alias is self-recursive in an invalid way:

                1│  R a : [Only (R a)]
                    ^

                Recursion in aliases is only allowed if recursion happens behind a
                tagged union, at least one variant of which is not recursive.
                "#
            ),
        )
    }

    #[test]
    fn recursive_type_alias_is_newtype_deep() {
        report_problem_as(
            indoc!(
                r#"
                R a : [Only { very: [Deep (R a)] }]

                v : R Str
                v
                "#
            ),
            indoc!(
                r#"
                ── CYCLIC ALIAS ────────────────────────────────────────── /code/proj/Main.roc ─

                The `R` alias is self-recursive in an invalid way:

                1│  R a : [Only { very: [Deep (R a)] }]
                    ^

                Recursion in aliases is only allowed if recursion happens behind a
                tagged union, at least one variant of which is not recursive.
                "#
            ),
        )
    }

    #[test]
    fn recursive_type_alias_is_newtype_mutual() {
        report_problem_as(
            indoc!(
                r#"
                Foo a : [Thing (Bar a)]
                Bar a : [Stuff (Foo a)]

                v : Bar Str
                v
                "#
            ),
            indoc!(
                r#"
                ── CYCLIC ALIAS ────────────────────────────────────────── /code/proj/Main.roc ─

                The `Foo` alias is recursive in an invalid way:

                1│  Foo a : [Thing (Bar a)]
                    ^^^

                The `Foo` alias depends on itself through the following chain of
                definitions:

                    ┌─────┐
                    │     Foo
                    │     ↓
                    │     Bar
                    └─────┘

                Recursion in aliases is only allowed if recursion happens behind a
                tagged union, at least one variant of which is not recursive.
                "#
            ),
        )
    }

    #[test]
    fn issue_2458() {
        report_problem_as(
            indoc!(
                r#"
                Result a b : [Ok a, Err b]

                Foo a : [Blah (Result (Bar a) [])]
                Bar a : Foo a

                v : Bar Str
                v
                "#
            ),
            "",
        )
    }

    #[test]
    fn opaque_type_not_in_scope() {
        report_problem_as(
            indoc!(
                r#"
                @Age 21
                "#
            ),
            indoc!(
                r#"
                ── OPAQUE TYPE NOT DEFINED ─────────────────────────────── /code/proj/Main.roc ─

                The opaque type Age referenced here is not defined:

                1│  @Age 21
                    ^^^^

                Note: It looks like there are no opaque types declared in this scope yet!
                "#
            ),
        )
    }

    #[test]
    fn opaque_reference_not_opaque_type() {
        report_problem_as(
            indoc!(
                r#"
                Age : Num.U8

                @Age 21
                "#
            ),
            indoc!(
                r#"
                ── OPAQUE TYPE NOT DEFINED ─────────────────────────────── /code/proj/Main.roc ─

                The opaque type Age referenced here is not defined:

                3│  @Age 21
                    ^^^^

                Note: There is an alias of the same name:

                1│  Age : Num.U8
                    ^^^

                Note: It looks like there are no opaque types declared in this scope yet!

                ── UNUSED DEFINITION ───────────────────────────────────── /code/proj/Main.roc ─

                `Age` is not used anywhere in your code.

                1│  Age : Num.U8
                    ^^^^^^^^^^^^

                If you didn't intend on using `Age` then remove it so future readers of
                your code don't wonder why it is there.
                "#
            ),
        )
    }

    #[test]
    fn qualified_opaque_reference() {
        report_problem_as(
            indoc!(
                r#"
                OtherModule.@Age 21
                "#
            ),
            // TODO: get rid of the first error. Consider parsing OtherModule.@Age to completion
            // and checking it during can. The reason the error appears is because it is parsed as
            // Apply(Error(OtherModule), [@Age, 21])
            indoc!(
                r#"
                ── OPAQUE TYPE NOT APPLIED ─────────────────────────────── /code/proj/Main.roc ─

                This opaque type is not applied to an argument:

                1│  OtherModule.@Age 21
                                ^^^^

                Note: Opaque types always wrap exactly one argument!

                ── SYNTAX PROBLEM ──────────────────────────────────────── /code/proj/Main.roc ─

                I am trying to parse a qualified name here:

                1│  OtherModule.@Age 21
                                ^

                I was expecting to see an identifier next, like height. A complete
                qualified name looks something like Json.Decode.string.
                "#
            ),
        )
    }

    #[test]
    fn opaque_used_outside_declaration_scope() {
        report_problem_as(
            indoc!(
                r#"
                age =
                    Age := Num.U8
                    21u8

                @Age age
                "#
            ),
            // TODO(opaques): there is a potential for a better error message here, if the usage of
            // `@Age` can be linked to the declaration of `Age` inside `age`, and a suggestion to
            // raise that declaration to the outer scope.
            indoc!(
                r#"
                ── UNUSED DEFINITION ───────────────────────────────────── /code/proj/Main.roc ─

                `Age` is not used anywhere in your code.

                2│      Age := Num.U8
                        ^^^^^^^^^^^^^

                If you didn't intend on using `Age` then remove it so future readers of
                your code don't wonder why it is there.

                ── OPAQUE TYPE NOT DEFINED ─────────────────────────────── /code/proj/Main.roc ─

                The opaque type Age referenced here is not defined:

                5│  @Age age
                    ^^^^

                Note: It looks like there are no opaque types declared in this scope yet!
                "#
            ),
        )
    }

    #[test]
    fn unimported_modules_reported() {
        report_problem_as(
            indoc!(
                r#"
                main : Task.Task {} []
                main = "whatever man you don't even know my type"
                main
                "#
            ),
            indoc!(
                r#"
                ── MODULE NOT IMPORTED ─────────────────────────────────── /code/proj/Main.roc ─

                The `Task` module is not imported:

                1│  main : Task.Task {} []
                           ^^^^^^^^^^^^^^^

                Is there an import missing? Perhaps there is a typo. Did you mean one
                of these?

                    Test
                    List
                    Num
                    Box
                "#
            ),
        )
    }

    #[test]
    fn opaque_mismatch_check() {
        report_problem_as(
            indoc!(
                r#"
                Age := Num.U8

                n : Age
                n = @Age ""

                n
                "#
            ),
            // TODO(opaques): error could be improved by saying that the opaque definition demands
            // that the argument be a U8, and linking to the definitin!
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This expression is used in an unexpected way:

                4│  n = @Age ""
                             ^^

                This argument to an opaque type has type:

                    Str

                But you are trying to use it as:

                    U8
                "#
            ),
        )
    }

    #[test]
    fn opaque_mismatch_infer() {
        report_problem_as(
            indoc!(
                r#"
                F n := n

                if True
                then @F ""
                else @F {}
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                This expression is used in an unexpected way:

                5│  else @F {}
                            ^^

                This argument to an opaque type has type:

                    {}

                But you are trying to use it as:

                    Str
                "#
            ),
        )
    }

    #[test]
    fn opaque_creation_is_not_wrapped() {
        report_problem_as(
            indoc!(
                r#"
                F n := n

                v : F Str
                v = ""

                v
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                Something is off with the body of the `v` definition:

                3│  v : F Str
                4│  v = ""
                        ^^

                The body is a string of type:

                    Str

                But the type annotation on `v` says it should be:

                    F Str

                Tip: Type comparisons between an opaque type are only ever equal if
                both types are the same opaque type. Did you mean to create an opaque
                type by wrapping it? If I have an opaque type Age := U32 I can create
                an instance of this opaque type by doing @Age 23.
                "#
            ),
        )
    }

    #[test]
    fn opaque_mismatch_pattern_check() {
        report_problem_as(
            indoc!(
                r#"
                Age := Num.U8

                f : Age -> Num.U8
                f = \Age n -> n

                f
                "#
            ),
            // TODO(opaques): error could be improved by saying that the user-provided pattern
            // probably wants to change "Age" to "@Age"!
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 1st argument to `f` is weird:

                4│  f = \Age n -> n
                         ^^^^^

                The argument is a pattern that matches a `Age` tag of type:

                    [Age a]

                But the annotation on `f` says the 1st argument should be:

                    Age

                Tip: Type comparisons between an opaque type are only ever equal if
                both types are the same opaque type. Did you mean to create an opaque
                type by wrapping it? If I have an opaque type Age := U32 I can create
                an instance of this opaque type by doing @Age 23.
                "#
            ),
        )
    }

    #[test]
    fn opaque_mismatch_pattern_infer() {
        report_problem_as(
            indoc!(
                r#"
                F n := n

                \x ->
                    when x is
                        @F A -> ""
                        @F {} -> ""
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 2nd pattern in this `when` does not match the previous ones:

                6│          @F {} -> ""
                            ^^^^^

                The 2nd pattern is trying to matchF unwrappings of type:

                    F {}a

                But all the previous branches match:

                    F [A]a
                "#
            ),
        )
    }

    #[test]
    fn opaque_pattern_match_not_exhaustive_tag() {
        report_problem_as(
            indoc!(
                r#"
                F n := n

                v : F [A, B, C]

                when v is
                    @F A -> ""
                    @F B -> ""
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The branches of this `when` expression don't match the condition:

                5│>  when v is
                6│       @F A -> ""
                7│       @F B -> ""

                This `v` value is a:

                    F [A, B, C]

                But the branch patterns have type:

                    F [A, B]

                The branches must be cases of the `when` condition's type!

                Tip: Looks like the branches are missing coverage of the `C` tag.

                Tip: Maybe you need to add a catch-all branch, like `_`?
                "#
            ),
        )
    }

    #[test]
    fn opaque_pattern_match_not_exhaustive_int() {
        report_problem_as(
            indoc!(
                r#"
                F n := n

                v : F Num.U8

                when v is
                    @F 1 -> ""
                    @F 2 -> ""
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────── /code/proj/Main.roc ─

                This `when` does not cover all the possibilities:

                5│>  when v is
                6│>      @F 1 -> ""
                7│>      @F 2 -> ""

                Other possibilities include:

                    @F _

                I would have to crash if I saw one of those! Add branches for them!
                "#
            ),
        )
    }

    #[test]
    fn let_polymorphism_with_scoped_type_variables() {
        report_problem_as(
            indoc!(
                r#"
                f : a -> a
                f = \x ->
                    y : a -> a
                    y = \z -> z

                    n = y 1u8
                    x1 = y x
                    (\_ -> x1) n

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

                The 1st argument to `y` is not what I expect:

                6│      n = y 1u8
                              ^^^

                This argument is an integer of type:

                    U8

                But `y` needs the 1st argument to be:

                    a

                Tip: The type annotation uses the type variable `a` to say that this
                definition can produce any type of value. But in the body I see that
                it will only produce a `U8` value of a single specific type. Maybe
                change the type annotation to be more specific? Maybe change the code
                to be more general?
                "#
            ),
        )
    }

    #[test]
    fn non_exhaustive_with_guard() {
        report_problem_as(
            indoc!(
                r#"
                x : [A]
                when x is
                    A if True -> ""
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────── /code/proj/Main.roc ─

                This `when` does not cover all the possibilities:

                2│>  when x is
                3│>      A if True -> ""

                Other possibilities include:

                    A    (note the lack of an if clause)

                I would have to crash if I saw one of those! Add branches for them!
                "#
            ),
        )
    }

    #[test]
    fn invalid_record_extension_type() {
        report_problem_as(
            indoc!(
                r#"
                f : { x : Num.Nat }[]
                f
                "#
            ),
            indoc!(
                r#"
                ── INVALID_EXTENSION_TYPE ──────────────────────────────── /code/proj/Main.roc ─

                This record extension type is invalid:

                1│  f : { x : Num.Nat }[]
                                       ^^

                Note: A record extension variable can only contain a type variable or
                another record.
                "#
            ),
        )
    }

    #[test]
    fn invalid_tag_extension_type() {
        report_problem_as(
            indoc!(
                r#"
                f : [A]Str
                f
                "#
            ),
            indoc!(
                r#"
                ── INVALID_EXTENSION_TYPE ──────────────────────────────── /code/proj/Main.roc ─

                This tag union extension type is invalid:

                1│  f : [A]Str
                           ^^^

                Note: A tag union extension variable can only contain a type variable
                or another tag union.
                "#
            ),
        )
    }

    #[test]
    fn unknown_type() {
        report_problem_as(
            indoc!(
                r#"
                Type : [Constructor UnknownType]

                insertHelper : UnknownType, Type -> Type
                insertHelper = \h, m ->
                    when m is
                        Constructor _ -> Constructor h

                insertHelper
                "#
            ),
            indoc!(
                r#"
                ── UNRECOGNIZED NAME ───────────────────────────────────── /code/proj/Main.roc ─

                Nothing is named `UnknownType` in this scope.

                1│  Type : [Constructor UnknownType]
                                        ^^^^^^^^^^^

                Did you mean one of these?

                    Type
                    True
                    Box
                    Ok

                ── UNRECOGNIZED NAME ───────────────────────────────────── /code/proj/Main.roc ─

                Nothing is named `UnknownType` in this scope.

                3│  insertHelper : UnknownType, Type -> Type
                                   ^^^^^^^^^^^

                Did you mean one of these?

                    Type
                    True
                    insertHelper
                    Box
                "#
            ),
        )
    }

    #[test]
    fn ability_first_demand_not_indented_enough() {
        report_problem_as(
            indoc!(
                r#"
                Eq has
                eq : a, a -> U64 | a has Eq

                1
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED ABILITY ──────────────────────────────────── /code/proj/Main.roc ─

                I was partway through parsing an ability definition, but I got stuck
                here:

                1│  Eq has
                2│  eq : a, a -> U64 | a has Eq
                    ^

                I suspect this line is not indented enough (by 1 spaces)
                "#
            ),
        )
    }

    test_report!(
        ability_demands_not_indented_with_first,
        indoc!(
            r#"
            Eq has
                eq : a, a -> U64 | a has Eq
                    neq : a, a -> U64 | a has Eq

            1
            "#
        ),
        indoc!(
            r#"
            ── UNFINISHED ABILITY ─── tmp/ability_demands_not_indented_with_first/Test.roc ─

            I was partway through parsing an ability definition, but I got stuck
            here:

            5│          eq : a, a -> U64 | a has Eq
            6│              neq : a, a -> U64 | a has Eq
                            ^

            I suspect this line is indented too much (by 4 spaces)"#
        )
    );

    test_report!(
        ability_demand_value_has_args,
        indoc!(
            r#"
                Eq has
                    eq b c : a, a -> U64 | a has Eq

                1
                "#
        ),
        indoc!(
            r#"
                ── UNFINISHED ABILITY ───────────── tmp/ability_demand_value_has_args/Test.roc ─

                I was partway through parsing an ability definition, but I got stuck
                here:

                5│          eq b c : a, a -> U64 | a has Eq
                               ^

                I was expecting to see a : annotating the signature of this value
                next."#
        )
    );

    #[test]
    fn ability_non_signature_expression() {
        report_problem_as(
            indoc!(
                r#"
                Eq has
                    123

                1
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED ABILITY ──────────────────────────────────── /code/proj/Main.roc ─

                I was partway through parsing an ability definition, but I got stuck
                here:

                1│  Eq has
                2│      123
                        ^

                I was expecting to see a value signature next.
                "#
            ),
        )
    }

    #[test]
    fn wildcard_in_alias() {
        report_problem_as(
            indoc!(
                r#"
                I : Num.Int *
                a : I
                a
                "#
            ),
            indoc!(
                r#"
                ── UNBOUND TYPE VARIABLE ───────────────────────────────── /code/proj/Main.roc ─

                The definition of `I` has an unbound type variable:

                1│  I : Num.Int *
                                ^

                Tip: Type variables must be bound before the `:`. Perhaps you intended
                to add a type parameter to this type?
                "#
            ),
        )
    }

    #[test]
    fn wildcard_in_opaque() {
        report_problem_as(
            indoc!(
                r#"
                I := Num.Int *
                a : I
                a
                "#
            ),
            indoc!(
                r#"
                ── UNBOUND TYPE VARIABLE ───────────────────────────────── /code/proj/Main.roc ─

                The definition of `I` has an unbound type variable:

                1│  I := Num.Int *
                                 ^

                Tip: Type variables must be bound before the `:=`. Perhaps you intended
                to add a type parameter to this type?
                "#
            ),
        )
    }

    #[test]
    fn multiple_wildcards_in_alias() {
        report_problem_as(
            indoc!(
                r#"
                I : [A (Num.Int *), B (Num.Int *)]
                a : I
                a
                "#
            ),
            indoc!(
                r#"
                ── UNBOUND TYPE VARIABLE ───────────────────────────────── /code/proj/Main.roc ─

                The definition of `I` has 2 unbound type variables.

                Here is one occurrence:

                1│  I : [A (Num.Int *), B (Num.Int *)]
                                    ^

                Tip: Type variables must be bound before the `:`. Perhaps you intended
                to add a type parameter to this type?
                "#
            ),
        )
    }

    #[test]
    fn inference_var_in_alias() {
        report_problem_as(
            indoc!(
                r#"
                I : Num.Int _
                a : I
                a
                "#
            ),
            indoc!(
                r#"
                ── UNBOUND TYPE VARIABLE ───────────────────────────────── /code/proj/Main.roc ─

                The definition of `I` has an unbound type variable:

                1│  I : Num.Int _
                                ^

                Tip: Type variables must be bound before the `:`. Perhaps you intended
                to add a type parameter to this type?
                "#
            ),
        )
    }

    #[test]
    fn unbound_var_in_alias() {
        report_problem_as(
            indoc!(
                r#"
                I : Num.Int a
                a : I
                a
                "#
            ),
            indoc!(
                r#"
                ── UNBOUND TYPE VARIABLE ───────────────────────────────── /code/proj/Main.roc ─

                The definition of `I` has an unbound type variable:

                1│  I : Num.Int a
                                ^

                Tip: Type variables must be bound before the `:`. Perhaps you intended
                to add a type parameter to this type?
                "#
            ),
        )
    }

    test_report!(
        ability_bad_type_parameter,
        indoc!(
            r#"
            app "test" provides [] to "./platform"

            Hash a b c has
              hash : a -> U64 | a has Hash
            "#
        ),
        indoc!(
            r#"
            ── ABILITY HAS TYPE VARIABLES ──────────────────────────── /code/proj/Main.roc ─

            The definition of the `Hash` ability includes type variables:

            3│  Hash a b c has
                     ^^^^^

            Abilities cannot depend on type variables, but their member values
            can!

            ── UNUSED DEFINITION ───────────────────────────────────── /code/proj/Main.roc ─

            `Hash` is not used anywhere in your code.

            3│  Hash a b c has
                ^^^^

            If you didn't intend on using `Hash` then remove it so future readers of
            your code don't wonder why it is there.
            "#
        )
    );

    test_report!(
        alias_in_has_clause,
        indoc!(
            r#"
            app "test" provides [hash] to "./platform"

            Hash has hash : a, b -> Num.U64 | a has Hash, b has Bool.Bool
            "#
        ),
        indoc!(
            r#"
            ── HAS CLAUSE IS NOT AN ABILITY ────────────────────────── /code/proj/Main.roc ─

            The type referenced in this "has" clause is not an ability:

            3│  Hash has hash : a, b -> Num.U64 | a has Hash, b has Bool.Bool
                                                                    ^^^^^^^^^
            "#
        )
    );

    test_report!(
        shadowed_type_variable_in_has_clause,
        indoc!(
            r#"
            app "test" provides [ab1] to "./platform"

            Ab1 has ab1 : a -> {} | a has Ab1, a has Ab1
            "#
        ),
        indoc!(
            r#"
            ── DUPLICATE NAME ──────────────────────────────────────── /code/proj/Main.roc ─

            The `a` name is first defined here:

            3│  Ab1 has ab1 : a -> {} | a has Ab1, a has Ab1
                                        ^^^^^^^^^

            But then it's defined a second time here:

            3│  Ab1 has ab1 : a -> {} | a has Ab1, a has Ab1
                                                   ^^^^^^^^^

            Since these variables have the same name, it's easy to use the wrong
            one on accident. Give one of them a new name.
            "#
        )
    );

    test_report!(
        ability_shadows_ability,
        indoc!(
            r#"
            app "test" provides [ab] to "./platform"

            Ability has ab : a -> U64 | a has Ability

            Ability has ab1 : a -> U64 | a has Ability
            "#
        ),
        indoc!(
            r#"
            ── DUPLICATE NAME ──────────────────────────────────────── /code/proj/Main.roc ─

            The `Ability` name is first defined here:

            3│  Ability has ab : a -> U64 | a has Ability
                ^^^^^^^

            But then it's defined a second time here:

            5│  Ability has ab1 : a -> U64 | a has Ability
                ^^^^^^^

            Since these abilities have the same name, it's easy to use the wrong
            one on accident. Give one of them a new name.
            "#
        )
    );

    test_report!(
        ability_member_does_not_bind_ability,
        indoc!(
            r#"
            app "test" provides [] to "./platform"

            Ability has ab : {} -> {}
            "#
        ),
        indoc!(
            r#"
            ── ABILITY MEMBER MISSING HAS CLAUSE ───────────────────── /code/proj/Main.roc ─

            The definition of the ability member `ab` does not include a `has` clause
            binding a type variable to the ability `Ability`:

            3│  Ability has ab : {} -> {}
                            ^^

            Ability members must include a `has` clause binding a type variable to
            an ability, like

                a has Ability

            Otherwise, the function does not need to be part of the ability!

            ── UNUSED DEFINITION ───────────────────────────────────── /code/proj/Main.roc ─

            `Ability` is not used anywhere in your code.

            3│  Ability has ab : {} -> {}
                ^^^^^^^

            If you didn't intend on using `Ability` then remove it so future readers
            of your code don't wonder why it is there.
            "#
        )
    );

    test_report!(
        ability_member_binds_parent_twice,
        indoc!(
            r#"
            app "test" provides [] to "./platform"

            Eq has eq : a, b -> Bool.Bool | a has Eq, b has Eq
            "#
        ),
        indoc!(
            r#"
            ── ABILITY MEMBER BINDS MULTIPLE VARIABLES ─────────────── /code/proj/Main.roc ─

            The definition of the ability member `eq` includes multiple variables
            bound to the `Eq`` ability:`

            3│  Eq has eq : a, b -> Bool.Bool | a has Eq, b has Eq
                                                ^^^^^^^^^^^^^^^^^^

            Ability members can only bind one type variable to their parent
            ability. Otherwise, I wouldn't know what type implements an ability by
            looking at specializations!

            Hint: Did you mean to only bind `a` to `Eq`?
            "#
        )
    );

    test_report!(
        has_clause_not_on_toplevel,
        indoc!(
            r#"
            app "test" provides [f] to "./platform"

            Hash has hash : (a | a has Hash) -> Num.U64

            f : a -> Num.U64 | a has Hash
            "#
        ),
        indoc!(
            r#"
            ── ILLEGAL HAS CLAUSE ──────────────────────────────────── /code/proj/Main.roc ─

            A `has` clause is not allowed here:

            3│  Hash has hash : (a | a has Hash) -> Num.U64
                                     ^^^^^^^^^^

            `has` clauses can only be specified on the top-level type annotations.

            ── ABILITY MEMBER MISSING HAS CLAUSE ───────────────────── /code/proj/Main.roc ─

            The definition of the ability member `hash` does not include a `has`
            clause binding a type variable to the ability `Hash`:

            3│  Hash has hash : (a | a has Hash) -> Num.U64
                         ^^^^

            Ability members must include a `has` clause binding a type variable to
            an ability, like

                a has Hash

            Otherwise, the function does not need to be part of the ability!
            "#
        )
    );

    test_report!(
        ability_specialization_with_non_implementing_type,
        indoc!(
            r#"
            app "test" provides [hash] to "./platform"

            Hash has hash : a -> Num.U64 | a has Hash

            hash = \{} -> 0u64
            "#
        ),
        indoc!(
            r#"
            ── ILLEGAL SPECIALIZATION ──────────────────────────────── /code/proj/Main.roc ─

            This specialization of `hash` is for a non-opaque type:

            5│  hash = \{} -> 0u64
                ^^^^

            It is specialized for

                {}a

            but structural types can never specialize abilities!

            Note: `hash` is a member of `#UserApp.Hash`
            "#
        )
    );

    test_report!(
        ability_specialization_does_not_match_type,
        indoc!(
            r#"
            app "test" provides [hash] to "./platform"

            Hash has hash : a -> U64 | a has Hash

            Id := U32

            hash = \@Id n -> n
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            Something is off with this specialization of `hash`:

            7│  hash = \@Id n -> n
                ^^^^

            This value is a declared specialization of type:

                Id -> U32

            But the type annotation on `hash` says it must match:

                Id -> U64
            "#
        )
    );

    test_report!(
        ability_specialization_is_incomplete,
        indoc!(
            r#"
            app "test" provides [eq, le] to "./platform"

            Eq has
                eq : a, a -> Bool | a has Eq
                le : a, a -> Bool | a has Eq

            Id := U64

            eq = \@Id m, @Id n -> m == n
            "#
        ),
        indoc!(
            r#"
            ── INCOMPLETE ABILITY IMPLEMENTATION ───────────────────── /code/proj/Main.roc ─

            The type `Id` does not fully implement the ability `Eq`. The following
            specializations are missing:

            A specialization for `le`, which is defined here:

            5│      le : a, a -> Bool | a has Eq
                    ^^
            "#
        )
    );

    test_report!(
        ability_specialization_overly_generalized,
        indoc!(
            r#"
            app "test" provides [hash] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            hash = \_ -> 0u64
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            This specialization of `hash` is overly general:

            6│  hash = \_ -> 0u64
                ^^^^

            This value is a declared specialization of type:

                a -> U64

            But the type annotation on `hash` says it must match:

                a -> U64 | a has Hash

            Note: The specialized type is too general, and does not provide a
            concrete type where a type variable is bound to an ability.

            Specializations can only be made for concrete types. If you have a
            generic implementation for this value, perhaps you don't need an
            ability?
            "#
        )
    );

    test_report!(
        ability_specialization_conflicting_specialization_types,
        indoc!(
            r#"
            app "test" provides [eq] to "./platform"

            Eq has
                eq : a, a -> Bool | a has Eq

            You := {}
            AndI := {}

            eq = \@You {}, @AndI {} -> False
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            Something is off with this specialization of `eq`:

            9│  eq = \@You {}, @AndI {} -> False
                ^^

            This value is a declared specialization of type:

                You, AndI -> [False, True]

            But the type annotation on `eq` says it must match:

                You, You -> Bool

            Tip: Type comparisons between an opaque type are only ever equal if
            both types are the same opaque type. Did you mean to create an opaque
            type by wrapping it? If I have an opaque type Age := U32 I can create
            an instance of this opaque type by doing @Age 23.
            "#
        )
    );

    test_report!(
        ability_specialization_checked_against_annotation,
        indoc!(
            r#"
            app "test" provides [hash] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            Id := U64

            hash : Id -> U32
            hash = \@Id n -> n
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            Something is off with the body of the `hash` definition:

            8│  hash : Id -> U32
            9│  hash = \@Id n -> n
                                 ^

            This `n` value is a:

                U64

            But the type annotation on `hash` says it should be:

                U32

            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            Something is off with this specialization of `hash`:

            9│  hash = \@Id n -> n
                       ^^^^^^^^^^^

            This value is a declared specialization of type:

                Id -> U32

            But the type annotation on `hash` says it must match:

                Id -> U64
            "#
        )
    );

    test_report!(
        ability_specialization_called_with_non_specializing,
        indoc!(
            r#"
            app "test" provides [noGoodVeryBadTerrible] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            Id := U64

            hash = \@Id n -> n

            User := {}

            noGoodVeryBadTerrible =
                {
                    nope: hash (@User {}),
                    notYet: hash (A 1),
                }
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            This expression has a type that does not implement the abilities it's expected to:

            15│          notYet: hash (A 1),
                                       ^^^

            Roc can't generate an implementation of the `#UserApp.Hash` ability for

                [A (Num a)]b

            Only builtin abilities can have generated implementations!

            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            This expression has a type that does not implement the abilities it's expected to:

            14│          nope: hash (@User {}),
                                     ^^^^^^^^

            The type `User` does not fully implement the ability `Hash`. The following
            specializations are missing:

            A specialization for `hash`, which is defined here:

            4│      hash : a -> U64 | a has Hash
                    ^^^^
            "#
        )
    );

    test_report!(
        ability_not_on_toplevel,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                Hash has
                    hash : a -> U64 | a has Hash

                123
            "#
        ),
        indoc!(
            r#"
            ── ABILITY NOT ON TOP-LEVEL ────────────────────────────── /code/proj/Main.roc ─

            This ability definition is not on the top-level of a module:

            4│>      Hash has
            5│>          hash : a -> U64 | a has Hash

            Abilities can only be defined on the top-level of a Roc module.
            "#
        )
    );

    test_report!(
        expression_generalization_to_ability_is_an_error,
        indoc!(
            r#"
            app "test" provides [hash, hashable] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            Id := U64
            hash = \@Id n -> n

            hashable : a | a has Hash
            hashable = @Id 15
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            Something is off with the body of the `hashable` definition:

             9│  hashable : a | a has Hash
            10│  hashable = @Id 15
                            ^^^^^^

            This Id opaque wrapping has the type:

                Id

            But the type annotation on `hashable` says it should be:

                a | a has Hash

            Tip: The type annotation uses the type variable `a` to say that this
            definition can produce any value implementing the `Hash` ability. But in
            the body I see that it will only produce a `Id` value of a single
            specific type. Maybe change the type annotation to be more specific?
            Maybe change the code to be more general?
            "#
        )
    );

    test_report!(
        ability_value_annotations_are_an_error,
        indoc!(
            r#"
            app "test" provides [result] to "./platform"

            Hash has
                hash : a -> U64 | a has Hash

            mulHashes : Hash, Hash -> U64
            mulHashes = \x, y -> hash x * hash y

            Id := U64
            hash = \@Id n -> n

            Three := {}
            hash = \@Three _ -> 3

            result = mulHashes (@Id 100) (@Three {})
            "#
        ),
        indoc!(
            r#"
            ── ABILITY USED AS TYPE ────────────────────────────────── /code/proj/Main.roc ─

            You are attempting to use the ability `Hash` as a type directly:

            6│  mulHashes : Hash, Hash -> U64
                            ^^^^

            Abilities can only be used in type annotations to constrain type
            variables.

            Hint: Perhaps you meant to include a `has` annotation, like

                a has Hash

            ── ABILITY USED AS TYPE ────────────────────────────────── /code/proj/Main.roc ─

            You are attempting to use the ability `Hash` as a type directly:

            6│  mulHashes : Hash, Hash -> U64
                                  ^^^^

            Abilities can only be used in type annotations to constrain type
            variables.

            Hint: Perhaps you meant to include a `has` annotation, like

                b has Hash
            "#
        )
    );

    test_report!(
        branches_have_more_cases_than_condition,
        indoc!(
            r#"
            foo : Bool -> Str
            foo = \bool ->
                when bool is
                    True -> "true"
                    False -> "false"
                    Wat -> "surprise!"
            foo
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            The branches of this `when` expression don't match the condition:

            6│>          when bool is
            7│               True -> "true"
            8│               False -> "false"
            9│               Wat -> "surprise!"

            This `bool` value is a:

                Bool

            But the branch patterns have type:

                [False, True, Wat]

            The branches must be cases of the `when` condition's type!
            "#
        )
    );

    #[test]
    fn always_function() {
        // from https://github.com/rtfeldman/roc/commit/1372737f5e53ee5bb96d7e1b9593985e5537023a
        // There was a bug where this reported UnusedArgument("val")
        // since it was used only in the returned function only.
        //
        // we want this to not give any warnings/errors!
        report_problem_as(
            indoc!(
                r#"
                always = \val -> \_ -> val

                always
                "#
            ),
            "",
        )
    }

    test_report!(
        imports_missing_comma,
        indoc!(
            r#"
            app "test-missing-comma"
                packages { pf: "platform" }
                imports [pf.Task Base64]
                provides [main, @Foo] to pf
            "#
        ),
        indoc!(
            r#"
            ── WEIRD IMPORTS ────────────────────────── tmp/imports_missing_comma/Test.roc ─

            I am partway through parsing a imports list, but I got stuck here:

            2│      packages { pf: "platform" }
            3│      imports [pf.Task Base64]
                                     ^

            I am expecting a comma or end of list, like

                imports [Shape, Vector]"#
        )
    );

    test_report!(
        not_enough_cases_for_open_union,
        indoc!(
            r#"
            foo : [A, B]a -> Str
            foo = \it ->
                when it is
                    A -> ""
            foo
            "#
        ),
        indoc!(
            r#"
            ── UNSAFE PATTERN ──────────────────────────────────────── /code/proj/Main.roc ─

            This `when` does not cover all the possibilities:

            6│>          when it is
            7│>              A -> ""

            Other possibilities include:

                B
                _

            I would have to crash if I saw one of those! Add branches for them!
            "#
        )
    );

    test_report!(
        issue_2778_specialization_is_not_a_redundant_pattern,
        indoc!(
            r#"
            formatColor = \color ->
              when color is
                Red -> "red"
                Yellow -> "yellow"
                _ -> "unknown"

            Red |> formatColor |> Str.concat (formatColor Orange)
            "#
        ),
        "" // no problem
    );

    test_report!(
        nested_specialization,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Default has default : {} -> a | a has Default

            main =
                A := {}
                default = \{} -> @A {}
                default {}
            "#
        ),
        indoc!(
            r#"
            ── SPECIALIZATION NOT ON TOP-LEVEL ─────────────────────── /code/proj/Main.roc ─

            This specialization of the `default` ability member is in a nested
            scope:

            7│      default = \{} -> @A {}
                    ^^^^^^^

            Specializations can only be defined on the top-level of a module.
            "#
        )
    );

    test_report!(
        recursion_var_specialization_error,
        indoc!(
            r#"
            Job a : [Job (List (Job a))]

            job : Job Str

            when job is
                Job lst -> lst == ""
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            The 2nd argument to `isEq` is not what I expect:

            9│          Job lst -> lst == ""
                                          ^^

            This argument is a string of type:

                Str

            But `isEq` needs the 2nd argument to be:

                List [Job ∞] as ∞
            "#
        )
    );

    test_report!(
        type_error_in_apply_is_circular,
        indoc!(
            r#"
            app "test" provides [go] to "./platform"

            S a : { set : Set a }

            go : a, S a -> Result (List a) *
            go = \goal, model ->
                    if goal == goal
                    then Ok []
                    else
                        new = { model & set : Set.remove goal model.set }
                        go goal new
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            The 1st argument to `remove` is not what I expect:

            10│              new = { model & set : Set.remove goal model.set }
                                                              ^^^^

            This `goal` value is a:

                a

            But `remove` needs the 1st argument to be:

                Set a

            Tip: The type annotation uses the type variable `a` to say that this
            definition can produce any type of value. But in the body I see that
            it will only produce a `Set` value of a single specific type. Maybe
            change the type annotation to be more specific? Maybe change the code
            to be more general?

            ── CIRCULAR TYPE ───────────────────────────────────────── /code/proj/Main.roc ─

            I'm inferring a weird self-referential type for `new`:

            10│              new = { model & set : Set.remove goal model.set }
                             ^^^

            Here is my best effort at writing down the type. You will see ∞ for
            parts of the type that repeat something already printed out
            infinitely.

                { set : Set ∞ }

            ── CIRCULAR TYPE ───────────────────────────────────────── /code/proj/Main.roc ─

            I'm inferring a weird self-referential type for `goal`:

            6│  go = \goal, model ->
                      ^^^^

            Here is my best effort at writing down the type. You will see ∞ for
            parts of the type that repeat something already printed out
            infinitely.

                Set ∞
            "#
        )
    );

    test_report!(
        cycle_through_non_function,
        indoc!(
            r#"
            force : ({} -> I64) -> I64
            force = \eval -> eval {}

            t1 = \_ -> force (\_ -> t2)

            t2 = t1 {}

            t2
            "#
        ),
        indoc!(
            r#"
            ── CIRCULAR DEFINITION ─────────────────────────────────── /code/proj/Main.roc ─

            The `t1` definition is causing a very tricky infinite loop:

            7│      t1 = \_ -> force (\_ -> t2)
                    ^^

            The `t1` value depends on itself through the following chain of
            definitions:

                ┌─────┐
                │     t1
                │     ↓
                │     t2
                └─────┘
            "#
        )
    );

    test_report!(
        function_does_not_implement_encoding,
        indoc!(
            r#"
            app "test" imports [Encode] provides [main] to "./platform"

            main = Encode.toEncoder \x -> x
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            This expression has a type that does not implement the abilities it's expected to:

            3│  main = Encode.toEncoder \x -> x
                                        ^^^^^^^

            Roc can't generate an implementation of the `Encode.Encoding` ability
            for

                a -> a

            Note: `Encoding` cannot be generated for functions.
            "#
        )
    );

    test_report!(
        unbound_type_in_record_does_not_implement_encoding,
        indoc!(
            r#"
            app "test" imports [Encode] provides [main] to "./platform"

            main = \x -> Encode.toEncoder { x: x }
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            This expression has a type that does not implement the abilities it's expected to:

            3│  main = \x -> Encode.toEncoder { x: x }
                                              ^^^^^^^^

            Roc can't generate an implementation of the `Encode.Encoding` ability
            for

                { x : a }

            In particular, an implementation for

                a

            cannot be generated.

            Tip: This type variable is not bound to `Encoding`. Consider adding a
            `has` clause to bind the type variable, like `| a has Encode.Encoding`
            "#
        )
    );

    test_report!(
        nested_opaque_does_not_implement_encoding,
        indoc!(
            r#"
            app "test" imports [Encode] provides [main] to "./platform"

            A := {}
            main = Encode.toEncoder { x: @A {} }
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            This expression has a type that does not implement the abilities it's expected to:

            4│  main = Encode.toEncoder { x: @A {} }
                                        ^^^^^^^^^^^^

            Roc can't generate an implementation of the `Encode.Encoding` ability
            for

                { x : A }

            In particular, an implementation for

                A

            cannot be generated.

            Tip: `A` does not implement `Encoding`. Consider adding a custom
            implementation or `has Encode.Encoding` to the definition of `A`.
            "#
        )
    );

    test_report!(
        cycle_through_non_function_top_level,
        indoc!(
            r#"
                app "test" provides [t2] to "./platform"

                force : ({} -> I64) -> I64
                force = \eval -> eval {}

                t1 = \_ -> force (\_ -> t2)

                t2 = t1 {}
                "#
        ),
        indoc!(
            r#"
                ── CIRCULAR DEFINITION ─────────────────────────────────── /code/proj/Main.roc ─

                The `t1` definition is causing a very tricky infinite loop:

                6│  t1 = \_ -> force (\_ -> t2)
                    ^^

                The `t1` value depends on itself through the following chain of
                definitions:

                    ┌─────┐
                    │     t1
                    │     ↓
                    │     t2
                    └─────┘
                "#
        )
    );

    test_report!(
        derive_non_builtin_ability,
        indoc!(
            r#"
            app "test" provides [A] to "./platform"

            Ab has ab : a -> a | a has Ab

            A := {} has [Ab]
            "#
        ),
        indoc!(
            r#"
            ── ILLEGAL DERIVE ──────────────────────────────────────── /code/proj/Main.roc ─

            This ability cannot be derived:

            5│  A := {} has [Ab]
                             ^^

            Only builtin abilities can be derived.

            Note: The builtin abilities are `Encode.Encoding`
            "#
        )
    );

    test_report!(
        has_encoding_for_function,
        indoc!(
            r#"
            app "test" imports [Encode] provides [A] to "./platform"

            A a := a -> a has [Encode.Encoding]
            "#
        ),
        indoc!(
            r#"
            ── INCOMPLETE ABILITY IMPLEMENTATION ───────────────────── /code/proj/Main.roc ─

            Roc can't derive an implementation of the `Encode.Encoding` for `A`:

            3│  A a := a -> a has [Encode.Encoding]
                                   ^^^^^^^^^^^^^^^

            Note: `Encoding` cannot be generated for functions.

            Tip: You can define a custom implementation of `Encode.Encoding` for `A`.
            "#
        )
    );

    test_report!(
        has_encoding_for_non_encoding_alias,
        indoc!(
            r#"
            app "test" imports [Encode] provides [A] to "./platform"

            A := B has [Encode.Encoding]

            B := {}
            "#
        ),
        indoc!(
            r#"
            ── INCOMPLETE ABILITY IMPLEMENTATION ───────────────────── /code/proj/Main.roc ─

            Roc can't derive an implementation of the `Encode.Encoding` for `A`:

            3│  A := B has [Encode.Encoding]
                            ^^^^^^^^^^^^^^^

            Tip: `B` does not implement `Encoding`. Consider adding a custom
            implementation or `has Encode.Encoding` to the definition of `B`.

            Tip: You can define a custom implementation of `Encode.Encoding` for `A`.
            "#
        )
    );

    test_report!(
        has_encoding_for_other_has_encoding,
        indoc!(
            r#"
            app "test" imports [Encode] provides [A] to "./platform"

            A := B has [Encode.Encoding]

            B := {} has [Encode.Encoding]
            "#
        ),
        indoc!("") // no error
    );

    test_report!(
        has_encoding_for_recursive_deriving,
        indoc!(
            r#"
            app "test" imports [Encode] provides [MyNat] to "./platform"

            MyNat := [S MyNat, Z] has [Encode.Encoding]
            "#
        ),
        indoc!("") // no error
    );

    test_report!(
        shadowing_top_level_scope,
        indoc!(
            r#"
            app "test" provides [ main ] to "./platform"

            main = 1

            main = \n -> n + 2
            "#
        ),
        indoc!(
            r#"
            ── DUPLICATE NAME ──────────────────────────────────────── /code/proj/Main.roc ─
            
            The `main` name is first defined here:
            
            3│  main = 1
                ^^^^
            
            But then it's defined a second time here:
            
            5│  main = \n -> n + 2
                ^^^^
            
            Since these variables have the same name, it's easy to use the wrong
            one on accident. Give one of them a new name.
            "#
        )
    );

    test_report!(
        has_encoding_dominated_by_custom,
        indoc!(
            r#"
            app "test" imports [Encode.{ Encoding, toEncoder, custom }] provides [A] to "./platform"

            A := {} has [Encode.Encoding]

            toEncoder = \@A {} -> custom \l, _ -> l
            "#
        ),
        indoc!(
            r#"
            ── CONFLICTING DERIVE AND IMPLEMENTATION ───────────────── /code/proj/Main.roc ─

            `A` both derives and custom-implements `Encode.Encoding`. We found the
            derive here:

            3│  A := {} has [Encode.Encoding]
                             ^^^^^^^^^^^^^^^

            and one custom implementation of `Encode.Encoding` here:

            5│  toEncoder = \@A {} -> custom \l, _ -> l
                ^^^^^^^^^

            Derived and custom implementations can conflict, so one of them needs
            to be removed!

            Note: We'll try to compile your program using the custom
            implementation first, and fall-back on the derived implementation if
            needed. Make sure to disambiguate which one you want!
            "#
        )
    );

    test_report!(
        issue_1755,
        indoc!(
            r#"
            Handle := {}

            await : Result a err, (a -> Result b err) -> Result b err
            open : {} -> Result Handle *
            close : Handle -> Result {} *

            withOpen : (Handle -> Result {} *) -> Result {} *
            withOpen = \callback ->
                handle <- await (open {})
                {} <- await (callback handle)
                close handle

            withOpen
            "#
        ),
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────── /code/proj/Main.roc ─

            Something is off with the body of the `withOpen` definition:

            10│       withOpen : (Handle -> Result {} *) -> Result {} *
            11│       withOpen = \callback ->
            12│>          handle <- await (open {})
            13│>          {} <- await (callback handle)
            14│>          close handle

            The type annotation on `withOpen` says this `await` call should have the
            type:

                Result {} *

            However, the type of this `await` call is connected to another type in a
            way that isn't reflected in this annotation.

            Tip: Any connection between types must use a named type variable, not
            a `*`! Maybe the annotation  on `withOpen` should have a named type
            variable in place of the `*`?
            "#
        )
    );
}
