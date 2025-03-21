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
    use roc_load::{self, ExecutionMode, LoadConfig, LoadedModule, LoadingProblem, Threading};
    use roc_module::symbol::{Interns, ModuleId};
    use roc_packaging::cache::RocCacheDir;
    use roc_parse::header::parse_header;
    use roc_parse::state::State;
    use roc_parse::test_helpers::parse_expr_with;
    use roc_problem::Severity;
    use roc_region::all::LineInfo;
    use roc_reporting::report::{
        can_problem, parse_problem, type_problem, RenderTarget, Report, ANSI_STYLE_CODES,
        DEFAULT_PALETTE,
    };
    use roc_reporting::report::{RocDocAllocator, RocDocBuilder};
    use roc_solve::FunctionKind;
    use roc_solve_problem::TypeError;
    use roc_test_utils_dir::TmpDir;
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

    fn maybe_save_parse_test_case(test_name: &str, src: &str, is_expr: bool) {
        // First check if the env var indicates we should migrate tests
        if std::env::var("ROC_MIGRATE_REPORTING_TESTS").is_err() {
            return;
        }

        // Check if we have parse errors
        let arena = Bump::new();
        let has_error = if is_expr {
            parse_expr_with(&arena, src).is_err()
        } else {
            parse_header(&arena, State::new(src.trim().as_bytes())).is_err()
            // TODO: also parse the module defs
        };

        if !has_error {
            return;
        }

        let mut path = PathBuf::from(std::env!("ROC_WORKSPACE_DIR"));
        path.push("crates");
        path.push("compiler");
        path.push("parse");
        path.push("tests");
        path.push("snapshots");
        path.push("fail");
        let kind = if is_expr { "expr" } else { "header" };
        path.push(format!("{test_name}.{kind}.roc"));

        std::fs::write(path, src).unwrap();
    }

    fn run_load_and_infer<'a>(
        subdir: &str,
        arena: &'a Bump,
        src: &'a str,
    ) -> (String, Result<LoadedModule, LoadingProblem<'a>>) {
        use std::fs::File;
        use std::io::Write;

        let module_src = if src.starts_with("app") || src.starts_with("module") {
            maybe_save_parse_test_case(subdir, src, false);
            // this is already a module
            src.to_string()
        } else {
            maybe_save_parse_test_case(subdir, src, true);
            // this is an expression, promote it to a module
            promote_expr_to_module(src)
        };

        let loaded = {
            // Use a deterministic temporary directory.
            // We can't have all tests use "tmp" because tests run in parallel,
            // so append the test name to the tmp path.
            let tmp = format!("tmp/{subdir}");
            let dir = TmpDir::new(&tmp);

            let filename = PathBuf::from("Test.roc");
            let file_path = dir.path().join(filename);
            let full_file_path = file_path.clone();
            let mut file = File::create(file_path).unwrap();
            writeln!(file, "{module_src}").unwrap();
            let load_config = LoadConfig {
                target: roc_target::Target::LinuxX64,
                render: RenderTarget::Generic,
                palette: DEFAULT_PALETTE,
                threading: Threading::Single,
                exec_mode: ExecutionMode::Check,
                function_kind: FunctionKind::LambdaSet,
            };
            let result = roc_load::load_and_typecheck(
                arena,
                full_file_path,
                None,
                RocCacheDir::Disallowed,
                load_config,
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
            Vec<TypeError>,
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
            Err(LoadingProblem::FormattedReport(fail, _)) => fail,
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
                panic!("failed to load: {other:?}");
            }
        }
    }

    fn infer_expr_help<'a>(
        arena: &'a Bump,
        expr_src: &'a str,
    ) -> Result<
        (
            Vec<TypeError>,
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
            mut types,
            ..
        } = can_expr(arena, expr_src)?;
        let mut subs = Subs::new_from_varstore(var_store);

        for named in output.introduced_variables.named {
            subs.rigid_var(named.variable, named.name);
        }

        for var in output.introduced_variables.wildcards {
            subs.rigid_var(var.value, "*".into());
        }

        let mut solve_aliases = roc_solve::Aliases::default();

        for (name, alias) in output.aliases {
            solve_aliases.insert(&mut types, name, alias);
        }

        let mut unify_problems = Vec::new();
        let mut abilities_store = AbilitiesStore::default();
        let (_content, _subs) = infer_expr(
            subs,
            &mut unify_problems,
            types,
            &constraints,
            constraint,
            // Use `new_report_problem_as` in order to get proper derives.
            // TODO: remove the non-new reporting test infra.
            PendingDerives::default(),
            &mut solve_aliases,
            &mut abilities_store,
            Default::default(),
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

        let state = State::new(src.as_bytes());

        let filename = filename_from_string(r"/code/proj/Main.roc");
        let src_lines: Vec<&str> = src.split('\n').collect();
        let lines = LineInfo::new(src);

        match roc_parse::header::parse_header(arena, state) {
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
                println!("                {line}");
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
    fn __new_report_problem_as(test_name: &str, src: &str, check_render: impl FnOnce(&str)) {
        let arena = Bump::new();

        let finalize_render = |doc: RocDocBuilder<'_>, buf: &mut String| {
            doc.1
                .render_raw(70, &mut roc_reporting::report::CiWrite::new(buf))
                .expect("list_reports")
        };

        let buf = list_reports_new(test_name, &arena, src, finalize_render);

        check_render(buf.as_str());
    }

    macro_rules! test_report {
        ($(#[$meta:meta])* $test_name:ident, $program:expr, @$output:literal) => {
            test_report!($(#[$meta])* $test_name, $program, |golden| insta::assert_snapshot!(golden, @$output) );
        };
        ($(#[$meta:meta])* $test_name: ident, $program:expr, $expecting:expr) => {
            #[test]
            $(#[$meta])*
            fn $test_name() {
                __new_report_problem_as(std::stringify!($test_name), $program, $expecting)
            }
        }
    }

    macro_rules! test_no_problem {
        ($(#[$meta:meta])* $test_name: ident, $program:expr) => {
            #[test]
            $(#[$meta])*
            fn $test_name() {
                __new_report_problem_as(std::stringify!($test_name), $program, |golden| pretty_assertions::assert_eq!(golden, ""))
            }
        }
    }

    fn human_readable(str: &str) -> String {
        str.replace(ANSI_STYLE_CODES.red, "<red>")
            .replace(ANSI_STYLE_CODES.white, "<white>")
            .replace(ANSI_STYLE_CODES.yellow, "<yellow>")
            .replace(ANSI_STYLE_CODES.green, "<green>")
            .replace(ANSI_STYLE_CODES.cyan, "<cyan>")
            .replace(ANSI_STYLE_CODES.reset, "<reset>")
            .replace(ANSI_STYLE_CODES.bold, "<bold>")
            .replace(ANSI_STYLE_CODES.underline, "<underline>")
    }

    test_report!(
        value_not_exposed,
        indoc!(
            r"
            List.isempty 1 2
        "
        ),
        @r"
    ── NOT EXPOSED in /code/proj/Main.roc ──────────────────────────────────────────

    The List module does not expose `isempty`:

    4│      List.isempty 1 2
            ^^^^^^^^^^^^

    Did you mean one of these?

        List.is_empty
        List.set
        List.get
        List.sum
    "
    );

    test_report!(
        report_unused_def,
        indoc!(
            r"
            x = 1
            y = 2

            x
        "
        ),
        @r"
    ── UNUSED DEFINITION in /code/proj/Main.roc ────────────────────────────────────

    `y` is not used anywhere in your code.

    5│      y = 2
            ^

    If you didn't intend on using `y` then remove it so future readers of
    your code don't wonder why it is there.
    "
    );

    test_report!(
        report_shadowing,
        indoc!(
            r"
           i = 1

           s = \i ->
               i + 1

           s i
       "
        ),
        @r"
    ── DUPLICATE NAME in /code/proj/Main.roc ───────────────────────────────────────

    The `i` name is first defined here:

    4│      i = 1
            ^

    But then it's defined a second time here:

    6│      s = \i ->
                 ^

    Since these variables have the same name, it's easy to use the wrong
    one by accident. Give one of them a new name.
    "
    );

    test_report!(
        report_shadowing_in_annotation,
        indoc!(
            r"
            Booly : [Yes, No]

            Booly : [Yes, No, Maybe]

            x : List Booly
            x = []

            x
       "
        ),
        @r"
    ── DUPLICATE NAME in /code/proj/Main.roc ───────────────────────────────────────

    The `Booly` name is first defined here:

    4│      Booly : [Yes, No]
            ^^^^^^^^^^^^^^^^^

    But then it's defined a second time here:

    6│      Booly : [Yes, No, Maybe]
            ^^^^^^^^^^^^^^^^^^^^^^^^

    Since these aliases have the same name, it's easy to use the wrong one
    by accident. Give one of them a new name.
    "
    );

    test_report!(
        report_precedence_problem_single_line,
        indoc!(
            r"x = 1
            y =
                if selected_id != this_id == admins_id then
                    4

                else
                    5

            { x, y }
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    Using != and == together requires parentheses, to clarify how they
    should be grouped.

    6│          if selected_id != this_id == admins_id then
                   ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    "
    );

    test_report!(
        #[ignore = "Blocked on https://github.com/roc-lang/roc/issues/3385"]
        unrecognized_name,
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
        @r#"
        ── UNRECOGNIZED NAME in /code/proj/Main.roc ────────────────────────────────────

        Nothing is named `bar` in this scope.

        8│          4 -> bar baz "yay"
                         ^^^

        Did you mean one of these?

            baz
            Str
            Err
            main
        "#
    );

    test_report!(
        lowercase_primitive_tag_bool,
        indoc!(
            r"
            if true then 1 else 2
            "
        ),
        @r###"
    ── UNRECOGNIZED NAME in /code/proj/Main.roc ────────────────────────────────────

    Nothing is named `true` in this scope.

    4│      if true then 1 else 2
               ^^^^

    Did you mean one of these?

        Str
        Frac
        Num
        U8
    "###
    );

    test_report!(
        report_precedence_problem_multiline,
        indoc!(
            r"
            if
                1
                    == 2
                    == 3
            then
                2

            else
                3
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    Using more than one == like this requires parentheses, to clarify how
    things should be grouped.

    5│>          1
    6│>              == 2
    7│>              == 3
    "
    );

    test_report!(
        unused_arg_and_unused_def,
        indoc!(
            r#"
             y = 9

             box = \class, html_children ->
                 div [class] []

             div = \_, _ -> 4

             box "wizard" []
         "#
        ),
        @r#"
    ── UNUSED ARGUMENT in /code/proj/Main.roc ──────────────────────────────────────

    `box` doesn't use `html_children`.

    6│      box = \class, html_children ->
                          ^^^^^^^^^^^^^

    If you don't need `html_children`, then you can just remove it. However,
    if you really do need `html_children` as an argument of `box`, prefix it
    with an underscore, like this: "_`html_children`". Adding an underscore
    at the start of a variable name is a way of saying that the variable
    is not used.

    ── UNUSED DEFINITION in /code/proj/Main.roc ────────────────────────────────────

    `y` is not used anywhere in your code.

    4│      y = 9
            ^

    If you didn't intend on using `y` then remove it so future readers of
    your code don't wonder why it is there.
    "#
    );

    #[test]
    fn report_value_color() {
        let src: &str = indoc!(
            r"
                activity_indicator_large = div

                view activity_indicator_large
            "
        );

        let arena = Bump::new();
        let (_type_problems, _can_problems, home, interns) =
            infer_expr_help(&arena, src).expect("parse error");

        let mut buf = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();

        let alloc = RocDocAllocator::new(&src_lines, home, &interns);

        let symbol = interns.symbol(test_home(), "activity_indicator_large".into());

        to_simple_report(alloc.symbol_unqualified(symbol)).render_color_terminal(
            &mut buf,
            &alloc,
            &DEFAULT_PALETTE,
        );

        assert_eq!(
            human_readable(&buf),
            "<cyan>activity_indicator_large<reset>"
        );
    }

    #[test]
    fn report_module_color() {
        let src: &str = indoc!(
            r"
                x = 1
                y = 2

                x
            "
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
                r"
                    is_disabled = \user -> user.is_admin

                    the_admin
                        |> is_disabled
                "
            ),
            indoc!(
                r"
                <cyan>── UNRECOGNIZED NAME in /code/proj/Main.roc ────────────────────────────────────<reset>

                Nothing is named `the_admin` in this scope.

                <cyan>3<reset><cyan>│<reset>  <white>the_admin<reset>
                    <red>^^^^^^^^^<reset>

                Did you mean one of these?

                    List
                    Box
                    Str
                    is_disabled
                "
            ),
        );
    }

    test_report!(
        if_condition_not_bool,
        indoc!(
            r#"
            if "foo" then 2 else 3
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This `if` condition needs to be a Bool:

    4│      if "foo" then 2 else 3
               ^^^^^

    Right now it’s a string of type:

        Str

    But I need every `if` condition to evaluate to a Bool—either `Bool.true`
    or `Bool.false`.
    "#
    );

    test_report!(
        when_if_guard,
        indoc!(
            r"
            when 1 is
                2 if 1 -> 0x0
                _ -> 0x1
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This `if` guard condition needs to be a Bool:

    4│       when 1 is
    5│>          2 if 1 -> 0x0
    6│           _ -> 0x1

    Right now it’s a number of type:

        Num *

    But I need every `if` guard condition to evaluate to a Bool—either
    `Bool.true` or `Bool.false`.
    "
    );

    test_report!(
        if_2_branch_mismatch,
        indoc!(
            r#"
            if Bool.true then 2 else "foo"
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This `if` has an `else` branch with a different type from its `then` branch:

    4│      if Bool.true then 2 else "foo"
                                     ^^^^^

    The `else` branch is a string of type:

        Str

    but the `then` branch has the type:

        Num *

    All branches in an `if` must have the same type!
    "#
    );

    test_report!(
        if_3_branch_mismatch,
        indoc!(
            r#"
             if Bool.true then 2 else if Bool.false then 2 else "foo"
             "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The 3rd branch of this `if` does not match all the previous branches:

    4│      if Bool.true then 2 else if Bool.false then 2 else "foo"
                                                               ^^^^^

    The 3rd branch is a string of type:

        Str

    But all the previous branches have type:

        Num *

    All branches in an `if` must have the same type!
    "#
    );

    test_report!(
        when_branch_mismatch,
        indoc!(
            r#"
            when 1 is
                2 -> "foo"
                3 -> {}
                _ -> ""
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The 2nd branch of this `when` does not match all the previous branches:

    4│       when 1 is
    5│           2 -> "foo"
    6│>          3 -> {}
    7│           _ -> ""

    The 2nd branch is a record of type:

        {}

    But all the previous branches have type:

        Str

    All branches of a `when` must have the same type!
    "#
    );

    test_report!(
        tuple_exhaustiveness_bad,
        indoc!(
            r#"
            Color : [Red, Blue]

            value : (Color, Color)
            value = (Red, Red)

            when value is
                (Blue, Blue) -> "foo"
                (Red, Blue) -> "foo"
                (Blue, Red) -> "foo"
                #(Red, Red) -> "foo"
            "#
        ),
        @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

     9│>      when value is
    10│>          (Blue, Blue) -> "foo"
    11│>          (Red, Blue) -> "foo"
    12│>          (Blue, Red) -> "foo"

    Other possibilities include:

        ( Red, Red )

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        tuple_exhaustiveness_good,
        indoc!(
            r#"
            Color : [Red, Blue]

            value : (Color, Color)
            value = (Red, Red)

            when value is
                (Blue, Blue) -> "foo"
                (Red, Blue) -> "foo"
                (Blue, Red) -> "foo"
                (Red, Red) -> "foo"
            "#
        ),
        @"" // No error
    );

    test_report!(
        elem_in_list,
        indoc!(
            r#"
            [1, 3, "foo"]
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This list contains elements with different types:

    4│      [1, 3, "foo"]
                   ^^^^^

    Its 3rd element is a string of type:

        Str

    However, the preceding elements in the list all have the type:

        Num *

    Every element in a list must have the same type!
    "#
    );

    test_report!(
        unwrap_num_elem_in_list,
        indoc!(
            r"
            [1, 2.2, 0x3]
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This list contains elements with different types:

    4│      [1, 2.2, 0x3]
                     ^^^

    Its 3rd element is an integer of type:

        Int *

    However, the preceding elements in the list all have the type:

        Frac *

    Every element in a list must have the same type!

    Tip: You can convert between integers and fractions using functions
    like `Num.to_frac` and `Num.round`.
    "
    );

    test_report!(
        record_update_value,
        indoc!(
            r#"
            x : { foo : {} }
            x = { foo: {} }

            { x & foo: "bar" }
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    I cannot update the `.foo` field like this:

    7│      { x & foo: "bar" }
                       ^^^^^

    You are trying to update `.foo` to be a string of type:

        Str

    But it should be:

        {}

    Record update syntax does not allow you to change the type of fields.
    You can achieve that with record literal syntax.
    "#
    );

    test_report!(
        circular_type,
        indoc!(
            r"
            f = \g -> g g

            f
            "
        ),
        @r"
    ── CIRCULAR TYPE in /code/proj/Main.roc ────────────────────────────────────────

    I'm inferring a weird self-referential type for `f`:

    4│      f = \g -> g g
            ^

    Here is my best effort at writing down the type. You will see ∞ for
    parts of the type that repeat something already printed out
    infinitely.

        (∞ -> a) -> a
    "
    );

    test_report!(
        polymorphic_recursion,
        indoc!(
            r"
            f = \x -> f [x]

            f
            "
        ),
        @r"
    ── CIRCULAR TYPE in /code/proj/Main.roc ────────────────────────────────────────

    I'm inferring a weird self-referential type for `f`:

    4│      f = \x -> f [x]
            ^

    Here is my best effort at writing down the type. You will see ∞ for
    parts of the type that repeat something already printed out
    infinitely.

        List ∞ -> *
    "
    );

    test_report!(
        polymorphic_mutual_recursion,
        indoc!(
            r"
            f = \x -> g x
            g = \x -> f [x]

            f
            "
        ),
        @r"
    ── CIRCULAR TYPE in /code/proj/Main.roc ────────────────────────────────────────

    I'm inferring a weird self-referential type for `f`:

    4│      f = \x -> g x
            ^

    Here is my best effort at writing down the type. You will see ∞ for
    parts of the type that repeat something already printed out
    infinitely.

        List ∞ -> *

    ── CIRCULAR TYPE in /code/proj/Main.roc ────────────────────────────────────────

    I'm inferring a weird self-referential type for `g`:

    5│      g = \x -> f [x]
            ^

    Here is my best effort at writing down the type. You will see ∞ for
    parts of the type that repeat something already printed out
    infinitely.

        List ∞ -> *
    "
    );

    test_report!(
        polymorphic_mutual_recursion_annotated,
        indoc!(
            r"
            f : a -> List a
            f = \x -> g x
            g = \x -> f [x]

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This returns something that's incompatible with the return type of the
    enclosing function:

    5│      f = \x -> g x
                      ^^^

    This `g` call produces:

        List List a

    But I expected the function to have return type:

        List a

    Tip: The type annotation uses the type variable `a` to say that this
    definition can produce any type of value. But in the body I see that
    it will only produce a `List` value of a single specific type. Maybe
    change the type annotation to be more specific? Maybe change the code
    to be more general?
    "
    );

    test_report!(
        polymorphic_mutual_recursion_dually_annotated_lie,
        indoc!(
            r"
            f : a -> List a
            f = \x -> g x
            g : b -> List b
            g = \x -> f [x]

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This returns something that's incompatible with the return type of the
    enclosing function:

    7│      g = \x -> f [x]
                      ^^^^^

    This `f` call produces:

        List List b

    But I expected the function to have return type:

        List b

    Tip: The type annotation uses the type variable `b` to say that this
    definition can produce any type of value. But in the body I see that
    it will only produce a `List` value of a single specific type. Maybe
    change the type annotation to be more specific? Maybe change the code
    to be more general?
    "
    );

    test_report!(
        polymorphic_recursion_inference_var,
        indoc!(
            r"
            f : _
            f = \x -> f [x]

            f
            "
        ),
        @r"
    ── CIRCULAR TYPE in /code/proj/Main.roc ────────────────────────────────────────

    I'm inferring a weird self-referential type for `f`:

    5│      f = \x -> f [x]
            ^

    Here is my best effort at writing down the type. You will see ∞ for
    parts of the type that repeat something already printed out
    infinitely.

        List ∞ -> *
    "
    );

    test_report!(
        polymorphic_recursion_with_deep_inference_var,
        indoc!(
            r"
            f : _ -> List _
            f = \x -> f [x]

            f
            "
        ),
        @r"
    ── CIRCULAR TYPE in /code/proj/Main.roc ────────────────────────────────────────

    I'm inferring a weird self-referential type for `f`:

    5│      f = \x -> f [x]
            ^

    Here is my best effort at writing down the type. You will see ∞ for
    parts of the type that repeat something already printed out
    infinitely.

        List ∞ -> List *
    "
    );

    test_report!(
        mutual_polymorphic_recursion_with_inference_var,
        indoc!(
            r"
            f : _ -> List _
            f = \x -> g x
            g = \x -> f [x]

            f
            "
        ),
        // TODO: the second error is duplicated because when solving `f : _ -> List _`, we
        // introduce the variable for `f` twice: once to solve `f` without generalization,
        // and then a second time to properly generalize it. When a def is unannotated
        // (like in `g`) the same variable gets used both times, because the type of `g` is
        // only an unbound type variable. However, for `f`, we run `type_to_var` twice,
        // receiving two separate variables, and the second variable doesn't have the cycle
        // error already recorded for the first.
        // The way to resolve this is to always give type annotation signatures an extra
        // variables they can put themselves in, and to run the constraint algorithm
        // against that extra variable, rather than possibly having to translate a `Type`
        // again.
        @r"
    ── CIRCULAR TYPE in /code/proj/Main.roc ────────────────────────────────────────

    I'm inferring a weird self-referential type for `f`:

    5│      f = \x -> g x
            ^

    Here is my best effort at writing down the type. You will see ∞ for
    parts of the type that repeat something already printed out
    infinitely.

        List ∞ -> List *

    ── CIRCULAR TYPE in /code/proj/Main.roc ────────────────────────────────────────

    I'm inferring a weird self-referential type for `g`:

    6│      g = \x -> f [x]
            ^

    Here is my best effort at writing down the type. You will see ∞ for
    parts of the type that repeat something already printed out
    infinitely.

        List ∞ -> List *
    "
    );

    test_report!(
        mutual_polymorphic_recursion_with_inference_var_second,
        indoc!(
            r"
            f = \x -> g x
            g : _ -> List _
            g = \x -> f [x]

            f
            "
        ),
        @r"
    ── CIRCULAR TYPE in /code/proj/Main.roc ────────────────────────────────────────

    I'm inferring a weird self-referential type for `f`:

    4│      f = \x -> g x
            ^

    Here is my best effort at writing down the type. You will see ∞ for
    parts of the type that repeat something already printed out
    infinitely.

        List ∞ -> List *

    ── CIRCULAR TYPE in /code/proj/Main.roc ────────────────────────────────────────

    I'm inferring a weird self-referential type for `g`:

    6│      g = \x -> f [x]
            ^

    Here is my best effort at writing down the type. You will see ∞ for
    parts of the type that repeat something already printed out
    infinitely.

        List ∞ -> List *
    "
    );

    test_report!(
        record_field_mismatch,
        indoc!(
            r"
            bar = { bar : 0x3 }

            f : { foo : Num.Int * } -> [Yes, No]
            f = \_ -> Yes

            f bar
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `f` has an unexpected type:

    9│      f bar
              ^^^

    This `bar` value is a:

        { bar : Int * }

    But `f` needs its 1st argument to be:

        { foo : Int * }

    Tip: Seems like a record field typo. Maybe `bar` should be `foo`?

    Tip: Can more type annotations be added? Type annotations always help
    me give more specific messages, and I think they could help a lot in
    this case
    "
    );

    test_report!(
        tag_mismatch,
        indoc!(
            r"
            f : [Red, Green] -> [Yes, No]
            f = \_ -> Yes

            f Blue
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `f` has an unexpected type:

    7│      f Blue
              ^^^^

    This `Blue` tag has the type:

        [Blue]

    But `f` needs its 1st argument to be:

        [
            Green,
            Red,
        ]

    Tip: Seems like a tag typo. Maybe `Blue` should be `Red`?

    Tip: Can more type annotations be added? Type annotations always help
    me give more specific messages, and I think they could help a lot in
    this case
    "
    );

    test_report!(
        tag_with_arguments_mismatch,
        indoc!(
            r#"
            f : [Red (Num.Int *), Green Str] -> Str
            f = \_ -> "yes"

            f (Blue 3.14)
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `f` has an unexpected type:

    7│      f (Blue 3.14)
               ^^^^^^^^^

    This `Blue` tag application has the type:

        [Blue (Frac *)]

    But `f` needs its 1st argument to be:

        [
            Green Str,
            Red (Int *),
        ]

    Tip: Seems like a tag typo. Maybe `Blue` should be `Red`?

    Tip: Can more type annotations be added? Type annotations always help
    me give more specific messages, and I think they could help a lot in
    this case
    "
    );

    test_report!(
        from_annotation_if,
        indoc!(
            r"
            x : Num.Int _
            x = if Bool.true then 3.14 else 4

            x
            "
        ),
        @r###"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the `then` branch of this `if` expression:

    4│      x : Num.Int _
    5│      x = if Bool.true then 3.14 else 4
                                  ^^^^

    This branch is a fraction of type:

        Frac *

    But the type annotation on `x` says it should be:

        Int *

    Tip: You can convert between integers and fractions using functions
    like `Num.to_frac` and `Num.round`.
    "###
    );

    test_report!(
        from_annotation_when,
        indoc!(
            r"
            x : Num.Int _
            x =
                when True is
                    _ -> 3.14

            x
            "
        ),
        @r###"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `x` definition:

    4│       x : Num.Int _
    5│       x =
    6│>          when True is
    7│>              _ -> 3.14

    This `when` expression produces:

        Frac *

    But the type annotation on `x` says it should be:

        Int *

    Tip: You can convert between integers and fractions using functions
    like `Num.to_frac` and `Num.round`.
    "###
    );

    test_report!(
        from_annotation_function,
        indoc!(
            r"
            x : Num.Int * -> Num.Int *
            x = \_ -> 3.14

            x
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `x` definition:

    4│      x : Num.Int * -> Num.Int *
    5│      x = \_ -> 3.14
                      ^^^^

    The body is a fraction of type:

        Frac *

    But the type annotation on `x` says it should be:

        Int *

    Tip: You can convert between integers and fractions using functions
    like `Num.to_frac` and `Num.round`.
    "
    );

    test_report!(
        fncall_value,
        indoc!(
            r"
            x : Num.I64
            x = 42

            x 3
            "
        ),
        @r"
    ── TOO MANY ARGS in /code/proj/Main.roc ────────────────────────────────────────

    The `x` value is not a function, but it was given 1 argument:

    7│      x 3
            ^

    Are there any missing commas? Or missing parentheses?
    "
    );

    test_report!(
        fncall_overapplied,
        indoc!(
            r"
            f : Num.I64 -> Num.I64
            f = \_ -> 42

            f 1 2
            "
        ),
        @r"
    ── TOO MANY ARGS in /code/proj/Main.roc ────────────────────────────────────────

    The `f` function expects 1 argument, but it got 2 instead:

    7│      f 1 2
            ^

    Are there any missing commas? Or missing parentheses?
    "
    );

    test_report!(
        fncall_underapplied,
        indoc!(
            r"
            f : Num.I64, Num.I64 -> Num.I64
            f = \_, _ -> 42

            f 1
            "
        ),
        @r"
    ── TOO FEW ARGS in /code/proj/Main.roc ─────────────────────────────────────────

    The `f` function expects 2 arguments, but it got only 1:

    7│      f 1
            ^

    Roc does not allow functions to be partially applied. Use a closure to
    make partial application explicit.
    "
    );

    test_report!(
        pattern_when_condition,
        indoc!(
            r"
            when 1 is
                {} -> 42
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The branches of this `when` expression don't match the condition:

    4│>      when 1 is
    5│           {} -> 42

    The `when` condition is a number of type:

        Num *

    But the branch patterns have type:

        {}a

    The branches must be cases of the `when` condition's type!
    "
    );

    test_report!(
        pattern_when_pattern,
        indoc!(
            r"
            when 1 is
                2 -> 3
                {} -> 42
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The 2nd pattern in this `when` does not match the previous ones:

    6│          {} -> 42
                ^^

    The 2nd pattern is trying to match record values of type:

        {}a

    But all the previous branches match:

        Num *
    "
    );

    test_report!(
        pattern_guard_mismatch_alias,
        indoc!(
            r"
             when { foo: 1 } is
                 { foo: True } -> 42
             "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The branches of this `when` expression don't match the condition:

    4│>      when { foo: 1 } is
    5│           { foo: True } -> 42

    The `when` condition is a record of type:

        { foo : Num * }

    But the branch patterns have type:

        { foo : [True] }

    The branches must be cases of the `when` condition's type!
    "
    );

    test_report!(
        pattern_guard_mismatch,
        indoc!(
            r#"
             when { foo: "" } is
                 { foo: True } -> 42
             "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The branches of this `when` expression don't match the condition:

    4│>      when { foo: "" } is
    5│           { foo: True } -> 42

    The `when` condition is a record of type:

        { foo : Str }

    But the branch patterns have type:

        { foo : [True] }

    The branches must be cases of the `when` condition's type!
    "#
    );

    // needs some improvement, but the principle works
    test_report!(
        pattern_guard_does_not_bind_label,
        indoc!(
            r"
             when { foo: 1 } is
                 { foo: _ } -> foo
             "
        ),
        @r"
    ── UNRECOGNIZED NAME in /code/proj/Main.roc ────────────────────────────────────

    Nothing is named `foo` in this scope.

    5│          { foo: _ } -> foo
                              ^^^

    Did you mean one of these?

        Box
        Bool
        U8
        F64
    "
    );

    test_report! {
        pattern_guard_can_be_shadowed_above,
        indoc!(
            r"
            foo = 3

            when { foo: 1 } is
                { foo: 2 } -> foo
                _ -> foo
             "
        ),
        @"" // should give no error
    }

    test_report! {
        pattern_guard_can_be_shadowed_below,
        indoc!(
            r"
            when { foo: 1 } is
                { foo: 2 } ->
                    foo = 3

                    foo
                _ -> 3
             "
        ),
        // should give no error
        @""
    }

    test_report!(
        pattern_or_pattern_mismatch,
        indoc!(
            r"
            when { foo: 1 } is
                {} | 1 -> 3
            "
        ),
        // Just putting this here. We should probably handle or-patterns better
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The 2nd pattern in this branch does not match the previous ones:

    5│          {} | 1 -> 3
                     ^

    The 2nd pattern is trying to match numbers:

        Num *

    But all the previous branches match:

        {}a
    "
    );

    test_report!(
        pattern_let_mismatch,
        indoc!(
            r"
            (Foo x) = 42

            x
            "
        ),
        // Maybe this should specifically say the pattern doesn't work?
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression is used in an unexpected way:

    4│      (Foo x) = 42
                      ^^

    It is a number of type:

        Num *

    But you are trying to use it as:

        [Foo *]
    "
    );

    test_report!(
        from_annotation_complex_pattern,
        indoc!(
            r"
            { x } : { x : Num.Int _ }
            { x } = { x: 4.0 }

            x
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of this definition:

    4│      { x } : { x : Num.Int _ }
    5│      { x } = { x: 4.0 }
                    ^^^^^^^^^^

    The body is a record of type:

        { x : Frac * }

    But the type annotation says it should be:

        { x : Int * }

    Tip: You can convert between integers and fractions using functions
    like `Num.to_frac` and `Num.round`.
    "
    );

    test_report!(
        malformed_int_pattern,
        indoc!(
            r"
            when 1 is
                100A -> 3
                _ -> 4
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This integer pattern is malformed:

    5│          100A -> 3
                ^^^^

    Tip: Learn more about number literals at TODO
    "
    );

    test_report!(
        malformed_float_pattern,
        indoc!(
            r"
            when 1 is
                2.X -> 3
                _ -> 4
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This float pattern is malformed:

    5│          2.X -> 3
                ^^^

    Tip: Learn more about number literals at TODO
    "
    );

    test_report!(
        malformed_hex_pattern,
        indoc!(
            r"
            when 1 is
                0xZ -> 3
                _ -> 4
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This hex integer pattern is malformed:

    5│          0xZ -> 3
                ^^^

    Tip: Learn more about number literals at TODO
    "
    );

    test_report!(
        malformed_oct_pattern,
        indoc!(
            r"
            when 1 is
                0o9 -> 3
                _ -> 4
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This octal integer pattern is malformed:

    5│          0o9 -> 3
                ^^^

    Tip: Learn more about number literals at TODO
    "
    );

    test_report!(
        malformed_bin_pattern,
        indoc!(
            r"
            when 1 is
                0b4 -> 3
                _ -> 4
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This binary integer pattern is malformed:

    5│          0b4 -> 3
                ^^^

    Tip: Learn more about number literals at TODO
    "
    );

    test_report!(
        missing_fields,
        indoc!(
            r"
            x : { a : Num.Int _, b : Num.Frac _, c : Str }
            x = { b: 4.0 }

            x
            "
        ),
        @r###"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `x` definition:

    4│      x : { a : Num.Int _, b : Num.Frac _, c : Str }
    5│      x = { b: 4.0 }
                ^^^^^^^^^^

    The body is a record of type:

        { b : Frac * }

    But the type annotation on `x` says it should be:

        {
            a : Int *,
            b : Frac *,
            c : Str,
        }

    Tip: Looks like the c and a fields are missing.
    "###
    );

    // this previously reported the message below, not sure which is better
    //
    //                Something is off with the body of the `f` definition:
    //
    //                1│ f : a, b -> a
    //                2│ f = \x, y -> if Bool.true then x else y
    //                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    //
    //                The body is an anonymous function of type:
    //
    //                    a, a -> a
    //
    //                But the type annotation on `f` says it should be:
    //
    //                    a, b -> a
    test_report!(
        bad_double_rigid,
        indoc!(
            r"
            f : a, b -> a
            f = \x, y -> if Bool.true then x else y

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the `else` branch of this `if` expression:

    4│      f : a, b -> a
    5│      f = \x, y -> if Bool.true then x else y
                                                  ^

    This `y` value is a:

        b

    But the type annotation on `f` says it should be:

        a

    Tip: Your type annotation uses `b` and `a` as separate type variables.
    Your code seems to be saying they are the same though. Maybe they
    should be the same in your type annotation? Maybe your code uses them
    in a weird way?
    "
    );

    test_report!(
        bad_rigid_function,
        indoc!(
            r"
            f : Str -> msg
            f = \_ -> Foo

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    4│      f : Str -> msg
    5│      f = \_ -> Foo
                      ^^^

    This `Foo` tag has the type:

        [Foo]

    But the type annotation on `f` says it should be:

        msg

    Tip: The type annotation uses the type variable `msg` to say that this
    definition can produce any type of value. But in the body I see that
    it will only produce a tag value of a single specific type. Maybe
    change the type annotation to be more specific? Maybe change the code
    to be more general?
    "
    );

    test_report!(
        bad_rigid_value,
        indoc!(
            r"
            f : msg
            f = 0x3

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    4│      f : msg
    5│      f = 0x3
                ^^^

    The body is an integer of type:

        Int *

    But the type annotation on `f` says it should be:

        msg

    Tip: The type annotation uses the type variable `msg` to say that this
    definition can produce any type of value. But in the body I see that
    it will only produce a `Int` value of a single specific type. Maybe
    change the type annotation to be more specific? Maybe change the code
    to be more general?
    "
    );

    // TODO improve tag suggestions
    test_report!(
        typo_lowercase_ok,
        indoc!(
            r"
            f : Str -> [Ok Num.I64, InvalidFoo]
            f = \_ -> ok 4

            f
            "
        ),
        @r"
    ── UNRECOGNIZED NAME in /code/proj/Main.roc ────────────────────────────────────

    Nothing is named `ok` in this scope.

    5│      f = \_ -> ok 4
                      ^^

    Did you mean one of these?

        U8
        Box
        Eq
        f
    "
    );

    // these error messages seem pretty helpful
    test_report!(
        typo_uppercase_ok,
        indoc!(
            r"
            f : Str -> Num.I64
            f = \_ ->
                ok = 3

                Ok

            f
            "
        ),
        @r"
    ── UNUSED DEFINITION in /code/proj/Main.roc ────────────────────────────────────

    `ok` is not used anywhere in your code.

    6│          ok = 3
                ^^

    If you didn't intend on using `ok` then remove it so future readers of
    your code don't wonder why it is there.

    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    4│      f : Str -> Num.I64
    5│      f = \_ ->
    6│          ok = 3
    7│
    8│          Ok
                ^^

    This `Ok` tag has the type:

        [Ok]

    But the type annotation on `f` says it should be:

        I64
    "
    );

    // invalid recursion
    test_report!(
        circular_definition_self,
        indoc!(
            r"
            f = f

            f
            "
        ),
        @r"
    ── CIRCULAR DEFINITION in /code/proj/Main.roc ──────────────────────────────────

    `f` is defined directly in terms of itself:

    4│      f = f
            ^^^^^

    Roc evaluates values strictly, so running this program would enter an
    infinite loop!

    Hint: Did you mean to define `f` as a function?
    "
    );

    // invalid mutual recursion
    test_report!(
        circular_definition,
        indoc!(
            r"
            foo = bar

            bar = foo

            foo
            "
        ),
        @r"
    ── CIRCULAR DEFINITION in /code/proj/Main.roc ──────────────────────────────────

    The `foo` definition is causing a very tricky infinite loop:

    4│      foo = bar
            ^^^

    The `foo` value depends on itself through the following chain of
    definitions:

        ┌─────┐
        │     foo
        │     ↓
        │     bar
        └─────┘
    "
    );

    test_report!(
        update_empty_record,
        indoc!(
            r"
            x = {}

            { x & foo: 3 }
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This `x` record doesn’t have a `foo` field:

    6│      { x & foo: 3 }
                  ^^^^^^

    In fact, `x` is a record with no fields at all!
    "
    );

    test_report!(
        update_record,
        indoc!(
            r"
            x = { fo: 3, bar: 4 }

            { x & foo: 3 }
            "
        ),
        // TODO also suggest fields with the correct type
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This `x` record doesn’t have a `foo` field:

    6│      { x & foo: 3 }
                  ^^^^^^

    There may be a typo. These `x` fields are the most similar:

        {
            fo : Num *,
            bar : Num *,
        }

    Maybe `foo:` should be `fo:` instead?
    "
    );

    test_report!(
        update_record_ext,
        indoc!(
            r"
            f : { fo: Num.I64 }ext -> Num.I64
            f = \r ->
                r2 = { r & foo: r.fo }

                r2.fo

            f
            "
        ),
        // TODO also suggest fields with the correct type
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This `r` record doesn’t have a `foo` field:

    6│          r2 = { r & foo: r.fo }
                           ^^^^^^^^^

    There may be a typo. These `r` fields are the most similar:

        {
            fo : I64,
        }ext

    Maybe `foo:` should be `fo:` instead?
    "
    );

    test_report!(
        update_record_snippet,
        indoc!(
            r"
            x = { fo: 3, bar: 4, baz: 3, spam: 42, foobar: 3 }

            { x & foo: 3 }
            "
        ),
        // TODO also suggest fields with the correct type
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This `x` record doesn’t have a `foo` field:

    6│      { x & foo: 3 }
                  ^^^^^^

    There may be a typo. These `x` fields are the most similar:

        {
            fo : Num *,
            foobar : Num *,
            bar : Num *,
            baz : Num *,
            …
        }

    Maybe `foo:` should be `fo:` instead?
    "
    );

    test_report!(
        plus_on_str,
        indoc!(
            r#"
            0x4 + "foo"
            "#
        ),
        // TODO also suggest fields with the correct type
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to + has an unexpected type:

    4│      0x4 + "foo"
                  ^^^^^

    The argument is a string of type:

        Str

    But + needs its 2nd argument to be:

        Int *
    "#
    );

    test_report!(
        int_frac,
        indoc!(
            r"
            0x4 + 3.14
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to + has an unexpected type:

    4│      0x4 + 3.14
                  ^^^^

    The argument is a fraction of type:

        Frac *

    But + needs its 2nd argument to be:

        Int *

    Tip: You can convert between integers and fractions using functions
    like `Num.to_frac` and `Num.round`.
    "
    );

    test_report!(
        boolean_tag,
        indoc!(
            r"
            42 + True
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to + has an unexpected type:

    4│      42 + True
                 ^^^^

    This `True` tag has the type:

        [True]

    But + needs its 2nd argument to be:

        Num *
    "
    );

    test_report!(
        tag_missing,
        indoc!(
            r"
            f : [A] -> [A, B]
            f = \a -> a

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    4│      f : [A] -> [A, B]
    5│      f = \a -> a
                      ^

    This `a` value is a:

        […]

    But the type annotation on `f` says it should be:

        [B, …]

    Tip: Looks like a closed tag union does not have the `B` tag.

    Tip: Closed tag unions can't grow, because that might change the size
    in memory. Can you use an open tag union?
    "
    );

    test_report!(
        tags_missing,
        indoc!(
            r"
            f : [A] -> [A, B, C]
            f = \a -> a

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    4│      f : [A] -> [A, B, C]
    5│      f = \a -> a
                      ^

    This `a` value is a:

        […]

    But the type annotation on `f` says it should be:

        [
            B,
            C,
            …
        ]

    Tip: Looks like a closed tag union does not have the `B` and `C` tags.

    Tip: Closed tag unions can't grow, because that might change the size
    in memory. Can you use an open tag union?
    "
    );

    test_report!(
        patterns_fn_not_exhaustive,
        indoc!(
            r"
            Either : [Left {}, Right Str]

            x : Either
            x = Left {}

            f : Either -> {}
            f = \Left v -> v

            f x
            "
        ),
        @r"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This pattern does not cover all the possibilities:

    10│      f = \Left v -> v
                  ^^^^^^

    Other possibilities include:

        Right _

    I would have to crash if I saw one of those! So rather than pattern
    matching in function arguments, put a `when` in the function body to
    account for all possibilities.

    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

     9│      f : Either -> {}
    10│      f = \Left v -> v
                 ^^^^^^^^^^^^

    The body is an anonymous function of type:

        […] -> {}

    But the type annotation on `f` says it should be:

        [Right Str, …] -> {}

    Tip: Looks like a closed tag union does not have the `Right` tag.

    Tip: Closed tag unions can't grow, because that might change the size
    in memory. Can you use an open tag union?
    "
    );

    test_report!(
        patterns_let_not_exhaustive,
        indoc!(
            r"
            x : [Left {}, Right Str]
            x = Left {}


            (Left y) = x

            y
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression is used in an unexpected way:

    8│      (Left y) = x
                       ^

    This `x` value is a:

        [Right Str, …]

    But you are trying to use it as:

        […]

    Tip: Looks like a closed tag union does not have the `Right` tag.

    Tip: Closed tag unions can't grow, because that might change the size
    in memory. Can you use an open tag union?
    "
    );

    test_report!(
        patterns_when_not_exhaustive,
        indoc!(
            r"
            when 0x1 is
                2 -> 0x3
            "
        ),
        @r"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    4│>      when 0x1 is
    5│>          2 -> 0x3

    Other possibilities include:

        _

    I would have to crash if I saw one of those! Add branches for them!
    "
    );

    test_report!(
        patterns_bool_not_exhaustive,
        indoc!(
            r"
            x : [Red, Green]
            x = Green

            when x is
                Red -> 3
            "
        ),
        @r"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    7│>      when x is
    8│>          Red -> 3

    Other possibilities include:

        Green

    I would have to crash if I saw one of those! Add branches for them!
    "
    );

    test_report!(
        patterns_enum_not_exhaustive,
        indoc!(
            r"
            x : [Red, Green, Blue]
            x = Red

            when x is
                Red -> 0
                Green -> 1
            "
        ),
        @r"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    7│>      when x is
    8│>          Red -> 0
    9│>          Green -> 1

    Other possibilities include:

        Blue

    I would have to crash if I saw one of those! Add branches for them!
    "
    );

    test_report!(
        patterns_remote_data_not_exhaustive,
        indoc!(
            r"
            RemoteData e a :  [NotAsked, Loading, Failure e, Success a]

            x : RemoteData Num.I64 Str

            when x is
                NotAsked -> 3
            "
        ),
        @r"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    8│>      when x is
    9│>          NotAsked -> 3

    Other possibilities include:

        Failure _
        Loading
        Success _

    I would have to crash if I saw one of those! Add branches for them!
    "
    );

    test_report!(
        patterns_record_not_exhaustive,
        indoc!(
            r"
            x = { a: 3 }

            when x is
                { a: 4 } -> 4
            "
        ),
        // Tip: Looks like a record field guard is not exhaustive. Learn more about record pattern matches at TODO.
        @r"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    6│>      when x is
    7│>          { a: 4 } -> 4

    Other possibilities include:

        { a }

    I would have to crash if I saw one of those! Add branches for them!
    "
    );

    test_report!(
        patterns_record_guard_not_exhaustive,
        indoc!(
            r"
            y : [Nothing, Just Num.I64]
            y = Just 4
            x = { a: y, b: 42}

            when x is
                { a: Nothing } -> 4
                { a: Just 3 } -> 4
            "
        ),
        @r"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

     8│>      when x is
     9│>          { a: Nothing } -> 4
    10│>          { a: Just 3 } -> 4

    Other possibilities include:

        { a: Just _ }

    I would have to crash if I saw one of those! Add branches for them!
    "
    );

    test_report!(
        patterns_nested_tag_not_exhaustive,
        indoc!(
            r"
            when Record Nothing 1 is
                Record (Nothing) b -> b
                Record (Just 3) b -> b
            "
        ),
        @r"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    4│>      when Record Nothing 1 is
    5│>          Record (Nothing) b -> b
    6│>          Record (Just 3) b -> b

    Other possibilities include:

        Record (Just _) _

    I would have to crash if I saw one of those! Add branches for them!
    "
    );

    test_report!(
        patterns_int_redundant,
        indoc!(
            r"
            when 0x1 is
                2 -> 3
                2 -> 4
                _ -> 5
            "
        ),
        @r"
    ── REDUNDANT PATTERN in /code/proj/Main.roc ────────────────────────────────────

    The 2nd pattern is redundant:

    4│       when 0x1 is
    5│           2 -> 3
    6│>          2 -> 4
    7│           _ -> 5

    Any value of this shape will be handled by a previous pattern, so this
    one should be removed.
    "
    );

    test_report!(
        unify_alias_other,
        indoc!(
            r"
            Foo a : { x : Num.Int a }

            f : Foo a -> Num.Int a
            f = \r -> r.x

            f { y: 3.14 }
            "
        ),
        // de-aliases the alias to give a better error message
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `f` has an unexpected type:

    9│      f { y: 3.14 }
              ^^^^^^^^^^^

    The argument is a record of type:

        { y : Frac * }

    But `f` needs its 1st argument to be:

        { x : Int a }

    Tip: Seems like a record field typo. Maybe `y` should be `x`?

    Tip: Can more type annotations be added? Type annotations always help
    me give more specific messages, and I think they could help a lot in
    this case
    "
    );

    test_report!(
        #[ignore]
        cyclic_alias,
        indoc!(
            r"
            Foo : { x : Bar }
            Bar : { y : Foo }

            f : Foo

            f
            "
        ),
        // should not report Bar as unused!
        @r"
    ── CYCLIC ALIAS in /code/proj/Main.roc ─────────────────────────────────────────

    The `Foo` alias is self-recursive in an invalid way:

    4│      Foo : { x : Bar }
            ^^^

    Recursion in aliases is only allowed if recursion happens behind a
    tagged union, at least one variant of which is not recursive.
    "
    );

    test_report!(
        self_recursive_alias,
        indoc!(
            r"
            Foo : { x : Foo }

            f : Foo
            f = 3

            f
            "
        ),
        // should not report Bar as unused!
        @r"
    ── CYCLIC ALIAS in /code/proj/Main.roc ─────────────────────────────────────────

    The `Foo` alias is self-recursive in an invalid way:

    4│      Foo : { x : Foo }
            ^^^

    Recursion in aliases is only allowed if recursion happens behind a
    tagged union, at least one variant of which is not recursive.
    "
    );

    test_report!(
        record_duplicate_field_same_type,
        indoc!(
            r"
            { x: 4, y: 3, x: 4 }
            "
        ),
        @r"
    ── DUPLICATE FIELD NAME in /code/proj/Main.roc ─────────────────────────────────

    This record defines the `.x` field twice!

    4│      { x: 4, y: 3, x: 4 }
              ^^^^        ^^^^

    In the rest of the program, I will only use the latter definition:

    4│      { x: 4, y: 3, x: 4 }
                          ^^^^

    For clarity, remove the previous `.x` definitions from this record.
    "
    );

    test_report!(
        record_duplicate_field_different_types,
        indoc!(
            r#"
            { x: 4, y: 3, x: "foo" }
            "#
        ),
        @r#"
    ── DUPLICATE FIELD NAME in /code/proj/Main.roc ─────────────────────────────────

    This record defines the `.x` field twice!

    4│      { x: 4, y: 3, x: "foo" }
              ^^^^        ^^^^^^^^

    In the rest of the program, I will only use the latter definition:

    4│      { x: 4, y: 3, x: "foo" }
                          ^^^^^^^^

    For clarity, remove the previous `.x` definitions from this record.
    "#
    );

    test_report!(
        record_duplicate_field_multiline,
        indoc!(
            r#"
            {
                x: 4,
                y: 3,
                x: "foo"
            }
            "#
        ),
        @r#"
    ── DUPLICATE FIELD NAME in /code/proj/Main.roc ─────────────────────────────────

    This record defines the `.x` field twice!

    4│       {
    5│>          x: 4,
    6│           y: 3,
    7│>          x: "foo"
    8│       }

    In the rest of the program, I will only use the latter definition:

    4│       {
    5│           x: 4,
    6│           y: 3,
    7│>          x: "foo"
    8│       }

    For clarity, remove the previous `.x` definitions from this record.
    "#
    );

    test_report!(
        record_update_duplicate_field_multiline,
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
        @r#"
    ── DUPLICATE FIELD NAME in /code/proj/Main.roc ─────────────────────────────────

    This record defines the `.x` field twice!

    5│           { r &
    6│>              x: 4,
    7│               y: 3,
    8│>              x: "foo"
    9│           }

    In the rest of the program, I will only use the latter definition:

    5│           { r &
    6│               x: 4,
    7│               y: 3,
    8│>              x: "foo"
    9│           }

    For clarity, remove the previous `.x` definitions from this record.
    "#
    );

    test_report!(
        record_type_duplicate_field,
        indoc!(
            r#"
            a : { foo : Num.I64, bar : {}, foo : Str }
            a = { bar: {}, foo: "foo" }

            a
            "#
        ),
        @r"
    ── DUPLICATE FIELD NAME in /code/proj/Main.roc ─────────────────────────────────

    This record type defines the `.foo` field twice!

    4│      a : { foo : Num.I64, bar : {}, foo : Str }
                  ^^^^^^^^^^^^^            ^^^^^^^^^

    In the rest of the program, I will only use the latter definition:

    4│      a : { foo : Num.I64, bar : {}, foo : Str }
                                           ^^^^^^^^^

    For clarity, remove the previous `.foo` definitions from this record
    type.
    "
    );

    test_report!(
        tag_union_duplicate_tag,
        indoc!(
            r#"
            a : [Foo Num.I64, Bar {}, Foo Str]
            a = Foo "foo"

            a
            "#
        ),
        @r"
    ── DUPLICATE TAG NAME in /code/proj/Main.roc ───────────────────────────────────

    This tag union type defines the `Foo` tag twice!

    4│      a : [Foo Num.I64, Bar {}, Foo Str]
                 ^^^^^^^^^^^          ^^^^^^^

    In the rest of the program, I will only use the latter definition:

    4│      a : [Foo Num.I64, Bar {}, Foo Str]
                                      ^^^^^^^

    For clarity, remove the previous `Foo` definitions from this tag union
    type.
    "
    );

    test_report!(
        annotation_definition_mismatch,
        indoc!(
            r"
            bar : Num.I64
            foo = \x -> x

            # NOTE: neither bar or foo are defined at this point
            4
            "
        ),
        @r"
    ── NAMING PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This annotation does not match the definition immediately following
    it:

    4│>      bar : Num.I64
    5│>      foo = \x -> x

    Is it a typo? If not, put either a newline or comment between them.
    "
    );

    test_report!(
        annotation_newline_body_is_fine,
        indoc!(
            r"
            bar : Num.I64

            foo = \x -> x

            foo bar
            "
        ),
        @""
    );

    test_report!(
        invalid_alias_rigid_var_pattern,
        indoc!(
            r"
            MyAlias 1 : Num.I64

            4
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This definition of `MyAlias` has an unexpected pattern:

    4│      MyAlias 1 : Num.I64
                    ^

    Only type variables like `a` or `value` can occur in this position.

    ── UNUSED DEFINITION in /code/proj/Main.roc ────────────────────────────────────

    `MyAlias` is not used anywhere in your code.

    4│      MyAlias 1 : Num.I64
            ^^^^^^^^^^^^^^^^^^^

    If you didn't intend on using `MyAlias` then remove it so future readers
    of your code don't wonder why it is there.
    "
    );

    test_report!(
        invalid_opaque_rigid_var_pattern,
        indoc!(
            r"
            Age 1 := Num.I64

            a : Age
            a
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This definition of `Age` has an unexpected pattern:

    4│      Age 1 := Num.I64
                ^

    Only type variables like `a` or `value` can occur in this position.
    "
    );

    test_report!(
        invalid_num,
        indoc!(
            r"
            a : Num.Num Num.I64 Num.F64
            a = 3

            a
            "
        ),
        @r"
    ── TOO MANY TYPE ARGUMENTS in /code/proj/Main.roc ──────────────────────────────

    The `Num` opaque expects 1 type argument, but it got 2 instead:

    4│      a : Num.Num Num.I64 Num.F64
                ^^^^^^^^^^^^^^^^^^^^^^^

    Are there missing parentheses?
    "
    );

    test_report!(
        invalid_num_fn,
        indoc!(
            r"
            f : Str -> Num.Num Num.I64 Num.F64
            f = \_ -> 3

            f
            "
        ),
        @r"
    ── TOO MANY TYPE ARGUMENTS in /code/proj/Main.roc ──────────────────────────────

    The `Num` opaque expects 1 type argument, but it got 2 instead:

    4│      f : Str -> Num.Num Num.I64 Num.F64
                       ^^^^^^^^^^^^^^^^^^^^^^^

    Are there missing parentheses?
    "
    );

    test_report!(
        too_few_type_arguments,
        indoc!(
            r"
            Pair a b : [Pair a b]

            x : Pair Num.I64
            x = Pair 2 3

            x
            "
        ),
        @r"
    ── TOO FEW TYPE ARGUMENTS in /code/proj/Main.roc ───────────────────────────────

    The `Pair` alias expects 2 type arguments, but it got 1 instead:

    6│      x : Pair Num.I64
                ^^^^^^^^^^^^

    Are there missing parentheses?
    "
    );

    test_report!(
        too_many_type_arguments,
        indoc!(
            r"
            Pair a b : [Pair a b]

            x : Pair Num.I64 Num.I64 Num.I64
            x = 3

            x
            "
        ),
        @r"
    ── TOO MANY TYPE ARGUMENTS in /code/proj/Main.roc ──────────────────────────────

    The `Pair` alias expects 2 type arguments, but it got 3 instead:

    6│      x : Pair Num.I64 Num.I64 Num.I64
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Are there missing parentheses?
    "
    );

    test_report!(
        phantom_type_variable,
        indoc!(
            r"
            Foo a : [Foo]

            f : Foo Num.I64

            f
            "
        ),
        @r#"
    ── UNUSED TYPE ALIAS PARAMETER in /code/proj/Main.roc ──────────────────────────

    The `a` type parameter is not used in the `Foo` alias definition:

    4│      Foo a : [Foo]
                ^

    Roc does not allow unused type parameters!

    Tip: If you want an unused type parameter (a so-called "phantom
    type"), read the guide section on phantom values.
    "#
    );

    test_report!(
        elm_function_syntax,
        indoc!(
            r"
            f x y = x
            "
        ),
        @r###"
    ── ARGUMENTS BEFORE EQUALS in tmp/elm_function_syntax/Test.roc ─────────────────

    I am partway through parsing a definition, but I got stuck here:

    1│  app "test" provides [main] to "./platform"
    2│
    3│  main =
    4│      f x y = x
              ^^^

    Looks like you are trying to define a function. In Roc, functions are
    always written as a lambda, like increment = |n| n + 1.
    "###
    );

    test_report!(
        two_different_cons,
        indoc!(
            r#"
            ConsList a : [Cons a (ConsList a), Nil]

            x : ConsList {}
            x = Cons {} (Cons "foo" Nil)

            x
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `x` definition:

    6│      x : ConsList {}
    7│      x = Cons {} (Cons "foo" Nil)
                ^^^^^^^^^^^^^^^^^^^^^^^^

    This `Cons` tag application has the type:

        [
            Cons {} [
                Cons Str [
                    Cons {} a,
                    Nil,
                ]b as a,
                Nil,
            ]b,
            Nil,
        ]b

    But the type annotation on `x` says it should be:

        [
            Cons {} a,
            Nil,
        ] as a
    "#
    );

    test_report!(
        mutually_recursive_types_with_type_error,
        indoc!(
            r#"
            AList a b : [ACons a (BList a b), ANil]
            BList a b : [BCons a (AList a b), BNil]

            x : AList Num.I64 Num.I64
            x = ACons 0 (BCons 1 (ACons "foo" BNil ))

            y : BList _ _
            y = BNil

            { x, y }
            "#
        ),
        // TODO render tag unions across multiple lines
        // TODO do not show recursion var if the recursion var does not render on the surface of a type
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `x` definition:

    7│      x : AList Num.I64 Num.I64
    8│      x = ACons 0 (BCons 1 (ACons "foo" BNil ))
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    This `ACons` tag application has the type:

        [
            ACons (Int Signed64) [
                BCons (Int Signed64) [
                    ACons Str [
                        BCons I64 [
                            ACons I64 (BList I64 I64),
                            ANil,
                        ]b as ∞,
                        BNil,
                    ]c,
                    ANil,
                ]b,
                BNil,
            ]c,
            ANil,
        ]b

    But the type annotation on `x` says it should be:

        [
            ACons I64 (BList I64 I64),
            ANil,
        ] as a
    "#
    );

    test_report!(
        integer_out_of_range,
        indoc!(
            r"
            x = 170_141_183_460_469_231_731_687_303_715_884_105_728_000

            y = -170_141_183_460_469_231_731_687_303_715_884_105_728_000

            h = 0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF
            l = -0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF

            minlit = -170_141_183_460_469_231_731_687_303_715_884_105_728
            maxlit =  340_282_366_920_938_463_463_374_607_431_768_211_455

            x + y + h + l + minlit + maxlit
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This integer literal is too big:

    4│      x = 170_141_183_460_469_231_731_687_303_715_884_105_728_000
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The largest number representable in Roc is the maximum U128 value,
    340_282_366_920_938_463_463_374_607_431_768_211_455.

    Tip: Learn more about number literals at TODO

    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This integer literal is too small:

    6│      y = -170_141_183_460_469_231_731_687_303_715_884_105_728_000
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The smallest number representable in Roc is the minimum I128 value,
    -170_141_183_460_469_231_731_687_303_715_884_105_728.

    Tip: Learn more about number literals at TODO

    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This integer literal is too big:

    8│      h = 0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The largest number representable in Roc is the maximum U128 value,
    340_282_366_920_938_463_463_374_607_431_768_211_455.

    Tip: Learn more about number literals at TODO

    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This integer literal is too small:

    9│      l = -0xFFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF_FFFF
                ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The smallest number representable in Roc is the minimum I128 value,
    -170_141_183_460_469_231_731_687_303_715_884_105_728.

    Tip: Learn more about number literals at TODO

    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to + has an unexpected type:

    14│      x + y + h + l + minlit + maxlit
                                      ^^^^^^

    This `maxlit` value is a:

        U128

    But + needs its 2nd argument to be:

        I128 or Dec
    "
    );

    // have to deal with some whitespace issues because of the format! macro
    test_report!(
        float_out_of_range,
        indoc!(
            r"
            overflow = 11.7976931348623157e308
            underflow = -11.7976931348623157e308

            overflow + underflow
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This float literal is too big:

    4│      overflow = 11.7976931348623157e308
                       ^^^^^^^^^^^^^^^^^^^^^^^

    Roc uses signed 64-bit floating points, allowing values between
    -1.7976931348623157e308 and 1.7976931348623157e308

    Tip: Learn more about number literals at TODO

    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This float literal is too small:

    5│      underflow = -11.7976931348623157e308
                        ^^^^^^^^^^^^^^^^^^^^^^^^

    Roc uses signed 64-bit floating points, allowing values between
    -1.7976931348623157e308 and 1.7976931348623157e308

    Tip: Learn more about number literals at TODO
    "
    );

    // the generated messages here are incorrect. Waiting for a rust nightly feature to land,
    // see https://github.com/rust-lang/rust/issues/22639
    // this test is here to spot regressions in error reporting
    test_report!(
        integer_malformed,
        indoc!(
            r"
            dec = 100A

            hex = 0xZZZ

            oct = 0o9

            bin = 0b2

            dec + hex + oct + bin
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This integer literal contains an invalid digit:

    4│      dec = 100A
                  ^^^^

    Integer literals can only contain the digits
    0-9, or have an integer suffix.

    Tip: Learn more about number literals at TODO

    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This hex integer literal contains an invalid digit:

    6│      hex = 0xZZZ
                  ^^^^^

    Hexadecimal (base-16) integer literals can only contain the digits
    0-9, a-f and A-F, or have an integer suffix.

    Tip: Learn more about number literals at TODO

    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This octal integer literal contains an invalid digit:

    8│      oct = 0o9
                  ^^^

    Octal (base-8) integer literals can only contain the digits
    0-7, or have an integer suffix.

    Tip: Learn more about number literals at TODO

    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This binary integer literal contains an invalid digit:

    10│      bin = 0b2
                   ^^^

    Binary (base-2) integer literals can only contain the digits
    0 and 1, or have an integer suffix.

    Tip: Learn more about number literals at TODO
    "
    );

    test_report!(
        integer_empty,
        indoc!(
            r"
            dec = 20

            hex = 0x

            oct = 0o

            bin = 0b

            dec + hex + oct + bin
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This hex integer literal contains no digits:

    6│      hex = 0x
                  ^^

    Hexadecimal (base-16) integer literals must contain at least one of
    the digits 0-9, a-f and A-F, or have an integer suffix.

    Tip: Learn more about number literals at TODO

    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This octal integer literal contains no digits:

    8│      oct = 0o
                  ^^

    Octal (base-8) integer literals must contain at least one of the
    digits 0-7, or have an integer suffix.

    Tip: Learn more about number literals at TODO

    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This binary integer literal contains no digits:

    10│      bin = 0b
                   ^^

    Binary (base-2) integer literals must contain at least one of the
    digits 0 and 1, or have an integer suffix.

    Tip: Learn more about number literals at TODO
    "
    );

    test_report!(
        float_malformed,
        indoc!(
            r"
            x = 3.0A

            x
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This float literal contains an invalid digit:

    4│      x = 3.0A
                ^^^^

    Floating point literals can only contain the digits 0-9, or use
    scientific notation 10e4, or have a float suffix.

    Tip: Learn more about number literals at TODO
    "
    );

    test_report!(
        invalid_record_update,
        indoc!(
            r"
            foo = { bar: 3 }
            update_nested_record = { foo.bar & x: 4 }

            example = { age: 42 }

            # these should work
            y = { Test.example & age: 3 }
            x = { example & age: 4 }

            { update_nested_record, foo, x, y }
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This expression cannot be updated:

    5│      update_nested_record = { foo.bar & x: 4 }
                                     ^^^^^^^

    Only variables can be updated with record update syntax.

    ── MODULE NOT IMPORTED in /code/proj/Main.roc ──────────────────────────────────

    The `Test` module is not imported:

    10│      y = { Test.example & age: 3 }
                   ^^^^^^^^^^^^

    Is there an import missing? Perhaps there is a typo. Did you mean one
    of these?

        Set
        List
        Dict
        Hash

    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This expression cannot be updated:

    10│      y = { Test.example & age: 3 }
                   ^^^^^^^^^^^^

    Only variables can be updated with record update syntax.
    "
    );

    test_report!(
        module_not_imported,
        indoc!(
            r"
            Foo.test
            "
        ),
        @r"
    ── MODULE NOT IMPORTED in /code/proj/Main.roc ──────────────────────────────────

    The `Foo` module is not imported:

    4│      Foo.test
            ^^^^^^^^

    Is there an import missing? Perhaps there is a typo. Did you mean one
    of these?

        Box
        Bool
        Num
        Set
    "
    );

    test_report!(
        optional_record_default_type_error,
        indoc!(
            r"
            \{ x, y ? True } -> x + y
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to + has an unexpected type:

    4│      \{ x, y ? True } -> x + y
                                    ^

    This `y` value is a:

        [True]

    But + needs its 2nd argument to be:

        Num a
    "
    );

    test_report!(
        optional_record_default_with_signature,
        indoc!(
            r#"
            f : { x : Num.I64, y ? Num.I64 } -> Num.I64
            f = \{ x, y ? "foo" } -> (\g, _ -> g) x y

            f
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The 1st argument to `f` is weird:

    5│      f = \{ x, y ? "foo" } -> (\g, _ -> g) x y
                 ^^^^^^^^^^^^^^^^

    The argument is a pattern that matches record values of type:

        { y ? Str, … }

    But the annotation on `f` says the 1st argument should be:

        { y ? I64, … }
    "#
    );

    test_report!(
        optional_record_invalid_let_binding,
        indoc!(
            r"
            \rec ->
                { x, y } : { x : Num.I64, y ? Str }
                { x, y } = rec

                { x, y }
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of this definition:

    5│>          { x, y } : { x : Num.I64, y ? Str }
    6│>          { x, y } = rec

    The body is a value of type:

        { y : Str, … }

    But the type annotation says it should be:

        { y ? Str, … }

    Tip: To extract the `.y` field it must be non-optional, but the type
    says this field is optional. Learn more about optional fields at TODO.
    "
    );

    test_report!(
        optional_record_invalid_function,
        indoc!(
            r"
            f : { x : Num.I64, y ? Num.I64 } -> Num.I64
            f = \{ x, y } -> x + y

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The 1st argument to `f` is weird:

    5│      f = \{ x, y } -> x + y
                 ^^^^^^^^

    The argument is a pattern that matches record values of type:

        { y : I64, … }

    But the annotation on `f` says the 1st argument should be:

        { y ? I64, … }

    Tip: To extract the `.y` field it must be non-optional, but the type
    says this field is optional. Learn more about optional fields at TODO.
    "
    );

    test_report!(
        optional_record_invalid_when,
        indoc!(
            r"
            f : { x : Num.I64, y ? Num.I64 } -> Num.I64
            f = \r ->
                    when r is
                        { x, y } -> x + y

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The branches of this `when` expression don't match the condition:

    6│>              when r is
    7│                   { x, y } -> x + y

    This `r` value is a:

        { y ? I64, … }

    But the branch patterns have type:

        { y : I64, … }

    The branches must be cases of the `when` condition's type!

    Tip: To extract the `.y` field it must be non-optional, but the type
    says this field is optional. Learn more about optional fields at TODO.
    "
    );

    test_report!(
        optional_record_invalid_access,
        indoc!(
            r"
            f : { x : Num.I64, y ? Num.I64 } -> Num.I64
            f = \r -> r.y

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression is used in an unexpected way:

    5│      f = \r -> r.y
                      ^^^

    This `r` value is a:

        { y ? I64, … }

    But you are trying to use it as:

        { y : I64, … }

    Tip: To extract the `.y` field it must be non-optional, but the type
    says this field is optional. Learn more about optional fields at TODO.
    "
    );

    test_report!(
        optional_record_invalid_accessor,
        indoc!(
            r"
                f : { x : Num.I64, y ? Num.I64 } -> Num.I64
                f = \r -> .y r

                f
                "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to this function has an unexpected type:

    5│      f = \r -> .y r
                         ^

    This `r` value is a:

        { y ? I64, … }

    But this function needs its 1st argument to be:

        { y : I64, … }

    Tip: To extract the `.y` field it must be non-optional, but the type
    says this field is optional. Learn more about optional fields at TODO.
    "
    );

    test_report!(
        guard_mismatch_with_annotation,
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
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The branches of this `when` expression don't match the condition:

    6│>              when r is
    7│                   { x, y : "foo" } -> x + 0
    8│                   _ -> 0

    This `r` value is a:

        { y : I64, … }

    But the branch patterns have type:

        { y : Str, … }

    The branches must be cases of the `when` condition's type!
    "#
    );

    test_report!(
        optional_field_mismatch_with_annotation,
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
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The branches of this `when` expression don't match the condition:

    6│>              when r is
    7│                   { x, y ? "foo" } -> (\g, _ -> g) x y
    8│                   _ -> 0

    This `r` value is a:

        { y ? I64, … }

    But the branch patterns have type:

        { y ? Str, … }

    The branches must be cases of the `when` condition's type!
    "#
    );

    test_report!(
        incorrect_optional_field,
        indoc!(
            r"
            { x: 5, y ? 42 }
            "
        ),
        @r"
    ── BAD OPTIONAL VALUE in /code/proj/Main.roc ───────────────────────────────────

    This record uses an optional value for the `.y` field in an incorrect
    context!

    4│      { x: 5, y ? 42 }
                    ^^^^^^

    You can only use optional values in record destructuring, like:

        { answer ? 42, otherField } = myRecord
    "
    );

    test_report!(
        first_wildcard_is_required,
        indoc!(
            r"
            when Foo 1 2 3 is
                Foo _ 1 _ -> 1
                _ -> 2
            "
        ),
        @""
    );

    test_report!(
        second_wildcard_is_redundant,
        indoc!(
            r"
            when Foo 1 2 3 is
                Foo _ 1 _ -> 1
                _ -> 2
                _ -> 3
            "
        ),
        @r"
    ── REDUNDANT PATTERN in /code/proj/Main.roc ────────────────────────────────────

    The 3rd pattern is redundant:

    4│      when Foo 1 2 3 is
    5│          Foo _ 1 _ -> 1
    6│          _ -> 2
    7│          _ -> 3
                ^

    Any value of this shape will be handled by a previous pattern, so this
    one should be removed.
    "
    );

    test_report!(
        alias_using_alias,
        indoc!(
            r"
            # The color of a node. Leaves are considered Black.
            NodeColor : [Red, Black]

            RBTree k v : [Node NodeColor k v (RBTree k v) (RBTree k v), Empty]

            # Create an empty dictionary.
            empty : {} -> RBTree k v
            empty = \{} -> Empty

            empty
            "
        ),
        @""
    );

    test_report!(
        unused_argument,
        indoc!(
            r"
            f = \foo -> 1

            f
            "
        ),
        @r#"
    ── UNUSED ARGUMENT in /code/proj/Main.roc ──────────────────────────────────────

    `f` doesn't use `foo`.

    4│      f = \foo -> 1
                 ^^^

    If you don't need `foo`, then you can just remove it. However, if you
    really do need `foo` as an argument of `f`, prefix it with an underscore,
    like this: "_`foo`". Adding an underscore at the start of a variable
    name is a way of saying that the variable is not used.
    "#
    );

    test_report!(
        qualified_tag,
        indoc!(
            r"
            Foo.Bar
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    I am trying to parse a qualified name here:

    4│      Foo.Bar
                   ^

    This looks like a qualified tag name to me, but tags cannot be
    qualified! Maybe you wanted a qualified name, something like
    Json.Decode.string?
    "
    );

    test_report!(
        module_ident_ends_with_dot,
        indoc!(
            r"
            Foo.Bar.
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    I am trying to parse a qualified name here:

    4│      Foo.Bar.
                    ^

    I was expecting to see an identifier next, like height. A complete
    qualified name looks something like Json.Decode.string.
    "
    );

    test_report!(
        record_access_ends_with_dot,
        indoc!(
            r"
            foo.bar.
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    I am trying to parse a record field access here:

    4│      foo.bar.
                    ^

    So I expect to see a lowercase letter next, like .name or .height.
    "
    );

    test_report!(
        type_annotation_double_colon,
        indoc!(
            r"
            f :: I64
            f = 42

            f
            "
        ),
        @r#"
    ── UNKNOWN OPERATOR in tmp/type_annotation_double_colon/Test.roc ───────────────

    This looks like an operator, but it's not one I recognize!

    1│  app "test" provides [main] to "./platform"
    2│
    3│  main =
    4│      f :: I64
              ^^

    I have no specific suggestion for this operator, see
    https://www.roc-lang.org/tutorial#operator-desugaring-table for the
    full list of operators in Roc.
    "#
    );

    // NOTE: VERY BAD ERROR MESSAGE
    //
    // looks like `x y` are considered argument to the add, even though they are
    // on a lower indentation level
    test_report!(
        double_equals_in_def,
        indoc!(
            r"
            x = 3
            y =
                x == 5
                Num.add 1 2

            { x,  y }
            "
        ),
        @r#"
    ── IGNORED RESULT in /code/proj/Main.roc ───────────────────────────────────────

    The result of this expression is ignored:

    6│          x == 5
                ^^^^^^

    Standalone statements are required to produce an empty record, but the
    type of this one is:

        Bool

    If you still want to ignore it, assign it to `_`, like this:

        _ = File.delete! "data.json"

    ── LEFTOVER STATEMENT in /code/proj/Main.roc ───────────────────────────────────

    This statement does not produce any effects:

    6│          x == 5
                ^^^^^^

    Standalone statements are only useful if they call effectful
    functions.

    Did you forget to use its result? If not, feel free to remove it.
    "#
    );

    test_report!(
        tag_union_open,
        indoc!(
            r"
            f : [
            "
        ),
        @r"
    ── UNFINISHED TAG UNION TYPE in tmp/tag_union_open/Test.roc ────────────────────

    I am partway through parsing a tag union type, but I got stuck here:

    4│      f : [
    5│
    6│
        ^

    I was expecting to see a closing square bracket before this, so try
    adding a ] and see if that helps?
    "
    );

    test_report!(
        tag_union_end,
        indoc!(
            r"
            f : [Yes,
            "
        ),
        @r"
    ── UNFINISHED TAG UNION TYPE in tmp/tag_union_end/Test.roc ─────────────────────

    I am partway through parsing a tag union type, but I got stuck here:

    4│      f : [Yes,
    5│
    6│
        ^

    I was expecting to see a closing square bracket before this, so try
    adding a ] and see if that helps?
    "
    );

    test_report!(
        tag_union_lowercase_tag_name,
        indoc!(
            r"
            f : [lowercase]
            "
        ),
        @r"
    ── WEIRD TAG NAME in tmp/tag_union_lowercase_tag_name/Test.roc ─────────────────

    I am partway through parsing a tag union type, but I got stuck here:

    4│      f : [lowercase]
                 ^

    I was expecting to see a tag name.

    Hint: Tag names start with an uppercase letter, like Err or Green.
    "
    );

    test_report!(
        tag_union_second_lowercase_tag_name,
        indoc!(
            r"
            f : [Good, bad]
            "
        ),
        @r"
    ── WEIRD TAG NAME in tmp/tag_union_second_lowercase_tag_name/Test.roc ──────────

    I am partway through parsing a tag union type, but I got stuck here:

    4│      f : [Good, bad]
                       ^

    I was expecting to see a tag name.

    Hint: Tag names start with an uppercase letter, like Err or Green.
    "
    );

    test_report!(
        record_type_open,
        indoc!(
            r"
            f : {
            "
        ),
        @r"
    ── UNFINISHED RECORD TYPE in tmp/record_type_open/Test.roc ─────────────────────

    I am partway through parsing a record type, but I got stuck here:

    4│      f : {
    5│
    6│
        ^

    I was expecting to see a closing curly brace before this, so try
    adding a } and see if that helps?
    "
    );

    test_report!(
        record_type_open_indent,
        indoc!(
            r"
            f : {
            foo : I64,
            "
        ),
        @r"
    ── UNFINISHED RECORD TYPE in tmp/record_type_open_indent/Test.roc ──────────────

    I am partway through parsing a record type, but I got stuck here:

    4│      f : {
    5│      foo : I64,
    6│
    7│
        ^

    I was expecting to see a closing curly brace before this, so try
    adding a } and see if that helps?
    "
    );

    test_report!(
        record_type_end,
        indoc!(
            r"
            f : { a: Int,
            "
        ),
        @r"
    ── UNFINISHED RECORD TYPE in tmp/record_type_end/Test.roc ──────────────────────

    I am partway through parsing a record type, but I got stuck here:

    4│      f : { a: Int,
    5│
    6│
        ^

    I was expecting to see a closing curly brace before this, so try
    adding a } and see if that helps?
    "
    );

    test_report!(
        record_type_keyword_field_name,
        indoc!(
            r"
            f : { if : I64 }
            "
        ),
        @r"
    ── UNFINISHED RECORD TYPE in tmp/record_type_keyword_field_name/Test.roc ───────

    I just started parsing a record type, but I got stuck on this field
    name:

    4│      f : { if : I64 }
                  ^^

    Looks like you are trying to use `if` as a field name, but that is a
    reserved word. Try using a different name!
    "
    );

    // a case where the message cannot be as good as elm's
    test_report!(
        record_type_missing_comma,
        indoc!(
            r"
            f : { foo  bar }
            "
        ),
        @r"
    ── UNFINISHED RECORD TYPE in tmp/record_type_missing_comma/Test.roc ────────────

    I am partway through parsing a record type, but I got stuck here:

    4│      f : { foo  bar }
                       ^

    I was expecting to see a colon, two question marks (??), comma or
    closing curly brace.
    "
    );

    // a case where the message cannot be as good as elm's
    test_report!(
        record_type_tab,
        "f : { foo \t }",
        @r###"
    ── TAB CHARACTER in tmp/record_type_tab/Test.roc ───────────────────────────────

    I encountered a tab character:

    4│      f : { foo 	 }
                      ^

    Tab characters are not allowed in Roc code. Please use spaces instead!
    "###
    );

    test_report!(
        comment_with_tab,
        "# comment with a \t char\n4",
        @r###"
    ── TAB CHARACTER in tmp/comment_with_tab/Test.roc ──────────────────────────────

    I encountered a tab character:

    4│      # comment with a 	 char
                             ^

    Tab characters are not allowed in Roc code. Please use spaces instead!
    "###
    );

    test_report!(
        comment_with_control_character,
        "# comment with a \x07 char\n",
        @r###"
    ── ASCII CONTROL CHARACTER in tmp/comment_with_control_character/Test.roc ──────

    I encountered an ASCII control character:

    4│      # comment with a  char
                             ^

    ASCII control characters are not allowed.
    "###
    );

    test_report!(
        record_type_carriage_return,
        "f : { \r foo }",
        @r"
    ── MISPLACED CARRIAGE RETURN in tmp/record_type_carriage_return/Test.roc ───────

    I encountered a stray carriage return (\r):

    4│      f : {  foo }
                  ^

    A carriage return (\r) has to be followed by a newline (\n).
    "
    );

    // TODO bad error message
    test_report!(
        type_in_parens_start,
        indoc!(
            r"
            f : (
            "
        ),
        @r"
    ── UNFINISHED PARENTHESES in tmp/type_in_parens_start/Test.roc ─────────────────

    I am partway through parsing a type in parentheses, but I got stuck
    here:

    4│      f : (
    5│
    6│
        ^

    I was expecting to see a closing parenthesis before this, so try
    adding a ) and see if that helps?
    "
    );

    test_report!(
        type_in_parens_end,
        indoc!(
            r"
            f : ( I64
            "
        ),
        @r"
    ── UNFINISHED PARENTHESES in tmp/type_in_parens_end/Test.roc ───────────────────

    I am partway through parsing a type in parentheses, but I got stuck
    here:

    4│      f : ( I64
    5│
    6│
        ^

    I was expecting to see a closing parenthesis before this, so try
    adding a ) and see if that helps?
    "
    );

    test_report!(
        type_apply_double_dot,
        indoc!(
            r"
            f : Foo..Bar

            f
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    I am confused by this type name:

    4│      f : Foo..Bar
                ^^^^^^^^

    Type names start with an uppercase letter, and can optionally be
    qualified by a module name, like Bool or Http.Request.Request.
    "
    );
    //                ── DOUBLE DOT ──────────────────────────────────────────────────────────────────
    //
    //                I encountered two dots in a row:
    //
    //                1│  f : Foo..Bar
    //                            ^
    //
    //                Try removing one of them.

    test_report!(
        type_apply_trailing_dot,
        indoc!(
            r"
            f : Foo.Bar.

            f
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    I am confused by this type name:

    4│      f : Foo.Bar.
                ^^^^^^^^

    Type names start with an uppercase letter, and can optionally be
    qualified by a module name, like Bool or Http.Request.Request.
    "
    );
    //                ── TRAILING DOT ────────────────────────────────────────────────────────────────
    //
    //                I encountered a dot with nothing after it:
    //
    //                1│  f : Foo.Bar.
    //                                ^
    //
    //                Dots are used to refer to a type in a qualified way, like
    //                Num.I64 or List.List a. Try adding a type name next.

    test_report!(
        type_apply_stray_dot,
        indoc!(
            r"
            f : .
            "
        ),
        @r"
    ── UNFINISHED TYPE in tmp/type_apply_stray_dot/Test.roc ────────────────────────

    I just started parsing a type, but I got stuck here:

    4│      f : .
                ^

    I am expecting a type next, like Bool or List a.
    "
    );

    test_report!(
        type_apply_start_with_number,
        indoc!(
            r"
            f : Foo.1

            f
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    I am confused by this type name:

    4│      f : Foo.1
                ^^^^^

    Type names start with an uppercase letter, and can optionally be
    qualified by a module name, like Bool or Http.Request.Request.
    "
    );
    //                ── WEIRD QUALIFIED NAME ────────────────────────────────────────────────────────
    //
    //                I encountered a number at the start of a qualified name segment:
    //
    //                1│  f : Foo.1
    //                            ^
    //
    //                All parts of a qualified type name must start with an uppercase
    //                letter, like Num.I64 or List.List a.

    test_report!(
        type_apply_start_with_lowercase,
        indoc!(
            r"
            f : Foo.foo

            f
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    I am confused by this type name:

    4│      f : Foo.foo
                ^^^^^^^

    Type names start with an uppercase letter, and can optionally be
    qualified by a module name, like Bool or Http.Request.Request.
    "
    );

    // TODO investigate this test. It was disabled in https://github.com/roc-lang/roc/pull/6634
    // as the way Defs without final expressions are handled. The changes probably shouldn't have
    // changed this error report. The exact same test_syntax test for this has not changed, so
    // we know the parser is parsing the same thing. Therefore the way the AST is desugared must be
    // the cause of the change in error report.
    // test_report!(
    //     def_missing_final_expression,
    //     indoc!(
    //         r"
    //         f : Foo.foo
    //         "
    //     ),
    //     @r#"
    // ── MISSING FINAL EXPRESSION in tmp/def_missing_final_expression/Test.roc ───────

    // I am partway through parsing a definition, but I got stuck here:

    // 1│  app "test" provides [main] to "./platform"
    // 2│
    // 3│  main =
    // 4│      f : Foo.foo
    //                    ^

    // This definition is missing a final expression. A nested definition
    // must be followed by either another definition, or an expression

    //     x = 4
    //     y = 2

    //     x + y
    // "#
    // );

    test_report!(
        expression_indentation_end,
        indoc!(
            r"
            f = Foo.foo
            "
        ),
        @r#"
    ── INDENT ENDS AFTER EXPRESSION in tmp/expression_indentation_end/Test.roc ─────

    I am partway through parsing an expression, but I got stuck here:

    1│  app "test" provides [main] to "./platform"
    2│
    3│  main =
    4│      f = Foo.foo
                       ^

    Looks like the indentation ends prematurely here. Did you mean to have
    another expression after this line?
    "#
    );

    test_report!(
        type_inline_alias,
        indoc!(
            r"
            f : I64 as
            f = 0

            f
            "
        ),
        @r"
    ── UNFINISHED INLINE ALIAS in tmp/type_inline_alias/Test.roc ───────────────────

    I just started parsing an inline type alias, but I got stuck here:

    4│      f : I64 as
                      ^

    Note: I may be confused by indentation
    "
    );

    test_report!(
        type_double_comma,
        indoc!(
            r"
            f : I64,,I64 -> I64
            f = 0

            f
            "
        ),
        @r"
    ── DOUBLE COMMA in tmp/type_double_comma/Test.roc ──────────────────────────────

    I just started parsing a function argument type, but I encountered two
    commas in a row:

    4│      f : I64,,I64 -> I64
                    ^

    Try removing one of them.
    "
    );

    test_report!(
        type_argument_no_arrow,
        indoc!(
            r"
            f : I64, I64
            f = 0

            f
            "
        ),
        @r"
    ── UNFINISHED TYPE in tmp/type_argument_no_arrow/Test.roc ──────────────────────

    I am partway through parsing a type, but I got stuck here:

    4│      f : I64, I64
                        ^

    Note: I may be confused by indentation
    "
    );

    // TODO could do better by pointing out we're parsing a function type
    test_report!(
        type_argument_arrow_then_nothing,
        indoc!(
            r"
            f : I64, I64 ->
            f = 0

            f
            "
        ),
        @r"
    ── UNFINISHED TYPE in tmp/type_argument_arrow_then_nothing/Test.roc ────────────

    I just started parsing a type, but I got stuck here:

    4│      f : I64, I64 ->
                           ^

    Note: I may be confused by indentation
    "
    );

    test_report!(
        unfinished_import,
        indoc!(
            r"
            import [
            "
        ),
        @r###"
    ── UNFINISHED IMPORT in tmp/unfinished_import/Test.roc ─────────────────────────

    I was partway through parsing an `import`, but I got stuck here:

    4│      import [
                   ^

    I was expecting to see a module name, like:

        import BigNum

    Or a package module name, like:

        import pf.Stdout

    Or a file path to ingest, like:

        import "users.json" as users : Str
    "###
    );

    test_report!(
        weird_import_params_record,
        indoc!(
            r"
            import Menu { x = 4 }
            "
        ),@r###"
    ── RECORD PARSE PROBLEM in tmp/weird_import_params_record/Test.roc ─────────────

    I am partway through parsing a record, but I got stuck here:

    4│      import Menu { x = 4 }
                        ^

    TODO provide more context.
    "###
    );

    test_report!(
        record_update_in_module_params,
        indoc!(
            r"
            import Menu { my_params & echo: echo_fn }
            "
        ),@r"
    ── RECORD UPDATE IN MODULE PARAMS in ...ecord_update_in_module_params/Test.roc ─

    I was partway through parsing module params, but I got stuck here:

    4│      import Menu { my_params & echo: echo_fn }
                          ^^^^^^^^^

    It looks like you're trying to update a record, but module params
    require a standalone record literal.
    "
    );

    test_report!(
        unfinished_import_as_or_exposing,
        indoc!(
            r"
            import svg.Path a
            "
        ),
        @r###"
    ── UNFINISHED IMPORT in tmp/unfinished_import_as_or_exposing/Test.roc ──────────

    I was partway through parsing an `import`, but I got stuck here:

    4│      import svg.Path a
                           ^

    I was expecting to see the `as` keyword next, like:

        import svg.Path as SvgPath

    Or the `exposing` keyword, like:

        import svg.Path exposing [arc, rx]

    Or module params, like:

        import Menu { echo, read }
    "###
    );

    test_report!(
        unfinished_import_alias,
        indoc!(
            r"
            import svg.Path as
            "
        ),
        @r###"
    ── UNFINISHED IMPORT in tmp/unfinished_import_alias/Test.roc ───────────────────

    I was partway through parsing an `import`, but I got stuck here:

    4│      import svg.Path as
                              ^

    I just saw the `as` keyword, so I was expecting to see an alias next.
    "###
    );

    test_report!(
        lowercase_import_alias,
        indoc!(
            r"
            import svg.Path as path
            "
        ),
        @r###"
    ── LOWERCASE ALIAS in tmp/lowercase_import_alias/Test.roc ──────────────────────

    This import is using a lowercase alias:

    4│      import svg.Path as path
                               ^^^^

    Module names and aliases must start with an uppercase letter.
    "###
    );

    test_report!(
        unfinished_import_exposing,
        indoc!(
            r"
            import svg.Path exposing
            "
        ),
        @r###"
    ── UNFINISHED IMPORT in tmp/unfinished_import_exposing/Test.roc ────────────────

    I was partway through parsing an `import`, but I got stuck here:

    4│      import svg.Path exposing
                                    ^

    I just saw the `exposing` keyword, so I was expecting to see `[` next.
    "###);

    test_report!(
        unfinished_import_exposing_name,
        indoc!(
            r"
            import svg.Path exposing [3
            "
        ),
        @r###"
    ── WEIRD EXPOSING in tmp/unfinished_import_exposing_name/Test.roc ──────────────

    I'm partway through parsing an exposing list, but I got stuck here:

    4│      import svg.Path exposing [3
                                      ^

    I was expecting a type, value, or function name next, like:

        import Svg exposing [Path, arc, rx]
    "###);

    test_report!(
        unfinished_ingested_file_name,
        indoc!(
            r#"
            import "example.json" as
            "#
        ),
        @r###"
    ── UNFINISHED IMPORT in tmp/unfinished_ingested_file_name/Test.roc ─────────────

    I was partway through parsing an `import`, but I got stuck here:

    4│      import "example.json" as
                                    ^

    I was expecting to see a name next, like:

        import "users.json" as users : Str
    "###
    );

    test_report!(
        ingested_file_import_ann_syntax_err,
        indoc!(
            r#"
            import "example.json" as example : List U8, U32
            "#
        ),
        @r###"
    ── UNFINISHED TYPE in tmp/ingested_file_import_ann_syntax_err/Test.roc ─────────

    I am partway through parsing a type, but I got stuck here:

    4│      import "example.json" as example : List U8, U32
                                                           ^

    Note: I may be confused by indentation
    "###
    );

    // TODO could do better by pointing out we're parsing a function type
    test_report!(
        dict_type_formatting,
        indoc!(
            r#"
            app "dict" imports [] provides [main] to "./platform"

            my_dict : Dict Num.I64 Str
            my_dict = Dict.insert (Dict.empty {}) "foo" 42

            main = my_dict
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `my_dict` definition:

    3│  my_dict : Dict Num.I64 Str
    4│  my_dict = Dict.insert (Dict.empty {}) "foo" 42
                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    This `insert` call produces:

        Dict Str (Num *)

    But the type annotation on `my_dict` says it should be:

        Dict I64 Str
    "#
    );

    test_report!(
        alias_type_diff,
        indoc!(
            r#"
            app "test" imports [] provides [main] to "./platform"

            HSet a : Set a

            foo : Str -> HSet {}

            my_dict : HSet Str
            my_dict = foo "bar"

            main = my_dict
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `my_dict` definition:

    7│  my_dict : HSet Str
    8│  my_dict = foo "bar"
                  ^^^^^^^^^

    This `foo` call produces:

        HSet {}

    But the type annotation on `my_dict` says it should be:

        HSet Str
    "#
    );

    // this should get better with time
    test_report!(
        if_guard_without_condition,
        indoc!(
            r"
            when Just 4 is
                Just if ->
                    4

                _ ->
                    2
            "
        ),
        @r"
    ── IF GUARD NO CONDITION in tmp/if_guard_without_condition/Test.roc ────────────

    I just started parsing an if guard, but there is no guard condition:

    4│      when Just 4 is
    5│          Just if ->
                        ^

    Try adding an expression before the arrow!
    "
    );

    test_report!(
        empty_or_pattern,
        indoc!(
            r"
            when Just 4 is
                Just 4 | ->
                    4

                _ ->
                    2
            "
        ),
        @r"
    ── UNFINISHED PATTERN in tmp/empty_or_pattern/Test.roc ─────────────────────────

    I just started parsing a pattern, but I got stuck here:

    5│          Just 4 | ->
                         ^

    Note: I may be confused by indentation
    "
    );

    // TODO check if "what_is_next" is a keyword
    test_report!(
        pattern_binds_keyword,
        indoc!(
            r"
            when Just 4 is
                Just when ->
                    4

                _ ->
                    2
            "
        ),
        @r"
    ── MISSING ARROW in tmp/pattern_binds_keyword/Test.roc ─────────────────────────

    I am partway through parsing a `when` expression, but got stuck here:

    4│      when Just 4 is
    5│          Just when ->
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
    "
    );

    // this should get better with time
    test_report!(
        when_missing_arrow,
        indoc!(
            r"
            when 5 is
                1 -> 2
                _
            "
        ),
        @r"
    ── UNFINISHED WHEN in tmp/when_missing_arrow/Test.roc ──────────────────────────

    I was partway through parsing a `when` expression, but I got stuck here:

    4│      when 5 is
    5│          1 -> 2
    6│          _
                 ^

    I was expecting to see a pattern next

    Note: Here is an example of a valid `when` expression for reference.

        when List.first plants is
          Ok n ->
            n

          Err _ ->
            200

    Notice the indentation. All patterns are aligned, and each branch is
    indented a bit more than the corresponding pattern. That is important!
    "
    );

    test_report!(
        lambda_double_comma,
        indoc!(
            r"
            \a,,b -> 1
            "
        ),
        @r"
    ── UNFINISHED ARGUMENT LIST in tmp/lambda_double_comma/Test.roc ────────────────

    I am partway through parsing a function argument list, but I got stuck
    at this comma:

    4│      \a,,b -> 1
               ^

    I was expecting an argument pattern before this, so try adding an
    argument before the comma and see if that helps?
    "
    );

    test_report!(
        lambda_leading_comma,
        indoc!(
            r"
            \,b -> 1
            "
        ),
        @r"
    ── UNFINISHED ARGUMENT LIST in tmp/lambda_leading_comma/Test.roc ───────────────

    I am partway through parsing a function argument list, but I got stuck
    at this comma:

    4│      \,b -> 1
             ^

    I was expecting an argument pattern before this, so try adding an
    argument before the comma and see if that helps?
    "
    );

    // this should get better with time
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
    test_report!(
        when_outdented_branch,
        indoc!(
            r"
            when 4 is
                5 -> 2
             2 -> 2
            "
        ),
        @r###"
    ── UNKNOWN OPERATOR in tmp/when_outdented_branch/Test.roc ──────────────────────

    This looks like an operator, but it's not one I recognize!

    1│  app "test" provides [main] to "./platform"
    2│
    3│  main =
    4│      when 4 is
    5│          5 -> 2
    6│       2 -> 2
               ^^

    Looks like you are trying to define a function. 

    In Roc, functions are always written as a lambda, like 

        increment = \n -> n + 1
    "###
    );

    test_report!(
        when_over_indented_underscore,
        indoc!(
            r"
            when 4 is
                5 -> 2
                 _ -> 2
            "
        ),
        @r###"
    ── UNEXPECTED ARROW in tmp/when_over_indented_underscore/Test.roc ──────────────

    I am parsing a `when` expression right now, but this arrow is confusing
    me:

    4│      when 4 is
    5│          5 -> 2
    6│           _ -> 2
                   ^^

    It makes sense to see arrows around here, so I suspect it is something
    earlier. Maybe this pattern is indented a bit farther from the
    previous patterns?

    Note: Here is an example of a valid `when` expression for reference.

        when List.first plants is
          Ok n ->
            n

          Err _ ->
            200

    Notice the indentation. All patterns are aligned, and each branch is
    indented a bit more than the corresponding pattern. That is important!
    "###
    );

    test_report!(
        when_over_indented_int,
        indoc!(
            r"
            when 4 is
                5 -> Num.neg
                 2 -> 2
            "
        ),
        @r###"
    ── UNEXPECTED ARROW in tmp/when_over_indented_int/Test.roc ─────────────────────

    I am parsing a `when` expression right now, but this arrow is confusing
    me:

    4│      when 4 is
    5│          5 -> Num.neg
    6│           2 -> 2
                   ^^

    It makes sense to see arrows around here, so I suspect it is something
    earlier. Maybe this pattern is indented a bit farther from the
    previous patterns?

    Note: Here is an example of a valid `when` expression for reference.

        when List.first plants is
          Ok n ->
            n

          Err _ ->
            200

    Notice the indentation. All patterns are aligned, and each branch is
    indented a bit more than the corresponding pattern. That is important!
    "###
    );

    // TODO I think we can do better here
    test_report!(
        if_outdented_then,
        indoc!(
            r"
            x =
                if 5 == 5
            then 2 else 3

            x
            "
        ),
        @r"
    ── UNFINISHED IF in tmp/if_outdented_then/Test.roc ─────────────────────────────

    I was partway through parsing an `if` expression, but I got stuck here:

    5│          if 5 == 5
                         ^

    I was expecting to see the `then` keyword next.
    "
    );

    // this should get better with time
    test_report!(
        if_missing_else,
        indoc!(
            r"
            if 5 == 5 then 2
            "
        ),
        @r"
    ── UNFINISHED IF in tmp/if_missing_else/Test.roc ───────────────────────────────

    I was partway through parsing an `if` expression, but I got stuck here:

    4│      if 5 == 5 then 2
                            ^

    I was expecting to see the `else` keyword next.
    "
    );

    test_report!(
        list_double_comma,
        indoc!(
            r"
            [1, 2, , 3]
            "
        ),
        @r"
    ── UNFINISHED LIST in tmp/list_double_comma/Test.roc ───────────────────────────

    I am partway through started parsing a list, but I got stuck here:

    4│      [1, 2, , 3]
                   ^

    I was expecting to see a list entry before this comma, so try adding a
    list entry and see if that helps?
    "
    );

    test_report!(
        list_without_end,
        indoc!(
            r"
            [1, 2,
            "
        ),
        @r"
    ── UNFINISHED LIST in tmp/list_without_end/Test.roc ────────────────────────────

    I am partway through started parsing a list, but I got stuck here:

    4│      [1, 2,
    5│
    6│
        ^

    I was expecting to see a closing square bracket before this, so try
    adding a ] and see if that helps?

    Note: When I get stuck like this, it usually means that there is a
    missing parenthesis or bracket somewhere earlier. It could also be a
    stray keyword or operator.
    "
    );

    test_report!(
        missing_return_expression,
        indoc!(
            r"
            return
            "
        ),
        @r#"
    ── MISSING EXPRESSION in tmp/missing_return_expression/Test.roc ────────────────

    I am partway through parsing a return statement, but I got stuck here:

    4│      return
                  ^

    I was expecting to see an expression like 42 or "hello".
    "#
    );

    test_report!(
        return_as_def_name,
        indoc!(
            r"
            return = \a -> a
            return
            "
        ),
        @r#"
    ── MISSING EXPRESSION in tmp/return_as_def_name/Test.roc ───────────────────────

    I am partway through parsing a return statement, but I got stuck here:

    4│      return = \a -> a
                   ^

    I was expecting to see an expression like 42 or "hello".
    "#
    );

    test_report!(
        return_space_problem,
        "return \t",
        @r###"
    ── TAB CHARACTER in tmp/return_space_problem/Test.roc ──────────────────────────

    I encountered a tab character:

    4│      return 	
                   ^

    Tab characters are not allowed in Roc code. Please use spaces instead!
    "###
    );

    test_report!(
        number_double_dot,
        indoc!(
            r"
            1.1.1
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This float literal contains an invalid digit:

    4│      1.1.1
            ^^^^^

    Floating point literals can only contain the digits 0-9, or use
    scientific notation 10e4, or have a float suffix.

    Tip: Learn more about number literals at TODO
    "
    );

    test_report!(
        unicode_not_hex,
        r#""abc\u(zzzz)def""#,
        @r#"
    ── WEIRD CODE POINT in tmp/unicode_not_hex/Test.roc ────────────────────────────

    I am partway through parsing a unicode code point, but I got stuck
    here:

    4│      "abc\u(zzzz)def"
                   ^

    I was expecting a hexadecimal number, like \u(1100) or \u(00FF).

    Learn more about working with unicode in roc at TODO
    "#
    );

    test_report!(
        unicode_too_large,
        r#""abc\u(110000)def""#,
        @r#"
    ── INVALID UNICODE in /code/proj/Main.roc ──────────────────────────────────────

    This unicode code point is invalid:

    4│      "abc\u(110000)def"
                   ^^^^^^

    Learn more about working with unicode in roc at TODO
    "#
    );

    test_report!(
        weird_escape,
        r#""abc\qdef""#,
        @r#"
    ── WEIRD ESCAPE in tmp/weird_escape/Test.roc ───────────────────────────────────

    I was partway through parsing a  string literal, but I got stuck here:

    4│      "abc\qdef"
                ^^

    This is not an escape sequence I recognize. After a backslash, I am
    looking for one of these:

        - A newline: \n
        - A caret return: \r
        - A tab: \t
        - An escaped quote: \"
        - An escaped backslash: \\
        - A unicode code point: \u(00FF)
    "#
    );

    test_report!(
        single_quote_too_long,
        r"'abcdef'",
        @r#"
    ── INVALID SCALAR in tmp/single_quote_too_long/Test.roc ────────────────────────

    I am part way through parsing this scalar literal (character literal),
    but it's too long to fit in a U32 so it's not a valid scalar.

    4│      'abcdef'
             ^

    You could change it to something like 'a' or '\n'. Note, roc strings
    use double quotes, like "hello".
    "#
    );

    test_report!(
        single_no_end,
        r#""there is no end"#,
        @r#"
    ── ENDLESS STRING in tmp/single_no_end/Test.roc ────────────────────────────────

    I cannot find the end of this string:

    4│      "there is no end
             ^

    You could change it to something like "to be or not to be" or even
    just "".
    "#
    );

    test_report!(
        multi_no_end,
        r#""""there is no end"#,
        @r#"
    ── ENDLESS STRING in tmp/multi_no_end/Test.roc ─────────────────────────────────

    I cannot find the end of this block string:

    4│      """there is no end
               ^

    You could change it to something like """to be or not to be""" or even
    just """""".
    "#
    );

    test_report!(
        multi_insufficient_indent,
        "    \"\"\"\n  testing\n    \"\"\"", // 4 space indent on the start, 2 space on the `testing` line
        @r#"
    ── INSUFFICIENT INDENT IN MULTI-LINE STRING in ...insufficient_indent/Test.roc ─

    This multiline string is not sufficiently indented:

    4│          """
    5│        testing
              ^

    Lines in a multi-line string must be indented at least as much as the
    beginning """. This extra indentation is automatically removed from
    the string during compilation.
    "#
    );

    test_report!(
        expect_without_final_expression,
        indoc!(
            r"
            expect 1 + 1 == 2
            "
        ),
        @r#"
    ── INDENT ENDS AFTER EXPRESSION in ...expect_without_final_expression/Test.roc ─

    I am partway through parsing an expect statement, but I got stuck
    here:

    4│      expect 1 + 1 == 2
                             ^

    I was expecting a final expression, like so

        expect 1 + 1 == 2
        "done"
    "#
    );

    test_report!(
        unhandled_parse_error,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            42
            "#
        ),
        @r#"
    ── UNHANDLED PARSE ERROR in tmp/unhandled_parse_error/Test.roc ─────────────────

    I got stuck while parsing this:

    1│  app "test" provides [main] to "./platform"
    2│
    3│  42
        ^

    Here's the internal parse problem:

        UnexpectedTopLevelExpr(@44)

    Unfortunately, I'm not able to provide a more insightful error message
    for this syntax problem yet. This is considered a bug in the compiler.

    Note: If you'd like to contribute to Roc, this would be a good first issue!
    "#
    );

    // https://github.com/roc-lang/roc/issues/1714
    test_report!(
    interpolate_concat_is_transparent_1714,
            indoc!(
                r#"
            greeting = "Privet"

            if Bool.true then 1 else "${greeting}, World!"
            "#,
            ),
            @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This `if` has an `else` branch with a different type from its `then` branch:

    6│      if Bool.true then 1 else "${greeting}, World!"
                                     ^^^^^^^^^^^^^^^^^^^^^

    The `else` branch is a string of type:

        Str

    but the `then` branch has the type:

        Num *

    All branches in an `if` must have the same type!
    "#
        );

    macro_rules! comparison_binop_transparency_tests {
        ($($op:expr, $name:ident),* $(,)?) => {
            $(
            test_report!(
                $name,
                &format!(r#"if Bool.true then "abc" else 1 {} 2"#, $op),
                |golden| assert_eq!(golden, format!(
r#"── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

This `if` has an `else` branch with a different type from its `then` branch:

4│      if Bool.true then "abc" else 1 {} 2
                                     ^^{}^^

This comparison produces:

    Bool

but the `then` branch has the type:

    Str

All branches in an `if` must have the same type!
"#,
                    $op, "^".repeat($op.len())
                ))
            );
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

    test_report!(
        keyword_record_field_access,
        indoc!(
            r"
            foo = {}

            foo.if
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This `foo` record doesn’t have a `if` field:

    6│      foo.if
            ^^^^^^

    In fact, `foo` is a record with no fields at all!
    "
    );

    test_report!(
        keyword_qualified_import,
        indoc!(
            r"
            Num.if
            "
        ),
        @r###"
    ── NOT EXPOSED in /code/proj/Main.roc ──────────────────────────────────────────

    The Num module does not expose `if`:

    4│      Num.if
            ^^^^^^

    Did you mean one of these?

        Num.sin
        Num.div
        Num.e
        Num.pi
    "###
    );

    test_report!(
        stray_dot_expr,
        indoc!(
            r"
            Num.add . 23
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    I am trying to parse a record field access here:

    4│      Num.add . 23
                     ^

    So I expect to see a lowercase letter next, like .name or .height.
    "
    );

    test_report!(
        opaque_ref_field_access,
        indoc!(
            r"
            @UUID.bar
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    I am very confused by this field access:

    4│      @UUID.bar
                 ^^^^

    It looks like a record field access on an opaque reference.
    "
    );

    test_report!(
        weird_accessor,
        indoc!(
            r"
            .foo.bar
            "
        ),
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    I am very confused by this field access

    4│      .foo.bar
            ^^^^^^^^

    It looks like a field access on an accessor. I parse.client.name as
    (.client).name. Maybe use an anonymous function like
    (\r -> r.client.name) instead?
    "
    );

    test_report!(
        #[ignore]
        double_binop,
        indoc!(
            r"
            key >= 97 and <= 122
            "
        ),
        @r"
        "
    );

    test_report!(
        #[ignore]
        case_of,
        indoc!(
            r"
            case 1 of
                1 -> True
                _ -> False
            "
        ),
        @r#"
    ── UNKNOWN OPERATOR in tmp/case_of/Test.roc ────────────────────────────────────

    This looks like an operator, but it's not one I recognize!

    1│  app "test" provides [main] to "./platform"
    2│
    3│  main =
    4│      case 1 of
    5│          1 -> True
                  ^^

    The arrow -> is used to define cases in a `when` expression:

        when color is
            Red -> "stop!"
            Green -> "go!"

    And to define a function:

        increment : I64 -> I64
        increment = \n -> n + 1

    "#
    );

    test_report!(
        argument_without_space,
        indoc!(
            r#"
            ["foo", bar("")]
            "#
        ),
        @r###"
    ── UNRECOGNIZED NAME in /code/proj/Main.roc ────────────────────────────────────

    Nothing is named `bar` in this scope.

    4│      ["foo", bar("")]
                    ^^^

    Did you mean one of these?

        Str
        U8
        F64
        Box
    "###
    );

    test_report!(
        invalid_operator,
        indoc!(
            r"
            main =
                5 ** 3
            "
        ),
        @r#"
    ── UNKNOWN OPERATOR in tmp/invalid_operator/Test.roc ───────────────────────────

    This looks like an operator, but it's not one I recognize!

    1│  app "test" provides [main] to "./platform"
    2│
    3│  main =
    4│      main =
    5│          5 ** 3
                  ^^

    I have no specific suggestion for this operator, see
    https://www.roc-lang.org/tutorial#operator-desugaring-table for the
    full list of operators in Roc.
    "#
    );

    test_report!(
        double_plus,
        indoc!(
            r"
            main =
                [] ++ []
            "
        ),
        @r#"
    ── UNKNOWN OPERATOR in tmp/double_plus/Test.roc ────────────────────────────────

    This looks like an operator, but it's not one I recognize!

    1│  app "test" provides [main] to "./platform"
    2│
    3│  main =
    4│      main =
    5│          [] ++ []
                   ^^

    To concatenate two lists or strings, try using List.concat or
    Str.concat instead.
    "#
    );

    test_report!(
        inline_hastype,
        indoc!(
            r"
            main =
                (\x -> x) : I64

                3
            "
        ),
        @r#"
    ── UNKNOWN OPERATOR in tmp/inline_hastype/Test.roc ─────────────────────────────

    This looks like an operator, but it's not one I recognize!

    1│  app "test" provides [main] to "./platform"
    2│
    3│  main =
    4│      main =
    5│          (\x -> x) : I64
                          ^

    The has-type operator : can only occur in a definition's type
    signature, like

        increment : I64 -> I64
        increment = \x -> x + 1
    "#
    );

    // this is still bad, but changing the order and progress of other parsers should improve it
    // down the line
    test_report!(
        wild_case_arrow,
        indoc!(
            r"
            main = 5 -> 3
            "
        ),
        @r###"
    ── SYNTAX PROBLEM in tmp/wild_case_arrow/Test.roc ──────────────────────────────

    I got stuck here:

    1│  app "test" provides [main] to "./platform"
    2│
    3│  main =
    4│      main = 5 -> 3
                    ^

    Whatever I am running into is confusing me a lot! Normally I can give
    fairly specific hints, but something is really tripping me up this
    time.
    "###
    );

    #[test]
    fn provides_to_identifier() {
        report_header_problem_as(
            indoc!(
                r#"
                app "test-base64"
                    packages { pf: "platform/main.roc" }
                    imports [pf.Task, Base64]
                    provides [main, @Foo] to pf
                "#
            ),
            indoc!(
                r"
                ── WEIRD PROVIDES in /code/proj/Main.roc ───────────────────────────────────────

                I am partway through parsing a provides list, but I got stuck here:

                3│      imports [pf.Task, Base64]
                4│      provides [main, @Foo] to pf
                                        ^

                I was expecting a type name, value name or function name next, like

                    provides [Animal, default, tame]
            "
            ),
        )
    }

    #[test]
    fn missing_provides_in_app_header() {
        report_header_problem_as(
            indoc!(
                r#"
                app "broken"
                    packages {
                        pf: "generic-test-platform/main.roc",
                    }
                    imports [
                        pf.Stdout,
                    ]

                main =
                    Stdout.line "answer"
                "#
            ),
            indoc!(
                r"
                ── WEIRD PROVIDES in /code/proj/Main.roc ───────────────────────────────────────

                I am partway through parsing a header, but I got stuck here:

                7│      ]
                         ^

                I am expecting the `provides` keyword next, like

                    provides [Animal, default, tame]
            "
            ),
        )
    }

    #[test]
    fn provides_missing_to_in_app_header() {
        report_header_problem_as(
            indoc!(
                r#"
                app "broken"
                    provides [main]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD PROVIDES in /code/proj/Main.roc ───────────────────────────────────────

                I am partway through parsing a header, but I got stuck here:

                1│  app "broken"
                2│      provides [main]
                                       ^

                I am expecting the `to` keyword next, like:

                    to pf
                "#
            ),
        )
    }

    #[test]
    fn provides_to_missing_platform_in_app_header() {
        report_header_problem_as(
            indoc!(
                r#"
                app "broken"
                    provides [main] to
                "#
            ),
            indoc!(
                r#"
                ── WEIRD PROVIDES in /code/proj/Main.roc ───────────────────────────────────────

                I am partway through parsing a header, but I got stuck here:

                1│  app "broken"
                2│      provides [main] to
                                          ^

                I am expecting the platform name next, like:

                    to pf
                "#
            ),
        )
    }

    #[test]
    fn module_params_with_missing_arrow() {
        report_header_problem_as(
            indoc!(
                r#"
                module {echo, read} [menu]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD MODULE PARAMS in /code/proj/Main.roc ──────────────────────────────────

                I am partway through parsing a module header, but I got stuck here:

                1│  module {echo, read} [menu]
                                        ^

                I am expecting `->` next, like:

                    module { echo, read } -> [menu]
                "#
            ),
        )
    }

    // TODO: this test seems out of date (what is the `effects` clause?) and as such should be removed
    #[test]
    fn platform_requires_rigids() {
        report_header_problem_as(
            indoc!(
                r#"
                platform "folkertdev/foo"
                    requires { main! : {} => Result {} [] }
                    exposes []
                    packages {}
                    imports []
                    provides [main_for_host]
                    effects fx.Effect
                         {
                             put_char : I64 -> Effect {},
                             put_line : Str -> Effect {},
                             get_line : Effect Str
                         }
                "#
            ),
            indoc!(
                r#"
                ── BAD REQUIRES in /code/proj/Main.roc ─────────────────────────────────────────

                I am partway through parsing a header, but I got stuck here:

                1│  platform "folkertdev/foo"
                2│      requires { main! : {} => Result {} [] }
                                   ^

                I am expecting a list of type names like `{}` or `{ Model }` next. A full
                `requires` definition looks like

                    requires { Model, Msg } { main! : {} => Result {} [] }
            "#
            ),
        )
    }

    #[test]
    fn missing_imports() {
        report_header_problem_as(
            indoc!(
                r"
                interface Foobar
                    exposes [main, Foo]
                "
            ),
            indoc!(
                r"
                ── WEIRD IMPORTS in /code/proj/Main.roc ────────────────────────────────────────

                I am partway through parsing a header, but I got stuck here:

                2│      exposes [main, Foo]
                                           ^

                I am expecting the `imports` keyword next, like

                    imports [Animal, default, tame]
                "
            ),
        )
    }

    #[test]
    fn exposes_identifier() {
        report_header_problem_as(
            indoc!(
                r"
                module [main, @Foo]
                "
            ),
            indoc!(
                r"
                ── WEIRD EXPOSES in /code/proj/Main.roc ────────────────────────────────────────

                I am partway through parsing an `exposes` list, but I got stuck here:

                1│  module [main, @Foo]
                                  ^

                I was expecting a type name, value name or function name next, like

                    [Animal, default, tame]
            "
            ),
        )
    }

    #[test]
    fn exposes_start() {
        report_header_problem_as(
            indoc!(
                r"
                module foobar []
                "
            ),
            indoc!(
                r#"
                ── WEIRD EXPOSES in /code/proj/Main.roc ────────────────────────────────────────

                I am partway through parsing a header, but I got stuck here:

                1│  module foobar []
                           ^

                I was expecting an `exposes` list like

                    [Animal, default, tame]
            "#
            ),
        )
    }

    #[test]
    fn exposes_missing_comma() {
        report_header_problem_as(
            indoc!(
                r"
                module [value func]
                "
            ),
            indoc!(
                r#"
                ── WEIRD EXPOSES in /code/proj/Main.roc ────────────────────────────────────────

                I am partway through parsing an `exposes` list, but I got stuck here:

                1│  module [value func]
                                  ^

                I was expecting a type name, value name or function name next, like

                    [Animal, default, tame]
            "#
            ),
        )
    }

    #[test]
    fn exposes_end() {
        report_header_problem_as(
            indoc!(
                r"
                module [value
                "
            ),
            indoc!(
                r#"
                ── WEIRD EXPOSES in /code/proj/Main.roc ────────────────────────────────────────

                I am partway through parsing an `exposes` list, but I got stuck here:

                1│  module [value
                2│
                    ^

                I was expecting a type name, value name or function name next, like

                    [Animal, default, tame]
            "#
            ),
        )
    }

    #[test]
    fn invalid_app_name() {
        report_header_problem_as(
            indoc!(
                r"
                app foobar
                    exposes [main, @Foo]
                    imports [pf.Task, Base64]
                "
            ),
            indoc!(
                r#"
                ── WEIRD APP NAME in /code/proj/Main.roc ───────────────────────────────────────

                I am partway through parsing a header, but got stuck here:

                1│  app foobar
                        ^

                I am expecting an application name next, like app "main" or
                app "editor". App names are surrounded by quotation marks.
            "#
            ),
        )
    }

    test_report!(
        apply_unary_negative,
        indoc!(
            r"
            foo = 3

            -foo 1 2
            "
        ),
        @r"
    ── TOO MANY ARGS in /code/proj/Main.roc ────────────────────────────────────────

    This value is not a function, but it was given 2 arguments:

    6│      -foo 1 2
            ^^^^

    Are there any missing commas? Or missing parentheses?
    "
    );

    test_report!(
        apply_unary_not,
        indoc!(
            r"
            foo = Bool.true

            !foo 1 2
            "
        ),
        @r"
    ── TOO MANY ARGS in /code/proj/Main.roc ────────────────────────────────────────

    This value is not a function, but it was given 2 arguments:

    6│      !foo 1 2
            ^^^^

    Are there any missing commas? Or missing parentheses?
    "
    );

    test_report!(
        applied_tag_function,
        indoc!(
            r"
            x : List [Foo Str]
            x = List.map [1, 2] Foo

            x
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `x` definition:

    4│      x : List [Foo Str]
    5│      x = List.map [1, 2] Foo
                ^^^^^^^^^^^^^^^^^^^

    This `map` call produces:

        List [Foo (Num *)]

    But the type annotation on `x` says it should be:

        List [Foo Str]
    "
    );

    test_report!(
        pattern_in_parens_open,
        indoc!(
            r"
            \( a
            "
        ),
        @r"
    ── UNFINISHED PARENTHESES in tmp/pattern_in_parens_open/Test.roc ───────────────

    I am partway through parsing a pattern in parentheses, but I got stuck
    here:

    4│      \( a
    5│
    6│
        ^

    I was expecting to see a closing parenthesis before this, so try
    adding a ) and see if that helps?
    "
    );

    test_report!(
        pattern_in_parens_end_comma,
        indoc!(
            r"
            \( a,
            "
        ),
        @r"
    ── UNFINISHED PARENTHESES in tmp/pattern_in_parens_end_comma/Test.roc ──────────

    I am partway through parsing a pattern in parentheses, but I got stuck
    here:

    4│      \( a,
    5│
    6│
        ^

    I was expecting to see a closing parenthesis before this, so try
    adding a ) and see if that helps?
    "
    );

    test_report!(
        pattern_in_parens_end,
        indoc!(
            r"
            \( a
            "
        ),
        @r"
    ── UNFINISHED PARENTHESES in tmp/pattern_in_parens_end/Test.roc ────────────────

    I am partway through parsing a pattern in parentheses, but I got stuck
    here:

    4│      \( a
    5│
    6│
        ^

    I was expecting to see a closing parenthesis before this, so try
    adding a ) and see if that helps?
    "
    );

    test_report!(
        unfinished_closure_pattern_in_parens,
        indoc!(
            r"
            x = \( a
            )
            "
        ),
        @r###"
    ── MISSING ARROW in tmp/unfinished_closure_pattern_in_parens/Test.roc ──────────

    I am partway through parsing a function argument list, but I got stuck
    here:

    4│      x = \( a
    5│      )
    6│
    7│
        ^

    I was expecting a -> next.
    "###
    );

    test_report!(
        pattern_in_parens_indent_open,
        indoc!(
            r"
            \(
            "
        ),
        @r"
    ── UNFINISHED PARENTHESES in tmp/pattern_in_parens_indent_open/Test.roc ────────

    I am partway through parsing a pattern in parentheses, but I got stuck
    here:

    4│      \(
    5│
    6│
        ^

    I was expecting to see a closing parenthesis before this, so try
    adding a ) and see if that helps?
    "
    );

    test_report!(
        expect_expr_type_error,
        indoc!(
            r#"
            expect "foobar"

            4
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This `expect` condition needs to be a Bool:

    4│      expect "foobar"
                   ^^^^^^^^

    Right now it’s a string of type:

        Str

    But I need every `expect` condition to evaluate to a Bool—either
    `Bool.true` or `Bool.false`.
    "#
    );

    test_report!(
        num_too_general_wildcard,
        indoc!(
            r"
            mult : Num.Num *, Num.F64 -> Num.F64
            mult = \a, b -> a * b

            mult 0 0
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to * has an unexpected type:

    5│      mult = \a, b -> a * b
                                ^

    This `b` value is a:

        F64

    But * needs its 2nd argument to be:

        Num *

    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `mult` definition:

    4│      mult : Num.Num *, Num.F64 -> Num.F64
    5│      mult = \a, b -> a * b
                            ^^^^^

    This `mul` call produces:

        Num *

    But the type annotation on `mult` says it should be:

        F64
    "
    );

    test_report!(
        num_too_general_named,
        indoc!(
            r"
            mult : Num.Num a, Num.F64 -> Num.F64
            mult = \a, b -> a * b

            mult 0 0
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to * has an unexpected type:

    5│      mult = \a, b -> a * b
                                ^

    This `b` value is a:

        F64

    But * needs its 2nd argument to be:

        Num a

    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `mult` definition:

    4│      mult : Num.Num a, Num.F64 -> Num.F64
    5│      mult = \a, b -> a * b
                            ^^^^^

    This `mul` call produces:

        Num a

    But the type annotation on `mult` says it should be:

        F64
    "
    );

    test_report!(
        inference_var_not_enough_in_alias,
        indoc!(
            r#"
            Result a b : [Ok a, Err b]

            can_i_go : _ -> Result _
            can_i_go = \color ->
                when color is
                    "green" -> Ok "go!"
                    "yellow" -> Err (SlowIt "whoa, let's slow down!")
                    "red" -> Err (StopIt "absolutely not")
                    _ -> Err (UnknownColor "this is a weird stoplight")
            can_i_go
            "#
        ),
        @r"
    ── DUPLICATE NAME in /code/proj/Main.roc ───────────────────────────────────────

    This alias has the same name as a builtin:

    4│      Result a b : [Ok a, Err b]
            ^^^^^^^^^^^^^^^^^^^^^^^^^^

    All builtin aliases are in scope by default, so I need this alias to
    have a different name!

    ── TOO FEW TYPE ARGUMENTS in /code/proj/Main.roc ───────────────────────────────

    The `Result` alias expects 2 type arguments, but it got 1 instead:

    6│      can_i_go : _ -> Result _
                            ^^^^^^^^

    Are there missing parentheses?
    "
    );

    test_report!(
        inference_var_too_many_in_alias,
        indoc!(
            r#"
            Result a b : [Ok a, Err b]

            can_i_go : _ -> Result _ _ _
            can_i_go = \color ->
                when color is
                    "green" -> Ok "go!"
                    "yellow" -> Err (SlowIt "whoa, let's slow down!")
                    "red" -> Err (StopIt "absolutely not")
                    _ -> Err (UnknownColor "this is a weird stoplight")
            can_i_go
            "#
        ),
        @r"
    ── DUPLICATE NAME in /code/proj/Main.roc ───────────────────────────────────────

    This alias has the same name as a builtin:

    4│      Result a b : [Ok a, Err b]
            ^^^^^^^^^^^^^^^^^^^^^^^^^^

    All builtin aliases are in scope by default, so I need this alias to
    have a different name!

    ── TOO MANY TYPE ARGUMENTS in /code/proj/Main.roc ──────────────────────────────

    The `Result` alias expects 2 type arguments, but it got 3 instead:

    6│      can_i_go : _ -> Result _ _ _
                            ^^^^^^^^^^^^

    Are there missing parentheses?
    "
    );

    test_report!(
        inference_var_conflict_in_rigid_links,
        indoc!(
            r"
            f : a -> (_ -> b) where a implements Eq
            f = \x -> \y -> if x == y then x else y
            f
            "
        ),
        // TODO: We should tell the user that we inferred `_` as `a`
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    4│      f : a -> (_ -> b) where a implements Eq
    5│      f = \x -> \y -> if x == y then x else y
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The body is an anonymous function of type:

        a -> a where a implements Eq, a implements Eq

    But the type annotation on `f` says it should be:

        a -> b where a implements Eq

    Tip: Your type annotation uses `b` and `a` as separate type variables.
    Your code seems to be saying they are the same though. Maybe they
    should be the same in your type annotation? Maybe your code uses them
    in a weird way?
    "
    );

    test_report!(
        error_wildcards_are_related,
        indoc!(
            r"
            f : * -> *
            f = \x -> x

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    4│      f : * -> *
    5│      f = \x -> x
                      ^

    The type annotation on `f` says this `x` value should have the type:

        *

    However, the type of this `x` value is connected to another type in a
    way that isn't reflected in this annotation.

    Tip: Any connection between types must use a named type variable, not
    a `*`! Maybe the annotation  on `f` should have a named type variable in
    place of the `*`?
    "
    );

    test_report!(
        error_nested_wildcards_are_related,
        indoc!(
            r"
            f : a, b, * -> {x: a, y: b, z: *}
            f = \x, y, z -> {x, y, z}

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    4│      f : a, b, * -> {x: a, y: b, z: *}
    5│      f = \x, y, z -> {x, y, z}
                            ^^^^^^^^^

    The type annotation on `f` says the body is a record should have the
    type:

        {
            x : a,
            y : b,
            z : *,
        }

    However, the type of the body is a record is connected to another type
    in a way that isn't reflected in this annotation.

    Tip: Any connection between types must use a named type variable, not
    a `*`! Maybe the annotation  on `f` should have a named type variable in
    place of the `*`?
    "
    );

    test_report!(
        error_wildcards_are_related_in_nested_defs,
        indoc!(
            r"
            f : a, b, * -> *
            f = \_, _, x2 ->
                inner : * -> *
                inner = \y -> y
                inner x2

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `inner` definition:

    6│          inner : * -> *
    7│          inner = \y -> y
                              ^

    The type annotation on `inner` says this `y` value should have the type:

        *

    However, the type of this `y` value is connected to another type in a
    way that isn't reflected in this annotation.

    Tip: Any connection between types must use a named type variable, not
    a `*`! Maybe the annotation  on `inner` should have a named type variable
    in place of the `*`?
    "
    );

    test_report!(
        error_inline_alias_not_an_alias,
        indoc!(
            r"
            f : List elem -> [Nil, Cons elem a] as a
            "
        ),
        @r"
    ── NOT AN INLINE ALIAS in tmp/error_inline_alias_not_an_alias/Test.roc ─────────

    The inline type after this `as` is not a type alias:

    4│      f : List elem -> [Nil, Cons elem a] as a
                                                   ^

    Inline alias types must start with an uppercase identifier and be
    followed by zero or more type arguments, like Point or List a.
    "
    );

    test_report!(
        error_inline_alias_qualified,
        indoc!(
            r"
            f : List elem -> [Nil, Cons elem a] as Module.LinkedList a
            "
        ),
        @r"
    ── QUALIFIED ALIAS NAME in tmp/error_inline_alias_qualified/Test.roc ───────────

    This type alias has a qualified name:

    4│      f : List elem -> [Nil, Cons elem a] as Module.LinkedList a
                                                   ^

    An alias introduces a new name to the current scope, so it must be
    unqualified.
    "
    );

    test_report!(
        error_inline_alias_argument_uppercase,
        indoc!(
            r"
            f : List elem -> [Nil, Cons elem a] as LinkedList U
            "
        ),
        @r"
    ── TYPE ARGUMENT NOT LOWERCASE in ..._inline_alias_argument_uppercase/Test.roc ─

    This alias type argument is not lowercase:

    4│      f : List elem -> [Nil, Cons elem a] as LinkedList U
                                                              ^

    All type arguments must be lowercase.
    "
    );

    test_report!(
        mismatched_single_tag_arg,
        indoc!(
            r#"
            is_empty =
                \email ->
                    Email str = email
                    Str.is_empty str

            is_empty (Name "boo")
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `is_empty` has an unexpected type:

    9│      is_empty (Name "boo")
                      ^^^^^^^^^^

    This `Name` tag application has the type:

        [Name Str]

    But `is_empty` needs its 1st argument to be:

        [Email Str]

    Tip: Seems like a tag typo. Maybe `Name` should be `Email`?

    Tip: Can more type annotations be added? Type annotations always help
    me give more specific messages, and I think they could help a lot in
    this case
    "#
    );

    test_report!(
        issue_2326,
        indoc!(
            r"
            C a b : a -> D a b
            D a b : { a, b }

            f : C a U64 -> D a U64
            f = \c -> c 6
            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `c` has an unexpected type:

    8│      f = \c -> c 6
                        ^

    The argument is a number of type:

        Num *

    But `c` needs its 1st argument to be:

        a

    Tip: The type annotation uses the type variable `a` to say that this
    definition can produce any type of value. But in the body I see that
    it will only produce a `Num` value of a single specific type. Maybe
    change the type annotation to be more specific? Maybe change the code
    to be more general?
    "
    );

    test_report!(
        issue_2380_annotations_only,
        indoc!(
            r"
            F : F
            a : F
            a
            "
        ),
        @r"
    ── CYCLIC ALIAS in /code/proj/Main.roc ─────────────────────────────────────────

    The `F` alias is self-recursive in an invalid way:

    4│      F : F
            ^

    Recursion in aliases is only allowed if recursion happens behind a
    tagged union, at least one variant of which is not recursive.
    "
    );

    test_report!(
        issue_2380_typed_body,
        indoc!(
            r"
            F : F
            a : F
            a = 1
            a
            "
        ),
        @r"
    ── CYCLIC ALIAS in /code/proj/Main.roc ─────────────────────────────────────────

    The `F` alias is self-recursive in an invalid way:

    4│      F : F
            ^

    Recursion in aliases is only allowed if recursion happens behind a
    tagged union, at least one variant of which is not recursive.
    "
    );

    test_report!(
        issue_2380_alias_with_vars,
        indoc!(
            r"
            F a b : F a b
            a : F Str Str
            a
            "
        ),
        @r"
    ── CYCLIC ALIAS in /code/proj/Main.roc ─────────────────────────────────────────

    The `F` alias is self-recursive in an invalid way:

    4│      F a b : F a b
            ^

    Recursion in aliases is only allowed if recursion happens behind a
    tagged union, at least one variant of which is not recursive.
    "
    );

    test_report!(
        issue_2167_record_field_optional_and_required_mismatch,
        indoc!(
            r#"
            Job : [Job { inputs : List Str }]
            job : { inputs ? List Str } -> Job
            job = \{ inputs } ->
                Job { inputs }

            job { inputs: ["build", "test"] }
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The 1st argument to `job` is weird:

    6│      job = \{ inputs } ->
                   ^^^^^^^^^^

    The argument is a pattern that matches record values of type:

        { inputs : List Str }

    But the annotation on `job` says the 1st argument should be:

        { inputs ? List Str }

    Tip: To extract the `.inputs` field it must be non-optional, but the
    type says this field is optional. Learn more about optional fields at
    TODO.
    "
    );

    test_report!(
        unify_recursive_with_nonrecursive,
        indoc!(
            r#"
            Job : [Job { inputs : List Job }]

            job : { inputs : List Str } -> Job
            job = \{ inputs } ->
                Job { inputs }

            job { inputs: ["build", "test"] }
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `job` definition:

    6│      job : { inputs : List Str } -> Job
    7│      job = \{ inputs } ->
    8│          Job { inputs }
                ^^^^^^^^^^^^^^

    This `Job` tag application has the type:

        [Job { inputs : List Str }]

    But the type annotation on `job` says it should be:

        [Job { inputs : List a }]a as a
    "
    );

    test_report!(
        nested_datatype,
        indoc!(
            r"
            Nested a : [Chain a (Nested (List a)), Term]

            s : Nested Str

            s
            "
        ),
        @r"
    ── NESTED DATATYPE in /code/proj/Main.roc ──────────────────────────────────────

    `Nested` is a nested datatype. Here is one recursive usage of it:

    4│      Nested a : [Chain a (Nested (List a)), Term]
                                 ^^^^^^^^^^^^^^^

    But recursive usages of `Nested` must match its definition:

    4│      Nested a : [Chain a (Nested (List a)), Term]
            ^^^^^^^^

    Nested datatypes are not supported in Roc.

    Hint: Consider rewriting the definition of `Nested` to use the recursive type with the same arguments.
    "
    );

    test_report!(
        nested_datatype_inline,
        indoc!(
            r"
            f : {} -> [Chain a (Nested (List a)), Term] as Nested a

            f
            "
        ),
        @r"
    ── NESTED DATATYPE in /code/proj/Main.roc ──────────────────────────────────────

    `Nested` is a nested datatype. Here is one recursive usage of it:

    4│      f : {} -> [Chain a (Nested (List a)), Term] as Nested a
                                ^^^^^^^^^^^^^^^

    But recursive usages of `Nested` must match its definition:

    4│      f : {} -> [Chain a (Nested (List a)), Term] as Nested a
                                                           ^^^^^^^^

    Nested datatypes are not supported in Roc.

    Hint: Consider rewriting the definition of `Nested` to use the recursive type with the same arguments.
    "
    );

    macro_rules! mismatched_suffix_tests {
        ($($number:expr, $suffix:expr, $name:ident)*) => {$(
            test_report!(
                $name,
                &{
                    let number = $number.to_string();
                    let mut typ = $suffix.to_string();
                    typ.get_mut(0..1).unwrap().make_ascii_uppercase();
                    let bad_type = if $suffix == "u8" { "I8" } else { "U8" };

                    format!(indoc!(
                        r"
                        use : Num.{} -> Num.U8
                        use {}{}
                        "
                    ), bad_type, number, $suffix)
                },
                |golden| {
                    let number = $number.to_string();
                    let mut typ = $suffix.to_string();
                    typ.get_mut(0..1).unwrap().make_ascii_uppercase();
                    let bad_type = if $suffix == "u8" { "I8" } else { "U8" };
                    let carets = "^".repeat(number.len() + $suffix.len());
                    let kind = match $suffix {
                        "dec"|"f32"|"f64" => "a fraction",
                        _ => "an integer",
                    };

                    let real = format!(indoc!(
                        r"
                        ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

                        This 1st argument to `use` has an unexpected type:

                        5│      use {}{}
                                    {}

                        The argument is {} of type:

                            {}

                        But `use` needs its 1st argument to be:

                            {}
                        "
                    ), number, $suffix, carets, kind, typ, bad_type);

                    assert_eq!(golden, real);
                }
            );
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
        1, "dec",  mismatched_suffix_dec
        1, "f32",  mismatched_suffix_f32
        1, "f64",  mismatched_suffix_f64
    }

    macro_rules! mismatched_suffix_tests_in_pattern {
        ($($number:expr, $suffix:expr, $name:ident)*) => {$(
            test_report!(
                $name,
                &{
                    let number = $number.to_string();
                    let mut typ = $suffix.to_string();
                    typ.get_mut(0..1).unwrap().make_ascii_uppercase();
                    let bad_suffix = if $suffix == "u8" { "i8" } else { "u8" };

                    format!(indoc!(
                        r"
                        when {}{} is
                            {}{} -> 1
                            _ -> 1
                        "
                    ), number, bad_suffix, number, $suffix)
                },
                |golden| {
                    let number = $number.to_string();
                    let mut typ = $suffix.to_string();
                    typ.get_mut(0..1).unwrap().make_ascii_uppercase();
                    let bad_suffix = if $suffix == "u8" { "i8" } else { "u8" };
                    let bad_type = if $suffix == "u8" { "I8" } else { "U8" };

                    let real = format!(indoc!(
                        r"
                        ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

                        The branches of this `when` expression don't match the condition:

                        4│>      when {}{} is
                        5│           {}{} -> 1
                        6│           _ -> 1

                        The `when` condition is an integer of type:

                            {}

                        But the branch patterns have type:

                            {}

                        The branches must be cases of the `when` condition's type!
                        "
                    ), number, bad_suffix, number, $suffix, bad_type, typ);

                    assert_eq!(golden, real);
                }
            );
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
        1, "dec",  mismatched_suffix_dec_pattern
        1, "f32",  mismatched_suffix_f32_pattern
        1, "f64",  mismatched_suffix_f64_pattern
    }

    test_report!(
        bad_numeric_literal_suffix,
        indoc!(
            r"
            1u256
            "
        ),
        // TODO: link to number suffixes
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This integer literal contains an invalid digit:

    4│      1u256
            ^^^^^

    Integer literals can only contain the digits
    0-9, or have an integer suffix.

    Tip: Learn more about number literals at TODO
    "
    );

    test_report!(
        numer_literal_multi_suffix,
        indoc!(
            r"
            1u8u8
            "
        ),
        // TODO: link to number suffixes
        @r"
    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    This integer literal contains an invalid digit:

    4│      1u8u8
            ^^^^^

    Integer literals can only contain the digits
    0-9, or have an integer suffix.

    Tip: Learn more about number literals at TODO
    "
    );

    test_report!(
        int_literal_has_float_suffix,
        indoc!(
            r"
            0b1f32
            "
        ),
        @r"
    ── CONFLICTING NUMBER SUFFIX in /code/proj/Main.roc ────────────────────────────

    This number literal is an integer, but it has a float suffix:

    4│      0b1f32
            ^^^^^^
    "
    );

    test_report!(
        float_literal_has_int_suffix,
        indoc!(
            r"
            1.0u8
            "
        ),
        @r"
    ── CONFLICTING NUMBER SUFFIX in /code/proj/Main.roc ────────────────────────────

    This number literal is a float, but it has an integer suffix:

    4│      1.0u8
            ^^^^^
    "
    );

    test_report!(
        u8_overflow,
        "256u8",
        @r"
    ── NUMBER OVERFLOWS SUFFIX in /code/proj/Main.roc ──────────────────────────────

    This integer literal overflows the type indicated by its suffix:

    4│      256u8
            ^^^^^

    Tip: The suffix indicates this integer is a U8, whose maximum value is
    255.
    "
    );

    test_report!(
        negative_u8,
        "-1u8",
        @r"
    ── NUMBER UNDERFLOWS SUFFIX in /code/proj/Main.roc ─────────────────────────────

    This integer literal underflows the type indicated by its suffix:

    4│      -1u8
            ^^^^

    Tip: The suffix indicates this integer is a U8, whose minimum value is
    0.
    "
    );

    test_report!(
        u16_overflow,
        "65536u16",
        @r"
    ── NUMBER OVERFLOWS SUFFIX in /code/proj/Main.roc ──────────────────────────────

    This integer literal overflows the type indicated by its suffix:

    4│      65536u16
            ^^^^^^^^

    Tip: The suffix indicates this integer is a U16, whose maximum value
    is 65535.
    "
    );

    test_report!(
        negative_u16,
        "-1u16",
        @r"
    ── NUMBER UNDERFLOWS SUFFIX in /code/proj/Main.roc ─────────────────────────────

    This integer literal underflows the type indicated by its suffix:

    4│      -1u16
            ^^^^^

    Tip: The suffix indicates this integer is a U16, whose minimum value
    is 0.
    "
    );

    test_report!(
        u32_overflow,
        "4_294_967_296u32",
        @r"
    ── NUMBER OVERFLOWS SUFFIX in /code/proj/Main.roc ──────────────────────────────

    This integer literal overflows the type indicated by its suffix:

    4│      4_294_967_296u32
            ^^^^^^^^^^^^^^^^

    Tip: The suffix indicates this integer is a U32, whose maximum value
    is 4_294_967_295.
    "
    );

    test_report!(
        negative_u32,
        "-1u32",
        @r"
    ── NUMBER UNDERFLOWS SUFFIX in /code/proj/Main.roc ─────────────────────────────

    This integer literal underflows the type indicated by its suffix:

    4│      -1u32
            ^^^^^

    Tip: The suffix indicates this integer is a U32, whose minimum value
    is 0.
    "
    );

    test_report!(
        u64_overflow,
        "18_446_744_073_709_551_616u64",
        @r"
    ── NUMBER OVERFLOWS SUFFIX in /code/proj/Main.roc ──────────────────────────────

    This integer literal overflows the type indicated by its suffix:

    4│      18_446_744_073_709_551_616u64
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Tip: The suffix indicates this integer is a U64, whose maximum value
    is 18_446_744_073_709_551_615.
    "
    );

    test_report!(
        negative_u64,
        "-1u64",
        @r"
    ── NUMBER UNDERFLOWS SUFFIX in /code/proj/Main.roc ─────────────────────────────

    This integer literal underflows the type indicated by its suffix:

    4│      -1u64
            ^^^^^

    Tip: The suffix indicates this integer is a U64, whose minimum value
    is 0.
    "
    );

    test_report!(
        negative_u128,
        "-1u128",
        @r"
    ── NUMBER UNDERFLOWS SUFFIX in /code/proj/Main.roc ─────────────────────────────

    This integer literal underflows the type indicated by its suffix:

    4│      -1u128
            ^^^^^^

    Tip: The suffix indicates this integer is a U128, whose minimum value
    is 0.
    "
    );

    test_report!(
        i8_overflow,
        "128i8",
        @r"
    ── NUMBER OVERFLOWS SUFFIX in /code/proj/Main.roc ──────────────────────────────

    This integer literal overflows the type indicated by its suffix:

    4│      128i8
            ^^^^^

    Tip: The suffix indicates this integer is a I8, whose maximum value is
    127.
    "
    );

    test_report!(
        i8_underflow,
        "-129i8",
        @r"
    ── NUMBER UNDERFLOWS SUFFIX in /code/proj/Main.roc ─────────────────────────────

    This integer literal underflows the type indicated by its suffix:

    4│      -129i8
            ^^^^^^

    Tip: The suffix indicates this integer is a I8, whose minimum value is
    -128.
    "
    );

    test_report!(
        i16_overflow,
        "32768i16",
        @r"
    ── NUMBER OVERFLOWS SUFFIX in /code/proj/Main.roc ──────────────────────────────

    This integer literal overflows the type indicated by its suffix:

    4│      32768i16
            ^^^^^^^^

    Tip: The suffix indicates this integer is a I16, whose maximum value
    is 32767.
    "
    );

    test_report!(
        i16_underflow,
        "-32769i16",
        @r"
    ── NUMBER UNDERFLOWS SUFFIX in /code/proj/Main.roc ─────────────────────────────

    This integer literal underflows the type indicated by its suffix:

    4│      -32769i16
            ^^^^^^^^^

    Tip: The suffix indicates this integer is a I16, whose minimum value
    is -32768.
    "
    );

    test_report!(
        i32_overflow,
        "2_147_483_648i32",
        @r"
    ── NUMBER OVERFLOWS SUFFIX in /code/proj/Main.roc ──────────────────────────────

    This integer literal overflows the type indicated by its suffix:

    4│      2_147_483_648i32
            ^^^^^^^^^^^^^^^^

    Tip: The suffix indicates this integer is a I32, whose maximum value
    is 2_147_483_647.
    "
    );

    test_report!(
        i32_underflow,
        "-2_147_483_649i32",
        @r"
    ── NUMBER UNDERFLOWS SUFFIX in /code/proj/Main.roc ─────────────────────────────

    This integer literal underflows the type indicated by its suffix:

    4│      -2_147_483_649i32
            ^^^^^^^^^^^^^^^^^

    Tip: The suffix indicates this integer is a I32, whose minimum value
    is -2_147_483_648.
    "
    );

    test_report!(
        i64_overflow,
        "9_223_372_036_854_775_808i64",
        @r"
    ── NUMBER OVERFLOWS SUFFIX in /code/proj/Main.roc ──────────────────────────────

    This integer literal overflows the type indicated by its suffix:

    4│      9_223_372_036_854_775_808i64
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Tip: The suffix indicates this integer is a I64, whose maximum value
    is 9_223_372_036_854_775_807.
    "
    );

    test_report!(
        i64_underflow,
        "-9_223_372_036_854_775_809i64",
        @r"
    ── NUMBER UNDERFLOWS SUFFIX in /code/proj/Main.roc ─────────────────────────────

    This integer literal underflows the type indicated by its suffix:

    4│      -9_223_372_036_854_775_809i64
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Tip: The suffix indicates this integer is a I64, whose minimum value
    is -9_223_372_036_854_775_808.
    "
    );

    test_report!(
        i128_overflow,
        "170_141_183_460_469_231_731_687_303_715_884_105_728i128",
        @r"
    ── NUMBER OVERFLOWS SUFFIX in /code/proj/Main.roc ──────────────────────────────

    This integer literal overflows the type indicated by its suffix:

    4│      170_141_183_460_469_231_731_687_303_715_884_105_728i128
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Tip: The suffix indicates this integer is a I128, whose maximum value
    is 170_141_183_460_469_231_731_687_303_715_884_105_727.
    "
    );

    test_report!(
        list_get_negative_number,
        indoc!(
            r"
             List.get [1,2,3] -1
             "
        ),
        // TODO: this error message could be improved, e.g. something like "This argument can
        // be used as ... because of its literal value"
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to `get` has an unexpected type:

    4│      List.get [1,2,3] -1
                             ^^

    The argument is a number of type:

        I8, I16, F32, I32, F64, I64, I128, or Dec

    But `get` needs its 2nd argument to be:

        U64
    "
    );

    test_report!(
        list_get_negative_number_indirect,
        indoc!(
            r"
             a = -9_223_372_036_854
             List.get [1,2,3] a
             "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to `get` has an unexpected type:

    5│      List.get [1,2,3] a
                             ^

    This `a` value is a:

        F64, I64, I128, or Dec

    But `get` needs its 2nd argument to be:

        U64
    "
    );

    test_report!(
        list_get_negative_number_double_indirect,
        indoc!(
            r"
             a = -9_223_372_036_854
             b = a
             List.get [1,2,3] b
             "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to `get` has an unexpected type:

    6│      List.get [1,2,3] b
                             ^

    This `b` value is a:

        F64, I64, I128, or Dec

    But `get` needs its 2nd argument to be:

        U64
    "
    );

    test_report!(
        compare_unsigned_to_signed,
        indoc!(
            r"
            when -1 is
               1u8 -> 1
               _ -> 1
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The branches of this `when` expression don't match the condition:

    4│>      when -1 is
    5│          1u8 -> 1
    6│          _ -> 1

    The `when` condition is a number of type:

        I8, I16, F32, I32, F64, I64, I128, or Dec

    But the branch patterns have type:

        U8

    The branches must be cases of the `when` condition's type!
    "
    );

    test_report!(
        recursive_type_alias_is_newtype,
        indoc!(
            r"
            R a : [Only (R a)]

            v : R Str
            v
            "
        ),
        @r"
    ── CYCLIC ALIAS in /code/proj/Main.roc ─────────────────────────────────────────

    The `R` alias is self-recursive in an invalid way:

    4│      R a : [Only (R a)]
            ^

    Recursion in aliases is only allowed if recursion happens behind a
    tagged union, at least one variant of which is not recursive.
    "
    );

    test_report!(
        recursive_type_alias_is_newtype_deep,
        indoc!(
            r"
            R a : [Only { very: [Deep (R a)] }]

            v : R Str
            v
            "
        ),
        @r"
    ── CYCLIC ALIAS in /code/proj/Main.roc ─────────────────────────────────────────

    The `R` alias is self-recursive in an invalid way:

    4│      R a : [Only { very: [Deep (R a)] }]
            ^

    Recursion in aliases is only allowed if recursion happens behind a
    tagged union, at least one variant of which is not recursive.
    "
    );

    test_report!(
        recursive_type_alias_is_newtype_mutual,
        indoc!(
            r"
            Foo a : [Thing (Bar a)]
            Bar a : [Stuff (Foo a)]

            v : Bar Str
            v
            "
        ),
        @r"
    ── CYCLIC ALIAS in /code/proj/Main.roc ─────────────────────────────────────────

    The `Foo` alias is recursive in an invalid way:

    4│      Foo a : [Thing (Bar a)]
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
    "
    );

    test_report!(
        issue_2458,
        indoc!(
            r"
            Result a b : [Ok a, Err b]

            Foo a : [Blah (Result (Bar a) [])]
            Bar a : Foo a

            v : Bar Str
            v
            "
        ),
        @r"
    ── DUPLICATE NAME in /code/proj/Main.roc ───────────────────────────────────────

    This alias has the same name as a builtin:

    4│      Result a b : [Ok a, Err b]
            ^^^^^^^^^^^^^^^^^^^^^^^^^^

    All builtin aliases are in scope by default, so I need this alias to
    have a different name!
    "
    );

    test_report!(
        opaque_type_not_in_scope,
        indoc!(
            r"
            @Age 21
            "
        ),
        @r"
    ── OPAQUE TYPE NOT DEFINED in /code/proj/Main.roc ──────────────────────────────

    The opaque type Age referenced here is not defined:

    4│      @Age 21
            ^^^^

    Note: It looks like there are no opaque types declared in this scope yet!
    "
    );

    test_report!(
        opaque_reference_not_opaque_type,
        indoc!(
            r"
            Age : Num.U8

            @Age 21
            "
        ),
        @r"
    ── OPAQUE TYPE NOT DEFINED in /code/proj/Main.roc ──────────────────────────────

    The opaque type Age referenced here is not defined:

    6│      @Age 21
            ^^^^

    Note: There is an alias of the same name:

    4│      Age : Num.U8
            ^^^

    Note: It looks like there are no opaque types declared in this scope yet!

    ── UNUSED DEFINITION in /code/proj/Main.roc ────────────────────────────────────

    `Age` is not used anywhere in your code.

    4│      Age : Num.U8
            ^^^^^^^^^^^^

    If you didn't intend on using `Age` then remove it so future readers of
    your code don't wonder why it is there.
    "
    );

    test_report!(
        qualified_opaque_reference,
        indoc!(
            r"
            OtherModule.@Age 21
            "
        ),
        // TODO: get rid of the first error. Consider parsing OtherModule.@Age to completion
        // and checking it during can. The reason the error appears is because it is parsed as
        // Apply(Error(OtherModule), [@Age, 21])
        @r"
    ── OPAQUE TYPE NOT DEFINED in /code/proj/Main.roc ──────────────────────────────

    The opaque type Age referenced here is not defined:

    4│      OtherModule.@Age 21
                        ^^^^

    Note: It looks like there are no opaque types declared in this scope yet!

    ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

    I am trying to parse a qualified name here:

    4│      OtherModule.@Age 21
                        ^

    I was expecting to see an identifier next, like height. A complete
    qualified name looks something like Json.Decode.string.
    "
    );

    test_report!(
        opaque_used_outside_declaration_scope,
        indoc!(
            r"
            age =
                Age := Num.U8
                21u8

            @Age age
            "
        ),
        // TODO(opaques): there is a potential for a better error message here, if the usage of
        // `@Age` can be linked to the declaration of `Age` inside `age`, and a suggestion to
        // raise that declaration to the outer scope.
        @r"
    ── UNUSED DEFINITION in /code/proj/Main.roc ────────────────────────────────────

    `Age` is not used anywhere in your code.

    5│          Age := Num.U8
                ^^^^^^^^^^^^^

    If you didn't intend on using `Age` then remove it so future readers of
    your code don't wonder why it is there.

    ── OPAQUE TYPE NOT DEFINED in /code/proj/Main.roc ──────────────────────────────

    The opaque type Age referenced here is not defined:

    8│      @Age age
            ^^^^

    Note: It looks like there are no opaque types declared in this scope yet!
    "
    );

    test_report!(
        unimported_modules_reported,
        indoc!(
            r#"
            alt : Unimported.CustomType
            alt = "whatever man you don't even know my type"
            alt
            "#
        ),
        @r"
    ── MODULE NOT IMPORTED in /code/proj/Main.roc ──────────────────────────────────

    The `Unimported` module is not imported:

    4│      alt : Unimported.CustomType
                  ^^^^^^^^^^^^^^^^^^^^^

    Is there an import missing? Perhaps there is a typo. Did you mean one
    of these?

        Encode
        Inspect
        Dict
        List
    "
    );

    test_report!(
        opaque_mismatch_check,
        indoc!(
            r#"
            Age := Num.U8

            n : Age
            n = @Age ""

            n
            "#
        ),
        // TODO(opaques): error could be improved by saying that the opaque definition demands
        // that the argument be a U8, and linking to the definition!
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression is used in an unexpected way:

    7│      n = @Age ""
                     ^^

    This argument to an opaque type has type:

        Str

    But you are trying to use it as:

        U8
    "#
    );

    test_report!(
        opaque_mismatch_infer,
        indoc!(
            r#"
            F n := n

            if Bool.true
            then @F ""
            else @F {}
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression is used in an unexpected way:

    8│      else @F {}
                    ^^

    This argument to an opaque type has type:

        {}

    But you are trying to use it as:

        Str
    "
    );

    test_report!(
        opaque_creation_is_not_wrapped,
        indoc!(
            r#"
            F n := n

            v : F Str
            v = ""

            v
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `v` definition:

    6│      v : F Str
    7│      v = ""
                ^^

    The body is a string of type:

        Str

    But the type annotation on `v` says it should be:

        F Str

    Tip: *Add type annotations* to functions or values to help you figure
    this out.
    "#
    );

    test_report!(
        opaque_mismatch_pattern_check,
        indoc!(
            r"
            Age := Num.U8

            f : Age -> Num.U8
            f = \Age n -> n

            f
            "
        ),
        // TODO(opaques): error could be improved by saying that the user-provided pattern
        // probably wants to change "Age" to "@Age"!
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The 1st argument to `f` is weird:

    7│      f = \Age n -> n
                 ^^^^^

    The argument is a pattern that matches a `Age` tag of type:

        [Age *]

    But the annotation on `f` says the 1st argument should be:

        Age

    Tip: *Add type annotations* to functions or values to help you figure
    this out.
    "
    );

    test_report!(
        opaque_mismatch_pattern_infer,
        indoc!(
            r#"
            F n := n

            \x ->
                when x is
                    @F A -> ""
                    @F {} -> ""
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The 2nd pattern in this `when` does not match the previous ones:

    9│              @F {} -> ""
                    ^^^^^

    The 2nd pattern is trying to matchF unwrappings of type:

        F {}a

    But all the previous branches match:

        F [A]
    "#
    );

    test_report!(
        opaque_pattern_match_not_exhaustive_tag,
        indoc!(
            r#"
            F n := n

            v : F [A, B, C]

            when v is
                @F A -> ""
                @F B -> ""
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The branches of this `when` expression don't match the condition:

     8│>      when v is
     9│           @F A -> ""
    10│           @F B -> ""

    This `v` value is a:

        F [C, …]

    But the branch patterns have type:

        F […]

    The branches must be cases of the `when` condition's type!

    Tip: Looks like the branches are missing coverage of the `C` tag.

    Tip: Maybe you need to add a catch-all branch, like `_`?
    "#
    );

    test_report!(
        opaque_pattern_match_not_exhaustive_int,
        indoc!(
            r#"
            F n := n

            v : F Num.U8

            when v is
                @F 1 -> ""
                @F 2 -> ""
            "#
        ),
        @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

     8│>      when v is
     9│>          @F 1 -> ""
    10│>          @F 2 -> ""

    Other possibilities include:

        @F _

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        let_polymorphism_with_scoped_type_variables,
        indoc!(
            r"
            f : a -> a
            f = \x ->
                y : a -> a
                y = \z -> z

                n = y 1u8
                x1 = y x
                (\_ -> x1) n

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `y` has an unexpected type:

    9│          n = y 1u8
                      ^^^

    The argument is an integer of type:

        U8

    But `y` needs its 1st argument to be:

        a

    Tip: The type annotation uses the type variable `a` to say that this
    definition can produce any type of value. But in the body I see that
    it will only produce a `U8` value of a single specific type. Maybe
    change the type annotation to be more specific? Maybe change the code
    to be more general?
    "
    );

    test_report!(
        non_exhaustive_with_guard,
        indoc!(
            r#"
            x : [A]
            when x is
                A if Bool.true -> ""
            "#
        ),
        @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    5│>      when x is
    6│>          A if Bool.true -> ""

    Other possibilities include:

        A    (note the lack of an if clause)

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        invalid_record_extension_type,
        indoc!(
            r"
            f : { x : U64 }[]
            f
            "
        ),
        @r"
    ── INVALID_EXTENSION_TYPE in /code/proj/Main.roc ───────────────────────────────

    This record extension type is invalid:

    4│      f : { x : U64 }[]
                           ^^

    Note: A record extension variable can only contain a type variable or
    another record.
    "
    );

    test_report!(
        invalid_tag_extension_type,
        indoc!(
            r"
            f : [A]Str
            f
            "
        ),
        @r"
    ── INVALID_EXTENSION_TYPE in /code/proj/Main.roc ───────────────────────────────

    This tag union extension type is invalid:

    4│      f : [A]Str
                   ^^^

    Note: A tag union extension variable can only contain a type variable
    or another tag union.
    "
    );

    test_report!(
        unknown_type,
        indoc!(
            r"
            Type : [Constructor UnknownType]

            insertHelper : UnknownType, Type -> Type
            insertHelper = \h, m ->
                when m is
                    Constructor _ -> Constructor h

            insertHelper
            "
        ),
        @r"
    ── UNRECOGNIZED NAME in /code/proj/Main.roc ────────────────────────────────────

    Nothing is named `UnknownType` in this scope.

    4│      Type : [Constructor UnknownType]
                                ^^^^^^^^^^^

    Did you mean one of these?

        Type
        Unsigned8
        Unsigned16
        Unsigned64

    ── UNRECOGNIZED NAME in /code/proj/Main.roc ────────────────────────────────────

    Nothing is named `UnknownType` in this scope.

    6│      insertHelper : UnknownType, Type -> Type
                           ^^^^^^^^^^^

    Did you mean one of these?

        Type
        Unsigned8
        Unsigned16
        Unsigned64
    "
    );

    test_report!(
        ability_first_demand_not_indented_enough,
        indoc!(
            r"
            MEq implements
            eq : a, a -> U64 where a implements MEq

            1
            "
        ),
        @r"
    ── UNFINISHED ABILITY in tmp/ability_first_demand_not_indented_enough/Test.roc ─

    I was partway through parsing an ability definition, but I got stuck
    here:

    4│      MEq implements
    5│      eq : a, a -> U64 where a implements MEq
            ^

    I suspect this line is not indented enough (by 1 spaces)
    "
    );

    test_report!(
        ability_demands_not_indented_with_first,
        indoc!(
            r"
            MEq implements
                eq : a, a -> U64 where a implements MEq
                    neq : a, a -> U64 where a implements MEq

            1
            "
        ),
        @r"
        ── UNFINISHED ABILITY in tmp/ability_demands_not_indented_with_first/Test.roc ──

        I was partway through parsing an ability definition, but I got stuck
        here:

        5│          eq : a, a -> U64 where a implements MEq
        6│              neq : a, a -> U64 where a implements MEq
                        ^

        I suspect this line is indented too much (by 4 spaces)"
    );

    test_report!(
        ability_demand_value_has_args,
        indoc!(
            r"
                MEq implements
                    eq b c : a, a -> U64 where a implements MEq

                1
                "
        ),
        @r"
        ── UNFINISHED ABILITY in tmp/ability_demand_value_has_args/Test.roc ────────────

        I was partway through parsing an ability definition, but I got stuck
        here:

        4│      MEq implements
        5│          eq b c : a, a -> U64 where a implements MEq
                       ^

        I was expecting to see a : annotating the signature of this value
        next."
    );

    test_report!(
        ability_non_signature_expression,
        indoc!(
            r"
            MEq implements
                123

            1
            "
        ),
        @r"
    ── UNFINISHED ABILITY in tmp/ability_non_signature_expression/Test.roc ─────────

    I was partway through parsing an ability definition, but I got stuck
    here:

    4│      MEq implements
    5│          123
                ^

    I was expecting to see a value signature next.
    "
    );

    test_report!(
        wildcard_in_alias,
        indoc!(
            r"
            I : Num.Int *
            a : I
            a
            "
        ),
        @r###"
    ── WILDCARD NOT ALLOWED HERE in /code/proj/Main.roc ────────────────────────────

    The definition of `I` includes a wildcard (`*`) type variable:

    4│      I : Num.Int *
                        ^

    Type alias definitions may not use wildcard (`*`) type variables. Only
    named type variables are allowed.
    "###
    );

    test_report!(
        underscore_in_alias,
        indoc!(
            r"
            I : Num.Int _
            a : I
            a
            "
        ),
        @r###"
    ── UNDERSCORE NOT ALLOWED HERE in /code/proj/Main.roc ──────────────────────────

    The definition of `I` includes an inferred (`_`) type:

    4│      I : Num.Int _
                        ^

    Type alias definitions may not use inferred types (`_`).
    "###
    );

    test_report!(
        wildcard_in_opaque,
        indoc!(
            r"
            I := Num.Int *
            a : I
            a
            "
        ),
        @r###"
    ── WILDCARD NOT ALLOWED HERE in /code/proj/Main.roc ────────────────────────────

    The definition of `I` includes a wildcard (`*`) type variable:

    4│      I := Num.Int *
                         ^

    Opaque type definitions may not use wildcard (`*`) type variables. Only
    named type variables are allowed.
    "###
    );

    test_report!(
        multiple_wildcards_in_alias,
        indoc!(
            r"
            I : [A (Num.Int *), B (Num.Int *)]
            a : I
            a
            "
        ),
        @r###"
    ── WILDCARD NOT ALLOWED HERE in /code/proj/Main.roc ────────────────────────────

    The definition of `I` includes 2 wildcard (`*`) type variables. Here is
    one of them:

    4│      I : [A (Num.Int *), B (Num.Int *)]
                            ^

    Type alias definitions may not use wildcard (`*`) type variables. Only
    named type variables are allowed.
    "###
    );

    test_report!(
        inference_var_in_alias,
        indoc!(
            r"
            I : Num.Int _
            a : I
            a
            "
        ),
        @r###"
    ── UNDERSCORE NOT ALLOWED HERE in /code/proj/Main.roc ──────────────────────────

    The definition of `I` includes an inferred (`_`) type:

    4│      I : Num.Int _
                        ^

    Type alias definitions may not use inferred types (`_`).
    "###
    );

    test_report!(
        unbound_var_in_alias,
        indoc!(
            r"
            I : Num.Int a
            a : I
            a
            "
        ),
        @r###"
    ── UNDECLARED TYPE VARIABLE in /code/proj/Main.roc ─────────────────────────────

    The definition of `I` includes an undeclared type variable:

    4│      I : Num.Int a
                        ^

    All type variables in type alias definitions must be declared.

    Tip: You can declare type variables by putting them right before the `:`
    symbol, separated by spaces.
    "###
    );

    test_report!(
        ability_bad_type_parameter,
        indoc!(
            r#"
            app "test" provides [] to "./platform"

            MHash a b c implements
              hash : a -> U64 where a implements MHash
            "#
        ),
        @r"
    ── ABILITY HAS TYPE VARIABLES in /code/proj/Main.roc ───────────────────────────

    The definition of the `MHash` ability includes type variables:

    3│  MHash a b c implements
              ^^^^^

    Abilities cannot depend on type variables, but their member values
    can!

    ── UNUSED DEFINITION in /code/proj/Main.roc ────────────────────────────────────

    `MHash` is not used anywhere in your code.

    3│  MHash a b c implements
        ^^^^^

    If you didn't intend on using `MHash` then remove it so future readers
    of your code don't wonder why it is there.
    "
    );

    test_report!(
        alias_in_implements_clause,
        indoc!(
            r#"
            app "test" provides [hash] to "./platform"

            MHash implements hash : a, b -> Num.U64 where a implements MHash, b implements Bool.Bool
            "#
        ),
        @r#"
    ── IMPLEMENTS CLAUSE IS NOT AN ABILITY in /code/proj/Main.roc ──────────────────

    The type referenced in this "implements" clause is not an ability:

    3│  MHash implements hash : a, b -> Num.U64 where a implements MHash, b implements Bool.Bool
                                                                                       ^^^^^^^^^
    "#
    );

    test_report!(
        shadowed_type_variable_in_has_clause,
        indoc!(
            r#"
            app "test" provides [ab1] to "./platform"

            Ab1 implements ab1 : a -> {} where a implements Ab1, a implements Ab1
            "#
        ),
        @r"
        ── DUPLICATE NAME in /code/proj/Main.roc ───────────────────────────────────────

        The `a` name is first defined here:

        3│  Ab1 implements ab1 : a -> {} where a implements Ab1, a implements Ab1
                                               ^^^^^^^^^^^^^^^^

        But then it's defined a second time here:

        3│  Ab1 implements ab1 : a -> {} where a implements Ab1, a implements Ab1
                                                                 ^^^^^^^^^^^^^^^^

        Since these variables have the same name, it's easy to use the wrong
        one by accident. Give one of them a new name.
        "
    );

    test_report!(
        ability_shadows_ability,
        indoc!(
            r#"
            app "test" provides [ab] to "./platform"

            Ability implements ab : a -> U64 where a implements Ability

            Ability implements ab1 : a -> U64 where a implements Ability
            "#
        ),
        @r"
        ── DUPLICATE NAME in /code/proj/Main.roc ───────────────────────────────────────

        The `Ability` name is first defined here:

        3│  Ability implements ab : a -> U64 where a implements Ability
            ^^^^^^^

        But then it's defined a second time here:

        5│  Ability implements ab1 : a -> U64 where a implements Ability
            ^^^^^^^

        Since these abilities have the same name, it's easy to use the wrong
        one by accident. Give one of them a new name.
        "
    );

    test_report!(
        ability_member_does_not_bind_ability,
        indoc!(
            r#"
            app "test" provides [] to "./platform"

            Ability implements ab : {} -> {}
            "#
        ),
        @r"
        ── ABILITY MEMBER MISSING IMPLEMENTS CLAUSE in /code/proj/Main.roc ─────────────

        The definition of the ability member `ab` does not include an `implements`
        clause binding a type variable to the ability `Ability`:

        3│  Ability implements ab : {} -> {}
                               ^^

        Ability members must include an `implements` clause binding a type
        variable to an ability, like

            a implements Ability

        Otherwise, the function does not need to be part of the ability!

        ── UNUSED DEFINITION in /code/proj/Main.roc ────────────────────────────────────

        `Ability` is not used anywhere in your code.

        3│  Ability implements ab : {} -> {}
            ^^^^^^^

        If you didn't intend on using `Ability` then remove it so future readers
        of your code don't wonder why it is there.
        "
    );

    test_report!(
        ability_member_binds_parent_twice,
        indoc!(
            r#"
            app "test" provides [] to "./platform"

            MEq implements eq : a, b -> Bool.Bool where a implements MEq, b implements MEq
            "#
        ),
        @r"
        ── ABILITY MEMBER BINDS MULTIPLE VARIABLES in /code/proj/Main.roc ──────────────

        The definition of the ability member `eq` includes multiple variables
        bound to the `MEq`` ability:`

        3│  MEq implements eq : a, b -> Bool.Bool where a implements MEq, b implements MEq
                                                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

        Ability members can only bind one type variable to their parent
        ability. Otherwise, I wouldn't know what type implements an ability by
        looking at specializations!

        Hint: Did you mean to only bind `a` to `MEq`?
        "
    );

    test_report!(
        has_clause_not_on_toplevel,
        indoc!(
            r#"
            app "test" provides [f] to "./platform"

            MHash implements hash : (a where a implements MHash) -> Num.U64

            f : a -> Num.U64 where a implements MHash
            "#
        ),
        @r"
    ── ILLEGAL IMPLEMENTS CLAUSE in /code/proj/Main.roc ────────────────────────────

    An `implements` clause is not allowed here:

    3│  MHash implements hash : (a where a implements MHash) -> Num.U64
                                         ^^^^^^^^^^^^^^^^^^

    `implements` clauses can only be specified on the top-level type
    annotations.

    ── ABILITY MEMBER MISSING IMPLEMENTS CLAUSE in /code/proj/Main.roc ─────────────

    The definition of the ability member `hash` does not include an
    `implements` clause binding a type variable to the ability `MHash`:

    3│  MHash implements hash : (a where a implements MHash) -> Num.U64
                         ^^^^

    Ability members must include an `implements` clause binding a type
    variable to an ability, like

        a implements MHash

    Otherwise, the function does not need to be part of the ability!
    "
    );

    test_report!(
        ability_specialization_does_not_match_type,
        indoc!(
            r#"
            app "test" provides [hash] to "./platform"

            MHash implements hash : a -> U64 where a implements MHash

            Id := U32 implements [MHash {hash}]

            hash = \@Id n -> n
            "#
        ),
        @r"
        ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

        Something is off with this specialization of `hash`:

        7│  hash = \@Id n -> n
            ^^^^

        This value is a declared specialization of type:

            Id -> U32

        But the type annotation on `hash` says it must match:

            Id -> U64
        "
    );

    test_report!(
        ability_specialization_is_incomplete,
        indoc!(
            r#"
            app "test" provides [eq, le] to "./platform"

            MEq implements
                eq : a, a -> Bool where a implements MEq
                le : a, a -> Bool where a implements MEq

            Id := U64 implements [MEq {eq}]

            eq = \@Id m, @Id n -> m == n
            "#
        ),
        @r"
    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    This type does not fully implement the `MEq` ability:

    7│  Id := U64 implements [MEq {eq}]
                              ^^^^^^^^

    The following necessary members are missing implementations:

        le
    "
    );

    test_report!(
        ability_specialization_is_unused,
        indoc!(
            r#"
            app "test" provides [hash] to "./platform"

            MHash implements
                hash : a -> U64 where a implements MHash

            hash = \_ -> 0u64
            "#
        ),
        @r"
    ── UNUSED DEFINITION in /code/proj/Main.roc ────────────────────────────────────

    `hash` is not used anywhere in your code.

    6│  hash = \_ -> 0u64
        ^^^^

    If you didn't intend on using `hash` then remove it so future readers of
    your code don't wonder why it is there.
    "
    );

    test_report!(
        ability_specialization_is_duplicated,
        indoc!(
            r#"
            app "test" provides [hash, One, Two] to "./platform"

            MHash implements
                hash : a -> U64 where a implements MHash

            One := {} implements [MHash {hash}]
            Two := {} implements [MHash {hash}]

            hash = \_ -> 0u64
            "#
        ),
        // TODO: the error message here could be seriously improved!
        @r"
    ── OVERLOADED SPECIALIZATION in /code/proj/Main.roc ────────────────────────────

    This ability member specialization is already claimed to specialize
    another opaque type:

    7│  Two := {} implements [MHash {hash}]
                                     ^^^^

    Previously, we found it to specialize `hash` for `One`.

    Ability specializations can only provide implementations for one
    opaque type, since all opaque types are different!

    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This specialization of `hash` is overly general:

    9│  hash = \_ -> 0u64
        ^^^^

    This value is a declared specialization of type:

        * -> U64

    But the type annotation on `hash` says it must match:

        a -> U64 where a implements MHash

    Note: The specialized type is too general, and does not provide a
    concrete type where a type variable is bound to an ability.

    Specializations can only be made for concrete types. If you have a
    generic implementation for this value, perhaps you don't need an
    ability?
    "
    );

    test_report!(
        ability_specialization_is_duplicated_with_type_mismatch,
        indoc!(
            r#"
            app "test" provides [hash, One, Two] to "./platform"

            MHash implements
                hash : a -> U64 where a implements MHash

            One := {} implements [MHash {hash}]
            Two := {} implements [MHash {hash}]

            hash = \@One _ -> 0u64
            "#
        ),
        @r"
    ── OVERLOADED SPECIALIZATION in /code/proj/Main.roc ────────────────────────────

    This ability member specialization is already claimed to specialize
    another opaque type:

    7│  Two := {} implements [MHash {hash}]
                                     ^^^^

    Previously, we found it to specialize `hash` for `One`.

    Ability specializations can only provide implementations for one
    opaque type, since all opaque types are different!
    "
    );

    test_report!(
        ability_specialization_conflicting_specialization_types,
        indoc!(
            r#"
            app "test" provides [eq] to "./platform"

            MEq implements
                eq : a, a -> Bool where a implements MEq

            You := {} implements [MEq {eq}]
            AndI := {}

            eq = \@You {}, @AndI {} -> False
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with this specialization of `eq`:

    9│  eq = \@You {}, @AndI {} -> False
        ^^

    This value is a declared specialization of type:

        You, AndI -> [False]

    But the type annotation on `eq` says it must match:

        You, You -> Bool

    Tip: Did you mean to use `Bool.false` rather than `False`?
    "
    );

    test_report!(
        ability_specialization_checked_against_annotation,
        indoc!(
            r#"
            app "test" provides [hash] to "./platform"

            MHash implements
                hash : a -> U64 where a implements MHash

            Id := U64 implements [MHash {hash}]

            hash : Id -> U32
            hash = \@Id n -> n
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `hash` definition:

    8│  hash : Id -> U32
    9│  hash = \@Id n -> n
                         ^

    This `n` value is a:

        U64

    But the type annotation on `hash` says it should be:

        U32
    "
    );

    test_report!(
        ability_specialization_called_with_non_specializing,
        indoc!(
            r#"
            app "test" provides [no_good_very_bad_terrible] to "./platform"

            MHash implements
                hash : a -> U64 where a implements MHash

            Id := U64 implements [MHash {hash}]

            hash = \@Id n -> n

            User := {}

            no_good_very_bad_terrible =
                {
                    nope: hash (@User {}),
                    not_yet: hash (A 1),
                }
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    15│          not_yet: hash (A 1),
                                ^^^

    I can't generate an implementation of the `MHash` ability for

        [A (Num *)]

    Only builtin abilities can have generated implementations!

    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    14│          nope: hash (@User {}),
                             ^^^^^^^^

    The type `User` does not fully implement the ability `MHash`.
    "
    );

    test_report!(
        ability_not_on_toplevel,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                MHash implements
                    hash : a -> U64 where a implements MHash

                123
            "#
        ),
        @r"
        ── ABILITY NOT ON TOP-LEVEL in /code/proj/Main.roc ─────────────────────────────

        This ability definition is not on the top-level of a module:

        4│>      MHash implements
        5│>          hash : a -> U64 where a implements MHash

        Abilities can only be defined on the top-level of a Roc module.
        "
    );

    test_report!(
        expression_generalization_to_ability_is_an_error,
        indoc!(
            r#"
            app "test" provides [hash, hashable] to "./platform"

            MHash implements
                hash : a -> U64 where a implements MHash

            Id := U64 implements [MHash {hash}]
            hash = \@Id n -> n

            hashable : a where a implements MHash
            hashable = @Id 15
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `hashable` definition:

     9│  hashable : a where a implements MHash
    10│  hashable = @Id 15
                    ^^^^^^

    This Id opaque wrapping has the type:

        Id

    But the type annotation on `hashable` says it should be:

        a where a implements MHash

    Note: The type variable `a` says it can take on any value that
    implements the ability `MHash`.

    But, I see that the type is only ever used as a a `Id` value. Can you
    replace `a` with a more specific type?
    "
    );

    test_report!(
        ability_value_annotations_are_an_error,
        indoc!(
            r#"
            app "test" provides [result] to "./platform"

            MHash implements
                hash : a -> U64 where a implements MHash

            mul_m_hashes : MHash, MHash -> U64
            mul_m_hashes = \x, y -> hash x * hash y

            Id := U64 implements [MHash {hash: hash_id}]
            hash_id = \@Id n -> n

            Three := {} implements [MHash {hash: hash_three}]
            hash_three = \@Three _ -> 3

            result = mul_m_hashes (@Id 100) (@Three {})
            "#
        ),
        @r"
    ── ABILITY USED AS TYPE in /code/proj/Main.roc ─────────────────────────────────

    You are attempting to use the ability `MHash` as a type directly:

    6│  mul_m_hashes : MHash, MHash -> U64
                       ^^^^^

    Abilities can only be used in type annotations to constrain type
    variables.

    Hint: Perhaps you meant to include an `implements` annotation, like

        a implements MHash

    ── ABILITY USED AS TYPE in /code/proj/Main.roc ─────────────────────────────────

    You are attempting to use the ability `MHash` as a type directly:

    6│  mul_m_hashes : MHash, MHash -> U64
                              ^^^^^

    Abilities can only be used in type annotations to constrain type
    variables.

    Hint: Perhaps you meant to include an `implements` annotation, like

        b implements MHash
    "
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
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The branches of this `when` expression don't match the condition:

    6│>          when bool is
    7│               True -> "true"
    8│               False -> "false"
    9│               Wat -> "surprise!"

    This `bool` value is a:

        Bool

    But the branch patterns have type:

        [
            False,
            True,
            Wat,
        ]

    The branches must be cases of the `when` condition's type!
    "#
    );

    // from https://github.com/roc-lang/roc/commit/1372737f5e53ee5bb96d7e1b9593985e5537023a
    // There was a bug where this reported UnusedArgument("val")
    // since it was used only in the returned function only.
    //
    // we want this to not give any warnings/errors!
    test_report!(
        always_function,
        indoc!(
            r"
            always = \val -> \_ -> val

            always
            "
        ),
        @""
    );

    test_report!(
        imports_missing_comma,
        indoc!(
            r#"
            app "test-missing-comma"
                packages { pf: "platform/main.roc" }
                imports [pf.Task Base64]
                provides [main, @Foo] to pf
            "#
        ),
        @r#"
        ── WEIRD IMPORTS in tmp/imports_missing_comma/Test.roc ─────────────────────────

        I am partway through parsing a imports list, but I got stuck here:

        2│      packages { pf: "platform/main.roc" }
        3│      imports [pf.Task Base64]
                                 ^

        I am expecting a comma or end of list, like

            imports [Shape, Vector]"#
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
        @r#"
        ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

        This `when` does not cover all the possibilities:

        6│>          when it is
        7│>              A -> ""

        Other possibilities include:

            B
            _

        I would have to crash if I saw one of those! Add branches for them!
        "#
    );

    test_report!(
        issue_2778_specialization_is_not_a_redundant_pattern,
        indoc!(
            r#"
            format_color = \color ->
              when color is
                Red -> "red"
                Yellow -> "yellow"
                _ -> "unknown"

            Red |> format_color |> Str.concat (format_color Orange)
            "#
        ),
        @"" // no problem
    );

    test_report!(
        nested_specialization,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Default implements default : {} -> a where a implements Default

            main =
                A := {} implements [Default {default}]
                default = \{} -> @A {}
                default {}
            "#
        ),
        @r"
        ── SPECIALIZATION NOT ON TOP-LEVEL in /code/proj/Main.roc ──────────────────────

        This specialization of the `default` ability member is in a nested
        scope:

        7│      default = \{} -> @A {}
                ^^^^^^^

        Specializations can only be defined on the top-level of a module.
        "
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
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to == has an unexpected type:

    9│          Job lst -> lst == ""
                                  ^^

    The argument is a string of type:

        Str

    But == needs its 2nd argument to be:

        List [Job ∞] as ∞
    "#
    );

    test_report!(
        #[ignore]
        type_error_in_apply_is_circular,
        indoc!(
            r#"
            app "test" imports [] provides [go] to "./platform"

            S a : { set : Set.Set a }

            go : a, S a -> Result (List a) *
            go = \goal, model ->
                    if goal == goal
                    then Ok []
                    else
                        new = { model & set : Set.remove goal model.set }
                        go goal new
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `remove` has an unexpected type:

    10│              new = { model & set : Set.remove goal model.set }
                                                      ^^^^

    This `goal` value is a:

        a

    But `remove` needs the 1st argument to be:

        Set k

    Tip: The type annotation uses the type variable `a` to say that this
    definition can produce any type of value. But in the body I see that
    it will only produce a `Set` value of a single specific type. Maybe
    change the type annotation to be more specific? Maybe change the code
    to be more general?

    ── CIRCULAR TYPE in /code/proj/Main.roc ────────────────────────────────────────

    I'm inferring a weird self-referential type for `new`:

    10│              new = { model & set : Set.remove goal model.set }
                     ^^^

    Here is my best effort at writing down the type. You will see ∞ for
    parts of the type that repeat something already printed out
    infinitely.

        { set : Set ∞ }

    ── CIRCULAR TYPE in /code/proj/Main.roc ────────────────────────────────────────

    I'm inferring a weird self-referential type for `goal`:

    6│  go = \goal, model ->
              ^^^^

    Here is my best effort at writing down the type. You will see ∞ for
    parts of the type that repeat something already printed out
    infinitely.

        Set ∞
    "
    );

    test_report!(
        cycle_through_non_function,
        indoc!(
            r"
            force : ({} -> I64) -> I64
            force = \eval -> eval {}

            t1 = \_ -> force (\_ -> t2)

            t2 = t1 {}

            t2
            "
        ),
        @r"
        ── CIRCULAR DEFINITION in /code/proj/Main.roc ──────────────────────────────────

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
        "
    );

    test_report!(
        function_does_not_implement_encoding,
        indoc!(
            r#"
            app "test" imports [] provides [main] to "./platform"

            main = Encode.to_encoder \x -> x
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    3│  main = Encode.to_encoder \x -> x
                                 ^^^^^^^

    I can't generate an implementation of the `Encoding` ability for

        a -> a

    Note: `Encoding` cannot be generated for functions.
    "
    );

    test_report!(
        nested_opaque_does_not_implement_encoding,
        indoc!(
            r#"
            app "test" imports [] provides [main] to "./platform"

            A := {}
            main = Encode.to_encoder { x: @A {} }
            "#
        ),
        // TODO: this error message is quite unfortunate. We should remove the duplication, and
        // also support regions that point to things in other modules. See also https://github.com/roc-lang/roc/issues/3056.
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    4│  main = Encode.to_encoder { x: @A {} }
                                 ^^^^^^^^^^^^

    I can't generate an implementation of the `Encoding` ability for

        { x : A }

    In particular, an implementation for

        A

    cannot be generated.

    Tip: `A` does not implement `Encoding`. Consider adding a custom
    implementation or `implements Encode.Encoding` to the definition of `A`.
    "
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
        @r"
            ── CIRCULAR DEFINITION in /code/proj/Main.roc ──────────────────────────────────

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
            "
    );

    test_report!(
        opaque_ability_impl_not_found_shorthand_syntax,
        indoc!(
            r#"
            app "test" provides [A] to "./platform"

            MEq implements eq : a, a -> U64 where a implements MEq

            A := U8 implements [MEq {eq}]
            "#
        ),
        @r"
    ── IMPLEMENTATION NOT FOUND in /code/proj/Main.roc ─────────────────────────────

    An implementation of `eq` could not be found in this scope:

    5│  A := U8 implements [MEq {eq}]
                                 ^^

    Tip: consider adding a value of name `eq` in this scope, or using
    another variable that implements this ability member, like
    { eq: myeq }

    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    This type does not fully implement the `MEq` ability:

    5│  A := U8 implements [MEq {eq}]
                            ^^^^^^^^

    The following necessary members are missing implementations:

        eq
    "
    );

    test_report!(
        opaque_ability_impl_not_found,
        indoc!(
            r#"
            app "test" provides [A, my_m_eq] to "./platform"

            MEq implements eq : a, a -> Bool where a implements MEq

            A := U8 implements [ MEq {eq: a_m_eq} ]

            my_m_eq = \m, n -> m == n
            "#
        ),
        @r"
    ── UNRECOGNIZED NAME in /code/proj/Main.roc ────────────────────────────────────

    Nothing is named `a_m_eq` in this scope.

    5│  A := U8 implements [ MEq {eq: a_m_eq} ]
                                      ^^^^^^

    Did you mean one of these?

        my_m_eq
        eq
        Eq
        Num

    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    This type does not fully implement the `MEq` ability:

    5│  A := U8 implements [ MEq {eq: a_m_eq} ]
                             ^^^^^^^^^^^^^^^^

    The following necessary members are missing implementations:

        eq
    "
    );

    test_report!(
        opaque_ability_impl_optional,
        indoc!(
            r#"
            app "test" provides [A, my_m_eq] to "./platform"

            MEq implements eq : a, a -> Bool where a implements MEq

            A := U8 implements [ MEq {eq ? a_m_eq} ]

            my_m_eq = \m, n -> m == n
            "#
        ),
        @r"
    ── OPTIONAL ABILITY IMPLEMENTATION in /code/proj/Main.roc ──────────────────────

    Ability implementations cannot be optional:

    5│  A := U8 implements [ MEq {eq ? a_m_eq} ]
                                  ^^^^^^^^^^^

    Custom implementations must be supplied fully.



    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    This type does not fully implement the `MEq` ability:

    5│  A := U8 implements [ MEq {eq ? a_m_eq} ]
                             ^^^^^^^^^^^^^^^^^

    The following necessary members are missing implementations:

        eq
    "
    );

    test_report!(
        opaque_builtin_ability_impl_optional,
        indoc!(
            r#"
            app "test"
                imports []
                provides [A, my_encoder] to "./platform"

            A := U8 implements [ Encoding {to_encoder ? my_encoder} ]

            my_encoder = 1
            "#
        ),
        @r"
    ── OPTIONAL ABILITY IMPLEMENTATION in /code/proj/Main.roc ──────────────────────

    Ability implementations cannot be optional:

    5│  A := U8 implements [ Encoding {to_encoder ? my_encoder} ]
                                       ^^^^^^^^^^^^^^^^^^^^^^^

    Custom implementations must be supplied fully.

    Hint: if you want this implementation to be derived, don't include a
    record of implementations. For example,    implements [Encoding] will
    attempt to derive `Encoding`

    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    This type does not fully implement the `Encoding` ability:

    5│  A := U8 implements [ Encoding {to_encoder ? my_encoder} ]
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The following necessary members are missing implementations:

        to_encoder
    "
    );

    test_report!(
        opaque_ability_impl_qualified,
        indoc!(
            r#"
            app "test" provides [A] to "./platform"

            MEq implements eq : a, a -> Bool where a implements MEq

            A := U8 implements [ MEq {eq : Bool.eq} ]
            "#
        ),
        @r"
    ── QUALIFIED ABILITY IMPLEMENTATION in /code/proj/Main.roc ─────────────────────

    This ability implementation is qualified:

    5│  A := U8 implements [ MEq {eq : Bool.eq} ]
                                       ^^^^^^^

    Custom implementations must be defined in the local scope, and
    unqualified.

    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    This type does not fully implement the `MEq` ability:

    5│  A := U8 implements [ MEq {eq : Bool.eq} ]
                             ^^^^^^^^^^^^^^^^^^

    The following necessary members are missing implementations:

        eq
    "
    );

    test_report!(
        opaque_ability_impl_not_identifier,
        indoc!(
            r#"
            app "test" provides [A] to "./platform"

            MEq implements eq : a, a -> Bool where a implements MEq

            A := U8 implements [ MEq {eq : \m, n -> m == n} ]
            "#
        ),
        @r"
    ── ABILITY IMPLEMENTATION NOT IDENTIFIER in /code/proj/Main.roc ────────────────

    This ability implementation is not an identifier:

    5│  A := U8 implements [ MEq {eq : \m, n -> m == n} ]
                                       ^^^^^^^^^^^^^^^

    Custom ability implementations defined in this position can only be
    unqualified identifiers, not arbitrary expressions.

    Tip: consider defining this expression as a variable.

    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    This type does not fully implement the `MEq` ability:

    5│  A := U8 implements [ MEq {eq : \m, n -> m == n} ]
                             ^^^^^^^^^^^^^^^^^^^^^^^^^^

    The following necessary members are missing implementations:

        eq
    "
    );

    test_report!(
        opaque_ability_impl_duplicate,
        indoc!(
            r#"
            app "test" provides [A] to "./platform"

            MEq implements eq : a, a -> Bool where a implements MEq

            A := U8 implements [ MEq {eq: eq_a, eq: eq_a} ]

            eq_a = \@A m, @A n -> m == n
            "#
        ),
        @r"
    ── DUPLICATE IMPLEMENTATION in /code/proj/Main.roc ─────────────────────────────

    This ability member implementation is duplicate:

    5│  A := U8 implements [ MEq {eq: eq_a, eq: eq_a} ]
                                            ^^^^^^^^

    The first implementation was defined here:

    5│  A := U8 implements [ MEq {eq: eq_a, eq: eq_a} ]
                                  ^^^^^^^^

    Only one custom implementation can be defined for an ability member.
    "
    );

    test_report!(
        implements_type_not_ability,
        indoc!(
            r#"
            app "test" provides [A, Foo] to "./platform"

            Foo := {}

            A := U8 implements [ Foo {} ]
            "#
        ),
        @r"
    ── NOT AN ABILITY in /code/proj/Main.roc ───────────────────────────────────────

    This identifier is not an ability in scope:

    5│  A := U8 implements [ Foo {} ]
                             ^^^

    Only abilities can be implemented.
    "
    );

    test_report!(
        derive_non_builtin_ability,
        indoc!(
            r#"
            app "test" provides [A] to "./platform"

            Ab implements ab : a -> a where a implements Ab

            A := {} implements [Ab]
            "#
        ),
        @r"
    ── ILLEGAL DERIVE in /code/proj/Main.roc ───────────────────────────────────────

    This ability cannot be derived:

    5│  A := {} implements [Ab]
                            ^^

    Only builtin abilities can be derived.

    Note: The builtin abilities are `Encoding`, `Decoding`, `Hash`, `Eq`, `Inspect`
    "
    );

    test_report!(
        has_encoding_for_function,
        indoc!(
            r#"
            app "test" imports [] provides [A] to "./platform"

            A a := a -> a implements [Encode.Encoding]
            "#
        ),
        @r"
    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Encoding` ability for `A`:

    3│  A a := a -> a implements [Encode.Encoding]
                                  ^^^^^^^^^^^^^^^

    Note: `Encoding` cannot be generated for functions.

    Tip: You can define a custom implementation of `Encoding` for `A`.
    "
    );

    test_report!(
        has_encoding_for_non_encoding_alias,
        indoc!(
            r#"
            app "test" imports [] provides [A] to "./platform"

            A := B implements [Encode.Encoding]

            B := {}
            "#
        ),
        @r"
    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Encoding` ability for `A`:

    3│  A := B implements [Encode.Encoding]
                           ^^^^^^^^^^^^^^^

    Tip: `B` does not implement `Encoding`. Consider adding a custom
    implementation or `implements Encode.Encoding` to the definition of `B`.

    Tip: You can define a custom implementation of `Encoding` for `A`.
    "
    );

    test_report!(
        has_encoding_for_other_has_encoding,
        indoc!(
            r#"
            app "test" imports [] provides [A] to "./platform"

            A := B implements [Encode.Encoding]

            B := {} implements [Encode.Encoding]
            "#
        ),
        @"" // no error
    );

    test_report!(
        has_encoding_for_recursive_deriving,
        indoc!(
            r#"
            app "test" imports [] provides [MyNat] to "./platform"

            MyNat := [S MyNat, Z] implements [Encode.Encoding]
            "#
        ),
        @"" // no error
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
        @r###"
    ── DUPLICATE NAME in /code/proj/Main.roc ───────────────────────────────────────

    The `main` name is first defined here:

    3│  main = 1
        ^^^^

    But then it's defined a second time here:

    5│  main = \n -> n + 2
        ^^^^

    Since these variables have the same name, it's easy to use the wrong
    one by accident. Give one of them a new name.

    ── UNNECESSARY DEFINITION in /code/proj/Main.roc ───────────────────────────────

    This destructure assignment doesn't introduce any new variables:

    5│  main = \n -> n + 2
        ^^^^

    If you don't need to use the value on the right-hand side of this
    assignment, consider removing the assignment. Since effects are not
    allowed at the top-level, assignments that don't introduce variables
    cannot affect a program's behavior
    "###
    );

    test_report!(
        issue_1755,
        indoc!(
            r"
            Handle := {}

            await : Result a err, (a -> Result b err) -> Result b err
            open : {} -> Result Handle *
            close : Handle -> Result {} *

            with_open : (Handle -> Result {} *) -> Result {} *
            with_open = \callback ->
                await (open {}) \handle ->
                    await (callback handle) \_ ->
                        close handle

            with_open
            "
        ),
        @r"
        ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

        Something is off with the body of the `with_open` definition:

        10│       with_open : (Handle -> Result {} *) -> Result {} *
        11│       with_open = \callback ->
        12│>          await (open {}) \handle ->
        13│>              await (callback handle) \_ ->
        14│>                  close handle

        The type annotation on `with_open` says this `await` call should have the
        type:

            Result {} *

        However, the type of this `await` call is connected to another type in a
        way that isn't reflected in this annotation.

        Tip: Any connection between types must use a named type variable, not
        a `*`! Maybe the annotation  on `with_open` should have a named type
        variable in place of the `*`?

        "
    );

    test_report!(
        recursive_body_and_annotation_with_inference_disagree,
        indoc!(
            r"
            f : _ -> (_ -> Str)
            f = \_ -> if Bool.true then {} else f {}

            f
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This returns something that's incompatible with the return type of the
    enclosing function:

    5│      f = \_ -> if Bool.true then {} else f {}
                      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    It a value of type:

        {}

    But I expected the function to have return type:

        * -> Str
    "
    );

    test_report!(
        same_phantom_types_unify,
        indoc!(
            r"
            F a b := b

            foo : F Str Str -> {}

            x : F Str Str

            foo x
            "
        ),
        @r"" // okay
    );

    test_report!(
        different_phantom_types,
        indoc!(
            r"
            F a b := b

            foo : F Str Str -> {}

            x : F U8 Str

            foo x
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `foo` has an unexpected type:

    10│      foo x
                 ^

    This `x` value is a:

        F U8 Str

    But `foo` needs its 1st argument to be:

        F Str Str
    "
    );

    test_report!(
        #[ignore = "TODO This should be a type error"]
        phantom_type_bound_to_ability_not_implementing,
        indoc!(
            r#"
            app "test" provides [x] to "./platform"

            Foo implements foo : a -> a where a implements Foo

            F a b := b where a implements Foo

            MHash := {}

            x : F MHash {}
            "#
        ),
        @r"
        "
    );

    test_report!(
        int_literals_cannot_fit_in_same_type,
        indoc!(
            r"
            0x80000000000000000000000000000000 == -0x80000000000000000000000000000000
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to == has an unexpected type:

    4│      0x80000000000000000000000000000000 == -0x80000000000000000000000000000000
                                                  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The argument is an integer of type:

        I128

    But == needs its 2nd argument to be:

        U128
    "
    );

    test_report!(
        num_literals_cannot_fit_in_same_type,
        indoc!(
            r"
            170141183460469231731687303715884105728 == -170141183460469231731687303715884105728
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to == has an unexpected type:

    4│      170141183460469231731687303715884105728 == -170141183460469231731687303715884105728
                                                       ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    The argument is a number of type:

        I128 or Dec

    But == needs its 2nd argument to be:

        U128
    "
    );

    test_report!(
        recursive_alias_cannot_leak_into_recursive_opaque,
        indoc!(
            r"
            OList := [Nil, Cons {} OList]

            AList : [Nil, Cons {} AList]

            alist : AList

            olist : OList
            olist =
                when alist is
                    Nil -> @OList Nil
                    Cons _ lst -> lst

            olist
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the 2nd branch of this `when` expression:

    10│      olist : OList
    11│      olist =
    12│          when alist is
    13│              Nil -> @OList Nil
    14│              Cons _ lst -> lst
                                   ^^^

    This `lst` value is a:

        [
            Cons {} ∞,
            Nil,
        ] as ∞

    But the type annotation on `olist` says it should be:

        OList

    Tip: *Add type annotations* to functions or values to help you figure
    this out.
    "
    );

    test_report!(
        opaque_wrap_function_mismatch,
        indoc!(
            r"
            A := U8
            List.map [1u16, 2u16, 3u16] @A
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to `map` has an unexpected type:

    4│      A := U8
    5│      List.map [1u16, 2u16, 3u16] @A
                                        ^^

    This A opaque wrapping has the type:

        U8 -> A

    But `map` needs its 2nd argument to be:

        U16 -> A
    "
    );

    test_report!(
        symbols_not_bound_in_all_patterns,
        indoc!(
            r#"
            when A "" is
                A x | B y -> x
            "#
        ),
        @r#"
        ── NAME NOT BOUND IN ALL PATTERNS in /code/proj/Main.roc ───────────────────────

        `x` is not bound in all patterns of this `when` branch

        5│          A x | B y -> x
                      ^

        Identifiers introduced in a `when` branch must be bound in all patterns
        of the branch. Otherwise, the program would crash when it tries to use
        an identifier that wasn't bound!

        ── NAME NOT BOUND IN ALL PATTERNS in /code/proj/Main.roc ───────────────────────

        `y` is not bound in all patterns of this `when` branch

        5│          A x | B y -> x
                            ^

        Identifiers introduced in a `when` branch must be bound in all patterns
        of the branch. Otherwise, the program would crash when it tries to use
        an identifier that wasn't bound!

        ── UNUSED DEFINITION in /code/proj/Main.roc ────────────────────────────────────

        `y` is not used in this `when` branch.

        5│          A x | B y -> x
                            ^

        If you don't need to use `y`, prefix it with an underscore, like "_y",
        or replace it with just an "_".
        "#
    );

    test_report!(
        issue_6279,
        indoc!(
            r#"
            when A "" is
                A x | B x | C -> x
            "#
        ),
        @r###"
        ── NAME NOT BOUND IN ALL PATTERNS in /code/proj/Main.roc ───────────────────────

        `x` is not bound in all patterns of this `when` branch

        5│          A x | B x | C -> x
                      ^

        Identifiers introduced in a `when` branch must be bound in all patterns
        of the branch. Otherwise, the program would crash when it tries to use
        an identifier that wasn't bound!
        "###
    );

    test_report!(
        issue_6825,
        indoc!(
            r#"
            when [] is
                [] | [_, .. as rest] if List.is_empty rest -> []
                _ -> []
            "#
        ),
        @r###"
        ── NAME NOT BOUND IN ALL PATTERNS in /code/proj/Main.roc ───────────────────────

        `rest` is not bound in all patterns of this `when` branch

        5│          [] | [_, .. as rest] if List.is_empty rest -> []
                                   ^^^^

        Identifiers introduced in a `when` branch must be bound in all patterns
        of the branch. Otherwise, the program would crash when it tries to use
        an identifier that wasn't bound!
        "###
    );

    test_report!(
        flip_flop_catch_all_branches_not_exhaustive,
        indoc!(
            r#"
            \x -> when x is
                    A B _ -> ""
                    A _ C -> ""
            "#
        ),
        @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    4│>      \x -> when x is
    5│>              A B _ -> ""
    6│>              A _ C -> ""

    Other possibilities include:

        A _ _

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        forgot_to_remove_underscore,
        indoc!(
            r"
            \_foo -> foo
            "
        ),
        |golden| pretty_assertions::assert_eq!(
            golden,
            indoc!(
                r"── UNRECOGNIZED NAME in /code/proj/Main.roc ────────────────────────────────────

                Nothing is named `foo` in this scope.

                4│      \_foo -> foo
                                 ^^^

                There is an ignored identifier of a similar name here:

                4│      \_foo -> foo
                         ^^^^

                Did you mean to remove the leading underscore?

                If not, did you mean one of these?

                    Box
                    Bool
                    U8
                    F64
                "
            ),
        )
    );

    test_report!(
        call_with_underscore_identifier,
        indoc!(
            r"
            f = \x, y, z -> x + y + z

            f 1 _ 1
            "
        ),
        |golden| pretty_assertions::assert_eq!(
            golden,
            indoc!(
                r"── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

                An underscore is being used as a variable here:

                6│      f 1 _ 1
                            ^

                An underscore can be used to ignore a value when pattern matching, but
                it cannot be used as a variable.
                "
            ),
        )
    );

    test_report!(
        call_with_declared_identifier_starting_with_underscore,
        indoc!(
            r"
            f = \x, y, z -> x + y + z

            \a, _b -> f a _b 1
            "
        ),
        |golden| pretty_assertions::assert_eq!(
            golden,
            indoc!(
                r"── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

                This variable's name starts with an underscore:

                6│      \a, _b -> f a _b 1
                            ^^

                But then it is used here:

                6│      \a, _b -> f a _b 1
                                      ^^

                A variable's name can only start with an underscore if the variable is
                unused. Since you are using this variable, you could remove the
                underscore from its name in both places.
                "
            ),
        )
    );

    test_report!(
        call_with_undeclared_identifier_starting_with_underscore,
        indoc!(
            r"
            f = \x, y, z -> x + y + z

            \a, _b -> f a _r 1
            "
        ),
        |golden| pretty_assertions::assert_eq!(
            golden,
            indoc!(
                r"
                ── SYNTAX PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

                This variable's name starts with an underscore:

                6│      \a, _b -> f a _r 1
                                      ^^

                A variable's name can only start with an underscore if the variable is
                unused. But it looks like the variable is being used here!
                "
            ),
        )
    );

    test_report!(
        call_with_declared_identifier_with_more_than_one_underscore,
        indoc!(
            r"
            f__arg = \x, y, z -> x + y + z

            \a, b -> f__arg a b 1
            "
        ),
        |golden| pretty_assertions::assert_eq!(
            golden,
            indoc!(
                r"── NAMING PROBLEM in /code/proj/Main.roc ───────────────────────────────────────

                I am trying to parse an identifier here:

                4│      f__arg = \x, y, z -> x + y + z
                        ^^^^^^

                Snake case is allowed here, but only a single consecutive underscore
                should be used.
                "
            ),
        )
    );

    // Record Builders

    test_report!(
        empty_record_builder,
        indoc!(
            r#"
            { a <- }
            "#
        ),
        @r#"
    ── EMPTY RECORD BUILDER in /code/proj/Main.roc ─────────────────────────────────

    This record builder has no fields:

    4│      { a <- }
            ^^^^^^^^

    I need at least two fields to combine their values into a record.
    "#
    );

    test_report!(
        single_field_record_builder,
        indoc!(
            r#"
            { a <-
                b: 123
            }
            "#
        ),
        @r#"
    ── NOT ENOUGH FIELDS IN RECORD BUILDER in /code/proj/Main.roc ──────────────────

    This record builder only has one field:

    4│>      { a <-
    5│>          b: 123
    6│>      }

    I need at least two fields to combine their values into a record.
    "#
    );

    test_report!(
        optional_field_in_record_builder,
        indoc!(
            r#"
            { a <-
                b: 123,
                c? 456
            }
            "#
        ),
        @r#"
    ── OPTIONAL FIELD IN RECORD BUILDER in /code/proj/Main.roc ─────────────────────

    Optional fields are not allowed to be used in record builders.

    4│       { a <-
    5│           b: 123,
    6│>          c? 456
    7│       }

    Record builders can only have required values for their fields.
    "#
    );

    // CalledVia::RecordBuilder => {
    //     alloc.concat([
    //         alloc.note(""),
    //         alloc.reflow("Record builders need a mapper function before the "),
    //         alloc.keyword("<-"),
    //         alloc.reflow(" to combine fields together with.")
    //     ])
    // }
    // _ => {
    //     alloc.reflow("Are there any missing commas? Or missing parentheses?")

    test_report!(
        record_builder_with_non_function_mapper,
        indoc!(
            r#"
            xyz = "abc"

            { xyz <-
                b: 123,
                c: 456
            }
            "#
        ),
        @r#"
    ── TOO MANY ARGS in /code/proj/Main.roc ────────────────────────────────────────

    The `xyz` value is not a function, but it was given 3 arguments:

    6│      { xyz <-
              ^^^

    Note: Record builders need a mapper function before the <- to combine
    fields together with.
    "#
    );

    test_report!(
        destructure_assignment_introduces_no_variables_nested,
        indoc!(
            r"
            Pair _ _ = Pair 0 1

            _ = Pair 0 1

            {} = {}

            Foo = Foo

            0
            "
        ),
        @r###"
    ── UNNECESSARY DEFINITION in /code/proj/Main.roc ───────────────────────────────

    This assignment doesn't introduce any new variables:

    4│      Pair _ _ = Pair 0 1
            ^^^^^^^^

    Since it doesn't call any effectful functions, this assignment cannot
    affect the program's behavior. If you don't need to use the value on
    the right-hand side, consider removing the assignment.

    ── UNNECESSARY DEFINITION in /code/proj/Main.roc ───────────────────────────────

    This assignment doesn't introduce any new variables:

    6│      _ = Pair 0 1
            ^

    Since it doesn't call any effectful functions, this assignment cannot
    affect the program's behavior. If you don't need to use the value on
    the right-hand side, consider removing the assignment.

    ── UNNECESSARY DEFINITION in /code/proj/Main.roc ───────────────────────────────

    This assignment doesn't introduce any new variables:

    8│      {} = {}
            ^^

    Since it doesn't call any effectful functions, this assignment cannot
    affect the program's behavior. If you don't need to use the value on
    the right-hand side, consider removing the assignment.

    ── UNNECESSARY DEFINITION in /code/proj/Main.roc ───────────────────────────────

    This assignment doesn't introduce any new variables:

    10│      Foo = Foo
             ^^^

    Since it doesn't call any effectful functions, this assignment cannot
    affect the program's behavior. If you don't need to use the value on
    the right-hand side, consider removing the assignment.
    "###
    );

    test_report!(
        destructure_assignment_introduces_no_variables_nested_toplevel,
        indoc!(
            r#"
            app "test" provides [] to "./platform"

            Pair _ _ = Pair 0 1

            _ = Pair 0 1

            {} = {}

            Foo = Foo
            "#
        ),
        @r###"
    ── UNNECESSARY DEFINITION in /code/proj/Main.roc ───────────────────────────────

    This destructure assignment doesn't introduce any new variables:

    3│  Pair _ _ = Pair 0 1
        ^^^^^^^^

    If you don't need to use the value on the right-hand side of this
    assignment, consider removing the assignment. Since effects are not
    allowed at the top-level, assignments that don't introduce variables
    cannot affect a program's behavior

    ── UNNECESSARY DEFINITION in /code/proj/Main.roc ───────────────────────────────

    This destructure assignment doesn't introduce any new variables:

    5│  _ = Pair 0 1
        ^

    If you don't need to use the value on the right-hand side of this
    assignment, consider removing the assignment. Since effects are not
    allowed at the top-level, assignments that don't introduce variables
    cannot affect a program's behavior

    ── UNNECESSARY DEFINITION in /code/proj/Main.roc ───────────────────────────────

    This destructure assignment doesn't introduce any new variables:

    7│  {} = {}
        ^^

    If you don't need to use the value on the right-hand side of this
    assignment, consider removing the assignment. Since effects are not
    allowed at the top-level, assignments that don't introduce variables
    cannot affect a program's behavior

    ── UNNECESSARY DEFINITION in /code/proj/Main.roc ───────────────────────────────

    This destructure assignment doesn't introduce any new variables:

    9│  Foo = Foo
        ^^^

    If you don't need to use the value on the right-hand side of this
    assignment, consider removing the assignment. Since effects are not
    allowed at the top-level, assignments that don't introduce variables
    cannot affect a program's behavior
    "###
    );

    test_report!(
        unused_shadow_specialization,
        indoc!(
            r#"
            app "test" provides [hash, Id] to "./platform"

            MHash implements hash : a -> U64 where a implements MHash

            Id := {}

            hash = \@Id _ -> 0
            "#
        ),
        @r"
    ── UNUSED DEFINITION in /code/proj/Main.roc ────────────────────────────────────

    `hash` is not used anywhere in your code.

    7│  hash = \@Id _ -> 0
        ^^^^

    If you didn't intend on using `hash` then remove it so future readers of
    your code don't wonder why it is there.
    "
    );

    test_report!(
        specialization_for_wrong_type,
        indoc!(
            r#"
            app "test" provides [hash, Id, Id2] to "./platform"

            MHash implements hash : a -> U64 where a implements MHash

            Id := {} implements [MHash {hash}]
            Id2 := {}

            hash = \@Id2 _ -> 0
            "#
        ),
        @r"
    ── WRONG SPECIALIZATION TYPE in /code/proj/Main.roc ────────────────────────────

    This specialization of `hash` is not for the expected type:

    8│  hash = \@Id2 _ -> 0
        ^^^^

    It was previously claimed to be a specialization for `Id`, but was
    determined to actually specialize `Id2`!
    "
    );

    test_report!(
        mismatched_record_annotation,
        indoc!(
            r"
                x : { y : Str }
                x = {}

                x
                "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `x` definition:

    4│      x : { y : Str }
    5│      x = {}
                ^^

    The body is a record of type:

        {}

    But the type annotation on `x` says it should be:

        { y : Str }

    Tip: Looks like the y field is missing.
    "
    );

    test_report!(
        cyclic_opaque,
        indoc!(
            r"
            Recursive := [Infinitely Recursive]

            0
            "
        ),
        @r"
    ── CYCLIC ALIAS in /code/proj/Main.roc ─────────────────────────────────────────

    The `Recursive` opaque is self-recursive in an invalid way:

    4│      Recursive := [Infinitely Recursive]
            ^^^^^^^^^

    Recursion in opaque types is only allowed if recursion happens behind
    a tagged union, at least one variant of which is not recursive.
    "
    );

    test_report!(
        derive_decoding_for_function,
        indoc!(
            r#"
            app "test" imports [] provides [A] to "./platform"

            A a := a -> a implements [Decode.Decoding]
            "#
        ),
        @r"
    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Decoding` ability for `A`:

    3│  A a := a -> a implements [Decode.Decoding]
                                  ^^^^^^^^^^^^^^^

    Note: `Decoding` cannot be generated for functions.

    Tip: You can define a custom implementation of `Decoding` for `A`.
    "
    );

    test_report!(
        derive_decoding_for_non_decoding_opaque,
        indoc!(
            r#"
            app "test" imports [] provides [A] to "./platform"

            A := B implements [Decode.Decoding]

            B := {}
            "#
        ),
        @r"
    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Decoding` ability for `A`:

    3│  A := B implements [Decode.Decoding]
                           ^^^^^^^^^^^^^^^

    Tip: `B` does not implement `Decoding`. Consider adding a custom
    implementation or `implements Decode.Decoding` to the definition of `B`.

    Tip: You can define a custom implementation of `Decoding` for `A`.

    "
    );

    test_report!(
        derive_decoding_for_other_has_decoding,
        indoc!(
            r#"
            app "test" imports [] provides [A] to "./platform"

            A := B implements [Decode.Decoding]

            B := {} implements [Decode.Decoding]
            "#
        ),
        @"" // no error
    );

    test_report!(
        derive_decoding_for_recursive_deriving,
        indoc!(
            r#"
            app "test" imports [] provides [MyNat] to "./platform"

            MyNat := [S MyNat, Z] implements [Decode.Decoding]
            "#
        ),
        @"" // no error
    );

    test_report!(
        function_cannot_derive_encoding,
        indoc!(
            r#"
            app "test" imports [] provides [main] to "./platform"

            import Decode exposing [decoder]

            my_decoder : Decoder (_ -> _) _
            my_decoder = decoder

            main =
                my_decoder
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    6│  my_decoder = decoder
                     ^^^^^^^

    I can't generate an implementation of the `Decoding` ability for

        * -> *

    Note: `Decoding` cannot be generated for functions.
    "
    );

    test_report!(
        nested_opaque_cannot_derive_encoding,
        indoc!(
            r#"
            app "test" imports [] provides [main] to "./platform"

            import Decode exposing [decoder]

            A := {}

            my_decoder : Decoder {x : A} _
            my_decoder = decoder

            main =
                my_decoder
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    8│  my_decoder = decoder
                     ^^^^^^^

    I can't generate an implementation of the `Decoding` ability for

        { x : A }

    In particular, an implementation for

        A

    cannot be generated.

    Tip: `A` does not implement `Decoding`. Consider adding a custom
    implementation or `implements Decode.Decoding` to the definition of `A`.
    "
    );

    test_report!(
        anonymous_function_does_not_use_param,
        indoc!(
            r"
            (\x -> 5) 1
            "
        ),
    @r#"
    ── UNUSED ARGUMENT in /code/proj/Main.roc ──────────────────────────────────────

    This function doesn't use `x`.

    4│      (\x -> 5) 1
              ^

    If you don't need `x`, then you can just remove it. However, if you
    really do need `x` as an argument of this function, prefix it with an
    underscore, like this: "_`x`". Adding an underscore at the start of a
    variable name is a way of saying that the variable is not used.
    "#
    );

    test_report!(
        expected_tag_has_too_many_args,
        indoc!(
            r#"
            app "test" provides [from_bytes] to "./platform"

            u8 : [Good (List U8), Bad [DecodeProblem]]

            from_bytes =
                when u8 is
                    Good _ _ ->
                        Ok "foo"

                    Bad _ ->
                        Ok "foo"
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The branches of this `when` expression don't match the condition:

     6│>      when u8 is
     7│           Good _ _ ->
     8│               Ok "foo"
     9│
    10│           Bad _ ->
    11│               Ok "foo"

    This `u8` value is a:

        [Good …, …]

    But the branch patterns have type:

        [Good … *, …]

    The branches must be cases of the `when` condition's type!
    "#
    );

    test_report!(
        create_value_with_optional_record_field_type,
        indoc!(
            r#"
            f : {a: Str, b ? Str}
            f = {a: "b", b: ""}
            f
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    4│      f : {a: Str, b ? Str}
    5│      f = {a: "b", b: ""}
                ^^^^^^^^^^^^^^^

    The body is a record of type:

        {
            a : Str,
            b : Str,
        }

    But the type annotation on `f` says it should be:

        {
            a : Str,
            b ? Str,
        }

    Tip: To extract the `.b` field it must be non-optional, but the type
    says this field is optional. Learn more about optional fields at TODO.
    "#
    );

    test_report!(
        create_value_with_conditionally_optional_record_field_type,
        indoc!(
            r#"
            f : {a: Str, b ? Str}
            f = if Bool.true then {a: ""} else {a: "b", b: ""}
            f
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the `then` branch of this `if` expression:

    4│      f : {a: Str, b ? Str}
    5│      f = if Bool.true then {a: ""} else {a: "b", b: ""}
                                  ^^^^^^^

    This branch is a record of type:

        { a : Str }

    But the type annotation on `f` says it should be:

        {
            a : Str,
            b ? Str,
        }

    Tip: Looks like the b field is missing.
    "#
    );

    test_report!(
        unused_def_in_branch_pattern,
        indoc!(
            r#"
            when A "" is
                A foo -> ""
            "#
        ),
    @r#"
    ── UNUSED DEFINITION in /code/proj/Main.roc ────────────────────────────────────

    `foo` is not used in this `when` branch.

    5│          A foo -> ""
                  ^^^

    If you don't need to use `foo`, prefix it with an underscore, like
    "_foo", or replace it with just an "_".
    "#
    );

    test_report!(
        infer_decoded_record_error_with_function_field,
        indoc!(
            r#"
            app "test" imports [] provides [main] to "./platform"

            ErrDecoder := {} implements [DecoderFormatting {
                u8: decode_u8,
                u16: decode_u16,
                u32: decode_u32,
                u64: decode_u64,
                u128: decode_u128,
                i8: decode_i8,
                i16: decode_i16,
                i32: decode_i32,
                i64: decode_i64,
                i128: decode_i128,
                f32: decode_f32,
                f64: decode_f64,
                dec: decode_dec,
                bool: decode_bool,
                string: decode_string,
                list: decode_list,
                record: decode_record,
                tuple: decode_tuple,
            }]
            decode_u8 = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_u16 = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_u32 = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_u64 = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_u128 = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_i8 = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_i16 = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_i32 = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_i64 = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_i128 = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_f32 = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_f64 = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_dec = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_bool = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_string = Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_list : Decoder elem (ErrDecoder) -> Decoder (List elem) (ErrDecoder)
            decode_list = \_ -> Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_record : state, (state, Str -> [Keep (Decoder state (ErrDecoder)), Skip]), (state, (ErrDecoder) -> Result val DecodeError) -> Decoder val (ErrDecoder)
            decode_record =\_, _, _ ->  Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}
            decode_tuple : state, (state, U64 -> [Next (Decoder state (ErrDecoder)), TooLong]), (state -> Result val DecodeError) -> Decoder val (ErrDecoder)
            decode_tuple = \_, _, _ -> Decode.custom \rest, @ErrDecoder {} -> {result: Err TooShort, rest}

            main =
                decoded = Str.to_utf8 "{\"first\":\"ab\",\"second\":\"cd\"}" |> Decode.from_bytes (@ErrDecoder {})
                when decoded is
                    Ok rcd -> rcd.first rcd.second
                    _ -> "something went wrong"
            "#
        ),
    @r###"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    48│          Ok rcd -> rcd.first rcd.second
                           ^^^^^^^^^

    I can't generate an implementation of the `Decoding` ability for

        * -> *

    Note: `Decoding` cannot be generated for functions.
    "###
    );

    test_report!(
        record_with_optional_field_types_cannot_derive_decoding,
        indoc!(
            r#"
             app "test" imports [] provides [main] to "./platform"

             import Decode exposing [decoder]

             my_decoder : Decoder {x : Str, y ? Str} _
             my_decoder = decoder

             main = my_decoder
             "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    6│  my_decoder = decoder
                     ^^^^^^^

    I can't generate an implementation of the `Decoding` ability for

        {
            x : Str,
            y ? Str,
        }

    Note: I can't derive decoding for a record with an optional field,
    which in this case is `.y`. Default value record fields are polymorphic
    over records that may or may not contain them at compile time, but are
    not a concept that extends to runtime!
    Maybe you wanted to use a `Result`?
    "
    );

    test_report!(
        uninhabited_type_is_trivially_exhaustive,
        indoc!(
            r#"
            x : Result {} []

            when x is
                Ok {} -> ""
            "#
        ),
    // no problem!
    @r"
    "
    );

    test_report!(
        uninhabited_type_is_trivially_exhaustive_nested,
        indoc!(
            r#"
            x : Result (Result [A, B] []) []

            when x is
                Ok (Ok A) -> ""
                Ok (Ok B) -> ""
            "#
        ),
    // no problem!
    @r"
    "
    );

    test_report!(
        branch_patterns_missing_nested_case,
        indoc!(
            r#"
            x : Result (Result [A, B] {}) {}

            when x is
                Ok (Ok A) -> ""
                Ok (Err _) -> ""
                Err _ -> ""
            "#
        ),
    @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    6│>      when x is
    7│>          Ok (Ok A) -> ""
    8│>          Ok (Err _) -> ""
    9│>          Err _ -> ""

    Other possibilities include:

        Ok (Ok B)

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        branch_patterns_missing_nested_case_with_trivially_exhausted_variant,
        indoc!(
            r#"
            x : Result (Result [A, B] []) []

            when x is
                Ok (Ok A) -> ""
            "#
        ),
    @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    6│>      when x is
    7│>          Ok (Ok A) -> ""

    Other possibilities include:

        Ok (Ok B)

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        uninhabited_err_branch_is_redundant_when_err_is_matched,
        indoc!(
            r#"
            x : Result {} []

            when x is
                Ok {} -> ""
                Err _ -> ""
            "#
        ),
    @r#"
    ── UNMATCHABLE PATTERN in /code/proj/Main.roc ──────────────────────────────────

    The 2nd pattern will never be matched:

    6│      when x is
    7│          Ok {} -> ""
    8│          Err _ -> ""
                ^^^^^

    It's impossible to create a value of this shape, so this pattern can
    be safely removed!
    "#
    );

    test_report!(
        uninhabited_err_branch_is_redundant_when_err_is_matched_nested,
        indoc!(
            r#"
            x : Result (Result {} []) []

            when x is
                Ok (Ok {}) -> ""
                Ok (Err _) -> ""
                Err _ -> ""
            "#
        ),
    @r#"
    ── UNMATCHABLE PATTERN in /code/proj/Main.roc ──────────────────────────────────

    The 2nd pattern will never be matched:

    6│       when x is
    7│           Ok (Ok {}) -> ""
    8│>          Ok (Err _) -> ""
    9│           Err _ -> ""

    It's impossible to create a value of this shape, so this pattern can
    be safely removed!

    ── UNMATCHABLE PATTERN in /code/proj/Main.roc ──────────────────────────────────

    The 3rd pattern will never be matched:

    6│      when x is
    7│          Ok (Ok {}) -> ""
    8│          Ok (Err _) -> ""
    9│          Err _ -> ""
                ^^^^^

    It's impossible to create a value of this shape, so this pattern can
    be safely removed!
    "#
    );

    test_report!(
        custom_type_conflicts_with_builtin,
        indoc!(
            r#"
            Dec := [ S Dec, Z ]

            ""
            "#
        ),
    @r"
    ── DUPLICATE NAME in /code/proj/Main.roc ───────────────────────────────────────

    This opaque type has the same name as a builtin:

    4│      Dec := [ S Dec, Z ]
            ^^^^^^^^^^^^^^^^^^^

    All builtin opaque types are in scope by default, so I need this
    opaque type to have a different name!
    "
    );

    test_report!(
        unused_value_import,
        indoc!(
            r#"
            app "test" imports [] provides [main] to "./platform"

            import List exposing [concat]

            main = ""
            "#
        ),
    @r###"
    ── UNUSED IMPORT in /code/proj/Main.roc ────────────────────────────────────────

    List is imported but not used.

    3│  import List exposing [concat]
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    Since List isn't used, you don't need to import it.
    "###
    );

    test_report!(
        #[ignore = "https://github.com/roc-lang/roc/issues/4096"]
        unnecessary_builtin_module_import,
        indoc!(
            r#"
            app "test" imports [Str] provides [main] to "./platform"

            main = Str.concat "" ""
            "#
        ),
    @r"
    "
    );

    test_report!(
        #[ignore = "https://github.com/roc-lang/roc/issues/4096"]
        unnecessary_builtin_type_import,
        indoc!(
            r#"
            app "test" imports [] provides [main, E] to "./platform"

            import Decode exposing [DecodeError]

            E : DecodeError

            main = ""
            "#
        ),
    @r"
    "
    );

    test_report!(
        unknown_shorthand_no_deps,
        indoc!(
            r#"
            import foo.Foo

            Foo.foo
            "#
        ),
        @r###"
    ── UNRECOGNIZED PACKAGE in tmp/unknown_shorthand_no_deps/Test.roc ──────────────

    This module is trying to import from `foo`:

    4│      import foo.Foo
                   ^^^^^^^

    A lowercase name indicates a package shorthand, but no packages have
    been specified.
    "###
    );

    test_report!(
        unknown_shorthand_in_app,
        indoc!(
            r#"
            app [main] { pf: platform "../../tests/platform.roc" }

            import foo.Foo

            main =
                Foo.foo
            "#
        ),
        @r###"
    ── UNRECOGNIZED PACKAGE in tmp/unknown_shorthand_in_app/Test.roc ───────────────

    This module is trying to import from `foo`:

    3│  import foo.Foo
               ^^^^^^^

    A lowercase name indicates a package shorthand, but I don't recognize
    this one. Did you mean one of these?

        pf
    "###
    );

    test_report!(
        import_qualified_builtin,
        indoc!(
            r#"
            app [main] { pf: platform "../../tests/platform.roc" }

            import pf.Bool

            main =
                ""
            "#
        ),
        @r###"
    [1;36m── FILE NOT FOUND in tmp/import_qualified_builtin/../../tests/Bool.roc ─────────[0m

    I am looking for this file, but it's not there:

        [1;33mtmp/import_qualified_builtin/../../tests/Bool.roc[0m

    Is the file supposed to be there? Maybe there is a typo in the file
    name?
    "###
    );

    test_report!(
        invalid_toplevel_cycle,
        indoc!(
            r#"
            app "test" imports [] provides [main] to "./platform"

            main =
                if Bool.true then {} else main
            "#
        ),
    @r"
    ── CIRCULAR DEFINITION in /code/proj/Main.roc ──────────────────────────────────

    `main` is defined directly in terms of itself:

    3│>  main =
    4│>      if Bool.true then {} else main

    Roc evaluates values strictly, so running this program would enter an
    infinite loop!

    Hint: Did you mean to define `main` as a function?
    "
    );

    test_report!(
        bool_vs_true_tag,
        indoc!(
            r#"
            if True then "" else ""
            "#
        ),
    @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This `if` condition needs to be a Bool:

    4│      if True then "" else ""
               ^^^^

    This `True` tag has the type:

        [True]

    But I need every `if` condition to evaluate to a Bool—either `Bool.true`
    or `Bool.false`.

    Tip: Did you mean to use `Bool.true` rather than `True`?
    "#
    );

    test_report!(
        bool_vs_false_tag,
        indoc!(
            r#"
            if False then "" else ""
            "#
        ),
    @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This `if` condition needs to be a Bool:

    4│      if False then "" else ""
               ^^^^^

    This `False` tag has the type:

        [False]

    But I need every `if` condition to evaluate to a Bool—either `Bool.true`
    or `Bool.false`.

    Tip: Did you mean to use `Bool.false` rather than `False`?
    "#
    );

    test_report!(
        derive_hash_for_function,
        indoc!(
            r#"
             app "test" provides [A] to "./platform"

             A a := a -> a implements [Hash]
             "#
        ),
        @r"
    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Hash` ability for `A`:

    3│  A a := a -> a implements [Hash]
                                  ^^^^

    Note: `Hash` cannot be generated for functions.

    Tip: You can define a custom implementation of `Hash` for `A`.
    "
    );

    test_report!(
        derive_hash_for_non_hash_opaque,
        indoc!(
            r#"
             app "test" provides [A] to "./platform"

             A := B implements [Hash]

             B := {}
             "#
        ),
        @r"
    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Hash` ability for `A`:

    3│  A := B implements [Hash]
                           ^^^^

    Tip: `B` does not implement `Hash`. Consider adding a custom
    implementation or `implements Hash.Hash` to the definition of `B`.

    Tip: You can define a custom implementation of `Hash` for `A`.

    "
    );

    test_report!(
        derive_hash_for_other_has_hash,
        indoc!(
            r#"
             app "test" provides [A] to "./platform"

             A := B implements [Hash]

             B := {} implements [Hash]
             "#
        ),
        @"" // no error
    );

    test_report!(
        derive_hash_for_recursive_deriving,
        indoc!(
            r#"
             app "test" provides [MyNat] to "./platform"

             MyNat := [S MyNat, Z] implements [Hash]
             "#
        ),
        @"" // no error
    );

    test_report!(
        derive_hash_for_record,
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             foo : a -> {} where a implements Hash

             main = foo {a: "", b: 1}
             "#
        ),
        @"" // no error
    );

    test_report!(
        derive_hash_for_tag,
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             foo : a -> {} where a implements Hash

             t : [A {}, B U8 U64, C Str]

             main = foo t
             "#
        ),
        @"" // no error
    );

    test_report!(
        cannot_derive_hash_for_function,
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             foo : a -> {} where a implements Hash

             main = foo (\x -> x)
             "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    5│  main = foo (\x -> x)
                    ^^^^^^^

    I can't generate an implementation of the `Hash` ability for

        a -> a

    Note: `Hash` cannot be generated for functions.
    "
    );

    test_report!(
        cannot_derive_hash_for_structure_containing_function,
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             foo : a -> {} where a implements Hash

             main = foo (A (\x -> x) B)
             "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    5│  main = foo (A (\x -> x) B)
                    ^^^^^^^^^^^^^

    I can't generate an implementation of the `Hash` ability for

        [A (a -> a) [B]a]

    In particular, an implementation for

        a -> a

    cannot be generated.

    Note: `Hash` cannot be generated for functions.
    "
    );

    test_no_problem!(
        derive_hash_for_tuple,
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             foo : a -> {} where a implements Hash

             main = foo ("", 1)
             "#
        )
    );

    test_report!(
        cannot_hash_tuple_with_non_hash_element,
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             foo : a -> {} where a implements Hash

             main = foo ("", \{} -> {})
             "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    5│  main = foo ("", \{} -> {})
                   ^^^^^^^^^^^^^^^

    I can't generate an implementation of the `Hash` ability for

        (
            Str,
            {}a -> {},
        )a

    In particular, an implementation for

        {}a -> {}

    cannot be generated.

    Note: `Hash` cannot be generated for functions.
    "#
    );

    test_report!(
        shift_by_negative,
        indoc!(
            r"
            {
                a: Num.shift_left_by 1 -1,
                b: Num.shift_right_by 1 -1,
                c: Num.shift_right_zf_by 1 -1,
            }
            "
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to `shift_right_zf_by` has an unexpected type:

    7│          c: Num.shift_right_zf_by 1 -1,
                                           ^^

    The argument is a number of type:

        I8, I16, F32, I32, F64, I64, I128, or Dec

    But `shift_right_zf_by` needs its 2nd argument to be:

        U8

    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to `shift_right_by` has an unexpected type:

    6│          b: Num.shift_right_by 1 -1,
                                        ^^

    The argument is a number of type:

        I8, I16, F32, I32, F64, I64, I128, or Dec

    But `shift_right_by` needs its 2nd argument to be:

        U8

    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to `shift_left_by` has an unexpected type:

    5│          a: Num.shift_left_by 1 -1,
                                       ^^

    The argument is a number of type:

        I8, I16, F32, I32, F64, I64, I128, or Dec

    But `shift_left_by` needs its 2nd argument to be:

        U8
    "
    );

    test_report!(
        big_char_does_not_fit_in_u8,
        indoc!(
            r"
            digits : List U8
            digits = List.range { start: At '0', end: At '9' }

            List.contains digits '☃'
            "
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to `contains` has an unexpected type:

    7│      List.contains digits '☃'
                                 ^^^^^

    The argument is a Unicode scalar value of type:

        U16, I32, U32, I64, U64, I128, or U128

    But `contains` needs its 2nd argument to be:

        Int Unsigned8
    "
    );

    test_report!(
        derive_eq_for_function,
        indoc!(
            r#"
             app "test" provides [A] to "./platform"

             A a := a -> a implements [Eq]
             "#
        ),
        @r"
    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Eq` ability for `A`:

    3│  A a := a -> a implements [Eq]
                                  ^^

    Note: `Eq` cannot be generated for functions.

    Tip: You can define a custom implementation of `Eq` for `A`.
    "
    );

    test_report!(
        big_char_does_not_fit_in_u8_pattern,
        indoc!(
            r#"
            x : U8

            when x is
                '☃' -> ""
                _ -> ""
            "#
        ),
    @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The branches of this `when` expression don't match the condition:

    6│>      when x is
    7│           '☃' -> ""
    8│           _ -> ""

    This `x` value is a:

        U8

    But the branch patterns have type:

        U16, I32, U32, I64, U64, I128, or U128

    The branches must be cases of the `when` condition's type!
    "#
    );

    test_report!(
        derive_eq_for_f32,
        indoc!(
            r#"
             app "test" provides [A] to "./platform"

             A := F32 implements [Eq]
             "#
        ),
        @r"
    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Eq` ability for `A`:

    3│  A := F32 implements [Eq]
                             ^^

    Note: I can't derive `Bool.is_eq` for floating-point types. That's
    because Roc's floating-point numbers cannot be compared for total
    equality - in Roc, `NaN` is never comparable to `NaN`. If a type
    doesn't support total equality, it cannot support the `Eq` ability!

    Tip: You can define a custom implementation of `Eq` for `A`.
    "
    );

    test_report!(
        derive_eq_for_f64,
        indoc!(
            r#"
             app "test" provides [A] to "./platform"

             A := F64 implements [Eq]
             "#
        ),
        @r"
    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Eq` ability for `A`:

    3│  A := F64 implements [Eq]
                             ^^

    Note: I can't derive `Bool.is_eq` for floating-point types. That's
    because Roc's floating-point numbers cannot be compared for total
    equality - in Roc, `NaN` is never comparable to `NaN`. If a type
    doesn't support total equality, it cannot support the `Eq` ability!

    Tip: You can define a custom implementation of `Eq` for `A`.
    "
    );

    test_report!(
        derive_eq_for_non_eq_opaque,
        indoc!(
            r#"
             app "test" provides [A] to "./platform"

             A := B implements [Eq]

             B := {}
             "#
        ),
        @r"
    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Eq` ability for `A`:

    3│  A := B implements [Eq]
                           ^^

    Tip: `B` does not implement `Eq`. Consider adding a custom implementation
    or `implements Bool.Eq` to the definition of `B`.

    Tip: You can define a custom implementation of `Eq` for `A`.

    "
    );

    test_report!(
        derive_eq_for_other_has_eq,
        indoc!(
            r#"
             app "test" provides [A] to "./platform"

             A := B implements [Eq]

             B := {} implements [Eq]
             "#
        ),
        @"" // no error
    );

    test_report!(
        derive_eq_for_recursive_deriving,
        indoc!(
            r#"
             app "test" provides [MyNat] to "./platform"

             MyNat := [S MyNat, Z] implements [Eq]
             "#
        ),
        @"" // no error
    );

    test_report!(
        derive_eq_for_record,
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             foo : a -> {} where a implements Eq

             main = foo {a: "", b: 1}
             "#
        ),
        @"" // no error
    );

    test_report!(
        derive_eq_for_tag,
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             foo : a -> {} where a implements Eq

             t : [A {}, B U8 U64, C Str]

             main = foo t
             "#
        ),
        @"" // no error
    );

    test_report!(
        cannot_derive_eq_for_function,
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             foo : a -> {} where a implements Eq

             main = foo (\x -> x)
             "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    5│  main = foo (\x -> x)
                    ^^^^^^^

    I can't generate an implementation of the `Eq` ability for

        a -> a

    Note: `Eq` cannot be generated for functions.
    "
    );

    test_report!(
        cannot_derive_eq_for_structure_containing_function,
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             foo : a -> {} where a implements Eq

             main = foo (A (\x -> x) B)
             "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    5│  main = foo (A (\x -> x) B)
                    ^^^^^^^^^^^^^

    I can't generate an implementation of the `Eq` ability for

        [A (a -> a) [B]a]

    In particular, an implementation for

        a -> a

    cannot be generated.

    Note: `Eq` cannot be generated for functions.
    "
    );

    test_report!(
        cannot_eq_functions,
        indoc!(
            r"
            (\x -> x) == (\x -> x)
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    4│      (\x -> x) == (\x -> x)
             ^^^^^^^

    I can't generate an implementation of the `Eq` ability for

        a -> a

    Note: `Eq` cannot be generated for functions.
    "
    );

    test_report!(
        cannot_not_eq_functions,
        indoc!(
            r"
            (\x -> x) == (\x -> x)
            "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    4│      (\x -> x) == (\x -> x)
             ^^^^^^^

    I can't generate an implementation of the `Eq` ability for

        a -> a

    Note: `Eq` cannot be generated for functions.
    "
    );

    test_no_problem!(
        derive_eq_for_tuple,
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             foo : a -> {} where a implements Eq

             main = foo ("", 1)
             "#
        )
    );

    test_report!(
        cannot_eq_tuple_with_non_eq_element,
        indoc!(
            r#"
             app "test" provides [main] to "./platform"

             foo : a -> {} where a implements Eq

             main = foo ("", 1.0f64)
             "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    5│  main = foo ("", 1.0f64)
                   ^^^^^^^^^^^^

    I can't generate an implementation of the `Eq` ability for

        (
            Str,
            F64,
        )a

    In particular, an implementation for

        F64

    cannot be generated.

    Note: I can't derive `Bool.is_eq` for floating-point types. That's
    because Roc's floating-point numbers cannot be compared for total
    equality - in Roc, `NaN` is never comparable to `NaN`. If a type
    doesn't support total equality, it cannot support the `Eq` ability!
    "#
    );

    test_report!(
        cannot_import_structural_eq_not_eq,
        indoc!(
            r"
            {
                a: Bool.structural_eq,
                b: Bool.structural_not_eq,
            }
            "
        ),
        @r"
    ── NOT EXPOSED in /code/proj/Main.roc ──────────────────────────────────────────

    The Bool module does not expose `structural_eq`:

    5│          a: Bool.structural_eq,
                   ^^^^^^^^^^^^^^^^^^

    Did you mean one of these?

        Bool.true
        Bool.is_not_eq
        Bool.false
        Bool.is_eq

    ── NOT EXPOSED in /code/proj/Main.roc ──────────────────────────────────────────

    The Bool module does not expose `structural_not_eq`:

    6│          b: Bool.structural_not_eq,
                   ^^^^^^^^^^^^^^^^^^^^^^

    Did you mean one of these?

        Bool.is_not_eq
        Bool.bool_is_eq
        Bool.true
        Bool.false
    "
    );

    test_report!(
        expand_ability_from_type_alias_mismatch,
        indoc!(
            r#"
            app "test" provides [f] to "./platform"

            F a : a where a implements Hash

            f : F ({} -> {})
            "#
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    5│  f : F ({} -> {})
               ^^^^^^^^

    I can't generate an implementation of the `Hash` ability for

        {} -> {}

    Note: `Hash` cannot be generated for functions.
    "
    );

    test_report!(
        demanded_vs_optional_record_field,
        indoc!(
            r#"
            foo : { a : Str } -> Str
            foo = \{ a ? "" } -> a
            foo
            "#
        ),
    @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The 1st argument to `foo` is weird:

    5│      foo = \{ a ? "" } -> a
                   ^^^^^^^^^^

    The argument is a pattern that matches record values of type:

        { a ? Str }

    But the annotation on `foo` says the 1st argument should be:

        { a : Str }
    "#
    );

    test_report!(
        underivable_opaque_doesnt_error_for_derived_bodies,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            F := U8 -> U8 implements [Hash, Eq, Encoding]

            main = ""
            "#
        ),
    @r"
    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Hash` ability for `F`:

    3│  F := U8 -> U8 implements [Hash, Eq, Encoding]
                                  ^^^^

    Note: `Hash` cannot be generated for functions.

    Tip: You can define a custom implementation of `Hash` for `F`.

    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Eq` ability for `F`:

    3│  F := U8 -> U8 implements [Hash, Eq, Encoding]
                                        ^^

    Note: `Eq` cannot be generated for functions.

    Tip: You can define a custom implementation of `Eq` for `F`.

    ── INCOMPLETE ABILITY IMPLEMENTATION in /code/proj/Main.roc ────────────────────

    I can't derive an implementation of the `Encoding` ability for `F`:

    3│  F := U8 -> U8 implements [Hash, Eq, Encoding]
                                            ^^^^^^^^

    Note: `Encoding` cannot be generated for functions.

    Tip: You can define a custom implementation of `Encoding` for `F`.
    "
    );

    test_report!(
        duplicate_ability_in_has_clause,
        indoc!(
            r"
            f : a -> {} where a implements Hash & Hash

            f
            "
        ),
    @r"
    ── DUPLICATE BOUND ABILITY in /code/proj/Main.roc ──────────────────────────────

    I already saw that this type variable is bound to the `Hash` ability
    once before:

    4│      f : a -> {} where a implements Hash & Hash
                                                  ^^^^

    Abilities only need to bound to a type variable once in an `implements`
    clause!
    "
    );

    test_report!(
        rigid_able_bounds_must_be_a_superset_of_flex_bounds,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            g : x -> x where x implements Decoding & Encoding

            main : x -> x where x implements Encoding
            main = \x -> g x
            "#
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `g` has an unexpected type:

    6│  main = \x -> g x
                       ^

    This `x` value is a:

        x where x implements Encoding

    But `g` needs its 1st argument to be:

        x where x implements Encoding & Decoding

    Note: The type variable `x` says it can take on any value that
    implements only the ability `Encoding`.

    But, I see that it's also used as if it implements the ability
    `Decoding`. Can you use `x` without that ability? If not, consider adding
    it to the `implements` clause of `x`.
    "
    );

    test_report!(
        rigid_able_bounds_must_be_a_superset_of_flex_bounds_multiple,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            g : x -> x where x implements Decoding & Encoding & Hash

            main : x -> x where x implements Encoding
            main = \x -> g x
            "#
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `g` has an unexpected type:

    6│  main = \x -> g x
                       ^

    This `x` value is a:

        x where x implements Encoding

    But `g` needs its 1st argument to be:

        x where x implements Hash & Encoding & Decoding

    Note: The type variable `x` says it can take on any value that
    implements only the ability `Encoding`.

    But, I see that it's also used as if it implements the abilities `Hash`
    and `Decoding`. Can you use `x` without those abilities? If not, consider
    adding them to the `implements` clause of `x`.
    "
    );

    test_report!(
        rigid_able_bounds_must_be_a_superset_of_flex_bounds_with_indirection,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            f : x -> x where x implements Hash
            g : x -> x where x implements Decoding & Encoding

            main : x -> x where x implements Hash & Encoding
            main = \x -> g (f x)
            "#
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `g` has an unexpected type:

    7│  main = \x -> g (f x)
                        ^^^

    This `f` call produces:

        x where x implements Hash & Encoding

    But `g` needs its 1st argument to be:

        x where x implements Encoding & Decoding

    Note: The type variable `x` says it can take on any value that
    implements only the abilities `Hash` and `Encoding`.

    But, I see that it's also used as if it implements the ability
    `Decoding`. Can you use `x` without that ability? If not, consider adding
    it to the `implements` clause of `x`.
    "
    );

    test_report!(
        list_pattern_not_terminated,
        indoc!(
            r#"
            when [] is
                [1, 2, -> ""
            "#
        ),
    @r#"
    ── UNFINISHED LIST PATTERN in tmp/list_pattern_not_terminated/Test.roc ─────────

    I am partway through parsing a list pattern, but I got stuck here:

    5│          [1, 2, -> ""
                       ^

    I was expecting to see a closing square brace before this, so try
    adding a ] and see if that helps?
    "#
    );

    test_report!(
        list_pattern_weird_rest_pattern,
        indoc!(
            r#"
            when [] is
                [...] -> ""
            "#
        ),
    @r#"
    ── INCORRECT REST PATTERN in tmp/list_pattern_weird_rest_pattern/Test.roc ──────

    It looks like you may trying to write a list rest pattern, but it's
    not the form I expect:

    5│          [...] -> ""
                 ^

    List rest patterns, which match zero or more elements in a list, are
    denoted with .. - is that what you meant?
    "#
    );

    test_report!(
        unnecessary_extension_variable,
        indoc!(
            r"
            f : {} -> [A, B]*
            f
            "
        ),
    @r"
    ── UNNECESSARY WILDCARD in /code/proj/Main.roc ─────────────────────────────────

    This type annotation has a wildcard type variable (`*`) that isn't
    needed.

    4│      f : {} -> [A, B]*
                            ^

    Annotations for tag unions which are constants, or which are returned
    from functions, work the same way with or without a `*` at the end. (The
    `*` means something different when the tag union is an argument to a
    function, though!)

    You can safely remove this to make the code more concise without
    changing what it means.
    "
    );

    test_report!(
        multiple_list_patterns_start_and_end,
        indoc!(
            r#"
            when [] is
                [.., A, ..] -> ""
            "#
        ),
    @r#"
    ── MULTIPLE LIST REST PATTERNS in /code/proj/Main.roc ──────────────────────────

    This list pattern match has multiple rest patterns:

    5│          [.., A, ..] -> ""
                        ^^

    I only support compiling list patterns with one .. pattern! Can you
    remove this additional one?

    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    4│>      when [] is
    5│>          [.., A, ..] -> ""

    Other possibilities include:

        _

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        multiple_list_patterns_in_a_row,
        indoc!(
            r#"
            when [] is
                [A, .., .., B] -> ""
            "#
        ),
    @r#"
    ── MULTIPLE LIST REST PATTERNS in /code/proj/Main.roc ──────────────────────────

    This list pattern match has multiple rest patterns:

    5│          [A, .., .., B] -> ""
                        ^^

    I only support compiling list patterns with one .. pattern! Can you
    remove this additional one?

    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    4│>      when [] is
    5│>          [A, .., .., B] -> ""

    Other possibilities include:

        _

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        mismatch_within_list_pattern,
        indoc!(
            r#"
            when [] is
                [A, 1u8] -> ""
            "#
        ),
    @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This list element doesn't match the types of other elements in the
    pattern:

    5│          [A, 1u8] -> ""
                    ^^^

    It matches integers:

        U8

    But the other elements in this list pattern match

        [A]
    "#
    );

    test_report!(
        mismatch_list_pattern_vs_condition,
        indoc!(
            r#"
            when [A, B] is
                ["foo", "bar"] -> ""
            "#
        ),
    @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    The branches of this `when` expression don't match the condition:

    4│>      when [A, B] is
    5│           ["foo", "bar"] -> ""

    The `when` condition is a list of type:

        List [
            A,
            B,
        ]

    But the branch patterns have type:

        List Str

    The branches must be cases of the `when` condition's type!
    "#
    );

    test_report!(
        list_match_non_exhaustive_only_empty,
        indoc!(
            r#"
            l : List [A]

            when l is
                [] -> ""
            "#
        ),
    @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    6│>      when l is
    7│>          [] -> ""

    Other possibilities include:

        [_, ..]

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_no_problem!(
        list_match_spread_exhaustive,
        indoc!(
            r#"
            l : List [A]

            when l is
                [..] -> ""
            "#
        )
    );

    test_report!(
        list_match_non_exhaustive_infinite,
        indoc!(
            r#"
            l : List [A]

            when l is
                [] -> ""
                [A] -> ""
                [A, A] -> ""
            "#
        ),
    @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    6│>      when l is
    7│>          [] -> ""
    8│>          [A] -> ""
    9│>          [A, A] -> ""

    Other possibilities include:

        [_, _, _, ..]

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_no_problem!(
        list_match_spread_required_front_back,
        indoc!(
            r#"
            l : List [A, B]

            when l is
                [A, ..] -> ""
                [.., A] -> ""
                [..] -> ""
            "#
        )
    );

    test_report!(
        list_match_spread_redundant_front_back,
        indoc!(
            r#"
            l : List [A]

            when l is
                [A, ..] -> ""
                [.., A] -> ""
                [..] -> ""
            "#
        ),
    @r#"
    ── REDUNDANT PATTERN in /code/proj/Main.roc ────────────────────────────────────

    The 2nd pattern is redundant:

    6│       when l is
    7│           [A, ..] -> ""
    8│>          [.., A] -> ""
    9│           [..] -> ""

    Any value of this shape will be handled by a previous pattern, so this
    one should be removed.
    "#
    );

    test_no_problem!(
        list_match_spread_as,
        indoc!(
            r"
            l : List [A, B]

            when l is
                [A, .. as rest] | [.. as rest, A] -> rest
                [.. as rest] -> rest
            "
        )
    );

    test_no_problem!(
        list_match_exhaustive_empty_and_rest_with_unary_head,
        indoc!(
            r#"
            l : List [A]

            when l is
                [] -> ""
                [_, ..] -> ""
            "#
        )
    );

    test_no_problem!(
        list_match_exhaustive_empty_and_rest_with_exhausted_head,
        indoc!(
            r#"
            l : List [A, B]

            when l is
                [] -> ""
                [A, ..] -> ""
                [B, ..] -> ""
            "#
        )
    );

    test_report!(
        list_match_exhaustive_empty_and_rest_with_nonexhaustive_head,
        indoc!(
            r#"
            l : List [A, B]

            when l is
                [] -> ""
                [A, ..] -> ""
            "#
        ),
    @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    6│>      when l is
    7│>          [] -> ""
    8│>          [A, ..] -> ""

    Other possibilities include:

        [B, ..]

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        list_match_no_small_sizes_and_non_exhaustive_head,
        indoc!(
            r#"
            l : List [A, B]

            when l is
                [A, B, ..] -> ""
            "#
        ),
    @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    6│>      when l is
    7│>          [A, B, ..] -> ""

    Other possibilities include:

        []
        [_]
        [_, A, ..]

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_no_problem!(
        list_match_exhaustive_empty_and_rest_with_exhausted_tail,
        indoc!(
            r#"
            l : List [A, B]

            when l is
                [] -> ""
                [.., A] -> ""
                [.., B] -> ""
            "#
        )
    );

    test_report!(
        list_match_exhaustive_empty_and_rest_with_nonexhaustive_tail,
        indoc!(
            r#"
            l : List [A, B]

            when l is
                [] -> ""
                [.., A] -> ""
            "#
        ),
    @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    6│>      when l is
    7│>          [] -> ""
    8│>          [.., A] -> ""

    Other possibilities include:

        [.., B]

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        list_match_no_small_sizes_and_non_exhaustive_tail,
        indoc!(
            r#"
            l : List [A, B]

            when l is
                [.., B, A] -> ""
            "#
        ),
    @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    6│>      when l is
    7│>          [.., B, A] -> ""

    Other possibilities include:

        []
        [_]
        [.., _, B]

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_no_problem!(
        list_match_exhaustive_empty_and_rest_with_exhausted_head_and_tail,
        indoc!(
            r#"
            l : List [A, B]

            when l is
                [] -> ""
                [A] -> ""
                [B] -> ""
                [A, .., A] -> ""
                [A, .., B] -> ""
                [B, .., A] -> ""
                [B, .., B] -> ""
            "#
        )
    );

    test_report!(
        list_match_exhaustive_empty_and_rest_with_nonexhaustive_head_and_tail,
        indoc!(
            r#"
            l : List [A, B]

            when l is
                [] -> ""
                [_] -> ""
                [A, .., B] -> ""
                [B, .., A] -> ""
            "#
        ),
    @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

     6│>      when l is
     7│>          [] -> ""
     8│>          [_] -> ""
     9│>          [A, .., B] -> ""
    10│>          [B, .., A] -> ""

    Other possibilities include:

        [_, .., _]

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        list_match_no_small_sizes_and_non_exhaustive_head_and_tail,
        indoc!(
            r#"
            l : List [A, B]

            when l is
                [A, .., B] -> ""
                [B, .., A] -> ""
                [B, .., B] -> ""
            "#
        ),
    @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    6│>      when l is
    7│>          [A, .., B] -> ""
    8│>          [B, .., A] -> ""
    9│>          [B, .., B] -> ""

    Other possibilities include:

        []
        [_]
        [A, .., A]

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        list_match_exhaustive_big_sizes_but_not_small_sizes,
        indoc!(
            r#"
            l : List [A]

            when l is
                [A, A, A, .., A, A, A] -> ""
                [A, A, A, .., A, A] -> ""
                [A, A, .., A, A] -> ""
            "#
        ),
    @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    6│>      when l is
    7│>          [A, A, A, .., A, A, A] -> ""
    8│>          [A, A, A, .., A, A] -> ""
    9│>          [A, A, .., A, A] -> ""

    Other possibilities include:

        []
        [_]
        [_, _]
        [_, _, _]

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_no_problem!(
        list_match_nested_list_exhaustive,
        indoc!(
            r#"
            l : List (List [A])

            when l is
                [] -> ""
                [[]] -> ""
                [[A, ..]] -> ""
                [[..], .., [..]] -> ""
            "#
        )
    );

    test_report!(
        list_match_nested_list_not_exhaustive,
        indoc!(
            r#"
            l : List (List [A, B])

            when l is
                [] -> ""
                [[]] -> ""
                [[A, ..]] -> ""
                [[..], .., [.., B]] -> ""
            "#
        ),
    @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

     6│>      when l is
     7│>          [] -> ""
     8│>          [[]] -> ""
     9│>          [[A, ..]] -> ""
    10│>          [[..], .., [.., B]] -> ""

    Other possibilities include:

        [[B, ..]]
        [_, .., []]
        [_, .., [.., A]]

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_report!(
        list_match_redundant_exact_size,
        indoc!(
            r#"
            l : List [A]

            when l is
                [] -> ""
                [_] -> ""
                [_] -> ""
                [..] -> ""
            "#
        ),
    @r#"
    ── REDUNDANT PATTERN in /code/proj/Main.roc ────────────────────────────────────

    The 3rd pattern is redundant:

     6│       when l is
     7│           [] -> ""
     8│           [_] -> ""
     9│>          [_] -> ""
    10│           [..] -> ""

    Any value of this shape will be handled by a previous pattern, so this
    one should be removed.
    "#
    );

    test_report!(
        list_match_redundant_any_slice,
        indoc!(
            r#"
            l : List [A]

            when l is
                [] -> ""
                [_, ..] -> ""
                [..] -> ""
            "#
        ),
    @r#"
    ── REDUNDANT PATTERN in /code/proj/Main.roc ────────────────────────────────────

    The 3rd pattern is redundant:

    6│      when l is
    7│          [] -> ""
    8│          [_, ..] -> ""
    9│          [..] -> ""
                ^^^^

    Any value of this shape will be handled by a previous pattern, so this
    one should be removed.
    "#
    );

    test_report!(
        list_match_redundant_suffix_slice_with_sized_prefix,
        indoc!(
            r#"
            l : List [A]

            when l is
                [] -> ""
                [_, ..] -> ""
                [.., _] -> ""
            "#
        ),
    @r#"
    ── REDUNDANT PATTERN in /code/proj/Main.roc ────────────────────────────────────

    The 3rd pattern is redundant:

    6│      when l is
    7│          [] -> ""
    8│          [_, ..] -> ""
    9│          [.., _] -> ""
                ^^^^^^^

    Any value of this shape will be handled by a previous pattern, so this
    one should be removed.
    "#
    );

    test_report!(
        list_match_redundant_based_on_ctors,
        indoc!(
            r#"
            l : List {}

            when l is
                [{}, .., _] -> ""
                [_, .., {}] -> ""
                [..] -> ""
            "#
        ),
    @r#"
    ── REDUNDANT PATTERN in /code/proj/Main.roc ────────────────────────────────────

    The 2nd pattern is redundant:

    6│       when l is
    7│           [{}, .., _] -> ""
    8│>          [_, .., {}] -> ""
    9│           [..] -> ""

    Any value of this shape will be handled by a previous pattern, so this
    one should be removed.
    "#
    );

    test_no_problem!(
        list_match_with_guard,
        indoc!(
            r#"
            l : List [A]

            when l is
                [ A, .. ] if Bool.true -> ""
                [ A, .. ] -> ""
                _ -> ""
            "#
        )
    );

    test_report!(
        suggest_binding_rigid_var_to_ability,
        indoc!(
            r#"
            app "test" provides [f] to "./p"

            f : List e -> List e
            f = \l -> if l == l then l else l
            "#
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    4│  f = \l -> if l == l then l else l
                     ^

    I can't generate an implementation of the `Eq` ability for

        List e

    In particular, an implementation for

        e

    cannot be generated.

    Tip: This type variable is not bound to `Eq`. Consider adding an
    `implements` clause to bind the type variable, like
    `where e implements Bool.Eq`
    "
    );

    test_report!(
        crash_given_non_string,
        indoc!(
            r"
            crash {}
            "
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This value passed to `crash` is not a string:

    4│      crash {}
                  ^^

    The value is a record of type:

        {}

    But I can only `crash` with messages of type

        Str
    "
    );

    test_report!(
        crash_unapplied,
        indoc!(
            r"
            crash
            "
        ),
    @r"
    ── UNAPPLIED CRASH in /code/proj/Main.roc ──────────────────────────────────────

    This `crash` doesn't have a message given to it:

    4│      crash
            ^^^^^

    `crash` must be passed a message to crash with at the exact place it's
    used. `crash` can't be used as a value that's passed around, like
    functions can be - it must be applied immediately!
    "
    );

    test_report!(
        crash_overapplied,
        indoc!(
            r#"
            crash "" ""
            "#
        ),
    @r#"
    ── OVERAPPLIED CRASH in /code/proj/Main.roc ────────────────────────────────────

    This `crash` has too many values given to it:

    4│      crash "" ""
                  ^^^^^

    `crash` must be given exactly one message to crash with.
    "#
    );

    test_no_problem!(
        resolve_eq_for_unbound_num,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            n : Num *

            main = n == 1
            "#
        )
    );

    test_report!(
        resolve_eq_for_unbound_num_float,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            n : Num *

            main = n == 1f64
            "#
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    5│  main = n == 1f64
                    ^^^^

    I can't generate an implementation of the `Eq` ability for

        FloatingPoint ?

    Note: I can't derive `Bool.is_eq` for floating-point types. That's
    because Roc's floating-point numbers cannot be compared for total
    equality - in Roc, `NaN` is never comparable to `NaN`. If a type
    doesn't support total equality, it cannot support the `Eq` ability!
    "
    );

    test_no_problem!(
        resolve_hash_for_unbound_num,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            n : Num *

            main = \hasher -> Hash.hash hasher n
            "#
        )
    );

    test_report!(
        self_recursive_not_reached,
        indoc!(
            r#"
            app "test" provides [f] to "./platform"
            f = h {}
            h = \{} -> 1
            g = \{} -> if Bool.true then "" else g {}
            "#
        ),
    @r#"
    ── DEFINITION ONLY USED IN RECURSION in /code/proj/Main.roc ────────────────────

    This definition is only used in recursion with itself:

    4│  g = \{} -> if Bool.true then "" else g {}
        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    If you don't intend to use or export this definition, it should be
    removed!
    "#
    );

    test_no_problem!(
        self_recursive_not_reached_but_exposed,
        indoc!(
            r#"
            app "test" provides [g] to "./platform"
            g = \{} -> if Bool.true then "" else g {}
            "#
        )
    );

    test_report!(
        mutual_recursion_not_reached,
        indoc!(
            r#"
            app "test" provides [h] to "./platform"
            h = ""
            f = \{} -> if Bool.true then "" else g {}
            g = \{} -> if Bool.true then "" else f {}
            "#
        ),
    @r#"
    ── DEFINITIONS ONLY USED IN RECURSION in /code/proj/Main.roc ───────────────────

    These 2 definitions are only used in mutual recursion with themselves:

    3│>  f = \{} -> if Bool.true then "" else g {}
    4│>  g = \{} -> if Bool.true then "" else f {}

    If you don't intend to use or export any of them, they should all be
    removed!

    "#
    );

    test_report!(
        mutual_recursion_not_reached_but_exposed,
        indoc!(
            r#"
            app "test" provides [f] to "./platform"
            f = \{} -> if Bool.true then "" else g {}
            g = \{} -> if Bool.true then "" else f {}
            "#
        ),
    @r"
    "
    );

    test_report!(
        self_recursive_not_reached_nested,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"
            main =
                g = \{} -> if Bool.true then "" else g {}
                ""
            "#
        ),
    @r#"
    ── DEFINITION ONLY USED IN RECURSION in /code/proj/Main.roc ────────────────────

    This definition is only used in recursion with itself:

    3│      g = \{} -> if Bool.true then "" else g {}
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

    If you don't intend to use or export this definition, it should be
    removed!
    "#
    );

    test_no_problem!(
        self_recursive_not_reached_but_exposed_nested,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"
            main =
                g = \{} -> if Bool.true then "" else g {}
                g
            "#
        )
    );

    test_report!(
        mutual_recursion_not_reached_nested,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"
            main =
                f = \{} -> if Bool.true then "" else g {}
                g = \{} -> if Bool.true then "" else f {}
                ""
            "#
        ),
    @r#"
    ── DEFINITIONS ONLY USED IN RECURSION in /code/proj/Main.roc ───────────────────

    These 2 definitions are only used in mutual recursion with themselves:

    3│>      f = \{} -> if Bool.true then "" else g {}
    4│>      g = \{} -> if Bool.true then "" else f {}

    If you don't intend to use or export any of them, they should all be
    removed!

    "#
    );

    test_report!(
        mutual_recursion_not_reached_but_exposed_nested,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"
            main =
                f = \{} -> if Bool.true then "" else g {}
                g = \{} -> if Bool.true then "" else f {}
                f
            "#
        ),
    @r"
    "
    );

    // TODO(weakening-reports)
    test_report!(
        concat_different_types,
        indoc!(
            r#"
            empty = []
            one = List.concat [1] empty
            str = List.concat ["blah"] empty

            {one, str}
        "#),
    @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to `concat` has an unexpected type:

    6│      str = List.concat ["blah"] empty
                                       ^^^^^

    This `empty` value is a:

        List (Num *)

    But `concat` needs its 2nd argument to be:

        List Str
    "#
    );

    test_report!(
        implicit_inferred_open_in_output_position_cannot_grow,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main : {} -> [One]
            main = \{} ->
                if Bool.true
                then One
                else Two
            "#
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the `else` branch of this `if` expression:

    3│  main : {} -> [One]
    4│  main = \{} ->
    5│      if Bool.true
    6│      then One
    7│      else Two
                 ^^^

    This `Two` tag has the type:

        [Two]

    But the type annotation on `main` says it should be:

        [One]
    "
    );

    test_report!(
        implicit_inferred_open_in_output_position_cannot_grow_alias,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            R : [One]

            main : {} -> R
            main = \{} ->
                if Bool.true
                then One
                else Two
            "#
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the `else` branch of this `if` expression:

    5│  main : {} -> R
    6│  main = \{} ->
    7│      if Bool.true
    8│      then One
    9│      else Two
                 ^^^

    This `Two` tag has the type:

        [Two]

    But the type annotation on `main` says it should be:

        [One]
    "
    );

    test_report!(
        implicit_inferred_open_in_output_position_cannot_grow_nested,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main : List [One, Two] -> List [One]
            main = \tags ->
                List.map tags \tag ->
                    when tag is
                        One -> One
                        Two -> Two
            "#
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `main` definition:

    3│   main : List [One, Two] -> List [One]
    4│   main = \tags ->
    5│>      List.map tags \tag ->
    6│>          when tag is
    7│>              One -> One
    8│>              Two -> Two

    This `map` call produces:

        List [Two, …]

    But the type annotation on `main` says it should be:

        List […]
    "
    );

    test_report!(
        implicit_inferred_open_in_output_position_cannot_grow_nested_alias,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            R : [One]

            main : List [One, Two] -> List R
            main = \tags ->
                List.map tags \tag ->
                    when tag is
                        One -> One
                        Two -> Two
            "#
        ),
    @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `main` definition:

     5│   main : List [One, Two] -> List R
     6│   main = \tags ->
     7│>      List.map tags \tag ->
     8│>          when tag is
     9│>              One -> One
    10│>              Two -> Two

    This `map` call produces:

        List [Two, …]

    But the type annotation on `main` says it should be:

        List […]
    "
    );

    test_no_problem!(
        explicit_inferred_open_in_output_position_can_grow,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main : List [One, Two] -> List [One]_
            main = \tags ->
                List.map tags \tag ->
                    when tag is
                        One -> One
                        Two -> Two
            "#
        )
    );

    test_no_problem!(
        derive_decoding_for_tuple,
        indoc!(
            r#"
            app "test" imports [] provides [main] to "./platform"

            import Decode exposing [decoder]

            my_decoder : Decoder (U32, Str) _
            my_decoder = decoder

            main = my_decoder
            "#
        )
    );

    test_report!(
        cannot_decode_tuple_with_non_decode_element,
        indoc!(
            r#"
            app "test" imports [] provides [main] to "./platform"

            import Decode exposing [decoder]

            my_decoder : Decoder (U32, {} -> {}) _
            my_decoder = decoder

            main = my_decoder
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    6│  my_decoder = decoder
                     ^^^^^^^

    I can't generate an implementation of the `Decoding` ability for

        U32, {} -> {}

    Note: `Decoding` cannot be generated for functions.
    "
    );

    test_no_problem!(
        derive_encoding_for_tuple,
        indoc!(
            r#"
            app "test" imports [] provides [main] to "./platform"

            x : (U32, Str)

            main = Encode.to_encoder x
            "#
        )
    );

    test_report!(
        cannot_encode_tuple_with_non_encode_element,
        indoc!(
            r#"
            app "test" imports [] provides [main] to "./platform"

            x : (U32, {} -> {})

            main = Encode.to_encoder x
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression has a type that does not implement the abilities it's expected to:

    5│  main = Encode.to_encoder x
                                 ^

    I can't generate an implementation of the `Encoding` ability for

        U32, {} -> {}

    Note: `Encoding` cannot be generated for functions.
    "
    );

    test_report!(
        exhaustiveness_check_function_or_tag_union_issue_4994,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            x : U8

            if_then_case =
                when x is
                    0 -> Red
                    1 -> Yellow
                    2 -> Purple
                    3 -> Zulip
                    _ -> Green

            main =
                when if_then_case is
                    Red -> "red"
                    Green -> "green"
                    Yellow -> "yellow"
                    Zulip -> "zulip"
            "#
        ),
        @r#"
    ── UNSAFE PATTERN in /code/proj/Main.roc ───────────────────────────────────────

    This `when` does not cover all the possibilities:

    14│>      when if_then_case is
    15│>          Red -> "red"
    16│>          Green -> "green"
    17│>          Yellow -> "yellow"
    18│>          Zulip -> "zulip"

    Other possibilities include:

        Purple
        _

    I would have to crash if I saw one of those! Add branches for them!
    "#
    );

    test_no_problem!(
        openness_constraint_opens_under_tuple,
        indoc!(
            r"
              x : [A, B, C]
              when (x, 1u8) is
                (A, _) -> Bool.true
                (B, _) -> Bool.true
                _ -> Bool.true
            "
        )
    );

    test_report!(
        apply_opaque_as_function,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            Parser a := Str -> a

            parser : Parser Str
            parser = @Parser \s -> Str.concat s "asd"

            main : Str
            main = parser "hi"
            "#
        ),
        @r#"
    ── TOO MANY ARGS in /code/proj/Main.roc ────────────────────────────────────────

    The `parser` value is an opaque type, so it cannot be called with an
    argument:

    9│  main = parser "hi"
               ^^^^^^

    I can't call an opaque type because I don't know what it is! Maybe you
    meant to unwrap it first?
    "#
    );

    test_report!(
        function_arity_mismatch_too_few,
        indoc!(
            r#"
            app "test" provides [f] to "./platform"

            f : U8, U8 -> U8
            f = \x -> x
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    3│  f : U8, U8 -> U8
    4│  f = \x -> x
            ^^^^^^^

    The body is an anonymous function of type:

        (U8 -> U8)

    But the type annotation on `f` says it should be:

        (U8, U8 -> U8)

    Tip: It looks like it takes too few arguments. I was expecting 1 more.
    "
    );

    test_report!(
        function_arity_mismatch_too_many,
        indoc!(
            r#"
            app "test" provides [f] to "./platform"

            f : U8, U8 -> U8
            f = \x, y, z -> x + y + z
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    3│  f : U8, U8 -> U8
    4│  f = \x, y, z -> x + y + z
            ^^^^^^^^^^^^^^^^^^^^^

    The body is an anonymous function of type:

        (U8, U8, Int Unsigned8 -> U8)

    But the type annotation on `f` says it should be:

        (U8, U8 -> U8)

    Tip: It looks like it takes too many arguments. I'm seeing 1 extra.
    "
    );

    test_report!(
        function_arity_mismatch_nested_too_few,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                f : U8, U8 -> U8
                f = \x -> x

                f
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    4│      f : U8, U8 -> U8
    5│      f = \x -> x
                ^^^^^^^

    The body is an anonymous function of type:

        (U8 -> U8)

    But the type annotation on `f` says it should be:

        (U8, U8 -> U8)

    Tip: It looks like it takes too few arguments. I was expecting 1 more.
    "
    );

    test_report!(
        function_arity_mismatch_nested_too_many,
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                f : U8, U8 -> U8
                f = \x, y, z -> x + y + z

                f
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `f` definition:

    4│      f : U8, U8 -> U8
    5│      f = \x, y, z -> x + y + z
                ^^^^^^^^^^^^^^^^^^^^^

    The body is an anonymous function of type:

        (U8, U8, Int Unsigned8 -> U8)

    But the type annotation on `f` says it should be:

        (U8, U8 -> U8)

    Tip: It looks like it takes too many arguments. I'm seeing 1 extra.
    "
    );

    test_report!(
        pizza_parens_right,
        indoc!(
            r"
            2 |> (Num.sub 3)
            "
        ),
        @r"
    ── TOO FEW ARGS in /code/proj/Main.roc ─────────────────────────────────────────

    The `sub` function expects 2 arguments, but it got only 1:

    4│      2 |> (Num.sub 3)
                  ^^^^^^^

    Roc does not allow functions to be partially applied. Use a closure to
    make partial application explicit.
    "
    );

    test_report!(
        pizza_parens_middle,
        indoc!(
            r"
            2 |> (Num.sub 3) |> Num.sub 3
            "
        ),
        @r"
    ── TOO FEW ARGS in /code/proj/Main.roc ─────────────────────────────────────────

    The `sub` function expects 2 arguments, but it got only 1:

    4│      2 |> (Num.sub 3) |> Num.sub 3
                  ^^^^^^^

    Roc does not allow functions to be partially applied. Use a closure to
    make partial application explicit.
    "
    );

    test_report!(
        dbg_unapplied,
        indoc!(
            r"
            1 + dbg + 2
            "
        ),
    @r"
    ── UNAPPLIED DBG in /code/proj/Main.roc ────────────────────────────────────────

    This `dbg` doesn't have a value given to it:

    4│      1 + dbg + 2
                ^^^

    `dbg` must be passed a value to print at the exact place it's used. `dbg`
    can't be used as a value that's passed around, like functions can be -
    it must be applied immediately!

    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to + has an unexpected type:

    4│      1 + dbg + 2
                ^^^

    This value is a:

        {}

    But + needs its 2nd argument to be:

        Num *
    "
    );

    test_report!(
        dbg_overapplied,
        indoc!(
            r#"
            1 + dbg "" "" + 2
            "#
        ),
    @r#"
    ── OVERAPPLIED DBG in /code/proj/Main.roc ──────────────────────────────────────

    This `dbg` has too many values given to it:

    4│      1 + dbg "" "" + 2
                    ^^^^^

    `dbg` must be given exactly one value to print.

    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 2nd argument to + has an unexpected type:

    4│      1 + dbg "" "" + 2
                ^^^^^^^^^

    This value is a:

        {}

    But + needs its 2nd argument to be:

        Num *
    "#
    );

    test_report!(
        issue_6240_1,
        indoc!(
            r"
            {}.abcde
            "
        ),
        @r###"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This record doesn’t have a `abcde` field:

    4│      {}.abcde
            ^^^^^^^^

    In fact, it’s a record with no fields at all!
    "###
    );

    test_report!(
        issue_6240_2,
        indoc!(
            r#"
              ("", "").abcde
              "#
        ),
        @r###"
      ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

      This expression is used in an unexpected way:

      4│      ("", "").abcde
              ^^^^^^^^^^^^^^

      It is a tuple of type:

          (
              Str,
              Str,
          )a

      But you are trying to use it as:

          { abcde : * }b
      "###
    );

    test_report!(
        issue_6240_3,
        indoc!(
            r"
              {}.0
              "
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This expression is used in an unexpected way:

    4│      {}.0
            ^^^^

    It is a record of type:

        {}

    But you are trying to use it as:

        (*)b
    "
    );

    test_report!(
        return_outside_of_function,
        indoc!(
            r"
            some_val =
                if 10 > 5 then
                    x = 5
                    return x
                else
                    6

            some_val + 2
            "
        ),
        @r###"
        ── RETURN OUTSIDE OF FUNCTION in /code/proj/Main.roc ───────────────────────────

        This `return` doesn't belong to a function:

        7│              return x
                        ^^^^^^^^

        I wouldn't know where to return to if I used it!
        "###
    );

    test_report!(
        statements_after_return,
        indoc!(
            r#"
            my_function = \x ->
                if x == 2 then
                    return x

                    log! "someData"
                    useX x 123
                else
                    x + 5

            my_function 2
            "#
        ),
        @r###"
        ── UNREACHABLE CODE in /code/proj/Main.roc ─────────────────────────────────────

        This code won't run because it follows a `return` statement:

        6│>              return x
        7│>
        8│>              log! "someData"
        9│>              useX x 123

        Hint: you can move the `return` statement below this block to make the
        code that follows it run.
        "###
    );

    test_report!(
        return_at_end_of_function,
        indoc!(
            r#"
            my_function = \x ->
                y = Num.to_str x

                return y

            my_function 3
            "#
        ),
        @r###"
        ── UNNECESSARY RETURN in /code/proj/Main.roc ───────────────────────────────────

        This `return` keyword is redundant:

        7│          return y
                    ^^^^^^^^

        The last expression in a function is treated like a `return` statement.
        You can safely remove `return` here.
        "###
    );

    test_report!(
        mismatch_early_return_with_function_output,
        indoc!(
            r#"
            my_function = \x ->
                if x == 5 then
                    return "abc"
                else
                    x

            my_function 3
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This returns something that's incompatible with the return type of the
    enclosing function:

    5│           if x == 5 then
    6│>              return "abc"
    7│           else
    8│               x

    This returns a value of type:

        Str

    But I expected the function to have return type:

        Num *
    "#
    );

    test_report!(
        try_in_bare_statement,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            validate_num = \num ->
                if num > 5 then
                    Ok {}
                else
                    Err TooBig

            main! = \{} ->
                Effect.put_line! "hello"

                # this returns {}, so it's ignored
                try validate_num 10

                # this returns a value, so we are incorrectly
                # dropping the parsed value
                try List.get [1, 2, 3] 5

                Ok {}
            "#
        ),
        @r###"
        ── IGNORED RESULT in /code/proj/Main.roc ───────────────────────────────────────
        
        The result of this expression is ignored:
        
        19│      try List.get [1, 2, 3] 5
                 ^^^^^^^^^^^^^^^^^^^^^^^^
        
        Standalone statements are required to produce an empty record, but the
        type of this one is:
        
            Num *
        
        If you still want to ignore it, assign it to `_`, like this:
        
            _ = File.delete! "data.json"
        "###
    );

    test_report!(
        return_in_bare_statement,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                Effect.put_line! "hello"

                # this outputs {}, so it's ignored
                if 7 > 5 then
                    {}
                else
                    return Err TooBig

                # this outputs a value, so we are incorrectly
                # dropping the parsed value
                when List.get [1, 2, 3] 5 is
                    Ok item -> item
                    Err err ->
                        return Err err

                Ok {}
            "#
        ),
        @r#"
    ── IGNORED RESULT in /code/proj/Main.roc ───────────────────────────────────────

    The result of this expression is ignored:

    16│>      when List.get [1, 2, 3] 5 is
    17│>          Ok item -> item
    18│>          Err err ->
    19│>              return Err err

    Standalone statements are required to produce an empty record, but the
    type of this one is:

        Num *

    If you still want to ignore it, assign it to `_`, like this:

        _ = File.delete! "data.json"
    "#
    );

    test_report!(
        mismatch_only_early_returns,
        indoc!(
            r#"
            my_function = \x ->
                if x == 5 then
                    return "abc"
                else
                    return 123

            my_function 3
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This returns something that's incompatible with the return type of the
    enclosing function:

    5│          if x == 5 then
    6│              return "abc"
    7│          else
    8│              return 123
                    ^^^^^^^^^^

    This returns a value of type:

        Num *

    But I expected the function to have return type:

        Str
    "#
    );

    test_report!(
        try_with_ignored_output,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                Effect.put_line! "hello"

                # not ignored, warning
                try List.get [1, 2, 3] 5

                # ignored, OK
                _ = try List.get [1, 2, 3] 5
                _ignored = try List.get [1, 2, 3] 5

                Ok {}
            "#
        ),
        @r###"
        ── IGNORED RESULT in /code/proj/Main.roc ───────────────────────────────────────
        
        The result of this expression is ignored:
        
        9│      try List.get [1, 2, 3] 5
                ^^^^^^^^^^^^^^^^^^^^^^^^
        
        Standalone statements are required to produce an empty record, but the
        type of this one is:
        
            Num *
        
        If you still want to ignore it, assign it to `_`, like this:
        
            _ = File.delete! "data.json"
        "###
    );

    test_report!(
        return_with_ignored_output,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                Effect.put_line! "hello"

                # not ignored, warning
                when List.get [1, 2, 3] 5 is
                    Ok item -> item
                    Err err ->
                        return Err err

                # ignored, OK
                _ =
                    when List.get [1, 2, 3] 5 is
                        Ok item -> item
                        Err err ->
                            return Err err

                # also ignored, also OK
                _ignored =
                    when List.get [1, 2, 3] 5 is
                        Ok item -> item
                        Err err ->
                            return Err err

                Ok {}
            "#
        ),
        @r#"
    ── IGNORED RESULT in /code/proj/Main.roc ───────────────────────────────────────

    The result of this expression is ignored:

     9│>      when List.get [1, 2, 3] 5 is
    10│>          Ok item -> item
    11│>          Err err ->
    12│>              return Err err

    Standalone statements are required to produce an empty record, but the
    type of this one is:

        Num *

    If you still want to ignore it, assign it to `_`, like this:

        _ = File.delete! "data.json"
    "#
    );

    test_report!(
        no_early_return_in_bare_statement,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                Effect.put_line! "hello"

                Num.to_str 123

                Ok {}
            "#
        ),
        @r#"
    ── IGNORED RESULT in /code/proj/Main.roc ───────────────────────────────────────

    The result of this call to `Num.to_str` is ignored:

    8│      Num.to_str 123
            ^^^^^^^^^^

    Standalone statements are required to produce an empty record, but the
    type of this one is:

        Str

    If you still want to ignore it, assign it to `_`, like this:

        _ = File.delete! "data.json"

    ── LEFTOVER STATEMENT in /code/proj/Main.roc ───────────────────────────────────

    This statement does not produce any effects:

    8│      Num.to_str 123
            ^^^^^^^^^^^^^^

    Standalone statements are only useful if they call effectful
    functions.

    Did you forget to use its result? If not, feel free to remove it.
    "#
    );

    test_report!(
        no_early_return_in_ignored_statement,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                Effect.put_line! "hello"

                _ignored = Num.to_str 123

                Ok {}
            "#
        ),
        @r"
    ── UNNECESSARY DEFINITION in /code/proj/Main.roc ───────────────────────────────

    This assignment doesn't introduce any new variables:

    8│      _ignored = Num.to_str 123
            ^^^^^^^^

    Since it doesn't call any effectful functions, this assignment cannot
    affect the program's behavior. If you don't need to use the value on
    the right-hand side, consider removing the assignment.
    "
    );

    test_report!(
        mismatch_early_return_annotated_function,
        indoc!(
            r#"
            my_function : U64 -> Str
            my_function = \x ->
                if x == 5 then
                    return 123
                else
                    "abc"

            my_function 3
            "#
        ),
        @r###"
        ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────
        
        Something is off with the body of the `my_function` definition:
        
        4│      my_function : U64 -> Str
        5│      my_function = \x ->
        6│          if x == 5 then
        7│              return 123
                        ^^^^^^^^^^
        
        This returns a value of type:
        
            Num *
        
        But the type annotation on `my_function` says it should be:
        
            Str

        "###
    );

    test_report!(
        function_with_early_return_generalizes,
        indoc!(
            r#"
            parse_items_with = \parser ->
                when List.map_try ["123", "456"] parser is
                    Ok ok -> Ok ok
                    Err err ->
                        return Err err

            u64_nums = parse_items_with Str.to_u64
            u8_nums = parse_items_with Str.to_u8

            "${Inspect.to_str(u64_nums)} ${Inspect.to_str(u8_nums)}"
            "#
        ),
        @"" // no errors
    );

    test_report!(
        keyword_try_with_non_result_target,
        indoc!(
            r#"
            invalid_try = \{} ->
                non_result = "abc"
                x = try non_result

                Ok (x * 2)

            invalid_try {}
            "#
        ),
        @r"
    ── INVALID TRY TARGET in /code/proj/Main.roc ───────────────────────────────────

    This expression cannot be used as a `try` target:

    6│          x = try non_result
                        ^^^^^^^^^^

    I expected a Result, but it actually has type:

        Str

    Hint: Did you forget to wrap the value with an `Ok` or an `Err` tag?
    "
    );

    test_report!(
        question_try_with_non_result_target,
        indoc!(
            r#"
            invalid_try = \{} ->
                non_result = "abc"
                x = non_result?

                Ok (x * 2)

            invalid_try {}
            "#
        ),
        @r"
    ── INVALID TRY TARGET in /code/proj/Main.roc ───────────────────────────────────

    This expression cannot be tried with the `?` operator:

    6│          x = non_result?
                    ^^^^^^^^^^^

    I expected a Result, but it actually has type:

        Str

    Hint: Did you forget to wrap the value with an `Ok` or an `Err` tag?
    "
    );

    test_report!(
        incompatible_try_errs,
        indoc!(
            r#"
            incompatible_trys = \{} ->
                x = try Err 123

                y = try Err "abc"

                Ok (x + y)

            incompatible_trys {}
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This returns something that's incompatible with the return type of the
    enclosing function:

    5│           x = try Err 123
    6│
    7│>          y = try Err "abc"
    8│
    9│           Ok (x + y)

    This returns an `Err` of type:

        [Err Str, …]

    But I expected the function to have return type:

        [Err (Num *), …]a
    "#
    );

    test_report!(
        keyword_try_prefix_in_pipe,
        indoc!(
            r#"
            read_file : Str -> Str

            get_file_contents : Str -> Result Str _
            get_file_contents = \file_path ->
                contents =
                    read_file file_path
                    |> try Result.map_err ErrWrapper

                contents

            get_file_contents "file.txt"
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to this function has an unexpected type:

     9│>              read_file file_path
    10│               |> try Result.map_err ErrWrapper

    This `read_file` call produces:

        Str

    But this function needs its 1st argument to be:

        Result ok a
    "
    );

    test_report!(
        keyword_try_suffix_in_pipe,
        indoc!(
            r#"
            read_file : Str -> Str

            get_file_contents : Str -> Result Str _
            get_file_contents = \file_path ->
                contents =
                    read_file file_path
                    |> Result.map_err ErrWrapper
                    |> try

                contents

            get_file_contents "file.txt"
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to |> has an unexpected type:

     9│>              read_file file_path
    10│               |> Result.map_err ErrWrapper

    This `read_file` call produces:

        Str

    But |> needs its 1st argument to be:

        Result ok a
    "
    );

    test_report!(
        question_try_in_pipe,
        indoc!(
            r#"
            read_file : Str -> Str

            get_file_contents : Str -> Result Str _
            get_file_contents = \file_path ->
                contents =
                    read_file file_path
                    |> Result.map_err? ErrWrapper

                contents

            get_file_contents "file.txt"
            "#
        ),
        @r"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to this function has an unexpected type:

     9│>              read_file file_path
    10│               |> Result.map_err? ErrWrapper

    This `read_file` call produces:

        Str

    But this function needs its 1st argument to be:

        Result ok a
    "
    );

    test_report!(
        leftover_statement,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                identity {}

                Effect.put_line! "hello"

            identity = \x -> x
            "#
        ),
        @r###"
    ── LEFTOVER STATEMENT in /code/proj/Main.roc ───────────────────────────────────

    This statement does not produce any effects:

    6│      identity {}
            ^^^^^^^^^^^

    Standalone statements are only useful if they call effectful
    functions.

    Did you forget to use its result? If not, feel free to remove it.
    "###
    );

    test_report!(
        fx_fn_annotated_as_pure,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                Effect.put_line! (get_cheer "hello")

            get_cheer : Str -> Str
            get_cheer = \msg ->
                name = Effect.get_line! {}

                "${msg}, ${name}!"
            "#
        ),
        @r"
    ── EFFECT IN PURE FUNCTION in /code/proj/Main.roc ──────────────────────────────

    This call to `Effect.get_line!` might produce an effect:

    10│      name = Effect.get_line! {}
                    ^^^^^^^^^^^^^^^^^^^

    However, the type of the enclosing function requires that it's pure:

    8│  get_cheer : Str -> Str
                    ^^^^^^^^^^

    Tip: Replace `->` with `=>` to annotate it as effectful.

    You can still run the program with this error, which can be helpful
    when you're debugging.
    "
    );

    test_report!(
        fx_fn_annotated_as_pure_stmt,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                trim "hello "

            trim : Str -> Str
            trim = \msg ->
                Effect.put_line!("Trimming ${msg}")
                Str.trim msg
            "#
        ),
        @r#"
    ── EFFECT IN PURE FUNCTION in /code/proj/Main.roc ──────────────────────────────

    This call to `Effect.put_line!` might produce an effect:

    10│      Effect.put_line!("Trimming ${msg}")
             ^^^^^^^^^^^^^^^^

    However, the type of the enclosing function requires that it's pure:

    8│  trim : Str -> Str
               ^^^^^^^^^^

    Tip: Replace `->` with `=>` to annotate it as effectful.

    You can still run the program with this error, which can be helpful
    when you're debugging.
    "#
    );

    test_report!(
        nested_function_def_fx_no_bang,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                print_hello = \{} ->
                    Effect.put_line! "hello"

                print_hello {}
            "#
        ),
        @r"
    ── MISSING EXCLAMATION in /code/proj/Main.roc ──────────────────────────────────

    This function is effectful, but its name does not indicate so:

    6│      print_hello = \{} ->
            ^^^^^^^^^^^

    Add an exclamation mark at the end, like:

        print_hello!

    This will help readers identify it as a source of effects.
    "
    );

    test_report!(
        ignored_result_stmt,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                Effect.get_line! {}
                {}
            "#
        ),
        @r#"
    ── IGNORED RESULT in /code/proj/Main.roc ───────────────────────────────────────

    The result of this call to `Effect.get_line!` is ignored:

    6│      Effect.get_line! {}
            ^^^^^^^^^^^^^^^^

    Standalone statements are required to produce an empty record, but the
    type of this one is:

        Str

    If you still want to ignore it, assign it to `_`, like this:

        _ = File.delete! "data.json"
    "#
    );

    test_report!(
        ignored_stmt_forgot_to_call,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                Effect.get_line!
                Effect.put_line! "hi"
            "#
        ),
        @r"
    ── IGNORED RESULT in /code/proj/Main.roc ───────────────────────────────────────

    The result of this expression is ignored:

    6│      Effect.get_line!
            ^^^^^^^^^^^^^^^^

    Standalone statements are required to produce an empty record, but the
    type of this one is:

        {} => Str

    Hint: Did you forget to call the function?

    ── LEFTOVER STATEMENT in /code/proj/Main.roc ───────────────────────────────────

    This statement does not produce any effects:

    6│      Effect.get_line!
            ^^^^^^^^^^^^^^^^

    Standalone statements are only useful if they call effectful
    functions.

    Did you forget to use its result? If not, feel free to remove it.
    "
    );

    test_report!(
        function_def_leftover_bang,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                Effect.put_line! (hello! {})

            hello! = \{} ->
                "hello"
            "#
        ),
        @r###"
    ── UNNECESSARY EXCLAMATION in /code/proj/Main.roc ──────────────────────────────

    This function is pure, but its name suggests otherwise:

    8│  hello! = \{} ->
        ^^^^^^

    The exclamation mark at the end is reserved for effectful functions.

    Hint: Did you forget to run an effect? Is the type annotation wrong?
    "###
    );

    test_report!(
        effect_in_top_level_value_def,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            hello =
                Effect.put_line! "calling hello!"
                "hello"

            main! = \{} ->
                Effect.put_line! hello
            "#
        ),
        @r#"
    ── EFFECT IN TOP-LEVEL in /code/proj/Main.roc ──────────────────────────────────

    This call to `Effect.put_line!` might produce an effect:

    6│      Effect.put_line! "calling hello!"
            ^^^^^^^^^^^^^^^^

    However, it appears in a top-level def instead of a function. If we
    allowed this, importing this module would produce a side effect.

    Tip: If you don't need any arguments, use an empty record:

        askName! : {} => Str
        askName! = \{} ->
            Stdout.line! "What's your name?"
            Stdin.line! {}

    This will allow the caller to control when the effects run.
    "#
    );

    test_report!(
        aliased_fx_fn,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                print_ln "Hello"

            print_ln = Effect.put_line!
            "#
        ),
        @r"
    ── MISSING EXCLAMATION in /code/proj/Main.roc ──────────────────────────────────

    This function is effectful, but its name does not indicate so:

    8│  print_ln = Effect.put_line!
        ^^^^^^^^

    Add an exclamation mark at the end, like:

        print_ln!

    This will help readers identify it as a source of effects.
    "
    );

    test_report!(
        unsuffixed_fx_in_record,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                fx = {
                    put_line: Effect.put_line!
                }
                fx.put_line "hello world!"
            "#
        ),
        @r"
    ── MISSING EXCLAMATION in /code/proj/Main.roc ──────────────────────────────────

    This field's value is an effectful function, but its name does not
    indicate so:

    7│          put_line: Effect.put_line!
                ^^^^^^^^^^^^^^^^^^^^^^^^^^

    Add an exclamation mark at the end, like:

        { read_file! : File.read! }

    This will help readers identify it as a source of effects.
    "
    );

    test_report!(
        unsuffixed_fx_in_record_annotation,
        indoc!(
            r#"
            module [Fx]

            Fx : {
                get_line: {} => Str
            }
            "#
        ),
        @r"
    ── MISSING EXCLAMATION in /code/proj/Main.roc ──────────────────────────────────

    The type of this record field is an effectful function, but its name
    does not indicate so:

    4│      get_line: {} => Str
            ^^^^^^^^^^^^^^^^^^^

    Add an exclamation mark at the end, like:

        { read_file!: Str => Str }

    This will help readers identify it as a source of effects.
    "
    );

    test_report!(
        suffixed_pure_fn_in_record_annotation,
        indoc!(
            r#"
            module [Fx]

            Fx : {
                get_line!: {} -> Str
            }
            "#
        ),
        @r"
    ── UNNECESSARY EXCLAMATION in /code/proj/Main.roc ──────────────────────────────

    The type of this record field is a pure function, but its name
    suggests otherwise:

    4│      get_line!: {} -> Str
            ^^^^^^^^^^^^^^^^^^^^

    The exclamation mark at the end is reserved for effectful functions.

    Hint: Did you mean to use `=>` instead of `->`?
    "
    );

    test_report!(
        unsuffixed_fx_arg,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                ["Hello", "world!"]
                |> for_each! Effect.put_line!

            for_each! : List a, (a => {}) => {}
            for_each! = \l, f ->
                when l is
                    [] -> {}
                    [x, .. as xs] ->
                        f x
                        for_each! xs f
            "#
        ),
        @r"
    ── MISSING EXCLAMATION in /code/proj/Main.roc ──────────────────────────────────

    This function is effectful, but its name does not indicate so:

    10│  for_each! = \l, f ->
                         ^

    Add an exclamation mark at the end, like:

        f!

    This will help readers identify it as a source of effects.
    "
    );

    test_report!(
        suffixed_pure_arg,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                Ok " hi "
                |> map_ok  Str.trim
                |> Result.with_default ""
                |> Effect.put_line!

            map_ok : Result a err, (a -> b) -> Result b err
            map_ok = \result, fn! ->
                when result is
                    Ok x -> Ok (fn! x)
                    Err e -> Err e
            "#
        ),
        @r"
    ── UNNECESSARY EXCLAMATION in /code/proj/Main.roc ──────────────────────────────

    This function is pure, but its name suggests otherwise:

    12│  map_ok = \result, fn! ->
                           ^^^

    The exclamation mark at the end is reserved for effectful functions.

    Hint: Did you forget to run an effect? Is the type annotation wrong?
    "
    );

    test_report!(
        unsuffixed_tuple_fx_field,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                (get, put) = (Effect.get_line!, Effect.put_line!)

                name = get {}
                put "Hi, ${name}"
            "#
        ),
        @r###"
    ── MISSING EXCLAMATION in /code/proj/Main.roc ──────────────────────────────────

    This function is effectful, but its name does not indicate so:

    6│      (get, put) = (Effect.get_line!, Effect.put_line!)
             ^^^

    Add an exclamation mark at the end, like:

        get!

    This will help readers identify it as a source of effects.

    ── MISSING EXCLAMATION in /code/proj/Main.roc ──────────────────────────────────

    This function is effectful, but its name does not indicate so:

    6│      (get, put) = (Effect.get_line!, Effect.put_line!)
                  ^^^

    Add an exclamation mark at the end, like:

        put!

    This will help readers identify it as a source of effects.
    "###
    );

    test_report!(
        suffixed_tuple_pure_field,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                (msg, trim!) = (" hi ", Str.trim)

                Effect.put_line! (trim! msg)
            "#
        ),
        @r###"
    ── UNNECESSARY EXCLAMATION in /code/proj/Main.roc ──────────────────────────────

    This function is pure, but its name suggests otherwise:

    6│      (msg, trim!) = (" hi ", Str.trim)
                  ^^^^^

    The exclamation mark at the end is reserved for effectful functions.

    Hint: Did you forget to run an effect? Is the type annotation wrong?
    "###
    );

    test_report!(
        unsuffixed_tag_fx_field,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                Tag get put = Tag Effect.get_line! Effect.put_line!

                name = get {}
                put "Hi, ${name}"
            "#
        ),
        @r###"
    ── MISSING EXCLAMATION in /code/proj/Main.roc ──────────────────────────────────

    This function is effectful, but its name does not indicate so:

    6│      Tag get put = Tag Effect.get_line! Effect.put_line!
                ^^^

    Add an exclamation mark at the end, like:

        get!

    This will help readers identify it as a source of effects.

    ── MISSING EXCLAMATION in /code/proj/Main.roc ──────────────────────────────────

    This function is effectful, but its name does not indicate so:

    6│      Tag get put = Tag Effect.get_line! Effect.put_line!
                    ^^^

    Add an exclamation mark at the end, like:

        put!

    This will help readers identify it as a source of effects.
    "###
    );

    test_report!(
        suffixed_tag_pure_field,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                Tag msg trim! = Tag " hi " Str.trim

                Effect.put_line! (trim! msg)
            "#
        ),
        @r###"
    ── UNNECESSARY EXCLAMATION in /code/proj/Main.roc ──────────────────────────────

    This function is pure, but its name suggests otherwise:

    6│      Tag msg trim! = Tag " hi " Str.trim
                    ^^^^^

    The exclamation mark at the end is reserved for effectful functions.

    Hint: Did you forget to run an effect? Is the type annotation wrong?
    "###
    );

    test_report!(
        unsuffixed_opaque_fx_field,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            PutLine := Str => {}

            main! = \{} ->
                @PutLine put = @PutLine Effect.put_line!

                put "Hi!"
            "#
        ),
        @r###"
    ── MISSING EXCLAMATION in /code/proj/Main.roc ──────────────────────────────────

    This function is effectful, but its name does not indicate so:

    8│      @PutLine put = @PutLine Effect.put_line!
                     ^^^

    Add an exclamation mark at the end, like:

        put!

    This will help readers identify it as a source of effects.
    "###
    );

    test_report!(
        suffixed_opaque_pure_field,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            Trim := Str -> Str

            main! = \{} ->
                @Trim trim! = @Trim Str.trim

                Effect.put_line! (trim! " hi ")
            "#
        ),
        @r###"
    ── UNNECESSARY EXCLAMATION in /code/proj/Main.roc ──────────────────────────────

    This function is pure, but its name suggests otherwise:

    8│      @Trim trim! = @Trim Str.trim
                  ^^^^^

    The exclamation mark at the end is reserved for effectful functions.

    Hint: Did you forget to run an effect? Is the type annotation wrong?
    "###
    );

    test_report!(
        fx_passed_to_untyped_pure_hof,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                pure_higher_order Effect.put_line! "hi"

            pure_higher_order = \f, x -> f x
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `pure_higher_order` has an unexpected type:

    6│      pure_higher_order Effect.put_line! "hi"
                              ^^^^^^^^^^^^^^^^

    This `Effect.put_line!` value is a:

        Str => {}

    But `pure_higher_order` needs its 1st argument to be:

        Str -> {}
    "#
    );

    test_report!(
        fx_passed_to_partially_inferred_pure_hof,
        indoc!(
            r#"
            app [main!] { pf: platform "../../../../../crates/cli/tests/test-projects/test-platform-effects-zig/main.roc" }

            import pf.Effect

            main! = \{} ->
                pure_higher_order Effect.put_line! "hi"

            pure_higher_order : _, _ -> _
            pure_higher_order = \f, x -> f x
            "#
        ),
        @r#"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    This 1st argument to `pure_higher_order` has an unexpected type:

    6│      pure_higher_order Effect.put_line! "hi"
                              ^^^^^^^^^^^^^^^^

    This `Effect.put_line!` value is a:

        Str => {}

    But `pure_higher_order` needs its 1st argument to be:

        Str -> {}
    "#
    );

    test_report!(
        invalid_generic_literal,
        indoc!(
            r#"
            module [v]

            v : *
            v = 1
            "#
        ),
        @r###"
    ── TYPE MISMATCH in /code/proj/Main.roc ────────────────────────────────────────

    Something is off with the body of the `v` definition:

    3│  v : *
    4│  v = 1
            ^

    The body is a number of type:

        Num *

    But the type annotation on `v` says it should be:

        *

    Tip: The type annotation uses the type variable `*` to say that this
    definition can produce any type of value. But in the body I see that
    it will only produce a `Num` value of a single specific type. Maybe
    change the type annotation to be more specific? Maybe change the code
    to be more general?
    "###
    );

    test_report!(
        invalid_generic_literal_list,
        indoc!(
            r#"
            module [v]

            v : List *
            v = []
            "#
        ),
        @r###"
    ── TYPE VARIABLE IS NOT GENERIC in /code/proj/Main.roc ─────────────────────────

    This type variable has a single type:

    3│  v : List *
                 ^

    Type variables tell me that they can be used with any type, but they
    can only be used with functions. All other values have exactly one
    type.

    Hint: If you would like the type to be inferred for you, use an
    underscore _ instead.
    "###
    );
}
