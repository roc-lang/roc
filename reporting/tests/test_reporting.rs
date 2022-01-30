#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate roc_reporting;

mod helpers;

#[cfg(test)]
mod test_reporting {
    use crate::helpers::test_home;
    use crate::helpers::{can_expr, infer_expr, CanExprOut, ParseErrOut};
    use bumpalo::Bump;
    use roc_module::symbol::{Interns, ModuleId};
    use roc_mono::ir::{Procs, Stmt, UpdateModeIds};
    use roc_mono::layout::LayoutCache;
    use roc_region::all::LineInfo;
    use roc_reporting::report::{
        can_problem, mono_problem, parse_problem, type_problem, Report, Severity, BLUE_CODE,
        BOLD_CODE, CYAN_CODE, DEFAULT_PALETTE, GREEN_CODE, MAGENTA_CODE, RED_CODE, RESET_CODE,
        UNDERLINE_CODE, WHITE_CODE, YELLOW_CODE,
    };
    use roc_reporting::report::{RocDocAllocator, RocDocBuilder};
    use roc_solve::solve;
    use roc_test_utils::assert_multiline_str_eq;
    use roc_types::pretty_print::name_all_type_vars;
    use roc_types::subs::Subs;
    use std::path::PathBuf;

    fn filename_from_string(str: &str) -> PathBuf {
        let mut filename = PathBuf::new();
        filename.push(str);

        filename
    }

    fn to_simple_report<'b>(doc: RocDocBuilder<'b>) -> Report<'b> {
        Report {
            title: "".to_string(),
            doc,
            filename: filename_from_string(r"\code\proj\Main.roc"),
            severity: Severity::RuntimeError,
        }
    }

    fn infer_expr_help<'a>(
        arena: &'a Bump,
        expr_src: &'a str,
    ) -> Result<
        (
            Vec<solve::TypeError>,
            Vec<roc_problem::can::Problem>,
            Vec<roc_mono::ir::MonoProblem>,
            ModuleId,
            Interns,
        ),
        ParseErrOut<'a>,
    > {
        let CanExprOut {
            loc_expr,
            output,
            var_store,
            var,
            constraint,
            home,
            mut interns,
            problems: can_problems,
            ..
        } = can_expr(arena, expr_src)?;
        let mut subs = Subs::new_from_varstore(var_store);

        for (var, name) in output.introduced_variables.name_by_var {
            subs.rigid_var(var, name);
        }

        for var in output.introduced_variables.wildcards {
            subs.rigid_var(var, "*".into());
        }

        let mut unify_problems = Vec::new();
        let (_content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        name_all_type_vars(var, &mut subs);

        let mut mono_problems = Vec::new();

        // MONO

        if unify_problems.is_empty() && can_problems.is_empty() {
            let arena = Bump::new();

            // Compile and add all the Procs before adding main
            let mut procs = Procs::new_in(&arena);
            let mut ident_ids = interns.all_ident_ids.remove(&home).unwrap();
            let mut update_mode_ids = UpdateModeIds::new();

            // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
            let target_info = roc_target::TargetInfo::default_x86_64();
            let mut layout_cache = LayoutCache::new(target_info);
            let mut mono_env = roc_mono::ir::Env {
                arena: &arena,
                subs: &mut subs,
                problems: &mut mono_problems,
                home,
                ident_ids: &mut ident_ids,
                update_mode_ids: &mut update_mode_ids,
                target_info,
                // call_specialization_counter=0 is reserved
                call_specialization_counter: 1,
            };
            let _mono_expr = Stmt::new(
                &mut mono_env,
                loc_expr.value,
                var,
                &mut procs,
                &mut layout_cache,
            );
        }

        Ok((unify_problems, can_problems, mono_problems, home, interns))
    }

    fn list_reports<F>(arena: &Bump, src: &str, buf: &mut String, callback: F)
    where
        F: FnOnce(RocDocBuilder<'_>, &mut String),
    {
        use ven_pretty::DocAllocator;

        let src_lines: Vec<&str> = src.split('\n').collect();
        let lines = LineInfo::new(src);

        let filename = filename_from_string(r"\code\proj\Main.roc");

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
            Ok((type_problems, can_problems, mono_problems, home, interns)) => {
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

                for problem in mono_problems {
                    let report = mono_problem(&alloc, &lines, filename.clone(), problem.clone());
                    reports.push(report);
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

        let filename = filename_from_string(r"\code\proj\Main.roc");
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
        if true && buf != expected_rendering {
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
        if true && buf != expected_rendering {
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

    fn human_readable(str: &str) -> String {
        str.replace(RED_CODE, "<red>")
            .replace(WHITE_CODE, "<white>")
            .replace(BLUE_CODE, "<blue>")
            .replace(YELLOW_CODE, "<yellow>")
            .replace(GREEN_CODE, "<green>")
            .replace(CYAN_CODE, "<cyan>")
            .replace(MAGENTA_CODE, "<magenta>")
            .replace(RESET_CODE, "<reset>")
            .replace(BOLD_CODE, "<bold>")
            .replace(UNDERLINE_CODE, "<underline>")
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
                ── NOT EXPOSED ─────────────────────────────────────────────────────────────────

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
                ── UNUSED DEFINITION ───────────────────────────────────────────────────────────

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
                ── DUPLICATE NAME ──────────────────────────────────────────────────────────────

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
                Booly : [ Yes, No ]

                Booly : [ Yes, No, Maybe ]

                x =
                    No

                x
           "#
            ),
            // Booly is called a "variable"
            indoc!(
                r#"
                ── DUPLICATE NAME ──────────────────────────────────────────────────────────────

                The `Booly` name is first defined here:

                1│  Booly : [ Yes, No ]
                    ^^^^^^^^^^^^^^^^^^^

                But then it's defined a second time here:

                3│  Booly : [ Yes, No, Maybe ]
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^

                Since these variables have the same name, it's easy to use the wrong
                one on accident. Give one of them a new name.

                ── UNUSED DEFINITION ───────────────────────────────────────────────────────────

                `Booly` is not used anywhere in your code.

                3│  Booly : [ Yes, No, Maybe ]
                    ^^^^^^^^^^^^^^^^^^^^^^^^^^
                
                If you didn't intend on using `Booly` then remove it so future readers
                of your code don't wonder why it is there.
                
                ── UNUSED DEFINITION ───────────────────────────────────────────────────────────
                
                `Booly` is not used anywhere in your code.
                
                1│  Booly : [ Yes, No ]
                    ^^^^^^^^^^^^^^^^^^^

                If you didn't intend on using `Booly` then remove it so future readers
                of your code don't wonder why it is there.
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
    //                 ]
    //
    //             Booly :
    //                 [
    //                     Yes,
    //                     No,
    //                     Maybe
    //                 ]
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
    //             5│>    ]
    //
    //             But then it's defined a second time here:
    //
    //             7 │> Booly :
    //             8 │>    [
    //             9 │>        Yes,
    //             10│>        No,
    //             11│>        Maybe
    //             12│>    ]
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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── UNRECOGNIZED NAME ───────────────────────────────────────────────────────────

                I cannot find a `bar` value

                8│          4 -> bar baz "yay"
                                 ^^^

                Did you mean one of these?

                    baz
                    Nat
                    Str
                    Err
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
                ── UNRECOGNIZED NAME ───────────────────────────────────────────────────────────

                I cannot find a `true` value

                1│  if true then 1 else 2
                       ^^^^

                Did you mean one of these?

                    True
                    Str
                    Num
                    Err
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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                     div [ class ] []

                 div = \_, _ -> 4

                 box "wizard" []
             "#
            ),
            indoc!(
                r#"
                 ── UNUSED ARGUMENT ─────────────────────────────────────────────────────────────

                 `box` doesn't use `htmlChildren`.

                 3│  box = \class, htmlChildren ->
                                   ^^^^^^^^^^^^

                 If you don't need `htmlChildren`, then you can just remove it. However,
                 if you really do need `htmlChildren` as an argument of `box`, prefix it
                 with an underscore, like this: "_`htmlChildren`". Adding an underscore
                 at the start of a variable name is a way of saying that the variable
                 is not used.

                 ── UNUSED DEFINITION ───────────────────────────────────────────────────────────

                 `y` is not used anywhere in your code.

                 1│  y = 9
                     ^

                 If you didn't intend on using `y` then remove it so future readers of
                 your code don't wonder why it is there.
                "#
            ),
        );
    }

    // #[test]
    // fn report_unused_import() {
    //     report_problem_as(
    //         indoc!(
    //             r#"
    //              interface Report
    //                  exposes [
    //                      plainText,
    //                      emText
    //                  ]
    //                  imports [
    //                      Symbol.{ Interns }
    //                  ]

    //              plainText = \str -> PlainText str

    //              emText = \str -> EmText str
    //          "#
    //         ),
    //         indoc!(
    //             r#"
    //              Nothing from Symbol is used in this module.

    //              6│ imports [
    //              7│     Symbol.{ Interns }
    //                      ^^^^^^
    //              8│ ]

    //              Since Symbol isn't used, you don't need to import it."#
    //         ),
    //     );
    // }

    #[test]
    fn report_value_color() {
        let src: &str = indoc!(
            r#"
                activityIndicatorLarge = div

                view activityIndicatorLarge
            "#
        );

        let arena = Bump::new();
        let (_type_problems, _can_problems, _mono_problems, home, interns) =
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
        let (_type_problems, _can_problems, _mono_problems, home, mut interns) =
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
                <cyan>── UNRECOGNIZED NAME ───────────────────────────────────────────────────────────<reset>

                I cannot find a `theAdmin` value

                <cyan>3<reset><cyan>│<reset>  <white>theAdmin<reset>
                    <red>^^^^^^^^<reset>

                Did you mean one of these?

                    Decimal
                    Dec
                    Result
                    Num
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                This `if` has an `else` branch with a different type from its `then` branch:

                1│  if True then 2 else "foo"
                                        ^^^^^

                The `else` branch is a string of type:

                    Str

                but the `then` branch has the type:

                    Num a

                I need all branches in an `if` to have the same type!
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 3rd branch of this `if` does not match all the previous branches:

                1│  if True then 2 else if False then 2 else "foo"
                                                             ^^^^^

                The 3rd branch is a string of type:

                    Str

                But all the previous branches have type:

                    Num a

                I need all branches in an `if` to have the same type!
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
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 2nd branch of this `when` does not match all the previous branches:

                1│  when 1 is
                2│      2 -> "foo"
                3│      3 -> {}
                             ^^

                The 2nd branch is a record of type:

                    {}

                But all the previous branches have type:

                    Str

                I need all branches of a `when` to have the same type!
                "#
            ),
        )
    }

    #[test]
    fn elem_in_list() {
        report_problem_as(
            indoc!(
                r#"
                [ 1, 3, "foo" ]
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                This list contains elements with different types:

                1│  [ 1, 3, "foo" ]
                            ^^^^^

                Its 3rd element is a string of type:

                    Str

                However, the preceding elements in the list all have the type:

                    Num a

                I need every element in a list to have the same type!
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                ── CIRCULAR TYPE ───────────────────────────────────────────────────────────────

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
                ── CIRCULAR TYPE ───────────────────────────────────────────────────────────────

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
    fn record_field_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                bar = { bar : 0x3 }

                f : { foo : Int * } -> Bool
                f = \_ -> True

                f bar
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                f : [ Red, Green ] -> Bool
                f = \_ -> True

                f Blue
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 1st argument to `f` is not what I expect:

                4│  f Blue
                      ^^^^

                This `Blue` global tag has the type:

                    [ Blue ]a

                But `f` needs the 1st argument to be:

                    [ Green, Red ]

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
                f : [ Red (Int *), Green Bool ] -> Bool
                f = \_ -> True

                f (Blue 3.14)
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 1st argument to `f` is not what I expect:

                4│  f (Blue 3.14)
                       ^^^^^^^^^

                This `Blue` global tag application has the type:

                    [ Blue (Float a) ]b

                But `f` needs the 1st argument to be:

                    [ Green Bool, Red (Int *) ]

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
                x : Int *
                x = if True then 3.14 else 4

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the `then` branch of this `if` expression:

                1│  x : Int *
                2│  x = if True then 3.14 else 4
                                     ^^^^

                The 1st branch is a float of type:

                    Float a

                But the type annotation on `x` says it should be:

                    Int *

                Tip: You can convert between Int and Float using functions like
                `Num.toFloat` and `Num.round`.
                "#
            ),
        )
    }

    #[test]
    fn from_annotation_when() {
        report_problem_as(
            indoc!(
                r#"
                x : Int *
                x =
                    when True is
                        _ -> 3.14

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `x` definition:

                1│   x : Int *
                2│   x =
                3│>      when True is
                4│>          _ -> 3.14

                This `when` expression produces:

                    Float a

                But the type annotation on `x` says it should be:

                    Int *

                Tip: You can convert between Int and Float using functions like
                `Num.toFloat` and `Num.round`.
                "#
            ),
        )
    }

    #[test]
    fn from_annotation_function() {
        report_problem_as(
            indoc!(
                r#"
                x : Int * -> Int *
                x = \_ -> 3.14

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `x` definition:

                1│  x : Int * -> Int *
                2│  x = \_ -> 3.14
                              ^^^^

                The body is a float of type:

                    Float a

                But the type annotation on `x` says it should be:

                    Int *

                Tip: You can convert between Int and Float using functions like
                `Num.toFloat` and `Num.round`.
                "#
            ),
        )
    }

    #[test]
    fn fncall_value() {
        report_problem_as(
            indoc!(
                r#"
                x : I64
                x = 42

                x 3
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY ARGS ───────────────────────────────────────────────────────────────

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
                f : I64 -> I64
                f = \_ -> 42

                f 1 2
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY ARGS ───────────────────────────────────────────────────────────────

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
                f : I64, I64 -> I64
                f = \_, _ -> 42

                f 1
                "#
            ),
            indoc!(
                r#"
                ── TOO FEW ARGS ────────────────────────────────────────────────────────────────

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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 1st pattern in this `when` is causing a mismatch:

                2│      {} -> 42
                        ^^

                The first pattern is trying to match record values of type:

                    {}a

                But the expression between `when` and `is` has the type:

                    Num a
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 1st pattern in this `when` is causing a mismatch:

                2│      { foo: True } -> 42
                        ^^^^^^^^^^^^^

                The first pattern is trying to match record values of type:

                    { foo : [ True ] }

                But the expression between `when` and `is` has the type:

                    { foo : Num a }
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 1st pattern in this `when` is causing a mismatch:

                2│      { foo: True } -> 42
                        ^^^^^^^^^^^^^

                The first pattern is trying to match record values of type:

                    { foo : [ True ] }

                But the expression between `when` and `is` has the type:

                    { foo : Str }
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
                     { foo: 2 } -> foo
                 "#
            ),
            indoc!(
                r#"
                ── UNRECOGNIZED NAME ───────────────────────────────────────────────────────────

                I cannot find a `foo` value

                2│      { foo: 2 } -> foo
                                      ^^^

                Did you mean one of these?

                    Bool
                    U8
                    F64
                    Nat
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 1st pattern in this `when` is causing a mismatch:

                2│      {} | 1 -> 3
                        ^^^^^^

                The first pattern is trying to match numbers:

                    Num a

                But the expression between `when` and `is` has the type:

                    { foo : Num a }
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                This expression is used in an unexpected way:

                1│  (Foo x) = 42
                              ^^

                It is a number of type:

                    Num a

                But you are trying to use it as:

                    [ Foo a ]
                "#
            ),
        )
    }

    #[test]
    fn from_annotation_complex_pattern() {
        report_problem_as(
            indoc!(
                r#"
                { x } : { x : Int * }
                { x } = { x: 4.0 }

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of this definition:

                1│  { x } : { x : Int * }
                2│  { x } = { x: 4.0 }
                            ^^^^^^^^^^

                The body is a record of type:

                    { x : Float a }

                But the type annotation says it should be:

                    { x : Int * }

                Tip: You can convert between Int and Float using functions like
                `Num.toFloat` and `Num.round`.
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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                x : { a : Int *, b : Float *, c : Bool }
                x = { b: 4.0 }

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `x` definition:

                1│  x : { a : Int *, b : Float *, c : Bool }
                2│  x = { b: 4.0 }
                        ^^^^^^^^^^

                The body is a record of type:

                    { b : Float a }

                But the type annotation on `x` says it should be:

                    { a : Int *, b : Float *, c : Bool }

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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                f : Bool -> msg
                f = \_ -> Foo

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `f` definition:

                1│  f : Bool -> msg
                2│  f = \_ -> Foo
                              ^^^

                This `Foo` global tag has the type:

                    [ Foo ]a

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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                f : Bool -> [ Ok I64, InvalidFoo ]
                f = \_ -> ok 4

                f
                "#
            ),
            indoc!(
                r#"
                ── UNRECOGNIZED NAME ───────────────────────────────────────────────────────────

                I cannot find a `ok` value

                2│  f = \_ -> ok 4
                              ^^

                Did you mean one of these?

                    Ok
                    U8
                    f
                    I8
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
                f : Bool -> I64
                f = \_ ->
                    ok = 3

                    Ok

                f
                "#
            ),
            indoc!(
                r#"
                ── UNUSED DEFINITION ───────────────────────────────────────────────────────────

                `ok` is not used anywhere in your code.

                3│      ok = 3
                        ^^

                If you didn't intend on using `ok` then remove it so future readers of
                your code don't wonder why it is there.

                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `f` definition:

                1│  f : Bool -> I64
                2│  f = \_ ->
                3│      ok = 3
                4│
                5│      Ok
                        ^^

                This `Ok` global tag has the type:

                    [ Ok ]a

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
                ── CIRCULAR DEFINITION ─────────────────────────────────────────────────────────

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
                ── CIRCULAR DEFINITION ─────────────────────────────────────────────────────────

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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The `x` record does not have a `.foo` field:

                3│  { x & foo: 3 }
                          ^^^^^^

                In fact, `x` is a record with NO fields!
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The `x` record does not have a `.foo` field:

                3│  { x & foo: 3 }
                          ^^^^^^

                This is usually a typo. Here are the `x` fields that are most similar:

                    { fo : Num b
                    , bar : Num a
                    }

                So maybe `.foo` should be `.fo`?
                "#
            ),
        )
    }

    #[test]
    fn update_record_ext() {
        report_problem_as(
            indoc!(
                r#"
                f : { fo: I64 }ext -> I64
                f = \r ->
                    r2 = { r & foo: r.fo }

                    r2.fo

                f
                "#
            ),
            // TODO also suggest fields with the correct type
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The `r` record does not have a `.foo` field:

                3│      r2 = { r & foo: r.fo }
                                   ^^^^^^^^^

                This is usually a typo. Here are the `r` fields that are most similar:

                    { fo : I64
                    }ext

                So maybe `.foo` should be `.fo`?
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The `x` record does not have a `.foo` field:

                3│  { x & foo: 3 }
                          ^^^^^^

                This is usually a typo. Here are the `x` fields that are most similar:

                    { fo : Num c
                    , foobar : Num d
                    , bar : Num a
                    , baz : Num b
                    , ...
                    }

                So maybe `.foo` should be `.fo`?
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
    fn int_float() {
        report_problem_as(
            indoc!(
                r#"
                0x4 + 3.14
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 2nd argument to `add` is not what I expect:

                1│  0x4 + 3.14
                          ^^^^

                This argument is a float of type:

                    Float a

                But `add` needs the 2nd argument to be:

                    Num (Integer a)

                Tip: You can convert between Int and Float using functions like
                `Num.toFloat` and `Num.round`.
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 2nd argument to `add` is not what I expect:

                1│  42 + True
                         ^^^^

                This `True` boolean has the type:

                    [ True ]a

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
                f : [ A ] -> [ A, B ]
                f = \a -> a

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `f` definition:

                1│  f : [ A ] -> [ A, B ]
                2│  f = \a -> a
                              ^

                This `a` value is a:

                    [ A ]

                But the type annotation on `f` says it should be:

                    [ A, B ]

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
                f : [ A ] -> [ A, B, C ]
                f = \a -> a

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `f` definition:

                1│  f : [ A ] -> [ A, B, C ]
                2│  f = \a -> a
                              ^

                This `a` value is a:

                    [ A ]

                But the type annotation on `f` says it should be:

                    [ A, B, C ]

                Tip: Looks like a closed tag union does not have the `C` and `B` tags.

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
                Either : [ Left I64, Right Bool ]

                x : Either
                x = Left 42

                f : Either -> I64
                f = \Left v -> v

                f x
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────────────────────────────

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
                x : [ Left I64, Right Bool ]
                x = Left 42


                (Left y) = x

                y
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                This expression is used in an unexpected way:

                5│  (Left y) = x
                               ^

                This `x` value is a:

                    [ Left I64, Right Bool ]

                But you are trying to use it as:

                    [ Left a ]

                Tip: Seems like a tag typo. Maybe `Right` should be `Left`?

                Tip: Can more type annotations be added? Type annotations always help
                me give more specific messages, and I think they could help a lot in
                this case
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
                ── UNSAFE PATTERN ──────────────────────────────────────────────────────────────

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
                x : Bool
                x = True

                when x is
                    False -> 3
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────────────────────────────

                This `when` does not cover all the possibilities:

                4│>  when x is
                5│>      False -> 3

                Other possibilities include:

                    True

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
                x : [ Red, Green, Blue ]
                x = Red

                when x is
                    Red -> 0
                    Green -> 1
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────────────────────────────

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
                RemoteData e a :  [ NotAsked, Loading, Failure e, Success a ]

                x : RemoteData I64 Str

                when x is
                    NotAsked -> 3
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────────────────────────────

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
                ── UNSAFE PATTERN ──────────────────────────────────────────────────────────────

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
                y : [ Nothing, Just I64 ]
                y = Just 4
                x = { a: y, b: 42}

                when x is
                    { a: Nothing } -> 4
                    { a: Just 3 } -> 4
                "#
            ),
            indoc!(
                r#"
                ── UNSAFE PATTERN ──────────────────────────────────────────────────────────────

                This `when` does not cover all the possibilities:

                5│>  when x is
                6│>      { a: Nothing } -> 4
                7│>      { a: Just 3 } -> 4

                Other possibilities include:

                    { a: Just _, b }

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
                ── UNSAFE PATTERN ──────────────────────────────────────────────────────────────

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
                ── REDUNDANT PATTERN ───────────────────────────────────────────────────────────

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
                Foo a : { x : Int a }

                f : Foo a -> Int a
                f = \r -> r.x

                f { y: 3.14 }
                "#
            ),
            // de-aliases the alias to give a better error message
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 1st argument to `f` is not what I expect:

                6│  f { y: 3.14 }
                      ^^^^^^^^^^^

                This argument is a record of type:

                    { y : Float a }

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
                ── CYCLIC ALIAS ────────────────────────────────────────────────────────────────

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

                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── CYCLIC ALIAS ────────────────────────────────────────────────────────────────

                The `Foo` alias is self-recursive in an invalid way:

                1│  Foo : { x : Foo }
                    ^^^

                Recursion in aliases is only allowed if recursion happens behind a
                tag.
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
                ── DUPLICATE FIELD NAME ────────────────────────────────────────────────────────

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
                ── DUPLICATE FIELD NAME ────────────────────────────────────────────────────────

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
                ── DUPLICATE FIELD NAME ────────────────────────────────────────────────────────

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
                ── DUPLICATE FIELD NAME ────────────────────────────────────────────────────────

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
                a : { foo : I64, bar : F64, foo : Str }
                a = { bar: 3.0, foo: "foo" }

                a
                "#
            ),
            indoc!(
                r#"
                ── DUPLICATE FIELD NAME ────────────────────────────────────────────────────────

                This record type defines the `.foo` field twice!

                1│  a : { foo : I64, bar : F64, foo : Str }
                          ^^^^^^^^^             ^^^^^^^^^

                In the rest of the program, I will only use the latter definition:

                1│  a : { foo : I64, bar : F64, foo : Str }
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
                a : [ Foo I64, Bar F64, Foo Str ]
                a = Foo "foo"

                a
                "#
            ),
            indoc!(
                r#"
                ── DUPLICATE TAG NAME ──────────────────────────────────────────────────────────

                This tag union type defines the `Foo` tag twice!

                1│  a : [ Foo I64, Bar F64, Foo Str ]
                          ^^^^^^^           ^^^^^^^

                In the rest of the program, I will only use the latter definition:

                1│  a : [ Foo I64, Bar F64, Foo Str ]
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
                bar : I64
                foo = \x -> x

                # NOTE: neither bar or foo are defined at this point
                4
                "#
            ),
            indoc!(
                r#"
                ── NAMING PROBLEM ──────────────────────────────────────────────────────────────

                This annotation does not match the definition immediately following
                it:

                1│>  bar : I64
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
                bar : I64

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
                MyAlias 1 : I64

                4
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This pattern in the definition of `MyAlias` is not what I expect:

                1│  MyAlias 1 : I64
                            ^

                Only type variables like `a` or `value` can occur in this position.

                ── UNUSED DEFINITION ───────────────────────────────────────────────────────────

                `MyAlias` is not used anywhere in your code.

                1│  MyAlias 1 : I64
                    ^^^^^^^^^^^^^^^

                If you didn't intend on using `MyAlias` then remove it so future readers
                of your code don't wonder why it is there.
                "#
            ),
        )
    }

    #[test]
    fn invalid_num() {
        report_problem_as(
            indoc!(
                r#"
                a : Num I64 F64
                a = 3

                a
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY TYPE ARGUMENTS ─────────────────────────────────────────────────────

                The `Num` alias expects 1 type argument, but it got 2 instead:

                1│  a : Num I64 F64
                        ^^^^^^^^^^^

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
                f : Bool -> Num I64 F64
                f = \_ -> 3

                f
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY TYPE ARGUMENTS ─────────────────────────────────────────────────────

                The `Num` alias expects 1 type argument, but it got 2 instead:

                1│  f : Bool -> Num I64 F64
                                ^^^^^^^^^^^

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
                Pair a b : [ Pair a b ]

                x : Pair I64
                x = Pair 2 3

                x
                "#
            ),
            indoc!(
                r#"
                ── TOO FEW TYPE ARGUMENTS ──────────────────────────────────────────────────────

                The `Pair` alias expects 2 type arguments, but it got 1 instead:

                3│  x : Pair I64
                        ^^^^^^^^

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
                Pair a b : [ Pair a b ]

                x : Pair I64 I64 I64
                x = 3

                x
                "#
            ),
            indoc!(
                r#"
                ── TOO MANY TYPE ARGUMENTS ─────────────────────────────────────────────────────

                The `Pair` alias expects 2 type arguments, but it got 3 instead:

                3│  x : Pair I64 I64 I64
                        ^^^^^^^^^^^^^^^^

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
                Foo a : [ Foo ]

                f : Foo I64

                f
                "#
            ),
            indoc!(
                r#"
                ── UNUSED TYPE ALIAS PARAMETER ─────────────────────────────────────────────────

                The `a` type parameter is not used in the `Foo` alias definition:

                1│  Foo a : [ Foo ]
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
                ── ARGUMENTS BEFORE EQUALS ─────────────────────────────────────────────────────

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
                ConsList a : [ Cons a (ConsList a), Nil ]

                x : ConsList {}
                x = Cons {} (Cons "foo" Nil)

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `x` definition:

                3│  x : ConsList {}
                4│  x = Cons {} (Cons "foo" Nil)
                        ^^^^^^^^^^^^^^^^^^^^^^^^

                This `Cons` global tag application has the type:

                    [ Cons {} [ Cons Str [ Cons {} a, Nil ] as a, Nil ], Nil ]

                But the type annotation on `x` says it should be:

                    [ Cons {} a, Nil ] as a
                "#
            ),
        )
    }

    #[test]
    fn mutually_recursive_types_with_type_error() {
        report_problem_as(
            indoc!(
                r#"
                AList a b : [ ACons a (BList a b), ANil ]
                BList a b : [ BCons a (AList a b), BNil ]

                x : AList I64 I64
                x = ACons 0 (BCons 1 (ACons "foo" BNil ))

                y : BList a a
                y = BNil

                { x, y }
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `x` definition:

                4│  x : AList I64 I64
                5│  x = ACons 0 (BCons 1 (ACons "foo" BNil ))
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                This `ACons` global tag application has the type:

                    [ ACons Num (Integer Signed64) [ BCons (Num a) [ ACons Str [ BNil
                    ]b ]c ]d, ANil ]

                But the type annotation on `x` says it should be:

                    [ ACons I64 BList I64 I64, ANil ]
                "#
            ),
        )
    }

    #[test]
    fn integer_out_of_range() {
        report_problem_as(
            indoc!(
                r#"
                x = 9_223_372_036_854_775_807_000

                y = -9_223_372_036_854_775_807_000

                h = 0x8FFF_FFFF_FFFF_FFFF
                l = -0x8FFF_FFFF_FFFF_FFFF

                minlit = -9_223_372_036_854_775_808
                maxlit =  9_223_372_036_854_775_807

                x + y + h + l + minlit + maxlit
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This integer literal is too big:

                1│  x = 9_223_372_036_854_775_807_000
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Roc uses signed 64-bit integers, allowing values between
                −9_223_372_036_854_775_808 and 9_223_372_036_854_775_807.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This integer literal is too small:

                3│  y = -9_223_372_036_854_775_807_000
                        ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                Roc uses signed 64-bit integers, allowing values between
                −9_223_372_036_854_775_808 and 9_223_372_036_854_775_807.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This integer literal is too big:

                5│  h = 0x8FFF_FFFF_FFFF_FFFF
                        ^^^^^^^^^^^^^^^^^^^^^

                Roc uses signed 64-bit integers, allowing values between
                −9_223_372_036_854_775_808 and 9_223_372_036_854_775_807.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This integer literal is too small:

                6│  l = -0x8FFF_FFFF_FFFF_FFFF
                        ^^^^^^^^^^^^^^^^^^^^^^

                Roc uses signed 64-bit integers, allowing values between
                −9_223_372_036_854_775_808 and 9_223_372_036_854_775_807.

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This float literal is too big:

                1│  overflow = 11.7976931348623157e308
                               ^^^^^^^^^^^^^^^^^^^^^^^

                Roc uses signed 64-bit floating points, allowing values between
                -1.7976931348623157e308 and 1.7976931348623157e308

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This integer literal contains an invalid digit:

                1│  dec = 100A
                          ^^^^

                Integer literals can only contain the digits 0-9.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This hex integer literal contains an invalid digit:

                3│  hex = 0xZZZ
                          ^^^^^

                Hexadecimal (base-16) integer literals can only contain the digits
                0-9, a-f and A-F.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This octal integer literal contains an invalid digit:

                5│  oct = 0o9
                          ^^^

                Octal (base-8) integer literals can only contain the digits 0-7.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This binary integer literal contains an invalid digit:

                7│  bin = 0b2
                          ^^^

                Binary (base-2) integer literals can only contain the digits 0 and 1.

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This hex integer literal contains no digits:

                3│  hex = 0x
                          ^^

                Hexadecimal (base-16) integer literals must contain at least one of
                the digits 0-9, a-f and A-F.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This octal integer literal contains no digits:

                5│  oct = 0o
                          ^^

                Octal (base-8) integer literals must contain at least one of the
                digits 0-7.

                Tip: Learn more about number literals at TODO

                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This binary integer literal contains no digits:

                7│  bin = 0b
                          ^^

                Binary (base-2) integer literals must contain at least one of the
                digits 0 and 1.

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This float literal contains an invalid digit:

                1│  x = 3.0A
                        ^^^^

                Floating point literals can only contain the digits 0-9, or use
                scientific notation 10e4

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── MODULE NOT IMPORTED ─────────────────────────────────────────────────────────

                The `Foo` module is not imported:

                1│  Foo.test
                    ^^^^^^^^

                Is there an import missing? Perhaps there is a typo. Did you mean one
                of these?

                    Bool
                    Num
                    Set
                    Str
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 2nd argument to `add` is not what I expect:

                1│  \{ x, y ? True } -> x + y
                                            ^

                This `y` value is a:

                    [ True ]a

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
                f : { x : I64, y ? I64 } -> I64
                f = \{ x, y ? "foo" } -> (\g, _ -> g) x y

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                    { x, y } : { x : I64, y ? Bool }
                    { x, y } = rec

                    { x, y }
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of this definition:

                2│>      { x, y } : { x : I64, y ? Bool }
                3│>      { x, y } = rec

                The body is a value of type:

                    { x : I64, y : Bool }

                But the type annotation says it should be:

                    { x : I64, y ? Bool }

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
                f : { x : I64, y ? I64 } -> I64
                f = \{ x, y } -> x + y

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                f : { x : I64, y ? I64 } -> I64
                f = \r ->
                        when r is
                            { x, y } -> x + y

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 1st pattern in this `when` is causing a mismatch:

                4│              { x, y } -> x + y
                                ^^^^^^^^

                The first pattern is trying to match record values of type:

                    { x : I64, y : I64 }

                But the expression between `when` and `is` has the type:

                    { x : I64, y ? I64 }

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
                f : { x : I64, y ? I64 } -> I64
                f = \r -> r.y

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                    f : { x : I64, y ? I64 } -> I64
                    f = \r -> .y r

                    f
                    "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                f : { x : I64, y : I64 } -> I64
                f = \r ->
                        when r is
                            { x, y : "foo" } -> x + 0
                            _ -> 0

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 1st pattern in this `when` is causing a mismatch:

                4│              { x, y : "foo" } -> x + 0
                                ^^^^^^^^^^^^^^^^

                The first pattern is trying to match record values of type:

                    { x : I64, y : Str }

                But the expression between `when` and `is` has the type:

                    { x : I64, y : I64 }
                "#
            ),
        )
    }

    #[test]
    fn optional_field_mismatch_with_annotation() {
        report_problem_as(
            indoc!(
                r#"
                f : { x : I64, y ? I64 } -> I64
                f = \r ->
                        when r is
                            { x, y ? "foo" } -> (\g, _ -> g) x y
                            _ -> 0

                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 1st pattern in this `when` is causing a mismatch:

                4│              { x, y ? "foo" } -> (\g, _ -> g) x y
                                ^^^^^^^^^^^^^^^^

                The first pattern is trying to match record values of type:

                    { x : I64, y ? Str }

                But the expression between `when` and `is` has the type:

                    { x : I64, y ? I64 }
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
                ── BAD OPTIONAL VALUE ──────────────────────────────────────────────────────────

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
            ── REDUNDANT PATTERN ───────────────────────────────────────────────────────────

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
                NodeColor : [ Red, Black ]

                RBTree k v : [ Node NodeColor k v (RBTree k v) (RBTree k v), Empty ]

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
            ── UNUSED ARGUMENT ─────────────────────────────────────────────────────────────

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
    fn qualified_global_tag() {
        report_problem_as(
            indoc!(
                r#"
                Foo.Bar
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                I trying to parse a record field access here:

                1│  foo.bar.
                            ^

                So I expect to see a lowercase letter next, like .name or .height.
            "#
            ),
        )
    }

    #[test]
    fn qualified_private_tag() {
        report_problem_as(
            indoc!(
                r#"
                @Foo.Bar
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                I am very confused by this expression:

                1│  @Foo.Bar
                        ^^^^

                Looks like a private tag is treated like a module name. Maybe you
                wanted a qualified name, like Json.Decode.string?
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
                ── UNKNOWN OPERATOR ────────────────────────────────────────────────────────────

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
                ── TOO MANY ARGS ───────────────────────────────────────────────────────────────

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
                ── UNFINISHED TAG UNION TYPE ───────────────────────────────────────────────────

                I just started parsing a tag union type, but I got stuck here:

                1│  f : [
                         ^

                Tag unions look like [ Many I64, None ], so I was expecting to see a
                tag name next.
            "#
            ),
        )
    }

    #[test]
    fn tag_union_end() {
        report_problem_as(
            indoc!(
                r#"
                f : [ Yes,
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED TAG UNION TYPE ───────────────────────────────────────────────────

                I am partway through parsing a tag union type, but I got stuck here:

                1│  f : [ Yes,
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
                f : [ lowercase ]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD TAG NAME ──────────────────────────────────────────────────────────────

                I am partway through parsing a tag union type, but I got stuck here:

                1│  f : [ lowercase ]
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
                f : [ Good, bad ]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD TAG NAME ──────────────────────────────────────────────────────────────

                I am partway through parsing a tag union type, but I got stuck here:

                1│  f : [ Good, bad ]
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
                ── UNFINISHED RECORD TYPE ──────────────────────────────────────────────────────

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
                ── UNFINISHED RECORD TYPE ──────────────────────────────────────────────────────

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
                ── UNFINISHED RECORD TYPE ──────────────────────────────────────────────────────

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
    fn record_type_indent_end() {
        report_problem_as(
            indoc!(
                r#"
                f : { a: Int
                }
                "#
            ),
            indoc!(
                r#"
                ── NEED MORE INDENTATION ───────────────────────────────────────────────────────

                I am partway through parsing a record type, but I got stuck here:

                1│  f : { a: Int
                2│  }
                    ^

                I need this curly brace to be indented more. Try adding more spaces
                before it!
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
                ── UNFINISHED RECORD TYPE ──────────────────────────────────────────────────────

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
                ── UNFINISHED RECORD TYPE ──────────────────────────────────────────────────────

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
                ── TAB CHARACTER ───────────────────────────────────────────────────────────────

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
                ── TAB CHARACTER ───────────────────────────────────────────────────────────────

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
                ── UNFINISHED TYPE ─────────────────────────────────────────────────────────────

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
                ── UNFINISHED PARENTHESES ──────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── UNFINISHED TYPE ─────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── MISSING FINAL EXPRESSION ────────────────────────────────────────────────────

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
                ── UNFINISHED INLINE ALIAS ─────────────────────────────────────────────────────

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
                ── DOUBLE COMMA ────────────────────────────────────────────────────────────────

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
                ── UNFINISHED TYPE ─────────────────────────────────────────────────────────────

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
                ── UNFINISHED TYPE ─────────────────────────────────────────────────────────────

                I just started parsing a type, but I got stuck here:

                1│  f : I64, I64 ->
                                   ^

                Note: I may be confused by indentation
            "#
            ),
        )
    }

    #[test]
    fn invalid_private_tag_name() {
        // TODO could do better by pointing out we're parsing a function type
        report_problem_as(
            indoc!(
                r#"
                f : [ @Foo Bool, @100 I64 ]
                f = 0

                f
                "#
            ),
            indoc!(
                r#"
                ── WEIRD TAG NAME ──────────────────────────────────────────────────────────────

                I am partway through parsing a tag union type, but I got stuck here:

                1│  f : [ @Foo Bool, @100 I64 ]
                                     ^

                I was expecting to see a private tag name.

                Hint: Private tag names start with an `@` symbol followed by an
                uppercase letter, like @UID or @SecretKey.
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
                myDict : Dict I64 Str
                myDict = Dict.insert Dict.empty "foo" 42

                myDict
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `myDict` definition:

                1│  myDict : Dict I64 Str
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                ── IF GUARD NO CONDITION ───────────────────────────────────────────────────────

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
                ── UNFINISHED PATTERN ──────────────────────────────────────────────────────────

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
                ── MISSING EXPRESSION ──────────────────────────────────────────────────────────

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
                ── MISSING ARROW ───────────────────────────────────────────────────────────────

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
                ── UNFINISHED ARGUMENT LIST ────────────────────────────────────────────────────

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
                ── UNFINISHED ARGUMENT LIST ────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── UNEXPECTED ARROW ────────────────────────────────────────────────────────────

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
                ── UNFINISHED IF ───────────────────────────────────────────────────────────────

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
                ── UNFINISHED IF ───────────────────────────────────────────────────────────────

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
                [ 1, 2, , 3 ]
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED LIST ─────────────────────────────────────────────────────────────

                I am partway through started parsing a list, but I got stuck here:

                1│  [ 1, 2, , 3 ]
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
                [ 1, 2,
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED LIST ─────────────────────────────────────────────────────────────

                I am partway through started parsing a list, but I got stuck here:

                1│  [ 1, 2,
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
    fn list_bad_indent() {
        report_problem_as(
            indoc!(
                r#"
                x = [ 1, 2,
                ]

                x
                "#
            ),
            indoc!(
                r#"
                ── UNFINISHED LIST ─────────────────────────────────────────────────────────────

                I cannot find the end of this list:

                1│  x = [ 1, 2,
                               ^

                You could change it to something like [ 1, 2, 3 ] or even just [].
                Anything where there is an open and a close square bracket, and where
                the elements of the list are separated by commas.

                Note: I may be confused by indentation
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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This float literal contains an invalid digit:

                1│  1.1.1
                    ^^^^^

                Floating point literals can only contain the digits 0-9, or use
                scientific notation 10e4

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
                ── WEIRD CODE POINT ────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── INVALID UNICODE ─────────────────────────────────────────────────────────────

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
                ── WEIRD ESCAPE ────────────────────────────────────────────────────────────────

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
                ── ENDLESS STRING ──────────────────────────────────────────────────────────────

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
                ── ENDLESS STRING ──────────────────────────────────────────────────────────────

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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                This `if` has an `else` branch with a different type from its `then` branch:

                3│  if True then 1 else "\(greeting), World!"
                                        ^^^^^^^^^^^^^^^^^^^^^

                The `else` branch is a string of type:

                    Str

                but the `then` branch has the type:

                    Num a

                I need all branches in an `if` to have the same type!
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
r#"── TYPE MISMATCH ───────────────────────────────────────────────────────────────

This `if` has an `else` branch with a different type from its `then` branch:

1│  if True then "abc" else 1 {} 2
                            ^^{}^^

This comparison produces:

    Bool

but the `then` branch has the type:

    Str

I need all branches in an `if` to have the same type!
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                This expression is used in an unexpected way:

                3│  foo.if
                    ^^^^^^

                This `foo` value is a:

                    {}

                But you are trying to use it as:

                    { if : a }b


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
                ── NOT EXPOSED ─────────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                I trying to parse a record field access here:

                1│  Num.add . 23
                             ^

                So I expect to see a lowercase letter next, like .name or .height.
            "#
            ),
        )
    }

    #[test]
    fn private_tag_not_uppercase() {
        report_problem_as(
            indoc!(
                r#"
                Num.add @foo 23
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                I am trying to parse a private tag here:

                1│  Num.add @foo 23
                             ^

                But after the `@` symbol I found a lowercase letter. All tag names
                (global and private) must start with an uppercase letter, like @UUID
                or @Secrets.
            "#
            ),
        )
    }

    #[test]
    fn private_tag_field_access() {
        report_problem_as(
            indoc!(
                r#"
                @UUID.bar
                "#
            ),
            indoc!(
                r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                I am very confused by this field access:

                1│  @UUID.bar
                         ^^^^

                It looks like a record field access on a private tag.
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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── NAMING PROBLEM ──────────────────────────────────────────────────────────────

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
                [ "foo", bar("") ]
                "#
            ),
            indoc!(
                r#"
                ── UNRECOGNIZED NAME ───────────────────────────────────────────────────────────

                I cannot find a `bar` value

                1│  [ "foo", bar("") ]
                             ^^^

                Did you mean one of these?

                    Nat
                    Str
                    Err
                    U8
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
                ── UNKNOWN OPERATOR ────────────────────────────────────────────────────────────

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
                ── UNKNOWN OPERATOR ────────────────────────────────────────────────────────────

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
                ── UNKNOWN OPERATOR ────────────────────────────────────────────────────────────

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
                ── UNKNOWN OPERATOR ────────────────────────────────────────────────────────────

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
                    imports [pf.Task, Base64 ]
                    provides [ main, @Foo ] to pf
                "#
            ),
            indoc!(
                r#"
                ── WEIRD PROVIDES ──────────────────────────────────────────────────────────────

                I am partway through parsing a provides list, but I got stuck here:

                3│      imports [pf.Task, Base64 ]
                4│      provides [ main, @Foo ] to pf
                                         ^

                I was expecting a type name, value name or function name next, like

                    provides [ Animal, default, tame ]
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
                    provides [ mainForHost ]
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
                ── BAD REQUIRES ────────────────────────────────────────────────────────────────

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
    fn exposes_identifier() {
        report_header_problem_as(
            indoc!(
                r#"
                interface Foobar
                    exposes [ main, @Foo ]
                    imports [pf.Task, Base64 ]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD EXPOSES ───────────────────────────────────────────────────────────────

                I am partway through parsing a exposes list, but I got stuck here:

                1│  interface Foobar
                2│      exposes [ main, @Foo ]
                                        ^

                I was expecting a type name, value name or function name next, like

                    exposes [ Animal, default, tame ]
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
                    exposes [ main, @Foo ]
                    imports [pf.Task, Base64 ]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD MODULE NAME ───────────────────────────────────────────────────────────

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
                    exposes [ main, @Foo ]
                    imports [pf.Task, Base64 ]
                "#
            ),
            indoc!(
                r#"
                ── WEIRD APP NAME ──────────────────────────────────────────────────────────────

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
                ── TOO MANY ARGS ───────────────────────────────────────────────────────────────

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
                ── TOO MANY ARGS ───────────────────────────────────────────────────────────────

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
                x : List [ Foo Str ]
                x = List.map [ 1, 2 ] Foo

                x
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `x` definition:

                1│  x : List [ Foo Str ]
                2│  x = List.map [ 1, 2 ] Foo
                        ^^^^^^^^^^^^^^^^^^^^^

                This `map` call produces:

                    List [ Foo Num a ]

                But the type annotation on `x` says it should be:

                    List [ Foo Str ]
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
                ── UNFINISHED PARENTHESES ──────────────────────────────────────────────────────

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
                ── UNFINISHED PARENTHESES ──────────────────────────────────────────────────────

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
                ── UNFINISHED PARENTHESES ──────────────────────────────────────────────────────

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
                ── NEED MORE INDENTATION ───────────────────────────────────────────────────────

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
                ── UNFINISHED PATTERN ──────────────────────────────────────────────────────────

                I just started parsing a pattern, but I got stuck here:

                1│  \(
                      ^

                Note: I may be confused by indentation
            "#
            ),
        )
    }

    #[test]
    fn outdented_alias() {
        report_problem_as(
            indoc!(
                r#"
                Box item : [
                    Box item,
                    Items item item
                ]

                4
                "#
            ),
            indoc!(
                r#"
                ── NEED MORE INDENTATION ───────────────────────────────────────────────────────

                I am partway through parsing a tag union type, but I got stuck here:

                1│  Box item : [
                2│      Box item,
                3│      Items item item
                4│  ]
                    ^

                I need this square bracket to be indented more. Try adding more spaces
                before it!
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
                ── NEED MORE INDENTATION ───────────────────────────────────────────────────────

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
    fn outdented_record() {
        report_problem_as(
            indoc!(
                r#"
                Box : {
                    id: Str
                }

                4
                "#
            ),
            indoc!(
                r#"
                ── NEED MORE INDENTATION ───────────────────────────────────────────────────────

                I am partway through parsing a record type, but I got stuck here:

                1│  Box : {
                2│      id: Str
                3│  }
                    ^

                I need this curly brace to be indented more. Try adding more spaces
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
                x <- List.map [ "a", "b" ]

                x + 1
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 2nd argument to `map` is not what I expect:

                1│>  x <- List.map [ "a", "b" ]
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
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                mult : Num *, F64 -> F64
                mult = \a, b -> a * b

                mult 0 0
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 2nd argument to `mul` is not what I expect:

                2│  mult = \a, b -> a * b
                                        ^

                This `b` value is a:

                    F64

                But `mul` needs the 2nd argument to be:

                    Num *

                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `mult` definition:

                1│  mult : Num *, F64 -> F64
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
                mult : Num a, F64 -> F64
                mult = \a, b -> a * b

                mult 0 0
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 2nd argument to `mul` is not what I expect:

                2│  mult = \a, b -> a * b
                                        ^

                This `b` value is a:

                    F64

                But `mul` needs the 2nd argument to be:

                    Num a

                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `mult` definition:

                1│  mult : Num a, F64 -> F64
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
                ── TOO FEW TYPE ARGUMENTS ──────────────────────────────────────────────────────

                The `Result` alias expects 2 type arguments, but it got 1 instead:

                1│  canIGo : _ -> Result _
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
                ── TOO MANY TYPE ARGUMENTS ─────────────────────────────────────────────────────

                The `Result` alias expects 2 type arguments, but it got 3 instead:

                1│  canIGo : _ -> Result _ _ _
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `f` definition:

                1│  f : a, b, * -> *
                2│  f = \_, _, x2 ->
                3│      inner : * -> *
                4│      inner = \y -> y
                5│      inner x2
                        ^^^^^^^^

                The type annotation on `f` says this `inner` call should have the type:

                    *

                However, the type of this `inner` call is connected to another type in a
                way that isn't reflected in this annotation.

                Tip: Any connection between types must use a named type variable, not
                a `*`! Maybe the annotation  on `f` should have a named type variable in
                place of the `*`?
                "#
            ),
        )
    }

    #[test]
    fn error_inline_alias_not_an_alias() {
        report_problem_as(
            indoc!(
                r#"
                f : List elem -> [ Nil, Cons elem a ] as a
                "#
            ),
            indoc!(
                r#"
                ── NOT AN INLINE ALIAS ─────────────────────────────────────────────────────────

                The inline type after this `as` is not a type alias:

                1│  f : List elem -> [ Nil, Cons elem a ] as a
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
                f : List elem -> [ Nil, Cons elem a ] as Module.LinkedList a
                "#
            ),
            indoc!(
                r#"
                ── QUALIFIED ALIAS NAME ────────────────────────────────────────────────────────

                This type alias has a qualified name:

                1│  f : List elem -> [ Nil, Cons elem a ] as Module.LinkedList a
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
                f : List elem -> [ Nil, Cons elem a ] as LinkedList U
                "#
            ),
            indoc!(
                r#"
                ── TYPE ARGUMENT NOT LOWERCASE ─────────────────────────────────────────────────

                This alias type argument is not lowercase:

                1│  f : List elem -> [ Nil, Cons elem a ] as LinkedList U
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
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 1st argument to `isEmpty` is not what I expect:

                6│  isEmpty (Name "boo")
                             ^^^^^^^^^^

                This `Name` global tag application has the type:

                    [ Name Str ]a

                But `isEmpty` needs the 1st argument to be:

                    [ Email Str ]

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

                f : C a Nat -> D a Nat
                f = \c -> c 6
                f
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                ── CYCLIC ALIAS ────────────────────────────────────────────────────────────────

                The `F` alias is self-recursive in an invalid way:

                1│  F : F
                    ^

                Recursion in aliases is only allowed if recursion happens behind a
                tag.
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
                ── CYCLIC ALIAS ────────────────────────────────────────────────────────────────

                The `F` alias is self-recursive in an invalid way:

                1│  F : F
                    ^

                Recursion in aliases is only allowed if recursion happens behind a
                tag.
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
                ── CYCLIC ALIAS ────────────────────────────────────────────────────────────────

                The `F` alias is self-recursive in an invalid way:

                1│  F a b : F a b
                    ^

                Recursion in aliases is only allowed if recursion happens behind a
                tag.
                "#
            ),
        )
    }

    #[test]
    fn issue_2167_record_field_optional_and_required_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                Job : [ @Job { inputs : List Str } ]
                job : { inputs ? List Str } -> Job
                job = \{ inputs } ->
                    @Job { inputs }

                job { inputs: [ "build", "test" ] }
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

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
                Job : [ @Job { inputs : List Job } ]

                job : { inputs : List Str } -> Job
                job = \{ inputs } ->
                    @Job { inputs }

                job { inputs: [ "build", "test" ] }
                "#
            ),
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                Something is off with the body of the `job` definition:

                3│  job : { inputs : List Str } -> Job
                4│  job = \{ inputs } ->
                5│      @Job { inputs }
                        ^^^^^^^^^^^^^^^

                This `@Job` private tag application has the type:

                    [ @Job { inputs : List Str } ]

                But the type annotation on `job` says it should be:

                    [ @Job { inputs : List a } ] as a
                "#
            ),
        )
    }
}
