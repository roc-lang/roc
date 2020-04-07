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
    use roc_module::symbol::{Interns, ModuleId};
    use roc_reporting::report::{
        can_problem, em_text, plain_text, url, Report, ReportText, BLUE_CODE, BOLD_CODE, CYAN_CODE,
        DEFAULT_PALETTE, GREEN_CODE, MAGENTA_CODE, RED_CODE, RESET_CODE, UNDERLINE_CODE,
        WHITE_CODE, YELLOW_CODE,
    };
    use roc_reporting::type_error::type_problem;
    use roc_types::pretty_print::name_all_type_vars;
    use roc_types::subs::Subs;
    use std::path::PathBuf;
    // use roc_region::all;
    use crate::helpers::{can_expr, infer_expr, CanExprOut};
    use roc_reporting::report::ReportText::{Concat, Module, Region, Type, Value};
    use roc_solve::solve;
    use roc_types::subs::Content::{FlexVar, RigidVar, Structure};
    use roc_types::subs::FlatType::EmptyRecord;

    fn filename_from_string(str: &str) -> PathBuf {
        let mut filename = PathBuf::new();
        filename.push(str);

        return filename;
    }

    // use roc_problem::can;
    fn to_simple_report(text: ReportText) -> Report {
        Report {
            title: "SYNTAX PROBLEM".to_string(),
            text: text,
            filename: filename_from_string(r"\code\proj\Main.roc"),
        }
    }

    fn infer_expr_help(
        expr_src: &str,
    ) -> (
        Vec<solve::TypeError>,
        Vec<roc_problem::can::Problem>,
        Subs,
        ModuleId,
        Interns,
    ) {
        let CanExprOut {
            output,
            var_store,
            var,
            constraint,
            home,
            interns,
            problems: can_problems,
            ..
        } = can_expr(expr_src);
        let mut subs = Subs::new(var_store.into());

        for (var, name) in output.introduced_variables.name_by_var {
            subs.rigid_var(var, name);
        }

        let mut unify_problems = Vec::new();
        let (_content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        name_all_type_vars(var, &mut subs);

        (unify_problems, can_problems, subs, home, interns)
    }

    fn report_renders_as_from_src(src: &str, report: Report, expected_rendering: &str) {
        let (_type_problems, _can_problems, mut subs, home, interns) = infer_expr_help(src);
        let mut buf: String = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();

        report
            .text
            .render_ci(&mut buf, &mut subs, home, &src_lines, &interns);

        assert_eq!(buf, expected_rendering);
    }

    fn report_problem_as(src: &str, expected_rendering: &str) {
        let (type_problems, can_problems, mut subs, home, interns) = infer_expr_help(src);

        let mut buf: String = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();

        match can_problems.first() {
            None => {}
            Some(problem) => {
                let report = can_problem(
                    filename_from_string(r"\code\proj\Main.roc"),
                    problem.clone(),
                );
                report
                    .text
                    .render_ci(&mut buf, &mut subs, home, &src_lines, &interns)
            }
        }

        match type_problems.first() {
            None => {}
            Some(problem) => {
                let report = type_problem(
                    filename_from_string(r"\code\proj\Main.roc"),
                    problem.clone(),
                );
                report
                    .text
                    .render_ci(&mut buf, &mut subs, home, &src_lines, &interns)
            }
        }

        assert_eq!(buf, expected_rendering);
    }

    fn human_readable(str: &str) -> String {
        return str
            .replace(RED_CODE, "<red>")
            .replace(WHITE_CODE, "<white>")
            .replace(BLUE_CODE, "<blue>")
            .replace(YELLOW_CODE, "<yellow>")
            .replace(GREEN_CODE, "<green>")
            .replace(CYAN_CODE, "<cyan>")
            .replace(MAGENTA_CODE, "<magenta>")
            .replace(RESET_CODE, "<reset>")
            .replace(BOLD_CODE, "<bold>")
            .replace(UNDERLINE_CODE, "<underline>");
    }

    fn report_renders_in_color_from_src(src: &str, report: Report, expected_rendering: &str) {
        let (_type_problems, _can_problems, mut subs, home, interns) = infer_expr_help(src);
        let mut buf: String = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();

        report.text.render_color_terminal(
            &mut buf,
            &mut subs,
            home,
            &src_lines,
            &interns,
            &DEFAULT_PALETTE,
        );

        assert_eq!(human_readable(&buf), expected_rendering);
    }

    fn report_renders_in_color(report: Report, expected_rendering: &str) {
        report_renders_in_color_from_src(
            indoc!(
                r#"
                    x = 1
                    y = 2

                    x
                "#
            ),
            report,
            expected_rendering,
        )
    }

    fn report_renders_as(report: Report, expected_rendering: &str) {
        report_renders_as_from_src(
            indoc!(
                r#"
                    x = 1
                    y = 2

                    x
                "#
            ),
            report,
            expected_rendering,
        )
    }

    #[test]
    fn report_plain() {
        report_renders_as(to_simple_report(plain_text("y")), "y");
    }

    #[test]
    fn report_emphasized_text() {
        report_renders_as(to_simple_report(em_text("y")), "*y*");
    }

    #[test]
    fn report_url() {
        report_renders_as(
            to_simple_report(url("package.roc.org")),
            "<package.roc.org>",
        );
    }

    #[test]
    fn report_symbol() {
        let src: &str = indoc!(
            r#"
                x = 1
                y = 2

                x
            "#
        );

        let (_type_problems, _can_problems, mut subs, home, interns) = infer_expr_help(src);

        let mut buf = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();

        to_simple_report(Value(interns.symbol(test_home(), "x".into())))
            .text
            .render_ci(&mut buf, &mut subs, home, &src_lines, &interns);

        assert_eq!(buf, "`x`");
    }

    #[test]
    fn report_module() {
        let src: &str = indoc!(
            r#"
                x = 1
                y = 2

                x
            "#
        );

        let (_type_problems, _can_problems, mut subs, home, mut interns) = infer_expr_help(src);

        let mut buf = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();
        let module_id = interns.module_id(&"Main".into());

        to_simple_report(Module(module_id))
            .text
            .render_ci(&mut buf, &mut subs, home, &src_lines, &interns);

        assert_eq!(buf, "Main");
    }

    #[test]
    fn report_wildcard() {
        report_renders_as(to_simple_report(Type(FlexVar(None))), "*");
    }

    #[test]
    fn report_flex_var() {
        report_renders_as(to_simple_report(Type(FlexVar(Some("msg".into())))), "msg");
    }

    #[test]
    fn report_rigid_var() {
        report_renders_as(to_simple_report(Type(RigidVar("Str".into()))), "Str");
    }

    #[test]
    fn report_empty_record() {
        report_renders_as(to_simple_report(Type(Structure(EmptyRecord))), "{}");
    }

    #[test]
    fn report_batch_of_plain_text() {
        let mut report_texts = Vec::new();

        report_texts.push(plain_text("Wait a second. "));
        report_texts.push(plain_text("There is a problem here. -> "));
        report_texts.push(em_text("y"));

        report_renders_as(
            to_simple_report(Concat(report_texts)),
            "Wait a second. There is a problem here. -> *y*",
        );
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
                `y` is not used anywhere in your code.

                2 ┆  y = 2
                  ┆  ^

                If you didn't intend on using `y` then remove it so future readers of your code don't wonder why it is there."#
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
                `i` is first defined here:

                1 ┆  i = 1
                  ┆  ^

                But then it's defined a second time here:

                3 ┆  s = \i ->
                  ┆       ^

                Since these variables have the same name, it's easy to use the wrong one on accident. Give one of them a new name."#
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
            indoc!(
                r#"
                `Booly` is first defined here:

                1 ┆  Booly : [ Yes, No ]
                  ┆  ^^^^^^^^^^^^^^^^^^^

                But then it's defined a second time here:

                3 ┆  Booly : [ Yes, No, Maybe ]
                  ┆  ^^^^^^^^^^^^^^^^^^^^^^^^^^

                Since these variables have the same name, it's easy to use the wrong one on accident. Give one of them a new name."#
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
    //             1 ┆> Booly :
    //             2 ┆>    [
    //             3 ┆>        Yes,
    //             4 ┆>        No
    //             5 ┆>    ]
    //
    //             But then it's defined a second time here:
    //
    //             7  ┆> Booly :
    //             8  ┆>    [
    //             9  ┆>        Yes,
    //             10 ┆>        No,
    //             11 ┆>        Maybe
    //             12 ┆>    ]
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
                Using != and == together requires parentheses, to clarify how they should be grouped.

                3 ┆      if selectedId != thisId == adminsId then
                  ┆         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

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
                Using more than one == like this requires parentheses, to clarify how things should be grouped.

                2 ┆>      1
                3 ┆>          == 2
                4 ┆>          == 3

                "#
            ),
        )
    }

    // #[test]
    // fn report_unused_argument() {
    //     report_problem_as(
    //         indoc!(r#"
    //             y = 9
    //
    //             box = \class, htmlChildren ->
    //                 div [ class ] []
    //
    //             div = 4
    //
    //             box "wizard" []
    //         "#),
    //         indoc!(
    //             r#"
    //             box doesn't use htmlChildren.
    //
    //             3 ┆  box = \class, htmlChildren ->
    //
    //             If you don't need htmlChildren, then you can just remove it. However, if you really do need htmlChildren as an argument of box, prefix it with an underscore, like this: "_htmlChildren". Adding an underscore at the start of a variable name is a way of saying that the variable is not used."#
    //         ),
    //     );
    // }

    // #[test]
    // fn report_unused_import() {
    //     report_problem_as(
    //         indoc!(r#"
    //             interface Report
    //                 exposes [
    //                     plainText,
    //                     emText
    //                 ]
    //                 imports [
    //                     Symbol.{ Interns }
    //                 ]
    //
    //             plainText = \str -> PlainText str
    //
    //             emText = \str -> EmText str
    //         "#),
    //         indoc!(
    //             r#"
    //             Nothing from Symbol is used in this module.
    //
    //             6 ┆  imports [
    //             7 ┆      Symbol.{ Interns }
    //               ┆      ^^^^^^
    //             8 ┆  ]
    //
    //             Since Symbol isn't used, you don't need to import it."#
    //         ),
    //     );
    // }

    #[test]
    fn report_plain_text_color() {
        report_renders_in_color(to_simple_report(plain_text("y")), "<white>y<reset>");
    }

    #[test]
    fn report_em_text_color() {
        report_renders_in_color(to_simple_report(em_text("HELLO!")), "<bold>HELLO!<reset>");
    }

    #[test]
    fn report_url_color() {
        report_renders_in_color(
            to_simple_report(url("www.roc.com/blog")),
            "<underline>www.roc.com/blog<reset>",
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

        let (_type_problems, _can_problems, mut subs, home, interns) = infer_expr_help(src);

        let mut buf = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();

        to_simple_report(Value(
            interns.symbol(test_home(), "activityIndicatorLarge".into()),
        ))
        .text
        .render_color_terminal(
            &mut buf,
            &mut subs,
            home,
            &src_lines,
            &interns,
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

        let (_type_problems, _can_problems, mut subs, home, mut interns) = infer_expr_help(src);

        let mut buf = String::new();
        let src_lines: Vec<&str> = src.split('\n').collect();
        let module_id = interns.module_id(&"Util.Int".into());

        to_simple_report(Module(module_id))
            .text
            .render_color_terminal(
                &mut buf,
                &mut subs,
                home,
                &src_lines,
                &interns,
                &DEFAULT_PALETTE,
            );

        assert_eq!(human_readable(&buf), "<green>Util.Int<reset>");
    }

    #[test]
    fn report_wildcard_in_color() {
        report_renders_in_color(to_simple_report(Type(FlexVar(None))), "<yellow>*<reset>");
    }

    #[test]
    fn report_flex_var_in_color() {
        report_renders_in_color(
            to_simple_report(Type(FlexVar(Some("msg".into())))),
            "<yellow>msg<reset>",
        );
    }

    #[test]
    fn report_rigid_var_in_color() {
        report_renders_in_color(
            to_simple_report(Type(RigidVar("Str".into()))),
            "<yellow>Str<reset>",
        );
    }

    #[test]
    fn report_empty_record_in_color() {
        report_renders_in_color(
            to_simple_report(Type(Structure(EmptyRecord))),
            "<green>{}<reset>",
        );
    }

    #[test]
    fn report_batch_in_color() {
        let mut report_texts = Vec::new();

        report_texts.push(Type(RigidVar("List".into())));
        report_texts.push(plain_text(" "));
        report_texts.push(Type(Structure(EmptyRecord)));

        report_renders_in_color(
            to_simple_report(Concat(report_texts)),
            "<yellow>List<reset><white> <reset><green>{}<reset>",
        );
    }

    #[test]
    fn report_region_in_color() {
        report_renders_in_color_from_src(
            indoc!(
                r#"
                    isDisabled = \user -> user.isAdmin

                    theAdmin
                        |> isDisabled
                "#
            ),
            to_simple_report(Region(roc_region::all::Region {
                start_line: 0,
                end_line: 3,
                start_col: 0,
                end_col: 0,
            })),
            indoc!(
                r#"


                    <cyan>1<reset><magenta> ┆<reset><red>><reset>  <white>isDisabled = \user -> user.isAdmin<reset>
                    <cyan>2<reset><magenta> ┆<reset><red>><reset>
                    <cyan>3<reset><magenta> ┆<reset><red>><reset>  <white>theAdmin<reset>
                    <cyan>4<reset><magenta> ┆<reset><red>><reset>  <white>    |> isDisabled<reset>

                "#
            ),
        );
    }

    #[test]
    fn report_region() {
        report_renders_as_from_src(
            indoc!(
                r#"
                    x = 1
                    y = 2
                    f = \a -> a + 4

                    f x
                "#
            ),
            to_simple_report(Region(roc_region::all::Region {
                start_line: 1,
                end_line: 3,
                start_col: 0,
                end_col: 0,
            })),
            indoc!(
                r#"


                2 ┆>  y = 2
                3 ┆>  f = \a -> a + 4
                4 ┆>

                "#
            ),
        );
    }

    #[test]
    fn report_region_line_number_length_edge_case() {
        // the highest line number is 9, but it's rendered as 10.
        // Make sure that we render the line number as 2-wide
        report_renders_as_from_src(
            indoc!(
                r#"




                    x = 1
                    y = 2
                    f = \a -> a + 4

                    f x
                "#
            ),
            to_simple_report(Region(roc_region::all::Region {
                start_line: 7,
                end_line: 9,
                start_col: 0,
                end_col: 0,
            })),
            indoc!(
                r#"


                 8 ┆>
                 9 ┆>  f x
                10 ┆>

                "#
            ),
        );
    }

    #[test]
    fn report_region_different_line_number_lengths() {
        report_renders_as_from_src(
            indoc!(
                r#"
                    x = 1








                    y = 2
                    f = \a -> a + 4

                    f x
                "#
            ),
            to_simple_report(Region(roc_region::all::Region {
                start_line: 8,
                end_line: 10,
                start_col: 0,
                end_col: 0,
            })),
            indoc!(
                r#"


                     9 ┆>
                    10 ┆>  y = 2
                    11 ┆>  f = \a -> a + 4

                    "#
            ),
        );
    }

    //    #[test]
    //    fn shadowing_type_alias() {
    //        report_problem_as(
    //            indoc!(
    //                r#"
    //                foo : Int as Int
    //                foo = 42
    //
    //                foo
    //                "#
    //            ),
    //            indoc!(
    //                r#"
    //                You cannot mix (!=) and (==) without parentheses
    //
    //                3 ┆      if selectedId != thisId == adminsId then
    //                  ┆         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    //
    //                "#
    //            ),
    //        )
    //    }

    //    #[test]
    //    fn invalid_as_type_alias() {
    //        report_problem_as(
    //            indoc!(
    //                r#"
    //                foo : Int as a
    //                foo = 42
    //
    //                foo
    //                "#
    //            ),
    //            indoc!(
    //                r#"
    //                You cannot mix (!=) and (==) without parentheses
    //
    //                3 ┆      if selectedId != thisId == adminsId then
    //                  ┆         ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
    //
    //                "#
    //            ),
    //        )
    //    }

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
                This `if` condition needs to be a Bool:

                1 ┆  if "foo" then 2 else 3
                  ┆     ^^^^^

                Right now it’s a string of type:

                    Str

                But I need every `if` condition to evaluate to a Bool—either `True` or `False`.
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
                This `if` guard condition needs to be a Bool:

                2 ┆      2 if 1 -> 0x0
                  ┆           ^

                Right now it’s a number of type:

                    Num a

                But I need every `if` guard condition to evaluate to a Bool—either `True` or `False`.
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
                This `if` has an `else` branch with a different type from its `then` branch:

                1 ┆  if True then 2 else "foo"
                  ┆                      ^^^^^

                The `else` branch is a string of type:

                    Str

                but the `then` branch has the type:

                    Num a

                I need all branches in an `if` to have the same type!
                "#
            ),
        )
    }

    // #[test]
    // fn if_3_branch_mismatch() {
    //     report_problem_as(
    //         indoc!(
    //             r#"
    //             if True then 2 else if False then 2 else "foo"
    //             "#
    //         ),
    //         indoc!(
    //             r#"
    //             The 2nd branch of this `if` does not match all the previous branches:

    //             1 ┆  if True then 2 else "foo"
    //               ┆                      ^^^^^

    //             The 2nd branch is a string of type

    //                 Str

    //             But all the previous branches have the type

    //                 Num a

    //             "#
    //         ),
    //     )
    // }

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
                The 2nd branch of this `when` does not match all the previous branches:

                3 ┆      3 -> {}
                  ┆           ^^

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
                The 3rd element of this list does not match all the previous elements:

                1 ┆  [ 1, 3, "foo" ]
                  ┆          ^^^^^

                The 3rd element is a string of type:

                    Str

                But all the previous elements in the list have type:

                    Num a

                I need all elements of a list to have the same type!
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
                I cannot update the `.foo` field like this:

                4 ┆  { x & foo: "bar" }
                  ┆  ^^^^^^^^^^^^^^^^^^

                You are trying to update `.foo` to be a string of type:

                    Str

                But it should be:

                    {}

                Record update syntax does not allow you to change the type of fields. You can achieve that with record literal syntax.
                "#
            ),
        )
    }

    // needs a bit more infrastructure re. diffing records
    //    #[test]
    //    fn record_update_keys() {
    //        report_problem_as(
    //            indoc!(
    //                r#"
    //                x : { foo : {} }
    //                x = { foo: {} }
    //
    //                { x & baz: "bar" }
    //                "#
    //            ),
    //            indoc!(
    //                r#"
    //                The `x` record does not have a `baz` field:
    //
    //                4 ┆  { x & baz: "bar" }
    //                  ┆        ^^^
    //
    //                This is usually a typo. Here are the `x` fields that are most similar:
    //
    //                    { foo : {}
    //                    }
    //
    //                So maybe `baz` should be `foo`?
    //                "#
    //            ),
    //        )
    //    }

    //    #[test]
    //    fn num_literal() {
    //        report_problem_as(
    //            indoc!(
    //                r#"
    //                x : Str
    //                x = 4
    //
    //                x
    //                "#
    //            ),
    //            indoc!(
    //                r#"
    //                Something is off with the body of the `x` definition:
    //
    //                4 ┆  x = 4
    //                  ┆      ^
    //
    //                The body is a number of type:
    //
    //                    Num a
    //
    //                But the type annotation on `x` says that it should be:
    //
    //                    Str
    //
    //                "#
    //            ),
    //        )
    //    }

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
                I'm inferring a weird self-referential type for `g`:

                1 ┆  f = \g -> g g
                  ┆       ^

                Here is my best effort at writing down the type. You will see ∞ for parts of the type that repeat something already printed out infinitely.

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
                I'm inferring a weird self-referential type for `f`:

                1 ┆  f = \x -> f [x]
                  ┆  ^

                Here is my best effort at writing down the type. You will see ∞ for parts of the type that repeat something already printed out infinitely.

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

                f : { foo : Int } -> Bool
                f = \_ -> True

                f bar
                "#
            ),
            indoc!(
                r#"
                The 1st argument to `f` is not what I expect:

                6 ┆  f bar
                  ┆    ^^^

                This `bar` value is a:

                    { bar : Int }

                But `f` needs the 1st argument to be:

                    { foo : Int }


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
                The 1st argument to `f` is not what I expect:

                4 ┆  f Blue
                  ┆    ^^^^

                This `Blue` global tag application has the type:

                    [ Blue ]a

                But `f` needs the 1st argument to be:

                    [ Green, Red ]


                "#
            ),
        )
    }

    #[test]
    fn tag_with_arguments_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                f : [ Red Int, Green Bool ] -> Bool
                f = \_ -> True

                f (Blue 3.14)
                "#
            ),
            indoc!(
                r#"
                The 1st argument to `f` is not what I expect:

                4 ┆  f (Blue 3.14)
                  ┆     ^^^^^^^^^

                This `Blue` global tag application has the type:

                    [ Blue Float ]a

                But `f` needs the 1st argument to be:

                    [ Green Bool, Red Int ]


                "#
            ),
        )
    }

    #[test]
    fn from_annotation_if() {
        report_problem_as(
            indoc!(
                r#"
                x : Int
                x = if True then 3.14 else 4

                x
                "#
            ),
            indoc!(
                r#"
                Something is off with the 1st branch of this `if` expression:

                2 ┆  x = if True then 3.14 else 4
                  ┆                   ^^^^

                The 1st branch is a float of type:

                    Float

                But the type annotation on `x` says it should be:

                    Int


                "#
            ),
        )
    }

    #[test]
    fn from_annotation_when() {
        report_problem_as(
            indoc!(
                r#"
                x : Int
                x = 
                    when True is
                        _ -> 3.14

                x
                "#
            ),
            indoc!(
                r#"
                Something is off with the 1st branch of this `when` expression:

                4 ┆          _ -> 3.14
                  ┆               ^^^^

                The 1st branch is a float of type:

                    Float

                But the type annotation on `x` says it should be:

                    Int


                "#
            ),
        )
    }

    #[test]
    fn from_annotation_function() {
        report_problem_as(
            indoc!(
                r#"
                x : Int -> Int
                x = \_ -> 3.14

                x
                "#
            ),
            indoc!(
                r#"
                Something is off with the body of the `x` definition:

                2 ┆  x = \_ -> 3.14
                  ┆      ^^^^^^^^^^

                The body is an anonymous function of type:

                    Int -> Float

                But the type annotation on `x` says it should be:

                    Int -> Int


                "#
            ),
        )
    }
}
