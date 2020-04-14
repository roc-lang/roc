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
    use bumpalo::Bump;
    use roc_module::symbol::{Interns, ModuleId};
    use roc_mono::expr::{Expr, Procs};
    use roc_reporting::report::{
        can_problem, mono_problem, Report, BLUE_CODE, BOLD_CODE, CYAN_CODE, DEFAULT_PALETTE,
        GREEN_CODE, MAGENTA_CODE, RED_CODE, RESET_CODE, UNDERLINE_CODE, WHITE_CODE, YELLOW_CODE,
    };
    use roc_reporting::type_error::type_problem;
    use roc_types::pretty_print::name_all_type_vars;
    use roc_types::subs::Subs;
    use std::path::PathBuf;
    // use roc_region::all;
    use crate::helpers::{can_expr, infer_expr, CanExprOut};
    use roc_reporting::report::{RocDocAllocator, RocDocBuilder};
    use roc_solve::solve;

    fn filename_from_string(str: &str) -> PathBuf {
        let mut filename = PathBuf::new();
        filename.push(str);

        return filename;
    }

    fn to_simple_report<'b>(doc: RocDocBuilder<'b>) -> Report<'b> {
        Report {
            title: "".to_string(),
            doc: doc,
            filename: filename_from_string(r"\code\proj\Main.roc"),
        }
    }

    fn infer_expr_help(
        expr_src: &str,
    ) -> (
        Vec<solve::TypeError>,
        Vec<roc_problem::can::Problem>,
        Vec<roc_mono::expr::MonoProblem>,
        ModuleId,
        Interns,
    ) {
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
        } = can_expr(expr_src);
        let mut subs = Subs::new(var_store.into());

        for (var, name) in output.introduced_variables.name_by_var {
            subs.rigid_var(var, name);
        }

        let mut unify_problems = Vec::new();
        let (_content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        name_all_type_vars(var, &mut subs);

        let mut mono_problems = Vec::new();

        // MONO

        if unify_problems.is_empty() && can_problems.is_empty() {
            let arena = Bump::new();

            // Compile and add all the Procs before adding main
            let mut procs = Procs::default();
            let mut ident_ids = interns.all_ident_ids.remove(&home).unwrap();

            // assume 64-bit pointers
            let pointer_size = std::mem::size_of::<u64>() as u32;

            // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
            let _mono_expr = Expr::new(
                &arena,
                &mut subs,
                &mut mono_problems,
                loc_expr.value,
                &mut procs,
                home,
                &mut ident_ids,
                pointer_size,
            );
        }

        (unify_problems, can_problems, mono_problems, home, interns)
    }

    fn list_reports<F>(src: &str, buf: &mut String, callback: F)
    where
        F: FnOnce(RocDocBuilder<'_>, &mut String) -> (),
    {
        use ven_pretty::DocAllocator;

        let (type_problems, can_problems, mono_problems, home, interns) = infer_expr_help(src);

        let src_lines: Vec<&str> = src.split('\n').collect();
        let alloc = RocDocAllocator::new(&src_lines, home, &interns);

        let filename = filename_from_string(r"\code\proj\Main.roc");
        let mut reports = Vec::new();

        for problem in can_problems {
            let report = can_problem(&alloc, filename.clone(), problem.clone());
            reports.push(report);
        }

        for problem in type_problems {
            let report = type_problem(&alloc, filename.clone(), problem.clone());
            reports.push(report);
        }

        for problem in mono_problems {
            let report = mono_problem(&alloc, filename.clone(), problem.clone());
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

    fn report_problem_as(src: &str, expected_rendering: &str) {
        let mut buf: String = String::new();

        let callback = |doc: RocDocBuilder<'_>, buf: &mut String| {
            doc.1
                .render_raw(70, &mut roc_reporting::report::CiWrite::new(buf))
                .expect("list_reports")
        };

        list_reports(src, &mut buf, callback);

        assert_eq!(buf, expected_rendering);
    }

    fn color_report_problem_as(src: &str, expected_rendering: &str) {
        let mut buf: String = String::new();

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

        list_reports(src, &mut buf, callback);

        let readable = human_readable(&buf);

        assert_eq!(readable, expected_rendering);
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
                -- SYNTAX PROBLEM --------------------------------------------------------------

                `y` is not used anywhere in your code.

                2 ┆  y = 2
                  ┆  ^

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
                -- SYNTAX PROBLEM --------------------------------------------------------------

                The `i` name is first defined here:

                1 ┆  i = 1
                  ┆  ^

                But then it's defined a second time here:

                3 ┆  s = \i ->
                  ┆       ^

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
                -- SYNTAX PROBLEM --------------------------------------------------------------

                The `Booly` name is first defined here:

                1 ┆  Booly : [ Yes, No ]
                  ┆  ^^^^^^^^^^^^^^^^^^^

                But then it's defined a second time here:

                3 ┆  Booly : [ Yes, No, Maybe ]
                  ┆  ^^^^^^^^^^^^^^^^^^^^^^^^^^

                Since these variables have the same name, it's easy to use the wrong
                one on accident. Give one of them a new name.

                -- SYNTAX PROBLEM --------------------------------------------------------------

                `Booly` is not used anywhere in your code.

                1 ┆  Booly : [ Yes, No ]
                  ┆  ^^^^^^^^^^^^^^^^^^^

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
                -- SYNTAX PROBLEM --------------------------------------------------------------

                Using != and == together requires parentheses, to clarify how they
                should be grouped.

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
                -- SYNTAX PROBLEM --------------------------------------------------------------

                Using more than one == like this requires parentheses, to clarify how
                things should be grouped.

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
    fn report_value_color() {
        let src: &str = indoc!(
            r#"
                activityIndicatorLarge = div

                view activityIndicatorLarge
            "#
        );

        let (_type_problems, _can_problems, _mono_problems, home, interns) = infer_expr_help(src);

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

        let (_type_problems, _can_problems, _mono_problems, home, mut interns) =
            infer_expr_help(src);

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
                -- SYNTAX PROBLEM --------------------------------------------------------------

                I cannot find a `theAdmin` value

                <cyan>3<reset><magenta> ┆<reset>  <white>theAdmin<reset>
                 <magenta> ┆<reset>  <red>^^^^^^^^<reset>

                these names seem close though:

                    Num
                    Set
                    Result
                    Int
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                This `if` condition needs to be a Bool:

                1 ┆  if "foo" then 2 else 3
                  ┆     ^^^^^

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
                -- TYPE MISMATCH ---------------------------------------------------------------

                This `if` guard condition needs to be a Bool:

                1 ┆   when 1 is
                2 ┆>      2 if 1 -> 0x0
                3 ┆       _ -> 0x1

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
                -- TYPE MISMATCH ---------------------------------------------------------------

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
    //  -- TYPE MISMATCH ---------------------------------------------------------------

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
                -- TYPE MISMATCH ---------------------------------------------------------------

                The 2nd branch of this `when` does not match all the previous branches:

                1 ┆  when 1 is
                2 ┆      2 -> "foo"
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
                -- TYPE MISMATCH ---------------------------------------------------------------

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
                -- TYPE MISMATCH ---------------------------------------------------------------

                I cannot update the `.foo` field like this:

                4 ┆  { x & foo: "bar" }
                  ┆             ^^^^^

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
                -- CIRCULAR TYPE ---------------------------------------------------------------

                I'm inferring a weird self-referential type for `g`:

                1 ┆  f = \g -> g g
                  ┆       ^

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
                -- CIRCULAR TYPE ---------------------------------------------------------------

                I'm inferring a weird self-referential type for `f`:

                1 ┆  f = \x -> f [x]
                  ┆  ^

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

                f : { foo : Int } -> Bool
                f = \_ -> True

                f bar
                "#
            ),
            indoc!(
                r#"
                -- TYPE MISMATCH ---------------------------------------------------------------

                The 1st argument to `f` is not what I expect:

                6 ┆  f bar
                  ┆    ^^^

                This `bar` value is a:

                    { bar : Int }

                But `f` needs the 1st argument to be:

                    { foo : Int }

                Hint: Seems like a record field typo. Maybe `bar` should be `foo`?

                Hint: Can more type annotations be added? Type annotations always help
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                The 1st argument to `f` is not what I expect:

                4 ┆  f Blue
                  ┆    ^^^^

                This `Blue` global tag application has the type:

                    [ Blue ]a

                But `f` needs the 1st argument to be:

                    [ Green, Red ]

                Hint: Seems like a tag typo. Maybe `Blue` should be `Red`?

                Hint: Can more type annotations be added? Type annotations always help
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
                f : [ Red Int, Green Bool ] -> Bool
                f = \_ -> True

                f (Blue 3.14)
                "#
            ),
            indoc!(
                r#"
                -- TYPE MISMATCH ---------------------------------------------------------------

                The 1st argument to `f` is not what I expect:

                4 ┆  f (Blue 3.14)
                  ┆     ^^^^^^^^^

                This `Blue` global tag application has the type:

                    [ Blue Float ]a

                But `f` needs the 1st argument to be:

                    [ Green Bool, Red Int ]

                Hint: Seems like a tag typo. Maybe `Blue` should be `Red`?

                Hint: Can more type annotations be added? Type annotations always help
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
                x : Int
                x = if True then 3.14 else 4

                x
                "#
            ),
            indoc!(
                r#"
                -- TYPE MISMATCH ---------------------------------------------------------------

                Something is off with the 1st branch of this `if` expression:

                2 ┆  x = if True then 3.14 else 4
                  ┆                   ^^^^

                The 1st branch is a float of type:

                    Float

                But the type annotation on `x` says it should be:

                    Int

                Hint: Convert between Int and Float with `Num.toFloat` and `Float.round`.
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                Something is off with the 1st branch of this `when` expression:

                4 ┆          _ -> 3.14
                  ┆               ^^^^

                The 1st branch is a float of type:

                    Float

                But the type annotation on `x` says it should be:

                    Int

                Hint: Convert between Int and Float with `Num.toFloat` and `Float.round`.
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                Something is off with the body of the `x` definition:

                1 ┆  x : Int -> Int
                2 ┆  x = \_ -> 3.14
                  ┆      ^^^^^^^^^^

                The body is an anonymous function of type:

                    Int -> Float

                But the type annotation on `x` says it should be:

                    Int -> Int

                Hint: Convert between Int and Float with `Num.toFloat` and `Float.round`.
                "#
            ),
        )
    }

    #[test]
    fn fncall_value() {
        report_problem_as(
            indoc!(
                r#"
                x : Int
                x = 42

                x 3
                "#
            ),
            indoc!(
                r#"
                -- TOO MANY ARGS ---------------------------------------------------------------

                The `x` value is not a function, but it was given 1 argument:

                4 ┆  x 3
                  ┆  ^

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
                f : Int -> Int
                f = \_ -> 42

                f 1 2
                "#
            ),
            indoc!(
                r#"
                -- TOO MANY ARGS ---------------------------------------------------------------

                The `f` function expects 1 argument, but it got 2 instead:

                4 ┆  f 1 2
                  ┆  ^

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
                f : Int, Int -> Int
                f = \_, _ -> 42

                f 1
                "#
            ),
            indoc!(
                r#"
                -- TOO FEW ARGS ----------------------------------------------------------------

                The `f` function expects 2 arguments, but it got only 1:

                4 ┆  f 1
                  ┆  ^

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
                -- TYPE MISMATCH ---------------------------------------------------------------

                The 1st pattern in this `when` is causing a mismatch:

                2 ┆      {} -> 42
                  ┆      ^^

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
                -- TYPE MISMATCH ---------------------------------------------------------------

                The 2nd pattern in this `when` does not match the previous ones:

                3 ┆      {} -> 42
                  ┆      ^^

                The 2nd pattern is trying to match record values of type:

                    {}a

                But all the previous branches match:

                    Num a
                "#
            ),
        )
    }

    #[test]
    fn pattern_guard_mismatch() {
        report_problem_as(
            indoc!(
                r#"
                 when { foo: 1 } is
                     { foo: True } -> 42
                 "#
            ),
            indoc!(
                r#"
                -- TYPE MISMATCH ---------------------------------------------------------------

                The 1st pattern in this `when` is causing a mismatch:

                2 ┆      { foo: True } -> 42
                  ┆      ^^^^^^^^^^^^^

                The first pattern is trying to match record values of type:

                    { foo : [ True ]a }

                But the expression between `when` and `is` has the type:

                    { foo : Num a }
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
                -- SYNTAX PROBLEM --------------------------------------------------------------

                I cannot find a `foo` value

                2 ┆      { foo: 2 } -> foo
                  ┆                    ^^^

                these names seem close though:

                    Bool
                    Int
                    Num
                    Map
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                The 1st pattern in this `when` is causing a mismatch:

                2 ┆      {} | 1 -> 3
                  ┆      ^^^^^^

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
                -- TYPE MISMATCH ---------------------------------------------------------------

                This expression is used in an unexpected way:

                1 ┆  (Foo x) = 42
                  ┆            ^^

                It is a number of type:

                    Num a

                But you are trying to use it as:

                    [ Foo a ]b
                "#
            ),
        )
    }

    #[test]
    fn from_annotation_complex_pattern() {
        report_problem_as(
            indoc!(
                r#"
                { x } : { x : Int }
                { x } = { x: 4.0 }

                x
                "#
            ),
            indoc!(
                r#"
                -- TYPE MISMATCH ---------------------------------------------------------------

                Something is off with the body of this definition:

                1 ┆  { x } : { x : Int }
                2 ┆  { x } = { x: 4.0 }
                  ┆          ^^^^^^^^^^

                The body is a record of type:

                    { x : Float }

                But the type annotation says it should be:

                    { x : Int }

                Hint: Convert between Int and Float with `Num.toFloat` and `Float.round`.
                "#
            ),
        )
    }

    #[test]
    fn missing_fields() {
        report_problem_as(
            indoc!(
                r#"
                x : { a : Int, b : Float, c : Bool }
                x = { b: 4.0 }

                x
                "#
            ),
            indoc!(
                r#"
                -- TYPE MISMATCH ---------------------------------------------------------------

                Something is off with the body of the `x` definition:

                1 ┆  x : { a : Int, b : Float, c : Bool }
                2 ┆  x = { b: 4.0 }
                  ┆      ^^^^^^^^^^

                The body is a record of type:

                    { b : Float }

                But the type annotation on `x` says it should be:

                    { a : Int, b : Float, c : Bool }

                Hint: Looks like the c and a fields are missing.
                "#
            ),
        )
    }

    #[test]
    fn bad_double_rigid() {
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                Something is off with the body of the `f` definition:

                1 ┆  f : a, b -> a
                2 ┆  f = \x, y -> if True then x else y
                  ┆      ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                The body is an anonymous function of type:

                    a, a -> a

                But the type annotation on `f` says it should be:

                    a, b -> a

                Hint: Your type annotation uses `a` and `b` as separate type variables.
                Your code seems to be saying they are the same though. Maybe they
                should be the same your type annotation? Maybe your code uses them in
                a weird way?
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                Something is off with the body of the `f` definition:

                1 ┆  f : Bool -> msg
                2 ┆  f = \_ -> Foo
                  ┆      ^^^^^^^^^

                The body is an anonymous function of type:

                    Bool -> [ Foo ]a

                But the type annotation on `f` says it should be:

                    Bool -> msg

                Hint: The type annotation uses the type variable `msg` to say that this
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                Something is off with the body of the `f` definition:

                1 ┆  f : msg
                2 ┆  f = 0x3
                  ┆      ^^^

                The body is an integer of type:

                    Int

                But the type annotation on `f` says it should be:

                    msg

                Hint: The type annotation uses the type variable `msg` to say that this
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
                f : Bool -> [ Ok Int, InvalidFoo ]
                f = \_ -> ok 4

                f
                "#
            ),
            indoc!(
                r#"
                -- SYNTAX PROBLEM --------------------------------------------------------------

                I cannot find a `ok` value

                2 ┆  f = \_ -> ok 4
                  ┆            ^^

                these names seem close though:

                    f
                    Int
                    Num
                    Map
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
                f : Bool -> Int
                f = \_ ->
                    ok = 3

                    Ok

                f
                "#
            ),
            indoc!(
                r#"
                -- SYNTAX PROBLEM --------------------------------------------------------------

                `ok` is not used anywhere in your code.

                3 ┆      ok = 3
                  ┆      ^^

                If you didn't intend on using `ok` then remove it so future readers of
                your code don't wonder why it is there.

                -- TYPE MISMATCH ---------------------------------------------------------------

                Something is off with the body of the `f` definition:

                1 ┆   f : Bool -> Int
                2 ┆>  f = \_ ->
                3 ┆>      ok = 3
                4 ┆>
                5 ┆>      Ok

                The body is an anonymous function of type:

                    Bool -> [ Ok ]a

                But the type annotation on `f` says it should be:

                    Bool -> Int
                "#
            ),
        )
    }

    #[test]
    fn circular_definition_self() {
        report_problem_as(
            indoc!(
                r#"
                f = f

                f
                "#
            ),
            indoc!(
                r#"
                -- SYNTAX PROBLEM --------------------------------------------------------------

                The `f` value is defined directly in terms of itself, causing an
                infinite loop.
                "#
            ),
        )
    }

    #[test]
    fn circular_definition() {
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
                -- SYNTAX PROBLEM --------------------------------------------------------------

                The `foo` definition is causing a very tricky infinite loop:

                1 ┆  foo = bar
                  ┆  ^^^

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
                -- TYPE MISMATCH ---------------------------------------------------------------

                The `x` record does not have a `.foo` field:

                3 ┆  { x & foo: 3 }
                  ┆        ^^^^^^

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
                -- TYPE MISMATCH ---------------------------------------------------------------

                The `x` record does not have a `.foo` field:

                3 ┆  { x & foo: 3 }
                  ┆        ^^^^^^

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
                f : { fo: Int }ext -> Int
                f = \r ->
                    r2 = { r & foo: r.fo }

                    r2.fo

                f
                "#
            ),
            // TODO also suggest fields with the correct type
            indoc!(
                r#"
                -- TYPE MISMATCH ---------------------------------------------------------------

                Something is off with the body of the `f` definition:

                1 ┆   f : { fo: Int }ext -> Int
                2 ┆>  f = \r ->
                3 ┆>      r2 = { r & foo: r.fo }
                4 ┆>
                5 ┆>      r2.fo

                The body is an anonymous function of type:

                    { fo : Int, foo : Int }a -> Int

                But the type annotation on `f` says it should be:

                    { fo : Int }ext -> Int

                Hint: Seems like a record field typo. Maybe `foo` should be `fo`?

                Hint: Can more type annotations be added? Type annotations always help
                me give more specific messages, and I think they could help a lot in
                this case
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                The `x` record does not have a `.foo` field:

                3 ┆  { x & foo: 3 }
                  ┆        ^^^^^^

                This is usually a typo. Here are the `x` fields that are most similar:

                    { fo : Num c
                    , foobar : Num a
                    , bar : Num e
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                The 2nd argument to `add` is not what I expect:

                1 ┆  0x4 + "foo"
                  ┆        ^^^^^

                This argument is a string of type:

                    Str

                But `add` needs the 2nd argument to be:

                    Num Integer
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                The 2nd argument to `add` is not what I expect:

                1 ┆  0x4 + 3.14
                  ┆        ^^^^

                This argument is a float of type:

                    Float

                But `add` needs the 2nd argument to be:

                    Num Integer

                Hint: Convert between Int and Float with `Num.toFloat` and `Float.round`.
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                Something is off with the body of the `f` definition:

                1 ┆  f : [ A ] -> [ A, B ]
                2 ┆  f = \a -> a
                  ┆      ^^^^^^^

                The body is an anonymous function of type:

                    [ A ] -> [ A ]

                But the type annotation on `f` says it should be:

                    [ A ] -> [ A, B ]

                Hint: Looks like a closed tag union does not have the `B` tag.

                Hint: Closed tag unions can't grow, because that might change the size
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
                -- TYPE MISMATCH ---------------------------------------------------------------

                Something is off with the body of the `f` definition:

                1 ┆  f : [ A ] -> [ A, B, C ]
                2 ┆  f = \a -> a
                  ┆      ^^^^^^^

                The body is an anonymous function of type:

                    [ A ] -> [ A ]

                But the type annotation on `f` says it should be:

                    [ A ] -> [ A, B, C ]

                Hint: Looks like a closed tag union does not have the `C` and `B` tags.

                Hint: Closed tag unions can't grow, because that might change the size
                in memory. Can you use an open tag union?
                "#
            ),
        )
    }

    #[test]
    fn open_tag_union_can_grow() {
        report_problem_as(
            indoc!(
                r#"
                f : [ A ]* -> [ A, B, C ]
                f = \a -> a

                f
                "#
            ),
            // should not give errors
            indoc!(""),
        )
    }

    #[test]
    fn patterns_fn_not_exhaustive() {
        report_problem_as(
            indoc!(
                r#"
                Either : [ Left Int, Right Bool ]

                x : Either
                x = Left 42

                f : Either -> Int
                f = \Left v -> v

                f x
                "#
            ),
            indoc!(
                r#"
                -- UNSAFE PATTERN --------------------------------------------------------------

                This pattern does not cover all the possibilities:

                7 ┆  f = \Left v -> v
                  ┆       ^^^^

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
                x : [ Left Int, Right Bool ]
                x = Left 42


                (Left y) = x

                y
                "#
            ),
            indoc!(
                r#"
                -- UNSAFE PATTERN --------------------------------------------------------------

                This pattern does not cover all the possibilities:

                5 ┆  (Left y) = x
                  ┆   ^^^^

                Other possibilities include:

                    Right _

                I would have to crash if I saw one of those! You can use a binding to
                deconstruct a value if there is only ONE possibility. Use a `when` to
                account for all possibilities.
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
                -- UNSAFE PATTERN --------------------------------------------------------------

                This `when` does not cover all the possibilities:

                1 ┆>  when 0x1 is
                2 ┆>      2 -> 0x3

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
                -- UNSAFE PATTERN --------------------------------------------------------------

                This `when` does not cover all the possibilities:

                4 ┆>  when x is
                5 ┆>      False -> 3

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
                -- UNSAFE PATTERN --------------------------------------------------------------

                This `when` does not cover all the possibilities:

                4 ┆>  when x is
                5 ┆>      Red -> 0
                6 ┆>      Green -> 1

                Other possibilities include:

                    Blue

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
                -- REDUNDANT PATTERN -----------------------------------------------------------

                The 2nd pattern is redundant:

                1 ┆   when 0x1 is
                2 ┆       2 -> 3
                3 ┆>      2 -> 4
                4 ┆       _ -> 5

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
                Foo : { x : Int }

                f : Foo -> Int
                f = \r -> r.x

                f { y: 3.14 }
                "#
            ),
            // de-aliases the alias to give a better error message
            indoc!(
                r#"
                -- TYPE MISMATCH ---------------------------------------------------------------

                The 1st argument to `f` is not what I expect:

                6 ┆  f { y: 3.14 }
                  ┆    ^^^^^^^^^^^

                This argument is a record of type:

                    { y : Float }

                But `f` needs the 1st argument to be:

                    { x : Int }

                Hint: Seems like a record field typo. Maybe `y` should be `x`?

                Hint: Can more type annotations be added? Type annotations always help
                me give more specific messages, and I think they could help a lot in
                this case
                "#
            ),
        )
    }

    #[test]
    fn circular_alias() {
        report_problem_as(
            indoc!(
                r#"
                Foo : { x: Bar }
                Bar : { y : Foo }

                f : Foo

                f
                "#
            ),
            // should not report Bar as unused!
            indoc!(
                r#"
                -- CYCLIC ALIAS ----------------------------------------------------------------

                The `Bar` alias is recursive in an invalid way:

                2 ┆  Bar : { y : Foo }
                  ┆        ^^^^^^^^^^^

                The `Bar` alias depends on itself through the following chain of
                definitions:

                    ┌─────┐
                    │     Bar
                    │     ↓
                    │     Foo
                    └─────┘

                Recursion in aliases is only allowed if recursion happens behind a
                tag.

                -- SYNTAX PROBLEM --------------------------------------------------------------

                `Bar` is not used anywhere in your code.

                2 ┆  Bar : { y : Foo }
                  ┆  ^^^^^^^^^^^^^^^^^

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
                -- CYCLIC ALIAS ----------------------------------------------------------------

                The `Foo` alias is self-recursive in an invalid way:

                1 ┆  Foo : { x : Foo }
                  ┆  ^^^

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
                -- SYNTAX PROBLEM --------------------------------------------------------------

                This record defines the `.x` field twice!

                1 ┆  { x: 4, y: 3, x: 4 }
                  ┆    ^^^^        ^^^^

                In the rest of the program, I will only use the latter definition:

                1 ┆  { x: 4, y: 3, x: 4 }
                  ┆                ^^^^

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
                -- SYNTAX PROBLEM --------------------------------------------------------------

                This record defines the `.x` field twice!

                1 ┆  { x: 4, y: 3, x: "foo" }
                  ┆    ^^^^        ^^^^^^^^

                In the rest of the program, I will only use the latter definition:

                1 ┆  { x: 4, y: 3, x: "foo" }
                  ┆                ^^^^^^^^

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
                -- SYNTAX PROBLEM --------------------------------------------------------------

                This record defines the `.x` field twice!

                1 ┆   {
                2 ┆>      x: 4,
                3 ┆       y: 3,
                4 ┆>      x: "foo"
                5 ┆   }

                In the rest of the program, I will only use the latter definition:

                1 ┆   {
                2 ┆       x: 4,
                3 ┆       y: 3,
                4 ┆>      x: "foo"
                5 ┆   }

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
                -- SYNTAX PROBLEM --------------------------------------------------------------

                This record defines the `.x` field twice!

                2 ┆       { r &
                3 ┆>          x: 4,
                4 ┆           y: 3,
                5 ┆>          x: "foo"
                6 ┆       }

                In the rest of the program, I will only use the latter definition:

                2 ┆       { r &
                3 ┆           x: 4,
                4 ┆           y: 3,
                5 ┆>          x: "foo"
                6 ┆       }

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
                a : { foo : Int, bar : Float, foo : Str }
                a = { bar: 3.0, foo: "foo" }

                a
                "#
            ),
            indoc!(
                r#"
                -- SYNTAX PROBLEM --------------------------------------------------------------

                This record type defines the `.foo` field twice!

                1 ┆  a : { foo : Int, bar : Float, foo : Str }
                  ┆        ^^^^^^^^^               ^^^^^^^^^

                In the rest of the program, I will only use the latter definition:

                1 ┆  a : { foo : Int, bar : Float, foo : Str }
                  ┆                                ^^^^^^^^^

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
                a : [ Foo Int, Bar Float, Foo Str ]
                a = Foo "foo"

                a
                "#
            ),
            indoc!(
                r#"
                -- SYNTAX PROBLEM --------------------------------------------------------------

                This tag union type defines the `Foo` tag twice!

                1 ┆  a : [ Foo Int, Bar Float, Foo Str ]
                  ┆        ^^^^^^^             ^^^^^^^

                In the rest of the program, I will only use the latter definition:

                1 ┆  a : [ Foo Int, Bar Float, Foo Str ]
                  ┆                            ^^^^^^^

                For clarity, remove the previous `Foo` definitions from this tag union
                type.
                "#
            ),
        )
    }

    #[test]
    fn invalid_num() {
        report_problem_as(
            indoc!(
                r#"
                a : Num Int Float
                a = 3

                a
                "#
            ),
            indoc!(
                r#"
                -- TOO MANY TYPE ARGUMENTS -----------------------------------------------------

                The `Num` alias expects 1 type argument, but it got 2 instead:

                1 ┆  a : Num Int Float
                  ┆      ^^^^^^^^^^^^^

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
                f : Bool -> Num Int Float
                f = \_ -> 3

                f
                "#
            ),
            indoc!(
                r#"
                -- TOO MANY TYPE ARGUMENTS -----------------------------------------------------

                The `Num` alias expects 1 type argument, but it got 2 instead:

                1 ┆  f : Bool -> Num Int Float
                  ┆              ^^^^^^^^^^^^^

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

                x : Pair Int
                x = 3

                x
                "#
            ),
            indoc!(
                r#"
                -- TOO FEW TYPE ARGUMENTS ------------------------------------------------------

                The `Pair` alias expects 2 type arguments, but it got 1 instead:

                3 ┆  x : Pair Int
                  ┆      ^^^^^^^^

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

                x : Pair Int Int Int
                x = 3

                x
                "#
            ),
            indoc!(
                r#"
                -- TOO MANY TYPE ARGUMENTS -----------------------------------------------------

                The `Pair` alias expects 2 type arguments, but it got 3 instead:

                3 ┆  x : Pair Int Int Int
                  ┆      ^^^^^^^^^^^^^^^^

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

                f : Foo Int

                f
                "#
            ),
            indoc!(
                r#"
                -- SYNTAX PROBLEM --------------------------------------------------------------

                The `a` type variable is not used in the `Foo` alias definition:

                1 ┆  Foo a : [ Foo ]
                  ┆      ^

                Roc does not allow phantom type parameters!
                "#
            ),
        )
    }
}
