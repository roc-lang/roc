#[macro_use]
extern crate indoc;

#[cfg(test)]
mod suffixed_tests {
    use bumpalo::Bump;
    use insta::assert_snapshot;
    use roc_can::desugar::desugar_defs_node_values;
    use roc_can::env::{Env, FxMode};
    use roc_can::scope::Scope;
    use roc_module::symbol::{IdentIds, ModuleIds, PackageModuleIds};
    use roc_parse::test_helpers::parse_defs_with;
    use std::path::Path;

    macro_rules! run_test {
        ($src:expr) => {{
            let arena = &Bump::new();
            let home = ModuleIds::default().get_or_insert(&"Test".into());

            let mut scope = Scope::new(
                home,
                "TestPath".into(),
                IdentIds::default(),
                Default::default(),
            );

            let dep_idents = IdentIds::exposed_builtins(0);
            let qualified_module_ids = PackageModuleIds::default();
            let mut env = Env::new(
                arena,
                $src,
                home,
                Path::new("test.roc"),
                &dep_idents,
                &qualified_module_ids,
                None,
                FxMode::Task,
            );

            let mut defs = parse_defs_with(arena, indoc!($src)).unwrap();
            desugar_defs_node_values(&mut env, &mut scope, &mut defs, true);

            let snapshot = format!("{:#?}", &defs);
            println!("{}", snapshot);
            assert_snapshot!(snapshot);
        }};
    }

    /**
     * This example tests a suffixed statement, followed
     * by a Body with an empty record pattern.
     *
     * The def final expression is explicitly provided.
     */
    #[test]
    fn multi_defs_stmts() {
        run_test!(
            r#"
            main =
                line! "Ahoy"
                {} = "There" |> Stdout.line!

                Task.ok {}
            "#
        );
    }

    /**
     * The most simple suffixed example. A single statement
     * without arguments and a final expression.
     */
    #[test]

    fn basic() {
        run_test!(
            r#"
        main =
            foo!

            ok {}
        "#
        );
    }

    /**
     * A single suffixed statement with arguments applied.
     * Note there is no final expression.
     */
    #[test]
    fn last_suffixed_single() {
        run_test!(
            r#"
            main = foo! "bar" {} "baz"
            "#
        );
    }

    /**
     * Multiple suffixed statements with no
     * arguments, and no final expression.
     */
    #[test]
    fn last_suffixed_multiple() {
        run_test!(
            r#"
            main =
                foo!
                bar!
                baz!
            "#
        );
    }

    /**
     * A definition with a closure that contains a Defs node, which also
     * contains a suffixed binops statement.
     */
    #[test]
    fn closure_simple() {
        run_test!(
            r#"
            main =
                x = \msg ->
                    msg |> line!
                    ok {}

                x "hi"
            "#
        );
    }

    /**
     * Example of unwrapping a pipline statement
     *
     * Note pipelines are desugared into Apply functions,
     * however this also tests the parser.
     *
     */
    #[test]
    fn simple_pizza() {
        run_test!(
            r#"
            main =
                "hello"
                |> Str.concat "world"
                |> line!

                Task.ok {}
            "#
        );
    }

    /**
     * Example of unwrapping a Result with ?? operator
     *
     * Note that ?? is desugared into a when expression,
     * however this also tests the parser.
     *
     */
    #[test]
    fn simple_double_question() {
        run_test!(
            r#"
            main =
                "123"
                |> Str.toU8 ?? 255
                |> Num.toStr
                |> line!

                Task.ok {}
            "#
        );
    }

    /**
     * Example with a parens suffixed sub-expression
     * in the function part of an Apply.
     *
     * Note how the parens unwraps into an intermediate answer #!0_arg instead of
     * unwrapping the def `do`.
     *
     */
    #[test]
    fn body_parens_apply() {
        run_test!(
            r#"
            main =
                do = (sayMultiple!) "hi"
                do
            "#
        );
    }

    /**
     * Example of unwrapping mixed Body defs with
     * Var's of both single and multiple suffixes
     */
    #[test]
    fn var_suffixes() {
        run_test!(
            r#"
            main =
                a = foo!
                b = bar!!
                baz a b
            "#
        );
    }

    /**
     * Example with a multiple suffixed Var
     *
     * Note it unwraps into an intermediate answer `#!0_arg`
     *
     */
    #[test]
    fn multiple_suffix() {
        run_test!(
            r#"
            main =
                foo!!
                bar
            "#
        );
    }

    /**
     * A suffixed expression in the function part of the Apply
     */
    #[test]
    fn apply_function_suffixed() {
        run_test!(
            r#"
            main =
                x = (foo! "bar") "hello"
                baz x
            "#
        );
    }

    /**
     * A suffixed expression in an Apply argument position.
     */
    #[test]
    fn apply_argument_suffixed() {
        run_test!(
            r#"
            main =
                x = bar (foo! "hello")
                baz x
            "#
        );
    }

    /**
     * Example where the suffixed def is not the first def
     */
    #[test]
    fn multiple_def_first_suffixed() {
        run_test!(
            r#"
            main =
                msg = "hello"
                x = foo! msg
                bar x
            "#
        );
    }

    /**
     * Annotated defs and a suffixed expression
     * with annotations inside a closure
     */

    #[test]
    fn closure_with_annotations() {
        run_test!(
            r#"
            main =
                x : Str -> Task _ _
                x = \msg ->

                    y : Task {} _
                    y = line! msg
                    y

                x "foo"
            "#
        );
    }

    /**
     * Nested suffixed expressions
     */
    #[test]
    fn nested_simple() {
        run_test!(
            r#"
            run = line! (nextMsg!)
            "#
        );
    }

    /**
     * Nested suffixed expressions
     */
    #[test]
    fn nested_complex() {
        run_test!(
            r#"
            main =
                z = foo! (bar! baz) (blah stuff)
                doSomething z
            "#
        );
    }

    /**
     * A closure that contains a Defs node
     */
    #[test]
    fn closure_with_defs() {
        run_test!(
            r#"
            main =

                foo : Str, {}, Str -> Task {} I32
                foo = \a, _, b ->
                    line! a
                    line! b

                    Task.ok {}

                foo "bar" {} "baz"
            "#
        );
    }

    /**
     * Test when the suffixed def being unwrapped is not the first or last
     */
    #[test]
    fn defs_suffixed_middle() {
        run_test!(
            r#"
            main =
                a = "Foo"
                Stdout.line! a

                printBar!

            printBar =
                b = "Bar"
                Stdout.line b
            "#
        );
    }

    /**
     * A simple if-then-else statement which is split
     */
    #[test]
    fn if_simple() {
        run_test!(
            r#"
            main =
                isTrue = Task.ok Bool.true
                isFalse = Task.ok Bool.false

                if isFalse! then
                    line "fail"
                else if isTrue! then
                    line "success"
                else
                    line "fail"
            "#
        );
    }

    /**
     * A more complex example including the use of nested Defs nodes
     */
    #[test]
    fn if_complex() {
        run_test!(
            r#"
            main =
                isTrue = Task.ok Bool.true
                isFalsey = \x -> Task.ok x
                msg : Task {} I32
                msg =
                    if !(isTrue!) then
                        line! "fail"
                        err 1
                    else if (isFalsey! Bool.false) then
                        line! "nope"
                        ok {}
                    else
                        line! "success"

                msg
            "#
        );
    }

    /**
     * Unwrap a trailing binops
     */
    #[test]
    fn trailing_binops() {
        run_test!(
            r#"
            copy = \a,b ->
                line! "FOO"

                CMD.new "cp"
                |> mapErr! ERR
            "#
        );
    }

    /**
     * Unwrap a when expression
     */
    #[test]
    fn when_simple() {
        run_test!(
            r#"
            list =
                when getList! is
                    [] -> "empty"
                    _ -> "non-empty"
            "#
        );
    }

    /**
     * Unwrap a when expression
     */
    #[test]
    fn when_branches() {
        run_test!(
            r#"
            list =
                when getList! is
                    [] ->
                        line! "foo"
                        line! "bar"
                    _ ->
                        ok {}
            "#
        );
    }

    #[test]
    fn trailing_suffix_inside_when() {
        run_test!(
            r#"
            main =
                result = Stdin.line!

                when result is
                    End ->
                        Task.ok {}

                    Input name ->
                        Stdout.line! "Hello, $(name)"
            "#
        );
    }

    #[test]
    fn dbg_simple() {
        run_test!(
            r#"
            main =
                foo = getFoo!
                dbg foo
                bar! foo
            "#
        );
    }

    #[test]
    fn dbg_expr() {
        run_test!(
            r#"
            main =
                dbg (dbg (1 + 1))
            "#
        );
    }

    #[test]
    fn pizza_dbg() {
        run_test!(
            r#"
            main =
                1
                |> dbg
                |> Num.add 2
                |> dbg
            "#
        )
    }

    #[test]
    fn apply_argument_single() {
        run_test!(
            r#"
            main =
                c = b a!
                c
            "#
        );
    }

    #[test]
    fn apply_argument_multiple() {
        run_test!(
            r#"
            main =
                c = b a! x!
                c
            "#
        );
    }

    #[test]
    fn bang_in_pipe_root() {
        run_test!(
            r#"
            main =
                c = a! |> b
                c
            "#
        );
    }

    #[test]
    fn expect_then_bang() {
        run_test!(
            r#"
            main =
                expect 1 == 2
                x!
            "#
        );
    }

    #[test]
    fn deep_when() {
        run_test!(
            r#"
            main =
                when a is
                    0 ->
                        when b is
                            1 ->
                                c!
            "#
        );
    }

    #[test]
    fn deps_final_expr() {
        run_test!(
            r#"
            main =
                when x is
                    A ->
                        y = 42

                        if a then
                            b!
                        else
                            c!
                    B ->
                        d!
            "#
        );
    }

    #[test]
    fn dbg_stmt_arg() {
        run_test!(
            r#"
            main =
                dbg a!

                b
            "#
        )
    }

    #[test]
    fn last_stmt_not_top_level_suffixed() {
        run_test!(
            r#"
            main =
                x = 42
                a b!
            "#
        );
    }

    #[test]
    fn nested_defs() {
        run_test!(
            r##"
            main =
                x =
                    a = b!
                    c! a

                x
            "##
        );
    }

    #[test]
    fn type_annotation() {
        run_test!(
            r##"
            f = \x ->
                r : A
                r = x!
                Task.ok r
            "##
        );
    }

    #[test]
    fn issue_7081() {
        run_test!(
            r##"
            inc = \i ->
                if i > 2 then
                    Err MaxReached
                else
                    Ok (i + 1)

            expect
                run = \i ->
                    newi =
                        i
                        |> inc?
                        |> inc?
                    Ok newi
                result = run 0
                result == Ok 2

            main =
                Stdout.line! "Hello world"
            "##
        );
    }

    #[test]
    fn issue_7103() {
        run_test!(
            r##"
            run : Task {} _
            run = line! "foo"

            main = run
            "##
        );
    }
}

#[cfg(test)]
mod test_suffixed_helpers {

    use roc_can::suffixed::is_matching_intermediate_answer;
    use roc_parse::ast::Expr;
    use roc_parse::ast::Pattern;
    use roc_region::all::Loc;

    #[test]
    fn test_matching_answer() {
        let loc_pat = Loc::at_zero(Pattern::Identifier { ident: "#!0_arg" });
        let loc_new = Loc::at_zero(Expr::Var {
            module_name: "",
            ident: "#!0_arg",
        });

        std::assert!(is_matching_intermediate_answer(&loc_pat, &loc_new));
    }
}
