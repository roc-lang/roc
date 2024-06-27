#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc_can;
extern crate roc_parse;
extern crate roc_region;

mod helpers;

#[cfg(test)]
mod test_can {
    use crate::helpers::{can_expr_with, test_home, CanExprOut};
    use bumpalo::Bump;
    use core::panic;
    use roc_can::expr::Expr::{self, *};
    use roc_can::expr::{ClosureData, IntValue, Recursive};
    use roc_module::symbol::Symbol;
    use roc_problem::can::{CycleEntry, FloatErrorKind, IntErrorKind, Problem, RuntimeError};
    use roc_region::all::{Position, Region};
    use std::{f64, i64};

    fn assert_can_runtime_error(input: &str, expected: RuntimeError) {
        let arena = Bump::new();
        let actual_out = can_expr_with(&arena, test_home(), input);

        match actual_out.loc_expr.value {
            Expr::RuntimeError(actual) => {
                assert_eq!(expected, actual);
            }
            actual => {
                panic!("Expected a Float, but got: {:?}", actual);
            }
        }
    }

    fn assert_can_string(input: &str, expected: &str) {
        let arena = Bump::new();
        let actual_out = can_expr_with(&arena, test_home(), input);

        match actual_out.loc_expr.value {
            Expr::Str(actual) => {
                assert_eq!(expected, &*actual);
            }
            actual => {
                panic!("Expected a Float, but got: {:?}", actual);
            }
        }
    }

    fn assert_can_float(input: &str, expected: f64) {
        let arena = Bump::new();
        let actual_out = can_expr_with(&arena, test_home(), input);

        match actual_out.loc_expr.value {
            Expr::Float(_, _, _, actual, _) => {
                assert_eq!(expected, actual);
            }
            actual => {
                panic!("Expected a Float, but got: {:?}", actual);
            }
        }
    }

    fn assert_can_int(input: &str, expected: i128) {
        let arena = Bump::new();
        let actual_out = can_expr_with(&arena, test_home(), input);

        match actual_out.loc_expr.value {
            Expr::Int(_, _, _, actual, _) => {
                assert_eq!(IntValue::I128(expected.to_ne_bytes()), actual);
            }
            actual => {
                panic!("Expected an Num.Int *, but got: {:?}", actual);
            }
        }
    }

    fn assert_can_num(input: &str, expected: i128) {
        let arena = Bump::new();
        let actual_out = can_expr_with(&arena, test_home(), input);

        match actual_out.loc_expr.value {
            Expr::Num(_, _, actual, _) => {
                assert_eq!(IntValue::I128(expected.to_ne_bytes()), actual);
            }
            actual => {
                panic!("Expected a Num, but got: {:?}", actual);
            }
        }
    }

    // NUMBER LITERALS

    #[test]
    fn int_too_large() {
        use roc_parse::ast::Base;

        let string = "340_282_366_920_938_463_463_374_607_431_768_211_456".to_string();

        assert_can_runtime_error(
            &string.clone(),
            RuntimeError::InvalidInt(
                IntErrorKind::Overflow,
                Base::Decimal,
                Region::zero(),
                string.into_boxed_str(),
            ),
        );
    }

    #[test]
    fn int_too_small() {
        use roc_parse::ast::Base;

        let string = "-170_141_183_460_469_231_731_687_303_715_884_105_729".to_string();

        assert_can_runtime_error(
            &string.clone(),
            RuntimeError::InvalidInt(
                IntErrorKind::Underflow,
                Base::Decimal,
                Region::zero(),
                string.into(),
            ),
        );
    }

    #[test]
    fn float_too_large() {
        let string = format!("{}1.0", f64::MAX);
        let region = Region::zero();

        assert_can_runtime_error(
            &string.clone(),
            RuntimeError::InvalidFloat(FloatErrorKind::PositiveInfinity, region, string.into()),
        );
    }

    #[test]
    fn float_too_small() {
        let string = format!("{}1.0", f64::MIN);
        let region = Region::zero();

        assert_can_runtime_error(
            &string.clone(),
            RuntimeError::InvalidFloat(FloatErrorKind::NegativeInfinity, region, string.into()),
        );
    }

    #[test]
    fn float_double_dot() {
        let string = "1.1.1";
        let region = Region::zero();

        assert_can_runtime_error(
            string,
            RuntimeError::InvalidFloat(FloatErrorKind::Error, region, string.into()),
        );
    }

    #[test]
    fn zero() {
        assert_can_num("0", 0);
    }

    #[test]
    fn minus_zero() {
        assert_can_num("-0", 0);
    }

    #[test]
    fn zero_point_zero() {
        assert_can_float("0.0", 0.0);
    }

    #[test]
    fn minus_zero_point_zero() {
        assert_can_float("-0.0", -0.0);
    }

    #[test]
    fn scientific_positive() {
        assert_can_float("5e4", 50000.0);
    }

    #[test]
    fn scientific_negative() {
        assert_can_float("5e-4", 0.0005);
    }

    #[test]
    fn num_max() {
        assert_can_num(&(i64::MAX.to_string()), i64::MAX.into());
    }

    #[test]
    fn num_min() {
        assert_can_num(&(i64::MIN.to_string()), i64::MIN.into());
    }

    #[test]
    fn hex_max() {
        assert_can_int(&format!("0x{:x}", i64::MAX), i64::MAX.into());
    }

    #[test]
    fn hex_min() {
        assert_can_int(&format!("-0x{:x}", i64::MAX as i128 + 1), i64::MIN.into());
    }

    #[test]
    fn oct_max() {
        assert_can_int(&format!("0o{:o}", i64::MAX), i64::MAX.into());
    }

    #[test]
    fn oct_min() {
        assert_can_int(&format!("-0o{:o}", i64::MAX as i128 + 1), i64::MIN.into());
    }

    #[test]
    fn bin_max() {
        assert_can_int(&format!("0b{:b}", i64::MAX), i64::MAX.into());
    }

    #[test]
    fn bin_min() {
        assert_can_int(&format!("-0b{:b}", i64::MAX as i128 + 1), i64::MIN.into());
    }

    #[test]
    fn hex_zero() {
        assert_can_int("0x0", 0x0);
    }

    #[test]
    fn hex_one_b() {
        assert_can_int("0x1b", 0x1b);
    }

    #[test]
    fn minus_hex_one_b() {
        assert_can_int("-0x1b", -0x1b);
    }

    #[test]
    fn octal_zero() {
        assert_can_int("0o0", 0o0);
    }

    #[test]
    fn octal_one_two() {
        assert_can_int("0o12", 0o12);
    }

    #[test]
    fn minus_octal_one_two() {
        assert_can_int("-0o12", -0o12);
    }

    #[test]
    fn binary_zero() {
        assert_can_int("0b0", 0b0);
    }

    #[test]
    fn binary_one_one() {
        assert_can_int("0b11", 0b11);
    }

    #[test]
    fn minus_binary_one_one() {
        assert_can_int("-0b11", -0b11);
    }

    // ANNOTATIONS
    #[test]
    fn correct_annotated_body() {
        let src = indoc!(
            r"
                f : Num.Int * -> Num.Int *
                f = \ a -> a

                f
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
    }

    #[test]
    fn correct_annotated_body_with_comments() {
        let src = indoc!(
            r"
                f : Num.Int * -> Num.Int * # comment
                f = \ a -> a

                f
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
    }

    #[test]
    fn name_mismatch_annotated_body() {
        let src = indoc!(
            r"
                f : Num.Int * -> Num.Int *
                g = \ a -> a

                g
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        // Here we have 2 issues:
        // 1. `g` doesn't match the previous annotation named `f`, so we
        //     have a `SignatureDefMismatch`.
        // 2. Thus, `g` is not defined then final reference to it is a
        //    `LookupNotInScope`.
        assert_eq!(problems.len(), 2);
        assert!(problems.iter().all(|problem| {
            matches!(
                problem,
                Problem::SignatureDefMismatch { .. }
                    | Problem::RuntimeError(RuntimeError::LookupNotInScope { .. })
            )
        }));
    }

    #[test]
    fn name_mismatch_annotated_body_with_comment() {
        let src = indoc!(
            r"
                f : Num.Int * -> Num.Int * # comment
                g = \ a -> a

                g
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        // Here we have 2 issues:
        // 1. `g` doesn't match the previous annotation named `f`, so we
        //     have a `SignatureDefMismatch`.
        // 2. Thus, `g` is not defined then final reference to it is a
        //    `LookupNotInScope`.
        assert_eq!(problems.len(), 2);
        assert!(problems.iter().all(|problem| {
            matches!(
                problem,
                Problem::SignatureDefMismatch { .. }
                    | Problem::RuntimeError(RuntimeError::LookupNotInScope { .. })
            )
        }));
    }

    #[test]
    fn separated_annotated_body() {
        let src = indoc!(
            r"
                f : Num.Int * -> Num.Int *

                f = \ a -> a

                f 42
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems.len(), 0);
    }

    #[test]
    fn separated_annotated_body_with_comment() {
        let src = indoc!(
            r"
                f : Num.Int * -> Num.Int *
                # comment
                f = \ a -> a

                f 42
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems.len(), 0);
    }

    #[test]
    fn shadowed_annotation() {
        let src = indoc!(
            r"
                f : Num.Int * -> Num.Int *

                f : Num.Int * -> Num.Int *

                f
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems.len(), 2);
        println!("{problems:#?}");
        assert!(problems.iter().any(|problem| matches!(
            problem,
            Problem::RuntimeError(RuntimeError::Shadowing { .. })
        )));
    }

    #[test]
    fn correct_nested_unannotated_body() {
        let src = indoc!(
            r"
                f : Num.Int *
                f =
                    g = 42

                    g + 1

                f
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
    }

    #[test]
    fn correct_nested_annotated_body() {
        let src = indoc!(
            r"
                f : Num.Int *
                f =
                    g : Num.Int *
                    g = 42

                    g + 1

                f
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
    }

    #[test]
    fn correct_nested_body_annotated_multiple_lines() {
        let src = indoc!(
            r"
                f : Num.Int *
                f =
                    g : Num.Int *
                    g = 42
                    h : Num.Int *
                    h = 5
                    z = 4
                    g + h + z

                f
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
    }

    #[test]
    fn correct_nested_body_unannotated_multiple_lines() {
        let src = indoc!(
            r"
                f : Num.Int *
                f =
                    g = 42
                    h : Num.Int *
                    h = 5
                    z = 4
                    g + h + z

                f
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
    }
    #[test]
    fn correct_double_nested_body() {
        let src = indoc!(
            r"
                f : Num.Int *
                f =
                    g =
                        h = 42
                        h + 1
                    g + 1


                f
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
    }

    #[test]
    fn annotation_followed_with_unrelated_affectation() {
        let src = indoc!(
            r"
                F : Str

                x = 1

                x
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems.len(), 1);
        assert!(problems
            .iter()
            .all(|problem| matches!(problem, Problem::UnusedDef(_, _))));
    }

    #[test]
    fn two_annotations_followed_with_unrelated_affectation() {
        let src = indoc!(
            r"
                G : Str

                F : {}

                x = 1

                x
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems.len(), 2);
        assert!(problems
            .iter()
            .all(|problem| matches!(problem, Problem::UnusedDef(_, _))));
    }
    // LOCALS

    // TODO rewrite this test to check only for UnusedDef reports
    // #[test]
    // fn closure_args_are_not_locals() {
    //     // "arg" shouldn't make it into output.locals, because
    //     // it only exists in the closure's arguments.
    //     let arena = Bump::new();
    //     let src = indoc!(
    //         r"
    //         func = \arg -> arg

    //         func 2
    //     "
    //     );
    //     let (_actual, output, problems, _var_store, _vars, _constraint) =
    //         can_expr_with(&arena, test_home(), src);

    //     assert_eq!(problems, vec![]);

    //     assert_eq!(
    //         output,
    //         Out {
    //             lookups: vec!["func"],
    //             calls: vec!["func"],
    //             tail_call: None
    //         }
    //         .into_output(scope)
    //     );
    // }

    // TODO rewrite this test to check only for UnusedDef reports
    // #[test]
    // fn call_by_pointer_for_fn_args() {
    //     // This function will get passed in as a pointer.
    //     let src = indoc!(
    //         r"
    //         apply = \f, x -> f x

    //         identity = \a -> a

    //         apply identity 5
    //     "
    //     );
    //     let arena = Bump::new();
    //     let (_actual, output, problems, _var_store, _vars, _constraint) =
    //         can_expr_with(&arena, test_home(), src);

    //     assert_eq!(problems, vec![]);

    //     assert_eq!(
    //         output,
    //         Out {
    //             lookups: vec!["identity", "apply"],
    //             calls: vec!["f", "apply"],
    //             tail_call: None
    //         }
    //         .into()
    //     );
    // }

    // OPTIONAL RECORDS
    #[test]
    fn incorrect_optional_value() {
        let src = indoc!(
            r"
                { x ? 42 }
            "
        );
        let arena = Bump::new();
        let CanExprOut {
            problems, loc_expr, ..
        } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems.len(), 1);
        assert!(problems
            .iter()
            .all(|problem| matches!(problem, Problem::InvalidOptionalValue { .. })));

        assert!(matches!(
            loc_expr.value,
            Expr::RuntimeError(roc_problem::can::RuntimeError::InvalidOptionalValue { .. })
        ));
    }

    // RECORD BUILDERS
    #[test]
    fn record_builder_desugar() {
        let src = indoc!(
            r#"
                succeed = \_ -> crash "succeed"
                apply = \_ -> crash "get"

                d = 3

                succeed {
                    a: 1,
                    b: <- apply "b",
                    c: <- apply "c",
                    d
                }
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems.len(), 0);

        // Assert that we desugar to:
        //
        // (apply "c") ((apply "b") (succeed \b -> \c -> { a: 1, b, c, d }))

        // (apply "c") ..
        let (apply_c, c_to_b) = simplify_curried_call(&out.loc_expr.value);
        assert_apply_call(apply_c, "c", &out.interns);

        // (apply "b") ..
        let (apply_b, b_to_succeed) = simplify_curried_call(c_to_b);
        assert_apply_call(apply_b, "b", &out.interns);

        // (succeed ..)
        let (succeed, b_closure) = simplify_curried_call(b_to_succeed);

        match succeed {
            Var(sym, _) => assert_eq!(sym.as_str(&out.interns), "succeed"),
            _ => panic!("Not calling succeed: {:?}", succeed),
        }

        // \b -> ..
        let (b_sym, c_closure) = simplify_builder_closure(b_closure);

        // \c -> ..
        let (c_sym, c_body) = simplify_builder_closure(c_closure);

        // { a: 1, b, c, d }
        match c_body {
            Record { fields, .. } => {
                match get_field_expr(fields, "a") {
                    Num(_, num_str, _, _) => {
                        assert_eq!(num_str.to_string(), "1");
                    }
                    expr => panic!("a is not a Num: {:?}", expr),
                }

                assert_eq!(get_field_var_sym(fields, "b"), b_sym);
                assert_eq!(get_field_var_sym(fields, "c"), c_sym);
                assert_eq!(get_field_var_sym(fields, "d").as_str(&out.interns), "d");
            }
            _ => panic!("Closure body wasn't a Record: {:?}", c_body),
        }
    }

    fn simplify_curried_call(expr: &Expr) -> (&Expr, &Expr) {
        match expr {
            LetNonRec(_, loc_expr) => simplify_curried_call(&loc_expr.value),
            Call(fun, args, _) => (&fun.1.value, &args[0].1.value),
            _ => panic!("Final Expr is not a Call: {:?}", expr),
        }
    }

    fn assert_apply_call(expr: &Expr, expected: &str, interns: &roc_module::symbol::Interns) {
        match simplify_curried_call(expr) {
            (Var(sym, _), Str(val)) => {
                assert_eq!(sym.as_str(interns), "apply");
                assert_eq!(val.to_string(), expected);
            }
            call => panic!("Not a valid (get {}) call: {:?}", expected, call),
        };
    }

    fn simplify_builder_closure(expr: &Expr) -> (Symbol, &Expr) {
        use roc_can::pattern::Pattern::*;

        match expr {
            Closure(closure) => match &closure.arguments[0].2.value {
                Identifier(sym) => (*sym, &closure.loc_body.value),
                pattern => panic!("Not an identifier pattern: {:?}", pattern),
            },
            _ => panic!("Not a closure: {:?}", expr),
        }
    }

    fn get_field_expr<'a>(
        fields: &'a roc_collections::SendMap<roc_module::ident::Lowercase, roc_can::expr::Field>,
        name: &'a str,
    ) -> &'a Expr {
        let ident = roc_module::ident::Lowercase::from(name);

        &fields.get(&ident).unwrap().loc_expr.value
    }

    fn get_field_var_sym(
        fields: &roc_collections::SendMap<roc_module::ident::Lowercase, roc_can::expr::Field>,
        name: &str,
    ) -> roc_module::symbol::Symbol {
        match get_field_expr(fields, name) {
            Var(sym, _) => *sym,
            expr => panic!("Not a var: {:?}", expr),
        }
    }

    #[test]
    fn record_builder_field_names_do_not_shadow() {
        let src = indoc!(
            r#"
            succeed = \_ -> crash "succeed"
            parse = \_ -> crash "parse"

            number = "42"

            succeed {
                number: <- parse number,
                raw: number,
            }
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems.len(), 0);

        let (_, number_to_succeed) = simplify_curried_call(&out.loc_expr.value);
        let (_, number_closure) = simplify_curried_call(number_to_succeed);
        let (apply_number_sym, record) = simplify_builder_closure(number_closure);

        match record {
            Record { fields, .. } => {
                assert_eq!(get_field_var_sym(fields, "number"), apply_number_sym);

                match get_field_expr(fields, "raw") {
                    Var(number_sym, _) => {
                        assert_ne!(number_sym.ident_id(), apply_number_sym.ident_id());
                        assert_eq!(number_sym.as_str(&out.interns), "number")
                    }
                    expr => panic!("a is not a Num: {:?}", expr),
                }
            }
            _ => panic!("Closure body wasn't a Record: {:?}", record),
        }
    }

    #[test]
    fn multiple_record_builders_error() {
        let src = indoc!(
            r#"
                succeed
                    { a: <- apply "a" }
                    { b: <- apply "b" }
            "#
        );
        let arena = Bump::new();
        let CanExprOut {
            problems, loc_expr, ..
        } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems.len(), 1);
        assert!(problems.iter().all(|problem| matches!(
            problem,
            Problem::RuntimeError(roc_problem::can::RuntimeError::MultipleRecordBuilders { .. })
        )));

        assert!(matches!(
            loc_expr.value,
            Expr::RuntimeError(roc_problem::can::RuntimeError::MultipleRecordBuilders { .. })
        ));
    }

    #[test]
    fn hanging_record_builder() {
        let src = indoc!(
            r#"
                { a: <- apply "a" }
            "#
        );
        let arena = Bump::new();
        let CanExprOut {
            problems, loc_expr, ..
        } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems.len(), 1);
        assert!(problems.iter().all(|problem| matches!(
            problem,
            Problem::RuntimeError(roc_problem::can::RuntimeError::UnappliedRecordBuilder { .. })
        )));

        assert!(matches!(
            loc_expr.value,
            Expr::RuntimeError(roc_problem::can::RuntimeError::UnappliedRecordBuilder { .. })
        ));
    }

    // TAIL CALLS
    fn get_closure(expr: &Expr, i: usize) -> roc_can::expr::Recursive {
        match expr {
            LetRec(assignments, body, _) => {
                match &assignments.get(i).map(|def| &def.loc_expr.value) {
                    Some(Closure(ClosureData {
                        recursive: recursion,
                        ..
                    })) => *recursion,
                    Some(other) => {
                        panic!("assignment at {} is not a closure, but a {:?}", i, other)
                    }
                    None => {
                        if i > 0 {
                            get_closure(&body.value, i - 1)
                        } else {
                            panic!("Looking for assignment at {} but the list is too short", i)
                        }
                    }
                }
            }
            LetNonRec(def, body) => {
                if i > 0 {
                    // recurse in the body (not the def!)
                    get_closure(&body.value, i - 1)
                } else {
                    match &def.loc_expr.value {
                        Closure(ClosureData {
                            recursive: recursion,
                            ..
                        }) => *recursion,
                        other => {
                            panic!("assignment at {} is not a closure, but a {:?}", i, other)
                        }
                    }
                }
            }
            // Closure(_, recursion, _, _) if i == 0 => recursion.clone(),
            _ => panic!(
                "expression is not a LetRec or a LetNonRec, but rather {:?}",
                expr
            ),
        }
    }

    #[test]
    fn recognize_tail_calls() {
        let src = indoc!(
            r"
                g = \x ->
                    when x is
                        0 -> 0
                        _ -> g (x - 1)

                # use parens to force the ordering!
                (
                    h = \x ->
                        when x is
                            0 -> 0
                            _ -> g (x - 1)

                    (
                        p = \x ->
                            when x is
                                0 -> 0
                                1 -> g (x - 1)
                                _ -> p (x - 1)


                        # variables must be (indirectly) referenced in the body for analysis to work
                        { x: p, y: h }
                    )
                )
            "
        );
        let arena = Bump::new();
        let CanExprOut {
            loc_expr, problems, ..
        } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
        assert!(problems
            .iter()
            .all(|problem| matches!(problem, Problem::UnusedDef(_, _))));

        let actual = loc_expr.value;

        let g_detected = get_closure(&actual, 0);
        let h_detected = get_closure(&actual, 1);
        let p_detected = get_closure(&actual, 2);

        assert_eq!(g_detected, Recursive::TailRecursive);
        assert_eq!(h_detected, Recursive::NotRecursive);
        assert_eq!(p_detected, Recursive::TailRecursive);
    }

    // TODO restore this test! It should report two unused defs (h and p), but only reports 1.
    // #[test]
    // fn reproduce_incorrect_unused_defs() {
    //     let src = indoc!(
    //         r"
    //             g = \x ->
    //                 when x is
    //                     0 -> 0
    //                     _ -> g (x - 1)

    //             h = \x ->
    //                 when x is
    //                     0 -> 0
    //                     _ -> g (x - 1)

    //             p = \x ->
    //                 when x is
    //                     0 -> 0
    //                     1 -> g (x - 1)
    //                     _ -> p (x - 1)

    //             # variables must be (indirectly) referenced in the body for analysis to work
    //             # { x: p, y: h }
    //             g
    //         "
    //     );
    //     let arena = Bump::new();
    //     let CanExprOut {
    //         loc_expr, problems, ..
    //     } = can_expr_with(&arena, test_home(), src);

    //     // There should be two UnusedDef problems: one for h, and one for p
    //     assert_eq!(problems.len(), 2);
    //     assert!(problems.iter().all(|problem| match problem {
    //         Problem::UnusedDef(_, _) => true,
    //         _ => false,
    //     }));

    //     let actual = loc_expr.value;
    //     // NOTE: the indices associated with each of these can change!
    //     // They come out of a hashmap, and are not sorted.
    //     let g_detected = get_closure(&actual, 0);
    //     let h_detected = get_closure(&actual, 2);
    //     let p_detected = get_closure(&actual, 1);

    //     assert_eq!(g_detected, Recursive::TailRecursive);
    //     assert_eq!(h_detected, Recursive::NotRecursive);
    //     assert_eq!(p_detected, Recursive::TailRecursive);
    // }

    #[test]
    fn when_tail_call() {
        let src = indoc!(
            r"
                g = \x ->
                    when x is
                        0 -> 0
                        _ -> g (x + 1)

                g 0
            "
        );
        let arena = Bump::new();
        let CanExprOut {
            loc_expr, problems, ..
        } = can_expr_with(&arena, test_home(), src);
        assert_eq!(problems, Vec::new());

        let detected = get_closure(&loc_expr.value, 0);
        assert_eq!(detected, Recursive::TailRecursive);
    }

    #[test]
    fn immediate_tail_call() {
        let src = indoc!(
            r"
                f = \x -> f x

                f 0
            "
        );
        let arena = Bump::new();
        let CanExprOut {
            loc_expr, problems, ..
        } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());

        let detected = get_closure(&loc_expr.value, 0);

        assert_eq!(detected, Recursive::TailRecursive);
    }

    #[test]
    fn when_condition_is_no_tail_call() {
        let src = indoc!(
            r"
            q = \x ->
                    when q x is
                        _ -> 0

            q 0
        "
        );
        let arena = Bump::new();
        let CanExprOut {
            loc_expr, problems, ..
        } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
        let detected = get_closure(&loc_expr.value, 0);
        assert_eq!(detected, Recursive::Recursive);
    }

    #[test]
    fn good_mutual_recursion() {
        let src = indoc!(
            r"
                q = \x ->
                        when x is
                            0 -> 0
                            _ -> p (x - 1)

                p = \x ->
                        when x is
                            0 -> 0
                            _ -> q (x - 1)

                q p
            "
        );
        let arena = Bump::new();
        let CanExprOut {
            loc_expr, problems, ..
        } = can_expr_with(&arena, test_home(), src);
        assert_eq!(problems, Vec::new());

        let actual = loc_expr.value;
        let detected = get_closure(&actual, 0);
        assert_eq!(detected, Recursive::Recursive);

        let detected = get_closure(&actual, 1);
        assert_eq!(detected, Recursive::Recursive);
    }

    #[test]
    fn valid_self_recursion() {
        let src = indoc!(
            r"
                boom = \_ -> boom {}

                boom
            "
        );
        let arena = Bump::new();
        let CanExprOut {
            loc_expr, problems, ..
        } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());

        let is_circular_def = matches!(loc_expr.value, RuntimeError(RuntimeError::CircularDef(_)));

        assert_eq!(is_circular_def, false);
    }

    #[test]
    fn invalid_mutual_recursion() {
        let src = indoc!(
            r"
                x = y
                y = z
                z = x

                x
            "
        );
        let home = test_home();
        let arena = Bump::new();
        let CanExprOut {
            loc_expr,
            problems,
            interns,
            ..
        } = can_expr_with(&arena, home, src);

        let problem = Problem::RuntimeError(RuntimeError::CircularDef(vec![
            CycleEntry {
                symbol: interns.symbol(home, "x".into()),
                symbol_region: Region::new(Position::new(0), Position::new(1)),
                expr_region: Region::new(Position::new(4), Position::new(5)),
            },
            CycleEntry {
                symbol: interns.symbol(home, "y".into()),
                symbol_region: Region::new(Position::new(6), Position::new(7)),
                expr_region: Region::new(Position::new(10), Position::new(11)),
            },
            CycleEntry {
                symbol: interns.symbol(home, "z".into()),
                symbol_region: Region::new(Position::new(12), Position::new(13)),
                expr_region: Region::new(Position::new(16), Position::new(17)),
            },
        ]));

        assert_eq!(problems, vec![problem]);

        match loc_expr.value {
            RuntimeError(RuntimeError::CircularDef(_)) => (),
            actual => {
                panic!("Expected a CircularDef runtime error, but got {:?}", actual);
            }
        }
    }

    #[test]
    fn dict() {
        let src = indoc!(
            r"
                x = Dict.empty {}

                Dict.len x
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
    }

    #[test]
    fn unused_def_regression() {
        let src = indoc!(
            r"
                Booly : [Yes, No, Maybe]

                y : Booly
                y = No

                # There was a bug where annotating a def meant that its
                # references no longer got reported.
                #
                # https://github.com/roc-lang/roc/issues/298
                x : List Booly
                x = [y]

                x
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
    }

    #[test]
    fn optional_field_not_unused() {
        let src = indoc!(
            r"
                fallbackZ = 3

                fn = \{ x, y, z ? fallbackZ } ->
                    { x, y, z }

                fn { x: 0, y: 1 }
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
    }

    #[test]
    fn issue_2534() {
        let src = indoc!(
            r"
            x = { a: 1 }
            {
                x & a: 2
            }
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
    }

    //#[test]
    //fn closing_over_locals() {
    //    // "local" should be used, because the closure used it.
    //    // However, "unused" should be unused.
    //    let (_, output, problems, _) = can_expr(indoc!(
    //        r"
    //        local = 5
    //        unused = 6
    //        func = \arg -> arg + local

    //        3 + func 2
    //    "
    //    ));

    //    assert_eq!(
    //        problems,
    //        vec![Problem::UnusedAssignment(loc((
    //            "unused".to_string()
    //        )))]
    //    );

    //    assert_eq!(
    //        output,
    //        Out {
    //            lookups: vec!["func", "local"],
    //            calls: vec!["func"],
    //            tail_call: None
    //        }
    //        .into()
    //    );
    //}

    //#[test]
    //fn unused_closure() {
    //    // "unused" should be unused because it's in func, which is unused.
    //    let (_, output, problems, _) = can_expr(indoc!(
    //        r"
    //        local = 5
    //        unused = 6
    //        func = \arg -> arg + unused

    //        local
    //    "
    //    ));

    //    assert_eq!(
    //        problems,
    //        vec![
    //            Problem::UnusedAssignment(loc(("unused".to_string()))),
    //            Problem::UnusedAssignment(loc(("func".to_string()))),
    //       ]
    //    );

    //    assert_eq!(
    //        output,
    //        Out {
    //            lookups: vec!["local"],
    //            calls: vec![],
    //            tail_call: None
    //        }
    //        .into()
    //    );
    //}

    //     // UNRECOGNIZED

    //     #[test]
    //     fn basic_unrecognized_constant() {
    //         let (expr, output, problems, _) = can_expr(indoc!(
    //             r"
    //             x
    //         "
    //         ));

    //         assert_eq!(
    //             problems,
    //             vec![Problem::LookupNotInScope(loc(("x".to_string())))]
    //         );

    //         assert_eq!(expr, LookupNotInScope(loc(("x".to_string()))));

    //         assert_eq!(
    //             output,
    //             Out {
    //                 lookups: vec![],
    //                 calls: vec![],
    //                 tail_call: None
    //             }
    //             .into()
    //         );
    //     }

    //#[test]
    //fn complex_unrecognized_constant() {
    //    let (_, output, problems, _) = can_expr(indoc!(
    //        r"
    //        a = 5
    //        b = 6

    //        a + b * z
    //    "
    //    ));

    //    assert_eq!(
    //        problems,
    //        vec![Problem::LookupNotInScope(loc((
    //            "z".to_string()
    //        )))]
    //    );

    //    assert_eq!(
    //        output,
    //        Out {
    //            lookups: vec!["a", "b"],
    //            calls: vec![],
    //            tail_call: None
    //        }
    //        .into()
    //    );
    //}

    // // UNUSED

    //#[test]
    //fn mutual_unused_circular_vars() {
    //    // This should report that both a and b are unused, since the return expr never references them.
    //    // It should not report them as circular, since we haven't solved the halting problem here.
    //    let (_, output, problems, _) = can_expr(indoc!(
    //        r"
    //        a = \arg -> if arg > 0 then b 7 else 0
    //        b = \arg -> if arg > 0 then a (arg - 1) else 0
    //        c = 5

    //        c
    //    "
    //    ));

    //    assert_eq!(problems, vec![unused("a"), unused("b")]);

    //    assert_eq!(
    //        output,
    //        Out {
    //            lookups: vec!["c"],
    //            calls: vec![],
    //            tail_call: None
    //        }
    //        .into()
    //    );
    //}

    //#[test]
    //fn can_fibonacci() {
    //    let (_, output, problems, _) = can_expr(indoc!(
    //        r"
    //        fibonacci = \num ->
    //            if num < 2 then
    //                num
    //            else
    //                fibonacci (num - 1) + fibonacci (num - 2)

    //        fibonacci 9
    //    "
    //    ));

    //    assert_eq!(problems, vec![]);

    //    assert_eq!(
    //        output,
    //        Out {
    //            lookups: vec!["fibonacci"],
    //            calls: vec!["fibonacci"],
    //            tail_call: None
    //        }
    //        .into()
    //    );
    //}

    //#[test]
    //fn can_tail_call() {
    //    // TODO check the global params - make sure this
    //    // is considered a tail call, even though it only
    //    // calls itself from one branch!
    //    let (_, output, problems, _) = can_expr(indoc!(
    //        r"
    //        factorial = \num ->
    //            factorialHelp num 0

    //        factorialHelp = \num total ->
    //            if num == 0 then
    //                total
    //            else
    //                factorialHelp (num - 1) (total * num)

    //        factorial 9
    //    "
    //    ));

    //    assert_eq!(problems, vec![]);

    //    assert_eq!(
    //        output,
    //        Out {
    //            lookups: vec!["factorial", "factorialHelp"],
    //            calls: vec!["factorial", "factorialHelp"],
    //            tail_call: None
    //        }
    //        .into()
    //    );
    //}

    //#[test]
    //fn transitively_used_function() {
    //    // This should report that neither a nor b are unused,
    //    // since if you never call a function but do return it, that's okay!
    //    let (_, output, problems, _) = can_expr(indoc!(
    //        r"
    //        a = \_ -> 42
    //        b = a

    //        b
    //    "
    //    ));

    //    assert_eq!(problems, Vec::new());

    //    assert_eq!(
    //        output,
    //        Out {
    //            lookups: vec!["a", "b"],
    //            calls: vec![],
    //            tail_call: None
    //        }
    //        .into()
    //    );
    //}

    // // ASSIGNMENT REORDERING

    //#[test]
    //fn reorder_assignments() {
    //    let (expr, output, problems, _) = can_expr(indoc!(
    //        r"
    //        increment = \arg -> arg + 1
    //        z = (increment 2) + y
    //        y = x + 1
    //        x = 9

    //        z * 3
    //    "
    //    ));

    //    assert_eq!(problems, vec![]);

    //    assert_eq!(
    //        output,
    //        Out {
    //            lookups: vec!["increment", "x", "y", "z"],
    //            calls: vec!["increment"],
    //            tail_call: None
    //        }
    //        .into()
    //    );

    //    let symbols = assigned_symbols(expr);

    //    // In code gen, for everything to have been set before it gets read,
    //    // the following must be true about when things are assigned:
    //    //
    //    // x must be assigned before y
    //    // y must be assigned before z
    //    //
    //    // The order of the increment function doesn't matter.
    //    assert_before("x", "y", &symbols);
    //    assert_before("y", "z", &symbols);
    //}

    //#[test]
    //fn reorder_closed_over_assignments() {
    //    let (expr, output, problems, _) = can_expr(indoc!(
    //        r"
    //        z = func1 x
    //        x = 9
    //        y = func2 3
    //        func1 = \arg -> func2 arg + y
    //        func2 = \arg -> arg + x

    //        z
    //    "
    //    ));

    //    assert_eq!(problems, vec![]);

    //    assert_eq!(
    //        output,
    //        Out {
    //            lookups: vec!["func1", "func2", "x", "y", "z"],
    //            calls: vec!["func1", "func2"],
    //            tail_call: None
    //        }
    //        .into()
    //    );

    //    let symbols = assigned_symbols(expr);

    //    // In code gen, for everything to have been set before it gets read,
    //    // the following must be true about when things are assigned:
    //    //
    //    // x and func2 must be assigned (in either order) before y
    //    // y and func1 must be assigned (in either order) before z
    //    assert_before("x", "y", &symbols);
    //    assert_before("func2", "y", &symbols);

    //    assert_before("func1", "z", &symbols);
    //    assert_before("y", "z", &symbols);
    //}

    //fn assert_before(before: &str, after: &str, symbols: &Vec<Symbol>) {
    //    assert_ne!(before, after);

    //    let before_symbol = sym(before);
    //    let after_symbol = sym(after);
    //    let before_index = symbols
    //        .iter()
    //        .position(|symbol| symbol == &before_symbol)
    //        .unwrap_or_else(|| {
    //            panic!(
    //                "error in assert_before({:?}, {:?}): {:?} could not be found in {:?}",
    //                before,
    //                after,
    //                sym(before),
    //                symbols
    //            )
    //        });
    //    let after_index = symbols
    //        .iter()
    //        .position(|symbol| symbol == &after_symbol)
    //        .unwrap_or_else(|| {
    //            panic!(
    //                "error in assert_before({:?}, {:?}): {:?} could not be found in {:?}",
    //                before,
    //                after,
    //                sym(after),
    //                symbols
    //            )
    //        });

    //    if before_index == after_index {
    //        panic!(
    //            "error in assert_before({:?}, {:?}): both were at index {} in {:?}",
    //            before, after, after_index, symbols
    //        );
    //    } else if before_index > after_index {
    //        panic!("error in assert_before: {:?} appeared *after* {:?} (not before, as expected) in {:?}", before, after, symbols);
    //    }
    //}

    //fn assigned_symbols(expr: Expr) -> Vec<Symbol> {
    //    match expr {
    //        Assign(assignments, _) => {
    //            assignments.into_iter().map(|(pattern, _)| {
    //                match pattern.value {
    //                    Identifier(symbol) => {
    //                        symbol
    //                    },
    //                    _ => {
    //                        panic!("Called assigned_symbols passing an Assign expr with non-Identifier patterns!");
    //                    }
    //                }
    //            }).collect()
    //        },
    //        _ => {
    //            panic!("Called assigned_symbols passing a non-Assign expr!");
    //        }
    //    }
    //}

    // // CIRCULAR ASSIGNMENT

    //#[test]
    //fn circular_assignment() {
    //    let (_, _, problems, _) = can_expr(indoc!(
    //        r"
    //        c = d + 3
    //        b = 2 + c
    //        d = a + 7
    //        a = b + 1

    //        2 + d
    //    "
    //    ));

    //    assert_eq!(
    //        problems,
    //        vec![Problem::CircularAssignment(vec![
    //            // c should appear first because it's assigned first in the original expression.
    //            loc(unqualified("c")),
    //            loc(unqualified("d")),
    //            loc(unqualified("a")),
    //            loc(unqualified("b")),
    //       ])]
    //    );
    //}

    //#[test]
    //fn always_function() {
    //    // There was a bug where this reported UnusedArgument("val")
    //    // since it was used only in the returned function only.
    //    let (_, _, problems, _) = can_expr(indoc!(
    //        r"
    //        \val -> \_ -> val
    //    "
    //    ));

    //    assert_eq!(problems, vec![]);
    //}

    // // TODO verify that Apply handles output.references.calls correctly

    // // UNSUPPORTED PATTERNS

    // // TODO verify that in closures and assignments, you can't assign to int/string/underscore/etc

    // // OPERATOR PRECEDENCE

    // // fn parse_with_precedence(input: &str) -> Result<(Expr, &str), easy::Errors<char, &str, IndentablePosition>> {
    // //     parse_without_loc(input)
    // //         .map(|(expr, remaining)| (expr::apply_precedence_and_associativity(loc(expr)).unwrap().value, remaining))
    // // }

    // // #[test]
    // // fn two_operator_precedence() {
    // //     assert_eq!(
    // //         parse_with_precedence("x + y * 5"),
    // //         Ok((BinOp(
    // //                 loc_box(var("x")),
    // //                 loc(Plus),
    // //                 loc_box(
    // //                     BinOp(
    // //                         loc_box(var("y")),
    // //                         loc(Star),
    // //                         loc_box(Int(5))
    // //                     )
    // //                 ),
    // //             ),
    // //         ""))
    // //     );

    // //     assert_eq!(
    // //         parse_with_precedence("x * y + 5"),
    // //         Ok((BinOp(
    // //                 loc_box(
    // //                     BinOp(
    // //                         loc_box(var("x")),
    // //                         loc(Star),
    // //                         loc_box(var("y")),
    // //                     )
    // //                 ),
    // //                 loc(Plus),
    // //                 loc_box(Int(5))
    // //             ),
    // //         ""))
    // //     );
    // // }

    // // #[test]
    // // fn compare_and() {
    // //     assert_eq!(
    // //         parse_with_precedence("x > 1 || True"),
    // //         Ok((BinOp(
    // //                 loc_box(
    // //                     BinOp(
    // //                         loc_box(var("x")),
    // //                         loc(GreaterThan),
    // //                         loc_box(Int(1))
    // //                     )
    // //                 ),
    // //                 loc(Or),
    // //                 loc_box(ApplyVariant(vname("True"), None))
    // //             ),
    // //         ""))
    // //     );
    // // }

    // // HELPERS

    //#[test]
    //fn sort_cyclic_idents() {
    //    let assigned_idents = unqualifieds(vec!["blah", "c", "b", "d", "a"]);

    //    assert_eq!(
    //        can::sort_cyclic_idents(
    //            loc_unqualifieds(vec!["a", "b", "c", "d"]),
    //            &mut assigned_idents.iter()
    //        ),
    //        loc_unqualifieds(vec!["c", "d", "a", "b"])
    //    );
    //}
    //
    //
    // STRING LITERALS

    #[test]
    fn string_with_valid_unicode_escapes() {
        assert_can_string(r#""x\u(00A0)x""#, "x\u{00A0}x");
        assert_can_string(r#""x\u(101010)x""#, "x\u{101010}x");
    }

    #[test]
    fn block_string() {
        assert_can_string(
            r#"
            """foobar"""
            "#,
            "foobar",
        );

        assert_can_string(
            indoc!(
                r#"
            """foo
            bar"""
            "#
            ),
            "foo\nbar",
        );
    }

    //     #[test]
    //     fn string_with_too_large_unicode_escape() {
    //         // Should be too big - max size should be 10FFFF.
    //         // (Rust has this restriction. I assume it's a good idea.)
    //         assert_malformed_str(
    //             r#""abc\u{110000}def""#,
    //             vec![Located::new(0, 7, 0, 12, Problem::UnicodeCodePtTooLarge)],
    //         );
    //     }

    //     #[test]
    //     fn string_with_no_unicode_digits() {
    //         // No digits specified
    //         assert_malformed_str(
    //             r#""blah\u{}foo""#,
    //             vec![Located::new(0, 5, 0, 8, Problem::NoUnicodeDigits)],
    //         );
    //     }

    //     #[test]
    //     fn string_with_no_unicode_opening_brace() {
    //         // No opening curly brace. It can't be sure if the closing brace
    //         // was intended to be a closing brace for the unicode escape, so
    //         // report that there were no digits specified.
    //         assert_malformed_str(
    //             r#""abc\u00A0}def""#,
    //             vec![Located::new(0, 4, 0, 5, Problem::NoUnicodeDigits)],
    //         );
    //     }

    //     #[test]
    //     fn string_with_no_unicode_closing_brace() {
    //         // No closing curly brace
    //         assert_malformed_str(
    //             r#""blah\u{stuff""#,
    //             vec![Located::new(0, 5, 0, 12, Problem::MalformedEscapedUnicode)],
    //         );
    //     }

    //     #[test]
    //     fn string_with_no_unicode_braces() {
    //         // No curly braces
    //         assert_malformed_str(
    //             r#""zzzz\uzzzzz""#,
    //             vec![Located::new(0, 5, 0, 6, Problem::NoUnicodeDigits)],
    //         );
    //     }

    // #[test]
    // fn string_with_escaped_interpolation() {
    //     assert_parses_to(
    //         // This should NOT be string interpolation, because of the \\
    //         indoc!(
    //             r#"
    //                      "abcd\$(efg)hij"
    //                      "#
    //         ),
    //         Str(r"abcd$(efg)hij".into()),
    //     );
    // }

    //     #[test]
    //     fn string_without_escape() {
    //         expect_parsed_str("a", r#""a""#);
    //         expect_parsed_str("ab", r#""ab""#);
    //         expect_parsed_str("abc", r#""abc""#);
    //         expect_parsed_str("123", r#""123""#);
    //         expect_parsed_str("abc123", r#""abc123""#);
    //         expect_parsed_str("123abc", r#""123abc""#);
    //         expect_parsed_str("123 abc 456 def", r#""123 abc 456 def""#);
    //     }

    //     #[test]
    //     fn string_with_special_escapes() {
    //         expect_parsed_str(r"x\x", r#""x\\x""#);
    //         expect_parsed_str(r#"x"x"#, r#""x\"x""#);
    //         expect_parsed_str("x\tx", r#""x\tx""#);
    //         expect_parsed_str("x\rx", r#""x\rx""#);
    //         expect_parsed_str("x\nx", r#""x\nx""#);
    //     }

    //     fn assert_malformed_str<'a>(input: &'a str, expected_probs: Vec<Located<Problem>>) {
    //         let arena = Bump::new();
    //         let actual = parse_with(&arena, input);

    //         assert_eq!(
    //             Ok(Expr::MalformedStr(expected_probs.into_boxed_slice())),
    //             actual
    //         );
    //     }
    //
    // TODO test what happens when interpolated strings contain 1+ malformed idents
    //
    // TODO test hex/oct/binary conversion to numbers
    //
    // TODO test for \t \r and \n in string literals *outside* unicode escape sequence!
    //
    // TODO test for multiline block string literals in pattern matches
}
