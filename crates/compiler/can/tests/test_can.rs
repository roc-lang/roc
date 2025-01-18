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
    use roc_can::expr::{ClosureData, IntValue, Recursive, WhenBranch};
    use roc_can::pattern::Pattern;
    use roc_module::called_via::CalledVia;
    use roc_problem::can::{CycleEntry, FloatErrorKind, IntErrorKind, Problem, RuntimeError};
    use roc_region::all::{Loc, Position, Region};
    use roc_types::subs::Variable;
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
                f = | a| a

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
                f = | a| a

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
                g = | a| a

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
                g = | a| a

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

                f = | a| a

                f 42
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
    }

    #[test]
    fn separated_annotated_body_with_comment() {
        let src = indoc!(
            r"
                f : Num.Int * -> Num.Int *
                # comment
                f = | a| a

                f 42
            "
        );
        let arena = Bump::new();
        let CanExprOut { problems, .. } = can_expr_with(&arena, test_home(), src);

        assert_eq!(problems, Vec::new());
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

        assert_eq!(problems.len(), 1);
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
    fn record_builder_desugar() {
        let src = indoc!(
            r#"
                map2 = |a, b, combine| combine a b
                double = |n| n * 2

                c = 3

                { map2 <-
                    a: 1,
                    b: double 2,
                    c
                }
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems.len(), 0);

        // Assert that we desugar to:
        //
        // map2
        //     (1)
        //     (map2
        //         (double 2)
        //         (c)
        //         (\#a, #b -> (#a, #b))
        //     )
        //     (\a, (b, c) -> { a: #a, b: #b, c: #c })

        let first_map2_args = assert_func_call(
            &out.loc_expr.value,
            "map2",
            CalledVia::RecordBuilder,
            &out.interns,
        );
        let (first_arg, second_arg, third_arg) = match &first_map2_args[..] {
            [first, second, third] => (&first.1.value, &second.1.value, &third.1.value),
            _ => panic!("map2 didn't receive three arguments"),
        };

        assert_num_value(first_arg, 1);

        let inner_map2_args =
            assert_func_call(second_arg, "map2", CalledVia::RecordBuilder, &out.interns);
        let (first_inner_arg, second_inner_arg, third_inner_arg) = match &inner_map2_args[..] {
            [first, second, third] => (&first.1.value, &second.1.value, &third.1.value),
            _ => panic!("inner map2 didn't receive three arguments"),
        };

        let double_args =
            assert_func_call(first_inner_arg, "double", CalledVia::Space, &out.interns);
        assert_eq!(double_args.len(), 1);
        assert_num_value(&double_args[0].1.value, 2);

        assert_var_usage(second_inner_arg, "c", &out.interns);

        match third_inner_arg {
            Expr::Closure(ClosureData {
                arguments,
                loc_body,
                ..
            }) => {
                assert_eq!(arguments.len(), 2);
                assert_pattern_name(
                    &arguments[0].2.value,
                    "#record_builder_closure_arg_a",
                    &out.interns,
                );
                assert_pattern_name(
                    &arguments[1].2.value,
                    "#record_builder_closure_arg_b",
                    &out.interns,
                );

                match &loc_body.value {
                    Expr::Tuple { elems, .. } => {
                        assert_eq!(elems.len(), 2);
                        assert_var_usage(
                            &elems[0].1.value,
                            "#record_builder_closure_arg_a",
                            &out.interns,
                        );
                        assert_var_usage(
                            &elems[1].1.value,
                            "#record_builder_closure_arg_b",
                            &out.interns,
                        );
                    }
                    _ => panic!("Closure body was not a tuple"),
                }
            }
            _ => panic!("inner map2's combiner was not a closure"),
        }

        match third_arg {
            Expr::Closure(ClosureData {
                arguments,
                loc_body,
                ..
            }) => {
                assert_eq!(arguments.len(), 2);
                assert_pattern_name(&arguments[0].2.value, "#a", &out.interns);
                match &arguments[1].2.value {
                    Pattern::TupleDestructure { destructs, .. } => {
                        assert_eq!(destructs.len(), 2);
                        assert_pattern_name(&destructs[0].value.typ.1.value, "#b", &out.interns);
                        assert_pattern_name(&destructs[1].value.typ.1.value, "#c", &out.interns);
                    }
                    _ => panic!("Second arg to builder func was not a tuple destructure"),
                }

                match &loc_body.value {
                    Expr::Record { fields, .. } => {
                        assert_eq!(fields.len(), 3);

                        assert_eq!(get_field_var_sym(fields, "a").as_str(&out.interns), "#a");
                        assert_eq!(get_field_var_sym(fields, "b").as_str(&out.interns), "#b");
                        assert_eq!(get_field_var_sym(fields, "c").as_str(&out.interns), "#c");
                    }
                    _ => panic!("Closure body was not a tuple"),
                }
            }
            _ => panic!("inner map2's combiner was not a closure"),
        }
    }

    #[test]
    fn question_suffix_simple() {
        let src = indoc!(
            r#"
                (Str.to_u64 "123")?
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems, Vec::new());

        // Assert that we desugar to:
        //
        // Try(Str.to_u64 "123")

        let cond_expr = assert_try_expr(&out.loc_expr.value);
        let cond_args = assert_func_call(cond_expr, "to_u64", CalledVia::Space, &out.interns);

        assert_eq!(cond_args.len(), 1);
        assert_str_value(&cond_args[0].1.value, "123");
    }

    #[test]
    fn question_suffix_after_function() {
        let src = indoc!(
            r#"
                Str.to_u64? "123"
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems, Vec::new());

        // Assert that we desugar to:
        //
        // Try(Str.to_u64 "123")

        let cond_expr = assert_try_expr(&out.loc_expr.value);
        let cond_args = assert_func_call(cond_expr, "to_u64", CalledVia::Try, &out.interns);

        assert_eq!(cond_args.len(), 1);
        assert_str_value(&cond_args[0].1.value, "123");
    }

    #[test]
    fn question_suffix_pipe() {
        let src = indoc!(
            r#"
                "123" |> Str.to_u64?
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems, Vec::new());

        // Assert that we desugar to:
        //
        // Try(Str.to_u64 "123")

        let cond_expr = assert_try_expr(&out.loc_expr.value);
        let cond_args = assert_func_call(cond_expr, "to_u64", CalledVia::Try, &out.interns);

        assert_eq!(cond_args.len(), 1);
        assert_str_value(&cond_args[0].1.value, "123");
    }

    #[test]
    fn question_suffix_pipe_nested() {
        let src = indoc!(
            r#"
                "123" |> Str.to_u64? (Ok 123)?
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems, Vec::new());

        // Assert that we desugar to:
        //
        // Try(Str.to_u64 "123" Try(Ok 123))

        let cond_expr = assert_try_expr(&out.loc_expr.value);
        let cond_args = assert_func_call(cond_expr, "to_u64", CalledVia::Try, &out.interns);

        assert_eq!(cond_args.len(), 2);

        assert_str_value(&cond_args[0].1.value, "123");

        let ok_tag = assert_try_expr(&cond_args[1].1.value);
        let tag_args = assert_tag_application(ok_tag, "Ok");

        assert_eq!(tag_args.len(), 1);

        assert_num_value(&tag_args[0].1.value, 123);
    }

    #[test]
    fn try_desugar_plain_prefix() {
        let src = indoc!(
            r#"
                try Str.to_u64 "123"
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems, Vec::new());

        // Assert that we desugar to:
        //
        // Try(Str.to_u64 "123")

        let cond_expr = assert_try_expr(&out.loc_expr.value);
        let cond_args = assert_func_call(cond_expr, "to_u64", CalledVia::Try, &out.interns);

        assert_eq!(cond_args.len(), 1);
        assert_str_value(&cond_args[0].1.value, "123");
    }

    #[test]
    fn try_desugar_pipe_prefix() {
        let src = indoc!(
            r#"
                "123" |> try Str.to_u64
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems, Vec::new());

        // Assert that we desugar to:
        //
        // Try(Str.to_u64 "123")

        let cond_expr = assert_try_expr(&out.loc_expr.value);
        let cond_args = assert_func_call(cond_expr, "to_u64", CalledVia::Try, &out.interns);

        assert_eq!(cond_args.len(), 1);
        assert_str_value(&cond_args[0].1.value, "123");
    }

    #[test]
    fn try_desugar_pipe_suffix() {
        let src = indoc!(
            r#"
                Str.to_u64 "123" |> try
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems, Vec::new());

        // Assert that we desugar to:
        //
        // Try(Str.to_u64 "123")

        let cond_expr = assert_try_expr(&out.loc_expr.value);
        let cond_args = assert_func_call(cond_expr, "to_u64", CalledVia::Space, &out.interns);

        assert_eq!(cond_args.len(), 1);
        assert_str_value(&cond_args[0].1.value, "123");
    }

    #[test]
    fn try_desugar_works_elsewhere() {
        let src = indoc!(
            r#"
                when Foo 123 is
                    Foo try -> try
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems, Vec::new());

        // Assert that we don't treat `try` as a keyword here
        // by desugaring to:
        //
        // when Foo 123 is
        //     Foo try -> try

        let (cond_expr, branches) = assert_when_expr(&out.loc_expr.value);
        match cond_expr {
            Expr::Tag {
                name, arguments, ..
            } => {
                assert_eq!(name.0.to_string(), "Foo");
                assert_eq!(arguments.len(), 1);
                assert_num_value(&arguments[0].1.value, 123);
            }
            _ => panic!("cond_expr was not a Tag: {:?}", cond_expr),
        }

        assert_eq!(branches.len(), 1);
        assert_eq!(branches[0].patterns.len(), 1);
        assert!(!branches[0].patterns[0].degenerate);

        match &branches[0].patterns[0].pattern.value {
            Pattern::AppliedTag {
                tag_name,
                arguments,
                ..
            } => {
                assert_eq!(tag_name.0.to_string(), "Foo");
                assert_eq!(arguments.len(), 1);
                assert_pattern_name(&arguments[0].1.value, "try", &out.interns);
            }
            other => panic!("First argument was not an applied tag: {:?}", other),
        }

        assert_var_usage(&branches[0].value.value, "try", &out.interns);
        assert!(&branches[0].guard.is_none());
    }

    #[test]
    fn desugar_double_question_binop() {
        let src = indoc!(
            r#"
                Str.to_u64("123") ?? Num.max_u64
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems, Vec::new());

        // Assert that we desugar to:
        //
        // when Str.to_u64("123")
        //   Ok(#double_question_ok_0_17) -> Ok(#double_question_ok_0_17)
        //   Err(_) -> Num.max_u64

        let (cond_expr, branches) = assert_when_expr(&out.loc_expr.value);
        let cond_args = assert_func_call(cond_expr, "to_u64", CalledVia::Space, &out.interns);

        assert_eq!(cond_args.len(), 1);
        assert_str_value(&cond_args[0].1.value, "123");

        assert_eq!(branches.len(), 2);
        assert_eq!(branches[0].patterns.len(), 1);
        assert_eq!(branches[1].patterns.len(), 1);

        assert_pattern_tag_apply_with_ident(
            &branches[0].patterns[0].pattern.value,
            "Ok",
            "#double_question_ok_0_17",
            &out.interns,
        );
        assert_var_usage(
            &branches[0].value.value,
            "#double_question_ok_0_17",
            &out.interns,
        );

        assert_pattern_tag_apply_with_underscore(&branches[1].patterns[0].pattern.value, "Err");
        assert_var_usage(&branches[1].value.value, "max_u64", &out.interns);
    }

    #[test]
    fn desugar_single_question_binop() {
        let src = indoc!(
            r#"
                Str.to_u64("123") ? FailedToConvert
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems, Vec::new());

        // Assert that we desugar to:
        //
        // when Str.to_u64("123")
        //   Ok(#single_question_ok_0_17) -> #single_question_ok_0_17
        //   Err(#single_question_err_0_17) -> return Err(FailedToConvert(#single_question_err_0_17))

        let (cond_expr, branches) = assert_when_expr(&out.loc_expr.value);
        let cond_args = assert_func_call(cond_expr, "to_u64", CalledVia::Space, &out.interns);

        assert_eq!(cond_args.len(), 1);
        assert_str_value(&cond_args[0].1.value, "123");

        assert_eq!(branches.len(), 2);
        assert_eq!(branches[0].patterns.len(), 1);
        assert_eq!(branches[1].patterns.len(), 1);

        assert_pattern_tag_apply_with_ident(
            &branches[0].patterns[0].pattern.value,
            "Ok",
            "#single_question_ok_0_17",
            &out.interns,
        );
        assert_var_usage(
            &branches[0].value.value,
            "#single_question_ok_0_17",
            &out.interns,
        );

        assert_pattern_tag_apply_with_ident(
            &branches[1].patterns[0].pattern.value,
            "Err",
            "#single_question_err_0_17",
            &out.interns,
        );

        let err_expr = assert_return_expr(&branches[1].value.value);
        let mapped_err = assert_tag_application(err_expr, "Err");
        assert_eq!(mapped_err.len(), 1);
        let inner_err = assert_tag_application(&mapped_err[0].1.value, "FailedToConvert");
        assert_eq!(inner_err.len(), 1);
        assert_var_usage(
            &inner_err[0].1.value,
            "#single_question_err_0_17",
            &out.interns,
        );
    }

    #[test]
    fn desugar_and_operator() {
        let src = indoc!(
            r#"
                left = Bool.true
                right = Bool.false

                left and right
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems, Vec::new());

        // Assert that we desugar to:
        //
        // if left then right else Bool.false

        let continuation1 = assert_let_expr(&out.loc_expr.value);
        let continuation2 = assert_let_expr(&continuation1.value);
        let (branches, final_else) = assert_if_expr(&continuation2.value);

        assert_eq!(branches.len(), 1);

        assert_var_usage(&branches[0].0.value, "left", &out.interns);
        assert_var_usage(&branches[0].1.value, "right", &out.interns);
        assert_var_usage(&final_else.value, "false", &out.interns);
    }

    #[test]
    fn desugar_or_operator() {
        let src = indoc!(
            r#"
                left = Bool.true
                right = Bool.false

                left or right
            "#
        );
        let arena = Bump::new();
        let out = can_expr_with(&arena, test_home(), src);

        assert_eq!(out.problems, Vec::new());

        // Assert that we desugar to:
        //
        // if left then Bool.true else right

        let continuation1 = assert_let_expr(&out.loc_expr.value);
        let continuation2 = assert_let_expr(&continuation1.value);
        let (branches, final_else) = assert_if_expr(&continuation2.value);

        assert_eq!(branches.len(), 1);

        assert_var_usage(&branches[0].0.value, "left", &out.interns);
        assert_var_usage(&branches[0].1.value, "true", &out.interns);
        assert_var_usage(&final_else.value, "right", &out.interns);
    }

    fn assert_num_value(expr: &Expr, num: usize) {
        match expr {
            Expr::Num(_, num_str, _, _) => {
                assert_eq!(&**num_str, &num.to_string())
            }
            _ => panic!("Expr wasn't a Num with value {num}: {:?}", expr),
        }
    }

    fn assert_str_value(expr: &Expr, str_val: &str) {
        match expr {
            Expr::Str(str_expr) => {
                assert_eq!(&**str_expr, str_val)
            }
            _ => panic!("Expr wasn't a Str with value {str_val}: {:?}", expr),
        }
    }

    fn assert_var_usage(expr: &Expr, name: &str, interns: &roc_module::symbol::Interns) {
        match expr {
            Expr::Var(sym, _) => assert_eq!(sym.as_str(interns), name),
            _ => panic!("Expr was not a variable usage: {:?}", expr),
        }
    }

    fn assert_func_call(
        expr: &Expr,
        name: &str,
        called_via: CalledVia,
        interns: &roc_module::symbol::Interns,
    ) -> Vec<(Variable, Loc<Expr>)> {
        match expr {
            Expr::LetNonRec(_, loc_expr) => {
                assert_func_call(&loc_expr.value, name, called_via, interns)
            }
            Expr::Call(fun, args, called) if called == &called_via => {
                match &fun.1.value {
                    Expr::Var(sym, _) => assert_eq!(sym.as_str(interns), name),
                    _ => panic!("Builder didn't desugar with mapper at front"),
                };

                args.clone()
            }
            _ => panic!(
                "Expr was not a Call with CalledVia={:?}: {:?}",
                called_via, expr
            ),
        }
    }

    fn assert_tag_application(expr: &Expr, tag_name: &str) -> Vec<(Variable, Loc<Expr>)> {
        match expr {
            Expr::LetNonRec(_, loc_expr) => assert_tag_application(&loc_expr.value, tag_name),
            Expr::Tag {
                name, arguments, ..
            } if *name == tag_name.into() => arguments.clone(),
            _ => panic!("Expr was not a Tag named {tag_name:?}: {expr:?}",),
        }
    }

    fn assert_pattern_name(pattern: &Pattern, name: &str, interns: &roc_module::symbol::Interns) {
        match pattern {
            Pattern::Identifier(sym) => assert_eq!(sym.as_str(interns), name),
            _ => panic!("Pattern was not an identifier: {:?}", pattern),
        }
    }

    fn assert_pattern_tag_apply_with_ident(
        pattern: &Pattern,
        name: &str,
        ident: &str,
        interns: &roc_module::symbol::Interns,
    ) {
        match pattern {
            Pattern::AppliedTag {
                tag_name,
                arguments,
                ..
            } if arguments.len() == 1 => {
                assert_eq!(tag_name.as_ident_str().as_str(), name);
                match arguments[0].1.value {
                    Pattern::Identifier(sym) => assert_eq!(sym.as_str(interns), ident),
                    _ => panic!(
                        "The tag was expected to be applied with {:?} but we instead found {:?}",
                        ident, arguments[0].1.value
                    ),
                }
            }
            _ => panic!("Pattern was not an applied tag: {:?}", pattern),
        }
    }

    fn assert_pattern_tag_apply_with_underscore(pattern: &Pattern, name: &str) {
        match pattern {
            Pattern::AppliedTag {
                tag_name,
                arguments,
                ..
            } if arguments.len() == 1 => {
                assert_eq!(tag_name.as_ident_str().as_str(), name);
                match arguments[0].1.value {
                    Pattern::Underscore => {},
                    _ => panic!(
                        "The tag was expected to be applied with an underscore but we instead found {:?}",
                        arguments[0].1.value
                    ),
                }
            }
            _ => panic!("Pattern was not an applied tag: {:?}", pattern),
        }
    }

    fn assert_let_expr(expr: &Expr) -> &Loc<Expr> {
        match expr {
            Expr::LetNonRec(_, continuation) | Expr::LetRec(_, continuation, _) => continuation,
            _ => panic!("Expr was not a Let(Non)?Rec: {expr:?}",),
        }
    }

    fn assert_when_expr(expr: &Expr) -> (&Expr, &Vec<WhenBranch>) {
        match expr {
            Expr::When {
                loc_cond, branches, ..
            } => (&loc_cond.value, branches),
            _ => panic!("Expr was not a When: {:?}", expr),
        }
    }

    #[allow(clippy::type_complexity)]
    fn assert_if_expr(expr: &Expr) -> (&[(Loc<Expr>, Loc<Expr>)], &Loc<Expr>) {
        match expr {
            Expr::If {
                branches,
                final_else,
                ..
            } => (&branches, &**final_else),
            _ => panic!("Expr was not a When: {:?}", expr),
        }
    }

    fn assert_try_expr(expr: &Expr) -> &Expr {
        match expr {
            Expr::Try { result_expr, .. } => &result_expr.value,
            _ => panic!("Expr was not a Try: {:?}", expr),
        }
    }

    fn assert_return_expr(expr: &Expr) -> &Expr {
        match expr {
            Expr::Return { return_value, .. } => &return_value.value,
            _ => panic!("Expr was not a Return: {:?}", expr),
        }
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
                g = |x|
                    when x is
                        0 -> 0
                        _ -> g (x - 1)

                # use parens to force the ordering!
                (
                    h = |x|
                        when x is
                            0 -> 0
                            _ -> g (x - 1)

                    (
                        p = |x|
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
                g = |x|
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
                f = |x| f x

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
            q = |x|
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
                q = |x|
                        when x is
                            0 -> 0
                            _ -> p (x - 1)

                p = |x|
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
                boom = |_| boom {}

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

                fn = |{ x, y, z ? fallbackZ }|
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
    //                      "abcd\${efg}hij"
    //                      "#
    //         ),
    //         Str(r"abcd${efg}hij".into()),
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
