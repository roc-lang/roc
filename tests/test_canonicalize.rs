#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_canonicalize {
    use crate::helpers::{can_expr_with, with_larger_debug_stack};
    use bumpalo::Bump;
    use roc::can::expr::Expr::{self, *};
    use roc::can::expr::Output;
    use roc::can::expr::Recursive;
    use roc::can::problem::RuntimeError;
    use roc::can::procedure::References;
    use roc::can::symbol::Symbol;
    use roc::collections::{ImMap, ImSet, SendMap};
    use std::{f64, i64};

    fn sym(name: &str) -> Symbol {
        Symbol::new("Test.Blah$", name)
    }

    struct Out<'a> {
        locals: Vec<&'a str>,
        globals: Vec<&'a str>,
        calls: Vec<&'a str>,
        tail_call: Option<&'a str>,
    }

    impl<'a> Into<Output> for Out<'a> {
        fn into(self) -> Output {
            let references = References {
                locals: vec_to_set(self.locals),
                globals: vec_to_set(self.globals),
                calls: vec_to_set(self.calls),
            };

            let tail_call = self.tail_call.map(sym);
            let rigids = SendMap::default();

            Output {
                references,
                tail_call,
                rigids,
            }
        }
    }

    fn vec_to_set<'a>(vec: Vec<&'a str>) -> ImSet<Symbol> {
        ImSet::from(vec.into_iter().map(sym).collect::<Vec<_>>())
    }

    fn assert_can(input: &str, expected: Expr) {
        let arena = Bump::new();
        let (actual, _, _, _, _, _) = can_expr_with(&arena, "Blah", input, &ImMap::default());

        assert_eq!(actual.value, expected);
    }

    // NUMBER LITERALS

    #[test]
    fn int_too_large() {
        let string = (i64::MAX as i128 + 1).to_string();

        assert_can(
            &string.clone(),
            RuntimeError(RuntimeError::IntOutsideRange(string.into())),
        );
    }

    #[test]
    fn int_too_small() {
        let string = (i64::MIN as i128 - 1).to_string();

        assert_can(
            &string.clone(),
            RuntimeError(RuntimeError::IntOutsideRange(string.into())),
        );
    }

    #[test]
    fn float_too_large() {
        let string = format!("{}1.0", f64::MAX);

        assert_can(
            &string.clone(),
            RuntimeError(RuntimeError::FloatOutsideRange(string.into())),
        );
    }

    #[test]
    fn float_too_small() {
        let string = format!("{}1.0", f64::MIN);

        assert_can(
            &string.clone(),
            RuntimeError(RuntimeError::FloatOutsideRange(string.into())),
        );
    }

    #[test]
    fn zero() {
        assert_can("0", Int(0));
    }

    #[test]
    fn minus_zero() {
        assert_can("-0", Int(0));
    }

    #[test]
    fn zero_point_zero() {
        assert_can("0.0", Float(0.0));
    }

    #[test]
    fn minus_zero_point_zero() {
        assert_can("-0.0", Float(-0.0));
    }

    #[test]
    fn hex_zero() {
        assert_can("0x0", Int(0x0));
    }

    #[test]
    fn hex_one_b() {
        assert_can("0x1b", Int(0x1b));
    }

    #[test]
    fn minus_hex_one_b() {
        assert_can("-0x1b", Int(-0x1b));
    }

    #[test]
    fn octal_zero() {
        assert_can("0o0", Int(0o0));
    }

    #[test]
    fn octal_one_two() {
        assert_can("0o12", Int(0o12));
    }

    #[test]
    fn minus_octal_one_two() {
        assert_can("-0o12", Int(-0o12));
    }

    #[test]
    fn binary_zero() {
        assert_can("0b0", Int(0b0));
    }

    #[test]
    fn binary_one_one() {
        assert_can("0b11", Int(0b11));
    }

    #[test]
    fn minus_binary_one_one() {
        assert_can("-0b11", Int(-0b11));
    }

    // LOCALS

    #[test]
    fn closure_args_are_not_locals() {
        // "arg" shouldn't make it into output.locals, because
        // it only exists in the closure's arguments.
        let arena = Bump::new();
        let src = indoc!(
            r#"
            func = \arg -> arg

            func 2
        "#
        );
        let (_actual, output, problems, _var_store, _vars, _constraint) =
            can_expr_with(&arena, "Blah", src, &ImMap::default());

        assert_eq!(problems, vec![]);

        assert_eq!(
            output,
            Out {
                locals: vec!["func"],
                globals: vec![],
                calls: vec!["func"],
                tail_call: None
            }
            .into()
        );
    }

    #[test]
    fn call_by_pointer_for_fn_args() {
        // This function will get passed in as a pointer.
        let src = indoc!(
            r#"
            apply = \f, x -> f x

            identity = \a -> a

            apply identity 5
        "#
        );
        let arena = Bump::new();
        let (_actual, output, problems, _var_store, _vars, _constraint) =
            can_expr_with(&arena, "Blah", src, &ImMap::default());

        assert_eq!(problems, vec![]);

        assert_eq!(
            output,
            Out {
                locals: vec!["identity", "apply"],
                globals: vec![],
                calls: vec!["f", "apply"],
                tail_call: None
            }
            .into()
        );
    }

    fn get_closure(expr: &Expr, i: usize) -> roc::can::expr::Recursive {
        match expr {
            Defs(assignments, _) => match &assignments.get(i).map(|def| &def.expr.value) {
                Some(Closure(_, recursion, _, _)) => recursion.clone(),
                Some(other @ _) => {
                    panic!("assignment at {} is not a closure, but a {:?}", i, other)
                }
                None => panic!("Looking for assignment at {} but the list is too short", i),
            },
            _ => panic!("expression is not a Defs, but a {:?}", expr),
        }
    }

    #[test]
    fn recognize_tail_calls() {
        with_larger_debug_stack(|| {
            let src = indoc!(
                r#"
            g = \x ->
                case x is
                    0 -> 0
                    _ -> g (x - 1)

            h = \x ->
                case x is
                    0 -> 0
                    _ -> g (x - 1)

            p = \x ->
                case x is
                    0 -> 0
                    1 -> g (x - 1)
                    _ -> p (x - 1)

            0
        "#
            );
            let arena = Bump::new();
            let (actual, _output, _problems, _var_store, _vars, _constraint) =
                can_expr_with(&arena, "Blah", src, &ImMap::default());

            let detected = get_closure(&actual.value, 0);
            assert_eq!(detected, Recursive::TailRecursive);

            let detected = get_closure(&actual.value, 1);
            assert_eq!(detected, Recursive::NotRecursive);

            let detected = get_closure(&actual.value, 2);
            assert_eq!(detected, Recursive::TailRecursive);
        });
    }

    #[test]
    fn case_tail_call() {
        with_larger_debug_stack(|| {
            let src = indoc!(
                r#"
                g = \x ->
                    case x is
                        0 -> 0
                        _ -> g (x + 1)

                0
            "#
            );
            let arena = Bump::new();
            let (actual, _output, _problems, _var_store, _vars, _constraint) =
                can_expr_with(&arena, "Blah", src, &ImMap::default());

            let detected = get_closure(&actual.value, 0);
            assert_eq!(detected, Recursive::TailRecursive);
        });
    }

    #[test]
    fn immediate_tail_call() {
        let src = indoc!(
            r#"
            f = \x -> f x

            0
        "#
        );
        let arena = Bump::new();
        let (actual, _output, _problems, _var_store, _vars, _constraint) =
            can_expr_with(&arena, "Blah", src, &ImMap::default());

        let detected = get_closure(&actual.value, 0);
        assert_eq!(detected, Recursive::TailRecursive);
    }

    #[test]
    fn case_condition_is_no_tail_call() {
        // TODO when a case witn no branches parses, remove the pattern wildcard here
        let src = indoc!(
            r#"
            q = \x ->
                    case q x is
                        _ -> 0

            0
        "#
        );
        let arena = Bump::new();
        let (actual, _output, _problems, _var_store, _vars, _constraint) =
            can_expr_with(&arena, "Blah", src, &ImMap::default());

        let detected = get_closure(&actual.value, 0);
        assert_eq!(detected, Recursive::Recursive);
    }

    #[test]
    fn mutual_recursion() {
        with_larger_debug_stack(|| {
            // TODO when a case with no branches parses, remove the pattern wildcard here
            let src = indoc!(
                r#"
            q = \x ->
                    case x is
                        0 -> 0
                        _ -> p (x - 1)

            p = \x ->
                    case x is
                        0 -> 0
                        _ -> q (x - 1)

            0
        "#
            );
            let arena = Bump::new();
            let (actual, _output, _problems, _var_store, _vars, _constraint) =
                can_expr_with(&arena, "Blah", src, &ImMap::default());

            let detected = get_closure(&actual.value, 0);
            assert_eq!(detected, Recursive::Recursive);

            let detected = get_closure(&actual.value, 1);
            assert_eq!(detected, Recursive::Recursive);
        });
    }

    //#[test]
    //fn closing_over_locals() {
    //    // "local" should be used, because the closure used it.
    //    // However, "unused" should be unused.
    //    let (_, output, problems, _) = can_expr(indoc!(
    //        r#"
    //        local = 5
    //        unused = 6
    //        func = \arg -> arg + local

    //        3 + func 2
    //    "#
    //    ));

    //    assert_eq!(
    //        problems,
    //        vec![Problem::UnusedAssignment(loc(Ident::Unqualified(
    //            "unused".to_string()
    //        )))]
    //    );

    //    assert_eq!(
    //        output,
    //        Out {
    //            locals: vec!["func", "local"],
    //            globals: vec![],
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
    //        r#"
    //        local = 5
    //        unused = 6
    //        func = \arg -> arg + unused

    //        local
    //    "#
    //    ));

    //    assert_eq!(
    //        problems,
    //        vec![
    //            Problem::UnusedAssignment(loc(Ident::Unqualified("unused".to_string()))),
    //            Problem::UnusedAssignment(loc(Ident::Unqualified("func".to_string()))),
    //        ]
    //    );

    //    assert_eq!(
    //        output,
    //        Out {
    //            locals: vec!["local"],
    //            globals: vec![],
    //            calls: vec![],
    //            tail_call: None
    //        }
    //        .into()
    //    );
    //}

    //// UNRECOGNIZED

    //#[test]
    //fn basic_unrecognized_constant() {
    //    let (expr, output, problems, _) = can_expr(indoc!(
    //        r#"
    //        x
    //    "#
    //    ));

    //    assert_eq!(
    //        problems,
    //        vec![Problem::UnrecognizedConstant(loc(Ident::Unqualified(
    //            "x".to_string()
    //        )))]
    //    );

    //    assert_eq!(
    //        expr,
    //        UnrecognizedConstant(loc(Ident::Unqualified("x".to_string())))
    //    );

    //    assert_eq!(
    //        output,
    //        Out {
    //            locals: vec![],
    //            globals: vec![],
    //            calls: vec![],
    //            tail_call: None
    //        }
    //        .into()
    //    );
    //}

    //#[test]
    //fn complex_unrecognized_constant() {
    //    let (_, output, problems, _) = can_expr(indoc!(
    //        r#"
    //        a = 5
    //        b = 6

    //        a + b * z
    //    "#
    //    ));

    //    assert_eq!(
    //        problems,
    //        vec![Problem::UnrecognizedConstant(loc(Ident::Unqualified(
    //            "z".to_string()
    //        )))]
    //    );

    //    assert_eq!(
    //        output,
    //        Out {
    //            locals: vec!["a", "b"],
    //            globals: vec![],
    //            calls: vec![],
    //            tail_call: None
    //        }
    //        .into()
    //    );
    //}

    //// UNUSED

    //#[test]
    //fn mutual_unused_circular_vars() {
    //    // This should report that both a and b are unused, since the return expr never references them.
    //    // It should not report them as circular, since we haven't solved the halting problem here.
    //    let (_, output, problems, _) = can_expr(indoc!(
    //        r#"
    //        a = \arg -> if arg > 0 then b 7 else 0
    //        b = \arg -> if arg > 0 then a (arg - 1) else 0
    //        c = 5

    //        c
    //    "#
    //    ));

    //    assert_eq!(problems, vec![unused("a"), unused("b")]);

    //    assert_eq!(
    //        output,
    //        Out {
    //            locals: vec!["c"],
    //            globals: vec![],
    //            calls: vec![],
    //            tail_call: None
    //        }
    //        .into()
    //    );
    //}

    //#[test]
    //fn can_fibonacci() {
    //    let (_, output, problems, _) = can_expr(indoc!(
    //        r#"
    //        fibonacci = \num ->
    //            if num < 2 then
    //                num
    //            else
    //                fibonacci (num - 1) + fibonacci (num - 2)

    //        fibonacci 9
    //    "#
    //    ));

    //    assert_eq!(problems, vec![]);

    //    assert_eq!(
    //        output,
    //        Out {
    //            locals: vec!["fibonacci"],
    //            globals: vec![],
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
    //        r#"
    //        factorial = \num ->
    //            factorialHelp num 0

    //        factorialHelp = \num total ->
    //            if num == 0 then
    //                total
    //            else
    //                factorialHelp (num - 1) (total * num)

    //        factorial 9
    //    "#
    //    ));

    //    assert_eq!(problems, vec![]);

    //    assert_eq!(
    //        output,
    //        Out {
    //            locals: vec!["factorial", "factorialHelp"],
    //            globals: vec![],
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
    //        r#"
    //        a = \_ -> 42
    //        b = a

    //        b
    //    "#
    //    ));

    //    assert_eq!(problems, Vec::new());

    //    assert_eq!(
    //        output,
    //        Out {
    //            locals: vec!["a", "b"],
    //            globals: vec![],
    //            calls: vec![],
    //            tail_call: None
    //        }
    //        .into()
    //    );
    //}

    //// ASSIGNMENT REORDERING

    //#[test]
    //fn reorder_assignments() {
    //    let (expr, output, problems, _) = can_expr(indoc!(
    //        r#"
    //        increment = \arg -> arg + 1
    //        z = (increment 2) + y
    //        y = x + 1
    //        x = 9

    //        z * 3
    //    "#
    //    ));

    //    assert_eq!(problems, vec![]);

    //    assert_eq!(
    //        output,
    //        Out {
    //            locals: vec!["increment", "x", "y", "z"],
    //            globals: vec![],
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
    //        r#"
    //        z = func1 x
    //        x = 9
    //        y = func2 3
    //        func1 = \arg -> func2 arg + y
    //        func2 = \arg -> arg + x

    //        z
    //    "#
    //    ));

    //    assert_eq!(problems, vec![]);

    //    assert_eq!(
    //        output,
    //        Out {
    //            locals: vec!["func1", "func2", "x", "y", "z"],
    //            globals: vec![],
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

    //// CIRCULAR ASSIGNMENT

    //#[test]
    //fn circular_assignment() {
    //    let (_, _, problems, _) = can_expr(indoc!(
    //        r#"
    //        c = d + 3
    //        b = 2 + c
    //        d = a + 7
    //        a = b + 1

    //        2 + d
    //    "#
    //    ));

    //    assert_eq!(
    //        problems,
    //        vec![Problem::CircularAssignment(vec![
    //            // c should appear first because it's assigned first in the original expression.
    //            loc(unqualified("c")),
    //            loc(unqualified("d")),
    //            loc(unqualified("a")),
    //            loc(unqualified("b")),
    //        ])]
    //    );
    //}

    //#[test]
    //fn always_function() {
    //    // There was a bug where this reported UnusedArgument("val")
    //    // since it was used only in the returned function only.
    //    let (_, _, problems, _) = can_expr(indoc!(
    //        r#"
    //        \val -> \_ -> val
    //    "#
    //    ));

    //    assert_eq!(problems, vec![]);
    //}

    //// TODO verify that Apply handles output.references.calls correctly

    //// UNSUPPORTED PATTERNS

    //// TODO verify that in closures and assignments, you can't assign to int/string/underscore/etc

    //// OPERATOR PRECEDENCE

    //// fn parse_with_precedence(input: &str) -> Result<(Expr, &str), easy::Errors<char, &str, IndentablePosition>> {
    ////     parse_without_loc(input)
    ////         .map(|(expr, remaining)| (expr::apply_precedence_and_associativity(loc(expr)).unwrap().value, remaining))
    //// }

    //// #[test]
    //// fn two_operator_precedence() {
    ////     assert_eq!(
    ////         parse_with_precedence("x + y * 5"),
    ////         Ok((BinOp(
    ////                 loc_box(var("x")),
    ////                 loc(Plus),
    ////                 loc_box(
    ////                     BinOp(
    ////                         loc_box(var("y")),
    ////                         loc(Star),
    ////                         loc_box(Int(5))
    ////                     )
    ////                 ),
    ////             ),
    ////         ""))
    ////     );

    ////     assert_eq!(
    ////         parse_with_precedence("x * y + 5"),
    ////         Ok((BinOp(
    ////                 loc_box(
    ////                     BinOp(
    ////                         loc_box(var("x")),
    ////                         loc(Star),
    ////                         loc_box(var("y")),
    ////                     )
    ////                 ),
    ////                 loc(Plus),
    ////                 loc_box(Int(5))
    ////             ),
    ////         ""))
    ////     );
    //// }

    //// #[test]
    //// fn compare_and() {
    ////     assert_eq!(
    ////         parse_with_precedence("x > 1 || True"),
    ////         Ok((BinOp(
    ////                 loc_box(
    ////                     BinOp(
    ////                         loc_box(var("x")),
    ////                         loc(GreaterThan),
    ////                         loc_box(Int(1))
    ////                     )
    ////                 ),
    ////                 loc(Or),
    ////                 loc_box(ApplyVariant(vname("True"), None))
    ////             ),
    ////         ""))
    ////     );
    //// }

    //// HELPERS

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
    //// STRING LITERALS

    //
    // #[test]
    // fn string_with_valid_unicode_escapes() {
    //     expect_parsed_str("x\u{00A0}x", r#""x\u{00A0}x""#);
    //     expect_parsed_str("x\u{101010}x", r#""x\u{101010}x""#);
    // }

    // #[test]
    // fn string_with_too_large_unicode_escape() {
    //     // Should be too big - max size should be 10FFFF.
    //     // (Rust has this restriction. I assume it's a good idea.)
    //     assert_malformed_str(
    //         r#""abc\u{110000}def""#,
    //         vec![Located::new(0, 7, 0, 12, Problem::UnicodeCodePointTooLarge)],
    //     );
    // }

    // #[test]
    // fn string_with_no_unicode_digits() {
    //     // No digits specified
    //     assert_malformed_str(
    //         r#""blah\u{}foo""#,
    //         vec![Located::new(0, 5, 0, 8, Problem::NoUnicodeDigits)],
    //     );
    // }

    // #[test]
    // fn string_with_no_unicode_opening_brace() {
    //     // No opening curly brace. It can't be sure if the closing brace
    //     // was intended to be a closing brace for the unicode escape, so
    //     // report that there were no digits specified.
    //     assert_malformed_str(
    //         r#""abc\u00A0}def""#,
    //         vec![Located::new(0, 4, 0, 5, Problem::NoUnicodeDigits)],
    //     );
    // }

    // #[test]
    // fn string_with_no_unicode_closing_brace() {
    //     // No closing curly brace
    //     assert_malformed_str(
    //         r#""blah\u{stuff""#,
    //         vec![Located::new(0, 5, 0, 12, Problem::MalformedEscapedUnicode)],
    //     );
    // }

    // #[test]
    // fn string_with_no_unicode_braces() {
    //     // No curly braces
    //     assert_malformed_str(
    //         r#""zzzz\uzzzzz""#,
    //         vec![Located::new(0, 5, 0, 6, Problem::NoUnicodeDigits)],
    //     );
    // }

    // #[test]
    // fn string_with_interpolation_at_start() {
    //     let input = indoc!(
    //         r#"
    //              "\(abc)defg"
    //              "#
    //     );
    //     let (args, ret) = (vec![("", Located::new(0, 2, 0, 4, Var("abc")))], "defg");
    //     let arena = Bump::new();
    //     let actual = parse_with(&arena, input);

    //     assert_eq!(
    //         Ok(InterpolatedStr(&(arena.alloc_slice_clone(&args), ret))),
    //         actual
    //     );
    // }

    // #[test]
    // fn string_with_interpolation_at_end() {
    //     let input = indoc!(
    //         r#"
    //              "abcd\(efg)"
    //              "#
    //     );
    //     let (args, ret) = (vec![("abcd", Located::new(0, 6, 0, 8, Var("efg")))], "");
    //     let arena = Bump::new();
    //     let actual = parse_with(&arena, input);

    //     assert_eq!(
    //         Ok(InterpolatedStr(&(arena.alloc_slice_clone(&args), ret))),
    //         actual
    //     );
    // }

    // #[test]
    // fn string_with_interpolation_in_middle() {
    //     let input = indoc!(
    //         r#"
    //              "abc\(defg)hij"
    //              "#
    //     );
    //     let (args, ret) = (vec![("abc", Located::new(0, 5, 0, 8, Var("defg")))], "hij");
    //     let arena = Bump::new();
    //     let actual = parse_with(&arena, input);

    //     assert_eq!(
    //         Ok(InterpolatedStr(&(arena.alloc_slice_clone(&args), ret))),
    //         actual
    //     );
    // }

    // #[test]
    // fn string_with_two_interpolations_in_middle() {
    //     let input = indoc!(
    //         r#"
    //              "abc\(defg)hi\(jkl)mn"
    //              "#
    //     );
    //     let (args, ret) = (
    //         vec![
    //             ("abc", Located::new(0, 5, 0, 8, Var("defg"))),
    //             ("hi", Located::new(0, 14, 0, 16, Var("jkl"))),
    //         ],
    //         "mn",
    //     );
    //     let arena = Bump::new();
    //     let actual = parse_with(&arena, input);

    //     assert_eq!(
    //         Ok(InterpolatedStr(&(arena.alloc_slice_clone(&args), ret))),
    //         actual
    //     );
    // }

    // #[test]
    // fn string_with_four_interpolations() {
    //     let input = indoc!(
    //         r#"
    //              "\(abc)def\(ghi)jkl\(mno)pqrs\(tuv)"
    //              "#
    //     );
    //     let (args, ret) = (
    //         vec![
    //             ("", Located::new(0, 2, 0, 4, Var("abc"))),
    //             ("def", Located::new(0, 11, 0, 13, Var("ghi"))),
    //             ("jkl", Located::new(0, 20, 0, 22, Var("mno"))),
    //             ("pqrs", Located::new(0, 30, 0, 32, Var("tuv"))),
    //         ],
    //         "",
    //     );
    //     let arena = Bump::new();
    //     let actual = parse_with(&arena, input);

    //     assert_eq!(
    //         Ok(InterpolatedStr(&(arena.alloc_slice_clone(&args), ret))),
    //         actual
    //     );
    // }

    //     #[test]
    //     fn string_with_escaped_interpolation() {
    //         assert_parses_to(
    //             // This should NOT be string interpolation, because of the \\
    //             indoc!(
    //                 r#"
    //                 "abcd\\(efg)hij"
    //                 "#
    //             ),
    //             Str(r#"abcd\(efg)hij"#.into()),
    //         );
    //     }
    //

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
    //         expect_parsed_str(r#"x\x"#, r#""x\\x""#);
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
}
