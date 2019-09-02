#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate combine;

extern crate roc;

mod helpers;

#[cfg(test)]
mod test_canonicalize {
    use helpers::{
        empty_region, loc, loc_box, mut_map_from_pairs, parse_without_loc, zero_loc_expr,
    };
    use roc::canonicalize;
    use roc::canonicalize::Expr::*;
    use roc::canonicalize::Pattern::*;
    use roc::canonicalize::{Expr, Output, Pattern, Problem, Procedure, References, Symbol};
    use roc::collections::{ImMap, ImSet, MutMap};
    use roc::expr::{Ident, VariantName};
    use roc::operator::Operator;
    use roc::region::{Located, Region};

    fn can_expr(expr_str: &str) -> (Expr, Output, Vec<Problem>, MutMap<Symbol, Procedure>) {
        can_expr_with("blah", expr_str, &ImMap::default(), &ImMap::default())
    }

    fn can_expr_with(
        name: &str,
        expr_str: &str,
        declared_idents: &ImMap<Ident, (Symbol, Region)>,
        declared_variants: &ImMap<Symbol, Located<VariantName>>,
    ) -> (Expr, Output, Vec<Problem>, MutMap<Symbol, Procedure>) {
        let (expr, unparsed) = parse_without_loc(expr_str).unwrap_or_else(|errors| {
            panic!(
                "Parse error trying to parse \"{}\" - {}",
                expr_str.to_string(),
                errors.to_string()
            )
        });

        assert_eq!(unparsed, "".to_string());

        let home = "Test".to_string();
        let (loc_expr, output, problems, procedures) = canonicalize::canonicalize_declaration(
            home,
            name,
            loc(zero_loc_expr(expr)),
            declared_idents,
            declared_variants,
        );

        (loc_expr.value, output, problems, procedures)
    }

    fn sym(name: &str) -> Symbol {
        Symbol::new("Test$blah$", name)
    }

    fn unqualified(string: &str) -> Ident {
        Ident::Unqualified(string.to_string())
    }

    fn unqualifieds(strings: Vec<&str>) -> Vec<Ident> {
        strings.into_iter().map(unqualified).collect()
    }

    fn loc_unqualifieds(strings: Vec<&str>) -> Vec<Located<Ident>> {
        strings
            .into_iter()
            .map(|string| loc(unqualified(string)))
            .collect()
    }

    fn unused(string: &str) -> Problem {
        Problem::UnusedAssignment(loc(unqualified(string)))
    }

    struct Out<'a> {
        locals: Vec<&'a str>,
        globals: Vec<&'a str>,
        variants: Vec<&'a str>,
        calls: Vec<&'a str>,
        tail_call: Option<&'a str>,
    }

    impl<'a> Into<Output> for Out<'a> {
        fn into(self) -> Output {
            let references = References {
                locals: vec_to_set(self.locals),
                globals: vec_to_set(self.globals),
                variants: vec_to_set(self.variants),
                calls: vec_to_set(self.calls),
            };

            let tail_call = self.tail_call.map(sym);

            Output {
                references,
                tail_call,
            }
        }
    }

    fn vec_to_set<'a>(vec: Vec<&'a str>) -> ImSet<Symbol> {
        ImSet::from(vec.into_iter().map(sym).collect::<Vec<_>>())
    }

    // BASIC CANONICALIZATION

    #[test]
    fn closure_args_are_not_locals() {
        // "arg" shouldn't make it into output.locals, because
        // it only exists in the closure's arguments.
        let (_, output, problems, procedures) = can_expr(indoc!(
            r#"
            func = \arg -> arg + 1

            3 + func 2
        "#
        ));

        assert_eq!(problems, vec![]);

        assert_eq!(
            output,
            Out {
                locals: vec!["func"],
                globals: vec![],
                variants: vec![],
                calls: vec!["func"],
                tail_call: None
            }
            .into()
        );

        assert_eq!(
            procedures,
            mut_map_from_pairs(vec![(
                sym("func"),
                Procedure {
                    name: Some("func".to_string()),
                    is_self_tail_recursive: false,
                    definition: empty_region(),
                    args: vec![loc(Pattern::Identifier(sym("arg")))],
                    body: loc(Expr::Operator(
                        loc_box(Expr::Var(sym("arg"))),
                        loc(Operator::Plus),
                        loc_box(Expr::Int(1))
                    )),
                    references: References {
                        locals: vec_to_set(vec![]),
                        globals: vec_to_set(vec![]),
                        variants: vec_to_set(vec![]),
                        calls: vec_to_set(vec![]),
                    }
                }
            )])
        );
    }

    #[test]
    fn closing_over_locals() {
        // "local" should be used, because the closure used it.
        // However, "unused" should be unused.
        let (_, output, problems, _) = can_expr(indoc!(
            r#"
            local = 5
            unused = 6
            func = \arg -> arg + local

            3 + func 2
        "#
        ));

        assert_eq!(
            problems,
            vec![Problem::UnusedAssignment(loc(Ident::Unqualified(
                "unused".to_string()
            )))]
        );

        assert_eq!(
            output,
            Out {
                locals: vec!["func", "local"],
                globals: vec![],
                variants: vec![],
                calls: vec!["func"],
                tail_call: None
            }
            .into()
        );
    }

    #[test]
    fn unused_closure() {
        // "unused" should be unused because it's in func, which is unused.
        let (_, output, problems, _) = can_expr(indoc!(
            r#"
            local = 5
            unused = 6
            func = \arg -> arg + unused

            local
        "#
        ));

        assert_eq!(
            problems,
            vec![
                Problem::UnusedAssignment(loc(Ident::Unqualified("unused".to_string()))),
                Problem::UnusedAssignment(loc(Ident::Unqualified("func".to_string()))),
            ]
        );

        assert_eq!(
            output,
            Out {
                locals: vec!["local"],
                globals: vec![],
                variants: vec![],
                calls: vec![],
                tail_call: None
            }
            .into()
        );
    }

    // UNRECOGNIZED

    #[test]
    fn basic_unrecognized_constant() {
        let (expr, output, problems, _) = can_expr(indoc!(
            r#"
            x
        "#
        ));

        assert_eq!(
            problems,
            vec![Problem::UnrecognizedConstant(loc(Ident::Unqualified(
                "x".to_string()
            )))]
        );

        assert_eq!(
            expr,
            UnrecognizedConstant(loc(Ident::Unqualified("x".to_string())))
        );

        assert_eq!(
            output,
            Out {
                locals: vec![],
                globals: vec![],
                variants: vec![],
                calls: vec![],
                tail_call: None
            }
            .into()
        );
    }

    #[test]
    fn complex_unrecognized_constant() {
        let (_, output, problems, _) = can_expr(indoc!(
            r#"
            a = 5
            b = 6

            a + b * z
        "#
        ));

        assert_eq!(
            problems,
            vec![Problem::UnrecognizedConstant(loc(Ident::Unqualified(
                "z".to_string()
            )))]
        );

        assert_eq!(
            output,
            Out {
                locals: vec!["a", "b"],
                globals: vec![],
                variants: vec![],
                calls: vec![],
                tail_call: None
            }
            .into()
        );
    }

    // UNUSED

    #[test]
    fn mutual_unused_circular_vars() {
        // This should report that both a and b are unused, since the return expr never references them.
        // It should not report them as circular, since we haven't solved the halting problem here.
        let (_, output, problems, _) = can_expr(indoc!(
            r#"
            a = \arg -> if arg > 0 then b 7 else 0
            b = \arg -> if arg > 0 then a (arg - 1) else 0
            c = 5

            c
        "#
        ));

        assert_eq!(problems, vec![unused("a"), unused("b")]);

        assert_eq!(
            output,
            Out {
                locals: vec!["c"],
                globals: vec![],
                variants: vec![],
                calls: vec![],
                tail_call: None
            }
            .into()
        );
    }

    #[test]
    fn can_fibonacci() {
        let (_, output, problems, _) = can_expr(indoc!(
            r#"
            fibonacci = \num ->
                if num < 2 then
                    num
                else
                    fibonacci (num - 1) + fibonacci (num - 2)

            fibonacci 9
        "#
        ));

        assert_eq!(problems, vec![]);

        assert_eq!(
            output,
            Out {
                locals: vec!["fibonacci"],
                globals: vec![],
                variants: vec![],
                calls: vec!["fibonacci"],
                tail_call: None
            }
            .into()
        );
    }

    #[test]
    fn can_tail_call() {
        // TODO check the global params - make sure this
        // is considered a tail call, even though it only
        // calls itself from one branch!
        let (_, output, problems, _) = can_expr(indoc!(
            r#"
            factorial = \num ->
                factorialHelp num 0

            factorialHelp = \num total ->
                if num == 0 then
                    total
                else
                    factorialHelp (num - 1) (total * num)

            factorial 9
        "#
        ));

        assert_eq!(problems, vec![]);

        assert_eq!(
            output,
            Out {
                locals: vec!["factorial", "factorialHelp"],
                globals: vec![],
                variants: vec![],
                calls: vec!["factorial", "factorialHelp"],
                tail_call: None
            }
            .into()
        );
    }

    #[test]
    fn transitively_used_function() {
        // This should report that neither a nor b are unused,
        // since if you never call a function but do return it, that's okay!
        let (_, output, problems, _) = can_expr(indoc!(
            r#"
            a = \_ -> 42
            b = a

            b
        "#
        ));

        assert_eq!(problems, Vec::new());

        assert_eq!(
            output,
            Out {
                locals: vec!["a", "b"],
                globals: vec![],
                variants: vec![],
                calls: vec![],
                tail_call: None
            }
            .into()
        );
    }

    // ASSIGNMENT REORDERING

    #[test]
    fn reorder_assignments() {
        let (expr, output, problems, _) = can_expr(indoc!(
            r#"
            increment = \arg -> arg + 1
            z = (increment 2) + y
            y = x + 1
            x = 9

            z * 3
        "#
        ));

        assert_eq!(problems, vec![]);

        assert_eq!(
            output,
            Out {
                locals: vec!["increment", "x", "y", "z"],
                globals: vec![],
                variants: vec![],
                calls: vec!["increment"],
                tail_call: None
            }
            .into()
        );

        let symbols = assigned_symbols(expr);

        // In code gen, for everything to have been set before it gets read,
        // the following must be true about when things are assigned:
        //
        // x must be assigned before y
        // y must be assigned before z
        //
        // The order of the increment function doesn't matter.
        assert_before("x", "y", &symbols);
        assert_before("y", "z", &symbols);
    }

    #[test]
    fn reorder_closed_over_assignments() {
        let (expr, output, problems, _) = can_expr(indoc!(
            r#"
            z = func1 x
            x = 9
            y = func2 3
            func1 = \arg -> func2 arg + y
            func2 = \arg -> arg + x

            z
        "#
        ));

        assert_eq!(problems, vec![]);

        assert_eq!(
            output,
            Out {
                locals: vec!["func1", "func2", "x", "y", "z"],
                globals: vec![],
                variants: vec![],
                calls: vec!["func1", "func2"],
                tail_call: None
            }
            .into()
        );

        let symbols = assigned_symbols(expr);

        // In code gen, for everything to have been set before it gets read,
        // the following must be true about when things are assigned:
        //
        // x and func2 must be assigned (in either order) before y
        // y and func1 must be assigned (in either order) before z
        assert_before("x", "y", &symbols);
        assert_before("func2", "y", &symbols);

        assert_before("func1", "z", &symbols);
        assert_before("y", "z", &symbols);
    }

    fn assert_before(before: &str, after: &str, symbols: &Vec<Symbol>) {
        assert_ne!(before, after);

        let before_symbol = sym(before);
        let after_symbol = sym(after);
        let before_index = symbols
            .iter()
            .position(|symbol| symbol == &before_symbol)
            .unwrap_or_else(|| {
                panic!(
                    "error in assert_before({:?}, {:?}): {:?} could not be found in {:?}",
                    before,
                    after,
                    sym(before),
                    symbols
                )
            });
        let after_index = symbols
            .iter()
            .position(|symbol| symbol == &after_symbol)
            .unwrap_or_else(|| {
                panic!(
                    "error in assert_before({:?}, {:?}): {:?} could not be found in {:?}",
                    before,
                    after,
                    sym(after),
                    symbols
                )
            });

        if before_index == after_index {
            panic!(
                "error in assert_before({:?}, {:?}): both were at index {} in {:?}",
                before, after, after_index, symbols
            );
        } else if before_index > after_index {
            panic!("error in assert_before: {:?} appeared *after* {:?} (not before, as expected) in {:?}", before, after, symbols);
        }
    }

    fn assigned_symbols(expr: Expr) -> Vec<Symbol> {
        match expr {
            Assign(assignments, _) => {
                assignments.into_iter().map(|(pattern, _)| {
                    match pattern.value {
                        Identifier(symbol) => {
                            symbol
                        },
                        _ => {
                            panic!("Called assigned_symbols passing an Assign expr with non-Identifier patterns!");
                        }
                    }
                }).collect()
            },
            _ => {
                panic!("Called assigned_symbols passing a non-Assign expr!");
            }
        }
    }

    // CIRCULAR ASSIGNMENT

    #[test]
    fn circular_assignment() {
        let (_, _, problems, _) = can_expr(indoc!(
            r#"
            c = d + 3
            b = 2 + c
            d = a + 7
            a = b + 1

            2 + d
        "#
        ));

        assert_eq!(
            problems,
            vec![Problem::CircularAssignment(vec![
                // c should appear first because it's assigned first in the original expression.
                loc(unqualified("c")),
                loc(unqualified("d")),
                loc(unqualified("a")),
                loc(unqualified("b")),
            ])]
        );
    }

    #[test]
    fn always_function() {
        // There was a bug where this reported UnusedArgument("val")
        // since it was used only in the returned function only.
        let (_, _, problems, _) = can_expr(indoc!(
            r#"
            \val -> \_ -> val
        "#
        ));

        assert_eq!(problems, vec![]);
    }

    // TODO verify that Apply handles output.references.calls correctly

    // UNSUPPORTED PATTERNS

    // TODO verify that in closures and assignments, you can't assign to int/string/underscore/etc

    // OPERATOR PRECEDENCE

    // fn parse_with_precedence(input: &str) -> Result<(Expr, &str), easy::Errors<char, &str, IndentablePosition>> {
    //     parse_without_loc(input)
    //         .map(|(expr, remaining)| (expr::apply_precedence_and_associativity(loc(expr)).unwrap().value, remaining))
    // }

    // #[test]
    // fn two_operator_precedence() {
    //     assert_eq!(
    //         parse_with_precedence("x + y * 5"),
    //         Ok((Operator(
    //                 loc_box(var("x")),
    //                 loc(Plus),
    //                 loc_box(
    //                     Operator(
    //                         loc_box(var("y")),
    //                         loc(Star),
    //                         loc_box(Int(5))
    //                     )
    //                 ),
    //             ),
    //         ""))
    //     );

    //     assert_eq!(
    //         parse_with_precedence("x * y + 5"),
    //         Ok((Operator(
    //                 loc_box(
    //                     Operator(
    //                         loc_box(var("x")),
    //                         loc(Star),
    //                         loc_box(var("y")),
    //                     )
    //                 ),
    //                 loc(Plus),
    //                 loc_box(Int(5))
    //             ),
    //         ""))
    //     );
    // }

    // #[test]
    // fn compare_and() {
    //     assert_eq!(
    //         parse_with_precedence("x > 1 || True"),
    //         Ok((Operator(
    //                 loc_box(
    //                     Operator(
    //                         loc_box(var("x")),
    //                         loc(GreaterThan),
    //                         loc_box(Int(1))
    //                     )
    //                 ),
    //                 loc(Or),
    //                 loc_box(ApplyVariant(vname("True"), None))
    //             ),
    //         ""))
    //     );
    // }

    // HELPERS

    #[test]
    fn sort_cyclic_idents() {
        let assigned_idents = unqualifieds(vec!["blah", "c", "b", "d", "a"]);

        assert_eq!(
            canonicalize::sort_cyclic_idents(
                loc_unqualifieds(vec!["a", "b", "c", "d"]),
                &mut assigned_idents.iter()
            ),
            loc_unqualifieds(vec!["c", "d", "a", "b"])
        );
    }
}
