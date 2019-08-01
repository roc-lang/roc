#[macro_use] extern crate pretty_assertions;
#[macro_use] extern crate indoc;
extern crate combine;

extern crate roc;

mod helpers;

#[cfg(test)]
mod test_canonicalize {
    use roc::canonicalize;
    use roc::canonicalize::{Expr, Output, Problem, Symbol, References, Procedure, Pattern};
    use roc::canonicalize::Expr::*;
    use roc::canonicalize::Pattern::*;
    use roc::expr::{Ident};
    use roc::expr;
    use roc::operator::Operator;
    use roc::region::{Located, Region};
    use roc::parse;
    use roc::collections::{ImSortedMap, ImSortedSet, MutSortedMap};
    use roc::parse_state::{IndentablePosition};
    use combine::{Parser, eof};
    use combine::stream::state::{State};
    use helpers::{loc, loc_box, empty_region, zero_loc_expr, mut_sorted_map_from_pairs};

    fn can_expr(expr_str: &str) -> (Expr, Output, Vec<Problem>, MutSortedMap<Symbol, Procedure>) {
        can_expr_with("testDecl", expr_str, &ImSortedMap::default(), &ImSortedMap::default())
    }

    fn can_expr_with(
        name: &str,
        expr_str: &str,
        declared_idents: &ImSortedMap<Ident, (Symbol, Region)>,
        declared_variants: &ImSortedMap<Symbol, Located<expr::VariantName>>,
    ) -> (Expr, Output, Vec<Problem>, MutSortedMap<Symbol, Procedure>) {
        let parse_state: State<&str, IndentablePosition> = State::with_positioner(expr_str, IndentablePosition::default());
        let expr = match parse::expr().skip(eof()).easy_parse(parse_state) {
            Ok((expr, state)) => {
                if !state.input.is_empty() {
                    panic!("There were unconsumed chars left over after parsing \"{}\" - the leftover string was: \"{}\"",
                        expr_str.to_string(), state.input.to_string())
                }

                expr
            },
            Err(errors) => {
                panic!("Parse error trying to parse \"{}\" - {}", expr_str.to_string(), errors.to_string())
            }
        };

        let home = "TestModule".to_string();
        let (loc_expr, output, problems, procedures) =
            canonicalize::canonicalize_declaration(home, name, loc(zero_loc_expr(expr)), declared_idents, declared_variants);

        (loc_expr.value, output, problems, procedures)
    }

    fn sym(name: &str) -> Symbol {
        Symbol::new("TestModule$testDecl$", name)
    }

    fn unqualified(string :&str) -> Ident {
        Ident::Unqualified(string.to_string())
    }

    fn unused(string: &str) -> Problem {
        Problem::UnusedAssignment(loc(unqualified(string)))
    }

    struct Out<'a> {
        locals: Vec<&'a str>,
        globals: Vec<&'a str>,
        variants: Vec<&'a str>,
        calls: Vec<&'a str>,
        tail_call: Option<&'a str>
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

            Output {references, tail_call}
        }
    }

    fn vec_to_set<'a>(vec: Vec<&'a str>) -> ImSortedSet<Symbol> {
        ImSortedSet::from(vec.into_iter().map(sym).collect::<Vec<_>>())
    }

    // BASIC CANONICALIZATION

    #[test]
    fn closure_args_are_not_locals() {
        // "arg" shouldn't make it into output.locals, because
        // it only exists in the closure's arguments.
        let (_, output, problems, procedures) = can_expr(indoc!(r#"
            func = \arg -> arg + 1

            3 + func 2
        "#));

        assert_eq!(problems, vec![]);

        assert_eq!(output, Out {
            locals: vec!["func"],
            globals: vec![],
            variants: vec![],
            calls: vec!["func"],
            tail_call: None
        }.into());

        assert_eq!(procedures,
            mut_sorted_map_from_pairs(vec![(sym("func"),
                Procedure {
                    name: Some("func".to_string()),
                    is_self_tail_recursive: false,
                    definition: empty_region(),
                    args: vec![Pattern::Identifier(sym("arg"))],
                    body: Expr::Operator(
                        loc_box(Expr::Var(sym("arg"))),
                        loc(Operator::Plus),
                        loc_box(Expr::Int(1))
                    ),
                    references: References {
                        locals: vec_to_set(vec![]),
                        globals: vec_to_set(vec![]),
                        variants: vec_to_set(vec![]),
                        calls: vec_to_set(vec![]),
                    }
            })])
        );
    }

    #[test]
    fn closing_over_locals() {
        // "local" should be used, because the closure used it.
        // However, "unused" should be unused.
        let (_, output, problems, _) = can_expr(indoc!(r#"
            local = 5
            unused = 6
            func = \arg -> arg + local

            3 + func 2
        "#));

        assert_eq!(problems, vec![
            Problem::UnusedAssignment(loc(Ident::Unqualified("unused".to_string())))
        ]);

        assert_eq!(output, Out {
            locals: vec!["func", "local"],
            globals: vec![],
            variants: vec![],
            calls: vec!["func"],
            tail_call: None
        }.into());
    }

    #[test]
    fn unused_closure() {
        // "unused" should be unused because it's in func, which is unused.
        let (_, output, problems, _) = can_expr(indoc!(r#"
            local = 5
            unused = 6
            func = \arg -> arg + unused

            local
        "#));

        assert_eq!(problems, vec![
            Problem::UnusedAssignment(loc(Ident::Unqualified("func".to_string()))),
            Problem::UnusedAssignment(loc(Ident::Unqualified("unused".to_string()))),
        ]);

        assert_eq!(output, Out {
            locals: vec!["local"],
            globals: vec![],
            variants: vec![],
            calls: vec![],
            tail_call: None
        }.into());
    }

    // UNRECOGNIZED

    #[test]
    fn basic_unrecognized_constant() {
        let (expr, output, problems, _) = can_expr(indoc!(r#"
            x
        "#));

        assert_eq!(problems, vec![
            Problem::UnrecognizedConstant(loc(Ident::Unqualified("x".to_string())))
        ]);

        assert_eq!(expr,
            UnrecognizedConstant(loc(Ident::Unqualified("x".to_string())))
        );

        assert_eq!(output, Out {
            locals: vec![],
            globals: vec![],
            variants: vec![],
            calls: vec![],
            tail_call: None
        }.into());
    }

    #[test]
    fn complex_unrecognized_constant() {
        let (_, output, problems, _) = can_expr(indoc!(r#"
            a = 5
            b = 6

            a + b * z
        "#));

        assert_eq!(problems, vec![
            Problem::UnrecognizedConstant(loc(Ident::Unqualified("z".to_string())))
        ]);

        assert_eq!(output, Out {
            locals: vec!["a", "b"],
            globals: vec![],
            variants: vec![],
            calls: vec![],
            tail_call: None
        }.into());
    }

    // UNUSED

    #[test]
    fn mutual_unused_circular_vars() {
        // This should report that both a and b are unused, since the return expr never references them.
        // It should not report them as circular, since we haven't solved the halting problem here.
        let (_, output, problems, _) = can_expr(indoc!(r#"
            a = \arg -> if arg > 0 then b 7 else 0
            b = \arg -> if arg > 0 then a (arg - 1) else 0
            c = 5

            c
        "#));

        assert_eq!(problems, vec![unused("b"), unused("a")]);

        assert_eq!(output, Out {
            locals: vec!["c"],
            globals: vec![],
            variants: vec![],
            calls: vec![],
            tail_call: None
        }.into());
    }

    #[test]
    fn can_fibonacci() {
        let (_, output, problems, _) = can_expr(indoc!(r#"
            fibonacci = \num ->
                if num < 2 then
                    num
                else
                    fibonacci (num - 1) + fibonacci (num - 2)

            fibonacci 9
        "#));

        assert_eq!(problems, vec![]);

        assert_eq!(output, Out {
            locals: vec!["fibonacci"],
            globals: vec![],
            variants: vec![],
            calls: vec!["fibonacci"],
            tail_call: Some("fibonacci")
        }.into());
    }

    // ASSIGNMENT REORDERING

    #[test]
    fn reorder_assignments() {
        let (expr, output, problems, _) = can_expr(indoc!(r#"
            func = \arg -> arg + y
            z = func 2
            y = x + 1
            x = 9

            z * 3
        "#));

        assert_eq!(problems, vec![]);

        assert_eq!(output, Out {
            locals: vec!["func", "x", "y", "z"],
            globals: vec![],
            variants: vec![],
            calls: vec!["func"],
            tail_call: None
        }.into());

        // This should get reordered to the following, so that in code gen
        // everything will have been set before it gets read.
        assert_eq!(expr, can_expr(indoc!(r#"
            x = 9
            y = x + 1
            func = \arg -> arg + y
            z = func 2

            z * 3
        "#)).0);
    }

    #[test]
    fn reorder_closed_over_assignments() {
        let (expr, output, problems, _) = can_expr(indoc!(r#"
            z = func1 x
            x = 9
            y = func2 3
            func1 = \arg -> func2 arg + y
            func2 = \arg -> arg + x

            z
        "#));

        assert_eq!(problems, vec![]);

        assert_eq!(output, Out {
            locals: vec!["func1", "func2", "x", "y", "z"],
            globals: vec![],
            variants: vec![],
            calls: vec!["func1", "func2"],
            tail_call: None
        }.into());

        // This should get reordered to the following, so that in code gen
        // everything will have been set before it gets read.
        // (The order of the function definitions doesn't matter.)
        assert_assignment_order(expr,
            vec!["func1", "x", "z", "func2", "y"],
        );
    }

    fn assert_assignment_order(expr: Expr, expected_strings: Vec<&str>) {
        match expr {
            Assign(assignments, _) => {
                let expected_symbols: Vec<Symbol> = expected_strings.into_iter().map(sym).collect();
                let actual_symbols: Vec<Symbol> = assignments.into_iter().map(|(pattern, _)| {
                    match pattern {
                        Identifier(symbol) => {
                            symbol
                        },
                        _ => {
                            panic!("Called assert_assignment_order passing an Assign expr with non-Identifier patterns!");
                        }
                    }
                }).collect();

                assert_eq!(actual_symbols, expected_symbols);
            }
            _ => {
                panic!("Called assert_assignment_order passing a non-Assign expr!");
            }
        }
    }


    // CIRCULAR ASSIGNMENT

    #[test]
    fn circular_assignment() {
        let (_, _, problems, _) = can_expr(indoc!(r#"
            a = b + 1
            b = 2 * c
            c = a 7

            2 + c
        "#));

        assert_eq!(problems, vec![
            Problem::CircularAssignment(vec![
                loc(unqualified("c")),
                loc(unqualified("b")),
                loc(unqualified("a")),
            ])
        ]);

        panic!("TODO strongly_connected_component doesn't sort these, but we want them sorted!");
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
}