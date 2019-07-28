#[macro_use] extern crate pretty_assertions;
#[macro_use] extern crate indoc;
extern crate combine;

extern crate roc;

mod helpers;

#[cfg(test)]
mod test_canonicalize {
    use roc::canonicalize;
    use roc::canonicalize::{Expr, Output, Problem, Resolved, LocalSymbol, Symbol};
    use roc::canonicalize::Expr::*;
    use roc::canonicalize::Pattern::*;
    use roc::expr::{Path, Ident};
    use roc::operator::Operator::*;
    use roc::expr;
    use roc::region::Located;
    use roc::parse;
    use roc::collections::{ImMap, ImSet};
    use roc::parse_state::{IndentablePosition};
    use combine::{Parser, eof};
    use combine::stream::state::{State};
    use helpers::{loc, loc_box, zero_loc_expr};

    fn can_expr(expr_str: &str) -> (Expr, Output, Vec<Problem>) {
        can_expr_with(expr_str, &ImMap::default(), &ImMap::default())
    }

    fn can_expr_with(
        expr_str: &str,
        declared_idents: &ImMap<Ident, Located<expr::Ident>>,
        declared_variants: &ImMap<(Path, String), Located<expr::VariantName>>,
    ) -> (Expr, Output, Vec<Problem>) {
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

        let home = Path::new("TestModule".to_string());
        let (loc_expr, output, problems) =
            canonicalize::canonicalize_declaration(home, loc(zero_loc_expr(expr)), declared_idents, declared_variants);

        (loc_expr.value, output, problems)
    }

    fn recognized_local_sym(string: &str) -> Resolved<Symbol> {
        Resolved::Recognized(local_sym(string))
    }

    fn local_sym(string: &str) -> Symbol {
        Symbol::Local(local(string))
    }

    fn local(string: &str) -> LocalSymbol {
        LocalSymbol::new(string.to_string())
    }

    fn unqualified(string :&str) -> Ident {
        Ident::Unqualified(string.to_string())
    }

    fn unused(string: &str) -> Problem {
        Problem::UnusedAssignment(loc(unqualified(string)))
    }

    fn check_output(
        output: Output,
        applied_variants: Vec<(Path, &str)>,
        referenced_idents: Vec<(Option<Path>, &str)>,
        tail_call: Option<Symbol>
    ) {
        assert_eq!(
            output,
            Output {
                referenced_idents:
                    ImSet::from(
                        referenced_idents.into_iter().map(|(opt_path, str_ref)|
                            match opt_path {
                                Some(path) => Ident::Qualified(path, str_ref.to_string()),
                                None => Ident::Unqualified(str_ref.to_string())
                            }
                        ).collect::<Vec<_>>()
                    ),
                applied_variants:
                    ImSet::from(
                        applied_variants.into_iter().map(|(path, str_ref)|
                            (path, str_ref.to_string()),
                        ).collect::<Vec<_>>()),
                tail_call
            }
        );
    }

    #[test]
    fn basic_unrecognized_constant() {
        let (expr, output, problems) = can_expr(indoc!(r#"
            x
        "#));

        assert_eq!(problems, vec![
            Problem::UnrecognizedConstant(loc(Ident::Unqualified("x".to_string())))
        ]);

        assert_eq!(expr,
            Var(Resolved::UnrecognizedConstant(loc(Ident::Unqualified("x".to_string()))))
        );

        check_output(output, vec![], vec![], None);
    }

    #[test]
    fn complex_unrecognized_constant() {
        let (expr, output, problems) = can_expr(indoc!(r#"
            a = 5
            b = 6

            a + b * z
        "#));

        assert_eq!(problems, vec![
            Problem::UnrecognizedConstant(loc(Ident::Unqualified("z".to_string())))
        ]);

        assert_eq!(expr,
            Assign(
                vec![
                    (loc(Identifier(local("a"))), loc(Int(5))),
                    (loc(Identifier(local("b"))), loc(Int(6))),
                ],
                loc_box(Operator(
                    loc_box(Var(recognized_local_sym("a"))),
                    loc(Plus),
                    loc_box(Operator(
                        loc_box(Var(recognized_local_sym("b"))),
                        loc(Star),
                        loc_box(Var(Resolved::UnrecognizedConstant(loc(Ident::Unqualified("z".to_string())))))
                    )),
                ))
            )
        );

        check_output(output, vec![], vec![(None, "a"), (None, "b")], None);
    }

    #[test]
    fn mutual_unused_vars() {
        // This should report that both a and b are unused, since the return expr never references them.
        let (_, output, problems) = can_expr(indoc!(r#"
            a = \_ -> b + 1
            b = \_ -> a + 1
            c = 5

            c
        "#));

        assert_eq!(problems, vec![unused("b"), unused("a")]);

        check_output(output, vec![], vec![(None, "c")], None);
    }


    #[test]
    fn can_fibonacci() {
        let (_, output, problems) = can_expr(indoc!(r#"
            fibonacci = \num ->
                if num < 2 then
                    num
                else
                    fibonacci (num - 1) + fibonacci (num - 2)

            fibonacci 9
        "#));

        assert_eq!(problems, vec![]);

        check_output(output,
            vec![],
            vec![(None, "num"), (None, "fibonacci")],
            Some(local_sym("fibonacci"))
        );
    }

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