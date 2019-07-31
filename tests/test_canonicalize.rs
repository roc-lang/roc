#[macro_use] extern crate pretty_assertions;
#[macro_use] extern crate indoc;
extern crate combine;

extern crate roc;

mod helpers;

#[cfg(test)]
mod test_canonicalize {
    use roc::canonicalize;
    use roc::canonicalize::{Expr, Output, Problem, Symbol, References};
    use roc::canonicalize::Expr::*;
    use roc::expr::{Ident};
    use roc::expr;
    use roc::region::{Located, Region};
    use roc::parse;
    use roc::collections::{ImMap, ImSet};
    use roc::parse_state::{IndentablePosition};
    use combine::{Parser, eof};
    use combine::stream::state::{State};
    use helpers::{loc, zero_loc_expr};

    fn can_expr(expr_str: &str) -> (Expr, Output, Vec<Problem>) {
        can_expr_with("testDecl", expr_str, &ImMap::default(), &ImMap::default())
    }

    fn can_expr_with(
        name: &str,
        expr_str: &str,
        declared_idents: &ImMap<Ident, (Symbol, Region)>,
        declared_variants: &ImMap<Symbol, Located<expr::VariantName>>,
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

        let home = "TestModule".to_string();
        let (loc_expr, output, problems) =
            canonicalize::canonicalize_declaration(home, name, loc(zero_loc_expr(expr)), declared_idents, declared_variants);

        (loc_expr.value, output, problems)
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
        tail_call: Option<&'a str>
    }

    impl<'a> Into<Output> for Out<'a> {
        fn into(self) -> Output {
            fn vec_to_set<'b>(vec: Vec<&'b str>) -> ImSet<Symbol> {
                ImSet::from(vec.into_iter().map(sym).collect::<Vec<_>>())
            }

            let references = References {
                locals: vec_to_set(self.locals),
                globals: vec_to_set(self.globals),
                variants: vec_to_set(self.variants)
            };

            let tail_call = self.tail_call.map(sym);

            Output {references, tail_call}
        }
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
            UnrecognizedConstant(loc(Ident::Unqualified("x".to_string())))
        );

        assert_eq!(output, Out {
            locals: vec![],
            globals: vec![],
            variants: vec![],
            tail_call: None
        }.into());
    }

    #[test]
    fn complex_unrecognized_constant() {
        let (_, output, problems) = can_expr(indoc!(r#"
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
            tail_call: None
        }.into());
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

        assert_eq!(output, Out {
            locals: vec!["c"],
            globals: vec![],
            variants: vec![],
            tail_call: None
        }.into());
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

        assert_eq!(output, Out {
            locals: vec!["num", "fibonacci"],
            globals: vec![],
            variants: vec![],
            tail_call: Some("fibonacci")
        }.into());
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