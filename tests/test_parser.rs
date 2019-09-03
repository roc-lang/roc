#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate combine; // OBSOLETE
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_parser {
    use bumpalo::Bump;
    use roc::parser::Expr::{self, *};
    use roc::parser::{Attempting, Parser, Problem, State};

    fn assert_parses_to<'a>(input: &'a str, expected_expr: Expr<'a>) {
        assert_parses_to_problems(input, expected_expr, Vec::new())
    }

    fn assert_parses_to_problems<'a>(
        input: &'a str,
        expected_expr: Expr<'a>,
        expected_problems: Vec<Problem>,
    ) {
        let mut problems = Vec::new();

        {
            let state = State::from_input(&input);
            let arena = Bump::new();
            let attempting = Attempting::Expression;
            let parser = roc::parser::expr();
            let answer = parser.parse(&arena, &state, &mut problems, attempting);
            let actual = answer
                .map(|(_, expr)| expr)
                .map_err(|(_, attempting)| attempting);

            assert_eq!(Ok(expected_expr), actual);
        }

        let mut actual_problems: Vec<Problem> = Vec::new();

        for loc_problem in problems {
            actual_problems.push(loc_problem.value);
        }

        assert_eq!(expected_problems, actual_problems);
    }

    #[test]
    fn empty_list() {
        assert_parses_to(
            indoc!(
                r#"
                ""
                "#
            ),
            EmptyStr,
        );
    }

    #[test]
    fn one_char_list() {
        assert_parses_to(
            indoc!(
                r#"
                "x"
                "#
            ),
            Str("x".into()),
        );
    }

    #[test]
    fn multi_char_list() {
        assert_parses_to(
            indoc!(
                r#"
                "foo"
                "#
            ),
            Str("foo".into()),
        );
    }
}
