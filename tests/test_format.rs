#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;
extern crate bumpalo;
extern crate combine; // OBSOLETE
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_formatter {
    use bumpalo::Bump;
    use roc::parse;
    use roc::parse::ast::{Attempting, Expr};
    use roc::parse::parser::{Fail, Parser, State};

    fn parse_with<'a>(arena: &'a Bump, input: &'a str) -> Result<Expr<'a>, Fail> {
        let state = State::new(&input, Attempting::Module);
        let parser = parse::expr();
        let answer = parser.parse(&arena, state);

        answer.map(|(expr, _)| expr).map_err(|(fail, _)| fail)
    }

    fn assert_formats_to(input: &str, expected: &str) {
        let arena = Bump::new();
        let input = input.trim_end();
        let expected = expected.trim_end();

        match parse_with(&arena, input) {
            Ok(actual) => assert_eq!(format!("{}", actual), expected),
            Err(error) => panic!("Unexpected parse failure when parsing this for formatting:\n\n{:?}\n\nParse error was:\n\n{:?}\n\n", input, error)
        }
    }

    fn assert_formats_same(input: &str) {
        assert_formats_to(input, input);
    }

    // STRING LITERALS

    #[test]
    fn empty_string() {
        assert_formats_same(indoc!(
            r#"
            ""
            "#
        ));
    }
}
