#[macro_use] extern crate maplit;
#[macro_use] extern crate pretty_assertions;

extern crate roc;

#[cfg(test)]
mod interpreter_tests {
    use roc::interpret::literal_to_string;
    use roc::unify::Expr::Literal;
    use roc::unify::Literal::{String, Record};

    #[test]
    fn test_string_literal() {
        let expected = "\"hi!\"";
        let literal = String("hi!");

        assert_eq!(expected, literal_to_string(literal));
    }

    #[test]
    fn test_record_literal() {
        let literal = Record(vec![
            ("string", Literal(String("blah"))),
            ("record", Literal(Record(vec![
                ("x", Literal(String("foo"))),
                ("y", Literal(String("bar"))),
            ])))
        ]);

        let expected = "{ string = \"blah\", record = { x = \"foo\", y = \"bar\" } }";

        assert_eq!(expected, literal_to_string(literal));
    }
}
