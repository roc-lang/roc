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

        assert_eq!(expected, literal_to_string(&literal));
    }

    #[test]
    fn test_record_literal() {
        let str0 = &String("blah");
        let str1 = &String("foo");
        let str2 = &String("bar");

        let x = ("x", &Literal(str1));
        let y = ("y", &Literal(str2));

        let subrec = &Record(vec![x, y]);
        let str_pair = ("string", &Literal(str0));
        let rec_pair = ("record", &Literal(subrec));
        let toprec = vec![str_pair, rec_pair];
        let literal = &Record(toprec);

        let expected = "{ string = \"blah\", record = { x = \"foo\", y = \"bar\" } }";

        assert_eq!(expected, literal_to_string(literal));
    }
}
