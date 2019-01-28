#[macro_use] extern crate pretty_assertions;

extern crate roc;

#[cfg(test)]
mod tests {
    use roc::unify::Type;
    use roc::unify::Expr::Literal;
    use roc::unify::Literal::{String, Record};
    use roc::unify::infer;

    #[test]
    fn test_infer_record_literals() {
        let str0 = &String("doesn't matter");
        let str1 = &String("ignored");
        let str2 = &String("also ignored");

        let x = ("x", &Literal(str1));
        let y = ("y", &Literal(str2));

        let subrec = &Record(vec![x, y]);
        let str_pair = ("string", &Literal(str0));
        let rec_pair = ("record", &Literal(subrec));
        let toprec = vec![str_pair, rec_pair];
        let literal = &Record(toprec);

        let expr = Literal(literal);

        let expected_type = Type::Record(vec![
            ("string", Type::String),
            ("record", Type::Record(vec![
                ("x", Type::String),
                ("y", Type::String)
            ]))
        ]);

        assert_eq!(expected_type, infer(&expr).unwrap());
    }
}
