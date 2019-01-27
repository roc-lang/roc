#[macro_use] extern crate maplit;
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
        let expr = Literal(Record(vec![
            ("string", Literal(String("doesn't matter"))),
            ("record", Literal(Record(vec![
                ("x", Literal(String("ignored"))),
                ("y", Literal(String("also ignored"))),
            ])))
        ]));

        let expected_type = Type::Record(hashmap!{
            "string" => Type::String,
            "record" => Type::Record(hashmap!{
                "x" => Type::String,
                "y" => Type::String,
            })
        });

        assert_eq!(expected_type, infer(expr));
    }
}
