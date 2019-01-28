#[macro_use] extern crate pretty_assertions;

extern crate roc;

#[cfg(test)]
mod repl_tests {
    use roc::repl::eval_and_print;
    use roc::unify::Expr::{Literal};
    use roc::unify::Literal::{String, Record};

    #[test]
    fn test_eval_and_print() {
        let expected = "\"hi!\"\n: String";
        let literal = String("hi!");
        let expr = Literal(&literal);

        assert_eq!(expected, eval_and_print(&expr));
    }

    #[test]
    fn test_record_literal() {
        let expected = "{ string = \"abc\", record = { x = \"one\", y = \"two\" } }\n: { record : { x : String, y : String }, string : String }";

        let str0 = &String("abc");
        let str1 = &String("one");
        let str2 = &String("two");

        let x = ("x", &Literal(str1));
        let y = ("y", &Literal(str2));

        let subrec = &Record(vec![x, y]);
        let str_pair = ("string", &Literal(str0));
        let rec_pair = ("record", &Literal(subrec));
        let toprec = vec![str_pair, rec_pair];
        let literal = &Record(toprec);

        let expr = Literal(&literal);

        assert_eq!(expected, eval_and_print(&expr));
    }
}
