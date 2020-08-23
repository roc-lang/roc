#[macro_use]
extern crate pretty_assertions;

mod helpers;

#[cfg(test)]
mod repl_run {
    use crate::helpers::repl_eval;

    fn expect_success(input: &str, expected: &str) {
        let out = repl_eval(input);

        assert_eq!(&out.stderr, "");
        assert_eq!(&out.stdout, expected);
        assert!(out.status.success());
    }

    #[test]
    fn literal_0() {
        expect_success("0", "0 : Num *");
    }

    #[test]
    fn literal_42() {
        expect_success("42", "42 : Num *");
    }

    #[test]
    fn literal_0x0() {
        expect_success("0x0", "0 : Int");
    }

    #[test]
    fn literal_0x42() {
        expect_success("0x42", "66 : Int");
    }

    #[test]
    fn literal_0point0() {
        expect_success("0.0", "0 : Float");
    }

    #[test]
    fn literal_4point2() {
        expect_success("4.2", "4.2 : Float");
    }

    #[test]
    fn num_addition() {
        expect_success("1 + 2", "3 : Num *");
    }

    #[test]
    fn int_addition() {
        expect_success("0x1 + 2", "3 : Int");
    }

    #[test]
    fn float_addition() {
        expect_success("1.1 + 2", "3.1 : Float");
    }

    #[test]
    fn literal_empty_str() {
        expect_success("\"\"", "\"\" : Str");
    }

    #[test]
    fn literal_ascii_str() {
        expect_success("\"Hello, World!\"", "\"Hello, World!\" : Str");
    }

    #[test]
    fn literal_utf8_str() {
        expect_success("\"👩‍👩‍👦‍👦\"", "\"👩‍👩‍👦‍👦\" : Str");
    }

    #[test]
    fn str_concat() {
        expect_success(
            "Str.concat \"Hello, \" \"World!\"",
            "\"Hello, World!\" : Str",
        );
    }

    #[test]
    fn literal_empty_list() {
        expect_success("[]", "[] : List *");
    }

    #[test]
    fn literal_num_list() {
        expect_success("[ 1, 2, 3 ]", "[ 1, 2, 3 ] : List (Num *)");
    }

    #[test]
    fn literal_int_list() {
        expect_success("[ 0x1, 0x2, 0x3 ]", "[ 1, 2, 3 ] : List Int");
    }

    #[test]
    fn literal_float_list() {
        expect_success("[ 1.1, 2.2, 3.3 ]", "[ 1.1, 2.2, 3.3 ] : List Float");
    }

    #[test]
    fn list_concat() {
        expect_success(
            "List.concat [ 1.1, 2.2 ] [ 3.3, 4.4, 5.5 ]",
            "[ 1.1, 2.2, 3.3, 4.4, 5.5 ] : List Float",
        );
    }
}
