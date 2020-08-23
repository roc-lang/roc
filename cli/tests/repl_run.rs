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
    fn eval_0() {
        expect_success("0", "0 : Num *");
    }

    #[test]
    fn eval_0x0() {
        expect_success("0x0", "0 : Int");
    }

    #[test]
    fn eval_0point0() {
        expect_success("0.0", "0 : Float");
    }
}
