#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;

#[cfg(test)]
mod specialize_types {
    use roc_specialize_types::{MonoExpr, Number};
    use test_compile::specialize_expr;

    fn expect_no_expr(input: impl AsRef<str>) {
        let out = specialize_expr(input.as_ref());
        let actual = out.mono_expr_id.map(|id| out.mono_exprs.get(id));

        assert_eq!(None, actual, "This input expr should have specialized to being dicarded as zero-sized, but it didn't: {:?}", input.as_ref());
    }

    fn expect_mono_expr(input: impl AsRef<str>, mono_expr: MonoExpr) {
        let out = specialize_expr(input.as_ref());
        let mono_expr_id = out
            .mono_expr_id
            .expect("This input expr should not have been discarded as zero-sized, but it was discarded: {input:?}");

        assert_eq!(&mono_expr, out.mono_exprs.get(mono_expr_id));
    }

    #[test]
    fn empty_record() {
        let todo = (); // TODO need to get earlier stage working, specifically constraint + solve, by replicating existing solve tests.
        expect_no_expr("{}");
    }

    #[test]
    fn unbound_num() {
        let expected = 42;
        expect_mono_expr(
            format!("{expected}"),
            MonoExpr::Number(Number::I64(expected)),
        );
    }
}
