#[cfg(test)]
mod test_can_expr {
    use roc_can::expr::Expr;
    use test_compile::can_expr;

    #[test]
    fn test_can_unit() {
        let output = can_expr("{}");

        assert_eq!(output.problems, Vec::new());
        assert!(matches!(output.expr, Expr::EmptyRecord));
    }
}
