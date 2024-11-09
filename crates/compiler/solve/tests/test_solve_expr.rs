#[cfg(test)]
mod test_solve_expr {
    use roc_can::expr::Expr;
    use roc_types::subs::Content;
    use test_compile::solve_expr;

    #[test]
    fn solve_empty_record() {
        let mut output = solve_expr("{}");

        assert_eq!(output.problems, Vec::new());
        assert!(matches!(output.expr, Expr::EmptyRecord));
        assert_eq!(
            Content::Structure(roc_types::subs::FlatType::EmptyRecord),
            output.subs.inner_mut().get(output.var).content
        );
    }
}
