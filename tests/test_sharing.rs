#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_infer {
    use helpers::can_expr;
    use roc::can::sharing;
    use roc::can::sharing::ReferenceCount::{self, *};
    use roc::can::symbol;

    // HELPERS

    fn sharing_eq(src: &str, expected: (&str, ReferenceCount)) {
        let (expr, _output, _, procedures, _subs, _variable) = can_expr(src);

        let usage = sharing::sharing_analysis(&expr, &procedures);

        dbg!(expr);
        dbg!(procedures);
        dbg!(usage.clone());

        let (varname, value) = expected;
        assert_eq!(
            usage.get(&symbol::Symbol::new("Test.blah$", varname)),
            Some(&value)
        );
    }

    #[test]
    fn sharing_identity() {
        sharing_eq(
            indoc!(
                r#"
                \x -> x
                ""
            "#
            ),
            ("x", Unique),
        );
    }

    #[test]
    fn sharing_shared() {
        sharing_eq(
            indoc!(
                r#"
                f = \x -> x

                (\x -> 
                    a = f x
                    b = f x

                    x
                )
            "#
            ),
            ("x", Shared),
        );
    }

    #[test]
    fn sharing_dup() {
        sharing_eq(
            indoc!(
                r#"
                \x -> if 1 == 1 then x else x
            "#
            ),
            ("1", Unique),
        );
    }
}
