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
    //use roc::infer::infer_expr;
    //use roc::pretty_print_types::content_to_string;
    use std::collections::HashMap;

    // HELPERS

    fn sharing_eq(src: &str, expected: (&str, ReferenceCount)) {
        let (expr, _output, _, _procedures, _subs, _variable) = can_expr(src);

        let mut usage = HashMap::new();
        sharing::sharing_analysis(&expr, &mut usage);

        dbg!(expr);
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
            ("1", Unique),
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
            ("1", Shared),
        );
    }

    #[test]
    fn sharing_dup() {
        sharing_eq(
            indoc!(
                r#"
                \x -> if True then x else x
            "#
            ),
            ("1", Unique),
        );
    }
}
