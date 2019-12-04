#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc;

mod helpers;

#[cfg(test)]
mod test_sharing {
    use crate::helpers::can_expr;
    use roc::can::symbol;
    use roc::uniqueness::sharing;
    use roc::uniqueness::sharing::ReferenceCount::{self, *};

    // HELPERS

    fn sharing_eq(src: &str, expected: (&str, ReferenceCount)) {
        let (expr, _output, _, _subs, _variable) = can_expr(src);

        let usage = sharing::sharing_analysis(&expr);

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
    fn sharing_case_unique() {
        sharing_eq(
            indoc!(
                r#"
                (\x ->
                case 1 when
                 1 -> x
                 3 -> x
                 )
            "#
            ),
            ("x", Unique),
        );
    }

    #[test]
    fn sharing_case_shared() {
        sharing_eq(
            indoc!(
                r#"
                (\x ->
                case x when
                 1 -> x
                 3 -> x
                 )
            "#
            ),
            ("x", Shared),
        );
    }
}
