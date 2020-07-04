#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc_can;
extern crate roc_parse;
extern crate roc_region;

mod helpers;

#[cfg(test)]
mod can_inline {
    use crate::helpers::{can_expr_with, test_home};
    use bumpalo::Bump;
    use roc_can::expr::inline_calls;
    use roc_can::expr::Expr::{self, *};
    use roc_can::scope::Scope;
    use roc_module::operator::CalledVia;
    use roc_module::symbol::Symbol;
    use roc_region::all::{Located, Region};
    use roc_types::subs::VarStore;

    fn assert_inlines_to(input: &str, expected: Expr, var_store: &mut VarStore) {
        let arena = Bump::new();
        let scope = &mut Scope::new(test_home());
        let actual_out = can_expr_with(&arena, test_home(), input);
        let actual = inline_calls(var_store, scope, actual_out.loc_expr.value);

        assert_eq!(actual, expected);
    }

    #[test]
    fn inline_list_len() {
        let var_store = &mut VarStore::default();

        assert_inlines_to(
            indoc!(
                r#"
                    Int.isZero 5
                "#
            ),
            Expr::Call(
                Box::new((
                    var_store.fresh(),
                    Located {
                        region: Region::zero(),
                        value: Expr::Var(Symbol::FLOAT_EQ),
                    },
                    var_store.fresh(),
                )),
                vec![(
                    var_store.fresh(),
                    Located {
                        region: Region::zero(),
                        value: Int(var_store.fresh(), 5),
                    },
                )],
                CalledVia::Space,
            ),
            var_store,
        )
    }
}
