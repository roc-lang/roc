#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc_mono;

mod helpers;

// Test monomorphization
#[cfg(test)]
mod test_mono {
    use crate::helpers::{can_expr, infer_expr, CanExprOut};
    use bumpalo::Bump;
    use roc_collections::all::MutMap;
    use roc_mono::expr::Expr::{self, *};
    use roc_types::subs::Subs;

    // HELPERS

    fn compiles_to(src: &str, expected: Expr<'_>) {
        let arena = Bump::new();
        let CanExprOut {
            loc_expr,
            var_store,
            var,
            constraint,
            home,
            mut interns,
            ..
        } = can_expr(src);
        let subs = Subs::new(var_store.into());
        let mut unify_problems = Vec::new();
        let (_content, subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        // Compile and add all the Procs before adding main
        let mut procs = MutMap::default();
        let mut ident_ids = interns.all_ident_ids.remove(&home).unwrap();

        // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
        let mono_expr = Expr::new(
            &arena,
            &subs,
            loc_expr.value,
            &mut procs,
            home,
            &mut ident_ids,
        );

        assert_eq!(mono_expr, expected);
    }

    #[test]
    fn int_literal() {
        compiles_to("5", Int(5));
    }

    #[test]
    fn float_literal() {
        compiles_to("0.5", Float(0.5));
    }
}
