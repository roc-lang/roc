#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate roc_mono;

mod helpers;

// Test optimizations
#[cfg(test)]
mod test_opt {
    use crate::helpers::uniq_expr;
    use bumpalo::Bump;
    use roc_collections::all::MutMap;
    use roc_mono::expr::Expr::{self, *};

    // HELPERS

    fn compiles_to(src: &str, expected: Expr<'_>) {
        let arena = Bump::new();
        let (loc_expr, _, _problems, subs, _, _, home, mut interns) = uniq_expr(src);
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
