#[macro_use]
extern crate pretty_assertions;
// #[macro_use]
// extern crate indoc;

extern crate bumpalo;
extern crate roc_mono;

mod helpers;

// Test optimizations
#[cfg(test)]
mod test_opt {
    use crate::helpers::{infer_expr, uniq_expr};
    use bumpalo::Bump;
    use roc_collections::all::MutMap;
    use roc_module::symbol::Symbol;
    use roc_mono::expr::Expr::{self, *};
    use roc_mono::layout::{Builtin, Layout};

    // HELPERS

    fn compiles_to(src: &str, expected: Expr<'_>) {
        let arena = Bump::new();
        let (loc_expr, _, _problems, subs, var, constraint, home, mut interns) = uniq_expr(src);

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

    #[test]
    fn set_unique_int_list() {
        // This should optimize List.set to List.set_in_place
        compiles_to(
            "List.getUnsafe (List.set [ 12, 9, 7, 3 ] 1 42) 1",
            CallByName(
                Symbol::LIST_GET_UNSAFE,
                &vec![
                    (
                        CallByName(
                            Symbol::LIST_SET_IN_PLACE,
                            &vec![
                                (
                                    Array {
                                        elem_layout: Layout::Builtin(Builtin::Int64),
                                        elems: &vec![Int(12), Int(9), Int(7), Int(3)],
                                    },
                                    Layout::Builtin(Builtin::List(&Layout::Builtin(
                                        Builtin::Int64,
                                    ))),
                                ),
                                (Int(1), Layout::Builtin(Builtin::Int64)),
                                (Int(42), Layout::Builtin(Builtin::Int64)),
                            ],
                        ),
                        Layout::Builtin(Builtin::List(&Layout::Builtin(Builtin::Int64))),
                    ),
                    (Int(1), Layout::Builtin(Builtin::Int64)),
                ],
            ),
        );
    }
}
