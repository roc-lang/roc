#[macro_use]
extern crate pretty_assertions;
// #[macro_use]
// extern crate indoc;

extern crate bumpalo;
extern crate roc_mono;

mod helpers;

// Test monomorphization
#[cfg(test)]
mod test_mono {
    use crate::helpers::{can_expr, infer_expr, test_home, CanExprOut};
    use bumpalo::Bump;
    use roc_collections::all::MutMap;
    use roc_module::ident::TagName::*;
    use roc_module::symbol::{Interns, Symbol};
    use roc_mono::expr::Expr::{self, *};
    use roc_mono::expr::Procs;
    use roc_mono::layout::{Builtin, Layout};
    use roc_types::subs::Subs;

    // HELPERS

    fn compiles_to(src: &str, expected: Expr<'_>) {
        compiles_to_with_interns(src, |_| expected)
    }

    fn compiles_to_with_interns<'a, F>(src: &str, get_expected: F)
    where
        F: FnOnce(Interns) -> Expr<'a>,
    {
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
        let (_content, mut subs) = infer_expr(subs, &mut unify_problems, &constraint, var);

        // Compile and add all the Procs before adding main
        let mut procs = Procs::default();
        let mut ident_ids = interns.all_ident_ids.remove(&home).unwrap();

        // assume 64-bit pointers
        let pointer_size = std::mem::size_of::<u64>() as u32;

        // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
        let mono_expr = Expr::new(
            &arena,
            &mut subs,
            loc_expr.value,
            &mut procs,
            home,
            &mut ident_ids,
            pointer_size,
        );

        // Put this module's ident_ids back in the interns
        interns.all_ident_ids.insert(home, ident_ids);

        assert_eq!(mono_expr, get_expected(interns));
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
    fn float_addition() {
        compiles_to(
            "3.0 + 4",
            CallByName(
                Symbol::FLOAT_ADD,
                &[
                    (Float(3.0), Layout::Builtin(Builtin::Float64)),
                    (Float(4.0), Layout::Builtin(Builtin::Float64)),
                ],
            ),
        );
    }

    #[test]
    fn int_addition() {
        compiles_to(
            "0xDEADBEEF + 4",
            CallByName(
                Symbol::INT_ADD,
                &[
                    (Int(3735928559), Layout::Builtin(Builtin::Int64)),
                    (Int(4), Layout::Builtin(Builtin::Int64)),
                ],
            ),
        );
    }

    #[test]
    fn num_addition() {
        // Default to Int for `Num *`
        compiles_to(
            "3 + 5",
            CallByName(
                Symbol::INT_ADD,
                &[
                    (Int(3), Layout::Builtin(Builtin::Int64)),
                    (Int(5), Layout::Builtin(Builtin::Int64)),
                ],
            ),
        );
    }

    #[test]
    fn specialize_closure() {
        compiles_to(
            r#"
            f = \x -> x + 5

            { x: f 0x4, y: f 3.14 }
            "#,
            {
                use self::Builtin::*;
                use Layout::Builtin;
                let home = test_home();

                let gen_symbol_3 = Interns::from_index(home, 3);
                let gen_symbol_4 = Interns::from_index(home, 4);

                Struct {
                    fields: &[
                        (
                            "x".into(),
                            CallByName(gen_symbol_3, &[(Int(4), Builtin(Int64))]),
                        ),
                        (
                            "y".into(),
                            CallByName(gen_symbol_4, &[(Float(3.14), Builtin(Float64))]),
                        ),
                    ],
                    layout: Layout::Struct(&[
                        ("x".into(), Builtin(Int64)),
                        ("y".into(), Builtin(Float64)),
                    ]),
                }
            },
        )
    }

    #[test]
    fn if_expression() {
        compiles_to(
            r#"
            if True then "bar" else "foo"
            "#,
            {
                use self::Builtin::*;
                use Layout::Builtin;

                Cond {
                    cond: &Expr::Bool(true),
                    cond_layout: Builtin(Bool(Global("False".into()), Global("True".into()))),
                    pass: &Expr::Str("bar"),
                    fail: &Expr::Str("foo"),
                    ret_layout: Builtin(Str),
                }
            },
        )
    }

    #[test]
    fn multiway_if_expression() {
        compiles_to(
            r#"
            if True then 
                "bar" 
            else if False then
                "foo"
            else
                "baz"
            "#,
            {
                use self::Builtin::*;
                use Layout::Builtin;

                Cond {
                    cond: &Expr::Bool(true),
                    cond_layout: Builtin(Bool(Global("False".into()), Global("True".into()))),
                    pass: &Expr::Str("bar"),
                    fail: &Cond {
                        cond: &Expr::Bool(false),
                        cond_layout: Builtin(Bool(Global("False".into()), Global("True".into()))),
                        pass: &Expr::Str("foo"),
                        fail: &Expr::Str("baz"),
                        ret_layout: Builtin(Str),
                    },
                    ret_layout: Builtin(Str),
                }
            },
        )
    }

    #[test]
    fn annotated_if_expression() {
        // an if with an annotation gets constrained differently. Make sure the result is still correct.
        compiles_to(
            r#"
            x : Str
            x = if True then "bar" else "foo"

            x
            "#,
            {
                use self::Builtin::*;
                use Layout::Builtin;

                let home = test_home();
                let symbol_x = Interns::from_index(home, 0);

                Store(
                    &[(
                        symbol_x,
                        Builtin(Str),
                        Cond {
                            cond: &Expr::Bool(true),
                            cond_layout: Builtin(Bool(
                                Global("False".into()),
                                Global("True".into()),
                            )),
                            pass: &Expr::Str("bar"),
                            fail: &Expr::Str("foo"),
                            ret_layout: Builtin(Str),
                        },
                    )],
                    &Load(symbol_x),
                )
            },
        )
    }

    //    #[test]
    //    fn record_pattern() {
    //        compiles_to(
    //            r#"
    //            \{ x } -> x + 0x5
    //            "#,
    //            { Float(3.45) },
    //        )
    //    }
    //
    //    #[test]
    //    fn tag_pattern() {
    //        compiles_to(
    //            r#"
    //            \Foo x -> x + 0x5
    //            "#,
    //            { Float(3.45) },
    //        )
    //    }

    #[test]
    fn polymorphic_identity() {
        compiles_to(
            r#"
            id = \x -> x

            id { x: id 0x4 }
            "#,
            {
                use self::Builtin::*;
                use Layout::Builtin;
                let home = test_home();

                let gen_symbol_3 = Interns::from_index(home, 3);
                let gen_symbol_4 = Interns::from_index(home, 4);

                CallByName(
                    gen_symbol_3,
                    &[(
                        Struct {
                            fields: &[(
                                "x".into(),
                                CallByName(gen_symbol_4, &[(Int(4), Builtin(Int64))]),
                            )],
                            layout: Layout::Struct(&[("x".into(), Builtin(Int64))]),
                        },
                        Layout::Struct(&[("x".into(), Builtin(Int64))]),
                    )],
                )
            },
        )
    }

    // needs LetRec to be converted to mono
    //    #[test]
    //    fn polymorphic_recursive() {
    //        compiles_to(
    //            r#"
    //            f = \x ->
    //                when x < 10 is
    //                    True -> f (x + 1)
    //                    False -> x
    //
    //            { x: f 0x4, y: f 3.14 }
    //            "#,
    //            {
    //                use self::Builtin::*;
    //                use Layout::Builtin;
    //                let home = test_home();
    //
    //                let gen_symbol_3 = Interns::from_index(home, 3);
    //                let gen_symbol_4 = Interns::from_index(home, 4);
    //
    //                Float(3.4)
    //
    //            },
    //        )
    //    }

    // needs layout for non-empty tag union
    //    #[test]
    //    fn is_nil() {
    //        let arena = Bump::new();
    //
    //        compiles_to_with_interns(
    //            r#"
    //                LinkedList a : [ Cons a (LinkedList a), Nil ]
    //
    //                isNil : LinkedList a -> Bool
    //                isNil = \list ->
    //                    when list is
    //                        Nil -> True
    //                        Cons _ _ -> False
    //
    //                listInt : LinkedList Int
    //                listInt = Nil
    //
    //                isNil listInt
    //            "#,
    //            |interns| {
    //                let home = test_home();
    //                let var_is_nil = interns.symbol(home, "isNil".into());
    //            },
    //        );
    //    }

    #[test]
    fn bool_literal() {
        let arena = Bump::new();

        compiles_to_with_interns(
            r#"
                x : Bool
                x = True

                x
            "#,
            |interns| {
                let home = test_home();
                let var_x = interns.symbol(home, "x".into());

                let stores = [(
                    var_x,
                    Layout::Builtin(Builtin::Bool(Global("False".into()), Global("True".into()))),
                    Bool(true),
                )];

                let load = Load(var_x);

                Store(arena.alloc(stores), arena.alloc(load))
            },
        );
    }

    #[test]
    fn two_element_enum() {
        let arena = Bump::new();

        compiles_to_with_interns(
            r#"
            x : [ Yes, No ]
            x = No

            x
            "#,
            |interns| {
                let home = test_home();
                let var_x = interns.symbol(home, "x".into());

                let stores = [(
                    var_x,
                    Layout::Builtin(Builtin::Bool(Global("No".into()), Global("Yes".into()))),
                    Bool(false),
                )];

                let load = Load(var_x);

                Store(arena.alloc(stores), arena.alloc(load))
            },
        );
    }

    #[test]
    fn three_element_enum() {
        let arena = Bump::new();

        compiles_to_with_interns(
            r#"
            # this test is brought to you by fruits.com!
            x : [ Apple, Orange, Banana ]
            x = Orange

            x
            "#,
            |interns| {
                let home = test_home();
                let var_x = interns.symbol(home, "x".into());

                let mut fruits = MutMap::default();

                fruits.insert(Global("Banana".into()), 0);
                fruits.insert(Global("Orange".into()), 1);
                fruits.insert(Global("Apple".into()), 2);

                let stores = [(var_x, Layout::Builtin(Builtin::Byte(fruits)), Byte(1))];

                let load = Load(var_x);

                Store(arena.alloc(stores), arena.alloc(load))
            },
        );
    }

    #[test]
    fn set_unique_int_list() {
        compiles_to("List.getUnsafe (List.set [ 12, 9, 7, 3 ] 1 42) 1", {
            CallByName(
                Symbol::LIST_GET_UNSAFE,
                &vec![
                    (
                        CallByName(
                            Symbol::LIST_SET,
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
            )
        });
    }

    //    #[test]
    //    fn when_on_result() {
    //        let arena = Bump::new();
    //
    //        compiles_to_with_interns(
    //            r#"
    //            x = Ok 0x3
    //
    //            when x is
    //                Err _ -> 0
    //                Ok n -> n + 1
    //            "#,
    //            |interns| {
    //                let home = test_home();
    //                let var_x = interns.symbol(home, "x".into());
    //
    //                let mut fruits = MutMap::default();
    //
    //                fruits.insert(Global("Banana".into()), 0);
    //                fruits.insert(Global("Orange".into()), 1);
    //                fruits.insert(Global("Apple".into()), 2);
    //
    //                let stores = [(var_x, Layout::Builtin(Builtin::Byte(fruits)), Byte(1))];
    //
    //                let load = Load(var_x);
    //
    //                Store(arena.alloc(stores), arena.alloc(load))
    //            },
    //        );
    //    }
}
