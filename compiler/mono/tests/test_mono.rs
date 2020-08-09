#[macro_use]
extern crate pretty_assertions;

extern crate bumpalo;
extern crate roc_mono;

mod helpers;

// Test monomorphization
#[cfg(test)]
mod test_mono {
    use crate::helpers::{can_expr, infer_expr, test_home, CanExprOut};
    use bumpalo::Bump;
    use roc_module::symbol::{Interns, Symbol};
    use roc_mono::expr::Expr::{self, *};
    use roc_mono::expr::Procs;
    use roc_mono::layout;
    use roc_mono::layout::{Builtin, Layout, LayoutCache};
    use roc_types::subs::Subs;

    // HELPERS

    const I64_LAYOUT: Layout<'static> = Layout::Builtin(Builtin::Int64);
    const F64_LAYOUT: Layout<'static> = Layout::Builtin(Builtin::Float64);

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

        // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
        let mut mono_problems = Vec::new();
        let mut mono_env = roc_mono::expr::Env {
            arena: &arena,
            subs: &mut subs,
            problems: &mut mono_problems,
            home,
            ident_ids: &mut ident_ids,
        };
        let mono_expr = Expr::new(&mut mono_env, loc_expr.value, &mut procs);
        let procs =
            roc_mono::expr::specialize_all(&mut mono_env, procs, &mut LayoutCache::default());

        assert_eq!(
            procs.runtime_errors,
            roc_collections::all::MutMap::default()
        );

        // Put this module's ident_ids back in the interns
        interns.all_ident_ids.insert(home, ident_ids);

        assert_eq!(get_expected(interns), mono_expr);
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
            CallByName {
                name: Symbol::NUM_ADD,
                layout: Layout::FunctionPointer(
                    &[
                        Layout::Builtin(Builtin::Float64),
                        Layout::Builtin(Builtin::Float64),
                    ],
                    &Layout::Builtin(Builtin::Float64),
                ),
                args: &[
                    (Float(3.0), Layout::Builtin(Builtin::Float64)),
                    (Float(4.0), Layout::Builtin(Builtin::Float64)),
                ],
            },
        );
    }

    #[test]
    fn int_addition() {
        compiles_to(
            "0xDEADBEEF + 4",
            CallByName {
                name: Symbol::NUM_ADD,
                layout: Layout::FunctionPointer(
                    &[
                        Layout::Builtin(Builtin::Int64),
                        Layout::Builtin(Builtin::Int64),
                    ],
                    &Layout::Builtin(Builtin::Int64),
                ),
                args: &[
                    (Int(3735928559), Layout::Builtin(Builtin::Int64)),
                    (Int(4), Layout::Builtin(Builtin::Int64)),
                ],
            },
        );
    }

    #[test]
    fn num_addition() {
        // Default to Int for `Num *`
        compiles_to(
            "3 + 5",
            CallByName {
                name: Symbol::NUM_ADD,
                layout: Layout::FunctionPointer(
                    &[
                        Layout::Builtin(Builtin::Int64),
                        Layout::Builtin(Builtin::Int64),
                    ],
                    &Layout::Builtin(Builtin::Int64),
                ),
                args: &[
                    (Int(3), Layout::Builtin(Builtin::Int64)),
                    (Int(5), Layout::Builtin(Builtin::Int64)),
                ],
            },
        );
    }

    #[test]
    fn specialize_closure() {
        compiles_to(
            r#"
            f = \x -> x + 5

            { y: f 3.14, x: f 0x4 }
            "#,
            {
                use self::Builtin::*;
                let home = test_home();
                let gen_symbol_0 = Interns::from_index(home, 0);

                Struct(&[
                    (
                        CallByName {
                            name: gen_symbol_0,
                            layout: Layout::FunctionPointer(
                                &[Layout::Builtin(Builtin::Int64)],
                                &Layout::Builtin(Builtin::Int64),
                            ),
                            args: &[(Int(4), Layout::Builtin(Int64))],
                        },
                        Layout::Builtin(Int64),
                    ),
                    (
                        CallByName {
                            name: gen_symbol_0,
                            layout: Layout::FunctionPointer(
                                &[Layout::Builtin(Builtin::Float64)],
                                &Layout::Builtin(Builtin::Float64),
                            ),
                            args: &[(Float(3.14), Layout::Builtin(Float64))],
                        },
                        Layout::Builtin(Float64),
                    ),
                ])
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

                let home = test_home();
                let gen_symbol_0 = Interns::from_index(home, 0);

                Store(
                    &[(
                        gen_symbol_0,
                        Layout::Builtin(layout::Builtin::Int1),
                        Expr::Bool(true),
                    )],
                    &Cond {
                        cond_symbol: gen_symbol_0,
                        branch_symbol: gen_symbol_0,
                        cond_layout: Builtin(Int1),
                        pass: (&[] as &[_], &Expr::Str("bar")),
                        fail: (&[] as &[_], &Expr::Str("foo")),
                        ret_layout: Builtin(Str),
                    },
                )
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

                let home = test_home();
                let gen_symbol_0 = Interns::from_index(home, 1);
                let gen_symbol_1 = Interns::from_index(home, 0);

                Store(
                    &[(
                        gen_symbol_0,
                        Layout::Builtin(layout::Builtin::Int1),
                        Expr::Bool(true),
                    )],
                    &Cond {
                        cond_symbol: gen_symbol_0,
                        branch_symbol: gen_symbol_0,
                        cond_layout: Builtin(Int1),
                        pass: (&[] as &[_], &Expr::Str("bar")),
                        fail: (
                            &[] as &[_],
                            &Store(
                                &[(
                                    gen_symbol_1,
                                    Layout::Builtin(layout::Builtin::Int1),
                                    Expr::Bool(false),
                                )],
                                &Cond {
                                    cond_symbol: gen_symbol_1,
                                    branch_symbol: gen_symbol_1,
                                    cond_layout: Builtin(Int1),
                                    pass: (&[] as &[_], &Expr::Str("foo")),
                                    fail: (&[] as &[_], &Expr::Str("baz")),
                                    ret_layout: Builtin(Str),
                                },
                            ),
                        ),
                        ret_layout: Builtin(Str),
                    },
                )
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
                let gen_symbol_0 = Interns::from_index(home, 1);
                let symbol_x = Interns::from_index(home, 0);

                Store(
                    &[(
                        symbol_x,
                        Builtin(Str),
                        Store(
                            &[(
                                gen_symbol_0,
                                Layout::Builtin(layout::Builtin::Int1),
                                Expr::Bool(true),
                            )],
                            &Cond {
                                cond_symbol: gen_symbol_0,
                                branch_symbol: gen_symbol_0,
                                cond_layout: Builtin(Int1),
                                pass: (&[] as &[_], &Expr::Str("bar")),
                                fail: (&[] as &[_], &Expr::Str("foo")),
                                ret_layout: Builtin(Str),
                            },
                        ),
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

                id { x: id 0x4, y: 0.1 }
            "#,
            {
                let home = test_home();

                let gen_symbol_0 = Interns::from_index(home, 0);
                let struct_layout = Layout::Struct(&[I64_LAYOUT, F64_LAYOUT]);

                CallByName {
                    name: gen_symbol_0,
                    layout: Layout::FunctionPointer(
                        &[struct_layout.clone()],
                        &struct_layout.clone(),
                    ),
                    args: &[(
                        Struct(&[
                            (
                                CallByName {
                                    name: gen_symbol_0,
                                    layout: Layout::FunctionPointer(&[I64_LAYOUT], &I64_LAYOUT),
                                    args: &[(Int(4), I64_LAYOUT)],
                                },
                                I64_LAYOUT,
                            ),
                            (Float(0.1), F64_LAYOUT),
                        ]),
                        struct_layout,
                    )],
                }
            },
        )
    }

    // #[test]
    // fn list_get_unique() {
    //     compiles_to(
    //         r#"
    //             unique = [ 2, 4 ]

    //             List.get unique 1
    //         "#,
    //         {
    //             use self::Builtin::*;
    //             let home = test_home();

    //             let gen_symbol_0 = Interns::from_index(home, 0);
    //             let list_layout = Layout::Builtin(Builtin::List(&I64_LAYOUT));

    //             CallByName {
    //                 name: gen_symbol_0,
    //                 layout: Layout::FunctionPointer(&[list_layout.clone()], &list_layout.clone()),
    //                 args: &[(
    //                     Struct(&[(
    //                         CallByName {
    //                             name: gen_symbol_0,
    //                             layout: Layout::FunctionPointer(
    //                                 &[Layout::Builtin(Builtin::Int64)],
    //                                 &Layout::Builtin(Builtin::Int64),
    //                             ),
    //                             args: &[(Int(4), Layout::Builtin(Int64))],
    //                         },
    //                         Layout::Builtin(Int64),
    //                     )]),
    //                     Layout::Struct(&[Layout::Builtin(Int64)]),
    //                 )],
    //             }
    //         },
    //     )
    // }

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

                let stores = [(var_x, Layout::Builtin(Builtin::Int1), Bool(true))];

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

                let stores = [(var_x, Layout::Builtin(Builtin::Int1), Bool(false))];

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

                // orange gets index (and therefore tag_id) 1
                let stores = [(var_x, Layout::Builtin(Builtin::Int8), Byte(2))];

                let load = Load(var_x);

                Store(arena.alloc(stores), arena.alloc(load))
            },
        );
    }

    #[test]
    fn set_unique_int_list() {
        compiles_to("List.get (List.set [ 12, 9, 7, 3 ] 1 42) 1", {
            CallByName {
                name: Symbol::LIST_GET,
                layout: Layout::FunctionPointer(
                    &[Layout::Builtin(Builtin::List(&I64_LAYOUT)), I64_LAYOUT],
                    &Layout::Union(&[&[I64_LAYOUT], &[I64_LAYOUT, I64_LAYOUT]]),
                ),
                args: &vec![
                    (
                        CallByName {
                            name: Symbol::LIST_SET,
                            layout: Layout::FunctionPointer(
                                &[
                                    Layout::Builtin(Builtin::List(&I64_LAYOUT)),
                                    I64_LAYOUT,
                                    I64_LAYOUT,
                                ],
                                &Layout::Builtin(Builtin::List(&I64_LAYOUT)),
                            ),
                            args: &vec![
                                (
                                    Array {
                                        elem_layout: I64_LAYOUT,
                                        elems: &vec![Int(12), Int(9), Int(7), Int(3)],
                                    },
                                    Layout::Builtin(Builtin::List(&I64_LAYOUT)),
                                ),
                                (Int(1), I64_LAYOUT),
                                (Int(42), I64_LAYOUT),
                            ],
                        },
                        Layout::Builtin(Builtin::List(&I64_LAYOUT)),
                    ),
                    (Int(1), I64_LAYOUT),
                ],
            }
        });
    }

    //    #[test]
    //    fn when_on_result() {
    //        compiles_to(
    //            r#"
    //            when 1 is
    //                1 -> 12
    //                _ -> 34
    //            "#,
    //            {
    //                use self::Builtin::*;
    //                use Layout::Builtin;
    //                let home = test_home();
    //
    //                let gen_symbol_3 = Interns::from_index(home, 3);
    //                let gen_symbol_4 = Interns::from_index(home, 4);
    //
    //                CallByName(
    //                    gen_symbol_3,
    //                    &[(
    //                        Struct(&[(
    //                            CallByName(gen_symbol_4, &[(Int(4), Builtin(Int64))]),
    //                            Builtin(Int64),
    //                        )]),
    //                        Layout::Struct(&[("x".into(), Builtin(Int64))]),
    //                    )],
    //                )
    //            },
    //        )
    //    }
}
