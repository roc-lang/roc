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
    use crate::helpers::{can_expr, infer_expr, test_home, CanExprOut};
    use bumpalo::Bump;
    use roc_module::ident::TagName;
    use roc_module::symbol::{Interns, Symbol};
    use roc_mono::expr::Expr::{self, *};
    use roc_mono::expr::{InProgressProc, Procs};
    use roc_mono::layout;
    use roc_mono::layout::Ownership::Owned;
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

        // assume 64-bit pointers
        let pointer_size = std::mem::size_of::<u64>() as u32;

        // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
        let mut mono_problems = Vec::new();
        let mut mono_env = roc_mono::expr::Env {
            arena: &arena,
            subs: &mut subs,
            problems: &mut mono_problems,
            home,
            ident_ids: &mut ident_ids,
            pointer_size,
            jump_counter: arena.alloc(0),
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

    fn compiles_to_string(src: &str, expected: &str) {
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
        let mut mono_problems = Vec::new();
        let mut mono_env = roc_mono::expr::Env {
            arena: &arena,
            subs: &mut subs,
            problems: &mut mono_problems,
            home,
            ident_ids: &mut ident_ids,
            pointer_size,
            jump_counter: arena.alloc(0),
        };
        dbg!(&procs);
        let mono_expr = Expr::new(&mut mono_env, loc_expr.value, &mut procs);
        let procs =
            roc_mono::expr::specialize_all(&mut mono_env, procs, &mut LayoutCache::default());
        dbg!(&procs);

        assert_eq!(
            procs.runtime_errors,
            roc_collections::all::MutMap::default()
        );

        // Put this module's ident_ids back in the interns
        interns.all_ident_ids.insert(home, ident_ids);

        let mut procs_string = procs
            .specialized
            .iter()
            .map(|(_, value)| {
                if let InProgressProc::Done(proc) = value {
                    proc.to_pretty(200)
                } else {
                    String::new()
                }
            })
            .collect::<Vec<_>>();

        dbg!(&mono_expr);
        procs_string.push(mono_expr.to_pretty(200));

        let result = procs_string.join("\n");

        // assert_eq!(result, expected);
        ()
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
                        branching_symbol: gen_symbol_0,
                        cond_layout: Builtin(Int1),
                        branching_layout: Builtin(Int1),
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
                        branching_symbol: gen_symbol_0,
                        cond_layout: Builtin(Int1),
                        branching_layout: Builtin(Int1),
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
                                    branching_symbol: gen_symbol_1,
                                    branching_layout: Builtin(Int1),
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
                                branching_symbol: gen_symbol_0,
                                cond_layout: Builtin(Int1),
                                branching_layout: Builtin(Int1),
                                pass: (&[] as &[_], &Expr::Str("bar")),
                                fail: (&[] as &[_], &Expr::Str("foo")),
                                ret_layout: Builtin(Str),
                            },
                        ),
                    )],
                    &DecAfter(symbol_x, &Load(symbol_x)),
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

                let dec = DecAfter(var_x, arena.alloc(load));

                Store(arena.alloc(stores), arena.alloc(dec))
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

                let dec = DecAfter(var_x, arena.alloc(load));

                Store(arena.alloc(stores), arena.alloc(dec))
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

                let dec = DecAfter(var_x, arena.alloc(load));
                Store(arena.alloc(stores), arena.alloc(dec))
            },
        );
    }

    #[test]
    fn set_unique_int_list() {
        compiles_to("List.get (List.set [ 12, 9, 7, 3 ] 1 42) 1", {
            CallByName {
                name: Symbol::LIST_GET,
                layout: Layout::FunctionPointer(
                    &[
                        Layout::Builtin(Builtin::List(Owned, &I64_LAYOUT)),
                        I64_LAYOUT,
                    ],
                    &Layout::Union(&[&[I64_LAYOUT], &[I64_LAYOUT, I64_LAYOUT]]),
                ),
                args: &vec![
                    (
                        CallByName {
                            name: Symbol::LIST_SET,
                            layout: Layout::FunctionPointer(
                                &[
                                    Layout::Builtin(Builtin::List(Owned, &I64_LAYOUT)),
                                    I64_LAYOUT,
                                    I64_LAYOUT,
                                ],
                                &Layout::Builtin(Builtin::List(Owned, &I64_LAYOUT)),
                            ),
                            args: &vec![
                                (
                                    Array {
                                        elem_layout: I64_LAYOUT,
                                        elems: &vec![Int(12), Int(9), Int(7), Int(3)],
                                    },
                                    Layout::Builtin(Builtin::List(Owned, &I64_LAYOUT)),
                                ),
                                (Int(1), I64_LAYOUT),
                                (Int(42), I64_LAYOUT),
                            ],
                        },
                        Layout::Builtin(Builtin::List(Owned, &I64_LAYOUT)),
                    ),
                    (Int(1), I64_LAYOUT),
                ],
            }
        });
    }

    //        #[test]
    //        fn when_on_result() {
    //            compiles_to(
    //                r#"
    //                when 1 is
    //                    1 -> 12
    //                    _ -> 34
    //                "#,
    //                {
    //                    use self::Builtin::*;
    //                    use Layout::Builtin;
    //                    let home = test_home();
    //
    //                    let gen_symbol_3 = Interns::from_index(home, 3);
    //                    let gen_symbol_4 = Interns::from_index(home, 4);
    //
    //                    CallByName(
    //                        gen_symbol_3,
    //                        &[(
    //                            Struct(&[(
    //                                CallByName(gen_symbol_4, &[(Int(4), Builtin(Int64))]),
    //                                Builtin(Int64),
    //                            )]),
    //                            Layout::Struct(&[("x".into(), Builtin(Int64))]),
    //                        )],
    //                    )
    //                },
    //            )
    //        }

    #[test]
    fn simple_to_string() {
        compiles_to_string(
            r#"
            x = 3

            x
            "#,
            indoc!(
                r#"
                Store Test.0: 3i64
                Load Test.0
                Dec Test.0
                "#
            ),
        )
    }

    #[test]
    fn if_to_string() {
        compiles_to_string(
            r#"
            if True then 1 else 2
            "#,
            indoc!(
                r#"
                Store Test.0: true
                if Test.0 then
                    1i64
                else
                    2i64
                "#
            ),
        )
    }

    #[test]
    fn maybe_map_to_string() {
        compiles_to_string(
            r#"
            Maybe a : [ Nothing, Just a ]

            maybe : Maybe Int
            maybe = Just 0x3

            when maybe is
                Just x -> Just (x + 1)
                Nothing -> Nothing
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    Lowlevel.NumAdd (Load #Attr.2) (Load #Attr.3)

                Store Test.1:
                    Store Test.3: 0i64
                    Store Test.4: 3i64
                    Just Test.3 Test.4
                Store Test.8: Lowlevel.And (Lowlevel.Eq 0i64 (Access @0 Load Test.1)) true
                if Test.8 then
                    Reset Test.1
                    Store Test.5: 0i64
                    Store Test.6: Call Num.14 (Load Test.2) 1i64
                    Reuse Test.1
                    Just Test.5 Test.6
                else
                    Reset Test.1
                    Store Test.7: 1i64
                    Reuse Test.1
                    Nothing Test.7
                Dec Test.1
                "#
            ),
        )
    }

    #[test]
    fn these_map_to_string() {
        compiles_to_string(
            r#"
            These a b : [ This a, That b, These a b ]

            these : These Int Int
            these = These 1 2

            when these is
                This a -> This a
                That b -> That b
                These a b -> These b a
            "#,
            indoc!(
                r#"
                Store Test.1:
                    Store Test.6: 1i64
                    Store Test.7: 1i64
                    Store Test.8: 2i64
                    These Test.6 Test.7 Test.8
                switch Test.1:
                    case 2:
                        Reset Test.1
                        Store Test.9: 2i64
                        Reuse Test.1
                        This Test.9 Test.2

                    case 0:
                        Reset Test.1
                        Store Test.11: 0i64
                        Reuse Test.1
                        That Test.11 Test.3

                    default:
                        Reset Test.1
                        Store Test.13: 1i64
                        Reuse Test.1
                        These Test.13 Test.5 Test.4

                Dec Test.1
                "#
            ),
        )
    }

    #[test]
    fn list_length() {
        compiles_to_string(
            r#"
            x = [ 1,2,3 ]

            List.len x
            "#,
            indoc!(
                r#"
                procedure List.7 (#Attr.2):
                    Lowlevel.ListLen (Load #Attr.2)

                Store Test.0: [ 1i64, 2i64, 3i64 ]
                Call List.7 (Load Test.0)
                Dec Test.0
                "#
            ),
        )
    }

    #[test]
    fn pass_list_to_function() {
        compiles_to_string(
            r#"
            x : List Int
            x = [1,2,3]

            id : a -> a
            id = \y -> y

            id x
            "#,
            indoc!(
                r#"
                procedure Test.1 (Test.3):
                    Load Test.3
                    Dec Test.3

                Store Test.0: [ 1i64, 2i64, 3i64 ]
                Call Test.1 (Load Test.0)
                Dec Test.0
                "#
            ),
        )
    }

    #[test]
    fn double_list_len() {
        compiles_to_string(
            r#"
            x : List Int
            x = [1,2,3]

            List.len x + List.len x
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    Lowlevel.NumAdd (Load #Attr.2) (Load #Attr.3)

                procedure List.7 (#Attr.2):
                    Lowlevel.ListLen (Load #Attr.2)

                Store Test.0: [ 1i64, 2i64, 3i64 ]
                Call Num.14 (Call List.7 (Load Test.0)) (Call List.7 (Load Test.0))
                Dec Test.0
                "#
            ),
        )
    }

    #[test]
    fn is_nil() {
        compiles_to_string(
            r#"
            isNil = \xs ->
                when xs is
                    Nil -> True
                    Cons _ _ -> False

            isNil Nil
            "#,
            indoc!(
                r#"
                procedure Test.0 (Test.2):
                    Store Test.3: Lowlevel.And (Lowlevel.Eq true (Load Test.2)) true
                    if Test.3 then
                        true
                    else
                        false
                    Dec Test.2

                Call Test.0 true
                "#
            ),
        )
    }

    #[test]
    fn y_is_dead() {
        compiles_to_string(
            r#"
            f = \y -> Pair y y

            f [1]
            "#,
            indoc!(
                r#"
                procedure Test.0 (Test.2):
                    Struct { Load Test.2, Load Test.2 }
                    Dec Test.2

                Call Test.0 [ 1i64 ]
                "#
            ),
        )
    }

    fn compiles_to_ir(src: &str, expected: &str) {
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
        let mut procs = roc_mono::experiment::Procs::default();
        let mut ident_ids = interns.all_ident_ids.remove(&home).unwrap();

        // assume 64-bit pointers
        let pointer_size = std::mem::size_of::<u64>() as u32;

        // Populate Procs and Subs, and get the low-level Expr from the canonical Expr
        let mut mono_problems = Vec::new();
        let mut mono_env = roc_mono::experiment::Env {
            arena: &arena,
            subs: &mut subs,
            problems: &mut mono_problems,
            home,
            ident_ids: &mut ident_ids,
            pointer_size,
            jump_counter: arena.alloc(0),
        };

        let mut layout_cache = LayoutCache::default();
        let ir_expr = roc_mono::experiment::from_can(
            &mut mono_env,
            loc_expr.value,
            &mut procs,
            &mut layout_cache,
        );

        // let mono_expr = Expr::new(&mut mono_env, loc_expr.value, &mut procs);
        let procs =
            roc_mono::experiment::specialize_all(&mut mono_env, procs, &mut LayoutCache::default());

        assert_eq!(
            procs.runtime_errors,
            roc_collections::all::MutMap::default()
        );

        // Put this module's ident_ids back in the interns
        interns.all_ident_ids.insert(home, ident_ids);

        let mut procs_string = procs
            .specialized
            .iter()
            .map(|(_, value)| {
                if let roc_mono::experiment::InProgressProc::Done(proc) = value {
                    proc.to_pretty(200)
                } else {
                    String::new()
                }
            })
            .collect::<Vec<_>>();

        procs_string.push(ir_expr.to_pretty(200));

        let result = procs_string.join("\n");

        let the_same = result == expected;
        if !the_same {
            println!("{}", result);
        }

        assert_eq!(result, expected);
    }

    #[test]
    fn ir_int_literal() {
        compiles_to_ir(
            r#"
            5
            "#,
            indoc!(
                r#"
                let Test.0 = 5i64;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn ir_assignment() {
        compiles_to_ir(
            r#"
            x = 5

            x
            "#,
            indoc!(
                r#"
                let Test.0 = 5i64;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn ir_if() {
        compiles_to_ir(
            r#"
            if True then 1 else 2
            "#,
            indoc!(
                r#"
                let Test.1 = true;
                if Test.1 then
                    let Test.2 = 1i64;
                    ret Test.2;
                else
                    let Test.0 = 2i64;
                    ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn ir_when_enum() {
        compiles_to_ir(
            r#"
            when Blue is
                Red -> 1
                White -> 2
                Blue -> 3
            "#,
            indoc!(
                r#"
                let Test.0 = 0u8;
                switch Test.0:
                    case 1:
                        let Test.1 = 1i64;
                        ret Test.1;

                    case 2:
                        let Test.2 = 2i64;
                        ret Test.2;

                    default:
                        let Test.3 = 3i64;
                        ret Test.3;

                "#
            ),
        )
    }

    #[test]
    fn ir_when_maybe() {
        compiles_to_ir(
            r#"
            when Just 3 is
                Just n -> n
                Nothing -> 0
            "#,
            indoc!(
                r#"
                let Test.10 = 0i64;
                let Test.11 = 3i64;
                let Test.1 = Just Test.10 Test.11;
                let Test.5 = true;
                let Test.7 = Index 0 Test.1;
                let Test.6 = 0i64;
                let Test.8 = lowlevel Eq Test.6 Test.7;
                let Test.4 = lowlevel And Test.8 Test.5;
                if Test.4 then
                    let Test.0 = Index 1 Test.1;
                    ret Test.0;
                else
                    let Test.3 = 0i64;
                    ret Test.3;
                "#
            ),
        )
    }

    #[test]
    fn ir_when_these() {
        // NOTE apparently loading the tag_id is not required?
        compiles_to_ir(
            r#"
            when These 1 2 is
                This x -> x
                That y -> y
                These x _ -> x
            "#,
            indoc!(
                r#"
                let Test.7 = 1i64;
                let Test.8 = 1i64;
                let Test.9 = 2i64;
                let Test.3 = These Test.7 Test.8 Test.9;
                switch Test.3:
                    case 2:
                        let Test.0 = Index 1 Test.3;
                        ret Test.0;

                    case 0:
                        let Test.1 = Index 1 Test.3;
                        ret Test.1;

                    default:
                        let Test.2 = Index 1 Test.3;
                        ret Test.2;

                "#
            ),
        )
    }

    #[test]
    fn ir_when_record() {
        // NOTE apparently loading the tag_id is not required?
        compiles_to_ir(
            r#"
            when { x: 1, y: 3.14 } is
                { x } -> x
            "#,
            indoc!(
                r#"
                let Test.4 = 1i64;
                let Test.5 = 3.14f64;
                let Test.1 = Struct {Test.4, Test.5};
                let Test.0 = Index 0 Test.1;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn ir_plus() {
        compiles_to_ir(
            r#"
            1 + 2
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.3 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.3;

                let Test.1 = 1i64;
                let Test.2 = 2i64;
                let Test.0 = CallByName Num.14 Test.1 Test.2;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn ir_round() {
        compiles_to_ir(
            r#"
            Num.round 3.6
            "#,
            indoc!(
                r#"
                procedure Num.36 (#Attr.2):
                    let Test.2 = lowlevel NumRound #Attr.2;
                    ret Test.2;

                let Test.1 = 3.6f64;
                let Test.0 = CallByName Num.36 Test.1;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn ir_when_idiv() {
        compiles_to_ir(
            r#"
            when 1000 // 10 is
                Ok val -> val
                Err _ -> -1
            "#,
            indoc!(
                r#"
            procedure Num.32 (#Attr.2, #Attr.3):
                let Test.19 = 0i64;
                let Test.15 = lowlevel NotEq #Attr.3 Test.19;
                if Test.15 then
                    let Test.17 = 1i64;
                    let Test.18 = lowlevel NumDivUnchecked #Attr.2 #Attr.3;
                    let Test.16 = Ok Test.17 Test.18;
                    ret Test.16;
                else
                    let Test.13 = 0i64;
                    let Test.14 = Struct {};
                    let Test.12 = Err Test.13 Test.14;
                    ret Test.12;

            let Test.10 = 1000i64;
            let Test.11 = 10i64;
            let Test.1 = CallByName Num.32 Test.10 Test.11;
            let Test.5 = true;
            let Test.7 = Index 0 Test.1;
            let Test.6 = 1i64;
            let Test.8 = lowlevel Eq Test.6 Test.7;
            let Test.4 = lowlevel And Test.8 Test.5;
            if Test.4 then
                let Test.0 = Index 1 Test.1;
                ret Test.0;
            else
                let Test.3 = -1i64;
                ret Test.3;
            "#
            ),
        )
    }

    #[test]
    fn ir_two_defs() {
        compiles_to_ir(
            r#"
            x = 3
            y = 4

            x + y
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.3 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.3;

                let Test.1 = 4i64;
                let Test.0 = 3i64;
                let Test.2 = CallByName Num.14 Test.0 Test.1;
                ret Test.2;
                "#
            ),
        )
    }

    #[test]
    fn ir_when_just() {
        compiles_to_ir(
            r#"
            x : [ Nothing, Just Int ]
            x = Just 41

            when x is
                Just v -> v + 0x1
                Nothing -> 0x1
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.4 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.4;

                let Test.12 = 0i64;
                let Test.13 = 41i64;
                let Test.0 = Just Test.12 Test.13;
                let Test.7 = true;
                let Test.9 = Index 0 Test.0;
                let Test.8 = 0i64;
                let Test.10 = lowlevel Eq Test.8 Test.9;
                let Test.6 = lowlevel And Test.10 Test.7;
                if Test.6 then
                    let Test.1 = Index 1 Test.0;
                    let Test.3 = 1i64;
                    let Test.2 = CallByName Num.14 Test.1 Test.3;
                    ret Test.2;
                else
                    let Test.5 = 1i64;
                    ret Test.5;
                "#
            ),
        )
    }

    #[test]
    fn one_element_tag() {
        compiles_to_ir(
            r#"
            x : [ Pair Int ]
            x = Pair 2

            x
            "#,
            indoc!(
                r#"
                let Test.2 = 2i64;
                let Test.0 = Pair Test.2;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn join_points() {
        compiles_to_ir(
            r#"
            x =
                if True then 1 else 2

            x
            "#,
            indoc!(
                r#"
                let Test.3 = true;
                if Test.3 then
                    let Test.0 = 1i64;
                    jump Test.2;
                else
                    let Test.0 = 2i64;
                    jump Test.2;
                joinpoint Test.2:
                    ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn guard_pattern_true() {
        compiles_to_ir(
            r#"
            when 2 is
                2 if False -> 42
                _ -> 0
            "#,
            indoc!(
                r#"
                let Test.0 = 2i64;
                let Test.6 = true;
                let Test.10 = lowlevel Eq Test.6 Test.2;
                let Test.9 = lowlevel And Test.10 Test.5;
                let Test.7 = 2i64;
                let Test.8 = lowlevel Eq Test.7 Test.0;
                let Test.5 = lowlevel And Test.8 Test.6;
                let Test.2 = true;
                jump Test.3;
                joinpoint Test.3:
                    if Test.5 then
                        let Test.1 = 42i64;
                        ret Test.1;
                    else
                        let Test.4 = 0i64;
                        ret Test.4;
               "#
            ),
        )
    }

    #[test]
    fn when_on_record() {
        compiles_to_ir(
            r#"
            when { x: 0x2 } is
                { x } -> x + 3
            "#,
            indoc!(
                r#"
                let Test.5 = 2i64;
                let Test.1 = Struct {Test.5};
                let Test.0 = Index 0 Test.1;
                let Test.3 = 3i64;
                let Test.2 = CallByName Num.14 Test.0 Test.3;
                ret Test.2;
                "#
            ),
        )
    }

    #[test]
    fn let_on_record() {
        compiles_to_ir(
            r#"
            { x } = { x: 0x2, y: 3.14 }

            x
            "#,
            indoc!(
                r#"
                let Test.4 = 2i64;
                let Test.5 = 3.14f64;
                let Test.1 = Struct {Test.4, Test.5};
                let Test.0 = Index 0 Test.1;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn when_nested_maybe() {
        compiles_to_ir(
            r#"
            Maybe a : [ Nothing, Just a ]

            x : Maybe (Maybe Int)
            x = Just (Just 41)

            when x is
                Just (Just v) -> v + 0x1
                _ -> 0x1
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.5 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.5;
 
                let Test.16 = 2i64;
                let Test.17 = 3i64;
                let Test.2 = Struct {Test.16, Test.17};
                let Test.7 = true;
                let Test.8 = 4i64;
                let Test.9 = Index 0 Test.2;
                let Test.14 = lowlevel Eq Test.8 Test.9;
                let Test.13 = lowlevel And Test.14 Test.6;
                let Test.10 = 3i64;
                let Test.11 = Index 1 Test.2;
                let Test.12 = lowlevel Eq Test.10 Test.11;
                let Test.6 = lowlevel And Test.12 Test.7;
                if Test.6 then
                    let Test.3 = 9i64;
                    ret Test.3;
                else
                    let Test.1 = Index 1 Test.2;
                    let Test.0 = Index 0 Test.2;
                    let Test.4 = CallByName Num.14 Test.0 Test.1;
                    ret Test.4;
                "#
            ),
        )
    }

    #[test]
    fn when_on_two_values() {
        compiles_to_ir(
            r#"
            when Pair 2 3 is
                Pair 4 3 -> 9
                Pair a b -> a + b
            "#,
            indoc!(
                r#"
                procedure Num.14 (#Attr.2, #Attr.3):
                    let Test.5 = lowlevel NumAdd #Attr.2 #Attr.3;
                    ret Test.5;
 
                let Test.16 = 2i64;
                let Test.17 = 3i64;
                let Test.2 = Struct {Test.16, Test.17};
                let Test.7 = true;
                let Test.8 = 4i64;
                let Test.9 = Index 0 Test.2;
                let Test.14 = lowlevel Eq Test.8 Test.9;
                let Test.13 = lowlevel And Test.14 Test.6;
                let Test.10 = 3i64;
                let Test.11 = Index 1 Test.2;
                let Test.12 = lowlevel Eq Test.10 Test.11;
                let Test.6 = lowlevel And Test.12 Test.7;
                if Test.6 then
                    let Test.3 = 9i64;
                    ret Test.3;
                else
                    let Test.1 = Index 1 Test.2;
                    let Test.0 = Index 0 Test.2;
                    let Test.4 = CallByName Num.14 Test.0 Test.1;
                    ret Test.4;
                "#
            ),
        )
    }

    #[test]
    fn list_push() {
        compiles_to_ir(
            r#"
            List.push [1] 2
            "#,
            indoc!(
                r#"
                procedure List.5 (#Attr.2, #Attr.3):
                    let Test.3 = lowlevel ListPush #Attr.2 #Attr.3;
                    ret Test.3;

                let Test.4 = 1i64;
                let Test.1 = Array [Test.4];
                let Test.2 = 2i64;
                let Test.0 = CallByName List.5 Test.1 Test.2;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn when_joinpoint() {
        compiles_to_ir(
            r#"
            x : [ Red, White, Blue ]
            x = Blue

            y = 
                when x is
                    Red -> 1
                    White -> 2
                    Blue -> 3

            y
            "#,
            indoc!(
                r#"
                procedure List.5 (#Attr.2, #Attr.3):
                    let Test.3 = lowlevel ListPush #Attr.2 #Attr.3;
                    ret Test.3;

                let Test.4 = 1i64;
                let Test.1 = Array [Test.4];
                let Test.2 = 2i64;
                let Test.0 = CallByName List.5 Test.1 Test.2;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn if_multi_branch() {
        compiles_to_ir(
            r#"
            if True then
                1
            else if False then
                2
            else 
                3
            "#,
            indoc!(
                r#"
                procedure List.5 (#Attr.2, #Attr.3):
                    let Test.3 = lowlevel ListPush #Attr.2 #Attr.3;
                    ret Test.3;

                let Test.4 = 1i64;
                let Test.1 = Array [Test.4];
                let Test.2 = 2i64;
                let Test.0 = CallByName List.5 Test.1 Test.2;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn when_on_result() {
        compiles_to_ir(
            r#"
            x : Result Int Int
            x = Ok 2

            y = when x is
                    Ok 3 -> 1
                    Ok _ -> 2
                    Err _ -> 3
            y
            "#,
            indoc!(
                r#"
                procedure List.5 (#Attr.2, #Attr.3):
                    let Test.3 = lowlevel ListPush #Attr.2 #Attr.3;
                    ret Test.3;

                let Test.4 = 1i64;
                let Test.1 = Array [Test.4];
                let Test.2 = 2i64;
                let Test.0 = CallByName List.5 Test.1 Test.2;
                ret Test.0;
                "#
            ),
        )
    }

    #[test]
    fn let_with_record_pattern() {
        compiles_to_ir(
            r#"
            { x } = { x: 0x2, y: 3.14 }

            x
            "#,
            indoc!(
                r#"
                let Test.6 = 2i64;
                let Test.7 = 3.14f64;
                let Test.1 = Struct {Test.6, Test.7};
                let Test.0 = Index 0 Test.1;
                jump Test.3 Test.0;
                joinpoint Test.3 Test.2:
                    ret Test.2;
                "#
            ),
        )
    }
}
