#[macro_use]
extern crate pretty_assertions;
#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate inkwell;
extern crate libc;
extern crate roc_gen;

#[macro_use]
mod helpers;

#[cfg(test)]
mod gen_tags {
    use crate::helpers::{can_expr, infer_expr, uniq_expr, CanExprOut};
    use bumpalo::Bump;
    use inkwell::context::Context;
    use inkwell::execution_engine::JitFunction;
    use inkwell::passes::PassManager;
    use inkwell::types::BasicType;
    use inkwell::OptimizationLevel;
    use roc_collections::all::ImMap;
    use roc_gen::llvm::build::{build_proc, build_proc_header};
    use roc_gen::llvm::convert::basic_type_from_layout;
    use roc_mono::expr::{Expr, Procs};
    use roc_mono::layout::Layout;
    use roc_types::subs::Subs;

    #[test]
    fn applied_tag_nothing() {
        assert_evals_to!(
            indoc!(
                r#"
                Maybe a : [ Just a, Nothing ]

                x : Maybe Int
                x = Nothing

                0x1
                "#
            ),
            1,
            i64
        );
    }

    #[test]
    fn applied_tag_just() {
        assert_evals_to!(
            indoc!(
                r#"
                Maybe a : [ Just a, Nothing ]

                y : Maybe Int
                y = Just 0x4

                0x1
                "#
            ),
            1,
            i64
        );
    }

    #[test]
    fn applied_tag_just_unit() {
        assert_evals_to!(
            indoc!(
                r#"
                Fruit : [ Orange, Apple, Banana ]
                Maybe a : [ Just a, Nothing ]

                orange : Fruit
                orange = Orange

                y : Maybe Fruit
                y = Just orange

                0x1
                "#
            ),
            1,
            i64
        );
    }

    // #[test]
    // fn raw_result() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             x : Result Int Int
    //             x = Err 41

    //             x
    //             "#
    //         ),
    //         0,
    //         i8
    //     );
    // }

    #[test]
    fn true_is_true() {
        assert_evals_to!(
            indoc!(
                r#"
                   bool : [True, False]
                   bool = True

                   bool
                "#
            ),
            true,
            bool
        );
    }

    #[test]
    fn false_is_false() {
        assert_evals_to!(
            indoc!(
                r#"
                   bool : [True, False]
                   bool = False

                   bool
                "#
            ),
            false,
            bool
        );
    }

    #[test]
    fn basic_enum() {
        assert_evals_to!(
            indoc!(
                r#"
                Fruit : [ Apple, Orange, Banana ]

                apple : Fruit
                apple = Apple

                orange : Fruit
                orange = Orange

                apple == orange
                "#
            ),
            false,
            bool
        );
    }

    //    #[test]
    //    fn linked_list_empty() {
    //        assert_evals_to!(
    //            indoc!(
    //                r#"
    //                LinkedList a : [ Cons a (LinkedList a), Nil ]
    //
    //                empty : LinkedList Int
    //                empty = Nil
    //
    //                1
    //                "#
    //            ),
    //            1,
    //            i64
    //        );
    //    }
    //
    //    #[test]
    //    fn linked_list_singleton() {
    //        assert_evals_to!(
    //            indoc!(
    //                r#"
    //                LinkedList a : [ Cons a (LinkedList a), Nil ]
    //
    //                singleton : LinkedList Int
    //                singleton = Cons 0x1 Nil
    //
    //                1
    //                "#
    //            ),
    //            1,
    //            i64
    //        );
    //    }
    //
    //    #[test]
    //    fn linked_list_is_empty() {
    //        assert_evals_to!(
    //            indoc!(
    //                r#"
    //                LinkedList a : [ Cons a (LinkedList a), Nil ]
    //
    //                isEmpty : LinkedList a -> Bool
    //                isEmpty = \list ->
    //                    when list is
    //                        Nil -> True
    //                        Cons _ _ -> False
    //
    //                isEmpty (Cons 4 Nil)
    //                "#
    //            ),
    //            false,
    //            bool
    //        );
    //    }

    #[test]
    fn even_odd() {
        assert_evals_to!(
            indoc!(
                r#"
                even = \n ->
                    when n is
                        0 -> True
                        1 -> False
                        _ -> odd (n - 1)

                odd = \n ->
                    when n is
                        0 -> False
                        1 -> True
                        _ -> even (n - 1)

                odd 5 && even 42
                "#
            ),
            true,
            bool
        );
    }

    #[test]
    fn gen_literal_true() {
        assert_evals_to!(
            indoc!(
                r#"
                if True then -1 else 1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn gen_if_float() {
        assert_evals_to!(
            indoc!(
                r#"
                if True then -1.0 else 1.0
                "#
            ),
            -1.0,
            f64
        );
    }
    #[test]
    fn when_on_nothing() {
        assert_evals_to!(
            indoc!(
                r#"
                x : [ Nothing, Just Int ]
                x = Nothing

                when x is
                    Nothing -> 0x2
                    Just _ -> 0x1
                "#
            ),
            2,
            i64
        );
    }

    #[test]
    fn when_on_just() {
        assert_evals_to!(
            indoc!(
                r#"
                x : [ Nothing, Just Int ]
                x = Just 41

                when x is
                    Just v -> v + 0x1
                    Nothing -> 0x1
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn when_on_result() {
        assert_evals_to!(
            indoc!(
                r#"
                x : Result Int Int
                x = Err 41

                when x is
                    Err v ->  v + 1
                    Ok _ -> 1
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn when_on_these() {
        assert_evals_to!(
            indoc!(
                r#"
                These a b : [ This a, That b, These a b ]

                x : These Int Int
                x = These 0x3 0x2

                when x is
                    These a b -> a + b
                    That v -> 8
                    This v -> v
                "#
            ),
            5,
            i64
        );
    }

    #[test]
    fn match_on_two_values() {
        // this will produce a Chain internally
        assert_evals_to!(
            indoc!(
                r#"
                when Pair 2 3 is
                    Pair 4 3 -> 9
                    Pair a b -> a + b
                "#
            ),
            5,
            i64
        );
    }

    #[test]
    fn pair_with_guard_pattern() {
        assert_evals_to!(
            indoc!(
                r#"
                when Pair 2 3 is
                    Pair 4 _ -> 1
                    Pair 3 _ -> 2
                    Pair a b -> a + b
                "#
            ),
            5,
            i64
        );
    }

    #[test]
    fn result_with_guard_pattern() {
        // This test revealed an issue with hashing Test values
        assert_evals_to!(
            indoc!(
                r#"
            x : Result Int Int
            x = Ok 2

            when x is
                Ok 3 -> 1
                Ok _ -> 2
                Err _ -> 3
            "#
            ),
            2,
            i64
        );
    }

    #[test]
    fn maybe_is_just() {
        assert_evals_to!(
            indoc!(
                r#"
                Maybe a : [ Just a, Nothing ]

                isJust : Maybe a -> Bool
                isJust = \list ->
                    when list is
                        Nothing -> False
                        Just _ -> True

                isJust (Just 42)
                "#
            ),
            true,
            bool
        );
    }

    #[test]
    fn nested_pattern_match() {
        assert_evals_to!(
            indoc!(
                r#"
                Maybe a : [ Nothing, Just a ]

                x : Maybe (Maybe Int)
                x = Just (Just 41)

                when x is
                    Just (Just v) -> v + 0x1
                    _ -> 0x1
                "#
            ),
            42,
            i64
        );
    }
    #[test]
    fn if_guard_pattern_false() {
        assert_evals_to!(
            indoc!(
                r#"
                when 2 is
                    2 if False -> 0
                    _ -> 42
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn if_guard_pattern_true() {
        assert_evals_to!(
            indoc!(
                r#"
                when 2 is
                    2 if True -> 42
                    _ -> 0
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn if_guard_exhaustiveness() {
        assert_evals_to!(
            indoc!(
                r#"
                when 2 is
                    _ if False -> 0
                    _ -> 42
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn when_on_enum() {
        assert_evals_to!(
            indoc!(
                r#"
                Fruit : [ Apple, Orange, Banana ]

                apple : Fruit
                apple = Apple

                when apple is
                    Apple -> 1
                    Banana -> 2
                    Orange -> 3
                "#
            ),
            1,
            i64
        );
    }

    #[test]
    fn pattern_matching_unit() {
        assert_evals_to!(
            indoc!(
                r#"
                Unit : [ Unit ]

                f : Unit -> Int
                f = \Unit -> 42

                f Unit
                "#
            ),
            42,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                Unit : [ Unit ]

                x : Unit
                x = Unit

                when x is
                    Unit -> 42
                "#
            ),
            42,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                f : {} -> Int
                f = \{} -> 42

                f {}
                "#
            ),
            42,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                when {} is
                    {} -> 42
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn one_element_tag() {
        assert_evals_to!(
            indoc!(
                r#"
                x : [ Pair Int ]
                x = Pair 2

                0x3
                "#
            ),
            3,
            i64
        );
    }

    #[test]
    fn nested_tag_union() {
        assert_evals_to!(
            indoc!(
                r#"
                Maybe a : [ Nothing, Just a ]

                x : Maybe (Maybe a)
                x = Just (Just 41)

                5
                "#
            ),
            5,
            i64
        );
    }
    #[test]
    fn unit_type() {
        assert_evals_to!(
            indoc!(
                r#"
                Unit : [ Unit ]

                v : Unit
                v = Unit

                1
                "#
            ),
            1,
            i64
        );
    }

    #[test]
    fn nested_record_load() {
        assert_evals_to!(
            indoc!(
                r#"
                Maybe a : [ Nothing, Just a ]

                x = { a : { b : 0x5 } }

                y = x.a

                y.b
                "#
            ),
            5,
            i64
        );
    }

    #[test]
    fn either_with_payload() {
        assert_evals_to!(
            indoc!(
                r#"
                    if True then
                        Left 42
                    else
                        Right 7
                "#
            ),
            (0, 42),
            (u64, i64)
        );
    }

    #[test]
    fn maybe_just() {
        assert_evals_to!(
            indoc!(
                // r#"
                //     Maybe a : [ Just a, Nothing ]

                //     answer : Maybe Int
                //     answer = Just 42

                //     answer
                // "#
                r#"
                    Maybe a : [ Just a, Nothing ]

                    answer : Maybe Int
                    answer = Just 42

                    answer
                "#
            ),
            std::mem::transmute::<(u64, i64), [u8; 16]>((0, 42)),
            [u8; 16]
        );
    }

    // define { [16 x i8] } @"$Test.main"() {
    // main_entry:
    // %answer = alloca { [16 x i8] }
    // %0 = alloca { i64, i64 }
    // store { i64, i64 } { i64 0, i64 42 }, { i64, i64 }* %0
    // %1 = bitcast { i64, i64 }* %0 to [16 x i8]*
    // %2 = load [16 x i8], [16 x i8]* %1
    // %insert_field = insertvalue { [16 x i8] } zeroinitializer, [16 x i8] %2, 0
    // store { [16 x i8] } %insert_field, { [16 x i8] }* %answer
    // %answer1 = load { [16 x i8] }, { [16 x i8] }* %answer
    // ret { [16 x i8] } %answer1
    // }
    //
    //
    //
    // define { i64, i64 } @"$Test.main"() {
    // main_entry:
    //   %answer = alloca { i64, i64 }
    //   store { i64, i64 } { i64 0, i64 42 }, { i64, i64 }* %answer
    //   %answer1 = load { i64, i64 }, { i64, i64 }* %answer
    //   ret { i64, i64 } %answer1
    // }
    //
    //
    // define i64 @"$Test.main"() {
    // main_entry:
    //   %x = alloca i64
    //   %_1 = alloca i1
    //   %_0 = alloca { [16 x i8] }
    //   %answer = alloca { [16 x i8] }
    //   %0 = alloca { i64, i64 }
    //   store { i64, i64 } { i64 0, i64 42 }, { i64, i64 }* %0
    //   %1 = bitcast { i64, i64 }* %0 to [16 x i8]*
    //   %2 = load [16 x i8], [16 x i8]* %1
    //   %insert_field = insertvalue { [16 x i8] } zeroinitializer, [16 x i8] %2, 0
    //   store { [16 x i8] } %insert_field, { [16 x i8] }* %answer
    //   %answer1 = load { [16 x i8] }, { [16 x i8] }* %answer
    //   store { [16 x i8] } %answer1, { [16 x i8] }* %_0
    //   %_02 = load { [16 x i8] }, { [16 x i8] }* %_0
    //   %3 = alloca { [16 x i8] }
    //   store { [16 x i8] } %_02, { [16 x i8] }* %3
    //   %4 = bitcast { [16 x i8] }* %3 to { i64, i64 }*
    //   %5 = load { i64, i64 }, { i64, i64 }* %4
    //   %6 = extractvalue { i64, i64 } %5, 0
    //   %cmp_i64 = icmp eq i64 0, %6
    //   br i1 %cmp_i64, label %then, label %else

    // then:                                             ; preds = %main_entry
    //   br label %branchcont

    // else:                                             ; preds = %main_entry
    //   br label %branchcont

    // branchcont:                                       ; preds = %else, %then
    //   %branch = phi i1 [ true, %then ], [ false, %else ]
    //   store i1 %branch, i1* %_1
    //   %_13 = load i1, i1* %_1
    //   br i1 %_13, label %then4, label %else5

    // then4:                                            ; preds = %branchcont
    //   %_07 = load { [16 x i8] }, { [16 x i8] }* %_0
    //   %7 = alloca { [16 x i8] }
    //   store { [16 x i8] } %_07, { [16 x i8] }* %7
    //   %8 = bitcast { [16 x i8] }* %7 to { i64, i64 }*
    //   %9 = load { i64, i64 }, { i64, i64 }* %8
    //   %10 = extractvalue { i64, i64 } %9, 1
    //   store i64 %10, i64* %x
    //   %x8 = load i64, i64* %x
    //   br label %branchcont6

    // else5:                                            ; preds = %branchcont
    //   br label %branchcont6

    // branchcont6:                                      ; preds = %else5, %then4
    //   %branch9 = phi i64 [ %x8, %then4 ], [ 0, %else5 ]
    //   ret i64 %branch9
    // }

    #[test]
    fn maybe_nothing() {
        assert_evals_to!(
            indoc!(
                r#"
                    Maybe a : [ Just a, Nothing ]

                    answer : Maybe Int
                    answer = Nothing

                    answer
                "#
            ),
            (1, 0),
            (u64, i64)
        );
    }
}
