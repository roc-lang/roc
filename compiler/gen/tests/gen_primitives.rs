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
mod gen_primitives {
    use crate::helpers::{can_expr, infer_expr, uniq_expr, with_larger_debug_stack, CanExprOut};
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
    use std::ffi::{CStr, CString};
    use std::os::raw::c_char;

    #[test]
    fn basic_str() {
        assert_evals_to!(
            "\"shirt and hat\"",
            CString::new("shirt and hat").unwrap().as_c_str(),
            *const c_char,
            CStr::from_ptr
        );
    }

    #[test]
    fn basic_int() {
        assert_evals_to!("123", 123, i64);
    }

    #[test]
    fn basic_float() {
        assert_evals_to!("1234.0", 1234.0, f64);
    }

    #[test]
    fn branch_first_float() {
        assert_evals_to!(
            indoc!(
                r#"
                    when 1.23 is
                        1.23 -> 12
                        _ -> 34
                "#
            ),
            12,
            i64
        );
    }

    #[test]
    fn branch_second_float() {
        assert_evals_to!(
            indoc!(
                r#"
                    when 2.34 is
                        1.23 -> 63
                        _ -> 48
                "#
            ),
            48,
            i64
        );
    }

    #[test]
    fn branch_third_float() {
        assert_evals_to!(
            indoc!(
                r#"
                   when 10.0 is
                       1.0 -> 63
                       2.0 -> 48
                       _ -> 112
                "#
            ),
            112,
            i64
        );
    }

    #[test]
    fn branch_first_int() {
        assert_evals_to!(
            indoc!(
                r#"
                    when 1 is
                        1 -> 12
                        _ -> 34
                "#
            ),
            12,
            i64
        );
    }

    #[test]
    fn branch_second_int() {
        assert_evals_to!(
            indoc!(
                r#"
                    when 2 is
                        1 -> 63
                        _ -> 48
                "#
            ),
            48,
            i64
        );
    }

    #[test]
    fn branch_third_int() {
        assert_evals_to!(
            indoc!(
                r#"
                    when 10 is
                        1 -> 63
                        2 -> 48
                        _ -> 112
                "#
            ),
            112,
            i64
        );
    }

    #[test]
    fn branch_store_variable() {
        assert_evals_to!(
            indoc!(
                r#"
                    when 0 is
                        1 -> 12
                        a -> a
                "#
            ),
            0,
            i64
        );
    }

    #[test]
    fn when_one_element_tag() {
        assert_evals_to!(
            indoc!(
                r#"
                x : [ Pair Int Int ]
                x = Pair 0x2 0x3

                when x is
                    Pair l r -> l + r
                "#
            ),
            5,
            i64
        );
    }

    #[test]
    fn when_two_element_tag_first() {
        assert_evals_to!(
            indoc!(
                r#"
                x : [A Int, B Int]
                x = A 0x2

                when x is
                    A v -> v
                    B v -> v
                "#
            ),
            2,
            i64
        );
    }

    #[test]
    fn when_two_element_tag_second() {
        assert_evals_to!(
            indoc!(
                r#"
                x : [A Int, B Int]
                x = B 0x3

                when x is
                    A v -> v
                    B v -> v
                "#
            ),
            3,
            i64
        );
    }

    #[test]
    fn gen_when_one_branch() {
        assert_evals_to!(
            indoc!(
                r#"
                    when 3.14 is
                        _ -> 23
                "#
            ),
            23,
            i64
        );
    }

    #[test]
    fn gen_large_when_int() {
        assert_evals_to!(
            indoc!(
                r#"
                    foo = \num ->
                        when num is
                            0 -> 200
                            -3 -> 111 # TODO adding more negative numbers reproduces parsing bugs here
                            3 -> 789
                            1 -> 123
                            2 -> 456
                            _ -> 1000

                    foo -3
                "#
            ),
            111,
            i64
        );
    }

    // #[test]
    // fn gen_large_when_float() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 foo = \num ->
    //                     when num is
    //                         0.5 -> 200.1
    //                         -3.6 -> 111.2 # TODO adding more negative numbers reproduces parsing bugs here
    //                         3.6 -> 789.5
    //                         1.7 -> 123.3
    //                         2.8 -> 456.4
    //                         _ -> 1000.6

    //                 foo -3.6
    //             "#
    //         ),
    //         111.2,
    //         f64
    //     );
    // }

    #[test]
    fn or_pattern() {
        assert_evals_to!(
            indoc!(
                r#"
                when 2 is
                    1 | 2 -> 42
                    _ -> 1
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn apply_identity() {
        assert_evals_to!(
            indoc!(
                r#"
                    identity = \a -> a

                    identity 5
                "#
            ),
            5,
            i64
        );
    }

    #[test]
    fn apply_unnamed_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    (\a -> a) 5
                "#
            ),
            5,
            i64
        );
    }

    #[test]
    fn return_unnamed_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    alwaysFloatIdentity : Int -> (Float -> Float)
                    alwaysFloatIdentity = \num ->
                        (\a -> a)

                    (alwaysFloatIdentity 2) 3.14
                "#
            ),
            3.14,
            f64
        );
    }

    #[test]
    fn gen_when_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    limitedNegate = \num ->
                        when num is
                            1 -> -1
                            -1 -> 1
                            _ -> num

                    limitedNegate 1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn gen_basic_def() {
        assert_evals_to!(
            indoc!(
                r#"
                    answer = 42

                    answer
                "#
            ),
            42,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                    pi = 3.14

                    pi
                "#
            ),
            3.14,
            f64
        );
    }

    #[test]
    fn gen_multiple_defs() {
        assert_evals_to!(
            indoc!(
                r#"
                    answer = 42

                    pi = 3.14

                    answer
                "#
            ),
            42,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                    answer = 42

                    pi = 3.14

                    pi
                "#
            ),
            3.14,
            f64
        );
    }

    #[test]
    fn gen_chained_defs() {
        with_larger_debug_stack(|| {
            assert_evals_to!(
                indoc!(
                    r#"
                        x = i1
                        i3 = i2
                        i1 = 1337
                        i2 = i1
                        y = 12.4
    
                        i3
                    "#
                ),
                1337,
                i64
            );
        })
    }
    #[test]
    fn gen_nested_defs() {
        with_larger_debug_stack(|| {
            assert_evals_to!(
                indoc!(
                    r#"
                    x = 5

                    answer =
                        i3 = i2

                        nested =
                            a = 1.0
                            b = 5

                            i1

                        i1 = 1337
                        i2 = i1


                        nested

                    # None of this should affect anything, even though names
                    # overlap with the previous nested defs
                    unused =
                        nested = 17

                        i1 = 84.2

                        nested

                    y = 12.4

                    answer
                "#
                ),
                1337,
                i64
            );
        })
    }
}
