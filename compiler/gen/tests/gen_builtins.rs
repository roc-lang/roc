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
mod gen_builtins {
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
    fn f64_sqrt() {
        assert_evals_to!("Float.sqrt 144", 12.0, f64);
    }

    #[test]
    fn f64_round() {
        assert_evals_to!("Float.round 3.6", 4, i64);
    }

    #[test]
    fn empty_list_literal() {
        assert_evals_to!("[]", &[], &'static [i64]);
    }

    #[test]
    fn int_list_literal() {
        assert_evals_to!("[ 12, 9, 6, 3 ]", &[12, 9, 6, 3], &'static [i64]);
    }

    #[test]
    fn gen_if_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    limitedNegate = \num ->
                        if num == 1 then
                            -1
                        else if num == -1 then
                            1
                        else
                            num

                    limitedNegate 1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn gen_float_eq() {
        assert_evals_to!(
            indoc!(
                r#"
                1.0 == 1.0
                "#
            ),
            true,
            bool
        );
    }

    #[test]
    fn gen_add_f64() {
        assert_evals_to!(
            indoc!(
                r#"
                    1.1 + 2.4 + 3
                "#
            ),
            6.5,
            f64
        );
    }

    #[test]
    fn gen_div_f64() {
        assert_evals_to!(
            indoc!(
                r#"
                    48 / 2
                "#
            ),
            24.0,
            f64
        );
    }

    #[test]
    fn gen_add_i64() {
        assert_evals_to!(
            indoc!(
                r#"
                    1 + 2 + 3
                "#
            ),
            6,
            i64
        );
    }

    #[test]
    fn gen_sub_f64() {
        assert_evals_to!(
            indoc!(
                r#"
                    1.5 - 2.4 - 3
                "#
            ),
            -3.9,
            f64
        );
    }

    #[test]
    fn gen_sub_i64() {
        assert_evals_to!(
            indoc!(
                r#"
                    1 - 2 - 3
                "#
            ),
            -4,
            i64
        );
    }

    #[test]
    fn gen_mul_i64() {
        assert_evals_to!(
            indoc!(
                r#"
                    2 * 4 * 6
                "#
            ),
            48,
            i64
        );
    }

    #[test]
    fn gen_order_of_arithmetic_ops() {
        assert_evals_to!(
            indoc!(
                r#"
                    1 + 3 * 7 - 2
                "#
            ),
            20,
            i64
        );
    }

    #[test]
    fn gen_order_of_arithmetic_ops_complex_float() {
        assert_evals_to!(
            indoc!(
                r#"
                    48 / 2 + 3
                "#
            ),
            27.0,
            f64
        );
    }

    #[test]
    fn if_guard_bind_variable() {
        assert_evals_to!(
            indoc!(
                r#"
                when 10 is
                    x if x == 5 -> 0
                    _ -> 42
                "#
            ),
            42,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                when 10 is
                    x if x == 10 -> 42
                    _ -> 0
                "#
            ),
            42,
            i64
        );
    }
    #[test]
    fn tail_call_elimination() {
        assert_evals_to!(
            indoc!(
                r#"
                sum = \n, accum ->
                    when n is
                        0 -> accum
                        _ -> sum (n - 1) (n + accum)

                sum 1_000_000 0
                "#
            ),
            500000500000,
            i64
        );
    }
    #[test]
    fn int_negate() {
        assert_evals_to!("Num.neg 123", -123, i64);
    }

    #[test]
    fn gen_basic_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    always42 : Num.Num Int.Integer -> Num.Num Int.Integer
                    always42 = \num -> 42

                    always42 5
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn empty_list_len() {
        assert_evals_to!("List.len []", 0, usize);
    }

    #[test]
    fn basic_int_list_len() {
        assert_evals_to!("List.len [ 12, 9, 6, 3 ]", 4, usize);
    }

    // #[test]
    // fn loaded_int_list_len() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 nums = [ 2, 4, 6 ]

    //                 List.len nums
    //             "#
    //         ),
    //         3,
    //         usize
    //     );
    // }

    // #[test]
    // fn fn_int_list_len() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 # TODO remove this annotation once monomorphization works!
    //                 getLen = \list -> List.len list

    //                 nums = [ 2, 4, 6 ]

    //                 getLen nums
    //             "#
    //         ),
    //         3,
    //         usize
    //     );
    // }

    //     #[test]
    //     fn int_list_is_empty() {
    //         assert_evals_to!("List.isEmpty [ 12, 9, 6, 3 ]", 0, u8, |x| x);
    //     }
    //

    #[test]
    fn head_int_list() {
        assert_evals_to!("List.getUnsafe [ 12, 9, 6, 3 ] 0", 12, i64);
    }

    #[test]
    fn get_int_list() {
        assert_evals_to!("List.getUnsafe [ 12, 9, 6 ] 1", 9, i64);
    }

    #[test]
    fn get_set_unique_int_list() {
        assert_evals_to!("List.getUnsafe (List.set [ 12, 9, 7, 3 ] 1 42) 1", 42, i64);
    }

    #[test]
    fn set_unique_int_list() {
        assert_evals_to!(
            "List.set [ 12, 9, 7, 1, 5 ] 2 33",
            &[12, 9, 33, 1, 5],
            &'static [i64]
        );
    }

    #[test]
    fn set_unique_list_oob() {
        assert_evals_to!(
            "List.set [ 3, 17, 4.1 ] 1337 9.25",
            &[3.0, 17.0, 4.1],
            &'static [f64]
        );
    }

    #[test]
    fn set_shared_int_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    shared = [ 2.1, 4.3 ]

                    # This should not mutate the original
                    x = List.getUnsafe (List.set shared 1 7.7) 1

                    { x, y: List.getUnsafe shared 1 }
                "#
            ),
            (7.7, 4.3),
            (f64, f64)
        );
    }

    #[test]
    fn set_shared_list_oob() {
        assert_evals_to!(
            indoc!(
                r#"
                    shared = [ 2, 4 ]

                    # This List.set is out of bounds, and should have no effect
                    x = List.getUnsafe (List.set shared 422 0) 1

                    { x, y: List.getUnsafe shared 1 }
                "#
            ),
            (4, 4),
            (i64, i64)
        );
    }

    #[test]
    fn get_unique_int_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    shared = [ 2, 4 ]

                    List.getUnsafe shared 1
                "#
            ),
            4,
            i64
        );
    }

    #[test]
    fn int_to_float() {
        assert_evals_to!(
            indoc!(
                r#"
                    Num.toFloat 0x9
                "#
            ),
            9.0,
            f64
        );
    }
}
