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
        assert_evals_to!("Num.sqrt 144", 12.0, f64);
    }

    #[test]
    fn f64_round() {
        assert_evals_to!("Num.round 3.6", 4, i64);
    }

    #[test]
    fn f64_abs() {
        assert_evals_to!("Num.abs -4.7", 4.7, f64);
        assert_evals_to!("Num.abs 5.8", 5.8, f64);
    }

    #[test]
    fn i64_abs() {
        assert_evals_to!("Num.abs -6", 6, i64);
        assert_evals_to!("Num.abs 7", 7, i64);
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
    fn gen_int_eq() {
        assert_evals_to!(
            indoc!(
                r#"
                4 == 4
                "#
            ),
            true,
            bool
        );
    }

    #[test]
    fn gen_int_neq() {
        assert_evals_to!(
            indoc!(
                r#"
                4 != 5
                "#
            ),
            true,
            bool
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
    fn gen_div_i64() {
        assert_evals_to!(
            indoc!(
                r#"
                    when 1000 // 10 is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            100,
            i64
        );
    }

    #[test]
    fn gen_div_by_zero_i64() {
        assert_evals_to!(
            indoc!(
                r#"
                    when 1000 // 0 is
                        Err DivByZero -> 99
                        _ -> -24
                "#
            ),
            99,
            i64
        );
    }

    #[test]
    fn gen_rem_i64() {
        assert_evals_to!(
            indoc!(
                r#"
                    when Num.rem 8 3 is
                        Ok val -> val
                        _ -> -1
                "#
            ),
            2,
            i64
        );
    }

    #[test]
    fn gen_rem_div_by_zero_i64() {
        assert_evals_to!(
            indoc!(
                r#"
                    when Num.rem 8 0 is
                        Err DivByZero -> 4
                        _ -> -23
                "#
            ),
            4,
            i64
        );
    }

    #[test]
    fn gen_is_zero_i64() {
        assert_evals_to!("Num.isZero 0", true, bool);
        assert_evals_to!("Num.isZero 1", false, bool);
    }

    #[test]
    fn gen_is_positive_i64() {
        assert_evals_to!("Num.isPositive 0", false, bool);
        assert_evals_to!("Num.isPositive 1", true, bool);
        assert_evals_to!("Num.isPositive -5", false, bool);
    }

    #[test]
    fn gen_is_negative_i64() {
        assert_evals_to!("Num.isNegative 0", false, bool);
        assert_evals_to!("Num.isNegative 3", false, bool);
        assert_evals_to!("Num.isNegative -2", true, bool);
    }

    #[test]
    fn gen_is_positive_f64() {
        assert_evals_to!("Num.isPositive 0.0", false, bool);
        assert_evals_to!("Num.isPositive 4.7", true, bool);
        assert_evals_to!("Num.isPositive -8.5", false, bool);
    }

    #[test]
    fn gen_is_negative_f64() {
        assert_evals_to!("Num.isNegative 0.0", false, bool);
        assert_evals_to!("Num.isNegative 9.9", false, bool);
        assert_evals_to!("Num.isNegative -4.4", true, bool);
    }

    #[test]
    fn gen_is_zero_f64() {
        assert_evals_to!("Num.isZero 0", true, bool);
        assert_evals_to!("Num.isZero 0_0", true, bool);
        assert_evals_to!("Num.isZero 0.0", true, bool);
        assert_evals_to!("Num.isZero 1", false, bool);
    }

    #[test]
    fn gen_is_odd() {
        assert_evals_to!("Num.isOdd 4", false, bool);
        assert_evals_to!("Num.isOdd 5", true, bool);
    }

    #[test]
    fn gen_is_even() {
        assert_evals_to!("Num.isEven 6", true, bool);
        assert_evals_to!("Num.isEven 7", false, bool);
    }

    #[test]
    fn sin() {
        assert_evals_to!("Num.sin 0", 0.0, f64);
        assert_evals_to!("Num.sin 1.41421356237", 0.9877659459922529, f64);
    }

    #[test]
    fn cos() {
        assert_evals_to!("Num.cos 0", 1.0, f64);
        assert_evals_to!("Num.cos 3.14159265359", -1.0, f64);
    }

    #[test]
    fn tan() {
        assert_evals_to!("Num.tan 0", 0.0, f64);
        assert_evals_to!("Num.tan 1", 1.557407724654902, f64);
    }

    #[test]
    fn lt_i64() {
        assert_evals_to!("1 < 2", true, bool);
        assert_evals_to!("1 < 1", false, bool);
        assert_evals_to!("2 < 1", false, bool);
        assert_evals_to!("0 < 0", false, bool);
    }

    #[test]
    fn lte_i64() {
        assert_evals_to!("1 <= 1", true, bool);
        assert_evals_to!("2 <= 1", false, bool);
        assert_evals_to!("1 <= 2", true, bool);
        assert_evals_to!("0 <= 0", true, bool);
    }

    #[test]
    fn gt_i64() {
        assert_evals_to!("2 > 1", true, bool);
        assert_evals_to!("2 > 2", false, bool);
        assert_evals_to!("1 > 1", false, bool);
        assert_evals_to!("0 > 0", false, bool);
    }

    #[test]
    fn gte_i64() {
        assert_evals_to!("1 >= 1", true, bool);
        assert_evals_to!("1 >= 2", false, bool);
        assert_evals_to!("2 >= 1", true, bool);
        assert_evals_to!("0 >= 0", true, bool);
    }

    #[test]
    fn lt_f64() {
        assert_evals_to!("1.1 < 1.2", true, bool);
        assert_evals_to!("1.1 < 1.1", false, bool);
        assert_evals_to!("1.2 < 1.1", false, bool);
        assert_evals_to!("0.0 < 0.0", false, bool);
    }

    #[test]
    fn lte_f64() {
        assert_evals_to!("1.1 <= 1.1", true, bool);
        assert_evals_to!("1.2 <= 1.1", false, bool);
        assert_evals_to!("1.1 <= 1.2", true, bool);
        assert_evals_to!("0.0 <= 0.0", true, bool);
    }

    #[test]
    fn gt_f64() {
        assert_evals_to!("2.2 > 1.1", true, bool);
        assert_evals_to!("2.2 > 2.2", false, bool);
        assert_evals_to!("1.1 > 2.2", false, bool);
        assert_evals_to!("0.0 > 0.0", false, bool);
    }

    #[test]
    fn gte_f64() {
        assert_evals_to!("1.1 >= 1.1", true, bool);
        assert_evals_to!("1.1 >= 1.2", false, bool);
        assert_evals_to!("1.2 >= 1.1", true, bool);
        assert_evals_to!("0.0 >= 0.0", true, bool);
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
                    always42 : Num.Num Num.Integer -> Num.Num Num.Integer
                    always42 = \num -> 42

                    always42 5
                "#
            ),
            42,
            i64
        );
    }

    // #[test]
    // fn list_push() {
    //     assert_evals_to!("List.push [] 1", &[1], &'static [i64]);
    // }

    #[test]
    fn list_single() {
        assert_evals_to!("List.single 1", &[1], &'static [i64]);
        assert_evals_to!("List.single 5.6", &[5.6], &'static [f64]);
    }

    #[test]
    fn empty_list_len() {
        assert_evals_to!("List.len []", 0, usize);
    }

    #[test]
    fn basic_int_list_len() {
        assert_evals_to!("List.len [ 12, 9, 6, 3 ]", 4, usize);
    }

    #[test]
    fn loaded_int_list_len() {
        assert_evals_to!(
            indoc!(
                r#"
                    nums = [ 2, 4, 6 ]

                    List.len nums
                "#
            ),
            3,
            usize
        );
    }

    #[test]
    fn fn_int_list_len() {
        assert_evals_to!(
            indoc!(
                r#"
                    getLen = \list -> List.len list

                    nums = [ 2, 4, 6, 8 ]

                    getLen nums
                "#
            ),
            4,
            usize
        );
    }

    #[test]
    fn int_list_is_empty() {
        assert_evals_to!("List.isEmpty [ 12, 9, 6, 3 ]", false, bool);
    }

    #[test]
    fn empty_list_is_empty() {
        assert_evals_to!("List.isEmpty []", true, bool);
    }

    #[test]
    fn first_int_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    when List.first [ 12, 9, 6, 3 ] is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            12,
            i64
        );
    }

    #[test]
    fn first_wildcard_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    when List.first [] is
                        Ok _ -> 5
                        Err _ -> -1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn first_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    when List.first [] is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn get_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                   when List.get [] 0 is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn get_wildcard_empty_list() {
        assert_evals_to!(
            indoc!(
                r#"
                   when List.get [] 0 is
                        Ok _ -> 5
                        Err _ -> -1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn get_int_list_ok() {
        assert_evals_to!(
            indoc!(
                r#"
                    when List.get [ 12, 9, 6 ] 1 is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            9,
            i64
        );
    }

    #[test]
    fn get_int_list_oob() {
        assert_evals_to!(
            indoc!(
                r#"
                    when List.get [ 12, 9, 6 ] 1000 is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            -1,
            i64
        );
    }

    #[test]
    fn get_set_unique_int_list() {
        assert_evals_to!(
            indoc!(
                r#"
                    when List.get (List.set [ 12, 9, 7, 3 ] 1 42) 1 is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            42,
            i64
        );
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
                    x =
                        when List.get (List.set shared 1 7.7) 1 is
                            Ok num -> num
                            Err _ -> 0

                    y =
                        when List.get shared 1 is
                            Ok num -> num
                            Err _ -> 0

                    { x, y }
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
                    x =
                        when List.get (List.set shared 422 0) 1 is
                            Ok num -> num
                            Err _ -> 0

                    y =
                        when List.get shared 1 is
                            Ok num -> num
                            Err _ -> 0

                    { x, y }
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
                    unique = [ 2, 4 ]

                    when List.get unique 1 is
                        Ok num -> num
                        Err _ -> -1
                "#
            ),
            4,
            i64
        );
    }

    #[test]
    fn int_to_float() {
        assert_evals_to!("Num.toFloat 0x9", 9.0, f64);
    }

    #[test]
    fn num_to_float() {
        assert_evals_to!("Num.toFloat 9", 9.0, f64);
    }

    #[test]
    fn float_to_float() {
        assert_evals_to!("Num.toFloat 0.5", 0.5, f64);
    }

    #[test]
    fn gen_quicksort() {
        if true {
            todo!("fix gen_quicksort");
        }

        assert_evals_to!(
            indoc!(
                r#"
                    quicksort : List (Num a) -> List (Num a)
                    quicksort = \list ->
                        quicksortHelp list 0 (List.len list - 1)


                    quicksortHelp : List (Num a), Int, Int -> List (Num a)
                    quicksortHelp = \list, low, high ->
                        if low < high then
                            when partition low high list is
                                Pair partitionIndex partitioned ->
                                    partitioned
                                        |> quicksortHelp low (partitionIndex - 1)
                                        |> quicksortHelp (partitionIndex + 1) high
                        else
                            list


                    swap : Int, Int, List a -> List a
                    swap = \i, j, list ->
                        when Pair (List.get list i) (List.get list j) is
                            Pair (Ok atI) (Ok atJ) ->
                                list
                                    |> List.set i atJ
                                    |> List.set j atI

                            _ ->
                                []

                    partition : Int, Int, List (Num a) -> [ Pair Int (List (Num a)) ]
                    partition = \low, high, initialList ->
                        when List.get initialList high is
                            Ok pivot ->
                                when partitionHelp (low - 1) low initialList high pivot is
                                    Pair newI newList ->
                                        Pair (newI + 1) (swap (newI + 1) high newList)

                            Err _ ->
                                Pair (low - 1) initialList


                    partitionHelp : Int, Int, List (Num a), Int, Int -> [ Pair Int (List (Num a)) ]
                    partitionHelp = \i, j, list, high, pivot ->
                        if j < high then
                            when List.get list j is
                                Ok value ->
                                    if value <= pivot then
                                        partitionHelp (i + 1) (j + 1) (swap (i + 1) j list) high pivot
                                    else
                                        partitionHelp i (j + 1) list high pivot

                                Err _ ->
                                    Pair i list
                        else
                            Pair i list



                    quicksort [ 7, 4, 21, 19 ]
                "#
            ),
            &[4, 7, 19, 21],
            &'static [i64]
        );
    }
}
