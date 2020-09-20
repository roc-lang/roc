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
mod gen_num {
    /*
    use inkwell::passes::PassManager;
    use inkwell::types::BasicType;
    use inkwell::OptimizationLevel;
    use roc_gen::llvm::build::{build_proc, build_proc_header};
    use roc_gen::llvm::convert::basic_type_from_layout;
    use roc_mono::layout::Layout;
    use roc_types::subs::Subs;
    */

    #[test]
    fn f64_sqrt() {
        // FIXME this works with normal types, but fails when checking uniqueness types
        assert_evals_to!(
            indoc!(
                r#"
                    when Num.sqrt 100 is
                        Ok val -> val
                        Err _ -> -1
                "#
            ),
            10.0,
            f64
        );
    }

    #[test]
    fn f64_round_old() {
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
        assert_evals_to!("Num.abs 0", 0, i64);
        assert_evals_to!("Num.abs -0", 0, i64);
        assert_evals_to!("Num.abs -1", 1, i64);
        assert_evals_to!("Num.abs 1", 1, i64);
        assert_evals_to!("Num.abs 9_000_000_000_000", 9_000_000_000_000, i64);
        assert_evals_to!("Num.abs -9_000_000_000_000", 9_000_000_000_000, i64);
    }

    #[test]
    fn gen_if_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    limitedNegate = \num ->
                        x = 
                            if num == 1 then
                                -1
                            else if num == -1 then
                                1
                            else
                                num
                        x

                    limitedNegate 1
                "#
            ),
            -1,
            i64
        );

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
    fn gen_wrap_add_nums() {
        assert_evals_to!(
            indoc!(
                r#"
                    add2 = \num1, num2 -> num1 + num2

                    add2 4 5
                "#
            ),
            9,
            i64
        );
    }

    #[test]
    fn gen_div_f64() {
        // FIXME this works with normal types, but fails when checking uniqueness types
        assert_evals_to!(
            indoc!(
                r#"
                    when 48 / 2 is
                        Ok val -> val
                        Err _ -> -1
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
    fn gen_wrap_int_neq() {
        assert_evals_to!(
            indoc!(
                r#"
                    wrappedNotEq : a, a -> Bool
                    wrappedNotEq = \num1, num2 ->
                        num1 != num2

                    wrappedNotEq 2 3
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
                        Err _ -> -1
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
                        Ok _ -> -23
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
                    3 - 48 * 2.0
                "#
            ),
            -93.0,
            f64
        );
    }

    #[test]
    fn if_guard_bind_variable_false() {
        assert_evals_to!(
            indoc!(
                r#"
                main = \{} -> 
                    when 10 is
                        x if x == 5 -> 0
                        _ -> 42

                main {}
                "#
            ),
            42,
            i64
        );
    }

    #[test]
    fn if_guard_bind_variable_true() {
        assert_evals_to!(
            indoc!(
                r#"
                main = \{} -> 
                    when 10 is
                        x if x == 10 -> 42
                        _ -> 0

                main {}
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
    fn gen_wrap_int_neg() {
        assert_evals_to!(
            indoc!(
                r#"
                    wrappedNeg = \num -> -num

                    wrappedNeg 3
                "#
            ),
            -3,
            i64
        );
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
    fn int_compare() {
        assert_evals_to!(
            indoc!(
                r#"
                when Num.compare 0 1 is
                    LT -> 0
                    EQ -> 1
                    GT -> 2
                "#
            ),
            0,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                when Num.compare 1 1 is
                    LT -> 0
                    EQ -> 1
                    GT -> 2
                "#
            ),
            1,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                when Num.compare 1 0 is
                    LT -> 0
                    EQ -> 1
                    GT -> 2
                "#
            ),
            2,
            i64
        );
    }

    #[test]
    fn float_compare() {
        assert_evals_to!(
            indoc!(
                r#"
                when Num.compare 0 3.14 is
                    LT -> 0
                    EQ -> 1
                    GT -> 2
                "#
            ),
            0,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                when Num.compare 3.14 3.14 is
                    LT -> 0
                    EQ -> 1
                    GT -> 2
                "#
            ),
            1,
            i64
        );

        assert_evals_to!(
            indoc!(
                r#"
                when Num.compare 3.14 0 is
                    LT -> 0
                    EQ -> 1
                    GT -> 2
                "#
            ),
            2,
            i64
        );
    }

    #[test]
    fn pow() {
        assert_evals_to!("Num.pow 2.0 2.0", 4.0, f64);
    }

    #[test]
    fn ceiling() {
        assert_evals_to!("Num.ceiling 1.1", 2, i64);
    }

    #[test]
    fn floor() {
        assert_evals_to!("Num.floor 1.9", 1, i64);
    }

    #[test]
    fn pow_int() {
        assert_evals_to!("Num.powInt 2 3", 8, i64);
    }

    #[test]
    #[should_panic(expected = r#"Roc failed with message: "integer addition overflowed!"#)]
    fn int_overflow() {
        assert_evals_to!(
            indoc!(
                r#"
                9_223_372_036_854_775_807 + 1
                "#
            ),
            0,
            i64
        );
    }
}
