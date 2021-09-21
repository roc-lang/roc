#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate indoc;

extern crate bumpalo;
extern crate libc;

#[macro_use]
mod helpers;

#[cfg(all(test, any(target_os = "linux", target_os = "macos"), any(target_arch = "x86_64"/*, target_arch = "aarch64"*/)))]
mod dev_num {
    #[test]
    fn i64_values() {
        assert_evals_to!("0", 0, i64);
        assert_evals_to!("-0", 0, i64);
        assert_evals_to!("-1", -1, i64);
        assert_evals_to!("1", 1, i64);
        assert_evals_to!("9_000_000_000_000", 9_000_000_000_000, i64);
        assert_evals_to!("-9_000_000_000_000", -9_000_000_000_000, i64);
        assert_evals_to!("0b1010", 0b1010, i64);
        assert_evals_to!("0o17", 0o17, i64);
        assert_evals_to!("0x1000_0000_0000_0000", 0x1000_0000_0000_0000, i64);
    }

    #[test]
    fn f64_values() {
        assert_evals_to!("0.0", 0.0, f64);
        assert_evals_to!("-0.0", 0.0, f64);
        assert_evals_to!("1.0", 1.0, f64);
        assert_evals_to!("-1.0", -1.0, f64);
        assert_evals_to!("3.1415926535897932", 3.141_592_653_589_793, f64);
        assert_evals_to!(&format!("{:0.1}", f64::MIN), f64::MIN, f64);
        assert_evals_to!(&format!("{:0.1}", f64::MAX), f64::MAX, f64);
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
    fn i64_force_stack() {
        // This claims 33 registers. One more than Arm and RISC-V, and many more than x86-64.
        assert_evals_to!(
            indoc!(
                r#"
                    a = 0
                    b = 1
                    c = 2
                    d = 3
                    e = 4
                    f = 5
                    g = 6
                    h = 7
                    i = 8
                    j = 9
                    k = 10
                    l = 11
                    m = 12
                    n = 13
                    o = 14
                    p = 15
                    q = 16
                    r = 17
                    s = 18
                    t = 19
                    u = 20
                    v = 21
                    w = 22
                    x = 23
                    y = 24
                    z = 25
                    aa = 26
                    ab = 27
                    ac = 28
                    ad = 29
                    ae = 30
                    af = 31
                    ag = 32

                    a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p + q + r + s + t + u + v + w + x + y + z + aa + ab + ac + ad + ae + af + ag
                "#
            ),
            528,
            i64
        );
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

        assert_evals_to!(
            indoc!(
                r#"
                    3 == 4
                "#
            ),
            false,
            bool
        );
    }

    #[test]
    fn gen_basic_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    always42 : Num.Num (Num.Integer Num.Signed64) -> Num.Num (Num.Integer Num.Signed64)
                    always42 = \_ -> 42

                    always42 5
                "#
            ),
            42,
            i64
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
    fn gen_wrap_add_nums_force_stack() {
        assert_evals_to!(
            indoc!(
                r#"
                    add9 = \num1, num2, num3, num4, num5, num6, num7, num8, num9 -> num1 + num2 + num3 + num4 + num5 + num6 + num7 + num8 + num9

                    add9 1 2 3 4 5 6 7 8 9
                "#
            ),
            45,
            i64
        );
    }

    #[test]
    fn pow_int() {
        assert_evals_to!("Num.powInt 2 3", 8, i64);
    }

    #[test]
    fn acos() {
        assert_evals_to!("Num.acos 0.5", 1.0471975511965979, f64);
    }

    #[test]
    fn asin() {
        assert_evals_to!("Num.asin 0.5", 0.5235987755982989, f64);
    }

    #[test]
    fn atan() {
        assert_evals_to!("Num.atan 10", 1.4711276743037347, f64);
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
    }

    #[test]
    fn gen_fib_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    fib = \n ->
                        if n == 0 then
                            0
                        else if n == 1 then
                            1
                        else
                            (fib (n - 1)) + (fib (n - 2))

                    fib 10
                "#
            ),
            55,
            i64
        );
    }

    #[test]
    fn gen_fast_fib_fn() {
        assert_evals_to!(
            indoc!(
                r#"
                    fib = \n, a, b ->
                        if n == 0 then
                            a
                        else
                            fib (n - 1) b (a + b)
                    fib 10 0 1
                "#
            ),
            55,
            i64
        );
    }

    #[test]
    fn f64_abs() {
        assert_evals_to!("Num.abs -4.7", 4.7, f64);
        assert_evals_to!("Num.abs 5.8", 5.8, f64);
    }

    #[test]
    fn f64_round() {
        assert_evals_to!("Num.round 3.6", 4, i64);
        assert_evals_to!("Num.round 3.4", 3, i64);
        assert_evals_to!("Num.round 2.5", 3, i64);
        assert_evals_to!("Num.round -2.3", -2, i64);
        assert_evals_to!("Num.round -2.5", -3, i64);
    }

    // #[test]
    // fn f64_sqrt() {
    //     // FIXME this works with normal types, but fails when checking uniqueness types
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 when Num.sqrt 100 is
    //                     Ok val -> val
    //                     Err _ -> -1
    //             "#
    //         ),
    //         10.0,
    //         f64
    //     );
    // }

    // #[test]
    // fn gen_float_eq() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 1.0 == 1.0
    //             "#
    //         ),
    //         true,
    //         bool
    //     );
    // }

    // #[test]
    // fn gen_div_f64() {
    //     // FIXME this works with normal types, but fails when checking uniqueness types
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 when 48 / 2 is
    //                     Ok val -> val
    //                     Err _ -> -1
    //             "#
    //         ),
    //         24.0,
    //         f64
    //     );
    // }

    // #[test]
    // fn gen_int_neq() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 4 != 5
    //             "#
    //         ),
    //         true,
    //         bool
    //     );
    // }

    // #[test]
    // fn gen_wrap_int_neq() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 wrappedNotEq : a, a -> Bool
    //                 wrappedNotEq = \num1, num2 ->
    //                     num1 != num2

    //                 wrappedNotEq 2 3
    //             "#
    //         ),
    //         true,
    //         bool
    //     );
    // }

    // #[test]
    // fn gen_sub_f64() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 1.5 - 2.4 - 3
    //             "#
    //         ),
    //         -3.9,
    //         f64
    //     );
    // }

    // #[test]
    // fn gen_div_i64() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 when 1000 // 10 is
    //                     Ok val -> val
    //                     Err _ -> -1
    //             "#
    //         ),
    //         100,
    //         i64
    //     );
    // }

    // #[test]
    // fn gen_div_by_zero_i64() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 when 1000 // 0 is
    //                     Err DivByZero -> 99
    //                     _ -> -24
    //             "#
    //         ),
    //         99,
    //         i64
    //     );
    // }

    // #[test]
    // fn gen_rem_i64() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 when Num.rem 8 3 is
    //                     Ok val -> val
    //                     Err _ -> -1
    //             "#
    //         ),
    //         2,
    //         i64
    //     );
    // }

    // #[test]
    // fn gen_rem_div_by_zero_i64() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 when Num.rem 8 0 is
    //                     Err DivByZero -> 4
    //                     Ok _ -> -23
    //             "#
    //         ),
    //         4,
    //         i64
    //     );
    // }

    // #[test]
    // fn gen_is_zero_i64() {
    //     assert_evals_to!("Num.isZero 0", true, bool);
    //     assert_evals_to!("Num.isZero 1", false, bool);
    // }

    // #[test]
    // fn gen_is_positive_i64() {
    //     assert_evals_to!("Num.isPositive 0", false, bool);
    //     assert_evals_to!("Num.isPositive 1", true, bool);
    //     assert_evals_to!("Num.isPositive -5", false, bool);
    // }

    // #[test]
    // fn gen_is_negative_i64() {
    //     assert_evals_to!("Num.isNegative 0", false, bool);
    //     assert_evals_to!("Num.isNegative 3", false, bool);
    //     assert_evals_to!("Num.isNegative -2", true, bool);
    // }

    // #[test]
    // fn gen_is_positive_f64() {
    //     assert_evals_to!("Num.isPositive 0.0", false, bool);
    //     assert_evals_to!("Num.isPositive 4.7", true, bool);
    //     assert_evals_to!("Num.isPositive -8.5", false, bool);
    // }

    // #[test]
    // fn gen_is_negative_f64() {
    //     assert_evals_to!("Num.isNegative 0.0", false, bool);
    //     assert_evals_to!("Num.isNegative 9.9", false, bool);
    //     assert_evals_to!("Num.isNegative -4.4", true, bool);
    // }

    // #[test]
    // fn gen_is_zero_f64() {
    //     assert_evals_to!("Num.isZero 0", true, bool);
    //     assert_evals_to!("Num.isZero 0_0", true, bool);
    //     assert_evals_to!("Num.isZero 0.0", true, bool);
    //     assert_evals_to!("Num.isZero 1", false, bool);
    // }

    // #[test]
    // fn gen_is_odd() {
    //     assert_evals_to!("Num.isOdd 4", false, bool);
    //     assert_evals_to!("Num.isOdd 5", true, bool);
    // }

    // #[test]
    // fn gen_is_even() {
    //     assert_evals_to!("Num.isEven 6", true, bool);
    //     assert_evals_to!("Num.isEven 7", false, bool);
    // }

    // #[test]
    // fn sin() {
    //     assert_evals_to!("Num.sin 0", 0.0, f64);
    //     assert_evals_to!("Num.sin 1.41421356237", 0.9877659459922529, f64);
    // }

    // #[test]
    // fn cos() {
    //     assert_evals_to!("Num.cos 0", 1.0, f64);
    //     assert_evals_to!("Num.cos 3.14159265359", -1.0, f64);
    // }

    // #[test]
    // fn tan() {
    //     assert_evals_to!("Num.tan 0", 0.0, f64);
    //     assert_evals_to!("Num.tan 1", 1.557407724654902, f64);
    // }

    // #[test]
    // fn lt_i64() {
    //     assert_evals_to!("1 < 2", true, bool);
    //     assert_evals_to!("1 < 1", false, bool);
    //     assert_evals_to!("2 < 1", false, bool);
    //     assert_evals_to!("0 < 0", false, bool);
    // }

    // #[test]
    // fn lte_i64() {
    //     assert_evals_to!("1 <= 1", true, bool);
    //     assert_evals_to!("2 <= 1", false, bool);
    //     assert_evals_to!("1 <= 2", true, bool);
    //     assert_evals_to!("0 <= 0", true, bool);
    // }

    // #[test]
    // fn gt_i64() {
    //     assert_evals_to!("2 > 1", true, bool);
    //     assert_evals_to!("2 > 2", false, bool);
    //     assert_evals_to!("1 > 1", false, bool);
    //     assert_evals_to!("0 > 0", false, bool);
    // }

    // #[test]
    // fn gte_i64() {
    //     assert_evals_to!("1 >= 1", true, bool);
    //     assert_evals_to!("1 >= 2", false, bool);
    //     assert_evals_to!("2 >= 1", true, bool);
    //     assert_evals_to!("0 >= 0", true, bool);
    // }

    // #[test]
    // fn lt_f64() {
    //     assert_evals_to!("1.1 < 1.2", true, bool);
    //     assert_evals_to!("1.1 < 1.1", false, bool);
    //     assert_evals_to!("1.2 < 1.1", false, bool);
    //     assert_evals_to!("0.0 < 0.0", false, bool);
    // }

    // #[test]
    // fn lte_f64() {
    //     assert_evals_to!("1.1 <= 1.1", true, bool);
    //     assert_evals_to!("1.2 <= 1.1", false, bool);
    //     assert_evals_to!("1.1 <= 1.2", true, bool);
    //     assert_evals_to!("0.0 <= 0.0", true, bool);
    // }

    // #[test]
    // fn gt_f64() {
    //     assert_evals_to!("2.2 > 1.1", true, bool);
    //     assert_evals_to!("2.2 > 2.2", false, bool);
    //     assert_evals_to!("1.1 > 2.2", false, bool);
    //     assert_evals_to!("0.0 > 0.0", false, bool);
    // }

    // #[test]
    // fn gte_f64() {
    //     assert_evals_to!("1.1 >= 1.1", true, bool);
    //     assert_evals_to!("1.1 >= 1.2", false, bool);
    //     assert_evals_to!("1.2 >= 1.1", true, bool);
    //     assert_evals_to!("0.0 >= 0.0", true, bool);
    // }

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

    // #[test]
    // fn gen_order_of_arithmetic_ops_complex_float() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 3 - 48 * 2.0
    //             "#
    //         ),
    //         -93.0,
    //         f64
    //     );
    // }

    // #[test]
    // fn if_guard_bind_variable_false() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             wrapper = \{} ->
    //                 when 10 is
    //                     x if x == 5 -> 0
    //                     _ -> 42

    //             wrapper {}
    //             "#
    //         ),
    //         42,
    //         i64
    //     );
    // }

    // #[test]
    // fn if_guard_bind_variable_true() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             wrapper = \{} ->
    //                 when 10 is
    //                     x if x == 10 -> 42
    //                     _ -> 0

    //             wrapper {}
    //             "#
    //         ),
    //         42,
    //         i64
    //     );
    // }

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

    // #[test]
    // fn int_negate() {
    //     assert_evals_to!("Num.neg 123", -123, i64);
    // }

    // #[test]
    // fn gen_wrap_int_neg() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //                 wrappedNeg = \num -> -num

    //                 wrappedNeg 3
    //             "#
    //         ),
    //         -3,
    //         i64
    //     );
    // }

    // #[test]
    // fn int_to_float() {
    //     assert_evals_to!("Num.toFloat 0x9", 9.0, f64);
    // }

    // #[test]
    // fn num_to_float() {
    //     assert_evals_to!("Num.toFloat 9", 9.0, f64);
    // }

    // #[test]
    // fn float_to_float() {
    //     assert_evals_to!("Num.toFloat 0.5", 0.5, f64);
    // }

    // #[test]
    // fn int_compare() {
    //     assert_evals_to!("Num.compare 0 1", RocOrder::Lt, RocOrder);
    //     assert_evals_to!("Num.compare 1 1", RocOrder::Eq, RocOrder);
    //     assert_evals_to!("Num.compare 1 0", RocOrder::Gt, RocOrder);
    // }

    // #[test]
    // fn float_compare() {
    //     assert_evals_to!("Num.compare 0.01 3.14", RocOrder::Lt, RocOrder);
    //     assert_evals_to!("Num.compare 3.14 3.14", RocOrder::Eq, RocOrder);
    //     assert_evals_to!("Num.compare 3.14 0.01", RocOrder::Gt, RocOrder);
    // }

    // #[test]
    // fn pow() {
    //     assert_evals_to!("Num.pow 2.0 2.0", 4.0, f64);
    // }

    // #[test]
    // fn ceiling() {
    //     assert_evals_to!("Num.ceiling 1.1", 2, i64);
    // }

    // #[test]
    // fn floor() {
    //     assert_evals_to!("Num.floor 1.9", 1, i64);
    // }

    // // #[test]
    // // #[should_panic(expected = r#"Roc failed with message: "integer addition overflowed!"#)]
    // // fn int_overflow() {
    // //     assert_evals_to!(
    // //         indoc!(
    // //             r#"
    // //             9_223_372_036_854_775_807 + 1
    // //             "#
    // //         ),
    // //         0,
    // //         i64
    // //     );
    // // }

    // #[test]
    // fn int_add_checked() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             when Num.addChecked 1 2 is
    //                 Ok v -> v
    //                 _ -> -1
    //             "#
    //         ),
    //         3,
    //         i64
    //     );

    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             when Num.addChecked 9_223_372_036_854_775_807 1 is
    //                 Err Overflow -> -1
    //                 Ok v -> v
    //             "#
    //         ),
    //         -1,
    //         i64
    //     );
    // }

    // #[test]
    // fn int_add_wrap() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             Num.addWrap 9_223_372_036_854_775_807 1
    //             "#
    //         ),
    //         std::i64::MIN,
    //         i64
    //     );
    // }

    // #[test]
    // fn float_add_checked_pass() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             when Num.addChecked 1.0 0.0 is
    //                 Ok v -> v
    //                 Err Overflow -> -1.0
    //             "#
    //         ),
    //         1.0,
    //         f64
    //     );
    // }

    // #[test]
    // fn float_add_checked_fail() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             when Num.addChecked 1.7976931348623157e308 1.7976931348623157e308 is
    //                 Err Overflow -> -1
    //                 Ok v -> v
    //             "#
    //         ),
    //         -1.0,
    //         f64
    //     );
    // }

    // //     #[test]
    // //     #[should_panic(expected = r#"Roc failed with message: "float addition overflowed!"#)]
    // //     fn float_overflow() {
    // //         assert_evals_to!(
    // //             indoc!(
    // //                 r#"
    // //                 1.7976931348623157e308 + 1.7976931348623157e308
    // //                 "#
    // //             ),
    // //             0.0,
    // //             f64
    // //         );
    // //     }

    // #[test]
    // fn max_i128() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             Num.maxI128
    //             "#
    //         ),
    //         i128::MAX,
    //         i128
    //     );
    // }

    // #[test]
    // fn num_max_int() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             Num.maxInt
    //             "#
    //         ),
    //         i64::MAX,
    //         i64
    //     );
    // }

    // #[test]
    // fn num_min_int() {
    //     assert_evals_to!(
    //         indoc!(
    //             r#"
    //             Num.minInt
    //             "#
    //         ),
    //         i64::MIN,
    //         i64
    //     );
    // }
}
