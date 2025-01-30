#[allow(unused_imports)]
use indoc::indoc;
#[allow(unused_imports)]
use roc_test_utils::assert_multiline_str_eq;

#[cfg(not(feature = "wasm"))]
use crate::cli::{expect_failure, expect_success, repl_eval};

#[cfg(feature = "wasm")]
#[allow(unused_imports)]
use crate::wasm::{expect_failure, expect_success};

#[test]
fn literal_0() {
    expect_success("0", "0 : Num *");
}

#[test]
fn literal_42() {
    expect_success("42", "42 : Num *");
}

#[test]
fn literal_0x0() {
    expect_success("0x0", "0 : Int *");
}

#[test]
fn literal_0x42() {
    expect_success("0x42", "66 : Int *");
}

#[test]
fn literal_0point0() {
    expect_success("0.0", "0 : Frac *");
}

#[test]
fn literal_4point2() {
    expect_success("4.2", "4.2 : Frac *");
}

#[test]
fn literal_4point2e3() {
    expect_success("4.2e3", "4200 : Frac *");
}

#[test]
fn num_addition() {
    expect_success("1 + 2", "3 : Num *");
}

#[test]
fn int_addition() {
    expect_success("0x1 + 2", "3 : Int *");
}

#[test]
fn float_addition() {
    expect_success("1.1 + 2", "3.1 : Frac *");
}

#[cfg(not(feature = "wasm"))]
#[test]
fn num_rem() {
    expect_success("299 % 10", "9 : Int *");
}

#[cfg(not(feature = "wasm"))]
#[test]
fn num_floor_division() {
    expect_success("Num.div_trunc 4 3", "1 : Int *");
}

#[cfg(not(feature = "wasm"))]
#[test]
fn num_floor_checked_division_success() {
    expect_success(
        "Num.div_trunc_checked 4 3",
        "Ok 1 : Result (Int *) [DivByZero]",
    );
}

#[cfg(not(feature = "wasm"))]
#[test]
fn num_floor_checked_division_divby_zero() {
    expect_success(
        "Num.div_trunc_checked 4 0",
        "Err DivByZero : Result (Int *) [DivByZero]",
    );
}

#[cfg(not(feature = "wasm"))]
#[test]
fn num_ceil_division() {
    expect_success("Num.div_ceil 4 3", "2 : Int *")
}

#[cfg(not(feature = "wasm"))]
#[test]
fn num_ceil_checked_division_success() {
    expect_success(
        "Num.div_ceil_checked 4 3",
        "Ok 2 : Result (Int *) [DivByZero]",
    )
}

#[test]
fn float_division_by_zero() {
    expect_success("1f64 / 0", "Num.infinity_f64 : F64");
    expect_success("-1f64 / 0", "-Num.infinity_f64 : F64");
    expect_success("0f64 / 0", "Num.nan_f64 : F64");

    expect_success("1f32 / 0", "Num.infinity_f32 : F32");
    expect_success("-1f32 / 0", "-Num.infinity_f32 : F32");
    expect_success("0f32 / 0", "Num.nan_f32 : F32");
}

#[test]
fn bool_in_record() {
    expect_success("{ x: 1 == 1 }", "{ x: Bool.true } : { x : Bool }");
    expect_success(
        "{ z: { y: { x: 1 == 1 } } }",
        "{ z: { y: { x: Bool.true } } } : { z : { y : { x : Bool } } }",
    );
    expect_success("{ x: 1 != 1 }", "{ x: Bool.false } : { x : Bool }");
    expect_success(
        "{ x: 1 == 1, y: 1 != 1 }",
        "{ x: Bool.true, y: Bool.false } : { x : Bool, y : Bool }",
    );
}

#[test]
fn bool_basic_equality() {
    expect_success("1 == 1", "Bool.true : Bool");
    expect_success("1 != 1", "Bool.false : Bool");
}

#[test]
fn bool_true() {
    expect_success(
        indoc!(
            r"
            Bool.true
            "
        ),
        r"Bool.true : Bool",
    );
}

#[test]
fn bool_false() {
    expect_success(
        indoc!(
            r"
            Bool.false
            "
        ),
        r"Bool.false : Bool",
    );
}

#[test]
fn arbitrary_tag_unions() {
    expect_success("if 1 == 1 then Red else Green", "Red : [Green, Red]");
    expect_success("if 1 != 1 then Red else Green", "Green : [Green, Red]");
}

#[test]
fn tag_without_arguments() {
    expect_success("True", "True : [True]");
    expect_success("False", "False : [False]");
}

#[test]
fn byte_tag_union() {
    expect_success(
        "if 1 == 1 then Red else if 1 == 1 then Green else Blue",
        "Red : [Blue, Green, Red]",
    );

    expect_success(
        "{ y: { x: if 1 == 1 then Red else if 1 == 1 then Green else Blue } }",
        "{ y: { x: Red } } : { y : { x : [Blue, Green, Red] } }",
    );
}

#[test]
fn tag_in_record() {
    expect_success(
        "{ x: Foo 1 2 3, y : 4 }",
        "{ x: Foo 1 2 3, y: 4 } : { x : [Foo (Num *) (Num *) (Num *)], y : Num * }",
    );
    expect_success(
        "{ x: Foo 1 2 3 }",
        "{ x: Foo 1 2 3 } : { x : [Foo (Num *) (Num *) (Num *)] }",
    );
    expect_success("{ x: Unit }", "{ x: Unit } : { x : [Unit] }");
}

#[test]
fn single_element_tag_union() {
    expect_success("True 1", "True 1 : [True (Num *)]");
    expect_success("Foo 1 3.14", "Foo 1 3.14 : [Foo (Num *) (Frac *)]");
}

#[test]
fn newtype_of_unit() {
    expect_success("Foo Bar", "Foo Bar : [Foo [Bar]]");
}

#[test]
fn newtype_of_big_data() {
    expect_success(
        indoc!(
            r#"
                Either a b : [Left a, Right b]
                lefty : Either Str Str
                lefty = Left "loosey"
                A lefty"#
        ),
        r#"A (Left "loosey") : [A (Either Str Str)]"#,
    )
}

#[test]
fn newtype_nested() {
    expect_success(
        indoc!(
            r#"
                Either a b : [Left a, Right b]
                lefty : Either Str Str
                lefty = Left "loosey"
                A (B (C lefty))"#
        ),
        r#"A (B (C (Left "loosey"))) : [A [B [C (Either Str Str)]]]"#,
    )
}

#[test]
fn newtype_of_big_of_newtype() {
    expect_success(
        indoc!(
            r#"
                Big a : [Big a [Wrapper [Newtype a]]]
                big : Big Str
                big = Big "s" (Wrapper (Newtype "t"))
                A big"#
        ),
        r#"A (Big "s" (Wrapper (Newtype "t"))) : [A (Big Str)]"#,
    )
}

#[test]
fn tag_with_arguments() {
    expect_success("True 1", "True 1 : [True (Num *)]");

    expect_success(
        "if 1 == 1 then True 3 else False 3.14",
        "True 3 : [False (Frac *), True (Num *)]",
    )
}

#[test]
fn literal_empty_str() {
    expect_success("\"\"", "\"\" : Str");
}

#[test]
fn literal_ascii_str() {
    expect_success("\"Hello, World!\"", "\"Hello, World!\" : Str");
}

#[test]
fn literal_utf8_str() {
    expect_success("\"👩‍👩‍👦‍👦\"", "\"👩‍👩‍👦‍👦\" : Str");
}

#[test]
fn str_concat() {
    expect_success(
        "Str.concat \"Hello, \" \"World!\"",
        "\"Hello, World!\" : Str",
    );
}

#[test]
fn literal_empty_list() {
    expect_success("[]", "[] : List *");
}

#[cfg(not(feature = "wasm"))]
#[test]
fn literal_empty_list_empty_record() {
    expect_success("[{}]", "[{}] : List {}");
}

#[test]
fn literal_num_list() {
    expect_success("[1, 2, 3]", "[1, 2, 3] : List (Num *)");
}

#[test]
fn literal_int_list() {
    expect_success("[0x1, 0x2, 0x3]", "[1, 2, 3] : List (Int *)");
}

#[test]
fn literal_float_list() {
    expect_success("[1.1, 2.2, 3.3]", "[1.1, 2.2, 3.3] : List (Frac *)");
}

#[test]
fn literal_string_list() {
    expect_success(r#"["a", "b", "cd"]"#, r#"["a", "b", "cd"] : List Str"#);
}

#[test]
fn nested_string_list() {
    expect_success(
        r#"[[["a", "b", "cd"], ["y", "z"]], [[]], []]"#,
        r#"[[["a", "b", "cd"], ["y", "z"]], [[]], []] : List (List (List Str))"#,
    );
}

#[test]
fn nested_num_list() {
    expect_success(
        r"[[[4, 3, 2], [1, 0]], [[]], []]",
        r"[[[4, 3, 2], [1, 0]], [[]], []] : List (List (List (Num *)))",
    );
}

#[test]
fn nested_int_list() {
    expect_success(
        r"[[[4, 3, 2], [1, 0x0]], [[]], []]",
        r"[[[4, 3, 2], [1, 0]], [[]], []] : List (List (List (Int *)))",
    );
}

#[test]
fn nested_float_list() {
    expect_success(
        r"[[[4, 3, 2], [1, 0.0]], [[]], []]",
        r"[[[4, 3, 2], [1, 0]], [[]], []] : List (List (List (Frac *)))",
    );
}

#[test]
fn num_bitwise_and() {
    expect_success("Num.bitwise_and 20 20", "20 : Int *");

    expect_success("Num.bitwise_and 25 10", "8 : Int *");

    expect_success("Num.bitwise_and 200 0", "0 : Int *")
}

#[test]
fn num_bitwise_xor() {
    expect_success("Num.bitwise_xor 20 20", "0 : Int *");

    expect_success("Num.bitwise_xor 15 14", "1 : Int *");

    expect_success("Num.bitwise_xor 7 15", "8 : Int *");

    expect_success("Num.bitwise_xor 200 0", "200 : Int *")
}

#[test]
fn num_add_wrap() {
    expect_success("Num.add_wrap Num.max_i64 1", "-9223372036854775808 : I64");
}

#[test]
fn num_sub_wrap() {
    expect_success("Num.sub_wrap Num.min_i64 1", "9223372036854775807 : I64");
}

#[test]
fn num_mul_wrap() {
    expect_success("Num.mul_wrap Num.max_i64 2", "-2 : I64");
}

#[test]
fn num_mul_saturated() {
    expect_success(
        "Num.mul_saturated Num.max_i64 2",
        "9223372036854775807 : I64",
    );
}

#[cfg(not(feature = "wasm"))]
#[test]
fn num_add_checked() {
    expect_success("Num.add_checked 1 1", "Ok 2 : Result (Num *) [Overflow]");
    expect_success(
        "Num.add_checked Num.max_i64 1",
        "Err Overflow : Result I64 [Overflow]",
    );
}

#[cfg(not(feature = "wasm"))]
#[test]
fn num_sub_checked() {
    expect_success("Num.sub_checked 1 1", "Ok 0 : Result (Num *) [Overflow]");
    expect_success(
        "Num.sub_checked Num.min_i64 1",
        "Err Overflow : Result I64 [Overflow]",
    );
}

#[cfg(not(feature = "wasm"))]
#[test]
fn num_mul_checked() {
    expect_success("Num.mul_checked 20 2", "Ok 40 : Result (Num *) [Overflow]");
    expect_success(
        "Num.mul_checked Num.max_i64 2",
        "Err Overflow : Result I64 [Overflow]",
    );
}

#[cfg(not(feature = "wasm"))]
#[test]
fn list_concat() {
    expect_success(
        "List.concat [1.1, 2.2] [3.3, 4.4, 5.5]",
        "[1.1, 2.2, 3.3, 4.4, 5.5] : List (Frac *)",
    );
}

#[cfg(not(feature = "wasm"))]
#[test]
fn list_contains() {
    expect_success("List.contains [] 0", "Bool.false : Bool");
    expect_success("List.contains [1, 2, 3] 2", "Bool.true : Bool");
    expect_success("List.contains [1, 2, 3] 4", "Bool.false : Bool");
}

#[cfg(not(feature = "wasm"))]
#[test]
fn list_sum_empty() {
    expect_success("List.sum []", "0 : Num a");
}

#[cfg(not(feature = "wasm"))]
#[test]
fn list_sum_num() {
    expect_success("List.sum [1, 2, 3]", "6 : Num *");
}

#[cfg(not(feature = "wasm"))]
#[test]
fn list_sum_frac() {
    expect_success("List.sum [1.1, 2.2, 3.3]", "6.6 : Frac *");
}

#[cfg(not(feature = "wasm"))]
#[test]
fn list_first() {
    expect_success(
        "List.first [12, 9, 6, 3]",
        "Ok 12 : Result (Num *) [ListWasEmpty]",
    );
    expect_success(
        "List.first []",
        "Err ListWasEmpty : Result a [ListWasEmpty]",
    );
}

#[cfg(not(feature = "wasm"))]
#[test]
fn list_last() {
    expect_success(
        "List.last [12, 9, 6, 3]",
        "Ok 3 : Result (Num *) [ListWasEmpty]",
    );

    expect_success("List.last []", "Err ListWasEmpty : Result a [ListWasEmpty]");
}

#[test]
fn empty_record() {
    expect_success("{}", "{} : {}");
}

#[test]
fn basic_1_field_i64_record() {
    // Even though this gets unwrapped at runtime, the repl should still
    // report it as a record
    expect_success("{ foo: 42 }", "{ foo: 42 } : { foo : Num * }");
}

#[test]
fn basic_1_field_f64_record() {
    // Even though this gets unwrapped at runtime, the repl should still
    // report it as a record
    expect_success("{ foo: 4.2 }", "{ foo: 4.2 } : { foo : Frac * }");
}

#[test]
fn nested_1_field_i64_record() {
    // Even though this gets unwrapped at runtime, the repl should still
    // report it as a record
    expect_success(
        "{ foo: { bar: { baz: 42 } } }",
        "{ foo: { bar: { baz: 42 } } } : { foo : { bar : { baz : Num * } } }",
    );
}

#[test]
fn nested_1_field_f64_record() {
    // Even though this gets unwrapped at runtime, the repl should still
    // report it as a record
    expect_success(
        "{ foo: { bar: { baz: 4.2 } } }",
        "{ foo: { bar: { baz: 4.2 } } } : { foo : { bar : { baz : Frac * } } }",
    );
}

#[test]
fn basic_2_field_i64_record() {
    expect_success(
        "{ foo: 0x4, bar: 0x2 }",
        "{ bar: 2, foo: 4 } : { bar : Int *, foo : Int * }",
    );
}

#[test]
fn basic_2_field_f64_record() {
    expect_success(
        "{ foo: 4.1, bar: 2.3 }",
        "{ bar: 2.3, foo: 4.1 } : { bar : Frac *, foo : Frac * }",
    );
}

#[test]
fn basic_2_field_mixed_record() {
    expect_success(
        "{ foo: 4.1, bar: 2 }",
        "{ bar: 2, foo: 4.1 } : { bar : Num *, foo : Frac * }",
    );
}

#[test]
fn basic_3_field_record() {
    expect_success(
        "{ foo: 4.1, bar: 2, baz: 0x5 }",
        "{ bar: 2, baz: 5, foo: 4.1 } : { bar : Num *, baz : Int *, foo : Frac * }",
    );
}

#[test]
fn list_of_1_field_records() {
    // Even though these get unwrapped at runtime, the repl should still
    // report them as records
    expect_success("[{ foo: 42 }]", "[{ foo: 42 }] : List { foo : Num * }");
}

#[test]
fn list_of_2_field_records() {
    expect_success(
        "[{ foo: 4.1, bar: 2 }]",
        "[{ bar: 2, foo: 4.1 }] : List { bar : Num *, foo : Frac * }",
    );
}

#[test]
fn three_element_record() {
    // if this tests turns out to fail on 32-bit targets, look at jit_to_ast_help
    expect_success(
        "{ a: 1, b: 2, c: 3 }",
        "{ a: 1, b: 2, c: 3 } : { a : Num *, b : Num *, c : Num * }",
    );
}

#[test]
fn four_element_record() {
    // if this tests turns out to fail on 32-bit targets, look at jit_to_ast_help
    expect_success(
        "{ a: 1, b: 2, c: 3, d: 4 }",
        "{ a: 1, b: 2, c: 3, d: 4 } : { a : Num *, b : Num *, c : Num *, d : Num * }",
    );
}

#[cfg(not(feature = "wasm"))]
#[test]
fn multiline_string_non_wasm() {
    // If a string contains newlines, format it as a multiline string in the output.

    // We can't use expect_success to test this, because it only looks at the last
    // line of output, and in this case we care about every line of output!
    let out = repl_eval(r#""\n\nhi!\n\n""#);
    let expected = indoc!(
        r#""""


                hi!


                """ : Str"#
    );

    assert_multiline_str_eq!("", out.stderr.as_str());
    assert_multiline_str_eq!(expected, out.stdout.trim());
    assert!(out.status.success());
}

#[cfg(feature = "wasm")]
#[test]
fn multiline_string_wasm() {
    // If a string contains newlines, format it as a multiline string in the output
    expect_success(
        r#""\n\nhi!\n\n""#,
        indoc!(
            r#""""


                hi!


                """ : Str"#
        ),
    );
}

#[test]
fn list_of_3_field_records() {
    expect_success(
        "[{ foo: 4.1, bar: 2, baz: 0x3 }]",
        "[{ bar: 2, baz: 3, foo: 4.1 }] : List { bar : Num *, baz : Int *, foo : Frac * }",
    );
}

// not(debug_assertions) because of issue #6946
#[cfg(not(debug_assertions))]
#[test]
fn identity_lambda() {
    expect_success("\\x -> x", "<function> : a -> a");
}

// not(debug_assertions) because of issue #6946
#[cfg(all(not(feature = "wasm"), not(debug_assertions)))]
#[test]
fn sum_lambda() {
    expect_success("\\x, y -> x + y", "<function> : Num a, Num a -> Num a");
}

#[cfg(not(feature = "wasm"))]
#[test]
fn stdlib_function() {
    expect_success("Num.abs", "<function> : Num a -> Num a");
}

#[cfg(not(feature = "wasm"))] // TODO: mismatch is due to terminal control codes!
#[test]
fn too_few_args() {
    expect_failure(
        "Num.add 2",
        indoc!(
            r"
                ── TOO FEW ARGS ────────────────────────────────────────────────────────────────

                The add function expects 2 arguments, but it got only 1:

                4│      Num.add 2
                        ^^^^^^^

                Roc does not allow functions to be partially applied. Use a closure to
                make partial application explicit.
                "
        ),
    );
}

#[cfg(not(feature = "wasm"))] // TODO: mismatch is due to terminal control codes!
#[test]
fn type_problem_function() {
    expect_failure(
        "Num.add 1 \"not a num\"",
        indoc!(
            r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                This 2nd argument to add has an unexpected type:

                4│      Num.add 1 "not a num"
                                  ^^^^^^^^^^^

                The argument is a string of type:

                    Str

                But add needs its 2nd argument to be:

                    Num *
                "#
        ),
    );
}

#[cfg(not(feature = "wasm"))] // TODO: mismatch is due to terminal control codes!
#[test]
fn type_problem_binary_operator() {
    expect_failure(
        "1 + \"\"",
        indoc!(
            r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                This 2nd argument to + has an unexpected type:

                4│      1 + ""
                            ^^

                The argument is a string of type:

                    Str

                But + needs its 2nd argument to be:

                    Num *
                "#
        ),
    );
}

#[cfg(not(feature = "wasm"))] // TODO: mismatch is due to terminal control codes!
#[test]
fn type_problem_unary_operator() {
    expect_failure(
        "!\"not a bool\"",
        indoc!(
            r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                This 1st argument to ! has an unexpected type:

                4│      !"not a bool"
                         ^^^^^^^^^^^^

                The argument is a string of type:

                    Str

                But ! needs its 1st argument to be:

                    Bool
                "#
        ),
    );
}

#[cfg(not(feature = "wasm"))] // TODO: mismatch is due to terminal control codes!
#[test]
fn type_problem_string_interpolation() {
    expect_failure(
        "\"This is not a string -> ${1}\"",
        indoc!(
            r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                This argument to this string interpolation has an unexpected type:

                4│      "This is not a string -> ${1}"
                                                   ^

                The argument is a number of type:

                    Num *

                But this string interpolation needs its argument to be:

                    Str
                "#
        ),
    );
}

#[cfg(not(feature = "wasm"))] // TODO: mismatch is due to terminal control codes!
#[test]
fn list_drop_at_negative_index() {
    expect_failure(
        "List.drop_at [1, 2, 3] -1",
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

            This 2nd argument to drop_at has an unexpected type:

            4│      List.drop_at [1, 2, 3] -1
                                           ^^

            The argument is a number of type:

                I8, I16, F32, I32, F64, I64, I128, or Dec

            But drop_at needs its 2nd argument to be:

                U64
            "#
        ),
    );
}

#[cfg(not(feature = "wasm"))] // TODO: mismatch is due to terminal control codes!
#[test]
fn list_get_negative_index() {
    expect_failure(
        "List.get [1, 2, 3] -1",
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

            This 2nd argument to get has an unexpected type:

            4│      List.get [1, 2, 3] -1
                                       ^^

            The argument is a number of type:

                I8, I16, F32, I32, F64, I64, I128, or Dec

            But get needs its 2nd argument to be:

                U64
            "#
        ),
    );
}

#[cfg(not(feature = "wasm"))] // TODO: mismatch is due to terminal control codes!
#[test]
fn invalid_string_interpolation() {
    expect_failure(
        "\"${123}\"",
        indoc!(
            r#"
            ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

            This argument to this string interpolation has an unexpected type:

            4│      "${123}"
                       ^^^

            The argument is a number of type:

                Num *

            But this string interpolation needs its argument to be:

                Str
            "#
        ),
    );
}

#[test]
fn issue_2149_i8_ok() {
    expect_success(r#"Str.to_i8 "127""#, "Ok 127 : Result I8 [InvalidNumStr]");
}

#[test]
fn issue_2149_i8_err() {
    expect_success(
        r#"Str.to_i8 "128""#,
        "Err InvalidNumStr : Result I8 [InvalidNumStr]",
    );
}

#[test]
fn issue_2149_i16_ok() {
    expect_success(
        r#"Str.to_i16 "32767""#,
        "Ok 32767 : Result I16 [InvalidNumStr]",
    );
}

#[test]
fn issue_2149_i16_err() {
    expect_success(
        r#"Str.to_i16 "32768""#,
        "Err InvalidNumStr : Result I16 [InvalidNumStr]",
    );
}

#[test]
fn multiline_input() {
    expect_success(
        indoc!(
            r#"
                a : Str
                a = "123"
                a
                "#
        ),
        r#""123" : Str"#,
    )
}

#[test]
fn recursive_tag_union_flat_variant() {
    expect_success(
        indoc!(
            r#"
                Expr : [Sym Str, Add Expr Expr]
                s : Expr
                s = Sym "levitating"
                s
                "#
        ),
        r#"Sym "levitating" : Expr"#,
    )
}

#[test]
fn large_recursive_tag_union_flat_variant() {
    expect_success(
        // > 7 variants so that to force tag storage alongside the data
        indoc!(
            r#"
                Item : [A Str, B Str, C Str, D Str, E Str, F Str, G Str, H Str, I Str, J Str, K Item]
                s : Item
                s = H "woo"
                s
                "#
        ),
        r#"H "woo" : Item"#,
    )
}

#[test]
fn recursive_tag_union_recursive_variant() {
    expect_success(
        indoc!(
            r#"
                Expr : [Sym Str, Add Expr Expr]
                s : Expr
                s = Add (Add (Sym "one") (Sym "two")) (Sym "four")
                s
                "#
        ),
        r#"Add (Add (Sym "one") (Sym "two")) (Sym "four") : Expr"#,
    )
}

#[test]
fn large_recursive_tag_union_recursive_variant() {
    expect_success(
        // > 7 variants so that to force tag storage alongside the data
        indoc!(
            r#"
                Item : [A Str, B Str, C Str, D Str, E Str, F Str, G Str, H Str, I Str, J Str, K Item, L Item]
                s : Item
                s = K (L (E "woo"))
                s
                "#
        ),
        r#"K (L (E "woo")) : Item"#,
    )
}

#[test]
fn recursive_tag_union_into_flat_tag_union() {
    expect_success(
        indoc!(
            r#"
                Item : [One [A Str, B Str], Deep Item]
                i : Item
                i = Deep (One (A "woo"))
                i
                "#
        ),
        r#"Deep (One (A "woo")) : Item"#,
    )
}

#[test]
fn non_nullable_unwrapped_tag_union() {
    expect_success(
        indoc!(
            r#"
                RoseTree a : [Tree a (List (RoseTree a))]
                e1 : RoseTree Str
                e1 = Tree "e1" []
                e2 : RoseTree Str
                e2 = Tree "e2" []
                combo : RoseTree Str
                combo = Tree "combo" [e1, e2]
                combo
                "#
        ),
        r#"Tree "combo" [Tree "e1" [], Tree "e2" []] : RoseTree Str"#,
    )
}

#[test]
fn nullable_unwrapped_tag_union() {
    expect_success(
        indoc!(
            r#"
                LinkedList a : [Nil, Cons a (LinkedList a)]
                c1 : LinkedList Str
                c1 = Cons "Red" Nil
                c2 : LinkedList Str
                c2 = Cons "Yellow" c1
                c3 : LinkedList Str
                c3 = Cons "Green" c2
                c3
                "#
        ),
        r#"Cons "Green" (Cons "Yellow" (Cons "Red" Nil)) : LinkedList Str"#,
    )
}

#[test]
fn nullable_wrapped_tag_union() {
    expect_success(
        indoc!(
            r#"
                Container a : [Empty, Whole a, Halved (Container a) (Container a)]

                meats : Container Str
                meats = Halved (Whole "Brisket") (Whole "Ribs")

                sides : Container Str
                sides = Halved (Whole "Coleslaw") Empty

                bbqPlate : Container Str
                bbqPlate = Halved meats sides

                bbqPlate
                "#
        ),
        r#"Halved (Halved (Whole "Brisket") (Whole "Ribs")) (Halved (Whole "Coleslaw") Empty) : Container Str"#,
    )
}

#[test]
fn large_nullable_wrapped_tag_union() {
    // > 7 non-empty variants so that to force tag storage alongside the data
    expect_success(
        indoc!(
            r#"
                Cont a : [Empty, S1 a, S2 a, S3 a, S4 a, S5 a, S6 a, S7 a, Tup (Cont a) (Cont a)]

                fst : Cont Str
                fst = Tup (S1 "S1") (S2 "S2")

                snd : Cont Str
                snd = Tup (S5 "S5") Empty

                tup : Cont Str
                tup = Tup fst snd

                tup
                "#
        ),
        r#"Tup (Tup (S1 "S1") (S2 "S2")) (Tup (S5 "S5") Empty) : Cont Str"#,
    )
}

// not(debug_assertions) because of issue #6946
#[cfg(all(not(feature = "wasm"), not(debug_assertions)))]
#[test]
fn issue_2300() {
    expect_success(
        r#"\Email str -> str == """#,
        r"<function> : [Email Str] -> Bool",
    )
}

#[cfg(not(feature = "wasm"))]
#[test]
fn function_in_list() {
    expect_success(
        r"[\x -> x + 1, \s -> s * 2]",
        r"[<function>, <function>] : List (Num a -> Num a)",
    )
}

#[cfg(not(feature = "wasm"))]
#[test]
fn function_in_record() {
    expect_success(
        r"{ n: 1, adder: \x -> x + 1 }",
        r"{ adder: <function>, n: 1 } : { adder : Num a -> Num a, n : Num * }",
    )
}

#[cfg(not(feature = "wasm"))]
#[test]
fn function_in_unwrapped_record() {
    expect_success(
        r"{ adder: \x -> x + 1 }",
        r"{ adder: <function> } : { adder : Num a -> Num a }",
    )
}

#[cfg(not(feature = "wasm"))]
#[test]
fn function_in_tag() {
    expect_success(
        r"Adder (\x -> x + 1)",
        r"Adder <function> : [Adder (Num a -> Num a)]",
    )
}

#[test]
fn newtype_of_record_of_tag_of_record_of_tag() {
    expect_success(
        r"A {b: C {d: 1}}",
        r"A { b: C { d: 1 } } : [A { b : [C { d : Num * }] }]",
    )
}

#[test]
fn print_u8s() {
    expect_success(
        indoc!(
            r"
                x : U8
                x = 129
                x
                "
        ),
        "129 : U8",
    )
}

#[cfg(not(feature = "wasm"))] // TODO: mismatch is due to terminal control codes!
#[test]
fn parse_problem() {
    expect_failure(
        "add m n = m + n",
        indoc!(
            r#"
                ── ARGUMENTS BEFORE EQUALS ─────────────────────────────────────────────────────

                I am partway through parsing a definition, but I got stuck here:

                1│  app "app" provides [repl_output] to "./platform"
                2│
                3│  repl_output =
                4│      add m n = m + n
                            ^^^

                Looks like you are trying to define a function. In Roc, functions are
                always written as a lambda, like increment = \n -> n + 1.
                "#
        ),
    );
}

#[ignore] // re-enable (and fix) after https://github.com/roc-lang/roc/issues/4425 is done!
#[cfg(not(feature = "wasm"))]
#[test]
fn issue_2343_complete_mono_with_shadowed_vars() {
    expect_failure(
        indoc!(
            r"
                b = False
                f = \b ->
                    when b is
                        True -> 5
                        False -> 15
                f b
                "
        ),
        indoc!(
            r"
                ── DUPLICATE NAME ──────────────────────────────────────────────────────────────

                The b name is first defined here:

                4│      b = False
                        ^

                But then it's defined a second time here:

                5│      f = \b ->
                             ^

                Since these variables have the same name, it's easy to use the wrong
                one by accident. Give one of them a new name.
                "
        ),
    );
}

#[test]
fn record_with_type_behind_alias() {
    expect_success(
        indoc!(
            r#"
            T : { a: Str }
            v : T
            v = { a: "value" }
            v
            "#
        ),
        r#"{ a: "value" } : T"#,
    );
}

#[test]
fn tag_with_type_behind_alias() {
    expect_success(
        indoc!(
            r#"
            T : [A Str]
            v : T
            v = A "value"
            v"#
        ),
        r#"A "value" : T"#,
    );
}

#[test]
fn aliases_named_err_and_ok() {
    expect_success(
        indoc!(
            r#"
            Err : [Blah]
            Ok : [Stuff]

            v : (Err, Ok)
            v = (Blah, Stuff)
            v"#
        ),
        r#"(Blah, Stuff) : ( [Blah], [Stuff] )*"#,
    );
}

#[cfg(not(feature = "wasm"))]
#[test]
fn issue_2588_record_with_function_and_nonfunction() {
    expect_success(
        indoc!(
            r"
            x = 1
            f = \n -> n * 2
            { y: f x, f }"
        ),
        r"{ f: <function>, y: 2 } : { f : Num a -> Num a, y : Num * }",
    )
}

#[test]
fn opaque_apply() {
    expect_success(
        indoc!(
            r"
            Age := U32

            @Age 23"
        ),
        "@Age 23 : Age",
    )
}

#[test]
fn opaque_apply_polymorphic() {
    expect_success(
        indoc!(
            r#"
            F t u := [Package t u]

            @F (Package "" { a: "" })"#
        ),
        r#"@F (Package "" { a: "" }) : F Str { a : Str }"#,
    )
}

#[test]
fn opaque_pattern_and_call() {
    expect_success(
        indoc!(
            r"
            F t u := [Package t u]

            f = \@F (Package A {}) -> @F (Package {} A)

            f (@F (Package A {}))"
        ),
        r"@F (Package {} A) : F {} [A]",
    )
}

#[test]
fn dec_in_repl() {
    expect_success(
        indoc!(
            r"
            x: Dec
            x=1.23
            x"
        ),
        "1.23 : Dec",
    )
}

#[test]
fn print_i8_issue_2710() {
    expect_success(
        indoc!(
            r"
            a : I8
            a = -1
            a"
        ),
        r"-1 : I8",
    )
}

#[cfg(not(feature = "wasm"))]
#[test]
fn box_box() {
    expect_success(
        indoc!(
            r#"
            Box.box "container store""#
        ),
        r#"Box.box "container store" : Box Str"#,
    )
}

#[cfg(not(feature = "wasm"))]
#[test]
fn box_box_type_alias() {
    expect_success(
        indoc!(
            r#"
            HeapStr : Box Str
            hello_heap : HeapStr
            hello_heap = Box.box "bye stacks"
            hello_heap"#
        ),
        r#"Box.box "bye stacks" : HeapStr"#,
    )
}

#[test]
// not(debug_assertions) because of issue #6946
#[cfg(all(not(feature = "wasm"), not(debug_assertions)))]
fn issue_2582_specialize_result_value() {
    expect_success(
        r#"\x, list -> if x > 0 then List.first list else Ok """#,
        r"<function> : Num *, List Str -> Result Str [ListWasEmpty]",
    )
}

#[test]
#[cfg(not(feature = "wasm"))]
fn issue_2818() {
    expect_success(
        indoc!(
            r"
            f : {} -> List Str
            f = \_ ->
              x = []
              x"
        ),
        r"<function> : {} -> List Str",
    )
}

#[test]
fn issue_2810_recursive_layout_inside_nonrecursive() {
    expect_success(
        indoc!(
            r"
            Command : [Command Tool]

            Job : [Job Command]

            Tool : [SystemTool, FromJob Job]

            a : Job
            a = Job (Command (FromJob (Job (Command SystemTool))))
            a"
        ),
        "Job (Command (FromJob (Job (Command SystemTool)))) : Job",
    )
}

#[test]
fn render_nullable_unwrapped_passing_through_alias() {
    expect_success(
        indoc!(
            r"
            Deep : [L DeepList]
            DeepList : [Nil, Cons Deep]
            v : DeepList
            v = (Cons (L (Cons (L (Cons (L Nil))))))
            v"
        ),
        "Cons (L (Cons (L (Cons (L Nil))))) : DeepList",
    )
}

#[test]
fn opaque_wrap_function() {
    expect_success(
        indoc!(
            r"
            A a := a
            List.map [1u8, 2u8, 3u8] @A"
        ),
        "[@A 1, @A 2, @A 3] : List (A U8)",
    );
}

#[test]
#[ignore]
// I think this is picking the wrong integer type on wasm I64 vs I32.
fn dict_get_single() {
    expect_success(
        indoc!(
            r"
            Dict.single 0 {a: 1, c: 2} |> Dict.get 0"
        ),
        r"Ok { a: 1, c: 2 } : Result { a : Num *, c : Num * } [KeyNotFound]",
    )
}

#[test]
fn record_of_poly_function() {
    expect_success(
        indoc!(
            r#"
            { a: \_ -> "a" }"#
        ),
        r"{ a: <function> } : { a : * -> Str }",
    );
}

#[test]
fn record_of_poly_function_and_string() {
    expect_success(
        indoc!(
            r#"
            { a: \_ -> "a", b: "b" }"#
        ),
        r#"{ a: <function>, b: "b" } : { a : * -> Str, b : Str }"#,
    );
}

#[test]
fn newtype_by_void_is_wrapped() {
    expect_success(
        indoc!(
            r"
            Result.try (Err 42) (\x -> Err (x+1))"
        ),
        r"Err 42 : Result b (Num *)",
    );

    expect_success(
        indoc!(
            r"
            Result.try (Ok 42) (\x -> Ok (x+1))"
        ),
        r"Ok 43 : Result (Num *) err",
    );
}

#[test]
fn enum_tag_union_in_list() {
    expect_success(
        indoc!(
            r"
            [E, F, G, H]
            "
        ),
        r"[E, F, G, H] : List [E, F, G, H]",
    );
}

#[test]
fn str_to_dec() {
    expect_success(
        indoc!(
            r#"
            Str.to_dec "1234.1234"
            "#
        ),
        r"Ok 1234.1234 : Result Dec [InvalidNumStr]",
    );
}

#[test]
fn tuple() {
    expect_success(
        indoc!(
            r#"
            ("a", 2u32)
            "#
        ),
        r#"("a", 2) : ( Str, U32 )*"#,
    );
}

#[test]
fn nested_tuple() {
    expect_success(
        indoc!(
            r#"
            ("a", (2u32, 3u32))
            "#
        ),
        r#"("a", (2, 3)) : ( Str, ( U32, U32 )a )a"#,
    );
}

#[test]
fn ordered_tag_union_memory_layout() {
    expect_success(
        indoc!(
            r"
            Loc : { line: U32, column: U32 }

            Node : [ A Loc, Height U8 Loc ]

            x : Node
            x = Height 1 { line: 2, column: 3 }
            x
            "
        ),
        r"Height 1 { column: 3, line: 2 } : Node",
    );
}

#[test]
fn interpolation_with_nested_strings() {
    expect_success(
        indoc!(
            r#"
            "foo ${Str.join_with ["a", "b", "c"] ", "} bar"
            "#
        ),
        r#""foo a, b, c bar" : Str"#,
    );
}

#[test]
fn interpolation_with_num_to_str() {
    expect_success(
        indoc!(
            r#"
            "foo ${Num.to_str Num.max_i8} bar"
            "#
        ),
        r#""foo 127 bar" : Str"#,
    );
}

#[test]
fn interpolation_with_operator_desugaring() {
    expect_success(
        indoc!(
            r#"
            "foo ${Num.to_str (1 + 2)} bar"
            "#
        ),
        r#""foo 3 bar" : Str"#,
    );
}

// This test doesn't work on wasm because wasm expects <span>s, but
// the point of the test is the string interpolation behavior.
#[cfg(not(feature = "wasm"))]
#[test]
fn interpolation_with_nested_interpolation() {
    expect_failure(
        indoc!(
            r#"
            "foo ${Str.join_with ["a${Num.to_str 5}", "b"] "c"}"
            "#
        ),
        indoc!(
            r#"
                ── SYNTAX PROBLEM ──────────────────────────────────────────────────────────────

                This string interpolation is invalid:

                4│      "foo ${Str.join_with ["a${Num.to_str 5}", "b"] "c"}"
                               ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

                String interpolations cannot contain newlines or other interpolations.

                You can learn more about string interpolation at
                <https://www.roc-lang.org/tutorial#string-interpolation>


                Enter an expression to evaluate, or a definition (like x = 1) to use later.

                  - ctrl-v + ctrl-j makes a newline
                  - :q quits
                  - :help shows this text again
            "#
        ),
        // TODO figure out why the tests prints the repl help text at the end, but only after syntax errors or something?
        // In the actual repl this doesn't happen, only in the test.
    );
}
