#[macro_use]
extern crate pretty_assertions;

#[macro_use]
extern crate indoc;

#[cfg(test)]
mod repl_eval {
    use cli_utils::helpers;

    const ERROR_MESSAGE_START: char = '─';

    fn expect_success(input: &str, expected: &str) {
        let out = helpers::repl_eval(input);

        assert_eq!(&out.stderr, "");
        assert_eq!(&out.stdout, expected);
        assert!(out.status.success());
    }

    fn expect_failure(input: &str, expected: &str) {
        let out = helpers::repl_eval(input);

        // there may be some other stuff printed (e.g. unification errors)
        // so skip till the header of the first error
        match out.stdout.find(ERROR_MESSAGE_START) {
            Some(index) => {
                assert_eq!(&out.stderr, "");
                assert_eq!(&out.stdout[index..], expected);
                assert!(out.status.success());
            }
            None => {
                assert_eq!(&out.stderr, "");
                assert!(out.status.success());
                panic!(
                    "I expected a failure, but there is no error message in stdout:\n\n{}",
                    &out.stdout
                );
            }
        }
    }

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
        expect_success("0.0", "0 : Float *");
    }

    #[test]
    fn literal_4point2() {
        expect_success("4.2", "4.2 : Float *");
    }

    #[test]
    fn num_addition() {
        expect_success("1 + 2", "3 : Num *");
    }

    #[test]
    fn int_addition() {
        expect_success("0x1 + 2", "3 : I64");
    }

    #[test]
    fn float_addition() {
        expect_success("1.1 + 2", "3.1 : F64");
    }

    #[test]
    fn num_rem() {
        expect_success("299 % 10", "Ok 9 : Result (Int *) [ DivByZero ]*");
    }

    #[test]
    fn num_floor_division_success() {
        expect_success("Num.divFloor 4 3", "Ok 1 : Result (Int *) [ DivByZero ]*");
    }

    #[test]
    fn num_floor_division_divby_zero() {
        expect_success(
            "Num.divFloor 4 0",
            "Err DivByZero : Result (Int *) [ DivByZero ]*",
        );
    }

    #[test]
    fn num_ceil_division_success() {
        expect_success("Num.divCeil 4 3", "Ok 2 : Result (Int *) [ DivByZero ]*")
    }

    #[test]
    fn bool_in_record() {
        expect_success("{ x: 1 == 1 }", "{ x: True } : { x : Bool }");
        expect_success(
            "{ z: { y: { x: 1 == 1 } } }",
            "{ z: { y: { x: True } } } : { z : { y : { x : Bool } } }",
        );
        expect_success("{ x: 1 != 1 }", "{ x: False } : { x : Bool }");
        expect_success(
            "{ x: 1 == 1, y: 1 != 1 }",
            "{ x: True, y: False } : { x : Bool, y : Bool }",
        );
    }

    #[test]
    fn bool_basic_equality() {
        expect_success("1 == 1", "True : Bool");
        expect_success("1 != 1", "False : Bool");
    }

    #[test]
    fn arbitrary_tag_unions() {
        expect_success("if 1 == 1 then Red else Green", "Red : [ Green, Red ]*");
        expect_success("if 1 != 1 then Red else Green", "Green : [ Green, Red ]*");
    }

    #[test]
    fn tag_without_arguments() {
        expect_success("True", "True : [ True ]*");
        expect_success("False", "False : [ False ]*");
    }

    #[test]
    fn byte_tag_union() {
        expect_success(
            "if 1 == 1 then Red else if 1 == 1 then Green else Blue",
            "Red : [ Blue, Green, Red ]*",
        );

        expect_success(
            "{ y: { x: if 1 == 1 then Red else if 1 == 1 then Green else Blue } }",
            "{ y: { x: Red } } : { y : { x : [ Blue, Green, Red ]* } }",
        );
    }

    #[test]
    fn tag_in_record() {
        expect_success(
            "{ x: Foo 1 2 3, y : 4 }",
            "{ x: Foo 1 2 3, y: 4 } : { x : [ Foo (Num *) (Num *) (Num *) ]*, y : Num * }",
        );
        expect_success(
            "{ x: Foo 1 2 3 }",
            "{ x: Foo 1 2 3 } : { x : [ Foo (Num *) (Num *) (Num *) ]* }",
        );
        expect_success("{ x: Unit }", "{ x: Unit } : { x : [ Unit ]* }");
    }

    #[test]
    fn single_element_tag_union() {
        expect_success("True 1", "True 1 : [ True (Num *) ]*");
        expect_success("Foo 1 3.14", "Foo 1 3.14 : [ Foo (Num *) (Float *) ]*");
    }

    #[test]
    fn newtype_of_unit() {
        expect_success("Foo Bar", "Foo Bar : [ Foo [ Bar ]* ]*");
    }

    #[test]
    fn tag_with_arguments() {
        expect_success("True 1", "True 1 : [ True (Num *) ]*");

        expect_success(
            "if 1 == 1 then True 3 else False 3.14",
            "True 3 : [ False (Float *), True (Num *) ]*",
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
    fn str_count_graphemes() {
        expect_success("Str.countGraphemes \"å🤔\"", "2 : Nat");
    }

    #[test]
    fn literal_empty_list() {
        expect_success("[]", "[] : List *");
    }

    #[test]
    fn literal_empty_list_empty_record() {
        expect_success("[ {} ]", "[ {} ] : List {}");
    }

    #[test]
    fn literal_num_list() {
        expect_success("[ 1, 2, 3 ]", "[ 1, 2, 3 ] : List (Num *)");
    }

    #[test]
    fn literal_int_list() {
        expect_success("[ 0x1, 0x2, 0x3 ]", "[ 1, 2, 3 ] : List (Int *)");
    }

    #[test]
    fn literal_float_list() {
        expect_success("[ 1.1, 2.2, 3.3 ]", "[ 1.1, 2.2, 3.3 ] : List (Float *)");
    }

    #[test]
    fn literal_string_list() {
        expect_success(r#"[ "a", "b", "cd" ]"#, r#"[ "a", "b", "cd" ] : List Str"#);
    }

    #[test]
    fn nested_string_list() {
        expect_success(
            r#"[ [ [ "a", "b", "cd" ], [ "y", "z" ] ], [ [] ], [] ]"#,
            r#"[ [ [ "a", "b", "cd" ], [ "y", "z" ] ], [ [] ], [] ] : List (List (List Str))"#,
        );
    }

    #[test]
    fn nested_num_list() {
        expect_success(
            r#"[ [ [ 4, 3, 2 ], [ 1, 0 ] ], [ [] ], [] ]"#,
            r#"[ [ [ 4, 3, 2 ], [ 1, 0 ] ], [ [] ], [] ] : List (List (List (Num *)))"#,
        );
    }

    #[test]
    fn nested_int_list() {
        expect_success(
            r#"[ [ [ 4, 3, 2 ], [ 1, 0x0 ] ], [ [] ], [] ]"#,
            r#"[ [ [ 4, 3, 2 ], [ 1, 0 ] ], [ [] ], [] ] : List (List (List I64))"#,
        );
    }

    #[test]
    fn nested_float_list() {
        expect_success(
            r#"[ [ [ 4, 3, 2 ], [ 1, 0.0 ] ], [ [] ], [] ]"#,
            r#"[ [ [ 4, 3, 2 ], [ 1, 0 ] ], [ [] ], [] ] : List (List (List F64))"#,
        );
    }

    #[test]
    fn num_bitwise_and() {
        expect_success("Num.bitwiseAnd 20 20", "20 : Int *");

        expect_success("Num.bitwiseAnd 25 10", "8 : Int *");

        expect_success("Num.bitwiseAnd 200 0", "0 : Int *")
    }

    #[test]
    fn num_bitwise_xor() {
        expect_success("Num.bitwiseXor 20 20", "0 : Int *");

        expect_success("Num.bitwiseXor 15 14", "1 : Int *");

        expect_success("Num.bitwiseXor 7 15", "8 : Int *");

        expect_success("Num.bitwiseXor 200 0", "200 : Int *")
    }

    #[test]
    fn num_add_wrap() {
        expect_success("Num.addWrap Num.maxInt 1", "-9223372036854775808 : Int *");
    }

    #[test]
    fn num_sub_wrap() {
        expect_success("Num.subWrap Num.minInt 1", "9223372036854775807 : Int *");
    }

    #[test]
    fn num_mul_wrap() {
        expect_success("Num.mulWrap Num.maxInt 2", "-2 : Int *");
    }

    #[test]
    fn num_add_checked() {
        expect_success("Num.addChecked 1 1", "Ok 2 : Result (Num *) [ Overflow ]*");
        expect_success(
            "Num.addChecked Num.maxInt 1",
            "Err Overflow : Result I64 [ Overflow ]*",
        );
    }

    #[test]
    fn num_sub_checked() {
        expect_success("Num.subChecked 1 1", "Ok 0 : Result (Num *) [ Overflow ]*");
        expect_success(
            "Num.subChecked Num.minInt 1",
            "Err Overflow : Result I64 [ Overflow ]*",
        );
    }

    #[test]
    fn num_mul_checked() {
        expect_success(
            "Num.mulChecked 20 2",
            "Ok 40 : Result (Num *) [ Overflow ]*",
        );
        expect_success(
            "Num.mulChecked Num.maxInt 2",
            "Err Overflow : Result I64 [ Overflow ]*",
        );
    }

    #[test]
    fn list_concat() {
        expect_success(
            "List.concat [ 1.1, 2.2 ] [ 3.3, 4.4, 5.5 ]",
            "[ 1.1, 2.2, 3.3, 4.4, 5.5 ] : List (Float *)",
        );
    }

    #[test]
    fn list_contains() {
        expect_success("List.contains [] 0", "False : Bool");
        expect_success("List.contains [ 1, 2, 3 ] 2", "True : Bool");
        expect_success("List.contains [ 1, 2, 3 ] 4", "False : Bool");
    }

    #[test]
    fn list_sum() {
        expect_success("List.sum []", "0 : Num *");
        expect_success("List.sum [ 1, 2, 3 ]", "6 : Num *");
        expect_success("List.sum [ 1.1, 2.2, 3.3 ]", "6.6 : F64");
    }

    #[test]
    fn list_first() {
        expect_success(
            "List.first [ 12, 9, 6, 3 ]",
            "Ok 12 : Result (Num *) [ ListWasEmpty ]*",
        );
        expect_success(
            "List.first []",
            "Err ListWasEmpty : Result * [ ListWasEmpty ]*",
        );
    }

    #[test]
    fn list_last() {
        expect_success(
            "List.last [ 12, 9, 6, 3 ]",
            "Ok 3 : Result (Num *) [ ListWasEmpty ]*",
        );

        expect_success(
            "List.last []",
            "Err ListWasEmpty : Result * [ ListWasEmpty ]*",
        );
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
        expect_success("{ foo: 4.2 }", "{ foo: 4.2 } : { foo : Float * }");
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
            "{ foo: { bar: { baz: 4.2 } } } : { foo : { bar : { baz : Float * } } }",
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
            "{ bar: 2.3, foo: 4.1 } : { bar : Float *, foo : Float * }",
        );
    }

    #[test]
    fn basic_2_field_mixed_record() {
        expect_success(
            "{ foo: 4.1, bar: 2 }",
            "{ bar: 2, foo: 4.1 } : { bar : Num *, foo : Float * }",
        );
    }

    #[test]
    fn basic_3_field_record() {
        expect_success(
            "{ foo: 4.1, bar: 2, baz: 0x5 }",
            "{ bar: 2, baz: 5, foo: 4.1 } : { bar : Num *, baz : Int *, foo : Float * }",
        );
    }

    #[test]
    fn list_of_1_field_records() {
        // Even though these get unwrapped at runtime, the repl should still
        // report them as records
        expect_success("[ { foo: 42 } ]", "[ { foo: 42 } ] : List { foo : Num * }");
    }

    #[test]
    fn list_of_2_field_records() {
        expect_success(
            "[ { foo: 4.1, bar: 2 } ]",
            "[ { bar: 2, foo: 4.1 } ] : List { bar : Num *, foo : Float * }",
        );
    }

    #[test]
    fn three_element_record() {
        // if this tests turns out to fail on 32-bit platforms, look at jit_to_ast_help
        expect_success(
            "{ a: 1, b: 2, c: 3 }",
            "{ a: 1, b: 2, c: 3 } : { a : Num *, b : Num *, c : Num * }",
        );
    }

    #[test]
    fn four_element_record() {
        // if this tests turns out to fail on 32-bit platforms, look at jit_to_ast_help
        expect_success(
            "{ a: 1, b: 2, c: 3, d: 4 }",
            "{ a: 1, b: 2, c: 3, d: 4 } : { a : Num *, b : Num *, c : Num *, d : Num * }",
        );
    }

    // #[test]
    // fn multiline_string() {
    //     // If a string contains newlines, format it as a multiline string in the output
    //     expect_success(r#""\n\nhi!\n\n""#, "\"\"\"\n\nhi!\n\n\"\"\"");
    // }

    #[test]
    fn list_of_3_field_records() {
        expect_success(
            "[ { foo: 4.1, bar: 2, baz: 0x3 } ]",
            "[ { bar: 2, baz: 3, foo: 4.1 } ] : List { bar : Num *, baz : Int *, foo : Float * }",
        );
    }

    #[test]
    fn identity_lambda() {
        expect_success("\\x -> x", "<function> : a -> a");
    }

    #[test]
    fn sum_lambda() {
        expect_success("\\x, y -> x + y", "<function> : Num a, Num a -> Num a");
    }

    #[test]
    fn stdlib_function() {
        expect_success("Num.abs", "<function> : Num a -> Num a");
    }

    #[test]
    fn too_few_args() {
        expect_failure(
            "Num.add 2",
            indoc!(
                r#"
                ── TOO FEW ARGS ────────────────────────────────────────────────────────────────

                The add function expects 2 arguments, but it got only 1:

                4│      Num.add 2
                        ^^^^^^^

                Roc does not allow functions to be partially applied. Use a closure to
                make partial application explicit.
                "#
            ),
        );
    }

    #[test]
    fn type_problem() {
        expect_failure(
            "1 + \"\"",
            indoc!(
                r#"
                ── TYPE MISMATCH ───────────────────────────────────────────────────────────────

                The 2nd argument to add is not what I expect:

                4│      1 + ""
                            ^^

                This argument is a string of type:

                    Str

                But add needs the 2nd argument to be:

                    Num a
                "#
            ),
        );
    }

    //    #[test]
    //    fn parse_problem() {
    //        // can't find something that won't parse currently
    //    }
    //
    //    #[test]
    //    fn mono_problem() {
    //        // can't produce a mono error (non-exhaustive pattern) yet
    //    }
}
