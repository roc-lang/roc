#[macro_use]
extern crate pretty_assertions;

mod helpers;

#[cfg(test)]
mod repl_eval {
    use crate::helpers;

    fn expect_success(input: &str, expected: &str) {
        let out = helpers::repl_eval(input);

        assert_eq!(&out.stderr, "");
        assert_eq!(&out.stdout, expected);
        assert!(out.status.success());
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
        expect_success("0x0", "0 : Int");
    }

    #[test]
    fn literal_0x42() {
        expect_success("0x42", "66 : Int");
    }

    #[test]
    fn literal_0point0() {
        expect_success("0.0", "0 : Float");
    }

    #[test]
    fn literal_4point2() {
        expect_success("4.2", "4.2 : Float");
    }

    #[test]
    fn num_addition() {
        expect_success("1 + 2", "3 : Num *");
    }

    #[test]
    fn int_addition() {
        expect_success("0x1 + 2", "3 : Int");
    }

    #[test]
    fn float_addition() {
        expect_success("1.1 + 2", "3.1 : Float");
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

    #[test]
    fn literal_num_list() {
        expect_success("[ 1, 2, 3 ]", "[ 1, 2, 3 ] : List (Num *)");
    }

    #[test]
    fn literal_int_list() {
        expect_success("[ 0x1, 0x2, 0x3 ]", "[ 1, 2, 3 ] : List Int");
    }

    #[test]
    fn literal_float_list() {
        expect_success("[ 1.1, 2.2, 3.3 ]", "[ 1.1, 2.2, 3.3 ] : List Float");
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
            r#"[ [ [ 4, 3, 2 ], [ 1, 0 ] ], [ [] ], [] ] : List (List (List Int))"#,
        );
    }

    #[test]
    fn nested_float_list() {
        expect_success(
            r#"[ [ [ 4, 3, 2 ], [ 1, 0.0 ] ], [ [] ], [] ]"#,
            r#"[ [ [ 4, 3, 2 ], [ 1, 0 ] ], [ [] ], [] ] : List (List (List Float))"#,
        );
    }

    #[test]
    fn list_concat() {
        expect_success(
            "List.concat [ 1.1, 2.2 ] [ 3.3, 4.4, 5.5 ]",
            "[ 1.1, 2.2, 3.3, 4.4, 5.5 ] : List Float",
        );
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
        expect_success("{ foo: 4.2 }", "{ foo: 4.2 } : { foo : Float }");
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
            "{ foo: { bar: { baz: 4.2 } } } : { foo : { bar : { baz : Float } } }",
        );
    }

    #[test]
    fn basic_2_field_i64_record() {
        expect_success(
            "{ foo: 0x4, bar: 0x2 }",
            "{ bar: 2, foo: 4 } : { bar : Int, foo : Int }",
        );
    }

    // TODO uncomment this once https://github.com/rtfeldman/roc/issues/295 is done
    // #[test]
    // fn basic_2_field_f64_record() {
    //     expect_success(
    //         "{ foo: 4.1, bar: 2.3 }",
    //         "{ bar: 2.3, foo: 4.1 } : { bar : Float, foo : Float }",
    //     );
    // }

    // #[test]
    // fn basic_2_field_mixed_record() {
    //     expect_success(
    //         "{ foo: 4.1, bar: 2 }",
    //         "{ bar: 2, foo: 4.1 } : { bar : Num *, foo : Float }",
    //     );
    // }

    // TODO uncomment this once https://github.com/rtfeldman/roc/issues/295 is done
    //
    // #[test]
    // fn basic_3_field_record() {
    //     expect_success(
    //         "{ foo: 4.1, bar: 2, baz: 0x5 }",
    //         "{ foo: 4.1, bar: 2, baz: 0x5 } : { foo : Float, bar : Num *, baz : Int }",
    //     );
    // }

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
            "[ { bar: 2, foo: 4.1 } ] : List { bar : Num *, foo : Float }",
        );
    }

    #[test]
    fn multiline_string() {
        // If a string contains newlines, format it as a multiline string in the output
        expect_success(r#""\n\nhi!\n\n""#, "\"\"\"\n\nhi!\n\n\"\"\"");
    }

    // TODO uncomment this once https://github.com/rtfeldman/roc/issues/295 is done
    //
    // #[test]
    // fn list_of_3_field_records() {
    //     expect_success(
    //         "[ { foo: 4.1, bar: 2, baz: 0x3 } ]",
    //         "[ { foo: 4.1, bar: 2, baz: 0x3 } ] : List { foo : Float, bar : Num *, baz : Int }",
    //     );
    // }
}
