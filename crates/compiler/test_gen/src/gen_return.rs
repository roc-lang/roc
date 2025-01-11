#![cfg(not(feature = "gen-wasm"))]

#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::identity;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::identity;

#[allow(unused_imports)]
use indoc::indoc;
#[allow(unused_imports)]
use roc_std::{RocList, RocResult, RocStr, I128, U128};

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn early_return_nested_ifs() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            display_n = \n ->
                first = Num.to_str n
                second =
                    if n == 1 then
                        return "early 1"
                    else
                        third = Num.to_str (n + 1)
                        if n == 2 then
                            return "early 2"
                        else
                            third

                "${first}, ${second}"

            main : List Str
            main = List.map [1, 2, 3] display_n
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("early 1"),
            RocStr::from("early 2"),
            RocStr::from("3, 4")
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn early_return_nested_whens() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            display_n = \n ->
                first = Num.to_str n
                second =
                    when n is
                        1 ->
                            return "early 1"

                        _ ->
                            third = Num.to_str (n + 1)
                            when n is
                                2 ->
                                    return "early 2"

                                _ ->
                                    third

                "${first}, ${second}"

            main : List Str
            main = List.map [1, 2, 3] display_n
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("early 1"),
            RocStr::from("early 2"),
            RocStr::from("3, 4")
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn early_return_solo() {
    assert_evals_to!(
        r#"
        identity = \x ->
            return x

        identity "abc"
        "#,
        RocStr::from("abc"),
        RocStr,
        identity,
        true
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
fn early_return_solo_annotated() {
    assert_evals_to!(
        r#"
        identity : Str -> Str
        identity = \x ->
            return x

        identity "abc"
        "#,
        RocStr::from("abc"),
        RocStr,
        identity,
        true
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn early_return_annotated_function() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            fail_if_less_than_five : U64 -> Result {} [LessThanFive]
            fail_if_less_than_five = \n ->
                if n < 5 then
                    Err LessThanFive
                else
                    Ok {}

            validate_input : Str -> Result U64 [InvalidNumStr, LessThanFive]
            validate_input = \str ->
                num = try Str.to_u64 str

                when fail_if_less_than_five num is
                    Err err ->
                        return Err err

                    Ok {} ->
                        Ok num

            main : List Str
            main =
                ["abc", "3", "7"]
                |> List.map validate_input
                |> List.map Inspect.to_str
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("(Err InvalidNumStr)"),
            RocStr::from("(Err LessThanFive)"),
            RocStr::from("(Ok 7)")
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn early_return_nested_annotated_function() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            validate_input : Str -> Result U64 [InvalidNumStr, LessThanFive]
            validate_input = \str ->
                fail_if_less_than_five : U64 -> Result {} [LessThanFive]
                fail_if_less_than_five = \n ->
                    if n < 5 then
                        Err LessThanFive
                    else
                        Ok {}

                num = try Str.to_u64 str

                when fail_if_less_than_five num is
                    Err err ->
                        return Err err

                    Ok {} ->
                        Ok num

            main : List Str
            main =
                ["abc", "3", "7"]
                |> List.map validate_input
                |> List.map Inspect.to_str
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("(Err InvalidNumStr)"),
            RocStr::from("(Err LessThanFive)"),
            RocStr::from("(Ok 7)")
        ]),
        RocList<RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn early_return_annotated_recursive_function() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            might_call_second : U64 -> Result U64 _
            might_call_second = \num ->
                next_num =
                    if num < 5 then
                        return Err LessThanFive
                    else
                        num - 1

                might_call_first next_num

            might_call_first : U64 -> Result U64 _
            might_call_first = \num ->
                next_num =
                    if num < 10 then
                        return Err LessThanTen
                    else
                        num * 2

                if next_num > 25 then
                    Ok next_num
                else
                    might_call_second next_num

            main : List Str
            main =
                [
                    might_call_second 3,
                    might_call_second 7,
                    might_call_second 20,
                    might_call_first 7,
                    might_call_first 15,
                ]
                |> List.map Inspect.to_str
            "#
        ),
        RocList::from_slice(&[
            RocStr::from("(Err LessThanFive)"),
            RocStr::from("(Err LessThanTen)"),
            RocStr::from("(Ok 38)"),
            RocStr::from("(Err LessThanTen)"),
            RocStr::from("(Ok 30)")
        ]),
        RocList<RocStr>
    );
}
