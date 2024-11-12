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

            displayN = \n ->
                first = Num.toStr n
                second =
                    if n == 1 then
                        return "early 1"
                    else
                        third = Num.toStr (n + 1)
                        if n == 2 then
                            return "early 2"
                        else
                            third

                "$(first), $(second)"

            main : List Str
            main = List.map [1, 2, 3] displayN
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

            displayN = \n ->
                first = Num.toStr n
                second =
                    when n is
                        1 ->
                            return "early 1"

                        _ ->
                            third = Num.toStr (n + 1)
                            when n is
                                2 ->
                                    return "early 2"

                                _ ->
                                    third

                "$(first), $(second)"

            main : List Str
            main = List.map [1, 2, 3] displayN
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
