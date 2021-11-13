#![cfg(feature = "gen-llvm")]

#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

// #[cfg(feature = "gen-dev")]
// use crate::helpers::dev::assert_evals_to;

// #[cfg(feature = "gen-wasm")]
// use crate::helpers::wasm::assert_evals_to;

use indoc::indoc;

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn with_default() {
    assert_evals_to!(
        indoc!(
            r#"
            result : Result I64 {}
            result = Ok 2

            Result.withDefault result 0
            "#
        ),
        2,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
            result : Result I64 {}
            result = Err {} 

            Result.withDefault result 0
            "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn result_map() {
    assert_evals_to!(
        indoc!(
            r#"
            result : Result I64 {}
            result = Ok 2

            result
                |> Result.map (\x -> x + 1)
                |> Result.withDefault 0
            "#
        ),
        3,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
            result : Result I64 {}
            result = Err {} 

            result
                |> Result.map (\x -> x + 1)
                |> Result.withDefault 0
            "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn result_map_err() {
    assert_evals_to!(
        indoc!(
            r#"
            result : Result {} I64
            result = Err 2

            when Result.mapErr result (\x -> x + 1) is
                Err n -> n
                Ok _ -> 0
            "#
        ),
        3,
        i64
    );

    assert_evals_to!(
        indoc!(
            r#"
            result : Result {} I64
            result = Ok {}

            when Result.mapErr result (\x -> x + 1) is
                Err n -> n
                Ok _ -> 0
            "#
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn err_type_var() {
    assert_evals_to!(
        indoc!(
            r#"
            Result.map (Ok 3) (\x -> x + 1)
                |> Result.withDefault -1
            "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn err_type_var_annotation() {
    assert_evals_to!(
        indoc!(
            r#"
            ok : Result I64 *
            ok = Ok 3

            Result.map ok (\x -> x + 1)
                |> Result.withDefault -1
            "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn err_empty_tag_union() {
    assert_evals_to!(
        indoc!(
            r#"
            ok : Result I64 []
            ok = Ok 3

            Result.map ok (\x -> x + 1)
                |> Result.withDefault -1
            "#
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn is_ok() {
    assert_evals_to!(
        indoc!(
            r#"
            result : Result I64 {}
            result = Ok 2

            Result.isOk result
            "#
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            result : Result I64 {}
            result = Err {}

            Result.isOk result
            "#
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn is_err() {
    assert_evals_to!(
        indoc!(
            r#"
            result : Result I64 {}
            result = Ok 2

            Result.isErr result
            "#
        ),
        false,
        bool
    );

    assert_evals_to!(
        indoc!(
            r#"
            result : Result I64 {}
            result = Err {}

            Result.isErr result
            "#
        ),
        true,
        bool
    );
}
