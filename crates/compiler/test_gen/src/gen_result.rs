#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

use indoc::indoc;

#[allow(unused_imports)]
use roc_std::{RocResult, RocStr};

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn with_default_ok() {
    assert_evals_to!(
        indoc!(
            r"
            result : Result I64 {}
            result = Ok 12345

            Result.with_default result 0
            "
        ),
        12345,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn with_default_err() {
    assert_evals_to!(
        indoc!(
            r"
            result : Result I64 {}
            result = Err {}

            Result.with_default result 0
            "
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn result_map() {
    assert_evals_to!(
        indoc!(
            r"
            result : Result I64 {}
            result = Ok 2

            result
                |> Result.map_ok (\x -> x + 1)
                |> Result.with_default 0
            "
        ),
        3,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
            result : Result I64 {}
            result = Err {}

            result
                |> Result.map_ok (\x -> x + 1)
                |> Result.with_default 0
            "
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn result_map_err() {
    assert_evals_to!(
        indoc!(
            r"
            result : Result {} I64
            result = Err 2

            when Result.map_err result (\x -> x + 1) is
                Err n -> n
                Ok _ -> 0
            "
        ),
        3,
        i64
    );

    assert_evals_to!(
        indoc!(
            r"
            result : Result {} I64
            result = Ok {}

            when Result.map_err result (\x -> x + 1) is
                Err n -> n
                Ok _ -> 0
            "
        ),
        0,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn err_type_var() {
    assert_evals_to!(
        indoc!(
            r"
            Result.map_ok (Ok 3) (\x -> x + 1)
                |> Result.with_default -1
            "
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn err_type_var_annotation() {
    assert_evals_to!(
        indoc!(
            r"
            ok : Result I64 *
            ok = Ok 3

            Result.map_ok ok (\x -> x + 1)
                |> Result.with_default -1
            "
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn err_empty_tag_union() {
    assert_evals_to!(
        indoc!(
            r"
            ok : Result I64 []
            ok = Ok 3

            Result.map_ok ok (\x -> x + 1)
                |> Result.with_default -1
            "
        ),
        4,
        i64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn is_ok() {
    assert_evals_to!(
        indoc!(
            r"
            result : Result I64 {}
            result = Ok 2

            Result.is_ok result
            "
        ),
        true,
        bool
    );

    assert_evals_to!(
        indoc!(
            r"
            result : Result I64 {}
            result = Err {}

            Result.is_ok result
            "
        ),
        false,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn is_err() {
    assert_evals_to!(
        indoc!(
            r"
            result : Result I64 {}
            result = Ok 2

            Result.is_err result
            "
        ),
        false,
        bool
    );

    assert_evals_to!(
        indoc!(
            r"
            result : Result I64 {}
            result = Err {}

            Result.is_err result
            "
        ),
        true,
        bool
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn roc_result_ok_i64() {
    assert_evals_to!(
        indoc!(
            r"
            result : Result I64 {}
            result = Ok 42

            result
            "
        ),
        RocResult::ok(42),
        RocResult<i64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn roc_result_ok_f64() {
    // NOTE: the dev backend does not currently use float registers when returning a more
    // complex type, but the rust side does expect it to. Hence this test fails with gen-dev

    assert_evals_to!(
        indoc!(
            r"
            result : Result F64 {}
            result = Ok 42.0

            result
            "
        ),
        RocResult::ok(42.0),
        RocResult<f64, ()>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn roc_result_err() {
    assert_evals_to!(
        indoc!(
            r#"
            result : Result I64 Str
            result = Err "foo"

            result
            "#
        ),
        RocResult::err(RocStr::from("foo")),
        RocResult<i64, RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn issue_2583_specialize_errors_behind_unified_branches() {
    assert_evals_to!(
        r#"
        if Bool.true then List.first [15] else Str.to_i64 ""
        "#,
        RocResult::ok(15i64),
        RocResult<i64, bool>
    )
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn roc_result_after_on_ok() {
    assert_evals_to!(indoc!(
        r#"
            input : Result I64 Str
            input = Ok 1

            Result.try input \num ->
                if num < 0 then Err "negative!" else Ok -num
            "#),
        RocResult::ok(-1),
        RocResult<i64, RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn roc_result_after_on_err() {
    assert_evals_to!(indoc!(
        r#"
            input : Result I64 Str
            input = (Err "already a string")

            Result.try input \num ->
                if num < 0 then Err "negative!" else Ok -num
        "#),
        RocResult::err(RocStr::from("already a string")),
        RocResult<i64, RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn roc_result_after_err() {
    assert_evals_to!(
        indoc!(
            r#"
            result : Result Str I64
            result =
              Result.on_err (Ok "already a string") \num ->
                if num < 0 then Ok "negative!" else Err -num

            result
            "#
        ),
        RocResult::ok(RocStr::from("already a string")),
        RocResult<RocStr, i64>
    );

    assert_evals_to!(indoc!(
        r#"
            result : Result Str I64
            result =
              Result.on_err (Err 100) \num ->
                if num < 0 then Ok "negative!" else Err -num

            result
            "#),
        RocResult::err(-100),
        RocResult<RocStr, i64>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn roc_result_map_both() {
    assert_evals_to!(
        indoc!(
            r#"
            result : Result I64 I64
            result = Ok 42

            result |> Result.map_both Num.to_str Num.to_str
            "#
        ),
        RocResult::ok(RocStr::from("42")),
        RocResult<RocStr, RocStr>
    );

    assert_evals_to!(
        indoc!(
            r#"
            result : Result I64 I64
            result = Err 24

            result |> Result.map_both Num.to_str Num.to_str
            "#
        ),
        RocResult::err(RocStr::from("24")),
        RocResult<RocStr, RocStr>
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
fn roc_result_map_two() {
    assert_evals_to!(
        indoc!(
            r#"
            first : Result I64 Str
            first = Ok 24

            second : Result I64 Str
            second = Ok -10

            Result.map2 first second \a, b -> a + b
            "#
        ),
        RocResult::ok(14i64),
        RocResult<i64, RocStr>
    );

    assert_evals_to!(
        indoc!(
            r#"
            first : Result I64 Str
            first = Err "foo"

            second : Result I64 Str
            second = Err "bar"

            Result.map2 first second \a, b -> a + b
            "#
        ),
        RocResult::err(RocStr::from("foo")),
        RocResult<i64, RocStr>
    );

    assert_evals_to!(
        indoc!(
            r#"
            first : Result I64 Str
            first = Ok 42

            second : Result I64 Str
            second = Err "bar"

            Result.map2 first second \a, b -> a + b
            "#
        ),
        RocResult::err(RocStr::from("bar")),
        RocResult<i64, RocStr>
    );
}
