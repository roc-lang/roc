use indoc::indoc;

#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::expect_runtime_error_panic;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::expect_runtime_error_panic;

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic = r#"User crash with message: "hello crash""#]
fn crash_literal() {
    expect_runtime_error_panic!(indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main = if Bool.true then crash "hello crash" else 1u8
        "#
    ));
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic = r#"User crash with message: "hello crash""#]
fn crash_variable() {
    expect_runtime_error_panic!(indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main =
            msg = "hello crash"
            if Bool.true then crash msg else 1u8
        "#
    ));
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic = r#"User crash with message: "turns out this was fallible""#]
fn crash_in_call() {
    expect_runtime_error_panic!(indoc!(
        r#"
        app "test" provides [main] to "./platform"

        getInfallible = \result -> when result is
            Ok x -> x
            _ -> crash "turns out this was fallible"

        main =
            x : [Ok U64, Err Str]
            x = Err ""
            getInfallible x
        "#
    ));
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm"))]
#[should_panic = r#"User crash with message: "no new even primes""#]
fn crash_in_passed_closure() {
    expect_runtime_error_panic!(indoc!(
        r#"
        app "test" provides [main] to "./platform"

        main = List.map [1, 2, 3] \n -> if n == 2 then crash "no new even primes" else ""
        "#
    ));
}
