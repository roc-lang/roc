use indoc::indoc;
use roc_std::RocList;

#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic = r#"User crash with message: "hello crash""#]
fn crash_literal() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main = if Bool.true then crash "hello crash" else 1u8
            "#
        ),
        1u8,
        u8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic = r#"User crash with message: "hello crash""#]
fn crash_variable() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main =
                msg = "hello crash"
                if Bool.true then crash msg else 1u8
            "#
        ),
        1u8,
        u8
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic = r#"User crash with message: "turns out this was fallible""#]
fn crash_in_call() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            get_infallible = \result -> when result is
                Ok x -> x
                _ -> crash "turns out this was fallible"

            main =
                x : [Ok U64, Err Str]
                x = Err ""
                get_infallible x
            "#
        ),
        1u64,
        u64
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-wasm", feature = "gen-dev"))]
#[should_panic = r#"User crash with message: "no new even primes""#]
fn crash_in_passed_closure() {
    assert_evals_to!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            main = List.map [1, 2, 3] \n -> if n == 2 then crash "no new even primes" else ""
            "#
        ),
        RocList::from_slice(&[1u8]),
        RocList<u8>
    );
}
