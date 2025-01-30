#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to_erased;

#[cfg(feature = "gen-llvm")]
use indoc::indoc;

#[test]
#[cfg(feature = "gen-llvm")]
fn capture_multiple() {
    assert_evals_to_erased!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            f = \n, m ->
              \{} -> n + m + 15u8

            main = (f 10u8 20u8) {}
            "#
        ),
        45,
        u8
    );
}

#[test]
#[cfg(feature = "gen-llvm")]
fn multi_branch_capturing() {
    assert_evals_to_erased!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            f = \t, s ->
              if t
              then \{} -> 15u64
              else \{} -> Str.count_utf8_bytes s

            main = ((f Bool.true "abc") {}, (f Bool.false "abc") {})
            "#
        ),
        (15, 3),
        (u64, u64)
    );
}
