#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to_erased;

use indoc::indoc;

#[test]
#[cfg(any(feature = "gen-llvm"))]
fn multi_branch_capturing() {
    assert_evals_to_erased!(
        indoc!(
            r#"
            app "test" provides [main] to "./platform"

            f = \t, s ->
              if t
              then \{} -> 0
              else \{} -> Str.countGraphemes s

            main = ((f Bool.true "abc") {}, (f Bool.false "abc") {})
            "#
        ),
        (0, 3),
        (usize, usize)
    );
}
