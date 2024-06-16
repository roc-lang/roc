#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

use indoc::indoc;

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn compare_i64() {
    assert_evals_to!(
        indoc!(
            r#"
                i : I64
                i = 1

                j : I64
                j = 2

                when List.compare i j is
                    Equals -> 0
                    GreaterThan -> 1
                    LessThan -> 2
            "#
        ),
        2,
        i64
    );
}
