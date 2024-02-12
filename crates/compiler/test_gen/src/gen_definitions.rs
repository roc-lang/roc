#[cfg(feature = "gen-llvm")]
use crate::helpers::llvm::assert_evals_to;

#[cfg(feature = "gen-dev")]
use crate::helpers::dev::assert_evals_to;

#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_evals_to;

// use crate::helpers::with_larger_debug_stack;
//use crate::assert_wasm_evals_to as assert_evals_to;
#[allow(unused_imports)]
use indoc::indoc;
#[allow(unused_imports)]
use roc_std::{RocList, RocResult, RocStr};

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn def_closure_in_parens() {
    assert_evals_to!(
        indoc!(
            r"
            id = (\x -> x)

            id 42u32
            "
        ),
        42,
        u32
    );
}

#[test]
#[cfg(any(feature = "gen-llvm", feature = "gen-dev", feature = "gen-wasm"))]
fn def_closure_in_multiple_parens() {
    assert_evals_to!(
        indoc!(
            r"
            id = (((\x -> x)))

            id 42u32
            "
        ),
        42,
        u32
    );
}
