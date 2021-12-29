#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_refcounts;

#[allow(unused_imports)]
use indoc::indoc;

#[allow(unused_imports)]
use roc_std::{RocList, RocStr};

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn list_of_same_str() {
    assert_refcounts!(
        indoc!(
            r#"
                a = "A long enough string"
                b = "to be heap-allocated"
                c = Str.joinWith [a, b] " "

                [c, c, c]
            "#
        ),
        RocList<RocStr>,
        &[
            1, // [a,b] (TODO: list decrement. This should be zero!)
            3, // c
            1  // result
        ]
    );
}
