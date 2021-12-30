#[cfg(feature = "gen-wasm")]
use crate::helpers::wasm::assert_refcounts;

#[allow(unused_imports)]
use indoc::indoc;

#[allow(unused_imports)]
use roc_std::{RocList, RocStr};

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn str_inc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"

                [s, s, s]
            "#
        ),
        RocList<RocStr>,
        &[
            3, // s
            1  // result
        ]
    );
}

#[test]
#[cfg(any(feature = "gen-wasm"))]
fn str_dealloc() {
    assert_refcounts!(
        indoc!(
            r#"
                s = Str.concat "A long enough string " "to be heap-allocated"

                Str.isEmpty s
            "#
        ),
        bool,
        &[0]
    );
}
