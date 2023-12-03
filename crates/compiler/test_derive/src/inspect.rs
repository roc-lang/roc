#![cfg(test)]
// Even with #[allow(non_snake_case)] on individual idents, rust-analyzer issues diagnostics.
// See https://github.com/rust-lang/rust-analyzer/issues/6541.
// For the `v!` macro we use uppercase variables when constructing tag unions.
#![allow(non_snake_case)]

use crate::{
    util::{check_derivable, derive_test},
    v,
};
use insta::assert_snapshot;

use roc_derive_key::{DeriveBuiltin::ToInspector, DeriveKey};

#[test]
fn derivable_num_unbound() {
    check_derivable(
        ToInspector,
        v!(@Symbol::NUM_NUM v!(*) => v!(*)),
        DeriveKey::ToInspector(roc_derive_key::inspect::FlatInspectableKey::Wrapper),
    );
}

#[test]
fn num_unbound() {
    derive_test(ToInspector, v!(@Symbol::NUM_NUM v!(*) => v!(*)), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for Num *
        # val -[[toInspector_wrapper(0)]]-> Inspector f where f implements InspectFormatter, val implements Inspect
        # val -[[toInspector_wrapper(0)]]-> (f -[[custom(2) val]]-> f) where f implements InspectFormatter, val implements Inspect
        # Specialization lambda sets:
        #   @<1>: [[toInspector_wrapper(0)]]
        #   @<2>: [[custom(2) val]] where val implements Inspect
        #Derived.toInspector_wrapper =
          \#Derived.val ->
            custom \#Derived.fmt -> apply (toInspector #Derived.val) #Derived.fmt
        "###
        )
    })
}
