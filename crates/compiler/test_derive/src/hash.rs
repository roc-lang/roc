#![cfg(test)]
// Even with #[allow(non_snake_case)] on individual idents, rust-analyzer issues diagnostics.
// See https://github.com/rust-lang/rust-analyzer/issues/6541.
// For the `v!` macro we use uppercase variables when constructing tag unions.
#![allow(non_snake_case)]

use crate::{
    test_key_eq, test_key_neq,
    util::{check_derivable, check_single_lset_immediate, check_underivable},
    v,
};
use roc_module::symbol::Symbol;
use roc_types::subs::Variable;

use roc_derive_key::{hash::FlatHashKey, DeriveBuiltin::Hash, DeriveError, DeriveKey};

test_key_eq! {
    Hash,

    same_record:
        v!({ a: v!(U8), }), v!({ a: v!(U8), })
    same_record_fields_diff_types:
        v!({ a: v!(U8), }), v!({ a: v!(STR), })
    same_record_fields_any_order:
        v!({ a: v!(U8), b: v!(U8), c: v!(U8), }),
        v!({ c: v!(U8), a: v!(U8), b: v!(U8), })
    explicit_empty_record_and_implicit_empty_record:
        v!(EMPTY_RECORD), v!({})
}

test_key_neq! {
    Hash,

    different_record_fields:
        v!({ a: v!(U8), }), v!({ b: v!(U8), })
    record_empty_vs_nonempty:
        v!(EMPTY_RECORD), v!({ a: v!(U8), })
}

#[test]
fn immediates() {
    check_single_lset_immediate(Hash, v!(U8), Symbol::HASH_ADD_U8);
    check_single_lset_immediate(Hash, v!(U16), Symbol::HASH_ADD_U16);
    check_single_lset_immediate(Hash, v!(U32), Symbol::HASH_ADD_U32);
    check_single_lset_immediate(Hash, v!(U64), Symbol::HASH_ADD_U64);
    check_single_lset_immediate(Hash, v!(U128), Symbol::HASH_ADD_U128);
    check_single_lset_immediate(Hash, v!(I8), Symbol::HASH_ADD_I8);
    check_single_lset_immediate(Hash, v!(I16), Symbol::HASH_ADD_I16);
    check_single_lset_immediate(Hash, v!(I32), Symbol::HASH_ADD_I32);
    check_single_lset_immediate(Hash, v!(I64), Symbol::HASH_ADD_I64);
    check_single_lset_immediate(Hash, v!(I128), Symbol::HASH_ADD_I128);
    check_single_lset_immediate(Hash, v!(STR), Symbol::HASH_HASH_STR_BYTES);
    check_single_lset_immediate(Hash, v!(Symbol::LIST_LIST v!(U8)), Symbol::HASH_HASH_LIST);
    check_single_lset_immediate(Hash, v!(Symbol::LIST_LIST v!(STR)), Symbol::HASH_HASH_LIST);
}

#[test]
fn optional_record_field_derive_error() {
    check_underivable(Hash, v!({ ?a: v!(U8), }), DeriveError::Underivable);
}

#[test]
fn derivable_record_ext_flex_var() {
    check_derivable(
        Hash,
        v!({ a: v!(STR), }* ),
        DeriveKey::Hash(FlatHashKey::Record(vec!["a".into()])),
    );
}

#[test]
fn derivable_record_ext_flex_able_var() {
    check_derivable(
        Hash,
        v!({ a: v!(STR), }a has Symbol::DECODE_DECODER ),
        DeriveKey::Hash(FlatHashKey::Record(vec!["a".into()])),
    );
}

#[test]
fn derivable_record_with_record_ext() {
    check_derivable(
        Hash,
        v!({ b: v!(STR), }{ a: v!(STR), } ),
        DeriveKey::Hash(FlatHashKey::Record(vec!["a".into(), "b".into()])),
    );
}
