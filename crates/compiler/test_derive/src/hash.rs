#![cfg(test)]
// Even with #[allow(non_snake_case)] on individual idents, rust-analyzer issues diagnostics.
// See https://github.com/rust-lang/rust-analyzer/issues/6541.
// For the `v!` macro we use uppercase variables when constructing tag unions.
#![allow(non_snake_case)]

use crate::{
    test_key_eq, test_key_neq,
    util::{check_derivable, check_single_lset_immediate, check_underivable, derive_test},
    v,
};
use insta::assert_snapshot;
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

    same_tag_union:
        v!([ A v!(U8) v!(STR), B v!(STR) ]), v!([ A v!(U8) v!(STR), B v!(STR) ])
    same_tag_union_tags_diff_types:
        v!([ A v!(U8) v!(U8), B v!(U8) ]), v!([ A v!(STR) v!(STR), B v!(STR) ])
    same_tag_union_tags_any_order:
        v!([ A v!(U8) v!(U8), B v!(U8), C ]), v!([ C, B v!(STR), A v!(STR) v!(STR) ])
    explicit_empty_tag_union_and_implicit_empty_tag_union:
        v!(EMPTY_TAG_UNION), v!([])

    same_recursive_tag_union:
        v!([ Nil, Cons v!(^lst)] as lst), v!([ Nil, Cons v!(^lst)] as lst)
    same_tag_union_and_recursive_tag_union_fields:
        v!([ Nil, Cons v!(STR)]), v!([ Nil, Cons v!(^lst)] as lst)
}

test_key_neq! {
    Hash,

    different_record_fields:
        v!({ a: v!(U8), }), v!({ b: v!(U8), })
    record_empty_vs_nonempty:
        v!(EMPTY_RECORD), v!({ a: v!(U8), })

    different_tag_union_tags:
        v!([ A v!(U8) ]), v!([ B v!(U8) ])
    tag_union_empty_vs_nonempty:
        v!(EMPTY_TAG_UNION), v!([ B v!(U8) ])
    different_recursive_tag_union_tags:
        v!([ Nil, Cons v!(^lst) ] as lst), v!([ Nil, Next v!(^lst) ] as lst)
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

#[test]
fn derivable_tag_ext_flex_var() {
    check_derivable(
        Hash,
        v!([ A v!(STR) ]* ),
        DeriveKey::Hash(FlatHashKey::TagUnion(vec![("A".into(), 1)])),
    );
}

#[test]
fn derivable_tag_ext_flex_able_var() {
    check_derivable(
        Hash,
        v!([ A v!(STR) ]a has Symbol::ENCODE_TO_ENCODER),
        DeriveKey::Hash(FlatHashKey::TagUnion(vec![("A".into(), 1)])),
    );
}

#[test]
fn derivable_tag_with_tag_ext() {
    check_derivable(
        Hash,
        v!([ B v!(STR) v!(U8) ][ A v!(STR) ]),
        DeriveKey::Hash(FlatHashKey::TagUnion(vec![
            ("A".into(), 1),
            ("B".into(), 2),
        ])),
    );
}

#[test]
fn empty_record() {
    derive_test(Hash, v!(EMPTY_RECORD), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for {}
        # hasher, {} -[[hash_{}(0)]]-> hasher | hasher has Hasher
        # hasher, {} -[[hash_{}(0)]]-> hasher | hasher has Hasher
        # Specialization lambda sets:
        #   @<1>: [[hash_{}(0)]]
        #Derived.hash_{} = \#Derived.hasher, #Derived.rcd -> #Derived.hasher
        "###
        )
    })
}

#[test]
fn zero_field_record() {
    derive_test(Hash, v!({}), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for {}
        # hasher, {} -[[hash_{}(0)]]-> hasher | hasher has Hasher
        # hasher, {} -[[hash_{}(0)]]-> hasher | hasher has Hasher
        # Specialization lambda sets:
        #   @<1>: [[hash_{}(0)]]
        #Derived.hash_{} = \#Derived.hasher, #Derived.rcd -> #Derived.hasher
        "###
        )
    })
}

#[test]
fn one_field_record() {
    derive_test(Hash, v!({ a: v!(U8), }), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for { a : U8 }
        # hasher, { a : a } -[[hash_{a}(0)]]-> hasher | a has Hash, hasher has Hasher
        # hasher, { a : a } -[[hash_{a}(0)]]-> hasher | a has Hash, hasher has Hasher
        # Specialization lambda sets:
        #   @<1>: [[hash_{a}(0)]]
        #Derived.hash_{a} =
          \#Derived.hasher, #Derived.rcd -> Hash.hash #Derived.hasher #Derived.rcd.a
        "###
        )
    })
}

#[test]
fn two_field_record() {
    derive_test(Hash, v!({ a: v!(U8), b: v!(STR), }), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for { a : U8, b : Str }
        # hasher, { a : a, b : a1 } -[[hash_{a,b}(0)]]-> hasher | a has Hash, a1 has Hash, hasher has Hasher
        # hasher, { a : a, b : a1 } -[[hash_{a,b}(0)]]-> hasher | a has Hash, a1 has Hash, hasher has Hasher
        # Specialization lambda sets:
        #   @<1>: [[hash_{a,b}(0)]]
        #Derived.hash_{a,b} =
          \#Derived.hasher, #Derived.rcd ->
            Hash.hash (Hash.hash #Derived.hasher #Derived.rcd.a) #Derived.rcd.b
        "###
        )
    })
}
