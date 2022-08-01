#![cfg(test)]
// Even with #[allow(non_snake_case)] on individual idents, rust-analyzer issues diagnostics.
// See https://github.com/rust-lang/rust-analyzer/issues/6541.
// For the `v!` macro we use uppercase variables when constructing tag unions.
#![allow(non_snake_case)]

use insta::assert_snapshot;
use pretty_assertions::assert_eq;

use crate::{test_hash_eq, test_hash_neq, util::derive_test, v};
use roc_derive::synth_var;
use roc_derive_key::Derived;
use roc_module::{ident::TagName, symbol::Symbol};
use roc_types::{
    subs::{
        AliasVariables, Content, FlatType, RecordFields, Subs, SubsIndex, SubsSlice, UnionTags,
        Variable,
    },
    types::{AliasKind, RecordField},
};

fn check_key<S1, S2>(eq: bool, synth1: S1, synth2: S2)
where
    S1: FnOnce(&mut Subs) -> Variable,
    S2: FnOnce(&mut Subs) -> Variable,
{
    let mut subs = Subs::new();
    let var1 = synth1(&mut subs);
    let var2 = synth2(&mut subs);

    let key1 = Derived::encoding(&subs, var1);
    let key2 = Derived::encoding(&subs, var2);

    if eq {
        assert_eq!(key1, key2);
    } else {
        assert_ne!(key1, key2);
    }
}

fn check_immediate<S>(synth: S, immediate: Symbol)
where
    S: FnOnce(&mut Subs) -> Variable,
{
    let mut subs = Subs::new();
    let var = synth(&mut subs);

    let key = Derived::encoding(&subs, var);

    assert_eq!(key, Ok(Derived::Immediate(immediate)));
}

// {{{ hash tests

test_hash_eq! {
    same_record:
        v!({ a: v!(U8), }), v!({ a: v!(U8), })
    same_record_fields_diff_types:
        v!({ a: v!(U8), }), v!({ a: v!(STR), })
    same_record_fields_any_order:
        v!({ a: v!(U8), b: v!(U8), c: v!(U8), }),
        v!({ c: v!(U8), a: v!(U8), b: v!(U8), })
    explicit_empty_record_and_implicit_empty_record:
        v!(EMPTY_RECORD), v!({})
    same_record_fields_required_vs_optional:
        v!({ a: v!(U8), b: v!(U8), }),
        v!({ ?a: v!(U8), ?b: v!(U8), })

    same_tag_union:
        v!([ A v!(U8) v!(STR), B v!(STR) ]), v!([ A v!(U8) v!(STR), B v!(STR) ])
    same_tag_union_tags_diff_types:
        v!([ A v!(U8) v!(U8), B v!(U8) ]), v!([ A v!(STR) v!(STR), B v!(STR) ])
    same_tag_union_tags_any_order:
        v!([ A v!(U8) v!(U8), B v!(U8), C ]), v!([ C, B v!(STR), A v!(STR) v!(STR) ])
    explicit_empty_tag_union_and_implicit_empty_tag_union:
        v!(EMPTY_TAG_UNION), v!([])

    same_recursive_tag_union:
        v!([ Nil, Cons v!(*lst)] as lst), v!([ Nil, Cons v!(*lst)] as lst)
    same_tag_union_and_recursive_tag_union_fields:
        v!([ Nil, Cons v!(STR)]), v!([ Nil, Cons v!(*lst)] as lst)

    list_list_diff_types:
        v!(Symbol::LIST_LIST v!(STR)), v!(Symbol::LIST_LIST v!(U8))
    set_set_diff_types:
        v!(Symbol::SET_SET v!(STR)), v!(Symbol::SET_SET v!(U8))
    dict_dict_diff_types:
        v!(Symbol::DICT_DICT v!(STR) v!(STR)), v!(Symbol::DICT_DICT v!(U8) v!(U8))
    str_str:
        v!(Symbol::STR_STR), v!(Symbol::STR_STR)

    alias_eq_real_type:
        v!(Symbol::BOOL_BOOL => v!([ True, False ])), v!([False, True])
    diff_alias_same_real_type:
        v!(Symbol::BOOL_BOOL => v!([ True, False ])), v!(Symbol::UNDERSCORE => v!([False, True]))

    opaque_eq_real_type:
        v!(@Symbol::BOOL_BOOL => v!([ True, False ])), v!([False, True])
    diff_opaque_same_real_type:
        v!(@Symbol::BOOL_BOOL => v!([ True, False ])), v!(@Symbol::UNDERSCORE => v!([False, True]))

    opaque_real_type_eq_alias_real_type:
        v!(@Symbol::BOOL_BOOL => v!([ True, False ])), v!(Symbol::UNDERSCORE => v!([False, True]))
}

test_hash_neq! {
    different_record_fields:
        v!({ a: v!(U8), }), v!({ b: v!(U8), })
    record_empty_vs_nonempty:
        v!(EMPTY_RECORD), v!({ a: v!(U8), })

    different_tag_union_tags:
        v!([ A v!(U8) ]), v!([ B v!(U8) ])
    tag_union_empty_vs_nonempty:
        v!(EMPTY_TAG_UNION), v!([ B v!(U8) ])
    different_recursive_tag_union_tags:
        v!([ Nil, Cons v!(*lst) ] as lst), v!([ Nil, Next v!(*lst) ] as lst)

    same_alias_diff_real_type:
        v!(Symbol::BOOL_BOOL => v!([ True, False ])), v!(Symbol::BOOL_BOOL => v!([ False, True, Maybe ]))
    diff_alias_diff_real_type:
        v!(Symbol::BOOL_BOOL => v!([ True, False ])), v!(Symbol::UNDERSCORE => v!([ False, True, Maybe ]))

    same_opaque_diff_real_type:
        v!(@Symbol::BOOL_BOOL => v!([ True, False ])), v!(@Symbol::BOOL_BOOL => v!([ False, True, Maybe ]))
    diff_opaque_diff_real_type:
        v!(@Symbol::BOOL_BOOL => v!([ True, False ])), v!(@Symbol::UNDERSCORE => v!([ False, True, Maybe ]))
}

// }}} hash tests

// {{{ deriver tests

#[test]
fn immediates() {
    check_immediate(v!(U8), Symbol::ENCODE_U8);
    check_immediate(v!(U16), Symbol::ENCODE_U16);
    check_immediate(v!(U32), Symbol::ENCODE_U32);
    check_immediate(v!(U64), Symbol::ENCODE_U64);
    check_immediate(v!(U128), Symbol::ENCODE_U128);
    check_immediate(v!(I8), Symbol::ENCODE_I8);
    check_immediate(v!(I16), Symbol::ENCODE_I16);
    check_immediate(v!(I32), Symbol::ENCODE_I32);
    check_immediate(v!(I64), Symbol::ENCODE_I64);
    check_immediate(v!(I128), Symbol::ENCODE_I128);
    check_immediate(v!(DEC), Symbol::ENCODE_DEC);
    check_immediate(v!(F32), Symbol::ENCODE_F32);
    check_immediate(v!(F64), Symbol::ENCODE_F64);
    check_immediate(v!(STR), Symbol::ENCODE_STRING);
}

use crate::util::DeriveBuiltin::ToEncoder;

#[test]
fn empty_record() {
    derive_test(ToEncoder, v!(EMPTY_RECORD), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for {}
        # {} -[[toEncoder_{}(0)]]-> Encoder fmt | fmt has EncoderFormatting
        # {} -[[toEncoder_{}(0)]]-> (List U8, fmt -[[custom(2) {}]]-> List U8) | fmt has EncoderFormatting
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_{}(0)]]
        #   @<2>: [[custom(2) {}]]
        #Derived.toEncoder_{} =
          \#Derived.rcd ->
            Encode.custom
              \#Derived.bytes, #Derived.fmt ->
                Encode.appendWith #Derived.bytes (Encode.record []) #Derived.fmt
        "###
        )
    })
}

#[test]
fn zero_field_record() {
    derive_test(ToEncoder, v!({}), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for {}
        # {} -[[toEncoder_{}(0)]]-> Encoder fmt | fmt has EncoderFormatting
        # {} -[[toEncoder_{}(0)]]-> (List U8, fmt -[[custom(2) {}]]-> List U8) | fmt has EncoderFormatting
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_{}(0)]]
        #   @<2>: [[custom(2) {}]]
        #Derived.toEncoder_{} =
          \#Derived.rcd ->
            Encode.custom
              \#Derived.bytes, #Derived.fmt ->
                Encode.appendWith #Derived.bytes (Encode.record []) #Derived.fmt
        "###
        )
    })
}

#[test]
fn one_field_record() {
    derive_test(ToEncoder, v!({ a: v!(U8), }), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for { a : U8 }
        # { a : val } -[[toEncoder_{a}(0)]]-> Encoder fmt | fmt has EncoderFormatting, val has Encoding
        # { a : val } -[[toEncoder_{a}(0)]]-> (List U8, fmt -[[custom(2) { a : val }]]-> List U8) | fmt has EncoderFormatting, val has Encoding
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_{a}(0)]]
        #   @<2>: [[custom(2) { a : val }]] | val has Encoding
        #Derived.toEncoder_{a} =
          \#Derived.rcd ->
            Encode.custom
              \#Derived.bytes, #Derived.fmt ->
                Encode.appendWith
                  #Derived.bytes
                  (Encode.record
                    [
                      { value: Encode.toEncoder #Derived.rcd.a, key: "a", },
                    ])
                  #Derived.fmt
        "###
        )
    })
}

#[test]
#[ignore = "TODO #3421 unification of unspecialized variables in lambda sets currently causes this to be derived incorrectly"]
fn two_field_record() {
    derive_test(ToEncoder, v!({ a: v!(U8), b: v!(STR), }), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for { a : U8, b : Str }
        # { a : val, b : a } -[[toEncoder_{a,b}(0)]]-> Encoder fmt | a has Encoding, fmt has EncoderFormatting, val has Encoding
        # { a : val, b : a } -[[toEncoder_{a,b}(0)]]-> (List U8, fmt -[[custom(2) { a : val, b : a }]]-> List U8) | a has Encoding, fmt has EncoderFormatting, val has Encoding
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_{a,b}(0)]]
        #   @<2>: [[custom(2) { a : val, b : a }]] | a has Encoding, val has Encoding
        #Derived.toEncoder_{a,b} =
          \#Derived.rcd ->
            Encode.custom \#Derived.bytes, #Derived.fmt ->
              Encode.appendWith #Derived.bytes (Encode.record [
                { value: Encode.toEncoder #Derived.rcd.a, key: "a", },
                { value: Encode.toEncoder #Derived.rcd.b, key: "b", },
              ]) #Derived.fmt
        "###
        )
    })
}

#[test]
#[ignore = "NOTE: this would never actually happen, because [] is uninhabited, and hence toEncoder can never be called with a value of []!
Rightfully it induces broken assertions in other parts of the compiler, so we ignore it."]
fn empty_tag_union() {
    derive_test(ToEncoder, v!(EMPTY_TAG_UNION), |golden| {
        assert_snapshot!(
            golden,
            @r#"
            "#
        )
    })
}

#[test]
fn tag_one_label_zero_args() {
    derive_test(ToEncoder, v!([A]), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for [A]
        # [A] -[[toEncoder_[A 0](0)]]-> Encoder fmt | fmt has EncoderFormatting
        # [A] -[[toEncoder_[A 0](0)]]-> (List U8, fmt -[[custom(2) [A]]]-> List U8) | fmt has EncoderFormatting
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_[A 0](0)]]
        #   @<2>: [[custom(2) [A]]]
        #Derived.toEncoder_[A 0] =
          \#Derived.tag ->
            Encode.custom
              \#Derived.bytes, #Derived.fmt ->
                Encode.appendWith
                  #Derived.bytes
                  (when #Derived.tag is A -> Encode.tag "A" [])
                  #Derived.fmt
        "###
        )
    })
}

#[test]
#[ignore = "TODO #3421 unification of unspecialized variables in lambda sets currently causes this to be derived incorrectly"]
fn tag_one_label_two_args() {
    derive_test(ToEncoder, v!([A v!(U8) v!(STR)]), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for [A U8 Str]
        # [A val a] -[[toEncoder_[A 2](0)]]-> Encoder fmt | a has Encoding, fmt has EncoderFormatting, val has Encoding
        # [A val a] -[[toEncoder_[A 2](0)]]-> (List U8, fmt -[[custom(4) [A val a]]]-> List U8) | a has Encoding, fmt has EncoderFormatting, val has Encoding
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_[A 2](0)]]
        #   @<2>: [[custom(4) [A val a]]] | a has Encoding, val has Encoding
        #Derived.toEncoder_[A 2] =
          \#Derived.tag ->
            Encode.custom \#Derived.bytes, #Derived.fmt ->
              Encode.appendWith #Derived.bytes (when #Derived.tag is
                A #Derived.2 #Derived.3 ->
                  Encode.tag "A" [
                    Encode.toEncoder #Derived.2,
                    Encode.toEncoder #Derived.3,
                  ]) #Derived.fmt
        "###
        )
    })
}

#[test]
#[ignore = "TODO #3421 unification of unspecialized variables in lambda sets currently causes this to be derived incorrectly"]
fn tag_two_labels() {
    derive_test(
        ToEncoder,
        v!([A v!(U8) v!(STR) v!(U16), B v!(STR)]),
        |golden| {
            assert_snapshot!(golden, @r###"
        # derived for [A U8 Str U16, B Str]
        # [A val a b, B c] -[[toEncoder_[A 3,B 1](0)]]-> Encoder fmt | a has Encoding, b has Encoding, c has Encoding, fmt has EncoderFormatting, val has Encoding
        # [A val a b, B c] -[[toEncoder_[A 3,B 1](0)]]-> (List U8, fmt -[[custom(6) [A val a b, B c]]]-> List U8) | a has Encoding, b has Encoding, c has Encoding, fmt has EncoderFormatting, val has Encoding
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_[A 3,B 1](0)]]
        #   @<2>: [[custom(6) [A val a b, B c]]] | a has Encoding, b has Encoding, c has Encoding, val has Encoding
        #Derived.toEncoder_[A 3,B 1] =
          \#Derived.tag ->
            Encode.custom \#Derived.bytes, #Derived.fmt ->
              Encode.appendWith #Derived.bytes (when #Derived.tag is
                A #Derived.2 #Derived.3 #Derived.4 ->
                  Encode.tag "A" [
                    Encode.toEncoder #Derived.2,
                    Encode.toEncoder #Derived.3,
                    Encode.toEncoder #Derived.4,
                  ]
                B #Derived.5 -> Encode.tag "B" [Encode.toEncoder #Derived.5])
              #Derived.fmt
        "###
            )
        },
    )
}

#[test]
#[ignore = "TODO #3421 unification of unspecialized variables in lambda sets currently causes this to be derived incorrectly"]
fn recursive_tag_union() {
    derive_test(
        ToEncoder,
        v!([Nil, Cons v!(U8) v!(*lst) ] as lst),
        |golden| {
            assert_snapshot!(golden, @r###"
        # derived for [Cons U8 $rec, Nil] as $rec
        # [Cons val a, Nil] -[[toEncoder_[Cons 2,Nil 0](0)]]-> Encoder fmt | a has Encoding, fmt has EncoderFormatting, val has Encoding
        # [Cons val a, Nil] -[[toEncoder_[Cons 2,Nil 0](0)]]-> (List U8, fmt -[[custom(4) [Cons val a, Nil]]]-> List U8) | a has Encoding, fmt has EncoderFormatting, val has Encoding
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_[Cons 2,Nil 0](0)]]
        #   @<2>: [[custom(4) [Cons val a, Nil]]] | a has Encoding, val has Encoding
        #Derived.toEncoder_[Cons 2,Nil 0] =
          \#Derived.tag ->
            Encode.custom \#Derived.bytes, #Derived.fmt ->
              Encode.appendWith #Derived.bytes (when #Derived.tag is
                Cons #Derived.2 #Derived.3 ->
                  Encode.tag "Cons" [
                    Encode.toEncoder #Derived.2,
                    Encode.toEncoder #Derived.3,
                  ]
                Nil -> Encode.tag "Nil" []) #Derived.fmt
        "###
            )
        },
    )
}

#[test]
fn list() {
    derive_test(ToEncoder, v!(Symbol::LIST_LIST v!(STR)), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for List Str
        # List val -[[toEncoder_list(0)]]-> Encoder fmt | fmt has EncoderFormatting, val has Encoding
        # List val -[[toEncoder_list(0)]]-> (List U8, fmt -[[custom(4) (List val)]]-> List U8) | fmt has EncoderFormatting, val has Encoding
        # Specialization lambda sets:
        #   @<1>: [[toEncoder_list(0)]]
        #   @<2>: [[custom(4) (List val)]] | val has Encoding
        #Derived.toEncoder_list =
          \#Derived.lst ->
            Encode.custom
              \#Derived.bytes, #Derived.fmt ->
                Encode.appendWith
                  #Derived.bytes
                  (Encode.list
                    #Derived.lst
                    \#Derived.elem -> Encode.toEncoder #Derived.elem)
                  #Derived.fmt
        "###
        )
    })
}

// }}} deriver tests
