#![cfg(test)]
// Even with #[allow(non_snake_case)] on individual idents, rust-analyzer issues diagnostics.
// See https://github.com/rust-lang/rust-analyzer/issues/6541.
// For the `v!` macro we use uppercase variables when constructing tag unions.
#![allow(non_snake_case)]

use insta::assert_snapshot;

use crate::{
    test_key_eq, test_key_neq,
    util::{check_derivable, check_immediate, derive_test},
    v,
};
use roc_derive_key::{encoding::FlatEncodableKey, DeriveBuiltin::ToEncoder, DeriveKey};
use roc_module::symbol::Symbol;
use roc_types::subs::Variable;

// {{{ hash tests

test_key_eq! {
    ToEncoder,

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

    same_tuple:
        v!((v!(U8), v!(U16),)), v!((v!(U8), v!(U16),))
    same_tuple_fields_diff_types:
        v!((v!(U8), v!(U16),)), v!((v!(U32), v!(U64),))

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

    list_list_diff_types:
        v!(Symbol::LIST_LIST v!(STR)), v!(Symbol::LIST_LIST v!(U8))
    set_set_diff_types:
        v!(Symbol::SET_SET v!(STR)), v!(Symbol::SET_SET v!(U8))
    dict_dict_diff_types:
        v!(Symbol::DICT_DICT v!(STR) v!(STR)), v!(Symbol::DICT_DICT v!(U8) v!(U8))
    str_str:
        v!(Symbol::STR_STR), v!(Symbol::STR_STR)

    alias_eq_real_type:
        v!(Symbol::ATTR_ATTR => v!([ True, False ])), v!([False, True])
    diff_alias_same_real_type:
        v!(Symbol::ATTR_ATTR => v!([ True, False ])), v!(Symbol::UNDERSCORE => v!([False, True]))

    opaque_eq_real_type:
        v!(@Symbol::ATTR_ATTR => v!([ True, False ])), v!([False, True])
    diff_opaque_same_real_type:
        v!(@Symbol::ATTR_ATTR => v!([ True, False ])), v!(@Symbol::UNDERSCORE => v!([False, True]))

    opaque_real_type_eq_alias_real_type:
        v!(@Symbol::ATTR_ATTR => v!([ True, False ])), v!(Symbol::UNDERSCORE => v!([False, True]))
}

test_key_neq! {
    ToEncoder,

    different_record_fields:
        v!({ a: v!(U8), }), v!({ b: v!(U8), })
    record_empty_vs_nonempty:
        v!(EMPTY_RECORD), v!({ a: v!(U8), })

    different_tuple_arities:
        v!((v!(U8), v!(U16),)), v!((v!(U8), v!(U16), v!(U32),))

    different_tag_union_tags:
        v!([ A v!(U8) ]), v!([ B v!(U8) ])
    tag_union_empty_vs_nonempty:
        v!(EMPTY_TAG_UNION), v!([ B v!(U8) ])
    different_recursive_tag_union_tags:
        v!([ Nil, Cons v!(^lst) ] as lst), v!([ Nil, Next v!(^lst) ] as lst)

    same_alias_diff_real_type:
        v!(Symbol::ATTR_ATTR => v!([ True, False ])), v!(Symbol::ATTR_ATTR => v!([ False, True, Maybe ]))
    diff_alias_diff_real_type:
        v!(Symbol::ATTR_ATTR => v!([ True, False ])), v!(Symbol::UNDERSCORE => v!([ False, True, Maybe ]))

    same_opaque_diff_real_type:
        v!(@Symbol::ATTR_ATTR => v!([ True, False ])), v!(@Symbol::ATTR_ATTR => v!([ False, True, Maybe ]))
    diff_opaque_diff_real_type:
        v!(@Symbol::ATTR_ATTR => v!([ True, False ])), v!(@Symbol::UNDERSCORE => v!([ False, True, Maybe ]))
}

// }}} hash tests

// {{{ deriver tests

#[test]
fn immediates() {
    check_immediate(ToEncoder, v!(U8), Symbol::ENCODE_U8);
    check_immediate(ToEncoder, v!(U16), Symbol::ENCODE_U16);
    check_immediate(ToEncoder, v!(U32), Symbol::ENCODE_U32);
    check_immediate(ToEncoder, v!(U64), Symbol::ENCODE_U64);
    check_immediate(ToEncoder, v!(U128), Symbol::ENCODE_U128);
    check_immediate(ToEncoder, v!(I8), Symbol::ENCODE_I8);
    check_immediate(ToEncoder, v!(I16), Symbol::ENCODE_I16);
    check_immediate(ToEncoder, v!(I32), Symbol::ENCODE_I32);
    check_immediate(ToEncoder, v!(I64), Symbol::ENCODE_I64);
    check_immediate(ToEncoder, v!(I128), Symbol::ENCODE_I128);
    check_immediate(ToEncoder, v!(DEC), Symbol::ENCODE_DEC);
    check_immediate(ToEncoder, v!(F32), Symbol::ENCODE_F32);
    check_immediate(ToEncoder, v!(F64), Symbol::ENCODE_F64);
    check_immediate(ToEncoder, v!(STR), Symbol::ENCODE_STRING);
}

#[test]
fn derivable_record_ext_flex_var() {
    check_derivable(
        ToEncoder,
        v!({ a: v!(STR), }* ),
        DeriveKey::ToEncoder(FlatEncodableKey::Record(vec!["a".into()])),
    );
}

#[test]
fn derivable_record_ext_flex_able_var() {
    check_derivable(
        ToEncoder,
        v!({ a: v!(STR), }a implements Symbol::ENCODE_TO_ENCODER),
        DeriveKey::ToEncoder(FlatEncodableKey::Record(vec!["a".into()])),
    );
}

#[test]
fn derivable_record_with_record_ext() {
    check_derivable(
        ToEncoder,
        v!({ b: v!(STR), }{ a: v!(STR), } ),
        DeriveKey::ToEncoder(FlatEncodableKey::Record(vec!["a".into(), "b".into()])),
    );
}

#[test]
fn derivable_tag_ext_flex_var() {
    check_derivable(
        ToEncoder,
        v!([ A v!(STR) ]* ),
        DeriveKey::ToEncoder(FlatEncodableKey::TagUnion(vec![("A".into(), 1)])),
    );
}

#[test]
fn derivable_tag_ext_flex_able_var() {
    check_derivable(
        ToEncoder,
        v!([ A v!(STR) ]a implements Symbol::ENCODE_TO_ENCODER),
        DeriveKey::ToEncoder(FlatEncodableKey::TagUnion(vec![("A".into(), 1)])),
    );
}

#[test]
fn derivable_tag_with_tag_ext() {
    check_derivable(
        ToEncoder,
        v!([ B v!(STR) v!(U8) ][ A v!(STR) ]),
        DeriveKey::ToEncoder(FlatEncodableKey::TagUnion(vec![
            ("A".into(), 1),
            ("B".into(), 2),
        ])),
    );
}

#[test]
fn empty_record() {
    derive_test(ToEncoder, v!(EMPTY_RECORD), |golden| {
        assert_snapshot!(golden, @r"
        # derived for {}
        # {} -[[to_encoder_{}(0)]]-> Encoder fmt where fmt implements EncoderFormatting
        # {} -[[to_encoder_{}(0)]]-> (List U8, fmt -[[custom(2) {}]]-> List U8) where fmt implements EncoderFormatting
        # Specialization lambda sets:
        #   @<1>: [[to_encoder_{}(0)]]
        #   @<2>: [[custom(2) {}]]
        #Derived.to_encoder_{} =
          \#Derived.rcd ->
            custom
              \#Derived.bytes, #Derived.fmt ->
                append_with #Derived.bytes (record []) #Derived.fmt
        "
        )
    })
}

#[test]
fn zero_field_record() {
    derive_test(ToEncoder, v!({}), |golden| {
        assert_snapshot!(golden, @r"
        # derived for {}
        # {} -[[to_encoder_{}(0)]]-> Encoder fmt where fmt implements EncoderFormatting
        # {} -[[to_encoder_{}(0)]]-> (List U8, fmt -[[custom(2) {}]]-> List U8) where fmt implements EncoderFormatting
        # Specialization lambda sets:
        #   @<1>: [[to_encoder_{}(0)]]
        #   @<2>: [[custom(2) {}]]
        #Derived.to_encoder_{} =
          \#Derived.rcd ->
            custom
              \#Derived.bytes, #Derived.fmt ->
                append_with #Derived.bytes (record []) #Derived.fmt
        "
        )
    })
}

#[test]
fn one_field_record() {
    derive_test(ToEncoder, v!({ a: v!(U8), }), |golden| {
        assert_snapshot!(golden, @r##"
        # derived for { a : U8 }
        # { a : val } -[[to_encoder_{a}(0)]]-> Encoder fmt where fmt implements EncoderFormatting, val implements Encoding
        # { a : val } -[[to_encoder_{a}(0)]]-> (List U8, fmt -[[custom(2) { a : val }]]-> List U8) where fmt implements EncoderFormatting, val implements Encoding
        # Specialization lambda sets:
        #   @<1>: [[to_encoder_{a}(0)]]
        #   @<2>: [[custom(2) { a : val }]] where val implements Encoding
        #Derived.to_encoder_{a} =
          \#Derived.rcd ->
            custom
              \#Derived.bytes, #Derived.fmt ->
                append_with
                  #Derived.bytes
                  (record [{ value: to_encoder #Derived.rcd.a, key: "a" }])
                  #Derived.fmt
        "##
        )
    })
}

#[test]
fn two_field_record() {
    derive_test(ToEncoder, v!({ a: v!(U8), b: v!(STR), }), |golden| {
        assert_snapshot!(golden, @r##"
        # derived for { a : U8, b : Str }
        # { a : val, b : val1 } -[[to_encoder_{a,b}(0)]]-> Encoder fmt where fmt implements EncoderFormatting, val implements Encoding, val1 implements Encoding
        # { a : val, b : val1 } -[[to_encoder_{a,b}(0)]]-> (List U8, fmt -[[custom(2) { a : val, b : val1 }]]-> List U8) where fmt implements EncoderFormatting, val implements Encoding, val1 implements Encoding
        # Specialization lambda sets:
        #   @<1>: [[to_encoder_{a,b}(0)]]
        #   @<2>: [[custom(2) { a : val, b : val1 }]] where val implements Encoding, val1 implements Encoding
        #Derived.to_encoder_{a,b} =
          \#Derived.rcd ->
            custom
              \#Derived.bytes, #Derived.fmt ->
                append_with
                  #Derived.bytes
                  (record
                    [
                      { value: to_encoder #Derived.rcd.a, key: "a" },
                      { value: to_encoder #Derived.rcd.b, key: "b" },
                    ])
                  #Derived.fmt
        "##
        )
    })
}

#[test]
fn two_field_tuple() {
    derive_test(ToEncoder, v!((v!(U8), v!(STR),)), |golden| {
        assert_snapshot!(golden, @r"
        # derived for ( U8, Str )*
        # ( val, val1 )* -[[to_encoder_(arity:2)(0)]]-> Encoder fmt where fmt implements EncoderFormatting, val implements Encoding, val1 implements Encoding
        # ( val, val1 )a -[[to_encoder_(arity:2)(0)]]-> (List U8, fmt -[[custom(2) ( val, val1 )a]]-> List U8) where fmt implements EncoderFormatting, val implements Encoding, val1 implements Encoding
        # Specialization lambda sets:
        #   @<1>: [[to_encoder_(arity:2)(0)]]
        #   @<2>: [[custom(2) ( val, val1 )*]] where val implements Encoding, val1 implements Encoding
        #Derived.to_encoder_(arity:2) =
          \#Derived.tup ->
            custom
              \#Derived.bytes, #Derived.fmt ->
                append_with
                  #Derived.bytes
                  (tuple [to_encoder #Derived.tup.0, to_encoder #Derived.tup.1])
                  #Derived.fmt
        "
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
        assert_snapshot!(golden, @r##"
        # derived for [A]
        # [A] -[[to_encoder_[A 0](0)]]-> Encoder fmt where fmt implements EncoderFormatting
        # [A] -[[to_encoder_[A 0](0)]]-> (List U8, fmt -[[custom(2) [A]]]-> List U8) where fmt implements EncoderFormatting
        # Specialization lambda sets:
        #   @<1>: [[to_encoder_[A 0](0)]]
        #   @<2>: [[custom(2) [A]]]
        #Derived.to_encoder_[A 0] =
          \#Derived.tag ->
            custom
              \#Derived.bytes, #Derived.fmt ->
                append_with
                  #Derived.bytes
                  (when #Derived.tag is
                    A -> tag "A" [])
                  #Derived.fmt
        "##
        )
    })
}

#[test]
fn tag_one_label_two_args() {
    derive_test(ToEncoder, v!([A v!(U8) v!(STR)]), |golden| {
        assert_snapshot!(golden, @r##"
        # derived for [A U8 Str]
        # [A val val1] -[[to_encoder_[A 2](0)]]-> Encoder fmt where fmt implements EncoderFormatting, val implements Encoding, val1 implements Encoding
        # [A val val1] -[[to_encoder_[A 2](0)]]-> (List U8, fmt -[[custom(4) [A val val1]]]-> List U8) where fmt implements EncoderFormatting, val implements Encoding, val1 implements Encoding
        # Specialization lambda sets:
        #   @<1>: [[to_encoder_[A 2](0)]]
        #   @<2>: [[custom(4) [A val val1]]] where val implements Encoding, val1 implements Encoding
        #Derived.to_encoder_[A 2] =
          \#Derived.tag ->
            custom
              \#Derived.bytes, #Derived.fmt ->
                append_with
                  #Derived.bytes
                  (when #Derived.tag is
                    A #Derived.2 #Derived.3 ->
                      tag "A" [to_encoder #Derived.2, to_encoder #Derived.3])
                  #Derived.fmt
        "##
        )
    })
}

#[test]
fn tag_two_labels() {
    derive_test(
        ToEncoder,
        v!([A v!(U8) v!(STR) v!(U16), B v!(STR)]),
        |golden| {
            assert_snapshot!(golden, @r##"
            # derived for [A U8 Str U16, B Str]
            # [A val val1 val1, B val1] -[[to_encoder_[A 3,B 1](0)]]-> Encoder fmt where fmt implements EncoderFormatting, val implements Encoding, val1 implements Encoding
            # [A val val1 val1, B val1] -[[to_encoder_[A 3,B 1](0)]]-> (List U8, fmt -[[custom(6) [A val val1 val1, B val1]]]-> List U8) where fmt implements EncoderFormatting, val implements Encoding, val1 implements Encoding
            # Specialization lambda sets:
            #   @<1>: [[to_encoder_[A 3,B 1](0)]]
            #   @<2>: [[custom(6) [A val val1 val1, B val1]]] where val implements Encoding, val1 implements Encoding
            #Derived.to_encoder_[A 3,B 1] =
              \#Derived.tag ->
                custom
                  \#Derived.bytes, #Derived.fmt ->
                    append_with
                      #Derived.bytes
                      (when #Derived.tag is
                        A #Derived.2 #Derived.3 #Derived.4 ->
                          tag
                            "A"
                            [
                              to_encoder #Derived.2,
                              to_encoder #Derived.3,
                              to_encoder #Derived.4,
                            ]
                        B #Derived.5 -> tag "B" [to_encoder #Derived.5])
                      #Derived.fmt
            "##
            )
        },
    )
}

#[test]
fn recursive_tag_union() {
    derive_test(
        ToEncoder,
        v!([Nil, Cons v!(U8) v!(^lst) ] as lst),
        |golden| {
            assert_snapshot!(golden, @r##"
            # derived for [Cons U8 $rec, Nil] as $rec
            # [Cons val val1, Nil] -[[to_encoder_[Cons 2,Nil 0](0)]]-> Encoder fmt where fmt implements EncoderFormatting, val implements Encoding, val1 implements Encoding
            # [Cons val val1, Nil] -[[to_encoder_[Cons 2,Nil 0](0)]]-> (List U8, fmt -[[custom(4) [Cons val val1, Nil]]]-> List U8) where fmt implements EncoderFormatting, val implements Encoding, val1 implements Encoding
            # Specialization lambda sets:
            #   @<1>: [[to_encoder_[Cons 2,Nil 0](0)]]
            #   @<2>: [[custom(4) [Cons val val1, Nil]]] where val implements Encoding, val1 implements Encoding
            #Derived.to_encoder_[Cons 2,Nil 0] =
              \#Derived.tag ->
                custom
                  \#Derived.bytes, #Derived.fmt ->
                    append_with
                      #Derived.bytes
                      (when #Derived.tag is
                        Cons #Derived.2 #Derived.3 ->
                          tag "Cons" [to_encoder #Derived.2, to_encoder #Derived.3]
                        Nil -> tag "Nil" [])
                      #Derived.fmt
            "##
            )
        },
    )
}

#[test]
fn list() {
    derive_test(ToEncoder, v!(Symbol::LIST_LIST v!(STR)), |golden| {
        assert_snapshot!(golden, @r"
        # derived for List Str
        # List val -[[to_encoder_list(0)]]-> Encoder fmt where fmt implements EncoderFormatting, val implements Encoding
        # List val -[[to_encoder_list(0)]]-> (List U8, fmt -[[custom(4) (List val)]]-> List U8) where fmt implements EncoderFormatting, val implements Encoding
        # Specialization lambda sets:
        #   @<1>: [[to_encoder_list(0)]]
        #   @<2>: [[custom(4) (List val)]] where val implements Encoding
        #Derived.to_encoder_list =
          \#Derived.lst ->
            custom
              \#Derived.bytes, #Derived.fmt ->
                append_with
                  #Derived.bytes
                  (list #Derived.lst \#Derived.elem -> to_encoder #Derived.elem)
                  #Derived.fmt
        "
        )
    })
}

// }}} deriver tests
