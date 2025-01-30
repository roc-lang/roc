#![cfg(test)]
// Even with #[allow(non_snake_case)] on individual idents, rust-analyzer issues diagnostics.
// See https://github.com/rust-lang/rust-analyzer/issues/6541.
// For the `v!` macro we use uppercase variables when constructing tag unions.
#![allow(non_snake_case)]

use crate::{
    test_key_eq, test_key_neq,
    util::{check_derivable, check_immediate, check_underivable, derive_test},
    v,
};
use insta::assert_snapshot;
use roc_module::symbol::Symbol;
use roc_types::subs::Variable;

use roc_derive_key::{decoding::FlatDecodableKey, DeriveBuiltin::Decoder, DeriveError, DeriveKey};

test_key_eq! {
    Decoder,

    same_record:
        v!({ a: v!(U8), }), v!({ a: v!(U8), })
    same_record_fields_diff_types:
        v!({ a: v!(U8), }), v!({ a: v!(STR), })
    same_record_fields_any_order:
        v!({ a: v!(U8), b: v!(U8), c: v!(U8), }),
        v!({ c: v!(U8), a: v!(U8), b: v!(U8), })
    explicit_empty_record_and_implicit_empty_record:
        v!(EMPTY_RECORD), v!({})

    same_tuple:
        v!((v!(U8), v!(U16),)), v!((v!(U8), v!(U16),))
    same_tuple_fields_diff_types:
        v!((v!(U8), v!(U16),)), v!((v!(U32), v!(U64),))

    list_list_diff_types:
        v!(Symbol::LIST_LIST v!(STR)), v!(Symbol::LIST_LIST v!(U8))
    str_str:
        v!(Symbol::STR_STR), v!(Symbol::STR_STR)
}

test_key_neq! {
    Decoder,

    different_record_fields:
        v!({ a: v!(U8), }), v!({ b: v!(U8), })
    record_empty_vs_nonempty:
        v!(EMPTY_RECORD), v!({ a: v!(U8), })

    different_tuple_arities:
        v!((v!(U8), v!(U16),)), v!((v!(U8), v!(U16), v!(U32),))
}

#[test]
fn immediates() {
    check_immediate(Decoder, v!(U8), Symbol::DECODE_U8);
    check_immediate(Decoder, v!(U16), Symbol::DECODE_U16);
    check_immediate(Decoder, v!(U32), Symbol::DECODE_U32);
    check_immediate(Decoder, v!(U64), Symbol::DECODE_U64);
    check_immediate(Decoder, v!(U128), Symbol::DECODE_U128);
    check_immediate(Decoder, v!(I8), Symbol::DECODE_I8);
    check_immediate(Decoder, v!(I16), Symbol::DECODE_I16);
    check_immediate(Decoder, v!(I32), Symbol::DECODE_I32);
    check_immediate(Decoder, v!(I64), Symbol::DECODE_I64);
    check_immediate(Decoder, v!(I128), Symbol::DECODE_I128);
    check_immediate(Decoder, v!(DEC), Symbol::DECODE_DEC);
    check_immediate(Decoder, v!(F32), Symbol::DECODE_F32);
    check_immediate(Decoder, v!(F64), Symbol::DECODE_F64);
    check_immediate(Decoder, v!(STR), Symbol::DECODE_STRING);
}

#[test]
fn optional_record_field_derive_error() {
    check_underivable(Decoder, v!({ ?a: v!(U8), }), DeriveError::Underivable);
}

#[test]
fn derivable_record_ext_flex_var() {
    check_derivable(
        Decoder,
        v!({ a: v!(STR), }* ),
        DeriveKey::Decoder(FlatDecodableKey::Record(vec!["a".into()])),
    );
}

#[test]
fn derivable_record_ext_flex_able_var() {
    check_derivable(
        Decoder,
        v!({ a: v!(STR), }a implements Symbol::DECODE_DECODER ),
        DeriveKey::Decoder(FlatDecodableKey::Record(vec!["a".into()])),
    );
}

#[test]
fn derivable_record_with_record_ext() {
    check_derivable(
        Decoder,
        v!({ b: v!(STR), }{ a: v!(STR), } ),
        DeriveKey::Decoder(FlatDecodableKey::Record(vec!["a".into(), "b".into()])),
    );
}

#[test]
fn list() {
    derive_test(Decoder, v!(Symbol::LIST_LIST v!(STR)), |golden| {
        assert_snapshot!(golden, @r"
        # derived for List Str
        # Decoder (List val) fmt where fmt implements DecoderFormatting, val implements Decoding
        # List U8, fmt -[[custom(3)]]-> { rest : List U8, result : [Err [TooShort], Ok (List val)] } where fmt implements DecoderFormatting, val implements Decoding
        # Specialization lambda sets:
        #   @<1>: [[custom(3)]]
        #Derived.decoder_list =
          custom
            \#Derived.bytes, #Derived.fmt ->
              decode_with #Derived.bytes (list decoder) #Derived.fmt
        "
        )
    })
}

#[test]
fn record_2_fields() {
    derive_test(Decoder, v!({first: v!(STR), second: v!(STR),}), |golden| {
        assert_snapshot!(golden, @r##"
        # derived for { first : Str, second : Str }
        # Decoder { first : val, second : val1 } fmt where fmt implements DecoderFormatting, val implements Decoding, val1 implements Decoding
        # List U8, fmt -[[custom(25)]]-> { rest : List U8, result : [Err [TooShort], Ok { first : val, second : val1 }] } where fmt implements DecoderFormatting, val implements Decoding, val1 implements Decoding
        # Specialization lambda sets:
        #   @<1>: [[custom(25)]]
        #Derived.decoder_{first,second} =
          custom
            \#Derived.bytes3, #Derived.fmt4 ->
              decode_with
                #Derived.bytes3
                (record
                  { second: Err NoField, first: Err NoField }
                  \#Derived.state_record2, #Derived.field ->
                    when #Derived.field is
                      "first" ->
                        Keep (custom
                          \#Derived.bytes, #Derived.fmt2 ->
                            when decode_with #Derived.bytes decoder #Derived.fmt2 is
                              #Derived.rec ->
                                {
                                  result: when #Derived.rec.result is
                                      Ok #Derived.val ->
                                        Ok { state_record2 & first: Ok #Derived.val }
                                      Err #Derived.err -> Err #Derived.err,
                                  rest: #Derived.rec.rest
                                })
                      "second" ->
                        Keep (custom
                          \#Derived.bytes2, #Derived.fmt3 ->
                            when decode_with #Derived.bytes2 decoder #Derived.fmt3 is
                              #Derived.rec2 ->
                                {
                                  result: when #Derived.rec2.result is
                                      Ok #Derived.val2 ->
                                        Ok { state_record2 & second: Ok #Derived.val2 }
                                      Err #Derived.err2 -> Err #Derived.err2,
                                  rest: #Derived.rec2.rest
                                })
                      _ -> Skip
                  \#Derived.state_record, #Derived.fmt ->
                    when when #Derived.state_record.first is
                        Ok #Derived.first -> Ok #Derived.first
                        Err (NoField) ->
                          when decode_with [] decoder #Derived.fmt is
                            #Derived.dec_rec2 -> #Derived.dec_rec2.result is
                      Ok #Derived.first ->
                        when when #Derived.state_record.second is
                            Ok #Derived.second -> Ok #Derived.second
                            Err (NoField) ->
                              when decode_with [] decoder #Derived.fmt is
                                #Derived.dec_rec -> #Derived.dec_rec.result is
                          Ok #Derived.second ->
                            Ok { second: #Derived.second, first: #Derived.first }
                          _ -> Err TooShort
                      _ -> Err TooShort)
                #Derived.fmt4
        "##
        )
    })
}

#[test]
fn tuple_2_fields() {
    derive_test(Decoder, v!((v!(STR), v!(U8),)), |golden| {
        assert_snapshot!(golden, @r"
        # derived for ( Str, U8 )*
        # Decoder ( val, val1 )* fmt where fmt implements DecoderFormatting, val implements Decoding, val1 implements Decoding
        # List U8, fmt -[[custom(22)]]-> { rest : List U8, result : [Err [TooShort], Ok ( val, val1 )a] } where fmt implements DecoderFormatting, val implements Decoding, val1 implements Decoding
        # Specialization lambda sets:
        #   @<1>: [[custom(22)]]
        #Derived.decoder_(arity:2) =
          custom
            \#Derived.bytes3, #Derived.fmt3 ->
              decode_with
                #Derived.bytes3
                (tuple
                  { e1: Err NoElem, e0: Err NoElem }
                  \#Derived.stateRecord2, #Derived.index ->
                    when #Derived.index is
                      0 ->
                        Next (custom
                          \#Derived.bytes, #Derived.fmt ->
                            when decode_with #Derived.bytes decoder #Derived.fmt is
                              #Derived.rec ->
                                {
                                  result: when #Derived.rec.result is
                                      Ok #Derived.val ->
                                        Ok { stateRecord2 & e0: Ok #Derived.val }
                                      Err #Derived.err -> Err #Derived.err,
                                  rest: #Derived.rec.rest
                                })
                      1 ->
                        Next (custom
                          \#Derived.bytes2, #Derived.fmt2 ->
                            when decode_with #Derived.bytes2 decoder #Derived.fmt2 is
                              #Derived.rec2 ->
                                {
                                  result: when #Derived.rec2.result is
                                      Ok #Derived.val2 ->
                                        Ok { stateRecord2 & e1: Ok #Derived.val2 }
                                      Err #Derived.err2 -> Err #Derived.err2,
                                  rest: #Derived.rec2.rest
                                })
                      _ -> TooLong
                  \#Derived.stateRecord ->
                    when #Derived.stateRecord.e0 is
                      Ok #Derived.0 ->
                        when #Derived.stateRecord.e1 is
                          Ok #Derived.1 -> Ok ( #Derived.0, #Derived.1 )
                          _ -> Err TooShort
                      _ -> Err TooShort)
                #Derived.fmt3
        "
        )
    })
}
