#![cfg(test)]
// Even with #[allow(non_snake_case)] on individual idents, rust-analyzer issues diagnostics.
// See https://github.com/rust-lang/rust-analyzer/issues/6541.
// For the `v!` macro we use uppercase variables when constructing tag unions.
#![allow(non_snake_case)]

use crate::{
    util::{check_immediate, derive_test},
    v,
};
use insta::assert_snapshot;
use roc_module::symbol::Symbol;
use roc_types::subs::Variable;

use roc_derive_key::DeriveBuiltin::Decoder;

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
fn list() {
    derive_test(Decoder, v!(Symbol::LIST_LIST v!(STR)), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for List Str
        # Decoder (List val) fmt | fmt has DecoderFormatting, val has Decoding
        # List U8, fmt -[[custom(3)]]-> { rest : List U8, result : [Err [TooShort], Ok (List val)] } | fmt has DecoderFormatting, val has Decoding
        # Specialization lambda sets:
        #   @<1>: [[custom(3)]]
        #Derived.decoder_list =
          Decode.custom
            \#Derived.bytes, #Derived.fmt ->
              Decode.decodeWith #Derived.bytes (Decode.list Decode.decoder) #Derived.fmt
        "###
        )
    })
}

#[test]
fn record_2_fields() {
    derive_test(Decoder, v!({first: v!(STR), second: v!(STR),}), |golden| {
        assert_snapshot!(golden, @r###"
        # derived for { first : Str, second : Str }
        # Decoder { first : val, second : val1 } fmt | fmt has DecoderFormatting, val has Decoding, val1 has Decoding
        # List U8, fmt -[[custom(22)]]-> { rest : List U8, result : [Err [TooShort], Ok { first : val, second : val1 }] } | fmt has DecoderFormatting, val has Decoding, val1 has Decoding
        # Specialization lambda sets:
        #   @<1>: [[custom(22)]]
        #Derived.decoder_{first,second} =
          Decode.custom
            \#Derived.bytes3, #Derived.fmt3 ->
              Decode.decodeWith
                #Derived.bytes3
                (Decode.record
                  { second: Err NoField, first: Err NoField }
                  \#Derived.stateRecord2, #Derived.field ->
                    when #Derived.field is
                      "first" ->
                        Keep (Decode.custom
                          \#Derived.bytes, #Derived.fmt ->
                            when Decode.decodeWith
                                #Derived.bytes
                                Decode.decoder
                                #Derived.fmt is
                              #Derived.rec ->
                                {
                                  result: when #Derived.rec.result is
                                      Ok #Derived.val ->
                                        Ok { stateRecord2 & first: Ok #Derived.val }
                                      Err #Derived.err -> Err #Derived.err,
                                  rest: #Derived.rec.rest
                                })
                      "second" ->
                        Keep (Decode.custom
                          \#Derived.bytes2, #Derived.fmt2 ->
                            when Decode.decodeWith
                                #Derived.bytes2
                                Decode.decoder
                                #Derived.fmt2 is
                              #Derived.rec2 ->
                                {
                                  result: when #Derived.rec2.result is
                                      Ok #Derived.val2 ->
                                        Ok { stateRecord2 & second: Ok #Derived.val2 }
                                      Err #Derived.err2 -> Err #Derived.err2,
                                  rest: #Derived.rec2.rest
                                })
                      _ -> Skip
                  \#Derived.stateRecord ->
                    when #Derived.stateRecord.first is
                      Ok #Derived.first ->
                        when #Derived.stateRecord.second is
                          Ok #Derived.second ->
                            Ok { second: #Derived.second, first: #Derived.first }
                          _ -> Err TooShort
                      _ -> Err TooShort)
                #Derived.fmt3
        "###
        )
    })
}
