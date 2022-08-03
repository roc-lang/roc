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
        # List U8, fmt -[[] + fmt:Decode.list(21):3]-> { rest : List U8, result : [Err [TooShort], Ok (List val)] } | fmt has DecoderFormatting, val has Decoding
        # Specialization lambda sets:
        #   @<1>: [[] + fmt:Decode.list(21):3] | fmt has DecoderFormatting
        #Derived.decoder_list = Decode.list Decode.decoder
        "###
        )
    })
}
