#![cfg(test)]
// Even with #[allow(non_snake_case)] on individual idents, rust-analyzer issues diagnostics.
// See https://github.com/rust-lang/rust-analyzer/issues/6541.
// For the `v!` macro we use uppercase variables when constructing tag unions.
#![allow(non_snake_case)]

use crate::{util::check_immediate, v};
use roc_module::symbol::Symbol;
use roc_types::subs::Variable;

use roc_derive_key::DeriveBuiltin::Hash;

#[test]
fn immediates() {
    check_immediate(Hash, v!(U8), Symbol::HASH_ADD_U8);
    check_immediate(Hash, v!(U16), Symbol::HASH_ADD_U16);
    check_immediate(Hash, v!(U32), Symbol::HASH_ADD_U32);
    check_immediate(Hash, v!(U64), Symbol::HASH_ADD_U64);
    check_immediate(Hash, v!(U128), Symbol::HASH_ADD_U128);
    check_immediate(Hash, v!(I8), Symbol::HASH_ADD_I8);
    check_immediate(Hash, v!(I16), Symbol::HASH_ADD_I16);
    check_immediate(Hash, v!(I32), Symbol::HASH_ADD_I32);
    check_immediate(Hash, v!(I64), Symbol::HASH_ADD_I64);
    check_immediate(Hash, v!(I128), Symbol::HASH_ADD_I128);
}
