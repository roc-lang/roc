#![cfg(test)]
// Even with #[allow(non_snake_case)] on individual idents, rust-analyzer issues diagnostics.
// See https://github.com/rust-lang/rust-analyzer/issues/6541.
// For the `v!` macro we use uppercase variables when constructing tag unions.
#![allow(non_snake_case)]

use crate::{util::check_single_lset_immediate, v};
use roc_module::symbol::Symbol;
use roc_types::subs::Variable;

use roc_derive_key::DeriveBuiltin::IsEq;

#[test]
fn immediates() {
    // Everything is an immediate for `Eq`.
    check_single_lset_immediate(IsEq, v!(U8), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(IsEq, v!(U16), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(IsEq, v!(U32), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(IsEq, v!(U64), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(IsEq, v!(U128), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(IsEq, v!(I8), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(IsEq, v!(I16), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(IsEq, v!(I32), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(IsEq, v!(I64), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(IsEq, v!(I128), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(IsEq, v!(STR), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(IsEq, v!(Symbol::LIST_LIST v!(U8)), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(
        IsEq,
        v!(Symbol::LIST_LIST v!(STR)),
        Symbol::EQ_STRUCTURAL_EQ,
    );
    check_single_lset_immediate(IsEq, v!({ a: v!(U8), }), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(IsEq, v!(EMPTY_RECORD), Symbol::EQ_STRUCTURAL_EQ);
    check_single_lset_immediate(
        IsEq,
        v!([ A v!(U8) v!(STR), B v!(STR) ]),
        Symbol::EQ_STRUCTURAL_EQ,
    );
    check_single_lset_immediate(
        IsEq,
        v!([ A v!(U8) v!(STR), B v!(STR) ]),
        Symbol::EQ_STRUCTURAL_EQ,
    );
    check_single_lset_immediate(
        IsEq,
        v!([ Nil, Cons v!(^lst)] as lst),
        Symbol::EQ_STRUCTURAL_EQ,
    );

    // NOTE: despite this reaching an immediate, `F64`s will never actually be allowed to be
    // compared, because obligation checking will rule them out from `isEq`!
    check_single_lset_immediate(IsEq, v!(F64), Symbol::EQ_STRUCTURAL_EQ);
}
