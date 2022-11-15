//! Implements Roc's unification algorithm, the heartstone of Roc's
//! [type inference](https://en.wikipedia.org/wiki/Type_inference).
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

mod fix;
pub mod unify;
