//! The entry point of Roc's [type inference](https://en.wikipedia.org/wiki/Type_inference)
//! system. Implements type inference and specialization of abilities.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

pub mod ability;
pub mod module;
pub mod solve;
pub mod specialize;
