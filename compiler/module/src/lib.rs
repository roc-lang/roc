#![warn(clippy::all, clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

pub mod ident;
pub mod low_level;
pub mod operator;
pub mod symbol;

#[macro_use]
extern crate lazy_static;
