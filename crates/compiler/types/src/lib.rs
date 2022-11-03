//! Various representations and utilities for dealing with types in the Roc compiler.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod num;
pub mod pretty_print;
pub mod subs;
pub mod types;
mod unification_table;
