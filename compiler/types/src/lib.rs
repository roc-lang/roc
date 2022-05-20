#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod builtin_aliases;
pub mod pretty_print;
pub mod solved_types;
pub mod subs;
pub mod types;
mod unification_table;
