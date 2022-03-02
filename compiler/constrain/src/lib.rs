#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod builtins;
pub mod expr;
pub mod module;
pub mod pattern;
pub mod soa_expr;
pub mod soa_pattern;
