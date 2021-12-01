#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod annotation;
pub mod builtins;
pub mod constraint;
pub mod constraint_soa;
pub mod def;
pub mod env;
pub mod expected;
pub mod expr;
pub mod module;
pub mod num;
pub mod operator;
pub mod pattern;
pub mod procedure;
pub mod scope;
pub mod string;
