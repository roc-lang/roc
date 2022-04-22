#![warn(clippy::dbg_macro)]
// See github.com/rtfeldman/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod abilities;
pub mod annotation;
pub mod builtins;
pub mod constraint;
pub mod def;
pub mod effect_module;
pub mod env;
pub mod exhaustive;
pub mod expected;
pub mod expr;
pub mod module;
pub mod num;
pub mod operator;
pub mod pattern;
pub mod procedure;
mod reference_matrix;
pub mod scope;
pub mod string;
pub mod traverse;
