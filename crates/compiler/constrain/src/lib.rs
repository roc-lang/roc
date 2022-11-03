//! Responsible for building the set of constraints that are used during
//! [type inference](https://en.wikipedia.org/wiki/Type_inference) of a program,
//! and for gathering context needed for pleasant error messages when a type
//! error occurs.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod builtins;
pub mod expr;
pub mod module;
pub mod pattern;
