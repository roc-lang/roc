//! Provides the Roc functions and modules that are implicitly imported into every module.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod bitcode;
pub mod roc;
