//! Responsible for generating warning and error messages.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]

pub mod cli;
pub mod error;
pub mod report;
