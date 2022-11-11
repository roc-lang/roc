//! Provides the LLVM backend to generate Roc binaries. Used to generate a
//! binary with the fastest possible execution speed.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
// we actually want to compare against the literal float bits
#![allow(clippy::float_cmp)]

pub mod llvm;

pub mod run_roc;
