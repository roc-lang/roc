//! Roc's abstract representation for the dev backend, optimzed for convertibility and optimizability to machine code
#![warn(missing_docs)]
///Convert mono IR into ROAR
pub(super) mod convert;
///Utilities 
pub(super) mod util;
pub(super) use util::*;
///Implements `std::fmt::Display` for various types
mod display;
pub mod ops;
///Where procedures get implemented
pub mod proc;
///All possible values to be used in ROAR
pub mod storage;


