//! Roc's abstract representation for the dev backend, optimzed for convertibility and optimizability to machine code
#![warn(missing_docs)]
///Convert mono IR into ROAR
pub(super) mod convert;
///Implements `std::fmt::Display` for various types
mod display;
mod ops;
///Where procedures get implemented
mod proc;
///All possible values to be used in ROAR
mod storage;


