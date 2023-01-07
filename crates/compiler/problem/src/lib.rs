//! Provides types to describe problems that can occur when compiling Roc code.
#![warn(clippy::dbg_macro)]
// See github.com/roc-lang/roc/issues/800 for discussion of the large_enum_variant check.
#![allow(clippy::large_enum_variant)]
pub mod can;

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum Severity {
    /// This will cause a runtime error if some code get srun
    /// (e.g. type mismatch, naming error)
    RuntimeError,

    /// This will never cause the code to misbehave,
    /// but should be cleaned up
    /// (e.g. unused def, unused import)
    Warning,
}
