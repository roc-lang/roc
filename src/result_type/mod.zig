//! Shared ResultType enum for JIT execution.
//!
//! This type is used by both llvm_compile (for JIT execution) and eval (for
//! bitcode generation). It's in a separate module so that eval can import it
//! without pulling in LLVM dependencies.

/// Type of the result value for JIT execution.
/// Determines how to interpret the raw bytes returned by roc_eval.
pub const ResultType = enum {
    i64,
    u64,
    i128,
    u128,
    f64,
    dec,
};
