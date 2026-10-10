//! Compiler-owned builtin source, independent of checked builtin artifacts.
/// The source used by both builtin compilation and parse-only declaration analysis.
pub const source = @embedFile("roc/Builtin.roc");
