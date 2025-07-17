//! REPL evaluation module that processes expressions and maintains state

const repl_impl = @import("repl_full.zig");

/// Read-Eval-Print Loop implementation for interactive Roc expression evaluation
pub const Repl = repl_impl.Repl;
