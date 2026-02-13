//! Monomorphic Intermediate Representation (MIR)
//!
//! MIR sits between CIR (Canonical IR) and LIR (Layout IR).
//! It is monomorphic, desugared, and uses globally unique symbols.
//! Lambda set inference happens later on top of MIR.

const std = @import("std");

pub const MIR = @import("MIR.zig");
pub const Monotype = @import("Monotype.zig");
pub const Lower = @import("Lower.zig");

// Re-export commonly used types
pub const Expr = MIR.Expr;
pub const Pattern = MIR.Pattern;
pub const MonoSymbol = MIR.MonoSymbol;
pub const ExprId = MIR.ExprId;
pub const PatternId = MIR.PatternId;
pub const Store = MIR.Store;

test "mir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(MIR);
    std.testing.refAllDecls(Monotype);
    std.testing.refAllDecls(Lower);
}
