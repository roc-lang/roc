//! Monomorphic Intermediate Representation (MIR)
//!
//! MIR sits between CIR (Canonical IR) and LIR (Layout IR).
//! It is monomorphic, desugared, and uses globally unique symbols.
//! Lambda set inference happens later on top of MIR.

const std = @import("std");

pub const MIR = @import("MIR.zig");
pub const Monotype = @import("Monotype.zig");
pub const Lower = @import("Lower.zig");

/// Re-export of MIR expression type
pub const Expr = MIR.Expr;
/// Re-export of MIR pattern type
pub const Pattern = MIR.Pattern;
/// Globally unique symbol identifier (module + ident)
pub const Symbol = MIR.Symbol;
/// Index into the MIR expression store
pub const ExprId = MIR.ExprId;
/// Index into the MIR pattern store
pub const PatternId = MIR.PatternId;
/// MIR expression and pattern store with parallel type mapping
pub const Store = MIR.Store;

test "mir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(MIR);
    std.testing.refAllDecls(Monotype);
    std.testing.refAllDecls(Lower);
    std.testing.refAllDecls(@import("test/lower_test.zig"));
}
