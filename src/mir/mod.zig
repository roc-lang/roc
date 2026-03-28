//! Monomorphic Intermediate Representation (MIR)
//!
//! MIR sits between CIR (Canonical IR) and LIR (Layout IR).
//! It is monomorphic, statement-oriented, local-centric, and uses globally
//! unique symbols only at top-level/global materialization boundaries.
//! Lambda set inference happens later on top of MIR.

const std = @import("std");

pub const MIR = @import("MIR.zig");
pub const Analyses = @import("Analyses.zig");
pub const Monotype = @import("Monotype.zig");
pub const Monomorphize = @import("Monomorphize.zig");
pub const Lower = @import("Lower.zig");
pub const LambdaSet = @import("LambdaSet.zig");
pub const ProcResultSummary = @import("ProcResultSummary.zig");

/// Re-export of MIR control-flow statement type
pub const CFStmt = MIR.CFStmt;
/// Re-export of MIR local type
pub const Local = MIR.Local;
/// Re-export of MIR local id type
pub const LocalId = MIR.LocalId;
/// Re-export of MIR local-id span type
pub const LocalSpan = MIR.LocalSpan;
/// Re-export of MIR pattern type
pub const Pattern = MIR.Pattern;
/// Globally unique opaque symbol identifier
pub const Symbol = MIR.Symbol;
/// Index into the MIR statement store
pub const CFStmtId = MIR.CFStmtId;
/// Index into the MIR pattern store
pub const PatternId = MIR.PatternId;
/// Index into the MIR lambda store
pub const LambdaId = MIR.LambdaId;
/// MIR lambda metadata
pub const Lambda = MIR.Lambda;
/// Index into the MIR constant-definition store
pub const ConstDefId = MIR.ConstDefId;
/// Index into the MIR function-definition store
pub const FunctionDefId = MIR.FunctionDefId;
/// MIR top-level constant definition metadata
pub const ConstDef = MIR.ConstDef;
/// MIR top-level function definition metadata
pub const FunctionDef = MIR.FunctionDef;
/// MIR statement/pattern/local/lambda store
pub const Store = MIR.Store;

test "mir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(MIR);
    std.testing.refAllDecls(Analyses);
    std.testing.refAllDecls(Monotype);
    std.testing.refAllDecls(Monomorphize);
    std.testing.refAllDecls(Lower);
    std.testing.refAllDecls(LambdaSet);
    std.testing.refAllDecls(ProcResultSummary);
    std.testing.refAllDecls(@import("test/lower_test.zig"));
}
