//! Monomorphic Intermediate Representation (MIR)
//!
//! MIR sits between specialized structured callable IR and LIR.
//! It is monomorphic, statement-oriented, local-centric, and uses globally
//! unique symbols only at top-level/global materialization boundaries.
//! Lambda-set solving and specialization must happen before MIR lowering.

const std = @import("std");

pub const MIR = @import("MIR.zig");
pub const Analyses = @import("Analyses.zig");
/// MIR exact-callable summary analysis.
pub const CallableSummary = @import("CallableSummary.zig");
pub const DebugVerifyMir = @import("DebugVerifyMir.zig");
pub const Monotype = @import("Monotype.zig");
/// Transitional coordinator for the staged CoreCIR -> ContextMono ->
/// LambdaSolved -> LambdaSpecialize pipeline.
pub const Monomorphize = @import("Monomorphize.zig");
pub const Lower = @import("Lower.zig");
pub const ResultSummary = @import("ResultSummary.zig");

/// Re-export of MIR control-flow statement type
pub const CFStmt = MIR.CFStmt;
/// Re-export of MIR local type
pub const Local = MIR.Local;
/// Re-export of MIR local id type
pub const LocalId = MIR.LocalId;
/// Re-export of MIR local-id span type
pub const LocalSpan = MIR.LocalSpan;
/// Globally unique opaque symbol identifier
pub const Symbol = MIR.Symbol;
/// Index into the MIR statement store
pub const CFStmtId = MIR.CFStmtId;
/// Index into the MIR lambda store
pub const LambdaId = MIR.LambdaId;
/// MIR lambda metadata
pub const Lambda = MIR.Lambda;
/// Index into the MIR constant-definition store
pub const ConstDefId = MIR.ConstDefId;
/// MIR top-level constant definition metadata
pub const ConstDef = MIR.ConstDef;
/// MIR statement/pattern/local/lambda store
pub const Store = MIR.Store;

test "mir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(MIR);
    std.testing.refAllDecls(Analyses);
    std.testing.refAllDecls(CallableSummary);
    std.testing.refAllDecls(DebugVerifyMir);
    std.testing.refAllDecls(Monotype);
    std.testing.refAllDecls(Monomorphize);
    std.testing.refAllDecls(ResultSummary);
}
