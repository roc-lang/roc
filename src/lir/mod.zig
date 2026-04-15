//! Statement-only LIR module.

const std = @import("std");

/// Core statement-only LIR type definitions.
pub const LIR = @import("LIR.zig");
/// Flat storage for statement-only LIR nodes and spans.
pub const LirStore = @import("LirStore.zig");
/// Tail-recursion rewriting for statement-only LIR procs.
pub const TailRecursion = @import("TailRecursion.zig");
/// Ownership fact inference for statement-only LIR.
pub const Ownership = @import("Ownership.zig");
/// Explicit RC statement insertion for statement-only LIR.
pub const RcInsert = @import("RcInsert.zig");
/// Ownership-boundary markers and invariant traps for non-builtin RC sites.
pub const OwnershipBoundary = @import("OwnershipBoundary.zig");
/// Lower cor-style IR into statement-only LIR.
pub const FromIr = @import("FromIr.zig");

/// Symbol identifiers used throughout statement-only LIR.
pub const Symbol = LIR.Symbol;
/// Explicit local metadata used throughout statement-only LIR.
pub const Local = LIR.Local;
/// Identifier of one LIR local.
pub const LocalId = LIR.LocalId;
/// Span into flat local-id storage.
pub const LocalSpan = LIR.LocalSpan;
/// Identifier for LIR join points.
pub const JoinPointId = LIR.JoinPointId;
/// Identifier for lexical borrow scopes.
pub const BorrowScopeId = LIR.BorrowScopeId;
/// Literal RHS values assignable in statement-only LIR.
pub const LiteralValue = LIR.LiteralValue;
/// Platform-hosted proc metadata.
pub const HostedProc = LIR.HostedProc;
/// Alias provenance rooted in another local.
pub const AliasedRef = LIR.AliasedRef;
/// Lifetime region for borrowed values.
pub const BorrowRegion = LIR.BorrowRegion;
/// Borrow provenance rooted in another local.
pub const BorrowedRef = LIR.BorrowedRef;
/// Ownership/provenance summary for a statement result.
pub const ResultSemantics = LIR.ResultSemantics;
/// Physical result materialization kind attached to value-producing statements.
pub const ResultMaterialization = LIR.ResultMaterialization;
/// Extra ownership data attached to value-producing statements.
pub const OwnershipSemantics = LIR.OwnershipSemantics;
/// Ref-producing operations lowerable by `assign_ref`.
pub const RefOp = LIR.RefOp;
/// Projection step applied when tracking aliases and borrows.
pub const RefProjection = LIR.RefProjection;
/// Span into flat ref-projection storage.
pub const RefProjectionSpan = LIR.RefProjectionSpan;
/// Param-relative provenance contract.
pub const ParamRefContract = LIR.ParamRefContract;
/// Proc-level return provenance contract.
pub const ProcResultContract = LIR.ProcResultContract;
/// Canonical statement/control-flow node.
pub const CFStmt = LIR.CFStmt;
/// Identifier of a stored `CFStmt`.
pub const CFStmtId = LIR.CFStmtId;
/// One explicit switch branch.
pub const CFSwitchBranch = LIR.CFSwitchBranch;
/// Span into flat switch-branch storage.
pub const CFSwitchBranchSpan = LIR.CFSwitchBranchSpan;
/// Stored proc specification rooted at a statement body.
pub const LirProcSpec = LIR.LirProcSpec;
/// Identifier of a stored proc specification.
pub const LirProcSpecId = LIR.LirProcSpecId;
/// Builtin low-level operation identifier reused from `base`.
pub const LowLevel = LIR.LowLevel;

test "lir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(LIR);
    std.testing.refAllDecls(LirStore);
    std.testing.refAllDecls(TailRecursion);
    std.testing.refAllDecls(Ownership);
    std.testing.refAllDecls(RcInsert);
    std.testing.refAllDecls(OwnershipBoundary);
    std.testing.refAllDecls(FromIr);
}
