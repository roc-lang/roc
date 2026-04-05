//! Statement-only LIR module.

const std = @import("std");
const DebugOwnershipSummary = @import("DebugOwnershipSummary.zig");
const DebugVerifyLir = @import("DebugVerifyLir.zig");

/// Core statement-only LIR type definitions.
pub const LIR = @import("LIR.zig");
/// Flat storage for statement-only LIR nodes and spans.
pub const LirStore = @import("LirStore.zig");
/// Tail-recursion rewriting for statement-only LIR procs.
pub const TailRecursion = @import("TailRecursion.zig");
/// Reference-count insertion over explicit LIR statements.
pub const RcInsert = @import("rc_insert.zig");

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
    std.testing.refAllDecls(DebugOwnershipSummary);
    std.testing.refAllDecls(DebugVerifyLir);
    std.testing.refAllDecls(TailRecursion);
    std.testing.refAllDecls(RcInsert);
}
