//! Statement-only LIR module.

const std = @import("std");
const DebugOwnershipSummary = @import("DebugOwnershipSummary.zig");
const DebugVerifyLir = @import("DebugVerifyLir.zig");

pub const LIR = @import("LIR.zig");
pub const LirStore = @import("LirStore.zig");
pub const TailRecursion = @import("TailRecursion.zig");
pub const MirToLir = @import("MirToLir.zig");
pub const RcInsert = @import("rc_insert.zig");

pub const Symbol = LIR.Symbol;
pub const LocalRef = LIR.LocalRef;
pub const LocalRefSpan = LIR.LocalRefSpan;
pub const JoinPointId = LIR.JoinPointId;
pub const BorrowScopeId = LIR.BorrowScopeId;
pub const LiteralValue = LIR.LiteralValue;
pub const HostedProc = LIR.HostedProc;
pub const AliasedRef = LIR.AliasedRef;
pub const BorrowRegion = LIR.BorrowRegion;
pub const BorrowedRef = LIR.BorrowedRef;
pub const ResultSemantics = LIR.ResultSemantics;
pub const RefOp = LIR.RefOp;
pub const RefProjection = LIR.RefProjection;
pub const RefProjectionSpan = LIR.RefProjectionSpan;
pub const ParamRefContract = LIR.ParamRefContract;
pub const ProcResultContract = LIR.ProcResultContract;
pub const CFStmt = LIR.CFStmt;
pub const CFStmtId = LIR.CFStmtId;
pub const CFSwitchBranch = LIR.CFSwitchBranch;
pub const CFSwitchBranchSpan = LIR.CFSwitchBranchSpan;
pub const LirProcSpec = LIR.LirProcSpec;
pub const LirProcSpecId = LIR.LirProcSpecId;

test "lir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(LIR);
    std.testing.refAllDecls(LirStore);
    std.testing.refAllDecls(MirToLir);
    std.testing.refAllDecls(DebugOwnershipSummary);
    std.testing.refAllDecls(DebugVerifyLir);
    std.testing.refAllDecls(TailRecursion);
    std.testing.refAllDecls(RcInsert);
}
