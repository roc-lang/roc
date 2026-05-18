//! Statement-only LIR module.

const std = @import("std");

/// Core statement-only LIR type definitions.
pub const LIR = @import("LIR.zig");
/// Flat storage for statement-only LIR nodes and spans.
pub const LirStore = @import("LirStore.zig");
/// Source-blind IR-to-LIR lowering boundary.
pub const LowerIr = @import("lower_ir.zig");
/// Public checked-artifact-to-LIR lowering entrypoint.
pub const CheckedPipeline = @import("checked_pipeline.zig");
/// Mechanical ARC insertion over explicit LIR values and control flow.
pub const Arc = @import("arc.zig");
/// Shared-memory ARC-inserted LIR runtime image for interpreter-shim execution.
pub const RuntimeImage = @import("runtime_image.zig");

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
/// Literal RHS values assignable in statement-only LIR.
pub const LiteralValue = LIR.LiteralValue;
/// Platform-hosted proc metadata.
pub const HostedProc = LIR.HostedProc;
/// Ref-producing operations lowerable by `assign_ref`.
pub const RefOp = LIR.RefOp;
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
    std.testing.refAllDecls(LowerIr);
    std.testing.refAllDecls(CheckedPipeline);
    std.testing.refAllDecls(Arc);
    std.testing.refAllDecls(RuntimeImage);
}
