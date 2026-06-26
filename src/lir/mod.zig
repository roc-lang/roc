//! Statement-only LIR module.

const std = @import("std");
const core = @import("lir_core");

/// Core statement-only LIR type definitions.
pub const LIR = core.LIR;
/// Resolved source location recorded in LIR side tables.
pub const SourceLoc = @import("base").SourceLoc;
/// Flat storage for statement-only LIR nodes and spans.
pub const LirStore = core.LirStore;
/// LIR-owned root metadata.
pub const RootMetadata = core.RootMetadata;
/// Hosted ABI metadata carried by LIR proc specs.
pub const Hosted = core.Hosted;
/// LIR program result shared by post-check lowering and consumers.
pub const Program = core.Program;
/// Public checked-module-to-LIR lowering entrypoint.
pub const CheckedPipeline = @import("checked_pipeline.zig");
/// Struct-typed join parameters split into per-field parameters before ARC.
pub const ScalarizeJoins = @import("scalarize_joins.zig");
/// Switch branch pruning from explicit possible-tag analysis.
pub const TagReachability = @import("tag_reachability.zig");
/// Demand-driven proc compaction before ARC and backend emission.
pub const ReachableProcs = @import("reachable_procs.zig");
/// ARC borrow inference and RC statement insertion over explicit LIR.
pub const Arc = @import("arc.zig");
/// Tail recursion modulo constructor + plain tail-call elimination.
pub const Trmc = @import("trmc.zig");
/// Compact textual LIR dumps for golden tests and debug flags.
pub const DebugPrint = @import("debug_print.zig");
/// ARC-stage per-proc ownership signatures.
pub const ArcSig = @import("arc_sig.zig");
/// ARC borrow-inference solver over ownership-neutral LIR.
pub const ArcSolve = @import("arc_solve.zig");
/// Debug borrow certifier for ARC-complete LIR.
pub const ArcCertify = @import("arc_certify.zig");
/// Shared-memory ARC-inserted LIR image for interpreter-shim execution.
pub const LirImage = @import("lir_image.zig");

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
/// Explicit proc-level native stack probing contract.
pub const StackProbe = LIR.StackProbe;
/// Native stack probe page-size threshold used by LIR producers/consumers.
pub const stack_probe_page_size = LIR.stack_probe_page_size;
/// Identifier of a stored proc specification.
pub const LirProcSpecId = LIR.LirProcSpecId;
/// Builtin low-level operation identifier reused from `base`.
pub const LowLevel = LIR.LowLevel;
/// Pattern type used in LIR.
pub const LirPattern = LIR.LirPattern;
/// Identifier of a stored LirPattern.
pub const LirPatternId = LIR.LirPatternId;
/// Span into flat pattern-id storage.
pub const LirPatternSpan = LIR.LirPatternSpan;

test "lir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(LIR);
    std.testing.refAllDecls(LirStore);
    std.testing.refAllDecls(RootMetadata);
    std.testing.refAllDecls(Hosted);
    std.testing.refAllDecls(Program);
    std.testing.refAllDecls(ReachableProcs);
    std.testing.refAllDecls(CheckedPipeline);
    std.testing.refAllDecls(ScalarizeJoins);
    std.testing.refAllDecls(TagReachability);
    std.testing.refAllDecls(Arc);
    std.testing.refAllDecls(ArcSig);
    std.testing.refAllDecls(ArcSolve);
    std.testing.refAllDecls(ArcCertify);
    std.testing.refAllDecls(LirImage);
}
