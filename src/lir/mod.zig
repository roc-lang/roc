//! Statement-only LIR module.

const std = @import("std");
const core = @import("lir_core");
const builtins = @import("builtins");

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
/// Direct boxed update wrapper rewrite before ARC.
pub const BoxReuse = @import("box_reuse.zig");
/// Internal aggregate return-slot variants before ARC.
pub const ReturnSlot = @import("return_slot.zig");
/// Internal append-into-string variants before ARC.
pub const StrAppend = @import("str_append.zig");
/// Shared proc-body cloning and rewrite-soundness helpers before ARC.
pub const BodyClone = @import("body_clone.zig");
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
/// Checked integer arithmetic metadata shared by LIR producers and consumers.
pub const CheckedArithmetic = core.CheckedArithmetic;
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
/// Identifier for a materialized readonly static-data value.
pub const StaticDataId = LIR.StaticDataId;
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

/// Domain category byte mixed into a builtin Hasher for a `hasher_write_*`
/// low-level op. This is the single source of truth shared by the interpreter
/// and every backend, so their hashes stay identical. The mapping is total over
/// the hashing ops (bool, the fixed-width ints, f32, f64, Dec, bytes, and str);
/// callers only pass `hasher_write_*` ops.
pub fn hasherDomain(op: LowLevel) builtins.hash.HasherDomain {
    return switch (op) {
        .hasher_write_bool => .bool,
        .hasher_write_u8 => .u8,
        .hasher_write_u16 => .u16,
        .hasher_write_u32 => .u32,
        .hasher_write_u64 => .u64,
        .hasher_write_u128 => .u128,
        .hasher_write_i8 => .i8,
        .hasher_write_i16 => .i16,
        .hasher_write_i32 => .i32,
        .hasher_write_i64 => .i64,
        .hasher_write_i128 => .i128,
        .hasher_write_f32 => .f32,
        .hasher_write_f64 => .f64,
        .hasher_write_dec => .dec,
        .hasher_write_bytes => .bytes,
        .hasher_write_str => .str,
        else => unreachable,
    };
}

/// Byte width of the scalar handed to `hasher_write_u64` for a fixed-width
/// `hasher_write_*` op. Defined for the 1/2/4/8-byte scalar ops, including f32
/// and f64; the u128, Dec, bytes, and str ops travel their own wider paths and
/// never call this.
pub fn hasherU64Width(op: LowLevel) u8 {
    return switch (op) {
        .hasher_write_bool,
        .hasher_write_u8,
        .hasher_write_i8,
        => 1,
        .hasher_write_u16,
        .hasher_write_i16,
        => 2,
        .hasher_write_u32,
        .hasher_write_i32,
        .hasher_write_f32,
        => 4,
        .hasher_write_u64,
        .hasher_write_i64,
        .hasher_write_f64,
        => 8,
        else => unreachable,
    };
}

test "lir tests" {
    std.testing.refAllDecls(@This());
    std.testing.refAllDecls(LIR);
    std.testing.refAllDecls(LirStore);
    std.testing.refAllDecls(RootMetadata);
    std.testing.refAllDecls(Hosted);
    std.testing.refAllDecls(Program);
    std.testing.refAllDecls(ReachableProcs);
    std.testing.refAllDecls(CheckedPipeline);
    std.testing.refAllDecls(BoxReuse);
    std.testing.refAllDecls(ReturnSlot);
    std.testing.refAllDecls(StrAppend);
    std.testing.refAllDecls(BodyClone);
    std.testing.refAllDecls(ScalarizeJoins);
    std.testing.refAllDecls(TagReachability);
    std.testing.refAllDecls(CheckedArithmetic);
    std.testing.refAllDecls(Arc);
    std.testing.refAllDecls(ArcSig);
    std.testing.refAllDecls(ArcSolve);
    std.testing.refAllDecls(ArcCertify);
    std.testing.refAllDecls(LirImage);
}
