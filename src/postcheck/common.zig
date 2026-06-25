//! Shared input and small ids for the post-check pipeline.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const lir_core = @import("lir_core");
const MonoType = @import("monotype/type.zig");

const checked = check.CheckedModule;
const LIR = lir_core.LIR;

/// Resource failure while converting checked module data toward LIR.
pub const LowerError = std.mem.Allocator.Error;

/// Root module plus imported modules visible to post-check stages.
pub const CheckedModules = struct {
    root: checked.LoweringModuleView,
    imports: []const checked.ImportedModuleView = &.{},
};

/// Explicit roots requested from checked module data.
pub const RootRequests = struct {
    requests: []const checked.RootRequest = &.{},
    layout_requests: []const checked.CheckedTypeId = &.{},
    static_data_requests: []const StaticDataRequest = &.{},
};

/// Checked const data that must produce a runtime layout and callable entries.
pub const StaticDataRequest = struct {
    data: checked.ProvidedDataExport,
};

/// Target settings carried through post-check lowering.
pub const Target = struct {
    target_usize: base.target.TargetUsize = base.target.TargetUsize.native,
    checked_module_state: CheckedModuleState = .complete,
};

/// Whether checking is complete or running compile-time finalization.
pub const CheckedModuleState = enum {
    complete,
    checking_finalization,
};

/// Stage-local symbol id for generated locals and procedures.
pub const Symbol = enum(u32) { _ };
/// Stage-local compile-time constant node id.
pub const ConstNodeId = enum(u32) { _ };
/// Stage-local finite callable set id.
pub const FnSetId = enum(u32) { _ };
/// Stage-local erased callable entry set id.
pub const ErasedFnsId = enum(u32) { _ };
/// Stage-local capture slot id.
pub const CaptureSlotId = enum(u32) { _ };

/// The `Builtin.Hasher.write_*` low-level op that feeds a primitive scalar of
/// the given monotype into a Hasher. Aggregate types are decomposed in Monotype
/// lowering, so only primitive leaves ever reach direct hash lowering.
pub fn hasherWriteOp(primitive: MonoType.Primitive) LIR.LowLevel {
    return switch (primitive) {
        .bool => .hasher_write_bool,
        .str => .hasher_write_str,
        .u8 => .hasher_write_u8,
        .i8 => .hasher_write_i8,
        .u16 => .hasher_write_u16,
        .i16 => .hasher_write_i16,
        .u32 => .hasher_write_u32,
        .i32 => .hasher_write_i32,
        .u64 => .hasher_write_u64,
        .i64 => .hasher_write_i64,
        .u128 => .hasher_write_u128,
        .i128 => .hasher_write_i128,
        .f32 => .hasher_write_f32,
        .f64 => .hasher_write_f64,
        .dec => .hasher_write_dec,
    };
}

/// Panic in debug builds for a violated post-check invariant.
pub fn invariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("postcheck invariant violated: {s}", .{message});
    }
    unreachable;
}

/// Monotonic symbol id generator for post-check stages.
pub const SymbolGen = struct {
    next: u32 = 0,

    pub fn fresh(self: *SymbolGen) Symbol {
        const symbol: Symbol = @enumFromInt(self.next);
        self.next += 1;
        return symbol;
    }
};

test "common declarations are referenced" {
    std.testing.refAllDecls(@This());
}
