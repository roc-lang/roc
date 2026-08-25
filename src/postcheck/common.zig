//! Shared input and small ids for the post-check pipeline.

const std = @import("std");
const base = @import("base");
const check = @import("check");
const layout = @import("layout");
const lir_core = @import("lir_core");

const checked = check.CheckedModule;
const LIR = lir_core.LIR;

/// Span into one of a post-check IR's flat side tables.
///
/// The three post-check IRs all address their side tables the same way, so the
/// type and the append that produces it live here rather than once per store.
pub fn Span(comptime _: type) type {
    return extern struct {
        start: u32,
        len: u32,

        pub fn empty() @This() {
            return .{ .start = 0, .len = 0 };
        }
    };
}

/// Append `values` to a flat side table and return the span addressing them.
/// `list` is any table exposing `len()` and `appendSlice(allocator, values)`.
pub fn appendSpan(
    comptime T: type,
    list: anytype,
    allocator: std.mem.Allocator,
    values: []const T,
) std.mem.Allocator.Error!Span(T) {
    const start: u32 = @intCast(list.len());
    try list.appendSlice(allocator, values);
    return .{ .start = start, .len = @intCast(values.len) };
}

/// `appendSpan` for a table whose spans are never empty. An empty span there
/// would read as "no children" at every consumer, which is a different shape
/// from the one the producer meant, so it fails as an invariant rather than
/// being stored.
pub fn appendNonemptySpan(
    comptime T: type,
    list: anytype,
    allocator: std.mem.Allocator,
    values: []const T,
    comptime message: []const u8,
) std.mem.Allocator.Error!Span(T) {
    if (values.len == 0) invariant(message);
    return try appendSpan(T, list, allocator, values);
}

/// Resource failure while converting checked module data toward LIR.
pub const LowerError = std.mem.Allocator.Error;

/// Crash message for reaching a declaration that has a type annotation and no
/// implementation. Checking reports this as a diagnostic and keeps the declared
/// type, so the program still compiles and crashes only if the declaration is
/// actually reached at runtime.
pub const unimplemented_declaration_crash = "declaration has no implementation";

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
    test_plan_metadata: []const RootTestPlanMetadata = &.{},
    procedure_template_root_grouping: ProcedureTemplateRootGrouping = .isolated,
};

/// Explicit grouping contract for adjacent procedure-template roots.
pub const ProcedureTemplateRootGrouping = enum {
    /// Seal each root in an independent instantiation graph.
    isolated,
    /// Share an instantiation graph across each adjacent template-root group.
    shared_adjacent,
};

/// Checked const data that must produce a runtime layout and callable entries.
pub const StaticDataRequest = struct {
    const_locator: checked.ConstLocator,
    node: ?checked.ConstNodeId = null,
    checked_type: checked.CheckedTypeId,
};

/// Stage-local readonly static-data value id.
pub const StaticDataId = enum(u32) { _ };

/// Optional command-level test-plan metadata for a checked root request.
pub const RootTestPlanMetadata = struct {
    root_order: u32,
    result_index: u32,
    module_index: u32,
    root_index: u32,
};

/// Return the command-level test-plan metadata for a checked root request.
pub fn testPlanMetadataForRoot(
    roots: RootRequests,
    root: checked.RootRequest,
) ?lir_core.RootMetadata.RootMetadata.TestPlanMetadata {
    for (roots.test_plan_metadata) |metadata| {
        if (metadata.root_order != root.order) continue;
        return .{
            .result_index = metadata.result_index,
            .module_index = metadata.module_index,
            .root_index = metadata.root_index,
        };
    }
    return null;
}

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

/// The storage layout of a primitive. This is the single source of truth
/// shared by every post-check layout producer; call it rather than writing a
/// second switch over `CheckedPrimitive`.
pub fn primitiveLayout(primitive: checked.CheckedPrimitive) layout.Idx {
    return switch (primitive) {
        .bool => .bool,
        .str => .str,
        .u8 => .u8,
        .i8 => .i8,
        .u16 => .u16,
        .i16 => .i16,
        .u32 => .u32,
        .i32 => .i32,
        .u64 => .u64,
        .i64 => .i64,
        .u128 => .u128,
        .i128 => .i128,
        .f32 => .f32,
        .f64 => .f64,
        .dec => .dec,
        .u8x16 => .u8x16,
        .i8x16 => .i8x16,
        .u16x8 => .u16x8,
        .i16x8 => .i16x8,
        .u32x4 => .u32x4,
        .i32x4 => .i32x4,
        .u64x2 => .u64x2,
        .i64x2 => .i64x2,
    };
}

/// The low-level op that renders a primitive scalar as a `Str`. This is the
/// single source of truth shared by every post-check inspect lowering; call it
/// rather than writing a second switch over `CheckedPrimitive`. Bool renders
/// through ordinary tag-union inspect and the SIMD vectors render through their
/// explicit `Builtin` bodies, so neither reaches this table.
pub fn primitiveInspectLowLevelOp(primitive: checked.CheckedPrimitive) LIR.LowLevel {
    return switch (primitive) {
        .str => .str_inspect,
        .u8 => .u8_to_str,
        .i8 => .i8_to_str,
        .u16 => .u16_to_str,
        .i16 => .i16_to_str,
        .u32 => .u32_to_str,
        .i32 => .i32_to_str,
        .u64 => .u64_to_str,
        .i64 => .i64_to_str,
        .u128 => .u128_to_str,
        .i128 => .i128_to_str,
        .f32 => .f32_to_str,
        .f64 => .f64_to_str,
        .dec => .dec_to_str,
        .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => invariant("SIMD inspect must lower through its explicit Builtin body"),
        .bool => invariant("Bool must lower as an ordinary tag union before Str.inspect"),
    };
}

/// The `Builtin.Hasher.write_*` low-level op that feeds a primitive scalar into
/// a Hasher. Aggregate types are decomposed before direct hash lowering, so only
/// primitive leaves ever reach this table. This is the single source of truth
/// shared by every post-check hash lowering; call it rather than writing a
/// second switch over `CheckedPrimitive`.
pub fn hasherWriteOp(primitive: checked.CheckedPrimitive) LIR.LowLevel {
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
        .u8x16, .i8x16, .u16x8, .i16x8, .u32x4, .i32x4, .u64x2, .i64x2 => .hasher_write_u128,
    };
}

/// Panic in debug builds for a violated post-check invariant.
pub fn invariant(comptime message: []const u8) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("postcheck invariant violated: {s}", .{message});
    }
    unreachable;
}

/// `invariant` with runtime context formatted into the panic message.
pub fn invariantFmt(comptime fmt: []const u8, args: anytype) noreturn {
    if (@import("builtin").mode == .Debug) {
        std.debug.panic("postcheck invariant violated: " ++ fmt, args);
    }
    unreachable;
}

/// Stop the build with a compiler-bug message in every build mode.
///
/// `invariant` compiles to `unreachable` outside debug builds, which is the
/// right cost for a consistency check whose violation cannot change the code
/// a release build emits. A violated host ABI contract is the other kind: the
/// extern would be emitted at a layout the host was never compiled against and
/// the host's return value would be misread at runtime, with no diagnostic and
/// no crash. That check has to hold in release builds too, so this one reports
/// and aborts instead of becoming undefined behavior.
pub fn compilerBug(message: []const u8) noreturn {
    std.debug.panic("compiler bug: {s}", .{message});
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
