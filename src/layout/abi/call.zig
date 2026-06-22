//! Target-agnostic lowering of a hosted-call signature into register/memory placements,
//! computed from the per-target C-ABI classifiers (`aarch64.zig`, `x86_64.zig`, `wasm.zig`).
//!
//! A `LoweredCall` says, for the return value and each argument, whether it travels in
//! registers (and which bytes go in which register file) or "indirect" (memory). Each
//! consumer interprets "indirect" for its own world:
//!   - the LLVM backend emits a pointer parameter, adding `byval` only for targets whose C
//!     ABI passes that memory-class argument as a stack copy rather than a pointer;
//!   - the dev backend and the interpreter's call trampoline place the bytes in the stack
//!     argument area or pass a pointer to a copy, per the target.
//!
//! This is the single place the classification is turned into placements, shared by every
//! consumer so they agree by construction.

const std = @import("std");

const layout = @import("../layout.zig");
const store_mod = @import("../store.zig");
const aarch64 = @import("aarch64.zig");
const x86_64 = @import("x86_64.zig");
const wasm = @import("wasm.zig");

const Store = store_mod.Store;
const Idx = layout.Idx;

/// Which register file a piece of a value travels in.
pub const RegClass = enum { integer, float };

/// One register's worth of a value: which register file, the byte offset within the value's
/// in-memory representation that this register carries, and how many bytes (1..16; >8 only for
/// a 128-bit value in a single SSE register).
pub const RegPiece = struct {
    class: RegClass,
    offset: u16,
    size: u8,
};

/// How a single value (an argument or the return) is passed.
pub const Placement = union(enum) {
    /// Zero-sized: not passed at all.
    none,
    /// Passed/returned in these registers, in order.
    registers: []const RegPiece,
    /// Passed in memory: by `byval`/`sret` pointer or stack copy, per the consumer/target.
    indirect,
};

/// The full lowering of a hosted call's signature.
pub const LoweredCall = struct {
    /// Whether `*RocOps` is prepended as the leading integer-register argument.
    leading_ops: bool,
    ret: Placement,
    /// One placement per source argument, in order (not counting the leading `*RocOps`).
    args: []const Placement,
};

/// The host targets whose C ABI we lower for. AArch64 is uniform across OSes (Windows on
/// ARM64 follows AAPCS64 for fixed prototypes); x86-64 splits System V vs Windows; wasm's
/// pointer width is reflected in the store's `targetUsize`.
pub const Target = enum { aarch64, x86_64_sysv, x86_64_windows, wasm32, wasm64 };

/// Lower a hosted call's signature for `target`. `arg_idxs`/`ret_idx` are layout indices;
/// `needs_ops` (from `needsRocOps`) decides the leading `*RocOps`. Allocations for the
/// returned slices come from `arena`.
pub fn lower(
    arena: std.mem.Allocator,
    store: *const Store,
    target: Target,
    arg_idxs: []const Idx,
    ret_idx: Idx,
    needs_ops: bool,
) std.mem.Allocator.Error!LoweredCall {
    const args = try arena.alloc(Placement, arg_idxs.len);
    for (arg_idxs, args) |arg_idx, *placement| {
        placement.* = try placementFor(arena, store, target, arg_idx, .arg);
    }
    return .{
        .leading_ops = needs_ops,
        .ret = try placementFor(arena, store, target, ret_idx, .ret),
        .args = args,
    };
}

const Context = enum { arg, ret };

fn placementFor(
    arena: std.mem.Allocator,
    store: *const Store,
    target: Target,
    idx: Idx,
    ctx: Context,
) std.mem.Allocator.Error!Placement {
    const lay = store.getLayout(idx);
    if (store.layoutSize(lay) == 0) return .none;

    return switch (target) {
        .aarch64 => placementAarch64(arena, store, idx),
        .x86_64_sysv => placementSysV(arena, store, idx, ctx),
        .x86_64_windows => placementWin64(arena, store, idx, ctx),
        .wasm32, .wasm64 => placementWasm(arena, store, idx),
    };
}

fn onePiece(arena: std.mem.Allocator, class: RegClass, offset: u16, size: u8) std.mem.Allocator.Error!Placement {
    const pieces = try arena.alloc(RegPiece, 1);
    pieces[0] = .{ .class = class, .offset = offset, .size = size };
    return .{ .registers = pieces };
}

/// Split a `size`-byte integer value into 8-byte general-purpose register pieces.
fn integerPieces(arena: std.mem.Allocator, size: u32) std.mem.Allocator.Error!Placement {
    const count = (size + 7) / 8;
    const pieces = try arena.alloc(RegPiece, count);
    var i: u32 = 0;
    while (i < count) : (i += 1) {
        pieces[i] = .{
            .class = .integer,
            .offset = @intCast(i * 8),
            .size = @intCast(@min(8, size - i * 8)),
        };
    }
    return .{ .registers = pieces };
}

fn placementAarch64(arena: std.mem.Allocator, store: *const Store, idx: Idx) std.mem.Allocator.Error!Placement {
    const lay = store.getLayout(idx);
    const size = store.layoutSize(lay);
    switch (aarch64.classifyType(store, idx)) {
        .memory => return .indirect,
        .integer => return onePiece(arena, .integer, 0, @intCast(size)),
        .double_integer => return integerPieces(arena, size),
        .float_array => |fa| {
            const elem_bytes: u8 = @intCast(fa.elem_bits / 8);
            const pieces = try arena.alloc(RegPiece, fa.count);
            var i: u8 = 0;
            while (i < fa.count) : (i += 1) {
                pieces[i] = .{ .class = .float, .offset = @as(u16, i) * elem_bytes, .size = elem_bytes };
            }
            return .{ .registers = pieces };
        },
        .byval => switch (lay.tag) {
            .scalar => {
                const scalar = lay.getScalar();
                if (scalar.tag == .frac) {
                    return switch (scalar.getFrac()) {
                        .f32 => onePiece(arena, .float, 0, 4),
                        .f64 => onePiece(arena, .float, 0, 8),
                        .dec => integerPieces(arena, size),
                    };
                }
                return integerPieces(arena, size);
            },
            .box, .box_of_zst, .ptr => return integerPieces(arena, size),
            else => unreachable,
        },
    }
}

fn placementSysV(arena: std.mem.Allocator, store: *const Store, idx: Idx, ctx: Context) std.mem.Allocator.Error!Placement {
    const size = store.layoutSize(store.getLayout(idx));
    const classes = x86_64.classifySystemV(store, idx, if (ctx == .ret) .ret else .arg);
    if (classes[0] == .memory) return .indirect;

    var pieces = std.ArrayList(RegPiece).empty;
    var i: usize = 0;
    while (i < classes.len) : (i += 1) {
        if (classes[i] == .none) break;
        const offset: u16 = @intCast(i * 8);
        const piece_size: u8 = @intCast(@min(@as(u32, 8), size - offset));
        switch (classes[i]) {
            .integer => try pieces.append(arena, .{ .class = .integer, .offset = offset, .size = piece_size }),
            .sse, .float, .float_combine => try pieces.append(arena, .{ .class = .float, .offset = offset, .size = piece_size }),
            // x87/sseup/win_i128 do not occur for Roc types under System V.
            else => return .indirect,
        }
    }
    return .{ .registers = try pieces.toOwnedSlice(arena) };
}

fn placementWin64(arena: std.mem.Allocator, store: *const Store, idx: Idx, ctx: Context) std.mem.Allocator.Error!Placement {
    const size = store.layoutSize(store.getLayout(idx));
    switch (x86_64.classifyWindows(store, idx)) {
        .memory => return .indirect,
        .integer => return onePiece(arena, .integer, 0, @intCast(size)),
        .sse => return onePiece(arena, .float, 0, @intCast(@min(@as(u32, 16), size))),
        // Win64 passes a 128-bit integer in memory but returns it in an SSE register.
        .win_i128 => return if (ctx == .ret) onePiece(arena, .float, 0, 16) else .indirect,
        else => return .indirect,
    }
}

fn placementWasm(arena: std.mem.Allocator, store: *const Store, idx: Idx) std.mem.Allocator.Error!Placement {
    switch (wasm.classifyType(store, idx)) {
        .indirect => return .indirect,
        .direct => |direct_idx| {
            const size = store.layoutSize(store.getLayout(direct_idx));
            const dlay = store.getLayout(direct_idx);
            const is_float = dlay.tag == .scalar and dlay.getScalar().tag == .frac and dlay.getScalar().getFrac() != .dec;
            const class: RegClass = if (is_float) .float else .integer;
            if (wasm.lowerAsDoubleI64(store, direct_idx)) {
                // A value wider than 64 bits is passed as two i64s.
                return integerPieces(arena, size);
            }
            return onePiece(arena, class, 0, @intCast(size));
        },
    }
}

const testing = std.testing;

fn testStruct(store: *Store, field_idxs: []const Idx) std.mem.Allocator.Error!Idx {
    var fields: [16]layout.StructField = undefined;
    for (field_idxs, 0..) |field_idx, i| {
        fields[i] = .{ .index = @intCast(i), .layout = field_idx };
    }
    return store.putStructFields(fields[0..field_idxs.len]);
}

fn expectRegisters(expected: []const RegPiece, actual: Placement) error{ TestUnexpectedResult, TestExpectedEqual }!void {
    try testing.expect(actual == .registers);
    try testing.expectEqualSlices(RegPiece, expected, actual.registers);
}

test "lower aarch64: random_plant(i32) -> Plant is all registers, no ops" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const plant = try testStruct(&store, &.{ .i32, .u32 });
    const call = try lower(arena, &store, .aarch64, &.{.i32}, plant, false);

    try testing.expect(!call.leading_ops);
    // i32 arg -> one integer register, 4 bytes.
    try testing.expectEqual(@as(usize, 1), call.args.len);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 4 }}, call.args[0]);
    // Plant return -> one integer register, 8 bytes.
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, call.ret);
}

test "lower aarch64: HFA and large aggregates" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // { f64, f64 } -> two float registers.
    const two_f64 = try testStruct(&store, &.{ .f64, .f64 });
    const c1 = try lower(arena, &store, .aarch64, &.{}, two_f64, false);
    try expectRegisters(&.{
        .{ .class = .float, .offset = 0, .size = 8 },
        .{ .class = .float, .offset = 8, .size = 8 },
    }, c1.ret);

    // RocStr arg -> indirect (24 bytes); needs ops since Str is heap.
    const c2 = try lower(arena, &store, .aarch64, &.{.str}, .i32, true);
    try testing.expect(c2.leading_ops);
    try testing.expectEqual(Placement.indirect, c2.args[0]);
}

test "lower aarch64: pointer-shaped byval layouts use integer registers" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    // Idx 32 has the bit pattern that makes a box/ptr layout's raw data
    // decode as ScalarTag.frac with invalid precision if treated as scalar.
    const target_elem_idx_int: u32 = 32;
    var elem_idx_opt: ?Idx = null;
    var i: u32 = 0;
    while (store.layouts.len() <= target_elem_idx_int) : (i += 1) {
        const idx = try store.insertLayout(layout.Layout.list(@enumFromInt(i)));
        if (@intFromEnum(idx) == target_elem_idx_int) elem_idx_opt = idx;
    }
    const elem_idx = elem_idx_opt.?;

    const box_idx = try store.insertLayout(layout.Layout.box(elem_idx));
    const ptr_idx = try store.insertLayout(layout.Layout.ptr(elem_idx));
    const call = try lower(arena, &store, .aarch64, &.{ box_idx, ptr_idx }, box_idx, false);

    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, call.args[0]);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, call.args[1]);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, call.ret);
}

test "lower x86_64 sysv: Plant in one int eightbyte, mixed struct splits" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const plant = try testStruct(&store, &.{ .i32, .u32 });
    const c1 = try lower(arena, &store, .x86_64_sysv, &.{.i32}, plant, false);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, c1.ret);

    const mixed = try testStruct(&store, &.{ .i64, .f64 });
    const c2 = try lower(arena, &store, .x86_64_sysv, &.{}, mixed, false);
    try expectRegisters(&.{
        .{ .class = .integer, .offset = 0, .size = 8 },
        .{ .class = .float, .offset = 8, .size = 8 },
    }, c2.ret);
}

test "lower win64: 16-byte struct is indirect, Plant is one register" {
    var store = try Store.init(testing.allocator, .u64);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const plant = try testStruct(&store, &.{ .i32, .u32 });
    const c1 = try lower(arena, &store, .x86_64_windows, &.{}, plant, false);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, c1.ret);

    const two_words = try testStruct(&store, &.{ .i64, .i64 });
    const c2 = try lower(arena, &store, .x86_64_windows, &.{two_words}, .i32, false);
    try testing.expectEqual(Placement.indirect, c2.args[0]);
}

test "lower wasm32: single-variant tag union returns payload register" {
    var store = try Store.init(testing.allocator, .u32);
    defer store.deinit();
    var arena_state = std.heap.ArenaAllocator.init(testing.allocator);
    defer arena_state.deinit();
    const arena = arena_state.allocator();

    const box_idx = try store.insertLayout(layout.Layout.box(.u64));
    const wrapped_u64 = try store.putTagUnion(&.{.u64});
    const call = try lower(arena, &store, .wasm32, &.{ box_idx, box_idx }, wrapped_u64, false);

    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 8 }}, call.ret);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 4 }}, call.args[0]);
    try expectRegisters(&.{.{ .class = .integer, .offset = 0, .size = 4 }}, call.args[1]);
}
