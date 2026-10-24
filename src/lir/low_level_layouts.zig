//! Debug-only verification that every low-level operation in LIR has the
//! layouts its signature declares (`base.low_level_signatures`).
//!
//! Backends read an operation's operands and result through their layouts and
//! never second-guess them, so an operation whose layouts differ from its
//! signature is a compiler bug in whichever stage produced it. Only statements
//! a procedure reaches are checked: earlier rewrites leave replaced statements
//! behind in the store, and those are not part of the program.

const std = @import("std");
const builtin = @import("builtin");
const base = @import("base");
const layout = @import("layout");
const core = @import("lir_core");
const collections = @import("collections");
const Body = @import("body_clone.zig");

const signatures = base.low_level_signatures;
const Operand = signatures.Operand;
const Slot = signatures.Slot;
const LIR = core.LIR;
const LirStore = core.LirStore;
const GuardedList = collections.GuardedList;
const Allocator = std.mem.Allocator;

/// Check every low-level operation that a procedure in `store` reaches.
pub fn verify(allocator: Allocator, store: *LirStore, layouts: *const layout.Store) Allocator.Error!void {
    if (builtin.mode != .debug) return;
    for (0..store.procSpecCount()) |index| {
        const body = store.getProcSpec(@fromBackingInt(@intCast(index))).body orelse continue;
        var walk = try Body.ReachableStmts.initWithAllocator(store, body, allocator);
        defer walk.deinit();
        while (try walk.next()) |stmt_id| {
            const stmt = store.getCFStmt(stmt_id);
            if (stmt != .assign_low_level) continue;
            const assign = stmt.assign_low_level;
            verifyStatement(store, layouts, assign.op, assign.args, assign.target);
        }
    }
}

/// Check one low-level operation's argument and result layouts against its
/// signature.
pub fn verifyStatement(
    store: *const LirStore,
    layouts: *const layout.Store,
    op: LIR.LowLevel,
    args: LIR.LocalSpan,
    target: LIR.LocalId,
) void {
    const signature = switch (op.lirSignature()) {
        .absent => base.invariant("LIR invariant violated: {s} reached LIR", .{@tagName(op)}),
        .present => |signature| signature,
    };
    const arg_locals = store.getLocalSpan(args);
    if (arg_locals.len < signature.args.len or arg_locals.len > signature.args.len + signature.optional_args.len) {
        base.invariant("LIR invariant violated: {s} has {d} arguments", .{ @tagName(op), arg_locals.len });
    }
    var arg_layouts: [max_args]layout.Idx = undefined;
    std.debug.assert(arg_locals.len <= max_args);
    for (0..arg_locals.len) |i| arg_layouts[i] = store.localLayout(GuardedList.at(arg_locals, i));
    const check: Check = .{
        .layouts = layouts,
        .args = arg_layouts[0..arg_locals.len],
        .result = store.localLayout(target),
    };
    for (check.args, 0..) |arg_layout, i| {
        const operand = if (i < signature.args.len) signature.args[i] else signature.optional_args[i - signature.args.len];
        if (!check.matches(operand, .{ .layout = arg_layout })) {
            base.invariant("LIR invariant violated: {s} argument {d} has a layout its signature does not allow; {f}", .{ @tagName(op), i, check });
        }
    }
    if (!check.matches(signature.result, .{ .layout = check.result })) {
        base.invariant("LIR invariant violated: {s} result has a layout its signature does not allow; {f}", .{ @tagName(op), check });
    }
}

/// The most arguments any signature takes.
const max_args = blk: {
    @setEvalBranchQuota(1 << 16);
    var most: usize = 0;
    for (std.enums.values(LIR.LowLevel)) |op| switch (op.lirSignature()) {
        .absent => {},
        .present => |signature| most = @max(most, signature.args.len + signature.optional_args.len),
    };
    break :blk most;
};

/// A layout a constraint is matched against. A list or box of a zero-sized
/// type stores no element layout, so its element is only known to be
/// zero-sized.
const Value = union(enum) {
    layout: layout.Idx,
    zero_sized,
};

const Check = struct {
    layouts: *const layout.Store,
    args: []const layout.Idx,
    result: layout.Idx,

    /// The statement's layouts, for a violation report.
    pub fn format(self: Check, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll("arguments:");
        for (self.args) |arg| try self.writeLayout(writer, arg);
        try writer.writeAll(", result:");
        try self.writeLayout(writer, self.result);
    }

    fn writeLayout(self: Check, writer: *std.Io.Writer, idx: layout.Idx) std.Io.Writer.Error!void {
        const l = self.layouts.getLayout(idx);
        try writer.print(" {d} ({s}", .{ @backingInt(idx), @tagName(l.tag) });
        if (l.tag == .list or l.tag == .box or l.tag == .ptr) try writer.print(" of {d}", .{@backingInt(l.getIdx())});
        try writer.writeAll(")");
    }

    fn slot(self: Check, s: Slot) layout.Idx {
        return switch (s) {
            .arg => |i| self.args[i],
            .result => self.result,
        };
    }

    fn matches(self: Check, operand: Operand, value: Value) bool {
        const idx = switch (value) {
            .layout => |idx| idx,
            .zero_sized => return switch (operand) {
                .any => true,
                .exact => |p| p == .zst,
                .same => |s| self.isZeroSized(self.slot(s)),
                .inner_of => |s| if (self.innerOf(self.slot(s))) |inner| self.valueIsZeroSized(inner) else false,
                .any_of => |options| for (options) |option| {
                    if (self.matches(option, .zero_sized)) break true;
                } else false,
                .int,
                .int_or_dec,
                .float,
                .frac,
                .num,
                .vector,
                .tag_union,
                .lane_of,
                .widened_of,
                .narrowed_of,
                .lane_sum_of,
                .bitmask_of,
                .unsigned_of,
                .list,
                .box,
                .ptr,
                .struct_fields,
                => false,
            },
        };
        return switch (operand) {
            .any => true,
            .exact => |p| idx == primIdx(p),
            .int => isInt(idx),
            .int_or_dec => isInt(idx) or idx == .dec,
            .float => idx == .f32 or idx == .f64,
            .frac => idx == .f32 or idx == .f64 or idx == .dec,
            .num => isInt(idx) or idx == .f32 or idx == .f64 or idx == .dec,
            .vector => vectorOf(idx) != null,
            .tag_union => self.layouts.getLayout(idx).tag == .tag_union,
            .any_of => |options| for (options) |option| {
                if (self.matches(option, value)) break true;
            } else false,
            .same => |s| idx == self.slot(s),
            .inner_of => |s| if (self.innerOf(self.slot(s))) |inner| switch (inner) {
                .layout => |inner_idx| inner_idx == idx,
                .zero_sized => self.isZeroSized(idx),
            } else false,
            .lane_of => |s| if (vectorOf(self.slot(s))) |vector| idx == laneIdx(vector) else false,
            .widened_of => |s| if (vectorOf(self.slot(s))) |vector| if (widened(vector)) |wide| idx == vectorIdx(wide) else false else false,
            .narrowed_of => |s| if (vectorOf(self.slot(s))) |vector| if (vectorOf(idx)) |narrow| narrow.laneBits() * 2 == vector.laneBits() else false else false,
            .lane_sum_of => |s| if (vectorOf(self.slot(s))) |vector| idx == laneSum(vector) else false,
            .bitmask_of => |s| if (vectorOf(self.slot(s))) |vector| idx == (if (vector.laneCount() == 16) layout.Idx.u16 else layout.Idx.u8) else false,
            .unsigned_of => |s| idx == unsignedOf(self.slot(s)),
            .list => |elem| self.containerMatches(idx, .list, .list_of_zst, elem.*),
            .box => |payload| self.containerMatches(idx, .box, .box_of_zst, payload.*),
            .ptr => |pointee| self.containerMatches(idx, .ptr, null, pointee.*),
            .struct_fields => |fields| self.structMatches(idx, fields),
        };
    }

    fn containerMatches(
        self: Check,
        idx: layout.Idx,
        tag: layout.LayoutTag,
        zero_sized_tag: ?layout.LayoutTag,
        inner: Operand,
    ) bool {
        const l = self.layouts.getLayout(idx);
        if (l.tag == tag) return self.matches(inner, .{ .layout = l.getIdx() });
        if (zero_sized_tag) |zst_tag| if (l.tag == zst_tag) return self.matches(inner, .zero_sized);
        return false;
    }

    fn structMatches(self: Check, idx: layout.Idx, fields: []const Operand) bool {
        const l = self.layouts.getLayout(idx);
        if (l.tag != .struct_) return false;
        const struct_idx = l.getStruct().idx;
        const count = self.layouts.getStructData(struct_idx).getFields().count;
        var semantic_fields: usize = 0;
        for (0..count) |position| {
            const field = self.layouts.getStructField(struct_idx, @intCast(position));
            if (field.is_padding) continue;
            if (field.index >= fields.len) return false;
            if (!self.matches(fields[field.index], .{ .layout = field.layout })) return false;
            semantic_fields += 1;
        }
        return semantic_fields == fields.len;
    }

    /// The element of a list, payload of a box, or pointee of a pointer.
    fn innerOf(self: Check, idx: layout.Idx) ?Value {
        const l = self.layouts.getLayout(idx);
        return switch (l.tag) {
            .list, .box, .ptr => .{ .layout = l.getIdx() },
            .list_of_zst, .box_of_zst => .zero_sized,
            .scalar,
            .struct_,
            .closure,
            .erased_callable,
            .zst,
            .tag_union,
            .erased_box,
            .hole,
            => null,
        };
    }

    fn isZeroSized(self: Check, idx: layout.Idx) bool {
        return self.layouts.isZeroSized(self.layouts.getLayout(idx));
    }

    fn valueIsZeroSized(self: Check, value: Value) bool {
        return switch (value) {
            .layout => |idx| self.isZeroSized(idx),
            .zero_sized => true,
        };
    }
};

fn primIdx(p: signatures.Prim) layout.Idx {
    return prim_idx[@backingInt(p)];
}

/// Each `Prim`'s layout, indexed by `Prim`; each is named the same in both.
const prim_idx = blk: {
    const prims = std.enums.values(signatures.Prim);
    var table: [prims.len]layout.Idx = undefined;
    for (prims) |p| table[@backingInt(p)] = @field(layout.Idx, @tagName(p));
    break :blk table;
};

fn isInt(idx: layout.Idx) bool {
    return idx == .u8 or idx == .i8 or idx == .u16 or idx == .i16 or idx == .u32 or
        idx == .i32 or idx == .u64 or idx == .i64 or idx == .u128 or idx == .i128;
}

/// The integer layout of one lane.
fn laneIdx(vector: layout.Vector) layout.Idx {
    return switch (vector) {
        .u8x16 => .u8,
        .i8x16 => .i8,
        .u16x8 => .u16,
        .i16x8 => .i16,
        .u32x4 => .u32,
        .i32x4 => .i32,
        .u64x2 => .u64,
        .i64x2 => .i64,
    };
}

fn vectorOf(idx: layout.Idx) ?layout.Vector {
    inline for (comptime std.enums.values(layout.Vector)) |vector| {
        if (idx == vectorIdx(vector)) return vector;
    }
    return null;
}

fn vectorIdx(vector: layout.Vector) layout.Idx {
    return vector_idx[@backingInt(vector)];
}

/// Each vector's layout, indexed by `layout.Vector`; each is named the same
/// in both.
const vector_idx = blk: {
    const vectors = std.enums.values(layout.Vector);
    var table: [vectors.len]layout.Idx = undefined;
    for (vectors) |vector| table[@backingInt(vector)] = @field(layout.Idx, @tagName(vector));
    break :blk table;
};

/// The vector with lanes twice as wide and the same signedness.
fn widened(vector: layout.Vector) ?layout.Vector {
    return switch (vector) {
        .u8x16 => .u16x8,
        .i8x16 => .i16x8,
        .u16x8 => .u32x4,
        .i16x8 => .i32x4,
        .u32x4 => .u64x2,
        .i32x4 => .i64x2,
        .u64x2, .i64x2 => null,
    };
}

/// The integer a vector sums its lanes into.
fn laneSum(vector: layout.Vector) layout.Idx {
    const bits = @max(32, @as(u16, vector.laneBits()) * 2);
    return switch (bits) {
        32 => if (vector.isSigned()) .i32 else .u32,
        64 => if (vector.isSigned()) .i64 else .u64,
        128 => if (vector.isSigned()) .i128 else .u128,
        else => unreachable, // lanes are at most 64 bits wide
    };
}

/// The unsigned integer of the same width, or the layout itself when it is
/// not a signed integer.
fn unsignedOf(idx: layout.Idx) layout.Idx {
    if (idx == .i8) return .u8;
    if (idx == .i16) return .u16;
    if (idx == .i32) return .u32;
    if (idx == .i64) return .u64;
    if (idx == .i128) return .u128;
    return idx;
}

const TestStatement = struct {
    store: LirStore,
    layouts: layout.Store,

    fn init() Allocator.Error!TestStatement {
        return .{
            .store = LirStore.init(std.testing.allocator),
            .layouts = try layout.Store.init(std.testing.allocator, base.target.TargetUsize.native),
        };
    }

    fn deinit(self: *TestStatement) void {
        self.store.deinit();
        self.layouts.deinit();
    }

    /// Verify `op` applied to locals of `arg_layouts`, assigned to a local of
    /// `result_layout`.
    fn verifyOp(self: *TestStatement, op: LIR.LowLevel, arg_layouts: []const layout.Idx, result_layout: layout.Idx) Allocator.Error!void {
        var args: [max_args]LIR.LocalId = undefined;
        for (arg_layouts, 0..) |arg_layout, i| args[i] = try self.store.addLocal(.{ .layout_idx = arg_layout });
        const target = try self.store.addLocal(.{ .layout_idx = result_layout });
        verifyStatement(&self.store, &self.layouts, op, try self.store.addLocalSpan(args[0..arg_layouts.len]), target);
    }
};

fn getListItemWithI128Index() Allocator.Error!void {
    var t = try TestStatement.init();
    defer t.deinit();
    const list = try t.layouts.insertLayout(layout.Layout.list(.u64));
    try t.verifyOp(.list_get_unsafe, &.{ list, .i128 }, .u64);
}

test "a list_get_unsafe index that is not a U64 is a violated invariant" {
    try base.death_test.expectPanic(
        "LIR invariant violated: list_get_unsafe argument 1 has a layout its signature does not allow",
        getListItemWithI128Index,
        .{},
    );
}

fn addWithSourcePolicy() Allocator.Error!void {
    var t = try TestStatement.init();
    defer t.deinit();
    try t.verifyOp(.num_plus, &.{ .u64, .u64 }, .u64);
}

test "source-policy arithmetic in LIR is a violated invariant" {
    try base.death_test.expectPanic("LIR invariant violated: num_plus reached LIR", addWithSourcePolicy, .{});
}

test "operations whose layouts match their signatures verify" {
    var t = try TestStatement.init();
    defer t.deinit();
    const list = try t.layouts.insertLayout(layout.Layout.list(.u64));
    const zst_list = try t.layouts.insertLayout(layout.Layout.listOfZst());
    try t.verifyOp(.list_get_unsafe, &.{ list, .u64 }, .u64);
    try t.verifyOp(.list_get_unsafe, &.{ zst_list, .u64 }, .zst);
    try t.verifyOp(.list_append_unsafe, &.{ list, .u64 }, list);
    try t.verifyOp(.num_int_add_crash_on_overflow, &.{ .dec, .dec }, .dec);
    try t.verifyOp(.num_abs_diff, &.{ .i32, .i32 }, .u32);
    try t.verifyOp(.simd_widen_lo, &.{.i8x16}, .i16x8);
    try t.verifyOp(.simd_sum_lanes, &.{.u8x16}, .u32);
    try t.verifyOp(.u8_to_i8_wrap, &.{.u8}, .i8);
}
