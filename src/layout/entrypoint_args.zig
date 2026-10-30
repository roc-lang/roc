//! The byte layout of a Roc entrypoint's argument buffer.
//!
//! The LLVM backend's host-facing wrapper takes an entrypoint's arguments in
//! the platform C ABI and packs them into one zeroed buffer, which it hands to
//! the compiled procedure or, under `--opt=interpreter`, to the interpreter
//! shim. (The LLVM backend passes every Roc procedure's arguments in a buffer
//! of this layout.) Whoever writes that buffer and whoever reads it must agree
//! on every offset, so they all compute them here.
//!
//! Arguments are placed in decreasing alignment order, ties in argument order.
//! Each occupies a slot of at least 8 bytes, so two `U32` arguments sit at
//! offsets 0 and 8, not 0 and 4.

const std = @import("std");

const layout_mod = @import("layout.zig");
const Store = @import("store.zig").Store;
const Idx = layout_mod.Idx;

/// Bytes an argument of `layout_idx` occupies in the buffer: 0 for a
/// zero-sized value, otherwise at least 8.
pub fn slotSize(store: *const Store, layout_idx: Idx) u32 {
    const runtime_idx = store.runtimeRepresentationLayoutIdx(layout_idx);
    if (runtime_idx == .str) return store.layoutSizeAlign(store.getLayout(.str)).size;
    if (runtime_idx == .i128 or runtime_idx == .u128 or runtime_idx == .dec) return 16;
    const layout_val = store.getLayout(runtime_idx);
    const size = store.layoutSizeAlign(layout_val).size;
    if (layout_val.tag == .zst or size == 0) return 0;
    // A vector occupies its full 16-byte value; an 8-byte slot would make the
    // next argument overlap its upper half.
    if (layout_val.tag == .scalar and layout_val.getScalar().tag == .vector) return 16;
    // A list's slot is three 8-byte words on every target.
    if (layout_val.tag == .list or layout_val.tag == .list_of_zst) return 3 * @sizeOf(u64);
    if ((layout_val.tag == .struct_ or layout_val.tag == .tag_union) and size > 8) {
        return std.mem.alignForward(u32, size, 8);
    }
    return 8;
}

fn alignmentOf(store: *const Store, layout_idx: Idx) u32 {
    return @intCast(store.layoutSizeAlign(store.getLayout(layout_idx)).alignment.toByteUnits());
}

/// Fill `out_offsets[i]` with the byte offset of argument `i` in the buffer.
pub fn computeOffsets(store: *const Store, arg_layouts: []const Idx, out_offsets: []u32) void {
    std.debug.assert(arg_layouts.len == out_offsets.len);
    var max_alignment: u32 = 1;
    for (arg_layouts) |arg_layout| max_alignment = @max(max_alignment, alignmentOf(store, arg_layout));

    var current: u32 = 0;
    var alignment = max_alignment;
    while (alignment >= 1) : (alignment /= 2) {
        for (arg_layouts, out_offsets) |arg_layout, *offset| {
            if (alignmentOf(store, arg_layout) != alignment) continue;
            current = std.mem.alignForward(u32, current, alignment);
            offset.* = current;
            current += slotSize(store, arg_layout);
        }
    }
}

/// Bytes the buffer needs to hold every argument at `offsets`, from
/// `computeOffsets`.
pub fn bufferSize(store: *const Store, arg_layouts: []const Idx, offsets: []const u32) u32 {
    std.debug.assert(arg_layouts.len == offsets.len);
    var total: u32 = 0;
    for (arg_layouts, offsets) |arg_layout, offset| {
        total = @max(total, offset + slotSize(store, arg_layout));
    }
    return total;
}

fn expectOffsets(store: *const Store, arg_layouts: []const Idx, expected: []const u32) error{TestExpectedEqual}!void {
    var offsets: [8]u32 = undefined;
    computeOffsets(store, arg_layouts, offsets[0..arg_layouts.len]);
    try std.testing.expectEqualSlices(u32, expected, offsets[0..arg_layouts.len]);
}

test "entrypoint args: every slot is at least 8 bytes" {
    var store = try Store.init(std.testing.allocator, .u64);
    defer store.deinit();

    try expectOffsets(&store, &.{ .u32, .u32 }, &.{ 0, 8 });
    try expectOffsets(&store, &.{ .u8, .u16 }, &.{ 8, 0 });
    try expectOffsets(&store, &.{ .bool, .i64 }, &.{ 8, 0 });
    try std.testing.expectEqual(@as(u32, 16), bufferSize(&store, &.{ .u32, .u32 }, &.{ 0, 8 }));
}

test "entrypoint args: wide values keep their full width" {
    var store = try Store.init(std.testing.allocator, .u64);
    defer store.deinit();

    const three_u32 = try store.putTuple(&.{ store.getLayout(.u32), store.getLayout(.u32), store.getLayout(.u32) });
    try std.testing.expectEqual(@as(u32, 16), slotSize(&store, three_u32));
    try expectOffsets(&store, &.{ three_u32, .u32 }, &.{ 0, 16 });

    try std.testing.expectEqual(@as(u32, 24), slotSize(&store, .str));
    try std.testing.expectEqual(@as(u32, 16), slotSize(&store, .dec));
    try std.testing.expectEqual(@as(u32, 16), slotSize(&store, .u32x4));
    try expectOffsets(&store, &.{ .u8, .dec, .str }, &.{ 40, 0, 16 });
}
