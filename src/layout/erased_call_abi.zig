//! Canonical packed-argument layout for the erased-callable ABI.

const std = @import("std");
const layout = @import("layout.zig");
const Store = @import("store.zig").Store;

pub const Metrics = struct {
    size: u32,
    alignment: u32,
};

/// Fill `offsets` with declaration-order, naturally aligned field offsets.
pub fn plan(store: *const Store, arg_layouts: []const layout.Idx, offsets: []u32) Metrics {
    std.debug.assert(arg_layouts.len == offsets.len);

    var size: u32 = 0;
    var max_alignment: u32 = 1;
    for (arg_layouts, offsets) |arg_layout, *offset| {
        const runtime_layout = store.runtimeRepresentationLayoutIdx(arg_layout);
        const size_align = store.layoutSizeAlign(store.getLayout(runtime_layout));
        const alignment: u32 = @intCast(@max(size_align.alignment.toByteUnits(), 1));
        size = std.mem.alignForward(u32, size, alignment);
        offset.* = size;
        size += size_align.size;
        max_alignment = @max(max_alignment, alignment);
    }

    return .{
        .size = std.mem.alignForward(u32, size, max_alignment),
        .alignment = max_alignment,
    };
}

test "erased call arguments preserve declaration order and natural alignment" {
    var store = try Store.init(std.testing.allocator, .u64);
    defer store.deinit();

    var offsets: [2]u32 = undefined;
    const metrics = plan(&store, &.{ .u8, .u64 }, &offsets);

    try std.testing.expectEqualSlices(u32, &.{ 0, 8 }, &offsets);
    try std.testing.expectEqual(@as(u32, 16), metrics.size);
    try std.testing.expectEqual(@as(u32, 8), metrics.alignment);
}

test "erased call argument plans pack adjacent sub-word fields" {
    var store = try Store.init(std.testing.allocator, .u64);
    defer store.deinit();

    var offsets: [3]u32 = undefined;
    const metrics = plan(&store, &.{ .u8, .u8, .u64 }, &offsets);

    try std.testing.expectEqualSlices(u32, &.{ 0, 1, 8 }, &offsets);
    try std.testing.expectEqual(@as(u32, 16), metrics.size);
    try std.testing.expectEqual(@as(u32, 8), metrics.alignment);
}
