//! Canonical packed-argument layout for the erased-callable ABI: the
//! arguments in declaration order, each at its natural alignment, measured
//! by the shared `shape.StructMeasure`.

const std = @import("std");
const layout = @import("layout.zig");
const shape = @import("layout_shape");
const Store = @import("store.zig").Store;

/// Total storage requirements for an erased-call argument buffer.
pub const Metrics = struct {
    size: u32,
    alignment: u32,
};

/// Fill `offsets` with declaration-order, naturally aligned field offsets.
pub fn plan(store: *const Store, arg_layouts: []const layout.Idx, offsets: []u32) Metrics {
    std.debug.assert(arg_layouts.len == offsets.len);

    var measure: shape.StructMeasure = .{};
    for (arg_layouts, offsets) |arg_layout, *offset| {
        const runtime_layout = store.getLayout(store.runtimeRepresentationLayoutIdx(arg_layout));
        const size_align = store.layoutSizeAlign(runtime_layout);
        offset.* = measure.place(.{
            .size = size_align.size,
            .alignment = @intCast(size_align.alignment.toByteUnits()),
            .sort_key = runtime_layout.sortKey(),
        });
    }

    const metrics = measure.finish();
    return .{
        .size = metrics.size,
        .alignment = metrics.alignment,
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
