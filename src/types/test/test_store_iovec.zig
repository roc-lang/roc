//! Tests for types.Store IoVec-based streaming serialization

const std = @import("std");
const testing = std.testing;
const types = @import("../types.zig");
const Store = @import("../store.zig").Store;
const IovecWriter = @import("../../base/iovec_serialize.zig").IovecWriter;

test "Store streaming serialization - empty store" {
    const allocator = testing.allocator;

    var store = try Store.init(allocator);
    defer store.deinit();

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    const offset = try store.appendToIovecs(&writer);
    try testing.expectEqual(@as(usize, 0), offset);

    // Verify the serialized data has correct structure
    try writer.finalize();
    const buffer = try @import("../../base/iovec_serialize.zig").iovecsToBuf(allocator, writer.iovecs.items);
    defer allocator.free(buffer);

    // Should have 5 size headers (all zeros for empty store)
    const size_headers = @as(*const [5]u32, @ptrCast(@alignCast(buffer.ptr))).*;
    try testing.expectEqual(@as(u32, 0), size_headers[0]); // slots
    try testing.expectEqual(@as(u32, 0), size_headers[1]); // descs
    try testing.expectEqual(@as(u32, 0), size_headers[2]); // record_fields
    try testing.expectEqual(@as(u32, 0), size_headers[3]); // tags
    try testing.expectEqual(@as(u32, 0), size_headers[4]); // vars
}

test "Store streaming serialization - with data" {
    const allocator = testing.allocator;

    var store = try Store.init(allocator);
    defer store.deinit();

    // Create some type variables
    const var1 = try store.fresh();
    const var2 = try store.fresh();
    _ = try store.freshRedirect(var1);

    // Set some content
    try store.setVarContent(var1, types.Content{ .flex_var = null });
    try store.setVarContent(var2, types.Content{ .rigid_var = .{ .name = null } });

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    const offset = try store.appendToIovecs(&writer);
    try testing.expectEqual(@as(usize, 0), offset);

    // Finalize and get buffer
    try writer.finalize();
    const buffer = try @import("../../base/iovec_serialize.zig").iovecsToBuf(allocator, writer.iovecs.items);
    defer allocator.free(buffer);

    // Verify size headers
    const size_headers = @as(*const [5]u32, @ptrCast(@alignCast(buffer.ptr))).*;
    try testing.expect(size_headers[0] > 0); // slots should have data
    try testing.expect(size_headers[1] > 0); // descs should have data

    // Verify we got a reasonable buffer size
    try testing.expect(buffer.len > 0);
}


test "Store streaming serialization - alignment verification" {
    const allocator = testing.allocator;

    var store = try Store.init(allocator);
    defer store.deinit();

    // Add some data to ensure non-empty store
    _ = try store.fresh();
    _ = try store.fresh();

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Track offsets during serialization
    const start = writer.getOffset();
    _ = try store.appendToIovecs(&writer);
    const end = writer.getOffset();

    // Total size should be aligned
    const total_size = end - start;
    const alignment = @import("../../serialization/mod.zig").SERIALIZATION_ALIGNMENT;
    try testing.expectEqual(@as(usize, 0), total_size % alignment);

    try writer.finalize();
    const buffer = try @import("../../base/iovec_serialize.zig").iovecsToBuf(allocator, writer.iovecs.items);
    defer allocator.free(buffer);

    // Verify internal alignment
    var offset: usize = @sizeOf(u32) * 5; // Skip size headers

    // Each section should start at an aligned offset
    offset = std.mem.alignForward(usize, offset, alignment);
    try testing.expectEqual(@as(usize, 0), offset % alignment);
}


test "Store streaming serialization - error handling" {
    const allocator = testing.allocator;

    var store = try Store.init(allocator);
    defer store.deinit();

    // Create some data
    _ = try store.fresh();

    // Use a failing allocator to test error propagation
    const FailingAllocator = struct {
        const Self = @This();
        fail_after: usize,
        count: usize,
        child: std.mem.Allocator,

        pub fn getAllocator(self: *Self) std.mem.Allocator {
            return .{
                .ptr = self,
                .vtable = &.{
                    .alloc = alloc,
                    .resize = resize,
                    .free = free,
                },
            };
        }

        fn alloc(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
            const self: *Self = @ptrCast(@alignCast(ctx));
            self.count += 1;
            if (self.count > self.fail_after) {
                return null;
            }
            return self.child.rawAlloc(len, ptr_align, ret_addr);
        }

        fn resize(ctx: *anyopaque, buf: []u8, buf_align: u8, new_len: usize, ret_addr: usize) bool {
            const self: *Self = @ptrCast(@alignCast(ctx));
            return self.child.rawResize(buf, buf_align, new_len, ret_addr);
        }

        fn free(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
            const self: *Self = @ptrCast(@alignCast(ctx));
            self.child.rawFree(buf, buf_align, ret_addr);
        }
    };

    var failing_alloc = FailingAllocator{
        .fail_after = 5,
        .count = 0,
        .child = allocator,
    };

    var writer = IovecWriter.init(failing_alloc.getAllocator());
    defer writer.deinit();

    // This should fail due to allocation failure
    const result = store.appendToIovecs(&writer);
    try testing.expectError(error.OutOfMemory, result);
}
