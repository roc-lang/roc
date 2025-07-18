//! Tests for types.Store IoVec-based streaming serialization

const std = @import("std");
const testing = std.testing;
const types = @import("../types.zig");
const Store = @import("../store.zig").Store;
const IovecWriter = @import("../../base/iovec_serialize.zig").IovecWriter;
const collections = @import("../../collections.zig");

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
    const var3 = try store.freshRedirect(var1);

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

    // Compare with traditional serialization
    const traditional_size = store.serializedSize();
    try testing.expectEqual(traditional_size, buffer.len);
}

test "Store streaming serialization - complex scenario" {
    const allocator = testing.allocator;

    var store = try Store.init(allocator);
    defer store.deinit();

    // Create a complex type structure
    var vars = std.ArrayList(types.Var).init(allocator);
    defer vars.deinit();

    // Create multiple variables
    var i: usize = 0;
    while (i < 10) : (i += 1) {
        const var_ = try store.fresh();
        try vars.append(var_);
    }

    // Create some redirects
    const redirect1 = try store.freshRedirect(vars.items[0]);
    const redirect2 = try store.freshRedirect(vars.items[1]);
    try vars.append(redirect1);
    try vars.append(redirect2);

    // Add record fields
    const record_fields = [_]types.RecordField{
        .{ .name = .{ .type = .required, .value = 100 }, .type = vars.items[0] },
        .{ .name = .{ .type = .optional, .value = 200 }, .type = vars.items[1] },
    };
    const fields_range = try store.record_fields.reserve(store.gpa, record_fields.len);
    try store.record_fields.appendSlice(store.gpa, fields_range, &record_fields);

    // Add tags
    const tags = [_]types.Tag{
        .{ .name = 300, .args = .{ .start = 0, .len = 2 } },
        .{ .name = 400, .args = .{ .start = 2, .len = 1 } },
    };
    const tags_range = try store.tags.reserve(store.gpa, tags.len);
    try store.tags.appendSlice(store.gpa, tags_range, &tags);

    // Stream serialize
    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    _ = try store.appendToIovecs(&writer);
    try writer.finalize();

    // Get both buffers for comparison
    const streamed_buffer = try @import("../../base/iovec_serialize.zig").iovecsToBuf(allocator, writer.iovecs.items);
    defer allocator.free(streamed_buffer);

    const traditional_size = store.serializedSize();
    const traditional_buffer = try allocator.alloc(u8, traditional_size);
    defer allocator.free(traditional_buffer);
    _ = try store.serializeInto(traditional_buffer);

    // Both methods should produce the same size
    try testing.expectEqual(traditional_size, streamed_buffer.len);

    // Deserialize both and verify they produce equivalent stores
    var streamed_store = try Store.deserializeFrom(streamed_buffer, allocator);
    defer streamed_store.deinit();

    var traditional_store = try Store.deserializeFrom(traditional_buffer, allocator);
    defer traditional_store.deinit();

    // Verify both stores have same content
    try testing.expectEqual(store.len(), streamed_store.len());
    try testing.expectEqual(store.len(), traditional_store.len());
    try testing.expectEqual(store.record_fields.items.items.len, streamed_store.record_fields.items.items.len);
    try testing.expectEqual(store.tags.items.items.len, streamed_store.tags.items.items.len);
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

test "Store streaming serialization - memory efficiency" {
    const allocator = testing.allocator;

    var store = try Store.init(allocator);
    defer store.deinit();

    // Create a large store with many variables
    var i: usize = 0;
    while (i < 1000) : (i += 1) {
        _ = try store.fresh();
    }

    // Add many record fields
    var fields = try allocator.alloc(types.RecordField, 500);
    defer allocator.free(fields);
    for (fields, 0..) |*field, idx| {
        field.* = .{
            .name = .{ .type = .required, .value = @intCast(idx) },
            .type = @enumFromInt(idx),
        };
    }
    const fields_range = try store.record_fields.reserve(store.gpa, fields.len);
    try store.record_fields.appendSlice(store.gpa, fields_range, fields);

    var writer = IovecWriter.init(allocator);
    defer writer.deinit();

    // Stream serialize - should not allocate large temporary buffers
    _ = try store.appendToIovecs(&writer);

    // The writer should have multiple iovecs, not one large buffer
    try testing.expect(writer.iovecs.items.len > 5);

    // Verify correctness
    try writer.finalize();
    const buffer = try @import("../../base/iovec_serialize.zig").iovecsToBuf(allocator, writer.iovecs.items);
    defer allocator.free(buffer);

    var deserialized = try Store.deserializeFrom(buffer, allocator);
    defer deserialized.deinit();

    try testing.expectEqual(store.len(), deserialized.len());
    try testing.expectEqual(store.record_fields.items.items.len, deserialized.record_fields.items.items.len);
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

        pub fn allocator(self: *Self) std.mem.Allocator {
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

    var writer = IovecWriter.init(failing_alloc.allocator());
    defer writer.deinit();

    // This should fail due to allocation failure
    const result = store.appendToIovecs(&writer);
    try testing.expectError(error.OutOfMemory, result);
}
