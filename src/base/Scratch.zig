//! A module that define a stack utility - Scratch - that is helpful
//! when working with recursive operations

const std = @import("std");
const base = @import("base");
const collections = @import("collections");
const serialization = @import("serialization");

/// A stack for easily adding and removing index types when doing recursive operations
pub fn Scratch(comptime T: type) type {
    return struct {
        items: std.ArrayListUnmanaged(T),

        const Self = @This();
        const ArrayList = std.ArrayListUnmanaged(T);

        pub fn init(gpa: std.mem.Allocator) std.mem.Allocator.Error!Self {
            const items = try ArrayList.initCapacity(gpa, std.math.ceilPowerOfTwoAssert(usize, 64));
            return .{
                .items = items,
            };
        }

        pub fn deinit(self: *Self, gpa: std.mem.Allocator) void {
            self.items.deinit(gpa);
        }

        /// Returns the start position for a new Span of indexes in scratch
        pub fn top(self: *Self) u32 {
            return @as(u32, @intCast(self.items.items.len));
        }

        /// Places a new index of type `T` in the scratch.  Will panic on OOM.
        pub fn append(self: *Self, gpa: std.mem.Allocator, idx: T) std.mem.Allocator.Error!void {
            try self.items.append(gpa, idx);
        }

        /// Creates slice from the provided indexes
        pub fn slice(self: *Self, start: u32, end: u32) []T {
            return self.items.items[@intCast(start)..@intCast(end)];
        }

        /// Creates slice from the provided start index
        pub fn sliceFromStart(self: *Self, start: u32) []T {
            return self.items.items[@intCast(start)..];
        }

        /// Creates a new span starting at start.  Moves the items from scratch
        /// to extra_data as appropriate.
        pub fn spanFromStart(self: *Self, start: u32, gpa: std.mem.Allocator, data: *std.ArrayListUnmanaged(u32)) std.mem.Allocator.Error!base.DataSpan {
            const end = self.items.len;
            defer self.items.shrinkRetainingCapacity(start);
            var i = @as(usize, @intCast(start));
            const data_start = @as(u32, @intCast(data.items.len));
            while (i < end) {
                data.append(gpa, self.items[i].id);
                i += 1;
            }
            return .{ .span = .{ .start = data_start, .len = @as(u32, @intCast(end)) - start } };
        }

        /// Clears any ids added to scratch from start until the end.
        /// Should be used wherever the scratch items will not be used,
        /// as in when parsing fails.
        pub fn clearFrom(self: *Self, start: u32) void {
            self.items.shrinkRetainingCapacity(start);
        }

        /// Serialized representation of a Scratch
        pub const Serialized = struct {
            offset: u64,
            len: u64,
            capacity: u64,

            /// Serialize a Scratch into this Serialized struct, appending data to the writer
            pub fn serialize(
                self: *Serialized,
                scratch: *const Self,
                allocator: std.mem.Allocator,
                writer: anytype,
            ) std.mem.Allocator.Error!void {
                const items = scratch.items.items;
                // Append the slice data first
                const slice_ptr = try writer.appendSlice(allocator, items);
                // Store the offset, len, and capacity
                self.offset = @intFromPtr(slice_ptr.ptr);
                self.len = items.len;
                self.capacity = scratch.items.capacity;
            }

            /// Deserialize this Serialized struct into a Scratch
            pub fn deserialize(self: *Serialized, offset: i64) *Self {
                // Debug assert that Serialized is at least as big as Self
                std.debug.assert(@sizeOf(Serialized) >= @sizeOf(Self));

                // Apply the offset to convert from serialized offset to actual pointer
                const adjusted_offset: u64 = if (offset >= 0)
                    self.offset + @as(u64, @intCast(offset))
                else
                    self.offset - @as(u64, @intCast(-offset));

                const items_ptr = @as([*]T, @ptrFromInt(adjusted_offset));
                
                // Cast self to Self pointer and construct the Scratch in place
                const scratch_ptr = @as(*Self, @ptrCast(self));
                scratch_ptr.* = .{
                    .items = .{
                        .items = items_ptr[0..self.len],
                        .capacity = self.capacity,
                    },
                };

                return scratch_ptr;
            }
        };
    };
}

test "Scratch.Serialized roundtrip" {
    const testing = std.testing;
    const gpa = testing.allocator;
    const CompactWriter = serialization.CompactWriter;

    // Define a test type
    const TestIdx = enum(u32) { _ };

    // Create original scratch and add some items
    var original = try Scratch(TestIdx).init(gpa);
    defer original.deinit(gpa);

    try original.append(gpa, @enumFromInt(100));
    try original.append(gpa, @enumFromInt(200));
    try original.append(gpa, @enumFromInt(300));

    // Create a CompactWriter and arena
    var arena = std.heap.ArenaAllocator.init(gpa);
    defer arena.deinit();
    const arena_alloc = arena.allocator();

    var tmp_dir = testing.tmpDir(.{});
    defer tmp_dir.cleanup();
    const tmp_file = try tmp_dir.dir.createFile("test.compact", .{ .read = true });
    defer tmp_file.close();

    var writer = CompactWriter{
        .iovecs = .{},
        .total_bytes = 0,
    };
    defer writer.deinit(arena_alloc);

    // Allocate and serialize using the Serialized struct
    const serialized_ptr = try writer.appendAlloc(arena_alloc, Scratch(TestIdx).Serialized);
    try serialized_ptr.serialize(&original, arena_alloc, &writer);

    // Write to file
    try writer.writeGather(arena_alloc, tmp_file);

    // Read back
    const file_size = try tmp_file.getEndPos();
    const buffer = try gpa.alloc(u8, file_size);
    defer gpa.free(buffer);
    _ = try tmp_file.pread(buffer, 0);

    // Deserialize
    const serialized_offset = writer.total_bytes - @sizeOf(Scratch(TestIdx).Serialized);
    const deserialized_ptr = @as(*Scratch(TestIdx).Serialized, @ptrCast(@alignCast(buffer.ptr + serialized_offset)));
    const scratch = deserialized_ptr.deserialize(@as(i64, @intCast(@intFromPtr(buffer.ptr))));

    // Verify the items are accessible
    try testing.expectEqual(@as(usize, 3), scratch.items.items.len);
    try testing.expectEqual(@as(u32, 100), @intFromEnum(scratch.items.items[0]));
    try testing.expectEqual(@as(u32, 200), @intFromEnum(scratch.items.items[1]));
    try testing.expectEqual(@as(u32, 300), @intFromEnum(scratch.items.items[2]));

    // Verify scratch methods work
    try testing.expectEqual(@as(u32, 3), scratch.top());
    const slice = scratch.slice(1, 3);
    try testing.expectEqual(@as(usize, 2), slice.len);
    try testing.expectEqual(@as(u32, 200), @intFromEnum(slice[0]));
    try testing.expectEqual(@as(u32, 300), @intFromEnum(slice[1]));
}

