//! A module that define a stack utility - Scratch - that is helpful
//! when working with recursive operations

const std = @import("std");
const DataSpan = @import("DataSpan.zig");

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

        /// Places a new index of type `T` in the scratch. Returns error on OOM.
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
        pub fn spanFromStart(self: *Self, start: u32, gpa: std.mem.Allocator, data: *std.ArrayListUnmanaged(u32)) std.mem.Allocator.Error!DataSpan {
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
    };
}
